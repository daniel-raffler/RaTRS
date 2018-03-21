{-# LANGUAGE FlexibleInstances #-}

-- | Type inference
--
--   We'll use a constrain based approach for type inference, see "Generalizing
--   Hindley-Milner Type Inference Algorithms" by Heeren, Hage and Swierstra for details on
--   the algorithm

module Core.Type (
  -- * Type
  typed,
  -- * Generation
  Assumptions,
  Constraint (..),
  annotate,
  generate,
  datatypes,
  -- * Resolution
  generalize,
  instantiate,
  active,
  apply,
  solve,
  recover
  ) where

import Protolude hiding (Type, All, Const, Constraint)
import Pre

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.List as List

import Data.Set ((\\))

import Text.PrettyPrint.Leijen.Text as Pretty (
  Pretty (..),
  (<+>),
  (<$$>),
  textStrict,
  tupled,
  list,
  equals,
  dot,
  hsep
  )

import Core.Data.Types
import Core.Data.Rewrite
import Core.Data.Program
import Core.Data.Symbol

-- | Typing constraints
data Constraint
  = Equal Type Type
  -- ^ Equality constraint
  --
  --   [@type@]
  --   [@type@]
  | Instantiate Type Scheme
  -- ^ Explicit instantiation
  --
  --   [@type@]   will be unified with an instance of the scheme
  --   [@scheme@] type scheme
  --
  --   Instantiates the type scheme with fresh variables and adds a new equality constraint
  --   to the set
  | Generalize [Type] Type Type
  -- ^ Implicit instantiation
  --
  --   [@env@]  bound variables, effectivly unused since we don't support nested let blocks
  --   [@type@] will be unified with an instance of the scheme
  --   [@type@] will be generalized to a scheme
  --
  --   Generalize the type on the right as soon as all unbound variables have been fully
  --   resolved, then add a new excplicit instantiation constraint to the set
  deriving (Show)

instance Pretty Constraint where
  pretty (Equal         t1 t2) = pretty t1 <+> equals <+> pretty t2
  pretty (Instantiate   t1 t2) = pretty t1 <+> textStrict "≺" <+> pretty t2
  pretty (Generalize bv t1 t2) =
    pretty t1 <+> textStrict "≺" <> (Pretty.list $ map pretty bv) <+> pretty t2

-- | Assumptions are used when locally bound variables are referenced. There may be
--   several assumptions for the same reference, the generated constraints are unified
--   at the lambda that created the type variable
type Assumptions = Map Text [Type]

type AC = (Assumptions, [Constraint])

-- | Generate type constraints for a program
generate :: Program -> Symbolic (Assumptions, [Constraint])
generate (Program modules types) =
  let merge :: AC -> AC -> AC
      merge (a,b) (n,m) = (Map.unionWith (++) a n, b ++ m)
      
      merges :: [AC] -> AC
      merges = (Map.unionsWith (++) *** concat) . List.unzip
      
      genMod :: AC -> Module -> Symbolic AC
      genMod nacx (Module rules) =
        do lacx <- map merges $ mapM genRule rules
           let nacx' = foldr poly nacx rules
               lacx' = foldr mono lacx rules
           return $ merge nacx' lacx'
      
      genRule :: Rule Text Text -> Symbolic AC
      genRule (Rule args body) = head $ types ! (label args)
        where head :: Type -> Symbolic AC
              head p =
                do f0  <- variable
                   f1  <- variable
                   ac1 <- (apply rhs) (Map.empty, []) (body,f0)
                   ac2 <-  unfold      ac1            (args,f1)
                   let (ax,cx) = ac2
                   return (ax, Equal (arrow f1 f0) p : cx)
              
              -- | Apply functions/constructors recursively
              apply :: (AC -> (Term Text Text, Type) -> AC)
                    -> AC
                    -> (Term Text Text, Type)
                    -> Symbolic AC
              apply op acx (Var lab, p) = return $ op acx (Var lab, p)
              apply op acx (Fun lab sub, p) =
                do f0 <- variable
                   fx <- variables $ length sub
                   (ax,cx) <- foldM (apply op) acx $ zip sub fx
                   return (
                     Map.insertWith (++) lab [f0] ax,
                     Equal (arrow (prod fx) p) f0 : cx
                     )
              
              -- | Right side of an expression, add an assuption for each reference
              rhs acx (Var lab, p) = merge (Map.singleton lab [p], []) acx
              
              -- | Left side of an expression, cancel assumption and add equality constraint
              lhs (ax,cx) (Var lab, p) =
                (Map.delete lab ax, map (Equal p) (ax !? lab) ++ cx)
              
              -- | Unfold the tuple on the left side of the expression
              unfold :: AC -> (Term Text Text, Type) -> Symbolic AC
              unfold acx (Fun lab sub,p) =
                do fx <- variables $ length sub
                   (ax,cx) <- foldM (apply lhs) acx $ zip sub fx
                   return (ax,
                     Equal (prod fx) p : cx
                     )
      
      mono = capture  Equal
      poly = capture (Generalize [])
      
      -- | Capture assumptions and generate constraints
      capture :: (Type -> Type -> Constraint) -> Rule Text Text -> AC -> AC
      capture op (Rule lt rt) (ax,cx) =
        (Map.delete sym ax, map (flip op p) (ax !? sym) ++ cx)
        where sym = label lt
              p   = types ! sym
      
  in foldM genMod (Map.empty,[]) modules

-- | Add subtyping constraints for all value constructors
datatypes :: Map Text Scheme -> (Assumptions, [Constraint]) -> [Constraint]
datatypes mp (ax,cx) =
  cx ++ [Instantiate asp sign | (sym,sign) <- Map.assocs mp, asp <- ax !? sym]

-- | Generalize a type with respect to a set of bound variables
generalize :: Set Symbol -> Type -> Symbolic Scheme
generalize bv sign = return $ All (vars sign \\ bv) sign

-- | Instantiate a type scheme with fresh variables
instantiate :: Scheme -> Symbolic Type
instantiate (All fv sign) =
  do fx <- variables (length fv)
     return $ substitute (
       Map.fromList $ zip (toList fv) fx
       ) sign

-- | List all variables that are active, that is appear freely in the constraints
active :: [Constraint] -> Set Symbol
active =
  Set.unions . map (
    \case Equal         t1         t2  -> on Set.union vars t1 t2
          Instantiate   t1 (All fv t2) -> on Set.union vars t1 t2 \\ fv
          Generalize bv t1         t2  ->
            vars t1 <> (
            Set.intersection (vars t2) $ Set.unions (map vars bv)
            )
    )

-- | Apply substitution to unbound variables in a constraint
apply :: Map Symbol Type -> Constraint -> Constraint
apply st c =
  case c of
    Equal t1 t2 -> on Equal (substitute st) t1 t2
    Instantiate t1 (All fv t2) -> 
      Instantiate (substitute st t1) (
        All fv $ substitute (restrict st fv) t2
        )
      where restrict st fv = Map.filterWithKey (\k _ -> elem k fv) st
    Generalize bv t1 t2 ->
      on (Generalize $ map (substitute st) bv)
        (substitute st) t1 t2

-- | Resolve constraint set
solve :: [Constraint] -> Symbolic (Map Symbol Type)
solve =
  let -- | Select the next constaint to process
      next st  []    = return st
      next st (x:xs) = reduce (active xs) (apply st x) >>=
        \case Nothing -> next st (xs ++ [x])
              Just s0 -> next (compose s0 st) xs
      
      -- | Try to process the next constraint
      reduce :: Set Symbol -> Constraint -> Symbolic (Maybe (Map Symbol Type))
      reduce av =
        \case Equal t1 t2 ->
                case runExcept (unify t1 t2) of
                  Left  m -> panic $ pshow m
                  Right s -> return $ Just s
              Instantiate   t1 t2 -> reduce av =<< map (Equal t1) (instantiate t2)
              Generalize bv t1 t2
                | null cut  -> reduce av =<< map (Instantiate t1) (generalize ctx t2)
                | otherwise -> return Nothing
                where cut = Set.intersection av (vars t2 \\ ctx)
                      ctx = Set.unions $ map vars bv
      
  in next Map.empty

-- | Store type signatures from the just generated typing in the sytax tree
recover :: Program -> Subst Symbol Const -> Program
recover (Program rules types) st = Program rules $ map lookup types
  where lookup (Var a) = Map.findWithDefault (Var a) a st

-- | Generate a typing for a program
typed :: Map Text Scheme -> Program -> Program
typed dt pr = runSymbolic (
  do ann <- annotate pr
     acx <- generate ann
     sub <- solve (datatypes dt acx)
     return $ recover ann sub
  )
