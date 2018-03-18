module Core.Data.Rewrite (
  -- | * Terms
  Term (..),
  label,
  isVar,
  isFun,
  -- | * Listings
  vars,
  funs,
  nullary,
  -- | * Rules
  Rule (..),
  left,
  right,
  -- | * Rewrites
  Partial,
  rewrite,
  rewrite',
  once,
  reduce,
  -- | * Substitution 
  Subst,
  unify,
  unifyS,
  compose,
  substitute
  ) where

import Protolude hiding (reduce)
import Pre

import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List

import Data.Bifunctor
import Data.Bifoldable

import Text.PrettyPrint.Leijen.Text (
  Pretty (..),
  (<$$>),
  (<+>),
  textStrict,
  tupled,
  pretty
  )

import Core.Data.Symbol

-- | Terms are built from functions and variables
data Term a b = Var a | Fun b [Term a b]
  deriving (Ord,Eq,Show)

instance Bifunctor Term where
  bimap f g (Var a  ) = Var (f a)
  bimap f g (Fun a b) = Fun (g a) $ map (bimap f g) b

instance Bifoldable Term where
  bifoldMap f _ (Var a  ) = f a
  bifoldMap f g (Fun a b) = mappend (g a) $ foldMap (bifoldMap f g) b

instance Symbolical a => Symbolical (Term a b) where
  encode (Var a) = encode a
  decode         = Var . decode

instance (Pretty a, Pretty b) => Pretty (Term a b) where
  pretty (Var a   ) = pretty a
  pretty (Fun a []) = pretty a
  pretty (Fun a b ) = pretty a <> tupled (map pretty b)

-- | Show label of the root node
label :: Term a b -> b
label (Fun a b) = a

-- | Direct subterms of the root node
subterms :: Term a b -> [Term a b]
subterms (Var a  ) = []
subterms (Fun a b) = b

-- | Check if term is a variable
isVar :: Term a b -> Bool
isVar (Var a  ) = True
isVar (Fun a b) = False

-- | Check if term is a function
isFun :: Term a b -> Bool
isFun = not . isVar

-- | List all variables
vars :: Ord a => Term a b -> Set a
vars = bifoldMap Set.singleton mempty

-- | List all function symbols
funs :: Ord b => Term a b -> Set b
funs = bifoldMap mempty Set.singleton

-- | List arity for function symbols
arity :: Ord b => Term a b -> Map b [Int]
arity = map toList . arity'
  where arity' (Var a  ) = Map.empty
        arity' (Fun a b) =
          Map.unionsWith mappend (
            Map.singleton a (Set.singleton $ length b) : map arity' b
          )

-- | List only constant function symbols
nullary :: (Ord a, Ord b) => Term a b -> Set b
nullary = Map.keysSet . Map.filter ([0] ==) . arity

-- | Rewrite rule
data Rule a b = Rule (Term a b) (Term a b)
  deriving (Eq,Ord,Show)

instance (Pretty a, Pretty b) => Pretty (Rule a b) where
  pretty (Rule a b) = pretty a <+> textStrict "→" <+> pretty b

-- | Left part of a rule
left (Rule a b) = a

-- | Right part of a rule
right (Rule a b) = b

type Path = [Int]

-- | Store a list of active nodes to avoid deadends while pattern matching
data Partial a b = Partial [Path] (Term a b)
  deriving (Show)

-- | Convert a term into a partial evaluation, all nodes will be consider active
toPartial term = Partial (paths [] term) term

-- | Convert a partial evaluation back into a term
fromPartial (Partial a b) = b

-- | Enumerate paths for all subterms
paths :: Path -> Term a b -> [Path]
paths pre (Var a  ) = [pre]
paths pre (Fun a b) =  pre : concat (zipWith paths [pre ++ [c] | c <- [0..]] b)

-- | List all possible reductions for the outermost term
outer :: (Ord a, Ord b) => [Rule a b] -> Term a b -> [Term a b]
outer rules term = mapMaybe outer' rules
  where outer' (Rule l r) =
          case runExcept $ unify l term of
            Left  e -> Nothing
            Right v -> Just $ substitute v r

-- | List all possible rewrites, includes rewrites on subterms
redex :: (Ord a, Ord b) => [Rule a b] -> Partial a b -> Map Path [Term a b]
redex rules (Partial pre term) = redex' (pre, []) term
  where redex' (pre, path) (Var a) = Map.empty
        redex' (pre, path)  node
          | elem [] pre =
              case outer rules node of
                [] -> subnodes
                re -> Map.singleton path re
          | otherwise = subnodes
              where subnodes = mconcat $ catMaybes $ zipWith forward [0..] $ subterms node
                    forward k sub =
                      case shift of
                        (hd, tails) | not $ elem k hd -> Nothing
                                    | otherwise       -> Just $ redex' (tails, k:path) sub
                    shift = List.unzip $ catMaybes $ map uncons pre

-- | Patch a nested rewrite into a term
nested :: (Path, Term a b) -> Term a b -> Term a b
nested ([] , re)  term     = re
nested (h:t, re) (Fun a b) = Fun a $ zipWith nested' [0..] b
  where nested' k v | k == h    = nested (t,re) v
                    | otherwise = v

-- | Rebuild the set of active nodes
active :: [(Path, Term a b)] -> [Path]
active = concatMap prefix . fst . List.unzip
 where prefix  []   = [[]]
       prefix (h:t) = [h:t, List.init (h:t)]

-- | Simple rewrite, evaluate only one redex if several rewrites are possible
rewrite :: (Ord a, Ord b) => [Rule a b] -> Partial a b -> [Partial a b]
rewrite rules part@(Partial pre term) =
  case redex rules part of
    re | (not . null) re -> pure $ apply $ Map.assocs $ map List.head re
       | otherwise       -> []
  where apply re = Partial (active re) $ foldr nested term re

-- | Parallel rewrite, evaluate all possible combinations of rewrites
rewrite' :: (Ord a, Ord b) => [Rule a b] -> Partial a b -> [Partial a b]
rewrite' rules part@(Partial pre term) =
  let expand re
        | null re   = []
        | otherwise =
            split $
            sortOn (length . snd) $
            Map.assocs re
      
      split re = par (seq term re1) re2
        where (re1,re2) = List.partition (\(k,v) -> length v < 2) re
      
      seq t0 re = Partial (active re') $ foldr nested t0 re'
        where re' = map (second List.head) re
      
      par (Partial p t0) re =
        map (\k -> Partial (active k ++ p) $ foldr nested t0 k) $
          products re
      
      products  []              = [[]]
      products ((path,ls):tail) = [(path, h):t | h <- ls, t <- products tail]
      
  in expand $ redex rules part

-- | Rewrite all subterms, but no more than once (non-recursive version)
once :: (Partial a b -> [Partial a b]) -> Term a b -> [Term a b]
once fx term = maybe [term] (pure . fromPartial) $ listToMaybe (fx $ toPartial term)

-- | Rewrite until normal form is reached
reduce :: (Partial a b -> [Partial a b]) -> Term a b -> [Term a b]
reduce fx term = map fromPartial $ reduce' (toPartial term, [toPartial term])
  where reduce' (Partial a b, []) = [Partial [] b]
        reduce' (term       , rw) = concatMap reduce' $ map (identity &&& fx) rw

-- | Substitution, maps variables to terms
type Subst a b = Map a (Term a b)

data SubstError a b
  = Conflict (Term a b) (Term a b)
    -- ^ Trying to unite two missmatching terms
  | Occurs (Term a b) (Term a b)
    -- ^ Occurs check failed, we would get an infinite expansion

instance (Pretty a, Pretty b) => Pretty (SubstError a b) where
  pretty (Conflict a b) =
    textStrict "attempting to unify incomparable terms" <$$>
      pretty a <+>
      textStrict "≠" <+>
      pretty b
  pretty (Occurs a b) =
    textStrict "occurs check failed, circular rewrite term found" <$$>
      pretty a  <+>
      textStrict "≠" <+>
      pretty b

-- | Checks if the variable occurs unbound
occurs :: (Ord a) => a -> Term a b -> Bool
occurs var term = elem var $ vars term

-- | Unify two terms
unify :: (Ord a, Ord b)
      => Term a b
      -> Term a b
      -> Except (SubstError a b) (Subst a b)
unify    (Var a)       (Var b)    | a == b      = return Map.empty
                                  | a /= b      = return $ Map.singleton a (Var b)
unify lt               (Var b)                  = unify (Var b) lt
unify lt@(Var a)    rt            | occurs a rt = throwError $ Occurs lt rt
                                  | otherwise   = return $ Map.singleton a rt
unify lt@(Fun a nx) rt@(Fun b mx) | a /= b      = throwError $ Conflict lt rt
                                  | otherwise   = unifyS $ zip nx mx

-- | Unify a list of terms
unifyS :: (Ord a, Ord b)
       => [(Term a b, Term a b)]
       -> Except (SubstError a b) (Subst a b)
unifyS = foldM (
  \st (t1,t2) ->
    map (compose st) $
      on unify (substitute st) t1 t2
  ) Map.empty

-- | Compose substitutions, right associative
compose :: (Ord a, Ord b) => Subst a b -> Subst a b -> Subst a b
compose lt rt = lt <> map (substitute lt) rt

-- | Apply substitution to variables
substitute :: (Ord a, Ord b) => Subst a b -> Term a b -> Term a b
substitute st (Var a  ) = Map.findWithDefault (Var a) a st
substitute st (Fun a b) = Fun a $ map (substitute st) b
