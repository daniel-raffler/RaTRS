{-# LANGUAGE FlexibleContexts #-}

module Core.Data.Automata (
  Automaton (..),
  State,
  Transition (..),
  -- * Alphabet
  alphabet,
  arity,
  -- * States
  states,
  initial,
  final,
  -- * graphViz
  Node,
  graph,
  graphDot,
  graphXlib,
  -- * Composition
  add,
  mult,
  hadamard,
  cauchy,
  -- * Evaluation
  eval,
  minimize
  ) where

import Protolude hiding (State, reduce, transpose, (<.>))
import Pre

import Text.PrettyPrint.Leijen.Text (
  Pretty (..),
  (<+>),
  (<$$>),
  textStrict,
  tupled,
  semiBraces,
  align,
  vsep,
  hsep,
  hang,
  pretty,
  )

import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List

import qualified Data.Text.Lazy as LT

import Data.Bifoldable

import Core.Data.Symbol as Symbol
import Core.Data.Matrix

import Core.Data.Rewrite
import Core.Data.Grammar

import Core.Semiring.Nat

import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Graph hiding ((&))
import Data.GraphViz.Types.Canonical (GlobalAttributes (..))
import Data.GraphViz.Commands.IO
import Data.GraphViz.Commands

type State = Int

data Transition = Transition [State] Text State
  deriving (Eq,Ord,Show)

-- | Weighted (bottum up) tree automata
data Automaton
  = Automaton 
      (Map Transition Rational)
      State
  -- ^ [@transitions@]
  --   [@final state@]

instance Pretty Transition where
  pretty (Transition qk s q) =
    textStrict s <+> textStrict ":" <+>
      (tupled $ map (textStrict . Symbol.decode) qk) <+> textStrict "→" <+>
        textStrict (Symbol.decode q)

instance Pretty Automaton where
  pretty at@(Automaton transition qf) =
    vsep [
      textStrict "Q:" <+> align (
        semiBraces $ map (textStrict . Symbol.decode) $
          states at
        ),
      textStrict "Σ:" <+> align (semiBraces $ map textStrict $ alphabet at),
      textStrict "δ:" <+> align (vsep $ map pretty $ Map.keys transition),
      textStrict "F:" <+> textStrict (Symbol.decode qf)
    ]

-- | Show alphabet
alphabet :: Automaton -> [Text]
alphabet (Automaton rules qf) = List.nub [s | (Transition qk s q) <- Map.keys rules]

-- | Arity for a given function symbol
arity :: Automaton -> Text -> Int
arity (Automaton rules qf) c =
  maximum [length qk | (Transition qk s q) <- Map.keys rules, s == c]

-- | Show states
states :: Automaton -> [State]
states (Automaton rules qf) =
  toList $ Set.insert qf $ mconcat
    [Set.fromList (q:qk) | Transition qk s q <- Map.keys rules]

-- | Show all initial states for a given input symbol
initial :: Text -> Automaton -> [State]
initial alpha (Automaton rules qf) =
  [q | (Transition [] s q, w) <- Map.assocs rules,
       s == alpha &&
       w /= 0]

-- | Show final state
final :: Automaton -> State
final (Automaton rules qf) = qf

type Node = Int

-- | Create a graph for the transition relation and return its graphViz representation
graph :: Automaton -> DotGraph Node
graph eta@(Automaton rules qf) =
  let nodes :: Symbolic (Map State Node, [DotNode Node])
      nodes =
        map (first Map.fromList) .
        map List.unzip .
        mapM (
          \(q,lab) ->
            do v <- variable
               return ((q,v), DotNode v lab)
        ) $
        map (identity &&& label) $
        states eta
      
      label q = [Label $ StrLabel $ (toSL :: Text -> LT.Text) $ Symbol.decode q]
      
      edges :: Map State Node -> Symbolic ([DotEdge Node], [DotNode Node])
      edges st =
        map (concat *** catMaybes) .
        map List.unzip .
        mapM (
          \(Transition qk s qt, wt) ->
            let label = [Label $ StrLabel $ toSL s]

                edge v []  = [DotEdge (st!qt)  v     label]
                edge v [q] = [DotEdge (st!qt) (st!q) label]
                edge v  qk =  DotEdge (st!qt)  v     label : map middle qk
                  where middle qs = DotEdge v (st!qs)
                                      [ArrowHead $ AType
                                        [(ArrMod FilledArrow RightSide, NoArrow)]]
                
                node v []  = Just $ DotNode v [Style [SItem Invisible []]]
                node v [q] = Nothing
                node v  qk = Just $ DotNode v [Shape PointShape]
            in do v <- variable
                  return (edge v qk, node v qk)
          ) $
        Map.assocs rules
      
      endstate :: Map State Node -> Symbolic ([DotEdge Node],[DotNode Node])
      endstate st =
        do v <- variable
           return (
             [DotEdge v (st!qf) []],
             [DotNode v [Style [SItem Invisible []]]])
      
      build :: Symbolic (DotGraph Node)
      build =
        do (st,n0) <- nodes
           (e1,n1) <- edges st
           (e2,n2) <- endstate st
           return $ mkGraph (n0 <> n1 <> n2) (e1 <> e2)
      
  in Symbol.runSymbolic build

-- | Create a graph for the transition relation and store it as a .dot file
--
--   (Requires graphViz to be installed)
graphDot :: Text -> Automaton -> IO ()
graphDot path eta =
  quitWithoutGraphviz "Could not find graphViz installation" >>
  writeDotFile (toS path) (graph eta)

-- | Display a graph of the transition relation in a new window
--
--   (Requires graphViz with Xlib support)
graphXlib :: Automaton -> IO ()
graphXlib eta =
  quitWithoutGraphviz "Could not find graphViz installation" >>
  runGraphvizCanvas Dot (graph eta) Xlib

-- | η₁+η₂
--
--   Addition
add :: Automaton -> Automaton -> Symbolic Automaton
add (Automaton r1 f1) (Automaton r2 f2) =
  return $
    Automaton
      (Map.union r1 $ Map.mapKeys last r2)
      f1
  where last (Transition qk s q)
          | q == f2   = Transition qk s f1
          | otherwise = Transition qk s q

-- | a×η
--
--   Scalar product
mult :: Int -> Automaton -> Symbolic Automaton
mult c (Automaton r qf) = return $ Automaton (Map.mapWithKey factor r) qf
  where factor (Transition qk s q) m
          | q == qf   = fromIntegral c * m
          | otherwise = m

-- | a⊙η
--
-- | Hadamard product
hadamard :: Automaton -> Automaton -> Symbolic Automaton
hadamard eta1@(Automaton r1 f1) eta2@(Automaton r2 f2) =
  do let pstates = on products' states eta1 eta2
         products' u v = map (\[a,b] -> (a,b)) $ products [u,v]
     st <- map (Map.fromList . zip pstates) (variables $ length pstates)
     return $
       Automaton
       (Map.fromList $
        [(Transition (map (st!) $ zip qk1 qk2) s1 (st!(q1,q2)), wt1 * wt2)
          | (Transition qk1 s1 q1, wt1) <- Map.assocs r1,
            (Transition qk2 s2 q2, wt2) <- Map.assocs r2,
            s1 == s2])
       (st!(f1,f2))

-- | η₁ ∘α η₂
--
--   Cauchy product
cauchy :: Automaton -> Text -> Automaton -> Symbolic Automaton
cauchy (Automaton r1 f1) alpha (Automaton r2 f2) =
  return $
    Automaton (
      Map.fromList (
          Map.assocs (Map.filterWithKey (\(Transition qk s qf) _ -> qf /= f2     ) r2) ++
          Map.assocs (Map.filterWithKey (\(Transition qk s qf) _ -> length qk > 0) r1) ++
          catMaybes ([mixed a u | a <- Map.assocs r1, u <- Map.assocs r2])
          )
      ) f1
  where mixed (Transition [] s1 q1, w1) (Transition qk2 s2 q2, w2)
          | q2 == f2 && s1 == alpha = Just (Transition qk2 s2 q1, w2 * w1)
        mixed _ _                   = Nothing

-- | Rename states to gain a more compact set of transitions
relabel :: Automaton -> Automaton
relabel eta@(Automaton rules qf) = Automaton (Map.mapKeys relabel' rules) (st!qf)
  where relabel' (Transition qk s q) = Transition (map (st!) qk) s (st!q)
        st = Map.fromList $ zip (states eta) [1..]

-- | Matrix representation for the automata transitions
matrix :: Automaton -> Map Text (SpMatrix Rational)
matrix = matrix' . relabel
  where matrix' eta@(Automaton rules qf) =
            Map.fromList $ map (identity &&& transitions) $ alphabet eta
          where transitions label = fromListSM (rows, cols)
                  [(offset (rows,cols) qf, q, wt)
                    | (Transition qf s q, wt) <- Map.assocs rules, label == s]
                  where cols = (length $ states eta)+1
                        rows = cols^(arity eta label)

-- | Evaluate the automaton
eval :: Automaton -> Term Text Text -> Rational
eval eta =
  let collapse' :: Map Text (SpMatrix Rational) -> Term Text Text -> SpMatrix Rational
      collapse' mt (Fun a []) = mt!a
      collapse' mt (Fun a nx) = kronecker' (map (collapse' mt) nx) ## (mt!a)
      
      eval' eta@(Automaton rules qf) term = lookup $ collapse' (matrix eta) term
        where lookup m = maybe 0 identity $ lookupSM m 0 qf
      
  in eval' (relabel eta)

minimize :: Automaton -> Automaton
minimize eta = undefined

{-
minimize :: Automaton -> SpMatrix Rational
minimize eta@(Automaton rules qf) =
  let -- | context
      --    k  : current iteration
      --    mt : transition matrices
      --    vx : vectors from earlier iterations
      --       : substituted transition matrices
      context :: Int
             ->  Map Text (SpMatrix Rational)
             ->  Map Int  [SpVector Rational]
             -> [SpMatrix Rational]
      context 0 mt vx = []
      context k mt vx = concatMap context' $ Map.assocs mt
        where context' (cx,ma) | ar == 0 = []
                               | ar == 1 = if k == 1 then [ma] else []
                               | otherwise =
                                   [slice d vx ma | d <- [0..ar-1], vx <- partial]
                where ar = arity eta cx
                      partial = map (toSV . kronecker' . map toSM) $
                        concat [products $ map (vx!) pt | pt <- partitions (ar-1) (k-1)]
      
      partitions :: Int -> Int -> [[Int]]
      partitions a k = filter ((== k) . sum) $ products $ replicate a [0..k]
      
      -- | prefix
      --    k  : current iteration
      --    mt : transition matrices
      --    vx : vectors from earlier iterations
      --       : updated vector set
      prefix :: Int
             -> Map Text (SpMatrix Rational)
             -> Map Int  [SpVector Rational]
             -> Map Int  [SpVector Rational]
      prefix 0 mt vx = Map.singleton 0 [toSV v | (k,v) <- Map.assocs mt, arity eta k == 0]
      prefix k mt vx = Map.insert k (
          List.nub $ concat
            [compose (vx!?k1) (context k2 mt vx) | [k1,k2] <- partitions 2 k]) vx
        where compose a b = [(toSV v) <# m | [v,m] <- products [map toSM a,b]]
      
      reduce :: Int
             -> Int
             -> Map Text (SpMatrix Rational)
             -> Map Int  [SpVector Rational]
             -> [SpVector Rational]
      reduce n k mt vx | k > n = concat $ Map.elems vx
      reduce n k mt vx =
        case prefix k mt vx of
          vx' | vx' == vx -> concat $ Map.elems vx
              | otherwise -> reduce n (k+1) mt vx'
      
      forward = echelon $ fromRowsL ((reduce (length $ states eta) 0 (matrix eta) Map.empty))
      
  in forward ## (matrix eta ! "s") ## pinverse forward
-}

