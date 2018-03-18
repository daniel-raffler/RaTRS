module Tasty.Automata (
  -- * Evaluation
  count,
  square,
  cube,
  binomial,
  evaluate,
  -- * Automata
  setFinal,
  nat,
  tree,
  redNat,
  redTree,
  natSquare,
  natMinSquare,
  natCube,
  treeSquare,
  treeMinSquare,
  -- * Grammars
  natGrammar,
  treeGrammar,
  redNatGrammar,
  redTreeGrammar
  ) where

import Protolude hiding (evaluate)

import Test.Tasty
import Test.Tasty.QuickCheck hiding (generate)

import Data.Bifoldable
import Data.List ((!!))

import qualified Data.Map as Map

import Core.Data.Symbol
import Core.Data.Rewrite
import Core.Data.Grammar
import Core.Data.Automata
import Core.Data.Types

import Core.Semiring.Nat

-- | Count number of nodes with a matching label
count :: Text -> Term Text Text -> Int
count lab term =
  length $ bifoldMap (const mempty) (\k -> if k == lab then [[]] else mempty) term

-- | Square of a linear function
square :: Text -> Term Text Text -> Int
square lab term = count lab term & \k -> (k*k)

-- | Cube of a linear function
cube :: Text -> Term Text Text -> Int
cube lab term = count lab term & \k -> k*k*k

-- | Binomial coefficients
binomial :: Int -> Text -> Term Text Text -> Int
binomial k lab term = count lab term & \n -> binomial' n k
  where binomial' n 0 = 1
        binomial' 0 k = 0
        binomial' n k = binomial' (n-1) (k-1) * n `div` k

-- | Compare costs calculated by resource automata with actual evaluation costs
evaluate :: (Grammar, Text, Text)
         -> Symbolic Automaton
         -> (Text -> Term Text Text -> Int)
         -> Property
evaluate (gr,ax,c) wta fx =
  forAll (generate gr $ Fun ax []) $
    \value ->
      (\k -> round k) (eval (runSymbolic wta) value)
      ===
      fx c value

-- | Natural numbers: 0, s(0), s(s(0)) ..
natGrammar :: Grammar
natGrammar =
  Grammar [
    Rule (Fun "Nat" []) (Fun "s" [Fun "Nat" []]),
    Rule (Fun "Nat" []) (Fun "0" [])
    ]
    
-- | Binary trees: Leaf, Node a Leaf Leaf, Node a (Node a Leaf Leaf) Leaf ..  
treeGrammar :: Grammar
treeGrammar =
  Grammar [
    Rule (Fun "Tree" []) (Fun "Node" [Fun "Tree" [], Fun "Tree" []]),
    Rule (Fun "Tree" []) (Fun "Leaf" [])
    ]

-- | Red-Black lists
redNatGrammar :: Grammar
redNatGrammar =
  Grammar [
    Rule (Fun "Nat" []) (Fun "Red"   [Fun "Nat" []]),
    Rule (Fun "Nat" []) (Fun "Black" [Fun "Nat" []]),
    Rule (Fun "Nat" []) (Fun "0"     [])
    ]

-- | Red-Black trees
redTreeGrammar :: Grammar
redTreeGrammar =
  Grammar [
    Rule (Fun "Tree" []) (Fun "Red"   [Fun "Tree" [], Fun "Tree" []]),
    Rule (Fun "Tree" []) (Fun "Black" [Fun "Tree" [], Fun "Tree" []]),
    Rule (Fun "Tree" []) (Fun "Leaf"  [])
    ]

-- | Change the final state of an automaton
setFinal :: Int -> Symbolic Automaton -> Symbolic Automaton
setFinal k = map setFinal'
  where setFinal' eta@(Automaton rules qf) =
          Automaton rules (states eta !! k)

nat :: Symbolic Automaton
nat =
  do q0 <- variable
     q1 <- variable
     q2 <- variable
     q3 <- variable
     return $
       Automaton (
         Map.fromList $
           zip [
             Transition [  ] "0" q0,
             Transition [q0] "s" q1,
             Transition [q0] "s" q0,
             Transition [q1] "s" q1,
             Transition [q1] "s" q2,
             Transition [q2] "s" q2,
             Transition [q2] "s" q3,
             Transition [q3] "s" q3
             ] $ repeat 1.0
         ) q1

tree :: Symbolic Automaton
tree =
  do q0 <- variable
     q1 <- variable
     q2 <- variable
     q3 <- variable
     return $
       Automaton (
         Map.fromList $
           zip [
             Transition [     ] "Leaf" q0,
             Transition [q0,q0] "Node" q0,
             Transition [q0,q0] "Node" q1,
             
             Transition [q0,q1] "Node" q1,
             Transition [q1,q0] "Node" q1,
             Transition [q0,q1] "Node" q2,
             Transition [q1,q0] "Node" q2,
             
             Transition [q0,q2] "Node" q2,
             Transition [q1,q1] "Node" q2,
             Transition [q2,q0] "Node" q2,
             Transition [q0,q2] "Node" q3,
             Transition [q1,q1] "Node" q3,
             Transition [q2,q0] "Node" q3,
             
             Transition [q3,q0] "Node" q3,
             Transition [q2,q1] "Node" q3,
             Transition [q1,q2] "Node" q3,
             Transition [q0,q3] "Node" q3
             ] $ repeat  1.0
         ) q1

redNat :: Symbolic Automaton
redNat =
  do q0 <- variable
     q1 <- variable
     q2 <- variable
     return $
       Automaton (
         Map.fromList $
           zip [
             Transition [  ] "0"     q0,
             Transition [q0] "Red"   q0,
             Transition [q0] "Black" q0,        
             Transition [q0] "Red"   q1,
             
             Transition [q1] "Black" q1,
             Transition [q1] "Red"   q1,
             Transition [q1] "Black" q2,
             
             Transition [q2] "Black" q2,
             Transition [q2] "Red"   q2
           ] $ repeat  1.0
         ) q1

redTree :: Symbolic Automaton
redTree =
  do q0 <- variable
     q1 <- variable
     q2 <- variable
     return $
       Automaton (
         Map.fromList $
           zip [
             Transition [     ] "Leaf"  q0,
             Transition [q0,q0] "Red"   q0,
             Transition [q0,q0] "Red"   q1,
             Transition [q0,q0] "Black" q0,
             
             Transition [q0,q1] "Red"   q1,
             Transition [q1,q0] "Red"   q1,
             Transition [q0,q1] "Black" q1,
             Transition [q0,q1] "Black" q2,
             Transition [q1,q0] "Black" q1,
             Transition [q1,q0] "Black" q2,

             Transition [q2,q0] "Black" q2,
             Transition [q1,q1] "Black" q2,
             Transition [q0,q2] "Black" q2,
             Transition [q2,q0] "Red"   q2,
             Transition [q1,q1] "Red"   q2,
             Transition [q0,q2] "Red"   q2      
           ] $ repeat  1.0
         ) q1

natSquare :: Symbolic Automaton
natSquare =
  do n <- nat2
     m <- nat2
     hadamard n m

nat2 :: Symbolic Automaton
nat2 =
  do q0 <- variable
     q1 <- variable
     return $
       Automaton (
         Map.fromList $
           zip [
             Transition [  ] "0" q0,
             Transition [q0] "s" q1,
             Transition [q0] "s" q0,
             Transition [q1] "s" q1
             ] $ repeat 1.0
         ) q1

natCube :: Symbolic Automaton
natCube =
  do n <- natSquare
     m <- nat2
     hadamard n m

treeSquare :: Symbolic Automaton
treeSquare =
  do n <- tree2
     m <- tree2
     hadamard n m

tree2 =
  do q0 <- variable
     q1 <- variable
     return $
       Automaton (
         Map.fromList $
           zip [
             Transition [     ] "Leaf" q0,
             Transition [q0,q0] "Node" q0,
             Transition [q0,q0] "Node" q1,
             
             Transition [q0,q1] "Node" q1,
             Transition [q1,q0] "Node" q1
             ] $ repeat 1.0
         ) q1

natMinSquare =
  do n <- natMinSquare'
     m <- setFinal 2 natMinSquare'
     add n m

natMinSquare' :: Symbolic Automaton
natMinSquare' =
  do q0 <- variable
     q1 <- variable
     q2 <- variable
     return $
       Automaton (
         Map.fromList $
           [(Transition [  ] "0" q0, 1),
            (Transition [q0] "s" q0, 1),
            (Transition [q0] "s" q1, 1),
            (Transition [q1] "s" q1, 1),
            (Transition [q1] "s" q2, 2),
            (Transition [q2] "s" q2, 1)
           ]
         ) q1

treeMinSquare :: Symbolic Automaton
treeMinSquare =
  do n <- treeMinSquare'
     m <- setFinal 2 treeMinSquare'
     add n m

treeMinSquare' :: Symbolic Automaton
treeMinSquare' = 
  do q0 <- variable
     q1 <- variable
     q2 <- variable
     return $
       Automaton (
         Map.fromList $
           [(Transition [     ] "Leaf" q0, 1),
            (Transition [q0,q0] "Node" q0, 1),
            (Transition [q0,q0] "Node" q1, 1),
            (Transition [q1,q0] "Node" q1, 1),
            (Transition [q1,q0] "Node" q2, 2),
            (Transition [q2,q0] "Node" q2, 1),
            (Transition [q0,q1] "Node" q1, 1),
            (Transition [q0,q1] "Node" q2, 2),
            (Transition [q1,q1] "Node" q2, 2),
            (Transition [q0,q2] "Node" q2, 1)
           ]
         ) q1

