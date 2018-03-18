module Tasty.Main (
  main
  ) where

import Protolude hiding (evaluate)
import Pre

import Data.Map ((!))

import Test.Tasty
import Test.Tasty.QuickCheck hiding (generate)

import Tasty.Automata
import Tasty.Type

import Core.Data.Automata

main :: IO ()
main = defaultMain =<<
  do examples <- readAll "example"
     let grType =
           testGroup "Type" [
             testProperty "dyade" $ validate dyadeSign (examples ! "dyade.trs"),
             testProperty "list"  $ validate listSign  (examples ! "list.trs")
             ]
         grAutomaton =
           testGroup "Automata" [
             testGroup "Linear" [
                 testProperty "Nat" $
                   evaluate (natGrammar, "Nat", "s") nat count,
                 testProperty "Red Black Nat" $
                   evaluate (redNatGrammar, "Nat", "Red") redNat count,
                 testProperty "Tree" $
                   evaluate (treeGrammar, "Tree", "Node") tree  count,
                 testProperty "Red Black Tree" $
                   evaluate (redTreeGrammar, "Tree", "Red") redTree count
                 ],
             testGroup "Binomial" [
                 testProperty "Nat (n,2)" $
                   evaluate (natGrammar, "Nat", "s") (setFinal 2 nat) $ binomial 2,
                 testProperty "Nat (n,3)" $
                   evaluate (natGrammar, "Nat", "s") (setFinal 3 nat) $ binomial 3
                 ],
             testGroup "Polynomial" [
                 testProperty "Nat  (x²)" $
                   evaluate (natGrammar, "Nat", "s") natSquare square,
                 testProperty "Nat  (x³)" $
                   evaluate (natGrammar, "Nat", "s") natCube cube,
                 testProperty "Tree (x²)" $
                   evaluate (treeGrammar, "Tree", "Node") treeSquare square
                 ],
             testGroup "Minimized" [
                 testProperty "Nat  (x²)" $
                   evaluate (natGrammar, "Nat", "s") natMinSquare square,
                 testProperty "Tree (x²)" $
                   evaluate (treeGrammar, "Tree", "Node") treeMinSquare square
                 ]
             ]
         grTasty = testGroup "Tasty" [grType, grAutomaton]
     return grTasty

