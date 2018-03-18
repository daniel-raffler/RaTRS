module Core.Data.Rules (
  scc
  ) where

import Protolude
import Pre

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.Graph.Inductive as Graph (mkUGraph, scc)

import Data.Graph.Inductive (Node,Edge,UGr)
import Data.Graph.Inductive.PatriciaTree

import Data.Set ((\\))

import Core.Data.Rewrite
import Core.Data.Symbol
      
-- | Create dependency graph for rule definitions and calculate SCCs 
scc :: [Rule Text Text] -> [[Rule Text Text]]
scc rules =
  let -- | Generate edges for all references occuring in the rules
      edges :: [Edge]
      edges = concatMap edge rules
        where edge (Rule left right) =
                map (\k -> (nodes ! label left, nodes!k)) $
                Set.toList $ Set.intersection defined (
                  symbols right \\ symbols left
                )
              symbols = (<>) <$> funs <*> vars
      
      -- | Generate a node for every symbol
      nodes :: Map Text Symbol
      nodes = runSymbolic $
        map (Map.fromList . zip (toList defined)) $
          variables $
            length defined
      
      -- | All symbols that are defined by the rules
      defined :: Set Text
      defined = Set.fromList $ map (label . left) rules
      
      graph =
        map (\cc -> filter (\(Rule left right) -> elem (nodes ! label left) cc) rules) $
        (Graph.scc :: UGr -> [[Node]]) $
        Graph.mkUGraph
          (Map.elems nodes) edges
      
  in graph
