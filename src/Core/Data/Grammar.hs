module Core.Data.Grammar (
  -- * Grammar
  Grammar (..),
  generate,
  signatures,
  ordered
  ) where

import Protolude hiding (Type, All, reduce, first)
import Pre

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive (
  Node,
  Edge,
  UGr,
  mkUGraph,
  topsort
  )

import qualified Data.Map  as Map
import qualified Data.List as List
import qualified Data.Set  as Set

import Text.PrettyPrint.Leijen.Text (
  Pretty (..),
  (<+>),
  (<$$>),
  textStrict,
  equals,
  colon,
  comma,
  tupled,
  punctuate,
  vsep,
  hsep,
  hang,
  pretty
  )

import Test.Tasty.QuickCheck (
  Gen,
  sized,
  scale,
  oneof,
  elements,
  frequency
  )

import Core.Data.Types
import Core.Data.Rewrite
import Core.Data.Rules
import Core.Data.Symbol

-- | Regular tree grammar
newtype Grammar = Grammar [Rule Text Text]

-- | Generate words from a grammar
generate :: Grammar -> Term Text Text -> Gen (Term Text Text)
generate (Grammar prods) sym =
  let (node, leaf) = List.partition (\(Rule a b) -> elem (label a) $ funs b) prods
      
      expand 0 t = elements $ reduce (rewrite leaf) t
      expand k t = expand (k `div` 2) =<< elements (once (rewrite' (node <> leaf)) t)
      
  in sized $ flip expand sym

-- | Derive constructor signatures for all productions in a grammar
signatures :: Grammar -> Map Text Scheme
signatures (Grammar prods) =
  Map.fromList $
    [(lab, All (vars left) $ arrow right left) | (lab, left, right) <- map toType prods]
  where toType (Rule left (Fun lab right)) = (lab, conv left, prod $ map conv right)
          where conv = bimap encode Cons
  
-- | Calculate dependencies between type definitions and return a topological ordering
ordered :: Grammar -> [Text]
ordered (Grammar prods) =
  case scc prods of
    comp | all (not . cycle) comp ->
             map (label . left) $ concat comp
         | otherwise ->
             panic $ pshow $
               textStrict "mutually recursive types are not supported" <$$>
                 hsep (punctuate comma $
                         map (textStrict . label . left) $ List.head $ filter cycle comp
                      )
  where cycle c = length c > 1
