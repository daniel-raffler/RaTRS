module Pre (
  module X,
  (!?),
  pshow,
  products
  )
  where

import Protolude

import qualified Data.Map as Map

import Data.Map      as X ((!))
import Control.Arrow as X ((&&&), (***))

import Text.PrettyPrint.Leijen.Text (
  Pretty,
  displayTStrict,
  renderPretty,
  pretty
  )

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = pretty . Map.assocs

-- | Pretty print a value
pshow :: Pretty a => a -> Text
pshow = displayTStrict . renderPretty 0.8 80 . pretty

-- | Access element in a map, try to return default if not available
(!?) :: (Ord k, Monoid v) => Map k v -> k -> v
m !? k = Map.findWithDefault mempty k m

-- | Cartesian product on lists
products :: [[a]] -> [[a]]
products  []   = [[]]
products (h:t) = [h':t' | h' <- h, t' <- products t]
