module Core.Data.Program (
  Program (..),
  Module  (..),
  modules,
  annotate
  ) where

import Protolude hiding (Type)
import Pre

import Text.PrettyPrint.Leijen.Text as Pretty (
  Pretty (..),
  (<+>),
  (<$$>),
  textStrict,
  hang,
  colon,
  equals,
  list,
  vsep,
  pretty
  )

import qualified Data.Map as Map
import qualified Data.Set as Set

import Core.Data.Types
import Core.Data.Rules
import Core.Data.Rewrite
import Core.Data.Symbol

-- | Programs consist of several modules and an optional map of type signatures
data Program = Program [Module] (Map Text Type)
  -- ^ [@modules@]
  --   [@types@]
  deriving (Show)

-- | Modules are sets of mutually recursive rewrite rules
newtype Module = Module [Rule Text Text] deriving (Show)

-- | Calculate SCCs of the callgraph and decompose the program into modules
modules :: Module -> Program
modules (Module rules) = Program (map Module $ scc rules) Map.empty

-- | Annotate program with fresh type variables
annotate :: Program -> Symbolic Program
annotate (Program modules types) =
  map (Program modules) $
  map (Map.fromList . zip (toList occurs)) $
    variables $
      length occurs
  where occurs = Set.fromList
          [label lt | Module rules <- modules, Rule lt rt <- rules]
