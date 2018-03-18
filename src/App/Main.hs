module App.Main (
  main
  )where

import Protolude hiding (product, evaluate)
import Pre

import qualified Data.Text as Text
import qualified Data.List as List

import qualified Core.Data.Program as Program
import qualified Core.Data.Grammar as Grammar
import qualified Core.Data.Automata as Automata

import qualified Core.Type  as Type
import qualified Core.Parse as Parse
import qualified Core.Run   as Run

import qualified Tasty.Main as Tasty

import Core.Data.Symbol
import Core.Data.Rewrite
import Core.Data.Automata

import qualified Test.Tasty.QuickCheck as QC

import Tasty.Automata

main = graphXlib $ runSymbolic natSquare

