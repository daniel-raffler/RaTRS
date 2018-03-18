module Tasty.Type (
  readAll,
  validate,
  dyadeSign,
  listSign
  ) where

import Protolude hiding (Type)
import Pre

import qualified Data.Map as Map

import System.Directory (listDirectory)

import Core.Data.Types
import Core.Data.Rewrite
import Core.Data.Symbol
import Core.Data.Grammar as Grammar
import Core.Data.Program as Program

import Core.Parse as Parse
import Core.Type  as Type

-- | Read all file from the directory and return a list of path/contents mappings
readAll :: FilePath -> IO (Map Text Text)
readAll path =
  map Map.fromList $
  mapM (\file -> 
          do con <- readFile (path ++ "/" ++ file)
             return (toS file, con)
       ) =<<
  listDirectory path

-- | Type the example and compare the result to the signatures provided
validate :: Map Text Type -> Text -> Bool
validate mp src =
  runExcept (Parse.runParser =<< runLexer src) &
  \case Left  m -> False
        Right v ->
          (Grammar.signatures *** Program.modules) v & (
            compare mp . uncurry Type.typed
            )
  where compare n (Program _ m) =
          Map.keys n == Map.keys m && (
            on zip Map.elems n m & unifyS & runExcept &
              \case Left  m -> False
                    Right v -> null $ mconcat $ map funs $ Map.elems v
            )

dyadeSign :: Map Text Type
dyadeSign = Map.fromList [
  ("dyade",
   prod [Fun (Cons "List") [Fun (Cons "Nat") []], Fun (Cons "List") [Fun (Cons "Nat") []]]
    `arrow` Fun (Cons "List") [Fun (Cons "List") [Fun (Cons "Nat") []]]),
  ("mult" ,
   prod [Fun (Cons "Nat") [], Fun (Cons "List") [Fun (Cons "Nat") []]]
    `arrow` Fun (Cons "List") [Fun (Cons "Nat") []]),
  ("*", prod [Fun (Cons "Nat") [], Fun (Cons "Nat") []] `arrow` Fun (Cons "Nat") []),
  ("+", prod [Fun (Cons "Nat") [], Fun (Cons "Nat") []] `arrow` Fun (Cons "Nat") [])
  ]

listSign :: Map Text Type
listSign = Map.fromList [
  ("copy"  , prod [Fun (Cons "List") [var "α"]] `arrow` Fun (Cons "List") [var "α"]),
  ("append",
    prod [Fun (Cons "List") [var "α"], Fun (Cons "List") [var "α"]]
      `arrow` Fun (Cons "List") [var "α"])
  ] where var :: Text -> Type
          var = Var . encode
