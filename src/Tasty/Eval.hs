module Tasty.Eval where

{-
listData :: Text -> Term,
listData m = foldr toS m
main = putStrLn . pshow . flip Eval.evaluate (Fun "+" [Fun "S" [Fun "S" [Fun "0" []]], Fun "S" []]) =<< map (identity *** Program.modules)
       (Parser.parseFile "example/dyade.trs")
-}
