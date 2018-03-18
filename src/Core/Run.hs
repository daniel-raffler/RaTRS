module Core.Run (
  eval
  ) where

import Protolude hiding (evaluate)
import Pre

import qualified Data.Map  as Map
import qualified Data.List as List

import Text.PrettyPrint.Leijen.Text as Pretty (
  Pretty (..),
  (<+>),
  (<$$>),
  textStrict,
  colon,
  pretty
  )

import Core.Data.Rewrite hiding (one)
import Core.Data.Grammar
import Core.Data.Program
import Core.Data.Symbol
import Core.Semiring.Nat

type Value = Term Text Text

-- | Evaluate a term, return its value and the resource costs
eval :: (Grammar, Program) -> Term Text Text -> (Int, Value)
eval (gr, Program modules types) =
  let -- | Collect all rewrite rules for a given function symbol
      heads :: Text -> [Rule Text Text]
      heads lab =
        [rule | Module rules <- modules, rule@(Rule lt rt) <- rules, label lt == lab]
      
      -- | Pick the matching rule
      match :: Subst Text Text
            -> [Rule Text Text]
            -> Term Text Text
            -> (Term Text Text, Subst Text Text)
      match st rules p =
        let unified rule =
              case runExcept $ unify (left rule) $ substitute st p of
                Left  v -> Nothing
                Right v -> Just (right rule, v)
            build =
              case catMaybes $ map unified rules of
                [rw]   -> rw
                others -> error $ pshow (
                  textStrict "No matching rule found while evaluating the program" <>
                    colon <$$> pretty p
                  )
        in build
      
      -- | Evaluate an expression within a given context of local variables
      eval' :: Subst Text Text -> Term Text Text -> Symbolic (Int, Value)
      eval' st (Var a) = return (0, substitute st $ Var a)
      eval' st (Fun lab sub)
        | all isVar sub && elem lab (Map.keys $ signatures gr) =
            return (
              0, Fun lab $ map (substitute st) sub
              )
        | all isVar sub =
            match st (heads lab) (Fun lab sub) &
              \(rt, s0) ->
                do (c,v) <- eval' (Map.union s0 st) rt
                   return (1+c, v)
        | otherwise =
            map List.unzip (mapM (eval' st) sub) >>=
              \(costs,vals) ->
                do fx    <- variables $ length sub
                   (c,v) <- eval' (
                     Map.union st $ Map.fromList $ zip fx vals
                     ) (Fun lab $ map Var fx)
                   return (c + sum costs, v)
      
  in runSymbolic . eval' Map.empty
