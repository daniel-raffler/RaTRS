{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Provides functions with an infinite set of fresh symbols

module Core.Data.Symbol (
  Symbol,
  SymbolicT,
  Symbolic,
  Symbolical(..),
  -- ** Non-Monadic
  strings,
  integers,
  -- ** Monadic
  variable,
  variables,
  -- ** Run Symbol Monad
  runSymbolicT,
  runSymbolic
  ) where

import Protolude

import qualified Data.Maybe     as Maybe
import qualified Data.List      as List
import qualified Data.Char      as Char
import qualified Data.Text      as Text
import qualified Data.Text.Lazy as Lazy

-- | Symbol type
type Symbol = Int

-- | Provides functions with fresh symbols
newtype SymbolicT m a = SymbolicT { unSymbolic :: StateT [Symbol] m a }
  deriving (
    Functor, Applicative,
    Monad,
    MonadState [Symbol]
    )

type Symbolic = SymbolicT Identity

-- | Map internal symbols to another representation
class Symbolical a where
  encode ::   a -> Int
  decode :: Int -> a

instance Symbolical Text where
  encode s | Text.null s = 0
           | otherwise   = (Char.ord h - Char.ord 'a') + 26*encode t
               where Just (h,t) = Text.uncons s
  decode k | d == 0    = Text.singleton (asChar m)
           | otherwise = Text.cons (asChar m) (decode d)
    where asChar c = Char.chr (Char.ord 'a' + c)
          d = div k 26
          m = mod k 26

instance Symbolical Int where
  encode = identity
  decode = identity

-- | Infinite list of fresh string values
strings :: [Text]
strings = map decode integers

-- | Infinite list of fresh integer values
integers :: [Int]
integers = [1..]

-- | Generate a fresh variable name from the current context
variable :: (Symbolical a, Monad m) => SymbolicT m a
variable = state (first decode . Maybe.fromJust . List.uncons)

-- | Generate k fresh variable names from the current context
variables :: (Symbolical a, Monad m) => Int -> SymbolicT m [a]
variables k = state (first (map decode) . List.splitAt k)

-- | Leave the monad, variable context will be destroyed
runSymbolicT :: Monad m => SymbolicT m a -> m a
runSymbolicT m = flip evalStateT integers $ unSymbolic m

runSymbolic :: Symbolic a -> a
runSymbolic m = runIdentity $ runSymbolicT m
