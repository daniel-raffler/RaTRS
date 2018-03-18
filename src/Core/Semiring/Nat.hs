module Core.Semiring.Nat where

{-
-- | Semiring of natural numbers with regular addition and multiplication

module Core.Semiring.Nat (
  Nat (..)
  ) where

import Protolude
import Base (Show(..))

import Text.PrettyPrint.Leijen.Text

-- | natural numbers
newtype Nat = Nat Int deriving (Eq,Ord)

instance Monoid Nat where
  mappend (Nat n) (Nat m) = Nat (n + m)
  mempty = Nat 0

instance Semiring Nat where
  (Nat n) <.> (Nat m) = Nat (n * m)
  one = Nat 1

instance Pretty Nat where
  pretty (Nat a) = int a

instance Show Nat where
  show (Nat a) = Base.show a
-}
