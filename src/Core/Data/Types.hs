{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- | Standard types, currently a light wrapper on terms

module Core.Data.Types (
  Const (..),
  Type,
  Scheme(..),
  arrow,
  prod
  ) where

import Protolude hiding (Type, All, Sum, Const)

import Text.PrettyPrint.Leijen.Text (
  Pretty,
  (<+>),
  textStrict,
  dot,
  comma,
  hsep,
  parens,
  tupled,
  punctuate,
  pretty
  )

import Core.Data.Symbol
import Core.Data.Rewrite

-- | Basic type constructors
data Const
  = Cons Text
  | Prod
  | Sum
  | Arrow
  deriving (Ord,Eq,Show)

-- | Monomorpic types
type Type = Term Symbol Const

instance Pretty Const where
  pretty (Cons a) = textStrict a
  pretty  Prod    = textStrict "*"
  pretty  Sum     = textStrict "+"
  pretty  Arrow   = textStrict "→"

-- | Polymorphic types
data Scheme = All (Set Symbol) Type deriving (Show)

instance Pretty Scheme where
  pretty (All fv sign) =
    textStrict "forall" <+> hsep (map (textStrict . decode) $ toList fv) <+> dot <+>
      pretty sign

-- | Function type
arrow :: Type -> Type -> Type
arrow a b = Fun Arrow [a,b]

-- | Product type
prod :: [Type] -> Type
prod = Fun Prod
