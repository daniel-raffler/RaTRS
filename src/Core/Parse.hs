{-# LANGUAGE RecursiveDo, FlexibleContexts #-}

module Core.Parse (
  grammar,
  runLexer,
  runParser,
  parseFile
  ) where

import Protolude hiding (reduce)
import Pre

import qualified Text.Earley as Earley

import Text.Earley (
  Prod,
  Report (..),
  (<?>),
  satisfy,
  rule,
  token,
  parser,
  fullParses
  )

import qualified Text.PrettyPrint.Leijen.Text as Pretty

import Text.PrettyPrint.Leijen.Text (
  Pretty (..),
  (<+>),
  (<$$>),
  align,
  hang,
  textStrict,
  int,
  comma,
  equals,
  dquote,
  punctuate,
  vsep,
  hsep,
  pretty
  )

import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import Core.Data.Rewrite
import Core.Data.Grammar
import Core.Data.Program

-- | Tokenize the input stream
tokenize = filter (not . Text.all Char.isSpace) . Text.groupBy eq
  where eq u v =
          on (&&) Char.isAlphaNum u v ||
          on (&&) Char.isSpace u v

-- | Remove all comments from the input stream
lexer :: Earley.Grammar r (Prod r Text Char Text)
lexer =
  do let except :: [Char] -> Prod r Text Char Char
         except t = satisfy (flip notElem t)
         
     rec oneline   <- rule $ " " <$ Earley.list "--" <* many (except "\n") <* token '\n'
         multiline <- rule $ " " <$ Earley.list "{-" <* expr' <* Earley.list  "-}"
         
         prefix    <- rule $ (Text.cons '-' . Text.singleton) <$> (token '-' *> except "-}")
         regular   <- rule $ Text.singleton <$>  except "-{"
         
         expr  <- rule $ mconcat <$> many (multiline <|> prefix <|> regular <|> oneline)
         expr' <- rule $ mconcat <$> many (multiline <|> prefix <|> regular)
     
     return (toS <$> expr)

-- | Remove all comments and then tokenize the input stream
runLexer :: Text -> Except Text [Text]
runLexer input = fullParses (parser lexer) input &
  \case
    (h:t, Report p e i) -> return $ tokenize h
    ([],  Report p e i) -> throwError $ pshow $
        textStrict "lexer error near line" <+> int line <> comma <+>
          textStrict "column" <+> int column <$$>
            textStrict "expecting" <+>
              align (Pretty.list $ map textStrict e)
      where (line,column) =
              (length &&& (p -) . List.last) $
                takeWhile (<=p) $ snd $ mapAccumR (
                  \acc ln -> (acc + Text.length ln, acc)
                  ) 0 $
                Text.lines input

-- | Grammar for .trs source files
--   @
--   type := "type" var* name "=" decl ("|" decl)*
--
--   decl  := var | "0" | "[]" | decl-fun | decl-op
--   decl0 := var | decl-fun
--
--   decl-fun := name | name "(" decl-fun ("," decl-fun)* ")"
--   decl-op  := decl0 operator decl0
--   @
--   @
--   rule := (op | fun) "=" term
--
--   term  := var | "0" | "[]" | cons | fun | op | nested
--   term0 := var | "0" | "[]" | cons | fun | nested
--
--   cons   := name | name "(" term ("," term)* ")"
--   fun    := var "(" term ("," term)* ")"
--   op     := term operator term0
--   nested := "(" term ")"
--   @
grammar :: Earley.Grammar r (Prod r Text Text [Either [Rule Text Text] (Rule Text Text)])
grammar =
  do let -- | Match tokens that fullfill the given predicates for the first letter and the
         --   rest of the word
         match :: (Char -> Bool) -> (Char -> Bool) -> Prod r Text Text Text
         match f0 fx = satisfy (test . Text.uncons)
           where test (Just (h,t)) =
                   f0 h &&
                   Text.all fx t &&
                   notElem (Text.cons h t) reserved
                 reserved = ["type","rule","="]
         
         -- | Identifier starting with captial and lowercase letters
         upper = match ((&&) <$> Char.isAlpha <*> Char.isUpper) Char.isAlphaNum
         lower = match ((&&) <$> Char.isAlpha <*> Char.isLower) Char.isAlphaNum
         
         -- | Match tokens that fullfill the given predicate for all letters
         match1 :: (Char -> Bool) -> Prod r Text Text Text
         match1 fx = match fx fx
         
         -- | Valid user-defined operators
         operator = mconcat <$> some (
           match1 $ flip (elem :: Char -> [Char] -> Bool) "+-*/|:=<>#~?!$"
           )
         
         -- | Sequence of words separted by a given token
         separated sep p = (:) <$> p <*> many (sep *> p)
         
     let fType args name =
             map (Rule lhs . List.head . once (rewrite [Rule (Fun name []) lhs]))
           where lhs = Fun name $ map Var args
         fCons name args  = Fun name $ fromMaybe [] args
         fOp lt op rt     = Fun op [lt,rt]
         fZero            = Fun "0"  []
         fNil             = Fun "[]" []
         
     rec pType <- rule $ fType <$>
           ("type" *> many lower) <*> upper <*> (
              "=" *> separated "|" pDecl
              )
           <?> "type T = .."
         
         pDecl  <- rule $ pVar <|> pZero <|> pNil <|> pDeclFun <|> pDeclOp
         pDecl0 <- rule $ pVar <|> pDeclFun
         
         pDeclFun <- rule $ fCons <$>
           upper <*> optional (
             token "(" *> separated "," pDecl0 <* token ")"
             )
           <?> "C(..)"
         
         pDeclOp <- rule $ fOp <$>
           pDecl0 <*> operator <*> pDecl0
           <?> "t₁ ⊕ t₂"
         
         pRule <-
           rule $ Rule <$> (pOp <|> pFun) <*> ("=" *> pTerm)
           <?> "f(x) = .."
         
         pTerm  <- rule $ pVar <|> pLit <|> pCons <|> pFun <|> pOp  <|> pNested
         pTerm0 <- rule $ pVar <|> pLit <|> pCons <|> pFun <|> pNested
         
         pVar <- rule $ Var <$> lower <?> "var"
         
         pLit  <- rule $ pZero <|> pNil
         pZero <- rule $ fZero <$ "0"        <?> "0"
         pNil  <- rule $ fNil  <$ "[" <* "]" <?> "[]"
         
         pCons <- rule $ fCons <$>
           upper <*> optional (
             "(" *> separated "," pTerm <* ")"
             )
           <?> "C(x)"
         
         pOp <- rule $ fOp <$>
           pTerm <*> operator <*> pTerm0
           <?> "t₁ ⊕ t₂"
         
         pFun <- rule $ Fun <$>
           lower <*> (
             "(" *> separated "," pTerm <* ")"
             )
           <?> "f(x)"
         
         pNested <- rule $ (
           "(" *> pTerm <* ")"
           )
     
     return $
       some (
         Left  <$> pType <|>
         Right <$> pRule
         )

-- | Parse input stream and return rewrite rules for type and function definitions
runParser :: [Text] -> Except Text (Grammar, Module)
runParser input = fullParses (parser grammar) input &
  \case
    (h:t, Report p e i) -> build h
    ([],  Report p e i) -> throwError $ pshow $
        textStrict "parser error near" <+>
          dquote <> hsep (map textStrict $ take 5 i) <> dquote <$$>
            textStrict "expecting" <+>
              align (Pretty.list $ map textStrict e)
    where build x =
            do let (left,right) = partitionEithers $  multihead x
               left'  <- merge left
               right' <- merge right
               return (Grammar left', Module right')
          
          -- | Merge multihead definitions
          multihead :: [Either [Rule Text Text] (Rule Text Text)]
                    -> [Either [Rule Text Text] [Rule Text Text]]
          multihead = map joined . List.groupBy cmp
            where cmp (Right n) (Right m) = on (==) (label . left) n m
                  cmp        n         m  = False
          
          -- | Shift right constructor one level down
          joined :: [Either [Rule Text Text] (Rule Text Text)]
                 ->  Either [Rule Text Text] [Rule Text Text]
          joined x = partitionEithers x &
            \case ( v, []) -> Left $ concat v
                  ([],  v) -> Right v
          
          -- | Combine definitions for the same function or type
          merge :: [[Rule Text Text]] -> Except Text [Rule Text Text]
          merge rules =
            do let labels = Set.fromList . map (label . left)
                   validate :: Set Text -> Set Text -> Except Text (Set Text)
                   validate v acc
                     | (not . null) intersect =
                         throwError $ pshow (
                           textStrict "multiple declarations for rule" <+>
                             (hsep $ punctuate comma $ map textStrict $ toList intersect)
                           )
                     | otherwise = return (v <> acc)
                     where intersect = Set.intersection v acc
               foldM validate Set.empty $ map labels rules
               return $ concat rules

-- | Parse a file, panic on error
parseFile :: FilePath -> IO (Grammar, Module)
parseFile p = readFile p >>= \i ->
  runExcept (runParser =<< runLexer i) &
    \case (Left  e) -> panic e
          (Right v) -> return v
