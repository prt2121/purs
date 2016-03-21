module Parser (parens, spaces) where

import Prelude (class Monad, ($), Unit)

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators (between, skipMany1)
import Text.Parsing.Parser.String (string, oneOf)

import Data.String

parens :: forall m a. (Monad m) => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

spaces :: forall m. (Monad m) => ParserT String m Unit
spaces = skipMany1 $ oneOf $ toCharArray ", \n"
