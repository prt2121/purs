module Test.Main where

import Prelude (class Eq, class Show, Unit, ($), bind, show, unit, (++), (==), (<>))

import Test.Assert (ASSERT, assert', assert)

import Text.Parsing.Parser (Parser, ParseError(ParseError), runParser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Pos

import Data.Maybe (Maybe(Just))
import Data.Either (Either(Left, Right))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Parser (parens, spaces)

type TestM = forall eff . Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit

parseTest :: forall s a eff. (Show a, Eq a) => s -> a -> Parser s a -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseTest input expected p = case runParser input p of
  Right actual -> assert (expected == actual)
  Left err -> assert' ("error: " ++ show err) false

parseErrorTestPosition :: forall s a eff. (Show a) => Parser s a -> s -> Position -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseErrorTestPosition p input expected = case runParser input p of
  Right _ -> assert' "error: ParseError expected!" false
  Left (ParseError { position: pos }) -> assert' ("expected: " <> show expected <> ", pos: " <> show pos) (expected == pos)

mkPos :: Int -> Position
mkPos n = mkPos' n 1

mkPos' :: Int -> Int -> Position
mkPos' column line = Position { column: column, line: line }

main :: TestM
main = do
  parseTest "(xy)" (Just "y") $ parens do
    string "x"
    optionMaybe $ string "y"

  parseTest " test" (unit) $ spaces

  parseErrorTestPosition spaces "test" $ mkPos 2
