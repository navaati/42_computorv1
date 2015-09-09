module Computor.Parsing where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Control.Applicative hiding ((<|>), many)
import Control.Monad.Except

import Computor.Error
import Computor.Math

type EqParser a = Parsec String Int a -- The state is the degree of the current term

termParser :: EqParser Double
termParser = do
  coeff <- sign <*> (either (fromIntegral :: Int -> Double) id <$> decimalFloat)
  string " * X^"
  string . show =<< getState
  modifyState (+ 1)
  return coeff

opParser :: EqParser (Double -> Double)
opParser = do
  char ' '
  op <- id <$ char '+'
        <|> negate <$ char '-'
  char ' '
  return op

polyParser :: EqParser Coefficients
polyParser = do
  putState 0
  first_term <- termParser
  other_terms <- many (try $ opParser <*> termParser)
  return (first_term:other_terms)

equationParser :: EqParser Equation
equationParser = do
  lhs <- polyParser
  string " = "
  rhs <- polyParser
  eof
  return (lhs, rhs)

runEquationParser :: String -> Either ParseError Equation
runEquationParser input = runParser equationParser 0 input input
