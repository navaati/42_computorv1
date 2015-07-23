module Computor.Error (
  module Computor.Error,
  ) where

import Text.Parsec (ParseError)

data ComputorError = WrongNumberOfArgument
                   | ParseError ParseError
                   | DegreeMoreThan2
                   | NoSolution
                   deriving (Show, Eq)
