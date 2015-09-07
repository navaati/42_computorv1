module Computor.Error (
  module Computor.Error,
  ) where

import Text.Parsec (ParseError)

data ComputorError = ParseError ParseError
                   | DegreeMoreThan2
                   | NoSolution
                   deriving (Show, Eq)
