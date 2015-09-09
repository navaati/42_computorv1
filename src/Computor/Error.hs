module Computor.Error (
  module Computor.Error,
  ) where

import Text.Parsec (ParseError)
import Text.Printf

data ComputorError = ParseError ParseError
                   | DegreeMoreThan2
                   | NoSolution
                   deriving (Show, Eq)

displayError :: ComputorError -> String
displayError (ParseError err) = show err
displayError DegreeMoreThan2 = "This program can only solve polynoms of degree 2 or less"
displayError NoSolution = "This equation holds false whatever X is"
