module Computor.Displaying where

import Text.Printf
import Data.Complex
import Data.List

import Computor.Math
import Computor.Error

displayReducedPoly :: ReducedPoly -> String
displayReducedPoly (RP []) = "0"
displayReducedPoly (RP coeffs) = intercalate " + " (zipWith (printf "%f * X^%u") coeffs [(0 :: Int)..])

displayDegree :: Degree -> String
displayDegree MinusInf = "negative infinity"
displayDegree (Degree n) = show n

displayComplex :: Complex Double -> String
displayComplex (a :+ 0) = show a
displayComplex (0 :+ b) = printf "%fi" b
displayComplex (a :+ b) = printf "%f + %fi" a b

displaySolution :: Solution -> String
displaySolution (TwoRoots (x1 :+ 0) (x2 :+ 0)) = printf "Discriminant is strictly positive, the two solutions are:\n%f\n%f" x1 x2
displaySolution (TwoRoots x1 x2) = printf "Discriminant is strictly negative, the two solutions are:\n%s\n%s" (displayComplex x1) (displayComplex x2)
displaySolution (DoubleRoot x) = printf "Discriminant is null, the double solution is: %f" x
displaySolution (OneRoot x) = printf "The solution is: %f" x
displaySolution AllReals = "This equation holds true whatever X is"

displayError :: ComputorError -> String
displayError WrongNumberOfArgument = "Only one argument accepted"
displayError (ParseError err) = show err
displayError DegreeMoreThan2 = "This program can only solve polynoms of degree 2 or less"
displayError NoSolution = "This equation holds false whatever X is"
