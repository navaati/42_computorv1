module Computor where

import Text.Printf
import Control.Monad.Except
import Control.Monad.Writer

import Computor.Error
import Computor.Parsing
import Computor.Math
import Computor.Displaying

type ComputorM a = ExceptT ComputorError (Writer [String]) a

runComputorM :: ComputorM a -> (Either ComputorError a, [String])
runComputorM = runWriter . runExceptT

zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault _ _ [] [] = []
zipWithDefault op def (x:xs) [] = (x `op` def):zipWithDefault op def xs []
zipWithDefault op def [] (y:ys) = (def `op` y):zipWithDefault op def [] ys
zipWithDefault op def (x:xs) (y:ys) = (x `op` y):zipWithDefault op def xs ys

computor :: [String] -> ComputorM Solution
computor [arg] = do
  (lhs, rhs) <- withExceptT ParseError . ExceptT . return $ runEquationParser arg
  let rp = reduce $ zipWithDefault (-) 0 lhs rhs
  tell [printf "Reduced form: %s = 0" $ displayReducedPoly rp]
  let deg = getDegree rp
  tell [printf "Polynomial degree: %s" $ displayDegree deg]
  poly <- reducedToPoly rp
  solve poly
computor _ = throwError WrongNumberOfArgument
