module Computor where

import Text.Printf
import Control.Monad.Except
import Control.Monad.Writer

import Computor.Error
import Computor.Parsing
import Computor.Math

type ComputorM a = ExceptT ComputorError (Writer [Message]) a

data Message = ReducedForm ReducedPoly
             | PolynomialDegree Degree
               deriving (Show)

runComputorM :: ComputorM a -> (Either ComputorError a, [Message])
runComputorM = runWriter . runExceptT

zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault _ _ [] [] = []
zipWithDefault op def (x:xs) [] = (x `op` def):zipWithDefault op def xs []
zipWithDefault op def [] (y:ys) = (def `op` y):zipWithDefault op def [] ys
zipWithDefault op def (x:xs) (y:ys) = (x `op` y):zipWithDefault op def xs ys

computor :: String -> ComputorM Solution
computor input = do
  (lhs, rhs) <- withExceptT ParseError . ExceptT . return $ runEquationParser input
  let rp = reduce $ zipWithDefault (-) 0 lhs rhs
  tell [ReducedForm rp]
  let deg = getDegree rp
  tell [PolynomialDegree deg]
  poly <- reducedToPoly rp
  solve poly
