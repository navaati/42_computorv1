{-# LANGUAGE FlexibleContexts #-}

module Computor.Math where

import Data.Maybe
import Data.Complex
import Data.List
import Control.Monad.Except

import Computor.Error

-- In Coefficients the head of the list is the zeroth degree coefficient
type Coefficients = [Double]

type Equation = (Coefficients, Coefficients)

-- In ReducedPoly the last coefficient is guaranteed to be non-zero
newtype ReducedPoly = RP Coefficients
                    deriving (Show, Eq)

reduce :: Coefficients -> ReducedPoly
reduce = RP . foldr trimPolyHelper []
  where trimPolyHelper 0 [] = []
        trimPolyHelper n l = n:l

data Poly = Poly {
  zerothDegree :: Double,
  firstDegree :: Double,
  secondDegree :: Double
  }
          deriving (Show, Eq)

reducedToPoly :: MonadError ComputorError m => ReducedPoly -> m Poly
reducedToPoly (RP [z, o, t]) = return $ Poly z o t
reducedToPoly (RP [z, o]) = return $ Poly z o 0
reducedToPoly (RP [z]) = return $ Poly z 0 0
reducedToPoly (RP []) = return $ Poly 0 0 0
reducedToPoly _ = throwError DegreeMoreThan2

data Degree = MinusInf | Degree Int
            deriving (Show, Eq)

getDegree :: ReducedPoly -> Degree
getDegree (RP []) = MinusInf
getDegree (RP (_:l)) = Degree $ length l


myRealSqrt :: Double -> Double
myRealSqrt a = fst . fromJust $ find (\(x_k, x_kp1) -> absError x_kp1 >= absError x_k) $ zip newtonSerie (tail newtonSerie)
  where absError x = abs (x * x - a)
        newtonSerie = iterate newtonNext seed
        newtonNext x_k = (x_k + a / x_k) / 2
        seed = a -- This could be better, for example by finding the nearest integer

mySqrt :: Double -> Complex Double
mySqrt x | x < 0 = (0 :+ myRealSqrt (-x))
         | otherwise = myRealSqrt x :+ 0

data Solution = TwoRoots (Complex Double) (Complex Double) | DoubleRoot Double | OneRoot Double | AllReals
              deriving (Show, Eq)

solve :: MonadError ComputorError m => Poly -> m Solution
solve (Poly 0 0 0) = return AllReals -- Equation 0 + 0X + 0X^2 = 0 <=> 0 = 0 holds for every X
solve (Poly c 0 0) = throwError NoSolution -- Equation c + 0X + 0X^2 = 0 <=> c = 0 where c /= 0 is always false, thus holds for no X
solve (Poly c b 0) = return $ OneRoot (-c / b) -- Equation c + bX + 0X^2 = 0 <=> bX + c = 0 <=> bX = -c <=> X = -c/b, wich always exist since b in not null
solve (Poly c b a) = case b^2 - 4 * a * c of -- Classical complex solution of 2nd degree equation
                      0 -> return $ DoubleRoot (-b / (2 * a))
                      discr -> return $ TwoRoots
                               ((-b' + sqrt_discr) / (2 * a'))
                               ((-b' - sqrt_discr) / (2 * a'))
                        where a' = a :+ 0
                              b' = b :+ 0
                              sqrt_discr = mySqrt discr
