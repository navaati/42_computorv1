module Computor.Tests.Math where

import Test.QuickCheck
import Data.Complex
import Control.Monad

import Computor.Math
import Computor.Error

prop_real_sqrt_prec :: Double -> Property
prop_real_sqrt_prec x = x > 0 ==> (myRealSqrt x)^2 - x < 0.00000000001

isReal :: Complex Double -> Bool
isReal = (== 0) . imagPart

isImag :: Complex Double -> Bool
isImag = (== 0) . realPart

prop_sqared_complex_sqrt_is_real :: NonZero Double -> Bool
prop_sqared_complex_sqrt_is_real (NonZero x) = isReal ((mySqrt x)^2)

prop_sqared_complex_sqrt_signum :: NonZero Double -> Property
prop_sqared_complex_sqrt_signum (NonZero x) = signum x === (realPart $ signum $ (mySqrt x)^2)

prop_complex_sqrt_either_real_or_imag :: NonZero Double -> Bool
prop_complex_sqrt_either_real_or_imag (NonZero x) = isReal (mySqrt x) /= isImag (mySqrt x)

prop_reduce_last_coeff_is_not_null :: NonEmptyList Double -> Bool
prop_reduce_last_coeff_is_not_null (NonEmpty cs) = let RP l = reduce cs in last l /= 0

newtype CoefficientsQC = CoefficientsQC Coefficients
                       deriving (Show, Eq)

sometimesZero :: (Num a, Arbitrary a) => Gen a
sometimesZero = frequency [
  (1, return 0),
  (5, arbitrary)
  ]

instance Arbitrary CoefficientsQC where
  arbitrary = CoefficientsQC <$> listOf sometimesZero

prop_reduce_idem :: CoefficientsQC -> Property
prop_reduce_idem (CoefficientsQC cs) = reduce cs === (reduce . unRP . reduce) cs
  where unRP (RP rp) = rp

prop_reduce_reducedPoly :: CoefficientsQC -> Property
prop_reduce_reducedPoly (CoefficientsQC cs) = not (null rp) ==> last rp /= 0
  where (RP rp) = reduce cs

instance Arbitrary ReducedPoly where
  arbitrary = do
    CoefficientsQC cs <- arbitrary
    NonZero last <- arbitrary
    return $ RP (replaceLast last cs)
    where replaceLast :: a -> [a] -> [a]
          replaceLast _ [] = []
          replaceLast x [_] = [x]
          replaceLast x (a:as) = a:replaceLast x as

prop_arbitrary_reducedPoly :: ReducedPoly -> Property
prop_arbitrary_reducedPoly (RP cs) = not (null cs) ==> last cs /= 0

prop_reducedToPoly_degree_error :: ReducedPoly -> Property
prop_reducedToPoly_degree_error rp@(RP cs) = length cs > 3 ==> reducedToPoly rp === Left DegreeMoreThan2
