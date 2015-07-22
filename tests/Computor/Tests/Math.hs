module Computor.Tests.Math where

import Test.QuickCheck
import Data.Complex

import Computor.Math

prop_real_sqrt_prec :: Double -> Property
prop_real_sqrt_prec x = x > 0 ==> (myRealSqrt x)^2 - x < 0.00000000001

isReal :: Complex Double -> Bool
isReal = (== 0) . imagPart

isImag :: Complex Double -> Bool
isImag = (== 0) . realPart

prop_sqared_complex_sqrt_is_real :: Double -> Property
prop_sqared_complex_sqrt_is_real x = x /= 0 ==> isReal ((mySqrt x)^2)

prop_sqared_complex_sqrt_signum :: Double -> Property
prop_sqared_complex_sqrt_signum x = x /= 0 ==> signum x == (realPart $ signum $ (mySqrt x)^2)

prop_complex_sqrt_either_real_or_imag :: Double -> Property
prop_complex_sqrt_either_real_or_imag x = x /= 0 ==> isReal (mySqrt x) /= isImag (mySqrt x)
