module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Computor.Tests.Math

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "Sqrt tests" [
      testProperty "Precision of myRealSqrt" prop_real_sqrt_prec,
      testProperty "Squared complex sqrt is real" prop_sqared_complex_sqrt_is_real,
      testProperty "Squared complex sqrt preserves the signum" prop_sqared_complex_sqrt_signum,
      testProperty "Complex sqrt is either real or pure imaginary" prop_complex_sqrt_either_real_or_imag
      ],
  testGroup "Polynoms tests" [
    testProperty "Last coeff of reduced polynom is not null" prop_reduce_last_coeff_is_not_null,
    testProperty "The function reduce is idempotent" prop_reduce_idem,
    testProperty "Calling reducedToPoly on a RP with more than 3 coefficients gives DegreeMoreThan2 error" prop_reducedToPoly_degree_error
    ]
  ]
