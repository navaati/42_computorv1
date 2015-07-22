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
      ]
  ]
