import System.Environment
import System.IO
import Text.Printf
import Data.Complex
import Data.List

import Computor
import Computor.Math
import Computor.Error

displayReducedPoly :: ReducedPoly -> String
displayReducedPoly (RP []) = "0"
displayReducedPoly (RP coeffs) = intercalate " + " (zipWith (printf "%f * X^%u") coeffs [(0 :: Int)..])

displayDegree :: Degree -> String
displayDegree MinusInf = "negative infinity"
displayDegree (Degree n) = show n

displaySolution :: Solution -> String
displaySolution (TwoRoots (x1 :+ 0) (x2 :+ 0)) = printf "Discriminant is strictly positive, the two solutions are:\n%f\n%f" x1 x2
displaySolution (TwoRoots x1 x2) = printf "Discriminant is strictly negative, the two solutions are:\n%s\n%s" (displayComplex x1) (displayComplex x2)
displaySolution (DoubleRoot x) = printf "Discriminant is null, the double solution is: %f" x
displaySolution (OneRoot x) = printf "The solution is: %f" x
displaySolution AllReals = "This equation holds true whatever X is"

displayMessage :: Message -> String
displayMessage (ReducedForm rp) = printf "Reduced form: %s = 0" $ displayReducedPoly rp
displayMessage (PolynomialDegree deg) = printf "Polynomial degree: %s" $ displayDegree deg

main :: IO ()
main = do
  args <- getArgs
  let (res, messages) = runComputorM $ computor (unwords args)
  putStr $ unlines (map displayMessage messages)
  case res of
   (Right solution) -> putStrLn $ displaySolution solution
   (Left err) -> do
     hPutStrLn stderr "Error:"
     hPutStrLn stderr (displayError err)
