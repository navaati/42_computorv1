{-# LANGUAGE ViewPatterns #-}

import Control.Applicative hiding ((<|>), many)
import Data.Complex
import Data.List
import Data.Maybe
import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Control.Monad.Except
import Control.Monad.Writer
import Text.Printf
import System.Environment
import System.IO

data Poly = Poly {
  zerothDegree :: Double,
  firstDegree :: Double,
  secondDegree :: Double
  }
          deriving (Show)

-- In Coefficients the head of the list is the zeroth degree coefficient
type Coefficients = [Double]

zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault _ _ [] [] = []
zipWithDefault op def (x:xs) [] = (x `op` def):zipWithDefault op def xs []
zipWithDefault op def [] (y:ys) = (def `op` y):zipWithDefault op def [] ys
zipWithDefault op def (x:xs) (y:ys) = (x `op` y):zipWithDefault op def xs ys

type Equation = (Coefficients, Coefficients)

type EqParser a = Parsec String Int a -- The state is the degree of the current term

termParser :: EqParser Double
termParser = do
  coeff <- sign <*> (either (fromIntegral :: Int -> Double) id <$> decimalFloat)
  string " * X^"
  string . show =<< getState
  modifyState (+ 1)
  return coeff

opParser :: EqParser (Double -> Double)
opParser = do
  char ' '
  op <- id <$ char '+'
        <|> negate <$ char '-'
  char ' '
  return op

polyParser :: EqParser Coefficients
polyParser = do
  putState 0
  first_term <- termParser
  other_terms <- many (try $ opParser <*> termParser)
  return (first_term:other_terms)

equationParser :: EqParser Equation
equationParser = do
  lhs <- polyParser
  string " = "
  rhs <- polyParser
  eof
  return (lhs, rhs)

data ComputorError = WrongNumberOfArgument
                   | ParseError ParseError
                   | DegreeMoreThan2
                   | NoSolution
                   deriving (Show)

type ComputorM a = ExceptT ComputorError (Writer [String]) a

readEquation :: [String] -> ComputorM Equation
readEquation [eq] = withExceptT ParseError . ExceptT . return $
                    runParser equationParser 0 "command line" eq
readEquation  _ = throwError WrongNumberOfArgument

-- In ReducedPoly the last coefficient is guaranteed to be non-zero
newtype ReducedPoly = RP Coefficients
                    deriving (Show)

reduce :: Coefficients -> ReducedPoly
reduce = RP . foldr trimPolyHelper []
  where trimPolyHelper 0 [] = []
        trimPolyHelper n l = n:l

reducedToPoly :: ReducedPoly -> ComputorM Poly
reducedToPoly (RP [z, o, t]) = return $ Poly z o t
reducedToPoly (RP [z, o]) = return $ Poly z o 0
reducedToPoly (RP [z]) = return $ Poly z 0 0
reducedToPoly (RP []) = return $ Poly 0 0 0
reducedToPoly _ = throwError DegreeMoreThan2


data Degree = MinusInf | Degree Int
            deriving (Show)

getDegree :: ReducedPoly -> Degree
getDegree (RP []) = MinusInf
getDegree (RP (_:l)) = Degree $ length l


data Solution = TwoRoots (Complex Double) (Complex Double) | DoubleRoot Double | OneRoot Double | AllReals
              deriving (Show)

mySqrt :: Double -> Complex Double
mySqrt x | x < 0 = (0 :+ myRealSqrt (-x))
         | otherwise = myRealSqrt x :+ 0

myRealSqrt :: Double -> Double
myRealSqrt a = fst . fromJust $ find (\(x_k, x_kp1) -> absError x_kp1 >= absError x_k) $ zip newtonSerie (tail newtonSerie)
  where absError x = abs (x * x - a)
        newtonSerie = iterate newtonNext seed
        newtonNext x_k = (x_k + a / x_k) / 2
        seed = a -- This could be better, for example by finding the nearest integer

solve :: Poly -> ComputorM Solution
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

computor :: [String] -> ComputorM Solution
computor args = do
  rp <- reduce . (uncurry $ zipWithDefault (-) 0) <$> readEquation args
  tell [printf "Reduced form: %s = 0" $ displayReducedPoly rp]
  let deg = getDegree rp
  tell [printf "Polynomial degree: %s" $ displayDegree deg]
  poly <- reducedToPoly rp
  solve poly

main :: IO ()
main = do
  args <- getArgs
  let (res, messages) = runWriter . runExceptT $ computor args
  putStr $ unlines messages
  case res of
   (Right solution) -> putStrLn $ displaySolution solution
   (Left err) -> do
     hPutStrLn stderr "Error:"
     hPutStrLn stderr (displayError err)
