{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Monad
import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Control.Monad.Except
import Control.Monad.Writer
import System.Environment
import System.IO

data Poly = Poly {
  zerothDegree :: Double,
  firstDegree :: Double,
  secondDegree :: Double
  }
          deriving (Show)

-- In GeneralPoly the head of the list is the zeroth degree coefficient
newtype GeneralPoly = GP [Double]
                    deriving (Show)


zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault _ _ [] [] = []
zipWithDefault op def (x:xs) [] = (x `op` def):zipWithDefault op def xs []
zipWithDefault op def [] (y:ys) = (def `op` y):zipWithDefault op def [] ys
zipWithDefault op def (x:xs) (y:ys) = (x `op` y):zipWithDefault op def xs ys

subtract_gp (GP l1, GP l2) = GP $ zipWithDefault (-) 0 l1 l2

type Equation = (GeneralPoly, GeneralPoly)

type EqParser a = Parsec String Int a -- The state is the degree of the current term

termParser :: EqParser Double
termParser = do
  coeff <- sign <*> (either (fromIntegral :: Int -> Double) id <$> decimalFloat)
  string " * X^"
  string . show =<< getState
  modifyState (+ 1)
  return coeff

polyParser :: EqParser GeneralPoly
polyParser = do
  putState 0
  GP <$> termParser `sepBy1` (try $ string " + ")

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

-- In ReducedPoly the head of the list is the zeroth degree coefficient and the last coefficient is guaranteed to be non-zero
newtype ReducedPoly = RP [Double]
                    deriving (Show)

reduce :: Equation -> ReducedPoly
reduce (subtract_gp -> GP p) = RP $ foldr trimPolyHelper [] p
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
getDegree (RP l) = Degree $ length l - 1


data Solution = TwoRoots Double Double | DoubleRoot Double | OneRoot Double | AllReals
              deriving (Show)

solve :: Poly -> ComputorM Solution
solve (Poly 0 0 0) = return AllReals -- Equation 0 + 0X + 0X^2 = 0 <=> 0 = 0 holds for every X
solve (Poly c 0 0) = throwError NoSolution -- Equation c + 0X + 0X^2 = 0 <=> c = 0 where c /= is always false, thus holds for no X
solve (Poly c b 0) = return $ OneRoot (-c / b) -- Equation c + bX + 0X^2 = 0 <=> bX + c = 0 <=> bX = -c <=> X = -c/b, wich always exist since b in not null
solve (Poly c b a) = case b^2 - 4 * a * c of -- Classical resolution of 2nd degree equation
                      0 -> return $ DoubleRoot (-b / (2 * a))
                      discr | discr > 0 -> return $ TwoRoots ((-b + sqrt discr) / (2 * a)) ((-b - sqrt discr) / (2 * a))
                            | otherwise -> throwError NoSolution

computor :: [String] -> ComputorM Solution
computor args = do
  rp <- reduce <$> readEquation args
  tell [show rp]
  let deg = getDegree rp
  tell [show deg]
  poly <- reducedToPoly rp
  solve poly

main :: IO ()
main = do
  args <- getArgs
  let (res, messages) = runWriter . runExceptT $ computor args
  putStr $ unlines messages
  case res of
   (Right solution) -> print solution
   (Left err) -> do
     hPutStrLn stderr "Error:"
     hPrint stderr err
