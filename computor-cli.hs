import System.Environment
import System.IO

import Computor
import Computor.Displaying

main :: IO ()
main = do
  args <- getArgs
  let (res, messages) = runComputorM $ computor (unwords args)
  putStr $ unlines messages
  case res of
   (Right solution) -> putStrLn $ displaySolution solution
   (Left err) -> do
     hPutStrLn stderr "Error:"
     hPutStrLn stderr (displayError err)
