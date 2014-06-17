import System.Environment
import SPLData
import SPLEval
import SPLParse

main :: IO ()
main = do input <- getContents
          case (runProgram . evalProgram . readProgram) input of
            Left (Fail str) -> putStrLn str
            Right val -> putStrLn $ show val

