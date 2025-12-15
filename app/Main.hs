module Main where

import Control.Monad (when)
import qualified Data.Text.IO as TIO
import Language.Parser
import Language.Syntax
import Language.TypeCheck
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> runFile file
    _ -> runFile "example/basic.ll"

runFile :: String -> IO ()
runFile path = do
  content <- TIO.readFile path
  case parseProgram content of
    Left err -> putStrLn $ "Parse Error: " ++ show err
    Right prog -> do
      putStrLn "Parsed Successfully."
      checkProgramIO prog

-- checkProgramIO will print "Type Check Passed" or fail
