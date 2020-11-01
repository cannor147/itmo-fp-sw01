module Main where

import Lib
import System.Directory.Internal.Prelude (getArgs)

readConsole :: IO String
readConsole = getContents

readString :: String -> IO String
readString = pure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "File path excepted"
    ["help"] -> putStrLn "Need 2 arguments: path to cpp file and work mode (interpret/print)"
    ["parse",     path] -> parseJava (readFile path) >>= Prelude.print
    ["interpret", path] -> (interpretJava . parseJava) (readFile path)
    ["pretty",     path] -> (prettyPrintJava . parseJava) (readFile path) >>= putStrLn
    _ -> putStrLn "Use 'help'"