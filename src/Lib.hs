module Lib where

import Lexer
import Parser
import Grammar
import Control.Monad.Trans.State
import JavaDsl()
import Interpreter
import Printer
import Data.Map

parseJava :: IO String -> IO [Statement]
parseJava = (<$>) (parser . alexScanTokens)

interpretJava :: IO [Statement] -> IO ()
interpretJava = (=<<) (\x -> evalStateT (interpret $ statementsToDsl empty x) empty)

prettyPrintJava :: IO [Statement] -> IO String
prettyPrintJava = (=<<) (\x -> printDsl (statementsToDsl empty x) 1)
