module Lib where

import Lexer
import Parser
import Grammar
import Control.Monad.Trans.State
import JavaDsl()
import Interpreter
import Printer
import Data.Map

parseJava :: IO String -> IO Program
parseJava = (<$>) (parser . alexScanTokens)

interpretJava :: IO Program -> IO ()
interpretJava = (=<<) (\program -> evalStateT (interpret $ programToDsl program) empty)

prettyPrintJava :: IO Program -> IO String
prettyPrintJava = (=<<) (\program -> printDsl (programToDsl program) 1)
