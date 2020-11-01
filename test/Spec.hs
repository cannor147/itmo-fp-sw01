module Main where

import Lib (parseJava)
import Test.Hspec
import Grammar
import Printer
import Interpreter
import Data.Map
import Control.Monad.State.Lazy (evalStateT)
import JavaDsl

helloWorldProgram :: String
helloWorldProgram = "public class Main{public static void main(String[] args){System.out.println(\"Hello, world!\");}}"

helloWorldProgramPretty :: String
helloWorldProgramPretty = "public class Main {\n"
                       <> "    public static void main(String[] args) {\n"
                       <> "        System.out.println(\"Hello, world!\");\n"
                       <> "    }\n"
                       <> "}\n"

myExpression :: Expression
myExpression = Ternary
               ( Gt
                 ( Mul
                   (IntValue 2)
                   (IntValue 2)
                 )
                 (IntValue 2)
               )
               ( StringValue "Hello, world!"                       )
               ( StringValue "FUNctional programmin is really FUN" )

testPrettyPrinting :: SpecWith ()
testPrettyPrinting = describe "test pretty print" $ do
  it "hello world" $ do
    helloWorldParse <- parseJava $ pure helloWorldProgram
    helloWorldResult <- printDsl (programToDsl helloWorldParse) 0
    shouldBe helloWorldResult helloWorldProgramPretty

testInterpret :: SpecWith ()
testInterpret = describe "test interpret"$ do
  it "simple ternary" $ do
    myExpressionResult <- evalStateT (interpretDsl (expressionToDsl (Context False) myExpression)) empty
    shouldBe (getString myExpressionResult) "Hello, world!"

main :: IO ()
main = hspec $ do
  testPrettyPrinting
  testInterpret

