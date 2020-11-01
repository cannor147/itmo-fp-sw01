{-# LANGUAGE RankNTypes #-}

module Grammar where

import Data.Map
import JavaDsl
import GHC.Base

data Type = TBoolean | TInt | TDouble | TString

data Statement = DeclarationStatement Grammar.Type String Expression
               | AssignmentStatement String Expression
               | IfStatement Expression [Statement] [Statement]
               | WhileStatement Expression [Statement]
               | PrintStatement Expression
               | PrintLnStatement Expression

data Expression = Ternary Expression Expression Expression
                | Or Expression Expression
                | And Expression Expression
                | Eq Expression Expression
                | Ne Expression Expression
                | Ge Expression Expression
                | Le Expression Expression
                | Gt Expression Expression
                | Lt Expression Expression
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | StringEq Expression Expression
                | Negate Expression
                | Not Expression
                | BooleanValue Bool
                | IntValue Int
                | DoubleValue Double
                | StringValue String
                | Variable String
                | FunctionCall0 String
                | FunctionCall1 String Expression
                | FunctionCall2 String Expression Expression

showMany :: (Show a) => [a] -> String
showMany args = unwords $ GHC.Base.map show args

instance Show Grammar.Type where
 show TBoolean                          = "boolean"
 show TInt                              = "int"
 show TDouble                           = "double"
 show TString                           = "String"

instance Show Statement where
  show (DeclarationStatement t n e)     = show t <> " " <> n <> " = " <> show e <> ";"
  show (AssignmentStatement n e)        = n <> " = " <> show e <> ";"
  show (IfStatement cond a b)           = "if (" <> show cond <> ") {" <> showMany a <> "} else {" <> showMany b <> "}"
  show (WhileStatement cond a)          = "while (" <> show cond <> ") {" <> showMany a <> "}"
  show (PrintStatement e)               = "System.out.print(" <> show e <> ");"
  show (PrintLnStatement e)             = "System.out.println(" <> show e <> ");"

instance Show Expression where
  show (Ternary a b c)                  = "(" <> show a <> " ? " <> show b <> " : " <> show c <> ")"
  show (Or a b)                         = "(" <> show a <> " || " <> show b <> ")"
  show (And a b)                        = "(" <> show a <> " && " <> show b <> ")"
  show (Eq a b)                         = "(" <> show a <> " == " <> show b <> ")"
  show (Ne a b)                         = "(" <> show a <> " != " <> show b <> ")"
  show (Ge a b)                         = "(" <> show a <> " >= " <> show b <> ")"
  show (Le a b)                         = "(" <> show a <> " <= " <> show b <> ")"
  show (Gt a b)                         = "(" <> show a <> " > " <> show b <> ")"
  show (Lt a b)                         = "(" <> show a <> " < " <> show b <> ")"
  show (Add a b)                        = "(" <> show a <> " + " <> show b <> ")"
  show (Sub a b)                        = "(" <> show a <> " - " <> show b <> ")"
  show (Mul a b)                        = "(" <> show a <> " * " <> show b <> ")"
  show (Div a b)                        = "(" <> show a <> " / " <> show b <> ")"
  show (StringEq a b)                   = show a <> ".equals(" <> show b <> ")"
  show (Negate a)                       = "(-" <> show a <> ")"
  show (Not a)                          = "(!" <> show a <> ")"
  show (BooleanValue v)                 = show v
  show (IntValue v)                     = show v
  show (DoubleValue v)                  = show v
  show (StringValue v)                  = show v
  show (Variable n)                     = n
  show (FunctionCall0 n)                = n <> "()"
  show (FunctionCall1 n a)              = n <> "(" <> show a <> ")"
  show (FunctionCall2 n a b)            = n <> "(" <> show a <> ", " <> show b <> ")"

type Context = Map String String

statementsToDsl :: JavaDsl p => Context -> [Statement] -> p ()
statementsToDsl ctx (x:xs) = group (statementToDsl ctx x) (statementsToDsl ctx xs)
statementsToDsl _   []     = idle

statementToDsl :: JavaDsl p => Context -> Statement -> p ()
statementToDsl ctx statement =
  case statement of
    (DeclarationStatement t n e) -> printGroup $ expressionToDsl ctx e
    (AssignmentStatement n e)    -> printGroup $ expressionToDsl ctx e
    (IfStatement cond a b)       -> ifGroup (expressionToDsl ctx cond) (statementsToDsl ctx a) (statementsToDsl ctx b)
    (WhileStatement cond a)      -> whileGroup (expressionToDsl ctx cond) (statementsToDsl ctx a)
    (PrintStatement e)           -> printGroup $ expressionToDsl ctx e
    (PrintLnStatement e)         -> printLnGroup $ expressionToDsl ctx e

expressionToDsl :: JavaDsl p => Context -> Expression -> p JValue
expressionToDsl ctx expression =
  let self = expressionToDsl ctx in
  case expression of
    (Ternary a b c)       -> (@?:) (self a) (self b) (self c)
    (Or a b)              -> self a @|| self b
    (And a b)             -> self a @&& self b
    (Eq a b)              -> self a @== self b
    (Ne a b)              -> self a @!= self b
    (Ge a b)              -> self a @>= self b
    (Le a b)              -> self a @<= self b
    (Gt a b)              -> self a @> self b
    (Lt a b)              -> self a @< self b
    (Add a b)             -> self a @+ self b
    (Sub a b)             -> self a @- self b
    (Mul a b)             -> self a @* self b
    (Div a b)             -> self a @/ self b
    (StringEq a b)        -> self a @=~ self b
    (Negate a)            -> (@!) $ self a
    (Not a)               -> (@~) $ self a
    (BooleanValue v)      -> (@@) $ JBoolean v
    (IntValue v)          -> (@@) $ JInt v
    (DoubleValue v)       -> (@@) $ JDouble v
    (StringValue v)       -> (@@) $ JString v
    (FunctionCall0 n)     -> fun0 n
    _                     -> (@@) $ JInt 0
