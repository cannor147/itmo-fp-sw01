{-# LANGUAGE RankNTypes #-}

module Grammar where

import JavaDsl
import GHC.Base

data Type = TBoolean | TInt | TDouble | TString

data Program = Program Bool [Statement]

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

instance Show Program where
  show = const "Successfully parsed"

newtype Context = Context {importContext :: Bool}

programToDsl :: JavaDsl p => Program -> p ()
programToDsl (Program i s) = program i $ statementsToDsl (Context i) s

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
    (FunctionCall0 n)     -> if importContext ctx
                             then fun0 n
                             else error $ "Can't parse function " <> n
    _                     -> (@@) $ JInt 0
