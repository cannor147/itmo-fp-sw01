module Printer where

import JavaDsl
import Base
import Control.Monad.Cont (liftM2, liftM3, join)

newtype Printer a = Printer { printDsl :: Int -> IO String }

importText :: Bool -> String
importText a = if a then "import java.util.Scanner;\n\n" else ""

classText :: String -> Int -> String
classText x offset = indent offset <> "public class Main" <> block offset x

mainText :: String -> Int -> String
mainText x offset = "\n" <> indent offset <> "public static void main(String[] args)" <> block offset x

printBlock :: Printer a -> Int -> IO String
printBlock y offset = printDsl y (offset + 1)

op1 :: String -> String -> String
op1 op b = "(" <> op <> b <> ")"

op2 :: String -> String -> String -> String
op2 op a b = "(" <> a <> " " <> op <> " " <> b <> ")"

op3 :: String -> String -> String -> String -> String -> String
op3 opPart1 opPart2 a b c = "(" <> a <> " " <> opPart1 <> " " <> b <> " " <> opPart2 <> " " <> c <> ")"

capture :: String -> String
capture text = if (head text == '(') && (last text == ')') then text else "(" <> text <> ")"

indent :: Int -> String
indent n = join (replicate n "    ")

block :: Int -> String -> String
block n x = " {" <> x <> "\n" <> indent n <> "}"

instance JavaDsl Printer where
  (@?:) = (.....) Printer . flip3 . (. flip printDsl) . (~~~) . liftM3 $ op3 "?" ":"
  (@||) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "||"
  (@&&) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "&&"
  (@>=) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 ">="
  (@<=) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "<="
  (@>)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 ">"
  (@<)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "<"
  (@==) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "=="
  (@=~) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "=~"
  (@!=) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "!="
  (@+)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "+"
  (@-)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "-"
  (@*)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "*"
  (@/)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "/"
  (@~)  = (.)     Printer . flip  . (. flip printDsl) . (.)   . fmap   $ op1 "~"
  (@!)  = (.)     Printer . flip  . (. flip printDsl) . (.)   . fmap   $ op1 "!"
  (@@)  =         Printer . const .                             pure   . \x -> show x

  idle      = Printer $ pure . const ""
  group a b = Printer (\offset -> liftM2 (\x y -> "\n" <> indent offset <> x <> y) (printDsl a offset) (printDsl b offset))

  printGroup        = (.) Printer . flip . (. flip printDsl) . (.) . fmap $ \x -> "System.out.print" <> capture x <> ";"
  printLnGroup      = (.) Printer . flip . (. flip printDsl) . (.) . fmap $ \x -> "System.out.println" <> capture x <> ";"
  whileGroup cond a = Printer (\offset -> liftM2 (\x y -> "while " <> capture x <> block offset y) (printDsl cond offset) (printBlock a offset))
  ifGroup cond a b  = Printer (\offset -> liftM3 (\x y z -> "if " <> capture x <> block offset y <> " else" <> block offset z) (printDsl cond offset) (printBlock a offset) (printBlock b offset))

  fun0        = Printer . const . pure . \n -> n <> "()"
  program i s = Printer $ fmap (\ x -> importText i <> classText (mainText x 1) 0) . const (printDsl s 2)