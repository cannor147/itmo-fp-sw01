module Printer where

import JavaDsl
import Base
import Control.Monad.Cont (liftM2, liftM3, join)

newtype Printer a = Printer { printDsl :: Int -> IO String }

printBlock :: Printer a -> Int -> IO String
printBlock y offset = printDsl y (offset + 1)

op1 :: String -> String -> String
op1 a b = "(" <> a <> " " <> b <> ")"

op2 :: String -> String -> String -> String
op2 a b c = "(" <> a <> " " <> b <> " " <> c <> ")"

op3 :: String -> String -> String -> String -> String
op3 a b c d = "(" <> a <> " " <> b <> " " <> c <> " " <> d <> ")"

indent :: Int -> String
indent n = join (replicate n "    ")

block :: String -> Int -> String -> String
block c n x = " " <> c <> " (\n" <> x <> "\n" <> indent n <> ")"

instance JavaDsl Printer where
  (@?:) = (.....) Printer . flip3 . (. flip printDsl) . (~~~) . liftM3 $ op3 "@?!"
  (@||) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@||"
  (@&&) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@&&"
  (@>=) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@>="
  (@<=) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@<="
  (@>)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@>"
  (@<)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@<"
  (@==) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@=="
  (@=~) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@=~"
  (@!=) = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@!="
  (@+)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@+"
  (@-)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@-"
  (@*)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@*"
  (@/)  = (...)   Printer . flip2 . (. flip printDsl) . (~~)  . liftM2 $ op2 "@/"
  (@~)  = (.)     Printer . flip  . (. flip printDsl) . (.)   . fmap   $ op1 "@~"
  (@!)  = (.)     Printer . flip  . (. flip printDsl) . (.)   . fmap   $ op1 "@!"
  (@@)  =         Printer . const .                             pure   . \x -> op1 "@@" (show x)

  idle      = Printer $ pure . \offset -> indent offset <> "@idle"
  group a b = Printer (\offset -> liftM2 (\x y -> indent offset <> "(@group " <> x <> ")\n" <> y) (printDsl a offset) (printDsl b offset))

  printGroup        = (.) Printer . flip . (. flip printDsl) . (.) . fmap $ op1 "@print"
  printLnGroup      = (.) Printer . flip . (. flip printDsl) . (.) . fmap $ op1 "@printLn"
  whileGroup cond a = Printer (\offset -> liftM2 (\x y -> op1 "@while" x <> block "@while#do" offset y) (printDsl cond offset) (printBlock a offset))
  ifGroup cond a b  = Printer (\offset -> liftM3 (\x y z -> op1 "@if" x <> block "@if#then" offset y <> block "@if#else" offset z) (printDsl cond offset) (printBlock a offset) (printBlock b offset))

  fun0 = Printer . const . pure . op1 "@call"