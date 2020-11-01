{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.State
import Data.Functor.Identity()
import JavaDsl
import Type()
import Console
import Base
import Data.Map.Internal (Map)

type Vars = Map String (Int, JValue)

newtype Interpretator m s = Interpretator { interpret :: StateT Vars m s }

instance Console m => Functor (Interpretator m) where
  fmap f = Interpretator . fmap f . interpret

instance Console m => Applicative (Interpretator m) where
  pure      = Interpretator . return
  (<*>) f a = Interpretator $ do
    f' <- interpret f
    a' <- interpret a
    return $ f' a'

instance Console m => Monad (Interpretator m) where
  (>>=) a func = Interpretator $ do
    a1 <- interpret a
    interpret $ func a1

instance Console m => JavaDsl (Interpretator m) where
  (@?:) = liftA3 $ \x -> (?:) (getBool x)
  (@||) = liftA2 $ JBoolean ... (||) ~~ getBool
  (@&&) = liftA2 $ JBoolean ... (&&) ~~ getBool
  (@>=) = liftA2 $ JBoolean ... (>=)
  (@<=) = liftA2 $ JBoolean ... (<=)
  (@>)  = liftA2 $ JBoolean ... (>)
  (@<)  = liftA2 $ JBoolean ... (<)
  (@==) = liftA2 $ JBoolean ... (==)
  (@=~) = liftA2 $ JBoolean ... (==) ~~ getString
  (@!=) = liftA2 $ JBoolean ... (/=)
  (@+)  = liftA2 (+)
  (@-)  = liftA2 (-)
  (@*)  = liftA2 (*)
  (@/)  = liftA2 (/)
  (@~)  = fmap negate
  (@!)  = fmap $ JBoolean . not . getBool
  (@@)  = pure
  idle  = pure ()
  group = (>>)

  printGroup v   = Interpretator $ interpret v >>= lift . Console.print . toString
  printLnGroup v = Interpretator $ interpret v >>= lift . Console.printLn . toString
  ifGroup c a b  = Interpretator $ interpret c >>= interpret . flip (? a) b . getBool
  whileGroup c a = Interpretator $ interpret c >>= interpret . flip when (group a $ whileGroup c a) . getBool

  fun0 name = case name of
    "new Scanner(System.in).nextBoolean" -> Interpretator $ JBoolean <$> lift Console.nextBoolean
    "new Scanner(System.in).nextInt"     -> Interpretator $ JInt <$> lift Console.nextInt
    "new Scanner(System.in).nextDouble"  -> Interpretator $ JDouble <$> lift Console.nextDouble
    "new Scanner(System.in).nextLine"    -> Interpretator $ JString <$> lift Console.nextLine
    _                                    -> pure $ JBoolean True