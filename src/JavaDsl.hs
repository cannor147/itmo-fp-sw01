module JavaDsl where

import Base
import Data.Char (toLower)

data JValue = JBoolean Bool | JInt Int | JDouble Double | JString String

__ :: Show a => a -> b
__ x = error $ "Unexpected expression: " <> show x

___ :: Show a => a -> a -> b
___ x y = error $ "Unexpected combination of expressions: " <> show x <> show y

switch :: (Bool -> a) -> (Int -> a) -> (Double -> a) -> (String -> a) -> JValue -> a
switch f _ _ _ (JBoolean a) = f a
switch _ g _ _ (JInt     a) = g a
switch _ _ h _ (JDouble  a) = h a
switch _ _ _ i (JString  a) = i a

switch2 :: (Bool -> Bool -> a) -> (Int -> Int -> a) -> (Double -> Double -> a) -> (String -> String -> a) ->
  (String -> String -> a) -> (JValue -> JValue -> a) -> JValue -> JValue -> a
switch2 f _ _ _ _ _ (JBoolean a) (JBoolean b) = f a b
switch2 _ g _ _ _ _ (JInt     a) (JInt     b) = g a b
switch2 _ _ h _ _ _ (JInt     a) (JDouble  b) = h (fromIntegral a) b
switch2 _ _ h _ _ _ (JDouble  a) (JInt     b) = h a (fromIntegral b)
switch2 _ _ h _ _ _ (JDouble  a) (JDouble  b) = h a b
switch2 _ _ _ i _ _ (JString  a) (JString  b) = i a b
switch2 _ _ _ _ j _ (JString  a)           b  = j a (toString b)
switch2 _ _ _ _ j _           a  (JString  b) = j (toString a) b
switch2 _ _ _ _ _ k           a            b  = k a b

class Questionable p where
  (?:) :: Bool -> p -> p -> p

class Valuable p where
  getBool   :: p -> Bool
  getInt    :: p -> Int
  getDouble :: p -> Double
  getString :: p -> String

class ToStringable p where
  toString  :: p -> String

instance Show JValue where
  show = switch show show show show

instance Eq JValue where
  (==) = switch2 (==) (==) (==) ___ ___ ___

instance Ord JValue where
  (<=) = switch2 ___ (<=) (<=) ___ ___ ___

instance Num JValue where
  (+)         = switch2 ___ (JInt ... (+)) (JDouble ... (+)) (JString ... (<>)) (JString ... (<>)) ___
  (-)         = switch2 ___ (JInt ... (-)) (JDouble ... (-)) ___ ___ ___
  (*)         = switch2 ___ (JInt ... (*)) (JDouble ... (*)) ___ ___ ___
  abs         = switch __ (JInt . abs) (JDouble . abs) __
  signum      = switch __ (JInt . signum) (JDouble . signum) __
  fromInteger = JInt . fromInteger

instance Fractional JValue where
  (/)          = switch2 ___ (JInt ... div) (JDouble ... (/)) ___ ___ ___
  fromRational = JDouble . fromRational

instance Questionable JValue where
  (?:) cond = switch2 (JBoolean ... (?) cond) (JInt ... (?) cond) (JDouble ... (?) cond) (JString ... (?) cond) ___ ___

instance Valuable JValue where
  getBool   = switch id __ __ __
  getInt    = switch __ id __ __
  getDouble = switch __ __ id __
  getString = switch __ __ __ id

instance ToStringable JValue where
  toString = switch (map toLower . show) show show id

class JavaDsl code where
  infixl 2 @||
  infixl 3 @&&
  infixl 4 @>=
  infixl 4 @<=
  infixl 4 @>
  infixl 4 @<
  infixl 4 @==
  infixl 4 @!=
  infixl 6 @+
  infixl 6 @-
  infixl 7 @*
  infixl 7 @/

  (@?:)  :: code JValue -> code JValue -> code JValue -> code JValue
  (@||)  :: code JValue -> code JValue -> code JValue
  (@&&)  :: code JValue -> code JValue -> code JValue
  (@>=)  :: code JValue -> code JValue -> code JValue
  (@<=)  :: code JValue -> code JValue -> code JValue
  (@>)   :: code JValue -> code JValue -> code JValue
  (@<)   :: code JValue -> code JValue -> code JValue
  (@==)  :: code JValue -> code JValue -> code JValue
  (@=~)  :: code JValue -> code JValue -> code JValue
  (@!=)  :: code JValue -> code JValue -> code JValue
  (@+)   :: code JValue -> code JValue -> code JValue
  (@-)   :: code JValue -> code JValue -> code JValue
  (@*)   :: code JValue -> code JValue -> code JValue
  (@/)   :: code JValue -> code JValue -> code JValue
  (@!)   :: code JValue -> code JValue
  (@~)   :: code JValue -> code JValue
  (@@)   :: JValue -> code JValue

  idle  :: code ()
  group :: code () -> code () -> code ()

  printGroup   :: code JValue -> code ()
  printLnGroup :: code JValue -> code ()
  ifGroup      :: code JValue -> code () -> code () -> code ()
  whileGroup   :: code JValue -> code () -> code ()

  fun0 :: String -> code JValue
--  fun1 :: code String -> code JValue -> code JValue
--  fun2 :: code String -> code JValue -> code JValue -> code JValue
