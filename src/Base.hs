module Base where

(...) :: (r -> t) -> (a -> b -> r) -> a -> b -> t
(...) = (.) . (.)

(.....) :: (r -> t) -> (a -> b -> c -> r) -> a -> b -> c -> t
(.....) = (.) . (.) . (.)

(~~) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(~~) f z a b = f (z a) (z b)

(~~~) :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
(~~~) f z a b c = f (z a) (z b) (z c)

flip2 :: (a -> b -> c -> z) -> b -> c -> a -> z
flip2 x = (.) flip $ flip x

flip3 :: (a -> b -> c -> d -> z) -> b -> c -> d -> a -> z
flip3 = (.) (((flip .) . flip) .) flip

(?) :: Bool -> p -> p -> p
(?) cond a b = if cond then a else b