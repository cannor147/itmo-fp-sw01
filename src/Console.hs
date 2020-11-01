module Console where

class Monad m => Console m where
  print       :: String -> m ()
  printLn     :: String -> m ()
  nextBoolean :: m Bool
  nextInt     :: m Int
  nextDouble  :: m Double
  nextLine    :: m String

instance Console IO where
  print       = putStr
  printLn     = putStrLn
  nextBoolean = fmap (\x -> (x == "true") || ((x /= "false") && error ("Can't parse boolean: " <> x))) getLine
  nextInt     = readLn :: IO Int
  nextDouble  = readLn :: IO Double
  nextLine    = getLine
