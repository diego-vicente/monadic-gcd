-- | simple_GCD is a function that computes the Greatest Common Divisor using
-- the Euclides algorithm.
simpleGCD :: (Integral a) => a -> a -> a
simpleGCD a b
  | a < b          = simpleGCD b a
  | a `mod` b == 0 = b
  | otherwise      = simpleGCD b (a `mod` b)


-- | Log type contains a value and a list of strings. It is implemented in this
-- code as a Monad
data Log a = Log { getValue :: a, getLogs :: [String] } deriving Show

instance Functor Log where
  fmap f (Log x logs) = Log (f x) logs

instance Applicative Log where
  pure x = Log x []
  Log f log <*> Log x log' = Log (f x) (log ++ log')

instance Monad Log where
  return = pure
  (Log x log) >>= f = let Log y new = f x in Log y (log ++ new)


-- | saveLog stores a message in a dummy Log. Useful in do-blocks
saveLog :: String -> Log ()
saveLog str = Log () [str]


-- | logGCD follows the Euclides algorithm but returns a log object with a
-- trace where it stores all the steps performed.
logGCD :: (Integral a, Show a) => a -> a -> Log a
logGCD x y
 | x < y = logGCD y x
 | y == 0 = do
     saveLog $ "Finished with " ++ show x
     return x
 | otherwise = do
     saveLog $ show x ++ " mod " ++ show y ++ " = " ++ show (x `mod` y)
     logGCD y (x `mod` y)


-- | stepsGCD prints all the steps needed to compute the GCD of two integers
stepsGCD :: (Integral a, Show a) => a -> a -> IO()
stepsGCD x y = mapM_ putStrLn $ getLogs (logGCD x y)
