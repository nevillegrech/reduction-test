import Control.Monad.State

type MemoizedFibs = [(Integer, Integer)]

main = print res where
        res = goFib 200

goFib :: Int -> (Integer, MemoizedFibs)
goFib n = runState (fib $ fromIntegral n) []

fib :: Integer -> State MemoizedFibs Integer
fib n | n < 2 = return 1
fib n = do
   nMinus2 <- lookupOrCompute (n - 2)
   nMinus1 <- lookupOrCompute (n - 1)
   let result = nMinus1 + nMinus2
   modify (\mems -> (n, result) : mems)
   return result

get :: State s s
get = St $ \s -> (s, s)
          
lookupOrCompute :: Integer -> State MemoizedFibs Integer
lookupOrCompute n = do
   currentState <- get
   case lookup n currentState of
       Nothing -> fib n
       (Just fibn) -> return fibn

-- Some implementation details:
--
-- newtype State s a = St (s -> (a, s))
--
-- runState :: (State s a) -> s -> (a, s)
-- runState st initState = case st of
--     (St fn) -> fn initState
--
-- so fib is really:
-- fib :: Integer -> MyState -> (Integer, MyState)

   class Monad m where
     return :: a -> m a
     (>>=) :: m a -> (a -> m b) -> m b  
--
-- instance Monad (State s) where
       return :: a -> State s a 
--     return a = \s -> (a, s)
--
       (>>=) :: State s a -> (a -> State s b) -> State s b
        >>                   State s b

       ma >> mb = ma >>= (\_ -> mb)
                  
--     ma >>= fn = St $ \s -> let (res, s') = runState ma s in
--                            let mb = fn res in
--                            mb s'
