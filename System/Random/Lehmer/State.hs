module System.Random.Lehmer.State (
    LehmerGen
  , select
  , over
  , len
  , poll
  , draw
  , evalState
  , execState
  , runState
  ) where

import System.Random.Lehmer.Base
import System.Random.Lehmer.Constants
import Control.Monad.State

type LehmerGen = State LehmerState

{-|
  Changes the current stream to the given Int
  If stream is greater than 255, nothing is changed
-}
select :: Int -- ^ Stream
       -> LehmerGen ()
select i = state (\ls -> ((), lehmerStream i ls))

{-|
  Checks if a stream possibly started drawing numbers from other streams.
  Returns how many streams it has overflowed into.
-}
over :: LehmerGen Int
over = state (\ls@(LehmerState s _ d) -> ((d !! s) `div` stream_len, ls))

-- | Returns how many draws have been made on the selected stream
len :: LehmerGen Int
len = state (\ls@(LehmerState s _ d) -> ((d !! s), ls))

-- | Returns the current value on the current stream (same as getSeed)
poll :: LehmerGen Int
poll = state (\ls@(LehmerState s g _) -> ((g !! s), ls))

-- | Returns the next next value in the current stream, updating the state as well
draw :: LehmerGen Int
draw = state lehmerNext

------------------
-- Some Testing --
------------------
-- check :: Int
-- check = 399268537 -- ^ should be the 10000 value in stream 0, with a seed of 1

-- main :: IO ()
-- main = do
--   putStrLn "If False, then the Lehmer Generator did not pass test"
--   print $ evalState checkTest $ lehmerInit 1


-- -- Simply a test function, using the check value, basically what is found in the C code
-- -- Expects to be given (lehmerInit 1)
-- checkTest :: LehmerGen Bool
-- checkTest = do
--   select 0
--   drawCount 10000
--   val <- poll
--   let matches = val == check
--   return matches

-- drawCount :: Int -> LehmerGen ()
-- drawCount = flip replicateM_ draw
