-- | A Lehmer Random Number Generator. Implemented with 256 streams.
module System.Random.Lehmer.Base (
    LehmerState(..)
  , lehmerInit
  , lehmerNext
  , lehmerStream
  ) where

import System.Random.Lehmer.Constants

data LehmerState = LehmerState { stream  :: Int   -- ^ The currently selected stream to be drawn from
                               , gens    :: [Int] -- ^ List containing the current state of all streams
                               , draws   :: [Int] -- ^ Counter for how many draws have been made from each stream
                               } deriving (Show)

-- | 'lehmerInit' creates a Lehmer Random Number Generator ('LehmerState') with 256 streams
lehmerInit :: Int         -- ^ Seed
           -> LehmerState
lehmerInit s = LehmerState 0 (seed_all s) (replicate streams 0)
  where seed_all = take streams . iterate (\x -> a256 * (x `mod` q_s) - r_s + (x `div` q_s))
        q_s      = modulus `div` a256
        r_s      = modulus `mod` a256

-- | Used to advance the state. Returns next value, and new 'LehmerState'
lehmerNext :: LehmerState -> (Int, LehmerState)
lehmerNext ls@(LehmerState i s d) = (next, ls { gens = ss, draws = dd })
  where next = multiplier * ((s !! i) `mod` q_n) - r_n * ((s !! i) `div` q_n)
        ss   = if next <= 0
                 then take i s ++ [next + modulus] ++ drop (i+1) s
                 else take i s ++ [next] ++ drop (i+1) s
        dd   = take i d ++ [(d !! i) + 1] ++ drop (i+1) d -- Increment draw counter for stream
        q_n  = modulus `div` multiplier
        r_n  = modulus `mod` multiplier

-- | Updates selected stream, returns new 'LehmerState'. Does nothing if stream > 255
lehmerStream :: Int         -- ^ Stream
             -> LehmerState -- ^ Current State
             -> LehmerState -- ^ Updated State
lehmerStream ns ls
  | ns < 256  = ls { stream = ns }
  | otherwise = ls


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
