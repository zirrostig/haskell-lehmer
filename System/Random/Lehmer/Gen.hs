-- | A Lehmer Random Number Generator. Implemented with 256 streams, and 
-- encapsulated into a State Monad.
module System.Random.Lehmer.Gen (
    LehmerState
  , Lehmer
  , lehmerInit
  , select
  , over
  , len
  , poll
  , draw
  ) where

import Control.Monad.State

--------------------------------
-- Our constants, and what not--
--------------------------------
modulus, multiplier, streams, a256, q_n, r_n, q_s, r_s, stream_len :: Int
modulus    = 2147483647           -- ^ 32Bit Lehmer Random
multiplier = 48271
streams    = 256
a256       = 22925
q_n        = modulus `div` multiplier
r_n        = modulus `mod` multiplier
q_s        = modulus `div` a256
r_s        = modulus `mod` a256
stream_len = modulus `div` streams -- ^ How many gaurenteed unique random numbers are in a given stream

type Lehmer = Int
data LehmerState = LehmerState { stream  :: Int       -- ^ The currently selected stream to be drawn from
                               , gens    :: [Lehmer]  -- ^ List containing the current state of all streams
                               , draws   :: [Int]     -- ^ Counter for how many draws have been made from each stream
                               } deriving (Show)

-- | 'init' creates a Lehmer Random Number Generator with 256 streams
lehmerInit :: Lehmer      -- ^ The seed
           -> LehmerState
lehmerInit s = LehmerState 0 (seed_all s) (replicate streams 0)
  where seed_all = take streams . iterate (\x -> a256 * (x `mod` q_s) - r_s + (x `div` q_s))

-- State Monad wrapper
type LehmerGen = State LehmerState

-- | changes the current stream to the given Int, breaks if greater than 255
select :: Int -> LehmerGen ()
select i = state (\ls -> ((), ls { stream = i }))

-- | checks if a stream possibly started drawing numbers from other streams, returns how many streams it has overflowed into.
over :: LehmerGen Int
over = state (\ls@(LehmerState s _ d) -> ((d !! s) `div` stream_len, ls))

-- | returns how many draws have been made on the selected stream
len :: LehmerGen Int
len = state (\ls@(LehmerState s _ d) -> ((d !! s), ls))

-- | returns the current value on the current stream (same as getSeed)
poll :: LehmerGen Int
poll = state (\ls@(LehmerState s g _) -> ((g !! s), ls))

-- | returns the next next value in the current stream, updating the state as well
draw :: LehmerGen Int
draw = state lehmerNext

-- | function 'draw' uses to advance the state, and return new value
lehmerNext :: LehmerState -> (Lehmer, LehmerState)
lehmerNext ls@(LehmerState i s d) = (next, ls { gens = ss, draws = dd })
  where next = multiplier * ((s !! i) `mod` q_n) - r_n * ((s !! i) `div` q_n)
        ss   = if next <= 0
                 then take i s ++ [next + modulus] ++ drop (i+1) s
                 else take i s ++ [next] ++ drop (i+1) s
        dd   = take i d ++ [(d !! i) + 1] ++ drop (i+1) d -- Increment draw counter for stream


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
