-- | Lehmer Random Number Generator. Makes use of the State monad. For people 
-- doing it the hard way, look at "System.Random.Lehmer.Base"
module System.Random.Lehmer.State (
  -- * Usage
  -- $usage

  -- * State Wrapper and Initilizer
    LehmerGen
  , LehmerState
  , lehmerInit

  -- * Basic Transistions/Inspection
  , select
  , over
  , len
  , poll
  , draw

  -- * More sophisticated random distributions
  , exponential
  , uniform
  , uniformRange

  -- * List generation
  , many
  , manyUniform
  , manyUniformRange
  , manyExponential

  -- * Re-Exported from Control.Monad.State
  , evalState
  , execState
  , runState
  ) where

import qualified System.Random.Lehmer.Base as L
import System.Random.Lehmer.Base (LehmerState, lehmerInit)
import Control.Monad.State

-- $usage
-- Here is a quick demo program, that simply creates a new Lehmer Generator,
-- runs a demo function that modifies the state, and returns some values,
-- then prints out the values generated.
--
-- > main :: IO ()
-- > main = do
-- >   let rng = lehmerInit 42    -- Creates new Lehmer RNG
-- >   let (val, newrng) = runState demo rng
-- >   print val
-- >   -- Continue doing stuff with newrng
--
-- Notice uniform/exponential can return either a Float or a Double, making
-- them very flexible for various code bases.
--
-- > demo :: LehmerGen (Int, Double, Float, [Int])
-- > demo = do
-- >   select 5             -- Selects stream number 5
-- >   v1 <- draw           -- Uniform(1, 2^31 - 2)
-- >   v2 <- uniform        -- Uniform(0,1)
-- >   select 200           -- Switches streams to stream 200
-- >   v3 <- exponential 4  -- Exponential(4)
-- >   v4 <- many 10        -- List of 10 draws
-- >   return (v1, v2, v3, v4)
--
type LehmerGen = State L.LehmerState

{-|
  Changes the current stream to the given Int
  If stream is greater than 255, nothing is changed
-}
select :: Int -- ^ Stream
       -> LehmerGen ()
select i = state (\ls -> ((), L.stream i ls))

{-|
  Checks if a stream possibly started drawing numbers from other streams.
  Returns how many streams it has overflowed into.
-}
over :: LehmerGen Int
over = state (\ls -> (L.overdraw ls, ls))

-- | Returns how many draws have been made on the selected stream
len :: LehmerGen Int
len = state (\ls -> (L.drawCount ls, ls))

-- | Returns the current value on the current stream (same as 'L.getSeed')
poll :: LehmerGen Int
poll = state (\ls -> (L.getSeed ls, ls))

-- | Returns the next next value in the current stream, updating the state as well
draw :: LehmerGen Int
draw = state L.next


-- | Returns Uniform(0,1)
uniform :: (Floating a) => LehmerGen a
uniform = state L.uniform

-- | Returns Uniform(a,b)
uniformRange :: (Floating a, Ord a)
             => a           -- ^ Lower Bound (a)
             -> a           -- ^ Upper Bonud (b)
             -> LehmerGen a -- ^ Result
uniformRange a b = state (L.uniformRange a b)

exponential :: Floating a
            => a            -- ^ λ value
            -> LehmerGen a  -- ^ Result
exponential = state . L.exponential

-- | Returns a list of random numbers
many :: Integral a
     => a               -- ^ Length of list
     -> LehmerGen [Int] -- ^ Result
many = state . L.list

-- | Returns a list of Uniform(0,1) numbers
manyUniform :: (Integral a, Floating b)
            => a              -- ^ Length of list
            -> LehmerGen [b]  -- ^ Result
manyUniform = state . L.listUniform

-- | Returns a list of Uniform(a,b) numbers
manyUniformRange :: (Integral a, Floating b, Ord b)
                 => a             -- ^ Length of list
                 -> b             -- ^ Lower Bound (a)
                 -> b             -- ^ Upper Bound (b)
                 -> LehmerGen [b] -- ^ Result
manyUniformRange l a b = state $ L.listUniformRange l a b

-- | Returns a list of Exponential(λ) numbers
manyExponential :: (Integral a, Floating b)
                => a              -- ^ Length of list
                -> b              -- ^ λ value
                -> LehmerGen [b]  -- ^ Result
manyExponential l lb = state $ L.listExponential l lb


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

-- main :: IO ()
-- main = do
--   let rng = lehmerInit 42    -- Creates new Lehmer RNG
--   let (val, newrng) = runState demo rng
--   print val
--   -- Continue doing stuff with newrng
--   --
-- demo :: LehmerGen (Int, Double, Float, [Int])
-- demo = do
--   select 5             -- Selects stream number 5
--   v1 <- draw           -- Uniform(1, 2^31 - 2)
--   v2 <- uniform        -- Uniform(0,1)
--   v3 <- exponential 4  -- Exponential(4)
--   v4 <- many 10        -- List of 10 draws
--   return (v1, v2, v3, v4)
