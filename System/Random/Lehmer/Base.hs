{-|
  A Lehmer Random Number Generator implemented with 256 streams. These are the
  base functions for manipulating the generator. For a nicer interface using the
  State Monad, use "System.Random.Lehmer.State".
-}
module System.Random.Lehmer.Base (
    LehmerState(..)
  , lehmerInit
  , stream
  , overdraw
  , drawCount
  , getSeed
  , next
  , exponential
  , uniform
  , uniformRange
  , list
  , listUniform
  , listUniformRange
  , listExponential
  ) where

import System.Random.Lehmer.Constants
import System.Random.Lehmer.Util

data LehmerState = LehmerState { ss      :: Int   -- ^ The currently selected stream to be drawn from
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

-- | Updates selected stream, returns new 'LehmerState'. Does nothing if stream > 255
stream :: Int         -- ^ Stream
       -> LehmerState -- ^ Current State
       -> LehmerState -- ^ Updated State
stream ns ls
  | ns < 256  = ls { ss = ns }
  | otherwise = ls

-- | Returns how many streams the current stream has overlapped
overdraw :: LehmerState -> Int
overdraw (LehmerState s _ d) = (d !! s) `div` stream_len

-- | Returns how many numbers have been generated in the current stream
drawCount :: LehmerState -> Int
drawCount (LehmerState s _ d) = d !! s

-- | Returns current value of stream, does not get new random number, use 'next' for that.
getSeed :: LehmerState -> Int
getSeed (LehmerState s g _) = g !! s

-- | Used to advance the state. Returns next value, and new 'LehmerState'
next :: LehmerState -> (Int, LehmerState)
next ls@(LehmerState i s d) = (val, ls { gens = gs, draws = dd })
  where nxt  = multiplier * ((s !! i) `mod` q_n) - r_n * ((s !! i) `div` q_n)
        val  = if nxt <= 0 then nxt + modulus else nxt
        gs   = take i s ++ [nxt] ++ drop (i+1) s
        dd   = take i d ++ [(d !! i) + 1] ++ drop (i+1) d -- Increment draw counter for stream
        q_n  = modulus `div` multiplier
        r_n  = modulus `mod` multiplier

-- | Makes a draw using an Exponential Distribution
exponential :: Floating a
          => a           -- ^ λ
          -> LehmerState -- ^ Starting state
          -> (a, LehmerState) -- ^ Resulting exponential draw, and new state
exponential l ls = (expDist l val, st)
  where nxt = uniform ls
        val = fst nxt
        st  = snd nxt

{-|
  Makes a draw using a Uniform Distribution
  Note that 0 and 1 are not included in the range, this is a known behavior of
    then Lehmer Generator
-}
uniform :: Floating a => LehmerState -> (a, LehmerState)
uniform ls = (stdUniform val, st)
  where nxt = next ls
        val = fst nxt
        st  = snd nxt

{-|
  Like 'uniform' but allows for a defined range.
  Returns a value in the range (a,b).
  Note that a and b are not included in the range.
-}
uniformRange :: (Floating a, Ord a)
                   => a                 -- ^ Lower bound (a)
                   -> a                 -- ^ Upper bound (b)
                   -> LehmerState       -- ^ Start State
                   -> (a, LehmerState)  -- ^ Resulting Uniform(a,b) and New State
uniformRange a b ls = (coerce a b uni, st)
  where nxt = next ls
        uni = stdUniform $ fst nxt
        st  = snd nxt

-- | Draws a series of numbers from the generater, returning them as a list
list :: (Integral a)
     => a                     -- ^ Length of list to generate
     -> LehmerState           -- ^ Starting state
     -> ([Int], LehmerState)  -- ^ Resulting list and new state
list len ls = foldl (\(xs, st) _ -> (xs ++ [fst $ next st], snd $ next st)) ([], ls) [0..(len - 1)]

-- | Draws a series of Uniform(0,1)
listUniform :: (Integral a, Floating b)
            => a                  -- ^ Length of list to generate
            -> LehmerState        -- ^ Starting State
            -> ([b], LehmerState) -- ^ The resulting list of Uniform(0,1) and New State
listUniform len ls = (unis, st)
  where nxt  = list len ls
        unis = map stdUniform (fst nxt)
        st   = snd nxt

-- | Draws a series of Uniform(a,b)
listUniformRange :: (Integral a, Floating b, Ord b)
                 => a           -- ^ Length of list to generate
                 -> b           -- ^ Lower Bound (a)
                 -> b           -- ^ Upper Bound (b)
                 -> LehmerState -- ^ Starting State
                 -> ([b], LehmerState) -- ^ Resulting List of Uniform(a,b) and New State
listUniformRange len a b ls = (unis, st)
  where nxt  = list len ls
        unis = map (coerce a b . stdUniform) (fst nxt)
        st   = snd nxt

-- | Draws a series of Exponential(λ)
listExponential :: (Integral a, Floating b)
                => a                  -- ^ Length of list to generate
                -> b                  -- ^ λ value
                -> LehmerState        -- ^ Starting State
                -> ([b], LehmerState) -- ^ Resulting List of Exponential(λ) and New State
listExponential len l ls = (exps, st)
  where nxt  = listUniform len ls
        exps = map (expDist l) (fst nxt)
        st   = snd nxt
