module System.Random.Lehmer.Util (
    (//)
  , coerce
  , stdUniform
  , expDist
) where

-- import Data.Function (on)
import System.Random.Lehmer.Constants

-- | Division of Int values into Floating.
(//) :: (Integral a, Integral b, Floating c) => a -> b -> c
a // b = (fromIntegral a) / (fromIntegral b)
-- Why doesn't this work
-- a // b = (/) `on` fromIntegral
-- It places a type Restriction of Integer, so Ints do not work

-- | Turns an Int in the range (0, m) where m = 2^31 - 1, into a Floating in the range (0,1).
stdUniform :: (Integral a, Floating b) => a -> b
stdUniform = (// modulus)

-- | Takes a value in the range (0,1) and expands it into the range (a,b).
coerce :: (Floating a, Ord a)
       => a -- ^ Lower bound (a)
       -> a -- ^ Upper bound (b)
       -> a -- ^ Uniform(0,1)
       -> a -- ^ Uniform(a,b)
coerce a b u
  | a < b = low + a
  | a > b = low + b
  | otherwise = error "Zero Range"
    where rng = abs $ b - a
          low = rng * u

expDist :: (Floating a)
        => a        -- ^ λ
        -> a        -- ^ Uniform(0,1)
        -> a        -- ^ Exp(λ)
expDist l u = (negate l) * log (1 - u)
