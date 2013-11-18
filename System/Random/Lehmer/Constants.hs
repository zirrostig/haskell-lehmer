module System.Random.Lehmer.Constants (
    modulus
  , multiplier
  , streams
  , a256
  , stream_len
) where

modulus, multiplier, streams, stream_len, a256 :: Int
-- | 32-Bit Modulus, 2^31 - 1
modulus    = 2147483647           -- 32Bit Lehmer Random
-- | Picked for good statistics
multiplier = 48271
-- | 256 Streams available
streams    = 256
-- | How many draws can be made from a stream before it overruns another stream
stream_len = modulus `div` streams -- How many gaurenteed unique random numbers are in a given stream
-- | Jump Mulitplier to seed the 256 streams, given one seed
a256       = 22925
