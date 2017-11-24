{-# LANGUAGE RecordWildCards #-}
{-|
Module: Generate
Description: Core logic for password generation.
-}
module Generate
    (generate, Options(..), upper, lower, letter, digit, symbol)
  where
import qualified Data.ByteString as B

import Control.Monad (replicateM, when)
import Crypto.RNG    (CryptoRNG, newCryptoRNGState, random, runCryptoRNGT)
import Data.Bits
import Data.Default  (Default(def))
import Data.Word

import qualified Data.Set    as S
import qualified Data.Vector as V

-- | Set of lowercase ascii letters
lower = S.fromList ['a'..'z']

-- | Set of uppercase ascii letters
upper = S.fromList ['A'..'Z']

-- | Set of ascii letters
letter = upper `S.union` lower

-- | Set of digits ('0' through '9')
digit = S.fromList ['0'..'9']

-- | Set of special characters
symbol = S.fromList "~`!@#$%^&*(){}[]-+=_,.<>?/;:|\\'\""

-- | @doUntil p m@ executes @m@ repeatedly, until it returns a value that
-- satisfies the predicate p.
doUntil :: Monad m => (a -> Bool) -> m a -> m a
doUntil p m = do
    ret <- m
    if p ret
        then return ret
        else doUntil p m

-- | @getUniform max@ gets a uniformly random value in the range [0, max).
getUniform :: CryptoRNG m => Word8 -> m Word8
getUniform max = doUntil (< max) $ do
    byte <- random
    return $ byte .&. maskFor max
  where
    -- compute the most restrictive mask that's still greater than max.
    -- this will statistically reduce the likelyhood that we'll need to reject a
    -- result.
    maskFor :: Word8 -> Word8
    maskFor n
        -- I'm sure there's a less dumb way to compute this, but doing every
        -- case is straightforward:
        | n < 2     = 1
        | n < 4     = 3
        | n < 8     = 7
        | n < 16    = 15
        | n < 32    = 31
        | n < 64    = 63
        | n < 128   = 127
        | otherwise = 255



-- | Select a random element from the vector. If the vector's length cannot
-- be represented by a Word8, an error occurs.
choose :: CryptoRNG m => V.Vector a -> m a
choose vec = do
    let len = V.length vec
    when (len > 255) $ error "too many elements"
    index <- getUniform (fromIntegral len)
    return (vec V.! fromIntegral index)


-- | Options for the password generator.
data Options = Options
    { charSets :: [S.Set Char] -- ^ Sets of legal characters for the password.
                               -- The generated password will have at least
                               -- one character from each set.
    , size     :: Int -- ^ The length of the password, in characters.
    }

instance Default Options where
    def = Options { charSets = [upper, lower, digit, symbol]
                  , size = 20
                  }

getPassword :: CryptoRNG m => Options -> m String
getPassword opts@Options{..} =
    doUntil isValid $ replicateM size $ choose vec
  where
    vec = V.fromList $ S.toList $ S.unions charSets
    isValid pass   = and $ [pass `has`      set | set <- charSets]
    pass `has` set = or  $ [char `S.member` set | char <- pass]

-- | Generate a cryptographically random password, using the provided options.
generate :: Options -> IO String
generate opts = do
    state <- newCryptoRNGState
    runCryptoRNGT state (getPassword opts)
