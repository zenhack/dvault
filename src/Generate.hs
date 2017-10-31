{-# LANGUAGE RecordWildCards #-}
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

lower = S.fromList ['a'..'z']
upper = S.fromList ['A'..'Z']
letter = upper `S.union` lower
digit = S.fromList ['0'..'9']
symbol = S.fromList "~`!@#$%^&*(){}[]-+=_,.<>?/;:|\\'\""

-- | @untilM p m@ executes @m@ repeatedly, until it returns a value that
-- satisfies the predicate p.
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p m = do
    ret <- m
    if p ret
        then return ret
        else untilM p m

-- | @getUniform max@ gets a uniformly random value in the range [0, max).
getUniform :: CryptoRNG m => Word8 -> m Word8
getUniform max = untilM (< max) $ do
    byte <- random
    return $ byte .&. maskFor max
  where
    maskFor :: Word8 -> Word8
    maskFor n
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
                               -- one character from each set,
    , size     :: Int -- ^ The length of the password, in characters.
    }

instance Default Options where
    def = Options { charSets = [upper, lower, digit, symbol]
                  , size = 20
                  }

getPassword :: CryptoRNG m => Options -> m String
getPassword opts@Options{..} =
    untilM isValid $ replicateM size $ choose vec
  where
    vec = V.fromList $ S.toList $ S.unions charSets
    isValid pass   = and $ [pass `has`      set | set <- charSets]
    pass `has` set = or  $ [char `S.member` set | char <- pass]

generate :: Options -> IO String
generate opts = do
    state <- newCryptoRNGState
    runCryptoRNGT state (getPassword opts)
