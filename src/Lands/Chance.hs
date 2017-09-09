module Lands.Chance
  ( -- * Dice
    Dice (..)
  , d
  , roll
  ) where

import Control.Monad.Random.Class (MonadRandom (getRandomR))

-- | @Dice n m@ represents @n@ dice each with @m@ faces.
data Dice = Dice !Word !Word
  deriving (Eq, Show)

-- | Infix operator for @Dice@, so you can write @2`d`6@.
d :: Word -> Word -> Dice
d = Dice
{-# INLINE d #-}

roll :: MonadRandom m => Dice -> m Word
roll (Dice n m) = getRandomR (n, n * m)
{-# INLINE roll #-}
