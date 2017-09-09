module Lands.Monster
  ( MonsterSpecies (..)
  , Monster (..)
  , createMonster
  , updateMonster
  , renderMonster
  , isMonsterAlive
  ) where

import Control.Monad.Random.Class (MonadRandom)
import Data.Binary.Builder (Builder)
import Lands.Chance (Dice, roll)

-- | How does a monster behave?
data MonsterSpecies a = MonsterSpecies
  { msChance  :: Dice           -- ^ Dice that are rolled for state operations.
  , msInitial :: Word -> a      -- ^ Initial state given dice roll.
  , msUpdate  :: Word -> a -> a -- ^ Update state given dice roll.
  , msHealth  :: a -> Word      -- ^ Health of this monster.
  , msRender  :: a -> Builder   -- ^ Render this monster.
  }

-- | An instance of a 'MonsterSpecies', with some state.
data Monster :: * where
  Monster :: MonsterSpecies a -> a -> Monster

-- | Create a new monster of the given species.
createMonster :: MonadRandom m => MonsterSpecies a -> m Monster
createMonster ms = Monster ms . msInitial ms <$> roll (msChance ms)

-- | Update an existing monster.
updateMonster :: MonadRandom m => Monster -> m Monster
updateMonster (Monster ms m) = Monster ms . flip (msUpdate ms) m <$> roll (msChance ms)

-- | Render an existing monster.
renderMonster :: Monster -> Builder
renderMonster (Monster ms m) = msRender ms m

-- | Does the given monster have a nonzero health?
isMonsterAlive :: Monster -> Bool
isMonsterAlive (Monster ms m) = msHealth ms m > 0
