module Lands.Monster
  ( MonsterSpecies (..)
  , Monster (..)
  , createMonster
  , updateMonster
  , renderMonster
  , monsterDrops
  , isMonsterAlive
  ) where

import Control.Monad.Random.Class (MonadRandom)
import Data.ByteString.Builder (Builder)
import Lands.Chance (Dice, roll)
import Lands.Item (Item)

-- | How does a monster behave?
data MonsterSpecies a = MonsterSpecies
  { msChance  :: Dice           -- ^ Dice that are rolled for state operations.
  , msInitial :: Word -> a      -- ^ Initial state given a dice roll.
  , msUpdate  :: Word -> a -> a -- ^ Update state given a dice roll.
  , msHealth  :: a -> Word      -- ^ Health of this monster.
  , msDrops   :: Word -> a -> [Item]
                                -- ^ Items dropped given a dice roll.
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

-- | Items this monster would drop upon death.
monsterDrops :: MonadRandom m => Monster -> m [Item]
monsterDrops (Monster ms m) = flip (msDrops ms) m <$> roll (msChance ms)

-- | Does the given monster have a nonzero health?
isMonsterAlive :: Monster -> Bool
isMonsterAlive (Monster ms m) = msHealth ms m > 0
