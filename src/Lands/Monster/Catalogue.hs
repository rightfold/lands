module Lands.Monster.Catalogue where

import Control.Monad (guard, join)
import Data.List (genericReplicate)
import Graphics.UI.SDL.Rect (Rect (..))
import Lands.Chance (d)
import Lands.Monster (MonsterSpecies (..))
import Lands.Render (Render (..))

import qualified Lands.Item.Catalogue as Item

data MonsterZombie = MonsterZombie
  { mZombieHealth  :: Word
  , mZombieHasBomb :: Bool
  }

msZombie :: MonsterSpecies MonsterZombie
msZombie = MonsterSpecies
  { msChance = 2`d`10
  , msInitial = \r ->
      MonsterZombie { mZombieHealth = 100 + r
                    , mZombieHasBomb = r > 18 }
  , msUpdate = \_ m -> m
  , msHealth = mZombieHealth
  , msDrops = \r m ->
      join [ guard (r >  2) *> genericReplicate (r `div` 4) Item.iStick
           , guard (r > 10) *> genericReplicate (r `div` 8) Item.iCloth
           , guard (mZombieHasBomb m) *> pure Item.iBomb ]
  , msRender = \_ -> [RenderSprite (Rect 0 0 24 24) (Rect 0 0 24 24)]
  }
