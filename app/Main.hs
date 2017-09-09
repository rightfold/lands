module Main
  ( main
  ) where

import Control.Concurrent (threadDelay)
import Data.Foldable (traverse_)
import Lands.Monster (createMonster, renderMonster)
import Lands.Monster.Catalogue (msZombie)
import Lands.Render (runRender)

import qualified Graphics.UI.SDL.General as SG
import qualified Graphics.UI.SDL.Types as ST
import qualified Graphics.UI.SDL.Video as SV

main :: IO ()
main = SG.withInit [SG.InitVideo] main'

main' :: IO ()
main' = do
  spritemap <- SV.loadBMP "assets/spritemap.bmp"
  window <- SV.setVideoMode 640 480 16 [ST.SWSurface]

  mZombie <- createMonster msZombie
  traverse_ (runRender spritemap window)
            (renderMonster mZombie)

  SV.flip window

  threadDelay 1000000

  pure ()
