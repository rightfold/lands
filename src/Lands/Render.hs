module Lands.Render
  ( Render (..)
  , runRender
  ) where

import Control.Monad (void)
import Graphics.UI.SDL.Rect (Rect (..))
import Graphics.UI.SDL.Types (Surface)
import Graphics.UI.SDL.Video (blitSurface)

-- | A rendering command.
data Render
  = RenderSprite !Rect !Rect
  deriving (Show)

-- | Interpret a rendering command to SDL I/O.
runRender :: Surface -> Surface -> Render -> IO ()
runRender source target (RenderSprite sourceRect targetRect) =
  void $ blitSurface source (Just sourceRect) target (Just targetRect)
