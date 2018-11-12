module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Graphics.PIXI.FFI.Rectangle (newRectangle)
import Graphics.Pixi.FF.Texture (newTexture)
import Graphics.Pixi.FFI.Application (Application, newApplication, _view)
import Graphics.Pixi.FFI.BaseTexture (fromImage)
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)



initStage :: Effect (Maybe Application)
initStage = do
  b <- window >>= document >>= body
  case b of 
    Nothing -> pure Nothing
    Just b2 -> do
        app <- newApplication  {height : 320, width : 240}
        view <- toNode <$> _view app
        _ <- appendChild view (toNode b2)
        pure (Just app)

main :: Effect Unit
main = do
  imageSheet <- fromImage "maps/tmw_desert_spacing.png"
  rect <- newRectangle {x:0.0,y:0.0,width:32.0,height:32.0}
  sprite <- newTexture imageSheet rect
  app <-  initStage

  log "Hello sailor!"
