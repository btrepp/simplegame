module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import DOM (loaded)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tiled.File.Map (Map)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Game.AssetLoader (loadMapAndAssets,Assets)
import Graphics.PIXI.FFI.Rectangle (newRectangle)
import Graphics.Pixi.FF.Texture (newTexture)
import Graphics.Pixi.FFI.Application (Application, newApplication, _view, _stage)
import Graphics.Pixi.FFI.BaseTexture (fromImage, Image)
import Graphics.Pixi.FFI.Container (addChild)
import Graphics.Pixi.FFI.Sprite (Sprite, newSprite, setPositionX)
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)
import Debug.Trace as Debug

initStage :: MaybeT Effect Application
initStage = do
  body <- toNode <$> (MaybeT $ (window >>= document >>= body))
  app <- lift $ newApplication {height: 240, width : 320}
  view <- toNode <$> (lift $ _view app)
  _ <- lift $ appendChild view body
  pure app

loadSprite :: Image 
                     -> {x::Number
                        ,y::Number
                        ,width::Number
                        ,height::Number} 
                      -> Effect Sprite
loadSprite imageSheet opts = do
  rect <- newRectangle opts
  texture <- newTexture imageSheet rect
  sprite <- newSprite texture
  pure sprite


level1 :: Aff (Either String Assets)
level1 = loadMapAndAssets "level1.json"

main :: Effect Unit
main = launchAff_ $ do
  loaded
  app <- liftEffect $ runMaybeT $ initStage 

  level <- level1
  case level of
    Right l ->do
         let _ = Debug.spy "M" l 
         pure unit
    Left e -> liftEffect $ log e


  case app of 
   Just a -> do
    stage <- liftEffect $ _stage a
    pure unit

   Nothing -> 
        liftEffect $ log "No body"    

  liftEffect $ log "Hello sailor!"
