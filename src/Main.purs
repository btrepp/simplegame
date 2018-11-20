module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import DOM (loaded)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tiled.File.Map (Map)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Game.AssetLoader (loadAsset)
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


level1 :: Aff (Either String Map)
level1 = loadAsset "maps/level1.json"

main :: Effect Unit
main = launchAff_ $ do
  image <- liftEffect $ fromImage "maps/tmw_desert_spacing.png"
  loaded
  app <- liftEffect $ runMaybeT $ initStage 
  _ <- level1
  case app of 
   Just a -> do
    stage <- liftEffect $ _stage a

    sprite1 <- liftEffect $ loadSprite image 
                  {x: 0.0,y:0.0,width:32.0,height:32.0}
    sprite2 <- liftEffect $ loadSprite image 
                  {x: 0.0,y:32.0,width:32.0,height:32.0}
    liftEffect $ setPositionX sprite2 32.0
    liftEffect $ addChild sprite1 stage
    liftEffect $ addChild sprite2 stage
    liftEffect $ setPositionX sprite2 64.0
   Nothing -> 
        liftEffect $ log "No body"    

  liftEffect $ log "Hello sailor!"
