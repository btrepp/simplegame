module Main where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import DOM (loaded)
import Data.Either (Either(..), note)
import Data.Tiled.File.Map (solveTilesets)
import Debug.Trace (spy)
import Data.Tiled (textures)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, throw)
import Game.AssetLoader (loadMapAssets, uriAssets)
import Graphics.Pixi.FFI.Application (Application, _view, newApplication)
import Web.DOM.Node (appendChild)
import Web.HTML (HTMLDocument, HTMLElement, window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)

-- | Runs an ExceptT in something that can
-- | throw
runExceptEff :: forall a 
              . ExceptT String Effect a 
              -> Effect a
runExceptEff m = 
  runExceptT m 
  >>= failLeft
  where failLeft (Left a) = throw a
        failLeft (Right a) = pure a

body' :: HTMLDocument -> Effect HTMLElement
body' doc = runExceptEff
            $ ExceptT 
            (note "Couldn't find document body" 
            <$> body doc)

--- Builds the dom things we need
initStage :: Effect Application
initStage = do
  dombody <- window >>= document >>= body'
  pixi <-  newApplication {height:240,width:320}
  view <- _view pixi
  _ <- appendChild (toNode view) 
                   (toNode dombody)
  pure pixi

app :: Aff Unit
app = do
  loaded
  level <- loadMapAssets "level1.json"
  stage <- liftEffect $ initStage
  blobs <- liftEffect $ uriAssets level
  let tilesets = solveTilesets level.map level.tilesets
  let textures' = textures level.map level.tilesets

  let _ = spy "LEVEL" level
  let _ = spy "STAGE" stage
  let _ = spy "BLOBS" blobs
  let _ = spy "TILESETS" tilesets
  let _ = spy "TEXTURES" (show $ textures')

  pure unit

crashed :: Error -> Aff Unit
crashed e = do
  -- | TODO, write to dom
  liftEffect $ log $ show e

main :: Effect Unit
main = launchAff_ $ 
       catchError app crashed

