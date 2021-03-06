module Main where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (Except, ExceptT(..), runExcept, runExceptT)
import DOM (loaded)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Tiled (texturesFromFiles, mapFromFiles)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Console (log)
import Effect.Exception (Error, throw)
import Game.AssetLoader (loadMapAssets, uriAssets)
import Graphics.Pixi.FFI.Application (Application, _stage, _view, newApplication)
import Render (renderTiles)
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

throwExcept :: forall a. Except String a -> Effect a
throwExcept a = 
  case runExcept a of
    Left e -> throw e
    Right a -> pure a



body' :: HTMLDocument -> Effect HTMLElement
body' doc = runExceptEff
            $ ExceptT 
            (note "Couldn't find document body" 
            <$> body doc)

--- Builds the dom things we need
initStage :: forall r . {height:: Int, width :: Int | r} -> Effect Application
initStage ({height,width}) = do
  dombody <- window >>= document >>= body'
  pixi <-  newApplication {height,width}
  view <- _view pixi
  _ <- appendChild (toNode view) 
                   (toNode dombody)
  pure pixi

app :: Aff Unit
app = do
  loaded
  level <- loadMapAssets "level1.json"
  blobs <- liftEffect $ uriAssets level
  textures' <- liftEffect
               $ throwExcept
               $ texturesFromFiles level.map level.tilesets level.images
  map' <- liftEffect
            $ throwExcept
            $ mapFromFiles level.map textures'
  stage <- liftEffect $ initStage map'
  scene <- liftEffect $ _stage stage
  _ <- liftEffect $ renderTiles scene map'.tiles
  let _ = spy "LEVEL" level
  let _ = spy "STAGE" stage
  let _ = spy "BLOBS" blobs
  let _ = spy "TEXTURES" (textures')
  let _ = spy "TILES" (Array.fromFoldable map'.tiles)

  pure unit

crashed :: Error -> Aff Unit
crashed e = do
  -- | TODO, write to dom
  liftEffect $ log ("CRASH:" <> show e)

main :: Effect Unit
main = launchAff_ $ 
       catchError app crashed

