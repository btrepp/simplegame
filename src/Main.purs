module Main where

import Prelude

import Control.Monad.Except (runExceptT, ExceptT(..))
import Control.Monad.Maybe.Trans (lift)
import DOM (loaded)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Game.AssetLoader (loadMapAndAssets, uriAssets)
import Graphics.Pixi.FFI.Application (Application, _view, newApplication)
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)
import Debug.Trace as Debug


failMaybe :: forall a . String -> Maybe a ->  Either String a
failMaybe _ (Just a) = pure a
failMaybe err Nothing = Left err 

--- Builds the dom things we need
initStage :: Effect (Either String Application)
initStage = runExceptT do
  b <- ExceptT $ (failMaybe "No Body" <$> body')
  app <- lift $ app
  view <- lift $ view 
  _ <- lift $ appendChild (toNode view) (toNode b)
  pure app

  where 
    body' = window >>= document >>= body
    app = newApplication {height: 240, width: 320}
    view = app >>= _view


main :: Effect Unit
main = launchAff_ $ do
  loaded
  exn <- runExceptT $ do
      ap <- ExceptT $ liftEffect  initStage 
      level <- ExceptT $ loadMapAndAssets "level1.json"
      x <- liftEffect $ uriAssets level
      let _ = Debug.spy "x" x
      pure unit

  case exn of          
    Right unit -> liftEffect $ log "Success"
    Left e -> liftEffect $ log e 

