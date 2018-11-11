module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Graphics.Pixijs.FFI.Application (newApplication, _view)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode, body)
import Web.HTML.HTMLElement (toParentNode,toNode)
import Web.HTML.Window (document)
import Web.DOM.Node(appendChild)
import Data.Maybe(Maybe(..))
main :: Effect Unit
main = do
  doc <- window >>= document
  b <- (body doc)
  case b of
    Just x -> do
      let b = toNode x
      app <- newApplication  $ {height : 320, width : 240}
      let view = _view app
      let v = toNode view
      _ <- appendChild v b
      pure unit
    Nothing -> pure unit

  log "Hello sailor!"
