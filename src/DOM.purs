module DOM where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)

-- | Handy to defer running of scripts until we are ready
loaded :: Aff Unit
loaded = 
    makeAff work
    where 
        work :: (Either Error Unit -> Effect Unit) -> Effect Canceler
        work done = do 
          let onEvent _ = done $ pure unit
          listener <- eventListener onEvent
          target <- toEventTarget <$> (window >>= document)
          addEventListener domcontentloaded listener false target
          pure mempty