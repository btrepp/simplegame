module Graphics.Pixijs.FFI.Application 
            (Application,newApplication,_view) where

import Data.Function.Uncurried (Fn1, runFn1)
import Web.HTML(HTMLElement)
import Effect (Effect)

type ApplicationConfig =
    { height :: Int
    , width :: Int }
foreign import data Application :: Type

foreign import newApplicationImpl ::Fn1 ApplicationConfig (Effect Application)
newApplication :: ApplicationConfig-> Effect Application
newApplication a= runFn1 newApplicationImpl a 

foreign import viewImpl :: Fn1 Application HTMLElement
_view :: Application -> HTMLElement
_view a = runFn1 viewImpl a
