    module Game.AssetLoader where

import Prelude
import Affjax (get, printResponseFormatError)
import Affjax.ResponseFormat (json)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Data.Argonaut (Json, class DecodeJson, decodeJson)
import Data.Either (Either)
import Effect.Aff (Aff)

getJson :: String -> ExceptT String Aff Json
getJson url = 
        withExceptT printResponseFormatError
        $ ExceptT (_.body <$> get json url)

loadAsset :: forall asset . DecodeJson asset => 
                 String -> Aff (Either String asset)
loadAsset name = runExceptT do
    js <-  getJson name
    map <- except $ decodeJson js
    pure map


