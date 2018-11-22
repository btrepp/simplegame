    module Game.AssetLoader where

import Prelude

import Affjax (get, printResponseFormatError)
import Affjax.ResponseFormat (ResponseFormat(..))
import Affjax.ResponseFormat as ResponseFormat
import Blob (blobUri)
import Control.Monad.Except (ExceptT(..), except, runExceptT, throwError, withExceptT)
import Data.Argonaut (Json, class DecodeJson, decodeJson)
import Data.Either (Either)
import Data.List (List)
import Data.Map as Map
import Data.MediaType.Common as Responseformat
import Data.Tiled.File.Map (Map, externalTileSets)
import Data.Tiled.File.Tileset (Tileset(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Web.File.Blob (Blob)


type Assets = 
    { map :: Map
      , tilesets :: List Tileset
      , images :: Map.Map String Blob }

getJson :: String -> ExceptT String Aff Json
getJson url = 
        withExceptT printResponseFormatError
        $ ExceptT (_.body <$> get ResponseFormat.json url)

loadAsset :: forall asset . DecodeJson asset => 
                 String -> Aff (Either String asset)
loadAsset name = runExceptT do
    js <-  getJson name
    map <- except $ decodeJson js
    pure map

loadImage :: String -> ExceptT String Aff (Tuple String Blob)
loadImage name =
    withName 
    <$> 
    (withExceptT printResponseFormatError
    $ ExceptT (_.body <$> get ResponseFormat.blob url))
    where withName blob = Tuple name blob
          url = "maps/" <> name

imageName :: Tileset -> String
imageName (Tileset t) = t.image

loadMapAndAssets :: String -> Aff (Either String Assets) 
loadMapAndAssets name = runExceptT do
    map' <- ExceptT $ loadAsset $ path name
    let names = externalTileSets map'
    tilesets <- traverse (path>>>loadAsset>>>ExceptT) names 
    let imagenames =  map imageName tilesets
    images <- Map.fromFoldable <$> (traverse loadImage imagenames)
    pure $ {
        map : map', tilesets, images
    }

    where path :: String -> String
          path x = "maps/" <> x


-- | Converts the blobs into data uris
uriAssets :: Assets -> Effect (Map.Map String String)
uriAssets assets = 
    traverse (blobUri) assets.images