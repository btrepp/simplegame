    module Game.AssetLoader where

import Prelude

import Affjax (get, printResponseFormatError)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.List (List)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tiled.File.Map (Map, externalTilesets,decodeJsonMap)
import Data.Tiled.File.Tileset (Tileset, decodeJsonTileset)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (error)
import Effect.Aff (Aff)
import Web.File.Blob (Blob)
import Web.File.Url (createObjectURL)

-- | Represents all the assets for a map
-- | Its the map, external tilesets
-- | and image references
type MapAssets = 
      { map :: Map
      , tilesets :: Map.Map String Tileset
      , images :: Map.Map String Blob }

-- | Runs an ExceptT of Aff and throws
-- | the exceptions in the aff
run :: forall a . ExceptT String Aff a -> Aff a
run e = do
    r <-  runExceptT e 
    case r of 
      Left ex -> throwError $ error ex
      Right x  -> pure x

-- | Loads either a map or tileset
loadAsset :: forall asset .
             (Json -> Either String asset) 
             -> String
             -> Aff asset
loadAsset decode url= run do
    response <- withExceptT printResponseFormatError
                $ ExceptT 
                $ _.body
                <$> get ResponseFormat.json url
    parsed <- except $ decode response
    pure parsed

-- | Loads images as blobs
loadImage :: String -> Aff Blob
loadImage url = run do
    response <- withExceptT printResponseFormatError
                $ ExceptT
                $ _.body 
                <$> get ResponseFormat.blob url
    pure response                

-- | Walk the resources and load all the items
-- | Needed to render the map
loadMapAssets :: String -> Aff MapAssets
loadMapAssets name = do
    map' <- loadMap
    let externalTileNames = externalTilesets map'
    tilesets <- Map.fromFoldable 
                <$> traverse loadTile externalTileNames
    let imageNames = map _.image $ Map.values tilesets                    
    images <- Map.fromFoldable 
              <$> traverse loadImage' imageNames

    pure {
        map : map', tilesets, images
    }

    where path :: String -> String
          path x = "maps/" <> x
          
          loadMap = loadAsset decodeJsonMap 
                                $ path name
          loadTile name' = Tuple name' 
                           <$> (loadAsset decodeJsonTileset $ path name')
          loadImage' name' = Tuple name'
                            <$> (loadImage $ path name')

-- | Converts the blobs into data uris
-- | These can be loaded into the browser
uriAssets :: MapAssets -> Effect (Map.Map String String)
uriAssets assets = 
    traverse createObjectURL assets.images