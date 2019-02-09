module Render 
    where

import Prelude

import Data.Int (toNumber)
import Data.Tiled.Tile (Tile)
import Data.Traversable (class Traversable, traverse_)
import Effect (Effect)
import Graphics.PIXI.FFI.Rectangle (newRectangle)
import Graphics.Pixi.FF.Texture (newTexture)
import Graphics.Pixi.FFI.BaseTexture (fromImage)
import Graphics.Pixi.FFI.Container (Container, addChild)
import Graphics.Pixi.FFI.Sprite (newSprite, setPositionX,setPositionY)
import Web.File.Url (createObjectURL)


renderTiles :: forall t .
               Traversable t 
               => Container -> t Tile -> Effect Unit
renderTiles scene tiles =
    traverse_ renderTile tiles
    where 
        renderTile :: Tile -> Effect Unit
        renderTile (tile) = do
            imageUrl <- createObjectURL tile.texture.image
            image <- fromImage imageUrl
            rectangle <- newRectangle { x: toNumber tile.texture.offsetX
                                      , y: toNumber tile.texture.offsetY
                                      , width: toNumber tile.texture.width
                                      , height : toNumber tile.texture.height
            }
            text <- newTexture image rectangle
            sprite <- newSprite text
            setPositionX sprite (toNumber tile.x)
            setPositionY sprite (toNumber tile.y)
            addChild sprite scene