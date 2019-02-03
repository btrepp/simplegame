{-

Welcome to a Spago project!

You can edit this file as you like.

-}

let tiled = ../purescript-tiled/spago.dhall
let pixi = ../purescript-pixijs/spago.dhall

in { name =
    "gametest"
, dependencies =
    [ "effect", "console", "web-html", "affjax", "argonaut", "debug" ] 
    # tiled.dependencies
    # pixi.dependencies
, packages =
    ./packages.dhall 
    // tiled.packages 
    // pixi.packages
}
