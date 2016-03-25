module Main (main) where

import Game

import Html exposing (Html)
import StartApp


main : Signal Html
main = app.html


app : StartApp.App Game.Model
app =
  StartApp.start { init = Game.init, view = Game.view, update = Game.update, inputs = [] }
