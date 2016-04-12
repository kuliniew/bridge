module Main (main) where

import Game
import View

import Effects
import Html exposing (Html)
import StartApp
import Task exposing (Task)


main : Signal Html
main =
  app.html


port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks


app : StartApp.App Game.Model
app =
  StartApp.start { init = Game.init, view = View.view, update = Game.update, inputs = [] }
