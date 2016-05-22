module Main exposing (main)

import Game
import View

import Html.App


main : Program Never
main =
  Html.App.program
    { init = Game.init
    , update = Game.update
    , view = View.view
    , subscriptions = \_ -> Sub.none }
