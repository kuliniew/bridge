module Game (Model, init, view, update) where

import Card exposing (Card)
import Seat exposing (Seat)

import Effects exposing (Effects)
import Html exposing (Html)
import Signal


type alias Model =
  { hands : Seat.Each (List Card)
  }

type Action
  = Placeholder


init : (Model, Effects Action)
init =
  let
    hands = { west = [], north = [], east = [], south = [] }
    model = { hands = hands }
  in
    (model, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
  (model, Effects.none)


view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [] (List.map (\seat -> viewHand seat (Seat.lookup seat model.hands)) [Seat.West, Seat.North, Seat.East, Seat.South])


viewHand : Seat -> List Card -> Html
viewHand seat hand =
  Html.div [] [ Html.text (toString seat) ]
