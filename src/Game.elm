module Game (Model, init, view, update) where

import Card exposing (Card)
import Seat
import View

import Array
import Effects exposing (Effects)
import Html exposing (Html)
import Random
import Random.Array
import Signal
import Time exposing (Time)


type alias Model = Maybe GameState


type alias GameState =
  { hands : Seat.Each (List Card)
  , seed : Random.Seed
  }


type Action
  = Reseed Time


init : (Model, Effects Action)
init =
  ( Nothing, Effects.tick Reseed )


update : Action -> Model -> (Model, Effects Action)
update action _ =
  case action of
    Reseed time ->
      let
        seed = Random.initialSeed (round time)
        (shuffled, seed') = Random.generate (Random.Array.shuffle Card.deck) seed
        cardsPerHand = Array.length shuffled // 4
        takeCards n = Array.toList (Array.slice (cardsPerHand * n) (cardsPerHand * (n + 1)) shuffled)
        hands =
          { west = takeCards 0
          , north = takeCards 1
          , east = takeCards 2
          , south = takeCards 3
          }
        state =
          { hands = hands
          , seed = seed'
          }
      in
        (Just state, Effects.none)


view : Signal.Address Action -> Model -> Html
view address model =
  case model of
    Just state -> viewState state
    Nothing -> Html.div [] []


viewState : GameState -> Html
viewState state =
  let
    emptyCell = Html.td [] []
    seatCell seat = Html.td [] [View.viewHand seat (Seat.lookup seat state.hands)]
  in
    Html.table []
      [ Html.tr [] [ emptyCell, seatCell Seat.North, emptyCell ]
      , Html.tr [] [ seatCell Seat.West, emptyCell, seatCell Seat.East ]
      , Html.tr [] [ emptyCell, seatCell Seat.South, emptyCell ]
      ]
