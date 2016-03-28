module Game (Model, init, view, update) where

import Auction
import Card exposing (Card)
import Seat exposing (Seat)
import View

import Array
import Effects exposing (Effects)
import Html exposing (Html)
import Html.Events as Events
import Random
import Random.Array
import Signal
import Time exposing (Time)


type alias Model = Maybe GameState


type alias GameState =
  { hands : Seat.Each (List Card)
  , dealer : Seat
  , auction : List Auction.Bid
  , seed : Random.Seed
  }


type Action
  = Reseed Time
  | NewDeal


init : (Model, Effects Action)
init =
  ( Nothing, Effects.tick Reseed )


update : Action -> Model -> (Model, Effects Action)
update action model =
  case (action, model) of
    (Reseed time, _) ->
      let
        seed = Random.initialSeed (round time)
        state = deal Seat.South seed
      in
        (Just state, Effects.none)
    (NewDeal, Just oldState) ->
      let
        newState = deal (Seat.next oldState.dealer) oldState.seed
      in
        (Just newState, Effects.none)
    (_, Nothing) ->
      {- TODO: indicate some kind of error for this impossible state -}
      (Nothing, Effects.none)


deal : Seat -> Random.Seed -> GameState
deal dealer seed =
  let
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
      , dealer = dealer
      , auction = []
      , seed = seed'
      }
  in
    state


view : Signal.Address Action -> Model -> Html
view address model =
  case model of
    Just state -> viewState address state
    Nothing -> Html.div [] []


viewState : Signal.Address Action -> GameState -> Html
viewState address state =
  let
    emptyCell = Html.td [] []
    seatCell seat = Html.td [] [View.viewHand seat (Seat.lookup seat state.hands)]
  in
    Html.div []
      [ Html.table []
          [ Html.tr [] [ emptyCell, seatCell Seat.North, emptyCell ]
          , Html.tr [] [ seatCell Seat.West, emptyCell, seatCell Seat.East ]
          , Html.tr [] [ emptyCell, seatCell Seat.South, emptyCell ]
          ]
      , Html.button [ Events.onClick address NewDeal ] [ Html.text "Rage Quit" ]
      ]
