module Game (Model, init, view, update) where

import Action exposing (Action)
import Auction
import Bidding
import Bidding.StandardAmerican
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


type alias Model = Maybe GameState


type alias GameState =
  { hands : Seat.Each (List Card)
  , dealer : Seat
  , auction : List Bidding.AnnotatedBid
  , seed : Random.Seed
  }


init : (Model, Effects Action)
init =
  ( Nothing, Effects.tick Action.Reseed )


update : Action -> Model -> (Model, Effects Action)
update action model =
  case (action, model) of
    (Action.Reseed time, _) ->
      let
        seed = Random.initialSeed (round time)
        state = deal Seat.South seed
      in
        (Just state, Effects.none)
    (Action.NewDeal, Just oldState) ->
      let
        newState = bidForBots <| deal (Seat.next oldState.dealer) oldState.seed
      in
        (Just newState, Effects.none)
    (Action.Bid bid, Just oldState) ->
      let
        annotated = Bidding.annotate Bidding.StandardAmerican.system oldState.auction bid
        newState = bidForBots { oldState | auction = annotated :: oldState.auction }
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


bidForBots : GameState -> GameState
bidForBots oldState =
  let
    nextBidder = List.foldl (<|) oldState.dealer (List.repeat (List.length oldState.auction) Seat.next)
  in
    if nextBidder /= Seat.South && Auction.isOpen (List.map .bid oldState.auction)
      then
        let
          (newBid, newSeed) =
            Bidding.choose Bidding.StandardAmerican.system oldState.auction (Seat.lookup nextBidder oldState.hands) oldState.seed
          newAuction = newBid :: oldState.auction
        in
          bidForBots { oldState | auction = newAuction, seed = newSeed }
      else oldState


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
      , View.viewAuction address state.dealer state.auction
      , Html.button [ Events.onClick address Action.NewDeal ] [ Html.text "Rage Quit" ]
      ]
