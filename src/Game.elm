module Game
  ( Model
  , GameState
  , Action (..)
  , init
  , update
  ) where

import Auction
import Bidding
import Bidding.StandardAmerican
import Card exposing (Card)
import Seat exposing (Seat)

import Array
import Effects exposing (Effects)
import Random
import Random.Array
import Time exposing (Time)


type alias Model = Maybe GameState


type alias GameState =
  { hands : Seat.Each (List Card)
  , dealer : Seat
  , auction : List Bidding.AnnotatedBid
  , seed : Random.Seed
  }


type Action
  = Reseed Time
  | NewDeal
  | Bid Auction.Bid


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
        newState = bidForBots <| deal (Seat.next oldState.dealer) oldState.seed
      in
        (Just newState, Effects.none)
    (Bid bid, Just oldState) ->
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
