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
import Vulnerability exposing (Vulnerability)

import Array
import Effects exposing (Effects)
import Random
import Random.Array
import Time exposing (Time)


type alias Model = Maybe GameState


type alias GameState =
  { hands : Seat.Each (List Card)
  , dealer : Seat
  , vulnerability : Vulnerability
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
        state = deal Seat.South Vulnerability.initial seed
      in
        (Just state, Effects.none)
    (NewDeal, Just oldState) ->
      let
        newVulnerability = Vulnerability.next oldState.vulnerability
        newState = bidForBots <| deal (Seat.next oldState.dealer) (Vulnerability.next oldState.vulnerability) oldState.seed
      in
        (Just newState, Effects.none)
    (Bid bid, Just oldState) ->
      let
        favorability = Vulnerability.favorability (currentBidder oldState) oldState.vulnerability
        annotated = Bidding.annotate Bidding.StandardAmerican.system favorability oldState.auction bid
        newState = bidForBots { oldState | auction = annotated :: oldState.auction }
      in
        (Just newState, Effects.none)
    (_, Nothing) ->
      {- TODO: indicate some kind of error for this impossible state -}
      (Nothing, Effects.none)


deal : Seat -> Vulnerability -> Random.Seed -> GameState
deal dealer vulnerability seed =
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
      , vulnerability = vulnerability
      , auction = []
      , seed = seed'
      }
  in
    state


bidForBots : GameState -> GameState
bidForBots oldState =
  let
    nextBidder = currentBidder oldState
  in
    if nextBidder /= Seat.South && Auction.isOpen (List.map .bid oldState.auction)
      then
        let
          favorability = Vulnerability.favorability nextBidder oldState.vulnerability
          (newBid, newSeed) =
            Bidding.choose Bidding.StandardAmerican.system favorability oldState.auction (Seat.lookup nextBidder oldState.hands) oldState.seed
          newAuction = newBid :: oldState.auction
        in
          bidForBots { oldState | auction = newAuction, seed = newSeed }
      else oldState


currentBidder : GameState -> Seat
currentBidder state = List.foldl (<|) state.dealer (List.repeat (List.length state.auction) Seat.next)
