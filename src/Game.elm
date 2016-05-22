module Game exposing
  ( Model
  , GameState
  , Msg (..)
  , init
  , update
  , currentBidder
  )

import Auction
import Bidding
import Bidding.StandardAmerican
import Card exposing (Card)
import Seat exposing (Seat)
import Vulnerability exposing (Vulnerability)

import Random
import Task
import Time exposing (Time)


type alias Model = Maybe GameState


type alias GameState =
  { system : Bidding.System
  , hands : Seat.Each (List Card)
  , dealer : Seat
  , vulnerability : Vulnerability
  , auction : List Bidding.AnnotatedBid
  , seed : Random.Seed
  , explained : Maybe Bidding.AnnotatedBid
  }


type Msg
  = Reseed Time
  | NewDeal
  | Bid Auction.Bid
  | Explain (Maybe Bidding.AnnotatedBid)


init : (Model, Cmd Msg)
init =
  ( Nothing, Task.perform Reseed Reseed Time.now )


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case (action, model) of
    (Reseed time, _) ->
      let
        seed = Random.initialSeed (round time)
        state = deal Seat.South Vulnerability.initial seed
      in
        (Just state, Cmd.none)
    (NewDeal, Just oldState) ->
      let
        newVulnerability = Vulnerability.next oldState.vulnerability
        newState = bidForBots <| deal (Seat.next oldState.dealer) (Vulnerability.next oldState.vulnerability) oldState.seed
      in
        (Just newState, Cmd.none)
    (Bid bid, Just oldState) ->
      let
        favorability = Vulnerability.favorability (currentBidder oldState.dealer oldState.auction) oldState.vulnerability
        annotated = Bidding.annotate oldState.system favorability oldState.auction bid
        newState = bidForBots { oldState | auction = annotated :: oldState.auction }
      in
        (Just newState, Cmd.none)
    (Explain explained, Just oldState) ->
      let
        newState = { oldState | explained = explained }
      in
        (Just newState, Cmd.none)
    (_, Nothing) ->
      {- TODO: indicate some kind of error for this impossible state -}
      (Nothing, Cmd.none)


deal : Seat -> Vulnerability -> Random.Seed -> GameState
deal dealer vulnerability seed =
  let
    (dealt, seed') = Random.step Card.deal seed
    hands =
      case dealt of
        [west, north, east, south] ->
          { west = west
          , north = north
          , east = east
          , south = south
          }
        _ -> Debug.crash "Card.deal didn't return exactly four hands!"
    state =
      { system = Bidding.StandardAmerican.system
      , hands = hands
      , dealer = dealer
      , vulnerability = vulnerability
      , auction = []
      , seed = seed'
      , explained = Nothing
      }
  in
    state


bidForBots : GameState -> GameState
bidForBots oldState =
  let
    nextBidder = currentBidder oldState.dealer oldState.auction
  in
    if nextBidder /= Seat.South && Auction.isOpen (List.map .bid oldState.auction)
      then
        let
          favorability = Vulnerability.favorability nextBidder oldState.vulnerability
          (newBid, newSeed) =
            Bidding.choose oldState.system favorability oldState.auction (Seat.lookup nextBidder oldState.hands) oldState.seed
          newAuction = newBid :: oldState.auction
        in
          bidForBots { oldState | auction = newAuction, seed = newSeed }
      else oldState


currentBidder : Seat -> List Bidding.AnnotatedBid -> Seat
currentBidder dealer auction = List.foldl (<|) dealer (List.repeat (List.length auction) Seat.next)
