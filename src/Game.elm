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


type alias Model = Maybe GameState


type alias GameState =
  { system : Bidding.System
  , hands : Seat.Each (List Card)
  , dealer : Seat
  , vulnerability : Vulnerability
  , auction : List Bidding.AnnotatedBid
  , explained : Maybe Bidding.AnnotatedBid
  }


type Msg
  = NewDeal (List (List Card))
  | BidFromBot Bidding.AnnotatedBid
  | BidFromHuman Auction.Bid
  | Explain (Maybe Bidding.AnnotatedBid)
  | RageQuit


init : (Model, Cmd Msg)
init =
  ( Nothing, Random.generate NewDeal Card.deal )


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case (action, model) of
    (NewDeal hands, Nothing) ->
      bidForBot <| deal Seat.South Vulnerability.initial hands
    (NewDeal hands, Just oldState) ->
      bidForBot <| deal (Seat.next oldState.dealer) (Vulnerability.next oldState.vulnerability) hands
    (BidFromBot bid, Just oldState) ->
      let
        newState = { oldState | auction = bid :: oldState.auction }
      in
        bidForBot newState
    (BidFromHuman bid, Just oldState) ->
      let
        favorability = Vulnerability.favorability (currentBidder oldState.dealer oldState.auction) oldState.vulnerability
        annotated = Bidding.annotate oldState.system favorability oldState.auction bid
        newState = { oldState | auction = annotated :: oldState.auction }
      in
        bidForBot newState
    (Explain explained, Just oldState) ->
      let
        newState = { oldState | explained = explained }
      in
        (Just newState, Cmd.none)
    (RageQuit, _) ->
      (model, Random.generate NewDeal Card.deal)
    (_, Nothing) ->
      {- TODO: indicate some kind of error for this impossible state -}
      (Nothing, Cmd.none)


deal : Seat -> Vulnerability -> List (List Card) -> GameState
deal dealer vulnerability dealt =
  let
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
      , explained = Nothing
      }
  in
    state


bidForBot : GameState -> (Model, Cmd Msg)
bidForBot state =
  let
    nextBidder = currentBidder state.dealer state.auction
  in
    if nextBidder /= Seat.South && Auction.isOpen (List.map .bid state.auction)
      then
        let
          favorability = Vulnerability.favorability nextBidder state.vulnerability
        in
          (Just state, Random.generate BidFromBot <| Bidding.choose state.system favorability state.auction (Seat.lookup nextBidder state.hands))
      else
        (Just state, Cmd.none)


currentBidder : Seat -> List Bidding.AnnotatedBid -> Seat
currentBidder dealer auction = List.foldl (<|) dealer (List.repeat (List.length auction) Seat.next)
