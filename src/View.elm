module View
  ( view
  , viewState
  , viewHand
  , viewSuit
  , viewAuction
  ) where

import Auction
import Bidding
import Card exposing (Card)
import Game exposing (Action)
import Seat exposing (Seat)
import Vulnerability exposing (Vulnerability)

import Debug
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import List.Extra


view : Signal.Address Action -> Game.Model -> Html
view address model =
  case model of
    Just state -> viewState address state
    Nothing -> Html.div [] []


viewState : Signal.Address Action -> Game.GameState -> Html
viewState address state =
  let
    emptyCell = Html.td [] []
    seatCell seat = Html.td [] [viewHand seat (Seat.lookup seat state.hands)]
  in
    Html.div []
      [ viewVulnerability state.vulnerability
      , Html.table []
          [ Html.tr [] [ emptyCell, seatCell Seat.North, emptyCell ]
          , Html.tr [] [ seatCell Seat.West, emptyCell, seatCell Seat.East ]
          , Html.tr [] [ emptyCell, seatCell Seat.South, emptyCell ]
          ]
      , viewAuction address state.dealer state.auction
      , Html.button [ Events.onClick address Game.NewDeal ] [ Html.text "Rage Quit" ]
      ]


viewHand : Seat -> List Card -> Html
viewHand seat hand =
  Html.div []
    [ Html.text (toString seat)
    , Html.ul [Attr.class "suits"] (List.map (viewSuit hand) [Card.Spades, Card.Hearts, Card.Diamonds, Card.Clubs])
    ]


viewSuit : List Card -> Card.Suit -> Html
viewSuit hand suit =
  let
    cards =
      List.filter (\card -> card.suit == suit) hand
      |> List.sortWith Card.rankDescending
    contents =
      if List.isEmpty cards
        then [Html.li [] [Html.text "—"]]
        else List.map viewRank cards
  in
    Html.li [suitClass suit] [ Html.ul [Attr.class "ranks"] contents ]


viewRank : Card -> Html
viewRank card =
  let
    value =
      case card.rank of
        Card.Ace -> "A"
        Card.King -> "K"
        Card.Queen -> "Q"
        Card.Jack -> "J"
        Card.Ten -> "10"
        Card.Nine -> "9"
        Card.Eight -> "8"
        Card.Seven -> "7"
        Card.Six -> "6"
        Card.Five -> "5"
        Card.Four -> "4"
        Card.Three -> "3"
        Card.Two -> "2"
  in
    Html.li [] [Html.text value]


viewAuction : Signal.Address Action -> Seat -> List Bidding.AnnotatedBid -> Html
viewAuction address dealer annotatedAuction =
  let
    auction =
      List.map .bid annotatedAuction
    headerCell name =
      Html.td [] [Html.text name]
    bidCell bid =
      Html.td [] (viewBid bid)
    blankCell =
      Html.td [] []
    nullCell =
      Html.td [] [Html.text "—"]
    nullCells =
      case dealer of
        Seat.West -> []
        Seat.North -> [nullCell]
        Seat.East -> [nullCell, nullCell]
        Seat.South -> [nullCell, nullCell, nullCell]
    makeBidCells =
      if Auction.isOpen auction
        then [makeBidCell address auction]
        else []
    allCells = nullCells ++ List.reverse (List.map bidCell auction) ++ makeBidCells
    row cells =
      Html.tr [] cells
  in
    Html.table []
      [ Html.thead []
          [ Html.tr [] (List.map headerCell ["West", "North", "East", "South"])
          ]
      , Html.tbody [] (List.map row <| cluster blankCell 4 allCells)
      ]


makeBidCell : Signal.Address Action -> List Auction.Bid -> Html
makeBidCell address auction =
  let
    legalBids =
      Auction.legalBids auction
    lookupLegalBid index =
      case List.Extra.getAt legalBids (index - 1) of
        Just bid -> bid
        Nothing -> Debug.crash "failed to lookup the selected bid"
    getSelectedIndex =
      Json.Decode.at ["target", "selectedIndex"] Json.Decode.int
    onSelect =
      Events.on "change" getSelectedIndex (Signal.message address << Game.Bid << lookupLegalBid)
    header =
      Html.option [] [Html.text "Make a bid..."]
    toChoice bid =
      Html.option [Events.onClick address (Game.Bid bid)] (viewBid bid)
    choices =
      List.map toChoice legalBids
  in
    Html.td [] [Html.select [onSelect] (header :: choices)]


suitClass : Card.Suit -> Html.Attribute
suitClass suit =
  let
    class =
      case suit of
        Card.Clubs -> "clubs"
        Card.Diamonds -> "diamonds"
        Card.Hearts -> "hearts"
        Card.Spades -> "spades"
  in
    Attr.class class


viewBid : Auction.Bid -> List Html
viewBid bid =
  let
    suitText suit =
      case suit of
        Card.Clubs -> "♣"
        Card.Diamonds -> "♦"
        Card.Hearts -> "♥"
        Card.Spades -> "♠"
    viewTrump trump =
      case trump of
        Just suit -> Html.span [suitClass suit] [Html.text <| suitText suit]
        Nothing -> Html.text "NT"
  in
    case bid of
      Auction.Pass -> [Html.text "Pass"]
      Auction.Double -> [Html.text "Double"]
      Auction.Redouble -> [Html.text "Redouble"]
      Auction.Bid level trump -> [Html.text (toString level), viewTrump trump]


cluster : a -> Int -> List a -> List (List a)
cluster filler count elems =
  let
    pad chunk =
      chunk ++ List.repeat (count - List.length chunk) filler
  in
    case List.take count elems of
      [] -> []
      chunk -> pad chunk :: cluster filler count (List.drop count elems)


viewVulnerability : Vulnerability -> Html
viewVulnerability vuln =
  let
    message =
      case (vuln.northSouth, vuln.eastWest) of
        (False, False) -> "neither side vulnerable"
        (False, True) -> "East/West vulnerable"
        (True, False) -> "North/South vulnerable"
        (True, True) -> "both sides vulnerable"
  in
    Html.text message
