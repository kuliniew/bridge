module View
  ( viewHand
  , viewSuit
  , viewAuction
  ) where

import Auction
import Card exposing (Card)
import Seat exposing (Seat)

import Html exposing (Html)
import Html.Attributes as Attr


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


viewAuction : Seat -> List Auction.Bid -> Html
viewAuction dealer auction =
  let
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
        then [makeBidCell auction]
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


makeBidCell : List Auction.Bid -> Html
makeBidCell auction =
  let
    header =
      Html.option [] [Html.text "Make a bid..."]
    toChoice bid =
      Html.option [] (viewBid bid)
    choices =
      List.map toChoice (Auction.legalBids auction)
  in
    Html.select [] (header :: choices)


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
