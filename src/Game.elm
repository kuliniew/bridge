module Game (Model, init, view, update) where

import Card exposing (Card)
import Seat exposing (Seat)

import Array
import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attr
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
  Html.div [] (List.map (\seat -> viewHand seat (Seat.lookup seat state.hands)) [Seat.West, Seat.North, Seat.East, Seat.South])


viewHand : Seat -> List Card -> Html
viewHand seat hand =
  Html.div []
    [ Html.text (toString seat)
    , Html.ul [Attr.class "suits"] (List.map (viewSuit hand) [Card.Spades, Card.Hearts, Card.Diamonds, Card.Clubs])
    ]


viewSuit : List Card -> Card.Suit -> Html
viewSuit hand suit =
  let
    cards = List.filter (\card -> card.suit == suit) hand
    contents = List.map viewRank cards
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
