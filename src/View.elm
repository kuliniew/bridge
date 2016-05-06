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
      , viewAuction address state.dealer state.system state.vulnerability state.auction
      , Html.button [ Events.onClick address Game.NewDeal ] [ Html.text "Rage Quit" ]
      , viewExplanation state.explained
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


viewAuction : Signal.Address Action -> Seat -> Bidding.System -> Vulnerability -> List Bidding.AnnotatedBid -> Html
viewAuction address dealer system vulnerability annotatedAuction =
  let
    auction =
      List.map .bid annotatedAuction
    favorability =
      Vulnerability.favorability (Game.currentBidder dealer annotatedAuction) vulnerability
    headerCell name =
      Html.th [] [Html.text name]
    bidCell bid =
      let
        events =
          [ Events.onMouseEnter address (Game.Explain <| Just bid.meaning)
          , Events.onMouseLeave address (Game.Explain Nothing)
          ]
      in
        Html.td events (viewBid bid.bid)
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
        then [makeBidCell address system favorability annotatedAuction]
        else []
    allCells = nullCells ++ List.reverse (List.map bidCell annotatedAuction) ++ makeBidCells
    row cells =
      Html.tr [] cells
  in
    Html.table [Attr.class "auction"]
      [ Html.thead []
          [ Html.tr [] (List.map headerCell ["West", "North", "East", "South"])
          ]
      , Html.tbody [] (List.map row <| cluster blankCell 4 allCells)
      ]


makeBidCell : Signal.Address Action -> Bidding.System -> Vulnerability.Favorability -> List Bidding.AnnotatedBid -> Html
makeBidCell address system favorability history =
  let
    legalBids =
      Auction.legalBids (List.map .bid history)
    annotatedLegalBids =
      List.map (Bidding.annotate system favorability history) legalBids
    button bid =
      let
        events =
          [ Events.onClick address (Game.Bid bid.bid)
          , Events.onMouseEnter address (Game.Explain <| Just bid.meaning)
          , Events.onMouseLeave address (Game.Explain Nothing)
          ]
      in
        Html.button events (viewBid bid.bid)
    buttons =
      annotatedLegalBids
        |> movePassToRightSide
        |> List.Extra.groupBy levels
        |> List.map (List.map button)
        |> List.Extra.intercalate [Html.br [] []]
    movePassToRightSide bids =
      case bids of
        bid1 :: bid2 :: rest ->
          if Auction.level bid2.bid == 0
             then bid2 :: bid1 :: rest
             else bids
        _ -> bids
    levels bid1 bid2 =
      Auction.level bid1.bid == Auction.level bid2.bid
  in
    Html.td [Attr.class "choices"] buttons


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


suitText : Card.Suit -> String
suitText suit =
  case suit of
    Card.Clubs -> "♣"
    Card.Diamonds -> "♦"
    Card.Hearts -> "♥"
    Card.Spades -> "♠"


suitSymbol : Card.Suit -> Html
suitSymbol suit =
  Html.span [suitClass suit] [Html.text <| suitText suit]


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


viewExplanation : Maybe Bidding.Meaning -> Html
viewExplanation explained =
  let
    content =
      case explained of
        Nothing -> []
        Just meaning ->
          [ Html.h1 [] [ Html.text "Explanation" ]
          , viewMeaning (simplify meaning)
          ]
  in
    Html.div [] content


viewMeaning : Bidding.Meaning -> Html
viewMeaning meaning =
  let
    list description meanings =
      Html.div []
        [ Html.text description
        , Html.ul [] (List.map (\m -> Html.li [] [viewMeaning m]) meanings)
        ]
  in
    case meaning of
      Bidding.OutOfSystem ->
        Html.text "(not part of the bidding system)"
      Bidding.InRange metric lo hi ->
        Html.span []
          [ viewMetric metric
          , Html.text (" between " ++ toString lo ++ " and " ++ toString hi)
          ]
      Bidding.Equal metric1 metric2 ->
        Html.span []
          [ viewMetric metric1
          , Html.text " = "
          , viewMetric metric2
          ]
      Bidding.Minimum metric bound ->
        Html.span []
          [ viewMetric metric
          , Html.text " ≥ "
          , viewMetric bound
          ]
      Bidding.Maximum metric bound ->
        Html.span []
          [ viewMetric metric
          , Html.text " ≤ "
          , viewMetric bound
          ]
      Bidding.GreaterThan metric bound ->
        Html.span []
          [ viewMetric metric
          , Html.text " > "
          , viewMetric bound
          ]
      Bidding.LessThan metric bound ->
        Html.span []
          [ viewMetric metric
          , Html.text " < "
          , viewMetric bound
          ]
      Bidding.Balanced ->
        Html.text "Balanced (4-3-3-3, 4-4-3-2, or 5-3-3-2)"
      Bidding.SemiBalanced ->
        Html.text "Semi-balaned (5-4-2-2 or 6-3-2-2)"
      Bidding.Or meanings ->
        list "Any of:" meanings
      Bidding.And meanings ->
        list "All of:" meanings
      Bidding.Deny sub ->
        list "Deny:" [sub]


viewMetric : Bidding.Metric -> Html
viewMetric metric =
  case metric of
    Bidding.Constant val ->
      Html.text (toString val)
    Bidding.HighCardPoints ->
      Html.text "HCP"
    Bidding.Points Nothing ->
      Html.text "Points (counting length)"
    Bidding.Points (Just Nothing) ->
      Html.text "Points (NT)"
    Bidding.Points (Just (Just suit)) ->
      Html.span [] [ Html.text "Points (", suitSymbol suit, Html.text ")" ]
    Bidding.Length suit ->
      Html.span [] [ Html.text "Length of ", suitSymbol suit ]
    Bidding.PlayingTricks ->
      Html.text "Playing Tricks"
    Bidding.QuickLosers suit ->
      Html.span [] [ Html.text "Quick Losers (", suitSymbol suit, Html.text ")" ]


simplify : Bidding.Meaning -> Bidding.Meaning
simplify = hideDenials >> collapseDegenerates >> collapseRedundancies


{-| Remove Deny nodes from And nodes of a meaning tree, to make the
displayed meaning more comprehensible.  (The Deny nodes are mostly
there to indicate preference between technically mutually allowed bids
rather than to convey human-level significance.)
-}
hideDenials : Bidding.Meaning -> Bidding.Meaning
hideDenials meaning =
  let
    scrub child =
      case child of
        Bidding.And children -> Just <| Bidding.And (List.map hideDenials children)
        Bidding.Or children -> Just <| Bidding.Or (List.map hideDenials children)
        Bidding.Deny _ -> Nothing
        _ -> Just child
  in
    case meaning of
      Bidding.And children -> Bidding.And (List.filterMap scrub children)
      Bidding.Or children -> Bidding.Or (List.map hideDenials children)
      _ -> meaning


{-| Remove And or Or nodes with only one child, since they're useless.
-}
collapseDegenerates : Bidding.Meaning -> Bidding.Meaning
collapseDegenerates meaning =
  case meaning of
    Bidding.And [child] -> collapseDegenerates child
    Bidding.And children -> Bidding.And (List.map collapseDegenerates children)
    Bidding.Or [child] -> collapseDegenerates child
    Bidding.Or children -> Bidding.Or (List.map collapseDegenerates children)
    Bidding.Deny child -> Bidding.Deny (collapseDegenerates child)
    _ -> meaning


{-| Collapse conjunctions who children are all the same conjunction.
-}
collapseRedundancies : Bidding.Meaning -> Bidding.Meaning
collapseRedundancies meaning =
  let
    isAnd meaning =
      case meaning of
        Bidding.And _ -> True
        _ -> False
    isOr meaning =
      case meaning of
        Bidding.Or _ -> True
        _ -> False
    grandchildren meaning =
      case meaning of
        Bidding.And grands -> grands
        Bidding.Or grands -> grands
        _ -> Debug.crash "tried to take grandchildren over non-conjunction nodes"
  in
    case meaning of
      Bidding.And children ->
        let
          children' = List.map collapseRedundancies children
        in
          if List.all isAnd children'
            then Bidding.And (List.concatMap grandchildren children')
            else Bidding.And children'
      Bidding.Or children ->
        let
          children' = List.map collapseRedundancies children
        in
          if List.all isOr children'
            then Bidding.Or (List.concatMap grandchildren children')
            else Bidding.Or children'
      _ -> meaning
