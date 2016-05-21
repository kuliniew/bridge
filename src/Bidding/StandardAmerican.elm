module Bidding.StandardAmerican (system) where

{-| This module implements the Standard American Yellow Card bidding system.
-}

import Auction
import Bidding
import Bidding.ConventionResponse
import Bidding.Stayman
import Card
import Vulnerability


{-| The bidding system itself.
-}
system : Bidding.System
system =
  { name = "Standard American Yellow Card"
  , suggestions = suggest
  }


{-| The suggestion function.
-}
suggest : Vulnerability.Favorability -> List Bidding.AnnotatedBid -> List Bidding.AnnotatedBid
suggest favorability history =
  case Bidding.ConventionResponse.conventionResponse favorability history of
    Just responses ->
      responses
    Nothing ->
      case Bidding.role history of
        Bidding.Openable -> openingBids favorability history
        Bidding.Responder -> responseBids history
        _ -> []


{-| Possible opening bids.
-}
openingBids : Vulnerability.Favorability -> List Bidding.AnnotatedBid -> List Bidding.AnnotatedBid
openingBids favorability history =
  let
    fourthSeat =
      case history of
        [_, _, _] -> True
        _ -> False
    noTrump level lo hi =
      { bid = Auction.Bid level Nothing
      , description = Just "Balanced opening"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.InRange Bidding.HighCardPoints lo hi
          , Bidding.Balanced
          ]
      }
    oneNoTrump = noTrump 1 15 17
    twoNoTrump = noTrump 2 20 21
    threeNoTrump = noTrump 3 25 27
    majorLength = 5
    minorLength = 3
    oneLevelPoints = 13
    strongPoints = 22
    oneLevelMinimumPoints =
      Bidding.Minimum (Bidding.Points Nothing) (Bidding.Constant oneLevelPoints)
    oneLevelMaximumPoints =
      Bidding.LessThan Bidding.HighCardPoints (Bidding.Constant strongPoints)
    openWithMinor minor major =
      Bidding.Or
        [ Bidding.LessThan (Bidding.Length major) (Bidding.Constant majorLength)
        , Bidding.GreaterThan (Bidding.Length minor) (Bidding.Length major)
        ]
    preemptTricks level =
      let
        margin =
          case favorability of
            Vulnerability.Unfavorable -> 2
            Vulnerability.Equal -> 3
            Vulnerability.Favorable -> 4
      in
        Bidding.Constant (6 + level - margin)
    threeInsteadOfOne suit =
      let
        betterBid =
          if fourthSeat
            then fourthSeatThree suit
            else highestPreempt 3 suit
      in
        betterBid.meaning
    oneSpades =
      { bid = Auction.Bid 1 (Just Card.Spades)
      , description = Just "Five-card major"
      , convention = Nothing
      , meaning = Bidding.And
          [ oneLevelMinimumPoints
          , oneLevelMaximumPoints
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Constant majorLength)
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Length Card.Hearts)
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Length Card.Diamonds)
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Length Card.Clubs)
          ]
      }
    oneHearts =
      { bid = Auction.Bid 1 (Just Card.Hearts)
      , description = Just "Five-card major"
      , convention = Nothing
      , meaning = Bidding.And
          [ oneLevelMinimumPoints
          , oneLevelMaximumPoints
          , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Constant majorLength)
          , Bidding.GreaterThan (Bidding.Length Card.Hearts) (Bidding.Length Card.Spades)
          , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Length Card.Diamonds)
          , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Length Card.Clubs)
          ]
      }
    oneDiamonds =
      { bid = Auction.Bid 1 (Just Card.Diamonds)
      , description = Just "No five-card major"
      , convention = Nothing
      , meaning = Bidding.And
          [ oneLevelMinimumPoints
          , oneLevelMaximumPoints
          , Bidding.Minimum (Bidding.Length Card.Diamonds) (Bidding.Constant minorLength)
          , openWithMinor Card.Diamonds Card.Spades
          , openWithMinor Card.Diamonds Card.Hearts
          , Bidding.Minimum (Bidding.Length Card.Diamonds) (Bidding.Length Card.Clubs)
          , Bidding.Or     -- 4-4 goes to diamonds, but 3-3 goes to clubs
              [ Bidding.GreaterThan (Bidding.Length Card.Diamonds) (Bidding.Length Card.Clubs)
              , Bidding.Minimum (Bidding.Length Card.Diamonds) (Bidding.Constant <| minorLength + 1)
              ]
          ]
      }
    oneClubs =
      { bid = Auction.Bid 1 (Just Card.Clubs)
      , description = Just "No five-card major"
      , convention = Nothing
      , meaning = Bidding.And
          [ oneLevelMinimumPoints
          , oneLevelMaximumPoints
          , Bidding.Minimum (Bidding.Length Card.Clubs) (Bidding.Constant minorLength)
          , openWithMinor Card.Clubs Card.Spades
          , openWithMinor Card.Clubs Card.Hearts
          , Bidding.Minimum (Bidding.Length Card.Clubs) (Bidding.Length Card.Diamonds)
          , Bidding.Or     -- 4-4 goes to diamonds, but 3-3 goes to clubs
              [ Bidding.GreaterThan (Bidding.Length Card.Clubs) (Bidding.Length Card.Diamonds)
              , Bidding.Maximum (Bidding.Length Card.Diamonds) (Bidding.Constant minorLength)
              ]
          ]
      }
    twoClubs =
      let
        standardMeaning =
          Bidding.Minimum Bidding.HighCardPoints (Bidding.Constant strongPoints)
        weakerMeaning =
          Bidding.And
            [ Bidding.Minimum Bidding.HighCardPoints (Bidding.Constant 17)
            , Bidding.Or [oneShyOfMajorGame, oneShyOfMinorGame]
            ]
        oneShyOfMajorGame =
          Bidding.And
            [ Bidding.Minimum Bidding.PlayingTricks (Bidding.Constant 9)
            , fiveCardMajor
            ]
        oneShyOfMinorGame =
          Bidding.Minimum Bidding.PlayingTricks (Bidding.Constant 10)
        fiveCardMajor =
          Bidding.Or
            [ Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Constant majorLength)
            , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Constant majorLength)
            ]
      in
        { bid = Auction.Bid 2 (Just Card.Clubs)
        , description = Just "Strong 2♣"
        , convention = Nothing
        , meaning = Bidding.Or [standardMeaning, weakerMeaning]
        }
    weakTwo suit =
      { bid = Auction.Bid 2 (Just suit)
      , description = Just "Weak"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.InRange Bidding.HighCardPoints 5 10    -- SAYC says 5-11, but 11 HCP + six cards == 13 points == 1-level opening
          , Bidding.LessThan (Bidding.Points Nothing) (Bidding.Constant oneLevelPoints)
          , Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 6)
          , Bidding.LessThan Bidding.PlayingTricks (preemptTricks 3)
          ]
      }
    preempt level suit trickCondition =
      { bid = Auction.Bid level (Just suit)
      , description = Just "Preempt"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.Maximum Bidding.HighCardPoints (Bidding.Constant 10)
          , Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 7)
          , trickCondition Bidding.PlayingTricks (preemptTricks level)
          ]
      }
    highestPreempt level suit =
      preempt level suit Bidding.Minimum
    moderatePreempt level suit =
      preempt level suit Bidding.Equal
    fourthSeatThree suit =
      { bid = Auction.Bid 3 (Just suit)
      , description = Just "Non-preempt"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.InRange Bidding.HighCardPoints 10 12
          , Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 7)
          ]
      }
    preempts =
      if fourthSeat
        then
          [ fourthSeatThree Card.Clubs
          , fourthSeatThree Card.Diamonds
          , fourthSeatThree Card.Hearts
          , fourthSeatThree Card.Spades
          ]
        else
          [ moderatePreempt 3 Card.Clubs
          , moderatePreempt 3 Card.Diamonds
          , moderatePreempt 3 Card.Hearts
          , moderatePreempt 3 Card.Spades
          , moderatePreempt 4 Card.Clubs
          , moderatePreempt 4 Card.Diamonds
          , highestPreempt 4 Card.Hearts
          , highestPreempt 4 Card.Spades
          , highestPreempt 5 Card.Clubs
          , highestPreempt 5 Card.Diamonds
          ]
    pass =
      { bid = Auction.Pass
      , description = Nothing
      , convention = Nothing
      , meaning = Bidding.LessThan (Bidding.Points Nothing) (Bidding.Constant oneLevelPoints)
      }
  in
    prioritized
      [ [threeNoTrump]
      , preempts
      , [twoClubs, weakTwo Card.Diamonds, weakTwo Card.Hearts, weakTwo Card.Spades]
      , [oneNoTrump, twoNoTrump, oneSpades, oneHearts, oneDiamonds, oneClubs]
      , [pass]
      ]


{-| Dispatcher for the various types of response bidding.
-}
responseBids : List Bidding.AnnotatedBid -> List Bidding.AnnotatedBid
responseBids history =
  let
    firstResponse =
      case List.map .bid history of
        Auction.Pass :: bid :: rest ->
          if List.all ((==) Auction.Pass) rest
            then Just bid
            else Nothing
        _ ->
          Nothing
  in
    case firstResponse of
      Just (Auction.Bid 1 Nothing) -> responsesToOneNoTrump
      Just (Auction.Bid 2 Nothing) -> responsesToTwoNoTrump
      Just (Auction.Bid 3 Nothing) -> responsesToThreeNoTrump
      _ -> []


{-| Responses to an opening bid of 1NT.
-}
responsesToOneNoTrump : List Bidding.AnnotatedBid
responsesToOneNoTrump =
  let
    inviteSlamPoints = 33 - 17
    noFourCardMajor =
      Bidding.And
        [ Bidding.Maximum (Bidding.Length Card.Spades) (Bidding.Constant 3)
        , Bidding.Maximum (Bidding.Length Card.Hearts) (Bidding.Constant 3)
        ]
    pass =
      { bid = Auction.Pass
      , description = Nothing
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.Maximum Bidding.HighCardPoints (Bidding.Constant 7)
          , Bidding.LessThan (Bidding.Length Card.Spades) (Bidding.Constant 5)
          , Bidding.LessThan (Bidding.Length Card.Hearts) (Bidding.Constant 5)
          , Bidding.LessThan (Bidding.Length Card.Diamonds) (Bidding.Constant 6)
          , Bidding.LessThan (Bidding.Length Card.Clubs) (Bidding.Constant 6)
          ]
      }
    jacobyTransfer target via =
      { bid = Auction.Bid 2 (Just via)
      , description = Nothing
      , convention = Just Bidding.JacobyTransfer
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Length target) (Bidding.Constant 5)
          , Bidding.Deny (inviteSlam target).meaning
          ]
      }
    minorTransfer =
      { bid = Auction.Bid 2 (Just Card.Spades)
      , description = Just "Minor Transfer"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.Or
              [ Bidding.Minimum (Bidding.Length Card.Clubs) (Bidding.Constant 6)
              , Bidding.Minimum (Bidding.Length Card.Diamonds) (Bidding.Constant 6)
              ]
          , Bidding.LessThan Bidding.HighCardPoints (Bidding.Constant 7)
          ]
      }
    stayman =
      Bidding.Stayman.bid 2 (Just <| Bidding.Minimum (Bidding.Points Nothing) (Bidding.Constant 8))
    inviteGame =
      { bid = Auction.Bid 2 Nothing
      , description = Just "Game invite"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.InRange Bidding.HighCardPoints 8 9
          , Bidding.Or [Bidding.Balanced, Bidding.SemiBalanced]
          , Bidding.Or [noFourCardMajor, fourThreeThreeThree]
          ]
      }
    inviteGameWithLongMinor suit =
      { bid = Auction.Bid 3 (Just suit)
      , description = Just "Game invite"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 6)
          , Bidding.InRange Bidding.HighCardPoints 7 9
          ]
      }
    bidGame =
      { bid = Auction.Bid 3 Nothing
      , description = Just "Game"
      , convention = Nothing
      , meaning = Bidding.InRange Bidding.HighCardPoints 10 15
      }
    inviteSlam suit =
      { bid = Auction.Bid 3 (Just suit)
      , description = Just "Slam invite"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 6)
          , Bidding.Minimum (Bidding.Points <| Just (Just suit)) (Bidding.Constant inviteSlamPoints)
          ]
      }
    inviteSlamNoTrump =
      { bid = Auction.Bid 4 Nothing
      , description = Just "Quantitative raise"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.InRange Bidding.HighCardPoints inviteSlamPoints (inviteSlamPoints + 1)
          , Bidding.Or [Bidding.Balanced, Bidding.SemiBalanced]
          ]
      }
    atLeastOneVoid =
      let
        voidIn suit =
          Bidding.Equal (Bidding.Length suit) (Bidding.Constant 0)
      in
        Bidding.Or <| List.map voidIn suits
    gerber =
      { bid = Auction.Bid 4 (Just Card.Clubs)
      , description = Nothing
      , convention = Just Bidding.Gerber
      , meaning = Bidding.And
          [ Bidding.GreaterThan (Bidding.Points Nothing) (Bidding.Constant inviteSlamPoints)    -- FIXME: probably should be based on a known fit?
          , noVoids
          , noTwoQuickLosers
          ]
      }
    minorSlam suit =
      { bid = Auction.Bid 6 (Just suit)
      , description = Just "Slam"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Points <| Just (Just suit)) (Bidding.Constant 20)
          , Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 6)
          , atLeastOneVoid
          ]
      }
    priority1 =
      [ inviteSlamNoTrump
      , gerber
      , minorSlam Card.Clubs
      , minorSlam Card.Diamonds
      ]
    priority2 =
      [ inviteGameWithLongMinor Card.Clubs
      , inviteGameWithLongMinor Card.Diamonds
      , inviteSlam Card.Hearts
      , inviteSlam Card.Spades
      ]
    priority3 =
      [ pass
      , jacobyTransfer Card.Hearts Card.Diamonds
      , jacobyTransfer Card.Spades Card.Hearts
      , minorTransfer
      , stayman
      , inviteGame
      , bidGame
      ]
  in
    prioritized [priority1, priority2, priority3]


{-| Responses to an opening bid of 2NT.
-}
responsesToTwoNoTrump : List Bidding.AnnotatedBid
responsesToTwoNoTrump =
  let
    gamePoints = 5
    inviteSlamPoints = 33 - 20
    pass =
      { bid = Auction.Pass
      , description = Nothing
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.LessThan Bidding.HighCardPoints (Bidding.Constant gamePoints)
          , Bidding.LessThan (Bidding.Length Card.Spades) (Bidding.Constant 5)
          , Bidding.LessThan (Bidding.Length Card.Hearts) (Bidding.Constant 5)
          ]
      }
    stayman =
      Bidding.Stayman.bid 3 (Just <| Bidding.InRange (Bidding.Points Nothing) gamePoints (inviteSlamPoints - 1))
    jacobyTransfer target via =
      { bid = Auction.Bid 3 (Just via)
      , description = Nothing
      , convention = Just Bidding.JacobyTransfer
      , meaning = Bidding.Minimum (Bidding.Length target) (Bidding.Constant 5)
      }
    game =
      { bid = Auction.Bid 3 Nothing
      , description = Just "raise to game"
      , convention = Nothing
      , meaning = Bidding.And
          [ Bidding.InRange Bidding.HighCardPoints gamePoints (inviteSlamPoints - 1)
          , Bidding.Or
              [ Bidding.And
                  [ Bidding.LessThan (Bidding.Length Card.Spades) (Bidding.Constant 4)
                  , Bidding.LessThan (Bidding.Length Card.Hearts) (Bidding.Constant 4)
                  ]
              , fourThreeThreeThree
              ]
          ]
      }
    inviteSlam =
      { bid = Auction.Bid 4 Nothing
      , description = Just "quantitative raise"
      , convention = Nothing
      , meaning =
          Bidding.Minimum Bidding.HighCardPoints (Bidding.Constant inviteSlamPoints)
      }
    gerber =
      { bid = Auction.Bid 4 (Just Card.Clubs)
      , description = Nothing
      , convention = Just Bidding.Gerber
      , meaning = Bidding.And
          [ Bidding.GreaterThan (Bidding.Points Nothing) (Bidding.Constant inviteSlamPoints)    -- FIXME: probably should be based on a known fit?
          , noVoids
          , noTwoQuickLosers
          ]
      }
    priority1 =
      [ gerber
      ]
    priority2 =
      [ pass
      , stayman
      , jacobyTransfer Card.Spades Card.Hearts
      , jacobyTransfer Card.Hearts Card.Diamonds
      , game
      , inviteSlam
      ]
  in
    prioritized [priority1, priority2]


{-| Responses to an opening bid of 3NT.
-}
responsesToThreeNoTrump : List Bidding.AnnotatedBid
responsesToThreeNoTrump =
  let
    otherMajor major =
      case major of
        Card.Spades -> Card.Hearts
        Card.Hearts -> Card.Spades
        _ -> Debug.crash (toString major ++ " is not a major suit, so it has no 'other major'")
    pass =
      { bid = Auction.Pass
      , description = Nothing
      , convention = Nothing
      , meaning = Bidding.Or
          [ Bidding.And
              [ Bidding.LessThan (Bidding.Length Card.Spades) (Bidding.Constant 4)
              , Bidding.LessThan (Bidding.Length Card.Hearts) (Bidding.Constant 4)
              ]
          , fourThreeThreeThree
          ]
      }
    jacobyTransfer target via =
      { bid = Auction.Bid 4 (Just via)
      , description = Nothing
      , convention = Just Bidding.JacobyTransfer
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Length target) (Bidding.Constant 5)
          , Bidding.LessThan (Bidding.Length (otherMajor target)) (Bidding.Constant 4)
          ]
      }
    stayman =
      Bidding.Stayman.bid 4 Nothing
  in
    [ pass
    , jacobyTransfer Card.Spades Card.Hearts
    , jacobyTransfer Card.Hearts Card.Diamonds
    , stayman
    ]


{-| Require having 4-3-3-3 distribution.
-}
fourThreeThreeThree : Bidding.Meaning
fourThreeThreeThree =
  Bidding.And <| List.map (\suit -> Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 3)) suits


{-| Deny having 4-3-3-3 distribution.
-}
notFourThreeThreeThree : Bidding.Meaning
notFourThreeThreeThree =
  Bidding.Or <| List.map (\suit -> Bidding.Maximum (Bidding.Length suit) (Bidding.Constant 2)) suits


{-| Require that a hand contain no voids.
-}
noVoids : Bidding.Meaning
noVoids =
  let
    noVoidIn suit =
      Bidding.GreaterThan (Bidding.Length suit) (Bidding.Constant 0)
  in
    Bidding.And <| List.map noVoidIn suits


{-| Require that a hand have no suit with two quick losers.
-}
noTwoQuickLosers : Bidding.Meaning
noTwoQuickLosers =
  let
    noTwoQuickLosersIn suit =
      Bidding.LessThan (Bidding.QuickLosers suit) (Bidding.Constant 2)
  in
    Bidding.And <| List.map noTwoQuickLosersIn suits


{-| Flatten a prioritized list of bids, such that the nth set of choices
deny having been able to make any of the bids in the previous sets.
-}
prioritized : List (List Bidding.AnnotatedBid) -> List Bidding.AnnotatedBid
prioritized =
  let
    downgrade denials bid =
      { bid | meaning = Bidding.And (bid.meaning :: denials) }
    prioritized' denials prefs =
      case prefs of
        [] -> []
        (next :: rest) ->
          let
            downgraded = List.map (downgrade denials) next
            denials' = denials ++ List.map (Bidding.Deny << .meaning) next
          in
            downgraded ++ prioritized' denials' rest
  in
    prioritized' []


{-| List of all suits.
-}
suits : List Card.Suit
suits = [ Card.Spades, Card.Hearts, Card.Diamonds, Card.Clubs ]
