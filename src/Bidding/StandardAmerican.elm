module Bidding.StandardAmerican (system) where

{-| This module implements the Standard American Yellow Card bidding system.
-}

import Auction
import Bidding
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
        , meaning = Bidding.Or [standardMeaning, weakerMeaning]
        }
    weakTwo suit =
      { bid = Auction.Bid 2 (Just suit)
      , meaning = Bidding.And
          [ Bidding.InRange Bidding.HighCardPoints 5 10    -- SAYC says 5-11, but 11 HCP + six cards == 13 points == 1-level opening
          , Bidding.LessThan (Bidding.Points Nothing) (Bidding.Constant oneLevelPoints)
          , Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 6)
          , Bidding.LessThan Bidding.PlayingTricks (preemptTricks 3)
          ]
      }
    preempt level suit trickCondition =
      { bid = Auction.Bid level (Just suit)
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
    justOpenedOneNoTrump =
      case List.map .bid history of
        Auction.Pass :: Auction.Bid 1 Nothing :: rest ->
          List.all (\bid -> bid == Auction.Pass) rest
        _ ->
          False
  in
    if justOpenedOneNoTrump
    then responsesToOneNoTrump
    else []


{-| Responses to an opening bid of 1NT.
-}
responsesToOneNoTrump : List Bidding.AnnotatedBid
responsesToOneNoTrump =
  let
    inviteSlamPoints = Bidding.Constant (33 - 17)
    noFourCardMajor =
      Bidding.And
        [ Bidding.Maximum (Bidding.Length Card.Spades) (Bidding.Constant 3)
        , Bidding.Maximum (Bidding.Length Card.Hearts) (Bidding.Constant 3)
        ]
    fourThreeThreeThree =
      Bidding.And <| List.map (\suit -> Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 3)) suits
    notFourThreeThreeThree =
      Bidding.Or <| List.map (\suit -> Bidding.Maximum (Bidding.Length suit) (Bidding.Constant 2)) suits
    pass =
      { bid = Auction.Pass
      , meaning = Bidding.And
          [ Bidding.Maximum (Bidding.Points Nothing) (Bidding.Constant 7)
          , Bidding.LessThan (Bidding.Length Card.Spades) (Bidding.Constant 5)
          , Bidding.LessThan (Bidding.Length Card.Hearts) (Bidding.Constant 5)
          , Bidding.LessThan (Bidding.Length Card.Diamonds) (Bidding.Constant 6)
          , Bidding.LessThan (Bidding.Length Card.Clubs) (Bidding.Constant 6)
          ]
      }
    jacobyTransfer target via =
      { bid = Auction.Bid 2 (Just via)
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Length target) (Bidding.Constant 5)
          , Bidding.Deny (inviteSlam target).meaning
          ]
      }
    minorTransfer =
      { bid = Auction.Bid 2 (Just Card.Spades)
      , meaning = Bidding.And
          [ Bidding.Or
              [ Bidding.Minimum (Bidding.Length Card.Clubs) (Bidding.Constant 6)
              , Bidding.Minimum (Bidding.Length Card.Diamonds) (Bidding.Constant 6)
              ]
          , Bidding.LessThan Bidding.HighCardPoints (Bidding.Constant 7)
          ]
      }
    stayman =
      { bid = Auction.Bid 2 (Just Card.Clubs)
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Points Nothing) (Bidding.Constant 8)
          , Bidding.Or
              [ Bidding.Equal (Bidding.Length Card.Hearts) (Bidding.Constant 4)
              , Bidding.Equal (Bidding.Length Card.Spades) (Bidding.Constant 4)
              ]
          , notFourThreeThreeThree
          ]
      }
    inviteGame =
      { bid = Auction.Bid 2 Nothing
      , meaning = Bidding.And
          [ Bidding.InRange Bidding.HighCardPoints 8 9
          , Bidding.Balanced
          , Bidding.Or [noFourCardMajor, fourThreeThreeThree]
          ]
      }
    inviteGameWithLongMinor suit =
      { bid = Auction.Bid 3 (Just suit)
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 6)
          , Bidding.InRange Bidding.HighCardPoints 7 9
          ]
      }
    bidGame =
      { bid = Auction.Bid 3 Nothing
      , meaning = Bidding.InRange Bidding.HighCardPoints 10 15
      }
    inviteSlam suit =
      { bid = Auction.Bid 3 (Just suit)
      , meaning = Bidding.And
          [ Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 6)
          , Bidding.Minimum (Bidding.Points <| Just (Just suit)) inviteSlamPoints
          ]
      }
    inviteSlamNoTrump =
      { bid = Auction.Bid 4 Nothing
      , meaning = Bidding.And
          [ Bidding.Equal Bidding.HighCardPoints inviteSlamPoints
          , Bidding.Or [Bidding.Balanced, Bidding.SemiBalanced]
          ]
      }
    noVoids =
      let
        noVoidIn suit =
          Bidding.GreaterThan (Bidding.Length suit) (Bidding.Constant 0)
      in
        List.map noVoidIn suits
    noTwoQuickLosers =
      let
        noTwoQuickLosersIn suit =
          Bidding.LessThan (Bidding.QuickLosers suit) (Bidding.Constant 2)
      in
        List.map noTwoQuickLosersIn suits
    gerber =
      { bid = Auction.Bid 4 (Just Card.Clubs)
      , meaning = Bidding.And
          ( Bidding.GreaterThan (Bidding.Points Nothing) inviteSlamPoints    -- FIXME: probably should be based on a known fit?
          :: noVoids
          ++ noTwoQuickLosers
          )
      }
    priority1 =
      [ inviteSlamNoTrump
      , gerber
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
