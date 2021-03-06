module Bidding.ConventionResponse exposing
  ( conventionResponse
  , withConventionResponse
  )

import Bidding
import Bidding.Gerber
import Bidding.JacobyTransfer
import Bidding.Stayman
import Vulnerability


{-| Get the response bids for a convention, if available.
-}
conventionResponse : Vulnerability.Favorability -> List Bidding.AnnotatedBid -> Maybe (List Bidding.AnnotatedBid)
conventionResponse favorability history =
  let
    dispatch convention =
      case convention of
        Bidding.Gerber -> Bidding.Gerber.response favorability history
        Bidding.JacobyTransfer -> Bidding.JacobyTransfer.response favorability history
        Bidding.Stayman -> Bidding.Stayman.response favorability history
  in
    case List.map .convention history of
      _ :: Just convention :: _ -> dispatch convention
      _ -> Nothing


{-| Add convention responses to an existing suggestion function.
-}
withConventionResponse : (Vulnerability.Favorability -> List Bidding.AnnotatedBid -> List Bidding.AnnotatedBid) ->
                         (Vulnerability.Favorability -> List Bidding.AnnotatedBid -> List Bidding.AnnotatedBid)
withConventionResponse suggestions favorability history =
  case conventionResponse favorability history of
    Just responses -> responses
    Nothing -> suggestions favorability history
