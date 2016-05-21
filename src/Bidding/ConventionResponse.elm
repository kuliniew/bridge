module Bidding.ConventionResponse
  ( conventionResponse
  ) where

import Bidding
import Bidding.Stayman
import Vulnerability


{-| Get the response bids for a convention, if available.
-}
conventionResponse : Vulnerability.Favorability -> List Bidding.AnnotatedBid -> Maybe (List Bidding.AnnotatedBid)
conventionResponse favorability history =
  let
    dispatch convention =
      case convention of
        Bidding.Stayman -> Bidding.Stayman.response favorability history
        _ -> Nothing
  in
    case List.map .convention history of
      _ :: Just convention :: _ -> dispatch convention
      _ -> Nothing
