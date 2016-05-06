module Convention
  ( Convention (..)
  , Usage (..)
  ) where

{-| This module describes various bidding conventions used by the bidding
systems.
-}


{-| Identifier of the bidding convention.
-}
type Convention
  = Gerber
  | JacobyTransfer
  | Stayman


{-| Where a particular bid falls inside a convention.
-}
type Usage
  = Start Convention
  | Finish Convention
