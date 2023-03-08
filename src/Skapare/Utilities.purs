module Skapare.Utilities
  ( getResponseStatusCode
  ) where

import Prelude

import Affjax.Node as Affjax
import Affjax.StatusCode (StatusCode)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))

getResponseStatusCode :: forall a. Either Affjax.Error (Affjax.Response a) -> Maybe StatusCode
getResponseStatusCode = either (const Nothing) (\{ status } -> Just status)
