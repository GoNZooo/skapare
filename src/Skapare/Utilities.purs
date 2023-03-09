module Skapare.Utilities
  ( getResponseStatusCode
  , handleError
  , makeParentDirectories
  ) where

import Prelude

import Affjax.Node as Affjax
import Affjax.StatusCode (StatusCode)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Error.Class as Error
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Node.FS.Aff as FileSystem
import Node.FS.Perms as Perms
import Node.Path as Path

getResponseStatusCode :: forall a. Either Affjax.Error (Affjax.Response a) -> Maybe StatusCode
getResponseStatusCode = either (const Nothing) (\{ status } -> Just status)

handleError :: forall a e m. MonadError e m => (e -> m a) -> m a -> m a
handleError handler action = Error.catchError action handler

makeParentDirectories :: String -> Aff Unit
makeParentDirectories path = FileSystem.mkdir' (Path.dirname path) { recursive: true, mode }
  where
  mode = Perms.mkPerms full Perms.none Perms.none
  full = Perms.read + Perms.write + Perms.execute
