module Skapare.GitHub
  ( getTree
  ) where

import Prelude

import Affjax as Affjax
import Affjax.Node as AffjaxNode
import Affjax.ResponseFormat as ResponseFormat
import Data.Maybe (fromMaybe)
import Effect.Aff.Class (liftAff)
import Foreign (MultipleErrors)
import Simple.JSON as Json
import Skapare.Types (GitHubSource(..), GitHubTreeResponse)
import Yoga.Om (Om)
import Yoga.Om as Om

-- | Gets the tree for a given repository and branch.
getTree
  :: forall ctx errors
   . GitHubSource
  -> Om (| ctx) (getError :: Affjax.Error, decodeError :: MultipleErrors | errors) GitHubTreeResponse
getTree (GitHubSource { repo: maybeRepo, user }) = do
  let
    url = "https://api.github.com/repos/" <> user <> "/" <> repo <> "/git/trees/main"
    repo = fromMaybe "skapare-templates" maybeRepo
  maybeResponse <- liftAff $ Affjax.get AffjaxNode.driver ResponseFormat.string url
  response <- Om.throwLeftAs (\getError -> Om.error { getError }) maybeResponse
  Om.throwLeftAs (\decodeError -> Om.error { decodeError }) $ Json.readJSON response.body

