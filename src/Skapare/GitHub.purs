module Skapare.GitHub
  ( getTree
  , GetTreeErrors
  ) where

import Prelude

import Affjax.Node as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Effect.Aff.Class (liftAff)
import Simple.JSON as Json
import Skapare.Types (JsonDecodingError, GitHubSource(..), GitHubTreeResponse)
import Skapare.Utilities as Utilities
import Yoga.Om (Om)
import Yoga.Om as Om
import Type.Row (type (+))

type GetTreeErrors e =
  JsonDecodingError +
    ( gitHubApiError :: Affjax.Error
    , repositoryNotFound :: GitHubSource
    | e
    )

-- | Gets the `main` tree for a given repository and branch.
getTree :: forall ctx e. GitHubSource -> Om (| ctx) (GetTreeErrors e) GitHubTreeResponse
getTree source@(GitHubSource { repo: maybeRepo, user }) = do
  let
    url = "https://api.github.com/repos/" <> user <> "/" <> repo <> "/git/trees/main"
    repo = fromMaybe "skapare-templates" maybeRepo
  maybeResponse <- liftAff $ Affjax.get ResponseFormat.string url
  when (Utilities.getResponseStatusCode maybeResponse == Just (wrap 404)) do
    Om.throw { repositoryNotFound: source }
  response <- Om.throwLeftAs (\gitHubApiError -> Om.error { gitHubApiError }) maybeResponse
  response.body
    # Json.readJSON
    # Om.throwLeftAs (\jsonDecodingError -> Om.error { jsonDecodingError })

