module Skapare.GitHub
  ( getTree
  , GetTreeErrors
  ) where

import Prelude

import Affjax as Affjax
import Affjax.Node as AffjaxNode
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Effect.Aff.Class (liftAff)
import Foreign (MultipleErrors)
import Simple.JSON as Json
import Skapare.Types (GitHubSource(..), GitHubTreeResponse)
import Yoga.Om (Om)
import Yoga.Om as Om

type GetTreeErrors errors =
  ( gitHubApiError :: Affjax.Error
  , repositoryNotFound :: GitHubSource
  , gitHubDecodeError :: MultipleErrors
  | errors
  )

-- | Gets the `main` tree for a given repository and branch.
getTree :: forall ctx e. GitHubSource -> Om (| ctx) (GetTreeErrors e) GitHubTreeResponse
getTree source@(GitHubSource { repo: maybeRepo, user }) = do
  let
    url = "https://api.github.com/repos/" <> user <> "/" <> repo <> "/git/trees/main"
    repo = fromMaybe "skapare-templates" maybeRepo
  maybeResponse <- liftAff $ Affjax.get AffjaxNode.driver ResponseFormat.string url
  when (getResponseStatusCode maybeResponse == Just (wrap 404)) do
    Om.throw { repositoryNotFound: source }
  response <- Om.throwLeftAs (\gitHubApiError -> Om.error { gitHubApiError }) maybeResponse
  response.body
    # Json.readJSON
    # Om.throwLeftAs (\gitHubDecodeError -> Om.error { gitHubDecodeError })

getResponseStatusCode :: forall a. Either Affjax.Error (Affjax.Response a) -> Maybe StatusCode
getResponseStatusCode = either (const Nothing) (\{ status } -> Just status)
