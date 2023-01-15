module Skapare.Templates
  ( instantiate
  , pathToTemplate
  , pathsToTemplate
  , loadTemplateFromPath
  , loadTemplateFromGitHub
  ) where

import Prelude

import Affjax.Node as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.TemplateString as TemplateString
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Aff as FileSystem
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Simple.JSON as Json
import Skapare.Types
  ( Bindings
  , Entity(..)
  , FileOutput(..)
  , GitHubSource(..)
  , LoadTemplateFromGitHubError(..)
  , Template(..)
  , TemplateDescription
  , TemplateId
  )

-- | Instantiates a template with a set of variables so that it can be filled in.
instantiate :: Template -> Array (Tuple String String) -> Array FileOutput
instantiate (Template { entities }) variables =
  foldMap (instantiateEntity variables) entities

-- | Turns an array of paths into a template, reading all files for all paths.
pathsToTemplate
  :: TemplateId -> TemplateDescription -> Bindings -> Array String -> Aff (Maybe (Array Template))
pathsToTemplate id description bindings paths = do
  sequence <$> traverse (pathToTemplate id description bindings) paths

-- | Turns a path into a template, reading all files for the path if it's a directory or just the
-- | file if it's a file.
pathToTemplate :: TemplateId -> TemplateDescription -> Bindings -> String -> Aff (Maybe Template)
pathToTemplate id description bindings path =
  map (\entity -> Template { id, description, entities: [ entity ] }) <$> pathToEntity bindings path

-- | Loads a template from a filename.
loadTemplateFromPath :: FilePath -> Aff (Either MultipleErrors Template)
loadTemplateFromPath path = do
  Json.readJSON <$> FileSystem.readTextFile Encoding.UTF8 path

-- | Loads a template from a given GitHub user and repository.  If a repository is not specified
-- | directly we assume `skapare-templates`.
loadTemplateFromGitHub
  :: GitHubSource -> TemplateId -> Aff (Either LoadTemplateFromGitHubError Template)
loadTemplateFromGitHub (GitHubSource { user, repo }) id = do
  let repository = fromMaybe "skapare-templates" repo
  let
    url = "https://raw.githubusercontent.com/" <> user <> "/" <> repository <> "/main/" <> unwrap id
      <> ".json"
  getResult <- Affjax.get ResponseFormat.string url
  case getResult of
    Left error -> pure $ Left $ LoadTemplateGitHubFetchError error
    Right response -> do
      let body = response.body
      case Json.readJSON body of
        Left error -> pure $ Left $ LoadTemplateDecodingError error
        Right template -> pure $ Right template

instantiateEntity :: Array (Tuple String String) -> Entity -> Array FileOutput
instantiateEntity variables (File { path, content }) =
  { path: TemplateString.template path variables
  , contents: TemplateString.template content variables
  } # FileOutput # Array.singleton
instantiateEntity variables (Directory { children }) =
  foldMap (instantiateEntity variables) children

pathToEntity :: Bindings -> String -> Aff (Maybe Entity)
pathToEntity bindings path = do
  stats <- FileSystem.stat path
  if Stats.isDirectory stats then directoryToEntity bindings path else fileToEntity bindings path

directoryToEntity :: Bindings -> String -> Aff (Maybe Entity)
directoryToEntity bindings path
  | String.contains (Pattern ".git") path = pure Nothing
  | otherwise = do
      children <- FileSystem.readdir path
      entities <- traverse (pathToEntity bindings) (map (\p -> path <> "/" <> p) children)
      let replacedPath = replaceBindings bindings path
      { path: replacedPath, children: Array.catMaybes entities } # Directory # Just # pure

fileToEntity :: Bindings -> String -> Aff (Maybe Entity)
fileToEntity bindings path = do
  buffer <- FileSystem.readFile path
  content <- buffer # Buffer.toString Encoding.UTF8 # liftEffect
  let
    replacedContent = replaceBindings bindings content
    replacedPath = replaceBindings bindings path
  { path: replacedPath, content: replacedContent } # File # Just # pure

replaceBindings :: Bindings -> String -> String
replaceBindings bindings content =
  Array.foldl
    (\c (k /\ v) -> String.replaceAll (Pattern v) (Replacement $ fold [ "${", k, "}" ]) c)
    content
    (bindings # unwrap # Map.toUnfoldable)
