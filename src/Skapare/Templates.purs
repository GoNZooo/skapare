module Skapare.Templates
  ( instantiate
  , pathToTemplate
  , loadTemplateFromPath
  , loadTemplateFromGitHub
  , listTemplatesInGitHub
  ) where

import Prelude

import Affjax.Node as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.List (filterM)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Utils as StringUtils
import Data.TemplateString as TemplateString
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Aff as FileSystem
import Node.FS.Stats as Stats
import Node.FS.Sync as FileSystemSync
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Simple.JSON as Json
import Skapare.GitHub as GitHub
import Skapare.Types
  ( Bindings
  , Entity(..)
  , FileOutput(..)
  , GitHubSource(..)
  , GitHubTreeItem(..)
  , GitHubTreeResponse(..)
  , InstantiationError(..)
  , LoadTemplateFromGitHubError(..)
  , Template(..)
  , TemplateDescription
  , TemplateId
  , TemplateVariable
  )
import Yoga.Om (Om)

-- | Lists template in a GitHub repository
listTemplatesInGitHub
  :: forall ctx errors
   . GitHubSource
  -> Om (| ctx) (getError :: Affjax.Error, decodeError :: MultipleErrors | errors) (Array TemplateId)
listTemplatesInGitHub source = do
  GitHubTreeResponse { tree } <- GitHub.getTree source
  tree
    # Array.filter (\(GitHubTreeItem { path }) -> StringUtils.endsWith ".json" path)
    # map
        ( \(GitHubTreeItem { path }) ->
            path # String.take (String.length path - 5) # wrap
        )
    # pure

-- | Instantiates a template with a set of variables so that it can be filled in.
instantiate :: Template -> Array (Tuple String String) -> Either InstantiationError (Array FileOutput)
instantiate (Template { entities, variables: expectedVariables }) variables = do
  case missingTemplateVariables variables expectedVariables of
    [] -> Right $ foldMap (instantiateEntity variables) entities
    missing -> Left $ MissingVariables missing

missingTemplateVariables
  :: Array (Tuple String String) -> Array TemplateVariable -> Array TemplateVariable
missingTemplateVariables variables expectedVariables =
  expectedVariables
    # map unwrap
    # Array.filter (\v -> variables # map Tuple.fst # Array.elem v # not)
    # map wrap

-- | Turns a path into a template, reading all files for the path if it's a directory or just the
-- | file if it's a file.
pathToTemplate :: TemplateId -> TemplateDescription -> Bindings -> String -> Aff (Maybe Template)
pathToTemplate id description bindings path = do
  currentDirectory <- liftEffect Process.cwd
  let relativePath = Path.relative currentDirectory path
  map
    ( \entity ->
        Template
          { id
          , description
          , entities: [ entity ]
          , variables: bindings # unwrap # Map.keys # Array.fromFoldable # map wrap
          }
    )
    <$> pathToEntity bindings relativePath

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
    url =
      fold
        [ "https://raw.githubusercontent.com/"
        , user
        , "/"
        , repository
        , "/main/"
        , unwrap id
        , ".json"
        ]
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
  let
    isSymbolicLink = Stats.isSymbolicLink stats
    isDirectory = Stats.isDirectory stats
  case isSymbolicLink, isDirectory of
    true, _ -> pure Nothing
    _, true -> directoryToEntity bindings path
    _, _ -> fileToEntity bindings path

directoryToEntity :: Bindings -> String -> Aff (Maybe Entity)
directoryToEntity bindings path = do
  ignorePatterns <- readIgnoreFile path
  children <- map (\p -> path <> "/" <> p) <$> FileSystem.readdir path
  validFiles <-
    children
      # List.fromFoldable
      # filterM isNotSymbolicLink
      # map (List.filter (isIgnored ignorePatterns >>> not))
      # map Array.fromFoldable
  entities <- traverse (pathToEntity bindings) validFiles
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

isNotSymbolicLink :: String -> Aff Boolean
isNotSymbolicLink path = do
  stats <- FileSystem.stat path
  stats # Stats.isSymbolicLink # not # pure

isIgnored :: Array String -> String -> Boolean
isIgnored ignorePatterns path =
  ignorePatterns # Array.any (\p -> String.contains (Pattern p) path)

readIgnoreFile :: String -> Aff (Array String)
readIgnoreFile path = do
  hasIgnoreFile <- liftEffect $ FileSystemSync.exists (path <> "/.skapareignore")
  if hasIgnoreFile then do
    (String.split (Pattern "\n") >>> Array.filter (_ /= "")) <$>
      FileSystem.readTextFile Encoding.UTF8 (path <> "/.skapareignore")
  else pure []
