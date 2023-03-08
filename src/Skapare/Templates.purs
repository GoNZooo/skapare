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
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
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
  , JsonDecodingError
  , Template(..)
  , TemplateDescription
  , TemplateId
  , TemplateVariable
  )
import Skapare.Utilities as Utilities
import Type.Row (type (+))
import Yoga.Om (Om)
import Yoga.Om as Om

-- | Lists template in a GitHub repository
listTemplatesInGitHub
  :: forall ctx e. GitHubSource -> Om (| ctx) (GitHub.GetTreeErrors e) (Array TemplateId)
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
instantiate
  :: Template -> Array (Tuple String String) -> Either (Array TemplateVariable) (Array FileOutput)
instantiate (Template { entities, variables: expectedVariables }) variables = do
  case missingTemplateVariables variables expectedVariables of
    [] -> Right $ foldMap (instantiateEntity variables) entities
    missing -> Left missing

missingTemplateVariables
  :: Array (Tuple String String) -> Array TemplateVariable -> Array TemplateVariable
missingTemplateVariables variables expectedVariables =
  expectedVariables
    # map unwrap
    # Array.filter (\v -> variables # map Tuple.fst # Array.elem v # not)
    # map wrap

-- | Turns a path into a template, reading all files for the path if it's a directory or just the
-- | file if it's a file.
pathToTemplate
  :: forall ctx e
   . TemplateId
  -> TemplateDescription
  -> Bindings
  -> String
  -> Om (| ctx) (PathToEntityErrors + e) (Maybe Template)
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

type LoadTemplateFromPathError e = JsonDecodingError + e

-- | Loads a template from a filename.
loadTemplateFromPath
  :: forall ctx e
   . FilePath
  -> Om (| ctx) (LoadTemplateFromPathError + e) Template
loadTemplateFromPath path = do
  maybeTemplate <- Json.readJSON <$> (path # FileSystem.readTextFile Encoding.UTF8 # liftAff)
  Om.throwLeftAs (\jsonDecodingError -> Om.error { jsonDecodingError }) maybeTemplate

type LoadTemplateFromGitHubErrors e = GitHub.GetTreeErrors + e

-- | Loads a template from a given GitHub user and repository.  If a repository is not specified
-- | directly we assume `skapare-templates`.
loadTemplateFromGitHub
  :: forall ctx e
   . GitHubSource
  -> TemplateId
  -> Om (| ctx) (LoadTemplateFromGitHubErrors + e) Template
loadTemplateFromGitHub source@(GitHubSource { user, repo }) id = do
  let
    repository = fromMaybe "skapare-templates" repo
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
  maybeResponse <- url # Affjax.get ResponseFormat.string # liftAff
  when (Utilities.getResponseStatusCode maybeResponse == Just (wrap 404)) do
    Om.throw { repositoryNotFound: source }
  response <- Om.throwLeftAs (\gitHubApiError -> Om.error { gitHubApiError }) maybeResponse
  response.body
    # Json.readJSON
    # Om.throwLeftAs (\jsonDecodingError -> Om.error { jsonDecodingError })

instantiateEntity :: Array (Tuple String String) -> Entity -> Array FileOutput
instantiateEntity variables (File { path, content }) =
  { path: TemplateString.template path variables
  , contents: TemplateString.template content variables
  } # FileOutput # Array.singleton
instantiateEntity variables (Directory { children }) =
  foldMap (instantiateEntity variables) children

type PathToEntityErrors e = StatError + IsSymbolicLinkError + e

type IsSymbolicLinkError e = (isSymbolicLinkError :: String | e)

type StatError e = (statError :: String | e)

pathToEntity :: forall ctx e. Bindings -> String -> Om (| ctx) (PathToEntityErrors + e) (Maybe Entity)
pathToEntity bindings path = do
  stats <- path # FileSystem.stat # liftAff -- # Aff.catchError (\e -> Om.throw { statError: path })
  let
    isSymbolicLink = Stats.isSymbolicLink stats
    isDirectory = Stats.isDirectory stats
  case isSymbolicLink, isDirectory of
    true, _ -> Om.throw { isSymbolicLinkError: path }
    _, true -> directoryToEntity bindings path
    _, _ -> liftAff $ fileToEntity bindings path

directoryToEntity
  :: forall ctx e. Bindings -> String -> Om (| ctx) (PathToEntityErrors + e) (Maybe Entity)
directoryToEntity bindings path = do
  ignorePatterns <- path # readIgnoreFile # liftAff
  children <- map (\p -> path <> "/" <> p) <$> (path # FileSystem.readdir # liftAff)
  validFiles <-
    children
      # List.fromFoldable
      # filterM (isNotSymbolicLink >>> liftAff)
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
