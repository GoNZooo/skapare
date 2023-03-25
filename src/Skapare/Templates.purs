module Skapare.Templates
  ( generateFromGitHub
  , generateFromPath
  , synthesize
  , listTemplates
  , instantiate
  , listCachedRepositories
  , isIgnored
  ) where

import Prelude

import Affjax.Node as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.List (filterM)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Utils as StringUtils
import Data.TemplateString as TemplateString
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
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
  , Sha
  , Template(..)
  , TemplateDescription
  , TemplateId
  , TemplateVariable
  )
import Skapare.Utilities as Utilities
import Type.Row (type (+))
import Yoga.Om (Om)
import Yoga.Om as Om

type TemplateContext ctx = { cacheDirectory :: String | ctx }

type InstantiationErrors e = (missingVariables :: Array TemplateVariable | e)

type ReadIgnoreFileError e = (badRegexInIgnoreFile :: String | e)

-- | Generates files from a template hosted on GitHub.
generateFromGitHub
  :: forall ctx e
   . GitHubSource
  -> TemplateId
  -> Bindings
  -> Om (TemplateContext ctx)
       (LoadTemplateFromGitHubErrors + LoadTemplateFromPathErrors + InstantiationErrors + e)
       Unit
generateFromGitHub source id bindings = do
  template <- loadTemplate source id
  fileOutputs <-
    bindings
      # unwrap
      # Map.toUnfoldable
      # instantiate template
      # Om.throwLeftAs (\missingVariables -> Om.error { missingVariables })
  fileOutputs
    # traverse_
        ( \(FileOutput { path: p, contents }) -> do
            Utilities.makeParentDirectories p
            FileSystem.writeTextFile Encoding.UTF8 p contents
        )
    # liftAff

-- | Generates files from a template stored on disk.
generateFromPath
  :: forall ctx e
   . FilePath
  -> Bindings
  -> Om (| ctx) (LoadTemplateFromPathErrors + InstantiationErrors + e) Unit
generateFromPath path bindings = do
  template <- loadTemplateFromPath path
  fileOutputs <- bindings # unwrap # Map.toUnfoldable # instantiate template
    # Om.throwLeftAs (\missingVariables -> Om.error { missingVariables })
  fileOutputs
    # traverse_
        ( \(FileOutput { path: p, contents }) -> do
            Utilities.makeParentDirectories p
            FileSystem.writeTextFile Encoding.UTF8 p contents
        )
    # liftAff

-- | Synthesizes a directory or file into a template.
synthesize
  :: forall ctx e
   . FilePath
  -> TemplateId
  -> TemplateDescription
  -> Bindings
  -> Maybe FilePath
  -> Om (| ctx) (ReadIgnoreFileError + PathToEntityErrors + e) Unit
synthesize path id description bindings outputDirectory = do
  template <- pathToTemplate id description bindings path
  let filename = fromMaybe "." outputDirectory <> "/" <> (unwrap id) <> ".json"
  template # Json.writeJSON # FileSystem.writeTextFile (Encoding.UTF8) filename # liftAff

-- | Prints templates in a GitHub repository.
listTemplates
  :: forall ctx e. GitHubSource -> Om (TemplateContext ctx) (GitHub.GetTreeErrors + e) Unit
listTemplates source = do
  (templateNames :: Array (Tuple TemplateId Sha)) <- Map.toUnfoldable <$> listTemplatesInGitHub source
  traverse_ printTemplateInfo templateNames
  where
  printTemplateInfo (id /\ sha) = do
    maybeTemplate <- loadCachedTemplate source id sha
    let cachedText = maybe "" (const "(cached)") maybeTemplate
    Console.log $ fold [ unwrap id, " ", cachedText ]

-- | Prints cached repositories.
listCachedRepositories :: forall ctx e. Om (TemplateContext ctx) e Unit
listCachedRepositories = do
  cachedRepositories <- getCachedRepositories
  traverse_ printCachedRepository cachedRepositories

-- | Gets cached repositories.
getCachedRepositories :: forall ctx e. Om (TemplateContext ctx) e (Array (Tuple FilePath FilePath))
getCachedRepositories = do
  { cacheDirectory } <- Om.ask
  users <- cacheDirectory # (_ <> "/github") # FileSystem.readdir # liftAff
  userRepositories <-
    users
      # traverse
          ( \u -> do
              repositories <- cacheDirectory # (_ <> "/github/" <> u) # FileSystem.readdir # liftAff
              repositories # map (\r -> u /\ r) # pure
          )
      # map join
  pure userRepositories

printCachedRepository :: forall ctx e. (Tuple FilePath FilePath) -> Om (TemplateContext ctx) e Unit
printCachedRepository (user /\ repository) = do
  { cacheDirectory } <- Om.ask
  let fullPath = cacheDirectory <> "/github/" <> user <> "/" <> repository
  templates <- fullPath # FileSystem.readdir # liftAff
  [ user, "/", repository ] # fold # Console.log
  traverse_ (("  " <> _) >>> Console.log) templates

-- | Lists template in a GitHub repository
listTemplatesInGitHub
  :: forall ctx e
   . GitHubSource
  -> Om (| ctx) (GitHub.GetTreeErrors + e) (Map TemplateId Sha)
listTemplatesInGitHub source = do
  GitHubTreeResponse { tree } <- GitHub.getTree source
  tree
    # Array.filter (\(GitHubTreeItem { path }) -> StringUtils.endsWith ".json" path)
    # map
        ( \(GitHubTreeItem { path, sha }) -> do
            let templateId = path # String.take (String.length path - 5) # wrap
            templateId /\ sha
        )
    # Map.fromFoldable
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
  -> Om (| ctx) (ReadIgnoreFileError + PathToEntityErrors + e) (Maybe Template)
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

type LoadTemplateFromPathErrors e = UnreadableFileError + TemplateDecodingError + e

type TemplateDecodingError e = (templateDecodingError :: MultipleErrors | e)

type UnreadableFileError e = (unreadableFile :: FilePath | e)

-- | Loads a template either from disk if cached or from GitHub if not.
loadTemplate
  :: forall ctx e
   . GitHubSource
  -> TemplateId
  -> Om (TemplateContext ctx) (LoadTemplateFromGitHubErrors + LoadTemplateFromPathErrors + e) Template
loadTemplate source id = do
  latestCachedVersion <- loadLatestCachedTemplate source id
  let
    mapWithCachedSha =
      maybe Map.empty (\(sha /\ _template) -> Map.singleton id sha) latestCachedVersion
    listTemplatesHandlers =
      { gitHubApiError: \_e -> pure mapWithCachedSha
      , jsonDecodingError: \_e -> pure mapWithCachedSha
      }
  githubTemplates <- Om.handleErrors listTemplatesHandlers $ listTemplatesInGitHub source
  let gitHubTemplateSha = Map.lookup id githubTemplates
  cachedTemplate <-
    maybe
      (pure Nothing)
      (\sha -> loadCachedTemplate source id sha <|> pure (map Tuple.snd latestCachedVersion))
      gitHubTemplateSha
  template <- maybe (loadTemplateFromGitHub source id) pure cachedTemplate
  traverse_ (cacheTemplate source template) gitHubTemplateSha
  pure template

-- | Loads the latest cached version of a template.
loadLatestCachedTemplate
  :: forall ctx e
   . GitHubSource
  -> TemplateId
  -> Om (TemplateContext ctx) e (Maybe (Tuple Sha Template))
loadLatestCachedTemplate source id = do
  { cacheDirectory } <- Om.ask
  let templateDirectory = templateCacheDirectory cacheDirectory source id
  versions <- (templateDirectory # FileSystem.readdir # liftAff) <|> pure []
  latest <- versions
    # map (\v -> templateDirectory <> "/" <> v)
    # traverse (\p -> (p /\ _) <$> liftAff (FileSystem.stat p))
    # map
        ( Array.sortBy
            ( \(_p1 /\ s1) (_p2 /\ s2) ->
                compare (Stats.modifiedTime s2) (Stats.modifiedTime s1)
            )
        )
    # map (map (Tuple.fst >>> flip Path.basenameWithoutExt ".json" >>> wrap) >>> Array.head)
  maybe (pure Nothing) (\sha -> sha # loadCachedTemplate source id # map (map (sha /\ _))) latest

cacheTemplate
  :: forall ctx e
   . GitHubSource
  -> Template
  -> Sha
  -> Om (TemplateContext ctx) (LoadTemplateFromGitHubErrors + e) Unit
cacheTemplate source template sha = do
  { cacheDirectory } <- Om.ask
  let templatePath = templateCachePath cacheDirectory source (template # unwrap # _.id) sha
  templatePath # Utilities.makeParentDirectories # liftAff
  template
    # Json.writeJSON
    # FileSystem.writeTextFile Encoding.UTF8 templatePath
    # liftAff
    # void

-- | Loads a cached template if it exists
loadCachedTemplate
  :: forall ctx e
   . GitHubSource
  -> TemplateId
  -> Sha
  -> Om (TemplateContext ctx) e (Maybe Template)
loadCachedTemplate source id sha = do
  { cacheDirectory } <- Om.ask
  let
    templatePath = templateCachePath cacheDirectory source id sha
    handlers = { unreadableFile: const (pure Nothing), templateDecodingError: const (pure Nothing) }
  Om.handleErrors handlers $ Just <$> loadTemplateFromPath templatePath

-- | Loads a template from a filename.
loadTemplateFromPath
  :: forall ctx e
   . FilePath
  -> Om (| ctx) (LoadTemplateFromPathErrors + e) Template
loadTemplateFromPath path = do
  fileData <-
    path
      # FileSystem.readTextFile Encoding.UTF8
      # liftAff
      # Utilities.handleError (const $ Om.throw { unreadableFile: path })
  let maybeTemplate = Json.readJSON fileData
  Om.throwLeftAs (\templateDecodingError -> Om.error { templateDecodingError }) maybeTemplate

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
  Console.log $ fold [ "Loading template '", unwrap id, "' from GitHub: ", url ]
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

pathToEntity
  :: forall ctx e
   . Bindings
  -> String
  -> Om (| ctx) (ReadIgnoreFileError + PathToEntityErrors + e) (Maybe Entity)
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
  :: forall ctx e
   . Bindings
  -> String
  -> Om (| ctx) (PathToEntityErrors + ReadIgnoreFileError + e) (Maybe Entity)
directoryToEntity bindings path = do
  ignorePatterns <- readIgnoreFile path
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

isIgnored :: Array Regex -> String -> Boolean
isIgnored ignorePatterns path =
  ignorePatterns # Array.any (\r -> Regex.test r path)

readIgnoreFile :: forall ctx e. String -> Om (| ctx) (ReadIgnoreFileError e) (Array Regex)
readIgnoreFile path = do
  hasIgnoreFile <- liftEffect $ FileSystemSync.exists (path <> "/.skapareignore")
  if hasIgnoreFile then do
    lines <- liftAff $ (String.split (Pattern "\n") >>> Array.filter (_ /= "")) <$>
      FileSystem.readTextFile Encoding.UTF8 (path <> "/.skapareignore")
    let linesAsRegex = traverse (\l -> Regex.regex l RegexFlags.noFlags) lines
    Om.throwLeftAs (\badRegexInIgnoreFile -> Om.error { badRegexInIgnoreFile }) linesAsRegex
  else pure []

templateCacheDirectory :: FilePath -> GitHubSource -> TemplateId -> FilePath
templateCacheDirectory cacheDirectory source id =
  fold
    [ cacheDirectory
    , "/github/"
    , source # unwrap # _.user
    , "/"
    , repo
    , "/"
    , unwrap id
    ]
  where
  repo = source # unwrap # _.repo # fromMaybe "skapare-templates"

templateCachePath :: FilePath -> GitHubSource -> TemplateId -> Sha -> FilePath
templateCachePath cacheDirectory source id sha =
  fold
    [ templateCacheDirectory cacheDirectory source id
    , "/"
    , unwrap sha
    , ".json"
    ]
