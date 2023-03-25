module Main where

import Prelude

import Affjax as Affjax
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParse
import Data.Array as Array
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Path as Path
import Node.Process as Process
import Skapare.Templates as Templates
import Skapare.Types
  ( Bindings(..)
  , Command(..)
  , GitHubSource(..)
  , TemplateDescription(..)
  , TemplateId(..)
  )
import Yoga.Om as Om

main :: Effect Unit
main = do
  arguments <- Array.drop 2 <$> Process.argv
  let
    parsedCommand = ArgParse.parseArgs "skapa" "A tool for creating files from templates" parseCommand
      arguments
    handlers =
      { gitHubApiError: \e -> do
          [ "GitHub API error: ", Affjax.printError e ]
            # Array.fold
            # exitWith 1
      , repositoryNotFound: \gitHubSource -> do
          [ "Repository not found: ", show gitHubSource ]
            # Array.fold
            # exitWith 1
      , jsonDecodingError: \e -> do
          [ "Unable to decode JSON data: ", show e ]
            # Array.fold
            # exitWith 1
      , templateDecodingError: \e -> do
          [ "Unable to decode JSON data: ", show e ]
            # Array.fold
            # exitWith 1
      , missingVariables: \variables -> do
          [ "Missing variables for template instantiation: "
          , variables # map unwrap # Array.intercalate ", "
          ]
            # Array.fold
            # exitWith 1
      , isSymbolicLinkError: \path -> do
          [ "Found symbolic link in template usage: ", path ]
            # Array.fold
            # exitWith 1
      , statError: \path -> do
          [ "Unable to stat file: ", path ]
            # Array.fold
            # exitWith 1
      , cliError: \error -> do
          [ "Command line parsing error: ", ArgParse.printArgError error ]
            # Array.fold
            # exitWith 1
      , exception: \e -> do
          [ "Unknown error: ", show e ]
            # Array.fold
            # exitWith 1
      , unreadableFile: \path ->
          do
            [ "Unable to read file: ", path ]
              # Array.fold
              # exitWith 1
      , badRegexInIgnoreFile: \s -> do
          [ "Bad regex in ignore file: ", s ]
            # Array.fold
            # exitWith 1
      }

  Aff.launchAff_ do
    cacheDirectory <- do
      homeDirectory <- fromMaybe "~" <$> liftEffect (Process.lookupEnv "HOME")
      baseCacheDirectory <-
        fromMaybe (fold [ homeDirectory, "/.cache/" ]) <$>
          liftEffect (Process.lookupEnv "XDG_CACHE_HOME")
      [ baseCacheDirectory, "skapare/" ] # fold # Path.resolve [ "." ] # liftEffect

    liftEffect $ Om.launchOm_ { cacheDirectory } handlers do
      command <- Om.throwLeftAs (\cliError -> Om.error { cliError }) parsedCommand
      case command of
        GenerateFromGitHub { source, id, bindings } -> do
          Templates.generateFromGitHub source id bindings

        GenerateFromPath { path, bindings } -> do
          Templates.generateFromPath path bindings

        Synthesize { path, id, description, bindings, outputDirectory } -> do
          Templates.synthesize path id description bindings outputDirectory

        ListTemplates { source } -> do
          Templates.listTemplates source

        ListCachedRepositories -> do
          Templates.listCachedRepositories

parseCommand :: ArgParser Command
parseCommand =
  ArgParse.choose "command"
    [ ArgParse.command [ "generate", "g" ] "Generate files from a template" do
        parseGenerate <* ArgParse.flagHelp
    , ArgParse.command [ "synthesize", "s" ] "Synthesize a template from a file/directory" do
        Synthesize
          <$> ArgParse.fromRecord
            { path: ArgParse.argument [ "-p", "--path" ] "Path to file/directory"
            , id: TemplateId <$> ArgParse.argument [ "-i", "--id" ] "Template name/ID"
            , description: TemplateDescription <$> ArgParse.argument [ "-d", "--description" ]
                "Template description"
            , bindings: parseBindings
            , outputDirectory: ArgParse.optional
                (ArgParse.argument [ "-o", "--output" ] "Output directory")
            }
          <* ArgParse.flagHelp
    , ArgParse.command [ "list", "l" ] "List available templates in repository" do
        ListTemplates
          <$> ArgParse.fromRecord
            { source: parseTemplateSource
            }
          <* ArgParse.flagHelp
    , ArgParse.command [ "cached-repos" ] "List cached repositories" do
        pure ListCachedRepositories <$> ArgParse.flagHelp
    ]

parseGenerate :: ArgParser Command
parseGenerate = ArgParse.choose "generate arguments"
  [ GenerateFromGitHub
      <$> ArgParse.fromRecord
        { source: parseTemplateSource
        , id: TemplateId <$> ArgParse.argument [ "-i", "--id" ] "Template name/ID"
        , bindings: parseBindings
        }
  , GenerateFromPath
      <$> ArgParse.fromRecord
        { path: ArgParse.argument [ "-p", "--path" ] "Path to template"
        , bindings: parseBindings
        }
  ]

parseTemplateSource :: ArgParser GitHubSource
parseTemplateSource =
  GitHubSource
    <$> ArgParse.fromRecord
      { user: ArgParse.argument [ "-u", "--user" ] "Which user to access"
      , repo:
          ArgParse.optional
            (ArgParse.argument [ "-r", "--repo" ] "Which repo to access")
      }

parseBindings :: ArgParser Bindings
parseBindings = (Map.fromFoldable >>> Bindings) <$> ArgParse.many parseEqualBinding

parseEqualBinding :: ArgParser (Tuple String String)
parseEqualBinding =
  ArgParse.any "BINDING" "A binding of the form `key=value`" \s ->
    case String.split (Pattern "=") s of
      [ key, value ] -> Just $ Tuple key value
      _ -> Nothing

exitWith :: forall m. MonadEffect m => Int -> String -> m Unit
exitWith code message = do
  Console.error message
  code # Process.exit # liftEffect
