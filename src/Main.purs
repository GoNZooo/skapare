module Main where

import Prelude

import Affjax as Affjax
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParse
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Encoding as Encoding
import Node.FS.Aff as FileSystem
import Node.FS.Perms as Perms
import Node.Path as Path
import Node.Process as Process
import Simple.JSON as Json
import Skapare.Templates as Templates
import Skapare.Types
  ( Bindings(..)
  , Command(..)
  , FileOutput(..)
  , TemplateDescription(..)
  , TemplateId(..)
  , GitHubSource(..)
  )
import Yoga.Om as Om

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
      , gitHubDecodeError: \e -> do
          [ "Unable to decode GitHub response data: ", show e ]
            # Array.fold
            # exitWith 1
      , exception: \e -> do
          [ "Unknown error: ", show e ]
            # Array.fold
            # exitWith 1
      }

  case parsedCommand of
    Left error -> do
      Console.error $ ArgParse.printArgError error
      Process.exit 1
    Right command -> do
      case command of
        GenerateFromGitHub { source, id, bindings } -> Aff.launchAff_ do
          maybeTemplate <- Templates.loadTemplateFromGitHub source id
          case maybeTemplate of
            Left error -> do
              liftEffect $ Console.error $ "Error loading template: " <> show error
              liftEffect $ Process.exit 1
            Right template -> do
              case Templates.instantiate template (bindings # unwrap # Map.toUnfoldable) of
                Right fileOutputs -> traverse_
                  ( \(FileOutput { path: p, contents }) -> do
                      makeParentDirectories p
                      FileSystem.writeTextFile Encoding.UTF8 p contents
                  )
                  fileOutputs
                Left error -> do
                  liftEffect $ Console.error $ "Error instantiating template: " <> show error
                  liftEffect $ Process.exit 1
        GenerateFromPath { path, bindings } -> Aff.launchAff_ do
          maybeTemplate <- Templates.loadTemplateFromPath path
          case maybeTemplate of
            Left errors -> do
              liftEffect $ Console.error $ "Failed to load template: " <> show errors
              liftEffect $ Process.exit 1
            Right template -> do
              case Templates.instantiate template (bindings # unwrap # Map.toUnfoldable) of
                Right fileOutputs -> traverse_
                  ( \(FileOutput { path: p, contents }) -> do
                      makeParentDirectories p
                      FileSystem.writeTextFile Encoding.UTF8 p contents
                  )
                  fileOutputs
                Left error -> do
                  liftEffect $ Console.error $ "Error instantiating template: " <> show error
                  liftEffect $ Process.exit 1

        Synthesize { path, id, description, bindings, outputDirectory } -> Aff.launchAff_ do
          template <- Templates.pathToTemplate id description bindings path
          let filename = fromMaybe "." outputDirectory <> "/" <> (unwrap id) <> ".json"
          FileSystem.writeTextFile (Encoding.UTF8) filename (Json.writeJSON template)

        ListTemplates { source } -> do
          Om.launchOm_ {} handlers do
            templateNames <- Templates.listTemplatesInGitHub source
            templateNames # traverse_ (unwrap >>> Console.log) # liftEffect

exitWith :: forall m. MonadEffect m => Int -> String -> m Unit
exitWith code message = do
  Console.error message
  code # Process.exit # liftEffect

makeParentDirectories :: String -> Aff Unit
makeParentDirectories path = FileSystem.mkdir' (Path.dirname path) { recursive: true, mode }
  where
  mode = Perms.mkPerms full Perms.none Perms.none
  full = Perms.read + Perms.write + Perms.execute
