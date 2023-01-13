module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParse
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Node.Process as Process
import Simple.JSON as Json
import Skapa.Templates as Templates
import Skapa.Types
  ( Bindings(..)
  , Command(..)
  , TemplateDescription(..)
  , TemplateId(..)
  , TemplateSource(..)
  )

parseCommand :: ArgParser Command
parseCommand =
  ArgParse.choose "command"
    [ ArgParse.command [ "generate", "g" ] "Generate files from a template" do
        Generate
          <$> ArgParse.fromRecord
            { source: parseTemplateSource
            , id: TemplateId <$> ArgParse.argument [ "-i", "-id" ] "Template name/ID"
            , bindings: parseBindings
            }
          <* ArgParse.flagHelp
    , ArgParse.command [ "synthesize", "s" ] "Synthesize a template from a file/directory" do
        Synthesize
          <$> ArgParse.fromRecord
            { path: ArgParse.argument [ "-p", "-path" ] "Path to file/directory"
            , id: TemplateId <$> ArgParse.argument [ "-i", "-id" ] "Template name/ID"
            , description: TemplateDescription <$> ArgParse.argument [ "-d", "-description" ]
                "Template description"
            , bindings: parseBindings
            , outputDirectory: ArgParse.optional
                (ArgParse.argument [ "-o", "-output" ] "Output directory")
            }
          <* ArgParse.flagHelp
    ]

parseTemplateSource :: ArgParser TemplateSource
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

  case parsedCommand of
    Left error -> do
      Console.error $ ArgParse.printArgError error
      Process.exit 1
    Right command -> do
      case command of
        Generate { source, id, bindings } -> do
          Console.log $ "Creating a new project from a template from " <> show source <> " with id "
            <> show id
            <> " and bindings "
            <> show bindings
        Synthesize { path, id, description, bindings, outputDirectory } -> Aff.launchAff_ do
          template <- Templates.pathToTemplate id description bindings path
          let filename = fromMaybe "." outputDirectory <> "/" <> (unwrap id) <> ".json"
          FS.writeTextFile (Encoding.UTF8) filename (Json.writeJSON template)
