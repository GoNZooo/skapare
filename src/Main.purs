module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParse
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Node.Process as Process
import Skapa.Types (TemplateId(..), TemplateSource(..))

type Bindings = Map String String

data Command = Generate { source :: TemplateSource, id :: TemplateId, bindings :: Bindings }

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
parseBindings = Map.fromFoldable <$> ArgParse.many parseEqualBinding

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
