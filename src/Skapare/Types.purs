module Skapare.Types where

import Prelude

import Affjax.Node as Affjax
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Foreign (ForeignError(..), MultipleErrors, fail)
import Node.Path (FilePath)
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype GitHubTreeResponse = GitHubTreeResponse
  { tree :: Array GitHubTreeItem
  }

derive instance Generic GitHubTreeResponse _
derive instance Eq GitHubTreeResponse

instance Show GitHubTreeResponse where
  show = genericShow

instance ReadForeign GitHubTreeResponse where
  readImpl f = GitHubTreeResponse <$> readImpl f

newtype GitHubTreeItem = GitHubTreeItem
  { path :: String
  }

derive instance Generic GitHubTreeItem _
derive instance Eq GitHubTreeItem

instance Show GitHubTreeItem where
  show = genericShow

instance ReadForeign GitHubTreeItem where
  readImpl f = GitHubTreeItem <$> readImpl f

data ListTemplatesInGitHubError
  = ListTemplatesGitHubFetchError Affjax.Error
  | ListTemplatesDecodingError MultipleErrors

derive instance Generic ListTemplatesInGitHubError _

instance Show ListTemplatesInGitHubError where
  show (ListTemplatesGitHubFetchError e) = "ListTemplatesGitHubFetchError " <> Affjax.printError e
  show (ListTemplatesDecodingError e) = "ListTemplatesDecodingError " <> show e

data LoadTemplateFromGitHubError
  = LoadTemplateGitHubFetchError Affjax.Error
  | LoadTemplateDecodingError MultipleErrors

derive instance Generic LoadTemplateFromGitHubError _

instance Show LoadTemplateFromGitHubError where
  show (LoadTemplateGitHubFetchError e) = "LoadTemplateGitHubFetchError " <> Affjax.printError e
  show (LoadTemplateDecodingError e) = "LoadTemplateDecodingError " <> show e

data InstantiationError = MissingVariables (Array TemplateVariable)

derive instance Generic InstantiationError _
derive instance Eq InstantiationError

instance Show InstantiationError where
  show (MissingVariables variables) = "MissingVariables " <> show variables

data Command
  = GenerateFromGitHub
      { source :: GitHubSource
      , id :: TemplateId
      , bindings :: Bindings
      }
  | GenerateFromPath
      { path :: FilePath
      , bindings :: Bindings
      }
  | Synthesize
      { path :: FilePath
      , id :: TemplateId
      , description :: TemplateDescription
      , bindings :: Bindings
      , outputDirectory :: Maybe FilePath
      }
  | ListTemplates { source :: GitHubSource }

newtype Bindings = Bindings (Map String String)

derive instance eqBindings :: Eq Bindings
derive instance newtypeBindings :: Newtype Bindings _
derive instance genericBindings :: Generic Bindings _
derive newtype instance showBindings :: Show Bindings

newtype GitHubSource = GitHubSource { user :: String, repo :: Maybe String }

derive instance eqTemplateSource :: Eq GitHubSource
derive instance genericTemplateSource :: Generic GitHubSource _
derive instance ordTemplateSource :: Ord GitHubSource

instance showTemplateSource :: Show GitHubSource where
  show = genericShow

newtype TemplateId = TemplateId String

derive newtype instance eqTemplateId :: Eq TemplateId
derive newtype instance ordTemplateId :: Ord TemplateId
derive newtype instance writeForeignTemplateId :: WriteForeign TemplateId
derive newtype instance readForeignTemplateId :: ReadForeign TemplateId
derive instance newtypeTemplateId :: Newtype TemplateId _
derive instance genericTemplateId :: Generic TemplateId _

instance showTemplateId :: Show TemplateId where
  show (TemplateId s) = "TemplateId " <> show s

newtype TemplateDescription = TemplateDescription String

derive newtype instance eqTemplateDescription :: Eq TemplateDescription
derive newtype instance ordTemplateDescription :: Ord TemplateDescription
derive newtype instance writeForeignTemplateDescription :: WriteForeign TemplateDescription
derive newtype instance readForeignTemplateDescription :: ReadForeign TemplateDescription
derive instance newtypeTemplateDescription :: Newtype TemplateDescription _
derive instance genericTemplateDescription :: Generic TemplateDescription _

instance showTemplateDescription :: Show TemplateDescription where
  show (TemplateDescription s) = "TemplateDescription " <> show s

newtype TemplateVariable = TemplateVariable String

derive newtype instance eqTemplateVariable :: Eq TemplateVariable
derive newtype instance ordTemplateVariable :: Ord TemplateVariable
derive newtype instance writeForeignTemplateVariable :: WriteForeign TemplateVariable
derive newtype instance readForeignTemplateVariable :: ReadForeign TemplateVariable
derive instance newtypeTemplateVariable :: Newtype TemplateVariable _
derive instance genericTemplateVariable :: Generic TemplateVariable _

instance showTemplateVariable :: Show TemplateVariable where
  show (TemplateVariable s) = "TemplateVariable " <> show s

newtype Template = Template
  { id :: TemplateId
  , description :: TemplateDescription
  , entities :: Array Entity
  , variables :: Array TemplateVariable
  }

derive newtype instance eqTemplate :: Eq Template
derive instance genericTemplate :: Generic Template _
derive instance newtypeTemplate :: Newtype Template _
derive newtype instance writeForeignTemplate :: WriteForeign Template
derive newtype instance readForeignTemplate :: ReadForeign Template

instance showTemplate :: Show Template where
  show = genericShow

data Entity
  = File { path :: String, content :: String }
  | Directory { path :: String, children :: Array Entity }

derive instance eqEntity :: Eq Entity
derive instance genericEntity :: Generic Entity _

instance showEntity :: Show Entity where
  show (File fileData) = "File " <> show fileData
  show (Directory directoryData) = "Directory " <> show directoryData

instance writeForeignEntity :: WriteForeign Entity where
  writeImpl (File d) = writeImpl $ d `Record.merge` { type: "file" }
  writeImpl (Directory d) = writeImpl $ d `Record.merge` { type: "directory" }

instance readForeignEntity :: ReadForeign Entity where
  readImpl f = do
    r <- (HasTypeField >>> unwrap) <$> readImpl f
    case r.type of
      "file" -> File <$> readImpl f
      "directory" -> Directory <$> readImpl f
      other -> fail $ ForeignError $ "Unknown 'Entity' type field: " <> other

newtype FileOutput = FileOutput
  { path :: String
  , contents :: String
  }

derive newtype instance eqFileOutput :: Eq FileOutput
derive instance genericFileOutput :: Generic FileOutput _
derive instance newtypeFileOutput :: Newtype FileOutput _

instance showFileOutput :: Show FileOutput where
  show = genericShow

newtype Identifier = Identifier String

derive newtype instance eqIdentifier :: Eq Identifier
derive newtype instance ordIdentifier :: Ord Identifier
derive instance genericIdentifier :: Generic Identifier _
derive instance newtypeIdentifier :: Newtype Identifier _

instance showIdentifier :: Show Identifier where
  show = genericShow

newtype HasTypeField = HasTypeField { type :: String }

derive instance newtypeHasTypeField :: Newtype HasTypeField _

instance readForeignHasTypeField :: ReadForeign HasTypeField where
  readImpl f = HasTypeField <$> readImpl f
