module Skapa.Types where

import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)
import Record as Record
import Simple.JSON (class WriteForeign, writeImpl)

-- | The source of templates we want to instantiate.
data TemplateSource = GitHubSource
  { user :: String, repo :: Maybe String }

derive instance eqTemplateSource :: Eq TemplateSource
derive instance genericTemplateSource :: Generic TemplateSource _
derive instance ordTemplateSource :: Ord TemplateSource

instance showTemplateSource :: Show TemplateSource where
  show = genericShow

newtype TemplateId = TemplateId String

derive newtype instance eqTemplateId :: Eq TemplateId
derive newtype instance ordTemplateId :: Ord TemplateId
derive newtype instance writeForeignTemplateId :: WriteForeign TemplateId
derive instance newtypeTemplateId :: Newtype TemplateId _
derive instance genericTemplateId :: Generic TemplateId _

instance showTemplateId :: Show TemplateId where
  show (TemplateId s) = "TemplateId " <> show s

newtype TemplateDescription = TemplateDescription String

derive newtype instance eqTemplateDescription :: Eq TemplateDescription
derive newtype instance ordTemplateDescription :: Ord TemplateDescription
derive newtype instance writeForeignTemplateDescription :: WriteForeign TemplateDescription
derive instance newtypeTemplateDescription :: Newtype TemplateDescription _
derive instance genericTemplateDescription :: Generic TemplateDescription _

instance showTemplateDescription :: Show TemplateDescription where
  show (TemplateDescription s) = "TemplateDescription " <> show s

newtype Template = Template
  { id :: TemplateId
  , description :: TemplateDescription
  , entities :: Array Entity
  }

derive newtype instance eqTemplate :: Eq Template
derive instance genericTemplate :: Generic Template _
derive instance newtypeTemplate :: Newtype Template _
derive newtype instance writeForeignTemplate :: WriteForeign Template

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
