module Skapa.Types where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)

newtype TemplateId = TemplateId String

derive newtype instance eqTemplateId :: Eq TemplateId
derive newtype instance ordTemplateId :: Ord TemplateId
derive instance newtypeTemplateId :: Newtype TemplateId _
derive instance genericTemplateId :: Generic TemplateId _

instance showTemplateId :: Show TemplateId where
  show (TemplateId s) = "TemplateId " <> show s

newtype TemplateDescription = TemplateDescription String

derive newtype instance eqTemplateDescription :: Eq TemplateDescription
derive newtype instance ordTemplateDescription :: Ord TemplateDescription
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

data Entity
  = File { path :: String, content :: String }
  | Directory { path :: String, children :: Array Entity }

derive instance eqEntity :: Eq Entity
derive instance genericEntity :: Generic Entity _

instance showEntity :: Show Entity where
  show (File fileData) = "File " <> show fileData
  show (Directory directoryData) = "Directory " <> show directoryData

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
