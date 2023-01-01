module Skapa.Types where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)

newtype Template = Template
  { id :: String
  , description :: String
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

newtype Identifier = Identifier String

derive newtype instance eqIdentifier :: Eq Identifier
derive newtype instance ordIdentifier :: Ord Identifier
derive instance genericIdentifier :: Generic Identifier _
derive instance newtypeIdentifier :: Newtype Identifier _

instance showIdentifier :: Show Identifier where
  show = genericShow
