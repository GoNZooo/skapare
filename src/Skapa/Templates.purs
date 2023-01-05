module Skapa.Templates
  ( instantiate
  , pathToTemplate
  , pathsToTemplate
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.TemplateString as TemplateString
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Aff as FileSystem
import Node.FS.Stats as Stats
import Skapa.Types (Entity(..), FileOutput(..), Template(..), TemplateDescription, TemplateId)

-- | Instantiates a template with a set of variables so that it can be filled in.
instantiate :: Template -> Array (Tuple String String) -> Array FileOutput
instantiate (Template { entities }) variables =
  foldMap (instantiateEntity variables) entities

-- | Turns an array of paths into a template, reading all files for all paths.
pathsToTemplate :: TemplateId -> TemplateDescription -> Array String -> Aff (Maybe (Array Template))
pathsToTemplate id description paths = do
  sequence <$> traverse (pathToTemplate id description) paths

-- | Turns a path into a template, reading all files for the path if it's a directory or just the
-- | file if it's a file.
pathToTemplate :: TemplateId -> TemplateDescription -> String -> Aff (Maybe Template)
pathToTemplate id description path =
  map (\entity -> Template { id, description, entities: [ entity ] }) <$> pathToEntity path

instantiateEntity :: Array (Tuple String String) -> Entity -> Array FileOutput
instantiateEntity variables (File { path, content }) =
  { path, contents: TemplateString.template content variables } # FileOutput # Array.singleton
instantiateEntity variables (Directory { path, children }) =
  foldMap (instantiateEntity variables) (map (prependPath path) children)

prependPath :: String -> Entity -> Entity
prependPath path (File { path: childPath, content }) =
  File { path: path <> "/" <> childPath, content }
prependPath path (Directory { path: childPath, children }) =
  Directory { path: path <> "/" <> childPath, children }

pathToEntity :: String -> Aff (Maybe Entity)
pathToEntity path = do
  stats <- FileSystem.stat path
  if Stats.isDirectory stats then directoryToEntity path else fileToEntity path

directoryToEntity :: String -> Aff (Maybe Entity)
directoryToEntity path = do
  children <- FileSystem.readdir path
  entities <- traverse pathToEntity (map (\p -> path <> "/" <> p) children)
  { path, children: Array.catMaybes entities } # Directory # Just # pure

fileToEntity :: String -> Aff (Maybe Entity)
fileToEntity path = do
  buffer <- FileSystem.readFile path
  content <- buffer # Buffer.toString Encoding.UTF8 # liftEffect
  { path, content } # File # Just # pure
