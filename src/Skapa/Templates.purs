module Skapa.Templates
  ( instantiate
  , pathToTemplate
  , pathsToTemplate
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.TemplateString as TemplateString
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Aff as FileSystem
import Node.FS.Stats as Stats
import Skapa.Types
  ( Bindings
  , Entity(..)
  , FileOutput(..)
  , Template(..)
  , TemplateDescription
  , TemplateId
  )

-- | Instantiates a template with a set of variables so that it can be filled in.
instantiate :: Template -> Array (Tuple String String) -> Array FileOutput
instantiate (Template { entities }) variables =
  foldMap (instantiateEntity variables) entities

-- | Turns an array of paths into a template, reading all files for all paths.
pathsToTemplate
  :: TemplateId -> TemplateDescription -> Bindings -> Array String -> Aff (Maybe (Array Template))
pathsToTemplate id description bindings paths = do
  sequence <$> traverse (pathToTemplate id description bindings) paths

-- | Turns a path into a template, reading all files for the path if it's a directory or just the
-- | file if it's a file.
pathToTemplate :: TemplateId -> TemplateDescription -> Bindings -> String -> Aff (Maybe Template)
pathToTemplate id description bindings path =
  map (\entity -> Template { id, description, entities: [ entity ] }) <$> pathToEntity bindings path

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

pathToEntity :: Bindings -> String -> Aff (Maybe Entity)
pathToEntity bindings path = do
  stats <- FileSystem.stat path
  if Stats.isDirectory stats then directoryToEntity bindings path else fileToEntity bindings path

directoryToEntity :: Bindings -> String -> Aff (Maybe Entity)
directoryToEntity bindings path
  | String.contains (Pattern ".git") path = pure Nothing
  | otherwise = do
      children <- FileSystem.readdir path
      entities <- traverse (pathToEntity bindings) (map (\p -> path <> "/" <> p) children)
      { path, children: Array.catMaybes entities } # Directory # Just # pure

fileToEntity :: Bindings -> String -> Aff (Maybe Entity)
fileToEntity bindings path = do
  buffer <- FileSystem.readFile path
  content <- buffer # Buffer.toString Encoding.UTF8 # liftEffect
  let
    replacedContent =
      Array.foldl
        (\c (k /\ v) -> String.replaceAll (Pattern v) (Replacement $ fold [ "{{", k, "}}" ]) c)
        content
        (bindings # unwrap # Map.toUnfoldable)
  { path, content: replacedContent } # File # Just # pure
