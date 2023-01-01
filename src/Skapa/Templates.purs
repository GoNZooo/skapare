module Skapa.Templates
  ( instantiate
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.TemplateString as TemplateString
import Data.Tuple (Tuple)
import Skapa.Types (Entity(..), FileOutput(..), Template(..))

-- | Instantiates a template with a set of variables so that it can be filled in.
instantiate :: Template -> Array (Tuple String String) -> Array FileOutput
instantiate (Template { entities }) variables =
  foldMap (instantiateEntity variables) entities

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
