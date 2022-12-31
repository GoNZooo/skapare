module Main where

import Prelude

import Data.TemplateString as TemplateString
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console as Console

main :: Effect Unit
main = do
  let templatedString = TemplateString.template "Hello, ${name}!" [ "name" /\ "you" ]
  Console.log templatedString
