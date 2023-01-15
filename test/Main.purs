module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Skapare.TemplatesSpec as TemplatesSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ] do
    TemplatesSpec.spec
