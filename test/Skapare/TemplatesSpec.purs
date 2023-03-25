module Skapare.TemplatesSpec where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Tuple.Nested ((/\))
import Partial.Unsafe as PartialUnsafe
import Skapare.Templates as Template
import Skapare.Types
  ( Entity(..)
  , FileOutput(..)
  , Template(..)
  , TemplateDescription(..)
  , TemplateId(..)
  )
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "Skapa.Templates" do
    it "should be able to instantiate a simple file entity" do
      let
        template = Template
          { id: TemplateId "id"
          , description: TemplateDescription "description"
          , entities
          , variables: map wrap [ "value" ]
          }
        entities =
          [ File { path: "path", content: "This is a templated value: ${value}" }
          ]
      Template.instantiate template [ "value" /\ "test" ] `shouldEqual`
        Right
          [ FileOutput { path: "path", contents: "This is a templated value: test" }
          ]

      quickCheck \(value :: String) -> do
        Template.instantiate template [ "value" /\ value ] === Right
          [ FileOutput { path: "path", contents: "This is a templated value: " <> value }
          ]

    it "should be able to instantiate a directory of file entities" do
      let
        template = Template
          { id: TemplateId "id"
          , description: TemplateDescription "description"
          , entities
          , variables: map wrap [ "value", "value1", "value2" ]
          }
        entities =
          [ Directory
              { path: "path"
              , children:
                  [ File { path: "path/file", content: "This is a templated value: ${value}" } ]
              }
          , Directory { path: "other", children: [] }
          , File { path: "file", content: "This is a templated value: ${value}" }
          , Directory
              { path: "nested"
              , children:
                  [ File
                      { path: "nested/file"
                      , content: "This is a templated value: ${value1}:${value2}"
                      }
                  , File
                      { path: "nested/other"
                      , content: "This is another templated value: ${value1}:${value2}"
                      }
                  ]
              }
          ]
      Template.instantiate template
        [ "value" /\ "test"
        , "value1" /\ "other-test-value-1"
        , "value2" /\ "42"
        ] `shouldEqual`
        Right
          [ FileOutput { path: "path/file", contents: "This is a templated value: test" }
          , FileOutput { path: "file", contents: "This is a templated value: test" }
          , FileOutput
              { path: "nested/file", contents: "This is a templated value: other-test-value-1:42" }
          , FileOutput
              { path: "nested/other"
              , contents: "This is another templated value: other-test-value-1:42"
              }
          ]

    it "should fail if a variable is missing" do
      let
        template = Template
          { id: TemplateId "id"
          , description: TemplateDescription "description"
          , entities
          , variables: map wrap [ "value" ]
          }
        entities =
          [ File { path: "path", content: "This is a templated value: ${value}" }
          ]
      Template.instantiate template [] `shouldEqual`
        Left [ wrap "value" ]

  describe "Ignore files" do
    it "Should ignore git top-level directory with correct ignore line" do
      let
        ignoredFiles = [ stringToRegex ".git$" ]
        path1 = "directory/.git"
        path2 = "directory/.git/HEAD"
        path3 = "directory/.gitignore"
      Template.isIgnored ignoredFiles path1 `shouldEqual` true
      Template.isIgnored ignoredFiles path2 `shouldEqual` false
      Template.isIgnored ignoredFiles path3 `shouldEqual` false

stringToRegex :: String -> Regex
stringToRegex s = Regex.regex s RegexFlags.noFlags # unsafeFromRight

unsafeFromRight :: forall l r. Either l r -> r
unsafeFromRight (Right v) = v
unsafeFromRight (Left _) = PartialUnsafe.unsafeCrashWith "unsafeFromRight: Left"
