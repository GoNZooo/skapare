module Skapa.TemplatesSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import Skapa.Templates as Template
import Skapa.Types (Entity(..), FileOutput(..), Template(..), TemplateDescription(..), TemplateId(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Skapa.Templates" do
    it "should be able to instantiate a simple file entity" do
      let
        template = Template
          { id: TemplateId "id", description: TemplateDescription "description", entities }
        entities =
          [ File { path: "path", content: "This is a templated value: ${value}" }
          ]
      Template.instantiate template [ "value" /\ "test" ] `shouldEqual`
        [ FileOutput { path: "path", contents: "This is a templated value: test" }
        ]

    it "should be able to instantiate a directory of file entities" do
      let
        template = Template
          { id: TemplateId "id", description: TemplateDescription "description", entities }
        entities =
          [ Directory
              { path: "path"
              , children: [ File { path: "file", content: "This is a templated value: ${value}" } ]
              }
          , Directory { path: "other", children: [] }
          , File { path: "file", content: "This is a templated value: ${value}" }
          , Directory
              { path: "nested"
              , children:
                  [ File { path: "file", content: "This is a templated value: ${value1}:${value2}" }
                  , File
                      { path: "other"
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
        [ FileOutput { path: "path/file", contents: "This is a templated value: test" }
        , FileOutput { path: "file", contents: "This is a templated value: test" }
        , FileOutput
            { path: "nested/file", contents: "This is a templated value: other-test-value-1:42" }
        , FileOutput
            { path: "nested/other"
            , contents: "This is another templated value: other-test-value-1:42"
            }
        ]
