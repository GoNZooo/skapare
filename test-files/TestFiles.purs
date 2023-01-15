module TestFiles where

import TestFiles.OtherFile as OtherFile

hello :: String
hello = "This is the TestFiles project!"

usesKebabCase :: String
usesKebabCase = "This is the test-files project!"

helloFromOtherFile :: String
helloFromOtherFile = OtherFile.hello
