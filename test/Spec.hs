module Main (main) where

import Effects.InteractSpec qualified as InteractSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  InteractSpec.spec
