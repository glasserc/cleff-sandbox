module Main (main) where

import Effects.InteractSpec qualified as InteractSpec
import SampleProgramSpec qualified as SampleProgramSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  InteractSpec.spec
  SampleProgramSpec.spec
