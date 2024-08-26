module Effects.InteractSpec where

import Cleff (runPure)
import Effects.Interact
import Effects.Teletype
import Test.Hspec

spec :: Spec
spec = describe "readUntilYesOrNo" do
  it "reads until it gets something it understands" do
    let inputs =
          [ "nop"
          , "yes"
          ]
        (val, outputs) = runPure $ runTeletypePure inputs readUntilYesOrNo
    outputs
      `shouldBe` [ "Please enter a response (yes or no):"
                 , "I'm sorry, I didn't understand your response."
                 , "Please enter a response (yes or no):"
                 ]
    val
      `shouldBe` True
  it "understands no" do
    let inputs =
          [ "no"
          , "yes"
          ]
        (val, outputs) = runPure $ runTeletypePure inputs readUntilYesOrNo
    outputs
      `shouldBe` [ "Please enter a response (yes or no):"
                 ]
    val
      `shouldBe` False
