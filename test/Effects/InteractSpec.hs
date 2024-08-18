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
        yesOrNoAndPrint = readUntilYesOrNo >>= writeTTY . show
        outputs = runPure $ runTeletypePure inputs yesOrNoAndPrint
    outputs
      `shouldBe` [ "Please enter a response (yes or no):"
                 , "I'm sorry, I didn't understand your response."
                 , "Please enter a response (yes or no):"
                 , "True"
                 ]
  it "understands no" do
    let inputs =
          [ "no"
          , "yes"
          ]
        yesOrNoAndPrint = readUntilYesOrNo >>= writeTTY . show
        outputs = runPure $ runTeletypePure inputs yesOrNoAndPrint
    outputs
      `shouldBe` [ "Please enter a response (yes or no):"
                 , "False"
                 ]
