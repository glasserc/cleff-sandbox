module SampleProgramSpec where

import Cleff (runPure)
import Cleff.Output (outputToListState)
import Cleff.State (runState)
import Data.List (isInfixOf)
import Effects.Interact
import SampleProgram
import Test.Hspec

spec :: Spec
spec = describe "chat" $ do
  it "works" $ do
    let talker =
          Talker
            { respondText = \_ -> "Ethan"
            , respondYesNo = \s -> case s of
                _
                  | "airplanes" `isInfixOf` s ->
                      True
                _
                  | "first class" `isInfixOf` s ->
                      False
                _ -> False
            }
    let (_, messages) =
          runPure . runState [] . outputToListState $ runInteractTalker talker chat
    reverse messages
      `shouldBe` [ "What's your name?"
                 , "Nice to meet you, Ethan!"
                 , "Do you like airplanes?"
                 , "Do you fly first class?"
                 , "Well, maybe one day after the IPO!"
                 , "It was nice talking to you, Ethan. Hope you enjoy your trip!"
                 ]
