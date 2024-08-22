module SampleProgramSpec where

import Cleff (Eff, runPure)
import Cleff.Output (Output, ignoreOutput, outputToListState)
import Cleff.State (runState)
import Data.List (isInfixOf)
import Data.Set qualified as Set
import Effects.Interact
import Effects.UserStore
import SampleProgram
import Test.Hspec

ethan :: Talker
ethan =
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

ethanRecord :: UserRecord
ethanRecord = UserRecord {name = "Ethan", flies = True, rich = False}

spec :: Spec
spec = describe "chat" $ do
  it "interacts properly" $ do
    let (_, messages) =
          runPure . runState [] . outputToListState $
            runUserStorePureDiscardResult mempty $
              runInteractTalker ethan chat
    reverse messages
      `shouldBe` [ "What's your name?"
                 , "Nice to meet you, Ethan!"
                 , "Do you like airplanes?"
                 , "Do you fly first class?"
                 , "Well, maybe one day after the IPO!"
                 , "It was nice talking to you, Ethan. Hope you enjoy your trip!"
                 ]
  it "stores new users" $ do
    let (_, users) =
          runPure . (ignoreOutput :: Eff (Output String : es) a -> Eff es a) $
            runUserStorePure mempty $
              runInteractTalker ethan chat
    Set.toList users
      `shouldBe` [ethanRecord]
