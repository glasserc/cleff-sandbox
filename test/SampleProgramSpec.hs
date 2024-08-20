module SampleProgramSpec where

import Cleff (runIOE)
import Data.IORef
import Data.List (isInfixOf)
import Effects.Interact
import SampleProgram
import Test.Hspec

spec :: Spec
spec = describe "chat" $ do
  it "works" $ do
    messagesRef <- newIORef []
    talker <- newTalker \nextText nextYesNo s -> do
      modifyIORef messagesRef (s :)
      case s of
        _
          | "name" `isInfixOf` s ->
              writeIORef nextText "Ethan"
        _
          | "airplanes" `isInfixOf` s ->
              writeIORef nextYesNo True
        _
          | "first class" `isInfixOf` s ->
              writeIORef nextYesNo False
        _ -> pure ()
    runIOE $ runInteractTalker talker chat
    messages <- readIORef messagesRef
    reverse messages
      `shouldBe` [ "What's your name?"
                 , "Nice to meet you, Ethan!"
                 , "Do you like airplanes?"
                 , "Do you fly first class?"
                 , "Well, maybe one day after the IPO!"
                 , "It was nice talking to you, Ethan. Hope you enjoy your trip!"
                 ]
