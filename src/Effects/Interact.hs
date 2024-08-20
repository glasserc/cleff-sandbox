{-# LANGUAGE TemplateHaskell #-}

module Effects.Interact where

import Cleff
import Data.IORef
import Effects.Teletype

-- An effect for code that wants to interact with the user through a
-- text interface.
data Interact :: Effect where
  InputText :: Interact m String
  YesOrNo :: Interact m Bool
  Display :: String -> Interact m ()

makeEffect ''Interact

-- Using the Teletype effect, ask the user for either a "yes" or "no"
-- input and convert it to a Bool.
readUntilYesOrNo :: (Teletype :> es) => Eff es Bool
readUntilYesOrNo = do
  writeTTY "Please enter a response (yes or no):"
  res <- readTTY
  case res of
    "yes" -> pure True
    "no" -> pure False
    _ -> do
      writeTTY "I'm sorry, I didn't understand your response."
      readUntilYesOrNo

runInteractTeletype :: Eff (Interact : es) a -> Eff (Teletype : es) a
runInteractTeletype = reinterpret \case
  InputText -> readTTY
  YesOrNo -> readUntilYesOrNo
  Display s -> writeTTY s

data Talker = Talker
  { nextText :: IORef String
  , nextYesNo :: IORef Bool
  , readDisplay :: String -> IO ()
  -- ^ Update the IO refs based on the program output
  }

newTalker :: (IORef String -> IORef Bool -> String -> IO ()) -> IO Talker
newTalker f = do
  nextText <- newIORef ""
  nextYesNo <- newIORef False
  pure Talker {readDisplay = f nextText nextYesNo, ..}

runInteractTalker :: (IOE :> es) => Talker -> Eff (Interact : es) a -> Eff es a
runInteractTalker talker = interpret \case
  InputText -> liftIO (readIORef talker.nextText)
  YesOrNo -> liftIO (readIORef talker.nextYesNo)
  Display s -> liftIO (talker.readDisplay s)
