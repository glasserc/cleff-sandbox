{-# LANGUAGE TemplateHaskell #-}

module Effects.Interact where

import Cleff
import Cleff.Output
import Effects.Teletype

-- An effect for code that wants to interact with the user through a
-- text interface.
--
-- In a real system, we probably wouldn't have this -- all the
-- operations could be converted into functions against the `Teletype`
-- effect -- but we are using this effect here as a demonstration of
-- what it might look like to build one effect "on top of" another
-- one. If you have a better idea for an example, let me know!
data Interact :: Effect where
  PromptText :: String -> Interact m String
  PromptYesOrNo :: String -> Interact m Bool
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
  PromptText s -> writeTTY s >> readTTY
  PromptYesOrNo s -> writeTTY s >> readUntilYesOrNo
  Display s -> writeTTY s

data Talker = Talker
  { respondText :: String -> String
  , respondYesNo :: String -> Bool
  }

runInteractTalker :: Talker -> Eff (Interact : es) a -> Eff (Output String : es) a
runInteractTalker talker = reinterpret \case
  PromptText s -> output s >> pure (talker.respondText s)
  PromptYesOrNo s -> output s >> pure (talker.respondYesNo s)
  Display s -> output s
