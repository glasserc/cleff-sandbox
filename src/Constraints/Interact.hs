module Constraints.Interact where

import Cleff
import Effects.Interact qualified as Interact

class Monad m => HasInteract m where
  promptText :: String -> m String
  promptYesOrNo :: String -> m Bool
  display :: String -> m ()

instance (Interact.Interact :> es) => HasInteract (Eff es) where
  promptText = Interact.promptText
  promptYesOrNo = Interact.promptYesOrNo
  display = Interact.display
