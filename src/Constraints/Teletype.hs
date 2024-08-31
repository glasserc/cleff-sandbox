module Constraints.Teletype where

import Cleff
import Effects.Teletype qualified as Teletype

class Monad m => HasTeletype m where
  readTTY :: m String
  writeTTY :: String -> m ()

instance (Teletype.Teletype :> es) => HasTeletype (Eff es) where
  readTTY = Teletype.readTTY
  writeTTY = Teletype.writeTTY
