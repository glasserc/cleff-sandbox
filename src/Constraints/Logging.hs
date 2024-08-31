module Constraints.Logging where

import Cleff
import Effects.Logging qualified as Logging

class Monad m => HasLogging m where
  log :: String -> m ()

instance (Logging.Logging :> es) => HasLogging (Eff es) where
  log = Logging.log
