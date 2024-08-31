module Constraints.UserStore where

import Cleff
import Effects.UserStore qualified as UserStore

class Monad m => HasUserStore m where
  lookupUserByName :: String -> m (Maybe UserStore.UserRecord)
  storeUser :: UserStore.UserRecord -> m ()

instance (UserStore.UserStore :> es) => HasUserStore (Eff es) where
  lookupUserByName = UserStore.lookupUserByName
  storeUser = UserStore.storeUser
