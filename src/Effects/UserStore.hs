{-# LANGUAGE TemplateHaskell #-}

module Effects.UserStore where

import Cleff
import Cleff.State
import Data.Foldable
import Data.Set (Set)
import Data.Set qualified as Set

data UserRecord = UserRecord
  { name :: String
  , flies :: Bool
  , rich :: Bool
  }
  deriving (Ord, Eq, Show, Read)

data UserStore :: Effect where
  LookupUserByName :: String -> UserStore m (Maybe UserRecord)
  StoreUser :: UserRecord -> UserStore m ()

makeEffect ''UserStore

runUserStorePure :: Set UserRecord -> Eff (UserStore : es) a -> Eff es (a, Set UserRecord)
runUserStorePure initialUsers =
  runState initialUsers . reinterpret \case
    LookupUserByName n -> do
      users <- get
      -- Put a type signature on `users` so that cleff knows that I'm
      -- trying to use the same `Set UserRecord` and not some other `State`
      pure $ find (\u -> u.name == n) (users :: Set UserRecord)
    StoreUser user -> do
      users <- get
      put $ Set.insert user $ Set.filter (\u -> u.name /= user.name) users
