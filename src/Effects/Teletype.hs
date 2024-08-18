{-# LANGUAGE TemplateHaskell #-}

module Effects.Teletype where

import Cleff
import Cleff.Input
import Cleff.Output
import Cleff.State
import Data.Maybe (fromMaybe)

-- Effect definition
data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeEffect ''Teletype

-- Effect Interpretation via IO
runTeletypeIO :: (IOE :> es) => Eff (Teletype : es) a -> Eff es a
runTeletypeIO = interpretIO \case
  ReadTTY -> getLine
  WriteTTY s -> putStrLn s

-- Effect interpretation via other pure effects
runTeletypePure :: [String] -> Eff (Teletype : es) w -> Eff es [String]
runTeletypePure tty =
  fmap (reverse . snd)
    . runState []
    . outputToListState
    . runState tty
    . inputToListState
    . reinterpret2 \case
      ReadTTY -> fromMaybe "" <$> input
      WriteTTY msg -> output msg
