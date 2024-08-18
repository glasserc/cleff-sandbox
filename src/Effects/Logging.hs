{-# LANGUAGE TemplateHaskell #-}

module Effects.Logging where

import Cleff
import Cleff.Output

-- An effect for logging messages.
data Logging :: Effect where
  Log :: String -> Logging m ()

makeEffect ''Logging

-- Perform logging effects by printing them to standard output.
runLoggingStdout :: (IOE :> es) => Eff (Logging : es) a -> Eff es a
runLoggingStdout = interpretIO \case
  Log s -> putStrLn s

-- Convert Logging into an Output
loggingToOutput :: Eff (Logging : es) w -> Eff (Output String : es) w
loggingToOutput =
  reinterpret \case
    Log s -> output s
