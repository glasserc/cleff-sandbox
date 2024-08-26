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
runTeletypePure :: [String] -> Eff (Teletype : es) a -> Eff es (a, [String])
runTeletypePure tty =
  dischargeOutput
    . dischargeInput
    . convertTeletypeToInputAndOutput
  where
    -- Convert the `Teletype` effect into an `Input` effect, which we
    -- will feed subsequent elements of `tty` until we run out (and
    -- then `Nothing` thereafter), and an `Output` effect, which we
    -- will use to dump all the `WriteTTY` messages.
    convertTeletypeToInputAndOutput :: Eff (Teletype : es) a -> Eff (Input (Maybe String) : Output String : es) a
    convertTeletypeToInputAndOutput =
      reinterpret2 \case
        ReadTTY -> fromMaybe "" <$> input
        WriteTTY msg -> output msg
    -- Use `tty` to satisfy `Input String`.
    dischargeInput :: Eff (Input (Maybe String) : es) a -> Eff es a
    dischargeInput =
      fmap fst -- discard inputs; it's whatever was left from `tty`
        . runState tty
        . inputToListState
    -- Collect the `Output String` into a list, with the first element
    -- of the list being the first string that was `output`.
    dischargeOutput :: Eff (Output String : es) a -> Eff es (a, [String])
    dischargeOutput =
      -- Reverse outputs -- see `outputToListState`.
      -- This isn't ideal but it's only intended for testing, so it's
      -- probably fine.
      fmap (\(a, outputs) -> (a, reverse outputs))
        . runState []
        . outputToListState
