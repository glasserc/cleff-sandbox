import Cleff
import Effects.Interact
import Effects.Teletype
import SampleProgram

main :: IO ()
main =
  runIOE
    . runTeletypeIO
    . runInteractTeletype
    $ chat
