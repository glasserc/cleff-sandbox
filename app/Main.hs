import Cleff
import Effects.Interact
import Effects.Teletype
import Effects.UserStore
import SampleProgram

main :: IO ()
main =
  runIOE
    . runUserStorePureDiscardResult mempty
    . runTeletypeIO
    . runInteractTeletype
    $ chat
