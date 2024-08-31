import Cleff
import Effects.Interact
import Effects.Logging
import Effects.Teletype
import Effects.UserStore
import SampleProgram

main :: IO ()
main =
  runIOE
    . runLoggingStdout
    . runUserStorePureDiscardResult mempty
    . runInteractProd
    $ chat --    chat has type Eff [Interact, UserStore, Logging, IOE] ()

runInteractProd :: (IOE :> es, Logging :> es) => Eff (Interact : es) a -> Eff es a
runInteractProd =
  runTeletypeIO
    . logAllTeletype
    . runInteractTeletype
