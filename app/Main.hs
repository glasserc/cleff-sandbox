import Cleff
import Effects.Interact
import Effects.Teletype
import Effects.UserStore
import SampleProgram

main :: IO ()
main =
  runIOE
    . runUserStorePureDiscardResult mempty
    . runInteractProd
    $ chat --    chat has type Eff [Interact, UserStore, IOE] ()

runInteractProd :: (IOE :> es) => Eff (Interact : es) a -> Eff es a
runInteractProd = runTeletypeIO . runInteractTeletype
