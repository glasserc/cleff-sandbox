import Cleff
import Effects.Interact
import Effects.Teletype

chat :: (Interact :> es) => Eff es ()
chat = do
  display "What's your name?"
  name <- inputText
  display $ "Nice to meet you, " ++ name ++ "!"
  display "Do you like airplanes?"
  plane <- yesOrNo
  if not plane
    then display "Me either! Personally, I don't mind takeoffs -- it's the landings that scare me!"
    else do
      display "Do you fly first class?"
      rich <- yesOrNo
      if rich
        then display "Wow! Those tech salaries are really something!"
        else display "Well, maybe one day after the IPO!"
  display $ "It was nice talking to you, " ++ name ++ ". Hope you enjoy your trip!"

main :: IO ()
main =
  runIOE
    . runTeletypeIO
    . runInteractTeletype
    $ chat
