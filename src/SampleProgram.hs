module SampleProgram where

import Cleff
import Effects.Interact

chat :: (Interact :> es) => Eff es ()
chat = do
  name <- promptText "What's your name?"
  display $ "Nice to meet you, " ++ name ++ "!"
  plane <- promptYesOrNo "Do you like airplanes?"
  if not plane
    then display "Me either! Personally, I don't mind takeoffs -- it's the landings that scare me!"
    else do
      rich <- promptYesOrNo "Do you fly first class?"
      if rich
        then display "Wow! Those tech salaries are really something!"
        else display "Well, maybe one day after the IPO!"
  display $ "It was nice talking to you, " ++ name ++ ". Hope you enjoy your trip!"
