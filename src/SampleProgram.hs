module SampleProgram where

import Cleff
import Effects.Interact
import Effects.UserStore

chat :: ([Interact, UserStore] :>> es) => Eff es ()
chat = do
  name <- promptText "What's your name?"
  userM <- lookupUserByName name
  case userM of
    Nothing -> do
      inputUser <- readUserFromInteract name
      storeUser inputUser
      display $ "It was nice talking to you, " ++ name ++ ". Hope you enjoy your trip!"
    Just user ->
      pure ()

readUserFromInteract :: (Interact :> es) => String -> Eff es UserRecord
readUserFromInteract name = do
  display $ "Nice to meet you, " ++ name ++ "!"
  plane <- promptYesOrNo "Do you like airplanes?"
  if not plane
    then do
      display "Me either! Personally, I don't mind takeoffs -- it's the landings that scare me!"
      -- FIXME: find another way to identify the affluence of the user
      pure UserRecord {name = name, flies = plane, rich = False}
    else do
      rich <- promptYesOrNo "Do you fly first class?"
      if rich
        then display "Wow! Those tech salaries are really something!"
        else display "Well, maybe one day after the IPO!"
      pure UserRecord {name = name, flies = plane, rich = rich}
