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
    Just user -> do
      inputUser <- refreshUserFromInteract user
      storeUser inputUser
  display $ "It was nice talking to you, " ++ name ++ ". Hope you enjoy your trip!"

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

-- | Verify that all the same information is still true about the user.
refreshUserFromInteract :: (Interact :> es) => UserRecord -> Eff es UserRecord
refreshUserFromInteract userRecord = do
  display $ "Good to see you again, " ++ userRecord.name ++ "!"
  let airplanePrompt = case userRecord.flies of
        True -> "Do you still like airplanes?"
        False -> "Do you like airplanes? (Previous answer: no)"
  plane <- promptYesOrNo airplanePrompt
  respondToRefreshedData userRecord.flies plane
  if not plane
    then -- FIXME: maybe we should refresh affluence too
      pure userRecord {flies = plane}
    else do
      let firstClassPrompt =
            case userRecord.rich of
              True -> "Do you still fly first class?"
              False -> "Are you flying first class yet?"
      rich <- promptYesOrNo firstClassPrompt
      respondToRefreshedData userRecord.rich rich
      pure userRecord {flies = plane, rich = rich}

respondToRefreshedData :: (Eq a, Interact :> es) => a -> a -> Eff es ()
respondToRefreshedData old new =
  if old == new
    then display "Thanks for confirming!"
    else display "Thanks for updating us!"
