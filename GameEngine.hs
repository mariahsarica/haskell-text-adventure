-- GameEngine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer

import System.IO
import Player
import Location


data GameState = GameState {
    player :: Player,
    location :: Location,
    messages :: String
}

instance Show GameState where
    show (GameState _ l _) = show l
    
showStateMessage :: GameState -> IO ()
showStateMessage st@(GameState _ _ m) = putStrLn m


-- prompts user to enter a command
getCommand :: IO Char
getCommand = do
   hPutStr stderr "\nEnter a command: "
   fmap head getLine

-- updates game state based on command user entered
updateState :: GameState -> Char -> GameState
updateState st@(GameState p l m) cmd = if cmd == 't' then takeItem st
                                  else if cmd == 'd' then dropItem st
                                  else if cmd == 'i' then showInventory st
                                  else if cmd == 'h' then help st
                                  else if cmd == 'l' then lookAround st
                                  else (GameState p l "\nInvalid Command")

-- displays game state
displayState :: GameState -> IO ()
displayState st = putStrLn $ "\n" ++ (show st)


gameLoop :: GameState -> IO ()
gameLoop st = do
    showStateMessage st
    cmd <- getCommand
    if cmd == 'q' then return ()
    else do
      result <- return $ updateState st cmd
      gameLoop result 

main = do
    header
    initSt <- getPlayer
    welcomeMsg initSt
    displayState initSt
    gameLoop initSt


-- prompts user to enter a name and creates player
getPlayer :: IO GameState
getPlayer = do
    hPutStr stderr "Please enter your name to being the game: "
    playerName <- getLine
    return (GameState (Player playerName Nothing) lobby "")


-- show player's inventory
showInventory :: GameState -> GameState
showInventory (GameState p l m) = (GameState p l inv)
    where inv = case inventory p of Nothing    -> "\nYou have no items."
                                    (Just itm) -> "\nYou currently have: " ++ (show itm)


takeItem :: GameState -> GameState
takeItem st@(GameState p l m) = if contents l == (Just itm) 
                                  then (GameState (p{inventory=(Just itm)}) (l{contents=Nothing}) ("\nYou have picked up a " ++ (show itm)))
                                  else (GameState p l "\nThere is nothing to pick up")
                              where (Just itm) = contents l

-- drops item from player's inventory to location
dropItem :: GameState -> GameState
dropItem st@(GameState p l m) = if inventory p == (Just itm)
                                  then (GameState (p{inventory=Nothing}) (l{contents=(Just itm)}) ("\nYou have dropped your " ++ (show itm)) )
                                  else (GameState p l "\nYou have nothing to drop")
                              where (Just itm) = inventory p


lookAround :: GameState -> GameState
lookAround (GameState p l m) = (GameState p l ("\n" ++ locDesc l) )


-- introductory message signifying the game has begun
header :: IO ()
header = putStrLn $ "\n    NATURE'S PANTRY Text Adventure Game    "
                 ++ "\n===========================================\n"



-- personal welcome message using the player's name
welcomeMsg :: GameState -> IO ()
welcomeMsg st@(GameState p _ _) = putStrLn $ "\nWelcome to NATURE’S PANTRY, " ++ (show p) ++ ", your favorite alternative grocery store!"
                                ++ "\n(Enter 'h' for help, or 'q' to quit)"


help :: GameState -> GameState
help (GameState p l m) = (GameState p l h)
    where h = "\nThe following commands are permitted:\n"
               ++ "l - look around current location\n"
               ++ "t - take item from current location\n"
               ++ "d - drop item to current location\n"
               ++ "i - display inventory\n"
               ++ "h - display these help instructions\n"
               ++ "q - quit game\n"
               ++ "PLEASE NOTE: Commands are case sensitive, so use only lowercase inputs."