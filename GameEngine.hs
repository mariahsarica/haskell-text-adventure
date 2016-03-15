-- GameEngine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer

import System.IO
import Player
import Location
import Item


data GameState = GameState {
    player :: Player,
    location :: Location,
    messages :: String
}

-- shows current location
instance Show GameState where
    show (GameState _ l _) = show l
    
    
-- shows current state message
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
    gameLoop initSt
    exitMsg


-- prompts user to enter a name and creates player
getPlayer :: IO GameState
getPlayer = do
    hPutStr stderr "Please enter your name to being the game: "
    playerName <- getLine
    return (GameState (Player playerName Nothing) lobby "\nYou are in the lobby")


-- shows player's inventory
showInventory :: GameState -> GameState
showInventory (GameState p l m) = (GameState p l inv)
    where inv = case inventory p of Nothing    -> "\nYou have no items."
                                    (Just itm) -> "\nYou currently have: " ++ (show itm)

-- takes item from location and adds it to player's inventory
takeItem :: GameState -> GameState
takeItem st@(GameState p l m) = if contents l == (Just itm) 
                                  then (GameState (p{inventory=(Just itm)}) (l{contents=Nothing}) ("\nYou have picked up a " ++ (show itm) ++ ". " ++ itemDesc itm))
                                  else (GameState p l "\nYou already have this item")
                              where (Just itm) = contents l

-- drops item from player's inventory to location
dropItem :: GameState -> GameState
dropItem st@(GameState p l m) = if inventory p == (Just itm)
                                  then (GameState (p{inventory=Nothing}) (l{contents=(Just itm)}) ("\nYou have dropped your " ++ (show itm)) )
                                  else (GameState p l "\nYou have nothing to drop")
                              where (Just itm) = inventory p

-- "looks around" location by displaying location description
lookAround :: GameState -> GameState
lookAround (GameState p l m) = (GameState p l ("\n" ++ locDesc l) )


-- displays list of commands available
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
               
               
-- introductory message signifying the game has begun
header :: IO ()
header = putStrLn $ "\n    NATURE'S PANTRY Text Adventure Game    "
                 ++ "\n===========================================\n"


-- personal welcome message using the player's name
welcomeMsg :: GameState -> IO ()
welcomeMsg st@(GameState p _ _) = putStrLn $ "\nWelcome to NATURE’S PANTRY, " ++ (show p) ++ ", your favorite alternative grocery store!"
                                ++ "\n(Enter 'h' for help, or 'q' to quit)"


-- displays exit message upon quitting game
exitMsg :: IO ()
exitMsg = putStrLn $ "\n==============================="
                  ++ "\nCopyright 2016. Mariah Molenaer\n"
       
                   
