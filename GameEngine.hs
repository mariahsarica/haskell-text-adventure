-- GameEngine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer

import System.IO
import Player
import Location


data GameState = GameState {
    player :: Player,
    location :: Location
}

instance Show GameState where
    show (GameState _ l) = show l


-- prompts user to enter a command
getCommand :: IO Char
getCommand = do
   hPutStr stderr "\nEnter a command: "
   fmap head getLine

-- updates game state based on command user entered
updateState :: GameState -> Char -> GameState
updateState st cmd = if cmd == 't' then takeItem st
                     else if cmd == 'd' then dropItem st
                     else st

-- displays game state
displayState :: GameState -> IO ()
displayState st = do
    putStrLn $ show st
    showInventory st 


gameLoop :: GameState -> IO ()
gameLoop st = do
    displayState st
    cmd <- getCommand
    result <- return $ updateState st cmd
    gameLoop result   

main = do
    header
    p <- getPlayer
    showInventory p
    welcomeMsg p
    gameLoop p


-- prompts user to enter a name and creates player
getPlayer :: IO GameState
getPlayer = do
    hPutStr stderr "Please enter your name: "
    playerName <- getLine
    return (GameState (Player playerName Nothing) lobby)


-- show player's inventory
showInventory :: GameState -> IO ()
showInventory st@(GameState p w) = putStrLn i 
    where i = case inventory p of Nothing    -> "You have no items."
                                  (Just itm) -> "You currently have: " ++ (show itm)


-- take item from location and add to player's inventory
takeItem :: GameState -> GameState
takeItem st@(GameState p l) = if contents l == itm 
                                  then (GameState (p{inventory=(itm)}) (l{contents=Nothing}) )
                                  else st
                              where itm = contents l

-- drop item from player's inventory to location
dropItem :: GameState -> GameState
dropItem st@(GameState p l) = if inventory p == itm
                                  then (GameState (p{inventory=Nothing}) (l{contents=itm}) )
                                  else st
                              where itm = inventory p



-- introductory message signifying the game has begun
header :: IO ()
header = putStrLn $ "\n    NATURE'S PANTRY Text Adventure Game    "
                 ++ "\n===========================================\n"


-- personal welcome message using the player's name
welcomeMsg :: GameState -> IO ()
welcomeMsg st@(GameState p _) = putStrLn $ "\nWelcome to NATURE’S PANTRY, " ++ (show p) ++ ", your favorite alternative grocery store!"
                                        ++ "\nKey in 'h' for help, or 'q' to quit\n"

help :: IO ()
help = putStrLn $ "The following commands are permitted:\n"
               ++ "l - look around current location\n"
               ++ "t - take item from current location\n"
               ++ "d - drop item to current location\n"
               ++ "i - display inventory\n"
               ++ "h - display these help instructions\n"
               ++ "q - quit game\n"
               ++ "PLEASE NOTE: Commands are case sensitive, so use lowercase inputs only.\n"