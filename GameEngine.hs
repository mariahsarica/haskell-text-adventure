-- GameEngine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer

import System.IO
import GameState
import Player
import Command
import Location
import Dir
import Desc

-- prompts user to enter a command
getCommand :: IO Command
getCommand = do
   hPutStr stderr "\n\nEnter a command: "
   fmap read getLine


-- updates game state based on command user entered
updateState :: GameState -> Command -> GameState
updateState Terminated _ = Terminated
updateState st@(Normal p l m) cmd = if cmd == Quit then Terminated
                               else if cmd == Take then takeItem st
                               else if cmd == Drop then dropItem st
                               else if cmd == ShowInv then showInventory st
                               else if cmd == Help then help st
                               else if cmd == Look then lookAround st
                               else if cmd == Move North then move st North
                               else if cmd == Move South then move st South
                               else if cmd == Move West then move st West
                               else if cmd == Move East then move st East
                               else (Normal p l ("\nError: " ++ (show cmd)))


gameLoop :: GameState -> IO ()
gameLoop Terminated = return ()
gameLoop st = do
    showStateMessage st
    cmd <- getCommand
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
    hPutStr stderr "Please enter your name to begin the game: "
    playerName <- getLine
    return (Normal (Player playerName Nothing) lobby (descrip lobby))
               
               
-- introductory message signifying the game has begun
header :: IO ()
header = putStrLn $ "\n    NATURE'S PANTRY Text Adventure Game    "
                 ++ "\n===========================================\n"


-- personal welcome message using the player's name
welcomeMsg :: GameState -> IO ()
welcomeMsg st@(Normal p _ _) = putStrLn $ "\nWelcome to NATURE’S PANTRY, " ++ (show p) ++ ", your favorite alternative grocery store!"
                                       ++ "\n(Enter 'h' for help, or 'q' to quit)"


-- displays exit message upon quitting game
exitMsg :: IO ()
exitMsg = putStrLn $ "\n==============================="
                  ++ "\nCopyright 2016. Mariah Molenaer\n"
       
                   
