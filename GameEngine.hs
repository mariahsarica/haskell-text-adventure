-- GameEngine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer

import System.IO
import Player
import World


data GameState = GameState {
    player :: Player,
    world :: World
}


main = do
    header
    p <- getPlayer
    welcomeMsg p


-- prompts user to enter a name and creates player
getPlayer :: IO GameState
getPlayer = do
    hPutStr stderr "Please enter your name: "
    playerName <- getLine
    return (GameState (Player playerName Nothing) pantry)


-- show player's inventory
showInventory :: GameState -> IO ()
showInventory st@(GameState p w) = putStrLn i 
    where i = case inventory p of Nothing    -> "You have no items."
                                  (Just itm) -> "You currently have: " ++ (show itm)


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