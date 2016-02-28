-- GameEngine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer

import System.IO
import Player
import Item
import Location



main = do
    header
    p <- getPlayerName
    welcomeMsg p


-- introductory message signifying the game has begun
header :: IO ()
header = putStrLn $ "\n    NATURE'S PANTRY Text Adventure Game    "
                 ++ "\n===========================================\n"


-- personal welcome message using the player's name
welcomeMsg :: Player -> IO ()
welcomeMsg p = putStrLn $ "\nWelcome to NATURE’S PANTRY, " ++ (show p) ++ ", your favorite alternative grocery store!"
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