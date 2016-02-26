-- GameEngine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer

import System.IO
import Player
import Item
import Location



main = do
    header
    n <- getPlayerName
    welcomeMsg n


-- introductory message signifying the game has begun
header :: IO ()
header = putStrLn $ "\n    NATURE'S PANTRY Text Adventure Game    "
                 ++ "\n===========================================\n"


-- personal welcome message using the player's name
welcomeMsg :: Player -> IO ()
welcomeMsg n = putStrLn $ "\nWelcome to NATURE’S PANTRY, " ++ (show n) ++ ", your favorite alternative grocery store!\n"