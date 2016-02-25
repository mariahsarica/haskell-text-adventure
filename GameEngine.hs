-- GameEngine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer

import System.IO


data Player = Player {
    playerName :: String
} 

-- shows player name without quotation marks
instance Show Player where
    show (Player n) = id n

-- prompts user to enter a name
getPlayerName :: IO Player
getPlayerName = do
    hPutStr stderr "Please enter your name: "
    playerName <- getLine
    return (Player playerName)


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