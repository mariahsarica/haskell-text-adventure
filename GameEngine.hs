-- GameEngine.hs
-- Author:  Mariah Molenaer

import System.IO

data Player = Player {
    playerName :: String
} deriving Show

getPlayerName :: IO Player
getPlayerName = do
    hPutStr stderr "Please enter your name: "
    playerName <- fmap read getLine
    return (Player playerName)

main = do
    getPlayerName

