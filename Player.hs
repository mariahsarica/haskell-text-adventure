module Player where

import Item


data Player = Player {
    playerName :: String,
    inventory :: Maybe Item
} 

-- shows player name without quotation marks
instance Show Player where
    show (Player n i) = id n
    

-- prompts user to enter a name
getPlayerName :: IO Player
getPlayerName = do
    hPutStr stderr "Please enter your name: "
    playerName <- getLine
    return (Player playerName Nothing)