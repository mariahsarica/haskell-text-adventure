module Player where

import Item


data Player = Player {
    playerName :: String,
    inventory :: Maybe Item
} 


-- shows player name without quotation marks
instance Show Player where
    show (Player n _) = id n