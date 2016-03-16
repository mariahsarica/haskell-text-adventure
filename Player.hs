module Player where

import Item
import Desc


data Player = Player {
    playerName :: String,
    gender :: Char,
    bag :: Char,
   -- currLoc :: Int,
    inventory :: [Item]
} 


-- shows player name without quotation marks
instance Show Player where
    show (Player _ _ _ inv) = show inv
    

instance Desc Player where
    name (Player n _ _ _) = id n
    describe (Player _ _ _ inv) = show inv
