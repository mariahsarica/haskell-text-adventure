module Player where

import Item
import Desc


data Player = Player {
    playerName :: String,
    gender :: Char,
    bag :: Char,
   -- currLoc :: Int,
    hasCart :: Bool,
    inventory :: [Item]
}
    

instance Desc Player where
    name (Player n _ _ _ _) = id n
    describe (Player _ _ _ _ inv) = show inv
