module Player where

import Item


data Player = Player {
    playerName :: String,
   -- currLoc :: Int,
    inventory :: [Item]
} 


-- shows player name without quotation marks
instance Show Player where
    show (Player n _) = id n
    

--instance Desc Player where
--    name (Player playerName _ _) = playerName
--    descrip =
