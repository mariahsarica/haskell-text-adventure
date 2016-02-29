module Player where

import Item
import Location


data Player = Player {
    playerName :: String,
    inventory :: Maybe Item
} 


-- shows player name without quotation marks
instance Show Player where
    show (Player n _) = id n
    
    
--shows player's inventory    
--showInventory :: Player -> String
--showInventory (Player _ Nothing)  = "You have no items."
--showInventory (Player _ (Just i)) = "You currently have: " ++ (show i) 

    

-- takes item from location and adds it to player's inventory
takeItem :: Player -> Location -> (Player, Location)
takeItem p loc@(Location _ _ Nothing) = (p,loc)
takeItem p@(Player _ (Just _)) loc = (p,loc)
takeItem p loc = (p{inventory=itm},loc{contents=Nothing})
    where itm = contents loc


-- drops item from player's inventory to location
dropItem :: Player -> Location -> (Player, Location)
dropItem p loc@(Location _ _ (Just _)) = (p,loc)
dropItem p@(Player _ Nothing) loc = (p,loc)
dropItem p loc = (p{inventory=Nothing},loc{contents=itm})
    where itm = inventory p
