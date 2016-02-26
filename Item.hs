module Item where

import Player
import Location

data Item = Item {
    itemName :: String,
    itemDesc :: String
}

cart = Item "Cart" "You now have something to put your groceries in!"


takeItem :: Player -> Location -> (Player, Location)
takeItem p loc@(Location _ _ Nothing) = (p,loc)
takeItem p@(Player _ (Just _)) loc = (p,loc)
takeItem p loc = (p{inventory=itm},loc{contents=Nothing})
    where itm = contents loc