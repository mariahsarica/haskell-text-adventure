module Location where

import Item
import Dir
import Desc


data Location = Location {
    locName :: String,
    locStmt :: String,
    locDesc :: String,
    contents :: Maybe Item
}


instance Desc Location where
    name (Location n _ _ _) = n
    descrip (Location _ s _ _) = "\n" ++ s


lobby = Location "Lobby" "You are in the lobby." "There are a row of carts to your right." (Just cart)
produce = Location "Produce" "You are in the produce section." "Ahh, there is that really cheap organic celery." (Just celery)
registers = Location "Cash Registers" "You are by the cash registers." "There are some flyers in a stand by the window." (Just flyer)
aisle2 = Location "Aisle 2" "You are in Aisle 2" "Cool! Gluten free flour! And for the low price of $2.31!" (Just flour) 

connections :: Location -> Dir -> Location
connections (Location "Lobby" _ _ _) North = produce
connections (Location "Lobby" _ _ _) West = registers
connections (Location "Cash Registers" _ _ _) East = lobby
connections (Location "Cash Registers" _ _ _) North = aisle2
connections (Location "Aisle 2" _ _ _) South = registers
connections (Location "Aisle 2" _ _ _) East = produce
connections (Location "Produce" _ _ _) West = aisle2
connections (Location "Produce" _ _ _) South = lobby