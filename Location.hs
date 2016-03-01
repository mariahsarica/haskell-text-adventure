module Location where

import Item


data Location = Location {
    locName :: String,
    locDesc :: String,
    contents :: Maybe Item
}

instance Show Location where
    show (Location n d _) = "You are in the " ++ (id n) 


lobby = Location "Lobby" "There are a row of carts to your right." (Just cart)