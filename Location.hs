module Location where

import Item


data Location = Location {
    locName :: String,
    locDesc :: String,
    contents :: Maybe Item
}

instance Show Location where
    show (Location _ desc _) = id desc

lobby = Location "Lobby" "You are in the lobby. There are a row of carts to your right." (Just cart)