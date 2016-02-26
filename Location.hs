module Location where

import Item


data Location = Location {
    locName :: String,
    locDesc :: String,
    contents :: Maybe Item
}

lobby = Location "Lobby" "You are in the lobby" (Just cart)