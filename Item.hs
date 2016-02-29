module Item where


data Item = Item {
    itemName :: String,
    itemDesc :: String
} deriving Eq

instance Show Item where
    show (Item n _) = id n


cart = Item "Cart" "You now have something to put your groceries in!"

