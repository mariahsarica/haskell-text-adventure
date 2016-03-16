module Item where

import Desc

data Item = Item {
    itemName :: String,
    itemDesc :: String
} deriving Eq

instance Show Item where
    show (Item n _) = id n

instance Desc Item where
    name (Item n _) = id n
    describe (Item _ d) = id d


cart = Item "Cart" "You now have something to put your groceries in!"
celery = Item "Celery" "I can't believe this celery is only 75¢!!"
flyer = Item "Flyer" ("You skim the flyer... \nWeekly Specials: 'gross' ... 'eww' ... "
                     ++ "Oooo! QUINOA on sale in bulk for $1.99/lb!! and discount organic "
                     ++ "CELERY for 75¢!!! Don't miss out on these KILLER deals!!!")
flour = Item "Flour" "*Checks flour off list*"
rb = Item "Reusable Bag" ""