module Types where

import Classes
import Data.Char



{-
*GAMESTATE*
-}

data GameState = Normal {
                     player :: Player,
                     location :: Location,
                     messages :: String
                 }
               | Terminated

-- shows current location
instance Show GameState where
    show (Normal _ l _) = describe l


{-
*COMMAND*
-}

data Command = Look | Take | Drop | ShowInv | Move Dir | Help | Quit | Invalid Char deriving (Show,Eq)


instance Read Command where
    readsPrec _ s
        | null s = [(Invalid ' ',"")]
        | map toLower s == "q" = [(Quit,"")]
        | map toLower s == "l" = [(Look,"")]
        | map toLower s == "t" = [(Take,"")]    
        | map toLower s == "d" = [(Drop,"")]
        | map toLower s == "i" = [(ShowInv,"")]
        | map toLower s == "n" = [(Move North,"")] 
        | map toLower s == "s" = [(Move South,"")] 
        | map toLower s == "w" = [(Move West,"")]  
        | map toLower s == "e" = [(Move East,"")] 
        | map toLower s == "h" = [(Help,"")]
        | otherwise = [(Invalid (head s),"")]


{-
*DIR*
-}

data Dir = North | South | West | East deriving (Show,Eq)


{-
*PLAYER*
-}

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



{-
*LOCATION*
-}

data Location = Location {
    locName :: String,
    locStmt :: String,
    locDesc :: String,
    contents :: [Item]
} deriving Eq


instance Desc Location where
    name (Location n _ _ _) = n
    describe (Location _ s _ _) = "\n" ++ s


lobby = Location "Lobby" "You are in the lobby." "There are a row of carts to your right." [cart]
produce = Location "Produce" "You are in the produce section." "Ahh, there is that really cheap organic celery." [celery]
registers = Location "Cash Registers" "You are by the cash registers." "There are some flyers in a stand by the window." [flyer]
aisle2 = Location "Aisle 2" "You are in Aisle 2" "Cool! Gluten free flour! And for the low price of $2.31!" [flour] 

connections :: Location -> Dir -> Location
connections (Location "Lobby" _ _ _) North = produce
connections (Location "Lobby" _ _ _) West = registers
connections (Location "Cash Registers" _ _ _) East = lobby
connections (Location "Cash Registers" _ _ _) North = aisle2
connections (Location "Aisle 2" _ _ _) South = registers
connections (Location "Aisle 2" _ _ _) East = produce
connections (Location "Produce" _ _ _) West = aisle2
connections (Location "Produce" _ _ _) South = lobby
--the following connections are not available, so location input and output are the same
connections (Location "Lobby" _ _ _) South = lobby
connections (Location "Lobby" _ _ _) East = lobby
connections (Location "Cash Registers" _ _ _) West = registers
connections (Location "Cash Registers" _ _ _) South = registers
connections (Location "Aisle 2" _ _ _) North = aisle2
connections (Location "Aisle 2" _ _ _) West = aisle2
connections (Location "Produce" _ _ _) East = produce
connections (Location "Produce" _ _ _) North = produce


{-
*ITEM*
-}

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