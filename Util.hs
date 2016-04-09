module Util where


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

data Command = Look | Take String | Drop String | ShowInv | Move Dir | Help | Quit | Invalid String deriving (Show,Eq)


instance Read Command where
    readsPrec _ s
        | null s = [(Invalid "NULL","")]
        | map toLower s == "q" = [(Quit,"")]
        | map toLower s == "l" = [(Look,"")]
        | head (words (map toLower s)) == "t" && (null (tail (words (map toLower s))) == False) = [(Take (head(tail(words s))),"")]
        | head (words (map toLower s)) == "d" && (null (tail (words (map toLower s))) == False) = [(Drop (head(tail(words s))),"")]    
        | map toLower s == "i" = [(ShowInv,"")]
        | map toLower s == "n" = [(Move North,"")] 
        | map toLower s == "s" = [(Move South,"")] 
        | map toLower s == "w" = [(Move West,"")]  
        | map toLower s == "e" = [(Move East,"")] 
        | map toLower s == "h" = [(Help,"")]
        | otherwise = [(Invalid s,"")]




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
    playerBag :: Char,
   -- currLoc :: Int,
    hasCart :: Bool,
    inventory :: [Item]
}

instance Desc Player where
    name (Player n _ _ _ _) = id n
    describe (Player _ _ _ _ inv) = show inv

instance Container Player where
    contents (Player _ _ _ _ inv) = inv
    acquire (Player n g b c inv) itm = Player n g b c (inv ++ [itm])
    release cont@(Player n g b c inv) itm = if itm `elem` inv then (Player n g b c (filter (/=itm) inv)) else cont




{-
*LOCATION*
-}

data Location = Location {
    locName :: String,
    locStmt :: String,
    locDesc :: String,
    locContents :: [Item]
} deriving Eq


instance Desc Location where
    name (Location n _ _ _) = n
    describe (Location _ s _ _) = "\n" ++ s

instance Container Location where
    contents (Location _ _ _ contnts) = contnts
    acquire (Location n s d contnts) itm = Location n s d (contnts ++ [itm])
    release cont@(Location n s d contnts) itm = if itm `elem` contnts then (Location n s d (filter (/=itm) contnts)) else cont


lobby = Location "Lobby" "You are in the lobby." "There are a row of CARTs to your right." [cart]
produce = Location "Produce" "You are in the produce section." "Ahh, there is that really cheap organic CELERY." [celery]
registers = Location "Cash Registers" "You are by the cash registers." "There are some FLYERs in a stand by the window." [flyer]
aisle2 = Location "Aisle 2" "You are in Aisle 2" "Cool! Gluten free FLOUR! And for the low price of $2.31!" [flour] 

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
    itemDesc :: String,
    plural :: Bool       --this is for cosmetic reasons while reading the output text (further described in the Desc instance)
} deriving Eq

instance Show Item where
    show (Item n _ _) = id n

instance Read Item where
    readsPrec _ s
        | null s = [(Item "" "" False,"")]
        | map toLower s == "cart" = [(cart,"")]
        | map toLower s == "celery" = [(celery,"")]
        | map toLower s == "flyer" = [(flyer,"")]
        | map toLower s == "flour" = [(flour,"")]
        | map toLower s == "bag" = [(rb,"")]
        | otherwise = [(Item s "" False,"")]

instance Desc Item where
    -- name function makes use of the plural attribute. Ex: a cart, some celery
    name (Item n _ p) = if p then id ("some " ++ n) else ("a " ++ n)
    describe (Item _ d _) = id d


cart = Item "Cart" "You now have something to put your groceries in!" False
celery = Item "Celery" "I can't believe this celery is only 75¢!!" True
flyer = Item "Flyer" ("You skim the flyer... \nWeekly Specials: 'gross' ... 'eww' ... "
                     ++ "Oooo! QUINOA on sale in bulk for $1.99/lb!! and discount organic "
                     ++ "CELERY for 75¢!!! Don't miss out on these KILLER deals!!!") False
flour = Item "Flour" "*Checks flour off list*" True
rb = Item "Bag" "" False




{-
*CLASSES*
-}

class Desc a where
    name :: a -> String
    describe :: a -> String
    

class Container c where
    contents :: c -> [Item]
    acquire :: c -> Item -> c
    release :: c -> Item -> c
    isEmpty :: c -> Bool
    isEmpty = null . contents
    contains :: c -> Item -> Bool
    contains c itm = if itm `elem` contents c then True else False