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

data Command = Look | Take String | Drop String | ShowInv | Move Dir | ViewMap | Help | Quit | Invalid String deriving (Show,Eq)


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
        | map toLower s == "m" = [(ViewMap,"")]
        | map toLower s == "h" = [(Help,"")]
        | otherwise = [(Invalid s,"")]


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
    locId :: Int,
    locName :: String,
    locStmt :: String,
    locDesc :: String,
    locContents :: [Item]
} deriving Eq


instance Desc Location where
    name (Location _ n _ _ _) = n
    describe (Location _ _ s _ _) = "\n" ++ s

instance Container Location where
    contents (Location _ _ _ _ contnts) = contnts
    acquire (Location i n s d contnts) itm = Location i n s d (contnts ++ [itm])
    release cont@(Location i n s d contnts) itm = if itm `elem` contnts then (Location i n s d (filter (/=itm) contnts)) else cont


lobby = Location 0 "Lobby" "You are in the lobby." "There are a row of CARTs to your right and a stack of MAPs to your left." [cart,storeMap]
registers = Location 1 "Cash Registers" "You are by the cash registers." "There are some FLYERs in a stand by the window." [flyer]
produce = Location 2 "Produce" "You are in the produce section." "Ahh, there is that really cheap organic CELERY." [celery]
bulk = Location 3 "Bulk" "You are in the bulk section." "bulk" []
aisle3 = Location 4 "Aisle 3" "You are in Aisle 3." "aisle3" []
deli = Location 5 "Deli" "You are in the Deli section." "deli" []
dairy = Location 6 "Dairy" "You are in the Dairy section." "dairy" []
aisle2 = Location 7 "Aisle 2" "You are in Aisle 2" "Cool! Gluten free FLOUR! And for the low price of $2.31!" [flour] 


{-
*DIR*
-}

data Dir = North | South | West | East deriving (Show,Eq)

dirId :: Dir -> Int
dirId North = 0
dirId South = 1
dirId East  = 2
dirId West  = 3


{-
*WORLD
-}

data World = World [Location]

pantry = [lobby,registers,produce,bulk,aisle3,deli,dairy,aisle2]

navMatrix :: [[Int]]
navMatrix =  -- N   S   E   W 
             -- 0   1   2   3 
            [[  2, -1, -1,  1 ], -- Lobby
             [  4, -1,  0, -1 ], -- Cash Registers
             [  3,  0, -1,  4 ], -- Produce
             [ -1,  2, -1, -1 ], -- Bulk
             [  5,  1,  2,  7 ], -- Aisle 3
             [ -1,  4, -1,  6 ], -- Deli
             [ -1,  7,  5, -1 ], -- Dairy
             [  6, -1,  4, -1 ]] -- Aisle 2  



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
        | map toLower s == "map" = [(storeMap,"")]
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
storeMap = Item "Map" "You have picked up a map. Key in 'm' to view it." False




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