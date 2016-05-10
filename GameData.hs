module GameData where


import Data.Char



{-
*GAMESTATE*
-}

data GameState = Normal {
                     player :: Player,
                     location :: Location,
                     messages :: String
                 }
               | Win { message :: String } 
               | Lose { message :: String }
               | Terminated

-- shows current location
instance Show GameState where
    show (Normal _ loc _) = describe loc




{-
*COMMAND*
-}

data Command = Look
             | Examine 
             | Take String 
             | Drop String 
             | ShowInv 
             | CheckStatus
             | Move Dir 
             | ViewMap 
             | SpecialItem Item 
             | EndGame
             | Help 
             | Quit 
             | Invalid String 
             deriving (Show,Eq)


instance Read Command where
    readsPrec _ s
        | null s = [(Invalid "NULL","")]
        | map toLower s == "q" = [(Quit,"")]
        | map toLower s == "l" = [(Look,"")]
        | map toLower s == "x" = [(Examine,"")]
        | head (words (map toLower s)) == "t" && (null (tail (words (map toLower s))) == False) = [(Take (head(tail(words s))),"")]
        | head (words (map toLower s)) == "d" && (null (tail (words (map toLower s))) == False) = [(Drop (head(tail(words s))),"")]    
        | map toLower s == "i" = [(ShowInv,"")]
        | map toLower s == "c" = [(CheckStatus,"")]
        | map toLower s == "n" = [(Move North,"")] 
        | map toLower s == "s" = [(Move South,"")] 
        | map toLower s == "w" = [(Move West,"")]  
        | map toLower s == "e" = [(Move East,"")] 
        | map toLower s == "m" = [(ViewMap,"")]
        | map toLower s == "h" = [(Help,"")]
        | map toLower s == "talk" = [(SpecialItem crazyGuy,"")]
        | map toLower s == "ring" = [(EndGame,"")]
        | map toLower s == "ring bell" = [(EndGame,"")]
        | otherwise = [(Invalid s,"")]


{-
*PLAYER*
-}

data Player = Player {
    playerName :: String,
    gender :: Char,
    playerBag :: Char,
    hasCart :: Bool,
    inventory :: [Item],
    numberOfMoves :: Int
}

instance Desc Player where
    name (Player name _ _ _ _ _) = id name
    describe (Player _ _ _ _ inv _) = show inv

instance Container Player where
    contents (Player _ _ _ _ inv _) = inv
    acquire (Player n g b c inv m) itm = Player n g b c (inv ++ [itm]) m
    release cont@(Player n g b c inv m) itm = if itm `elem` inv then (Player n g b c (filter (/=itm) inv) m) else cont




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
    name (Location _ name _ _ _) = name
    describe (Location _ _ stmt _ _) = "\n" ++ stmt

instance Container Location where
    contents (Location _ _ _ _ contnts) = contnts
    acquire (Location i n s d contnts) itm = Location i n s d (contnts ++ [itm])
    release cont@(Location i n s d contnts) itm = if itm `elem` contnts then (Location i n s d (filter (/=itm) contnts)) else cont


lobby     = Location 0 
            "Lobby" "You are in the lobby." 
            "There are a row of CARTs to your right and a stack of MAPs to your left." 
            [cart,storeMap]
            
registers = Location 1 
            "Cash Registers" "You are by the cash registers." 
            "There are some FLYERs in a stand by the window." 
            [flyer]
            
produce   = Location 2 
            "Produce" "You are in the produce section." 
            "Ahh, there is that really cheap organic CELERY." 
            [celery]
            
bulk      = Location 3 
            "Bulk" "You are in the bulk section."
            "Oooo so many options, I really only came in for flour and tofu.... but the QUINOA is such a great deal!"
            [quinoa]
            
aisle3    = Location 4 "Aisle 3" 
            "You are in Aisle 3." 
            "A crazy guy starts running down the aisle!\nType \"talk\" to see what he has to say." 
            []
            
deli      = Location 5 "Deli" 
            "You are in the Deli section." 
            "No one seems to be around. There is a sign that reads, 'RING BELL for service'." 
            []
            
dairy     = Location 6 "Dairy" 
            "You are in the Dairy section." 
            "There is the TOFU I came in for!" 
            [tofu]
            
aisle2    = Location 7 "Aisle 2" 
            "You are in Aisle 2" 
            "Cool! Gluten free FLOUR! And for the low price of $2.31!" 
            [flour] 


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
    show (Item name _ _) = id name

instance Read Item where
    readsPrec _ s
        | null s = [(Item "" "" False,"")]
        | map toLower s == "cart" = [(cart,"")]
        | map toLower s == "map" = [(storeMap,"")]
        | map toLower s == "flyer" = [(flyer,"")]
        | map toLower s == "celery" = [(celery,"")]
        | map toLower s == "quinoa" = [(quinoa,"")]
        | map toLower s == "flour" = [(flour,"")]
        | map toLower s == "tofu" = [(tofu,"")]
        | map toLower s == "bag" = [(rb,"")]
        | otherwise = [(Item s "" False,"")]

instance Desc Item where
    -- name function makes use of the plural attribute. Ex: a cart, some celery
    name (Item name _ plural) = if plural then id ("some " ++ name) else ("a " ++ name)
    describe (Item _ desc _) = id desc


cart     = Item "Cart" 
                "You now have something to put your groceries in!" 
                False

storeMap = Item "Map" 
                "You have picked up a map of the store! Key in 'm' to view it." 
                False
                
flyer    = Item "Flyer" 
                ("You skim the flyer... \nWeekly Specials: 'gross' ... 'eww' ... "
                ++ "Oooo! QUINOA on sale in bulk for $1.99/lb!! and discount organic "
                ++ "CELERY for 75¢!!! Don't miss out on these KILLER deals!!!") 
                False

celery   = Item "Celery" 
                "I can't believe this celery is only 75¢!!" 
                True

quinoa   = Item "Quinoa"
                "Sweet! I can't wait to make some quinoa salad later!!"
                True

flour    = Item "Flour" 
                "*Checks flour off list*" 
                True

tofu     = Item "Tofu"
                "Yummm can't wait to grill this up later!"
                True                 

rb       = Item "Bag" 
                "" 
                False

crazyGuy = Item "Crazy Guy" 
                "\nCrazy Guy: DON'T GO TO THE DELI" 
                False




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