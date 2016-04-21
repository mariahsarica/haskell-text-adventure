module Functions where

import Util
import Data.List


-- shows current state message
showStateMessage :: GameState -> IO ()
showStateMessage (Normal p _ m) = putStrLn m
showStateMessage (Win m) = putStrLn m
showStateMessage (Lose m) = putStrLn m

-- moves player in specified direction
move :: GameState -> Dir -> GameState
move (Normal p l m) dir = if (numberOfMoves p) == 18 
                              then Lose "\nYou ran out of time! The store is now closed."
                          else if getLoc == -1 
                              then (Normal p l "\nYou cannot go that way")
                          else (Normal p{numberOfMoves=newNumOfMoves} newLoc (describe newLoc))
                          where row = navMatrix !! (locId l)
                                getLoc = row !! (dirId dir)
                                newLoc = pantry !! getLoc
                                newNumOfMoves = (numberOfMoves p) + 1      


-- converts the player's number of moves into a time to go along with the story
getTime :: GameState -> String
getTime (Normal p l m) = case numberOfMoves p of
    0  -> "7:30"
    1  -> "7:35"
    2  -> "7:40"
    3  -> "7:45"
    4  -> "7:50"
    5  -> "7:55"
    6  -> "8:00"
    7  -> "8:05"
    8  -> "8:10"
    9  -> "8:15"
    10 -> "8:20"
    11 -> "8:25"
    12 -> "8:30"
    13 -> "8:35"
    14 -> "8:40"
    15 -> "8:45"
    16 -> "8:50"
    17 -> "8:55"
    18 -> "9:00"


-- used in showInventory to display list of items in a more readable manner
-- Found this online at http://stackoverflow.com/questions/5829985
showItemList :: Show a => [a] -> String
showItemList = intercalate ", " . map show

-- shows player's inventory
showInventory :: GameState -> GameState
showInventory (Normal p l m) = Normal p l inv
    where inv = case inventory p of []   -> "\nYou have no items."
                                    itms -> "\nYou currently have: " ++ (showItemList itms)


-- gives the current time, location, and inventory
checkStatus :: GameState -> GameState
checkStatus st@(Normal p l m) = Normal p l status
    where status = "\nTime: " ++ getTime st
                ++ "\nCurrent Location: " ++ name l
                ++ "\nInventory: " ++ (showItemList (inventory p))
                                 


-- takes specified item from location contents and adds it to end of player's inventory list
-- doesn't allow players to take any items unless they have a cart first
takeItem :: GameState -> String -> GameState
takeItem (Normal p l m ) itm = if cartTaken then 
                                  if contains p item 
                                      then (Normal p l "\nYou already have this item")
                                  else if contains l item 
                                      then (Normal (acquire p item) l ("\nYou have taken " ++ (name item) ++ ". " ++ describe item))
                                  else (Normal p l ("\nHmm, where do you see " ++ (name item) ++ " in here??"))
                               else if item == cart && l == lobby 
                                  then (Normal (acquire p{hasCart=True} cart) l ("\nYou have taken " ++ (name item) ++ ". " ++ describe item))
                               else if contains l item 
                                  then (Normal p l "\nYou need something to put your groceries in.")
                               else (Normal p l ("\nHmm, where do you see " ++ (name item) ++ " in here??"))
                               where item = read itm
                                     cartTaken = hasCart p
                                                         


-- removes specified item from players inventory
dropItem :: GameState -> String -> GameState
dropItem (Normal p l m) itm = if isEmpty p 
                                  then (Normal p l "\nYou have nothing to drop")
                              else if item == cart && contains p cart 
                                  then (Normal p{hasCart=False, inventory=[]} l ("\nYou have dropped your cart. Hopefully you didn't have any groceries in there!") )  
                              else if contains p item 
                                  then (Normal (release p item) l ("\nYou have dropped the " ++ (show item)) )
                              else (Normal p l ("\nYou don't have any " ++ (show item)) )
                              where item = read itm    



-- "looks around" location by displaying location description
lookAround :: GameState -> GameState
lookAround (Normal p l m) = (Normal p l ("\n" ++ locDesc l) )


-- displays list of commands available
help :: GameState -> GameState
help (Normal p l m) = (Normal p l h)
    where h = "\nThe following commands are permitted:\n"
               ++ "l        - look around current location\n"
               ++ "t [ITEM] - take specified item from current location\n"
               ++ "d [ITEM] - drop specified item\n"
               ++ "i        - display inventory\n"
               ++ "n        - move north\n"
               ++ "s        - move south\n"
               ++ "w        - move west\n"
               ++ "e        - move east\n"
               ++ "m        - view map\n"
               ++ "h        - display these help instructions\n"
               ++ "q        - quit game\n"
               ++ "*Note: Items listed in capital letters in each location are available to take"
            

-- displays map of                
viewMap :: GameState -> GameState
viewMap (Normal p l msg) = if contains p storeMap then (Normal p l showMap)
                           else (Normal p l "\nYou don't have a map!")
    where showMap = "\n---------------------------%%%%%%%%%%%%%---------------------------\n"++
                      "---------------------------%%%  MAP  %%%---------------------------\n"++
                      "---------------------------%%%%%%%%%%%%%---------------------------\n"++
                      "-------------------------------------------------------------------\n"++
                      "-------------------------------------------------------------------\n"++
                      "-------*************-------*************-------*************-------\n"++
                      "-------*           *-------*           *-------*           *-------\n"++
                      "-------*   DAIRY   *#######*   DELI    *-------*   BULK    *-------\n"++
                      "-------*           *-------*           *-------*           *-------\n"++
                      "-------*************-------*************-------*************-------\n"++
                      "-------------#-------------------#-------------------#-------------\n"++
                      "-------------#-------------------#-------------------#-------------\n"++
                      "-------------#-------------------#-------------------#-------------\n"++
                      "-------*************-------*************-------*************-------\n"++
                      "-------*           *-------*           *-------*           *-------\n"++
                      "-------*  AISLE 2  *#######*  AISLE 3  *#######*  PRODUCE  *-------\n"++
                      "-------*           *-------*           *-------*           *-------\n"++
                      "-------*************-------*************-------*************-------\n"++
                      "---------------------------------#-------------------#-------------\n"++
                      "---------------------------------#-------------------#-------------\n"++
                      "---------------------------------#-------------------#-------------\n"++
                      "---------------------------*************-------*************-------\n"++
                      "---------------------------*   CASH    *-------*           *-------\n"++
                      "---------------------------* REGISTERS *#######*   LOBBY   *-------\n"++
                      "---------------------------*           *-------*           *-------\n"++
                      "---------------------------*************-------*************-------\n"++
                      "-------------------------------------------------------------------\n"++
                      "-------------------------------------------------------------------"