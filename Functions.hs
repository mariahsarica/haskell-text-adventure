module Functions where

import GameData
import Data.List


-- shows current state message
showStateMessage :: GameState -> IO ()
showStateMessage (Normal _ _ msg) = putStrLn msg
showStateMessage (EndGame _ msg) = putStrLn msg
showStateMessage (Win msg) = putStrLn msg
showStateMessage (Lose msg) = putStrLn msg


-- determines whether the player wins or loses and generates the appropriate gamestate
endOfGame :: GameState -> GameState
endOfGame (Normal plyr loc msg) = if loc == deli 
                                      then (EndGame plyr "\nA shadowy figure emerges from the back... AHHHH IT'S THE CABBAGE CRUSHER!! \n\n")
                                      {-if (length (inventory p) > 5)
                                          then Win ("\nA shadowy figure emerges from the back... AHHHH IT'S THE CABBAGE CRUSHER!! \n\n"
                                                 ++ "You launch your cart full of groceries at him, causing him wither away to nothing!\n"
                                                 ++ "Congratulations! You saved NATURE'S PANTRY from utter destruction!")
                                      else Lose ("\nA shadowy figure emerges from the back... AHHHH IT'S THE CABBAGE CRUSHER!! \n\n"
                                              ++ "You didn't collect enough items to defeat him!\n"
                                              ++ "You lose.")-}
                                  else (Normal plyr loc "\nError: Invalid command.") 


use :: GameState -> String -> GameState
use (EndGame plyr msg) itm = if isEmpty plyr 
                                 then (EndGame plyr "\nYou have no items to use to defeat him with!" )
                             else if contains plyr item
                                 then (EndGame plyr (describe item))
                             else EndGame plyr msg
                             where item = read itm



-- moves player in specified direction
move :: GameState -> Dir -> GameState
move (Normal plyr loc msg) dir = if (numberOfMoves plyr) == 18 
                                     then Lose "\nYou ran out of time! The store is now closed."
                                 else if getLoc == -1 
                                     then (Normal plyr loc "\nYou cannot go that way")
                                 else (Normal plyr{numberOfMoves=newNumOfMoves} newLoc (describe newLoc))
                                 where row = navMatrix !! (locId loc)
                                       getLoc = row !! (dirId dir)
                                       newLoc = pantry !! getLoc
                                       newNumOfMoves = (numberOfMoves plyr) + 1      


-- converts the player's number of moves into a time to go along with the story
getTime :: GameState -> String
getTime (Normal plyr loc msg) = case numberOfMoves plyr of
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
showInventory (Normal plyr loc msg) = Normal plyr loc inv
    where inv = case inventory plyr of []   -> "\nYou have no items."
                                       itms -> "\nYou currently have: " ++ (showItemList itms)


-- gives the current time, location, and inventory
checkStatus :: GameState -> GameState
checkStatus st@(Normal plyr loc msg) = Normal plyr loc status
    where status = "\nTime: " ++ getTime st
                ++ "\nCurrent Location: " ++ name loc
                ++ "\nInventory: " ++ (showItemList (inventory plyr))
                                 


-- takes specified item from location contents and adds it to end of player's inventory list
-- doesn't allow players to take any items unless they have a cart first
takeItem :: GameState -> String -> GameState
takeItem (Normal plyr loc msg ) itm = if cartTaken then 
                                          if contains plyr item 
                                              then (Normal plyr loc "\nYou already have this item")
                                          else if contains loc item 
                                              then (Normal (acquire plyr item) loc ("\nYou have taken " ++ (name item) ++ ". " ++ describe item))
                                          else (Normal plyr loc ("\nHmm, where do you see " ++ (name item) ++ " in here??"))
                                      else if item == cart && loc == lobby 
                                          then (Normal (acquire plyr{hasCart=True} cart) loc ("\nYou have taken " ++ (name item) ++ ". " ++ describe item))
                                      else if contains loc item 
                                          then (Normal plyr loc "\nYou need something to put your groceries in.")
                                      else (Normal plyr loc ("\nHmm, where do you see " ++ (name item) ++ " in here??"))
                                      where item = read itm
                                            cartTaken = hasCart plyr
                                                         


-- removes specified item from players inventory
dropItem :: GameState -> String -> GameState
dropItem (Normal plyr loc msg) itm = if isEmpty plyr 
                                         then (Normal plyr loc "\nYou have nothing to drop")
                                     else if item == cart && contains plyr cart 
                                         then (Normal plyr{hasCart=False, inventory=[]} loc ("\nYou have dropped your cart. Hopefully you didn't have any groceries in there!") )  
                                     else if contains plyr item 
                                         then (Normal (release plyr item) loc ("\nYou have dropped the " ++ (show item)) )
                                     else (Normal plyr loc ("\nYou don't have any " ++ (show item)) )
                                     where item = read itm    



-- displays the current location in a statement
look :: GameState -> GameState
look (Normal plyr loc msg) = (Normal plyr loc ("\n" ++ locStmt loc) )

-- examines location to provide description with items
examine :: GameState -> GameState
examine (Normal plyr loc msg) = (Normal plyr loc ("\n" ++ locDesc loc) )


-- displays list of commands available
help :: GameState -> GameState
help (Normal plyr loc msg) = (Normal plyr loc helpMsg)
    where helpMsg = "\nThe following commands are permitted:\n"
                 ++ "l        - look around to see where you are\n"
                 ++ "x        - examine current location\n"
                 ++ "t [ITEM] - take specified item from current location\n"
                 ++ "d [ITEM] - drop specified item\n"
                 ++ "i        - display inventory\n"
                 ++ "c        - check status\n"
                 ++ "n        - move north\n"
                 ++ "s        - move south\n"
                 ++ "w        - move west\n"
                 ++ "e        - move east\n"
                 ++ "m        - view map\n"
                 ++ "h        - display these help instructions\n"
                 ++ "q        - quit game\n"
                 ++ "*Note: Items listed in capital letters in each location are available to take"
help (EndGame plyr msg) = (EndGame plyr helpMsg)
    where helpMsg = "\nThe following commands are permitted:\n"
                 ++ "u [ITEM] - use specified item to attack\n"
                 ++ "h        - display these help instructions\n"
                 ++ "q        - quit game"

            

-- displays map of world                
viewMap :: GameState -> GameState
viewMap (Normal plyr loc msg) = if contains plyr storeMap then (Normal plyr loc showMap)
                                else (Normal plyr loc "\nYou don't have a map!")
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