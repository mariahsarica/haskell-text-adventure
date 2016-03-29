module Functions where

import Classes
import Types
import Data.List


-- shows current state message
showStateMessage :: GameState -> IO ()
showStateMessage (Normal _ _ m) = putStrLn m

-- moves player in specified direction
move :: GameState -> Dir -> GameState
move (Normal p l m) dir = if newLoc == l then (Normal p l "\nYou cannot go that way")
                              else (Normal p newLoc (describe newLoc))
                          where newLoc = connections l dir


-- used in showInventory to display list of items in a more readable manner
-- Found this online at http://stackoverflow.com/questions/5829985
showItemList :: Show a => [a] -> String
showItemList = intercalate ", " . map show

-- shows player's inventory
showInventory :: GameState -> GameState
showInventory (Normal p l m) = (Normal p l inv)
    where inv = case inventory p of []   -> "\nYou have no items."
                                    itms -> "\nYou currently have: " ++ (showItemList itms)
                                 


-- takes first item from location's list of contents and adds it to end of player's inventory list
-- doesn't allow players to take any items unless they have a cart first
takeItem :: GameState -> GameState
takeItem (Normal p l m) = if itm == cart then (Normal p{hasCart=True, inventory=newInv} l ("\nYou have taken the " ++ (name itm) ++ ". " ++ describe itm))
                          else if (hasCart p) == False then (Normal p l "\nYou need something to put your groceries in.")
                          else if itm `elem` (inventory p) then (Normal p l "\nYou already have this item")
                          else (Normal (p{inventory=newInv}) l ("\nYou have taken the " ++ (name itm) ++ ". " ++ describe itm))
                          where itm = head (contents l)
                                newInv = (inventory p) ++ [itm]
          
                             


-- drops last item listed in player's inventory
dropItem :: GameState -> GameState
dropItem (Normal p l m) = if inventory p == [] then (Normal p l "\nYou have nothing to drop")
                          else if itm == cart then (Normal p{hasCart=False, inventory=newInv} l ("\nYou have dropped the " ++ (name itm)) )
                          else (Normal (p{inventory=newInv}) l ("\nYou have dropped the " ++ (name itm)) )
                             where itm = last (inventory p)
                                   newInv = init (inventory p)



-- "looks around" location by displaying location description
lookAround :: GameState -> GameState
lookAround (Normal p l m) = (Normal p l ("\n" ++ locDesc l) )


-- displays list of commands available
help :: GameState -> GameState
help (Normal p l m) = (Normal p l h)
    where h = "\nThe following commands are permitted:\n"
               ++ "l - look around current location\n"
               ++ "t - take item from current location\n"
               ++ "d - drop item to current location\n"
               ++ "i - display inventory\n"
               ++ "n - move north\n"
               ++ "s - move south\n"
               ++ "w - move west\n"
               ++ "e - move east\n"
               ++ "h - display these help instructions\n"
               ++ "q - quit game"