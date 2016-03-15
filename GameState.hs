module GameState where

import Player
import World
import Location
import Item
import Dir
import Desc

data GameState = Normal {
                     player :: Player,
                     location :: Location,
                     messages :: String
                 }
               | Terminated

-- shows current location
instance Show GameState where
    show (Normal _ l _) = descrip l
    
    
-- shows current state message
showStateMessage :: GameState -> IO ()
showStateMessage (Normal _ _ m) = putStrLn m


-- moves player in specified direction *crashes if player tries moving a direction without a connection*
move :: GameState -> Dir -> GameState
move (Normal p l m) dir = if newLoc == l then (Normal p l "\nYou cannot go that way")
                              else (Normal p newLoc (descrip newLoc))
                          where newLoc = connections l dir


-- shows player's inventory
showInventory :: GameState -> GameState
showInventory (Normal p l m) = (Normal p l inv)
    where inv = case inventory p of Nothing    -> "\nYou have no items."
                                    (Just itm) -> "\nYou currently have: " ++ (show itm)


-- takes item from location and adds it to player's inventory
takeItem :: GameState -> GameState
takeItem (Normal p l m) = if contents l == (Just itm) 
                             then (Normal (p{inventory=(Just itm)}) (l{contents=Nothing}) ("\nYou have picked up a " ++ (show itm) ++ ". " ++ itemDesc itm))
                             else (Normal p l "\nYou already have this item")
                          where (Just itm) = contents l


-- drops item from player's inventory to location
dropItem :: GameState -> GameState
dropItem (Normal p l m) = if inventory p == (Just itm)
                             then (Normal (p{inventory=Nothing}) (l{contents=(Just itm)}) ("\nYou have dropped your " ++ (show itm)) )
                             else (Normal p l "\nYou have nothing to drop")
                          where (Just itm) = inventory p


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