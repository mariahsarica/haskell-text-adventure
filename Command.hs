module Command where

import Dir
import Data.Char


data Command = Look | Take | Drop | ShowInv | Move Dir | Help | Quit | Invalid Char deriving (Show,Eq)


instance Read Command where
    readsPrec _ s
        | null s = [(Invalid (head s),"")]
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