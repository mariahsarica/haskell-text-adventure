-- Engine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer


import System.IO
import Util
import Functions



-- prompts user to enter a command
getCommand :: IO Command
getCommand = do
   hPutStr stderr "\n\nEnter a command: "
   fmap read getLine


-- updates game state based on command user entered
updateState :: GameState -> Command -> GameState
updateState Terminated _ = Terminated
updateState (Win m) _ = Win m
updateState (Lose m) _ = Lose m
updateState st@(Normal p l m) cmd = case cmd of
    Quit            -> Terminated
    Take itm        -> takeItem st itm 
    Drop itm        -> dropItem st itm
    ShowInv         -> showInventory st
    Help            -> help st
    Look            -> lookAround st
    Move dir        -> move st dir
    ViewMap         -> viewMap st
    SpecialItem itm -> (Normal p l (describe itm)) 
    Invalid c       -> (Normal p l ("\nError: " ++ c ++ " is not a valid command."))


gameLoop :: GameState -> IO ()
gameLoop Terminated  = return ()
gameLoop st@(Win m)  = showStateMessage st
gameLoop st@(Lose m) = showStateMessage st
gameLoop st = do
    showStateMessage st
    cmd <- getCommand
    result <- return $ updateState st cmd
    gameLoop result 

main = do
    header
    initSt <- getPlayer
    welcomeMsg initSt
    gameLoop initSt
    exitMsg


-- prompts user to enter a name and creates player
getPlayer :: IO GameState
getPlayer = do
    hPutStr stderr "Please enter your name: "
    playerName <- getLine
    gender <- getGender
    bag <- getBag
    if bag == 'y' then return (Normal (Player playerName gender bag False [rb] 0) lobby (describe lobby))
    else return (Normal (Player playerName gender bag False [] 0) lobby (describe lobby))
     
-- prompts user to enter a gender, if they enter anything other than 'm' or 'f', it reprompts them
getGender :: IO Char
getGender = do
    hPutStr stderr "Please enter your gender (m or f): "
    gender <- getLine
    case gender of
       "m" -> return 'm'
       "f" -> return 'f'
       _   -> getGender

-- prompts user to answer question, if they enter anything other than 'y' or 'n', it reprompts them
getBag :: IO Char
getBag = do
    hPutStr stderr "Did you remember to bring your reusable bags? (y or n): "
    bag <- getLine
    case bag of
       "y" -> return 'y'
       "n" -> return 'n'
       _   -> getBag
                  
               
-- introductory message signifying the game has begun
header :: IO ()
header = putStrLn $ "\n    NATURE'S PANTRY Text Adventure Game    "
                 ++ "\n===========================================\n"


-- personal welcome message using the player's name
welcomeMsg :: GameState -> IO ()
welcomeMsg st@(Normal p _ _) = putStrLn $ "\nWelcome to NATURE’S PANTRY, " ++ (name p) ++ ", your favorite alternative grocery store!"
                                       ++ "\n(Enter 'h' for help, or 'q' to quit)"


-- displays exit message upon quitting game
exitMsg :: IO ()
exitMsg = putStrLn $ "\nThank you for visiting Nature's Pantry!\n"
                  ++ "\n==========================================="
                  ++ "\n      Copyright 2016. Mariah Molenaer\n"
       