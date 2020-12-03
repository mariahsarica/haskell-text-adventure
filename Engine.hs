-- Engine.hs - NATURE'S PANTRY Game Engine
-- Author:  Mariah Molenaer


import System.IO
import GameData
import Functions



-- prompts user to enter a command
getCommand :: IO Command
getCommand = do
   hPutStr stderr "\n\nEnter a command: "
   fmap read getLine


-- updates game state based on command user entered
updateState :: GameState -> Command -> GameState
updateState Terminated _ = Terminated
updateState (Win msg) _ = Win msg
updateState (Lose msg) _ = Lose msg
updateState st@(Normal plyr loc msg) cmd = case cmd of
    Quit            -> Terminated
    Take itm        -> takeItem st itm 
    Drop itm        -> dropItem st itm
    ShowInv         -> showInventory st
    CheckStatus     -> checkStatus st
    Help            -> help st
    Look            -> look st
    Examine         -> examine st
    Move dir        -> move st dir
    ViewMap         -> viewMap st
    SpecialItem itm -> (Normal plyr loc (describe itm)) 
    LaunchEndGame   -> endOfGame st
    Invalid c       -> (Normal plyr loc ("\nError: " ++ c ++ " is not a valid command."))
    _               -> (Normal plyr loc ("\nError: Invaild command."))
updateState st@(EndGame plyr hlth bsHlth msg) cmd = case cmd of
    Quit      -> Terminated
    Help      -> help st
    Use itm   -> use st itm
    ShowInv   -> showInventory st
    Invalid c -> (EndGame plyr hlth bsHlth ("\nError: " ++ c ++ " is not a vaild command."))
    _         -> (EndGame plyr hlth bsHlth ("\nError: This command is not valid in end game mode! Your only task is to defeat the CABBAGE CRUSHER!"))


gameLoop :: GameState -> IO ()
gameLoop Terminated  = return ()
gameLoop st@(Win msg)  = showStateMessage st
gameLoop st@(Lose msg) = showStateMessage st
gameLoop st@(EndGame plyr hlth bsHlth msg) = do
    showStateMessage st
    showHealth st
    cmd <- getCommand
    result <- return $ (updateState st cmd)
    newSt <- return $ checkHealth result
    gameLoop newSt  
gameLoop st@(Normal plyr loc msg) = do
    showStateMessage st
    cmd <- getCommand
    result <- return $ (updateState st cmd)
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
       "M" -> return 'm'
       "f" -> return 'f'
       "F" -> return 'f'
       _   -> getGender

-- prompts user to answer question, if they enter anything other than 'y' or 'n', it reprompts them
getBag :: IO Char
getBag = do
    hPutStr stderr "Did you remember to bring your reusable bags? (y or n): "
    bag <- getLine
    case bag of
       "y" -> return 'y'
       "Y" -> return 'y'
       "n" -> return 'n'
       "N" -> return 'n'
       _   -> getBag
                  

-- shows health stats in end game mode
showHealth :: GameState -> IO ()
showHealth (EndGame plyr hlth bsHlth msg) = putStrLn $ "\nHealth: " ++ (show hlth) ++ "\nEnemy Health: " ++ (show bsHlth)

               
-- introductory message signifying the game has begun
header :: IO ()
header = putStrLn $ "\n    NATURE'S PANTRY Text Adventure Game    "
                 ++ "\n===========================================\n"


-- personal welcome message using the player's name
welcomeMsg :: GameState -> IO ()
welcomeMsg (Normal plyr _ _) = putStrLn $ "\nWelcome to NATURE’S PANTRY, " ++ (name plyr) ++ ", your favorite alternative grocery store!\n"
                                       ++ "What was it that I needed to get again? Oh yeah! Gluten free flour and tofu.\n" 
                                       ++ "Sounds like an easy enough plan...\n\n"
                                       ++ "The time is 7:30, make sure you get all of your groceries before the store closes at 9:00!\n"
                                       ++ "(Enter 'h' for help, or 'q' to quit)"


-- displays exit message upon quitting game
exitMsg :: IO ()
exitMsg = putStrLn $ "\n\nThank you for visiting NATURE'S PANTRY!"
                  ++ "\n============================================"
                  ++ "\n      Copyright 2016. Mariah Molenaer.\n"
       