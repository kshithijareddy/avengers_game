module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Room 
import Command
import Item

-- GameIO Type
type GameIO a = StateT GameState IO a

-- Function to change the game state
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange = modify

-- Visible promt for the user
prompt :: GameIO ()
prompt = lift (putStr "->" >> hFlush stdout)

-- Prints the message of the current state
printMessage :: GameIO ()
printMessage = do
    gs <- get
    case message gs of
        Just msg ->  lift (putStrLn msg) >> effectChange (setMessage "")
        Nothing -> pure ()

-- Prints the description of the current room
printDescription :: GameIO ()
printDescription = do
    gs <- get
    lift (putStrLn (desc (currentRoom gs)))

-- Prints the objects in the current room
printObjects :: GameIO ()
printObjects = do
    gs <- get
    case nearbyObjects gs of
        [] -> pure ()
        lst -> lift (putStrLn "You see the following objects:" 
                    >> do putStrLn (unlines (map show lst)))

-- Prints exits in the current room
printExits :: GameIO ()
printExits = do
    gs <- get
    case exits $ currentRoom gs of
        [] -> pure ()
        lst -> lift (putStrLn "There are exits in the following directions:"
                     >> do putStrLn (unlines (map (\x -> show $ fst x) lst)))

-- Prints player's current inventory
printInventory :: GameIO ()
printInventory = do
    gs <- get
    case currentInventory gs of
        [] -> lift (putStrLn "You aren't carrying anything.")
        lst -> lift (putStrLn "You are carrying the following items:"
                     >> do putStrLn (unlines (map show lst)))

-- Performs actions on Items
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList _ [] = pure ()
actionOverList act (x:xs) = do
    gs <- get
    put (act x gs)
    printMessage
    actionOverList act xs

-- Prints message and moves the player to level 2
finishLevel1 :: GameIO ()
finishLevel1 = lift (putStrLn "You successfully brought the quantum suit and gauntlet into the Dyne Laboratory!" 
                     >> putStrLn "You can now travel worlds with the quantum suit so suit up for level 2!"
                     >> putStrLn "Level 2 : Collect all the 6 infinity stones to put inside the gauntlet and reach Earth to win the game!"
                     >> putStrLn "Infinity Stones : Space stone, Mind stone, Power stone, Time stone, Reality stone and Soul Stone."
                     >> putStrLn "Keep in mind that one of the worlds is completely disconnected and can only be entered by teleporting using the space stone!")
                     >> effectChange (level2State)
                     >> forever replLevel2  

-- Prints message and exits the game when user wins
finishGame :: GameIO ()
finishGame = lift (putStrLn "You collected all the infinity stones and reached Earth!" 
                   >> putStrLn "Congrats! You win. You can perform the bip now."
                   >> putStrLn "See you after 5 years!🤓"
                   >> exitSuccess)  

-- Exits the game 
exit :: GameIO ()
exit = lift (putStrLn "Goodbye!" >> exitSuccess)

-- Teleports player to and from Vormir world
teleport :: GameIO ()
teleport = do
    gs <- get
    case checkUniverse gs of
        False -> lift (putStrLn "This command is only allowed in level 2 when you have the space stone.")
        True -> case elem SpaceStone (currentInventory gs) of
                        False -> lift(putStrLn "You do not have the space stone.")
                        True -> case rname (currentRoom gs) of
                                Vormir -> effectChange(changeRoom Asgard) 
                                          >> lift(putStrLn "You have been teleported to Asgard!") 
                                _ -> effectChange(changeRoom Vormir) 
                                     >> lift(putStrLn "You have been teleported to Vormir!") 


thanos :: GameIO ()
thanos = do
    gs <- get
    case checkUniverse gs of
        False -> lift (putStrLn "This command can only be used in level 2.")
        True -> effectChange(winState)
                >> lift (putStrLn "You received all the infinity stones and are teleported to Earth as requested by Thanos 😈")


-- Checks if current game state is the winning state
checkGameOver :: GameIO ()
checkGameOver = do
    gs <- get
    case haveWonGame gs of
        False -> pure ()
        True -> finishGame

-- Checks if current game state is level 1 winning state
checkLevel1Over :: GameIO () 
checkLevel1Over = do
    gs <- get
    case havewonLevel1 gs of
        False -> pure ()
        True -> finishLevel1

-- Prints syntax Error message
syntaxError :: GameIO ()
syntaxError = lift (putStrLn "I don't understand that.")

-- Prints opening message
opening :: GameIO ()
opening = lift (putStrLn "Welcome to Functional Adventure!😁"
                >> putStrLn "A game inspired from the Marvel Cinematic Universe!"
                >> putStrLn "Level 1 : Collect the Gauntlet and Quantum Suit and reach the Dyne Laboratory to win this level."
                >> putStrLn "You can then use the Quantum suit to travel across worlds (if you know, you know 😉) to collect the infinity stones!")

-- Performs the command
performCommand :: Command -> GameIO ()
performCommand cmd = do
    gs <- get
    case cmd of
        Look -> (printDescription >> printObjects >> printExits)
        Move dir -> do
                    put (move dir gs)
                    printMessage
        Inventory -> printInventory
        Take lst -> actionOverList takeItem lst
        Drop lst -> actionOverList dropItem lst
        Exit -> exit
        Teleport -> teleport
        Thanos -> thanos

-- Performs commands in a Conjunction
performConjunction :: Conjunction -> GameIO ()
performConjunction [] = pure ()
performConjunction (x:xs) = performCommand x >> performConjunction xs

-- Parses the string and performs the commands in the Conjunction
parseConjunction :: String -> GameIO ()
parseConjunction str = case parseInput str of
    Nothing -> syntaxError
    Just x -> performConjunction x

-- Takes input from user and performs the command
repl :: GameIO ()
repl = prompt >> do
                 input <- lift getLine
                 parseConjunction input
                 checkLevel1Over

-- repl for level 2
replLevel2 :: GameIO ()
replLevel2 = prompt >> do
                 input <- lift getLine
                 parseConjunction input
                 checkGameOver
                 


    



