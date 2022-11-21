module Example where

import System.Random
import qualified Data.List

import Item
import Direction
import Player
import Room
import GameState

class Example a where 
    example :: IO a

-- Example instance for Item
instance Example Item where
    example = do 
        iname <- choose itemNames
        weight <- randomRIO (0,100)
        return (Item iname weight)

-- Example instance for Direction
instance Example Direction where
    example = do
        choose [N,S,W,E]

-- Example instance for Room
instance Example Room where
    example = do
        rname <- choose roomNames
        exits <- exampleList exitExample (randomRIO (2,4)) 
        objects <- exampleList (choose itemNames) (randomRIO (2,5))
        return (Room rname ("You are in a randomly-generated room, which is the " ++ show rname) exits objects)

-- Example instance for Player
instance Example Player where
    example = do
        inventory <- exampleList (choose itemNames) (randomRIO (0,10))
        location <- choose roomNames
        weight <- randomRIO (40,60)
        return (Player (Data.List.nub inventory) weight location)

exitExample :: IO Exit
exitExample = do
    dir <- example :: IO Direction
    room <- choose roomNames
    return (dir, room)

-- Example instance for GameState
instance Example GameState where 
    example = do
        message <- choose [Just "One possible message.", Just "Yet another possible message", Nothing]
        player <- example :: IO Player
        items <- exampleList (example :: IO Item) (randomRIO (5,10))
        rooms <- exampleList (example :: IO Room) (randomRIO (2,3))
        return (GameState message (mkMap rooms) (mkUniverse items) player)

-- Function to randomly choose a element in a list
choose :: [a] -> IO a
choose lst = do
    index <- randomRIO (0,length lst -1)
    return (lst !! index)

-- Function to create a list of given random length with random numbers as elements
exampleList :: IO a -> IO Int -> IO [a]
exampleList x y = do
    len <- y
    sequence (replicate len x)