module Player where

import Item
import Room
import qualified Data.List

-- Defining Player record type
data Player = Player {inventory :: [ItemName],
                      maxWeight :: Integer,
                      location :: RoomName}
              deriving (Show, Eq)

-- Function to add ItemName to a player's inventory
addItem :: ItemName -> Player -> Player
addItem x p = Player (x : inventory p ) (maxWeight p) (location p)

-- Function to remove ItemName from a player's inventory
removeItem :: ItemName -> Player -> Player
removeItem x p = Player (Data.List.delete x $ inventory p) (maxWeight p) (location p)

-- Function to change the location of a player
newLocation :: RoomName -> Player -> Player
newLocation r p = Player (inventory p) (maxWeight p) r

-- Function to check if player has items in inventory
isCarryingAnything :: Player -> Bool
isCarryingAnything p = if null (inventory p) then False else True

you :: Player
you = Player [] 150 ShieldLab

youLevel2 :: Player
youLevel2 = Player [Gauntlet, QuantumSuit] 150 Ego