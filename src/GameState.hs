module GameState where

import Control.Exception
import qualified Data.Map as M

import Item
import Room as R
import Player as P
import Direction

type Error a = Either String a

type GameMap = M.Map RoomName Room

data GameState = GameState {message :: Maybe String,
                            gmap :: GameMap,
                            universe :: Universe,
                            player :: Player}
                deriving Show

data KeyError = KeyError
  deriving Show

instance Exception KeyError

-- A map of room names and rooms
mkMap :: [Room] -> GameMap
mkMap lst = let x =  map (\y -> (rname y, y)) lst
                 in M.fromList x

gameMap :: GameMap
gameMap = mkMap allRooms

initialState :: GameState
initialState = GameState Nothing gameMap univ you

-- Returns a item if it is present in the universe
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

-- Returns the item for the given Item name
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

-- Returns the room if present in the game map
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

-- Returns the room for the given room name
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- Updating a room based on room name in a GameMap
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap roomName room gMap = M.insert roomName room gMap

-- Updating message of a GameState
setMessage :: String -> GameState -> GameState
setMessage "" gameState = gameState {message = Nothing}
setMessage msg gameState = gameState {message = Just msg}

-- Returns the inventory of a player in a GameState
currentInventory :: GameState -> [ItemName]
currentInventory gameState = inventory (player gameState)

-- Returns the current room of the player of the given GameState
currentRoom :: GameState -> Room
currentRoom gameState = getRoom (location $ player gameState) gameState

-- Returns items in the current location of the player of the given GameState
nearbyObjects :: GameState -> [ItemName]
nearbyObjects gameState = objects $ currentRoom gameState

-- Removes an item from current room and adds it to the player in the GameState
takeItem :: ItemName -> GameState -> GameState
takeItem iname gs = case (alreadyHaveTakeCheck iname gs >>= inRoomTakeCheck iname >>= weightCheck iname) of
                        Left err -> setMessage err gs
                        Right gsNew -> let newRoom = R.removeItem iname (currentRoom gsNew)
                           in
                               setMessage ("You take the " ++ show iname ++ ".") 
                                          (gsNew {gmap = setRoomMap (rname newRoom) newRoom (gmap gsNew),
                                                      player = P.addItem iname (player gsNew)})

-- Drops an item from the player of the GameState and adds the item to the current room
dropItem :: ItemName -> GameState -> GameState
dropItem iname gs = case (anywhereDropCheck iname gs >>= inRoomDropCheck iname) of
                           Left err -> setMessage err gs
                           Right gsNew -> let newRoom = R.addItem iname (currentRoom gsNew)
                            in
                               setMessage ("You drop the " ++ show iname ++ ".")
                                          (gsNew {gmap = setRoomMap (rname newRoom) newRoom (gmap gsNew),
                                                      player = P.removeItem iname (player gsNew)})


-- Returns the total weight of the inventory of a GameState's player
inventoryWeight :: GameState -> Integer
inventoryWeight gs = sum (map weight (map (\x -> getObject x gs) (inventory $ player gs)))

-- Checks if a player in the given GameState already has the given Item
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname gs = if elem iname (inventory $ player gs) 
                                   then Left ("You are already carrying the " ++ show iname)
                                   else Right gs

-- Checks if the given item is present in the room in which the player of the 
-- GameState is currently in
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname gs = if elem iname $ nearbyObjects gs
                              then Right gs
                              else Left ("There is no " ++ show iname ++ " in this room.")

-- Checks if the player in a GameState can carry the given item
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck iname gs = if ((inventoryWeight gs) + (weight $getObject iname gs)) > (maxWeight $ player gs)
                          then Left ("That's too much weight for you to carry.") 
                          else Right gs

-- Checks if an item is present in the room of in the players's inventory
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname gs = if (elem iname (inventory $ player gs)) || (elem iname $ nearbyObjects gs)
                                then Right gs
                                else Left ("What do you mean, drop the " ++ show iname ++ " ?")

-- Checks if the item is present in the room the player is currently in
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck iname gs = if elem iname $ nearbyObjects gs
                              then Left ("You aren't carrying the " ++ show iname)
                              else Right gs

-- Predicate to check if the room in which the player is currently in, has objects
roomHasObjects :: GameState -> Bool
roomHasObjects gs = R.hasObjects (currentRoom gs)

-- Returns the room if walked in the given direction from input room
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir room = lookup dir (exits room)

-- Moves the player to the goven direction
move :: Direction -> GameState -> GameState
move dir gs = case destinationName dir $ currentRoom gs of 
              Nothing -> setMessage "There is no exit in that direction." gs
              Just rname -> setMessage ("You go " ++ show dir ++ ".") (gs {player = P.newLocation rname $ player gs})

-- Predicate to check if player finished level 1
havewonLevel1 :: GameState -> Bool
havewonLevel1 gs = rname (currentRoom gs) == DyneLab && checkItems [QuantumSuit, Gauntlet] (inventory $ player gs)


-- Predticate to check if player won the game
haveWonGame :: GameState -> Bool
haveWonGame gs = rname (currentRoom gs) == Earth && checkItems itemsToWin (inventory $ player gs)

-- Predicate to check if second list has all elements in first list
checkItems :: [ItemName] -> [ItemName] -> Bool
checkItems [] _ = True
checkItems (x : xs) lst2 = if (elem x lst2) then checkItems xs lst2 else False

-- Changes game state from level 1 to level 2
level2State :: GameState -> GameState
level2State _ = GameState Nothing gameMap univLevel2 youLevel2

-- Changes players room - Used in teleport
changeRoom :: RoomName -> GameState -> GameState
changeRoom rname gs = gs {player = P.newLocation rname (player gs)}

-- Predicate to check if game is in level 1 or level 2
checkUniverse :: GameState -> Bool
checkUniverse gs = if (universe gs == univ) then False else True

-- Changes game state to winning game state
winState :: GameState -> GameState
winState _ = GameState Nothing gameMap univLevel2 (Player itemsToWin 150 Earth)


