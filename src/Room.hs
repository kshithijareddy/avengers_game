module Room where

import Item
import Direction
import qualified Data.List


data RoomName
  = StarkTower
  | ShieldLab
  | Sanctum
  | Wakanda
  | DyneLab
  | Asgard
  | Statesman
  | Knowhere
  | Morag
  | Titan
  | Vormir
  | Ego
  | Xandar
  | Earth
  deriving (Eq, Ord)

instance Show RoomName where
      show rname = case rname of StarkTower -> "stark tower"
                                 ShieldLab -> "shield lab"
                                 Sanctum -> "sanctum"
                                 Wakanda -> "wakanda"
                                 DyneLab -> "dyne lab"
                                 Asgard -> "argard"
                                 Statesman -> "statesman"
                                 Knowhere -> "knowhere"
                                 Morag -> "morag"
                                 Titan -> "titan"
                                 Vormir -> "vormir"
                                 Ego -> "ego"
                                 Xandar -> "xandar"
                                 Earth -> "earth"

type Exit = (Direction,RoomName)

data Room = Room {rname :: RoomName,
                  desc :: String,
                  exits :: [Exit],
                  objects :: [ItemName]}
            deriving (Show, Eq)

starkTower :: Room
starkTower = Room StarkTower "You are in the Stark Tower." [(N,Wakanda),(S,Sanctum),(E,ShieldLab)] [MarkOne,Gauntlet]

shieldLab :: Room 
shieldLab = Room ShieldLab "You are in the Shield Lab." [(W,StarkTower)] [AntimatterGenerator, WarpDevice]

sanctum :: Room 
sanctum = Room Sanctum "You are in the Sanctum." [(N,StarkTower)] [BlackMirror, SlingRing]

wakanda :: Room
wakanda = Room Wakanda "You are in Wakanda. Wakanda Forever!" [(N,DyneLab), (S,StarkTower)] [WakandanShields,QuantumSuit]

dyneLab :: Room
dyneLab = Room DyneLab "You are in the Dyne Laboratory! If you have Quantum Suit and the Gauntlet you can move to the next level!" [(S, Wakanda)] [AntmanSuit]

asgard :: Room
asgard = Room Asgard "You are on Asgard." [(N, Ego), (E, Morag), (S, Knowhere)] [SpaceStone,Sceptor]

statesman :: Room
statesman = Room Statesman "You are on Statesman Spaceship!" [(W, Morag), (S, Xandar), (N, Earth)] [MindStone, Mjolnir]

knowhere :: Room
knowhere = Room Knowhere "You are on Knowhere!" [(N, Asgard), (E, Titan), (W, Earth)] [PowerStone]

morag :: Room
morag = Room Morag "You are on Morag." [(W, Asgard), (E, Statesman)] [RealityStone, Shield]

titan :: Room
titan = Room Titan "You are on Titan! Beware of Thanos!" [(W, Knowhere), (S, Earth)] [TimeStone, Agamotto]

vormir :: Room
vormir = Room Vormir "You are on Vormir. The only world disconnected from every other world." [] [SoulStone]

ego :: Room
ego = Room Ego "You are on Ego" [(S, Asgard), (N, Xandar)] [Stormbreaker,Nanite]

xandar :: Room
xandar = Room Xandar "You are on Xandar." [(S, Ego), (N,Statesman), (E, Earth)] [Darkhold, Godslayer]

earth :: Room
earth = Room Earth "You are on Earth." [(S, Statesman), (N,Titan), (W, Earth), (E, Knowhere)] []


-- List of room names
roomNames :: [RoomName]
roomNames = map rname allRooms

-- Adds an object to the specified room
addItem :: ItemName -> Room -> Room
addItem item room = room {objects = item : objects room}

-- Removed an object from the specified room
removeItem :: ItemName -> Room -> Room
removeItem item room = room {objects = Data.List.delete item $ objects room}

-- list of all rooms
allRooms :: [Room]
allRooms = [starkTower, shieldLab, sanctum, dyneLab, wakanda, asgard, statesman, morag, titan, vormir,knowhere, ego, xandar, earth]

-- Predicate to check if a room has objects
hasObjects :: Room -> Bool
hasObjects room = not (objects room == [])

