module Item where

import qualified Data.Map as M
import Data.List

data ItemName
  = MarkOne
  | Gauntlet
  | AntimatterGenerator
  | WarpDevice
  | SlingRing
  | BlackMirror
  | WakandanShields
  | QuantumSuit
  | AntmanSuit
  | SpaceStone
  | MindStone
  | RealityStone
  | PowerStone
  | TimeStone
  | SoulStone
  | Sceptor
  | Mjolnir
  | Shield
  | Stormbreaker
  | Agamotto
  | Darkhold
  | Godslayer
  | Nanite
  deriving ( Eq, Ord)

instance Show ItemName where
    show iname = case iname of MarkOne -> "mark one "
                               Gauntlet -> "gauntlet"
                               AntimatterGenerator -> "antimatter generator"
                               WarpDevice -> "warp device"
                               SlingRing -> "sling ring"
                               BlackMirror -> "black mirror"
                               WakandanShields -> "wakandan shields"
                               QuantumSuit -> "quantum suit"
                               AntmanSuit -> "antman suit"
                               MindStone -> "mind stone"
                               RealityStone -> "reality stone"
                               PowerStone -> "power stone"
                               TimeStone -> "time stone"
                               SpaceStone -> "space stone"
                               SoulStone -> "soul stone"
                               Sceptor -> "sceptor"
                               Stormbreaker -> "stormbreaker"
                               Darkhold -> "darkhold"
                               Shield -> "shield"
                               Agamotto -> "agamotto"
                               Mjolnir -> "mjolnir"
                               Godslayer -> "godslayer"
                               Nanite -> "nanite"

type Universe = M.Map ItemName Item

-- Defining Item record type
data Item = Item{iname :: ItemName, weight :: Integer}
            deriving (Show,Eq)

-- Function to create a new universe
mkUniverse :: [Item] -> Universe
mkUniverse lst = let x = associationList lst
                 in M.fromList x

-- Function to create a association list
associationList :: [Item] -> [(ItemName,Item)]
associationList [] = []
associationList (x : xs) = (iname x,x) : associationList xs

markOne :: Item
markOne = Item MarkOne 20

gauntlet :: Item
gauntlet = Item Gauntlet 10

antimatterGenerator :: Item
antimatterGenerator = Item AntimatterGenerator 30

warpDevice :: Item
warpDevice = Item WarpDevice 60

slingRing :: Item
slingRing = Item SlingRing 80

blackMirror :: Item
blackMirror = Item BlackMirror 10

wakandanShields :: Item
wakandanShields = Item WakandanShields 15

quantumSuit :: Item
quantumSuit = Item QuantumSuit 50

antmanSuit :: Item
antmanSuit = Item AntmanSuit 80

powerStone :: Item
powerStone = Item PowerStone 5

timeStone :: Item
timeStone = Item TimeStone 5

soulStone :: Item
soulStone = Item SoulStone 5

spaceStone :: Item
spaceStone = Item SpaceStone 5

mindStone :: Item
mindStone = Item MindStone 5

realityStone :: Item
realityStone = Item RealityStone 5

sceptor :: Item
sceptor = Item Sceptor 20

stormbreaker :: Item
stormbreaker = Item Stormbreaker 20

darkhold :: Item
darkhold = Item Darkhold 10

shield :: Item
shield = Item Shield 40

mjolnir :: Item
mjolnir = Item Mjolnir 20

agamotto :: Item
agamotto = Item Agamotto 5

godslayer :: Item
godslayer = Item Godslayer 10

nanite :: Item
nanite = Item Nanite 30 

-- A map of all item names and the items in Level 1
univ :: Universe
univ = mkUniverse [gauntlet,markOne,slingRing,antimatterGenerator,warpDevice,quantumSuit,wakandanShields,blackMirror,antmanSuit]

-- A map of all item names and the items in Level 2
univLevel2 :: Universe
univLevel2 = mkUniverse [gauntlet,quantumSuit,spaceStone, mindStone, soulStone, realityStone, powerStone, timeStone, sceptor, mjolnir, shield, agamotto, stormbreaker, darkhold, nanite, godslayer]

-- List of all item names
itemNames :: [ItemName]
itemNames = nub ((M.keys univ) ++ (M.keys univLevel2))

-- List of items for player to win
itemsToWin :: [ItemName]
itemsToWin = [SpaceStone, MindStone, SoulStone, RealityStone, PowerStone, TimeStone, Gauntlet, QuantumSuit]
