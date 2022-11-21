module Command where

import Text.Parsec hiding (parse, runParser, (<|>), sepBy1, choice)
import qualified Text.Parsec as P 
import Text.Parsec.String (Parser)
import Item
import Direction

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Exit
  | Teleport
  | Thanos
  deriving (Eq, Show)

type Conjunction = [Command]

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = sepBy1 body (P.try sep)

parse :: Parser a -> String -> Either ParseError a 
parse prsr = P.parse prsr ""

-- Parser for ItemName
itemNameP :: Parser ItemName
itemNameP = choice [MarkOne <$ string "mark one",
                    Gauntlet <$ string "gauntlet",
                    AntimatterGenerator <$ string "antimatter generator",
                    WarpDevice <$ string "warp device",
                    SlingRing <$ string "sling ring",
                    BlackMirror <$ string "black mirror",
                    WakandanShields <$ string "wakandan shields",
                    QuantumSuit <$ string "quantum suit",
                    AntmanSuit <$ string "antman suit",
                    SpaceStone <$ string "space stone",
                    MindStone <$ string "mind stone",
                    RealityStone <$ string "reality stone",
                    PowerStone <$ string "power stone",
                    TimeStone <$ string "time stone",
                    SoulStone <$ string "soul stone",
                    Sceptor <$ string "sceptor",
                    Mjolnir <$ string "mjolnir",
                    Shield <$ string "shield",
                    Stormbreaker <$ string "stormbreaker",
                    Agamotto <$ string "agamotto",
                    Darkhold <$ string "darkhold",
                    Godslayer <$ string "godslayer",
                    Nanite <$ string "nanite"]
 
-- Stub parser for noun phrases
nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = many1 itemNameP

-- Parser for noun phrases
nounPhrase :: Parser [ItemName]
nounPhrase = P.sepBy1 itemNameP (choice [string "," <* string " ", string ","])

inventoryP :: Parser Command
inventoryP = Inventory <$ (string "inventory")

takeP :: Parser Command
takeP = Take <$> (string "take " *> nounPhrase)

exitP :: Parser Command
exitP = Exit <$ (string "exit" <|> string "quit")

dropP :: Parser Command
dropP = Drop <$> (string "drop " *> nounPhrase)

lookP :: Parser Command
lookP = Look <$ (string "look")

directionP :: Parser Direction
directionP = choice [N <$ string "north",
                     S <$ string "south",
                     E <$ string "east",
                     W <$ string "west"]

moveP :: Parser Command
moveP = Move <$> directionP

teleportP :: Parser Command
teleportP = Teleport <$ (string "teleport")

thanosP :: Parser Command
thanosP = Thanos <$ (string "thanos")

commandP :: Parser Command
commandP = choice [inventoryP, lookP, takeP, dropP, moveP, exitP, teleportP, thanosP]

conjunctionP :: Parser Conjunction
conjunctionP = (P.sepBy1 (commandP) (string " and ")) <* eof

-- Function to parse the input by user
parseInput :: String -> Maybe Conjunction
parseInput str = case (parse conjunctionP str) of
             Right parsed -> Just parsed
             Left _ -> Nothing




          