module Direction where

--Direction datatype
data Direction = N | S | E | W deriving Eq

-- Show type class instance
instance Show Direction where
    show dir = case dir of N -> "north"
                           S -> "south"
                           E -> "east"
                           W -> "west"
