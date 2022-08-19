module Direction where

data Direction = N
               | E
               | S
               | W
               deriving (Read, Eq)

-- read a character 'N', 'E', 'S', 'W' to a direction
readDirection:: Char -> Direction
readDirection char = read ([char] :: String)

instance Show Direction where
  show (N) = show "north"
  show (E) = show "east"
  show (S) = show "south"
  show (W) = show "west"

-- all directions
directions :: [Direction]
directions = [N, E, S, W]