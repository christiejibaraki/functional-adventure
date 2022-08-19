module Player where

import Item
import Room

data Player
   = Player { inventory :: [ItemName]
            , maxWeight :: Integer
            , location :: RoomName
            }
     deriving (Show, Eq)

-- add itemname to player inventory
addItem :: ItemName -> Player -> Player
addItem iname player = player {inventory = (iname:(inventory player))}

-- remove itemname from player inventory
removeItem :: ItemName -> Player -> Player
removeItem iname player = player {inventory =
                          (filter (\x -> if (x == iname) then False else True)
                          (inventory player))}

-- returns true if inventory not empty
isCarryingAnything :: Player -> Bool
isCarryingAnything player = length (inventory player) > 0

-- | the game player
-- constant you of type Player
-- max weight of 100, empty inventory, located in the kitchen
you :: Player
you = Player { inventory = [], maxWeight = 100, location = Trailhead}