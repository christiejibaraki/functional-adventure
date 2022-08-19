module Example where
-- | Example Type Class (week 3)
-- Provides randomly-generated instances of our datatypes
-- Use this for testing

import System.Random
import qualified Data.Map as M

import Item
import Direction
import Room
import Player
import GameState

class Example a where
  example :: IO a

-- Example instances

-- randomly generated name and weight (0,100)
instance Example Item where
  example = do
    name <- choose itemNames
    weight <- randomRIO (0,100)
    let newItem = Item { iname = name
                        , weight = weight
                        , description = "An item description."
                        , use = "You use the item."
                        , used = False
                        }
    return newItem

-- randomly select direction
instance Example Direction where
  example = choose directions

-- (not an instance of Example)
-- random direction and random roomname
exitExample :: IO Exit
exitExample = do
  randDirection <- choose directions
  randRoomName <- choose roomNamesLevelOne
  return (randDirection, randRoomName)

-- | Example Room
-- rname: randomly selected from roomNames
-- desc: tells you what room you're in (based on room name)
-- exits: (2-4) randomly generated exits
-- objects: (2-5) randomly generated items
instance Example Room where
  example = do
    randRoomName <- choose roomNamesLevelOne
    let roomDesc = "You are in a randomly generated room, which is the "
                 <> (show randRoomName)
    exits <- exampleList (exitExample) (randomRIO (2,4))
    items <- exampleList (example) (randomRIO (2,5))
    let smallUniv = mkUniverse items
    let smallItemNames = M.keys smallUniv
    let newRoom = Room randRoomName roomDesc exits smallItemNames
    return newRoom

-- | Example Player
-- inventory: 0-10 random items, without duplicates
-- maxWeight: random weight between (minItemWeight, maxItemWeight)
-- location : randomly generated room
instance Example Player where
  example = do
    playerItems <- exampleList (example :: IO Item) (randomRIO (0,10))
    let playerUniv = mkUniverse playerItems -- will never be duplicates
    let playerItemNames = M.keys playerUniv
    playerMaxWeight <- randomRIO(minItemWeight, maxItemWeight)
    randRoomName <- choose roomNamesLevelOne
    let newPlayer = Player playerItemNames playerMaxWeight randRoomName
    return newPlayer

-- | Example GameState
-- (week 4: rooms will not be spacially reasonable)
-- (1) message can be randomly selected from (Maybe String)
--  "One possible message.", "Yet another possible message", no message.
-- (2) 2-3 randomly generated rooms
-- (3) 5-10 randomly generated Items
-- (4) player is randomly generated
instance Example GameState where
  example = do
    player <- example :: IO Player
    playerItems <- exampleList (example :: IO Item) (randomRIO (5,10))
    ranRooms <- exampleList (example :: IO Room) (randomRIO (2,3))
    ranMessage <- choose [Just "One possible message."
                         ,Just "Yet another possible message."
                         ,Nothing]
    return GameState
      { message = ranMessage
      , gmap = mkMap ranRooms
      , universe = mkUniverse playerItems
      , player = player
      }

-- |returns length of list as Integer
length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length'(xs)

-- |outputs a (single) random element from list
choose :: [a] -> IO a
choose lst = do
  let ranIndex = randomRIO (0,(length' lst)-1)
  x <- ranIndex
  return (lst!!(fromInteger x))

{-|
  exampleList generates list of randomly generated items
  Input: IO a, IO Int
  Output: IO [a] of randomly-generated items, length of IO Int
-}
exampleList :: IO a -> IO Int -> IO [a]
exampleList ioA ioInt = do
  pureInt <- ioInt
  sequence $ replicate pureInt ioA
