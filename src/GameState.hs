module GameState where
-- | Describes transitions from one game state to other

import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction

type GameMap = M.Map RoomName Room
type Error a = Either String a

data GameState
   = GameState { message :: Maybe String
          , gmap :: GameMap
          , universe :: Universe
          , player :: Player
          }
     deriving (Show, Eq)

-- initial state with a level one game map
initialState :: GameState
initialState = GameState Nothing gameMapLevelOne univ you

-- | turns list of Room into a GameMap
mkMap :: [Room] -> GameMap
mkMap lst = M.fromList (map (\room -> (rname room, room)) lst)

-- | gameMapLevelOne is GameMap consisting of
-- trailhead, parking lot, campsite, boulder field, stream
gameMapLevelOne :: GameMap
gameMapLevelOne = mkMap allRoomsLevelOne

-- | gameMapLevelTwo is GameMap consisting of
-- cave, cliff, pine forest
gameMapLevelTwo :: GameMap
gameMapLevelTwo = mkMap allRoomsLevelTwo

-- | Functions to look up objects by their name
-- if no object with that name exists throw KeyError
-- ** Don't ever call getObject or getRoom on a string literal
-- ** Only call them on ItemName or RoomName respectively,
--    which are extracted from GameState

data KeyError = KeyError
  deriving Show

instance Exception KeyError

-- | getObjectUniv
-- helper function for getObject
-- given itenname and a universe, return associated item
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

-- | getObject
-- given itenname and a gamestate, return associated item
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

-- | getRoomMap
-- helper function for getRoom
-- given RoomName and GameMap, get associated Room
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

-- | getRoom
-- given RoomName and GameState, get associated Room
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- | setRoomMap
-- Helper function to replace existing room instance
-- with another room instance in a GameMap
-- Inputs: room name, room, game map as inputs.
-- Output: new game map with room (given by room name) replaced
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap rname newRoom gameMapUpdate = M.insert rname newRoom gameMapUpdate

-- | setMessage
-- Inputs: String message, GameState
-- Output: GameState with its message field replaced
setMessage :: String -> GameState -> GameState
setMessage newMessage gameStateUpdate =
  gameStateUpdate { message = if null newMessage
                              then Nothing
                              else Just newMessage }

-- | setGameMap
-- Inputs: game map, game state
-- Output: game state with map updated
setGameMapLevelTwo :: GameState -> GameState
setGameMapLevelTwo  gameStateUpdate =
  gameStateUpdate { gmap = gameMapLevelTwo,
                    player = (player gameStateUpdate) {location = Cave}}

-- | roomHasObjects
-- true if there are objects in players current room
roomHasObjects :: GameState -> Bool
roomHasObjects gs = hasObjects $ currentRoom gs

-- | currentInventory
-- Inputs: GameState
-- Output: returns the inventory of that GameState's player
currentInventory :: GameState -> [ItemName]
currentInventory gameStateToCheck = inventory (player gameStateToCheck)

-- |inventoryWeight
-- returns total weight of inventory carried by Player
inventoryWeight :: GameState -> Integer
inventoryWeight gs =
  foldl (\tot iname -> tot +(weight (getObject iname gs))) 0 (currentInventory gs)

-- | currentRoom
-- Inputs: GameState
-- Output: Room the Player is located at in that game state
currentRoom :: GameState -> Room
currentRoom gameStateToCheck =
  let locationRoomName = location (player gameStateToCheck)
  in getRoom locationRoomName gameStateToCheck

-- | nearbyObjects
-- Inputs: GameState
-- Output: list of item names in the room where the player is
nearbyObjects :: GameState -> [ItemName]
nearbyObjects gameStateToCheck = objects (currentRoom gameStateToCheck)

-- | inspectItem
-- Inspect item in your location. Item should not be in your inventory.
-- Inputs: ItemName and a GameState
-- Output: GameState with
--  (1) message in the game state containing item description
validateInspectItem :: ItemName -> GameState -> Error GameState
validateInspectItem iname gs =
  alreadyHaveTakeCheck iname gs
    >>= inRoomTakeCheck iname

inspectItem :: ItemName -> GameState -> GameState
inspectItem itemToInspect gameStateToUpdate =
  let validation = validateInspectItem itemToInspect gameStateToUpdate
      newMessage = Just (description ((getObjectUniv itemToInspect univ)))
  in case validation of
    Left err -> gameStateToUpdate {message = Just err}
    Right _ -> gameStateToUpdate {message = newMessage}

-- | takeItem
-- Pick up an item from a room you're in and add it to your inventory.
-- Inputs: ItemName and a GameState
-- Output: GameState with
--  (1) ItemName removed from the room's objects
--  (2) ItemName added to the GameState's Player's inventory
--  (3) message in the game state saying 'You take the ITEMNAME.'
-- Version 1.0: assume that this function will only ever be called
-- on an item that it actually makes sense for the player to take
validateTakeItem :: ItemName -> GameState -> Error GameState
validateTakeItem  iname gs =
  alreadyHaveTakeCheck iname gs
    >>= inRoomTakeCheck iname
    >>= weightCheck iname

takeItem :: ItemName -> GameState -> GameState
takeItem itemToTake gameStateToUpdate =
  let validation = validateTakeItem itemToTake gameStateToUpdate
      roomToUpdate = currentRoom gameStateToUpdate
      updatedRoom = Room.removeItem itemToTake roomToUpdate
      updatedGameMap =
        setRoomMap (rname updatedRoom) updatedRoom (gmap gameStateToUpdate)
      updatedPlayer = Player.addItem itemToTake (player gameStateToUpdate)
      newMessage = Just ("You take the " <> (show itemToTake))
  in case validation of
    Left err -> gameStateToUpdate {message = Just err}
    Right _ -> gameStateToUpdate {message = newMessage
                                  , gmap = updatedGameMap
                                  , player = updatedPlayer}

-- | dropItem
-- Drop an item from your inventory into Room you're in.
-- Inputs: ItemName and a GameState
-- Output: GameState with
--  (1) ItemName added to room's objects
--  (2) ItemName removed from the GameState's Player's inventory
--  (3) message in the game state saying 'You drop the ITEMNAME.'
-- Version 1.0: assume that this function will only ever be called
-- on an item that it actually makes sense for the player to drop
validateDropItem  :: ItemName -> GameState -> Error GameState
validateDropItem  iname gs =
  anywhereDropCheck iname gs
    >>= inRoomDropCheck iname

dropItem :: ItemName -> GameState -> GameState
dropItem itemToDrop gameStateToUpdate =
  let validation = validateDropItem itemToDrop gameStateToUpdate
      roomToUpdate = currentRoom gameStateToUpdate
      updatedRoom = Room.addItem itemToDrop roomToUpdate
      updatedGameMap =
        setRoomMap (rname updatedRoom) updatedRoom (gmap gameStateToUpdate)
      updatedPlayer = Player.removeItem itemToDrop (player gameStateToUpdate)
      newMessage = Just ("You drop the " <> (show itemToDrop) <> ".")
  in case validation of
    Left err -> gameStateToUpdate {message = Just err}
    Right _ -> gameStateToUpdate {message = newMessage
                                  , gmap = updatedGameMap
                                  , player = updatedPlayer}

-- | useItem
-- use an item in your inventory
-- Inputs: ItemName and a GameState
-- Output: GameState with
--  (1) message in the game state describing usage
--  (2) update item to used
validateUseItem :: ItemName -> GameState -> Error GameState
validateUseItem iname gs = alreadyHaveUseCheck iname gs

useItem :: ItemName -> GameState -> GameState
useItem itemToUse gameStateToUpdate =
  let validation = validateUseItem itemToUse gameStateToUpdate
      itemFromName = (getObjectUniv itemToUse univ)
      updatedItem = itemFromName {used = True}
      updatedUniverse =
        M.insert itemToUse updatedItem (universe $ gameStateToUpdate)
      newMessage = Just (use itemFromName)
  in case validation of
    Left err -> gameStateToUpdate {message = Just err}
    Right _ -> gameStateToUpdate {message = newMessage
                                  , universe = updatedUniverse}

-- | alreadyHaveTakeCheck
-- If player carrying item return error message
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname gs
  | (elem iname $ currentInventory gs) =
    Left ("You are already carrying the " <> (show iname) <> ".")
  | otherwise = Right gs

-- | alreadyHaveUseCheck
-- If player isn't carrying item return error message
-- (a player can use the boulder without taking it)
alreadyHaveUseCheck :: ItemName -> GameState -> Error GameState
alreadyHaveUseCheck iname gs
  | ((elem iname $ currentInventory gs) || iname == Boulder) = Right gs
  | otherwise = Left ("You are aren't carrying the " <> (show iname) <> ".")

-- | inRoomTakeCheck
-- considers whether an item user might try to pick up is nearby
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname gs
  | elem iname $ nearbyObjects gs = Right gs
  | otherwise = Left ("There is no " <> (show iname) <> " here.")

-- | weightCheck
-- determine whether player can pick up new item
-- based on weight of current inventory
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck iname gs = let iweight = weight (getObject iname gs)
                           inventoryweight = inventoryWeight gs
                           maxw = maxWeight $ player gs
                       in if (iweight + inventoryweight) > maxw
                       then Left ("That's too much weight for you to carry.")
                       else Right gs

-- | anywhereDropCheck
-- determine whether item is in player inventory or in player current room
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname gs = let inInventory = elem iname $ currentInventory gs
                                 inRoom = elem iname $ nearbyObjects gs
                             in if (inInventory || inRoom)
                             then Right gs
                             else Left ("What do you mean, drop the "
                                        <> (show iname) <> "?")

-- | inRoomDropCheck
-- determine whether item player wants to drop is in player current room
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck  iname gs = if elem iname $ nearbyObjects gs
                            then Left ("You aren't carrying the "
                                      <> (show iname) <> ".")
                            else Right gs

-- Functions to move the player
-- | destinationName
-- Input: direction, room
-- Output: roomname associated with exit in that direction (if exists)
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir room = lookup dir (exits room)

-- | move
-- Move player to room/exit based on direction
move :: Direction -> GameState -> GameState
move dir gs =
  let destination = destinationName dir $ currentRoom gs
      errorMessage = "There is no trail in that direction"
  in case (destination) of
    Nothing -> gs {message = Just errorMessage}
    Just roomName -> gs {message = Just ("You go " <> (show dir) <>".")
                        ,player = (player gs) {location = roomName}}

-- | passLevelOne
-- returns true if you drop the hotdog near the babybear at the stream...
passLevelOne :: GameState -> Bool
passLevelOne gs =
  let location = rname $ currentRoom gs
      placesHotDog = elem HotDog $ objects (currentRoom gs)
  in (location == Stream && placesHotDog)

-- | haveWonGame
-- returns true if you use the flare gun
haveWonGame :: GameState -> Bool
haveWonGame gs = used (getObjectUniv FlareGun (universe $ gs))

-- | takeBear
-- returns true if you've taken the baby bear
takeBear :: GameState -> Bool
takeBear gs = elem BabyBear $ inventory (player gs)