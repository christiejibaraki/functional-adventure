module Room where

import Item
import Direction

type Exit = (Direction, RoomName)

data Room
   = Room { rname :: RoomName
          , desc :: String
          , exits :: [Exit]
          , objects :: [ItemName]
          }
     deriving (Show, Eq)

data RoomName
  = Trailhead
  | Stream
  | ParkingLot
  | CampSite
  | BoulderField
  | Cave
  | Cliff
  | PineForest
  deriving (Eq, Ord)

instance Show RoomName where
  show Trailhead = "trailhead"
  show Stream = "stream"
  show ParkingLot = "parking lot"
  show CampSite = "camp site"
  show BoulderField = "boulder field"
  show Cave = "cave"
  show Cliff = "cliff"
  show PineForest = "pine forest"

-- |room constants
trailhead :: Room
trailhead = Room
  { rname = Trailhead
  , desc = "You are at the trailhead."
  , exits = [(N, CampSite), (E, Stream), (S, ParkingLot)]
  , objects = [TrailMap, HikingStick]
  }

stream :: Room
stream = Room
  { rname = Stream
  , desc = "This is a quiet stream."
  , exits = [(W, Trailhead)]
  , objects = [BabyBear, DeadFish]
  }

parkingLot :: Room
parkingLot = Room
  { rname = ParkingLot
  , desc = "You are in a small, gravel parking lot."
  , exits = [(N, Trailhead)]
  , objects = [BeerCan]
  }

campSite :: Room
campSite = Room
  { rname = CampSite
  , desc = "This is a beautiful campsite."
  , exits = [(N, BoulderField), (S, Trailhead)]
  , objects = [HotDog, WalkieTalkie, Lighter]
  }

boulderField :: Room
boulderField = Room
  { rname = BoulderField
  , desc = "This is a field full of large sandstone boulders."
  , exits = [(S, CampSite)]
  , objects = [Boulder]
  }

cave :: Room
cave = Room
  { rname = Cave
  , desc = "This is a cold, dark cave."
  , exits = [(N, Cliff), (S, PineForest)]
  , objects = [Blanket, Rope]
  }

pineForest :: Room
pineForest = Room
  { rname = PineForest
  , desc = "This is a large pine forest."
  , exits = [(N, Cave)]
  , objects = [FlareGun, BearSpray]
  }

cliff :: Room
cliff = Room
  { rname = Cliff
  , desc = "This is a cliff overlooking a gorge."
  , exits = [(S, Cave)]
  , objects = [Knife]
  }

-- |list of rooms and roomnames
-- previously called "rooms" (pre week 4)
allRoomsLevelOne :: [Room]
allRoomsLevelOne = [trailhead, campSite, stream, boulderField, parkingLot]

allRoomsLevelTwo :: [Room]
allRoomsLevelTwo = [cave, cliff, pineForest]

roomNamesLevelOne :: [RoomName]
roomNamesLevelOne =  map rname allRoomsLevelOne

-- week 4: room getters and setters
-- add itemname to room objects
addItem :: ItemName -> Room -> Room
addItem iname room = room {objects= (iname:(objects room))}

-- remove itemname from room objects
removeItem :: ItemName -> Room -> Room
removeItem iname room = room {objects = (filter (/= iname) (objects room))}

-- returns true if there are objects in the room
hasObjects :: Room -> Bool
hasObjects room
  | objects room == [] = False
  | otherwise = True