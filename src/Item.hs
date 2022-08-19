module Item where

import qualified Data.Map as M
import Data.Char
import Data.List

type Universe = M.Map ItemName Item

data Item
   = Item { iname :: ItemName
          , weight :: Integer
          , description :: String
          , use :: String
          , used :: Bool
          }
     deriving (Show, Eq)

data ItemName
  = TrailMap
  | WalkieTalkie
  | Lighter
  | HikingStick
  | HotDog
  | BabyBear
  | DeadFish
  | BeerCan
  | Boulder
  | FlareGun
  | Rope
  | Knife
  | Blanket
  | BearSpray
  deriving (Read, Eq, Ord)

instance Show ItemName where
  show TrailMap = "trail map"
  show WalkieTalkie = "walkie talkie"
  show Lighter = "lighter"
  show HikingStick = "hiking stick"
  show HotDog = "hot dog"
  show BabyBear = "baby bear"
  show DeadFish = "dead fish"
  show BeerCan = "beer can"
  show Boulder = "boulder"
  show FlareGun = "flare gun"
  show Rope = "rope"
  show Knife = "knife"
  show Blanket = "blanket"
  show BearSpray = "bear spray"

-- function to read string to ItemName
readItemName :: String -> ItemName
readItemName str = read str

-- helper function for properCase
-- uppercase first letter of string
capitalizeFirstLetter :: String -> String
capitalizeFirstLetter [] = []
capitalizeFirstLetter (x:xs) = (toUpper x) : xs

-- proper case a string
-- so each (white space separated) token starts with uppercase letter
properCase :: String -> String
properCase inputStr =
  intercalate "" $ map capitalizeFirstLetter (words inputStr)

trailMap :: Item
trailMap = Item
  { iname = TrailMap
  , weight = 10
  , description = "A faded trail map."
  , use = "You can't figure out where you are on the map."
  , used = False
  }

walkieTalkie :: Item
walkieTalkie = Item
  { iname = WalkieTalkie
  , weight = 20
  , description = "A single walkie talkie."
  , use = "\"Hello? Can anybody hear me?\""
  , used = False
  }

lighter :: Item
lighter = Item
  { iname = Lighter
  , weight = 10
  , description = "A mini bic lighter."
  , use = "A small flame!"
  , used = False
  }

hikingStick:: Item
hikingStick = Item
  { iname = HikingStick
  , weight = 40
  , description = "Someone left their hiking stick behind."
  , use = "You walk in a circle with the stick."
  , used = False
  }

hotDog :: Item
hotDog = Item
  { iname = HotDog
  , weight = 15
  , description = "An old hot dog."
  , use = "You wave the hot dog in the air."
  , used = False
  }

babyBear :: Item
babyBear = Item
  { iname = BabyBear
  , weight = 50
  , description = "A cute baby black bear ʕ·ᴥ·ʔ He looks hungry."
  , use = "You can't use a bear."
  , used = False
  }

deadFish :: Item
deadFish = Item
  { iname = DeadFish
  , weight = 15
  , description = "Dead fish. Looks like he has a story to tell."
  , use = "You have a short conversation with the dead fish.\
           \\nAs a young guppy, he dreamed of living on land with the bear \
           \and the people.\nHe strengthened his tail and practiced his \
           \jumping util one day he jumped out of the stream.\n\
           \He was surprised to find that he couldn't swim on land;\
           \ he has been stuck here ever since."
  , used = False
  }

beerCan :: Item
beerCan = Item
  { iname = BeerCan
  , weight = 5
  , description = "Someone left an empty beer can."
  , use = "You take a sip from the can. \
          \A drop of nasty puddle water comes out."
  , used = False
  }

boulder :: Item
boulder = Item
  { iname = Boulder
  , weight = 120
  , description = "A 10ft tall boulder."
  , use = "You climb the boulder! It's a V3."
  , used = False
  }

flareGun :: Item
flareGun = Item
  { iname = FlareGun
  , weight = 50
  , description = "A small red flare gun."
  , use = "You shoot a flare into the air!"
  , used = False
  }

rope :: Item
rope = Item
  { iname = Rope
  , weight = 70
  , description = "A 70 meter rope. Looks like a climber left it behind."
  , use = "You tie a figure eight into the rope."
  , used = False
  }

knife :: Item
knife = Item
  { iname = Knife
  , weight = 30
  , description = "A gerber knife."
  , use = "You touch the blade. The knife is sharp!"
  , used = False
  }

blanket :: Item
blanket = Item
  { iname = Blanket
  , weight = 25
  , description = "A wool blanket."
  , use = "You take a short nap with the blanket."
  , used = False
  }

bearSpray :: Item
bearSpray = Item
  { iname = BearSpray
  , weight = 20
  , description = "An unused can of bear spray."
  , use = "The bear is gone. Don't waste it!"
  , used = False
  }

-- |Universe helper functions and constants
allItems :: [Item]
allItems = [trailMap, walkieTalkie, lighter,
            hikingStick, hotDog, babyBear,
            deadFish, beerCan, boulder,
            flareGun, rope, knife,
            blanket, bearSpray]

-- given a  list of Items, create map with key item name, value item
mkUniverse :: [Item] -> Universe
mkUniverse lst = M.fromList (map (\item -> (iname item, item)) lst)

-- | univ is a list of Items, consisting of
-- stove, pot, couch, sandbag, jug, grill, bed, tarragon, beans
univ :: Universe
univ = mkUniverse allItems

-- list of all item names
itemNames :: [ItemName]
itemNames = M.keys univ

-- the max weight of all item constants
maxItemWeight :: Integer
maxItemWeight = maximum (map (\item -> weight item) allItems)

-- the min weight of all item constants
minItemWeight :: Integer
minItemWeight = minimum (map (\item -> weight item) allItems)
