module GameIO where

import Control.Monad.State
import System.Exit
import System.IO
import Data.List
import GameState
import Room
import Command
import Item

type GameIO a = StateT GameState IO a

-- |effectChange
-- input: transition between game states
-- output: a value of type GameIO
--    that performs the transition within the monadic GameIO value
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange f = do
  gs <- get
  put $ f gs


-- |prompt
-- prints the string "->" (without a newline char) to the console
-- which is a visible prompt for the user
prompt :: GameIO ()
prompt = lift $ do
  putStr "->"
  hFlush stdout


---- PRINTY FUNCTIONS
-- |printMessage
-- check current state of game, if there is a just value in message
-- print message to screen, then set gs message to Nothing
-- else do nothing
printMessage :: GameIO ()
printMessage = do
  gs <- get
  case (message gs) of
    Just msg -> do
      lift $ putStrLn msg
      effectChange (setMessage "")
    Nothing -> pure ()

-- |printDescription
-- print description of the room where the player is in gamestate
printDescription :: GameIO ()
printDescription = do
  gs <- get
  lift . putStrLn . desc . currentRoom $ gs
-- alternatively
--printDescription = get >>= lift . putStrLn . desc . currentRoom

-- |printObjects
-- IF there are objects in the room the player is in:
-- prints "You see the following objects:",
--    followed by a list of all the items in the room
-- otherwise does nothing
printObjects :: GameIO ()
printObjects = do
  gs <- get
  let nearbyItems = nearbyObjects $ gs
  case nearbyItems of
    [] -> return ()
    x-> (lift $ putStrLn $ "You see the following things:\n"
        ++ (intercalate "\n" $ map show (x)))

-- |printExits
-- If there are exits in the room:
-- prints "There are exits in the following directions:"
-- followed by a list of all the directions there are exits in
-- otherwise does nothing
printExits :: GameIO ()
printExits = do
  gs <- get
  let exitString = (intercalate "\n" $ map (\(x, y)-> show x)
                    (exits $ currentRoom $ gs))
  case exitString of
    [] -> return ()
    x-> lift $ putStrLn $ "There are paths in the following directions:\n" ++ x

-- |printExits
-- If the player's current inventory is nonempty:
-- prints "You are carrying the following items:"
-- followed by a list of all the ItemName-s in the player's inventory
-- otherwise prints "You aren't carrying anything."
printInventory :: GameIO ()
printInventory = do
  gs <- get
  let inventoryString = intercalate "\n" $ map show (currentInventory  $ gs)
  case inventoryString of
    [] -> lift $ putStrLn "You aren't carrying anything."
    x ->  lift $ putStrLn $ "You are carrying the following items:\n" ++ x

-- |actionOverList
-- Performs action on each item in input and runs printMessage
-- Input:
--  function describing an action on items (e.g. take, drop)
--  list of items
-- Output: NA, prints message
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList f [] = return ()
actionOverList f (x:xs) = do
  effectChange (f x)
  gs <- get
  case (message gs) of
    Just msg -> do
      lift $ putStrLn msg
      effectChange (setMessage "")
      actionOverList f xs
    Nothing -> pure ()

-- |finishLevelOne
-- Moves player to different part of game
finishLevelOne :: GameIO ()
finishLevelOne = do
  lift $ putStrLn "The baby bear eats the old hot dog ⊂ZZZ⊃\nOut of nowhere, \
    \Mama Bear comes charging towards you!\nYou run and run, \
    \until you find a cave to hide in.\nYou've managed to ditch the bears, \
    \but you are lost..."
  effectChange (setGameMapLevelTwo)

-- |winLevelTwo
-- Prints a success message to the screen, then quits the program
winLevelTwo :: GameIO ()
winLevelTwo = do
  lift $ putStrLn "The park rangers have your location and are coming to save \
    \you! You can hear the helicopter nearby.\nCongrats, you are safe! Goodbye."
  lift exitSuccess

-- |die
-- Prints a success message to the screen, then quits the program
die :: GameIO ()
die = do
  lift $ putStrLn "The baby bear starts crying. \
                  \Mama Bear jumps out of a tree and claws your face off.\
                  \\nGAME OVER"
  lift exitSuccess

-- |exit
-- Exit the game when the user wants to quit
exit :: GameIO ()
exit =  do
  lift $ putStrLn "OK BYE ʕ·ᴥ·ʔ ʕ ·ᴥ· ʔ"
  lift exitSuccess

-- |checkLevelOneOver
-- checks whether player has passed level one
checkLevelOneOver :: GameIO ()
checkLevelOneOver  = do
  gs <- get
  let won = passLevelOne $ gs
  case won of
    True -> finishLevelOne
    False  -> pure ()

-- |checkGameOver
-- checks whether the current game state is the winning state.
checkGameOver :: GameIO ()
checkGameOver = do
  gs <- get
  let won = haveWonGame $ gs
  case won of
    True -> winLevelTwo
    False  -> pure ()

-- |checkDie
-- checks whether you die
checkDie :: GameIO ()
checkDie = do
  gs <- get
  let haveBear = takeBear gs
  case haveBear of
    True -> GameIO.die
    False  -> pure ()

-- |syntaxError
-- prints the message 'I don't understand that'
syntaxError :: GameIO ()
syntaxError = lift $ putStrLn "I don't understand that."

-- |opening
-- prints the message '"Welcome to Functional Adventure!"'
opening :: GameIO ()
opening = lift $ putStrLn "          (c).-.(c)\n           \
                          \/ o o \\ \n         __\\( Y )/__\n        \
                          \(_.-/'-'\\-._)\n           ||   ||  \n         \
                          \_.' `-' '._\n        (.-./`-'\\.-.)\n         \
                          \`-'     `-' \n Welcome to Functional Adventure!"

-- |performCommand
-- takes any Command as an input,
-- and executes the action corresponding to the command.
performCommand :: Command -> GameIO ()
performCommand command = do
  case command of
    Look -> do
          printDescription
          printObjects
          printExits
    Move dir -> do
          effectChange (move dir)
          printMessage
    Inventory -> printInventory
    Inspect items -> actionOverList inspectItem items
    Take items -> actionOverList takeItem items
    Drop items -> actionOverList dropItem items
    Use items -> actionOverList useItem items
    Exit -> exit

-- |performConjunction
-- performs every command in a Conjunction, in order:
performConjunction :: Conjunction -> GameIO ()
performConjunction [] = pure ()
performConjunction (x:xs) = do
                            performCommand x
                            performConjunction xs

-- |parseConjunction
-- parses an input string
-- if the parse succeeds: runs performConjunction on the result.
-- else run syntaxError
parseConjunction :: String -> GameIO ()
parseConjunction inputString =
  let maybeConjunction= parseInput inputString
  in case (maybeConjunction) of
    Just a -> performConjunction a
    Nothing -> syntaxError

-- |repl
-- performs one round of:
--    printing the prompt,
--    getting input from the user
--    parsing it
--    conditionally performing the command
--    checkGameOver
repl :: GameIO ()
repl =  do
        prompt
        userInput <- lift getLine
        parseConjunction userInput
        checkDie
        checkLevelOneOver
        checkGameOver