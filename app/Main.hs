module Main where

import Control.Monad.State

import GameState
import GameIO

-- flip eval and give default value initialState
eval ::   StateT GameState IO a -> IO a
eval = (flip evalStateT) initialState

-- run repl in loop
main :: IO ()
main = do
  evalStateT opening initialState
  eval (forever repl)