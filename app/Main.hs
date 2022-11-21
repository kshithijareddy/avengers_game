module Main where

import Control.Monad.State

import GameState
import GameIO

main :: IO ()
main = do
  evalStateT opening initialState 
  evalStateT (forever repl) initialState
