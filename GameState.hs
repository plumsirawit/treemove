module GameState where

import Level

data GameState = GameState
  { levelIdent :: String,
    initialLevel :: LTree,
    zipper :: LZip,
    movesCount :: Integer,
    bonusCount :: Integer
  }