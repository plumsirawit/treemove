module LevelSeq where

import Data.Maybe
import GameState
import Level
import Levels.L1 (t)
import Levels.L2 (t)
import Levels.L3 (t)
import Levels.L4 (t)

loadLevel :: String -> Maybe LTree
loadLevel id = case id of
  "L1" -> Just Levels.L1.t
  "L2" -> Just Levels.L2.t
  "L3" -> Just Levels.L3.t
  "L4" -> Just Levels.L4.t
  _ -> Nothing

nextLevelIdent :: GameState -> Maybe String
nextLevelIdent z = case levelIdent z of
  "L1" -> Just "L2"
  "L2" -> Just "L3"
  "L3" -> Just "L4"
  "L4" -> Nothing
  _ -> Nothing

firstLevel :: GameState
firstLevel =
  GameState
    { levelIdent = "L1",
      initialLevel = fromJust $ loadLevel "L1",
      zipper = fromJust $ zipToStart $ fromJust $ loadLevel "L1",
      movesCount = 0,
      bonusCount = 0,
      inventory = Nothing
    }
