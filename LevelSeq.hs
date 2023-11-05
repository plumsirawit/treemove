module LevelSeq where

import Data.Maybe
import GameState
import Level
import Levels.L1 (t)
import Levels.L2 (t)
import Levels.L3 (t)
import Levels.L4 (t)
import Levels.L5 (t)

loadLevel :: String -> Maybe LTree
loadLevel id = case id of
  "L1-hello" -> Just Levels.L1.t
  "L2-happy" -> Just Levels.L2.t
  "L3-water" -> Just Levels.L3.t
  "L4-haste" -> Just Levels.L4.t
  "L5-multi" -> Just Levels.L5.t
  _ -> Nothing

nextLevelIdent :: GameState -> Maybe String
nextLevelIdent z = case levelIdent z of
  "L1-hello" -> Just "L2-happy"
  "L2-happy" -> Just "L3-water"
  "L3-water" -> Just "L4-haste"
  "L4-haste" -> Just "L5-multi"
  "L5-multi" -> Nothing
  _ -> Nothing

firstLevel :: GameState
firstLevel =
  GameState
    { levelIdent = "L1-hello",
      initialLevel = fromJust $ loadLevel "L1-hello",
      zipper = fromJust $ zipToStart $ fromJust $ loadLevel "L1-hello",
      movesCount = 0,
      bonusCount = 0,
      inventory = Nothing
    }
