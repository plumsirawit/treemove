module GameState where

import Data.Tree
import Level

data Item = ItemBox | ItemKey Char
  deriving (Show, Eq, Ord)

data GameState = GameState
  { levelIdent :: String,
    initialLevel :: LTree,
    zipper :: LZip,
    movesCount :: Integer,
    bonusCount :: Integer,
    inventory :: Maybe Item
  }

getItem :: LZip -> Maybe Item
getItem (c, t) = case fst $ rootLabel t of
  B -> Just ItemBox
  E -> Nothing
  S -> Nothing
  F -> Nothing
  W -> Nothing
  Key x -> Just (ItemKey x)
  Gate _ -> Nothing
  Z -> Nothing