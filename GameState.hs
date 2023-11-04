module GameState where

import Data.Tree
import Level

data Item = ItemBox | ItemKey Char
  deriving (Eq, Ord)

instance Show Item where
  show ItemBox = "a box"
  show (ItemKey c) = "the key " ++ [c]

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

toType :: Item -> LType
toType item = case item of
  ItemBox -> B
  ItemKey s -> Key s