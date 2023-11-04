{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}

module Level where

import Data.Foldable (find)
import Data.List (delete)
import Data.Maybe (fromJust)
import Data.Tree

data LType = B | E | S | F | W | Key Char | Gate Char | Z
  deriving (Show, Eq, Ord)

type LNode = (LType, String)

type LTree = Tree LNode

data LTreeCxt
  = Hole
  | TreeHole LNode LTreeCxt [LTree]
  deriving (Show, Eq, Ord)

plug :: LTreeCxt -> LTree -> LTree
plug Hole t = t
plug (TreeHole k c t2) t = plug c (Node k (subForest t ++ t2))

type LZip = (LTreeCxt, LTree)

checkLegal :: LZip -> Bool
checkLegal (c, t) = case fst (rootLabel t) of
  B -> True
  E -> True
  S -> True
  F -> True
  W -> False
  Key _ -> True
  Gate _ -> False
  Z -> True

goDown :: LZip -> String -> Maybe LZip
goDown (c, Node k f) s =
  let g = find (\a -> snd (rootLabel a) == s) f
   in case g of
        Nothing -> Nothing
        Just subtree -> Just (TreeHole k c (delete subtree f), subtree)

goUp :: LZip -> Maybe LZip
goUp (TreeHole k c t, out) = Just (c, Node k (out : t))
goUp (Hole, _) = Nothing

unzipToTree :: LZip -> LTree
unzipToTree (c, t)
  | c == Hole = t
  | otherwise = unzipToTree (fromJust (goUp (c, t)))

markCurrentPosition :: LZip -> LZip
markCurrentPosition (c, Node k f) = (c, Node (fst k, '@' : snd k) f)

combineIdent :: LTree -> Tree String
combineIdent (Node k t) = Node (show (fst k) ++ " (" ++ snd k ++ ")") (map combineIdent t)

drawLTree :: LTree -> String
drawLTree = drawTree . combineIdent

dfsAux :: LZip -> Maybe LZip
dfsAux (c, t) =
  if fst (rootLabel t) == S
    then Just (c, t)
    else foldr (\ch res -> let x = dfsAux $ fromJust $ goDown (c, t) (snd $ rootLabel ch) in if x == Nothing then res else x) Nothing (subForest t)

zipToStart :: LTree -> Maybe LZip
zipToStart lt = dfsAux (Hole, lt)