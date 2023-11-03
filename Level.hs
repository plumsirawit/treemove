module Level where

import Data.Foldable (find)
import Data.List (delete)
import Data.Tree

data LType = B | W | Key Char | Gate Char | Z
  deriving (Show, Eq, Ord)

type LNode = (LType, String)

type LTree = Tree LNode

data LTreeCxt
  = Hole
  | TreeHole LNode LTreeCxt [LTree]

plug :: LTreeCxt -> LTree -> LTree
plug Hole t = t
plug (TreeHole k c t2) t = plug c (Node k (subForest t ++ t2))

type LZip = (LTreeCxt, LTree)

goDown :: LZip -> String -> Maybe LZip
goDown (c, Node k f) s =
  let g = find (\a -> snd (rootLabel a) == s) f
   in case g of
        Nothing -> Nothing
        Just subtree -> Just (TreeHole k c (delete subtree f), subtree)

goUp :: LZip -> Maybe LZip
goUp (TreeHole k c t, out) = Just (c, Node k (out : t))
goUp (Hole, _) = Nothing