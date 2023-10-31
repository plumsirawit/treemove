module Level where

import Data.Tree

data Node = B | W | Key Char | Gate Char | Z
  deriving (Show, Eq, Ord)
 
newtype Level = Tree Node
  deriving (Show, Eq, Ord)

