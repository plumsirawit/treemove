module Levels.L1 (t) where

import Data.Tree
import Level

t :: LTree
t =
  Node
    (S, "N1")
    [ Node
        (E, "N2")
        [ Node
            (F, "N4")
            []
        ],
      Node
        (E, "N3")
        []
    ]