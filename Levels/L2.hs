module Levels.L2 (t) where

import Data.Tree
import Level

t :: LTree
t =
  Node
    (S, "N1")
    [ Node
        (E, "N2")
        [ Node
            (W, "N4")
            [ Node
                (F, "N5")
                []
            ]
        ],
      Node
        (E, "N3")
        [Node (B, "N6") []]
    ]