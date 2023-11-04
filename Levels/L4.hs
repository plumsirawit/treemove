module Levels.L4 (t) where

import Data.Tree
import Level

t :: LTree
t =
  Node
    (S, "N1")
    [ Node
        (Gate 'A', "N2")
        [ Node
            (Gate 'B', "N5")
            [ Node (F, "N9") []
            ],
          Node (Z, "N6") []
        ],
      Node
        (E, "N3")
        [ Node (Key 'A', "N7") [],
          Node (Key 'B', "N8") []
        ],
      Node (E, "N4") []
    ]