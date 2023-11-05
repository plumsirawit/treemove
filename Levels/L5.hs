module Levels.L5 (t) where

import Data.Tree
import Level

t :: LTree
t =
  Node
    (Z, "N1")
    [ Node
        (E, "N2")
        [ Node
            (Key 'A', "N5")
            [Node (B, "N8") [Node (Gate 'B', "N11") [Node (F, "N13") []]]]
        ],
      Node
        (S, "N3")
        [ Node
            (E, "N6")
            [ Node
                (W, "N9")
                [ Node (Z, "N10") [],
                  Node (Gate 'A', "N12") [Node (Key 'B', "N14") []]
                ]
            ]
        ],
      Node
        (E, "N4")
        [Node (Z, "N7") []]
    ]