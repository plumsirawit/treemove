module Levels.L3 (t) where

import Data.Tree
import Level

t :: LTree
t =
  Node
    (E, "N1")
    [ Node
        (E, "N2")
        [ Node
            (S, "N4")
            [ Node (E, "N6") []
            ],
          Node
            (W, "N5")
            [ Node
                (W, "N7")
                [ Node
                    (W, "N8")
                    [ Node (F, "N9") []
                    ]
                ]
            ]
        ],
      Node
        (E, "N3")
        [ Node
            (E, "N10")
            [ Node (B, "N11") []
            ],
          Node
            (E, "N12")
            [ Node
                (W, "N13")
                [ Node (B, "N14") [],
                  Node (B, "N15") []
                ]
            ]
        ]
    ]

-- to try: putStrLn $ drawLTree $ unzipToTree $ markCurrentPosition $ fromJust (goDown (Hole, t1) "N2")