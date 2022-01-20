
module Graph.Samples (
    g1, g2, g3, g4,
    v0, v5
) where

import Graph


-- Graph samples --

g1 :: (Graph String Float)
g1 = (
        [ -- Vertices
            (0, "Vertex 0"),
            (1, "Vertex 1"),
            (2, "Vertex 2"),
            (3, "Vertex 3"),
            (4, "Vertex 4")
        ],
        [ -- Edges
            (0.5, 0, 1),
            (2.2, 1, 2),
            (1.3, 2, 3),
            (3.7, 3, 1),
            (3.2, 2, 4)
        ]
    )

g2 :: (Graph String Float)
g2 = (
        [ -- Vertices
            (5, "Vertex 5"),
            (6, "Vertex 6"),
            (7, "Vertex 7"),
            (8, "Vertex 8"),
            (9, "Vertex 9")
        ],
        [ -- Edges
            (0.5, 5, 6),
            (2.2, 7, 6),
            (1.3, 7, 8),
            (3.7, 8, 6),
            (3.2, 7, 9)
        ]
    )

g3 :: (Graph String Float)
g3 = g1 +* g2

g4 :: (Graph String Float)
g4 = (
        [ -- Vertices
            (0, "Vertex 0"),
            (1, "Vertex 1"),
            (2, "Vertex 2")
        ],
        [ -- Edges
            (0.5, 0, 1),
            (2.2, 1, 2),
            (1.3, 2, 0)
        ]
    )

v0 = head $ fst g1
v5 = head $ fst g2


{-
Drawing g1:
 v0 -> v1
     _^ |
   _/   v
 v3 <- v2 -> v4

Drawing g2:
 v5 -> v6
     _^ ^
   _/   |
 v8 <- v7 -> v9

Drawing g3:
 v0 -> v1            v5 -> v6
     _^ |                _^ ^
   _/   v              _/   |
 v3 <- v2 -> v4      v8 <- v7 -> v9

Drawing g4:
 v0 -> v1
   ^_   |
     \_ v
       v2
-}
