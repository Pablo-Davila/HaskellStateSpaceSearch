
module Graph.Samples (
    g1, g2, g3, g4,
    v0, v1, v2, v3, v4, v5,
    drawG1, drawG2, drawG3, drawG4
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
            (1.5, 5, 8),
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

[v0,v1,v2,v3,v4] = vertices g1
v5 = head $ vertices g2


drawG1 :: IO ()
drawingG1 = do
    putStrLn (
        "Drawing g1:\n"
        ++ "v0 -> v1\n"
        ++ "    _^ |\n"
        ++ "  _/   v\n"
        ++ "v3 <- v2 -> v4")

drawG2 :: IO ()
drawingG2 = do
    putStrLn (
        "Drawing g2:\n"
        ++ "v5 -> v6\n"
        ++ " |  _^ ^\n"
        ++ " v_/   |\n"
        ++ "v8 <- v7 -> v9")

drawG3 :: IO ()
drawingG3 = do
    putStrLn (
        "Drawing g3:\n"
        ++ "v0 -> v1            v5 -> v6\n"
        ++ "    _^ |             |  _^ ^\n"
        ++ "  _/   v             v_/   |\n"
        ++ "v3 <- v2 -> v4      v8 <- v7 -> v9")

drawG4 :: IO ()
drawingG4 = do
    putStrLn (
        "Drawing g4:\n"
        ++ "v0 -> v1\n"
        ++ "  ^_   |\n"
        ++ "    \\_ v\n"
        ++ "      v2")
