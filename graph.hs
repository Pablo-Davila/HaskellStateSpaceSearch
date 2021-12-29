
-- Data types --

-- (id, tag)
type Vertex a = (Int, a)

-- (tag, id1, id2)
type Edge a = (a, Int, Int)

-- ([vertices], [edges])
type Graph a b = ([Vertex a], [Edge b])


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
-}


-- Vertex helper functions --

vertexId :: Vertex a -> Int
vertexId v = fst v

vertexTag :: Vertex a -> a
vertexTag v = snd v


-- Edge helper functions --

edgeTag :: Edge b -> b
edgeTag (t,_,_) = t


-- Graph helper functions --

vertices :: Graph a b -> [Vertex a]
vertices g = fst g 

edges :: Graph a b -> [Edge b]
edges g = snd g

(-*) :: (Eq a, Eq b) => Graph a b -> Graph a b -> Graph a b
g1 -* g2 = (vs, es)
    where
        vs = [v | v<-vertices g1, not (elem v (vertices g2))]
        es = [e | e<-edges g1, not (elem e (edges g2))]

(+*) :: (Eq a, Eq b) => Graph a b -> Graph a b -> Graph a b
g1 +* g2 = (vs, es)
    where
        vs = vertices g2 ++ [v | v<-vertices g1, not (elem v (vertices g2))]
        es = edges g2 ++ [e | e<-edges g1, not (elem e (edges g2))]

-- For vertices
vertexEdges :: Graph a b -> Vertex a -> [Edge b]
vertexEdges g v = undefined

inEdges :: Graph a b -> Vertex a -> [Edge b]
inEdges = undefined

outEdges :: Graph a b -> Vertex a -> [Edge b]
outEdges = undefined

vertexFromId :: Graph a b -> Int -> Vertex a
vertexFromId g i = aux i vs
    where
        vs = vertices g
        aux _ [] = error $ "Vertex with id " ++ show i ++ " not found"
        aux i (v:vs)
            | vertexId v==i = v
            | otherwise = aux i vs

-- For edges
edgeSource :: Graph a b -> Edge b -> Vertex a
edgeSource g (_,i,_) = vertexFromId g i

edgeTarget :: Graph a b -> Edge b -> Vertex a
edgeTarget g (_,_,i) = vertexFromId g i


-- Algorithms --

isConnected :: Graph a b -> Bool
isConnected g = undefined
-- Comprobar si el grafo es conexo

connectedComponents :: Graph a b -> [Graph a b]
connectedComponents g = undefined
-- Obtener las componentes conexas del grafo

edgeCover :: Graph a b -> [Edge b]
edgeCover g = undefined
-- Buscar un recubrimiento mínimo por aristas (algoritmo de Kruskal)

vertexCover :: Graph a b -> [Vertex a]
vertexCover g = undefined
-- Buscar un recubrimiento mínimo por vértices

shortestPath :: Graph a b -> Vertex a -> Vertex b -> [Edge b]
shortestPath g v1 v2 = undefined
-- Buscar el camino mínimo (algoritmo de Dijkstra)
