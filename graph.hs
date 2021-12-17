
-- Data types --

type Vertex a = (Int, a)

type Edge a = (a, Int, Int)

type Graph a b = ([Vertex a], [Edge b])


-- Graph samples --

g1 :: (Graph String Float)
g1 = (
        [ -- Vertices
            (0, "Vertex 0"),
            (1, "Vertex 1"),
            (2, "Vertex 2"),
            (3, "Vertex 3")
        ],
        [ -- Edges
            (0.5, 0, 1),
            (2.2, 1, 2),
            (1.3, 2, 3),
            (3.7, 3, 0)
        ]
    )

{- Dibujo g1:

 v0 -> v1
     _^ |
   _/   v
 v3 <- v2
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
