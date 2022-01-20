
import Data.List (sortBy)

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

graph :: [Vertex a] -> [Edge b] -> Graph a b
graph vs es
    | checkGraphIds g = g
    | otherwise = error "Incompatible graph arguments"
    where g = graph vs es

addVertex :: Graph a b -> Vertex a -> Graph a b
addVertex g v = (v:vertices g, edges g)

addEdge :: Graph a b -> Edge b -> Graph a b
addEdge g e = (vertices g, e:edges g)

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

vertexEdges :: Graph a b -> Vertex a -> [Edge b]
vertexEdges g v = inEdges g v ++ outEdges g v

inEdges :: Graph a b -> Vertex a -> [Edge b]
inEdges g v = [e | e<-edges g, vertexId v==vertexId (edgeTarget g e)]

outEdges :: Graph a b -> Vertex a -> [Edge b]
outEdges g v = [e | e<-edges g, vertexId v==vertexId (edgeSource g e)]

vertexFromId :: Graph a b -> Int -> Vertex a
vertexFromId g i = aux i vs
    where
        vs = vertices g
        aux _ [] = error $ "Vertex with id " ++ show i ++ " not found"
        aux i (v:vs)
            | vertexId v==i = v
            | otherwise = aux i vs

neighbours :: Graph a b -> Vertex a -> [Vertex a]
neighbours g v = [aux (edgeVertices g e) | e<-es]
    where
        es = vertexEdges g v
        aux [v1,v2]
            | vertexId v1==vertexId v = v2
            | otherwise = v1

edgeSource :: Graph a b -> Edge b -> Vertex a
edgeSource g (_,i,_) = vertexFromId g i

edgeTarget :: Graph a b -> Edge b -> Vertex a
edgeTarget g (_,_,i) = vertexFromId g i

edgeVertices :: Graph a b -> Edge b -> [Vertex a]
edgeVertices g e = [edgeSource g e, edgeTarget g e]

-- Property
checkGraphIds :: Graph a b -> Bool
checkGraphIds g = checkVertices ids && checkEdgesSource && checkEdgesTarget
    where
        ids = map vertexId (vertices g)
        checkVertices [] = True
        checkVertices (id:ids) = not (elem id ids) && checkVertices ids
        es = edges g
        checkEdgesSource = all (\id -> elem id ids) [id | (_,id,_)<-es]
        checkEdgesTarget = all (\id -> elem id ids) [id | (_,_,id)<-es]


-- Connetion algorithms --

-- Obtener la componente conexa de un vértice del grafo
connectedComponent :: (Eq a, Eq b) => Graph a b -> Vertex a -> Graph a b
connectedComponent g v = connectedComponentAux g [v] ([v],[])

connectedComponentAux :: (Eq a, Eq b) => Graph a b -> [Vertex a] -> Graph a b -> Graph a b
connectedComponentAux _ [] acc = acc
connectedComponentAux g (v:vs) acc = connectedComponentAux g (vs++vs2) (vertices acc ++ vs2, edges acc ++ es)
    where
        es = filter (\e -> not (elem e (edges acc))) (vertexEdges g v)
        vs2 = filter
            (\v -> not (elem v (vertices acc)) && not (elem v vs))
            (neighbours g v)

-- Obtener las componentes conexas del grafo
connectedComponents :: (Eq a, Eq b) => Graph a b -> [Graph a b]
connectedComponents ([],[]) = []
connectedComponents g = g1:connectedComponents (g -* g1)
    where
        v = head $ vertices g
        g1 = connectedComponent g v

-- Obtener la componente fuertemente conexa de un vértice del grafo
stronglyConnectedComponent :: (Eq a, Eq b) => Graph a b -> Vertex a -> Graph a b
stronglyConnectedComponent g v = stronglyConnectedComponentAux g [v] ([v],[])

stronglyConnectedComponentAux :: (Eq a, Eq b) => Graph a b -> [Vertex a] -> Graph a b -> Graph a b
stronglyConnectedComponentAux _ [] acc = acc
stronglyConnectedComponentAux g (v:vs) acc = stronglyConnectedComponentAux g (vs++vs2) (vertices acc ++ vs2, edges acc ++ es)
    where
        es = filter (\e -> not (elem e (edges acc))) (outEdges g v)
        vs2 = filter
            (\v -> not (elem v (vertices acc)) && not (elem v vs))
            [edgeTarget g e | e<-es]

stronglyConnectedComponents :: (Eq a, Eq b) => Graph a b -> [Graph a b]
stronglyConnectedComponents g = map (stronglyConnectedComponent g) (vertices g)

-- Comprobar si el grafo es conexo
isConnected :: (Eq a, Eq b) => Graph a b -> Bool
isConnected g = length (vertices g) == length (vertices (connectedComponent g (head $ vertices g)))

-- Comprobar si el grafo es fuertemente conexo
isStronglyConnected :: (Eq a, Eq b) => Graph a b -> Bool
isStronglyConnected g = all aux (vertices g)
    where
        n = length (vertices g)
        aux v = n == length (vertices (stronglyConnectedComponent g v))


-- Other algorithms --

-- Buscar un recubrimiento mínimo por aristas (algoritmo de Kruskal)
edgeCover :: Graph a b -> [Edge b]
edgeCover g = undefined

-- Buscar un recubrimiento mínimo por vértices
vertexCover :: Graph a b -> [Vertex a]
vertexCover g = undefined

-- Buscar el camino mínimo (algoritmo de Dijkstra)
shortestPath :: Graph a b -> Vertex a -> Vertex b -> [Edge b]
shortestPath g v1 v2 = undefined
