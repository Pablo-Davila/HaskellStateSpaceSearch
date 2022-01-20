
module Graph.Connection (
    connectedComponent,
    connectedComponents,
    stronglyConnectedComponent,
    stronglyConnectedComponents,
    isConnected,
    isStronglyConnected
) where

import Graph


-- Connection algorithms --

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
