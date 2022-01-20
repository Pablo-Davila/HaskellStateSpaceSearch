
module Graph.TreeCover (
    treeCover,
    minTreeCover,
    minForestCover
) where

import Data.List (sortBy)
import Graph
import Graph.Connection


-- Tree cover algorithms --

-- Obtener un árbol recubridor
treeCover :: (Eq a) => Graph a b -> Graph a b
treeCover g = aux (vertices g) ([head (vertices g)],[])
    where
        aux [] acc = acc 
        aux (v:vs) (vacc,eacc) = aux vs (vacc++vIn++vOut, eacc++eIn++eOut)
            where
                eIn = [e | e<-inEdges g v, not (elem (edgeSource g e) vacc)]
                vIn = [edgeSource g e | e<-eIn]
                eOut = [e | e<-outEdges g v, not (elem (edgeTarget g e) (vacc++vIn))]
                vOut = [edgeTarget g e | e<-eOut]

-- Buscar un árbol recubridor de peso mínimo en un grafo conexo (algoritmo de Kruskal)
minTreeCover :: (Eq a, Eq b, Ord b) => Graph a b -> Graph a b
minTreeCover g
    | isConnected g = (vertices g, treeEdges)
    | otherwise = error "The graph must be connected"
    where
        ids = map vertexId (vertices g)
        treeEdges = aux (zip ids ids) (customSortBy edgeTag (edges g))
        aux :: [(Int,Int)] -> [Edge b] -> [Edge b]
        aux vs [] = []
        aux vs (e:es)
            | vertexTag v1 == vertexTag v2 = aux vs es
            | otherwise = e:aux vsNew es
            where
                v1 = edgeSource (vs,[e]) e
                v2 = edgeTarget (vs,[e]) e
                vsNew = [(id, if cc==vertexTag v2 then vertexTag v1 else cc) | (id,cc)<-vs]

-- Buscar un bosque recubridor de peso mínimo
minForestCover :: (Eq a, Eq b, Ord b) => Graph a b -> [Graph a b]
minForestCover g = [minTreeCover h | h<-connectedComponents g]


-- Utils --

customSortBy :: (Ord o) => (a -> o) -> [a] -> [a]
customSortBy f xs = sortBy (\x y -> compare (f x) (f y)) xs
