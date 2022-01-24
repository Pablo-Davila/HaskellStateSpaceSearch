
module Graph (
    Vertex,
    Edge,
    Graph,
    vertex,
    vertexId,
    vertexTag,
    edge,
    edgeTag,
    vertices,
    edges,
    graph,
    addVertex,
    addEdge,
    (-*),
    (+*),
    vertexEdges,
    inEdges,
    outEdges,
    vertexFromId,
    neighbours,
    edgeSource,
    edgeTarget,
    edgeVertices,
    checkGraphIds
 ) where

import Data.List (nub)


-- Data types --

-- (id, tag)
type Vertex a = (Int, a)

-- (tag, id1, id2)
type Edge a = (a, Int, Int)

-- ([vertices], [edges])
type Graph a b = ([Vertex a], [Edge b])


-- Vertex helper functions --

vertex :: Int -> a -> Vertex a
vertex i t = (i,t)

vertexId :: Vertex a -> Int
vertexId v = fst v

vertexTag :: Vertex a -> a
vertexTag v = snd v


-- Edge helper functions --

edge :: b -> Int -> Int -> Edge b
edge t i1 i2 = (t,i1,i2)

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
    where g = (vs, es)

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

checkGraphIds :: Graph a b -> Bool
checkGraphIds g = checkVertices && checkEdgesSource && checkEdgesTarget
    where
        ids = map vertexId (vertices g)
        checkVertices = length ids == length (nub ids)
        es = edges g
        checkEdgesSource = all (\id -> elem id ids) [id | (_,id,_)<-es]
        checkEdgesTarget = all (\id -> elem id ids) [id | (_,_,id)<-es]
