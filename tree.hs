
data Tree a = Node a [Tree a]
    deriving (Show, Read, Eq)


-- Utility Functions --

value :: (Show a, Read a, Eq a) => Tree a -> a
value (Node v _) = v

children :: (Show a, Read a, Eq a) => Tree a -> [Tree a]
children (Node _ ts) = ts

firstChild :: (Show a, Read a, Eq a) => Tree a -> Tree a
firstChild (Node _ ts) = head ts


-- Tree samples --

b1 = Node 0 [
        (Node 1 []),
        (Node 2 [
            (Node 3 []),
            (Node 4 [])
        ])
    ]


-- Algorithms --

customMinimumBy _ [] = error "Empty list"
customMinimumBy _ [x] = x
customMinimumBy f (x:xs)
    | f x < f nexti = x
    | otherwise = nexti
    where nexti = customMinimumBy f xs

searchDFS :: (Show a, Read a, Eq a, Ord b) => (a -> b) -> Tree a -> a
searchDFS f (Node v ts) = customMinimumBy f resto
    where
        resto = v:[searchDFS f t | t<-ts]

searchBFS :: (Show a, Read a, Eq a, Ord b) => (a -> b) -> Tree a -> a
searchBFS f n = searchBFSAux f [n]

searchBFSAux :: (Show a, Read a, Eq a, Ord b) => (a -> b) -> [Tree a] -> a
searchBFSAux _ [] = error "Empty tree"
searchBFSAux f (n:ns)
    | f v < f rest = v
    | otherwise = rest
    where
        v = value n
        ls = ns ++ children n
        rest = if null ls then v else searchBFSAux f ls
