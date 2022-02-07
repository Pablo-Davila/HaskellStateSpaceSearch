
module StateSpaceSearch.Greedy (
    greedyMin,
    greedyMax,
    reconstructSolState
) where

import Pila


-- Greedy search dynamic algorithm --

greedyMin :: (Ord v, Ord c) => (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (e -> a -> c) -> e -> (Pila a, v)
greedyMin genActions applyAction optFunc isFinalState heuristic e@state
    | isFinalState e = (vacia, optFunc e)
    | otherwise = (apila bestAction as, v)
    where
        actions = genActions state
        bestAction = customMinimumBy (heuristic e) actions
        (as,v) = greedyMin genActions applyAction optFunc isFinalState heuristic (applyAction state bestAction)

greedyMax :: (Num v, Ord v, Num c, Ord c) => (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (e -> a -> c) -> e -> (Pila a, v)
greedyMax genActions applyAction optFunc isFinalState heuristic state = (as, -v)
    where (as, v) = greedyMin genActions applyAction (\e -> -optFunc e) isFinalState (\e a -> -heuristic e a) state

-- Function used to reconstruct the final solution state applying a sequence of actions
reconstructSolState :: (e -> a -> e) -> e -> Pila a -> e
reconstructSolState applyAction e@state as@actions
    | esVacia as = e
    | otherwise = reconstructSolState applyAction (applyAction e (cima as)) (desapila as)


-- Utils --

customMinimumBy :: (Ord v) => (a -> v) -> [a] -> a
customMinimumBy _ [] = error "Empty list"
customMinimumBy _ [x] = x
customMinimumBy f (x:xs)
    | f x < f nexti = x
    | otherwise = nexti
    where nexti = customMinimumBy f xs
