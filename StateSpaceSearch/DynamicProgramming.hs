
module StateSpaceSearch.DynamicProgramming (
    dynamicpMin,
    dynamicpMax,
    dynamicpRedMin,
    dynamicpRedMax,
    reconstructSolState
) where

import Pila


-- Dynamic programming generic algorithms --

-- Fully generic dynamic programming algorithm
dynamicpMin :: (Ord v) => e -> (e -> [a]) -> (e -> a -> [e]) -> (e -> v) -> (e -> Bool) -> (e -> a -> [v] -> v) -> v
dynamicpMin e@state genActions applyAction optFunc isFinalState partialSolution
    | isFinalState e = optFunc e
    | otherwise = minimum ls
    where
        subproblemSols es = [dynamicpMin e2 genActions applyAction optFunc isFinalState partialSolution | e2<-es]
        next a = partialSolution e a (subproblemSols $ applyAction e a)
        ls = [next a | a<-genActions e]

dynamicpMax :: (Num v, Ord v) => e -> (e -> [a]) -> (e -> a -> [e]) -> (e -> v) -> (e -> Bool) -> (e -> a -> [v] -> v) -> v
dynamicpMax state genActions applyAction optFunc isFinalState partialSolution = (
    -dynamicpMin state genActions applyAction (\e -> -optFunc e) isFinalState partialSolution)

-- Dynamic programming algorithm with reduction
dynamicpRedMin :: (Ord v) => e -> (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (Pila a, v)
dynamicpRedMin e@state genActions applyAction optFunc isFinalState
    | isFinalState e = (vacia, optFunc e)
    | otherwise = customMinimumBy snd ls
    where
        next a = dynamicpRedMin (applyAction e a) genActions applyAction optFunc isFinalState
        addAction a (as,v) = (apila a as, v)
        ls = [
                addAction a (next a)
                | a<-genActions e
            ]

dynamicpRedMax :: (Num v, Ord v) => e -> (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (Pila a, v)
dynamicpRedMax state genActions applyAction optFunc isFinalState = (as, -v)
    where (as, v) = dynamicpRedMin state genActions applyAction (\e -> -optFunc e) isFinalState

-- Function used to reconstruct the final solution state in a reduction problem applying a sequence of actions
reconstructSolState :: (e -> a -> e) -> e -> Pila a -> e
reconstructSolState applyAction e@state as@acciones
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

