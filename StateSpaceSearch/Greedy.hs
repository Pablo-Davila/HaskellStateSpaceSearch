
module StateSpaceSearch.Greedy (
    greedyMin,
    greedyMax,
    reconstructSolState
) where

import Pila


-- Greedy search dynamic algorithm --

greedyMin :: (Ord v, Ord c) => (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (e -> a -> c) -> e -> (Pila a, v)
greedyMin generarAcciones aplicarAccion fOpt esEstadoFinal heuristica e@estado
    | esEstadoFinal e = (vacia, fOpt e)
    | otherwise = (apila mejorAccion as, v)
    where
        acciones = generarAcciones estado
        mejorAccion = customMinimumBy (heuristica e) acciones
        (as,v) = greedyMin generarAcciones aplicarAccion fOpt esEstadoFinal heuristica (aplicarAccion estado mejorAccion)

greedyMax :: (Num v, Ord v, Num c, Ord c) => (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (e -> a -> c) -> e -> (Pila a, v)
greedyMax generarAcciones aplicarAccion fOpt esEstadoFinal heuristica estado = (as, -v)
    where (as, v) = greedyMin generarAcciones aplicarAccion (\e -> -fOpt e) esEstadoFinal (\e a -> -heuristica e a) estado

-- Function used to reconstruct the final solution state applying a sequence of actions
reconstructSolState :: (e -> a -> e) -> e -> Pila a -> e
reconstructSolState aplicarAccion e@estado as@acciones
    | esVacia as = e
    | otherwise = reconstructSolState aplicarAccion (aplicarAccion e (cima as)) (desapila as)


-- Utils --

customMinimumBy :: (Ord v) => (a -> v) -> [a] -> a
customMinimumBy _ [] = error "Empty list"
customMinimumBy _ [x] = x
customMinimumBy f (x:xs)
    | f x < f nexti = x
    | otherwise = nexti
    where nexti = customMinimumBy f xs
