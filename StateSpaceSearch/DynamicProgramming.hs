
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
dynamicpMin e@estado generarAcciones aplicarAccion fOpt esEstadoFinal solucionParcial
    | esEstadoFinal e = fOpt e
    | otherwise = minimum ls
    where
        resSubproblemas es = [dynamicpMin e2 generarAcciones aplicarAccion fOpt esEstadoFinal solucionParcial | e2<-es]
        next a = solucionParcial e a (resSubproblemas $ aplicarAccion e a)
        ls = [next a | a<-generarAcciones e]

dynamicpMax :: (Num v, Ord v) => e -> (e -> [a]) -> (e -> a -> [e]) -> (e -> v) -> (e -> Bool) -> (e -> a -> [v] -> v) -> v
dynamicpMax estado generarAcciones aplicarAccion fOpt esEstadoFinal solucionParcial = (
    -dynamicpMin estado generarAcciones aplicarAccion (\e -> -fOpt e) esEstadoFinal solucionParcial)

-- Dynamic programming algorithm with reduction
dynamicpRedMin :: (Ord v) => e -> (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (Pila a, v)
dynamicpRedMin e@estado generarAcciones aplicarAccion fOpt esEstadoFinal
    | esEstadoFinal e = (vacia, fOpt e)
    | otherwise = customMinimumBy snd ls
    where
        next a = dynamicpRedMin (aplicarAccion e a) generarAcciones aplicarAccion fOpt esEstadoFinal
        addAction a (as,v) = (apila a as, v)
        ls = [
                addAction a (next a)
                | a<-generarAcciones e
            ]

dynamicpRedMax :: (Num v, Ord v) => e -> (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (Pila a, v)
dynamicpRedMax estado generarAcciones aplicarAccion fOpt esEstadoFinal = (as, -v)
    where (as, v) = dynamicpRedMin estado generarAcciones aplicarAccion (\e -> -fOpt e) esEstadoFinal

-- Function used to reconstruct the final solution state in a reduction problem applying a sequence of actions
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

