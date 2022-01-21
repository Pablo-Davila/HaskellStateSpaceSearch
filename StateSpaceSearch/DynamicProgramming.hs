
module StateSpaceSearch.DynamicProgramming (
    dynamicpMin,
    dynamicpRedMin,
    reconstructSolState
) where


-- Dynamic programming generic algorithms --

-- Fully generic dynamic programming algorithm
dynamicpMin :: (Ord v) => e -> (e -> [a]) -> (e -> a -> [e]) -> (e -> v) -> (e -> Bool) -> ([v] -> v) -> v
dynamicpMin e@estado generarAcciones aplicarAccion fOpt esEstadoFinal solucionParcial
    | esEstadoFinal e = fOpt e
    | otherwise = minimum ls
    where
        resSubproblemas es = [dynamicpMin e2 generarAcciones aplicarAccion fOpt esEstadoFinal solucionParcial | e2<-es]
        next a = solucionParcial $ resSubproblemas $ aplicarAccion e a
        ls = fOpt e:[next a | a<-generarAcciones e]

-- Dynamic programming algorithm with reduction
dynamicpRedMin :: (Ord v) => e -> (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> ([a], v)
dynamicpRedMin e@estado generarAcciones aplicarAccion fOpt esEstadoFinal
    | esEstadoFinal e = ([], fOpt e)
    | otherwise = customMinimumBy snd ls
    where
        next a = dynamicpRedMin (aplicarAccion e a) generarAcciones aplicarAccion fOpt esEstadoFinal
        addAction a (as,v) = (a:as, v)
        ls = ([], fOpt e):[
                addAction a (next a)
                | a<-generarAcciones e
            ]

-- Function used to reconstruct the final solution state in a reduction problem applying a sequence of actions
reconstructSolState :: (e -> a -> e) -> (e -> Bool) -> e -> [a] -> e
reconstructSolState aplicarAccion esEstadoFinal e@estado acciones
    | esEstadoFinal e = e
    | otherwise = reconstructSolState aplicarAccion esEstadoFinal (aplicarAccion e (head acciones)) (tail acciones)


-- Utils --

customMinimumBy :: (Ord v) => (a -> v) -> [a] -> a
customMinimumBy _ [] = error "Empty list"
customMinimumBy _ [x] = x
customMinimumBy f (x:xs)
    | f x < f nexti = x
    | otherwise = nexti
    where nexti = customMinimumBy f xs

