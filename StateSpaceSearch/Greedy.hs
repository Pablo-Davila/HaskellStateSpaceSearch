
module StateSpaceSearch.Greedy (
    greedyMin,
    reconstructSolState
) where


-- Greedy search dynamic algorithm --

greedyMin :: (Ord v, Ord c) => (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (e -> a -> c) -> e -> ([a], v)
greedyMin generarAcciones aplicarAccion fOpt esEstadoFinal heuristica e@estado
    | esEstadoFinal e = ([], fOpt estado)
    | otherwise = (mejorAccion:fst resto, snd(resto))
    where
        minV = greedyMin generarAcciones aplicarAccion fOpt esEstadoFinal heuristica
        acciones = generarAcciones estado
        mejorAccion = customMinimumBy (heuristica e) acciones
        resto = minV $ aplicarAccion estado mejorAccion

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
