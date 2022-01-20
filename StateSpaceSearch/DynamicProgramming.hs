
module StateSpaceSearch.DynamicProgramming (
    dynamicpMin,
    reconstructSolState
) where


-- Dynamic programming generic algorithm --

dynamicpMin :: (Ord v) => e -> (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> ([a], v)
dynamicpMin e@estado generarAcciones aplicarAccion f_opt esEstadoFinal
    | esEstadoFinal e = ([], f_opt e)
    | otherwise = customMinimumBy snd ls
    where
        next ac = dynamicpMin (aplicarAccion e ac) generarAcciones aplicarAccion f_opt esEstadoFinal
        ls = ([], f_opt e):[
                (a:(fst (next a)), snd (next a))
                | a<-generarAcciones e
            ]

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

