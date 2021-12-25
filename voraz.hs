
-- Algoritmo genérico de búsqueda voraz --

customMinimumBy :: (Ord v) => (a -> v) -> [a] -> a
customMinimumBy _ [] = error "Empty list"
customMinimumBy _ [x] = x
customMinimumBy f (x:xs)
    | f x < f nexti = x
    | otherwise = nexti
    where nexti = customMinimumBy f xs

minimoVoraz :: (Ord v, Ord c) => (e -> [a]) -> (e -> a -> e) -> (e -> v) -> (e -> Bool) -> (e -> a -> c) -> e -> ([a], v)
minimoVoraz generarAcciones aplicarAccion fOpt esEstadoFinal heuristica e@estado
    | esEstadoFinal e = ([], fOpt estado)
    | otherwise = (mejorAccion:fst resto, snd(resto))
    where
        minV = minimoVoraz generarAcciones aplicarAccion fOpt esEstadoFinal heuristica
        acciones = generarAcciones estado
        mejorAccion = customMinimumBy (heuristica e) acciones
        resto = minV $ aplicarAccion estado mejorAccion

reconstruyeSolucion :: (e -> a -> e) -> (e -> Bool) -> e -> [a] -> e
reconstruyeSolucion aplicarAccion esEstadoFinal e@estado acciones
    | esEstadoFinal e = e
    | otherwise = reconstruyeSolucion aplicarAccion esEstadoFinal (aplicarAccion e (head acciones)) (tail acciones)


-- Problema de la mochila --

type EstadoMochila = (Int,Int,Int)  -- Índice, valor acumulado, peso restante
type AccionMochila = Int            -- Cantidad de objetos que tomar
type DatosObjeto = (Int,Int,Int)    -- Valor, peso y unidades disponibles

objetos = [(3,9,2), (4,8,2), (2,6,1)]
capacidadInicial = 25

esEstadoFinalMochila :: EstadoMochila -> Bool
esEstadoFinalMochila (i,_,_) = i == length objetos

genAccionesMochila :: EstadoMochila -> [AccionMochila]
genAccionesMochila (i,v,p) = [0 .. min mi (div p pi)]
    where (vi,pi,mi) = objetos !! i

fOptMochila :: EstadoMochila -> Int
fOptMochila (_,v,_) = -v

aplicarAccionMochila :: EstadoMochila -> AccionMochila -> EstadoMochila
aplicarAccionMochila (i,v,p) a = (i+1, v+vi*a, p-pi*a)
    where (vi,pi,_) = objetos !! i

heuristicaMochila _ _ = -(1/0) -- -Infinity

resultadoMochila = minimoVoraz genAccionesMochila aplicarAccionMochila fOptMochila esEstadoFinalMochila heuristicaMochila (0,0,capacidadInicial)

solucionMochila = reconstruyeSolucion aplicarAccionMochila esEstadoFinalMochila (0,0,capacidadInicial) (fst resultadoMochila)
