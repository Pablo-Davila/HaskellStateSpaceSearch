

data Solution e v = Null | Sol e v
    deriving (Show, Eq)

nullSolucion Null = True
nullSolucion _ = False

estadoSolucion (Sol e _) = e
estadoSolucion _ = error "La solución nula no tiene estado"

valorSolucion (Sol _ v) = v
valorSolucion _ = error "La solución nula no tiene valor"


-- Algoritmo genérico de backtracking --

minimoBacktracking :: Ord v => (e -> a -> v) -> (e -> [a]) -> (e -> v) -> (e -> Bool) -> (e -> a -> e) -> e -> Solution e v -> Solution e v
minimoBacktracking heuristica acciones fOpt esCasoBase avanza e@estado sol
    | esCasoBase e = elige sol (Sol e (fOpt e))
    | otherwise = aux accionesFiltradas
    where
        minBT = minimoBacktracking heuristica acciones fOpt esCasoBase avanza
        accionesFiltradas
            | nullSolucion sol = acciones e
            | otherwise = [a | a <- acciones e, heuristica e a < valorSolucion sol]
        elige sol1 sol2
            | nullSolucion sol1 = sol2
            | nullSolucion sol2 = sol1
            | valorSolucion sol2 < valorSolucion sol1 = sol2
            | otherwise = sol1
        aux [] = Null
        aux (a:as) = elige (minBT (avanza e a) sol) (aux as)


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

res = minimoBacktracking heuristicaMochila genAccionesMochila fOptMochila esEstadoFinalMochila aplicarAccionMochila (0,0,capacidadInicial) (Null)
