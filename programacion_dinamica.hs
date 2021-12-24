
-- Algoritmo genérico de programación dinámica --

customMinimumBy :: (Ord b) => (a -> b) -> [a] -> a
customMinimumBy _ [] = error "Empty list"
customMinimumBy _ [x] = x
customMinimumBy f (x:xs)
    | f x < f nexti = x
    | otherwise = nexti
    where nexti = customMinimumBy f xs

minimoPD :: (Ord b) => e -> (e -> [a]) -> (e -> a -> e) -> (e -> b) -> (e -> Bool) -> ([a], b)
minimoPD e@estado generarAcciones aplicarAccion f_opt esEstadoFinal
    | esEstadoFinal e = ([], f_opt e)
    | otherwise = customMinimumBy snd ls
    where
        next ac = minimoPD (aplicarAccion e ac) generarAcciones aplicarAccion f_opt esEstadoFinal
        ls = ([], f_opt e):[
                (a:(fst (next a)), snd (next a))
                | a<-generarAcciones e
            ]

reconstruyeSolucionPD :: (e -> a -> e) -> (e -> Bool) -> e -> [a] -> e
reconstruyeSolucionPD aplicarAccion esEstadoFinal e@estado acciones
    | esEstadoFinal e = e
    | otherwise = reconstruyeSolucionPD aplicarAccion esEstadoFinal (aplicarAccion e (head acciones)) (tail acciones)


-- Problema de las jarras --

type EstadoJarras = (Int,Int)

data AccionJarras = LlenarA | LlenarB | VaciarA | VaciarB | EcharAB | EcharBA
    deriving (Show, Read, Eq)

maxA = 3
maxB = 5

genAccionesJarras :: EstadoJarras -> [AccionJarras]
genAccionesJarras (x,y) = []
    ++ (if x<maxA then [LlenarA] else [])
    ++ (if y<maxB then [LlenarB] else [])
    ++ (if x>0 then [VaciarA] else [])
    ++ (if y>0 then [VaciarB] else [])
    ++ (if x>0 && y<maxB then [EcharAB] else [])
    ++ (if y>0 && x<maxA then [EcharBA] else [])

aplicarAccionJarras :: EstadoJarras -> AccionJarras -> EstadoJarras
aplicarAccionJarras (x,y) a
    | a == LlenarA = (maxA,y)
    | a == LlenarB = (x,maxB)
    | a == VaciarA = (0,y)
    | a == VaciarB = (x,0)
    | a == EcharAB = (x'AB, y'AB)
    | a == EcharBA = (x'BA, y'BA)
    where
        x'AB = max 0 (x+y-maxB)
        y'AB = min maxB (x+y)
        y'BA = max 0 (x+y-maxA)
        x'BA = min maxA (x+y)

obj = 4
fOptJarras :: EstadoJarras -> Int
fOptJarras (_,y) = abs (obj - y)

-- res = minimoPD (0,0) genAccionesJarras aplicarAccionJarras fOptJarras
-- Se queda colgado, porque entra en un bucle de acciones


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

resultadoMochila = minimoPD (0,0,capacidadInicial) genAccionesMochila aplicarAccionMochila fOptMochila esEstadoFinalMochila

solucionMochila = reconstruyeSolucionPD aplicarAccionMochila esEstadoFinalMochila (0,0,capacidadInicial) (fst resultadoMochila)
