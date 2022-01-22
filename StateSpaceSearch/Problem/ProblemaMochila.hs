
module StateSpaceSearch.Problem.ProblemaMochila (
    EstadoMochila,
    AccionMochila,
    DatosObjeto,
    objetos,
    capacidadInicial,
    estadoInicialMochila,
    esEstadoFinalMochila,
    genAccionesMochila,
    fOptMochila,
    aplicarAccionMochila,
    heuristicaMochila
) where


-- Modelado del problema de la mochila --

type EstadoMochila = (Int,Int,Int)  -- Índice, valor acumulado, peso restante
type AccionMochila = Int            -- Cantidad de objetos que tomar
type DatosObjeto = (Int,Int,Int)    -- Valor, peso y unidades disponibles

objetos = [(3,9,2), (4,8,2), (2,6,1)] :: [DatosObjeto]
capacidadInicial = 25 :: Int
estadoInicialMochila = (0,0,capacidadInicial) :: EstadoMochila

esEstadoFinalMochila :: EstadoMochila -> Bool
esEstadoFinalMochila (i,_,_) = i == length objetos

genAccionesMochila :: EstadoMochila -> [AccionMochila]
genAccionesMochila (i,v,p) = [0 .. min mi (div p pi)]
    where (vi,pi,mi) = objetos !! i

fOptMochila :: EstadoMochila -> Int
fOptMochila (_,v,_) = v

aplicarAccionMochila :: EstadoMochila -> AccionMochila -> EstadoMochila
aplicarAccionMochila (i,v,p) a = (i+1, v+vi*a, p-pi*a)
    where (vi,pi,_) = objetos !! i

heuristicaMochila :: EstadoMochila -> AccionMochila -> Int
heuristicaMochila _ _ = maxBound  -- Lowest possible Int value
