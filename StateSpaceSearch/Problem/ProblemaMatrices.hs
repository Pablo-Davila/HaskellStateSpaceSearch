
module StateSpaceSearch.Problem.ProblemaMatrices (
    EstadoMatrices,
    AccionMatrices,
    matrices,
    estadoInicialMatrices,
    esEstadoFinalMatrices,
    genAccionesMatrices,
    fOptMatrices,
    solucionParcialMatrices,
    aplicarAccionMatrices,
    heuristicaMatrices
) where


-- Modelado del problema de las Matrices --

type EstadoMatrices = (Int,Int)  -- i, j
type AccionMatrices = Int        -- Índice de la matriz por la que dividir la cadena

matrices = [(5,50),(50,10),(10,10),(10,100)] :: [(Int,Int)]
estadoInicialMatrices = (0,length matrices - 1) :: EstadoMatrices

esEstadoFinalMatrices :: EstadoMatrices -> Bool
esEstadoFinalMatrices (i,j) = j-i+1 <= 2

genAccionesMatrices :: EstadoMatrices -> [AccionMatrices]
genAccionesMatrices (i,j) = [i+1..j-1]

fOptMatrices :: EstadoMatrices -> Int
fOptMatrices (i,j)
    | j-i == 1 = (fst mi) * (snd mi) * (snd mj)
    | otherwise = 0
    where
        mi = matrices !! i
        mj = matrices !! j

solucionParcialMatrices :: EstadoMatrices -> AccionMatrices -> [Int] -> Int
solucionParcialMatrices (i,j) k [v1,v2] = v1 + v2 + (fst mi)*(snd mk)*(snd mj)
    where
        mi = matrices !! i
        mk = matrices !! k
        mj = matrices !! j
solucionParcialMatrices _ _ _ = error "Como mucho puede haber dos subproblemas de matrices"

aplicarAccionMatrices :: EstadoMatrices -> AccionMatrices -> [EstadoMatrices]
aplicarAccionMatrices (i,j) k = [(i,k), (k+1,j)]

heuristicaMatrices :: EstadoMatrices -> AccionMatrices -> Int
heuristicaMatrices _ _ = undefined
