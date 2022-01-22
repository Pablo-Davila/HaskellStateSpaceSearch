
module StateSpaceSearch.Problem.ProblemaJarras (
    EstadoJarras,
    AccionJarras,
    estadoInicialJarras,
    esEstadoFinalJarras,
    genAccionesJarras,
    fOptJarras,
    aplicarAccionJarras,
) where


-- Modelado del problema de las Jarras --

type EstadoJarras = (Int,Int)

data AccionJarras = LlenarA | LlenarB | VaciarA | VaciarB | EcharAB | EcharBA
    deriving (Show, Read, Eq)

maxA = 3 :: Int
maxB = 5 :: Int
obj = 4  :: Int
estadoInicialJarras = (0,0) :: EstadoJarras

esEstadoFinalJarras :: EstadoJarras -> Bool
esEstadoFinalJarras (_,y) = y == obj

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

fOptJarras :: EstadoJarras -> Int
fOptJarras (_,y) = abs (obj - y)
