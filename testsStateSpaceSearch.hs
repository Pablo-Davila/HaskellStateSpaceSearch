
import StateSpaceSearch.Backtracking
import StateSpaceSearch.DynamicProgramming
import StateSpaceSearch.Greedy hiding (reconstructSolState)

import StateSpaceSearch.Problem.ProblemaMochila


-- Problema de la mochila --

-- Backtracking
mochilaBT = backtrackingMin heuristicaMochila genAccionesMochila fOptMochila esEstadoFinalMochila aplicarAccionMochila (0,0,capacidadInicial) (getNull)

-- Programación dinámica
mochilaPDR = dynamicpRedMin (0,0,capacidadInicial) genAccionesMochila aplicarAccionMochila fOptMochila esEstadoFinalMochila
mochilaPDRState = reconstructSolState aplicarAccionMochila esEstadoFinalMochila (0,0,capacidadInicial) (fst mochilaPDR)

mochilaPD = dynamicpMin (0,0,capacidadInicial) genAccionesMochila (\e a -> [aplicarAccionMochila e a]) fOptMochila esEstadoFinalMochila head

-- Voraz
mochilaGD = greedyMin genAccionesMochila aplicarAccionMochila fOptMochila esEstadoFinalMochila heuristicaMochila (0,0,capacidadInicial)
mochilaGDState = reconstructSolState aplicarAccionMochila esEstadoFinalMochila (0,0,capacidadInicial) (fst mochilaGD)

