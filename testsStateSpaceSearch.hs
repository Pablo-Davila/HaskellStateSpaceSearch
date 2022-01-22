
import StateSpaceSearch.Backtracking
import StateSpaceSearch.DynamicProgramming
import StateSpaceSearch.Greedy hiding (reconstructSolState)

import StateSpaceSearch.Problem.ProblemaMochila
import StateSpaceSearch.Problem.ProblemaMatrices


-- Problema de la mochila --

-- Backtracking
mochilaBT = backtrackingMin heuristicaMochila genAccionesMochila fOptMochila esEstadoFinalMochila aplicarAccionMochila (0,0,capacidadInicial) (getNull)

-- Programación dinámica con reducción
mochilaPDR = dynamicpRedMin (0,0,capacidadInicial) genAccionesMochila aplicarAccionMochila fOptMochila esEstadoFinalMochila
mochilaPDRState = reconstructSolState aplicarAccionMochila esEstadoFinalMochila (0,0,capacidadInicial) (fst mochilaPDR)

-- Programación dinámica
mochilaPD = dynamicpMin (0,0,capacidadInicial) genAccionesMochila (\e a -> [aplicarAccionMochila e a]) fOptMochila esEstadoFinalMochila (\_ _ vs -> head vs)

-- Voraz
mochilaGD = greedyMin genAccionesMochila aplicarAccionMochila fOptMochila esEstadoFinalMochila heuristicaMochila (0,0,capacidadInicial)
mochilaGDState = reconstructSolState aplicarAccionMochila esEstadoFinalMochila (0,0,capacidadInicial) (fst mochilaGD)


-- Problema Matrices --

-- Programación dinámica
matricesPD = dynamicpMin estadoInicialMatrices genAccionesMatrices aplicarAccionMatrices fOptMatrices esEstadoFinalMatrices solucionParcialMatrices

-- Este problema no se puede resolver mediante backtracking, programación
-- dinámica con reducción ni el algoritmo voraz
