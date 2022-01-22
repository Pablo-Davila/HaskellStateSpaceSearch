
import StateSpaceSearch.Backtracking
import StateSpaceSearch.DynamicProgramming
import StateSpaceSearch.Greedy hiding (reconstructSolState)

import StateSpaceSearch.Problem.ProblemaMochila
import StateSpaceSearch.Problem.ProblemaMatrices
import StateSpaceSearch.Problem.ProblemaJarras


-- Problema de la mochila --

-- Backtracking
mochilaBT = backtrackingMax heuristicaMochila genAccionesMochila fOptMochila esEstadoFinalMochila aplicarAccionMochila estadoInicialMochila (getNull)

-- Programación dinámica con reducción
mochilaPDR = dynamicpRedMax estadoInicialMochila genAccionesMochila aplicarAccionMochila fOptMochila esEstadoFinalMochila
mochilaPDRState = reconstructSolState aplicarAccionMochila esEstadoFinalMochila estadoInicialMochila (fst mochilaPDR)

-- Programación dinámica
mochilaPD = dynamicpMax estadoInicialMochila genAccionesMochila (\e a -> [aplicarAccionMochila e a]) fOptMochila esEstadoFinalMochila (\_ _ vs -> head vs)

-- Voraz
mochilaGD = greedyMax genAccionesMochila aplicarAccionMochila fOptMochila esEstadoFinalMochila heuristicaMochila estadoInicialMochila
mochilaGDState = reconstructSolState aplicarAccionMochila esEstadoFinalMochila estadoInicialMochila (fst mochilaGD)


-- Problema Matrices --

-- Programación dinámica
matricesPD = dynamicpMin estadoInicialMatrices genAccionesMatrices aplicarAccionMatrices fOptMatrices esEstadoFinalMatrices solucionParcialMatrices

-- Este problema no se puede resolver mediante backtracking, programación
-- dinámica con reducción ni el algoritmo voraz


-- Problema de las jarras --

-- Este problema no se puede resolver en general mediante ninguno de los
-- algoritmos anteriores, debido a que se pueden producir ciclos infinitos de
-- acciones que pasan por los mismos estados sin llegar a un caso base

-- Al acceder a jarrasPD se calcula su valor (evaluación perezosa) y el
-- programa queda bloqueado

jarrasPDR = dynamicpRedMin estadoInicialJarras genAccionesJarras aplicarAccionJarras fOptJarras esEstadoFinalJarras
