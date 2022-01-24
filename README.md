
# Trabajo-PD

Trabajo de Pablo Dávila Herrero y Raquel Valdez López para la asignatura
"Programación Declarativa".


## Instrucciones de ejecución

Este trabajo contiene dos librerías: una para trabajar con grafos y otra para
resolver problemas de búsqueda en espacios de estados. Por lo tanto, carece de
un programa principal que ejecutar.

Sin embargo, sí cuenta con datos de ejemplo y problemas ya modelados que se
utilizan en los dos programas de pruebas/ejemplos siguientes:
 
 - `testsGraph.hs`
 - `testsStateSpaceSearch.hs`

El objetivo de estos programas es, por un lado, comprobar el buen funcionamiento
de las librerías y, por otro, que el usuario pueda ver qué ofrecen las
librerías y cómo son sus funciones a nivel de código.

### GHCI
A continuación se muestran ejemplos de cómo ejecutar los progrmas de prueba
utilizando GHCI:

``` Bash
> ghci ./testsGraph.hs
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 5] Compiling Graph            ( Graph.hs, interpreted )
[2 of 5] Compiling Graph.Connection ( Graph/Connection.hs, interpreted )
[3 of 5] Compiling Graph.Samples    ( Graph/Samples.hs, interpreted )
[4 of 5] Compiling Graph.TreeCover  ( Graph/TreeCover.hs, interpreted )
[5 of 5] Compiling Main             ( testsGraph.hs, interpreted )
Ok, five modules loaded.
*Main> result
Everything correct!
```

``` Bash
> ghci ./testsStateSpaceSearch.hs        
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 8] Compiling Pila             ( Pila.hs, interpreted )
[2 of 8] Compiling StateSpaceSearch.Backtracking ( StateSpaceSearch/Backtracking.hs, interpreted )
[3 of 8] Compiling StateSpaceSearch.DynamicProgramming ( StateSpaceSearch/DynamicProgramming.hs, interpreted )
[4 of 8] Compiling StateSpaceSearch.Greedy ( StateSpaceSearch/Greedy.hs, interpreted )
[5 of 8] Compiling StateSpaceSearch.Problem.ProblemaJarras ( StateSpaceSearch/Problem/ProblemaJarras.hs, interpreted )
[6 of 8] Compiling StateSpaceSearch.Problem.ProblemaMatrices ( StateSpaceSearch/Problem/ProblemaMatrices.hs, interpreted )
[7 of 8] Compiling StateSpaceSearch.Problem.ProblemaMochila ( StateSpaceSearch/Problem/ProblemaMochila.hs, interpreted )
[8 of 8] Compiling Main             ( testsStateSpaceSearch.hs, interpreted )
Ok, 8 modules loaded.
*Main> mochilaBT
Sol (3,11,0) 11  
*Main> mochilaPDR
(1|2|0|-,11)  
*Main> mochilaPDRState
(3,11,0)   
*Main> mochilaPD
11
*Main> mochilaGD
(2|0|1|-,8)
*Main> mochilaGDState
(3,8,1)
*Main> matricesPD
8000
*Main> jarrasPDR
# La evaluación de jarrasPDR no termina. La razón está detallada en la memoria
```
