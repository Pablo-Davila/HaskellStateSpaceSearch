
# HaskellStateSpaceSearch

HaskellStateSpaceSearch is a library containing state space search algorithms for Haskell. It was initially developed as part of an assignment for the subject "Declarative Programming" at the University of Seville.

The remaining part is [HaskellGraph](https://github.com/Pablo-Davila/HaskellGraph), a library used to work with graph data structures.


## Contents

 - `StateSpaceSearch.Backtracking`: Implementation of the well-known [backtracking algoritm](https://en.wikipedia.org/w/index.php?title=Backtracking&oldid=1058260479).
 - `StateSpaceSearch.DynamicProgramming`: Implementation of the [dynamic programming algorithm](https://en.wikipedia.org/wiki/Dynamic_programming), both for problems that produce many subproblems in each step and a simplified version for reduction problems.
 - `StateSpaceSearch.Greedy`: Implementation of a [greedy algorithm](https://en.wikipedia.org/wiki/Greedy_algorithm).

_Note_: [testsStateSpaceSearch.hs](./testsStateSpaceSearch.hs) contains usage examples of all the modules. It also analyzes the problems that can be adressed with these algorithms.


## Usage

You may import the modules you need by using the following lines:

``` Haskell
import StateSpaceSearch.Backtracking
import StateSpaceSearch.DynamicProgramming
import StateSpaceSearch.Greedy
```

_Note_: `reconstructSolState` function can be found in both the DynamicProgramming and the Greedy submodules. If you want to import both you will need to hide it from in one of them as follows:

``` Haskell
import StateSpaceSearch.Greedy hiding (reconstructSolState)
```
