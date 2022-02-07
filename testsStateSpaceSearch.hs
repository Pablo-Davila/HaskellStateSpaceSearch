
import StateSpaceSearch.Backtracking
import StateSpaceSearch.DynamicProgramming
import StateSpaceSearch.Greedy hiding (reconstructSolState)

import StateSpaceSearch.Problem.Backpack
import StateSpaceSearch.Problem.MatricesMultiplication
import StateSpaceSearch.Problem.WaterJubs


-- Backpack problem --

-- Backtracking
backpackBT = backtrackingMax backpackHeuristic backpackGenActions backpackOptFunc backpackIsFinalState backpackApplyAction backpackInitialState (getNull)

-- Dynamic programming with reduction
backpackPDR = dynamicpRedMax backpackInitialState backpackGenActions backpackApplyAction backpackOptFunc backpackIsFinalState
backpackPDRState = reconstructSolState backpackApplyAction backpackInitialState (fst backpackPDR)

-- Dynamic programming
backpackPD = dynamicpMax backpackInitialState backpackGenActions (\e a -> [backpackApplyAction e a]) backpackOptFunc backpackIsFinalState (\_ _ vs -> head vs)

-- Voraz
backpackGD = greedyMax backpackGenActions backpackApplyAction backpackOptFunc backpackIsFinalState backpackHeuristic backpackInitialState
backpackGDState = reconstructSolState backpackApplyAction backpackInitialState (fst backpackGD)


-- Matrices multiplication problem --

-- Dynamic programming
matricesPD = dynamicpMin matricesInitialState matricesGenActions matricesApplyAction matricesOptFunc matricesIsFinalState matricesPartialSolution

-- This problem cannot be solved using backtracking, dynamic programming with
-- reduction nor the greedy algorithm. This is because every action taken leads
-- to more than one subproblems.


-- Water jubs problem --

-- In general, this problem cannot be solved using any of the implemented
-- algorithms because infinite actions loops may appear without reaching a
-- solution.

-- Haskell uses lazy evaluation, so jubsPDR will be calculated when called and the program will freeze

jubsPDR = dynamicpRedMin jubsInitialState jubsGenActions jubsApplyAction jubsOptFunc jubsIsFinalState
