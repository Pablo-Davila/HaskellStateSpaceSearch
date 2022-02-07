
module StateSpaceSearch.Problem.MatricesMultiplication (
    MatricesState,
    MatricesAction,
    matrices,
    matricesInitialState,
    matricesIsFinalState,
    matricesGenActions,
    matricesOptFunc,
    matricesPartialSolution,
    matricesApplyAction,
    matricesHeuristic
) where


-- Matrices multiplication problem modelization --

type MatricesState = (Int,Int)  -- i, j
type MatricesAction = Int       -- Index used to divide the matrices sequence

matrices = [(5,50),(50,10),(10,10),(10,100)] :: [(Int,Int)]
matricesInitialState = (0,length matrices - 1) :: MatricesState

matricesIsFinalState :: MatricesState -> Bool
matricesIsFinalState (i,j) = j-i+1 <= 2

matricesGenActions :: MatricesState -> [MatricesAction]
matricesGenActions (i,j) = [i+1..j-1]

matricesOptFunc :: MatricesState -> Int
matricesOptFunc (i,j)
    | j-i == 1 = (fst mi) * (snd mi) * (snd mj)
    | otherwise = 0
    where
        mi = matrices !! i
        mj = matrices !! j

matricesPartialSolution :: MatricesState -> MatricesAction -> [Int] -> Int
matricesPartialSolution (i,j) k [v1,v2] = v1 + v2 + (fst mi)*(snd mk)*(snd mj)
    where
        mi = matrices !! i
        mk = matrices !! k
        mj = matrices !! j
matricesPartialSolution _ _ _ = error "There cannot be more than two matrices multiplication subproblems"

matricesApplyAction :: MatricesState -> MatricesAction -> [MatricesState]
matricesApplyAction (i,j) k = [(i,k), (k+1,j)]

matricesHeuristic :: MatricesState -> MatricesAction -> Int
matricesHeuristic _ _ = undefined
