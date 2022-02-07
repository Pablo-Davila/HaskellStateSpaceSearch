
module StateSpaceSearch.Problem.Backpack (
    BackpackState,
    BackpackAction,
    ObjectData,
    objects,
    initialCapacity,
    backpackInitialState,
    backpackIsFinalState,
    backpackGenActions,
    backpackOptFunc,
    backpackApplyAction,
    backpackHeuristic
) where


-- Backpack problem modelization --

type BackpackState = (Int,Int,Int)  -- Index, accumulated value, free weight
type BackpackAction = Int           -- Amount of objects to take of the current type
type ObjectData = (Int,Int,Int)     -- Value, weight and available units

objects = [(3,9,2), (4,8,2), (2,6,1)] :: [ObjectData]
initialCapacity = 25 :: Int
backpackInitialState = (0,0,initialCapacity) :: BackpackState

backpackIsFinalState :: BackpackState -> Bool
backpackIsFinalState (i,_,_) = i == length objects

backpackGenActions :: BackpackState -> [BackpackAction]
backpackGenActions (i,v,w) = [0 .. min mi (div w wi)]
    where (vi,wi,mi) = objects !! i

backpackOptFunc :: BackpackState -> Int
backpackOptFunc (_,v,_) = v

backpackApplyAction :: BackpackState -> BackpackAction -> BackpackState
backpackApplyAction (i,v,w) a = (i+1, v+vi*a, w-wi*a)
    where (vi,wi,_) = objects !! i

backpackHeuristic :: BackpackState -> BackpackAction -> Int
backpackHeuristic _ _ = maxBound  -- Lowest possible Int value
