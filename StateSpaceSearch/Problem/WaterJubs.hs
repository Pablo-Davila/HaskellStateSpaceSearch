
module StateSpaceSearch.Problem.WaterJubs (
    JubsState,
    JubsAction,
    jubsInitialState,
    jubsIsFinalState,
    jubsGenActions,
    jubsOptFunc,
    jubsApplyAction,
) where


-- Water jubs problem modelization --

type JubsState = (Int,Int)

data JubsAction = FillA | FillB | EmptyA | EmptyB | TransferAB | TransferBA
    deriving (Show, Read, Eq)

maxA = 3 :: Int
maxB = 5 :: Int
obj = 4  :: Int
jubsInitialState = (0,0) :: JubsState

jubsIsFinalState :: JubsState -> Bool
jubsIsFinalState (_,y) = y == obj

jubsGenActions :: JubsState -> [JubsAction]
jubsGenActions (x,y) = []
    ++ (if x<maxA then [FillA] else [])
    ++ (if y<maxB then [FillB] else [])
    ++ (if x>0 then [EmptyA] else [])
    ++ (if y>0 then [EmptyB] else [])
    ++ (if x>0 && y<maxB then [TransferAB] else [])
    ++ (if y>0 && x<maxA then [TransferBA] else [])

jubsApplyAction :: JubsState -> JubsAction -> JubsState
jubsApplyAction (x,y) a
    | a == FillA = (maxA,y)
    | a == FillB = (x,maxB)
    | a == EmptyA = (0,y)
    | a == EmptyB = (x,0)
    | a == TransferAB = (x'AB, y'AB)
    | a == TransferBA = (x'BA, y'BA)
    where
        x'AB = max 0 (x+y-maxB)
        y'AB = min maxB (x+y)
        y'BA = max 0 (x+y-maxA)
        x'BA = min maxA (x+y)

jubsOptFunc :: JubsState -> Int
jubsOptFunc (_,y) = abs (obj - y)
