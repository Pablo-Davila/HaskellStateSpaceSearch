
module StateSpaceSearch.Backtracking (
    Solution,
    getNull,
    isNull,
    solState,
    solValue,
    backtrackingMin,
    backtrackingMax
) where


-- Solution type and helper functions --

data Solution e v = Null | Sol e v
    deriving (Show, Eq)

getNull :: Solution e v
getNull = Null

isNull :: Solution e v -> Bool
isNull Null = True
isNull _ = False

solState :: Solution e v -> e
solState (Sol e _) = e
solState _ = error "Null solution does not provide a state"

solValue :: Solution e v -> v
solValue (Sol _ v) = v
solValue _ = error "Null solution does not provide a value"


-- Backtracking generic algorithm --

backtrackingMin :: Ord v => (e -> a -> v) -> (e -> [a]) -> (e -> v) -> (e -> Bool) -> (e -> a -> e) -> e -> Solution e v -> Solution e v
backtrackingMin heuristic actions optFunc isFinalState applyAction e@state sol
    | isFinalState e = choose sol (Sol e (optFunc e))
    | otherwise = aux filteredActions
    where
        minBT = backtrackingMin heuristic actions optFunc isFinalState applyAction
        filteredActions
            | isNull sol = actions e
            | otherwise = [a | a <- actions e, heuristic e a < solValue sol]
        choose sol1 sol2
            | isNull sol1 = sol2
            | isNull sol2 = sol1
            | solValue sol2 < solValue sol1 = sol2
            | otherwise = sol1
        aux as = case as of
            [] -> Null
            (a:as) -> choose (minBT (applyAction e a) sol) (aux as)

backtrackingMax :: (Ord v, Num v) => (e -> a -> v) -> (e -> [a]) -> (e -> v) -> (e -> Bool) -> (e -> a -> e) -> e -> Solution e v -> Solution e v
backtrackingMax heuristic actions optFunc isFinalState applyAction state sol = (
    res $ backtrackingMin heuristic actions (\e -> -optFunc e) isFinalState applyAction state sol)
    where
        res Null = Null
        res (Sol e v) = Sol e (-v)
