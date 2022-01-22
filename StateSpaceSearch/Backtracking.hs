
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
backtrackingMin heuristica acciones fOpt esCasoBase avanza e@estado sol
    | esCasoBase e = elige sol (Sol e (fOpt e))
    | otherwise = aux accionesFiltradas
    where
        minBT = backtrackingMin heuristica acciones fOpt esCasoBase avanza
        accionesFiltradas
            | isNull sol = acciones e
            | otherwise = [a | a <- acciones e, heuristica e a < solValue sol]
        elige sol1 sol2
            | isNull sol1 = sol2
            | isNull sol2 = sol1
            | solValue sol2 < solValue sol1 = sol2
            | otherwise = sol1
        aux [] = Null
        aux (a:as) = elige (minBT (avanza e a) sol) (aux as)

backtrackingMax :: (Ord v, Num v) => (e -> a -> v) -> (e -> [a]) -> (e -> v) -> (e -> Bool) -> (e -> a -> e) -> e -> Solution e v -> Solution e v
backtrackingMax heuristica acciones fOpt esCasoBase avanza estado sol = (
    res $ backtrackingMin heuristica acciones (\e -> -fOpt e) esCasoBase avanza estado sol)
    where
        res Null = Null
        res (Sol e v) = Sol e (-v)
