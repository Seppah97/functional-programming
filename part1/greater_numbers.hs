nextIsGreater :: [Int] -> [Int] 
 
nextIsGreater [] = []
nextIsGreater [_] = []
nextIsGreater (first:second:rest)
    | first < second = first : nextIsGreater(second:rest)
    | otherwise = nextIsGreater(second:rest)