onlyDigits :: String -> Bool

onlyDigits "" = False
onlyDigits (x:xs)
    | (x `elem` ['0'..'9']) && xs /= "" = onlyDigits xs
    | (x `elem` ['0'..'9']) && xs == "" = True
    | otherwise = False

