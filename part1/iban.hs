
validate :: String -> Bool

validate str
    | length str /= 18 = False
    | take 2 str /= "FI" = False
    | not (onlyDigits (drop 2 str)) = False
    | takeMod (convert (drop 2 str)) == 1 = True
    | otherwise = False


takeMod :: Integer -> Integer

takeMod num = num `mod` 97

convert :: String -> Integer
convert iban = read (generateNewString iban) :: Integer

generateNewString :: String -> String

generateNewString str = drop 2 str ++ "1518" ++ take 2 str


onlyDigits :: String -> Bool

onlyDigits "" = False
onlyDigits (x:xs)
    | (x `elem` ['0'..'9']) && xs /= "" = onlyDigits xs
    | (x `elem` ['0'..'9']) && xs == "" = True
    | otherwise = False