distance1 :: String -> String -> Float

distance1 "" "" = 0
distance1 str1 str2 = (fromIntegral (length (unCommonChar str1 str2)) + fromIntegral (length (unCommonChar str2 str1))) / fromIntegral (length str1 + length str2)


unCommonChar :: String -> String -> String

unCommonChar "" _ = []
unCommonChar str1@(x:xs) str2
    | x `notElem` str2 && xs /= "" = x : unCommonChar xs str2
    | x `notElem` str2 && xs == "" = [x]
    | otherwise = unCommonChar xs str2


distance2 :: String -> String -> Float 


distance2 "" "" = 0
distance2 str1 str2 = (fromIntegral (length (notDigits str1)) + fromIntegral (length (notDigits str2))) / fromIntegral (length str1 + length str2)


notDigits :: String -> String

notDigits "" = []
notDigits (x:xs) 
    | (x `notElem` ['0'..'9']) && xs /= "" = x : notDigits xs
    | (x `notElem` ['0'..'9']) && xs == "" = [x]
    | otherwise = notDigits xs