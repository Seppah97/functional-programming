
distance1 :: String -> String -> Float

distance1 [] [] = 0
distance1 str1 str2 = (fromIntegral (length (charNotAppear str1 str2)) + fromIntegral (length (charNotAppear str2 str1))) / fromIntegral (length str1 + length str2)

charNotAppear :: String -> String -> String

charNotAppear "" _ = []
charNotAppear (x:xs) str2
    | x `notElem` str2 && xs /= "" = x : charNotAppear xs str2
    | x `notElem` str2 && xs == "" = [x]
    | otherwise = charNotAppear xs str2



distance2 :: String -> String -> Float  

distance2 [] [] = 0
distance2 str1 str2 = (fromIntegral (length (nonDigits str1)) + fromIntegral (length (nonDigits str2))) / fromIntegral (length str1 + length str2)


nonDigits :: String -> String

nonDigits "" = []
nonDigits (x:xs) 
    | x `notElem` ['0'..'9'] && xs /= "" = x : nonDigits xs
    | x `notElem` ['0'..'9'] && xs == "" = [x]
    | otherwise = nonDigits xs