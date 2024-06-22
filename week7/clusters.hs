clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]


clusters _ _ [] = []

clusters f d ss = foldr (\x acc -> distanceFilter f d x ss : acc) [] ss


distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]

distanceFilter f d s ss = filter (\x -> f s x <= d) ss


distance1 :: String -> String -> Float

distance1 [] [] = 0
distance1 str1 str2 = (fromIntegral (length (charNotAppear str1 str2)) + fromIntegral (length (charNotAppear str2 str1))) / fromIntegral (length str1 + length str2)

charNotAppear :: String -> String -> String

charNotAppear "" _ = []
charNotAppear (x:xs) str2
    | x `notElem` str2 && xs /= "" = x : charNotAppear xs str2
    | x `notElem` str2 && xs == "" = [x]
    | otherwise = charNotAppear xs str2
