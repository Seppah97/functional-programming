

commonSubstring :: String -> String -> String


commonSubstring _ "" = ""
commonSubstring "" _ = ""


commonSubstring s1@(x:xs) s2@(y:ys)
  | x `elem` s2 && x /= y = commonSubstring (x:xs) ys
  | y `elem` s1 && x /= y = commonSubstring xs (y:ys)
  | x == y && length xs > length ys = x : commonSubstring  (dropWhile (/=x) s1) ys
  | x == y && length xs < length ys = y : commonSubstring xs (dropWhile (/=y) s2)
  | x == y = x : commonSubstring xs ys 
  | otherwise = commonSubstring xs ys



