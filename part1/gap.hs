gap :: (Char, Char) -> Int -> String -> Int

gap _ _ "" = 0

gap (c1, c2) gapL str@(x:xs)
  | length str < 2 + gapL = 0
  | c1 == x && c2 == getNextChar str gapL = 1 + gap (c1, c2) gapL xs
  | otherwise = gap (c1, c2) gapL xs
  
  
getNextChar :: String -> Int -> Char

getNextChar str gapL = str !! (1 + gapL)