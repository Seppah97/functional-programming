gap :: (Char, Char) -> Int -> String -> Int

gap _ _ "" = 0

gap (c1, c2) gapLen str@(x:rest)
  | length str < 2 + gapLen = 0
  | c1 == x && c2 == getNextChar str gapLen = 1 + gap (c1, c2) gapLen rest
  | otherwise = gap (c1, c2) gapLen rest
  
  
getNextChar :: String -> Int -> Char

getNextChar str gapL = str !! (1 + gapL)