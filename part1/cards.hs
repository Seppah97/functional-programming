credits :: (Char, Int) -> (Char, Int) -> Int

credits (suit1, value1) (suit2, value2)
    | (suit1 == 's' && value1 == 14) || (suit2 == 's' && value2 == 14) = 14
    | suit1 == suit2 && abs(value2 - value1) == 1 = 8
    | value1 == value2 = 6
    | abs(value2 - value1) == 1 = 4
    | suit1 == suit2 = 2
    | otherwise = 0
