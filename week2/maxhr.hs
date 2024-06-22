maxhr :: Float -> Float


maxhr age
    | age > 40 = 207-0.7*age
    | otherwise = 220-age