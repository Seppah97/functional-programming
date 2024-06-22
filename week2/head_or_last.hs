headOrLast :: [String] -> Char -> [String]
headOrLast [] _ = []
headOrLast strings char = filter (\str -> not (null str) && (head str == char || last str == char)) strings