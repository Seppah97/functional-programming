headOrLast :: [String] -> Char -> [String]

headOrLast strings char = filter (\str -> head str == char || last str == char) strings