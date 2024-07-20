calculate :: [String] -> [String]
calculate [] = []
calculate calc = map performCalculation calc

performCalculation :: String -> String
performCalculation expr =
    case words expr of
        [a, op, b] -> case (readMaybe a, readMaybe b) of
            (Just x, Just y) -> case op of
                "+" -> show (x + y)
                "-" -> show (x - y)
                "*" -> show (x * y)
                _   -> "I cannot calculate that"
            _ -> "I cannot calculate that"
        _ -> "I cannot calculate that"

readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
                    [(x,"")] -> Just x
                    _ -> Nothing