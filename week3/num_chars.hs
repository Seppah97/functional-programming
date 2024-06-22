

intToChar :: Int -> Char
intToChar n
  | n < 1 || n > 26 = error "Input must be between 1 and 26"
  | otherwise = toEnum (n - 1 + fromEnum 'a')


charsDivisibleBy :: Int -> [Char] -- given a number n, returns all the characters that have a number divisible by n.


charsDivisibleBy 0 = []
charsDivisibleBy num = [intToChar x | x <- [1..26], x `mod` num == 0]

charsProductOf :: [Int] -> [Char] -- given a list of numbers ns, returns all the characters that have a number that is a product of any two numbers in ns. 
-- You may assume that each number only appears in the list once.

charsProductOf nums = [intToChar x | x <- [1..26], x `elem` calculateList nums nums]



calculateList :: [Int] -> [Int] -> [Int]
calculateList [] _ = []
calculateList num1@(x:xs) num2@(y:ys)
  | null xs = []
  | x == y = calculateList (x:xs) (dropWhile (==x) ys)
  | not (null ys) = x * y: calculateList (x:xs) ys
  | otherwise = x * y: calculateList xs xs