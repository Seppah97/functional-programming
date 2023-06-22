charsDivisibleBy :: Int -> [Char] -- given a number n, returns all the characters that have a number divisible by n.


charsDivisibleBy 0 = []
charsDivisibleBy num = [numToLetter x | x <- [1..26], x `mod` num == 0]


charsProductOf :: [Int] -> [Char] -- given a list of numbers ns, returns all the characters that have a number that is a product of any two numbers in ns. 
-- You may assume that each number only appears in the list once.

charsProductOf nums = [numToLetter x | x <- [1..26], x `elem` calculateList nums nums]



calculateList :: [Int] -> [Int] -> [Int]
calculateList [] _ = []
calculateList num1@(x:xs) num2@(y:ys)
  | null xs = []
  | x == y = calculateList (x:xs) (dropWhile (==x) ys)
  | not (null ys) = x * y: calculateList (x:xs) ys
  | otherwise = x * y: calculateList xs xs


numToLetter :: Int -> Char

numToLetter num
    | num == 1 = 'a'
    | num == 2 = 'b'
    | num == 3 = 'c'
    | num == 4 = 'd'
    | num == 5 = 'e'
    | num == 6 = 'f'
    | num == 7 = 'g'
    | num == 8 = 'h'
    | num == 9 = 'i'
    | num == 10 = 'j'
    | num == 11 = 'k'
    | num == 12 = 'l'
    | num == 13 = 'm'
    | num == 14 = 'n'
    | num == 15 = 'o'
    | num == 16 = 'p'
    | num == 17 = 'q'
    | num == 18 = 'r'
    | num == 19 = 's'
    | num == 20 = 't'
    | num == 21 = 'u'
    | num == 22 = 'v'
    | num == 23 = 'w'
    | num == 24 = 'x'
    | num == 25 = 'y'
    | num == 26 = 'z'
    | otherwise = ' '

