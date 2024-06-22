-- Value
-- 3

-- Create variable (name)
v :: Integer
v = 3

-- Read value of variable
-- v

-- (No chaging variable!)


-- Store in data structure
lst :: [Integer]
lst = [4, v]

-- Pass as parameter
f :: Integer -> Integer
f x = 2*x
-- f 4, f v
-- Return from function



-- Create function (name)
-- See above

-- Value
-- \ x -> 2*x
f2 :: Integer -> Integer
f2 = \ x -> 2*x
-- f2 3

-- Read the value of function
-- (f)
-- Call the function
-- f 3

-- (No changing function!)



-- Store in data structure
flst :: [(Integer -> Integer)]
flst = [\ x -> -x, (f)]
-- (flst!!0) 3
-- (flst!!1) 3

-- Pass as parameter
pass3 :: (Integer -> Integer) -> Integer
pass3 func = func 3
-- pass3 f
-- pass3 (\x -> x*x)

-- Return from func
lessthan :: Integer -> (Integer -> Bool)
lessthan x = \ val -> val < x

lessthan3 :: (Integer -> Bool)
lessthan3 = lessthan 3
-- lessthan3 2
-- lessthan3 5
-- (lessthan 5) 8
-- lessthan 5 8
