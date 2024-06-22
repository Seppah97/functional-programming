-- Simple examples of function as parameter

-- map sqrt [1.0, 2.0, 3.0, 4.0]

hasExcl :: String -> Bool
hasExcl str = elem '!' str
-- filter hasExcl ["Hello!", "Bonjour", "Hei!"]

-- A function creating a function (lambda), that is then used as a parameter
createHasChar :: Char -> (String -> Bool)
createHasChar char = \str -> elem char str
-- filter (createHasChar '!') ["Hello!", "Bonjour", "Hei!"]

-- Lambda as a parameter
-- filter (\str -> elem '!' str) ["Hello!", "Bonjour", "Hei!"]

-- Function creating a function (lambda), that has the first parameter "fixed"
bind1st :: (a -> b -> c) -> a -> (b -> c)
bind1st func par1 = \par2 -> func par1 par2
-- filter (bind1st elem '!') ["Hello!", "Bonjour", "Hei!"]

-- Currying
-- filter (elem '!') ["Hello!", "Bonjour", "Hei!"]
