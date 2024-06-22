-- Add filename suffix only if it is given
addSuffix :: String -> Maybe String -> String
addSuffix filename (Just suffix) = filename ++ "." ++ suffix
addSuffix filename Nothing       = filename

-- Produce a result only if a filename suffix is given
addSuffix2 :: String -> Maybe String -> Maybe String
addSuffix2 filename (Just suffix) = Just (filename ++ "." ++ suffix)
addSuffix2 filename Nothing       = Nothing

x1 :: Either String Integer
x1 = Right 1 -- The "successful" value case

x2 :: Either String Integer
x2 = Left "Something went wrong" -- The "failure" case with error message

-- Behave like addSuffix, but produce an error value if filename is empty
addSuffix3 :: String -> Maybe String -> Either String String
addSuffix3 ""       _             = Left "Empty filename given in addSuffix3"
addSuffix3 filename (Just suffix) = Right (filename ++ "." ++ suffix)
addSuffix3 filename Nothing       = Right filename -- Don't add suffix, if it is not given
-- addSuffix3 filename Nothing       = Left "No suffix given" -- alternatively, no suffix is error

getConfFileName appname =
    case (addSuffix3 appname (Just "cfg") of
         Right confname -> confname -- Success: appname.cfg
         Left  _        -> "defaultapp.cfg" -- Failure in addSuffix3, use default app appname
    )