
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show,Eq)

newtype CountryCode = CountryCode Integer deriving (Eq)

newtype PhoneNo = PhoneNo Integer deriving (Eq)

toCountryCode :: Integer -> CountryCode

toCountryCode cc
    | cc < 0 = error "Negative country code"
    | otherwise = CountryCode cc

toPhoneNo :: Integer -> PhoneNo

toPhoneNo pn
    | pn < 0 = error "Negative phone number"
    | otherwise = PhoneNo pn

instance Show CountryCode where
    show (CountryCode value) = "+" ++ show value

instance Show PhoneNo where
    show (PhoneNo value) = show value

data Phone = Phone {
    phoneType :: Maybe PhoneType,
    countryCode :: Maybe CountryCode,
    phoneNo :: PhoneNo
} deriving Eq

instance Show Phone where
    show (Phone pt cc pn)  =
        let ccStr = maybe "" (\code -> show code ++ " ") cc
            ptStr = maybe "" (\ptype -> " (" ++ show ptype ++ ")") pt
        in ccStr ++ show pn ++ ptStr

removeCountryCodePrefix :: String -> String
removeCountryCodePrefix str
    | take 2 str == "00" = drop 2 str
    | take 1 str == "+"  = drop 1 str
    | otherwise        = str

isInteger :: String -> Bool
isInteger str =
    case reads str :: [(Integer, String)] of
        [(n, "")] -> True
        _         -> False

readPhoneType :: String -> Maybe PhoneType
readPhoneType "" = Nothing
readPhoneType typeStr = 
    case typeStr of
        "WorkLandline"  -> Just WorkLandline
        "PrivateMobile" -> Just PrivateMobile
        "WorkMobile"    -> Just WorkMobile
        "Other"         -> Just Other
        _               -> Nothing

readCountryCode :: String -> [Integer] -> Maybe CountryCode
readCountryCode "" _ = Nothing
readCountryCode cc ccList = 
    let cleaned = removeCountryCodePrefix cc
    in if isInteger cleaned 
        then let n = read cleaned::Integer 
            in if n `elem` ccList
                then Just $ toCountryCode n
                else error "Unknown country code"
        
        else error "Incorrect country code"


readPhoneNo :: String -> PhoneNo
readPhoneNo "" = error "Empty phone number"
readPhoneNo pn = 
    if isInteger pn
        then let p = read pn::Integer
            in toPhoneNo p
    else error "Incorrect phone number"

readPhone :: String -> String -> String -> [Integer] -> Phone

readPhone phonetypestr countrycodestr phonenostr ccodelist =
    let pt = readPhoneType phonetypestr
        cc = readCountryCode countrycodestr ccodelist
        pn = readPhoneNo phonenostr
    in Phone pt cc pn



