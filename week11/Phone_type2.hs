module Phone_type2
  ( Phone(..),
    PhoneType(..),
    CountryCode(..),
    PhoneNo(..),
    toPhoneNo,
    toCountryCode,
    fromPhoneNo,
    readPhone
    -- rest of exported stuff
  ) where

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show,Eq)

data CountryCode = CountryCode Integer deriving (Show, Eq)

data PhoneNo = PhoneNo Integer deriving (Show, Eq)

toCountryCode :: Integer -> CountryCode

toCountryCode cc
    | cc < 0 = error "Negative country code"
    | otherwise = CountryCode cc

toPhoneNo :: Integer -> PhoneNo

toPhoneNo pn
    | pn < 0 = error "Negative phone number"
    | otherwise = PhoneNo pn

data Phone = Phone {
    phoneType :: PhoneType,
    countryCode :: CountryCode,
    phoneNo :: PhoneNo
} deriving (Show, Eq)

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

parsePhoneType :: String -> PhoneType
parsePhoneType "" = error "Missing phone type"
parsePhoneType typeStr = 
    case typeStr of
        "WorkLandline"  -> WorkLandline
        "PrivateMobile" -> PrivateMobile
        "WorkMobile"    -> WorkMobile
        "Other"         -> Other
        _               -> error "Incorrect phone type"

parseCountryCode :: String -> [Integer] -> CountryCode
parseCountryCode "" _ = error "Empty country code"
parseCountryCode cc ccList = 
    let cleaned = removeCountryCodePrefix cc
    in if isInteger cleaned 
        then let n = read cleaned::Integer 
            in if n `elem` ccList
                then toCountryCode n
                else error "Unknown country code"
        
        else error "Incorrect country code"

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo n) = n


parsePhoneNo :: String -> PhoneNo
parsePhoneNo "" = error "Empty phone number"
parsePhoneNo pn = 
    if isInteger pn
        then let p = read pn::Integer
            in toPhoneNo p 
    else error "Incorrect phone number"

readPhone :: String -> String -> String -> [Integer] -> Phone

readPhone phonetypestr countrycodestr phonenostr ccodelist =
    let pt = parsePhoneType phonetypestr
        cc = parseCountryCode countrycodestr ccodelist
        pn = parsePhoneNo phonenostr
    in Phone pt cc pn