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

