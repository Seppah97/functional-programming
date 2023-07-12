

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Ord, Read)


data CountryCode = CountryCode Integer deriving (Eq, Ord)

instance Show CountryCode where
    show (CountryCode value) = "+" ++ show value

instance Num CountryCode where
    (CountryCode v1) + (CountryCode v2) = CountryCode (v1 + v2)
    (CountryCode v1) - (CountryCode v2) = CountryCode (v1 - v2)
    (CountryCode v1) * (CountryCode v2) = CountryCode (v1 * v2)
    fromInteger = CountryCode

data PhoneNo = PhoneNo Integer deriving (Eq, Ord)

instance Num PhoneNo where
    (PhoneNo v1) + (PhoneNo v2) = PhoneNo (v1 + v2)
    (PhoneNo v1) - (PhoneNo v2) = PhoneNo (v1 - v2)
    (PhoneNo v1) * (PhoneNo v2) = PhoneNo (v1 * v2)
    fromInteger = PhoneNo

instance Show PhoneNo where
    show (PhoneNo value) = show value


toCountryCode :: Integer -> CountryCode
toCountryCode n
    | n < 0 = error "Negative country code"
    | otherwise = CountryCode n

toPhoneNo :: Integer -> PhoneNo
toPhoneNo n
    | n < 0 = error "Negative phone number"
    | otherwise = PhoneNo n


data Phone = Phone {
    phoneType :: PhoneType,
    countryCode :: CountryCode,
    phoneNo :: PhoneNo
} deriving (Eq, Ord)

instance Show Phone where
    show (Phone pt cc pn) = show cc ++" " ++ show pn ++ " (" ++ show pt ++ ")"

makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone


makePhone = Phone