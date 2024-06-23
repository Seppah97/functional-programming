module Phone_book_map
  ( PhoneBook(..),
    Name(..),
    findEntries,
    addEntry,
    emptyBook
    -- rest of exported stuff
  ) where

import Phone_type2
import qualified Data.Map as Map
type Name = String
type PhoneBook = Map.Map Name [Phone]



findEntries :: Name -> PhoneBook -> [Phone]
findEntries name phonebook = 
    case Map.lookup name phonebook of
        Just phones -> phones
        Nothing -> []

addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry newName phonetype ccode phonenum ccodelist currentbook =
    let newPhone = readPhone phonetype ccode phonenum ccodelist
        phoneExists = any (any (\phone -> phoneNo phone == phoneNo newPhone)) (Map.elems currentbook)
        nameExists = Map.member newName currentbook
    in if nameExists && phoneExists
       then currentbook
       else Map.insertWith (++) newName [newPhone] currentbook

emptyBook :: PhoneBook
emptyBook = Map.empty