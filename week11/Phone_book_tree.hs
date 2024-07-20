module Phone_book_tree 
(
    PhoneBook(Empty,Node),
    Name,
    addEntry,
    findEntries,
    emptyBook
) where

import Phone_type2
type Name = String
data PhoneBook = Empty | Node String [Phone] PhoneBook PhoneBook deriving (Show,Eq)

findEntries :: Name -> PhoneBook -> [Phone]
findEntries _ Empty = []
findEntries name (Node n phones left right)
    | name == n = phones
    | name < n = findEntries name left
    | name > n = findEntries name right


addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook

addEntry newName phonetype ccode phonenum ccodelist Empty =
    Node newName [newPhone] Empty Empty
  where
    newPhone = readPhone phonetype ccode phonenum ccodelist

addEntry newName phonetype ccode phonenum ccodelist (Node n phones left right)
    | newName == n = 
        let newPhone = readPhone phonetype ccode phonenum ccodelist
            phoneExists = any (\phone -> phoneNo phone == phoneNo newPhone) phones
        in if phoneExists
           then Node n phones left right
           else Node n (newPhone:phones) left right
    | newName < n  = Node n phones (addEntry newName phonetype ccode phonenum ccodelist left) right
    | newName > n  = Node n phones left (addEntry newName phonetype ccode phonenum ccodelist right)



emptyBook :: PhoneBook
emptyBook = Empty