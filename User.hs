--------------------------------Le meilleur de la création de module-----------------------------------
module SystemManagement.User(
    newUser,
    User,
    searchUser,
    listUser
)where
--------------------------------Fin de la création de module-----------------------------------
-----------------------------Le meilleur des importations--------------------------------------
import System.IO.Error
import Data.List
-----------------------------Fin des importations--------------------------------------
-------------------------------------Crétaion des type-----------------------------------------
data User = User{
    prenom::String,
    nom::String,
    organisation::String,
    email::Email
}deriving (Show)
instance Eq User where
    (==) = equalUser
instance Ord User where
    compare u1 u2 = compare (nom u1 ++ "" ++ prenom u1) (nom u2 ++ "" ++ prenom u2)
type Email = String
-------------------------------------Fin de création des type-----------------------------------------

------------------------------Les fonctions----------------------------------------
newUser :: [User]->String->String->String->String->User
newUser t p n o e = last $ newUsers t p n o e

newUsers::[User]->String->String->String->String->[User]
newUsers t p n o e = if not (verifyExistance t e) then t++[User p n o e] else error "UserDuplicatedException"

searchUser::[User]->String->[User]
searchUser tab c = filter (recherche c) tab
    where
        recherche::String->User->Bool
        recherche c u = nomi c (prenom u) || nomi c (nom u) || nomi c (email u) || nomi c (organisation u)

        nomi::String->String->Bool
        nomi _ [] = False
        nomi a (x:xs)
            | head a == x = a == x : take (length a - 1) xs
            | otherwise = nomi a xs
listUser::[User]->[User]
listUser = sort --Vu que notre type s'ordonne déjà en fonction de la concatenation du nom et du prénom

equalUser::User->User->Bool
equalUser a b = email a == email b

verifyExistance::[User]->Email->Bool
verifyExistance t e = if e `elem` map email t then error "UserDuplicatedException" else False

------------------------------Les fonctions----------------------------------------