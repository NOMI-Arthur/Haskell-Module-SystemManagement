----------------------------------------Le meilleur des créations de module-----------------------------------
module SystemManagement.Conference(
    newConference,
    listConference,
    listConferencesBetween2Years,
    searchWork,
    submit,
    addAuthor,
    listAuthors
)where
----------------------------------------Fin de créations de module-----------------------------------

--------------------------------------Le meilleur des importations-----------------------------------------------
import System.IO
import Data.Char (toUpper)
import Data.List (sort)
import SystemManagement.User
--------------------------------------Fin des importations-----------------------------------------------

----------------------------------Le meilleur de création des types et types synonyms------------------------------
data Date = Date{mois::String,jour::Int} 
type Sponsor = String
instance Show Date where
    show = afficheDate
instance Ord Date where
    (<=) :: Date -> Date -> Bool
    (<=) = ordre
instance Eq Date where
    (==) :: Date -> Date -> Bool 
    (==) = egaliteDate
data Conference = Conference{
    idConference::String,
    nomConference::String,
    lieuConference::String,
    annee::Integer,
    dateDebut::Date,
    dateFin::Date,
    montant::Float,
    sponsor::[Sponsor]
}deriving (Show)
instance Eq Conference where
    (==) = egalite
instance Ord Conference where
    compare = comparaison

data TypeWork = Poster | Article deriving (Show, Eq)
type Titre = String
type AcronymeConference = String
type Email = String
type Identifiant = String

data Work = Work{
    idWork::Identifiant,
    titre::Titre,
    typeOeuvre::TypeWork,
    listOfAuthors::[Email]
}deriving (Show)

newtype ListeConferences = ListeConferences{conferences::[Conference]}deriving (Eq, Ord)
instance Show ListeConferences where
    (show) c = concatMap show (conferences c)
----------------------------------Fin de création des types------------------------------

--------------------------------------Le meilleur des création des fonctions-------------------------------
afficheDate::Date->String
afficheDate d = show (mois d) ++ "/" ++ show (jour d)

ordre::Date->Date->Bool
ordre d1 d2 
    | mois d1 == mois d2 = testJour (jour d1) (jour d2)
    | mois d1 <= mois d2 = True
    | otherwise = False
    where 
        testJour::Int->Int->Bool
        testJour = (<=)
egalite::Conference->Conference->Bool
egalite c1 c2 
    | (==) (nomConference c1) (nomConference c2) = True
    | otherwise = False
egaliteDate::Date -> Date -> Bool
egaliteDate d1 d2  = mois d1 == mois d2 && jour d1 == jour d2 

comparaison :: Conference->Conference->Ordering
comparaison c1 c2 = compare (nomConference c1) (nomConference c2)

newConference::String->String->Integer->Date->Date->Float->Conference
newConference n l a dd df m = 
    let
        composeur = words n
        tete = map head composeur
        i =  map toUpper tete ++ show a
    in Conference i n l a dd df m []

listConference:: ListeConferences ->IO ListeConferences
listConference lc =
    let
        sorted_conferences = sort (conferences lc)
    in return $ ListeConferences sorted_conferences

listConferencesBetween2Years::[Conference]->Integer->Integer->IO [Conference]
listConferencesBetween2Years tabConferences a1 a2 = 
    if a1 >= a2 then
        error "Mbout man !! Comment la premiere date peut elle etre superieur a la seconde?"
        else
            return $ filter (\ conf -> annee conf > a1 && annee conf < a2) tabConferences

addSponsor::Sponsor ->Conference->Conference
addSponsor s c
    | s `elem` sponsor c = c --La fonction n'a aucun effet sur la conférence
    | otherwise = Conference (idConference c) (nomConference c) (lieuConference c) (annee c) (dateDebut c) (dateFin c) (montant c) (sponsor c ++ [s])

listSponsor::Conference->[Sponsor]
listSponsor = sponsor

submit::[Conference]->AcronymeConference->Titre->TypeWork->Email->Identifiant
submit t a _ _ _ = a ++ "-" ++ show (1 + if taille == 1 then taille + 0 else taille + 1)
    where 
        taille = length (filter ((==) a) (map idConference t)) -- On suppose dans le if que l'acronyme existe au moins une fois dans la liste de conférences

searchWork :: Identifiant -> ListeConferences -> ListeConferences
searchWork i l = 
    let
        t = conferences l
        confe = takeWhile (/='-') i
    in ListeConferences $ filter (\ tampon -> confe == idConference tampon) t
addAuthor ::Identifiant-> Email -> [User]  -> Work  -> IO Work 
addAuthor id eml tabUser travail =
    let
        t = searchUser tabUser eml 
    in if null t then return travail else return $ Work (idWork travail) (titre travail) (typeOeuvre travail) (listOfAuthors travail ++  [eml]) 

listAuthors:: Identifiant -> [Work] -> [Email]
listAuthors id [x] 
    | id == idWork x = listOfAuthors x
listAuthors id (x:xs) = if id == idWork x then listOfAuthors x else listAuthors id xs
--------------------------------------Fin de création des fonctions-------------------------------