--------------------------Définition du module------------------------------------------
module SystemManagement.Revision(
    newRevision,
    calculatePointMedium,
)where
--------------------------Fin de Définition du module-----------------------------------

---------------------------Le meilleur des importations---------------------------------
import System.IO
import SystemManagement.Conference
---------------------------Fin des importations---------------------------------

----------------------Le meilleur des types personalisés------------------------
type Utilisateur = String
type Oeuvre = String

data Revision = Revision{
    idOeuvre::Oeuvre,
    email::Utilisateur,
    commentaire::String,
    notation::Double
}
----------------------Fin des types personalisés------------------------

---------------------Le meilleur des fonctions -----------------------------------
newRevision::Oeuvre->Utilisateur->String->Double->IO Revision
newRevision _ u _ n 
    | ((n > 5) || (n < 1)) || null u = error "RevisionRefusedException"
newRevision o u c n = return $ Revision o u c n

calculatePointMedium::Oeuvre->[Revision]->Double
calculatePointMedium o tab =  sum (map notation (t tab)) / fromIntegral (length (t tab))
    where
        t = filter (\ tampon -> idOeuvre tampon == o)
---------------------Fin des fonctions -----------------------------------