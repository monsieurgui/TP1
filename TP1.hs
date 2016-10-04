module TP1 where

--Importation de modules nécessaires
import Data.List
import Data.Char
import Data.Function
import Control.Exception
import Data.Typeable
import Data.Time.Clock
import Data.Time.Calendar

--Déclaration de synonimes de types utiles
type Annee = Integer
type Mois = Int
type Jour = Int
type Nom = String
type Adresse = String
type Budget = Int
type Titre = String
type Cout = Int
type Duree = Int
type NombreActeurs = Int
type RevenuMinimum = Int

-- les composantes d'une date sont l'année, le mois et le jour
data Date' = Date' Annee Mois Jour deriving Show
-- les composantes d'un réalisateur sont son nom et son adresse
data Realisateur = PasDeRealisateur | Realisateur Nom Adresse  deriving (Show, Typeable, Eq)
-- les composantes d'une maison de production sont son nom, son budget et la liste des films qu'elle a produit
data MaisonDeProd = PasDeProducteur | MaisonDeProd Nom Budget ListeFilms deriving (Show, Typeable, Eq)
-- Un Film se caractérise par son titre, son type, son réalisateur, son coût, sa durée, le nombre d’acteurs, son budget et liste des cinémas dans lequel le film est projeté
data Film = Film Titre TypeF Realisateur MaisonDeProd Cout Duree NombreActeurs Budget
data TypeF = Action | Drame | Humour | Horreur | Policier deriving (Show, Eq)
-- Un Cinéma se caractérise par son nom, son adresse, la liste des films, le nombre d’entrées totales pour chaque film et le prix pour chaque entrée d'un film
data Cinema = Cinema Nom Adresse RepertoireFilms deriving Eq
-- les composantes d'un acteur sont son nom, son sexe, le revenu minimum qu'il accepte pour jouer dans un film, la date de son début comme acteur, ses restrictions et la liste des films dans lesquls il a joué
data Acteur = Acteur Nom Sexe RevenuMinimum Date' Restriction ListeFilms
data Sexe = M | F deriving (Show, Eq)

--- Fonctions définies pour des fin d'instanciation des types Acteur, Cinema et Film pour permettre leur affichage
showActeur (Acteur nomA sexeA revenuMinA dateA _ _) = "(Acteur" ++ " " ++ show nomA ++ " " ++ show sexeA ++ " " ++ show revenuMinA ++ " " ++ show dateA ++")"
showCinema (Cinema nomC adresseC repertoireC) = "(Cinema" ++ " " ++ show nomC ++ " " ++ show adresseC ++ " " ++ show repertoireC ++")"
showFilm (Film titreF typeF realisateurF _ coutF dureeF nbreActeursF budgetF) = "(Film" ++ " " ++ show titreF++ " " ++ show typeF ++ " " ++ show realisateurF ++ " " ++ show coutF ++ " " ++ show dureeF ++ " " ++ show nbreActeursF ++ " " ++ show budgetF ++")"

--instanciation de types
instance Show Acteur where
 show  = showActeur
instance Show Cinema where
 show  = showCinema
instance Show Film where
 show  = showFilm

--Autre synonimes de types utiles
type RepertoireFilms = [(Film, Int, Int)]
type Restriction = Film -> Bool
type ListeFilms = [Film]
type Critere = Acteur -> Bool ---- On utilisera les listes de critères pour sélectionner les acteurs d'un film

-- Pour la gestion des exceptions
data Elements = PasAssezDelements deriving (Show, Typeable)
data NosExceptions = BudgetInsuffisant | DejaProduit | PasAssezDacteurs deriving (Show, Typeable, Eq)
instance Exception NosExceptions
instance Exception Elements
instance Exception MaisonDeProd
instance Exception Realisateur

-- Quelques donctions d'accès (getters)
-- la fonction titre retourne le titre du film passé en paramètre
titreFilm :: Film -> Titre
titreFilm (Film titreF _ _ _ _ _ _ _ ) = titreF
-- la fonction experience retourne le nombre d'années d'expérience de l'acteur donne en argument
experience :: Acteur -> Integer
experience (Acteur _ _ _ (Date' adebut _ _) _ _) = 2016 - adebut
-- la fonction revenuMin retourne le revenu minimum demande par l'acteur donné en argument
revenuMin (Acteur _ _ revenuM _ _ _) = revenuM
-- la fonction nomActeur retourne le nom d'un acteur.
nomActeur (Acteur nom _ _ _ _ _) = nom
-- la fonction restriction retourne la composante restriction de l'argument de type acteur
restriction (Acteur _ _ _ _ r _) = r
-- la fonction nomRealisateur retourne le nom du realisateur.
nomRealisateur (Realisateur nom _) = nom
-- la fonction adresseRealisateur retourne l'adresse du realisateur.
adresseRealisateur (Realisateur  _ adresse) = adresse

-- Autres instanciation de types
instance Ord Film where
                        compare x y
                            | titreFilm x == titreFilm y = EQ
                            | titreFilm x < titreFilm y = LT
                            | otherwise = GT
instance Ord Acteur where
                        compare x y
                            | nomActeur x == nomActeur y = EQ
                            | nomActeur x < nomActeur y = LT
                            | otherwise = GT
eqActeur (Acteur nom1 _ _ _ _ _) (Acteur nom2 _ _ _ _ _ ) = nom1 == nom2
eqFilm (Film titre1 _ _ _ _ _ _ _ ) (Film titre2 _ _ _ _ _ _ _ ) = titre1 == titre2
instance Eq Acteur where
                         (==)  = eqActeur
instance Eq Film where
                         (==)  = eqFilm


-- ***********************************************************************************************************--
-- *********************************** VOUS DEVEZ COMPLETEZ CETTE PARTIE *************************************--
-- ***********************************************************************************************************--

{-	1- la fonction selectionActeursCriteres retourne la liste formée des acteurs qui satisfont tous les critères de la liste de critères
    Entrées: Liste de critères, Liste d'acteurs (dans cette ordre)
    Sortie: Liste d'acteurs
	3pts-}



{-  2- la fonction selectionActeursFilm retourne la liste formée des acteurs pour lesquels le film passé en paramètre satisfait leurs restrictions
    Entrées: un Film, une Liste d'acteurs (dans cette ordre)
	Sortie: Liste d'acteurs
    3pts	-}
    selectionActeursFilm :: Film -> [Acteur] -> [Acteur]
    selectionActeursFilm [] _ = []
    selectionActeursFilm x (y:ys) =
                                filter (titreFilm x == titreFilm y) ys 



{-  3- la fonction prendrePremier prend un couple (n,l) et retourne les n premiers elements de la liste l.
    si l comporte moins de n éléments, on invoque l'exception PasAssezDelements. Elle pourrait vous être utile à plusieurs endroits.
    Entrées: : (n,l)
    Sortie: les n premiers éléments de l-}
prendrePremiers (0,l) = []
prendrePremiers (n, []) = throw PasAssezDelements
prendrePremiers (n, (x:xs)) = x : prendrePremiers(n-1, xs)

{-  4- la fonction sommeSalaires fait la somme des revenus minimums demandés par les acteurs d'une liste . Elle pourrait vous être utile à plusieurs endroits.
    Entrée: Liste d'acteurs
    Sortie: somme des revenus minimums -}
sommeSalaires lacteurs = sum (map revenuMin lacteurs)

{- 5- la fonction produire(maison, film) retourne le couple formée
        1) d'une MaisonDeProd de même nom que maison dont le budget a été diminué du coût du film
        2) d'un Film egal à film sauf que la composante producteur est égale à la maison de production ci-dessus .
   De plus:
        - Si le coût de film est supérieur strictement au budget de maison alors, l'exception BudgetInsuffisant est invoquée
        - Si la composante realisateur de film est PasDeRealisateur on invoque l'exception PasDeRealisateur
        - Si la composante producteur de film est autre chose que PasDeProducteur on invoque l'exception DejaProduit.
   Remarque: n'oubliez pas qu'une fois qu'une maison produit un film, ce film doit s'ajouter à sa liste de films déjà produits
   Entrée: Couple formé d'une maison de production et d'un film
   Sortie: Couple formé d'une maison de production et d'un film
   10pts-}






{-  6- la fonction acteursSelectionnes (film, lcriteres, lacteurs) retourne la liste des acteurs sélectionnés pour le film donné en paramètre.
        - tous les acteurs retournés doivent satisfaire à tous les critères de lcriteres
        - le film satisfait toutes leurs restrictions
    Les validations suivantes doivent être faites:
        - Si le réalisateur de film est PasDeRealisateur on invoque l'exception PasDeRealisateur
        - Si le producteur de film est PasDeProducteur on invoque l'exception PasDeProducteur
        - S'il n'y a pas assez d'acteurs on invoque l'exception PasAssezDacteurs
        - Si le total des salaires minimums demandes par ces acteurs est superieur au budget de film on invoque l'exception BudgetInsuffisant
	Remarque: n'oubliez pas qu'une fois un acteur sélectionné, ce film doit s'ajouter à sa liste de films
	Entrée: Triplet formé d'un film, d'une liste de critères et d'une liste d'acteurs
    Sortie: Liste d'acteurs
    10pts	-}

-- *************************************************************************************************************
-- *********************************** Fonctions d'aide pour cette question ************************************
-- *************************************************************************************************************
-- cette fonction extrait le nombre d'acteurs d'un film
getnbacteurF (Film _ _ _ _ _ _ nbacteurF _ ) = nbacteurF
getbudgetF (Film _ _ _ _ _ _ _ budgetF ) = budgetF
-- cette fonction ajoute un film à la liste des films des acteurs qui joueront dans ce film
ajouterFilmActeurs :: (Film, [Acteur]) -> [Acteur]
ajouterFilmActeurs (_, [])= []
ajouterFilmActeurs (film, ((Acteur nomA sexeA revenuM dateA restrictionA listeFilms ):lacteurs)) = let acteur = (Acteur nomA sexeA revenuM dateA restrictionA (listeFilms++[film]) )
                                                                                                  in [acteur]++ (ajouterFilmActeurs (film, lacteurs))
-- *************************************************************************************************************
-- ************************************** Fin des fonctions d'aide *********************************************
-- *************************************************************************************************************





{-  7- la fonction affectationDesRoles (film, lcriteres, lacteurs) retourne un couple dont
       - la première composante est formée d'un Film égal à film sauf que son budget a été remplacé par la somme des revenus minimums des acteurs choisis (voir deuxième
	     composante) et son coût a été diminué de la différence entre le budget initial et le nouveau budget
       - la seconde composante est formée de la liste des n premiers acteurs (ou n est le nombre d'acteurs dans le film) qui satisfont:
         * Tous les critères de lcriteres
         * et Pour lesquels film satisfait pour chacun, toutes les restrictions
    De plus:
       - Si le réalisateur de film est PasDeRealisateur on invoque l'exception PasDeRealisateur
       - S'il n' y a pas assez d'acteurs on invoque l'exception PasAssezDacteurs
       - Si le total des salaires minimums demandes par ces acteurs est supérieur au budget du film, on invoque l'exception BudgetInsuffisant.
	Entrées: Triplet formé d'un film, d'une liste de critères et d'une liste d'acteurs
    Sortie: Couple formé d'un film et d'une liste d'acteurs
	8pts-}





{-  8- la fonction selectionnesSansRole retourne les acteurs d'une liste d'acteurs qui n'ont pas été selectionnés pour un rôle (car il y avait trop de candidats) mais qui satisfaisaient quand même :
       - Tous les criteres de la liste des critères
       - Pour lesquels film satisfait leur restrictions
    Sinon selectionnesSansRole invoque les mêmes exception que affectationDesRoles.
    ATTENTION: On suppose qu'il n'y a qu'un seul acteur qui porte chaque nom
	Entrées: Triplet formé d'un film, d'une liste de critères et d'une liste d'acteurs
    Sortie: liste d'acteurs -}
selectionnesSansRole :: (Film, [Critere], [Acteur]) -> [Acteur]
selectionnesSansRole (film, lcriteres, lacteurs) = let acSelect = acteursSelectionnes (film, lcriteres, lacteurs)
                                                   in if length acSelect == getnbacteurF film then [] else drop (getnbacteurF film) acSelect

{-  9- La fonction exclureActeurs (acteur, lacteurs) retourne la  liste d'acteurs passée en paramètre, mais après y avoir enlevé l'acteur passé en paramètre.
	Entrée: Couple formé d'un acteur et d'une liste d'acteurs
    Sortie: liste d'acteurs -}
exclureActeurs :: (Acteur, [Acteur]) -> [Acteur]
exclureActeurs (_,[])= []
exclureActeurs (acteur, (x:xs)) = if nomActeur (acteur) == nomActeur (x) then xs else x: exclureActeurs (acteur, xs)

{-  10- la fonction selectionActeursCriteresNouvelle (lcriteres, lacteurs) retourne la liste formée des acteurs qui satisfont les critères de la liste de critères dans cet ordre:
       - le premier qui satisfait le premier critère,
	   - le premier qui satisfait le deuxième critère
	   - etc...
	Entrées: Triplet formé d'un film, d'une liste de critères et d'une liste d'acteurs
    Sortie: liste d'acteurs
	8pts-}




{-  11- la fonction acteursSelectionnesNouvelle (film, lcriteres, lacteurs) retourne la liste des acteurs selectionnes pour le film donne en paramètre en considérant:
      - le premier acteur qui satisfait le premier critere de Lcriteres, le premier acteur qui satisfait le deuxième critère etc.
      - que le film satisfait leur restrictions
    et fait les validations suivantes:
      - Si le réalisateur de film est PasDeRealisateur on invoque l'exception PasDeRealisateur
      - Si le producteur de film est PasDeProducteur on invoque l'exception PasDeProducteur
      - S'il n'y a pas assez d'acteurs on invoque l'exception PasAssezDacteurs
      - Si le total des salaires minimums demandés par ces acteurs est supérieur au budget de film on invoque l'exception BudgetInsuffisant.
	Entrée: Triplet formé d'un film, d'une liste de critères et d'une liste d'acteurs
    Sortie: liste d'acteurs
	8pts-}





{-  12- La fonction affectationDesRolesNouvelle (film, lcriteres, lacteurs) retourne un couple dont
      - la première composante est formée d'un Film égal à film sauf que son budget a été remplacé par la somme des salaires minimums des acteurs choisis (voir deuxième
	    composante) et son coût a été diminué de la différence entre le budget initial et le nouveau budget
      - la seconde composante est formée de la liste des n premiers éléments (ou n est le nombre de rôles dans film) de lacteurs qui satisfont:
	    * Le premier acteur qui satisfait le premier critère de lcritres, le premier acteur qui satisfait le deuxième critère etc.
        * Le film doit satisfaire la restriction de chacun des acteurs choisis
    De plus:
      - Si le réalisateur de film est PasDeRealisateur on invoque l'exception PasDeRealisateur
      - Si le producteur de film est PasDeProducteur on invoque l'exception PasDeProducteur
      - S'il n' y a pas assez d'acteurs on invoque l'exception PasAssezDacteurs
      - Si le total des salaires minimums demandes par ces acteurs est supérieur au budget du film, on invoque l'exception BudgetInsuffisant
	Entrée: Triplet formé d'un film, d'une liste de critères et d'une liste d'acteurs
    Sortie: Couple formé d'un film et de la liste d'acteurs
	6pts-}




{- 13 - la fonction attribuerFilmCinema prend un couple formé d'un cinéma, un film et le prix (un entier) d'une entrée puis retourne ce cinéma avec son repertoire modifié. NB: une nouvelle attribution ajoute le film toujours en tête du répertoire.
   Entrée: triplet formé  d'un cinéma, un film et le prix d'une entrée
   Sortie: le cinéma
   5pts-}
attribuerFilmCinema :: (Cinema, Film, Int) -> Cinema


-- ************************************************************************************* ----
-- *  Début méthodes de services utiles pour certaines des prochaines questions             * ----
-- ************************************************************************************* ----
--pour contourner le type Maybe retourné par elemIndex, il y a deux solutions: soit utilisé fromJust pour récupérer
--la valeur retournée par le constructeur Just (si on n'a pas à gérer le Nothing) ou utiliser le code suivant:
trouverLaPosition :: (Eq a) => a -> [a] -> Int
trouverLaPosition valeur = (\(Just i)->i) . elemIndex valeur

{- Modifier une entrée du répertoire (de position connue) en ajustant le prix ou les entrées d'un film  -}
modifierRepertoirePos:: Int -> [(Film, Int, Int)] -> (Film, Int, Int) -> [(Film, Int, Int)]
modifierRepertoirePos i repertoireC nouvelleValeur =
  let (ys,zs) = splitAt (i-1) repertoireC in  ys ++ [nouvelleValeur] ++ tail zs

-- listePairesTriee2e trie une liste de paires en ordre croissant du deuxième élément de la paire
listePairesTriee2e :: Ord b => [(a,b)] -> [(a,b)]
listePairesTriee2e [] = []
listePairesTriee2e ((x1,x2):xs) = (listePairesTriee2e plusGrand) ++ [(x1,x2)] ++ (listePairesTriee2e plusPetit)
    where
        plusPetit = filter ((<x2).snd) xs
        plusGrand = filter ((>=x2).snd) xs

--gestion d'accès à un triplet
premier (a,_,_) = a
deuxieme (_,b,_) = b
troisieme (_,_,c) = c

-- ************************************************************************************* ----
-- *  Fin méthodes de services utiles pour certaines des prochaines questions          * ----
-- ************************************************************************************* ----

{- 14a - le producteur décide de changer le prix d'un film donné dans un cinéma donné  -}
modifierPrixFilmCinema :: (Cinema, Film, Int) -> Cinema
modifierPrixFilmCinema ((Cinema nomC adrC repertoireC), film, prix) = (Cinema nomC adrC (modifierRepertoirePos indice repertoireC (film, nbE, prix)))
	where
		nbE = deuxieme (repertoireC !! (trouverLaPosition film (premier (unzip3 repertoireC))))
		indice = trouverLaPosition film (premier $ unzip3 repertoireC)

{- 14b - un cinema ajuste le nombre d'entrée pour un film donné 3pts-}
ajusterEntreesFilmCinema :: (Cinema, Film, Int) -> Cinema


{- 14c - Quel est le revenu total d'un film dans un cinema donnée? Retourner 0 si la liste des cinéma est vide ou si le film n'existe pas
4pts-}
revenuFilmCinema :: Film -> Cinema -> Int


{- 14d -  reconstitution du répertoire d'un cinéma après perte de données. Un cinéma doit absolument reconstituer son répertoire suite à une perte de celui-ci.
seul recours: les 3 listes distinctes récupérées chez les producteurs, une contenant les films attribués, l'autre pour les entrées et la dernière pour les prix.
Heureusement que ces informations en provenance des producteurs, sont bien allignées. Écrire la fonction restorerRepertoire qui fait le travail
pour un cinéma donné.
 5pts-}
restorerRepertoire :: Cinema -> [Film] ->[Int] ->[Int]-> Cinema

{- 14e Quel est le niveau moyen d'achalandage d'un cinéma donné (soit le nombre moyen d'entrées par film); nom de la fonction: achalandage
4pts-}



{-  15a - Meilleurs films... En considérant que la valeur d'un film est donnée par son revenu total, écrire une fonction qui retourne étant donnée une liste de
films et une liste de cinemas, retourne une liste de pairs (film, revenuTotal) triée par ordre croissant de la valeur de chaque film.
7pts
 -}
meilleursFilms :: [Film] -> [Cinema] -> [(Film, Int)]


{-  15b - Meilleur(s) acteur(s)... Il s'agit de l'acteur ou des acteurs ayant joué dans LE(S) meilleur(s) film(s) et qui a(ont) le plus d'expérience. Attention: Juste pour cette question, la notion
d'expérience d'un acteur s'estime uniquement par le nombre de films dans lesquels il a joué (et non par rapport au nombre d'années passé dans l'industrie.
Le resultat est une liste de paires dont le premier élément est le nom de l'acteur, et le second le nombre de films de ce dernier. Cette liste doit être limitée seulement aux MEILLEURS acteurs.
 8pts -}
 -- ************************************************************************************************************
-- *********************** Début: Fonctions d'aide pour cette question   ***************************************
-- *************************************************************************************************************
-- cette fonction retourne une liste formé de couples dont le deuxième élément est le nombre de films dans lequel l'acteur qui est le premier élément a joué.
experienceActeurs :: [Acteur] -> [(Acteur, Int)]
experienceActeurs [] = []
experienceActeurs ((Acteur nomA sexeA revenuM dateA restrictionA listeFilms ):lacteurs) = ((Acteur nomA sexeA revenuM dateA restrictionA listeFilms), length listeFilms): (experienceActeurs lacteurs)

-- cette fonction extrait le ou les acteurs (si ils ont joués dans le même nombre de films) ayant joués dans le plus grand nombre de films
lesPlusExperimentes :: [(Acteur, Int)] -> [(Acteur, Int)]
lesPlusExperimentes [] = []
lesPlusExperimentes (l@((act, nbreFilms):ls)) = [(act, nbreFilms) | (act, nbreFilms) <- l, (nbreFilms == snd(head ls))]

-- joueDans retourne True si l'acteur passé en paramètre joue dans au moins un film de la liste des films passée en paramètre
joueDans :: Acteur -> [Film] -> Bool
joueDans (Acteur _ _ _ _ _ []) _ = False
joueDans _ [] = False
joueDans act@(Acteur _ _ _ _ _ facteur) (f:fs) =  (elem f facteur) || (joueDans act fs)
-- *************************************************************************************************************
-- *********************** Fin: Fonctions d'aide pour cette questions      *************************************
-- *************************************************************************************************************
meilleursActeurs :: [Acteur] -> [Film] -> [Cinema] -> [(Acteur, Int)]



{- 15c - écrire la fonction box_office qui calcule et retourne le revenu total d'un film pour une liste de salle de cinema donnée. Tenir compte du prix du film pour chaque cinema et du nombre d'entrées.
Retourner 0 si la liste des cinéma est vide ou si le film n'existe pas
7pts-}
box_office :: Film -> [Cinema] -> Int


{-  15d- La fonction profit retourne vraie si le film diffusé dans les salles de cinéma données a engendré des profits et faux la cas échéant.
il y a profit pour un film si son revenu total dans l'ensemble des cinemas de la liste donnée est est supérieur à son coût de production.
4pts
-}
