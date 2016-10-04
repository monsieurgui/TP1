{-
Ce fichier comporte les données et les tests relatifs au TP1. Vous devez pouvoir tester votre travail sans avoir à le modifier. En d'autres termes,
votre travail devra fonctionner sans aucun changement de ce fichier.

Vous pouvez l'utiliser en deux modes: interprété et compilé.

En mode inteprété, vous devez d'abord charger votre module TP1.hs avant de charger ce fichier. Une fois les deux chargés, vous pouvez de manière interactive
soit exécuter les fonctions testXX un à la fois ou alors exécuter la fonction main qui elle exécute tous les tests. L'exécution test par test peut vous 
aider à une mise au point progressive de votre projet. Les tests ne sont pas disposés en ordre mais vous pouvez facilement identifier le(s) test(s) 
qui correspond(ent) à chacune des questions. Je vous rapplelle qu'ils ne sont pas exhaustifs et c'est à vous de vous assurer de tester toutes les possibilités.

En mode compilé, vous pouvez compiler votre programme comme suit: ghc --make testTP1.hs (assurez-vous que les deux fichiers (TP1.hs et testTP1.hs) soient dans le même répertoire).
La compilation générera un fichier testTP1.exe que vous pouvez exécuter directement.
-}

-- importation des modules nécessaires
import TP1 
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

-----------                      --------------
-----------  LA BASE DE DONNÉES  --------------
-----------                      --------------
-- les réalisateurs
woody = (TP1.Realisateur "Allen Woody" "New York New York")
denis = (TP1.Realisateur "Arcan Denis" "Montreal Quebec")
joss = (TP1.Realisateur "Joss Whedon" "New York -")
jon = (TP1.Realisateur "Jon Favreau" "New York Queens")
guy = (TP1.Realisateur "Guy Ritchie" "Hatfield Royaume-Uni")

-- les acteurs
arnold = (TP1.Acteur "Schwarzennegger Arnold" M 1000 (Date' 1978 4 1) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> typeF == Action && realisateurF /= PasDeRealisateur && producteurF /= PasDeProducteur && coutF > 100000) []) 
meryll = (TP1.Acteur "Streep Meryll" F 1500 (Date' 1984 9 1) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> typeF == Action && realisateurF /= PasDeRealisateur && dureeF > 90 || nbacteursF == 2) []) 
maxi = (TP1.Acteur "Farfelu Maximo" M 1 (Date' 1991 1 1) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> (length titreF) > 5 || nomRealisateur (realisateurF) == "Allen Woody" || (take 4 (adresseRealisateur realisateurF)) == "L.A." && coutF > 100000) []) 
julia = (TP1.Acteur "Roberts Julia" F 2500 (Date' 1978 4 12) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> (typeF == Drame || typeF == Humour) && ( budgetF < 4000)) []) 
reno = (TP1.Acteur "Reno Jean" M 100 (Date' 1975 8 15) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> typeF == Drame) [])
sharon = (TP1.Acteur "Stone Sharon" F 2000 (Date' 1990 8 15) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> ((length titreF) > 4 && (length titreF) < 150) && realisateurF /= woody ) [])
robert = (TP1.Acteur "Robert Downey Jr" M 4500 (Date' 1987 10 1) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> budgetF >= 100000 ) [iron, sherlock, avengers])
scarlett = (TP1.Acteur "Scarlett Johansson" F 4000 (Date' 2003 5 9) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> (typeF == Action) || (typeF == Humour))  [avengers])
chris = (TP1.Acteur "Chris Evans " M 2700 (Date' 2001 9 5) ( \(Film titreF typeF realisateurF producteurF coutF dureeF nbacteursF budgetF) -> typeF == Action)  [])

-- les maisons de productions
flash = (TP1.MaisonDeProd "Flash Films" 100000 [])
ua = (TP1.MaisonDeProd "United Artists" 1000000 [])
flop = (TP1.MaisonDeProd "Flop Films" 50 [])
marvel = (TP1.MaisonDeProd "Marvel Studios" 2000000 [iron])
silver = (TP1.MaisonDeProd "Silver Picture" 900000 [sherlock] )

-- les films
destruction = (TP1.Film "Destruction" Action PasDeRealisateur PasDeProducteur 100000 95 150 60000)
nuit = (TP1.Film "Une nuit a Casablanca" Humour woody PasDeProducteur 2000 100 24 1000)
coupe = (TP1.Film "Coupe de coupe !" Horreur denis flop 45000 45 2 10000)
wind = (TP1.Film "Gone with the wind !" Drame woody flash 450 120 4 100 )
wind2 = (TP1.Film "Gone with the wind, the sequell !" Drame woody flash 4500 120 3 3999) 
avengers = (TP1.Film "The Avengers" Action joss marvel 200000 143 4 110000 ) 
iron = (TP1.Film "Iron Man" Action jon marvel 160000 126 3 100000)
sherlock = (TP1.Film "Sherlock Holmes" Policier guy silver 90000 128 1 90000 )

-- les cinemas
starcite = (TP1.Cinema "Starcite" "Montreal Quebec" [(avengers, 5800, 10), (wind, 800, 10), (nuit, 1555, 10), (iron, 4100, 10), (sherlock, 4900, 10), (coupe, 1500, 10)])
welcome = (TP1.Cinema "Wecolme" "New york" [(sherlock, 25500, 8), (iron, 36300, 11), (avengers, 40020, 10)])
megarama = (TP1.Cinema "Megarama" "Casablanca" [(nuit, 500, 5), (iron, 1050, 5), (coupe, 700, 5)])
leverdict = (TP1.Cinema "Le verdict" "nulle part" [])

-- les critères
critereMasculin (Acteur nomA sexeA revenuMinA dateA restrictionA _) = sexeA == M 
critereFeminin (Acteur nomA sexeA revenuMinA dateA restrictionA _) = sexeA == F
critereExperienceMin nbAnnees acteur = experience (acteur) > nbAnnees
critereExperienceMax nbAnnees acteur = experience (acteur) <= nbAnnees
critereRevenuMin min acteur = (revenuMin acteur) > min
critereRevenuMax max acteur = (revenuMin acteur) <= max
critereNom nom acteur = (nomActeur acteur) /= nom

-- listes de ciné. d'acteurs et de films dans le système
listeCinemas = [starcite, welcome, megarama, leverdict] :: [Cinema]
listeActeurs = [chris, scarlett, robert, sharon, reno, julia, maxi, meryll, arnold]
listeFilms = [destruction, nuit, coupe, wind, wind2, avengers, iron, sherlock]

-----------                      --------------
-----------  LES TESTS           --------------
-----------                      --------------

-- quelques requêtes utilitaires liées aux tests 8, 22 et 28 (pour faire court)
r8 = TP1.affectationDesRoles (coupe, [critereFeminin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris])
r22 = TP1.affectationDesRolesNouvelle (coupe, [(\x -> (critereFeminin x) || (critereNom "Schwarzennegger Arnold" x)), (\x -> (critereMasculin x) && (critereExperienceMin 5 x)) ], [arnold, meryll, maxi, julia, reno, sharon, robert, chris])
r28 = TP1.attribuerFilmCinema ((attribuerFilmCinema (leverdict, destruction, 15)), nuit,10)

-- traitement de l'exécution et l'affichage des résultats de tous les tests (après compilation ou par l'appel explicite de la fonction main)
main = do
         print "************Test du TP1*****************"
         print "****************MENU********************"
         result <- try (print(TP1.produire (flash, destruction))) :: IO (Either Realisateur () )
         case result of
           Left ex  -> putStrLn $ "test01 = " ++ show (ex == test01)
           Right val -> putStrLn $ "test01 = False " 
	 putStrLn ( "test02 = " ++ show (test02))
	 putStrLn ( "test03 = " ++ show (test03))
	 do
	   result <- try (print (TP1.produire (flop, nuit))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test04 = " ++ show (ex == test04)
            Right val -> putStrLn $ "test04 = False " 
	 do
	   result <- try (print(TP1.affectationDesRoles (destruction, [critereMasculin], [arnold, meryll, maxi, julia, reno, sharon, scarlett,chris]))) :: IO (Either Realisateur () )
	   case result of
            Left ex  -> putStrLn $ "test05 = " ++ show (ex == test05)
            Right val -> putStrLn $ "test05 = False"
	 do
	   result <- try (print(TP1.affectationDesRoles (nuit, [critereMasculin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either MaisonDeProd () )
	   case result of
            Left ex  -> putStrLn $ "test06 = " ++ show (ex == test06)
            Right val -> putStrLn $ "test06 = False"
	 do
	   result <- try (print(TP1.affectationDesRoles (coupe, [critereMasculin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test07 = " ++ show (ex == test07)
            Right val -> putStrLn $ "test07 = False"
	 putStrLn ( "test08 = " ++ show (test08))
	 do
	   result <- try (print(TP1.affectationDesRoles (wind, [ (critereExperienceMin 5), (critereExperienceMax 25)], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test09 = " ++ show (ex == test09)
            Right val -> putStrLn $ "test09 = False"
	 do
	   result <- try (print(TP1.selectionnesSansRole (destruction, [critereMasculin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either Realisateur () )
	   case result of
            Left ex  -> putStrLn $ "test10 = " ++ show (ex == test10)
            Right val -> putStrLn $ "test10 = False"
	 do
	   result <- try (print(TP1.selectionnesSansRole (nuit, [critereMasculin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either MaisonDeProd () )
	   case result of
            Left ex  -> putStrLn $ "test11 = " ++ show (ex == test11)
            Right val -> putStrLn $ "test11 = False"
         do
	   result <- try (print(TP1.selectionnesSansRole (coupe, [critereMasculin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test12 = " ++ show (ex == test12)
            Right val -> putStrLn $ "test12 = False"
	 putStrLn ( "test13 = " ++ show (test13))
	 do
	   result <- try (print(TP1.selectionnesSansRole (wind, [ (critereExperienceMin 5 ), (critereExperienceMax 25)], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test14 = " ++ show (ex == test14)
            Right val -> putStrLn $ "test14 = False"
	 putStrLn ( "test15 = " ++ show (test15))
	 putStrLn ( "test16 = " ++ show (test16))
	 putStrLn ( "test17 = " ++ show (test17))
	 putStrLn ( "test18 = " ++ show (test18))
	 do
	   result <- try (print(TP1.affectationDesRolesNouvelle (wind, [critereNom "Schwarzennegger Arnold", (\x -> (critereRevenuMin 1000 x) && (critereRevenuMax 10000 x))], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test19 = " ++ show (ex == test19)
            Right val -> putStrLn $ "test19 = False"
	 putStrLn ( "test20 = " ++ show (test20))
	 do
	   result <- try (print(TP1.affectationDesRolesNouvelle (wind2, [(\x -> (critereFeminin x) && (critereRevenuMin 100 x)), (\x -> (critereMasculin x) && (critereRevenuMax 1000000 x)) ], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test21 = " ++ show (ex == test21)
            Right val -> putStrLn $ "test21 = False"
	 putStrLn ( "test22 = " ++ show (test22))
	 do
	   result <- try (print(TP1.affectationDesRoles (avengers, [(\x -> critereRevenuMin 2500 x)], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test23 = " ++ show (ex == test23)
            Right val -> putStrLn $ "test23 = False"
	 putStrLn ( "test24 = " ++ show (test24))	
         putStrLn ( "test25 = " ++ show (test25))	
         putStrLn ( "test26 = " ++ show (test26))	
         putStrLn ( "test27 = " ++ show (test27))	
         putStrLn ( "test28 = " ++ show (test28))	
         putStrLn ( "test29 = " ++ show (test29))	
         putStrLn ( "test31 = " ++ show (test30))	
         putStrLn ( "test31 = " ++ show (test31))	
         putStrLn ( "test32 = " ++ show (test32))	
         putStrLn ( "test33 = " ++ show (test33))	
         putStrLn( "test34 = " ++ show (test34))	
         putStrLn( "test35 = " ++ show (test35))		 
	 do
	   --print "************Suite des tests*****************"
	   putStrLn( "test36 = " ++ show (test36))	
	   putStrLn( "test37 = " ++ show (test37))
	   putStrLn ( "test38 = " ++ show (testq1a))	
	   putStrLn ( "test39 = " ++ show (testq1b))	
	   putStrLn ( "test40 = " ++ show (testq2a))
	   putStrLn ( "test41 = " ++ show (testq2b))
	   putStrLn ( "test42 = " ++ show (testq6a))	
	   putStrLn ( "test44 = " ++ show (testq6c))	
	   putStrLn ( "test46 = " ++ show (testq6e))
	 do
	   result <- try (print(TP1.acteursSelectionnes (avengers, [critereExperienceMin 10, critereFeminin], listeActeurs))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test43 = " ++ show (ex == testq6b)
            Right val -> putStrLn $ "test43 = False"
	 do
	   result <- try (print(TP1.acteursSelectionnes (avengers, [critereExperienceMin 10, critereFeminin], listeActeurs))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test45 = " ++ show (ex == testq6d)
            Right val -> putStrLn $ "testq45 = False"
	 do
	   result <- try (print(TP1.acteursSelectionnesNouvelle (avengers, [critereFeminin, critereMasculin, critereRevenuMin 1000], listeActeurs))) :: IO (Either NosExceptions () )
	   case result of
            Left ex  -> putStrLn $ "test47 = " ++ show (ex == testq11a)
            Right val -> putStrLn $ "test47 = False"
	 do
		putStrLn ( "test48 = " ++ show (testq11b))	
		putStrLn ( "test49 = " ++ show (testq11c))
		putStrLn ( "test50 = " ++ show (testq14b))
		putStrLn ( "test51 = " ++ show (testq14c))
	   
-- *********************** 
-- Définition des tests	  
-- ***********************   
testq1a = TP1.selectionActeursCriteres [critereRevenuMin 1000] listeActeurs == [chris,scarlett,robert,sharon,julia,meryll]
testq1b = TP1.selectionActeursCriteres [critereFeminin, critereRevenuMin 1000, critereExperienceMin 25] listeActeurs == [sharon,julia,meryll]
testq2a = TP1.selectionActeursFilm nuit listeActeurs == [scarlett,julia,maxi]
testq2b = TP1.selectionActeursFilm avengers listeActeurs == [chris,scarlett,robert,sharon,maxi,meryll,arnold]

testq6a = TP1.acteursSelectionnes (avengers, [critereExperienceMin 10], listeActeurs) == [chris,scarlett,robert,sharon,maxi,meryll,arnold]
testq6b = PasAssezDacteurs
testq6c = TP1.acteursSelectionnes (avengers, [critereExperienceMin 10, critereMasculin], listeActeurs) == [chris,robert,maxi,arnold]
testq6d = PasAssezDacteurs
testq6e = TP1.acteursSelectionnes (avengers, [critereExperienceMin 10, critereRevenuMin 1000], listeActeurs) == [chris,scarlett,robert,sharon,meryll]

testq11a = PasAssezDacteurs
testq11b = TP1.acteursSelectionnesNouvelle (avengers, [critereFeminin, critereMasculin, critereRevenuMin 1000, critereExperienceMin 10], listeActeurs) == [scarlett,chris,robert,sharon]
testq11c = TP1.acteursSelectionnesNouvelle (avengers, [critereExperienceMin 10, critereRevenuMin 1000, critereMasculin, critereExperienceMin 25, critereFeminin], listeActeurs) == [chris,scarlett,robert,sharon,meryll]

testq14b = ((TP1.deuxieme (unzip3 (repertoireCinema (TP1.ajusterEntreesFilmCinema (starcite, nuit, 230))))) !! 1) == 230

testq14c = (TP1.revenuFilmCinema nuit starcite == 15550) && (revenuFilmCinema avengers welcome == 400200)

test01 = PasDeRealisateur
test02 = TP1.produire (flash,nuit) == (MaisonDeProd "Flash Films" 98000 [(Film "Une nuit a Casablanca" Humour (Realisateur "Allen Woody" "New York New York") flash 2000 100 24 1000)],(Film "Une nuit a Casablanca" Humour (Realisateur "Allen Woody" "New York New York") flash 2000 100 24 1000))
test03 = TP1.produire (ua, nuit) == (MaisonDeProd "United Artists" 998000 [(Film "Une nuit a Casablanca" Humour woody ua 2000 100 24 1000)],(Film "Une nuit a Casablanca" Humour woody ua 2000 100 24 1000))
test04 = BudgetInsuffisant
test05 = PasDeRealisateur
test06 = PasDeProducteur
test07 = PasAssezDacteurs
-- affectation de rôles
test08 = r8 == ((Film "Coupe de coupe !" Horreur denis flop 38500 45 2 3500),[meryll,sharon]) && (head(listeFilmsActeur (extractionActeur r8)) == coupe)
test09 = PasAssezDacteurs
test10 = PasDeRealisateur
test11 = PasDeProducteur
test12 = PasAssezDacteurs
test13 = TP1.selectionnesSansRole (coupe, [critereFeminin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]) == []
test14 = PasAssezDacteurs
test15 = TP1.selectionActeursCriteresNouvelle ([], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]) == []
test16 = TP1.selectionActeursCriteresNouvelle ([critereMasculin, critereFeminin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]) ==[arnold,meryll]
test17 = TP1.selectionActeursCriteresNouvelle ([critereRevenuMin 1000, critereRevenuMax 10000], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]) ==[meryll,arnold]
test18 = TP1.selectionActeursCriteresNouvelle ([critereNom "Schwarzennegger Arnold", critereNom "Streep Meryll"], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]) == [meryll,arnold]
test19 = PasAssezDacteurs
test20 = (TP1.affectationDesRolesNouvelle (wind2, [critereMasculin, critereMasculin, critereFeminin, critereFeminin], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]) == (wind2,[maxi,reno,julia])) 
test21 = PasAssezDacteurs
test22 = (r22 == (coupe,[meryll,maxi])) && ( (coutFilm (extractionFilm r22)) == 36501) 
test24 = TP1.affectationDesRoles (avengers, [], [arnold, meryll, maxi, julia, reno, sharon, robert, chris]) == (avengers,[arnold,meryll,maxi,sharon])
test23 = PasAssezDacteurs
test25 = TP1.profit (avengers, listeCinemas) == True
test26 = TP1.meilleursActeurs listeActeurs listeFilms listeCinemas == [(robert,3)]
test26'= TP1.meilleursActeurs listeActeurs listeFilms listeCinemas
test27 = TP1.meilleursFilms listeFilms listeCinemas == [(avengers,458200),(iron,445550),(sherlock,253000),(coupe,18500),(nuit,18050),(wind,8000),(wind2,0),(destruction,0)]
test27' = TP1.meilleursFilms listeFilms listeCinemas
test27'' = head (TP1.meilleursFilms listeFilms listeCinemas)
test28 = (r28 == (Cinema  "Le verdict" "nulle part" [(nuit, 0, 10),(destruction, 0, 15)]))
test28' = repertoireCinema r28
test29 = (TP1.meilleursFilms [] [] == [])
test30 = (TP1.meilleursFilms [] listeCinemas == [])
test31 = (TP1.meilleursFilms listeFilms [] == [])
test32 = TP1.profit (avengers, []) == False
test33 = TP1.profit (sherlock, listeCinemas) == True
--les acteurs qui jouent dans le(s) meilleur(s) film(s)
test34 = [a | a <- listeActeurs, joueDans a [head (fst (unzip test27'))]] == [scarlett,robert]
--box_office
test35 = TP1.box_office avengers listeCinemas == 458200
--restorer le répertoire d'un cinéma
test36 = repertoireCinema (TP1.restorerRepertoire starcite [nuit, coupe, avengers][340, 23456, 35600][7, 9, 15]) == [(nuit,340,7),(coupe,23456,9),(avengers,35600,15)]
-- test j
test37 = TP1.achalandage starcite + TP1.achalandage welcome == 37049

-- fonctions de services utilisées dans les tests
extractionActeur :: (TP1.Film, [TP1.Acteur]) -> Acteur
extractionActeur (film, (a:lacteurs)) = a
extractionFilm :: (TP1.Film, [TP1.Acteur]) -> Film
extractionFilm (film, (a:lacteurs)) = film
listeFilmsActeur (Acteur _ _ _ _ _ listeFilms) = listeFilms
coutFilm (Film _ _ _ _ coutF _ _ _ ) = coutF
budgetFilm (Film _ _ _ _ _ _ _ budgetF) = budgetF
repertoireCinema (Cinema _ _ repertoire) = repertoire