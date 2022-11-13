################################################################################
#   DOCUMENTATION : LANGAGE FORTRAN                                            #
#   -------------------------------                                            #
#   Julien VALENTIN                                                            #
#   30 Octobre 2022                                                            #
################################################################################

SITE WEB
--------
https://fortran-lang.org/en/


USAGE DE GFORTRAN
-----------------
    `gfortran main.f90`
    -------------------
        Compile le fichier source main.f90 dans un executable nomme a.out.

    `gfortran -o main main.f90`
    ---------------------------
        Compile le fichier source main.f90 dans l'executable main.


EXEMPLES
--------
    ./helloworld.f90
    ----------------
        Programme d'affichage du message "Hello, World !" a l'ecran.

    ./algebre/intrinseques.f90
    --------------------------
        Programme de presentation des operations standards implementees en 
        Fortran a propos des vecteurs, des matrices, ainsi que de leurs algebres

    ./algebre/lu.f90 [TO DO]
    ----------------
        Programme de calcul de la decomposition LU d'une matrice carree. Pour 
        memoire, toute matrice carree admet une unique factorisation de la forme
        L U ou L est triangulaire inferieure ("lower") et U est triangulaire 
        superieure ("upper").

    ./algebre/printer.f90
    ---------------------
        Programme d'affichage d'un vecteur ainsi que d'une matrice sur la sortie
        console.

    ./algorithmes/arrays.f90
    ------------------------
        Programme presentant les manipulations classiques des arrays.

    ./algorithmes/boolean.f90
    -------------------------
        Programme presentant les manipulations classiques sur les booleens.
    
    ./algorithmes/cmd_args_file_io.f90
    ----------------------------------
        Programme utilisant des arguments passes en ligne de commande et 
        d'ecriture dans un fichier texte.

    ./algorithmes/event_handler.f90 [TO FIX (Ubuntu), OK (Windows)]
    -------------------------------
        Programme de gestion des evenements utilisateurs.

    ./algorithmes/sleeper.f90
    -------------------------
        Programme qui dort pendant une duree determinee.
    
    ./analyse/bisection.f90
    -----------------------
        Programme de recherche de la racine reelle de la fonction scalaire
        f : x |--> x^3 + 4 x^2 - 10 par methode de la bisection. Le critere 
        d'arret est l'evaluation de l'erreur relative entre deux iterations 
        successives. Une approximation de la racine est donnee par x ~ 1.3652

    ./analyse/derivee_00.f90 [TO DO]
    ------------------------
        Programme de calcul de la derivee de la fonction exponentielle entre 0 
        et 1 par le schema de differences finies centrees (a l'interieur) et a 
        trois points decentre sur les bords.

    ./analyse/integration_00.f90
    ----------------------------
        Programme de calcul de l'integrale de la fonction cosinus entre 0 et 2pi 
        par la methode des rectangles (evalues a droite).
    
    ./analyse/integration_01.f90
    ----------------------------
        Programme de calcul de l'integrale de la fonction cosinus entre 0 et 2pi
        par la methode des rectangles (evalues au milieu).

    ./analyse/integration_02.f90
    ----------------------------
        Programme de calcul de l'integrale de la fonction cosinus entre 0 et 2pi
        par la methode de Simpson.

    ./analyse/integration_03.f90 [TO DO]
    ----------------------------
        Programme de calcul de l'integrale de la fonction cosinus entre 0 et 2pi
        par la quadrature de Gauss produite aux points x0/sqrt(3) et x1/sqrt(3)

    ./analyse/interpolation.f90
    ---------------------------
        Programme d'interpolation d'un ensemble de points du plan au format
        (x, f(x)). Les donnees sont lues a partir du fichier interpolation.dat, 
        dans un format libre. L'interpolation utilise les polynomes de Lagrange.

    ./analyse/newton.f90
    --------------------
        Programme de recherche de la racine reelle de la fonction scalaire
        f : x |--> x^3 + 4 x^2 - 10 par methode de la Newton. Le critere 
        d'arret est l'evaluation de l'erreur relative entre deux iterations 
        successives. Une approximation de la racine est donnee par x ~ 1.3652
