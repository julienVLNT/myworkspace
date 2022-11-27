################################################################################
#   DOCUMENTATION : LANGAGE FREEFEM++                                          #
#   ---------------------------------                                          #
#   Julien VALENTIN                                                            #
#   30 Octobre 2022                                                            #
################################################################################

SITE WEB
--------
https://freefem.org/


INSTALLATION
------------
    Ubuntu
    ------
        `sudo apt-get update && sudo apt-get install --yes freefem++`
        ou
        télécharger le .deb 


WORKFLOW
--------


USAGE
-----
    `FreeFem++ myscript.pde`
    ------------------------
        Execute l'interpreteur de commandes FreeFEM++ avec le fichier 
        myscript.pde
        
    `FreeFem++ myscript.pde -ns`
    -----------------------------
        Execute l'interpreteur de commandes FreeFEM++ avec le fichier 
        myscript.pde sans citer le script sur la sortie standard.
    
    `FreeFem++ myscript.pde -v 0`
    -----------------------------
        Execute l'interpreteur de commandes FreeFEM++ avec le fichier 
        myscript.pde avec la profondeur de logs 0 (v = 0, ... 5 ?)


EXEMPLES
--------
    ./helloworld
    ------------
        Programme d'affichage du message "Hello, World !" sur la sortie standard

    ./parallele/introduction_mpi.edp
    --------------------------------
        D'après l'exposé de Pierre Jolivet aux FreeFEM days 2021.
        Script d'introduction a la parallélisation avec distribution d'un 
        maillage avec buildDmesh, différence entre un champ global, un champ 
        local et la réduction des champs globaux par partition de l'unité. On 
        introduit également les vecteurs et matrices PETSc et les numérotations.

    ./parallele/metis_scotch.edp
    -------------------------------
        Script de découverte des outils de partition pour un domaine en deux 
        dimension d'espace. D'après l'exposé de Pierre Jolivet aux FreeFEM days 
        2021. On y découvre les interfaces Metis et Scotch. Les cas traités sont
        la coloration des sous-domaines d'un domaine donné, puis la description
        d'un sous-domaine, la définition de la restriction d'un champ global à 
        un sous-domaine et le prolongement par continuité d'un champ défini sur 
        un sous-domaine au domaine global.

    ./parallele/poisson_mpi.edp
    ---------------------------
        Script de résolution d'un problème de Poisson dans un contexte parallèle
        D'après l'exposé de Pierre Jolivet aux FreeFEM days 2021.
