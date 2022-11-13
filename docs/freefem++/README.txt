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
