################################################################################
#   DOCUMENTATION : LANGAGE JULIA 1.0                                          #
#   ---------------------------------                                          #
#   Julien VALENTIN                                                            #
#   30 Octobre 2022                                                            #
################################################################################

SITE WEB
--------
https://julialang.org/


INSTALLATION
------------
    Ubuntu
    ------
        `sudo apt-get update && sudo apt-get install --yes julia`


USAGE
-----
    `julia`
    -------
        Execute l'interpreteur de commande (REPL) de Julia.

    `julia myscript.jl`
    -------------------
        Execute le script myscript.jl.


EXEMPLES
--------
    ./helloworld.jl
    ---------------
        Script d'affichage du message "Hello, World !" sur la sortie standard.

    ./algorithmes/cmd_args_file_io.jl
    ---------------------------------
        Programme qui utilise les arguments passes en ligne de commande et ecrit
        dans un fichier texte.

    ./algorithmes/event_handler.jl
    ------------------------------
        Programme qui compte en attendant un evenement provoque par l'agent : 
        appui sur une touche.

    ./algorithmes/functions.jl
    --------------------------
        Programme d'introduction a la notion de fonctions et a leur manipulation
        en langage Julia.

    ./algorithmes/sleeper.jl
    ------------------------
        Programme qui dort pour une duree determinee.
