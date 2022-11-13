################################################################################
#   DOCUMENTATION : LANGAGE C                                                  #
#   -------------------------                                                  #
#   Julien VALENTIN                                                            #
#   30 Octobre 2022                                                            #
################################################################################

SITE WEB
--------
https://www.open-std.org/jtc1/sc22/wg14/
https://www.intel.com/content/www/us/en/develop/documentation/get-started-with-dpcpp-compiler/top.html


INSTALLER GCC
-------------
    Linux/Ubuntu
    ------------
        `sudo apt-get update && sudo apt-get install --yes gcc`


INSTALLER INTEL ONEAPI HPC TOOLKIT
----------------------------------
    Linux/Ubuntu
    ------------
```bash
# download the key to system keyring
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

# add signed entry to apt sources and configure the APT client to use Intel repository:
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

sudo apt update

sudo apt install intel-hpckit
```


USAGE DE GCC
------------
    `gcc --help`
    --------
        Affiche l'aide de la commande `gcc`

    `gcc source.c`
    --------------
        Compile le script `source.c`. L'executable est, par defaut, note `a.out`

    `gcc -o main source.c`
    ----------------------
        Compile le script `source.c`. L'executable est alors nomme `main`

    `gcc -Wall source.c`
    --------------------
        Compile le script `source.c`. Les messages d'alerte sont rendus visibles

    `gcc source.c -lm`
    ------------------
        Compile le script `source.c`. L'option `-lm` est un raccourci pour lier
        d'eventuelles bibliotheques externes. Un exemple plus complet serait, en 
        presence de la librairie `math.h`.

    `gcc -I/opt/gdbm-1.8.3/include -L/opt/gdbm-1.8.3/lib/ source.c -lgdbm`
    ----------------------------------------------------------------------
        Compile le script `source.c`. Les options `-I` et `-L` permettent de 
        renseigner des lieux de l'arborescence ou chercher `lgdbm`, un element
        qui ne fait pas partie de la distribution standard du compilateur GCC.
        `-I` : ajoute un repertoire pour la recherche d'un fichier d'en-tete `.h`
               Par defaut "/usr/local/include/" et "/usr/include/"
        `-L` : ajoute un repertoire pour la recherche des librairies. Par defaut
               "/usr/local/lib/" et "/usr/lib/".


EXEMPLES
--------
    helloworld.c
    ------------
        Programme qui affiche "Hello, World !" a l'ecran.
    
    ./algebre/vecteurs_aleatoires.c
    -------------------------------
        Programme qui initialise un vecteur avec des valeurs pseudo-aleatoires
        entre 0 et 1. Prend comme argument le nombre de composantes du vecteur.
    
    ./algebre/vecteurs_plans.c
    --------------------------
        Programme comprenant une structure pour les vecteurs du plan et quelques
        fonctions usuelles associees. Contient une methode d'initialisation
        aleatoire.

    ./algebre/vecteurs_plans_transformations.c
    ------------------------------------------
        Programme contenant une structure pour les matrices denses du plan et 
        quelques fonctions usuelles associees.

    ./algorithmes/arrays.c
    ----------------------
        Programme de presentation et de manipulations standards sur la structure
        array.

    ./algorithmes/boolean.c
    -----------------------
        Programme de presentation et de manipulations standards sur la structure
        booleenne et sa representation entiere.

    ./algorithmes/bubblesort.c
    --------------------------
        Programme de tri de la liste des 10 entiers compris entre 0 et 0 inclus
        par l'algorithme bubble sort.

    ./algorithmes/cmd_args_file_io.c
    --------------------------------
        Programme d'utilisation des parametres console et ecriture dans un 
        fichier texte.

    ./algorithmes/event_handler.c
    -----------------------------
        Programme qui execute le comptage des entiers jusqu'a interruption par 
        l'utilisateur.

    ./algorithmes/functions.c
    -------------------------
        Programme de presentation des fonctions en langage C.

    ./algorithmes/heapsort.c
    ------------------------
        Programme de tri de la liste des 10 entiers compris entre 0 et 9 inclus 
        par l'algorithme heap sort.

    ./algorithmes/mergesort.c
    -------------------------
        Programme de tri de la liste des 10 entiers compris entre 0 et 9 inclus
        par l'algorithme merge sort.

    ./algorithmes/pointeurs.c
    -------------------------
        Programme de presentation des pointeurs et de leur usage en langage C.

    ./algorithmes/quicksort.c
    -------------------------
        Programme de tri de la liste des 10 entiers compris entre 0 et 9 inclus 
        par algorithme quick sort.

    ./algorithmes/sleeper.c
    -----------------------
        Programme qui dort pendant une duree determinee.

    ./algorithmes/shufflearray.c
    ----------------------------
        Programme de melange aleatoire d'une liste donnee. On s'assure que tous 
        les elements de la liste initiale sont presents dans la liste melangee.

    ./analyse/newton_scalaire.c
    ---------------------------
        Recherche d'une racine de la fonction scalaire f : x |-> x**3 - x par la
        methode de Newton-Raphson.
