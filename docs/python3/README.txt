################################################################################
#   DOCUMENTATION : LANGAGE PYTHON 3                                           #
#   --------------------------------                                           #
#   Julien VALENTIN                                                            #
#   30 Octobre 2022                                                            #
################################################################################

SITE WEB
--------
https://www.python.org/


INSTALLER PYTHON3
-----------------
    Linux/Ubuntu
    ------------
        `sudo apt-get update && sudo apt-get install --yes python3 python3-pip`


ENVIRONNEMENTS
--------------
    MATHEMATIQUES NUMERIQUES
    ------------------------
        - matplotlib : `pip3 install matplotlib`
        - numpy : `pip3 install numpy`
        - scipy : `pip3 install scipy`
        - sympy : `pip3 install sympy`

    DATA VISUALIZATION
    ------------------
        - bokeh : `pip3 install bokeh`
        - matplotlib : `pip3 install matplotlib`
        - plotly : `pip3 install plotly`
        - seaborn : `pip3 install seaborn`

    ELEMENTS FINIS
    --------------
        - meshio : `pip3 install meshio[all]`
        - scikit-fem : `pip3 install scikit-fem`

    INTERACTIF
    ----------
        - jupyter Lab : `pip3 install jupyterlab`
        - widgetsnbextension : `pip3 install widgetsnbextension`

    MACHINE LEARNING
    ----------------
        - pytorch : `pip3 install torch`
        - scikit-learn : `pip3 install sklearn`
        - tensorflow : `pip3 install tensorflow`


EXEMPLES
--------
C.F MEMOIRE DE RECHERCHE : workspace/docs/latex/memoire/memoire_2021_i2m/main.pdf
    --------------------

    ./algorithmes/cmd_args_file_io.py
    ---------------------------------
        Programme qui accepte des arguments passes en ligne de commande et ecrit 
        dans un fichier texte.

    ./algorithmes/event_handler.py
    ------------------------------
        Programme qui compte jusqu'a interruption par une action (appui sur une 
        touche) de l'utilisateur.

    ./algorithmes/functions.py [A COMPLETER AU PLUS VITE]
    --------------------------
        Programme qui presente les rudiments de l'utilisation des fonctions en
        langage Python.

    ./algorithmes/projecteuler.md
    -----------------------------
        Presentation des algorithmes de resolution des premiers problemes 
        presentes sur le depot projecteuler.net

    ./algorithmes/pythonchallenge.md
    --------------------------------
        Presentation des algorithmes de resolution des problemes presentes sur 
        le depot http://www.pythonchallenge.com/.

    ./algorithmes/sleeper.py
    ------------------------
        Programme qui dort un certain nombre de secondes.

    ./analyse/lagrange.py
    ---------------------
        Fonction d'evaluation des polynomes de Lagrange de degre k en un point x
        compris entre 0 et 1. La fonction prend en entree la liste x_ des points
        a interpoler, le reel x en lequel on souhaite evaluer et i le degre du 
        polynome a interpoler.

    ./analyse/legendre.py [TO DO]
    ---------------------

    ./edo/eulerExplicite_00.py
    --------------------------
        Programme de resolution de l'E.D.O lineaire autonome d'ordre 1 : u' = u, 
        u(0) = 1, par methode d'Euler explicite sur discretisation uniforme.

    ./edo/eulerImplicite_00.py
    --------------------------
        Programme de resolution de l'E.D.O lineaire autonome d'ordre 1 : u' = u, 
        u(0) = 1, par methode d'Euler implicite sur discretisation uniforme.

    ./edo/rungeKutta2_00.py
    -----------------------
        Programme de resolution de l'E.D.O lineaire autonome d'ordre 1 : u' = u, 
        u(0) = 1, par methode de Runge-Kutta d'ordre 2, explicite, sur 
        discretisation uniforme.

    ./edo/rungeKutta4_00.py
    -----------------------
        Programme de resolution de l'E.D.O lineaire autonome d'ordre 1 : u' = u, 
        u(0) = 1, par methode de Runge-Kutta d'ordre 4, explicite, sur 
        discretisation uniforme.

    ./edp/differences_finies/laplace1d_00.py
    ----------------------------------------
        Programme de resolution de l'equation de Laplace avec conditions aux li-
        mites de Dirichlet par schema a trois points sur grille uniforme.

    ./edp/differences_finies/laplace1d_01.py
    ----------------------------------------
        Programme de resolution de l'equation de Laplace avec conditions aux li-
        mites de Dirichlet a gauche et de Neumann a droite par schema a trois 
        points sur grille uniforme.

    ./edp/differences_finies/laplace1d_02.py
    ----------------------------------------
        Programme de resolution de l'equation de Laplace avec conditions aux li-
        mites de Robin a gauche et de Dirichlet a droite par schema a trois 
        points sur grille uniforme.

    ./edp/differences_finies/laplace1d_03.py
    ----------------------------------------
        Programme de resolution de l'equation de Laplace avec conditions aux li-
        mites de Dirichlet par schema a trois points sur grille non uniforme.

    ./edp/differences_finies/laplace1d_04.py
    -----------------------------------------
        Programme de resolution d'un probleme de Laplace lineaire par methode de 
        Newton-Raphson adaptee au stencil trois points sur maillage uniforme.
        Les conditions aux limites sont de Dirichlet.

    ./edp/differences_finies/laplace1d_05.py [TO DO]
    ----------------------------------------
        Programme de resolution d'un probleme de Laplace lineaire par methode de 
        Newton-Raphson adaptee au stencil trois points sur maillage uniforme.
        Les conditions aux limites sont de Dirichlet a gauche et de Neumann a 
        droite.

    ./edp/differences_finies/poisson1d_00.py
    -----------------------------------------
        Programme de resolution d'un probleme de Poisson lineaire par 
        schema a trois points sur maillage uniforme, avec conditions de 
        Dirichlet aux bords.

    ./edp/differences_finies/poisson1d_01.py [TO DO]
    -----------------------------------------
        Programme de resolution d'un probleme de Poisson quasi-lineaire par 
        methode de Newton-Raphson avec conditions de Dirichlet aux bords.

    ./edp/differences_finies/poisson1d_02.py
    -----------------------------------------
        Programme de resolution d'un probleme de Poisson non lineaire par 
        methode de Newton-Raphson adapte au stencil trois points sur maillage
        uniforme. Les conditions aux limites sont de Dirichlet.

    ./edp/elements_finis/laplace1d_00.py
    ------------------------------------
        Programme de resolution du Laplacien de Dirichlet par methode des 
        elements finis, interpolation par base P1-Lagrange sur maillage uniforme
        La condition a droite vaut u = 1.

    ./edp/elements_finis/laplace1d_01.py [TO FIX]
    ------------------------------------
        Programme de resolution du Laplacien de Dirichlet par methode des 
        elements finis, interpolation par base P2-Lagrange sur maillage uniforme
        La condition a droite vaut u = 1.

    ./edp/elements_finis/laplace1d_02.py [TO DO]
    ------------------------------------
        Programme de resolution du Laplacien de Dirichlet par methode des 
        elements finis, interpolation par base P1-Lagrange sur maillage uniforme
        La condition a droite vaut u = 1, structure comme un veritable script 
        elements finis.

    ./geometrie/maillage.ipynb
    --------------------------
        Notebook de presentation de manieres d'obtenir des maillages structures 
        ou non pour des geometries simples d'abord puis complexes a l'aide de 
        librairies en langage Python3.
