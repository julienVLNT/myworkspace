################################################################################
#   DOCUMENTATION : LOGICIEL GNU GMSH                                          #
#   ---------------------------------                                          #
#   Julien VALENTIN                                                            #
#   30 Octobre 2022                                                            #
################################################################################

SITE WEB
--------
https://gmsh.info/

INSTALLER GMSH
--------------
    Ubuntu
    ------
        `sudo apt-get update && sudo apt-get install --yes gmsh`


WORKFLOW
--------


RUDIMENTS DE SCRIPTS GEOMETRIQUES
---------------------------------
    `h = 1;`
    --------
        Assigne la valeur 1 a la variable numerique h.

    `c = h/2;`
    ----------
        Assigne la valeur h/2 a la variable numerique c, ou h est une variable
        numerique declaree et initialisee.

    `Include "mygeo.geo";`
    ----------------------
        Importe le fichier `mygeo.geo` : revient à copier-coller son contenu 
        dans le script courant.

    `Point(i) = {xi, yi, zi, hi};`
    ------------------------------
        Declare une instance d'un point, indice par l'entier i. Les parametres 
        sont ses coordonnees en 3.D suivi de la distance maximum toleree entre 
        deux noeuds au voisinage du point.

    `Line(k) = {i, j};`
    ------------------
        Declare une instance d'un segment, indice par l'entier k, avec pour 
        extremites les points d'indice i et j.

    `Circle(l) = {i, k, j};`
    -----------------------
        Declare une instance d'arc de cercle de centre le point d'indice k et 
        passant par les points d'indices i et j. L'angle intercepte par l'arc 
        doit etre inferieur ou egal a Pi = 3.14159...

    `Ellipse(m) = {i, j, k, l};`
    ---------------------------
        Declare une instance d'arc d'ellipse, indicee par m. i est l'indice du 
        Point origine de l'arc, j l'indice du Point centre de l'ellipse, k est 
        l'indice d'un point pris sur le demi-grand axe et l l'indice du Point
        final de l'arc d'ellipse.

    `Spline(k) = {i, j, ...};`
    -------------------------
        Declare une instance de courbe Spline, indicee par l'entier k. La liste
        d'entiers i, j, ... correspond aux indices des points de contrôle de la
        spline.

    `BSpline(k) = {i, j, ...};`
    --------------------------
        Declare une instance de courbe B-spline, ...

    `Curve Loop(k) = {a, b, c...};`
    ------------------------------
        Declare une boucle, i.e un chemin ferme, definie comme la concatenation
        des entites en une dimension indicees par a, b, c...

    `Plane Surface(k) = {i, j, ...};`
    --------------------------------
        Declare une instance de surface indicee par l'entier k, delimitee par 
        les boucles fermees, i.e les instances de `Curve Loop` d'indice i, j, ..
        La premiere definit la surface exterieure, les autres forment les trous.

    `Ruled Surface(k) = {i, j, ...} <In Sphere {l}>;`
    ------------------------------------------------
        Declare une instance de surface indicee par l'entier k, delimitee par 
        les boucles fermees, i.e les instances de `Curve Loop` d'indice i, j, ..
        Une surface reglee est une surface qui peut etre interpolee par des 
        interpolations transfinies. La liste de lignes doit contenir trois ou
        quatre elements. `In Sphere` est optionnel, l est l'indice du centre de 
        la sphere.

    `Surface Loop(k) = {i, j, ...};`
    -------------------------------
        Declare une instance de surface fermee. Attention au respect des
        orientations.

    `Volume(k) = {i, j, ...};`
    -------------------------
        Declare le volume d'indice k defini par les boucles, i.e `Surface Loop`,
        indicees par les entiers i, j, ...

    `Physical Point("label") = {i, ...};`
    ------------------------------------
        Applique l'etiquette `label` aux entites definies par l'ensemble des 
        indices. Les indices sont ceux des entites visees, ici Point. On pourrait
        renseigner des lignes, surfaces, volumes...

    `Transfinite Line(k) = 15;`
    --------------------------
        Applique 15-1=14 elements sur la courbe de type visee et indicee par k.
    

USAGES
------
    Obtenir de l'aide sur la commande `gmsh`
    ----------------------------------------
        `gmsh -h`

    Informations sur la version installee de `gmsh`
    -----------------------------------------------
        `gmsh --info`

    Profondeur de log
    -----------------
        `gmsh -v 1`
        `gmsh -v 10`
        `gmsh -v 100`

    Ouvrir une geometrie .geo
    -------------------------
        `gmsh mygeo.geo`

    Mailler une geometrie 1.D
    -------------------------
        `gmsh mygeo.geo -1`

    Mailler une geometrie 2.D
    -------------------------
        `gmsh mygeo.geo -2`

    Mailler une geometrie 3.D
    -------------------------
        `gmsh mygeo.geo -3`

    Fixer le fichier de sortie du maillage
    --------------------------------------
        `gmsh mygeo.geo -2 -o mymesh.msh`

    Fixer la valeur d'un ou plusieurs parametres numeriques
    -------------------------------------------------------
        `gmsh mygeo.geo -2 -setnumber h 10`
        `gmsh mygeo.geo -2 -setnumber h 10 -setnumber xmax 50`
    
    Envoyer les logs dans un fichier
    --------------------------------
        `gmsh mygeo.geo -2 $1 > mylog.log`

    Multi-threading (si GMSH est compile avec OpenMP)
    ---------------
        `gmsh mygeo.geo -nt 2`


EXEMPLES : ./geometries/
--------
    Dimension 0
    -----------
        point.geo
            Definition d'un point et affectation d'un label physique.

    Dimension 1
    -----------
        segment_00.geo
            Definition du segment [-1, 1], distance 0.1.
        segment_01.geo
            Definition du segment [-1, 1], distance h.
        segment_02.geo
            Definition du segment [a, b], distance h.
        spline.geo
            Usage d'une Spline pour definir une courbe passant par trois points.

    Dimension 2
    -----------
        carre.geo
            Definition du carre de diagonale (0,0,0) - (1,1,0) avec h=0.1.
        cercle.geo
            Definition du cercle unite (centre en (0,0,0) et de rayon 1) avec 
            h = 0.1.
        ellipse.geo
            Definition de l'ellipse centree en (0, 0, 0), de demi-grand axe 1 et 
            de demi-petit axe 0.75.