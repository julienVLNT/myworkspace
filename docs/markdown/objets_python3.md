- [Généralités](#généralités)
- [Booléens, `bool`](#booléens-bool)
- [Nombres entiers relatifs, `int`](#nombres-entiers-relatifs-int)
- [Nombres décimaux, `Decimal`](#nombres-décimaux-decimal)
- [Nombres rationnels, `Fraction`](#nombres-rationnels-fraction)
- [Nombres réels, `float`](#nombres-réels-float)
- [Nombres complexes, `complex`](#nombres-complexes-complex)
- [Ensembles, `set`](#ensembles-set)
- [Collections constantes, `tuple`](#collections-constantes-tuple)
- [Collections mutables, `list`](#collections-mutables-list)
- [Les associations, `dict`](#les-associations-dict)
- [Les fonctions : `function`](#les-fonctions--function)
  - [Les expressions `lambda`](#les-expressions-lambda)
  - [L'instruction `def`](#linstruction-def)
- [Les graphes](#les-graphes)
- [Les chaînes de caractères : `str`](#les-chaînes-de-caractères--str)
- [Interlude sur `numpy`](#interlude-sur-numpy)
- [Les vecteurs : `numpy.ndarray` de dimension 1](#les-vecteurs--numpyndarray-de-dimension-1)
- [Les matrices : `numpy.ndarray` de dimension 2](#les-matrices--numpyndarray-de-dimension-2)

Aide-mémoire composé par Julien VALENTIN en mars 2022.

# Généralités

```python
In[1] type(1j)             # connaître la classe d'un objet
<class 'complex'>
In[2] isinstance('a', str) # test booléen d'appartenance à une classe
True 
```

# Booléens, `bool`

```python
In[1] a = bool()         # déclaration et affectation à False
In[2] b = bool(True)     # déclaration et affectation à True
In[3] c = True           # déclaration et affectation à True
In[4] d, e = False, True # déclaration et affectation multiple

In[5] d + e              # addition dans les entiers
1
In[6] d - e              # soustraction dans les entiers
-1
In[7] d * e              # multiplication dans les entiers
0
In[8] d / e              # division dans les rationnels
0.0
In[9] d // e             # quotient de la division euclidienne dans les entiers
0
In[10] d % e             # reste de la division euclidienne dans les entiers
0
In[11] d ** e            # exponentiation dans les entiers
0

In[12] d == e            # test d'égalité
False
In[13] d <= e            # test inférieur ou égal à
True
In[14] d < e             # test strictement inférieur à
True
In[15] d >= e            # test supérieur ou égal à
False
In[16] d > e             # test strictement supérieur à
False

In[17] not d             # opérateur booléen négation, ou not(d)
True
In[18] d and e           # opérateur booléen et
False
In[19] d or e            # opérateur booléen ou
True
In[20] ~ d               # opérateur logique négation
-1
In[21] ~ e               # opérateur logique négation
-2
In[21] d & e             # opérateur logique et
False
In[22] d | e             # opérateur logique ou
True
```

# Nombres entiers relatifs, `int`

Les nombres entiers sont représentés par l'objet `int`. On retrouve l'algèbre des entiers ainsi que la relation d'ordre totale sur $\mathbb{Z}$.

```python
In[1] a = int()    # déclaration et affectation à 0
In[2] b = int(10)  # déclaration et affectation à 10
In[3] c = int(2+3) # déclaration et affectation à 5
In[4] d = -1       # déclaration et affectation à -1
In[5] m, n = 5, 2  # déclaration et affectation multiple

In[] +m            # identité
In[] -m            # opposé
In[] abs(m)        # valeur absolue

In[6] m + n        # addition
7
In[7] m - n        # soustraction
3
In[8] m * n        # multiplication
10
In[9] m / n        # division réelle
2.5
In[10] m // n      # quotient de la division euclidienne
2
In[11] m % n       # reste de la division euclidienne
1
In[12] m ** n      # exponentiation
25

In[13] m == n      # test d'égalité
False
In[14] m <= n      # test inférieur ou égal à
False
In[15] m < n       # test strictement inférieur à
False
In[16] m >= n      # test supérieur ou égal à
True
In[17] m > n       # test strictement supérieur à
True
```

# Nombres décimaux, `Decimal`

Les nombres décimaux sont représentés par l'objet `Decimal` du module `decimal`. Il s'agit des rationnels possédant un nombre fini de décimales, on note quelques fois l'ensemble des décimaux $\mathbb{D}$.

```python
In[1] d = decimal.Decimal()          # déclaration et affectation à Decimal('0')
In[2] d = decimal.Decimal('1.23456') # déclaration et affectation à Decimal('1.23456')
In[3] d = decimal.Decimal('3.1415')  # déclaration et affectation

In[] +d                              # identité
In[] -d                              # opposé
In[] abs(d)                          # valeur absolue

In[4] d1 + d2                        # addition
In[5] d1 - d2                        # soustraction
In[6] d1 * d2                        # multiplication
In[7] d1 / d2                        # division réelle
In[8] d1 // d2                       # quotient de la division euclidienne étendue
In[9] d1 % d2                        # reste de la division euclidienne étendue
In[10] d1 ** d2                      # exponentiation

In[11] d1 == d2                      # test d'égalité
In[12] d1 <= d2                      # test inférieur ou égal à
In[13] d1 < d2                       # test strictement inférieur à
In[14] d1 >= d2                      # test supérieur ou égal à
In[15] d1 > d2                       # test strictement supérieur à

In[16] d.exp()                       # composition par l'exponentielle
In[17] d.ln()                        # composition par le logarithme naturel
In[18] d.log10()                     # composition par le logarithme de base 10
In[19] d.logb()                      # exposant ajusté de l'argument
In[20] d.sqrt()                      # composition par la racine carrée

In[21] decimal.getcontext()          # paramètres globaux du module decimal
Context(prec=28, rounding=ROUND_HALF_EVEN, Emin=-999999, Emax=999999,
        capitals=1, clamp=0, flags=[], traps=[Overflow, DivisionByZero,
        InvalidOperation])
In[22] decimal.getcontext().prec = 3 # édition d'un paramètre de l'objet Context
```

# Nombres rationnels, `Fraction`

Les nombres rationnels sont représentés par l'objet `Fraction` du module `fractions`. On retrouve encore l'algèbre et l'ordre mais sur le corps $\mathbb{Q}$.

```python
In[1] import fractions
In[2] q = fractions.Fraction()    # déclaration et affectation à Fraction(0, 1)
In[3] r = fractions.Fraction(2,3) # déclaration et affectation à Fraction(2,3) i.e 2/3
In[4] s = fractions.Fraction(3,2) # déclaration et affection à Fraction(3/2) i.e 3/2

In[] +p                           # identité
In[] -p                           # opposé
In[] abs(p)                       # valeur absolue

In[5] r + s                       # addition
In[6] r - s                       # soustraction
In[7] r * s                       # multiplication interne
In[8] r / s                       # division rationnelle
In[9] r // s                      # quotient de la division euclidienne étendue
In[10] r % s                      # reste de la division euclidienne étendue
In[11] r ** s                     # exponentiation

In[12] r == s                     # test d'égalité
False
In[13] r <= s                     # test inférieur ou égal à
True
In[14] r < s                      # test strictement inférieur à
True
In[15] r >= s                     # test supérieur ou égal à
False
In[16] r > s                      # test strictement supérieur à
False
```

# Nombres réels, `float`

Les nombres réels sont représentés par l'objet `float`. L'algèbre et l'ordre sont ceux de $\mathbb{R}$.

```python
In[1] x = float()      # déclaration et affectation à 0.0
In[2] x = float(1.1)   # déclaration et affectation à 1.1
In[3] x = float(2*3.1) # déclaration et affectation à 6.2
In[4] x = -1.          # déclaration et affectation à -1.0
In[5] x, y = 1.4, 3.1  # déclaration et affectation multiple

In[] +x                # identité
In[] -x                # opposé
In[] abs(x)            # valeur absolue

In[6] x + y            # addition
In[7] x - y            # soustraction
In[8] x * y            # multiplication
In[9] x / y            # division réelle
In[10] x // y          # quotient de la division euclidienne étendue
In[11] x % y           # reste de la division euclidienne étendue
In[12] x ** y          # exponentiation

In[]   x is y          # test d'identité
In[13] x == y          # test d'égalité
In[14] x <= y          # test inférieur ou égal à
In[15] x < y           # test strictement inférieur à
In[16] x >= y          # test supérieur ou égal à
In[17] x > y           # test strictement supérieur à
In[18] x.is_integer()  # test d'appartenance à l'ensemble des entiers
```

# Nombres complexes, `complex`

En langage `Python`, les nombres complexes sont représentés par l'objet `complex`. On rappelle que $\mathbb{C}$ n'est pas muni d'un ordre total, on ne pourra pas utiliser les opérateurs `<=`, `<`, `>=` ou `>`. De même, la division euclidienne n'est pas extensible, d'où l'absence des opérateurs `//` et `%`.

```python
In[1] z = complex()       # déclaration et affectation à 0 complexe
In[2] z = complex(1.+1.j) # déclaration et affectation à 1 + i
In[3] z = complex(1.+1j)  # déclaration et affectation à 1 + i
In[4] z = complex(1+1.j)  # déclaration et affectation à 1 + i
In[5] z = complex(1+1j)   # déclaration et affectation à 1 + i
In[6] z = 1 + 3j          # déclaration et affectation à 1 + 3i
In[7] w, z = 1+1j, 1.-1j  # déclaration et affectation multiple

In[]  +z                  # identité
In[]  -z                  # opposé
In[]  abs(z)              # module
In[9] z.real              # partie réelle
In[10] z.imag             # partie imaginaire
In[11] z.conjugate()      # complexe conjugué

In[12] w + z              # addition
In[13] w - z              # soustraction
In[14] w * z              # multiplication
In[15] w / z              # division complexe
In[16] w ** z             # exponentiation

In[17] w is z             # test d'identité
In[18] w == z             # test d'égalité
```

# Ensembles, `set`

Les ensembles sont des objets modélisant des collections non-ordonnées et sans élément en double.

```python
In[1] a = set()                             # déclaration et affectation à set()
In[2] a = set({'h', 'e', 'l', 'l', 'o'}); a #
{'l', 'e', 'o', 'h'}
In[3] a = set("Hello"); a                   #
{'l', 'e', 'H', 'o'}
In[4] a = {'h', 'e', 'l', 'l', 'o'}; a      #
{'l', 'e', 'o', 'h'}

In[5] A = set('abracadabra')
In[6] B = set('alacazam') 
In[7] A - B                                 # ensemble A privé de B
{'r', 'd', 'b'}
In[8] A | B                                 # ensemble A union B
{'a', 'c', 'r', 'd', 'b', 'm', 'z', 'l'}
In[9] A & B                                 # ensemble A intersection B
{'a', 'c'}
In[10] A ^ B                                # ensemble différence symétrique
{'r', 'd', 'b', 'm', 'z', 'l'}

In[11] {'a'} is {'a'}                       # test d'identité
False 
In[11] {'a', 'b', 'c'} == {'a', 'b', 'c'}   # test d'égalité
True
In[12] {'a', 'b', 'c'} <= {'b'}             # test d'inclusion large
False
In[13] {'a', 'b', 'c'} < {'b'}              # test d'inclusion stricte
False
In[14] {'a', 'b', 'c'} >= {'b'}             # test d'inclusion large
True
In[15] {'a', 'b', 'c'} > {'b'}              # test d'inclusion stricte
True
In[16] 'h' in a                             # test d'appartenance à l'ensemble
True
In[17] 'b' not in a                         # test d'absence de l'ensemble
False

In[18] {x for x in 'abracadabra' if x not in 'abc'} # écriture en compréhension
{'r', 'd'}
```

# Collections constantes, `tuple`

Les tuples modélisent des collections, éventuellement non-homogènes, d'objets ; les éléments sont séparés par une virgule. Les tuples sont constants (non-mutables).

```python
In[1] t = tuple()             # déclaration et initialisation à ()
In[2] t = tuple({1, 'a', 2j}) # déclaration et initialisation
In[3] t = (1, 'a', 2j)        # déclaration et initialisation
In[4] t = 1, 'a', 2j          # déclaration et initialisation
In[5] t = t, 'hello'          # déclaration avec t nichée
((1, 'a', 2j), 'hello')
In[6] t[0]                    # sélection d'un élément du tuple
(1, 'a', 2j)
In[7] t[0][1]                 # sélection du deuxième élément du premier élément
'a'
In[8] t = (1, 2, 3)           #
In[9] u = (1, 2)              #

In[10] t + t                  # concaténation
(1, 2, 3, 1, 2, 3)

In[11] tuple({'a'}) is tuple({'a'}) # test d'identité
False
In[12] u == t                 # test d'égalité
False
In[13] u <= t                 # test d'inclusion large
True
In[14] u < t                  # test d'inclusion stricte
True
In[15] u >= t                 # test d'inclusion large
False
In[16] u > t                  # test d'inclusion stricte
False
In[17] 1 in t                 # test d'appartenance
True
In[18] 10 not in t            # test d'absence
True

In[19] (t+t).count(1)         # comptage des occurences de l'objet 1 dans t+t
2

In[20] t = tuple(i if i%2==0 else -i for i in range(10)) # écriture en compréhension
(0, -1, 2, -3, 4, -5, 6, -7, 8, -9)
```

# Collections mutables, `list`

```python
In[1] l = list()                   # déclaration et affectation à []
In[2] l = list( {'a', 2, 1., 0j} ) # déclaration et affectation
[0j, 1.0, 2, 'a']
In[3] l = list( ('a', 2, 1., 0j) ) # déclaration et affectation
['a', 2, 1.0, 0j]
In[4] l = []                       # déclaration et affectation à []
In[5] l = [[]]                     # liste contenant 1 élément : la liste vide []
In[6] l = [1, 'a', 1j]             # déclaration et affectation
In[7] l = list(i if i**2%2==0 else -i for i in range(10)) # écriture en compréhension
[0, -1, 2, -3, 4, -5, 6, -7, 8, -9]

In[8] k = [1, 2, 3]
In[9] l = [3, 4, 5]
In[10] k + l                       # concaténation
[1, 2, 3, 4, 5, 6]
In[11] 2 * k                       # duplication (multiplication par un entier)
[1, 2, 3, 1, 2, 3]

In[12] [1, 2] is [1, 2]            # test d'identité
False
In[13] [1, 2] == [1, 2]            # test d'égalité
True
In[14] [2] <= [1, 2]               # test d'inclusion large
False
In[15] [2] < [1, 2]                # test d'inclusion stricte
False
In[16] [2] >= [1, 2]               # test d'inclusion large
True
In[17] [2] > [1, 2]                # test d'inclusion stricte
True

In[18] k[:]                        # sélection de la liste entière
[1, 2, 3]
In[19] k[1]                        # sélection du (i+1)-ème élément
2
In[20] k[:2]                       # sélection des éléments d'indice [0,i)
[1, 2]
In[21] k[1:]                       # sélection des éléments d'indice supérieur à i inclus
[2, 3]
In[22] k[::2]                      # sélection des éléments d'indice pair
[1, 3]
In[23] k[-1]                       # sélection par parcours inverse
3
In[24] k[::-1]                     # sélection par parcours inverse
[3, 2, 1]

In[25] len(k)                      # longueur d'une liste
3
In[26] k.index(2)                  # indexe de la première occurence de l'objet passé en paramètre
1
In[27] k.insert(1, 5)              # insère à l'indexe 1 l'objet 5
[1, 5, 2, 3]
In[28] k.remove(5)                 # retire la première occurence de l'objet 5
[1, 2, 3]
In[29] k.append(5)                 # insère l'objet 5 à la fin de la liste
[1, 2, 3, 5]
In[30] k.pop()                     # retire et renvoie le dernier objet de la liste
5
In[31] k
[1, 2, 3]
In[32] k.reverse(); k              # inverse l'ordre des éléments (modifie k : return None)
[3, 2, 1]
In[33] k.sort(); k                 # trie dans l'ordre croissant (modifie k : return None)
[1, 2, 3]
In[34] k.sort(reverse=True); k     # trie dans l'ordre décroissant (modifie k : return None)
[3, 2, 1]
```

# Les associations, `dict`

Il est question d'associer deux objets : `clef : valeur`. `Python` les représente par l'objet `dict`.

```python
In[1] d = dict()                            # déclaration et initialisation à {}
In[2] d = dict({1: 'a', 2: [], 3: -1.2})    # déclaration et initialisation
In[3] d = {1: 'a', 2: [], 3: -1.2}          # déclaration et initialisation
In[4] d = {'i1': {1: 'a'}, 'i2': [1, 2, 3]} # déclaration et initialisation
In[5] d = dict.fromkeys([1, 2, 3, 4]); d    # déclaration par clefs sans valeurs
{0: None, 1: None, 2: None, 3: None, 4: None}
In[6] d = dict.fromkeys([1, 2, 3], 0)       # déclaration par clefs et affectation des valeurs à 0
{1: 0, 2: 0, 3: 0}

In[7] {'a': 1} is {'a': 1}                  # test d'identité
False
In[8] {'a': 1} == {'a': 1}                  # test d'égalité
True
In[9] 'b' in {'a': 1, 'b': 3, 'c': []}      # test d'appartenance d'une clef
True

In[10] d = {'i1': {1: 'a'}, 'i2': [1, 2, 3]}
In[11] d.keys()                             # objet dict_keys contenant la liste des clefs
dict_keys(['i1', 'i2'])
In[12] d.values()                           # objet dict_values contenant la liste des valeurs
dict_values([{1: 'a'}, [1, 2, 3]])
In[13] d.items()                            # objet dict_items contenant la liste des paires (clef : valeur)
dict_items([('i1', {1: 'a'}), ('i2', [1, 2, 3])])
In[14] d['i1']                              # accède à la valeur associée à la clef 'i1'
{1: 'a'}
In[15] d.get('i2')                          # accède à la valeur associée à la clef 'i2'
[1, 2, 3]
In[16] d['i3'] = 1j; d                      # affecte à la clef 'i3' la valeur 1j, ou l'insère si elle n'existe pas
{'i1': {1: 'a'}, 'i2': [1, 2, 3], 'i3': 1}

In[17] d.pop('i1'); d                       # efface et renvoie la paire (clef, valeur) désignée par sa clef
{'i2': [1, 2, 3], 'i3': 1j}
In[18] d.popitem()                          # efface et renvoie la dernière paire (clef, valeur) du dictionnaire
('i3', 1j)
In[19] d
{'i2': [1, 2, 3]}
In[20] d.clear(); d                         # efface les paires (clef, valeur) du dictionnaire
{}
```

# Les fonctions : `function`

## Les expressions `lambda`

```python
In[1] lambda x : 0                           # déclaration
<function <lambda> at 0x7f35fc1655e0>
In[2] lambda x, y : x + y                    # fonction de plusieurs variables
In[3] lambda x, y : {x, 2*x, (x+y)**2}       # fonction à valeurs multiples (set)
In[4] lambda x, y : (x, 2*x, (x+y)**2)       # fonction à valeurs multiples (tuple)
In[5] lambda x, y : [x, 2*x, (x+y)**2]       # fonction à valeurs multiples (liste)
In[6] f = lambda x : 0 if x < 0 else x       # fonction définie par une condition ("rectified linear" ici)
In[7] f(-1)                                  # évaluation de la fonction
```

## L'instruction `def`

L'instruction `def` est une instruction permettant d'instancier des fonctions, très souvent préférable à l'usage d'une fonction `lambda` de par sa lisibilité. Prenons l'exemple de la fonction élévation au carré.

```python
In[] def carre(x):
...      return x**2
```

Une version pleinement documentée de la même fonction serait : 

```python
In[1] def carre(x: float) -> float:
...       """ Calcul le carré du nombre x.
...           
...           PARAMETRES:
...           -----------
...           x : float, valeur de l'argument
...
...           RETOURS:
...           --------
...           x**2 : float 
...
...           EXEMPLES:
...           ---------
...           >>> carre(-1)
...           1 """
...
...        return x**2
```

On note que `x: float` n'est qu'une indication, de même que `-> float`. Le programme s'exécute malgré la violation de ces "règles", qui n'en sont donc pas ; par exemple, `carre(1j)` renvoie `-1`, ce qui est mathématiquement juste, mais du point de vue de la documentation ne va pas. La seule contrainte est que l'opérateur `**` soit défini pour l'argument prescrit. Ainsi déclarée, appeler la fonction `help(carre)` renvoie la chaîne de caractère proposée ainsi que le squelette de la fonction.

# Les graphes

Un graphe est un couple d'ensembles, le premier représentant les sommets et le second ses arêtes. On connaît au moins deux manière de les représenter en mémoire. La première manière est d'utiliser un dictionnaire dont les clefs sont chaque sommet et les valeurs sont les arêtes, plus précisément, un ensemble, un tuple ou une liste telle qu'un autre sommet en est élément si et seulement s'il existe un chemin entre les deux sommets. Une autre manière de représenter un graphe est d'associer à chaque sommet un entier naturel. Cet entier indexe une ligne d'une matrice, et l'entrée de la matrice $(a_{i,j})$ est non-nulle si, et seulement si, il existe une arête du sommet $i$ vers le sommet $j$. Il n'y a donc pas d'objet spécifique dans la librairie standard pour ceux-ci.

# Les chaînes de caractères : `str` 

```python
In[1] s = str()                                # déclaration et affectation à ''
In[2] s = ''                                   # déclaration et affectation à ''
In[3] s = ""                                   # déclaration et affectation à ''
In[4] s = 'Hello'                              # déclaration et affectation à <Hello>
In[5] s = "'Hello'"                            # déclaration et affectation à <'Hello'>

In[6] 'a' + 'b'                                # concaténation
'ab'
In[7] 3 * 'a'                                  # duplication
'aaa'

In[8] 'a' is 'a'                               # test d'identité
In[9] 'a' == 'a'                               # test d'égalité
In[10] 'a' <= 'abc'                            # test d'inclusion large
In[11] 'a' < 'abc'                             # test d'inclusion stricte
In[12] 'a' >= 'abc'                            # test d'inclusion large
In[13] 'a' > 'abc'                             # test d'inclusion stricte
In[14] 'abc'.startswith('a')                   # test d'appartenance
True
In[15] 'b' in 'abc'                            # test d'appartenance
True
In[16] 'abc'.endswith('c')                     # test d'appartenance
True
In[17] not 'a'                                 # ?
False
In[18] 'a' and 'b'                             # ?
'a'
In[19] 'a' or 'b'                              # ?
'b'
In[20] 'a'.isascii()                           # test booléen de caractère de la table ASCII
True
In[21] 'a'.isalpha()                           # test booléen de caractère alphabétique
True
In[22] 'A'.isupper()                           # test booléen pour les majuscules
True
In[23] ' '.isspace()                           # test booléen de caractère espace
True
In[24] 'Hello !'.isprintable()                 # ?
True
In[25] f = 3; 'f'.isidentifier()               # test booléen : est-ce un nom de variable ?
True
In[26] '1j'.isalnum()                          # test booléen de caractères alpha-numériques
True
In[27] '1101'.isdigit()                        # test booléen de caractères entiers naturels
True

In[28] 'abacad'.count('a')                     # compte le nombre d'occurences du caractère 'a'
3
In[29] 'abc'.capitalize()                      # rend la première lettre capitale
'Abc'
In[30] 'pi is the most popular number'.title() # capitalise chaque première lettre suivant un espace
'Pi Is The Most Popular Number'
In[31] 'AbC'.casefold()                        # ?
'abc'
In[32] 'AbC'.swapcase()                        # inverse la casse
'aBc'
In[33] 'AbC'.lower()                           # en miniscule
'abc'
In[34] 'AbC'.upper()                           # en capitale
'ABC'

In[35] 'pi\t=\t3.1415'.expandtabs()            # affiche les tabulations
'pi      =       3.1415'
```

# Interlude sur `numpy`

`Numpy` est une librairie disponible en `Python` développée pour l'analyse numérique ou statistique de haute performance. Cette librairie propose essentiellement trois choses : vingt-quatre manières de représenter les objets abstraits en mémoire, les scalaires ; l'objet `dtype` qui permet de structurer les objets en mémoire, et les `ndarray` qui sont très généralement des conteneurs de données. Il se trouve que ce dernier correspond très bien à la représentation des objets de l'algèbre linéaire et de l'analyse numérique, mais son usage est largement plus répandu que ces deux applications là.

```python
In[1] import numpy
In[2] array.ndim                      # nombre de dimensions, ou d'axes
In[3] array.shape                     # tuple de nombres de composantes par dimension
In[4] len(array)                      # nombre d'éléments selon l'axe 0 (lignes)
In[5] len(array[0])                   # nombre d'éléments selon l'axe 1 (colonnes)
In[6] len(array[0][0])                # nombre d'éléments selon l'axe 2 ...
In[7] array.nbytes                    # nombre d'octets nécessaires pour le stockage
In[8] array.reshape((k,l,m))          # change la taille du tableau (ici sur 3 axes)
In[9] array.dtype                     # type donné aux entrées du tableau
In[10] array.astype(str)              # convertit les entrées du tableau en str
In[11] array.copy()                   # renvoie une copie du tableau
In[12] np.hstack((u, v))              # assemble en ligne les vecteurs u et v (concaténation)
In[13] np.hstack([u, v])              #    "          "          "            "            "
In[14] np.concatenate((u, v), axis=0) #    "          "          "            "            "
In[15] np.concatenate([u, v], axis=0) #    "          "          "            "            "
In[16] np.vstack((u, v))              # superpose les vecteurs u et v en colonne
In[17] np.vstack([u, v])              #    "          "          "            "
In[18] np.concatenate((u, v), axis=1) #    "          "          "            "
In[19] np.concatenate([u, v], axis=1) #    "          "          "            "
In[20] array[i,j,k]                   # valeur de l'entrée ligne i, colonne j, profondeur k ...
```

# Les vecteurs : `numpy.ndarray` de dimension 1

```python
In[] import numpy as np
In[] np.empty(n)                  # vecteur de n composantes "vide" /!\
In[] np.zeros(n)                  # vecteur de n composantes initilisées à 0.
In[] np.ones(n)                   # vecteur de n composantes initilisées à 1.
In[] np.empty_like(v)             # copie la dimension et le nombre de composante de v
In[] np.zeros_like(v)             #   "        "       "        "        "        "
In[] np.ones_like(v)              #   "        "       "        "        "        "

In[] +v                           # identité
In[] -v                           # opposé
In[] np.linalg.norm(v,1)          # norme "1" sur l'espace vectoriel
In[] np.linalg.norm(v)            # norme euclidienne ("2") sur l'espace vecoriel
In[] np.linalg.norm(v, np.inf)    # norme "max" sur l'espace vectoriel
In[] np.linalg.norm(u-v,1)        # distance "1" sur l'espace vectoriel
In[] np.linalg.norm(u-v)          # distance euclidienne ("2") sur l'espace vectoriel
In[] np.linalg.norm(u-v, np.inf)  # norme "max" sur l'espace vectoriel

In[] u + v                        # addition vectorielle
In[] u - v                        # soustraction vectorielle
In[] mu * u                       # multiplication par un scalaire mu

In[] u * v                        # multiplication composante par composante
In[] np.einsum('i,i->i', u, v)    # multiplication composante par composante

In[] u @ v                        # produit scalaire
In[] np.inner(u, v)               # produit scalaire
In[] np.einsum('i,i->', u, v)     # produit scalaire

In[] np.kron(u, v)                # produit de Kronecker

In[] np.outer(u, v)               # produit extérieur, ou tensoriel
In[] np.tensordot(u, v, axes=0)   # produit extérieur, ou tensoriel
In[] np.einsum('i,j->ij', u, v)   # produit extérieur, ou tensoriel

In[] u is v                       # test d'identité
In[] np.allclose(u,v,eps)         # test d'égalité des vecteurs à la précision eps près
In[] np.all(u > 0)                # test : True <=> "u[i] > 0" pour tout i ; False sinon
In[] np.any(u > 0)                # test : True <=> "u[i] > 0" pour au moins un i ; False sinon
In[] u == v                       # test d'égalité composante par composante (vecteur booléen : b[i] = True si u[i] <= v[i] False sinon)
In[] u <= v                       # test inférieur ou égal à (vecteur booléen)
In[] u < v                        # test strictement inférieur à (vecteur booléen)
In[] u >= v                       # test supérieur ou égal à (vecteur booléen)
In[] u > v                        # test strictement supérieur à (vecteur booléen)

In[] np.argmin(u)                 # indexe de la plus petite composante du vecteur
In[] np.argmax(u)                 # indexe de la plus grande composante du vecteur
In[] np.argwhere(u<0)             # vecteur colonne des indexes pour lesquels u[i] < 0
```

# Les matrices : `numpy.ndarray` de dimension 2

Veiller à ce que les dimensions des matrices sont compatibles.

```python
In[] import numpy as np
In[] np.empty((m,n))              # matrice m x n "vide" /!\
In[] np.zeros((m,n))              # initialisation à 0. d'une matrice m x n
In[] np.ones((m,n))               # initialisation à 1. d'une matrice m x n
In[] np.empty_like(M)             # copie la taille de M
In[] np.zeros_like(M)             # copie la taille de M
In[] np.ones_like(M)              # copie la taille de M

In[] +M                           # identité
In[] -M                           # opposée
In[] np.linalg.norm(M)            # norme euclidienne sur Mat(m,n) (c.f section précédente pour les variantes)
In[] np.linalg.norm(M-N)          # distance euclidienne sur Mat(m,n) (c.f section précédente pour les variantes)

In[] np.linalg.matrix_rank(M)     # rang de la matrice M

In[] np.transpose(M)              # transposée de M
In[] M.T                          # transposée de M

In[] np.triu(M)                   # matrice dont les entrées sont celles de la partie triangulaire supérieure de M
In[] np.triu_indices(entier)      # indices de la partie triangulaire supérieure d'une matrice ayant <entier> colonnes
In[] np.triu_indices(liste)       # indices des parties triangulaires supérieures de blocs d'une matrice, imbriqués, de tailles <liste[0]>, <liste[1]> ...
In[] np.triu_indices_from(M)      # indices de la partie triangulaire supérieure de la matrice M
In[] np.tril(M)                   # matrice dans les entrées sont celles de la partie triangulaire inférieure de M
In[] np.tril_indices(entier)      # indices de la partie triangulaire inférieure d'une matrice ayant <entier> colonnes
In[] np.tril_indices(liste)       # indices des parties triangulaires inférieures de blocs d'une matrice, imbriqués, de tailles <liste[0]>, <liste[1]> ...
In[] np.tril_indices_from(M)      # indices de la partie triangulaire inférieure de la matrice M

In[] M + N                        # addition matricielle
In[] M - N                        # soustraction matricielle
In[] mu * M                       # produit par un scalaire mu

In[] M * N                        # multiplication composante par composante, produit "de Hadamard" ou "de Schur"
In[] np.einsum('ij,ij->ij', M, N) # multiplication composante par composante, produit "de Hadamard" ou "de Schur"

In[] M @ N                        # produit matriciel
In[] np.dot(M, N)                 # produit matriciel
In[] np.tensordot(M, N, axes=1)   # produit matriciel
In[] np.einsum('ij,jk->ik', M, N) # produit matriciel

In[] np.inner(M, N)               # produit intérieur
In[] np.einsum('ij,kj->ik', M, N) # produit intérieur

In[] np.kron(M, N)                # produit de Kronecker

In[] np.outer(M, N)               # produit extérieur, ou tensoriel

In[] M is N                       # test d'identité
In[] np.allclose(M,N,eps)         # test d'égalité des vecteurs à la précision eps près
In[] np.all(M > 0)                # test : True <=> "M[i,j] > 0" pour tout (i,j) ; False sinon
In[] np.any(M > 0)                # test : True <=> "M[i,j] > 0" pour au moins un (i,j) ; False sinon
In[] M == N                       # test d'égalité composante par composante (vecteur booléen : b[i,j] = True si M[i,j] <= N[i,j] False sinon)
In[] M <= N                       # test inférieur ou égal à (vecteur booléen)
In[] M < N                        # test strictement inférieur à (vecteur booléen)
In[] M >= N                       # test supérieur ou égal à (vecteur booléen)
In[] M > N                        # test strictement supérieur à (vecteur booléen)
```
