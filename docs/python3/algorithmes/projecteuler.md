Mes solutions aux problèmes archivés sur [ProjectEuler.net](https://projecteuler.net/).

| **Problème** | **Résolu ?**  |
|--------------|---------------|
| 1            | ✓ 16/03/2022  |
| 2            | ✓ 16/03/2022  |
| 3            | ✓ 16/03/2022  |
| 4            | ✓ 17/03/2022  |
| 5            | ✓ 01/04/2022  |
| 6            | ✓ 04/04/2022  |
| 7            | ✓ 04/04/2022  |
| 8            |  |

--- 

# Solutions <!-- omit in toc -->

- [Problème 1](#problème-1)
  - [Solution naïve](#solution-naïve)
  - [Un peu plus élégant](#un-peu-plus-élégant)
- [Problème 2](#problème-2)
  - [Solution naïve](#solution-naïve-1)
  - [Un peu plus élégant](#un-peu-plus-élégant-1)
- [Problème 3](#problème-3)
  - [Solution naïve](#solution-naïve-2)
  - [Beaucoup plus efficace sans beaucoup d'efforts](#beaucoup-plus-efficace-sans-beaucoup-defforts)
- [Problème 4](#problème-4)
  - [Une première solution](#une-première-solution)
- [Problème 5](#problème-5)
  - [Solution](#solution)

## Problème 1

- [énoncé](https://projecteuler.net/problem=1)

On note

- `L` la borne supérieure
- `m` le premier diviseur
- `n` le second diviseur

de sorte que la solution est `problem1(1000, 3, 5)`.

### Solution naïve

Il suffit de parcourir les entiers entre $1$ et `L` exclu et d'additionner lorsque l'entier est divisible par `m` ou par `n`.

```python
def problem1(L, m, n):
    S = 0
    for i in range(L):
        if i % m == 0 or i % n == 0:
            S += i
    return S

assert problem1(10, 3, 5) == 23, "Le cas test a échoué !"

print(problem1(1000, 3, 5))
```

La somme $S$ cherchée est $$ \boxed{ S = 233168 } $$

### Un peu plus élégant

```python
def problem1(L, m, n):
    i_m  = (L-1) // m
    i_n  = (L-1) // n
    i_mn = (L-1) // (m*n)    # (m*n) est à remplacer par PPCM(m,n) dans un cas plus général

    S = (i_m+1)*i_m/2 * m + (i_n+1)*i_n/2 * n - (i_mn+1)*i_mn/2 * (m*n)
    return S

assert problem1(10, 3, 5) == 23, "Le cas test a échoué !"
print(problem1(1000, 3, 5))
```

La somme $S$ cherchée est $$ \boxed{ S = 233168 } $$

## Problème 2

- [énoncé](https://projecteuler.net/problem=2)

On note

- `L` la borne supérieure des valeurs $4.000.000.000$
- `p` la valeur du modulo $2$

### Solution naïve

```python
def problem2(L, p):

    def suivant(i, j):
        return i + j

    S = 0
    k = 0
    l = 1
    m = suivant(k, l)
    while m < L:
        if m % p == 0:
            S += m
        k = l
        l = m
        m = suivant(k, l)
    return S

print(problem2(4000000, 2))
```

La somme cherchée est $$ \boxed{ S = 4.613.732 } $$

### Un peu plus élégant

```python
from math import sqrt

def problem2(L):

    def fib(n):
        "Implémente la formule de Binet"
        return int( ( ( (1 + sqrt(5))/2 )**n - ( (1 - sqrt(5))/2 )**n ) / sqrt(5) )

    S = 0
    i = 3
    f = fib(i)
    while f < L:
        print(f)
        S += f
        i += 3
        f  = fib(i)
    return S

print(problem2(4000000))
```

renvoie

$$ \boxed{ S = 4.613.732 } $$

## Problème 3

- [énoncé](https://projecteuler.net/problem=3)

On note

- `L` le nombre à factoriser : $600.851.475.143$

### Solution naïve

La solution naïve consiste à parcourir les entiers entre $2$ et $(L/2)+1$. Ainsi, dès qu'un entier est premier, on teste s'il divise $L$. Dans ce cas, on l'ajoute à la liste et alors on peut diviser $L$ par ce nombre et continuer les tests.

```python
from math import floor, sqrt

def probleme3(L):

    def estPremier(i):
        drapeau = True
        if i < 2: drapeau = False
        for j in range(2, floor(sqrt(i))+1):
            if i % j == 0:
                drapeau = False
        return drapeau

    def facteurs(i):
        liste = list()
        for j in range(i):
            if estPremier(j):
                if i % j == 0:
                    liste.append(j)
                    i = i/j
        return liste

    liste = facteurs(L)
    print(f"La liste des facteurs premiers du cas test est\n{liste}")
    return liste[-1]

print("\n", probleme3(13195))
```

Cet algorithme fonctionne sur le cas test, on trouve comme liste `[5, 7, 13, 29]` et la solution renvoyée par `probleme3(13195)` est bien $29$. Cependant, le nombre cité par le problème est bien trop grand pour le factoriser de la sorte (exécution > 15 min). Heureusement, il y a des pistes d'amélioration.

### Beaucoup plus efficace sans beaucoup d'efforts

En fait, il suffisait de continuer à diviser $L$ par le nombre premier trouvé pour le réduire encore ; dès que $L = 1$, la factorisation est terminée. Il est raisonnable d'espérer un énorme gain en temps d'exécution.

```python
from math import floor, sqrt

def probleme3(L):

    def estPremier(i):
        drapeau = True
        if i < 2: drapeau = False
        for j in range(2, floor(sqrt(i))+1):
            if i % j == 0:
                drapeau = False
        return drapeau

    def facteurs(i):
        liste = list()
        for j in range(2, floor(i/2)+1):
            if i == 1: break    # Astuce
            
            if estPremier(j):
                if i % j == 0:
                    liste.append(j)
                    i = i/j
                    while(i%j==0):    # division en cascade
                        i = i/j
        
        return liste

    liste = facteurs(L)
    return liste[-1]

print(probleme3(600851475143))
```

renvoie comme plus grand facteur $$ \boxed{ p = 6857 } $$

## Problème 4

- [énoncé](https://projecteuler.net/problem=4)

On note

- `L` le nombre de chiffres, $L = 3$.

### Une première solution

Un palindrome à $4$ chiffres s'écrit, dans le système décimal, 

$$ a_0 10^3 + a_1 10^2 + a_1 10^1 + a_0 10^0 $$

De même, un palindrome à $6$ chiffres s'écrit

$$ a_0 10^5 + a_1 10^4 + a_2 10^3 + a_2 10^2 + a_1 10 + a_0 $$

Les construire demande d'itérer sur trois variables, chaque $a_i$ parcourant la liste $\{0, 1, \dots, 9\}$. On ordonne la liste dans l'ordre décroissant.

```python
def probleme4(L):
    
    def palindrome2():
        liste = list()
        for a0 in range(1, 10):
            for a1 in range(10):
                    liste.append(a0*10**3 + a1*10**2 + a1*10 + a0)
        liste.sort(reverse=True)
        return liste

    def palindrome3():
        "Renvoie la liste des palindromes à six chiffres."
        liste = list()
        for a0 in range(1, 10):
            for a1 in range(10):
                for a2 in range(10):
                    liste.append(a0*10**5 + a1*10**4 + a2*10**3 + a2*10**2 + a1*10 + a0)
        liste.sort(reverse=True)
        return liste

    generateur = palindrome3 if L == 3 else palindrome2 if L == 2 else None
    for p in generateur():
        for m in range(10**L-1, 10**(L-1)-1, -1):
            q, r = p // m, p % m
            if r == 0 and q < 10**L and q > 10**(L-1):
                return p
    return None
    

assert probleme4(2) == 9009
print(probleme4(3))
```

Le programme ci-dessus rend $$ \boxed{ P = 906609 } $$

## Problème 5

- [énoncé](https://projecteuler.net/problem=5)

### Solution

On cherche le plus petit nombre divisible par chaque entier compris entr $1$ et $20$ inclus. Il suffit de construire la liste des décompositions en facteurs premiers de chaque entier dans la liste $\{1, \dots, L\}$ et de construire le produit des facteurs apparaissant dans l'ensemble de ces factorisations, élevés à la puissance la plus forte.

Les nombres premiers inférieurs à $20$ sont $2$, $3$, $5$, $7$, $11$, $13$, $17$ et $19$. D'autre part, on écrit les décompositions des autres nombres

|                    |                   |                   |            |                      |
|--------------------|-------------------|-------------------|------------|----------------------|
| $4 = 2^2$          | $6 = 2 \times 3$  | $8 = 2^3$         | $9=3^2$    | $10 = 2 \times 5$    |
| $12 = 2^2 \times 3 | $14 = 2 \times 7$ | $15 = 3 \times 5$ | $16 = 2^4$ | $18 = 2 \times 3^2$  |

D'où le résultat :

```python
>>> 2**4 * 3**2 * 5 * 7 * 11 * 13 * 17 * 19
232792560
```

## Problème 6

- [énoncé](https://projecteuler.net/problem=6)

### Solution

Le carré de la somme des entiers inférieurs ou égaux à $100$ s'écrit $$ S = \left( \frac{100 \times 101}{2} \right)^2 $$ D'autre part, on prétend que la somme des mêmes entiers élevés au carré s'exprime $$ \frac{100 \times 101 \times 201}{6} = \frac{100^3}{3} - \frac{100^2}{2} + \frac{100}{6} $$ d'où le résultat

```python
>>> (100*101/2)**2 - (100**3)/3 + (100**2)/2 - 100/6
25174150
```

## Problème 7

- [énoncé](https://projecteuler.net/problem=7)

La réponse donnée par [PrimePage](https://primes.utm.edu/nthprime/algorithm.php) est $104743$

## Problème 8

- [énoncé](https://projecteuler.net/problem=8)
