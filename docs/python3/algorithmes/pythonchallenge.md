Mes solutions aux problèmes de [The Python Challenge](http://www.pythonchallenge.com/). Les codes sont numérotés par problème.

| **Problème** | **URL**                                                | **Résolu ?** |
|--------------|--------------------------------------------------------|--------------|
| 0            | [clic](http://www.pythonchallenge.com/pc/def/0.html)   | ✓ 16/03/2002 |
| 1            | [clic](http://www.pythonchallenge.com/pc/def/map.html) | ✓ 25/03/2002 |
| 2            | [clic](http://www.pythonchallenge.com/pc/def/ocr.html) | 

---

# Résultats <!-- omit in toc -->

- [Problème 0 : "Warmup"](#problème-0--warmup)
- [Problème 1](#problème-1)
- [Problème 2](#problème-2)

## Problème 0 : "Warmup"

Il suffit de modifier l'URL de la page : changer $0$ pour le résultat de $2^{38}$. L'URL est donc

```
http://www.pythonchallenge.com/pc/def/274877906944.html
```

## Problème 1

On voit trois associations de lettres. Chaque fois, le décalage entre les deux vaut +2. On devine qu'il va falloir appliquer ce décalage à chaque caractère du texte écrit en mauve.
On déclare la variable `crypte` contenant la chaîne de caractère à traduire.

```python
>>> crypte = "g fmnc wms bgblr rpylqjyrc gr zw fylb. rfyrq ufyr amknsrcpq ypc dmp. bmgle gr gl zw fylb gq glcddgagclr ylb rfyr'q ufw rfgq rcvr gq qm jmle. sqgle qrpgle.kyicrpylq() gq pcamkkclbcb. lmu ynnjw ml rfc spj."
```

Essayons

```python
>>> clef  = lambda c : chr(ord(c)+2)
>>> clair = ''.join([clef(car) for car in crypte])
```

On obtient

```python
>>> clair
'i"hope"you"didnt"tr{nsl{te"it"|y"h{nd0"th{ts"wh{t"computers"{re"for0"doing"it"in"|y"h{nd"is"inefficient"{nd"th{t)s"why"this"text"is"so"long0"using"string0m{ketr{ns*+"is"recommended0"now"{pply"on"the"url0'
```

Ce n'est pas diablement efficace. Le problème vient du fait que seuls les $26$ premiers caractères sont des lettres, si bien que les caractères dont le code est $25$ ou $26$ sont envoyés sur des symboles. Pour ces deux caractères, il est nécessaire de leur associer respectivement les codes $1$ et $2$.

```python
>>> clair  = ''.join([chr(((ord(car) + 2) - ord('a')) % 26 + ord('a')) if car >= 'a' and car <= 'z' else car for car in crypte])
>>> clair
"i hope you didnt translate it by hand. thats what computers are for. doing it in by hand is inefficient and that's why this text is so long. using string.maketrans() is recommended. now apply on the url."
```

La suggestion d'utiliser la méthode pour les chaînes de caractères : `maketrans()`, se fait ainsi

```python
>>> table1 = "abcdefghijklmnopqrstuvwxyz"
>>> table2 = "cdefghijklmnopqrstuvwxyzab"
>>> clef = str.maketrans(table1, table2)

>>> clair = crypte.translate(clef)
>>> clair
"i hope you didnt translate it by hand. thats what computers are for. doing it in by hand is inefficient and that's why this text is so long. using string.maketrans() is recommended. now apply on the url."
```

Cela nous donne, appliqué au mot crypté `map`:

```python
>>> "map".translate(clef)
'ocr'
```

## Problème 2

