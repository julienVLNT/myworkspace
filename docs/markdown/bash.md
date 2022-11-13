- [Administration du système](#administration-du-système)
  - [Connaître son O.S](#connaître-son-os)
  - [Connaître son matériel](#connaître-son-matériel)
  - [l'arborescence](#larborescence)
  - [Lire ou écrire dans un fichier](#lire-ou-écrire-dans-un-fichier)
  - [Les utilisateurs et les groupes](#les-utilisateurs-et-les-groupes)
  - [Les permissions sur les répertoires et les fichiers](#les-permissions-sur-les-répertoires-et-les-fichiers)
  - [Les pages web](#les-pages-web)
  - [Les archives](#les-archives)
  - [Terminer la session](#terminer-la-session)
- [Un peu de script](#un-peu-de-script)
  - [Artihmétique en `bash`](#artihmétique-en-bash)
  - [Itérer avec `for`](#itérer-avec-for)
  - [Itérer avec `while`](#itérer-avec-while)
  - [Structure de contrôle `if...then...else`](#structure-de-contrôle-ifthenelse)
  - [Structure de contrôle `case`](#structure-de-contrôle-case)

Aide-mémoire composé par Julien VALENTIN en mars 2022.

## Administration du système

### Connaître son O.S

`uname`

`uname -a` pour tous les détails

### Connaître son matériel

`lshw -short` donne un court résumé du matériel

`lshw` donne un rapport complet du matériel

`lshw -html` donne un rapport complet au format HTML

`lshw -xml` donne un rapport complet au format HTML

`lshw -C memory` liste les caractéristiques liées à la mémoire uniquement

`lshw -C cpu` liste les caractéristiques liées au CPU uniquement

`lshw -C storage` liste les caractéristiques liées au stockage uniquement

`lshw -C network` liste les caractéristiques liées au réseau uniquement

`dmidecode` donne un autre rapport sur le matériel (issu des informations du BIOS)

### l'arborescence

- lister les fichiers du répertoire courant
    - `ls`
    - `ls /my/folder` montrera les fichiers du répertoire `my/folder`
    - `ls -a` inclut les fichiers cachés
    - `ls -l` inclut les métadonnées des fichiers
    - `tree` affiche une superbe arborescence

- se déplacer dans l'arborescence
    - `cd ~` revient dans le répertoire `/home`
    - `cd ..` remonte d'un niveau
    - `cd subfolder` descend dans le dossier `subfolder`
    - `cd /my/absolute/path` se place dans le dossier visé par le chemin absolu

- créer un dossier
    - `mkdir folder` crée le dossier `folder` dans le répertoire courant
    - `mkdir f1 f2 f3` crée les trois dossiers dans le répertoire courant
    - `mkdir -p folder/subfolder` crée la hiérarchie `folder/subfolder`, le paramètre `-p` signifiant *parent*

- copier/coller un dossier
    - `cp -r /chemin/source /chemin/destination` copie le répertoire et ses éléments de la source vers la destination. Destination est écrasée si elle existait déjà !
    - `cp -i -r /chemin/source /chemin/destination` demande confirmation avant d'écraser

- couper/coller un dossier
    - `mv /chemin/source/* /chemin/destination` coupe la source et la colle dans le dossier destination. Il faut que la destination soit déjà existante !

- supprimer un dossier
    - `rmdir folder` supprime le dossier `folder` ainsi que ses éléments
    - `rm -r folder` est équivalent ; `rm` supprime le fichier (en fait dossier `folder`) et récursivement (i.e ainsi que tous ses éléments)

- rechercher un dossier s'il existe
    - `find -type d -iname "myfolder"` recherche par nom
    - `find /ici -type d -iname "myfolder"` recherche par nom à l'emplacement `/ici`
    - `find -type d -size +50M -iname "myfolder"` recherche par nom les dossiers dont la taille est supérieure à 50Mb

- créer un fichier vide
    - `touch myfile` crée un fichier vide nommé `myfile`
    - `echo "" > myfile` fait la même chose
    - `fallocate -l 5G myfile` alloue l'espace de 5Gb sur le disque pour le fichier `myfile`

- copier/coller un fichier
    - `cp /chemin/source/myfile /chemin/destination/myfile`
    - `cp -i /chemin/source/myfile /chemin/destination/myfile` avec confirmation avant d'écraser

- couper/coller un fichier
    - `mv /chemin/source/myfile /chemin/destination/myfile` 

- rechercher un fichier s'il existe
    - `find -iname "myfile"` recherche par nom
    - `find /ici -iname "myfile"` recherche par nom dans le dossier `/ici`
    - `find -type f -iname "myfile` recherche parmis les fichiers le nom spécifié
    - `find -size +3K -iname "myfile"` recherche parmis les fichiers de taille supérieure ou égale à 3Kb

### Lire ou écrire dans un fichier

- écrire dans un fichier
    - `echo "des informations" > myfile.txt` écrit dans le fichier en écrasant les informations précédentes
  
- ajouter une ligne dans un fichier
    - `echo "encore des informations >> myfile.txt` ajoute la ligne à la fin du fichier

- lire le contenu d'un fichier
    - `cat myfile` lit le contenu et l'affiche sur la console
    - `cat myfile | less` affiche le fichier *par page*
    - `sed -n 12,18p myfile` lit uniquement les lignes 12 à 18 du fichier
    - `sed 12,18d myfile` lit le fichier sauf les lignes 12 à 18

- remplacer une chaîne de caractère dans un fichier
    - `sed 's/Foo/Bar/ myfile` remplace les occurences de `Foo` par `Bar`
    - `sed 's/Foo|foo/John/g' myfile` remplace les occurences de `Foo` et de `foo`
    - `sed '5!s/ham/cheese/'` remplace les occurences de `ham` par `cheese` à l'exception de la cinquième ligne
    - `sed '/str1/s/str2/str3/g' myfile` remplace les occurences de `str2` par `str3` seulement si `str1` est présent sur la ligne

- supprimer la dernière ligne d'un fichier
    - `sed '$d' myfile`

### Les utilisateurs et les groupes

- connaître l'utilisateur courant
    - `whoami`

- lister les utilisateurs
    - `cat /etc/passwd`

- changer d'utilisateur depuis le même invite de commandes
    - `su name`

- créer un utilisateur
    - `useradd name` crée l'utilisateur `name`. La commande est à exécuter en mode administrateur
    - `useradd -m name` crée l'utilisateur ainsi que son répertoire `/home/name`
    - `useradd -m -d /opt/name name` crée l'utilisateur et un répertoire `/opt/name` comme lieu de référence
    - `useradd -u 1500 name` crée l'utilisateur avec un UID spécifique
    - `useradd -G group name` crée l'utilisateur et l'inclut dans le groupe `group`
    - `useradd -s /bin/sh name` crée l'utilisateur et lui fixe comme shell `/bin/sh`

- modifier les informations ou paramètres d'un utilisateur
    - `usermod -d /ici name` paramètre `/ici` comme répertoire d'utilisateur
    - `usermod -g GID name` modifie le GID de l'utilisateur
    - `usermod -l id name` modifie l'identifiant de l'utilisateur
    - `usermod -u UID name` modifie l'UID de l'utilisateur
    - `usermod -s /bin/sh name` modifie le shell par défaut de l'utilisateur
    - `usermod -a -G sec_group name` ajoute un groupe secondaire `sec_group` à l'utilisateur
    - `usermod -m /ici name` déplace le contenu du répertoire de l'utilisateur `/ici`  

- créer ou modifier un mot de passe pour un utilisateur
    - `passwd name` permet de modifier le mot de passe de l'utilisateur `name`

- afficher les paramètres par défaut de `useradd`
    - `useradd –D`

- supprimer un utilisateur
    - `userdel name`
    - `userdel -r name` force l'effacement du répertoire de l'utilisateur

- créer un groupe d'utilisateurs
    - `groupadd name` crée le groupe d'utilisateurs `name`

- ajouter un utilisateur à un groupe
    - `gpasswd -a user group` ajoute `user` au groupe `group`

- retirer un utilisateur d'un groupe
    - `gpasswd -d name group` supprime l'utilisateur `name` du groupe `group`

- modifier les paramètres d'un groupe
    - `groupmod -g GID`

### Les permissions sur les répertoires et les fichiers

- changer le propriétaire d'un fichier
    - `chown user:group myfile` change le propriétaire de `myfile`, il est propriété de l'utilisateur `user` du groupe `group`

- changer les droits d'un fichier
    - `chmod a+r myfile` donne le droit de lecture à tout le monde
    - `chmod a+w myfile` donne le droit d'écriture à tout le monde
    - `chmod a+x myfile` donne le droit d'exécution à tout le monde
    - `chmod a+rwx myfile` donne les trois droits à tout le monde
    - `chmod a-r myfile` retire le droit de lecture à tout le monde
    - `chmod a-w myfile` retire le droit d'écriture à tout le monde
    - `chmod a-x myfile` retire le droit d'exécution à tout le monde
    - `chmod a-rwx myfile` retire les trois droits à tout le monde
    - `chmod u+r myfile` donne le droit de lecture à l'utilisateur
    - `chmod u+w myfile` donne le droit d'écriture à l'utilisateur
    - `chmod u+x myfile` donne le droit d'exécution à l'utilisateur
    - `chmod g+r myfile` donne le droit de lecture au groupe
    - `chmod g+w myfile` donne le droit d'écriture au groupe
    - `chmod g+x myfile` donne le droit d'exécution au groupe
    - `chmod o+r myfile` donne le droit de lecture aux autres
    - `chmod o+w myfile` donne le droit d'écriture aux autres
    - `chmod o+x myfile` donne le droit d'exécution aux autres

- changer les droits d'un dossier
    - `chmod -R u+rwx folder` comme précédemment mais récursivement

### Les pages web

`wget http://my-url.ext`

`wget http://my-url.ext --no-check-certificate` pour se passer de certificat

`wget -O filename.ext https://my-url.ext` enregistre le fichier dans `filename.ext`

`wget -P /ici/la http://my-url.ext` enregistre le fichier dans le dossier `/ici/la`

`wget -c http://my-url.ext` permet de couper le téléchargement, utile en cas de connexion instable

`wget -i url-list.txt` permet de télécharger toutes les URL contenues dans `url-list.txt`

`wget -m https://my-url.ext` crée un miroir du site visé

`wget -m -k -p https://my-url.ext` crée un miroir du site avec toutes les données nécessaires à l'affichage HTML

### Les archives

- créer une archive à partir d'un dossier
    - `tar -c -f myarchive.tar /ici/cela` compresse le contenu du dossier `/ici/cela`
    - `tar -c -v -f myarchive.tar /ici/cela` en montrant l'avancée de la compression
    - `tar -c -v -z -f myarchive.tar.gz /ici/cela` compresse le contenu au format `.tar.gz`
    - `tar -c -v -j -f myarchive.tar.bz2 /ici/cela` compresse le contenu au format `.tar.bz2`

- décompresser une archive `.tar`
    - `tar -x -v -f myarchive.tar` décompresse l'archive `myarchive.tar`
    - `tar -x -v -f myarchive.tar -C /ici` décompresse l'archive à l'emplacement `/ici`

- décompresser une archive `.tar.gz`
    - `tar -xvf myarchive.tar.gz`

- décompresser une archive `.tar.bz2`
    - `tar -xvf myarchive.tar.bz2`

- décompresser un fichier d'une archive
    - `tar -xvf archive.tar file`
    - `tar -zxvf archive.tar.gz file`
    - `tar -jxvf archive.tar.bz2 file`

- décompresser plusieurs fichiers d'une archive
    - `tar -xvf archive.tar file1 file2 file3`
    - `tar -zxvf archive.tar.gz file1 file2 file3`
    - `tar -jxvf archive.tar.bz2 file1 file2 file3`

- ajouter un fichier à une archive
    - `tar -rvf archive.tar file`
    - `tar -rvf archive.tar.gz file`
    - `tar -rvf archive.tar.bz2 file`

- lister le contenu d'une archive
    - `tar -tvf myarchive.tar`
    - `tar -t -v -f myarchive.tar.gz`
    - `tar -t -v -f myarchive.tar.bz2`

### Terminer la session

`exit`

## Un peu de script

- exécuter un script `shell`
    - `./myscript.sh`

- afficher un texte sur la sortie standard
    - `echo "Hello, World !"`

- accès au nombre d'arguments passés lors de l'appel `./myscript 1 2 3 4 5`
    - `echo $#` affiche 5 ici

- accès à l'ensemble des valeurs passées en paramètre
    - `echo $@` affiche `1 2 3 4 5`
    - `echo $*` idem

- accès au troisième paramètre passé lors de l'appel
    - `echo $3` affiche `3`

- modifier la sortie standard
    - `command > out` redirige la sortie de `command` vers `out`

- modifier le canal d'erreur standard
    - `command 2> err` redirige les messages d'erreur de `command` vers `err`

- affecter une variable
    - `var=value` affecte la valeur `value` à la variable `var`
    - `var=$(command)` affecte le retour de `command` à la variable `var`
    - `read var` demande à l'utilisateur la vaeur de la variable `var`
    - `read -p "valeur de la variable 'var': " var` permet d'insérer un texte avant la saisie
    - `let "a = 1"` affecte la valeur numérique `1` à la variable `a`

- lire la valeur d'une variable
    - `$var`
    - `${var}` pour l'insertion dans une chaîne

- protéger une variable en écriture
    - `readonly var` avant l'affectation

- supprimer une variable
    - `unset var` efface de la mémoire la variable `var`

### Artihmétique en `bash`

Pour évaluer des opérations artihmétiques `$(( 1 + 1 ))`

### Itérer avec `for`

```bash
for i in item1 item2 item3 item4
do
    echo $i
done
```

```bash
for i in {1..10..2}
do
   echo $i
done
```

```bash
for i in $(seq 1 2 20)
do
   echo $i
done
```

```bash
for (( i=0; i<11; i++ ))
do
	echo $i
done
```

```bash
mylist = (a b c d e f)
for elt in "${mylist[@]}"
do
	echo $elt
done
```

```bash
mystr = "Hello, World !"
for char in $mystr
do
	echo $char
done
```

### Itérer avec `while`

```bash
while [ BOOL ]
do
    mycommand
done
```

### Structure de contrôle `if...then...else`

```bash
if [ BOOL ]
then
    mycommand
else
    mycommand
fi
```

### Structure de contrôle `case`

```bash
case  $variable-name  in
    pattern1) mycommand;;
    pattern2) mycommand;;
    pattern3) mycommand;;            
    *)              
esac
```

```bash
case  $variable-name  in
    pattern1|pattern2|pattern3) mycommand;;
    pattern4|pattern5|pattern6) mycommand;;
    pattern7|pattern8|patternN) mycommand;;
    *)
esac
```