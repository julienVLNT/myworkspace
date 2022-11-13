- [Première configuration](#première-configuration)
- [Construire une clef SSH](#construire-une-clef-ssh)
- [Gérer un dépôt](#gérer-un-dépôt)
- [Travailler sur un dépot distant](#travailler-sur-un-dépot-distant)

Aide-mémoire composé par Julien VALENTIN en mars 2022.

## Première configuration

Directement après l'installation de `git`, on peut paramétrer des variables globales pour l'environnement `git`.

- paramétrer le nom d'utilisateur
    - `git config --global user.name "Prénom NOM"`

- paramétrer l'email de l'utilisateur
    - `git config --global user.email my.adresse@email.com`

- paramétrer l'éditeur de texte favoris
    - `git config --global core.editor "My Editor"`
    - plus de détails sur les éditeurs [ici](https://git-scm.com/book/en/v2/Appendix-C%3A-Git-Commands-Setup-and-Config#ch_core_editor)

- paramétrer le nom de la branche par défaut
    - `git config --global init.defaultBranch main`

- paramétrer les couleurs pour l'affichage
    - `git config --global color.status auto`
    - `git config --global color.branch auto`
    - `git config --global color.interactive auto`
    - `git config --global color.diff auto`

- lister l'ensemble des paramètres globaux renseignés
    - `git config --list`

- afficher la valeur du paramètre spécifié
    - `git config user.name` affiche le nom de l'utilisateur

## Construire une clef SSH

- construire une clef SSH, depuis l'invite de commande `git`
    - `ssh-keygen -t ed25519 -C "my.adress@email.com"`, avec l'algorithme `ed25519`
    - `ssh-keygen -t rsa -b 4096 -C "my.adress@email.com"`, avec l'algorithme `RSA`

- déclarer la clef à l'agent SSH
    - `eval "$(ssh-agent -s)" && ssh-add ~/.ssh/id_ed25519`

Enfin, copier la clef publique et la déclarer sur son site préféré dans les paramètres du compte `Github`, `Gitlab`, ou autre.

## Gérer un dépôt

On souhaite créer un dépôt dans le répertoire `work`. On suppose que l'invite de commande est situé dans ledit dossier.

- cloner un dépôt distant
    - `git clone https://github.com/url/to/the/repo.git` pour un clonage par protocol HTTPS
    - `git clone git@github.com:github_user_name/repo.git` pour un clonage par protocol SSH
    - `gh repo clone github_user_name/repo` pour un clonage par protocol Github CLI

- initialiser un dépôt dans un dossier existant
    - `git init` pour initialiser un dépôt

- ajouter un fichier
    - `git add myfile`

- ajouter tous les fichiers avec l'extension `.c`
    - `git add *.c`

- vérifier le statut (à jour ou non) du dépôt
    - `git status`

- supprimer un fichier
    - `git rm myfile`
    - `git rm --cached myfile` supprime le fichier de la mémoire mais pas de l'arbre

- déplacer un fichier
    - `git mv /from/here to/there`

- valider les changements
    - `git commit -m "Un message"`
    - `git commit -a -m "Un message"` pour valider un changement

## Travailler sur un dépot distant

- se connecter à un dépôt distant
    - `git remote add shortname https://github.com/username/repo`

- lister l'ensemble des URL enregistrées
    - `git remote -v`

- obtenir des informations sur un dépôt distant
    - `git fetch shortname`

- enregistrer les changements sur le dépôt distant
    - `git push remote banch`

- renommer une URL
    - `git remote rename shortname newname`

- supprimer une URL
    - `git remote remove shortname`
