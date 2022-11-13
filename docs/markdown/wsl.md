- [Windows Subsystem for Linux v.2](#windows-subsystem-for-linux-v2)
  - [Installation](#installation)
    - [Première configuration](#première-configuration)
  - [Gestion de WSL](#gestion-de-wsl)
    - [Gestion des distributions](#gestion-des-distributions)
  - [Exécuter](#exécuter)
  - [Paramétrer l'accélération G.P.U de WSL](#paramétrer-laccélération-gpu-de-wsl)
  - [Usage de fenêtres depuis WSL](#usage-de-fenêtres-depuis-wsl)
  - [Désinstaller une distribution](#désinstaller-une-distribution)

Aide-mémoire composé par Julien VALENTIN en mars 2022.

# Windows Subsystem for Linux v.2

## Installation

Depuis un terminal *Windows* muni des droits d'administrateur, saisir `wsl --install`. Cette commande active les composants facultatifs requis, télécharge et installe le dernier noyau *Linux* et définit *WSL2* comme valeur par défaut. Enfin, elle installe la distribution par défaut à savoir *Ubuntu*. A l'issue de cette première étape, redémarrer l'ordinateur.

- lister les distributions disponibles dans le magasin
    - `wsl --list --online`
    - `wsl -l -o`

- installer une autre distribution
    - `wsl --install -d distrib` où `distrib` est le nom de la distribution

- installer une nouvelle distribution *Linux* **depuis un invite Linux/bash**
    - `wsl.exe --install -d disrib`

- importer une distribution depuis une archive `.tar`
    - `wsl --import distrib /installer/ici archive.tar`

### Première configuration

A la première ouverture de la distribution, l'étape nécessaire est la création d'un compte utilisateur. Ce nom et le mot de passe associé sont propres à chaque distribution, et est muni des droits administrateur sur la machine virtuelle.

En cas d'oubli du mot de passe administrateur de la machine virtuelle, saisir depuis l'invite *Powershell* `wsl -u root` et alors la commande `passwd username` permet de réinitialiser le mot de passe de l'utilisateur. Si le compte n'est pas sur la distribution par défaut, exécuter plutôt `wsl -d distrib -u root`.

## Gestion de WSL

- mettre à jour *WSL*
    - `wsl --update`

- vérifier l'état de *WSL*
    - `wsl --status`

- lister les distributions installées
    - `wsl -l`

- lister les distributions installées ainsi que la version *WSL*
    - `wsl -l -v`

- définir la version *WSL* utilisée par défaut
    - `wsl --set-default-version 1` définit la version 1 par défaut
    - `wsl --set-default-version 2` définit la version 2 par défaut

- définir la distribution utilisée par défaut
    - `wsl -s defaultDistrib`

- exporter une distribution vers une archive `.tar`
    - `wsl --export distrib file.tar`

- terminer l'exécution d'une distribution
    - `wsl --terminate distrib`

- éteindre *WSL* : terminer toutes les distributions
    - `wsl --shutdown`

### Gestion des distributions

- changer l'utilisateur par défaut d'une distribution
    - `distrib config --default-user user`

## Exécuter

- exécuter une commande *bash* depuis *Powershell*
    - `wsl command` exécute `command` dans le bash de la distribution par défaut
    - `wsl -d distrib command` l'exécute dans le bash de la distribution spécifiée

- exécuter une commande *bash* depuis *Powershell* avec un utilisateur spécifique
    - `wsl -u user command`
    - `wsl --user user command`

- exécuter une commande *Windows* depuis le *shell*
    - `wsl.exe command`

- exécuter une distribution spécifique à partir de *Powershell* avec un utilisateur spécifique
    - `wsl --distribution distrib --user user`

## Paramétrer l'accélération G.P.U de WSL

## Usage de fenêtres depuis WSL

Après m'être inscrit au programme *Windows Insider*, canal *bêta*, lié à mon compte *Windows*, j'ai effectué une série de mise à jour. Après les deux redémarrages, mettre à jour le pilote graphique et enfin mettre à jour le noyau *WSL* avec `wsl --update` dans un invite de commande administrateur. Redémarrer *WSL* avec `wsl --shutdown` et c'est terminé.

Pour afficher l'environnement de bureau *Linux* sur *Windows*, précéder l'application graphique de `wsl.exe` :
- `wsl.exe xcalc`

## Désinstaller une distribution

`wsl --unregister disrib`