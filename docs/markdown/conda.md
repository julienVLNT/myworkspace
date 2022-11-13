
- [Installer *Anaconda*](#installer-anaconda)
  - [Via le dépôt (*Debian-based* distro)](#via-le-dépôt-debian-based-distro)
- [Usage](#usage)
  - [Gestion de `conda`](#gestion-de-conda)
  - [Les environnements](#les-environnements)
  - [Installer des librairies](#installer-des-librairies)

Aide-mémoire composé par Julien VALENTIN en mars 2022.

# Installer *Anaconda*

Télécharger le fichier d'installation en fonction de la plateforme et de la distribution voulue : minimale ou standard.
- [distribution standard](https://www.anaconda.com/products/individual)
- [distribution minimale](https://docs.conda.io/en/latest/miniconda.html)

*Mamba* est une réimplémentation du logiciel *Anaconda*. Il est également disponible en version ou minimale ou standard.
- [distribution Mamba](https://github.com/mamba-org/mamba)

Lancer le script ou l'exécutable d'installation.

## Via le dépôt (*Debian-based* distro)

```bash
mkdir conda
cd conda
curl https://repo.anaconda.com/pkgs/misc/gpgkeys/anaconda.asc | gpg --dearmor > conda.gpg
sudo install -o root -g root -m 644 conda.gpg /usr/share/keyrings/conda-archive-keyring.gpg
gpg --keyring /usr/share/keyrings/conda-archive-keyring.gpg --no-default-keyring --fingerprint 34161F5BF5EB1D4BFBBB8F0A8AEB4F8B29D82806
echo "deb [arch=amd64 signed-by=/usr/share/keyrings/conda-archive-keyring.gpg] https://repo.anaconda.com/pkgs/misc/debrepo/conda stable main" | sudo tee -a /etc/apt/sources.list.d/conda.list
sudo apt update
sudo apt install conda
source /opt/conda/etc/profile.d/conda.sh
```

# Usage

## Gestion de `conda`

- informations sur l'installation
    - `conda info`

- mise à jour de la commande `conda`
    - `conda update conda`

- installer une librairie
    - `conda install mylib`

- mise à jour d'une librairie
    - `conda update mylib`

## Les environnements

- créer un environnement
    - `conda create -n name` crée l'environnement `name`
    - `conda create -n name mylib` crée l'environnement `name` et installe la librairie `mylib` pour l'environnement
    - `conda env create --file myenv.txt` crée l'environnement à partir du fichier requirements.txt

- cloner un environnement existant
    - `conda create --clone name --name name2`

- exporter l'environnement dans un fichier texte
    - `conda list --explicit > myenv.txt`

- activer l'environnement `name`
    - `conda activate name`

- lister les librairies installées dans l'environnement courant
    - `conda list`

- désactiver l'environnement
    - `conda deactivate`

- lister les environnements
    - `conda env list`

- supprimer l'environnement `name` ainsi que ses librairies
    - `conda env remove --name name`

## Installer des librairies

- installer une librairie
    - `conda install mylib` installe dans l'environnement activé
    - `conda install mylib myotherlib` installe les deux librairies dans l'environnement activé
    - `conda install --name myenv mylib` installe dans l'environnement visé
    - `conda install numpy=1.11` installe la dernière version 1.11.x
    - `conda install numpy==1.11` installe exactement la version 1.11.0
    - `conda install numpy>=1.11` installe la dernière version ultérieure à 1.11.0
    - `conda install numpy=1.11.1|1.11.3` installe la version 1.11.1 ou 1.11.3
    - `conda install numpy>=1.8,<2` installe une version entre 1.8 incluse et 2.0 exclue

- mettre à jour une librairie
    - `conda update mylib` met à jour dans l'environnement courant

- désinstaller une librairie
    - `conda remove mylib` désinstalle la librairie de l'environnement courant
    - `conda remove mylib otherlib`
    - `conda remove --name myenv mylib` désinstalle `mylib` de l'environnement `myenv`