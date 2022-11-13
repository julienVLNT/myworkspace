################################################################################
#   DOCUMENTATION : DOCKER ET SES DOCKERFILES                                  #
#   -----------------------------------------                                  #
#   Julien VALENTIN                                                            #
#   30 Octobre 2022                                                            #
################################################################################

SITE WEB
--------
https://www.docker.com/


INSTALLATION
------------
    Linux/Ubuntu
    ------------
```
sudo apt-get update
sudo apt-get install \
    ca-certificates \
    curl \
    gnupg \
    lsb-release

sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg

echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-compose-plugin
```


USAGE
-----


EXEMPLE
-------
    ./cartes_meteo_python/dockerfile
    --------------------------------
        Environnement de developpement virtuel pour telecharger, utiliser et 
        post-traiter les donnees de sorties des modeles ALADIN, ARPEGE et AROME
        de Meteo-France, hebergees sur Amazon Web Service et distribuees au
        format GRIB2. L'environnement connait les scripts de telechargement des 
        donnees ainsi que la commande conda pour la gestion de l'environnement 
        Python. Les outils de data sciences et d'analyse numerique classiques y 
        sont installes. L'interface est assuree par le service Jupyter Lab.
