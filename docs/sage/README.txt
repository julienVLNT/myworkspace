################################################################################
#   DOCUMENTATION : LANGAGE SAGE                                               #
#   ----------------------------                                               #
#   Julien VALENTIN                                                            #
#   04 Novembre 2022                                                           #
################################################################################

SITE WEB
--------
https://www.sagemath.org/


INSTALLATION
------------
    Linux/Ubuntu
    ------------
```
sudo apt-get update
sudo apt-get install --yes bc \
                           binutils \
                           bzip2 \
                           ca-certificates \
                           cliquer \
                           cmake \
                           curl \
                           ecl \
                           eclib-tools \
                           fflas-ffpack \
                           flintqs \
                           g++ \
                           gcc \
                           gengetopt \
                           gfan \
                           gfortran \
                           glpk-utils \
                           gmp-ecm \
                           lcalc \
                           libatomic-ops-dev \
                           libboost-dev \
                           libbraiding-dev \
                           libbrial-dev \
                           libbrial-groebner-dev \
                           libbz2-dev \
                           libcdd-dev \
                           libcdd-tools \
                           libcliquer-dev \
                           libcurl4-openssl-dev \
                           libec-dev libecm-dev \
                           libffi-dev \
                           libflint-arb-dev \
                           libflint-dev \
                           libfplll-dev \
                           libfreetype6-dev \
                           libgc-dev \
                           libgd-dev \
                           libgf2x-dev \
                           libgiac-dev \
                           libgivaro-dev \
                           libglpk-dev \
                           libgmp-dev \
                           libgsl-dev \
                           libhomfly-dev \
                           libiml-dev \
                           liblfunction-dev \
                           liblinbox-dev \
                           liblrcalc-dev \
                           liblzma-dev \
                           libm4ri-dev \
                           libm4rie-dev \
                           libmpc-dev \
                           libmpfi-dev \
                           libmpfr-dev \
                           libncurses5-dev \
                           libntl-dev \
                           libopenblas-dev \
                           libpari-dev \
                           libpcre3-dev \
                           libplanarity-dev \
                           libppl-dev \
                           libprimesieve-dev \
                           libpython3-dev \
                           libqhull-dev \
                           libreadline-dev \
                           librw-dev \
                           libsingular4-dev \
                           libsqlite3-dev \
                           libssl-dev \
                           libsuitesparse-dev \
                           libsymmetrica2-dev \
                           libz-dev \
                           libzmq3-dev \
                           libzn-poly-dev \
                           m4 \
                           make \
                           nauty \
                           ninja-build \
                           openssl \
                           palp \
                           pari-doc \
                           pari-elldata \
                           pari-galdata \
                           pari-galpol \
                           pari-gp2c \
                           pari-seadata \
                           patch \
                           perl \
                           pkg-config \
                           planarity \
                           ppl-dev \
                           python3 \
                           python3-distutils \
                           python3-venv \
                           r-base-dev \
                           r-cran-lattice \
                           singular \
                           singular-doc \
                           sqlite3 \
                           sympow \
                           tachyon \
                           tar \
                           tox \
                           xcas \
                           xz-utils
sudo apt-get install --yes default-jdk \
                           dvipng \
                           ffmpeg \
                           imagemagick \
                           latexmk \
                           libavdevice-dev \
                           pandoc \
                           tex-gyre \
                           texlive-fonts-recommended \
                           texlive-lang-cyrillic \
                           texlive-lang-english \
                           texlive-lang-european \
                           texlive-lang-french \
                           texlive-lang-german \
                           texlive-lang-italian \
                           texlive-lang-japanese \
                           texlive-lang-polish \
                           texlive-lang-portuguese \
                           texlive-lang-spanish \
                           texlive-latex-extra \
                           texlive-xetex

cd ~
mkdir .sage
cd .sage
ORIG=https://github.com/sagemath/sage.git
git clone -c core.symlinks=true --branch develop --tags $ORIG
cd sage
make configure
./configure
export MAKE="make -j4"
export V=0
make
```


USAGE
-----


EXEMPLES
--------
    ./hellosagemath.ipynb
    ---------------------
        Notebook d'introduction a SageMath.

    ./algebre/module_libre_de_rang_fini_sur_anneau_commutatif.ipynb
    ---------------------------------------------------------------
        Notebook de rappel de la theorie des modules libres de rang finis sur 
        un anneau commutatif, disponible dans la documentation SageMath.
