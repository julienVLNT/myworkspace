#!/bin/bash

# Script de téléchargement des données de température A.R.O.M.E données par le
# run 0 du jour.

for i in {0..24}
do
get_inv.pl "https://mf-nwp-models.s3.amazonaws.com/arome-france-hd/v2/2020-08-18/00/TMP/2m/""$i""h.grib2.inv" | grep -e ":TMP:" | get_grib.pl "https://mf-nwp-models.s3.amazonaws.com/arome-france-hd/v2/2020-08-18/00/TMP/2m/""$i""h.grib2" "~/Documents/AROME/tmp_""$i""h.grib2"
done
