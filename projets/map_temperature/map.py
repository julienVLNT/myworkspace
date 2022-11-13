#! conda activate Climate
#! -*- encoding: utf-8 -*-

import glob
import matplotlib.pyplot as plt
from matplotlib.animation import ArtistAnimation
import xarray as xr

# Lecture des fichiers
liste=glob.glob('./*.grib2.tmp')

ds = []
for nom in liste:
	print("Import du dataset n°", len(ds)+1)
	ds.append(xr.load_dataset(nom, engine='cfgrib'))

print("Imports terminés !")
print("Construction de l'animation...")

# Création de l'animation
fig=plt.figure(figsize=(15,15))

print("\tConstruction de l'image n°", 1)
images=[[ds[0].t2m.plot()]]
for i in range(1,len(ds)):
	print("\tConstruction de l'image n°", i+1)
	images.append([ds[i].t2m.plot(add_colorbar=False)])

del liste
del ds

# Export
print("Enregistrement au format .mp4 ...")
animation=ArtistAnimation(fig,images,interval=1000,repeat=True,repeat_delay=2000)
animation.save("tmp3.mp4")