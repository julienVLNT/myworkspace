# -*- coding: utf-8 -*-


import numpy as np
import os


__author__  = "Julien VALENTIN"
__date__    = "January, 2023"
__email__   = "julien.valentin@umontpellier.fr"
__version__ = "alpha"


current_dir = os.getcwd()                                                                   # ./source directory
work_dir    = "".join ( [directory + "/" for directory in os.getcwd().split("/")[:-1]] )    # project directory
data_dir    = work_dir + "data/Peierls"                                                     # data directory
# os.chdir (work_dir)                                                                       # go to the project directory


files = [ file for file in os.listdir (data_dir) ]
files.sort()


peierls = np.loadtxt (data_dir + "/" + files.pop(0), dtype=np.float64)
while (len (files) > 0):

    peierls = np.vstack ([
                            peierls,
                            np.loadtxt (data_dir + "/" + files.pop(0), dtype=np.float64)
                        ])


temporal_statistics = { 'median' : np.median (peierls, axis=0),
                        'mean'   : np.mean (peierls, axis=0),
                        'std'    : np.std (peierls, axis=0),
                        'var'    : np.var (peierls, axis=0) }

spatial_statistics = { 'median' : np.median (peierls, axis=1),
                       'mean'   : np.mean (peierls, axis=1),
                       'std'    : np.std (peierls, axis=1),
                       'var'    : np.var (peierls, axis=1) }
