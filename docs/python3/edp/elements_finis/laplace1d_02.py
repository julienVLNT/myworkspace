#!/bin/python3
#! -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

def baseP1(x):
    return 0.5*np.asarray([1.0-x, 1.0+x])

def dBaseP1(x):
    return 0.5*np.asarray([-1.0, 1.0])

a = 0
b = 1
n = 5
h = (b-a)/n

noeuds   = np.linspace(a, b, n+1, endpoint=True).reshape(n+1, 1)
elements = np.asarray([[i, i+1] for i in range(n)])

matriceRaideur = np.zeros((n+1, n+1))
divergence = np.zeros((1, 2))
abscisse_quadrature = np.asarray([x/np.sqrt(3) for x in [-1, 1]])

for e in elements:

    x_global = noeuds[e, :]
    raideurLocale = np.zeros((2, 2))

    for xq in abscisse_quadrature:
        dN = dBaseP1(xq)
        J  = 1/h
        dN = J*dN
        divergence[]
