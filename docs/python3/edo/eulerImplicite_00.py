#!/bin/python3
#! -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np
import scipy.optimize as spo

def f(x):
    return x

t0 = 0
tf = 1
nt = 32
dt = (tf-t0)/nt
t  = np.linspace(t0, tf, nt+1, endpoint=True)

y0 = 1
y  = np.asarray([y0])

for _ in range(nt):
    y = np.hstack((y, [spo.fsolve(
            lambda u: u - y[-1] - dt*f(u), 
            y[-1])[0]])
        )

plt.plot(t, y, label="$y_h(t)$")
plt.legend()
plt.title("Euler Implicite")
plt.savefig("sol.jpg")
