#!/bin/python3
#! -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

def f(x):
    return x

t0 = 0
tf = 1
nt = 32
dt = (tf-t0)/nt
t  = np.linspace(t0, tf, nt+1, endpoint=True)

y0 = 1
y  = np.asarray([y0])

for n in range(nt):
    y_ = y[-1] + dt*f(y[-1])
    y  = np.hstack((y, [y_]))

plt.plot(t, y, label="$y_h(t)$")
plt.legend()
plt.title("Euler explicite")
plt.savefig("sol.jpg")
