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

for _ in range(nt):
    k1 = y[-1] + dt/2*f(y[-1])
    k2 = f(k1)
    y = np.hstack((y, [y[-1] + dt*k2]))

plt.plot(t, y, label="$y_h(t)$")
plt.legend()
plt.title("Runge-Kutta 2")
plt.savefig("sol.jpg")
