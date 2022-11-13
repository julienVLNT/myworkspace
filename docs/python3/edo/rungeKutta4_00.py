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

    k1 = f(y[-1])
    k2 = f(y[-1]+dt*k1/2)
    k3 = f(y[-1]+dt*k2/2)
    k4 = f(y[-1]+dt*k3)

    y = np.hstack((y, [y[-1]+dt/6*(k1+2*k2+2*k3+k4)]))

plt.plot(t, y, label="$y_h(t)$")
plt.legend()
plt.title("Runge-Kutta 4")
plt.savefig("sol.jpg")
