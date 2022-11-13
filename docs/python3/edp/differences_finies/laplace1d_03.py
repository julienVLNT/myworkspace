#!/bin/python3
#! -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

a = 0
b = 1
n = 30
x = np.zeros(n+2)
for i in range(n+2):
    if i==0:
        x[i] = a
    elif i==n+1:
        x[i] = b
    else:
        x[i] = x[i-1] + 1/2**i
h = np.diff(x)

A = np.zeros((n,n))
for i in range(n):
    if i==0:
        A[i, i]   =  2/(h[0]*h[1])
        A[i, i+1] = -2/(h[1]**2 + h[0]*h[1])
    elif i==n-1:
        A[i, i-1] = -2/(h[-2]**2 + h[-2]*h[-1])
        A[i, i]   =  2/(h[-2]*h[-1])
    else:
        A[i, i-1] = -2/(h[i-1]**2 + h[i-1]*h[i])
        A[i, i]   =  2/(h[i-1]*h[i])
        A[i, i+1] = -2/(h[i]**2 + h[i-1]*h[i])

B = np.zeros(n)
B[-1] += 2/(h[-1]**2 + h[-2]*h[-1])
        
u = np.linalg.solve(A, B)
u = np.hstack(([0], u, [1]))

plt.plot(x, x, label="$u(x)$")
plt.plot(x, u, 'x', label="$u_h(x)$")
plt.legend()
plt.title("Laplacien Dirichlet par D.F")
plt.savefig("sol.jpg")
