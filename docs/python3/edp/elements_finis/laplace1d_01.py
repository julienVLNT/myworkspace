#!/bin/python3
#! -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

a = 0
b = 1
n = 5
h = (b-a)/n
x = np.linspace(a, b, 2*n+1, endpoint=True)

K = np.zeros((2*n-1,2*n-1))
Ke = 2/h*np.asarray([
    [7/6, -4/3, 1/6],
    [-4/3, 8/3, -4/3],
    [1/6, -4/3, 7/6]
])
for k in range(n-1):
    for i in range(3):
        for j in range(3):
            p = 2*k + i
            q = 2*k + j
            K[p, q] -= Ke[i, j]
K[0, 0]   -= 7/3/h
K[-1, -1] -= 7/3/h

B = np.zeros(2*n-1)
B[-1] -= 7/3/h

u = np.linalg.solve(K, B)
u = np.hstack(([0], u, [1]))

plt.figure()
plt.plot(x, u, label="$u_h(x)$")
plt.legend()
plt.title("Laplacien de Dirichlet, F.E P2")
plt.savefig("sol.jpg")
