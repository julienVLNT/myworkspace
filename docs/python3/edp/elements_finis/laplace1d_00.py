#!/bin/python3
#! -*- coding : utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

a = 0
b = 1
n = 15
h = (b-a)/n
x = np.linspace(a, b, n+1, endpoint=True)

K = np.zeros((n,n))
Ke = 1/h*np.asarray([
    [1, -1],
    [-1, 1]
])
for k in range(n-1):
    for i in range(2):
        for j in range(2):
            p = k + i
            q = k + j
            K[p, q] += Ke[i, j]
K[0 , 0]  += 1/h
K[-1, -1] += 1/h

B = np.zeros(n)
B[-1] += 1/h

u = np.linalg.solve(K, B)
u = np.hstack(([0], u, [1]))

plt.plot(u, label="$u_h(x)$")
plt.legend()
plt.title("Laplacien de Dirichlet, F.E P1")
plt.savefig("sol.jpg")
