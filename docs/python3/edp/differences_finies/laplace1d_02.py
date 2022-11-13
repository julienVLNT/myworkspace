#!/bin/python3
#! -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

a = 0
b = 1
n = 126
h = (b-a)/(n+1)
x = np.linspace(a, b, n+2, endpoint=True)

A = 2/h**2 * np.diag(np.ones(n+1))   \
  - 1/h**2 * np.diag(np.ones(n), -1) \
  - 1/h**2 * np.diag(np.ones(n),  1)

A[0, 0] += 1/h**2 
A[0, 1] -= 1/h**2

B      = np.zeros(n+1)
B[0]  -= 2/h*0
B[-1] += 1/h**2

u = np.linalg.solve(A, B)
u = np.hstack((u, [1]))

plt.plot(x, x, label="$u(x)$")
plt.plot(x, u, 'x', label="$u_h(x)$")
plt.legend()
plt.title("Laplacien Dirichlet / Robin par D.F")
plt.savefig("sol.jpg")
