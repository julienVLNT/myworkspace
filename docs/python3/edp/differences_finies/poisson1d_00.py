#!/bin/python3
#! -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

def f(x):
    "Second membre"
    return np.cos(x)

a = 0
b = 2*np.pi
n = 62
h = (b-a)/(n+1)
x = np.linspace(a, b, n+2, endpoint=True)

A = 2/h**2 * np.diag(np.ones(n))       \
  - 1/h**2 * np.diag(np.ones(n-1), -1) \
  - 1/h**2 * np.diag(np.ones(n-1),  1)

B      = f(x[1:-1])
B[0]  += 1/h**2
B[-1] += 1/h**2

u = np.linalg.solve(A, B)
u = np.hstack(([1], u, [1]))

plt.plot(x, np.cos(x), label="$u(x)$")
plt.plot(x, u, 'x', label="$u_h(x)$")
plt.legend()
plt.title("Poisson par D.F")
plt.savefig("sol.jpg")
