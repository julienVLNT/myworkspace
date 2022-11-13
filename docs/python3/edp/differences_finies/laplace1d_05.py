#/bin/python3
#! -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

a = 0
b = 1
n = 62
h = (b-a)/(n+1)
x = np.linspace(a, b, n+2, endpoint=True)

def Q(u):
    "Coefficient non lineaire."
    return np.ones_like(u)

def dQ(u):
    "Variation du coefficient non lineaire."
    return np.zeros_like(u)

def f(u):
    "Second membre."
    return np.zeros_like(u)

def df(u):
    "Variation du second membre."
    return np.zeros_like(u)

def F(u):
    "Residu."
    res = np.zeros_like(u)

    for i in range(len(res)):
        if i == 0:
            res[i] = u[i] - 0
        elif i == len(res)-1:
            res[i] = u[i] - 1
        else:
            res[i] = 1/(2*h**2)*(                              \
                   + (Q(u[i])+Q(u[i-1]))             * u[i-1] \
                   - (Q(u[i+1])+2*Q(u[i])+Q(u[i-1])) * u[i]   \
                   + (Q(u[i+1])+Q(u[i]))             * u[i+1] \
                   ) - f(u[i])

    return res

def J(u):
    "Variations du residu."
    jac = np.zeros((len(u),len(u)))

    for i in range(len(jac)):
        if i == 0:
            jac[i, i] = 1
        elif i == len(jac)-1:
            jac[i, i-2] = 
            jac[i, i-1] =
            jac[i, i]   = 1
        else:
            jac[i, i-1] = 1/(2*h**2) * (-dQ(u[i-1])*u[i] + dQ(u[i-1])*u[i-1] + (Q(u[i])+Q(u[i-1])))
            jac[i, i]   = 1/(2*h**2) * (dQ(u[i])*(u[i+1]-2*u[i]+u[i-1]) - (Q(u[i+1])+2*Q(u[i])+Q(u[i-1])))
            jac[i, i+1] = 1/(2*h**2) * (-dQ(u[i+1])*u[i]+dQ(u[i+1])*u[i+1]+(Q(u[i])+Q(u[i+1])))

    return jac

ite = 10
tol = 1e-5
up  = np.zeros_like(x)

for i in range(ite):
    res = F(up)
    jac = J(up)
    u = up - np.linalg.inv(jac) @ res

    eps = np.max(np.abs(res))
    print(f"Ite {i+1}\terr {eps}")
    if(eps < tol):
        break

    up = u

plt.plot(x, u, label="$u_h(x)$")
plt.legend()
plt.title("Poisson non lineaire par DF")
plt.savefig("sol.jpg")
