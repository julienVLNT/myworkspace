#!/bin/python3
#! -*- coding: utf-8 -*-
import numpy as np

def lagrange(x_, x, i):

    prod = 1

    for j in range(len(x_)):
        if i != j:
            prod *= (x_[j]-x)/(x_[j]-x_[i])

    return prod
