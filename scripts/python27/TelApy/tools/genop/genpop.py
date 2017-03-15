# -*- coding: utf-8 -*-
"""
Generate initial individuals from a uniform distribution
    in their domain constraints

Author(s) : Fabrice Zaoui

Copyright EDF 2016

:param 'bounds': array of domain constraints (lower and upper bounds)
:param 'nvar': the number of variables
:param 'npop': the size of the population
:return: the random population
"""

import numpy as np


def genpop(bounds, nvar, npop):
    lower = bounds[:, 0]
    xrange = bounds[:, 1] - bounds[:, 0]
    pop = np.array([]).reshape(0, nvar)
    for i in range(0, npop):
        randval = np.random.uniform(0, 1, (nvar, 1))
        randval = randval.reshape(nvar, )
        individual = lower + xrange*randval
        pop = np.vstack([pop, individual])
    return pop
