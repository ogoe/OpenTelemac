# -*- coding: utf-8 -*-
"""
Evaluate the cost functions

Author(s) : Fabrice Zaoui

Copyright EDF 2016

:param 'pop': the genetic population
:param 'npop': the size of the population
:param 'nvar': the number of variables
:param 'fname': name of the cost function
:param 'ind1': first individual of couples
:param 'ind2': second individual of couples
:param 'f1': cost function for the first individual
:param 'f2': cost function for the second individual
:param 'todo1': consider or not the re-computation of the cost function for 'ind1'
:param 'todo2': consider or not the re-computation of the cost function for 'ind2'
:return: the random population
"""

import numpy as np
import multiprocessing as mp

def cost(pop, npop, nvar, fname, nproc):
    # serial computations
    if nproc == 1:
        ncalls = 0
        feval = np.array([]).reshape(0, 1)
        for i in range(0, npop):
            res = fname(pop[i])
            feval = np.vstack([feval, res])
            ncalls = ncalls + 1
        return feval, ncalls
    # parallel computations
    else:
        pool = mp.Pool(processes=nproc)
        feval = pool.map(fname, pop)
        pool.close()
        pool.join()
        ncalls = npop
        return np.asarray(feval).reshape(npop, 1), ncalls

def updatecost(ind1, ind2, f1, f2, todo1, todo2, fname, nproc):
    # serial computations
    if nproc == 1:
        ncalls = 0
        (nbcouples, nvar) = ind1.shape
        for i in range(0, nbcouples):
            if todo1[i]:
                f1[i] = fname(ind1[i])
                ncalls = ncalls + 1
            if todo2[i]:
                f2[i] = fname(ind2[i])
                ncalls = ncalls + 1
    # parallel computations
    else:
        ncalls = 0
        (nbcouples, nvar) = ind1.shape
        ind1tocompute = np.array([]).reshape(0, nvar)
        ind2tocompute = np.array([]).reshape(0, nvar)
        for i in range(0, nbcouples):
            if todo1[i]:
                ind1tocompute = np.vstack([ind1tocompute, ind1[i]])
                ncalls = ncalls + 1
            if todo2[i]:
                ind2tocompute = np.vstack([ind2tocompute, ind2[i]])
                ncalls = ncalls + 1
        pool = mp.Pool(processes=nproc)
        g1 = pool.map(fname, ind1tocompute)
        g2 = pool.map(fname, ind2tocompute)
        pool.close()
        pool.join()
        g1 = np.asarray(g1)
        g2 = np.asarray(g2)
        k1 = 0
        k2 = 0
        for i in range(0, nbcouples):
            if todo1[i]:
                f1[i] = g1[k1]
                k1 = k1 + 1    
            if todo2[i]:
                f2[i] = g2[k2]
                k2 = k2 + 1
    return f1, f2, ncalls

