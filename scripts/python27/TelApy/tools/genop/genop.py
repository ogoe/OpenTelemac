#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    A genetic algorithm optimizer (minimizer) in Python

    Author(s): Fabrice Zaoui

    Copyright EDF 2016

    Comments : adapted from the original algorithm of Scilab module 'optim_ga'
"""


from validate import validate
from costfunction import cost, updatecost
from genpop import genpop
from selection import selection, elitist
from crossover import crossover
from mutation import mutation
from showresults import printinfo
import numpy as np


class Genop(object):
    def __init__(self, popsize=100, pbcross=0.7, pbmut=0.1):
        """
        Initialize some algorithmic parameters to default values
        :return: a new object from Genop
        """
        self.popsize = popsize  # size of the population of individuals
        self.pbmut = pbmut  # mutation probability
        self.pbcross = pbcross  # crossover probability
        self.nbcouples = np.int_(np.ceil(1.2 * self.popsize)) # number of couples for the crossover and mutation
        self.pressure = 0.05  # efficiency of the worst individual
        self.verbose = True  # print information
        self.function = ""  # name of the cost function
        self.nvar = 0  # number of optimization variables
        self.nsimul = 0 # number of calls to the simulation function
        self.bounds = []  # lower and upper bounds of optimization variables
        self._pop = []  # population of individuals
        self._fvalpop = []  # cost function values of the population
        self.__ready = False  # intialization is done
        self.foptiter = np.array([]).reshape(0, 1)  # save the best cost functions
        self.epsconv = np.finfo(float).eps # convergence value to stop the iterations (default: machine precision)

    def initialize(self, f, n, bounds):
        """
        Description of the minimization problem
        :param 'f': the name of the python function where the cost function\
            is implemented (type: str)
        :param 'n': the number of optimization variables (type: int)
        :param 'bounds': a numpy array for the lower and upper bounds of\
            optimization variables (type: int)
        :return: error code = 0 if successful (type: int)
        """
        # Test the arguments
        error = validate(f, n, bounds, self.verbose)
        if error:
            if self.verbose:
                print('--> genop Error!')
                print('\tIncorrect arguments.')
            return -1
        # Save all the information
        self.function = f
        self.nvar = n
        self.bounds = bounds
        # Initialize the population and the cost function
        self._pop = np.zeros((self.popsize, self.nvar))
        self._fvalpop = np.zeros((self.popsize, 1))
        # Job done
        self.__ready = True
        return 0

    def optimize(self, nbgen=10, nproc=1):
        """
        Do the genetic algorithm with 'niter' generations
        :param 'nbgen': the maximal number of gnerations (i.e. iterations)
        :param 'nproc': the number of processes (parallel simulations)
        :return: a tuple of two elements:
                    - the optimal individual of the last population (list)
                    - the associated optimal cost function (float)
        """
        if self.__ready == False:
            if self.verbose:
                print('--> genop Error!')
                print('\tThe problem is not correctly initialized.')
                print('\tRun the "initialize" method before optimizing.')
            return -1
        # initialize arrays for the solutions
        self.popiter = np.array([]).reshape(0, self.nvar)
        self.foptiter = np.array([]).reshape(0, 1)
        # nsimul must be equal to 0
        self.nsimul = 0
        # Initialize a population from a uniform distribution
        self._pop = genpop(self.bounds, self.nvar, self.popsize)
        # Compute the initial score for the population
        self._fvalpop, ncalls = cost(self._pop, self.popsize, self.nvar, self.function, nproc)
        self.nsimul = self.nsimul + ncalls
        # Efficiency
        fmin = np.ndarray.min(self._fvalpop)
        fmax = np.ndarray.max(self._fvalpop)
        efficiency = (1. - self.pressure) * (fmax - self._fvalpop) / \
                     np.ndarray.max(np.array([fmax - fmin, np.spacing(1)])) + self.pressure
        # Evolution
        for i in range(0, nbgen):
            # Selection
            (ind1, f1, ind2, f2) = selection(self._pop, self._fvalpop, \
                self.nbcouples, efficiency)
            # Crossover
            (ind1, ind2, tocompute1, tocompute2) =  \
                crossover(ind1, ind2, self.bounds, self.pbcross)
            # Mutation
            (ind1, ind2, tocompute1, tocompute2) =  \
                mutation(ind1, ind2, tocompute1, tocompute2, self.bounds, self.pbmut)
            # Update cost function values
            f1, f2, ncalls = updatecost(ind1, ind2, f1, f2, tocompute1, tocompute2, self.function, nproc)
            self.nsimul = self.nsimul + ncalls
            # Recombination
            (self._pop, self._fvalpop, efficiency) = elitist(self._pop, ind1, ind2, self._fvalpop, f1, f2, self.pressure)
            # Show info
            if self.verbose:
                printinfo(i, nbgen, self._fvalpop[0], self._pop[0, :], self.nsimul)
            # Save the best cost function and solution
            self.foptiter = np.vstack([self.foptiter, self._fvalpop[0]])
            self.popiter = np.vstack([self.popiter, self._pop[0, :]])
            # Check the stopping criteria over the last ten iterations
            if i > 9:
                cond = 0.
                for j in range(i-9, i+1):
                    cond = cond + (self.foptiter[j-1] - self.foptiter[j]) / \
                    np.max([self.foptiter[j-1], self.foptiter[j], 1.])
                if (cond <= self.epsconv):
                    if self.verbose:
                        printinfo(i, -1, self._fvalpop[0], self._pop[0, :], self.nsimul)
                    break
        return self.foptiter, self.popiter

