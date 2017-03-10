# -*- coding: utf-8 -*-
"""
Test all the given arguments

Author(s) : Fabrice Zaoui

Copyright EDF 2016-2017

:param 'bounds': array of domain constraints (lower and upper bounds)
:param 'n': the number of variables
:param 'f': the name of the function to optimize
:return: the random population
"""

import numpy as np


def validate(func, nvar, bounds, verbose):
    """
    Test the types of arguments
    """
    if 'function' not in str(type(func)):
        if verbose:
            print '--> genop Error!'
            print '\tFirst argument must be a function'
        return -1
    if 'int' not in str(type(nvar)):
        if verbose:
            print '--> genop Error!'
            print '\tSecond argument must be a integer value'
        return -1
    if 'numpy' not in str(type(bounds)):
        if verbose:
            print '--> genop Error!'
            print '\tThird argument must be a numpy array'
        return -1
    # Verify argument values
    if nvar <= 0:
        if verbose:
            print '--> genop Error!'
            print '\tThe number of variables must greater than 0'
        return -1
    (nrow, ncol) = bounds.shape
    if nrow != nvar:
        if verbose:
            print '--> genop Error!'
            print '\tThird argument: the number of bounds must equal to the \
                number of variables'
            print '\tThe Number of rows must equal to the number of variables:\
                %d' % nvar
        return -1
    if ncol != 2:
        if verbose:
            print '--> genop Error!'
            print '\tThird argument: the number of columns is not equal to 2'
            print '\tThe fisrt column is for lower bound values and the second\
                one for the upper bounds'
        return -1
    try:
        res = func((bounds[:, 0] + bounds[:, 1]) / 2.)
    except:
        if verbose:
            print '--> genop Error!'
            print '\tUnable to evaluate the function'
        return -1
    try:
        res = res + 2.
    except:
        if verbose:
            print '--> genop Error!'
            print '\tThe return value of the cost function must be numeric'
        return -1
    if 'False' in str(bounds[:, 1] - bounds[:, 0] > 0.):
        if verbose:
            print '--> genop Error!'
            print '\tA lower bound must be smaller than the corresponding\
                upper bound'
        return -1
    return 0
