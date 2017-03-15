# -*- coding: utf-8 -*-
"""
Definition of the cost function for 'genop' optimizer

Author(s) : Fabrice Zaoui

Copyright EDF 2016

:param 'x': array of optimization variables
:return: value of the cost function
"""
import numpy as np

# user function
def simul():
    return

# test function
def rosenbrock(x):
    val = (1.-x[0])**2+100.*(x[1]-x[0]**2)**2
    return val

# test function
def maccormick(x):
    val = np.sin(x[0]+x[1])+(x[0]-x[1])**2 -1.5*x[0]+2.5*x[1]+1.
    return val

# test function
def easom(x):
    val = -np.cos(x[0])*np.cos(x[1])*np.exp(-((x[0]-np.pi)**2+(x[1]-np.pi)**2))
    return val

# test function
def eggholder(x):
    val = -(x[1]+47.)*np.sin(np.sqrt(np.abs(0.5*x[0]+x[1]+47.)))-x[0]*np.sin(np.sqrt(np.abs(x[0]-x[1]-47.)))
    return val

# test function
def linear(x):
    val = x[0] + x[1] - x[2]
    return val
    
# test function    
def abslinear(x):
    val = np.abs(x[0]-2.)
    return val

