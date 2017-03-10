"""
GENOP : a Python (v2) Genetic Optimizer

Adapted from the Scilab module 'ga_optim'

Auteur : Fabrice Zaoui (EDF R&D LNHE)

Copyright EDF 2016-2017

"""


__author__ = "Fabrice Zaoui"
__copyright__ = "Copyright EDF 2016"
__license__ = "GPL"
__maintainer__ = "Fabrice Zaoui"
__email__ = "fabrice.zaoui@edf.fr"
__status__ = "Implementation"
__version__ = "0.02"
__all__ = ['validate', 'genpop', 'costfunction', 'selection', \
    'crossover', 'mutation', 'showresults', 'Genop']


import validate
import genpop
import costfunction
import selection
import crossover
import mutation
import showresults
from genop import Genop
