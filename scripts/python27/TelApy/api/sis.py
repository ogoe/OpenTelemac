#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Python wrapper to the Fortran APIs of Sisyphe

    Author(s): Fabrice Zaoui, Yoann Audouin, Cedric Goeury, Renaud Barate

    Copyright EDF 2016
"""

from __future__ import print_function
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from TelApy.tools.polygon import is_in_polygon
from TelApy.api.api_module import Api_module


class Sisyphe(Api_module):
    """The Telemac 2D Python class for APIs"""
    _instanciated = False

    def __new__(cls, *args, **kwargs):
        if cls._instanciated:
            raise Exception("a Telemac2d instance already exists")
        instance = super(Sisyphe, cls).__new__(cls, *args, **kwargs)
        cls._instanciated = True
        return instance

    def __init__(self, casfile, 
                 user_fortran = None,
                 dicofile = None, 
                 lang = 2, stdout = 6,
                 comm =  None):
        if dicofile is None:
            hometel = os.getenv("HOMETEL")
            if(hometel != None):
                default_dicofile = os.path.join(os.getenv("HOMETEL"),
                                                "sources",
                                                "sisyphe",
                                                "sisyphe.dico")
            else:
                default_dicofile = 'sisyphe.dico'
            dicofile = default_dicofile
        super(Sisyphe,self).__init__("sis",casfile,user_fortran,dicofile,lang,stdout,comm)

    def __del__(self):
        """
        Destructor
        """
        Sisyphe._instanciated = False
