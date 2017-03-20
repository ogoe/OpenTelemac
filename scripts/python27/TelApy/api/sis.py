#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    @package sis
    Python wrapper to the Fortran APIs of Sisyphe

    Author(s): Fabrice Zaoui, Yoann Audouin, Cedric Goeury, Renaud Barate

    Copyright EDF 2016
"""

from __future__ import print_function
import os
from TelApy.api.api_module import ApiModule


class Sisyphe(ApiModule):
    """The Sisyphe Python class for APIs"""
    _instanciated = False

    def __new__(cls, *args, **kwargs):
        if cls._instanciated:
            raise Exception("a Sisyphe instance already exists")
        instance = super(Sisyphe, cls).__new__(cls, *args, **kwargs)
        cls._instanciated = True
        return instance

    def __init__(self, casfile,
                 user_fortran=None,
                 dicofile=None,
                 lang=2, stdout=6,
                 comm=None,
                 recompile=True):
        """
        Constructor for Sisyphe
        @param casFile Name of the steering file
        @param user_fortran Name of the user Fortran (default=None)
        @param dicofile Path to the dictionary (default=None)
        @param lang Language for ouput (1: French, 2:English) (default=2)
        @param stdout Where to put the listing (default on terminal)
        @param comm MPI communicator (default=None)
        @param recompile If true recompiling the API (default=True)
        """
        if dicofile is None:
            hometel = os.getenv("HOMETEL")
            if hometel is not None:
                default_dicofile = os.path.join(os.getenv("HOMETEL"),
                                                "sources",
                                                "sisyphe",
                                                "sisyphe.dico")
            else:
                default_dicofile = 'sisyphe.dico'
            dicofile = default_dicofile
        super(Sisyphe, self).__init__("sis", casfile, user_fortran,
                                      dicofile, lang, stdout, comm, recompile)

    def __del__(self):
        """
        Destructor
        """
        Sisyphe._instanciated = False
