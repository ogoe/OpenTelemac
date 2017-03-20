#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    @package t2d
    Python wrapper to the Fortran APIs of Telemac 2D

    Author(s): Fabrice Zaoui, Yoann Audouin, Cedric Goeury, Renaud Barate

    Copyright EDF 2016
"""

from __future__ import print_function
import os
import numpy as np
import matplotlib.pyplot as plt
from TelApy.tools.polygon import is_in_polygon
from TelApy.api.api_module import ApiModule


class Telemac2d(ApiModule):
    """The Telemac 2D Python class for APIs"""
    _instanciated = False

    def __new__(cls, *args, **kwargs):
        if cls._instanciated:
            raise Exception("a Telemac2d instance already exists")
        instance = super(Telemac2d, cls).__new__(cls, *args, **kwargs)
        cls._instanciated = True
        return instance

    def __init__(self, casfile,
                 user_fortran=None,
                 dicofile=None,
                 lang=2, stdout=6,
                 comm=None,
                 recompile=True):
        """
        Constructor for Telemac2d

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
                                                "telemac2d",
                                                "telemac2d.dico")
            else:
                default_dicofile = 'telemac2d.dico'
            dicofile = default_dicofile
        super(Telemac2d, self).__init__("t2d", casfile, user_fortran,
                                        dicofile, lang, stdout, comm, recompile)
        self.hsave = None
        self.usave = None
        self.vsave = None
        self.depth = None
        self.u_vel = None
        self.v_vel = None

    def save_state(self):
        """
        Save the hydraulic state
        """
        self.nbnodes = self.get('MODEL.NPOIN')
        self.hsave = np.zeros((self.nbnodes, ))
        self.usave = np.zeros((self.nbnodes, ))
        self.vsave = np.zeros((self.nbnodes, ))
        for i in xrange(self.nbnodes):
            self.hsave[i] = self.get('MODEL.WATERDEPTH', i=i+1)
            self.usave[i] = self.get('MODEL.VELOCITYU', i=i+1)
            self.vsave[i] = self.get('MODEL.VELOCITYV', i=i+1)
        return

    def restore_state(self):
        """
        Restore the hydraulic state
        """
        if self.hsave is None:
            self.ierr = -1
            raise Exception('Error: unable to restore the hydraulic state.' \
                        '\nNo saved state found')
        for i in xrange(self.nbnodes):
            self.set('MODEL.WATERDEPTH', self.hsave[i], i=i+1)
            self.set('MODEL.VELOCITYU', self.usave[i], i=i+1)
            self.set('MODEL.VELOCITYV', self.vsave[i], i=i+1)

    def get_state(self):
        """
        Get the hydraulic state

        @retuns the hydraulic state: depth (m) .. u_vel (m/s) .. v_vel (m/s)
        """
        nbnodes = self.get('MODEL.NPOIN')
        self.depth = np.zeros((nbnodes, ))
        self.u_vel = np.zeros((nbnodes, ))
        self.v_vel = np.zeros((nbnodes, ))
        for i in xrange(nbnodes):
            self.depth[i] = self.get('MODEL.WATERDEPTH', i=i+1)
            self.u_vel[i] = self.get('MODEL.VELOCITYU', i=i+1)
            self.v_vel[i] = self.get('MODEL.VELOCITYV', i=i+1)
        return self.depth, self.u_vel, self.v_vel

    def set_state(self, hval, uval, vval):
        """
        Set the hydraulic state: hval (m) .. uval (m/s) .. vval (m/s)

        @param hval Water depth value
        @param uval Velocity U value
        @param vval Velocity V value
        """
        nbnodes = self.get('MODEL.NPOIN')
        for i in xrange(nbnodes):
            self.set('MODEL.WATERDEPTH', hval[i], i=i+1)
            self.set('MODEL.VELOCITYU', uval[i], i=i+1)
            self.set('MODEL.VELOCITYV', vval[i], i=i+1)
        return

    def show_state(self, show=True):
        """
        Show the hydraulic state with matplotlib

        @param show Display the graph (Default True)

        @retuns the figure object
        """
        if self.coordx is not None:
            _, _, _ = self.get_mesh()
        values = self.get_state()
        fig = plt.figure()
        plt.subplot(1, 2, 1) # water levels
        plt.tripcolor(self.coordx, self.coordy, self.tri, values[0],
                      shading='gouraud', cmap=plt.cm.winter)
        plt.colorbar()
        plt.title('Water levels (m)')
        plt.xlabel('X-coordinate (m)')
        plt.ylabel('Y-coordinate (m)')
        plt.subplot(1, 2, 2) # velocity
        uvnorm = np.sqrt(values[1]**2 + values[2]**2)
        plt.quiver(self.coordx, self.coordy, values[1], values[2], uvnorm,
                   units='xy', angles='uv', scale=0.01)
        plt.colorbar()
        plt.title('Velocity field (m/s)')
        plt.xlabel('X-coordinate (m)')
        plt.ylabel('Y-coordinate (m)')
        if show:
            plt.show()
        return fig

    def set_bathy(self, bathy, polygon=None):
        """
        Set a new bathy in the geometry file

        @param bathy Array containing the new bathymetry for each point
        @param polygon Polygon on which to modify the bathymetry
        """
        if polygon is None:
            for i in xrange(len(bathy)):
                self.set("MODEL.BOTTOMELEVATION", bathy[i], i=i+1)
        else:
            for i in xrange(len(bathy)):
                coordx = self.get("MODEL.X", i=i)
                coordy = self.get("MODEL.Y", i=i)
                if is_in_polygon(coordx, coordy, polygon):
                    self.set("MODEL.BOTTOMELEVATION",
                             bathy[i], i=i+1)

        return

    def __del__(self):
        """
        Destructor
        """
        Telemac2d._instanciated = False
