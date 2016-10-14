#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Python wrapper to the Fortran APIs of Telemac 2D

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


class Telemac2d(Api_module):
    """The Telemac 2D Python class for APIs"""
    _instanciated = False

    def __new__(cls, *args, **kwargs):
        if cls._instanciated:
            raise Exception("a Telemac2d instance already exists")
        instance = super(Telemac2d, cls).__new__(cls, *args, **kwargs)
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
                                                "telemac2d",
                                                "telemac2d.dico")
            else:
                default_dicofile = 'telemac2d.dico'
            dicofile = default_dicofile
        super(Telemac2d,self).__init__("t2d",casfile,user_fortran,dicofile,lang,stdout,comm)

    def save_state(self):
        """
        Save the hydraulic state
        """
        self.nbnodes = self.get_integer('MODEL.NPOIN')
        self.hsave = np.zeros((self.nbnodes,))
        self.usave = np.zeros((self.nbnodes,))
        self.vsave = np.zeros((self.nbnodes,))
        for i in xrange(self.nbnodes):
            self.hsave[i] = self.get_double('MODEL.WATERDEPTH', True, i+1)
            self.usave[i] = self.get_double('MODEL.VELOCITYU', True, i+1)
            self.vsave[i] = self.get_double('MODEL.VELOCITYV', True, i+1)
        return

    def restore_state(self):
        """
        Restore the hydraulic state
        :return: error code
        """
        if hasattr(self, 'hsave') == False:
            self.ierr = -1
            raise Exception('Error: unable to restore the hydraulic state.' \
                        '\nNo saved state found')
        for i in xrange(self.nbnodes):
            self.set_double('MODEL.WATERDEPTH', self.hsave[i], True, i+1)
            self.set_double('MODEL.VELOCITYU' , self.usave[i], True, i+1)
            self.set_double('MODEL.VELOCITYV' , self.vsave[i], True, i+1)

    def get_state(self):
        """
        Get the hydraulic state
        :return: the hydraulic state: h (m) .. u (m/s) .. v (m/s)
        """
        nbnodes = self.get_integer('MODEL.NPOIN')
        self.h = np.zeros((nbnodes,))
        self.u = np.zeros((nbnodes,))
        self.v = np.zeros((nbnodes,))
        for i in xrange(nbnodes):
            self.h[i] = self.get_double('MODEL.WATERDEPTH', True, i+1)
            self.u[i] = self.get_double('MODEL.VELOCITYU', True, i+1)
            self.v[i] = self.get_double('MODEL.VELOCITYV', True, i+1)
        return self.h, self.u, self.v

    def set_state(self, hval, uval, vval):
        """
        Set the hydraulic state: hval (m) .. uval (m/s) .. vval (m/s)
        :param hval: Water depth value
        :param uval: Velocity U value
        :param vval: Velocity V value
        """
        nbnodes = self.get_integer('MODEL.NPOIN')
        for i in xrange(nbnodes):
            self.set_double('MODEL.WATERDEPTH', hval[i], True, i+1)
            self.set_double('MODEL.VELOCITYU' , uval[i], True, i+1)
            self.set_double('MODEL.VELOCITYV' , vval[i], True, i+1)
        return

    def show_state(self, show=True):
        """
        Show the hydraulic state with matplotlib
        :param show: Display the graph (Default True)
        :return: the figure object
        """
        if hasattr(self, 'x') == False:
            x, y, tri = self.get_mesh()
        values = self.get_state()
        fig = plt.figure()
        plt.subplot(1,2,1) # water levels
        plt.tripcolor(self.x, self.y, self.tri, values[0], shading='gouraud', cmap=plt.cm.winter)
        plt.colorbar()
        plt.title('Water levels (m)')
        plt.xlabel('X-coordinate (m)')
        plt.ylabel('Y-coordinate (m)')
        plt.subplot(1,2,2) # velocity
        uvnorm = np.sqrt(values[1]**2 + values[2]**2)
        plt.quiver(self.x, self.y, values[1], values[2], uvnorm, units='xy', angles='uv', scale=0.01)
        plt.colorbar()
        plt.title('Velocity field (m/s)')
        plt.xlabel('X-coordinate (m)')
        plt.ylabel('Y-coordinate (m)')
        if show:
            plt.show()
        return fig

    def set_bathy(self,bathy,polygon=None):
        """
        Set a new bathy in the geometry file
        :param bathy: Array containing the new bathymetry for each point
        :param polygon: Polygon on which to modify the bathymetry
        """
        if polygon is None:
            for i in xrange(len(bathy)):
                self.set_double("MODEL.BOTTOMELEVATION",bathy[i],True,i+1)
        else:
            for i in xrange(len(bathy)):
                x = self.get_double("MODEL.X",True,i)
                y = self.get_double("MODEL.Y",True,i)
                if is_in_polygon(x,y,polygon):
                    self.set_double("MODEL.BOTTOMELEVATION",bathy[i],True,i+1)

        return

    def __del__(self):
        """
        Destructor
        """
        Telemac2d._instanciated = False
