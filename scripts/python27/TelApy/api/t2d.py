#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Python wrapper to the Fortran APIs of Telemac 2D

    Author(s): Fabrice Zaoui, Yoann Audouin, Cedric Goeury, Renaud Barate

    Copyright EDF 2016
"""

from __future__ import print_function
import sys, os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
import tempfile
from ctypes import cdll
from TelApy.tools.polygon import is_in_polygon

hometel = os.getenv("HOMETEL")
if(hometel != None):
    default_dicofile = os.path.join(os.getenv("HOMETEL"),"sources","telemac2d","telemac2d.dico")
else:
    default_dicofile = 'telemac2d.dico'

class Telemac2d(object):
    """The Telemac 2D Python class for APIs"""
    _instanciated = False
    apit2d_inter = None
    apit2d = None

    def __new__(cls, *args, **kwargs):
        if cls._instanciated:
            raise Exception("a Telemac2d instance already exists")
        instance = super(Telemac2d, cls).__new__(cls, *args, **kwargs)
        cls._instanciated = True
        return instance

    def __init__(self, casfile, user_fortran = None,dicofile=default_dicofile, lang=2, stdout=6,comm = None):
        """
        Initialize a Telemac 2D instance
        :param casfile: Name of the steering file
        :param user_fortran: Name of the user fortran if there is one (Default None)
        :param dicofile: Name of the dictionary for Telemac2d (Default The one from the telemac environment)
        :param lang: Language output of Telemac2d (Default English)
        :param stdout: Standard output for Telemac2d (Default stdout i.e. terminal output)
        :param comm: MPI Communicator (Default None)
        :return: a new object from the class Telemac2d
        :param casfile:
        :return: a new object from the class PiT2d
        """
        self.user_fortran_lib = None
        self.user_fortran_lib_path = None
# User Fortran MUST be loaded before apit2d importation
        rank = 0
        if comm is not None:
            rank = comm.Get_rank()
        if rank == 0:
            if user_fortran is not None:
                root=os.getcwd()+os.sep
                ret=os.system("compileAPI.py --fcompiler=gfortran --user-fortran=%s"%(root+user_fortran))
            else:
                # Recompiling API in case it was compiled before with a user Fortran
                ret=os.system("compileAPI.py --fcompiler=gfortran")
            if ret != 0:
                raise Exception("Cannot recompile API")
        try:
            import _api
        except Exception:
            if sys.platform.startswith('linux'):
                 ext = 'so'
            elif sys.platform.startswith('win'):
                 ext = 'dll'
            else:
                 raise Exception('Error: unsupported Operating System!')
            raise Exception('Error: unable to load the dynamic library _api.'+ext+\
                        '\nYou can check the environment variable PYTHONPATH')
        self.apit2d = sys.modules['_api']
        self.apit2d_inter = self.apit2d.api_interface
        self.lang = lang
        self.stdout = stdout
        self.casfile = casfile
        self.dicofile = dicofile
        if comm!=None:
            self.comm = comm.py2f()
        else:
            self.comm = 0
        self._initstate = 0
        # run_set_config
        self.id, self.ierr = self.apit2d_inter.run_set_config_t2d(self.stdout,self.lang,self.comm)
        if self.ierr != 0:
            raise Exception('Error: unable to initialize Telemac2d.'
                        '\nTry to use get_error_message for more information')

    def set_case(self):
        """
           Read the steering file and run allocation
        """
        # run_read_case
        self.ierr = self.apit2d_inter.run_read_case_t2d(self.id, self.casfile, \
                                                self.dicofile)
        if self.ierr != 0:
            raise Exception('Error: unable to read_case for Telemac2d.'
                        '\nTry to use get_error_message for more information')
        # run_allocation
        self.ierr = self.apit2d_inter.run_allocation_t2d(self.id)
        if self.ierr != 0:
            raise Exception('Error: unable to run_allocation for Telemac2d.'
                        '\nTry to use get_error_message for more information')
        self._initstate = 1

        return self.ierr

    def init_state_default(self):
        """
        Initialize the state of the model Telemac 2D with the values of
        disharges and water levels as indicated by the steering file
        """
        if self._initstate == 0:
            raise Exception('Error: the object is not a Telemac 2D instance')
        else:
            self.ierr = self.apit2d_inter.run_init_t2d(self.id)
            if self.ierr == 0:
                self._initstate = 2
            else:
                raise Exception('Error: unable to set the initial conditions' \
                        '\nTry to use get_error_message for more information')
        return self.ierr

    def run_one_time_step(self):
        """
        Run one time step
        """
        if self._initstate != 2:
            raise Exception('Error: the initial conditions are not set'
                    '\nUse init_state_default first')
            self.ierr = -1
        else:
            self.ierr = self.apit2d_inter.run_timestep_t2d(self.id)
            if self.ierr:
                raise Exception('Error: the computation does not perform' \
                        '\nTry to use get_error_message for more information')
        return self.ierr

    def run_all_time_steps(self):
        """
        Run all the time steps
        :return: the number of computed time steps
        """
        ntimesteps = self.get_integer("MODEL.NTIMESTEPS")
        for i in xrange(ntimesteps):
            ierr = self.run_one_time_step()
            if self.ierr:
                raise Exception('Error: the computation does not perform' \
                        '\nTry to use get_error_message for more information')
        return ntimesteps

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

    def get_mesh(self):
        """
        Get the 2D mesh of triangular cells
        :return: XY coordinates and connectivity
        """
        self.nbnodes = self.get_integer('MODEL.NPOIN')
        self.x = np.zeros((self.nbnodes,))
        self.y = np.zeros((self.nbnodes,))
        for i in xrange(self.nbnodes):
            self.x[i] = self.get_double('MODEL.X', True, i+1)
            self.y[i] = self.get_double('MODEL.Y', True, i+1)
        self.nelem = self.get_integer('MODEL.NELEM')
        self.tri = np.zeros((self.nelem*3,), dtype=np.int)
        for i in xrange(self.nelem*3):
            self.tri[i] = self.get_integer('MODEL.IKLE', i+1)
        self.tri = self.tri.reshape(3, self.nelem).transpose() - 1
        return self.x, self.y, self.tri

    def get_node(self, xval, yval):
        """
        Get the nearest node number for the coordinates (xval, yval)
        :param xval: X coordinate
        :param yval: Y coordinate
        :return: integer value from 0 to (nbnode-1)
        """
        pt = np.array([[xval, yval]])
        x, y, tri = self.get_mesh()
        XY = np.array([x, y]).transpose()
        return np.argmin(cdist(XY, pt))

    def get_elem(self, xval, yval):
        """
        Get the triangle where the point (xval, yval) is
        :param xval: X coordinate
        :param yval: Y coordinate
        :return: integer value from 0 to (nbtriangle-1) (-1 if no triangle found)
        """
        x, y, tri = self.get_mesh()
        XY = np.array([x, y]).transpose()
        pt = np.array([xval, yval])
        dimtri = tri.shape
        triangle = -1
        for i in range(dimtri[0]):
            a = XY[tri[i, 0]]
            b = XY[tri[i, 1]]
            c = XY[tri[i, 2]]
            v0 = c - a
            v1 = b - a
            v2 = pt - a
            dot00 = np.dot(v0, v0)
            dot01 = np.dot(v0, v1)
            dot02 = np.dot(v0, v2)
            dot11 = np.dot(v1, v1)
            dot12 = np.dot(v1, v2)
            invden = 1. / (dot00 * dot11 - dot01 * dot01)
            u = (dot11 * dot02 - dot01 * dot12) * invden
            v = (dot00 * dot12 - dot01 * dot02) * invden
            if ((u >= 0) & (v >= 0) & (u + v < 1)):
                triangle = i
                break
        return triangle


    def show_mesh(self, show=True, visu2d=True):
        """
        Show the 2D mesh with topography
        :param show: Display the graph (Default True)
        :param visu2d: 2d display (Default True)
        :return: the figure object
        """
        if hasattr(self, 'x') == False:
            x, y, tri = self.get_mesh()
        if hasattr(self, 'bottom') == False:
            self.bottom = np.zeros((self.nbnodes,))
            for i in xrange(self.nbnodes):
                self.bottom[i] = self.get_double('MODEL.BOTTOMELEVATION', True, i+1)
        fig = plt.figure()
        if visu2d:
            plt.tripcolor(self.x, self.y, self.tri, self.bottom, shading='flat', edgecolor='w', cmap=plt.cm.terrain)
            plt.colorbar()
        else:
            ax = fig.gca(projection='3d')
            ax.plot_trisurf(self.x, self.y, self.tri, self.bottom, cmap=plt.cm.terrain, linewidth=0.1)
        plt.title('2D mesh (%d triangles, %d nodes) with the bottom elevation (m)' % (self.nelem, self.nbnodes))
        plt.xlabel('X-coordinate (m)')
        plt.ylabel('Y-coordinate (m)')
        if show:
            plt.show()
        return fig

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

    def list_variables(self):
        """
        List the names and the meaning of available variables and parameters
        :return: two lists of strings (name and meaning)
        """
        nbvar = self.apit2d.api_handle_var_t2d.nb_var_t2d

        # TODO: Solve that shit charcater Fortran to character Python
        varnames = []
        varinfo = []
        ierr = self.apit2d_inter.get_var_list_t2d(varnames,varinfo)
        vnames = []
        vinfo = []
        for i in range(nbvar):
            vnames.append(''.join(map(str, varnames[i])).strip())
            vinfo.append(''.join(map(str, varinfo[i])).strip())
        return vnames, vinfo

    def get_integer(self, varname, *args):
        """
        Get the integer value of a variable of Telemac 2D
        :param varname: Name of the variable
        :param args: List of indexes By Default 0,0,0
        :return: integer variable
        """
        args = list(args) + [0] * (3 - len(args))
        value, self.ierr = self.apit2d_inter.get_integer_t2d(self.id, varname, *args)
        if self.ierr:
                print('Error: get_integer does not perform' \
                        '\nTry to use get_error_message for more information')
        return value

    def get_double(self, varname, global_num=True, *args):
        """
        Get the real value of a variable of Telemac 2D
        :param varname: Name of the variable
        :param global_num: Are the index in Global Numbering (Default True)
        :param args: List of indexes By Default 0,0,0
        :return: real variable
        """
        args = list(args) + [0] * (3 - len(args))
        value, self.ierr = self.apit2d_inter.get_double_t2d(self.id, varname, global_num, *args)
        if self.ierr:
                print('Error: get_double does not perform' \
                        '\nTry to use get_error_message for more information')
        return value


    def set_double(self, varname, value, global_num = True, *args):
        """
        Set the real value of a variable of Telemac 2D
        :param varname: Name of the variable
        :param value: The value to set
        :param global_num: Are the index in Global Numbering (Default True)
        :param args: List of indexes By Default 0,0,0
        :return: error code
        """
        args = list(args) + [0] * (3 - len(args))
        self.ierr = self.apit2d_inter.set_double_t2d(self.id, varname, value, global_num, *args)
        if self.ierr:
            raise Exception("error in setting double :"+varname+\
                            "\n"+self.get_error_message())
            print('Error: set_double does not perform' \
                        '\nTry to use get_error_message for more information')
        return self.ierr

    def set_integer(self, varname, value, *args):
        """
        Set the integer value of a variable of Telemac 2D
        :param varname: Name of the variable
        :param value: The value to set
        :param args: List of indexes By Default 0,0,0
        :return: error code
        """
        args = list(args) + [0] * (3 - len(args))
        self.ierr = self.apit2d_inter.set_integer_t2d(self.id, varname, value, *args)
        if self.ierr:
            raise Exception("error in setting integer "+varname+"\n"+\
                            self.get_error_message())
            print('Error: set_integer does not perform' \
                        '\nTry to use get_error_message for more information')
        return self.ierr

    def get_error_message(self):
        """
        Get the error message from the Fortran sources of Telemac 2D
        :return: character string of the error message
        """
        return self.apit2d.api_handle_error.err_mess.tostring().strip()

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

    def finalize(self):
        """
        Delete the Telemac 2D instance
        :return: error code
        """
        ierr = self.apit2d_inter.run_finalize_t2d(self.id)
        if ierr:
            print('Error: no deletion' \
                    '\nTry to use get_error_message for more information')
        return ierr

    def __del__(self):
        """
        Destructor
        """
        Telemac2d._instanciated = False
