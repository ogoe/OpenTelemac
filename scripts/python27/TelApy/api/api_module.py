#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Python wrapper to the Fortran APIs of Telemac-Mascaret

    Author(s): Fabrice Zaoui, Yoann Audouin, Cedric Goeury, Renaud Barate

    Copyright EDF 2016
"""
import sys, os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from scipy.spatial.distance import cdist

class Api_module(object):
    """The Telemac 2D Python class for APIs"""
    name = ''
    api_inter = None
    modules = None

    def __init__(self, name, casfile,
                 user_fortran,
                 dicofile,
                 lang, stdout,
                 comm,
                 code=None):

        self.name = name

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
        except Exception as e:
            if sys.platform.startswith('linux'):
                 ext = 'so'
            elif sys.platform.startswith('win'):
                 ext = 'dll'
            else:
                 raise Exception('Error: unsupported Operating System!')
            raise Exception('Error: unable to load the dynamic library _api.'+ext+\
                        '\nYou can check the environment variable PYTHONPATH'+'\n'+str(e))
        self.api = sys.modules['_api']
        self.api_inter = self.api.api_interface

        # Making links to all the functions
        self.run_set_config = getattr(self.api_inter,"run_set_config_"+self.name)
        self.run_read_case = getattr(self.api_inter,"run_read_case_"+self.name)
        self.run_allocation = getattr(self.api_inter,"run_allocation_"+self.name)
        self.run_init = getattr(self.api_inter,"run_init_"+self.name)
        self.run_timestep = getattr(self.api_inter,"run_timestep_"+self.name)
        self.run_finalize = getattr(self.api_inter,"run_finalize_"+self.name)
        self.api_get_integer = getattr(self.api_inter,"get_integer_"+self.name)
        self.api_set_integer = getattr(self.api_inter,"set_integer_"+self.name)
        self.api_get_double = getattr(self.api_inter,"get_double_"+self.name)
        self.api_set_double = getattr(self.api_inter,"set_double_"+self.name)
        self.api_get_string = getattr(self.api_inter,"get_string_"+self.name)
        self.api_set_string = getattr(self.api_inter,"set_string_"+self.name)
        self.api_get_boolean = getattr(self.api_inter,"get_boolean_"+self.name)
        self.api_set_boolean = getattr(self.api_inter,"set_boolean_"+self.name)
        self.get_var_type = getattr(self.api_inter,"get_var_type_"+self.name)
        self.get_var_size = getattr(self.api_inter,"get_var_size_"+self.name)
        self.mod_handle_var = getattr(self.api,"api_handle_var_"+self.name)

        self.lang = lang
        self.stdout = stdout
        self.casfile = casfile
        self.dicofile = dicofile
        if comm!=None:
            self.comm = comm.py2f()
        else:
            self.comm = 0
        self.code = code
        self._initstate = 0
        # run_set_config
        self.id, self.ierr = self.run_set_config(self.stdout,self.lang,self.comm)
        if self.ierr != 0:
            raise Exception('Error: unable to initialize Telemac2d.'
                        '\nTry to use get_error_message for more information')

    def set_case(self):
        """
           Read the steering file and run allocation
        """
        # run_read_case
        if self.name == "sis":
            self.ierr = self.run_read_case(self.id, self.code, self.casfile, \
                                                    self.dicofile)
        else:
            self.ierr = self.run_read_case(self.id, self.casfile, \
                                                    self.dicofile)
        if self.ierr != 0:
            raise Exception('Error: unable to read_case for Telemac2d.'
                        '\nTry to use get_error_message for more information')
        # run_allocation
        self.ierr = self.run_allocation(self.id)
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
            self.ierr = self.run_init(self.id)
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
            self.ierr = self.run_timestep(self.id)
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

    def list_variables(self):
        """
        List the names and the meaning of available variables and parameters
        :return: two lists of strings (name and meaning)
        """
        nbVar = getattr(self.mod_handle_var,"nb_var_"+self.name)
        varLen = getattr(self.mod_handle_var,self.name+"_var_len")
        infoLen = getattr(self.mod_handle_var,self.name+"_info_len")

        # TODO: Solve that shit charcater Fortran to character Python
        vnames = []
        vinfo = []
        varnames = []
        varinfo = []
        # Reordering string array for variable names
        tmp = getattr(self.mod_handle_var,"vname_"+self.name)
        for j in range(varLen):
             for i in range(nbVar):
                 varnames.append(tmp[i][j])
        # Reordering string array for variable info
        tmp = getattr(self.mod_handle_var,"vinfo_"+self.name)
        for j in range(infoLen):
             for i in range(nbVar):
                 varinfo.append(tmp[i][j])
        # Extracting name and info into a list
        for i in range(nbVar):
            vnames.append(''.join(varnames[(i-1)*varLen:i*varLen]).strip())
            vinfo.append(''.join(varinfo[(i-1)*infoLen:i*infoLen]).strip())
        return vnames, vinfo


    def get(self, varname, i=0, j=0, k=0, global_num=True):
        """
        Get the value of a variable of Telemac 2D
        :param varname: Name of the variable
        :param i: index on first dimension
        :param j: index on second dimension
        :param k: index on third dimension
        :param global_num: Are the index on local/global numbering
        :return: variable value
        """
        value = None
        vartype,readonly,ndim,_,_,_,self.ierr = self.get_var_type(varname)
        dim1,dim2,dim3,ierr = self.get_var_size(self.id,varname)

        if (ndim >= 1):
           if(not 0<i<=dim1):
               print "i=",i," is not within [",1,",",dim1,"]"
               return value
        if (ndim >= 2):
           if(not 0<j<=dim2):
               print "j=",j," is not within [",1,",",dim2,"]"
               return value
        if (ndim == 3):
           if(not 0<k<=dim3):
               print "k=",k," is not within [",1,",",dim3,"]"
               return value
        if "DOUBLE" in vartype:
            value, self.ierr = self.api_get_double(self.id,varname,global_num,i,j,k)
        elif "INTEGER"in vartype:
            value, self.ierr = self.api_get_integer(self.id,varname,i,j,k)
        elif "STRING" in vartype:
            tmp_value, self.ierr = self.api_get_string(self.id,varname,dim1)
            value = tmp_value.tostring().strip()
        elif "BOOLEAN" in vartype:
            value, self.ierr = self.api_get_boolean(self.id,varname,i,j,k)
        else:
            print "Unknown data type ",vartype
        return value

    def set(self, varname, value, i=0, j=0, k=0, global_num=True):
        """
        Get the value of a variable of Telemac 2D
        :param varname: Name of the variable
        :param i: index on first dimension
        :param j: index on second dimension
        :param k: index on third dimension
        :param global_num: Are the index on local/global numbering
        :return: variable value
        """
        value = None
        vartype,readonly,ndim,_,_,_,self.ierr = self.get_var_type(varname)
        dim1,dim2,dim3,ierr = self.get_var_size(self.id,varname)

        if ndim >= 1:
           if(not 0<i<=dim1):
               print "i=",i," is not within [",1,",",dim1,"]"
               return value
        if ndim >= 2:
           if(not 0<j<=dim2):
               print "j=",j," is not within [",1,",",dim2,"]"
               return value
        if ndim == 3:
           if(not 0<k<=dim3):
               print "k=",k," is not within [",1,",",dim3,"]"
               return value
        if "DOUBLE" in vartype:
            self.ierr = self.api_set_double(self.id,varname,value,global_num,i,j,k)
        elif "INTEGER"in vartype:
            self.ierr = self.api_set_integer(self.id,varname,value,i,j,k)
        elif "STRING" in vartype:
            self.ierr = self.api_set_string(self.id,varname,value,dim1)
            tmp_value.tostring().strip()
        elif "BOOLEAN" in vartype:
            self.ierr = self.api_set_boolean(self.id,varname,value,i,j,k)
        else:
            print "Unknown data type ",vartype
        return value

    def get_integer(self, varname, *args):
        """
        Get the integer value of a variable of Telemac 2D
        :param varname: Name of the variable
        :param args: List of indexes By Default 0,0,0
        :return: integer variable
        """
        args = list(args) + [0] * (3 - len(args))
        value, self.ierr = self.api_get_integer(self.id, varname, *args)
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
        value, self.ierr = self.api_get_double(self.id, varname, global_num, *args)
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
        self.ierr = self.api_set_double(self.id, varname, value, global_num, *args)
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
        self.ierr = self.api_set_integer(self.id, varname, value, *args)
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
        return self.api_handle_error.err_mess.tostring().strip()

    def finalize(self):
        """
        Delete the Telemac 2D instance
        :return: error code
        """
        ierr = self.run_finalize(self.id)
        if ierr:
            print('Error: no deletion' \
                    '\nTry to use get_error_message for more information')
        return ierr

    def __del__(self):
        """
        Destructor
        """
        Telemac2d._instanciated = False
