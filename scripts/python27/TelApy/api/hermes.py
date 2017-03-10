#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    @package hermes
    Python wrapper to the Fortran APIs of module hermes of Telemac-Mascaret

    Author(s): Fabrice Zaoui, Yoann Audouin, Cedric Goeury, Renaud Barate

    Copyright EDF 2016
"""
import sys
import numpy as np
import time
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.mplot3d import Axes3D
#
PRISM = 40
TETRAHEDRON = 30
QUADRANGLE = 20
TRIANGLE = 10
BND_SEGMENT = 55
BND_POINT = 1

class TelemacFile(object):
    """The Generic Python class for TELEMAC-MASCARET APIs"""
    _hermes = None

    def __init__(self, file_name, fformat,
                 access='r', boundary_file=None):
        """
        Constructor for TelemacFile

        @param file_name Name of the file
        @param fformat File format
        @param access Access to the file ('r' for read 'w' for write)
        @param boundary_file Name of the boundary file
        """

        try:
            import _hermes
        except Exception as execpt:
            if sys.platform.startswith('linux'):
                ext = 'so'
            elif sys.platform.startswith('win'):
                ext = 'dll'
            else:
                raise Exception('Error: unsupported Operating System!')
            raise Exception('Error: unable to load the dynamic library '+\
                            '_hermes.'+ext+\
                        '\nYou can check the environment variable PYTHONPATH'+\
                        '\n'+str(execpt))
        TelemacFile._hermes = sys.modules['_hermes']

        self.fformat = fformat + ' '*(8 - len(fformat))
        self.file_name = file_name
        self.boundary_file = boundary_file

        if 'r' in access:
            if 'w' in access:
                self.openmode = 'READWRITE'
            else:
                self.openmode = 'READ     '
        elif 'w' in access:
            if 'SERAFIN' in self.fformat:
                self.openmode = 'READWRITE'
            else:
                self.openmode = 'WRITE    '
        else:
            raise Exception("Error in access string '%s' \
                             should contain only r and/or w "%access)

        self.my_id, ierr = TelemacFile._hermes.open_mesh(self.fformat,
                                                         self.file_name,
                                                         self.openmode)
        if ierr != 0:
            raise Exception("The function open_mesh crashed with the \
                             following error %d"%ierr)
        if self.boundary_file is not None:
            ierr = TelemacFile._hermes.open_bnd(self.fformat,
                                              self.boundary_file,
                                              self.my_id,
                                              self.openmode)
            if ierr != 0:
                raise Exception("The function open_bnd crashed \
                                 with the following error %d"%ierr)


        if 'READ' in self.openmode:
            # Identifying elements type in files
            ndim = self.get_mesh_dimension()
            if ndim == 2:
                self.typ_elem = TRIANGLE
            else:
                self.typ_elem = PRISM
            if 'med' in fformat.lower():
                self.typ_bnd_elem = BND_SEGMENT
            else:
                self.typ_bnd_elem = BND_POINT
        else:
            self.typ_elem = None
            self.typ_bnd_elem = None


    def __del__(self):
        """
        Destructor
        """
        if self.boundary_file is not None:
            ierr = TelemacFile._hermes.close_bnd(self.fformat, self.my_id)
            if ierr != 0:
                raise Exception("The function close_bnd crashed \
                                 with the following error %d"%ierr)

        ierr = TelemacFile._hermes.close_mesh(self.fformat, self.my_id)
        if ierr != 0:
            raise Exception("The function close_mesh crashed \
                             with the following error %d"%ierr)

    def get_mesh_title(self):
        """
        Retuns the title of the file

        @returns The title
        """

        title, ierr = TelemacFile._hermes.get_mesh_title(self.fformat, self.my_id)
        if ierr != 0:
            raise Exception("The function get_mesh_title crashed \
                             with the following error %d"%ierr)

        return title

    def get_mesh_date(self):
        """
        Retuns the date of the file

        @returns The date (6-integer-array)
        """

        date = np.zeros((6), dtype=np.int32)
        ierr = TelemacFile._hermes.get_mesh_date(self.fformat, self.my_id, date)
        if ierr != 0:
            raise Exception("The function get_mesh_date crashed \
                             with the following error %d"%ierr)

        return date

    def get_mesh_nelem(self):
        """
        Retuns the number of element in the file

        @returns The number of elements
        """

        nelem, ierr = TelemacFile._hermes.get_mesh_nelem(self.fformat,
                                                  self.my_id, self.typ_elem)
        if ierr != 0:
            raise Exception("The function get_mesh_nelem crashed \
                             with the following error %d"%ierr)

        return nelem

    def get_mesh_npoin_per_element(self):
        """
        Retuns the number of points per element in the file

        @returns The number of points per element
        """

        ndp, ierr = TelemacFile._hermes.get_mesh_npoin_per_element(\
                       self.fformat,
                       self.my_id, self.typ_elem)
        if ierr != 0:
            raise Exception("The function get_mesh_npoin_per_element crashed \
                             with the following error %d"%ierr)

        return ndp

    def get_mesh_connectivity(self):
        """
        Retuns the connectivity for the given type

        @returns An 2d array of shape (nelem, ndp)
        """

        nelem = self.get_mesh_nelem()
        ndp = self.get_mesh_npoin_per_element()
        tmp_ikle = np.zeros((nelem*ndp), dtype=np.int32)
        ierr = TelemacFile._hermes.get_mesh_connectivity(self.fformat,
                                                  self.my_id, self.typ_elem,
                                                  tmp_ikle, nelem, ndp)
        if ierr != 0:
            raise Exception("The function get_mesh_connectivity crashed \
                             with the following error %d"%ierr)
        ikle = tmp_ikle.reshape((nelem,ndp))

        return ikle

    def get_mesh_npoin(self):
        """
        Retuns the number of points

        @returns The number of points
        """

        npoin, ierr = TelemacFile._hermes.get_mesh_npoin(self.fformat,
                                                  self.my_id, self.typ_elem)
        if ierr != 0:
            raise Exception("The function get_mesh_npoin crashed \
                             with the following error %d"%ierr)

        return npoin

    def get_mesh_nplan(self):
        """
        Retuns the number of planes

        @returns The number of planes
        """

        nplan, ierr = TelemacFile._hermes.get_mesh_nplan(self.fformat,
                                                  self.my_id)
        if ierr != 0:
            raise Exception("The function get_mesh_nplan crashed \
                             with the following error %d"%ierr)

        return nplan

    def get_mesh_dimension(self):
        """
        Retuns the number of dimensions

        @returns The number of dimensions
        """

        ndim, ierr = TelemacFile._hermes.get_mesh_dimension(self.fformat,
                                                  self.my_id)
        if ierr != 0:
            raise Exception("The function get_mesh_dimension crashed \
                             with the following error %d"%ierr)

        return ndim

    def get_mesh_coord(self, jdim):
        """
        Retuns the coordinates of each points for a given dimension

        @param jdim Index of dimension [1-ndim]

        @returns A numpy array of size npoin
        """

        ndim = 0
        npoin = self.get_mesh_npoin()
        ndim = self.get_mesh_dimension()
        coord = np.zeros((npoin))
        # If in serafin and dimension 3 z coordinates is the
        # first variable at the first time step
        if jdim == 3 and 'SERAFIN' in self.fformat:
            ierr = 0
            var_names, var_units = self.get_data_var_list()
            coord = self.get_data_value(var_names[0], 0)
        else:
            ierr = TelemacFile._hermes.get_mesh_coord(self.fformat,
                                                      self.my_id,
                                                      jdim,
                                                      ndim,
                                                      coord,
                                                      npoin)
        if ierr != 0:
            raise Exception("The function get_mesh_coord crashed \
                             with the following error %d"%ierr)

        return coord

    def get_mesh_l2g_numbering(self):
        """
        Retuns the local to global numbering

        @returns The local to global numbering
        """

        ndim = 0
        npoin = self.get_mesh_npoin()
        knolg = np.zeros((npoin), dtype=np.int32)
        ierr = TelemacFile._hermes.get_mesh_l2g_numbering(self.fformat,
                                                  self.my_id,
                                                  knolg,
                                                  npoin)
        if ierr != 0:
            raise Exception("The function get_mesh_l2g_numbering crashed \
                             with the following error %d"%ierr)

        return knolg

    def get_mesh_nptir(self):
        """
        Retuns the number of interface points

        @returns The number of interface points
        """
        nptir, ierr = TelemacFile._hermes.get_mesh_nptir(self.fformat,
                                                  self.my_id)
        if ierr != 0:
            raise Exception("The function get_mesh_nptir crashed \
                             with the following error %d"%ierr)

        return nptir

    def get_bnd_ipobo(self):
        """
        Retuns the ipobo array

        @returns The ipobo array
        """

        ndim = 0
        npoin = self.get_mesh_npoin()
        nelebd = self.get_bnd_nelem()
        ipobo = np.zeros((npoin), dtype=np.int32)
        ierr = TelemacFile._hermes.get_bnd_ipobo(self.fformat,
                                                  self.my_id,
                                                  nelebd,
                                                  self.typ_bnd_elem,
                                                  ipobo,
                                                  npoin)
        if ierr != 0:
            raise Exception("The function get_bnd_ipobo crashed \
                             with the following error %d"%ierr)

        return ipobo

    def get_bnd_numbering(self):
        """
        Retuns the boundary to general numbering

        @returns The boundary to general numbering
        """

        nptfr = self.get_bnd_npoin()
        nbor = np.zeros((nptfr), dtype=np.int32)
        ierr = TelemacFile._hermes.get_bnd_numbering(self.fformat,
                                                  self.my_id,
                                                  self.typ_bnd_elem,
                                                  nbor,
                                                  nptfr)
        if ierr != 0:
            raise Exception("The function get_bnd_numbering crashed \
                             with the following error %d"%ierr)

        return nbor

    def get_bnd_connectivity(self):
        """
        Retuns the connectivity array for the boundary elements

        @returns The connectivity array for the boundary elements
        """
        nelebd = self.get_bnd_nelem()
        if self.typ_bnd_elem == BND_SEGMENT:
            ndp = 2
        else:
            ndp = 1
        tmp_ikle_bnd = np.zeros((nelebd*ndp), dtype=np.int32)
        ierr = TelemacFile._hermes.get_bnd_connectivity(self.fformat,
                                                  self.my_id,
                                                  self.typ_bnd_elem,
                                                  nelebd, ndp,
                                                  tmp_ikle_bnd)
        if ierr != 0:
            raise Exception("The function get_bnd_connectivity crashed \
                             with the following error %d"%ierr)
        ikle_bnd = np.zeros((nelebd, ndp), dtype=np.int32)
        for i in xrange(nelebd):
            for j in xrange(ndp):
                ikle_bnd[i, j] = tmp_ikle_bnd[j*nelebd+i]
        del(tmp_ikle_bnd)

        return ikle_bnd

    def get_bnd_npoin(self):
        """
        Retuns the number of boundary points

        @returns The number of boundary points
        """

        nptfr, ierr = TelemacFile._hermes.get_bnd_npoin(self.fformat,
                                                  self.my_id,
                                                  self.typ_bnd_elem)
        if ierr != 0:
            raise Exception("The function get_bnd_npoin crashed \
                             with the following error %d"%ierr)

        return nptfr

    def get_bnd_nelem(self):
        """
        Retuns the number of boundary elements

        @returns The number of boundary elements
        """

        nelebd, ierr = TelemacFile._hermes.get_bnd_nelem(self.fformat,
                                                  self.my_id,
                                                  self.typ_bnd_elem)
        if ierr != 0:
            raise Exception("The function get_bnd_nelem crashed \
                             with the following error %d"%ierr)

        return nelebd

    def get_bnd_value(self):
        """
        Retuns the information on the boundary values

        @returns liubor, lihbor, livbor, hbor, ubor, vbor, chbord,
                 litbor, tbor, atbor, btbor
        """

        nptfr = self.get_bnd_npoin()
        nelebd = self.get_bnd_nelem()
        nbor = self.get_bnd_numbering()
        lihbor = np.zeros((nptfr), dtype=np.int32)
        liubor = np.zeros((nptfr), dtype=np.int32)
        livbor = np.zeros((nptfr), dtype=np.int32)
        hbor = np.zeros((nptfr))
        ubor = np.zeros((nptfr))
        vbor = np.zeros((nptfr))
        chbord = np.zeros((nptfr))
        trac = True
        litbor = np.zeros((nptfr), dtype=np.int32)
        tbor = np.zeros((nptfr))
        atbor = np.zeros((nptfr))
        btbor = np.zeros((nptfr))
        ierr = TelemacFile._hermes.get_bnd_value(self.fformat,
                                                  self.my_id,
                                                  self.typ_bnd_elem,
                                                  nelebd,
                                                  lihbor,
                                                  liubor,
                                                  livbor,
                                                  hbor,
                                                  ubor,
                                                  vbor,
                                                  chbord,
                                                  trac,
                                                  litbor,
                                                  tbor,
                                                  atbor,
                                                  btbor,
                                                  nbor,
                                                  nptfr)
        if ierr != 0:
            raise Exception("The function get_bnd_value crashed \
                             with the following error %d"%ierr)

        return liubor, lihbor, livbor, hbor, ubor, vbor, chbord, \
               litbor, tbor, atbor, btbor

    def get_data_nvar(self):
        """
        Retuns the number of variables

        @returns The number of variables
        """

        nvar, ierr = TelemacFile._hermes.get_data_nvar(self.fformat,
                                                  self.my_id)
        if ierr != 0:
            raise Exception("The function get_data_nvar crashed \
                             with the following error %d"%ierr)

        return nvar

    def get_data_var_list(self):
        """
        Retuns the list of the variables name and units

        @returns Two arrays of size nvar first for name second for units
        """

        nvar = self.get_data_nvar()
        res = TelemacFile._hermes.get_data_var_list2(self.fformat,
                                                    self.my_id,
                                                    nvar)
        tmp_var_name, tmp_var_unit, ierr = res
        if ierr != 0:
            raise Exception("The function get_data_var_list crashed \
                             with the following error %d"%ierr)

        vnames = []
        vunit = []
        varnames = []
        varinfo = []
        # Reordering string array for variable names
        # Extracting name and info into a list
        for i in range(nvar):
            vnames.append(''.join(tmp_var_name[i*16:(i+1)*16]).strip())
            vunit.append(''.join(tmp_var_unit[i*16:(i+1)*16]).strip())
        return vnames, vunit

    def get_data_ntimestep(self):
        """
        Retuns the number of time steps

        @returns The number of time steps
        """

        ntimestep, ierr = TelemacFile._hermes.get_data_ntimestep(self.fformat,
                                                  self.my_id)
        if ierr != 0:
            raise Exception("The function get_data_ntimestep crashed \
                             with the following error %d"%ierr)

        return ntimestep

    def get_data_time(self, record):
        """
        Retuns the time of a given record

        @param record Number of the record (starts from 0)

        @returns The time
        """

        time, ierr = TelemacFile._hermes.get_data_time(self.fformat,
                                                  self.my_id,
                                                  record)
        if ierr != 0:
            raise Exception("The function get_data_time crashed \
                             with the following error %d"%ierr)

        return time

    def get_data_value(self, var_name, record):
        """
        Retuns the value for each point for a given variable and a given record

        @param var_name Name of the variable
        @param record Number of the record (starts from 0)

        @returns A numpy array of size npoin
        """

        npoin = self.get_mesh_npoin()
        values = np.zeros((npoin))
        var_name2 = var_name + ' '*(16 - len(var_name))
        ierr = TelemacFile._hermes.get_data_value(self.fformat,
                                                  self.my_id,
                                                  record,
                                                  var_name2,
                                                  values,
                                                  npoin)
        if ierr != 0:
            raise Exception("The function get_data_value crashed \
                             with the following error %d"%ierr)

        return values

    def set_header(self, title, nvar, var_name, var_unit):
        """
        Write header of the file

        @param title Title of the file
        @param nvar Number of variables
        @param var_name Name for each variable
        @param var_unit Unit for each variable
        """
        tmp_title = title + ' '*(80-len(title))
        tmp_var_name = [' ']*32*nvar
        for i, (var, unit) in enumerate(zip(var_name,var_unit)):
            for j in xrange(len(var)):
                tmp_var_name[i*32+j] = var[j]
            for j in xrange(len(unit)):
                tmp_var_name[i*32+16+j] = unit[j]
        ierr = TelemacFile._hermes.set_header(self.fformat, self.my_id,
                                              tmp_title, tmp_var_name, nvar)

        if ierr != 0:
            raise Exception("The function set_header crashed \
                             with the following error %d"%ierr)

    def set_mesh(self, mesh_dim, typ_elem, ndp, nptfr, nptir, nelem, npoin,
                 ikles, ipobo, knolg, coordx, coordy, nplan, date,
                 time, coordz=None):
        """
        Write the mesh information into the file

        @param mesh_dim
        @param mesh_dim Dimension of the mesh
        @param typelm TYPE OF THE MESH ELEMENTS
        @param ndp Number of points per element
        @param nptfr Number of boundary point
        @param nptir Number of interface point
        @param nelem Number of element in the mesh
        @param npoin Number of points in the mesh
        @param ikle Connectivity array for the main element
        @param ipobo Is a boundary point ? array
        @param knolg Local to global numbering array
        @param coordx X coordinates of the mesh points
        @param coordy Y coordinates of the mesh points
        @param nplan Number of planes
        @param date Date of the creation of the mesh
        @param time Time of the creation of the mesh
        @param coordz Z coordinates of the mesh points
        """
        if coordz is None:
            tmp_z = np.zeros((npoin))
        else:
            tmp_z = coordz

        self.typ_elem = typ_elem

        tmp_ikle = ikles.T.reshape((nelem*ndp))

        ierr = TelemacFile._hermes.set_mesh(self.fformat, self.my_id,
                                            mesh_dim, typ_elem, ndp, nptfr,
                                            nptir, nelem, tmp_ikle,
                                            ipobo, knolg, coordx, coordy,
                                            nplan, date, time, npoin, tmp_z)
        if ierr != 0:
            raise Exception("The function set_mesh crashed \
                             with the following error %d"%ierr)
        del(tmp_ikle)
        if coordz is None:
            del(tmp_z)

    def add_data(self, var_name, var_unit, time, record, first_var, values):
        """
        Write inform ation for a given variable and a given timestep

        @param var_name Name of the variable
        @param time Time of the data
        @param record Time step of the data (starts from 0)
        @param first_var True if it is the first variable of the dataset
        @param var_value The value for each point of the mesh
        """
        nval = len(values)
        tmp_var_name = var_name + ' '*(16-len(var_name)) +\
                       var_unit + ' '*(16-len(var_unit))
        ierr = TelemacFile._hermes.add_data(self.fformat, self.my_id,
                                            tmp_var_name, time, record,
                                            first_var, values, nval)

        if ierr != 0:
            raise Exception("The function add_data crashed \
                             with the following error %d"%ierr)

    def set_bnd(self, typ_bnd_elem, nelebd, ikle, lihbor, liubor,
                livbor, hbor, ubor, vbor, chbord, litbor, tbor, atbor, btbor):
        """
        Write boundary information

        @param nelebd Number of boundary elements
        @param ndp Number of points per boundary element
        @param ikle Connectivity array for the boundary elements
        @param lihbor Type of boundary conditions on depth
        @param liubor Type of boundary conditions on u
        @param livbor Type of boundary conditions on v
        @param hbor Prescribed boundary condition on depth
        @param ubor Prescribed boundary condition on velocity u
        @param vbor Prescribed boundary condition on velocity v
        @param chbord Friction coefficient at boundary
        @param litbor Physical boundary conditions for tracers
        @param tbor Prescribed boundary condition on tracer
        @param atbor Thermal exchange coefficients
        @param btbor Thermal exchange coefficients
        """
        self.typ_bnd_elem = typ_bnd_elem
        if self.typ_bnd_elem == BND_SEGMENT:
            ndp = 2
        else:
            ndp = 1
        # Switching
        tmp_ikle = ikle.T.reshape((nelebd*ndp))

        ierr = TelemacFile._hermes.set_bnd(self.fformat, self.my_id,
                                           typ_bnd_elem, ndp, tmp_ikle, lihbor,
                                           liubor, livbor, hbor, ubor, vbor,
                                           chbord, litbor, tbor, atbor, btbor,
                                           nelebd)

        if ierr != 0:
            raise Exception("The function set_bnd crashed \
                             with the following error %d"%ierr)
        del(tmp_ikle)

    def show_mesh(self, show=True, visu2d=True):
        """
        Show the 2D mesh with topography

        @param show Display the graph (Default True)
        @param visu2d 2d display (Default True)

        @retuns the figure object
        """
        coordx = self.get_mesh_coord(1)
        coordy = self.get_mesh_coord(2)
        tri = self.get_mesh_connectivity() - 1
        bottom = self.get_data_value('WATER DEPTH',0)
        fig = plt.figure()
        if visu2d:
            plt.tripcolor(coordx, coordy, tri, bottom,
                          shading='flat', edgecolor='w', cmap=cm.terrain)
            plt.colorbar()
        else:
            axe = Axes3D(fig)
            axe.plot_trisurf(coordx, coordy, tri, bottom,\
                             cmap=cm.terrain, linewidth=0.1)
        plt.title('2D mesh (%d triangles, %d nodes) \
                   with the bottom elevation (m)' % (self.get_mesh_nelem(),
                                                     self.get_mesh_npoin()))
        plt.xlabel('X-coordinate (m)')
        plt.ylabel('Y-coordinate (m)')
        if show:
            plt.show()
        return fig

    def import_group_info(self, src):
        """
        Import group information from a given TelemacFile

        @param src TelemacFile from which group will be imported
        """
        self.typ_bnd_elem = src.typ_bnd_elem
        if self.typ_bnd_elem is None:
            tmp_typ_bnd_elem = 0
        else:
            tmp_typ_bnd_elem = self.typ_bnd_elem
        ikle_bnd = src.get_bnd_connectivity()
        nelebd, ndp = ikle_bnd.shape
        tmp_ikle_bnd = ikle_bnd.T.reshape((nelebd*ndp))
        ierr = TelemacFile._hermes.transfer_group_info(\
                   self.fformat, src.my_id,
                   self.my_id, self.typ_elem,
                   tmp_typ_bnd_elem, tmp_ikle_bnd, nelebd, ndp,
                   False, False)

        if ierr != 0:
            raise Exception("The function transfer_group_info crashed \
                             with the following error %d"%ierr)

    def import_from(self, src):
        """
        Rewriting src into dst

        @param src TelemacFile from which the data is imported
        """
        title = src.get_mesh_title()
        date = src.get_mesh_date()

        ndim = src.get_mesh_dimension()
        typ_elem = src.typ_elem
        typ_bnd_elem = src.typ_bnd_elem

        npoin = src.get_mesh_npoin()
        nelem = src.get_mesh_nelem()
        ndp = src.get_mesh_npoin_per_element()
        nplan = src.get_mesh_nplan()

        coordx = src.get_mesh_coord(1)
        coordy = src.get_mesh_coord(2)
        coordz = None
        if ndim == 3:
            coordz = src.get_mesh_coord(3)

        ikle = src.get_mesh_connectivity()

        nptir = src.get_mesh_nptir()
        if nptir > 0:
            knolg = src.get_mesh_l2g_numbering()
        else:
            knolg = np.zeros((npoin), dtype=np.int32)


        if src.boundary_file is not None:
            typ_bnd_elem = src.typ_bnd_elem
            nptfr = src.get_bnd_npoin()
            nelebd = src.get_bnd_nelem()
            ipobo = src.get_bnd_ipobo()
            nbor = src.get_bnd_numbering()
            ikle_bnd = src.get_bnd_connectivity()

            lihbor, liubor, livbor, hbor, ubor, vbor, chbord, \
                    litbor, tbor, atbor, btbor = src.get_bnd_value()
        else:
            nptfr = 0
            nelebd = 0
            ipobo = np.zeros((npoin), dtype=np.int32)


        ntimestep = src.get_data_ntimestep()
        nvar = src.get_data_nvar()
        var_name, var_unit = src.get_data_var_list()

        self.set_header(title, nvar, var_name, var_unit)

        date2 = np.zeros((3), dtype=np.int32)
        time2 = np.zeros((3), dtype=np.int32)
        date2[0] = date[0]
        date2[1] = date[1]
        date2[2] = date[2]
        time2[0] = date[3]
        time2[1] = date[4]
        time2[2] = date[5]

        self.set_mesh(ndim, typ_elem, ndp, nptfr, nptir, nelem, npoin,
                         ikle, ipobo, knolg, coordx, coordy, nplan, date2,
                         time2, coordz)


        if self.fformat == src.fformat and 'SERAFIN' not in self.fformat:
            self.import_group_info(src)
        else:
            if src.boundary_file is not None:
                self.set_bnd(typ_bnd_elem, nelebd, ikle_bnd, lihbor, liubor,
                            livbor, hbor, ubor, vbor, chbord, litbor, tbor,
                            atbor, btbor)


        for i in xrange(ntimestep):
            time = src.get_data_time(i)
            for j in xrange(nvar):
                values = src.get_data_value(var_name[j], i)
                self.add_data(var_name[j], var_unit[j], time, i, j==0, values)
                del(values)

    def __repr__(self):
        """
        representation of the object
        """
        string = '*'*32 + '\n'
        string += 'Generic info' + '\n'
        string += '*'*32 + '\n'

        string += "Title: %s\n"%self.get_mesh_title()
        date = self.get_mesh_date()
        string += "Date: %d/%d/%d %dH%dM%dS\n"%(date[2], date[1], date[0],
                                                date[3], date[4], date[5])

        string += '*'*32 + '\n'
        string += 'Mesh info\n'
        string += '*'*32 + '\n'

        ndim = self.get_mesh_dimension()
        string += "Ndim: %d\n"%ndim

        if self.typ_elem == TRIANGLE:
            string += "Element type: TRIANGLE\n"
        elif self.typ_elem == PRISM:
            string += "Element type: PRISM\n"
        else:
            string += "Element type: UNKNOWN\n"
        string += "Npoin: %d\n"%self.get_mesh_npoin()
        string += "Nelem: %d\n"%self.get_mesh_nelem()
        string += "Ndp: %d\n"%self.get_mesh_npoin_per_element()
        string += "nplan: %d\n"%self.get_mesh_nplan()

        string += "coordinates:\n"
        string += " - On x :%s\n"%str(self.get_mesh_coord(1))
        string += " - On y :%s\n"%str(self.get_mesh_coord(2))
        if ndim == 3:
            string += " - On z :%s\n"%str(self.get_mesh_coord(3))

        string += "ikle: %s\n"%str(self.get_mesh_connectivity())

        string += '*'*32 + '\n'
        string += 'Parallel info\n'
        string += '*'*32 + '\n'
        nptir = self.get_mesh_nptir()
        if nptir != 0:
            string += "Nptir: %d\n"%nptir
            string += "knolg: %s\n"%str(self.get_mesh_l2g_numbering())
        else:
            string += 'No parallel information'

        string += '*'*32 + '\n'
        string += 'Bnd info\n'
        string += '*'*32 + '\n'
        if self.boundary_file is not None:
            if self.typ_bnd_elem == BND_POINT:
                string += "Bnd element: BND_POINT\n"
            elif self.typ_bnd_elem == BND_SEGMENT:
                string += "Bnd element: BND_SEGMENT\n"
            else:
                string += "Bnd element: UNKNOWN\n"
            string += "Nptfr: %d\n"%self.get_bnd_npoin()
            string += "Nelebd: %d\n"%self.get_bnd_nelem()

            string += "ipobo: %s\n"%self.get_bnd_ipobo()
            string += "nbor: %s\n"%self.get_bnd_numbering()
            string += "ikle_bnd: %s\n"%self.get_bnd_connectivity()

            lihbor, liubor, livbor, _, _, _, _, \
                    litbor, _, _, _ = self.get_bnd_value()
            string += "bnd_values: \n"
            string += str(zip(lihbor, liubor, livbor, litbor)) + '\n'
        else:
            string += "No boundary information"

        string += '*'*32 + '\n'
        string += 'Data info\n'
        string += '*'*32 + '\n'

        ntimestep = self.get_data_ntimestep()
        string += "ntimestep: %d\n"%ntimestep
        nvar = self.get_data_nvar()
        string += "nvar: %d\n"%nvar
        var_name, var_unit = self.get_data_var_list()
        string += "var info:\n"
        for var in zip(var_name, var_unit):
            string += str(var) + '\n'

        for i in xrange(ntimestep):
            string += "Time: %fs\n"%self.get_data_time(i)
            for j in xrange(nvar):
                string += " - for %s:\n"%var_name[j]
                string += "     " + str(self.get_data_value(var_name[j], i)) + '\n'

        return string
