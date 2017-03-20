#!/usr/bin/env python

from TelApy.api.hermes import TelemacFile
from os import path
import filecmp

def read_write(fformat, mesh_file, bnd_file=None, compare=True):
    """
    Read a couple mesh, boundary and rewrite it
    And check that the dump are identical
    And the binaries as well

    @param fformat Format for the files
    @param mesh_file Name of the mesh file
    @param bnd_file Name of the boundary file
    """

    print "Opening Serafin file"
    myfile = TelemacFile(mesh_file, fformat, 'r', bnd_file)
    str_file = str(myfile)

    mesh_file2 = 'hermes-'+mesh_file
    if bnd_file is not None:
        bnd_file2 = 'hermes-'+bnd_file
    else:
        bnd_file2 = None

    myfile2 = TelemacFile(mesh_file2, fformat, 'w', bnd_file2)

    myfile2.import_from(myfile)
    print 'import passed'

    del(myfile)
    del(myfile2)

    # Binary files should be identical
    if compare:
        assert(filecmp.cmp(mesh_file, mesh_file2))

if __name__ == "__main__":
    print "Testing r2d_gouttedo.slf"
    read_write('SERAFIN', 'r2d_gouttedo.slf', 'geo_gouttedo.cli')
    print "Testing r3d_gouttedo.slf"
    read_write('SERAFIN', 'r3d_gouttedo.slf', 'geo_gouttedo.cli')
    print "Testing geo_gouttedo.med"
    read_write('MED', 'geo_gouttedo.med', 'geo_gouttedo.clm', False)
