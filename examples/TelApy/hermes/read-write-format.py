#!/usr/bin/env python

from TelApy.api.hermes import TelemacFile
from os import path

def read_write(mesh_file, fformat, outformat, compare=True):
    """
    Open file mesh_file and rewrite it in outformat
    And check that they contain the same data

    @param mesh_file Name of the input file
    @param fformat Format of mesh_file
    @param outformat Output format
    """
    print "Opening MED file"
    myfile = TelemacFile(mesh_file, fformat, 'r')

    print "Opening Serafin file"
    root, ext = path.splitext(mesh_file)
    mesh_file2 = 'hermes-'+root+'-from-'+fformat.lower()+ext
    myfile2 = TelemacFile(mesh_file2, outformat, 'w')

    print "Importing data"
    myfile2.import_from(myfile)

    str_file = str(myfile)

    del(myfile)
    del(myfile2)

    myfile2 = TelemacFile(mesh_file2, outformat, 'r')
    str_file2 = str(myfile2)

    # Both file should contain the same data
    if compare:
        assert(str_file == str_file2)


if __name__ == "__main__":
    print "Testing MED -> SRF in 2D"
    read_write('geo_gouttedo.med', 'MED', 'SERAFIN')
    print "Testing SRF -> MED in 2D"
    read_write('r2d_gouttedo.slf', 'SERAFIN', 'MED')
    print "Testing MED -> SRF in 3D"
    read_write('r3d_gouttedo.med', 'MED', 'SERAFIN', False)
    print "Testing SRF -> MED in 3D"
    read_write('r3d_gouttedo.slf', 'SERAFIN', 'MED')
