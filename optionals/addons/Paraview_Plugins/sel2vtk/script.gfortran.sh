rm sel2vtk_gfortran *.o
gfortran -c -fconvert=big-endian sel2vtk.f90
gfortran -o sel2vtk_gfortran sel2vtk.o
