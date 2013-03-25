rm sel2vtk_ifort *.o
ifort -c -convert big_endian sel2vtk.f90
ifort -o sel2vtk_ifort sel2vtk.o
