rm sel2vtk_ifort *.o
ifort -c sel2vtk.f90
ifort -o sel2vtk_ifort sel2vtk.o
sel2vtk_ifort < vtk.par
