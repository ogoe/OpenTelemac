rm sel2vtk_pgi *.o
ifort -c sel2vtk.f90
ifort -o sel2vtk_pgi sel2vtk.o
sel2vtk_pgi < vtk.par
