### TELEMAC settings -----------------------------------------------------------
export HOMETEL=/home/G55570/opentelemac/branches/zebrafish
export PATH=$HOMETEL/bin:.:$PATH
### ALIASES -----------------------------------------------------------
export SOURCEFILE=$HOMETEL/config_perl/source.gfortran_linux_C7.sh
export SYSTELCFG=$HOMETEL/config_perl/gfortran_linux_C7
export RELTEL=v6p2
### COMPILERS -----------------------------------------------------------

### MPI -----------------------------------------------------------
export PATH=/netdata/systel/LIBRARY//mpi/gfortran_linux_C7/bin:$PATH
export LD_LIBRARY_PATH=/netdata/systel/LIBRARY//mpi/gfortran_linux_C7/lib:$LD_LIBRARY_PATH
### EXTERNAL LIBRARIES -----------------------------------------------------------

### HDF5 -----------------------------------------------------------
HDF5HOME=/netdata/systel/LIBRARY//HDF5/hdf5_1.6.4_gfortran_linux_C7/
export HDF5HOME
LD_LIBRARY_PATH=$HDF5HOME/lib:$LD_LIBRARY_PATH
LD_RUN_PATH=$HDF5HOME/lib:$MEDHOME/lib:$LD_RUN_PATH
export LD_LIBRARY_PATH LD_RUN_PATH
### MED  -----------------------------------------------------------
MEDHOME=/netdata/systel/LIBRARY//MED/med_2.3.4_gfortran_linux_C7/
export MEDHOME
LD_LIBRARY_PATH=$MEDHOME/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH
PATH=$MEDHOME/bin:$PATH
export PATH
### PARAVIEW  ---------------------------------------------------------
export PATH=/netdata/systel/PRE_POST/PARAVIEW/ParaView3.6/bin:$PATH
export PV_PLUGIN_PATH=/netdata/systel/PRE_POST/PARAVIEW/ParaView3.6/bin/plugins

