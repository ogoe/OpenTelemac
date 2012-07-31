### TELEMAC settings -----------------------------------------------------------
export HOMETEL=/home/G55570/opentelemac/branches/zebrafish
export PATH=/netdata/telemac/V6P2/bin:.:$PATH
### ALIASES -----------------------------------------------------------
export SOURCEFILE=/netdata/telemac/V6P2/config/source.pgi_64_9_C7.sh
export SYSTELCFG=/netdata/telemac/V6P2/config/pgi_64_9_C7
export RELTEL=v6p2
### COMPILERS -----------------------------------------------------------
### PGI  ---------------------------------------------------------
PGI=/logiciels/pgi/9.0-1/linux86-64/9.0-1
PATH=$PGI/bin:$PATH
MANPATH=$PGI/man
export PGI PATH MANPATH
### MPI -----------------------------------------------------------
export PATH=/netdata/systel/LIBRARY//mpi/pgi_64_9_C7/bin:$PATH
export LD_LIBRARY_PATH=/netdata/systel/LIBRARY//mpi/pgi_64_9_C7/lib:$LD_LIBRARY_PATH
### EXTERNAL LIBRARIES -----------------------------------------------------------

### HDF5 -----------------------------------------------------------
HDF5HOME=/netdata/systel/LIBRARY//HDF5/hdf5_1.6.4_pgi_64_9_C7/
export HDF5HOME
LD_LIBRARY_PATH=$HDF5HOME/lib:$LD_LIBRARY_PATH
LD_RUN_PATH=$HDF5HOME/lib:$MEDHOME/lib:$LD_RUN_PATH
export LD_LIBRARY_PATH LD_RUN_PATH
### MED  -----------------------------------------------------------
MEDHOME=/netdata/systel/LIBRARY//MED/med_2.3.4_pgi_64_9_C7/
export MEDHOME
LD_LIBRARY_PATH=$MEDHOME/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH
PATH=$MEDHOME/bin:$PATH
export PATH
### PARAVIEW  ---------------------------------------------------------
export PATH=/netdata/pvmeshl/PARAVIEW_SERAFIN/Paraview/Install/bin:$PATH
export PV_PLUGIN_PATH=/netdata/pvmeshl/PARAVIEW_SERAFIN/Paraview/Install/bin/plugins
