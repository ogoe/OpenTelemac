# This file is a template for a Linux environement file 
# runnning "source pysource.template.sh" will position all 
# the necessary environement variable for telemac
# To adapt to your installation replace word <word> by their local value
###
### TELEMAC settings -----------------------------------------------------------
###
# Path to telemac root dir
export HOMETEL=/home/I21149/Modeles/TELEMAC/chromis
# Adding python scripts to PATH
export PATH=$HOMETEL/scripts/python27:.:$PATH
# Path to this file
export SOURCEFILE=$HOMETEL/configs/pysource.template.sh
# Configuration file
export SYSTELCFG=$HOMETEL/configs/systel.edf.cfg
# Name of the configuration to use
export USETELCFG=C9.gfortranHPC.debug
### Python
# To force python to flush its output
export PYTHONUNBUFFERED='true'
###
### COMPILERS -----------------------------------------------------------
###
export SYSTEL=/projets/projets.002/systel.002
# Here are a few exemple for external libraries

### MPI -----------------------------------------------------------
#export MPIHOME=$SYSTEL/LIBRARY/mpi/ifort.10.1.008
#export PATH=$MPIHOME/bin:$PATH
#export LD_LIBRARY_PATH=$MPIHOME/lib:$LD_LIBRARY_PATH
###
### EXTERNAL LIBRARIES -----------------------------------------------------------
###
### HDF5 -----------------------------------------------------------
export HDF5HOME=$SYSTEL/LIBRARY/hdf5-1.8.11/arch/C9
export LD_LIBRARY_PATH=$HDF5HOME/lib:$LD_LIBRARY_PATH
export LD_RUN_PATH=$HDF5HOME/lib:$MEDHOME/lib:$LD_RUN_PATH
### MED  -----------------------------------------------------------
export MEDHOME=$SYSTEL/LIBRARY/med-3.0.6/arch/C9
export LD_LIBRARY_PATH=$MEDHOME/lib:$LD_LIBRARY_PATH
export PATH=$MEDHOME/bin:$PATH
