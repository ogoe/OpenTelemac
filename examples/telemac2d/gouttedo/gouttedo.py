#!/usr/bin/env python
# Class Telemac2d import
from TelApy.api.t2d import Telemac2d
from mpi4py import MPI
# Creation of the instance Telemac3d
comm = MPI.COMM_WORLD
t2d = Telemac2d('t2d_gouttedo.cas',user_fortran ='t2d_gouttedo.f',comm=comm)
ierr = 0
rank = comm.Get_rank()
ncsize = comm.Get_size()
# Running partel
if ( rank == 0 and ncsize > 1):
    ierr = t2d.api_inter.run_partel(t2d.id,'geo_gouttedo.slf','geo_gouttedo.cli',ncsize,1,'SERAFIN ',' ',' ',' ')
comm.Barrier()
print ierr
t2d.set_case()
# Initalization
t2d.init_state_default()
# Run all time steps
t2d.run_all_time_steps()
# Running gretel
comm.Barrier()
# Ending the run
t2d.finalize()
if ( rank == 0 and ncsize > 1):
    t2d.api_inter.run_gretel(t2d.id,'geo_gouttedo.slf','SERAFIN ','r2d_gouttedo_v1p0.slf','SERAFIN ',ncsize,0)
# Instance delete
del(t2d)
