#!/usr/bin/env python
# Class Telemac2d import
from TelApy.api.t2d import Telemac2d
from mpi4py import MPI
# Creation of the instance Telemac3d
comm = MPI.COMM_WORLD
t2d = Telemac2d('t2d_gouttedo.cas',user_fortran ='t2d_gouttedo.f',comm=comm)
ierr = 0
# Running partel
if comm.Get_rank == 0:
    ierr = t2d.api_inter.run_partel(t2d.id,'geo_gouttedo.slf','geo_gouttedo.cli',4,1,'SERAFIN ',' ',' ',' ')
comm.Barrier()
print ierr
t2d.set_case()
# Initalization
t2d.init_state_default()
# Run all time steps
t2d.run_all_time_steps()
# Running gretel
comm.Barrier()
if comm.Get_rank == 0:
    t2d.api_inter.run_gretel(t2d.id,'geo_gouttedo.slf','SERAFIN ','r2d_gouttedo_v1p0.slf','SERAFIN ',4,0)
# Ending the run
t2d.finalize()
# Instance delete
del(t2d)
