#!/usr/bin/env python
from TelApy.api.sis import Sisyphe
import sys
from mpi4py import MPI
# Creation of the instance Telemac3d
comm = MPI.COMM_WORLD

sis = Sisyphe("sis_bosse.cas",user_fortran='sis_bosse.f')

rank = comm.Get_rank()
ncsize = comm.Get_size()
# Running partel
if ( rank == 0 and ncsize > 1):
    ierr = sis.api_inter.run_partel(t2d.id,'geo_bosse.slf','geo_bosse.cli',ncsize,1,'SERAFIN ',' ',' ',' ')

sis.set_case()

varnames,varinfo = sis.list_variables()
for name,info in zip(varnames,varinfo):
    print name
    print info

sis.init_state_default()

sis.run_all_time_steps()

sis.finalize()
if ( rank == 0 and ncsize > 1):
    sis.api_inter.run_gretel(t2d.id,'geo_bosse.slf','SERAFIN ','sis_bosse.slf','SERAFIN ',ncsize,0)

del(sis)

