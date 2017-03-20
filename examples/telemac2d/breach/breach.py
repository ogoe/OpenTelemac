#!/usr/bin/env python
from TelApy.api.t2d import Telemac2d
import numpy as np
from mpi4py import MPI
import sys

t2d = Telemac2d("t2d_breach.cas",comm=MPI.COMM_WORLD)

t2d.set_case()

varnames, varinfo = t2d.list_variables()
for name, info in zip(varnames, varinfo):
    print name
    print info

t2d.init_state_default()

t2d.run_all_time_steps()

del(t2d)
