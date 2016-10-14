#!/bin/env python
from TelApy.api.sis import Sisyphe
import sys

if __name__ == '__main__':
    sis = Sisyphe("sis_bosse.cas",user_fortran='sis_bosse.f')

    sis.set_case()

    varnames,varinfo = sis.list_variables()
    for name,info in zip(varnames,varinfo):
        print name
        print info

    sis.init_state_default()

    sis.run_all_time_steps()

    sis.finalize()

    del(sis)

