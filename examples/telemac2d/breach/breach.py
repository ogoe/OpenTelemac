#!/bin/env python
from TelApy.api.t2d import Telemac2d

if __name__ == '__main__':
    t2d = Telemac2d("t2d_breach.cas")

    t2d.set_case()

    t2d.init_state_default()

    t2d.run_all_time_steps()

    t2d.finalize()

    del(t2d)
