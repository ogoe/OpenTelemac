#!/bin/env python
import sys
from ctypes import cdll
import os
import tempfile
import numpy as np
from TelApy.api.t2d import Telemac2d

if __name__ == '__main__':
    t2d = Telemac2d("t2d_breach.cas")

    t2d.init_state_default()

    t2d.run_all_time_steps()

    del(t2d)
