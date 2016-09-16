# Class Telemac2d import
from TelApy.api.t2d import Telemac2d
# Creation of the instance Telemac2d
t2d = Telemac2d('t2d_gouttedo.cas',user_fortran ='t2d_gouttedo.f')
# Initalization
t2d.init_state_default()
# Run all time steps
t2d.run_all_time_steps()
# Instance delete
del(t2d)
