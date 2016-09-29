# Class Telemac2d import
from TelApy.api.t2d import Telemac2d
# Creation of the instance Telemac2d
t2d = Telemac2d('t2d_gouttedo.cas',user_fortran ='t2d_gouttedo.f')
t2d.set_case()
# Initalization
t2d.init_state_default()
# Run all time steps
t2d.run_all_time_steps()
# Ending the run
t2d.finalize()
# Instance delete
del(t2d)
