import os,sys
import subprocess
from time import strftime
from math import modf
#
verbose=False
valgrind=False
exe = ''
try:
    source = os.environ['SOURCEFILE']
except:
    print ''
# ------------------------------------------------------------#
#
# Print with color in Xterm
# ------------------------------------------------------------#
class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

    def disable(self):
        self.HEADER = ''
        self.OKBLUE = ''
        self.OKGREEN = ''
        self.WARNING = ''
        self.FAIL = ''
        self.ENDC = ''

# ------------------------------------------------------------#
#
# Fill submit file
# ------------------------------------------------------------#        
def RunScript_queue(code,cas,jobname,source,ncsize,exe,valgrind):#:,queuename):
    if verbose:
        print bcolors.OKGREEN + "Function : RunScript_queue" + bcolors.ENDC
    sourcefile = open(jobname, mode='w')
    if modf(int(ncsize)/12.)[0] == 0:
	Nnodes = int(modf(long(ncsize)/12)[1])
    else:
	Nnodes = int(modf(long(ncsize)/12)[1])+1
    Ncores=12
    runfile = \
"""#!/bin/bash
source %(sourcefile)s
##@ group       = TELEMAC
#@ job_name    = %(jobname)s
#@ initialdir  = %(pwd)s
#@ output      = %(jobname)s.o%(time)s
#@ error       = %(jobname)s.e%(time)s
#@ class       = standard
#@ job_type    = mpich
#@ node        = %(Nnodes)s
#@ tasks_per_node = %(Ncores)s
#@ node_usage  = not_shared
#@ queue
for line in $(echo | cat $LOADL_HOSTFILE)
do
  echo -e "$line 1"
done > mpitasks.conf
echo $LOADL_TOTAL_TASKS > mpitasks | cat mpitasks mpitasks.conf > mpi_telemac.conf
#
rm -f mpitasks mpitasks.conf
"""
    sourcefile.write(runfile %{'pwd':os.getcwd(),'jobname':jobname,'Nnodes':Nnodes,'Ncores':Ncores,'sourcefile':source,'time':strftime("%Y-%m-%d_%H:%M:%S")})
    if valgrind:
        runfile = \
"""mpirun -f $LOADL_HOSTFILE -n %(ncsize)s valgrind --tool=callgrind --base=%(basename)s %(exe)s
#
# End of the Script
echo 'fin'
"""
        sourcefile.write(runfile %{'exe':exe,'ncsize':ncsize,'basename':os.path.join(os.getcwd(),'callgrind.out')})    
    else:
        runfile = \
"""%(code)s -t %(case)s
#
# End of the Script
echo 'fin'
"""
        sourcefile.write(runfile %{'code':code,'case':cas})
    # close the source file
    sourcefile.close()

# Main
# ------------------------------------------------------------#
# read variables from command line, one by one:
while len(sys.argv) > 1:
    option = sys.argv[1]
    del sys.argv[1]
    if option == '-code':
        # if then, print everything it could...
        code = sys.argv[1]
        del sys.argv[1]
    elif option == '-case':
        # if then, print everything it could...
        cas = sys.argv[1]
        del sys.argv[1]
    elif option == '-job':
        # if then, print everything it could...
        jobname = sys.argv[1]
        del sys.argv[1]
    elif option == '-ncsize':
        # if then, print everything it could...
        ncsize = sys.argv[1]
        del sys.argv[1]        
    elif option == '-queue':
        # if then, print everything it could...
        queuename = sys.argv[1]
        del sys.argv[1]
    elif option == '-source':
        # if then, print everything it could...
        source = sys.argv[1]
        del sys.argv[1]
    elif option == '-exe':
        # if then, print everything it could...
        exe = sys.argv[1]
        del sys.argv[1]
        valgrind = True
    else:
        print bcolors.FAIL + "invalid option :%s "%option + bcolors.ENDC
        sys.exit(1)

RunScript_queue(code,cas,jobname,source,int(ncsize),exe,valgrind)#,queuename)
subprocess.call ('llsubmit %s'%jobname, shell = True)
sys.exit(0)
