#!/usr/bin/env python
import sys, os, shutil
import subprocess #call, Popen, PIPE
# ------------------------------------------------------------#
# Objectif : lanceur TELEMAC sur machine BlueGene/P
#
# ------------------------------------------------------------#
#
# Global variable and default values of input parameters
# ------------------------------------------------------------#
# Debug
verbose = False
debug = False
split = False # Noeuds de calcul : Pas de decoupage du domaine par defaut
merge = False # Noeuds de calcul : Pas d agglomeration des resultats par defaut
splitfront = False # Frontale : Pas de decoupage du domaine par defaut
mergefront = False # Frontale : Pas d agglomeration des resultats par defaut
mpirun = False # Option de lancement directement sur les noeuds d'un executable (ex: out*.exe)
runcode = True # Option mpirun couplee a runcode...
version = 'v6p0'
#
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
# Error raised by LoadLeveler
# ------------------------------------------------------------#
errorbgp = ['WARN','ERROR','killed by']
# ------------------------------------------------------------#
#
# Class machine
# ------------------------------------------------------------#
class machine:
    def __init__(self):
        self.setup()
    def setup(self):
        self.name = os.name
        self.arch = sys.platform
        uname = os.uname()
        if verbose:
            print bcolors.OKGREEN + "function    : set_hosttype" + bcolors.ENDC
#        if uname[1] == 'fenp':
        self.hosttype = ['BGP_XLF_11']
        #else:
        #    print bcolors.FAIL + "this machine is not a BlueGene:%s "%self.hosttype + bcolors.ENDC
        #    sys.exit(1)
        if verbose:
            print bcolors.OKBLUE + "OS:%s ; Platform:%s"%(self.name,self.arch) + bcolors.ENDC
            for item in self.hosttype:
                print bcolors.OKBLUE + "hosttype : %s"%item + bcolors.ENDC
# ------------------------------------------------------------#
#
# Class perl
# ------------------------------------------------------------#
class perl:
    def __init__(self,hosttype):
        self.hosttype = hosttype
        self.setup()
    def setup(self):
        self.export()
        self.project()
        self.printvar()
        paral = os.path.join(self.project,'parallel/parallel_%s'%version,self.hosttype)
        self.partel = os.path.join(paral,'partel')
        self.gretel = os.path.join(paral,'gretel')
        self.partelfront = os.path.join(paral,'partelfront')
        self.gretelfront = os.path.join(paral,'gretelfront')
    def export(self):
        # Get/Export $SYSTELCFG & BIN
        if verbose:
            print bcolors.OKGREEN + "function    : systelcfg" + bcolors.ENDC            
        # Set the $SYSTELCFG environment variable from current config
        self.systelcfg = os.environ['SYSTELCFG']
        self.systelini = self.systelcfg + os.path.sep + 'systel.ini'
        return        
    def project(self):
        if verbose:
            print bcolors.OKGREEN + "function    : project" + bcolors.ENDC    
            # Get the $SYSTELCFG environment variable
        systel = open(self.systelini, 'r')
        # open systel.ini to extract PROJECT
        for line in systel:
            if 'PROJECT' in line:
                project = line.split('=')     
                if project[0] == 'PROJECT':
                    # get the home phe directory
                    self.project = project[1]
                    self.project = self.project.replace("\n", "")
                    break
        systel.close
        return
    def printvar(self):
        if verbose:
            print bcolors.OKGREEN + "function    : printvar" + bcolors.ENDC    
            print bcolors.OKBLUE + "SYSTELCFG : %s"%self.systelcfg + bcolors.ENDC
            print bcolors.OKBLUE + "SYSTELINI   : %s"%self.systelini + bcolors.ENDC
            print bcolors.OKBLUE + "PROJECT     : %s"%self.project + bcolors.ENDC
            print bcolors.OKBLUE + "$PATH : %s"%os.environ['PATH'] + bcolors.ENDC
            print bcolors.OKBLUE + "$SYSTELCFG : %s"%os.environ['SYSTELCFG'] + bcolors.ENDC
    def cfgmak(self):
        # Update PHE variables
        if verbose:
            print bcolors.OKGREEN + "function    : cfgmak" + bcolors.ENDC
        if 'SYSTELCFG' in os.environ:
            os.system('cfgmak')
        else:
            print bcolors.FAIL + "$SYSTELCFG is not defined" + bcolors.ENDC
            sys.exit(1)
        return
# ------------------------------------------------------------#
#
# Launch PHE
# ------------------------------------------------------------#
def launch_phe():
    if verbose:
        print bcolors.OKGREEN + "function    : launch_phe" + bcolors.ENDC
#    submit('%s %s' %(code,cas)) : pas possible a cause du planton dans partel
    os.system('%s %s' %(code,cas))
    return
# ------------------------------------------------------------#
#
# Wait partel... 
# ------------------------------------------------------------#
def wait_partel():
    if verbose:
        print bcolors.OKGREEN + "function    : wait_partel" + bcolors.ENDC
    encore = True
    while encore:        
        cmd = 'grep -i "normal termination" %s.o* '%jobname
        returnwait = subprocess.Popen(cmd,
                                      shell=True,
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.STDOUT)
        stdout = returnwait.communicate()
        if 'normal termination' in stdout[0]:
            print bcolors.HEADER + "Partel ok" + bcolors.ENDC
            encore = False
            break
        for item in errorbgp:
            cmd = 'grep -i "%s" %s.e*' %(item,jobname)
            returnwait = subprocess.Popen(cmd,
                                          shell=True,
                                          stdout=subprocess.PIPE,
                                          stderr=subprocess.STDOUT)
            stderr = returnwait.communicate()
            if item in stderr[0]:
                print bcolors.FAIL + "partel failure with return code:%s" %item + bcolors.ENDC
                sys.exit(1)
    return
# ------------------------------------------------------------#
#
# Create run.ll : file containing instruction for LoadLeveler
# ------------------------------------------------------------#
def create_run_LoadLeveler():
    if verbose:
        print bcolors.OKGREEN + "function    : create_run_LoadLeveler" + bcolors.ENDC
    loadfile = \
"""#@ shell       = /bin/ksh
#@ job_type    = BLUEGENE
#@ group       = TELEMAC
#@ class       = %(queue)s
#@ bg_connection = PREFER_TORUS
%(input_partel)s
#@ step_name = step_1
#@ job_name = PARTEL
#@ output      = $(job_name).o$(jobid)
#@ error       = $(job_name).e$(jobid)
#@ queue
#######################
#@ step_name = step_2
#@ dependency = ( step_1 == 0 )
#@ input = /dev/null
#@ job_type    = BLUEGENE
#@ group       = TELEMAC
#@ job_name    = %(jobname)s
#@ output      = $(job_name).o$(jobid)
#@ error       = $(job_name).e$(jobid)
#@ class       = %(queue)s
#@ bg_connection = PREFER_TORUS
#@queue
#######################
case $LOADL_STEP_NAME in
    step_1) echo "je partitionne en phase $LOADL_STEP_NAME"
	#step 1....
        ## Lancement du partitionnement
        %(launch_partel)s
	;;
    step_2) echo "Calcul en phase $LOADL_STEP_NAME"
	#step 2...
        mpirun -verbose 1 -label -mode VN -np %(ncsize)s -exe %(exe)s -cwd %(workdir)s
	;;
    *)  echo "Fin du calcul..."
        ;;
esac
"""
    run_file = open('run.ll', mode='w')
    casedir = cas + '_tmp'
    workdir = os.path.join(home_case,casedir)
    if runcode:
        exe='out_'+arch.hosttype[0]+'.exe'
    else:
        exe=mpiexe
    #
    # Traitement du cas scalaire
    if int(ncsize) > 1:
        launch_partel = 'mpirun -verbose 1 -label -mode SMP -np 1 -exe %s -cwd %s'%(home.partel,workdir)
        input_partel = '#@ input = %s'%os.path.join(workdir,'partel.par')
    else:
        launch_partel = 'echo "Calcul sequentiel : pas de partitionnement..." '
        input_partel= '####'
    #
    run_file.write(loadfile %{'jobname':jobname,'ncsize':ncsize,'queue':queue,'launch_partel':launch_partel,'exe':exe,'input_partel':input_partel,'workdir':workdir})
    run_file.close()
    return
# ------------------------------------------------------------#
#
# Soumission du 'file'
# ------------------------------------------------------------#
def submit(cmd):
    if verbose:
        print bcolors.OKGREEN + "function    : submit" + bcolors.ENDC
        #    os.system('llsubmit %s' %file)
    try:
        print bcolors.HEADER + "Soumission de la commande : %s..."%(cmd) + bcolors.ENDC
        returnsubmit = subprocess.call(cmd,shell=True)
        if returnsubmit:
            print bcolors.FAIL + "Cmd %s failure with return code:%s" %(cmd,returnsubmit) + bcolors.ENDC
            sys.exit(1)
    except OSError, message:
        print bcolors.FAIL + "Execution failed!\n %s" %message + bcolors.ENDC
        sys.exit(1)
    return

# ------------------------------------------------------------#
#
# Fonction : MAIN
# read variables from command line, one by one:
# ------------------------------------------------------------#
__author__="Fabien Decung"
__date__ ="$01-Jan-2011 00:00:00$"

if __name__ == "__main__":
    
    while len(sys.argv) > 1:
        option = sys.argv[1]
        del sys.argv[1]
        if option == '-job':
            # get the jobname
            jobname = sys.argv[1];
            del sys.argv[1]
        elif option == '-ncsize':
            # get the number of processors
            ncsize = sys.argv[1];
            del sys.argv[1]
        elif option == '-code':
            code = sys.argv[1];
            del sys.argv[1]
        elif option == '-cas':
            cas = sys.argv[1];
            del sys.argv[1]
        elif option == '-queue':
            queue = sys.argv[1];
            del sys.argv[1]
        elif option == '-wall':
            walltime = sys.argv[1];
            del sys.argv[1]
        elif option == '-exe':
            mpiexe = sys.argv[1];
            mpirun = True
            del sys.argv[1]
        elif option == '-verbose':
            # if then, print everything it could...
            verbose = True     
        elif option == '-help' or option == '-h' :
            # if then, printout the help page
            print bcolors.OKGREEN + "usage : python script.py [-h|prefix|source...]" + bcolors.ENDC
            print bcolors.OKGREEN + "  -h       : print this page" + bcolors.ENDC
            print bcolors.OKGREEN + "  -verbose : un peu plus causant" + bcolors.ENDC
            print '';
            sys.exit(1)        
        else:
            print bcolors.FAIL + "invalid option :%s "%option + bcolors.ENDC
            sys.exit(1)

    # -----------------------------------------# -----------------------------------------#
    # Some information...
    arch = machine()
    #
    # Preparation des etapes:
    ################################ SPLIT & MERGE SUR LES NOEUDS POUR L INSTANT
    if ncsize > 1:
        split = True
        split = False # Pas de split, tout se fait sur dans la sequence queue...
    if mpirun:
        split = False
        runcode = False
        merge = False
    #    if code != 'estel3d':
    #        merge = True
    ################################ PAS DE SPLIT & MERGE SUR LA FRONTALE POUR L INSTANT
    ## if ncsize > 1:
    ##     splitfront = True
    ##     if code != 'estel3d':
    ##         mergefront = False #True
    #    
    # -----------------------------------------# -----------------------------------------#
    #                            STEP 1 : Lancement du systeme TELEMAC
    #        
    print bcolors.HEADER + " --------- Lancement de TELEMAC...!"+ bcolors.ENDC
    home_case = os.getcwd()
    if runcode:
        launch_phe()
    home = perl(arch.hosttype[0])
    #
    # -----------------------------------------# -----------------------------------------#
    # Lancement de l etape "Decoupage du maillage"
    # -----------------------------------------# -----------------------------------------#
    if split:
        create_split_LoadLeveler()
        file = 'split.ll'
        cmd = 'llsubmit %s'%file    
    #    submit(cmd)
        # Wait
    #    wait_partel()
    if splitfront:
        file = 'partel.par'
        cmd = 'home.partelfront < %s'%file
    #    submit(cmd)
    # -----------------------------------------# -----------------------------------------#
    # Lancement du calcul (mpirun...)
    # -----------------------------------------# -----------------------------------------#
    create_run_LoadLeveler()
    file = 'run.ll'
    cmd = 'llsubmit %s'%file
    submit(cmd)
    # -----------------------------------------# -----------------------------------------#
    # Lancement de l etape "Agglomeration des resultats
    # -----------------------------------------# -----------------------------------------#
    if merge:    
        create_merge_LoadLeveler()
        file = 'merge.ll'
        cmd = 'llsubmit %s'%file    
    #    submit(cmd)
    if mergefront:
        file = 'gretel.par'
        cmd = 'home.gretelfront < %s'%file
    #    submit(cmd)

    # -----------------------------------------# -----------------------------------------#
    # Lancement du calcul
    # -----------------------------------------# -----------------------------------------#
    print bcolors.HEADER + "------------      Calcul sur BlueGene a ete lance    ----------" + bcolors.ENDC
    print bcolors.HEADER + "------------          Attention aux erreurs...       ------------"   + bcolors.ENDC
print bcolors.HEADER + "------------ Gretel en fin de calcul non fonctionnel ------------"   + bcolors.ENDC









######################## BROUILLON ##################################




# ------------------------------------------------------------#
#
# Create split.ll : file containing instruction for LoadLeveler
# ------------------------------------------------------------#
def create_split_LoadLeveler():
    if verbose:
        print bcolors.OKGREEN + "function    : create_split_LoadLeveler" + bcolors.ENDC
    loadfile = \
"""#@ shell       = /bin/ksh
#@ job_type    = BLUEGENE
#@ group       = TELEMAC
#@ job_name    = %(jobname)s
#@ output      = $(job_name).o$(jobid)
#@ error       = $(job_name).e$(jobid)
#@ class       = BGP64_1H
#@ bg_connection = PREFER_TORUS
#@ input       = %(workdir)s/partel.par
#@ queue
mpirun -verbose 1 -label -mode SMP -np 1 \\
-exe %(exe)s \\
-cwd %(workdir)s
"""
    split_file = open('split.ll', mode='w')
    casedir = cas + '_tmp'
    workdir = os.path.join(home_case,casedir)
    split_file.write(loadfile %{'jobname':jobname,'exe':home.partel,'workdir':workdir})
    split_file.close()
    return
# ------------------------------------------------------------#
#
# Create merge.ll : file containing instruction for LoadLeveler
# ------------------------------------------------------------#
def create_merge_LoadLeveler():
    if verbose:
        print bcolors.OKGREEN + "function    : create_merge_LoadLeveler" + bcolors.ENDC
    loadfile = \
"""#@ shell       = /bin/ksh
#@ job_type    = BLUEGENE
#@ group       = TELEMAC
#@ job_name    = %(jobname)s
#@ output      = $(job_name).o$(jobid)
#@ error       = $(job_name).e$(jobid)
#@ class       = BGP64_1H
#@ bg_connection = PREFER_TORUS
#@ input       = %(workdir)s/gretel.par
#@ queue
mpirun -verbose 1 -label -mode SMP -np 1 \\
-exe %(exe)s \\
-cwd %(workdir)s
"""
    merge_file = open('merge.ll', mode='w')
    casedir = cas + '_tmp'
    workdir = os.path.join(home_case,casedir)
    merge_file.write(loadfile %{'jobname':jobname,'exe':home.gretel,'workdir':workdir})
    merge_file.close()
    return
