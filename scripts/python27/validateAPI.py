#!/usr/bin/env python

from TelApy.api.t2d import Telemac2d
import numpy as np
import os
import sys
import glob
import shutil
import argparse
from os import path, sep, walk, chdir, remove, environ, mkdir, \
               listdir, getcwd
from config import parseConfigFile
# ~~> dependencies towards other pytel/modules
from utils.messages import MESSAGES, filterMessage, banner
from parsers.parserKeywords import scanCAS,readCAS,getKeyWord,setKeyValue,scanDICO,getIOFilesSubmit
from utils.files import getFileContent
from mpi4py import MPI

def main(config_name, config_file, root_dir, validation_directory,example,clean):
    """
       Main function

       :param: config_name Name of the telemac configuration
       :param: config_file Name of the configuration file
       :param: root_dir Path to the root folder of telemac
       :param: validation_directory name of the example repository to validate
       :param: example case to compute
       :param: clean keyword to know if or not the tmp directory are remove after validation
    """

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # path to the root
    hometel = ''
    if 'HOMETEL' in environ:
        hometel = environ['HOMETEL']
    if root_dir == '':
        root_dir = hometel
    # user configuration name
    usetelcfg = ''
    if 'USETELCFG' in environ:
        usetelcfg = environ['USETELCFG']
    if config_name == '':
        config_name = usetelcfg
    # user configuration file
    systelcfg = path.join(hometel, 'configs')
    if 'SYSTELCFG' in environ:
        systelcfg = environ['SYSTELCFG']
    if config_file != '':
        systelcfg = config_file
    if path.isdir(systelcfg):
        systelcfg = path.join(systelcfg, 'systel.cfg')
    config_file = systelcfg

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
    if not path.isfile(config_file):
        print '\nNot able to get to the configuration file: '\
              + config_file + '\n'
        dircfg = path.abspath(path.dirname(config_file))
        if path.isdir(dircfg):
            print ' ... in directory: ' + dircfg + '\n ... use instead: '
            _, _, filenames = walk(dircfg).next()
            for fle in filenames:
                _, tail = path.splitext(fle)
                if tail == '.cfg':
                    print '    +> ', fle
        raise Exception('Error in configuration file')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xcptss = MESSAGES()
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
    cfgs = parseConfigFile(config_file, config_name)

    for cfgname in cfgs:
        # still in lower case
        if not cfgs[cfgname].has_key('root'):
            cfgs[cfgname]['root'] = root_dir
        # parsing for proper naming
        #cfg = parseConfig_CompileTELEMAC(cfgs[cfgname])
        print '\n\n'+'\n'.join(banner(cfgname))
    return xcptss
    
def PretreatmentTest(TestCasePath):
    if not path.exists(TestCasePath +sep+'tmp'):
        mkdir(pathtotestcase +sep+'tmp')
    chdir(pathtotestcase +sep+'tmp')

    dicoFile = path.join(environ['HOMETEL'],'sources', ARGS.validationFolder,ARGS.validationFolder+'.dico')
    print '\n... reading the main module dictionary'
    frgb,dico = scanDICO(dicoFile)

    iFS,oFS = getIOFilesSubmit(frgb,dico)

    casFilesPath = glob.glob(pathtotestcase +sep+'*.cas')
    list_file = [] 
    for casFile in casFilesPath:
        shutil.copyfile(casFile,path.basename(casFile))
        cas = readCAS(scanCAS(getFileContent(casFile)),dico,frgb)
        user_fortran = None
        for key in iFS:
            print '~~~~~~~~~~>',key
            value,defaut = getKeyWord(key,cas,dico,frgb)
            if value != []:
                ffile=value[0].strip("'")
                shutil.copyfile(pathtotestcase+sep+ffile,ffile)
                if 'FORTRAN' in key:
                    user_fortran = ffile
        list_file.append((path.basename(casFile),user_fortran))
    return list_file

def partel_python(t2d,ncsize,casFile):
    dicoFile = path.join(environ['HOMETEL'],'sources', ARGS.validationFolder,ARGS.validationFolder+'.dico')
    print '\n... reading the main module dictionary'
    frgb,dico = scanDICO(dicoFile)
    iFS,_ = getIOFilesSubmit(frgb,dico)
    cas = readCAS(scanCAS(getFileContent(casFile)),dico,frgb)
    value,defaut = getKeyWord('BOUNDARY CONDITIONS FILE',cas,dico,frgb)
    cli_file = value[0].strip("'")
    for key in iFS:
        value,defaut = getKeyWord(key,cas,dico,frgb)
        if value != []:
            ffile=value[0].strip("'")
            if iFS[key].split(';')[5][0:7] == 'SELAFIN':  
                ierr = t2d.api_inter.run_partel(t2d.my_id,ffile,cli_file,ncsize,1,'SERAFIN ',' ',' ',' ')

def RunTelemac(t2d):
##    Telemac Steering file reading
##        self.t2d = Telemac2d(self.casFile,user_fortran =self.UserForFile[1:len(self.UserForFile)-1])
    t2d.set_case()
    t2d.init_state_default()
    t2d.run_all_time_steps()        
    t2d.finalize()

def gretel_python(t2d,ncsize,casFile):
    dicoFile = path.join(environ['HOMETEL'],'sources', ARGS.validationFolder,ARGS.validationFolder+'.dico')
    print '\n... reading the main module dictionary'
    frgb,dico = scanDICO(dicoFile)
    _,oFS = getIOFilesSubmit(frgb,dico)
    cas = readCAS(scanCAS(getFileContent(casFile)),dico,frgb)
    value,defaut = getKeyWord('GEOMETRY FILE',cas,dico,frgb)
    geo_file = value[0].strip("'")
    for key in oFS:
        value,defaut = getKeyWord(key,cas,dico,frgb)
        if value != []:
            ffile=value[0].strip("'")
            if oFS[key].split(';')[5][0:7] == 'SELAFIN':  
                ierr = t2d.api_inter.run_gretel(t2d.my_id,geo_file,'SERAFIN ',ffile,'SERAFIN ',ncsize,0)

def ResFilePost(t2d,comm,GeoFile,ResFile):
    if ( comm.Get_size() > 1):
        print ResFile,ResFile[1:len(ResFile)-1]
        ierr = t2d.api_inter.run_gretel(t2d.my_id,GeoFile,'SERAFIN ',ResFile,'SERAFIN ',comm.Get_size(),0)

if __name__ == "__main__":
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()
    ncsize = comm.Get_size()
    if rank==0:
        print '@@@@Nb_proc==>',ncsize
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
        PARSER = argparse.ArgumentParser(\
            description='Make the validation of Telemac-Mascaret API '\
                'and/or executable using the API')
        PARSER.add_argument(\
            "-c", "--configname",
            dest='configName',
            default="",
            help="specify configuration name, default is randomly \
                    found in the configuration file")
        PARSER.add_argument(\
            "-f", "--configfile",
            dest='configFile',
            default="",
            help="specify configuration file, default is systel.cfg")
        PARSER.add_argument(\
            "-r", "--rootdir",
            dest='rootDir',
            default="",
            help="specify the root, default is taken from config file")
        PARSER.add_argument(\
            "-v", "--valdir",
            dest='validationFolder',
            default="telemac2d",
            help="specify the folder to validate")
        PARSER.add_argument(\
            "-e", "--example",
        dest='example',
            default="gouttedo",
            help="specify the name of the test case to compute")
        PARSER.add_argument(\
            "-cl", "--clean",
            dest='clean',
            default="yes",
            help="specify if or not the Tmp folder must be clean")
        ARGS = PARSER.parse_args()

# Running main function
        XCPTS = main(ARGS.configName, ARGS.configFile, ARGS.rootDir,
                     ARGS.validationFolder,ARGS.example,ARGS.clean)
    else:
        ARGS = None
        XCPTS = None

    XCPTS =comm.bcast(XCPTS, root=0)
    ARGS = comm.bcast(ARGS, root=0)
    ARGS.configName =comm.bcast(ARGS.configName, root=0)
    ARGS.configFile =comm.bcast(ARGS.configFile, root=0)
    ARGS.rootDir =comm.bcast(ARGS.rootDir, root=0)
    ARGS.validationFolder =comm.bcast(ARGS.validationFolder, root=0)
    ARGS.example =comm.bcast(ARGS.example, root=0)
    ARGS.clean =comm.bcast(ARGS.clean, root=0)
    
    if ARGS.validationFolder == 'telemac2d':
        if ARGS.example != '':
            pathtotestcase = path.join(environ['HOMETEL'], 'examples', ARGS.validationFolder,ARGS.example)
            if rank==0:
                list_file=PretreatmentTest(pathtotestcase)
            else:
                list_file = None
            comm.Barrier()
            chdir(pathtotestcase +sep+'tmp')
            list_file=comm.bcast(list_file, root=0)
            print 'my rank=',comm.Get_rank()
            print 'je suis ici',getcwd()
            for cas,fortran in list_file:
                print '***********',cas,fortran,'*****************'
                t2d = Telemac2d(cas,user_fortran=fortran,comm=comm)
                comm.Barrier()
                if comm.Get_size()>1:
                    partel_python(t2d,comm.Get_size(),cas)
                comm.Barrier()
                RunTelemac(t2d)
                comm.Barrier()
                if comm.Get_size()>1:
                    gretel_python(t2d,comm.Get_size(),cas)
                del(t2d)
