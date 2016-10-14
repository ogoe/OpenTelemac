#!/usr/bin/env python
"""@author Y. Audouin
"""
"""@note ... this work is based on a collaborative effort between
  .________.                                                          ,--.
  |        |                                                      .  (  (
  |,-.    /   HR Wallingford                EDF - LNHE           / \_ \_/ .--.
  /   \  /    Howbery Park,                 6, quai Watier       \   )   /_   )
   ,.  `'     Wallingford, Oxfordshire      78401 Cedex           `-'_  __ `--
  /  \   /    OX10 8BA, United Kingdom      Chatou, France        __/ \ \ `.
 /    `-'|    www.hrwallingford.com         innovation.edf.com   |    )  )  )
!________!                                                        `--'   `--
@brief Run the partiotionning step
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
import re
import sys
import time
import shutil
from subprocess import call
from os import path, sep, walk, chdir, remove, environ, mkdir, system, symlink
import argparse
# ~~> dependencies towards the root of pytel
from config import parseConfigFile,parseConfig_RunningTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.messages import MESSAGES,filterMessage,banner
from utils.progressbar import ProgressBar
from runcode import runPARTEL, getPartelCmd

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
   parser = argparse.ArgumentParser(description='Compile Telemac-Mascaret API '\
                                                'and/or executable using the API')
   parser.add_argument("-c","--configname",
             dest='configName',
             default="",
             help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_argument("-f","--configfile",
             dest='configFile',
             default="",
             help="specify configuration file, default is systel.cfg" )
   parser.add_argument("-r","--rootdir",
             dest='rootDir',
             default="",
             help="specify the root, default is taken from config file" )
   parser.add_argument("--input-file",
             dest="inputFile",
             default='',
             help="Name of partel parameter file. This option will surcharge all the others" )
   parser.add_argument("--file",
             dest="geoFile",
             default='T2DGEO',
             help="Name of the file to be partitionned" )
   parser.add_argument("--file-format",
             dest="geoFileFmt",
             default='SERAFIN',
             help="Format of the geometry file(SERAFIN,SERAFIND or MED), default is SERAFIN" )
   parser.add_argument("--bnd-file",
             dest="bndFile",
             default='T2DCLI',
             help="Name of the boundary file associated to the mesh file, default is T2DCLI" )
   parser.add_argument("--ncsize",
             dest="ncsize",
             default=8,
             help="Number of partitions (should be equal to number of parallel processors), "\
                  "default is 8" )
   parser.add_argument("--section-name",
             dest="sectionFile",
             default='',
             help="Name of the section file, default no section file" )
   parser.add_argument("--zone-name",
             dest="zoneFile",
             default='',
             help="Name of the zone file, default no zone file" )
   parser.add_argument("--weir-name",
             dest="weirFile",
             default='',
             help="Name of the weir file, default no weir file" )
   parser.add_argument("--partitioning-method",
             dest="partitionningMethod",
             default=1,
             help="Method used for the partitionning (1:metis, 2:scotch)" )
   args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # path to the root
   HOMETEL = ''
   if 'HOMETEL' in environ: HOMETEL = environ['HOMETEL']
   if args.rootDir == '': args.rootDir = HOMETEL
   # user configuration name
   USETELCFG = ''
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   if args.configName == '': args.configName = USETELCFG
   # user configuration file
   SYSTELCFG = path.join(HOMETEL,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if args.configFile != '': SYSTELCFG = args.configFile
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   args.configFile = SYSTELCFG

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   if not path.isfile(args.configFile):
      print '\nNot able to get to the configuration file: ' + args.configFile + '\n'
      dircfg = path.abspath(path.dirname(args.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         _, _, filenames = walk(dircfg).next()
         for fle in filenames :
            head,tail = path.splitext(fle)
            if tail == '.cfg' :
               print '    +> ',fle
      raise Exception('Error in configuration file')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(args.configFile,args.configName)
   if not cfgs[args.configName].has_key('root'):
      cfgs[args.configName]['root'] = HOMETEL
   if args.rootDir != '':
      cfgs[args.configName]['root'] = path.abspath(args.rootDir)
   cfg = parseConfig_RunningTELEMAC(cfgs[args.configName])
   if (args.inputFile != ""):
      with open(args.inputFile,'r') as f:
         geoFile = f.readline().strip('\n')
         geoFileFmt = f.readline().strip('\n')
         bndFile = f.readline().strip('\n')
         ncsize = f.readline().strip('\n')
         partitionningMethod = f.readline().strip('\n')
         sectionFile = f.readline().strip('\n')
         zoneFile = f.readline().strip('\n')
         weirFile = f.readline().strip('\n')
   else:
      geoFile = args.geoFile
      geoFileFmt = args.geoFileFmt
      bndFile = args.bndFile
      ncsize = args.ncsize
      sectionFile = args.sectionFile
      zoneFile = args.zoneFile
      weirFile = args.weirFile
      partitionningMethod = args.partitionningMethod

   # Getting partel command from configuration
   pbin = path.join(args.rootDir,'builds',args.configName,'bin')
   parcmd = getPartelCmd(pbin,cfg,{'mpi':''})
   # Running paritionning

   runPARTEL(parcmd,geoFile,geoFileFmt,bndFile,ncsize,False,
             sectionFile,zoneFile,weirFile,
             geoFile,geoFileFmt,partitionningMethod)

   print '\n\nMy work is done\n\n'
   sys.exit(0)
