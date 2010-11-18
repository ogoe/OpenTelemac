"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from config import parseConfigFile, parseConfig_CompactTELEMAC
from os import path
from utils import createDirectories,removeDirectories,zip,copyFiles
import sys

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   debug = False

# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   for cfgname in parseConfigFile('').keys():
      cfgs = parseConfig_CompactTELEMAC(cfgname)

      for cfg in cfgs:
# ~~ Scans all source files to build a relation database ~~~~~~~~~~~
         print '\n\nConfiguration' + cfg + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

         pt = cfgs[cfg]['TELDIR']
         pc = path.join(pt,cfg)
         dirs = ['sources','lib',cfg]
         for mod in cfgs[cfg]['MODULES'].keys():
            print '... now extracting ' + mod
            pi = cfgs[cfg]['MODULES'][mod]['path']
            for d in dirs:
               pid = path.join(pi,d)
               if path.exists(pid) :
                  po = pid.replace(pt,pc)
                  createDirectories(po)
                  copyFiles(pid,po)
         dirs = ['bin','config','pytel']
         for d in dirs:
            pid = path.join(pt,d)
            if path.exists(pid) :
               po = pid.replace(pt,pc)
               createDirectories(po)
               copyFiles(pid,po)

         print '\n... now zipping ' + cfg
         zip(cfg,pc,cfgs[cfg]['ZIPPER'])

         print '\n... now cleaning '
         removeDirectories(pc)
         
         #print '... now publishing ' ... Hudson does this

   sys.exit()