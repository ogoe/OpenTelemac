"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from config import OptionParser,parseConfigFile, parseConfig_CompactTELEMAC
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
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   CFGNAME = ''
   SYSTELCFG = 'systel.cfg'
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configname",
                      type="string",
                      dest="configName",
                      default=CFGNAME,
                      help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_option("-f", "--configfile",
                      type="string",
                      dest="configFile",
                      default=SYSTELCFG,
                      help="specify configuration file, default is systel.cfg" )
   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   if options.configName != '':
      cfgnames = [options.configName]
   else:
      cfgnames = parseConfigFile(options.configFile).keys()

   for cfgname in cfgnames:
      cfg = parseConfig_CompactTELEMAC(cfgname)[cfgname]

# ~~ Scans all source files to build a relation database ~~~~~~~~~~~
      print '\n\nConfiguration ' + cfgname + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

      pt = cfg['TELDIR']
      pc = path.join(pt,cfgname)
      dirs = ['sources','lib',cfgname]
      for mod in cfg['MODULES'].keys():
         print '... now extracting ' + mod
         pi = cfg['MODULES'][mod]['path']
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

      print '\n... now zipping ' + cfgname
      zip(cfgname,pc,cfg['ZIPPER'])

      print '\n... now cleaning '
      removeDirectories(pc)
         
      #print '... now publishing ' ... Hudson does this

   sys.exit()