"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
"""@history 17/04/2011 -- Sebastien Bourban: Updated to the latest runcode,
      which includes POSTEL and COUPLAGE
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
from runcode import runCAS
from os import path,environ
import sys

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$11-Mar-2010 13:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
                      help="specify configuration name, default is the first found in the configuration file" )
   parser.add_option("-f", "--configfile",
                      type="string",
                      dest="configFile",
                      default=SYSTELCFG,
                      help="specify configuration file, default is systel.cfg" )
   parser.add_option("-s", "--sortiefile",
                      action="store_true",
                      dest="sortieFile",
                      default=False,
                      help="specify whether there is a sortie file, default is no" )
   parser.add_option("-t", "--tmpdirectory",
                      action="store_false",
                      dest="tmpdirectory",
                      default=True,
                      help="specify whether the temporary directory is removed, default is yes" )
   parser.add_option("-x", "--compileonly",
                      action="store_true",
                      dest="compileonly",
                      default=False,
                      help="specify whether to only create an executable but not run, default is no" )
   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   if options.configName != '':
      if options.configName not in parseConfigFile(options.configFile).keys():
         print '\nNot able to find your configuration in the configuration file: ' + options.configFile + '\n'
         sys.exit()
      cfgnames = [options.configName]
   else:
      cfgnames = parseConfigFile(options.configFile).keys()

   for cfgname in cfgnames:

      cfg = parseConfig_ValidateTELEMAC(cfgname)[cfgname]

      for codeName in cfg['VALIDATION'].keys():
# ~~ Scans all CAS files to launch validation ~~~~~~~~~~~~~~~~~~~~~~
         print '\n\nConfiguration ' + cfgname + ', Module '+ codeName + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

         for casFile in cfg['VALIDATION'][codeName]:
            casFile = path.realpath(casFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
            print '\n\nRunning ' + path.basename(casFile) + ' with '+ codeName + ' under ' + path.dirname(casFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
            print '... reading module dictionary'

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Run the Code from the CAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            runCAS(cfgname,cfg,codeName,casFile,options)

   sys.exit()