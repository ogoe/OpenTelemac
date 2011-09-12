#!/usr/bin/env python
"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
"""@history 17/04/2011 -- Sebastien Bourban: Updated to the latest runcode,
      which includes POSTEL and COUPLAGE
"""
"""@history 28/04/2011 -- Sebastien Bourban: Now supports SYSTELCFG
         as a directory (old Perl version, to which systel.cfg is added)
         or as a file.
"""
"""@history 30/04/2011 -- Sebastien Bourban: Upgrade made to config parsing
         to include the option to reset the version and the root from the
         command line option:
         -v <version>, reset the version read in the config file with this
         -r <root>, reset the root path read in the config file with this
"""
"""@history 05/07/2011 -- Sebastien Bourban: python interpreter added for
         linux calls. This is a temporary solution as "/usr/bin/env" is not
         strickly portable cross operating systems
"""
"""@history 21/08/2011 -- David Roscoe and Sebastien Bourban: addition of
         a program of validation in the form of a local XML file. This
         XML file sets the validation instructions for every test cases.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
from parserXML import runXML
from os import path,walk,environ
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
   PWD = path.dirname(sys.argv[0])
   SYSTELCFG = path.join(path.dirname(PWD),'config')
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   oparser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   oparser.add_option("-c", "--configname",
                      type="string",
                      dest="configName",
                      default=CFGNAME,
                      help="specify configuration name, default is the first found in the configuration file" )
   oparser.add_option("-f", "--configfile",
                      type="string",
                      dest="configFile",
                      default=SYSTELCFG,
                      help="specify configuration file, default is systel.cfg" )
   oparser.add_option("-r", "--rootdir",
                      type="string",
                      dest="rootDir",
                      default=path.dirname(PWD),
                      help="specify the root, default is taken from config file" )
   oparser.add_option("-v", "--version",
                      type="string",
                      dest="version",
                      default='',
                      help="specify the version number, default is taken from config file" )
   oparser.add_option("-s", "--sortiefile",
                      action="store_true",
                      dest="sortieFile",
                      default=False,
                      help="specify whether there is a sortie file, default is no" )
   oparser.add_option("-p", "--process",
                      type="string",
                      dest="process",
                      default='',
                      help="filter specific process actions from the XML file" )
   oparser.add_option("-d", "--draw",
                      type="string",
                      dest="draw",
                      default='',
                      help="filter specific drawing actions from the XML file" )
   options, args = oparser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         for dirpath,dirnames,filenames in walk(dircfg) : break
         for file in filenames :
            head,tail = path.splitext(file)
            if tail == '.cfg' : print '    +> ',file
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile)
   cfgnames = cfgs.keys()
   if options.configName != '':
      if options.configName not in cfgnames:
         print '\nNot able to find your configuration in the configuration file: ' + options.configFile + '\n'
         print ' ... use instead:'
         for cfgname in cfgnames : print '    +> ',cfgname
         sys.exit()
      cfgnames = [options.configName]

   for cfgname in cfgnames:
      # still in lower case
      if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
      if options.version != '': cfgs[cfgname]['version'] = options.version
      # parsing for proper naming
      cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
      cfg.update({ 'PWD':PWD })

      for codeName in cfg['VALIDATION'].keys():
# ~~ Scans all XML files to launch validation ~~~~~~~~~~~~~~~~~~~~~~
         print '\n\nConfiguration ' + cfgname + ', Module '+ codeName + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

         xmlKeys = cfg['VALIDATION'][codeName]
         for key in xmlKeys.keys():
            if key != 'path':
               xmlDir = path.join(xmlKeys['path'],key)
               for xmlFile in xmlKeys[key]:
                  print '\n\nRunning ' + xmlFile + ' under ' + path.dirname(xmlDir) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Run the Code from the CAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  runXML(cfgname,cfg,codeName,path.join(xmlDir,xmlFile),options)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
