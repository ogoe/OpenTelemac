#!/usr/bin/env python
"""@author Sebastien E. Bourban and Noemie Durand
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
"""
"""@history 17/04/2011 -- Sebastien E. Bourban
         Updated to the latest runcode, which includes POSTEL and COUPLAGE
"""
"""@history 28/04/2011 -- Sebastien E. Bourban
         Now supports SYSTELCFG as a directory (old Perl version, to which
         systel.cfg is added) or as a file.
"""
"""@history 30/04/2011 -- Sebastien E. Bourban
         Upgrade made to config parsing to include the option to reset
         the version and the root from the command line option:
         -v <version>, reset the version read in the config file with this
         -r <root>, reset the root path read in the config file with this
"""
"""@history 05/07/2011 -- Sebastien Bourban
         Python interpreter added for linux calls. This is a temporary
         solution as "/usr/bin/env" is not strickly portable cross
         operating systems
"""
"""@history 21/08/2011 -- David Roscoe and Sebastien Bourban
         Addition of a program of validation in the form of a local XML
         file. This XML file sets the validation instructions for every
         test cases.
"""
"""@history 27/01/2012 -- Sebastien E. Bourban
         A new option (--modules) added to the command line, which if present
         will reset the value of the key in the configuration file.
         This development was triggered by Christophe Coulet (Artelia-Sogreah)
         who asked about it on the open TELEMAC forum.
"""
"""@history 19/03/2012 -- Sebastien E. Bourban
         A new option (--screen) added to the command line, in order for
         Jenkins to use any Xwindows backend on its virtual boxes.
"""
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path,walk,environ
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
# ~~> dependencies towards other pytel/modules
from parsers.parserXML import runXML

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
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
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configname",type="string",dest="configName",default=CFGNAME,
      help="specify configuration name, default is the first found in the configuration file" )
   parser.add_option("-f", "--configfile",type="string",dest="configFile",default=SYSTELCFG,
      help="specify configuration file, default is systel.cfg" )
   parser.add_option("-r", "--rootdir",type="string",dest="rootDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_option("-v", "--version",type="string",dest="version",default='',
      help="specify the version number, default is taken from config file" )
   #parser.add_option("-s", "--sortiefile",action="store_true",dest="sortieFile",default=False,
   #   help="specify whether there is a sortie file, default is no" )
   parser.add_option("-a", "--action",type="string",dest="do",default='',
      help="filter specific process actions from the XML file" )
   parser.add_option("-d", "--draw",type="string",dest="draw",default='',
      help="filter specific drawing actions from the XML file" )
   parser.add_option("-m", "--modules",type="string",dest="modules",default='',
      help="specify the list modules, default is taken from config file" )
   parser.add_option("-s", "--screen",action="store_true",dest="display",default=False,
      help="specify whether to display on screen or save silently" )
   options, args = parser.parse_args()
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

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Forces not to use any Xwindows backend for Jenkins ~~~~~~~~~~
   if not options.display:
      import matplotlib.pyplot as plt
      plt.switch_backend('agg')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Turning XML / config loops inside out ~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nScanning XML files and configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   xmls = {}
   for cfgname in cfgnames:
      # still in lower case
      if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
      if options.version != '': cfgs[cfgname]['version'] = options.version
      if options.modules != '': cfgs[cfgname]['modules'] = options.modules
      cfgs[cfgname]['display'] = options.display
      # parsing for proper naming
      cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
      cfg.update({ 'PWD':PWD })

      for codeName in cfg['VALIDATION'].keys():
         xmlKeys = cfg['VALIDATION'][codeName]
         if not xmls.has_key(codeName): xmls.update({codeName:{}})
         for key in xmlKeys.keys():
            if key != 'path':
               if not xmls[codeName].has_key(key): xmls[codeName].update({key:{}})
               xmlDir = path.join(xmlKeys['path'],key)
               for xmlFile in xmlKeys[key]:
                  xmlPath = path.join(xmlDir,xmlFile)
                  if not xmls[codeName][key].has_key(xmlPath): xmls[codeName][key].update({xmlPath:{}})
                  xmls[codeName][key][xmlPath].update({cfgname: { 'cfg':cfg, 'options':options } })
   # ~~> Print summary
   for codeName in xmls.keys():
      print '    +> ',codeName
      for key in xmls[codeName]:
         print '    |    +> ',key
         for xmlFile in xmls[codeName][key]:
            print '    |    |    +> ',path.basename(xmlFile),xmls[codeName][key][xmlFile].keys()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for codeName in xmls.keys():
      for key in xmls[codeName]:
         print '\n\nValidation of ' + key + ' of module ' + codeName + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         for xmlFile in xmls[codeName][key]:
            runXML(xmlFile,xmls[codeName][key][xmlFile])

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
