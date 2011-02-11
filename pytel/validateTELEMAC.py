"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
from parserKeywords import scanDICO,getIOFilesSubmit
from runcode import processCAS,checkConsistency,runCAS
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
   SYSTELCFG = 'systel.cfg'
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configfile",
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
# ~~~~ Loop over configurations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for cfgname in parseConfigFile(options.configFile).keys():
      cfgs = parseConfig_ValidateTELEMAC(cfgname)

      for cfg in cfgs.keys():
         for mod in cfgs[cfg]['VALIDATION'].keys():
# ~~ Scans all CAS files to launch validation ~~~~~~~~~~~~~~~~~~~~~~
            print '\n\nConfiguration ' + cfg + ', Module '+ mod + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
            print '... reading module dictionary'
            dicoFile = path.join(path.join(cfgs[cfg]['MODULES'][mod]['path'],'lib'),mod+cfgs[cfg]['TELVER']+'.dico')
            frgb,dico = scanDICO(dicoFile)
            iFS,oFS = getIOFilesSubmit(frgb,dico)

            for casFile in cfgs[cfg]['VALIDATION'][mod]:

               cas,lang = processCAS(casFile,frgb)
               if not checkConsistency(cas,dico,frgb,cfgs[cfg]):
                  print '... inconsistent CAS file: ',casFile
                  continue

               runCAS(cfg,cfgs[cfg],mod,casFile,dico,frgb,iFS,oFS,options)

   sys.exit()