"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from config import parseConfigFile, parseConfig_ValidateTELEMAC
from parserKeywords import scanDICO,getIOFilesSubmit,getKeyWord
from runcode import processCAS,processTMP,processLIT,processCONFIG,processPARALLEL,processExecutable,runCode,processECR,checkConsistency
from utils import getFileContent,removeDirectories
from os import path,chdir,sep,remove
import sys

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$11-Mar-2010 13:51:29$"

if __name__ == "__main__":
   debug = False

# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   for cfgname in parseConfigFile('').keys():
      cfgs = parseConfig_ValidateTELEMAC(cfgname)

      for cfg in cfgs:
         for mod in cfgs[cfg]['VALIDATION'].keys():
# ~~ Scans all CAS files to launch validation ~~~~~~~~~~~~~~~~~~~~~~
            print '\n\nConfiguration ' + cfg + ', Module '+ mod + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
            print '... reading module dictionary'
            dicoFile = path.join(path.join(cfgs[cfg]['MODULES'][mod]['path'],'lib'),mod+cfgs[cfg]['TELVER']+'.dico')
            frgb,dico = scanDICO(dicoFile)
            iFS,oFS = getIOFilesSubmit(frgb,dico)

            for casFile in cfgs[cfg]['VALIDATION'][mod]:

               cas,lang = processCAS(casFile,dicoFile,frgb)
               if not checkConsistency(cas,dico,frgb,cfgs[cfg]):
                  print '... inconsistent CAS file: ',casFile
                  continue

               print '... CAS file: ',casFile
               CASDir = path.dirname(casFile)
               TMPDir = processTMP(casFile)

               chdir(CASDir)
               if not processLIT(cas,iFS,TMPDir):
                  sys.exit()

               chdir(TMPDir)
               processCONFIG(lang)
               proc = processPARALLEL(cas,dico,frgb,TMPDir+sep)
               if proc > 1:
                  print '... sorry, parallel option not yet available'
                  sys.exit()

               # ~~ Names for the executable set
               #> names within TMPDir
               f90File = iFS['FICHIER FORTRAN'].split(';')[1]
               objFile = path.splitext(f90File)[0] + cfgs[cfg]['SYSTEM']['SFX_OBJ']
               #> default executable name
               exeFile = path.join(path.join(cfgs[cfg]['MODULES'][mod]['path'],cfg),mod+cfgs[cfg]['TELVER']+cfgs[cfg]['SYSTEM']['SFX_EXE'])
               #> user defined executable name
               useFile = exeFile
               value,defaut = getKeyWord('FICHIER FORTRAN',cas,dico,frgb)
               if value != []:
                  useFile = path.join(CASDir,path.splitext(value[0])[0]+cfgs[cfg]['SYSTEM']['SFX_EXE'])
                  if path.exists(useFile) and cfgs[cfg]['REBUILD'] > 0: remove(useFile)
               #> default command line compilation and linkage
               objCmd = getFileContent(path.join(path.join(cfgs[cfg]['MODULES'][mod]['path'],cfg),mod+cfgs[cfg]['TELVER']+'.cmdo'))[0]
               exeCmd = getFileContent(path.join(path.join(cfgs[cfg]['MODULES'][mod]['path'],cfg),mod+cfgs[cfg]['TELVER']+'.cmdx'))[0]
               # ~~ process Executable
               if not processExecutable(useFile,objFile,f90File,objCmd,exeCmd,CASDir):
                  sys.exit()

               if not runCode(useFile):
                  sys.exit()

               if not processECR(cas,oFS,CASDir):
                  sys.exit()

               chdir(CASDir) #; removeDirectories(TMPDir)

   sys.exit()