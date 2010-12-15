"""@brief
"""
"""@author Sebastien E. Bourban, Noemie Durand and Alain Weisgerber
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from os import path,walk,mkdir,getcwd,chdir,remove,rmdir,listdir,system
from utils import getFileContent,putFileContent
from parserKeywords import scanCAS,getKeyWord
import sys
import shutil
from time import gmtime, strftime

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def checkConsistency(cas,dico,frgb,cfg):

   # ~~ check for parallel consistency ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   value,defaut = getKeyWord('PROCESSEURS PARALLELES',cas,dico,frgb)
   proc = 0
   if value != []: proc = int(value[0])
   elif defaut != []: proc = int(defaut[0])
   if proc > 1 and 'parallel' not in cfg['MODULES'].keys(): return False
   if proc < 2 and 'paravoid' not in cfg['MODULES'].keys(): return False

   # ~~ check for openmi consistency ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   return True

def processCAS(casFile,dicoFile,frgb):

   # ~~ extract keywords ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cas = scanCAS(casFile)
   sys.exit()


   # ~~ check language on one of the input file names ~~~~~~~~~~~~~~
   lang = 1
   if cas.keys()[0] not in frgb.keys(): lang = 2

   # ~~ check language on one of the input file names ~~~~~~~~~~~~~~
   if lang == 1:
      print '... simulation en Francais'
      cas.update({'FICHIER DES PARAMETRES':[casFile]})
      cas.update({'DICTIONNAIRE':[dicoFile]})
   if lang == 2:
      print '... running in English'
      cas.update({'STEERING FILE':[casFile]})
      cas.update({'DICTIONARY':[dicoFile]})

   return cas,lang

def processTMP(casFile):

   # ~~ TMP Directory ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   TMPDir = casFile + '_' + strftime("%Y-%m-%d-%Hh%Mmin%Ss", gmtime())
   mkdir(TMPDir)

   return TMPDir

def processLIT(cas,iFiles,TMPDir):

   # ~~ copy input files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k in cas.keys():
      if iFiles.has_key(k):
         cref = cas[k][0]
         if not path.isfile(cref):
            print '... file does not exist ',cref
            return False
         crun = path.join(TMPDir,iFiles[k].split(';')[1])
         if iFiles[k].split(';')[3] == 'ASC':
            putFileContent(crun,getFileContent(cref))
         else:
            shutil.copy(cref,crun)

   return True

def processECR(cas,oFiles,CASDir):

   # ~~ copy output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k in cas.keys():
      if oFiles.has_key(k):
         cref = path.join(CASDir,cas[k][0])
         if path.isfile(cref): shutil.copy(cref,cref+'.old')
         crun = oFiles[k].split(';')[1]
         if not path.isfile(crun):
            print '... did not create outfile ',cref,' (',crun,')'
            return False
         shutil.copy(crun,cref)

   return True

def processCONFIG(lang):

   # ~~ create CONFIG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('CONFIG',[str(lang),'6'])
   return True

def processPARALLEL(cas,dico,frgb,wdir):

   # ~~ check keyword ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   value,defaut = getKeyWord('PROCESSEURS PARALLELES',cas,dico,frgb)
   proc = -1
   if value != []: proc = int(value[0])
   elif defaut != []: proc = int(defaut[0])

   # ~~ parallel case ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if proc >= 0:
      putFileContent('PARAL',[str(proc),str(len(wdir)),wdir])
   elif proc > 1:
      putFileContent('PARAL',[str(proc),str(len(wdir)),wdir])

   return proc

def processExecutable(useName,objName,f90Name,objCmd,exeCmd,CASDir):

   if path.exists(f90Name) and not path.exists(useName):
   # ~~ requires compilation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      objCmd = objCmd.replace('<f95name>',f90Name)
      print objCmd
      failure = system(objCmd)
      if failure:
         '... could not compile your FORTRAN. Please verify your code.'
         return False
      exeCmd = exeCmd.replace('<objs>',objName)
      exeCmd = exeCmd.replace('<exename>',path.basename(useName))
      print exeCmd
      failure = system(exeCmd)
      if failure:
         '... could not create the Executable. Please look for missing libraries.'
         return False
   
   else:
   # ~~ default executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shutil.copy(useName,path.basename(useName))

   # ~~ save a copy for future uses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   shutil.copy(path.basename(useName),path.join(CASDir,path.basename(useName)))

   return True

def runCode(exe):

   failure = system(exe)
   if not failure:
      return True
   else: return False

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   debug = False

   sys.exit()
