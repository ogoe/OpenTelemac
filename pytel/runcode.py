"""@brief
"""
"""@author Sebastien E. Bourban, Noemie Durand and Alain Weisgerber
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from subprocess import *
from os import path,mkdir,chdir,remove,system,sep,environ, waitpid
from utils import getFileContent,putFileContent,removeDirectories
from parserKeywords import scanCAS,scanDICO,getKeyWord,getIOFilesSubmit
from config import OptionParser,parseConfigFile,parseConfig_RunningTELEMAC
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
   print casFile

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
   #p = Popen(["exe"], stdout=PIPE, stderr=PIPE ,shell=True)
   #print p.communicate()[0]
   #failure = False
   #if p.communicate()[1] != '': failure = True
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

   """p = Popen(["dir"], stdout=PIPE, stderr=PIPE ,shell=True)
   print p.communicate()[0]
   p.stdout.read
   #sts = waitpid(p.pid, 0)
   #print sts
   ##p = Popen(["dir","*.py"], shell=False)
   ##sts = waitpid(p.pid, 0)
   system("C:\\opentelemac\\bin\\systall.bat")
   p = Popen(["C:\\opentelemac\\bin\\systall.bat"], stdout=PIPE, stderr=PIPE, shell=True)
   print p.communicate()
   #(child_stdin, child_stdout) = (p.stdin, p.stdout)
   #print child_stdin
   #print child_stdout

   sys.exit()"""
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
   if args == []:
      print '\nThe name of the module to run and one CAS file at least are required\n'
      sys.exit()
   codeName = args[0]
   casFiles = args[1:]
   for cfgname in parseConfigFile(options.configFile).keys():
      cfgs = parseConfig_RunningTELEMAC(cfgname)

      for cfg in cfgs.keys():
         cfgs[cfg].update({'TELCOD':codeName})
         if codeName not in cfgs[cfg]['MODULES']:
            print '\nThe code to run is not installed on this system : ' + codeName + '\n'
            sys.exit()
         for casFile in casFiles:
            casFile = path.realpath(casFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
            print '\n\nRunning ' + path.basename(casFile) + ' with '+ codeName + ' under ' + path.dirname(casFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
            print '... reading module dictionary'
            dicoFile = path.join(path.join(cfgs[cfg]['MODULES'][codeName]['path'],'lib'),codeName+cfgs[cfg]['TELVER']+'.dico')
            frgb,dico = scanDICO(dicoFile)
            iFS,oFS = getIOFilesSubmit(frgb,dico)
            cas,lang = processCAS(casFile,dicoFile,frgb)
            if not checkConsistency(cas,dico,frgb,cfgs[cfg]):
               print '... inconsistent CAS file: ',casFile
               continue

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
            if options.compileonly: cfgs[cfg]['REBUILD'] = 2
            #> names within TMPDir
            f90File = iFS['FICHIER FORTRAN'].split(';')[1]
            objFile = path.splitext(f90File)[0] + cfgs[cfg]['SYSTEM']['SFX_OBJ']
            #> default executable name
            exeFile = path.join(path.join(cfgs[cfg]['MODULES'][codeName]['path'],cfg),codeName+cfgs[cfg]['TELVER']+cfgs[cfg]['SYSTEM']['SFX_EXE'])
            #> user defined executable name
            useFile = exeFile
            value,defaut = getKeyWord('FICHIER FORTRAN',cas,dico,frgb)
            if value != []:
               useFile = path.join(CASDir,path.splitext(value[0])[0]+cfgs[cfg]['SYSTEM']['SFX_EXE'])
               if path.exists(useFile) and cfgs[cfg]['REBUILD'] > 0: remove(useFile)
            #> default command line compilation and linkage
            objCmd = getFileContent(path.join(path.join(cfgs[cfg]['MODULES'][codeName]['path'],cfg),codeName+cfgs[cfg]['TELVER']+'.cmdo'))[0]
            exeCmd = getFileContent(path.join(path.join(cfgs[cfg]['MODULES'][codeName]['path'],cfg),codeName+cfgs[cfg]['TELVER']+'.cmdx'))[0]
            # ~~ process Executable
            if not processExecutable(useFile,objFile,f90File,objCmd,exeCmd,CASDir):
               sys.exit()

            if not options.compileonly:

               if not runCode(useFile):
                  sys.exit()

               if not processECR(cas,oFS,CASDir):
                  sys.exit()

            chdir(CASDir)
            if options.tmpdirectory or options.compileonly: removeDirectories(TMPDir)

   sys.exit()
