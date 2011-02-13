"""@brief
"""
"""@author Sebastien E. Bourban, Noemie Durand and Alain Weisgerber
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from subprocess import *
from os import path,mkdir,chdir,remove,system,sep,environ
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
   ncsize = 0
   if value != []: ncsize = int(value[0])
   elif defaut != []: ncsize = int(defaut[0])
   if ncsize > 1 and 'parallel' not in cfg['MODULES'].keys(): return False
   if ncsize < 2 and 'paravoid' not in cfg['MODULES'].keys(): return False  # /!\ you might want to be more relaxed about this

   # ~~ check for openmi consistency ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   return True

def processCAS(casFile,frgb):

   # ~~ extract keywords ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cas = scanCAS(casFile)
   print casFile

   # ~~ check language on one of the input file names ~~~~~~~~~~~~~~
   lang = 1
   if cas.keys()[0] not in frgb['FR'].keys(): lang = 2

   # ~~ check language on one of the input file names ~~~~~~~~~~~~~~
   if lang == 1:
      print '... simulation en Francais'
      cas.update({'FICHIER DES PARAMETRES':[casFile]})
      cas.update({'DICTIONNAIRE':[frgb['DICO']]})
   if lang == 2:
      print '... running in English'
      cas.update({'STEERING FILE':[casFile]})
      cas.update({'DICTIONARY':[frgb['DICO']]})

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
            print ' copying: ', path.basename(cref)
         else:
            shutil.copy(cref,crun)
            print ' copying: ', path.basename(cref)

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
         print ' copying: ', path.basename(cref)

   return True

def processCONFIG(lang):

   # ~~ create CONFIG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('CONFIG',[str(lang),'6'])
   return True

def getNCSIZE(cas,dico,frgb):

   # ~~ check keyword ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   value,defaut = getKeyWord('PROCESSEURS PARALLELES',cas,dico,frgb)
   ncsize = -1
   if value != []: ncsize = int(value[0])
   elif defaut != []: ncsize = int(defaut[0])

   return ncsize

def processPARALLEL(ncsize,wdir):

   # ~~ parallel case ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if ncsize >= 0:
      putFileContent('PARAL',[str(ncsize),str(len(wdir)),wdir])
   elif ncsize > 1:
      putFileContent('PARAL',[str(ncsize),str(len(wdir)),wdir])

   return

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

def getCONLIM(cas,iFiles):

   # ~~ look for CONLIM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CONLIM = ''
   for k in cas.keys():
      if iFiles.has_key(k):
         if iFiles[k].split(';')[5] == 'CONLIM': CONLIM = iFiles[k].split(';')[1]
   return CONLIM

def getGLOGEO(cas,iFiles):

   # ~~ look for GLOBAL GEO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   GLOGEO = ''
   for k in cas.keys():
      if iFiles.has_key(k):
         if iFiles[k].split(';')[5][-4:] == 'GEOM': GLOGEO = iFiles[k].split(';')[1]
   return GLOGEO

def runPartition(partel,cas,conlim,iFiles,ncsize):

   # ~~ split input files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k in cas.keys():
      if iFiles.has_key(k):
         crun = iFiles[k].split(';')[1]
         if iFiles[k].split(';')[5][0:7] == 'SELAFIN':
            print ' partitioning: ', path.basename(crun)   # path.basename(cas[k][0])
            runPARTEL(partel,crun,conlim,ncsize)
         elif iFiles[k].split(';')[5][0:4] == 'SCAL':
            print ' duplicating: ', path.basename(crun)    # path.basename(cas[k][0])
            for n in rang(ncsize): shutil.copy(crun,crun+('000000'+str(n))[-6:])

   return True

def runPARTEL(partel,file,conlim,ncsize):

   putFileContent('partel_'+file+'.par',[file,conlim,str(ncsize),str(1),str(0)]) # option 1, without sections 0
   failure = system(partel+' < partel_'+file+'.par >> partel_'+file+'.log')
   if not failure: return True
   return False

def runCode(exe):

   failure = system(exe)
#   failure = system(exe + ' >> sortie.txt')
   #p = Popen(["exe"], stdout=PIPE, stderr=PIPE ,shell=True)
   #print p.communicate()[0]
   #failure = False
   #if p.communicate()[1] != '': failure = True
   if not failure: return True
   return False

def runRecollection(gretel,cas,glogeo,oFiles,ncsize):

   # ~~ aggregate output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k in cas.keys():
      if oFiles.has_key(k):
         crun = oFiles[k].split(';')[1]
         type = oFiles[k].split(';')[5]
         if type[0:7] == 'SELAFIN':
            print ' recollectioning: ', path.basename(crun)
            runGRETEL(gretel,crun,glogeo,ncsize)
         if type[0:6] == 'DELWAQ':
            print ' recollectioning: ', path.basename(crun)
            runGREDEL(gretel,crun,glogeo,type[6:],ncsize)

   return True

def runGRETEL(gretel,file,geom,ncsize):

   # ~~ Run GRETEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('gretel_'+file+'.par',[geom,file,str(ncsize)])
   failure = system(gretel+' < gretel_'+file+'.par >> gretel_'+file+'.log')
   if not failure: return True
   return False

def runGREDEL(gredel,file,geom,type,ncsize):

   # ~~ Change GRETEL into GREDEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   pg = path.dirname(gredel)
   bg,eg = path.splitext(path.basename(gredel))
   gredel = path.join(pg,'gredel' + type.lower() + eg)
   # ~~ Run GREDEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('gretel_'+file+'.par',[geom,file,str(ncsize)])
   failure = system(gredel+' < gretel_'+file+'.par >> gretel_'+file+'.log')
   if not failure: return True
   return False
   
   return

def runCAS(cfgName,cfg,codeName,casFile,dico,frgb,iFS,oFS,options):

   # ~~ Read the CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cas,lang = processCAS(casFile,frgb)
   if not checkConsistency(cas,dico,frgb,cfg):
      print '... inconsistent CAS file: ',casFile
      return    # /!\ should you stop or carry on ?

   # ~~ Handling Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CASDir = path.dirname(casFile)
   TMPDir = processTMP(casFile)

   # ~~ Handling all input files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # >>> Placing yourself where the CAS File is
   chdir(CASDir)
   # >>> Copy INPUT files into TMPDir
   if not processLIT(cas,iFS,TMPDir): sys.exit()
   # >>> Placing yourself into the TMPDir
   chdir(TMPDir)
   # >>> Creating LNG file
   processCONFIG(lang)

   # ~~ Handling Executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # >>> Names for the executable set
      #> names within TMPDir
   f90File = iFS['FICHIER FORTRAN'].split(';')[1]
   objFile = path.splitext(f90File)[0] + cfg['SYSTEM']['SFX_OBJ']
      #> default executable name
   exeFile = path.join(path.join(cfg['MODULES'][codeName]['path'],cfgName),codeName+cfg['TELVER']+cfg['SYSTEM']['SFX_EXE'])
      #> user defined executable name
   useFile = exeFile
   value,defaut = getKeyWord('FICHIER FORTRAN',cas,dico,frgb)
   if value != []:
      useFile = path.join(CASDir,path.splitext(value[0])[0]+cfg['SYSTEM']['SFX_EXE'])
      if path.exists(useFile) and cfg['REBUILD'] > 0: remove(useFile)
      #> default command line compilation and linkage
   objCmd = getFileContent(path.join(path.join(cfg['MODULES'][codeName]['path'],cfgName),codeName+cfg['TELVER']+'.cmdo'))[0]
   exeCmd = getFileContent(path.join(path.join(cfg['MODULES'][codeName]['path'],cfgName),codeName+cfg['TELVER']+'.cmdx'))[0]
   # >>> Compiling the executable if required
   if not processExecutable(useFile,objFile,f90File,objCmd,exeCmd,CASDir): sys.exit()

   # >>> Rename executable because of firewall issues ~~~~~~~~~~~~~~
   runCmd = path.join(CASDir,'out_'+path.basename(useFile))
   shutil.move(path.basename(useFile),runCmd)

   if not options.compileonly:

   # ~~ Handling the parallelisation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ncsize = getNCSIZE(cas,dico,frgb)
      
      if ncsize > 1:
      # >>> MPI configuration
         # ~~> Executable                  /!\ you need one if ncsize > 1
         mpiCmd = ''
         if cfg.has_key('MPI'):
            if cfg['MPI'].has_key('EXEC'):
               mpiCmd = cfg['MPI']['EXEC']
         if mpiCmd == '':
            print '... I do not know how to run MPI, can you help ?'
            return    # /!\ should you stop or carry on ?
         # ~~> Assign the mpi_telemac.conf
         hosts = ''
         if cfg.has_key('MPI'):
            if cfg['MPI'].has_key('HOSTS'):
               hosts = cfg['MPI']['HOSTS']
         # ~~> MPI Command line
         mpiCmd = mpiCmd.replace('<wdir>','-wdir '+TMPDir)   # /!\ Make sure TMPDir works in UNC convention
         mpiCmd = mpiCmd.replace('<ncsize>','-n '+str(ncsize))
         mpiCmd = mpiCmd.replace('<hosts>',hosts)
         mpiCmd = mpiCmd.replace('<exename>',runCmd)
         runCmd = mpiCmd

      # >>> Parallel tools
         # ~~> Default path
         PARDir = path.join(cfg['MODULES']['parallel']['path'],cfgName)
         # ~~> User path
         if cfg.has_key('PARALLEL'):
            if cfg['PARALLEL'].has_key('PATH'):
               PARDir = cfg['PARALLEL']['PATH'].replace('<root>',cfg['TELDIR']).replace('<config>',path.join(cfg['MODULES']['parallel']['path'],cfgName))
         # ~~> Creating PARA file and the mpi_telemac.conf
         processPARALLEL(ncsize,TMPDir+sep)  # /!\ Make sure TMPDir works in UNC convention

      # >>> Running the partionning
      if ncsize > 1:
         # ~~> PARTEL Executable
         exeCmd = path.join(PARDir,'partel'+cfg['SYSTEM']['SFX_EXE'])
         # ~~> Run PARTEL
         CONLIM = getCONLIM(cas,iFS)      # no check on existence
         runPartition(exeCmd,cas,CONLIM,iFS,ncsize)

      # >>> Running the Executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print runCmd
      if not runCode(runCmd): sys.exit()

      # >>> Handling the recollection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ncsize > 1:
         # ~~> GRETEL Executable
         exeCmd = path.join(PARDir,'gretel'+cfg['SYSTEM']['SFX_EXE'])
         # ~~> Run GRETEL
         GLOGEO = getGLOGEO(cas,iFS)      # no check on existence
         runRecollection(exeCmd,cas,GLOGEO,oFS,ncsize)

   # ~~ Handling all output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if not processECR(cas,oFS,CASDir): sys.exit()

   # ~~ Handling Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   chdir(CASDir)
   if options.tmpdirectory or options.compileonly: removeDirectories(TMPDir)

   return

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
   if len(args) < 2:
      print '\nThe name of the module to run and one CAS file at least are required\n'
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = args[0]
   casFiles = args[1:]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Loop over configurations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for cfgname in parseConfigFile(options.configFile).keys():
      cfgs = parseConfig_RunningTELEMAC(cfgname)

# >>> Check wether the config has been compiled for the runcode
      for cfg in cfgs.keys():
         if options.compileonly: cfgs[cfg]['REBUILD'] = 2
         cfgs[cfg].update({'TELCOD':codeName})
         if codeName not in cfgs[cfg]['MODULES']:
            print '\nThe code requested is not installed on this system : ' + codeName + '\n'
            sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Loop over CAS Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for casFile in casFiles:
            casFile = path.realpath(casFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
            print '\n\nRunning ' + path.basename(casFile) + ' with '+ codeName + ' under ' + path.dirname(casFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
            print '... reading module dictionary'

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Read the DICO File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            dicoFile = path.join(path.join(cfgs[cfg]['MODULES'][codeName]['path'],'lib'),codeName+cfgs[cfg]['TELVER']+'.dico')
            frgb,dico = scanDICO(dicoFile)
            iFS,oFS = getIOFilesSubmit(frgb,dico)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Run the Code from the CAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            runCAS(cfg,cfgs[cfg],codeName,casFile,dico,frgb,iFS,oFS,options)

   sys.exit()
