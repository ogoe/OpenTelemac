#!/usr/bin/env python
"""@brief
"""
"""@author Sebastien E. Bourban, Noemie Durand and Alain Weisgerber
"""
"""@history 10/03/2011 -- Chris Cawthorn: Amended to enable listing
         file in addition to output to stdout.
"""
"""@history 04/04/2011 -- Sebastien Bourban: Correction for POSTEL3D
         Use of key 'MULTI' for output file recollection.
"""
"""@history 05/04/2011 -- Sebastien Bourban: Correction, adding an empty line
         at the end of all ASCII files (bug reported with CONFIG).
"""
"""@history 05/04/2011 -- Sebastien Bourban: Amended to support reccursively
          coupled CAS Files, using "COUPLAGE AVEC".
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

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from subprocess import *
from os import path,walk,mkdir,chdir,remove,system,sep,environ
from utils import getFileContent,putFileContent,removeDirectories
from parserKeywords import scanCAS,scanDICO,getKeyWord,getIOFilesSubmit
from config import OptionParser,parseConfigFile,parseConfig_RunningTELEMAC
import sys
import shutil
import threading
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
            putFileContent(crun,getFileContent(cref)+[''])
            print ' copying: ', path.basename(cref)
         else:
            shutil.copy2(cref,crun)
            print ' copying: ', path.basename(cref)

   return True

def processECR(cas,oFiles,CASDir,TMPDir,sortiefile,ncsize):

   # ~~ copy output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k in cas.keys():
      if oFiles.has_key(k):
         if oFiles[k].split(';')[5] == 'MULTI':   # POSTEL3D
            npsize = 1
            while 1:                              # HORIZONTAL SECTION FILES
               cref = path.join(CASDir,cas[k][0]+'_{0:03d}'.format(npsize))
               if path.isfile(cref): shutil.copy2(cref,cref+'.old')
               crun = oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)
               if not path.isfile(crun): break
               shutil.copy2(crun,cref)
               print ' copying: ', path.basename(cref)
               npsize = npsize + 1
            npsize = 1
            while 1:                              # VERTICAL SECTION FILES
               nptime = 1
               if not path.isfile(oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime)): break
               while 1:
                  cref = path.join(CASDir,cas[k][0]+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime))
                  if path.isfile(cref): shutil.copy2(cref,cref+'.old')
                  crun = oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime)
                  if not path.isfile(crun): break
                  shutil.copy2(crun,cref)
                  print ' copying: ', path.basename(cref)
                  nptime = nptime + 1
               npsize = npsize + 1
         else:
            cref = path.join(CASDir,cas[k][0])
            if path.isfile(cref): shutil.copy2(cref,cref+'.old')
            crun = oFiles[k].split(';')[1]
            if not path.isfile(crun):
               print '... did not create outfile ',cref,' (',crun,')'
               return False
            shutil.copy2(crun,cref)
            print ' copying: ', path.basename(cref)

   # ~~~ copy the sortie file(s) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if sortiefile.rstrip() != '':
      crun = path.join(TMPDir,sortiefile)
      if not path.isfile(crun):
         print '... did not create listing file',cref,' (',crun,')'
      cref = path.join(CASDir,sortiefile)
      shutil.copy(crun,cref)
      print ' copying: ', path.basename(cref)

      # ~~~ If in parallel, also copy the slave log files     ~~~~~~
      # ~~~ called PEnnnnn_xxxxx.LOG for slave x of n         ~~~~~~
      # ~~~ Note that n=ncsize-1; output from the Master goes ~~~~~~
      # ~~~ directly in to the sortie file                    ~~~~~~
      if ncsize > 1:
         for i in range(ncsize-1):
            slavefile = 'PE{0:05d}-{1:05d}.LOG'.format(ncsize-1,i+1)
            bs,es = path.splitext(path.basename(sortiefile))
            slogfile  = bs+'_p'+'{0:05d}'.format(i+1)+es
            crun = path.join(TMPDir,slavefile)
            cref = path.join(CASDir,slogfile)
            shutil.copy(crun,cref)
            print ' copying: ',path.basename(cref)            
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   return True

def processCONFIG(lang):

   # ~~ create CONFIG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('CONFIG',[str(lang),'6',''])
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
      putFileContent('PARAL',[str(ncsize),str(len(wdir)),wdir,''])
   elif ncsize > 1:
      putFileContent('PARAL',[str(ncsize),str(len(wdir)),wdir,''])

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
      shutil.copy2(useName,path.basename(useName))

   # ~~ save a copy for future uses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   shutil.copy2(path.basename(useName),path.join(CASDir,path.basename(useName)))

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
         elif iFiles[k].split(';')[5][0:5] == 'PARAL':
            print ' duplicating: ', path.basename(crun)    # path.basename(cas[k][0])
            for n in range(ncsize): shutil.copy2(crun,crun+('00000'+str(ncsize-1))[-5:]+'-'+('00000'+str(n))[-5:])

   return True

def runPARTEL(partel,file,conlim,ncsize):

   putFileContent('partel_'+file+'.par',[file,conlim,str(ncsize),str(1),str(0),'']) # option 1, without sections 0
   failure = system(partel+' < partel_'+file+'.par >> partel_'+file+'.log')
   if not failure: return True
   return False

# ~~~ CCW: amended runCode to include optional listing file        ~~~
# ~~~      print_twice echos the listing output to the sortie file ~~~
def print_twice(pipe,ofile,lastlineempty):

   # Utility subroutine to print listing data both to stdout 
   # and to the listing file, accessed via the ofile handle
   for line in iter(pipe.readline,''):
      dat = line.rstrip()
      # This IF statement just avoid printing a lot of blank lines 
      # at the end of the run, before Python realises that the process
      # has stopped. 
      if (dat == ''):
         if not lastlineempty:
            print dat
            if ofile != None:
               ofile.write(dat+'\n')
            lastlineempty = True
      else:
         lastlineempty = False
         print dat
         if ofile != None:
            ofile.write(dat+'\n')

def runCode(exe,sortiefile):
   ofile = None
   if sortiefile != None: ofile = open(sortiefile,"w")
   lastlineempty=False
   proc = Popen(exe,bufsize=1024,stdout=PIPE,stderr=PIPE,shell=True)
   t1 = threading.Thread(target=print_twice,args=(proc.stdout,ofile,lastlineempty,))
   t1.start()
   t1.join()
   if ofile != None: ofile.close()
   proc.wait()
   if proc.returncode == 0: return True

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
   gredel = path.join(pg,'gredel' + type.lower() + '_autop' + eg)
   # ~~ Run GREDEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('gretel_'+file+'.par',[geom,file,str(ncsize)])
   failure = system(gredel+' < gretel_'+file+'.par >> gretel_'+file+'.log')
   if not failure: return True
   return False
   
   return

def runCAS(cfgName,cfg,codeName,casFile,options):

   # ~~~~ Read the DICO File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   dicoFile = path.join(path.join(cfg['MODULES'][codeName]['path'],'lib'),codeName+cfg['TELVER']+'.dico')
   frgb,dico = scanDICO(dicoFile)
   iFS,oFS = getIOFilesSubmit(frgb,dico)

   # ~~ Read the principal CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if not path.exists(casFile):
      print '... inexistent CAS file: ',casFile
      return    # /!\ should you stop or carry on ?
   cas,lang = processCAS(casFile,frgb)
   if not checkConsistency(cas,dico,frgb,cfg):
      print '... inconsistent CAS file: ',casFile
      print '    +> you may be using an inappropriate configuration:',cfgName
      print '    +> or may be wishing for scalar mode while using parallel'
      return    # /!\ should you stop or carry on ?

   # ~~ Handling Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CASDir = path.dirname(casFile)
   TMPDir = processTMP(casFile)

   # ~~ Read the included CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cplages,defaut = getKeyWord('COUPLING WITH',cas,dico,frgb)
   #/!\ having done the loop this way it will not check for DELWAQ
   COUPLAGE = {}
   for cplage in cplages:
      for mod in cfg['MODULES'].keys():
         if mod in cplage.lower():

            # ~~~~ Extract the CAS File name ~~~~~~~~~~~~~~~~~~~~~~~
            casFilePlage,defaut = getKeyWord(mod.upper()+' STEERING FILE',cas,dico,frgb)
            if casFilePlage == []: casFilePlage = defaut
            casFilePlage = path.join(CASDir,casFilePlage[0])
            if not path.isfile(casFilePlage):
               print '... missing coupling CAS file for',mod,': ',casFilePlage
               return    # /!\ should you stop or carry on ?

            # ~~~~ Read the DICO File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            dicoFilePlage = path.join(path.join(cfg['MODULES'][mod]['path'],'lib'),mod+cfg['TELVER']+'.dico')
            frgbPlage,dicoPlage = scanDICO(dicoFilePlage)
            iFSPlage,oFSPlage = getIOFilesSubmit(frgbPlage,dicoPlage)

            # ~~ Read the coupled CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~
            casPlage,lang = processCAS(casFilePlage,frgbPlage)
            if not checkConsistency(casPlage,dicoPlage,frgbPlage,cfg):
               print '... inconsistent CAS file: ',casFilePlage
               return    # /!\ should you stop or carry on ?

            COUPLAGE.update({mod:{}})
            COUPLAGE[mod].update({'cas':casPlage,'frgb':frgbPlage,'iFS':iFSPlage,'oFS':oFSPlage,'dico':dicoPlage})

   # ~~ Handling sortie file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if options.sortieFile:
      sortiefile =  path.basename(TMPDir)+'.sortie'
   else:
      sortiefile = None

   # ~~ Handling all input files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # >>> Placing yourself where the CAS File is
   chdir(CASDir)
   # >>> Copy INPUT files into TMPDir
   if not processLIT(cas,iFS,TMPDir): sys.exit()
   for mod in COUPLAGE.keys():
      if not processLIT(COUPLAGE[mod]['cas'],COUPLAGE[mod]['iFS'],TMPDir): sys.exit()
   # >>> Placing yourself into the TMPDir
   chdir(TMPDir)
   # >>> Creating LNG file
   processCONFIG(lang)

   # ~~ Handling Executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # >>> Names for the executable set
      #> names within TMPDir
   f90File = iFS['FICHIER FORTRAN'].split(';')[1]
      #> aggregation of PRINCI files
   for mod in COUPLAGE.keys():
      f90FilePlage = COUPLAGE[mod]['iFS']['FICHIER FORTRAN'].split(';')[1]
      if path.isfile(f90FilePlage):
         putFileContent(f90File,getFileContent(f90File)+['']+getFileContent(f90FilePlage))
         remove(f90FilePlage)
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
   runCmd = path.join(TMPDir,'out_'+path.basename(useFile))
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
         for mod in COUPLAGE.keys():
            CONLIM = getCONLIM(COUPLAGE[mod]['cas'],COUPLAGE[mod]['iFS'])
            runPartition(exeCmd,COUPLAGE[mod]['cas'],CONLIM,COUPLAGE[mod]['iFS'],ncsize)

      # >>> Running the Executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print runCmd
      if not runCode(runCmd,sortiefile): sys.exit()

      # >>> Handling the recollection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ncsize > 1:
         # ~~> GRETEL Executable
         exeCmd = path.join(PARDir,'gretel_autop'+cfg['SYSTEM']['SFX_EXE'])
         # ~~> Run GRETEL
         GLOGEO = getGLOGEO(cas,iFS)      # no check on existence
         runRecollection(exeCmd,cas,GLOGEO,oFS,ncsize)
         for mod in COUPLAGE.keys():
            GLOGEO = getGLOGEO(COUPLAGE[mod]['cas'],COUPLAGE[mod]['iFS'])
            runRecollection(exeCmd,COUPLAGE[mod]['cas'],GLOGEO,COUPLAGE[mod]['oFS'],ncsize)

   # ~~ Handling all output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if not processECR(cas,oFS,CASDir,TMPDir,sortiefile,ncsize): sys.exit()
      for mod in COUPLAGE.keys():
         if not processECR(COUPLAGE[mod]['cas'],COUPLAGE[mod]['oFS'],CASDir,TMPDir,'',ncsize): sys.exit()

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
   CFGNAME = ''
   PWD = path.dirname(path.dirname(sys.argv[0]))
   SYSTELCFG = path.join(PWD,'config')
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
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
   parser.add_option("-r", "--rootdir",
                      type="string",
                      dest="rootDir",
                      default=PWD,
                      help="specify the root, default is taken from config file" )
   parser.add_option("-v", "--version",
                      type="string",
                      dest="version",
                      default='',
                      help="specify the version number, default is taken from config file" )
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
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         for dirpath,dirnames,filenames in walk(dircfg) : break
         for file in filenames :
            head,tail = path.splitext(file)
            if tail == '.cfg' : print '    +> ',file
      sys.exit()
   if len(args) < 2:
      print '\nThe name of the module to run and one CAS file at least are required\n'
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = args[0]
   casFiles = args[1:]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile)
   cfgnames = cfgs.keys()
   cfgname = options.configName
   if options.configName == '':
      cfgname = cfgnames[0]
   if cfgname not in cfgnames:
      print '\nNot able to get to find your configurtaion in the configuration file: ' + options.configFile + '\n'
      print ' ... use instead:'
      for cfgname in cfgnames : print '    +> ',cfgname
      sys.exit()

   # still in lower case
   if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
   if options.version != '': cfgs[cfgname]['version'] = options.version
   # parsing for proper naming
   cfg = parseConfig_RunningTELEMAC(cfgs[cfgname])

# >>> Check wether the config has been compiled for the runcode
   if options.compileonly: cfg['REBUILD'] = 2
   cfg.update({'TELCOD':codeName})
   if codeName not in cfg['MODULES']:
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
# ~~~~ Run the Code from the CAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      runCAS(cfgname,cfg,codeName,casFile,options)

   sys.exit()
