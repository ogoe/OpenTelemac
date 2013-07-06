#!/usr/bin/env python
"""@author Sebastien E. Bourban, Noemie Durand and Alain Weisgerber
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
"""@history 10/03/2011 -- Chris Cawthorn
      Amended to enable listing file in addition to output to stdout.
      Use of multi-threads.
"""
"""@history 04/04/2011 -- Sebastien E. Bourban
         Correction for POSTEL3D Use of key 'MULTI' for output file
         recollection.
"""
"""@history 05/04/2011 -- Sebastien E. Bourban
         Correction, adding an empty line at the end of all ASCII files
         (bug reported with CONFIG).
"""
"""@history 05/04/2011 -- Sebastien E. Bourban
         Amended to support reccursively coupled CAS Files, using
         "COUPLAGE AVEC".
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
"""@history 05/07/2011 -- Sebastien E. Bourban
         Python interpreter added for linux calls. This is a temporary
         solution as "/usr/bin/env" is not strickly portable cross
         operating systems
"""
"""@history 10/10/2011 -- Jan-Philip Gehrcke
         Correction made to the management of sortie files. (search JPG)
"""
"""@history 10/02/2012 -- Sebastien E. Bourban
         Addition of the fixed directory option, which is particulalry useful
         for parallel simulations.
"""
"""@history 20/02/2012 -- Sebastien E. Bourban
         Allowing PARTEL to run in parallel, having received the PARTEL
         source code from Charles (STFC-DL).
"""
"""@history 28/02/2012 -- Sebastien E. Bourban
         Allowing the python version of PARTEL_PARA to run in parallel, finding
         that PARTEL in PARALLEL did not solve our partitioning problem.
"""
"""@history 07/03/2012 -- Sebastien E. Bourban
         Allowing a launch of the main executable (MPIEXEC) to run on an HPC queue
         Example given with BSUB to run on encore.ocf.co.uk
"""
"""@history 04/04/2012 -- Sebastien E. Bourban
         Three new options are now available toruncode:
         --split: only does the copying of files (and the split when in parallel)
         --run  : only does the running (by copying the CAS and the PRINCI
            again, for good measure)
         --merge: only does the re-collection and copy back of files,
            which is most useful when the simulation is put on an HPC queue.
"""
"""@history 12/04/2012 -- Sebastien E. Bourban
         Removed the dependency of the compilation of the PRINCI at run time
         from the configuration files (update or clean statements).
"""
"""@history 12/05/2012 -- Fabien Decung & Sebastien E. Bourban
         Modified checkConsistency so the behaviour is as follows :
          - if parallel not in module keys => ncsize should be 0 or stop
          - else ncsize should be 1 or more
         Also, forces the re-writing of the CAS file in the temporary directory
         so keywords can now be modified before running the CAS
"""
"""@history 18/06/2012 -- Sebastien E. Bourban & Fabien Decung
         Calls to sys.exit() and os.system() have been progressively captured
         into a try/except statement to better manage errors.
         This, however, assumes that all errors are anticipated.
"""
"""@history 29/08/2012 -- Sebastien E. Bourban
         Additonal option --nctile setting the number of cores per node.
         In the case of HPC use, the variable <ncsize> is replaced by ncsize,
            and now two other variables are available: <nctile> and <ncnodes>.
         ncsize must be ncnodes x nctile.
"""
"""@history 04/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Simplifying call to parseConfigFile, which now takes two arguments
      options.configFile, and options.configName and return one or more
      valid configurations in an array. Testing for validity is now done
      within config.py
"""
"""@brief
         runcode is the execution launcher for all TELEMAC modules
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import shutil
import threading
from time import localtime, strftime
from subprocess import *
from os import path,walk,mkdir,chdir,remove,sep,environ,listdir
# ~~> dependencies towards other modules
from config import OptionParser,parseConfigFile,parseConfig_RunningTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.files import getFileContent,putFileContent,removeDirectories,isNewer
from utils.messages import MESSAGES,filterMessage
from parsers.parserKeywords import scanCAS,readCAS,rewriteCAS,scanDICO,getKeyWord,setKeyValue,getIOFilesSubmit
from parsers.parserSortie import getLatestSortieFiles

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def checkConsistency(cas,dico,frgb,cfg):

   kl,vl = cas
   # ~~ check language on one of the input file names ~~~~~~~~~~~~~~
   lang = 1
   # Look to find the first key that is different in both language
   i = 0
   while kl[i][0] == '&' or \
      ( kl[i] in frgb['FR'].keys() and kl[i] in frgb['GB'].keys() ):
      i+=1
   if kl[i] not in frgb['FR'].keys(): lang = 2

   # ~~ check for parallel consistency ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   value,defaut = getKeyWord('PROCESSEURS PARALLELES',cas,dico,frgb)
   ncsize = 0
   if value != []: ncsize = value[0]
   elif defaut != []: ncsize = int(defaut[0])
   if cfg['PARALLEL'] == None:
      if ncsize != 0: return False
   if cfg['PARALLEL'] != None:
      if ncsize == 0: return False
   if lang == 1: cas = setKeyValue('PROCESSEURS PARALLELES',cas,frgb,max(ncsize,1))
   if lang == 2: cas = setKeyValue('PARALLEL PROCESSORS',cas,frgb,max(ncsize,1))

   # ~~ check for openmi consistency ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   return True

def processCAS(casFile,dico,frgb):

   # ~~ extract keywords ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cas = readCAS(scanCAS(casFile),dico,frgb)

   # ~~ check language on one of the input file names ~~~~~`~~~~~~~~~
   lang = 1
   kl = cas[0]
   # Look to find the first key that is different in both language
   i = 0
   while kl[i][0] == '&' or \
      ( kl[i] in frgb['FR'].keys() and kl[i] in frgb['GB'].keys() ):
      i+=1
   if kl[i] not in frgb['FR'].keys(): lang = 2

   # ~~ check language on one of the input file names ~~~~~~~~~~~~~~
   if lang == 1:
      print '... simulation en Francais avec ',path.basename(casFile)
      cas = setKeyValue('FICHIER DES PARAMETRES',cas,frgb,repr(path.basename(casFile)))
      cas = setKeyValue('DICTIONNAIRE',cas,frgb,repr(path.normpath(frgb['DICO'])))
   if lang == 2:
      print '... running in English with ',path.basename(casFile)
      cas = setKeyValue('STEERING FILE',cas,frgb,repr(path.basename(casFile)))
      cas = setKeyValue('DICTIONARY',cas,frgb,repr(path.normpath(frgb['DICO'])))

   return cas,lang

def processTMP(casFile):

   # ~~ TMP Directory ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   TMPDir = casFile + '_' + strftime("%Y-%m-%d-%Hh%Mmin%Ss", localtime())

   return TMPDir

def processLIT(cas,iFiles,TMPDir,ncsize,update):

   xcpt = []                            # try all files for full report
   # ~~ copy input files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k,v in zip(*cas):
      if iFiles.has_key(k):
         cref = eval(v[0]) #eval(v[k][0])
         if not path.isfile(cref):
            xcpt.append({'name':'processLIT','msg':'file does not exist: '+path.basename(cref)})
            continue
         crun = path.join(TMPDir,iFiles[k].split(';')[1])
         if path.exists(crun) and update:
            if not isNewer(crun,cref) == 1:
               if iFiles[k].split(';')[5][0:7] == 'SELAFIN' or iFiles[k].split(';')[5][0:5] == 'PARAL':
                  # ~~> check if all files are there before skipping
                  found = True
                  for npsize in range(ncsize):
                     if not path.isfile(crun+'{0:05d}-{1:05d}'.format(ncsize-1,npsize)): found = False
                  # ~~> skip if all files are there
                  if found: iFiles[k] = iFiles[k].replace('SELAFIN','DONE').replace('PARAL','DONE')
               # special case for FORTRAN and CAS files in case of update
               if iFiles[k].split(';')[5][0:7] == 'FORTRAN':
                  print ' re-copying: ', path.basename(cref),crun
                  putFileContent(crun,getFileContent(cref)+[''])
               elif iFiles[k].split(';')[5][0:3] == 'CAS':
                  print ' re-writing: ', crun
                  putFileContent(crun,rewriteCAS(cas))
               else:
                  print '   ignoring: ', path.basename(cref),crun
                  #putFileContent(crun,getFileContent(cref)+[''])
               continue
         if iFiles[k].split(';')[3] == 'ASC':
            if iFiles[k].split(';')[5][0:3] == 'CAS':
               print ' re-writing: ', crun
               putFileContent(crun,rewriteCAS(cas))
               # An input mesh may be a binary or an ascii file
               # It depends on the selected format (Selafin, Ideas, Med)
            elif iFiles[k].split(';')[5][0:12] == 'SELAFIN-GEOM':
               print '    copying: ', path.basename(cref),crun
               shutil.copy(cref,crun)
            else:
               # FD : this is not a true copy. Why ?
               print '    copying: ', path.basename(cref),crun
               putFileContent(crun,getFileContent(cref)+[''])
         else:
            print '    copying: ', path.basename(cref),crun
            shutil.copy(cref,crun)

   if xcpt != []: raise Exception(xcpt) # raise full report
   return

def processECR(cas,oFiles,CASDir,TMPDir,sortiefile,ncsize,bypass):

   xcpt = []                            # try all files for full report
   # ~~ copy output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k,v in zip(*cas):
      if oFiles.has_key(k):
         if oFiles[k].split(';')[5] == 'MULTI':   # POSTEL3D
            npsize = 1
            while 1:                              # HORIZONTAL SECTION FILES
               cref = path.join(CASDir,eval(v[0])+'_{0:03d}'.format(npsize))
               if path.isfile(cref): shutil.move(cref,cref+'.old') #shutil.copy2(cref,cref+'.old')
               crun = oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)
               if not path.isfile(crun): break
               shutil.move(crun,cref) #shutil.copy2(crun,cref)
               print '  moving: ', path.basename(cref)
               npsize = npsize + 1
            npsize = 1
            while 1:                              # VERTICAL SECTION FILES
               nptime = 1
               if not path.isfile(oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime)): break
               while 1:
                  cref = path.join(CASDir,eval(v[0])+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime))
                  if path.isfile(cref): shutil.move(cref,cref+'.old') #shutil.copy2(cref,cref+'.old')
                  crun = oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime)
                  if not path.isfile(crun): break
                  shutil.move(crun,cref) #shutil.copy2(crun,cref)
                  print '  moving: ', path.basename(cref)
                  nptime = nptime + 1
               npsize = npsize + 1
         elif oFiles[k].split(';')[5] == 'PARAL' and ncsize > 1: # MAIN MODULE
            npsize = 1
            cb,ce = path.splitext(eval(v[0]))
            while 1:
               cref = path.join(CASDir,cb+'{0:05d}-{1:05d}'.format(ncsize-1,npsize)+ce)
               if path.isfile(cref): shutil.move(cref,cref+'.old') #shutil.copy2(cref,cref+'.old')
               crun = oFiles[k].split(';')[1]+'{0:05d}-{1:05d}'.format(ncsize-1,npsize)
               if not path.isfile(crun): break
               shutil.move(crun,cref) #shutil.copy2(crun,cref)
               #print ' copying: ', path.basename(cref)
               print '  moving: ', path.basename(cref)
               npsize = npsize + 1
         elif oFiles[k].split(';')[5] == 'MULTI2':
            for crun in listdir('.') :
               if crun.count(oFiles[k].split(';')[1]) == 1:
                  cref = path.join(CASDir,crun.lower().replace(oFiles[k].split(';')[1].lower(),eval(v[0]).split('.')[0])) + '.' + eval(v[0]).split('.')[1]
                  if path.isfile(cref): shutil.move(cref,cref+'.old') #shutil.copy2(cref,cref+'.old')
                  shutil.move(crun,cref)
                  print '  moving: ', path.basename(cref)
         else:
            cref = path.join(CASDir,eval(v[0]))
            if path.isfile(cref): shutil.move(cref,cref+'.old') #shutil.copy2(cref,cref+'.old')
            crun = oFiles[k].split(';')[1]
            if not path.isfile(crun):
               xcpt.append({'name':'processECR','msg':'did not create outfile: '+path.basename(cref)+' ('+crun+')'})
               continue
            shutil.move(crun,cref) #shutil.copy2(crun,cref)
            print '  moving: ', path.basename(cref)

   # ~~~ copy the sortie file(s) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   sortiefiles = []
   if sortiefile != None:    # sortiefile.rstrip() != '':
      crun = path.basename(sortiefile)
      cref = path.join(CASDir,sortiefile)
      if not path.isfile(crun):
         xcpt.append({'name':'processECR','msg':'did not create listing file: '+path.basename(cref)+' ('+crun+')'})
         raise Exception(xcpt) # raise full report
      shutil.copy(crun,cref)
      print ' copying: ', path.basename(cref)
      sortiefiles.append(cref)

      # ~~~ If in parallel, also copy the slave log files     ~~~~~~
      # ~~~ called PEnnnnn_xxxxx.log for slave x of n         ~~~~~~
      # ~~~ Note that n=ncsize-1; output from the Master goes ~~~~~~
      # ~~~ directly in to the sortie file                    ~~~~~~
      if ncsize > 1:
         for i in range(ncsize-1):
            slavefile = 'PE{0:05d}-{1:05d}.LOG'.format(ncsize-1,i+1)
            bs,es = path.splitext(sortiefile) # (path.basename(sortiefile))
            slogfile  = bs+'_p'+'{0:05d}'.format(i+1)+es
            crun = slavefile
            cref = path.join(CASDir,slogfile)
            if not path.isfile(crun):
               xcpt.append({'name':'processECR','msg':'could not find the listing file: '+crun})
               raise Exception(xcpt) # raise full report
            shutil.copy(crun,cref)
            print ' copying: ',path.basename(cref)            
            sortiefiles.append(cref)
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpt != []: raise Exception(xcpt) # raise full report
   return sortiefiles

def processCONFIG(lang):

   # ~~ create CONFIG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('CONFIG',[repr(lang),'6',''])
   return True

def getNCSIZE(cas,dico,frgb):

   # ~~ check keyword ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   value,defaut = getKeyWord('PROCESSEURS PARALLELES',cas,dico,frgb)
   ncsize = -1
   if value != []: ncsize = value[0]
   elif defaut != []: ncsize = int(defaut[0])

   return ncsize

def getMPICommand(cfgMPI):
   # ~~> Executable
   mpiCmd = cfgMPI['EXEC']
   # ~~> host file
   hostfile = ''
   if cfgMPI.has_key('HOSTFILE'): hostfile = cfgMPI['HOSTFILE']
   mpiCmd = mpiCmd.replace('<hostfile>',hostfile)
   # ~~> stdin file
   infile = ''
   if cfgMPI.has_key('INFILE'): infile = cfgMPI['INFILE']
   mpiCmd = mpiCmd.replace('<mpi_infile>',infile)

   return mpiCmd

def getHPCCommand(cfgHPC):
   # ~~> Executable
   hpcCmd = cfgHPC['EXEC']
   # ~~> script
   if cfgHPC.has_key('STDIN'):
      infile = 'HPC_STDIN'
      cfgHPC['STDIN'] = { infile: cfgHPC['STDIN'].replace(r'\n','\n') }
      hpcCmd = hpcCmd.replace('<hpc_stdin>',infile)

   return hpcCmd

def processPARALLEL(ncsize,wdir):

   # ~~ parallel case ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if ncsize > 0: putFileContent('PARAL',[str(ncsize),str(len(wdir)),wdir,''])

   return

def processExecutable(useName,objName,f90Name,objCmd,exeCmd,CASDir,bypass):

   if path.exists(f90Name) and not path.exists(useName):
   # ~~ requires compilation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      objCmd = objCmd.replace('<f95name>',f90Name)
      print objCmd
      mes = MESSAGES(size=10)
      try:
         tail,code = mes.runCmd(objCmd,bypass)
      except Exception as e:
         raise Exception([filterMessage({'name':'processExecutable','msg':'something went wrong for no reason. Please verify your compiler installation.'},e,bypass)])
      if code != 0: raise Exception([{'name':'processExecutable','msg':'could not compile your FORTRAN (runcode='+str(code)+').\n      '+tail}])
      exeCmd = exeCmd.replace('<objs>',objName)
      exeCmd = exeCmd.replace('<exename>',path.basename(useName))
      print exeCmd
      try:
         tail,code = mes.runCmd(exeCmd,bypass)
      except Exception as e:
         raise Exception([filterMessage({'name':'processExecutable','msg':'something went wrong for no reason. Please verify your external library installation.'},e,bypass)])
      if code != 0: raise Exception([{'name':'processExecutable','msg':'could not link your executable (runcode='+str(code)+').\n      '+tail}])
   
   else:
   # ~~ default executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shutil.copy2(useName,path.basename(useName))

   # ~~ save a copy for future uses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if CASDir != '': shutil.copy2(path.basename(useName),path.join(CASDir,path.basename(useName)))

   return True

def compilePRINCI(princiFile,codeName,cfgName,cfg,bypass):

   plib = cfg['MODULES'][codeName]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+cfgName+sep+'lib')
   pbin = cfg['root']+sep+'builds'+sep+cfgName+sep+'bin'
   objFile = path.join(plib,codeName+'.cmdo')
   exeFile = path.join(plib,codeName+'.cmdx')
   if not path.exists(objFile) or not path.exists(exeFile):
      raise Exception([{'name':'compilePRINCI','msg':'... could not find:' + exeFile + \
         '\n    ~~~> you may need to compile your system with the configuration: ' + cfgName }])
   objCmd = getFileContent(objFile)[0]
   exeCmd = getFileContent(exeFile)[0]
   chdir(path.dirname(princiFile))
   princiFile = path.basename(princiFile)
   objFile = path.splitext(princiFile)[0] + cfg['SYSTEM']['sfx_obj']
   exeFile = path.splitext(princiFile)[0] + cfg['SYSTEM']['sfx_exe']
   if path.exists(exeFile): remove(exeFile)
   try:
      processExecutable(exeFile,objFile,princiFile,objCmd,exeCmd,'',bypass)
   except Exception as e:
      raise Exception([filterMessage({'name':'compilePRINCI','msg':'could not compile: ' + princiFile},e,bypass)])  # only one item here
   if path.exists(objFile): remove(objFile)

   return exeFile

def getCONLIM(cas,iFiles):

   # ~~ look for CONLIM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CONLIM = ''
   for k in cas[0]:
      if iFiles.has_key(k):
         if iFiles[k].split(';')[5] == 'CONLIM': CONLIM = iFiles[k].split(';')[1]
   return CONLIM

def getGLOGEO(cas,iFiles):

   # ~~ look for GLOBAL GEO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   GLOGEO = ''
   for k in cas[0]:
      if iFiles.has_key(k):
         if iFiles[k].split(';')[5][-4:] == 'GEOM': GLOGEO = iFiles[k].split(';')[1]
   return GLOGEO

def runPartition(partel,cas,conlim,iFiles,ncsize,bypass):

   if ncsize < 2: return True
   # ~~ split input files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k in cas[0]:
      if iFiles.has_key(k):
         crun = iFiles[k].split(';')[1]
         if iFiles[k].split(';')[5][0:7] == 'SELAFIN':
            print ' partitioning: ', path.basename(crun)
            try:
               runPARTEL(partel,crun,conlim,ncsize,bypass)
            except Exception as e:
               raise Exception([filterMessage({'name':'runPartition'},e,bypass)])
         elif iFiles[k].split(';')[5][0:5] == 'PARAL':
            print ' duplicating: ', path.basename(crun)
            for n in range(ncsize): shutil.copy2(crun,crun+('00000'+str(ncsize-1))[-5:]+'-'+('00000'+str(n))[-5:])

   return True

def runPARTEL(partel,file,conlim,ncsize,bypass):

   putFileContent('PARTEL.PAR',[file,conlim,str(ncsize),str(1),str(0),str(1),str(1),''])
   parCmd = partel.replace('<partel.log>','partel_'+file+'.log').split(';')
   mes = MESSAGES(size=10)
   for p in parCmd:
      try:
         print '    +> ',p
         tail,code = mes.runCmd(p,bypass)
      except Exception as e:
         raise Exception([filterMessage({'name':'runPARTEL','msg':'Could not execute the following partition task:\n      '+p},e,bypass)])
      if code != 0: raise Exception([{'name':'runPARTEL','msg':'Could not complete partition (runcode='+str(code)+').\n      '+tail}])
   return

# ~~~ CCW: amended runCode to include optional listing file        ~~~
# ~~~      print_twice echos the listing output to the sortie file ~~~
def print_twice(pipe,ofile):

   # Utility subroutine to print listing data both to stdout 
   # and to the listing file, accessed via the ofile handle
   lastlineempty = False      # JPG addition here as opposed to argument
   for line in iter(pipe.readline,''):
      dat = line.rstrip()
      # This IF statement just avoid printing a lot of blank lines 
      # at the end of the run, before Python realises that the process
      # has stopped. 
      if (dat == ''):
         if not lastlineempty:
            print dat                   # Print to screen
            if ofile != None:
               ofile.write(dat+'\n')    # Write to sortiefile (if requested)
            lastlineempty = True        # Set to avoid printing multiple consecutive newlines
      else:
         lastlineempty = False
         print dat                      # Print to screen
         if ofile != None:
            ofile.write(dat+'\n')       # Write to sortiefile (if requested)

def runCode(exe,sortiefile):
   ofile = None
   # If sortiefile is required, open it
   if sortiefile != None: ofile = open(sortiefile,"w")
   # Start process with command 'exe', and direct standard output and
   # standard error into PIPE (part of the Popen object called proc)
   proc = Popen(exe,bufsize=1024,stdout=PIPE,stderr=PIPE,shell=True)
   # Define a thread, t1, that will execute the subroutine 'print_twice', with
   # the args given.
   t1 = threading.Thread(target=print_twice,args=(proc.stdout,ofile))
   # Start the print_twice thread. This continues until the stdout buffer is empty
   # (usually when the Telemac code has terminated)
   t1.start()
   # Wait for t1 to terminate before continuing
   t1.join()
   # Close the sortiefile, if used
   if ofile: ofile.close()
   # Wait to make sure that the Telemac code really has terminated
   # Note: this is probably unnecessary, but done to make sure that
   #       a zero return code is returned, indicating successful completion.
   proc.wait()
   if proc.returncode == 0: return True
   raise Exception({'name':'runCode','msg':'Fail to run\n'+exe+
      '\n~~~~~~~~~~~~~~~~~~\n'+str(proc.stderr.read().strip())+'\n~~~~~~~~~~~~~~~~~~'})
   return False

def runRecollection(gretel,cas,glogeo,oFiles,ncsize,bypass):

   if ncsize < 2: return True
   # ~~ aggregate output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k in cas[0]:
      if oFiles.has_key(k):
         crun = oFiles[k].split(';')[1]
         type = oFiles[k].split(';')[5]
         if type[0:7] == 'SELAFIN':
            print ' recollectioning: ', path.basename(crun)
            try:
               runGRETEL(gretel,crun,glogeo,ncsize,bypass)
            except Exception as e:
               raise Exception([filterMessage({'name':'runRecollection'},e,bypass)])
         if type[0:6] == 'DELWAQ':
            print ' recollectioning: ', path.basename(crun)
            try:
               runGREDEL(gretel,crun,glogeo,type[6:],ncsize,bypass)
            except Exception as e:
               raise Exception([filterMessage({'name':'runRecollection'},e,bypass)])
   return True

def runGRETEL(gretel,file,geom,ncsize,bypass):

   # ~~ Run GRETEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('gretel_'+file+'.par',[geom,file,str(ncsize)])
   mes = MESSAGES(size=10)
   cmd = gretel+' < gretel_'+file+'.par >> gretel_'+file+'.log'
   try:
      tail,code = mes.runCmd(cmd,bypass)
   except Exception as e:
      raise Exception([filterMessage({'name':'runGRETEL','msg':'something went wrong, I am not sure why. Please verify your compilation or the python script itself.'},e,bypass)])
   if code != 0: raise Exception([{'name':'runGRETEL','msg':'Could not split your file (runcode='+str(code)+').\n     '+file+'\n      '+tail}])
   return

def runGREDEL(gredel,file,geom,type,ncsize,bypass):

   # ~~ Change GRETEL into GREDEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   pg = path.dirname(gredel)
   bg,eg = path.splitext(path.basename(gredel))
   gredel = path.join(pg,'gredel' + type.lower() + '_autop' + eg)
   # ~~ Run GREDEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('gretel_'+file+'.par',[geom,file,str(ncsize)])
   mes = MESSAGES(size=10)
   cmd = gredel+' < gretel_'+file+'.par >> gretel_'+file+'.log'
   try:
      tail,code = mes.runCmd(cmd,bypass)
   except Exception as e:
      raise Exception([filterMessage({'name':'runGREDEL','msg':'something went wrong, I am not sure why. Please verify your compilation or the python script itself.'},e,bypass)])
   if code != 0: raise Exception([{'name':'runGREDEL','msg':'Could not split your file (runcode='+str(code)+').\n     '+file+'\n      '+tail}])
   return

def runCAS(cfgName,cfg,codeName,casFile,options):

   # ~~~~ Read the DICO File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   dicoFile = path.join(cfg['MODULES'][codeName]['path'],codeName+'.dico')
   if not path.exists(dicoFile): raise Exception([{'name':'runCAS','msg':'could not find the DICO file: '+dicoFile}])
   frgb,dico = scanDICO(dicoFile)
   iFS,oFS = getIOFilesSubmit(frgb,dico)

   # ~~ Read the principal CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if not path.exists(casFile): raise Exception([{'name':'runCAS','msg':'inexistent CAS file: '+casFile}])
   # ~~ Read the principal CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cas,lang = processCAS(casFile,dico,frgb)
   # ~~ Forces run in parallel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if options.ncsize != '':
      if lang == 1: cas = setKeyValue('PROCESSEURS PARALLELES',cas,frgb,int(options.ncsize))
      if lang == 2: cas = setKeyValue('PARALLEL PROCESSORS',cas,frgb,int(options.ncsize))
   # ~~ Consistency checks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if not checkConsistency(cas,dico,frgb,cfg):
      raise Exception([{'name':'runCAS','msg':'inconsistent CAS file: '+casFile+ \
         '    +> you may be using an inappropriate configuration: '+cfgName+ \
         '    +> or may be wishing for parallel mode while using scalar configuration'}])
   ncsize = getNCSIZE(cas,dico,frgb)
   nctile = max( 1,int(options.nctile) )
   ncnodes = int(ncsize/nctile)
   if ncnodes*nctile != ncsize:
      raise Exception([{'name':'runCAS','msg':'ncsize not a multiple of nctile'}])

   # ~~ Handling Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CASDir = path.dirname(casFile)
   TMPDir = processTMP(casFile)
   WDir = TMPDir
   if options.wDir != '':
      WDir = path.join(path.dirname(casFile),options.wDir)
   if not path.exists(WDir):
      mkdir(WDir)
      options.wDir = ''

   # ~~ Read the included CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cplages,defaut = getKeyWord('COUPLING WITH',cas,dico,frgb)
   #/!\ having done the loop this way it will not check for DELWAQ
   COUPLAGE = {}
   for cplage in cplages:
      for mod in cfg['MODULES'].keys():
         if mod in cplage.lower():

            # ~~~~ Extract the CAS File name ~~~~~~~~~~~~~~~~~~~~~~~
            casFilePlage,defaut = getKeyWord(mod.upper()+' STEERING FILE',cas,dico,frgb)
            if casFilePlage == []: casFilePlage = defaut[0]
            else: casFilePlage = eval(casFilePlage[0])
            casFilePlage = path.join(CASDir,casFilePlage)
            if not path.isfile(casFilePlage): raise Exception([{'name':'runCAS','msg':'missing coupling CAS file for '+mod+': '+casFilePlage}])

            # ~~~~ Read the DICO File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            dicoFilePlage = path.join(cfg['MODULES'][mod]['path'],mod+'.dico')
            if not path.exists(dicoFilePlage): raise Exception([{'name':'getDICO','msg':'could not find the DICO file: '+dicoFilePlage}])
            frgbPlage,dicoPlage = scanDICO(dicoFilePlage)
            iFSPlage,oFSPlage = getIOFilesSubmit(frgbPlage,dicoPlage)

            # ~~ Read the coupled CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~
            casPlage,lang = processCAS(casFilePlage,dicoPlage,frgbPlage)
            if not checkConsistency(casPlage,dicoPlage,frgbPlage,cfg): raise Exception([{'name':'runCAS','msg':'inconsistent CAS file: '+casFilePlage}])

            COUPLAGE.update({mod:{}})
            COUPLAGE[mod].update({'cas':casPlage,'frgb':frgbPlage,'iFS':iFSPlage,'oFS':oFSPlage,'dico':dicoPlage})

   # ~~ Handling sortie file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   sortiefile = None
   if options.sortieFile:
      if options.merge:
         # try re-using existing/latest sortie file with same root name
         sortiefile =  path.basename(getLatestSortieFiles(path.join(WDir,path.basename(casFile)))[0])
      else:
         # define the filename (basename) of the sortie file
         sortiefile =  path.basename(TMPDir)+'.sortie'
   sortiefiles = []

   # ~~ Handling all input files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # >>> Placing yourself where the CAS File is
   chdir(CASDir)
   if not options.merge:
      # >>> Copy INPUT files into WDir
      try:
         processLIT(cas,iFS,WDir,ncsize,(options.wDir!=''))
      except Exception as e:
         raise Exception([filterMessage({'name':'runCAS'},e,options.bypass)])  # only one item here
      for mod in COUPLAGE.keys():
         try:
            processLIT(COUPLAGE[mod]['cas'],COUPLAGE[mod]['iFS'],WDir,ncsize,(options.wDir!=''))
         except Exception as e:
            raise Exception([filterMessage({'name':'runCAS'},e,options.bypass)])  # only one item here
   # >>> Placing yourself into the WDir
   chdir(WDir)
   # >>> Creating LNG file
   processCONFIG(lang)

   # ~~ Handling Time Step for Progress ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #time,defaut = getKeyWord('TIME STEP',cas,dico,frgb)
   #time,defaut = getKeyWord('NUMBER OF TIME STEPS',cas,dico,frgb)

   # ~~ Handling Executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # >>> Names for the executable set
      #> names within WDir
   f90File = iFS['FICHIER FORTRAN'].split(';')[1]
      #> aggregation of PRINCI files
   for mod in COUPLAGE.keys():
      f90FilePlage = COUPLAGE[mod]['iFS']['FICHIER FORTRAN'].split(';')[1]
      if path.isfile(f90FilePlage):
         putFileContent(f90File,getFileContent(f90File)+['']+getFileContent(f90FilePlage))
         remove(f90FilePlage)
   plib = cfg['MODULES'][codeName]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+cfgName+sep+'lib')
   pbin = cfg['root']+sep+'builds'+sep+cfgName+sep+'bin'
   objFile = path.splitext(f90File)[0] + cfg['SYSTEM']['sfx_obj']
      #> default executable name
   exeFile = path.join(pbin,codeName+cfg['SYSTEM']['sfx_exe'])
      #> user defined executable name
   useFile = exeFile
   value,defaut = getKeyWord('FICHIER FORTRAN',cas,dico,frgb)
   if value != []:
      useFort = path.join(CASDir,eval(value[0]))
      useFile = path.join(CASDir,path.splitext(eval(value[0]))[0]+cfg['SYSTEM']['sfx_exe'])
      # /!\ removing dependency over cfg['REBUILD']:
      if path.exists(useFile):
         if isNewer(useFile,useFort) == 1 or cfg['REBUILD'] == 1 : remove(useFile)
      #> default command line compilation and linkage
   if not path.exists(path.join(plib,codeName+'.cmdo')):
      raise Exception([{'name':'runCAS','msg': \
         '\nNot able to find your OBJECT command line: ' + path.join(plib,codeName+'.cmdo') + '\n' + \
         '\n ... you have to compile this module at least: '+codeName}])
   objCmd = getFileContent(path.join(plib,codeName+'.cmdo'))[0]
   if not path.exists(path.join(plib,codeName+'.cmdx')):
      raise Exception([{'name':'runCAS','msg': \
         '\nNot able to find your OBJECT command line: ' + path.join(plib,codeName+'.cmdx') + '\n' + \
         '\n ... you have to compile this module at least: '+codeName}])
   exeCmd = getFileContent(path.join(plib,codeName+'.cmdx'))[0]
   # >>> Compiling the executable if required
   exename = path.join(WDir,'out_'+path.basename(useFile))
   runCmd = exename
   if not options.merge and not options.split:
      try:
         processExecutable(useFile,objFile,f90File,objCmd,exeCmd,CASDir,options.bypass)
      except Exception as e:
         raise Exception([filterMessage({'name':'runCAS','msg':'could not compile: ' + path.basename(useFile)},e,options.bypass)])  # only one item here
      shutil.move(path.basename(useFile),exename) # rename executable because of firewall issues

   # ~~ Handling the parallelisation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if ncsize > 0:
      # >>> Parallel execution configuration
      mpiCmd = ''
      if cfg.has_key('MPI') and not options.merge:
         # ~~> MPI host file provided through the command line
         if options.hosts != '':
            if cfg['MPI'].has_key('HOSTS'): cfg['MPI']['HOSTS'] = options.hosts.replace(':',' ')
            else: cfg['MPI'].update( {'HOSTS':options.hosts.replace(':',' ')} )
         # ~~> MPI Command line ( except <exename> )
         mpiCmd = getMPICommand(cfg['MPI']) # /!\ cfg['MPI'] is also modified
         mpiCmd = mpiCmd.replace('<ncsize>',str(ncsize))
         mpiCmd = mpiCmd.replace('<wdir>',WDir)   # /!\ Make sure WDir works in UNC convention
         hostfile = cfg['MPI']['HOSTFILE']
         # ~~> MPI host file ( may be re-written by the HPC INFILE script )
         hosts = []; n = 0
         while n < ncsize:
            for i in cfg['MPI']['HOSTS'].split():
               hosts.append(i); n += 1
               if n == ncsize: break
         putFileContent(hostfile,hosts)

         # >>> Replace running command
         runCmd = mpiCmd.replace('<exename>',exename)

      # >>> Parallel tools
      # ~~> Path
      PARDir = pbin
      if cfg['PARALLEL'] != None:
         if cfg['PARALLEL'].has_key('PATH'): PARDir = cfg['PARALLEL']['PATH'].replace('<root>',cfg['root']).replace('<config>',pbin)
      # ~~> Call to PARTEL
      parCmd = path.join(pbin+sep+'partel'+cfg['SYSTEM']['sfx_exe']+' < PARTEL.PAR >> <partel.log>')
      if cfg['PARALLEL'] != None:
         if cfg['PARALLEL'].has_key('EXEC'): parCmd = cfg['PARALLEL']['EXEC']
      parCmd = parCmd.replace('<mpi_cmdexec>',mpiCmd).replace('<exename>','')
      parCmd = parCmd.replace('<root>',cfg['root']).replace('<config>',PARDir)
      # ~~> Creating the PARA files
      if not options.merge: processPARALLEL(ncsize,WDir+sep)  # /!\ Make sure WDir works in UNC convention


   if options.compileonly:
      print '\n\n... Your simulation is ready for launch and you can now :\n'
      print '    +> re-run without option -x (--compileonly) or with option --run\n'
      print '    +> or run the following command within : ',path.basename(WDir)
      print '       ~>    run with EXE: ',path.basename(exename)
      if cfg.has_key('MPI'): print '       ~> or run with MPI: ',mpiCmd.replace('<exename>',path.basename(exename))
      return []

   # ~~ Handling the partionning ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if not options.compileonly:
      if ncsize > 0:
         # >>> Running the partionning
         # ~~> Run PARTEL
         CONLIM = getCONLIM(cas,iFS)    # Global CONLIM file
         if not options.merge:
            try:
               runPartition(parCmd,cas,CONLIM,iFS,ncsize,options.bypass)
            except Exception as e:
               raise Exception([filterMessage({'name':'runCAS','msg':'Partioning primary input files of the CAS file: '+path.basename(casFile)},e,options.bypass)])
            for mod in COUPLAGE.keys():
               CONLIM = getCONLIM(COUPLAGE[mod]['cas'],COUPLAGE[mod]['iFS'])
               try:
                  runPartition(parCmd,COUPLAGE[mod]['cas'],CONLIM,COUPLAGE[mod]['iFS'],ncsize,options.bypass)
               except Exception as e:
                  raise Exception([filterMessage({'name':'runCAS','msg':'Partioning coupling input files'},e,options.bypass)])

      # >>> Running the Executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if not options.merge:
         if options.split:
            if path.exists(exename):
               print '\n\n... Unless you wish to re-compile your executable, your simulation is ready for launch and you can now :\n'
               print '    +> re-run without option --split or with option --run'
               print '    +> or run the following command within : ',path.basename(WDir)
               print '       ~>    run with EXE: ',path.basename(exename)
               if cfg.has_key('MPI'): print '       ~> or run with MPI: ',mpiCmd.replace('<exename>',path.basename(exename))
            else:
               print '\n\n... Your simulation is almost ready for launch. You need to compile your executable with the option -x (--compileonly)\n'
            return []          # Split only: do not proceed any further
         print '\n\nRunning your simulation :\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         print runCmd+'\n\n'
         if not runCode(runCmd,sortiefile): raise Exception([filterMessage({'name':'runCAS','msg':'Did not seem to catch that error...'})])
      if options.run:
         print '\n\n... Your simulation has been completed but you need to re-collect files using the option --merge\n'
         return []  # Run only: do not proceed any further

      # >>> Handling the recollection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ncsize > 0:
         # ~~> GRETEL Executable
         exeCmd = path.join(PARDir,'gretel_autop'+cfg['SYSTEM']['sfx_exe'])
         # ~~> Run GRETEL
         GLOGEO = getGLOGEO(cas,iFS)    # Global GEO file
         runRecollection(exeCmd,cas,GLOGEO,oFS,ncsize,options.bypass)
         for mod in COUPLAGE.keys():
            GLOGEO = getGLOGEO(COUPLAGE[mod]['cas'],COUPLAGE[mod]['iFS'])
            runRecollection(exeCmd,COUPLAGE[mod]['cas'],GLOGEO,COUPLAGE[mod]['oFS'],ncsize,options.bypass)

   # ~~ Handling all output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      try:
         files = processECR(cas,oFS,CASDir,WDir,sortiefile,ncsize,options.bypass)
      except Exception as e:
         raise Exception([filterMessage({'name':'runCAS','msg':'I could not copy the output files back from the temporary directory:\n      '+WDir},e,options.bypass)])  # only one item here
      sortiefiles.extend(files)
      for mod in COUPLAGE.keys():
         try:
            files = processECR(COUPLAGE[mod]['cas'],COUPLAGE[mod]['oFS'],CASDir,WDir,None,ncsize,options.bypass)
         except Exception as e:
            raise Exception([filterMessage({'name':'runCAS','msg':'I could not copy the output files back from the temporary directory:\n      '+WDir},e,options.bypass)])  # only one item here
         sortiefiles.extend(files)

   # ~~ Handling Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   chdir(CASDir)
   if options.tmpdirectory: removeDirectories(WDir)

   return sortiefiles

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   USETELCFG = ''
   if environ.has_key('USETELCFG'): USETELCFG = environ['USETELCFG']
   PWD = path.dirname(path.dirname(sys.argv[0]))
   SYSTELCFG = path.join(PWD,'config')
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configname",
                      type="string",
                      dest="configName",
                      default=USETELCFG,
                      help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_option("-f", "--configfile",
                      type="string",
                      dest="configFile",
                      default=SYSTELCFG,
                      help="specify configuration file, default is systel.cfg" )
   parser.add_option("-r", "--rootdir",
                      type="string",
                      dest="rootDir",
                      default='',
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
   parser.add_option("-w", "--workdirectory",
                      type="string",
                      dest="wDir",
                      default='',
                      help="specify whether to re-run within a defined subdirectory" )
   parser.add_option("--jobname",
                      type="string",
                      dest="jobname",
                      default='job_unamed',
                      help="specify a jobname for HPC queue tracking" )
   parser.add_option("--queue",
                      type="string",
                      dest="hpc_queue",
                      default='',
                      help="specify a queue for HPC queue tracking" )
   parser.add_option("--walltime",
                      type="string",
                      dest="walltime",
                      default='01:00:00',
                      help="specify a walltime for HPC queue tracking" )
   parser.add_option("--email",
                      type="string",
                      dest="email",
                      default='s.bourban@hrwallingford.com',
                      help="specify an e-mail adress to warn when HPC job is finished" )
   parser.add_option("--hosts",
                      type="string",
                      dest="hosts",
                      default='',
                      help="specify the list of hosts available for parallel mode, ';' delimited" )
   parser.add_option("--ncsize",
                      type="string",
                      dest="ncsize",
                      default='',
                      help="the number of processors forced in parallel mode" )
   parser.add_option("--nctile",
                      type="string",
                      dest="nctile",
                      default='1',
                      help="the number of core per node. ncsize/nctile is the number of compute nodes" )
   parser.add_option("--split",
                      action="store_true",
                      dest="split",
                      default=False,
                      help="will only do the trace (and the split in parallel) if option there" )
   parser.add_option("--merge",
                      action="store_true",
                      dest="merge",
                      default=False,
                      help="will only do the output copying (and recollection in parallel) if option there" )
   parser.add_option("--run",
                      action="store_true",
                      dest="run",
                      default=False,
                      help="will only run the simulation if option there" )
   parser.add_option("--hpc",
                      action="store_true",
                      dest="hpc",
                      default=False,
                      help="Run hpc_cmdexec instead of runcode.py" )
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
      parser.print_help()
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = args[0]
   casFiles = args[1:]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)
   cfgname = cfgs.keys()[0]

   # still in lower case
   if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
   if options.version != '': cfgs[cfgname]['version'] = options.version
   options.bypass = False
   if options.split or options.merge or options.run:
      if options.wDir == '':
         print '\nPlease use option -w (--workdirectory) with either of the options --split, --run or --merge\n'
         sys.exit()
   if (options.split and options.merge) or (options.split and options.run) or (options.split and options.compileonly) or \
      (options.merge and options.run) or (options.merge and options.compileonly) or \
      (options.run and options.compileonly):
      print '\nOnly one of the options --split, --run, --merge or --compileonly (-x) can be used at a time'
      sys.exit()
   # parsing for proper naming
   cfg = parseConfig_RunningTELEMAC(cfgs[cfgname])
   # in case we are launching on a cluster
   if cfg.has_key('HPC') and options.hpc: 
      ncsize = int(options.ncsize)
      nctile = max( 1,int(options.nctile) )
      ncnodes = int(ncsize/nctile)
      hpcCmd = getHPCCommand(cfg['HPC'])
      if cfg['HPC'].has_key('STDIN'):
         stdinfile = cfg['HPC']['STDIN'].keys()[0] # only one key for now
         stdin = cfg['HPC']['STDIN'][stdinfile]
         # Recreate the options for runcode.py
         hpc_options = ''
         if options.configName != '': hpc_options = hpc_options + ' -c ' + options.configName
         if options.configFile != '': hpc_options = hpc_options + ' -f ' + options.configFile
         if options.rootDir != '': hpc_options = hpc_options + ' -r ' + options.rootDir
         if options.version != '': hpc_options = hpc_options + ' -v ' + options.version
         if options.sortieFile: hpc_options = hpc_options + ' -s '
         if options.tmpdirectory: hpc_options = hpc_options + ' -t '
         if options.compileonly: hpc_options = hpc_options + ' -x '
         if options.wDir != '': hpc_options = hpc_options + ' -w ' + options.wDir
         if options.ncsize != '': hpc_options = hpc_options + ' --ncsize ' + options.ncsize
         if options.split: hpc_options = hpc_options + ' --split '
         if options.merge: hpc_options = hpc_options + ' --merge '
         if options.run: hpc_options = hpc_options + ' --run '
         print 'hpc_options '+hpc_options
         # Replace the parameter in the hpc_stdin script
         stdin = stdin.replace('<options>',hpc_options)
         stdin = stdin.replace('<ncsize>',str(ncsize))
         stdin = stdin.replace('<nctile>',str(nctile))
         stdin = stdin.replace('<ncnodes>',str(ncnodes))
         stdin = stdin.replace('<wdir>',options.wDir)
         stdin = stdin.replace('<email>',options.email)
         stdin = stdin.replace('<time>',strftime("%Y-%m-%d-%Hh%Mmin%Ss", localtime()))
         stdin = stdin.replace('<jobname>',options.jobname)
         stdin = stdin.replace('<queue>',options.hpc_queue)
         stdin = stdin.replace('<walltime>',options.walltime)
         stdin = stdin.replace('<codename>',codeName)
         casFilesStr = ''
         for cas in casFiles:
           casFilesStr = casFilesStr + ' ' + cas
         stdin = stdin.replace('<casfiles>',casFilesStr)
         putFileContent(stdinfile,stdin.split('\n'))
      print '\n\nRunning python in hpc mode\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      runCode(hpcCmd,None)
      sys.exit()
      
   print '\n\nRunning your CAS file for:\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '    +> configuration: ' +  cfgname
   print '    +> root:          ' +  cfgs[cfgname]['root']
   print '    +> version        ' +  cfgs[cfgname]['version']
   print '    +> options:       ' +  cfgs[cfgname]['options']
   if options.wDir != '':
      print '    +> directory      ' +  options.wDir
      options.tmpdirectory = False
   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

# >>> Check wether the config has been compiled for the runcode
   if options.compileonly: cfg['REBUILD'] = 1
   if codeName not in cfg['MODULES'].keys():
      print '\nThe code requested is not installed on this system : ' + codeName + '\n'
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Loop over CAS Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for casFile in casFiles:
      casFile = path.realpath(casFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
      print '\n\nRunning ' + path.basename(casFile) + ' with '+ codeName + ' under ' + path.dirname(casFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      print '... reading module dictionary'

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Run the Code from the CAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      try:
         runCAS(cfgname,cfg,codeName,casFile,options)
      except Exception as e:
         xcpts.addMessages(filterMessage({'name':'_____________\nruncode::main:\n      '+path.dirname(casFile)},e,options.bypass))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty():
      print '\n\nHummm ... I could not complete my work.\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n' \
      + xcpts.exceptMessages()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
