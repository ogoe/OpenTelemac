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
"""@history 28/04/2011 -- Sebastien E. Bourban
         Now supports SYSTELCFG as a directory (old Perl version,
         to which systel.cfg is added) or as a file.
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
"""@history 27/01/2012 -- Sebastien E. Bourban
         A new option (--modules) added to the command line, which if present
         will reset the value of the key in the configuration file.
         This development was triggered by Christophe Coulet (Artelia-Sogreah)
         who asked about it on the open TELEMAC forum.
"""
"""@history 04/06/2012 -- Fabien Decung
         Allowing 'out of sources' build, compiling directly into 'ObjDir'
         -> nice deal with subdirectories (if not, object are build in
            dirname(odict['path']))
         -> sources directory remains clean
         -> allow concurrent compilation tasks (nasty behaviour with the
            shutil.move operations)
"""
"""@history 18/06/2012 -- Sebastien E. Bourban & Fabien Decung
         Calls to sys.exit() and os.system() have been progressively captured
         into a try/except statement to better manage errors.
         This, however, assumes that all errors are anticipated.
"""
"""@history 04/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Simplifying call to parseConfigFile, which now takes two arguments
      options.configFile, and options.configName and return one or more
      valid configurations in an array. Testing for validity is now done
      within config.py
"""
"""@history 05/12/2012 -- Sebastien E. Bourban
   Allowing the cmd_exe, cmd_lib and cmd_obj to be module specific, with the
      option to have the former as defaults, but reseting the specifics with
      cmd_exe_parallel (for instance)
   As a consequence cmd = cfg['COMPILER']['cmd_obj'] is replaced by
      cmd = cfg['COMPILER']['MODULES'][odict['libname']]['xobj'], etc.
"""
"""@history 12/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Allowing the compilation to be carried out based on the CMDF file,
      created on first build, without having to re-scan the entire
      source code.
"""
"""@history 18/01/2013 -- Yoann Audouin
   Change compilation behaviour alway read the cmdf file even if we do a scan
   So that the compilation part is the same with or without the scan
"""
"""@history 21/06/2013 -- Sebastien E. Bourban
   Upgrade to the new structure of the system.
   Also, checking recursive behaviour with the getTree, by using level as a path.
"""
"""@history 13/07/2013 -- Sebastien E. Bourban
   Final upgrade to the scan to sort by libraries rather than by files
   Also, checking recursive behaviour between libraries.
"""
"""@history 29/09/2014 -- Sebastien E. Bourban
   Addition of a new feature: the ability for the cmd_exe to create static or
   dynamic libraries, using <libname> instead of <exename> in the command.
"""
"""@history 25/12/2014 -- Sebastien E. Bourban
   'version' is not mandatroy anymore.
   It has been removed from having to be in the configuration file.
"""
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import re
import sys
import shutil
from time import sleep
from os import path, sep, walk, chdir, remove, environ, getcwd
from argparse import ArgumentParser,RawDescriptionHelpFormatter
from multiprocessing.sharedctypes import Value,Array
# ~~> dependencies towards the root of pytel
from config import getScanContent,putScanContent,parseConfigFile,parseConfig_CompileTELEMAC,cleanConfig
from parsers.parserFortran import scanSources,getPrincipalWrapNames,refactorSources
# ~~> dependencies towards other pytel/modules
from utils.files import createDirectories,putFileContent,isNewer
from utils.messages import MESSAGES,filterMessage,banner
from utils.progressbar import ProgressBar
from compileAPI import compile_api

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#
def trimTree(name,lname,lst,rebuild):
   liborder = []
   #
   # ~~: lrank
   #  provides 'dw' and 'up' lists of calls / called respectively for each
   #  lib in in the tree of dependencies of name. For instance,
   #  >  lrank['special']['dw'] is probably empty []
   #  >  lrank[lname]['up'] should also be empty []
   lrank = {}
   _ = getTree(name,lname,lst,[],lrank,rebuild)
   #
   # ~~: libdws
   #  the list of libs touced by name, incuding lname, but not necessarily
   #  in order of dependencies
   libdws = [lrank.keys()]
   #
   # gradually emptying lrank until none are left, sorting out ups and downs
   while lrank != {}:
      for libdw in libdws:
         for lib in libdw:
            if lrank[lib]['up'] == [] and lib not in liborder: liborder.insert(0,lib)
      if liborder == []:
         print ' ... found recursive loop with',name,' :',lrank
         sys.exit(1)
      libdws = []
      for lib in liborder:
         if lib in lrank:
            libdws.append(lrank[lib]['dw'])
            del lrank[lib]
         for ldw in lrank:
            if lib in lrank[ldw]['up']:
               lrank[ldw]['up'].remove(lib)
   return liborder

def getTree(name,lname,lst,level,lrank,rebuild):
   #debug = False
   # ~~ Recursive tree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if level != []:
      if name in zip(*level)[0]:
         print 'found recursive loop with',name,' :',' => '.join(zip(*level)[0])
         #sys.exit(1)
         return lst[lname][name]['time']
   # ~~ New leaf ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   level.append((name,lname))
   # ~~ Ranking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if lname not in lrank:
      lrank.update({ lname:{'up':[],'dw':[]} })
   for lib in zip(*level)[1]:
      if lib not in lrank[lname]['up'] and lib != lname: lrank[lname]['up'].append(lib)
   # ~~ prints the tree to screen:
   time = lst[lname][name]['time']
   #if debug: print '===> use',name,lname,lst[lname][name]['uses']
   for use in lst[lname][name]['uses']:
      libname = lname
      if lst[lname].get(use) == None:
         for lib in lst:
            if lib != lname and lst[lib].get(use) != None: libname = lib
      if lst[libname].get(use) != None:
         if libname not in lrank[lname]['dw'] and libname != lname: lrank[lname]['dw'].append(libname)
         tTree = getTree(use,libname,lst,level,lrank,rebuild)
         level.pop()
         if rebuild < 3: time = time * tTree
   #if debug: print '===> call',name,lname,lst[lname][name]['calls']
   for call in lst[lname][name]['calls']:
      libname = lname
      if lst[lname].get(call) == None:
         for lib in lst:
            if lib != lname and lst[lib].get(call) != None:
               libname = lib
      if lst[libname].get(call) != None:
         if libname not in lrank[lname]['dw'] and libname != lname: lrank[lname]['dw'].append(libname)
         tTree = getTree(call.strip(),libname,lst,level,lrank,rebuild)
         level.pop()
         if rebuild < 3: time = time * tTree
   #if debug and lst[lname][name]['functions'] != []: print '===> fcts',name,lname,lst[lname][name]['functions']
   for function in lst[lname][name]['functions']:
      libname = lname
      if lst[lname].get(function) == None:
         for lib in lst:
            if lib != lname and lst[lib].get(function) != None:
               libname = lib
      if lst[libname].get(function) != None:
         if libname not in lrank[lname]['dw'] and libname != lname: lrank[lname]['dw'].append(libname)
         tTree = getTree(function.strip(),libname,lst,level,lrank,rebuild)
         level.pop()
         if rebuild < 3: time = time * tTree
   lst[lname][name]['time'] = time
   if time == 0:
      if [lst[lname][name]['file'],lname] not in MAKSYSTEL['add']: MAKSYSTEL['add'].append([lst[lname][name]['file'],lname])
   else:
      if [name,lname] not in MAKSYSTEL['tag']: MAKSYSTEL['tag'].append([name,lname])
   #print "|  "*len(level) + name + "  > "  + lname
   #ndu print to check 0s and 1s print "|  "*len(level) + name + '  (' + str(time) + ')'

   return lst[lname][name]['time']

def compileMascaretDependencies(cfg,cfgName):
   """
      Compile the c file needed by mascaret

      param cfg Configuration obj
      param cfgName Name of the configuration
   """
   if cfg['cmd_obj_c'] == '':
      print "Missing cmd_obj_c in your configuration file to "+\
            "compile mascaret C dependencies"
      sys.exit(1)

   srcName = path.join(cfg['root'],'sources','mascaret','Deriv','adstack.c')
   objName = path.join(cfg['root'],'builds',cfgName,'lib','mascaret','adstack.o')
   cmd = cfg['cmd_obj_c'].replace('<srcName>',srcName)
   cmd = cmd.replace('<objName>',objName)

   """
   cmd = cfg['cmd_obj_c'].replace('<srcName>',
                              path.join(cfg['root'],
                                       'sources',
                                       'mascaret',
                                       'Deriv',
                                       'adstack.c'))
   cmd = cmd.replace('<objName>',path.join(cfg['root'],
                                          'builds',
                                          cfgName,
                                          'lib',
                                          'mascaret',
                                          'adstack.o'))
   """
   mes = MESSAGES(size=10)
   try:
      if (isNewer(srcName,objName) == 1) and rebuild < 2:
         print '      +> There is no need to compile C object'
      else:
         try:
            tail,code = mes.runCmd(cmd,False)
         except Exception as e:
            raise Exception([filterMessage(
                  {'name':'compileMascaretDepencies',
                   'msg':'something went wrong, I am not sure why.\n'
                  },e,bypass)])
         if code != 0:
            raise Exception([
                  {'name':'compileMascaretDepencies',
                   'msg':'Could not compile your file adstack'
                  }])
         print '   - completed: ../sources/mascaret/Deriv/adstack.c'

   except Exception as e:
      xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> Could not find the following file for compilation: '+path.basename(srcName)+'\n         ... so it may have to be removed from the following cmdf file: '+cmdFile},e,options.bypass)])

   """
   try:
      tail,code = mes.runCmd(cmd,False)
   except Exception as e:
      raise Exception([filterMessage(
            {'name':'compileMascaretDepencies',
             'msg':'something went wrong, I am not sure why.\n'
            },e,bypass)])
   if code != 0:
      raise Exception([
            {'name':'compileMascaretDepencies',
             'msg':'Could not compile your file adstack'
            }])

   print '   - completed: ../sources/mascaret/Deriv/adstack.c'
   """
   HOMERES['HOMERE_MASCARET']['add'].append((path.dirname(objName),'adstack.o','mascaret'))

def createObjFiles(cfg,oname,oprog,odict,ocfg,mes,tasks,bypass):
   # ~~ Assumes that the source filenames are in lower case ~~~~~~~~
   Root,Suffix = path.splitext(path.basename(oname))

   # ~~ Taggings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   libName = odict['libname'].split('.')[0]
   if HOMERES[oprog]['lib'] != '': libName = HOMERES[oprog]['lib']

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjDir = odict['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ocfg+sep+'lib')
   createDirectories(ObjDir)
   chdir(ObjDir)

   # ~~ Removes existing objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if odict['type'][0] == 'M':
      ModFile = path.join(ObjDir,odict['type'][1] + cfg['SYSTEM']['sfx_mod'])
      if path.exists(ModFile): remove(ModFile)
   ObjFile = path.join(ObjDir,Root + cfg['SYSTEM']['sfx_obj'])
   if path.exists(ObjFile): remove(ObjFile)

   # ~~ creation of the module:
   # ~~ ifort.exe /c /Ot /names:uppercase /convert:big_endian /extend_source:132 /include:..\..\..\postel3d\1 declarations_postel3d.f
   cmd = cfg['MODULES'][libName]['xobj']
   incs = cfg['MODULES'][libName]['incs']
   cmd = cmd.replace('<incs>',incs)
   mods = ''
   if HOMERES[oprog]['lib'] != '':
      for ones in HOMERES[oprog]['deps']:
         mod = ones.split('.')[0]
         mods = mods + cfg['MODULES'][libName]['mods'].replace('<config>',path.join(cfg['MODULES'][mod]['path'],'.'+HOMERES[oprog]['lib'])).replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ocfg+sep+'lib') + ' '
   else:
      for ones in HOMERES[oprog]['deps']:
         mod = ones.split('.')[0]
         mods = mods + cfg['MODULES'][libName]['mods'].replace('<config>',cfg['MODULES'][mod]['path']).replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ocfg+sep+'lib') + ' '
   cmd = cmd.replace('<mods>',mods)
   cmd = cmd.replace('<f95name>',path.join(odict['path'],oname))
   cmd = cmd.replace('<config>',ObjDir).replace('<root>',cfg['root'])

   if debug: print cmd
   # ~~> remove gosts
   out = mes.cleanCmd(tasks)
   task = mes.startCmd( tasks,(cmd,bypass,Array('c',' '*10000),Value('i',0)),path.join(odict['path'],oname).replace(path.dirname(cfg['root']),'...') )
   if odict['type'][0] == 'M': out.extend( mes.flushCmd(tasks) )
   # ~~> and remove .f from objList
   odict['time'] = 1
   return out

def createLibFiles(cfg,lname,lmdul,lcfg,lprog,mprog,mes,tasks,bypass):
   # ~~ Assumes that all objects are in <config> ~~~~~~~~~~~~~~~~~~~
   # /!\ why path.basename(lname) ?
   # lname takes the successive values of HOMERES[lprog]['deps']
   #    so it can be passive.solve
   # lmdul is passive
   # lprog is passive.solve

   # ~~ Taggings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   libName = lname.split('.')[0]
   if lname == lprog: libName = lmdul

   # ~~ May not wish to go the extra mile ~~~~~~~~~~~~~~~~~~~~~~~~~~
   cmd = cfg['MODULES'][lmdul]['xlib']
   if cmd == '': return True

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibDir = cfg['MODULES'][libName]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+lcfg+sep+'lib')
   chdir(LibDir)

   # ~~ Lists all dependent libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibFiles = ''
   for lib in HOMERES[lprog]['deps'][:HOMERES[lprog]['deps'].index(lname)]:
      l = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+lcfg+sep+'lib'),lib+'4'+mprog+cfg['SYSTEM']['sfx_lib'])
      #print '     -> ',lib,l
      if not path.exists(l): raise Exception([{'name':'createLibFiles','msg':'Library missing:\n      '+l}])
      LibFiles = l + ' ' + LibFiles

   # LibFile is now created directly within prg[0]'s directory - /!\ hopefuly, the directory exists
   if lmdul == libName: LibFile = path.join(cfg['MODULES'][libName]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+lcfg+sep+'lib'),mprog + cfg['SYSTEM']['sfx_lib'])
   else: LibFile = path.join(cfg['MODULES'][libName]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+lcfg+sep+'lib'),libName+'4'+mprog + cfg['SYSTEM']['sfx_lib'])

   # ~~ Lists all objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for pth,obj,lib in HOMERES[item]['add']:
      obj = path.splitext(path.join(pth,obj))[0].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+lcfg+sep+'lib').replace(LibDir,'.')+cfg['SYSTEM']['sfx_obj']
      if lib == lname: ObjFiles = ObjFiles + (obj+' ')
   for pth,obj,lib in HOMERES[item]['tag']:
      obj = path.splitext(path.join(pth,obj))[0].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+lcfg+sep+'lib').replace(LibDir,'.')+cfg['SYSTEM']['sfx_obj']
      if lib == lname: ObjFiles = ObjFiles + (obj+' ')

   # ~~ is linkage necessary ? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if cfg['COMPILER']['REBUILD'] > 0 and cfg['COMPILER']['REBUILD'] < 2 and path.exists(LibFile): remove(LibFile)
   if path.exists(LibFile):
      refresh = False
      for o in ObjFiles.split(): refresh = refresh or ( isNewer(o,LibFile) == 0 ) or ( not path.exists(o) )
      if refresh: remove(LibFile)
   if path.exists(LibFile): return True

   # ~~ creation of the librairies (according to makefile.wnt + systel.ini):
   # ~~ xilink.exe -lib /nologo /out:postel3dV5P9.lib declarations_postel3d.obj coupeh.obj lecdon_postel3d.obj postel3d.obj coupev.obj lecr3d.obj pre2dh.obj pre2dv.obj ecrdeb.obj nomtra.obj homere_postel3d.obj point_postel3d.obj
   cmd = cmd.replace('<libs>',LibFiles)
   if lprog == 'HOMERE_MASCARET':
      cmd = cmd.replace('<objs>','*.o')
   else:
      cmd = cmd.replace('<objs>',ObjFiles)
   cmd = cmd.replace('<libname>',path.basename(LibFile))

   if debug : print cmd
   mes.startCmd( tasks,(cmd,bypass,Array('c',' '*10000),Value('i',0)),LibFile.replace(path.dirname(cfg['root']),'...') )
   return False

def createExeFiles(cfg,ename,emdul,ecfg,eprog,mes,bypass):

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibDir = cfg['MODULES'][emdul]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib')
   chdir(LibDir)   # this is because of the aggregation of the local objects within the executable project
   ExeDir = cfg['root']+sep+'builds'+sep+ecfg+sep+'bin'
   createDirectories(ExeDir)

   # ~~ Command line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cmd = cfg['MODULES'][emdul]['xexe']

   # ~~ Executables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ExeFile = path.join(ExeDir,eprog + cfg['SYSTEM']['sfx_exe'])
   if '<exename>' in cfg['MODULES'][emdul]['xexe']: cmd = cmd.replace('<exename>',ExeFile).replace('<config>',LibDir).replace('<root>',cfg['root'])
   ObjCmd = path.join(LibDir,eprog + '.cmdo')
   ExeCmd = path.join(LibDir,eprog + '.cmdx')
   #if cfg['COMPILER']['REBUILD'] > 0 and path.exists(ExeFile): remove(ExeFile)

   # ~~ Lists all system libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibFiles = ''
   for lib in HOMERES[ename]['deps'][:len(HOMERES[ename]['deps'])-1]:   # /!\ [1:] to create the exe from local objs.
      l = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib'),lib+'4'+eprog+cfg['SYSTEM']['sfx_lib'])
      if not path.exists(l): raise Exception([{'name':'createExeFiles','msg':'Library missing:\n      '+l}])
      LibFiles = l + ' ' + LibFiles
   for lib in cfg['ADDONES']:
      for fli in cfg['ADDONES'][lib]:
         ones = lib+'.'+path.splitext(fli)[0]
         if not ones in HOMERES: raise Exception([{'name':'createExeFiles','msg':'You may have forgotten to rescan for: '+ones+' not in HOMERES ('+repr(HOMERES.keys())}])
         for one in HOMERES[ones]['deps'][:len(HOMERES[ones]['deps'])-1]:
            l = path.join(cfg['MODULES'][one]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib'),one+'4'+ones+cfg['SYSTEM']['sfx_lib'])
            if not path.exists(l): raise Exception([{'name':'createExeFiles','msg':'Added library missing:\n      '+l}])
            LibFiles = l + ' ' + LibFiles
         l = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib'),ones+cfg['SYSTEM']['sfx_lib'])
         if not path.exists(l): raise Exception([{'name':'createExeFiles','msg':'Library missing:\n      '+l}])
         LibFiles = l + ' ' + LibFiles
   lib = HOMERES[ename]['deps'][len(HOMERES[ename]['deps'])-1]
   if lib == emdul: LibFile = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib'),eprog+cfg['SYSTEM']['sfx_lib'])
   else: LibFile = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib'),lib+'4'+eprog+cfg['SYSTEM']['sfx_lib'])
   if not path.exists(LibFile): raise Exception([{'name':'createExeFiles','msg':'Library missing:\n      '+LibFile}])

   # ~~ Add external libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'libs' in cfg['MODULES'][emdul]: LibFiles = LibFiles + cfg['MODULES'][emdul]['libs']

   # ~~ Lists local objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for pth,obj,lib in HOMERES[ename]['add']:
      Root,Suffix = path.splitext(path.basename(path.join(pth,obj)))
      if lib == emdul and path.basename(obj).lower()+cfg['SYSTEM']['sfx_obj'] not in ObjFiles.split():
         o = Root.lower()+cfg['SYSTEM']['sfx_obj']
         if not path.exists(o): raise Exception([{'name':'createExeFiles','msg':'Object missing:\n      '+o}])
         ObjFiles = ObjFiles + o + ' '
   # TODO: check this: if ObjFiles.strip() == '' and path.exists(ExeFile): return True
   for pth,obj,lib in HOMERES[ename]['tag']:
      Root,Suffix = path.splitext(path.basename(path.join(pth,obj)))
      if lib == emdul and path.basename(obj).lower()+cfg['SYSTEM']['sfx_obj'] not in ObjFiles.split():
         o = Root.lower()+cfg['SYSTEM']['sfx_obj']
         if not path.exists(o): raise Exception([{'name':'createExeFiles','msg':'Object missing:\n      '+o}])
         ObjFiles = ObjFiles + o +' '

   # ~~ is executable necessary ? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if path.exists(ExeFile):
      if cfg['COMPILER']['REBUILD'] > 0 and cfg['COMPILER']['REBUILD'] < 3: remove(ExeFile)
   if path.exists(ExeFile):
      if cfg['COMPILER']['REBUILD'] > 2 or cfg['COMPILER']['REBUILD'] == 0:
         refresh = False
         for o in ObjFiles.split(): refresh = refresh or ( isNewer(o,ExeFile) == 0 )
         for l in LibFiles.split():
            # Only checks the telemac libraries
            if l.find(cfg['root']+sep+'builds'+sep+ecfg+sep+'lib') != -1:
               refresh = refresh or ( isNewer(l,ExeFile) == 0 )
         if refresh: remove(ExeFile)
   if path.exists(ExeFile): return True

   # ~~ creation of the exe (according to makefile.wnt + systel.ini):
   # ~~ xilink.exe /stack:536870912 /out:postel3dV5P9.exe declarations_postel3d.obj coupeh.obj lecdon_postel3d.obj postel3d.obj coupev.obj lecr3d.obj pre2dh.obj pre2dv.obj ecrdeb.obj nomtra.obj homere_postel3d.obj point_postel3d.obj ..\..\..\bief\bief_V5P9\1\biefV5P9.lib ..\..\..\damocles\damo_V5P9\1\damoV5P9.lib ..\..\..\paravoid\paravoid_V5P9\1\paravoidV5P9.lib ..\..\..\special\special_V5P9\1\specialV5P9.lib
   cmd = cmd.replace('<libs>',LibFiles)
   if ename == 'HOMERE_MASCARET':
      cmd = cmd.replace('<objs>','*.o')
   else:
      cmd = cmd.replace('<objs>',ObjFiles)

   xocmd = cfg['MODULES'][emdul]['xobj']
   xocmd = xocmd.replace('<incs>',cfg['MODULES'][emdul]['incs'])
   mods = ''
   for mod in HOMERES[ename]['deps']:
      mods = mods + cfg['MODULES'][emdul]['mods'].replace('<config>',cfg['MODULES'][mod]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib')) + ' '
   xocmd = xocmd.replace('<mods>',mods)
   # <f95name> ... still to be replaced
   xocmd = xocmd.replace('<config>',LibDir).replace('<root>',cfg['root'])

   LibFiles = LibFile + ' ' + LibFiles
   xecmd = cfg['MODULES'][emdul]['xexe']
   xecmd = xecmd.replace('<libs>',LibFiles)
   # Special keyword for nag with ',' separating the libraries
   xecmd = xecmd.replace('<libsnag>',LibFiles.replace(' ',','))
   # <exename> and <objs> ... still to be replaced
   xecmd = xecmd.replace('<config>',LibDir).replace('<root>',cfg['root'])

   if debug : print cmd
   try:
      tail,code = mes.runCmd(cmd,bypass)
      if tail != '':
         if path.exists(ObjCmd): remove(ObjCmd)
         if path.exists(ExeCmd): remove(ExeCmd)
   except Exception as e:
      if path.exists(ObjCmd): remove(ObjCmd)
      if path.exists(ExeCmd): remove(ExeCmd)
      raise Exception([filterMessage({'name':'createExeFiles','msg':'Could not link your executable. Please verify your external library installation or the python script itself.'},e,bypass)])
   if code != 0:
      if path.exists(ObjCmd): remove(ObjCmd)
      if path.exists(ExeCmd): remove(ExeCmd)
      raise Exception([{'name':'createExeFiles','msg':'something went wrong, I am not sure why (runcode='+str(code)+').\n      '+tail}])
   print '   - created ' + ExeFile.replace(path.dirname(cfg['root']),'...')

   # ~~> Make the keys portable (no full path)
   for k in cfg['TRACE']:
      xocmd = xocmd.replace(cfg['TRACE'][k],'['+k+']')
      xecmd = xecmd.replace(cfg['TRACE'][k],'['+k+']')
   putFileContent(ObjCmd,[xocmd])
   putFileContent(ExeCmd,[xecmd])

   return False

def createPydFiles(cfg,yname,yfile,ymdul,ycfg,yprog,mes,bypass):

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibDir = cfg['MODULES'][ymdul]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ycfg+sep+'lib')
   PydDir = cfg['root']+sep+'builds'+sep+ycfg+sep+'bin'
   createDirectories(PydDir)
   chdir(PydDir)

   # ~~ Command line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cmd = cfg['MODULES'][ymdul]['xpyd']

   # ~~ Python interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # TODO:  sort out the intermediate step of pyf - although not necessary
   #if 'sfx_pyf' not in cfg['SYSTEM']: raise Exception([{'name':'createPydFiles','msg':'suffix extension missing from your configuration file (just add sfx_pyf:.pyf)'}])
   #PyfFile = path.join(PydDir,yprog + cfg['SYSTEM']['sfx_pyf'])
   #if '<pyfname>' in cfg['MODULES'][ymdul]['xpyf']: cmd = cmd.replace('<pyfname>',PyfFile).replace('<config>',LibDir).replace('<root>',cfg['root'])
   # ~~ Python module ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'sfx_pyd' not in cfg['SYSTEM']: raise Exception([{'name':'createPydFiles','msg':'suffix extension missing from your configuration file (just add sfx_pyd:.pyd)'}])
   PydFile = path.join(PydDir,yprog + cfg['SYSTEM']['sfx_pyd'])
   if '<pydname>' in cfg['MODULES'][ymdul]['xpyd']: cmd = cmd.replace('<pydname>',yprog).replace('<config>',LibDir).replace('<root>',cfg['root'])
   # ~~~ /!\ TODO: check with more than one file under the same ymdul
   # in which case, reference to yprog and .4yprog will be bundled under ymdul and .4ymdul
   #if '<pydname>' in cfg['MODULES'][ymdul]['xpyd']: cmd = cmd.replace('<pydname>',ymdul).replace('<config>',LibDir).replace('<root>',cfg['root'])
   # ~~~
   f95File = path.join(cfg['MODULES'][ymdul]['path'],yfile)
   if '<f95name>' in cfg['MODULES'][ymdul]['xpyd']: cmd = cmd.replace('<f95name>',f95File)
   #PydCmd = path.join(LibDir,yprog + '.pydx')
   #if cfg['COMPILER']['REBUILD'] > 0 and path.exists(PydFile): remove(PydFile)

   # ~~ Lists all system libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibFiles = ''
   for lib in HOMERES[yname]['deps'][:len(HOMERES[yname]['deps'])-1]:   # /!\ [1:] to create the exe from local objs.
      l = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ycfg+sep+'lib'),lib+'4'+yprog+cfg['SYSTEM']['sfx_lib'])
      if not path.exists(l): raise Exception([{'name':'createPydFiles','msg':'Library missing:\n      '+l}])
      LibFiles = l + ' ' + LibFiles
   for lib in cfg['ADDONES']:
      for fli in cfg['ADDONES'][lib]:
         ones = lib+'.'+path.splitext(fli)[0]
         if not ones in HOMERES: raise Exception([{'name':'createPydFiles','msg':'You may have forgotten to rescan for: '+ones+' not in HOMERES ('+repr(HOMERES.keys())}])
         for one in HOMERES[ones]['deps'][:len(HOMERES[ones]['deps'])-1]:
            l = path.join(cfg['MODULES'][one]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ycfg+sep+'lib'),one+'4'+ones+cfg['SYSTEM']['sfx_lib'])
            if not path.exists(l): raise Exception([{'name':'createPydFiles','msg':'Added library missing:\n      '+l}])
            LibFiles = l + ' ' + LibFiles
         l = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ycfg+sep+'lib'),ones+cfg['SYSTEM']['sfx_lib'])
         if not path.exists(l): raise Exception([{'name':'createPydFiles','msg':'Library missing:\n      '+l}])
         LibFiles = l + ' ' + LibFiles
   lib = HOMERES[yname]['deps'][len(HOMERES[yname]['deps'])-1].split('.')[0]
   libFile = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ycfg+sep+'lib'),yprog+cfg['SYSTEM']['sfx_lib'])
   if not path.exists(libFile): raise Exception([{'name':'createPydFiles','msg':'Library missing:\n      '+libFile}])

   # ~~ Add external libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'libs' in cfg['MODULES'][ymdul]: LibFiles = LibFiles + cfg['MODULES'][ymdul]['libs']

   # ~~ Lists local objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for obj,lib in HOMERES[yname]['add']:
      Root,Suffix = path.splitext(path.basename(obj))
      if lib == ymdul and obj.lower()+cfg['SYSTEM']['sfx_obj'] not in ObjFiles.split():
         o = Root.lower()+cfg['SYSTEM']['sfx_obj']
         if not path.exists(o): raise Exception([{'name':'createPydFiles','msg':'Object missing:\n      '+o}])
         ObjFiles = ObjFiles + o + ' '
   #if ObjFiles.strip() == '' and path.exists(ExeFile): return True
   for obj,lib in HOMERES[yname]['tag']:
      if lib == ymdul and (path.basename(obj)).lower()+cfg['SYSTEM']['sfx_obj'] not in ObjFiles.split():
         o = (path.basename(obj)).lower()+cfg['SYSTEM']['sfx_obj']
         if not path.exists(o): raise Exception([{'name':'createPydFiles','msg':'Object missing:\n      '+o}])
         ObjFiles = ObjFiles + o +' '
   # ~~ Lists local files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # ~~ is executable necessary ? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if path.exists(PydFile):
      if cfg['COMPILER']['REBUILD'] > 0 and cfg['COMPILER']['REBUILD'] < 3: remove(PydFile)
   if path.exists(PydFile):
      if cfg['COMPILER']['REBUILD'] > 2 or cfg['COMPILER']['REBUILD'] == 0:
         refresh = False
         for o in ObjFiles.split(): refresh = refresh or ( isNewer(o,PydFile) == 0 )
         for l in LibFiles.split():
            # Only checks the telemac libraries
            if l.find(cfg['root']+sep+'builds'+sep+ycfg+sep+'lib') != -1:
               refresh = refresh or ( isNewer(l,PydFile) == 0 )
         if refresh: remove(PydFile)
   if path.exists(PydFile): return True

   # ~~ creation of the exe (according to makefile.wnt + systel.ini):
   # ~~ xilink.exe /stack:536870912 /out:postel3dV5P9.exe declarations_postel3d.obj coupeh.obj lecdon_postel3d.obj postel3d.obj coupev.obj lecr3d.obj pre2dh.obj pre2dv.obj ecrdeb.obj nomtra.obj homere_postel3d.obj point_postel3d.obj ..\..\..\bief\bief_V5P9\1\biefV5P9.lib ..\..\..\damocles\damo_V5P9\1\damoV5P9.lib ..\..\..\paravoid\paravoid_V5P9\1\paravoidV5P9.lib ..\..\..\special\special_V5P9\1\specialV5P9.lib
   cmd = cmd.replace('<libs>',LibFiles.strip())
   cmd = cmd.replace('<objs>',ObjFiles.strip())

   #xocmd = cfg['MODULES'][ymdul]['xobj']
   #xocmd = xocmd.replace('<incs>',cfg['MODULES'][ymdul]['incs'])
   cmd = cmd.replace('<incs>',cfg['MODULES'][ymdul]['incs'].strip())
   mods = ''
   for mod in HOMERES[yname]['deps'][:-1]:
      mods = cfg['MODULES'][ymdul]['mods'].replace('<config>',cfg['MODULES'][mod.split('.')[0]]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ycfg+sep+'lib')) + ' ' + mods
   cmd = cmd.replace('<mods>',mods.strip())
   #xocmd = xocmd.replace('<mods>',mods)
   #xocmd = xocmd.replace('<config>',LibDir).replace('<root>',cfg['root'])

   LibFiles = libFile + ' ' + LibFiles
   #xecmd = cfg['MODULES'][ymdul]['xexe']
   #xecmd = xecmd.replace('<libs>',LibFiles)
   # Special keyword for nag with ',' separating the libraries
   #xecmd = xecmd.replace('<libsnag>',LibFiles.replace(' ',','))
   # <pydname> and <objs> ... still to be replaced
   #xecmd = xecmd.replace('<config>',LibDir).replace('<root>',cfg['root'])

   if debug : print cmd
   try:
      tail,code = mes.runCmd(cmd,False)
      #if tail != '':
      #   if path.exists(ObjCmd): remove(ObjCmd)
      #   if path.exists(ExeCmd): remove(ExeCmd)
   except Exception as e:
      #if path.exists(ObjCmd): remove(ObjCmd)
      #if path.exists(ExeCmd): remove(ExeCmd)
      raise Exception([filterMessage({'name':'createExeFiles','msg':'Could not link your executable. Please verify your external library installation or the python script itself.'},e,bypass)])
   if code != 0:
      #if path.exists(ObjCmd): remove(ObjCmd)
      #if path.exists(ExeCmd): remove(ExeCmd)
      raise Exception([{'name':'createExeFiles','msg':'something went wrong, I am not sure why (runcode='+str(code)+').\n      '+tail}])
   print '   - created ' + PydFile.replace(path.dirname(cfg['root']),'...')

   # ~~> Make the keys portable (no full path)
   #for k in cfg['TRACE']:
   #   xocmd = xocmd.replace(cfg['TRACE'][k],'['+k+']')
   #   xecmd = xecmd.replace(cfg['TRACE'][k],'['+k+']')
   #putFileContent(ObjCmd,[xocmd])
   #putFileContent(ExeCmd,[xecmd])

   return False

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   debug = False
   BYPASS = True  # /!\ Temporary bypass for subroutine within programs

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Compile the TELEMAC system:\n
1. rescan the tree dependencies if necessary
2. check which files need re-compilation
3. create object files, libraries, executable, and other binaries
   depending on your configuration settings
Work with all active configurations.
      '''))
   parser.add_argument(\
      "-c", "--configname",metavar="config name",
      dest="configName",default='',
      help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_argument(\
      "-f", "--configfile",metavar="config file",
      dest="configFile",default='',
      help="specify configuration file, default is systel.cfg" )
   parser.add_argument(\
      "-r", "--rootdir",metavar="TELEMAC root",
      dest="rootDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_argument(\
      "-m", "--modules",metavar="modules",
      dest="modules",default='',
      help="specify the list modules, default is taken from config file" )
   parser.add_argument(\
      "-b","--bypass",action="store_true",
      dest="bypass",default=False,
      help="will bypass execution failures and try to carry on (final report at the end)" )
   parser.add_argument(\
      "--rescan",action="store_true",
      dest="rescan",default=False,
      help="will redo the scan of sources for an update of all the cmdf files" )
   parser.add_argument(\
      "--clean",action="store_true",
      dest="cleanup",default=False,
      help="will erase all object, executable libraries from folder on the selected configs/modules" )
   parser.add_argument(\
      "-j",type=int,
      dest="ncsize",default=0,
      help="set the number of core used for the parallel compilation of objects" )
   options = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # The path to the root relates to the script launched, which implies
   # that the user environment knows which to run
   # (this script is stored under .../scripts/python27/)
   #PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   PWD = path.dirname(path.dirname( path.dirname(__file__)) )
   if options.rootDir != '': PWD = path.abspath(options.rootDir)
   # The path to the python scripts is defined by the script launched
   PYT = path.dirname(__file__)

   # user configuration name
   USETELCFG = ''
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   if options.configName == '': options.configName = USETELCFG
   # user configuration file
   SYSTELCFG = path.join(PWD,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if options.configFile != '': SYSTELCFG = options.configFile
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   options.configFile = SYSTELCFG

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ banners ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mes = MESSAGES()  # runcode takes its version number from the CAS file
   svnrev = ''
   svnurl = ''
   try:
      key_equals = re.compile(r'(?P<key>[^:]*)(?P<after>.*)',re.I)
      tail,code = mes.runCmd('svn info '+PWD,True)
      for line in tail.split('\n'):
         proc = re.match(key_equals,line)
         if proc:
            if proc.group('key').strip() == 'Revision': svnrev = proc.group('after')[1:].strip()
            if proc.group('key').strip() == 'URL': svnurl = proc.group('after')[1:].strip()
   except:
      pass
   if svnrev+svnurl == '':
      print '\n'.join(banner('unknown revision'))
      print '      (the command svn is not installed, or your local copy is not linked to the repository)\n\n'
   else:
      if svnurl != '': print '\n'.join(banner(svnurl.split('/')[-1]))
      if svnrev != '': print '\n'.join(banner('rev. #'+svnrev))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         _, _, filenames = walk(dircfg).next()
         for fle in filenames :
            head,tail = path.splitext(fle)
            if tail == '.cfg' :
               print '    +> ',fle
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)
   for cfgname in cfgs:
      # still in lower case
      if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
      cfgs[cfgname]['pytel'] = PYT
      if options.rootDir != '': cfgs[cfgname]['root'] = PWD
      if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
      # cleaning up
      if options.cleanup:
         try: cleanConfig({'root':cfgs[cfgname]['root']},cfgname)
         except Exception as e:
           xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> could not clean up your configuration: '+ cfgname},e,options.bypass)])

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Separately dealing with rescan for all configs ? ~~~~~~~~~~~~
   for cfgname in cfgs:

      print '\n\n'+'\n'.join(banner(cfgname))
      print 'Scanning the source code for:\n'+'~'*72+'\n'
      print '    +> configuration: ' +  cfgname
      if 'brief' in cfgs[cfgname]: print '\n    +> '+'\n    |  '.join(cfgs[cfgname]['brief'].split('\n')) + '\n'
      print '    +> root:          ' +  cfgs[cfgname]['root']
      print '    +> modules:       ' +  cfgs[cfgname]['modules']
      print '\n\n'+'~'*72+'\n'

      # parsing for proper naming
      try: cfg = parseConfig_CompileTELEMAC(cfgs[cfgname],options.cleanup and options.rescan,options.bypass)
      except Exception as e:
         xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> could not parse your system and associated configuration: '+ cfgname},e,options.bypass)])
      # Only if we ask for a scan
      if options.rescan:
# ~~ Scans all source files to build a relation database ~~~~~~~~~~~
         # TODO: parallelistaion of the scanSources
         fic,mdl,sbt,fct,prg,top,odd,whocallswho = scanSources(cfgname,cfg,BYPASS)

# ~~ Builds the Call Tree for each tree top ~~~~~~~~~~~~~~~~~~~~~~~~
         HOMERES = {}
         print '\nUpdating your cmdf file for compilation without scan\n'+'~'*72+'\n'
         # ~~> top
         #   has the shape of a dictionary file, where the entry key is the name
         #   of a main program (e.g. SPLITSEL, HOMERE_PARTEL, GREDELMET_AUTOP, ...)
         #   and where the value are single cell array with the name of the lib
         #   the main program depends upon (e.g. ['splitsel'], ['partel'], ['gretel'], ...)
         for item in top:
            # TODO: check whether you can expect more than one element in top[item]
            for ones in top[item]:
               mod = ones.split('.')[0]
               # ~~> filtering unwanted modules
               if mod not in cfg['COMPILER']['MODULES']: continue
               # ~~> for each of those, you are expected to write a cmdf file,
               #   which will be used to compile that particular top of the tree
               #   and its dependencies.
               print '      +> '+item,'(',ones,')'

# ~~ Builds the Call Tree for each main program ~~~~~~~~~~~~~~~~~~~~
               debug = False; rebuild = cfg['COMPILER']['REBUILD']
               MAKSYSTEL = {'add':[],'tag':[],'deps':[]}
               MAKSYSTEL['deps'] = trimTree(item,ones,whocallswho,rebuild)
               HOMERES.update({ones:MAKSYSTEL})
# ~~ Prepare the cmdf file to avoid future scans ~~~~~~~~~~~~~~~~~~~
               ForDir = whocallswho[ones]['path'] #cfg['MODULES'][mod]['path']
               if mod in cfg['ADDONES']:
                  ForDir = whocallswho[ones]['path']
                  ForCmd = path.join(ForDir,ones + '.cmdf')
               elif 'homere' in item.lower() or 'systeme' in item.lower():
                  ForCmd = path.join(ForDir,'_'.join((item.lower()).split('_')[1:]) + '.cmdf')
               else:
                  ForCmd = path.join(ForDir,item.lower() + '.cmdf')
               # /!\ 'name' defines the name of the executable
               FileList = {'general':{'path':whocallswho[ones]['path'],'name':path.splitext(path.basename(ForCmd))[0],'module':mod,'liborder':MAKSYSTEL['deps']}}
               for obj,lib in HOMERES[ones]['add']:
                  try:
                     fic = whocallswho[lib][path.splitext(path.basename(obj.replace('|',sep)))[0].upper()]
                  except  Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> missmatch between Fortran name and file name for: '+path.splitext(obj)[0].upper()},e,options.bypass)])
                  if not lib in FileList: FileList.update({lib:{'path':fic['path'],'files':[]}})
                  FileList[lib]['files'].append(fic['file'])
               for obj,lib in HOMERES[ones]['tag']:
                  try:
                     fic = whocallswho[lib][path.splitext(path.basename(obj.replace('|',sep)))[0].upper()]
                  except  Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> missmatch between Fortran name and file name for: '+path.splitext(obj)[0].upper()},e,options.bypass)])
                  if not FileList: FileList.update({lib:{'path':fic['path'],'files':[]}})
                  FileList[lib]['files'].append(fic['file'])
               if not path.exists(ForCmd) or rebuild == 2 or options.cleanup: putScanContent(ForCmd,cfg['root'],FileList)
               else:
                  FixeList = getScanContent(ForCmd,cfg['root'],options.bypass)
                  # ~~> check the update for new libraries
                  if FileList['general']['liborder'] != FixeList['general']['liborder']:
                     FixeList['general']['liborder'] = FileList['general']['liborder']
                     print '         The number of elements linked together has changed: ' + ' | '.join(FileList['general']['liborder'])
                     fixes = FixeList.keys()
                     for lib in fixes:
                        if lib == 'general': continue
                        if lib not in FileList: del FixeList[lib]
                     for lib in FileList:
                        if lib == 'general': continue
                        if lib not in FixeList: FixeList.update({lib:{'path':FileList[lib]['path'],'files':FileList[lib]['files']}})
                  # ~~> add new files
                  mes = ''
                  for lib in FileList:
                     if lib == 'general': continue
                     if lib in FixeList:
                        for fic in FileList[lib]['files']:
                           if fic not in FixeList[lib]['files']:
                              mes += '\n         ~ ' + lib + ' | ' + fic
                              FixeList[lib]['files'].append(fic)
                  if mes != '': print '         The following have been added to the CMDF file: ' + ForCmd + mes
                  # ~~> remove inexistant files
                  mes = ''
                  for lib in FixeList:
                     if lib == 'general': continue
                     fixes = FixeList[lib]['files']
                     for fix in fixes:
                        if not path.exists(path.join(FixeList[lib]['path'],fix.replace('|',sep))):
                           mes += '\n         ~ ' + lib + ' | ' + fix
                           del FixeList[lib]['files'][ FixeList[lib]['files'].index(fix) ]
                  if mes != '': print '         The following will be removed from the CMDF file: ' + ForCmd + mes
                  # ~~> put content as CMDF file
                  putScanContent(ForCmd,cfg['root'],FixeList)

         # /!\ multiple configurations will now generate multiple rescan
         # (because of tags and adds, specific to some configurations)
         # options.rescan = False
         cfg = parseConfig_CompileTELEMAC(cfgs[cfgname],False,options.bypass)

# ~~ Scans all cmdf files found in all modules ~~~~~~~~~~~~~~~~~~~~~
      cmdfFiles = {}; HOMERES = {}; found = False
      rebuild = cfg['COMPILER']['REBUILD']
      for mod in cfg['COMPILER']['MODULES']:
         cmdfFiles.update({mod:{}})
         if mod in cfg['MODULES']:
            found = found or ( cfg['MODULES'][mod]['cmdfs'] != [] )
            for cmdFile in cfg['MODULES'][mod]['cmdfs']:   # make sure the key cmdfs exists
               try: cmdf = getScanContent(path.join(cfg['MODULES'][mod]['path'],cmdFile),cfg['root'],options.bypass)
               except Exception as e:
                  xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> Scanning the cmdf file: '+path.basename(cmdFile)},e,options.bypass)])
               cmdfFiles[mod].update({cmdf['general']['name']:cmdf})
# ~~ Look whether .o older than .f ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for item in cmdfFiles[mod]:
            print '\n\nCompiling from the tree top ' + item + ' plus dependents\n'+'~'*72+'\n'
            MAKSYSTEL = {'add':[],'tag':[],'deps':cmdfFiles[mod][item]['general']['liborder'],'lib':''}
            HOMERES.update({item:MAKSYSTEL})
            for lib in MAKSYSTEL['deps']:
               ForDir = cmdfFiles[mod][item][lib]['path']
               if mod in cfg['ADDONES']:
                  HOMERES[item]['lib'] = mod
                  ForDir = cmdfFiles[mod][item][lib]['path']+sep+'.'+mod
                  createDirectories(ForDir)
               for fli in cmdfFiles[mod][item][lib]['files'] :
                  #In case the file is in a subfolder of the module replace the | that defines the separator by the os separator
                  fle = fli.replace('|',sep)
                  srcName = cmdfFiles[mod][item][lib]['path']+sep+fle           # /!\ original file
                  p = ForDir.replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+cfgname+sep+'lib')
                  createDirectories(p)
                  objName = p + sep + path.splitext(path.basename(fle))[0] + cfg['SYSTEM']['sfx_obj']
                  try:
                     if (isNewer(srcName,objName) == 1) and rebuild < 2:
                        HOMERES[item]['tag'].append((ForDir,fle,lib))
                     else:
                        if mod in cfg['ADDONES']: shutil.copy(srcName,ForDir+sep+fle)
                        HOMERES[item]['add'].append((ForDir,fle,lib))
                  except Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> Could not find the following file for compilation: '+path.basename(srcName)+'\n         ... so it may have to be removed from the following cmdf file: '+cmdFile},e,options.bypass)])
# ~~ Parallel log files
            tasks = []
            mes = MESSAGES(size=10,ncsize=options.ncsize)
# ~~ Creates modules and objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if HOMERES[item]['add'] == []: print '      +> There is no need to compile any object'
            else:
# ~~ Refactor some of the names
               if mod in cfg['ADDONES']:
                  print '      +> Refactoring the tree top ',item
                  refactorSources(HOMERES[item]['add'],cmdfFiles[mod][item],False)
               print '      +> Compile / Assemble / Link'
               ibar = 0; pbar = ProgressBar(maxval=len(HOMERES[item]['add'])).start()
               # ~~> just checking
               #print mod,HOMERES[item]['deps'],cfg['MODULES'].keys()
               #for mod in HOMERES[item]['deps']:
               #   if mod not in cfg['MODULES']:
               #      xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> dependency checks','msg':'\n ... The compilation of '+item+' depends on '+mod+' but you seem to have removed it from the "modules" of your configuration file.'})])
               #      print '\n\nHummm ... I could not complete my work.\n'+'~'*72 + '\n\n' + xcpts.exceptMessages()
               #      sys.exit(1)
               for pth,obj,lib in HOMERES[item]['add']:
                  # path is where the .o and .mod will eventually be created, for each file
                  out = createObjFiles(cfg,obj,item, \
                     {'libname':lib,'type':getPrincipalWrapNames(pth+sep+obj)[0],'path':pth}, \
                      cfgname,mes,tasks,options.bypass)
                  for x,o,e,c,m in out:
                     if e == '': pbar.write( '   - completed: ' + m,ibar )
                     else: xcpts.addMessages([{'name':'compileTELEMAC::createObjFiles:\n      +> failed: '+x+'\n'+e}])
                     ibar = ibar + 1; pbar.update(ibar)
                  if xcpts.notEmpty():
                     print '\n\nHummm ... I could not complete my work.\n'+'~'*72 + '\n\n' + xcpts.exceptMessages()
                     sys.exit(1)
               # ~~> waiting for the remaining queued jobs to complete
               out = mes.flushCmd(tasks)
               for x,o,e,c,m in out:
                  if e == '': pbar.write( '   - completed: ' + m,ibar )
                  else: xcpts.addMessages([{'name':'compileTELEMAC::createObjFiles:\n      +> failed: '+x+'\n'+e}])
                  ibar = ibar + 1; pbar.update(ibar)
               pbar.finish()
               if xcpts.notEmpty():
                  print '\n\nHummm ... I could not complete my work.\n'+'~'*72 + '\n\n' + xcpts.exceptMessages()
                  sys.exit(1)
            if item == 'HOMERE_MASCARET':
               compileMascaretDependencies(cfg,cfgname)
            sleep(1)
# ~~ Creates libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            foundLib = True
            for lib in HOMERES[item]['deps']:
               prog = item.lower()
               if 'homere_' in item.lower(): prog = prog.split('homere_')[1]
               f = createLibFiles(cfg,lib,mod,cfgname,item,prog,mes,tasks,options.bypass)
               # ~~> waiting for the remaining queued jobs to complete
               out = mes.flushCmd(tasks)
               for x,o,e,c,m in out:
                  if e == '': print '   - completed: ' + m
                  else: xcpts.addMessages([{'name':'compileTELEMAC::createLibFiles:\n      +> failed: '+x+'\n'+e}])
               if xcpts.notEmpty():
                  print '\n\nHummm ... I could not complete my work.\n'+'~'*72 + '\n\n' + xcpts.exceptMessages()
                  sys.exit(1)
               foundLib = foundLib and f
            if foundLib: print '      +> There is no need to package any library'
            sleep(1)
# ~~ Creates executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if cfg['MODULES'][mod]['xexe'] != '':
               if mod not in cfg['ADDONES'].keys() and mod not in cfg['ODDONES'].keys():
                  foundExe = True
                  try:
                     prog = item.lower()
                     if 'homere_' in prog: prog = prog.split('homere_')[1]
                     foundExe = createExeFiles(cfg,item,mod,cfgname,prog,mes,options.bypass)
                  except Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating executable: '+ item.lower()},e,options.bypass)])
                  if foundExe: print '      +> There is no need to create the associate executable'
# ~~ Creates a python module ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if cfg['MODULES'][mod]['xpyd'] != '':
               foundPyd = True
               f = ''
               for obj,lib in HOMERES[item]['add']:
                  if lib == mod+'.'+item: f = obj
               for obj,lib in HOMERES[item]['tag']:
                  if lib == mod+'.'+item: f = obj
               if f == '': xcpts.addMessages([{'name':'compileTELEMAC::createPydFiles:\n      +> failed: '+item+' not found in adds or tags of\n            '+repr(HOMERES[item])}])
               try:
                  prog = item.lower()
                  if 'homere_' in prog: prog = prog.split('homere_')[1]
                  foundPyd = createPydFiles(cfg,item,f,mod,cfgname,prog,mes,options.bypass)
               except Exception as e:
                  xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating python module: '+ item.lower()},e,options.bypass)])
               if xcpts.notEmpty():
                  print '\n\nHummm ... I could not complete my work.\n'+'~'*72 + '\n\n' + xcpts.exceptMessages()
                  sys.exit(1)
               if foundPyd: print '      +> There is no need to create the associate python module'
# ~~ End of scans for all cmdf files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if not found: xcpts.addMessages([{'name':'compileTELEMAC::main:','msg':'Could not find any cmdf file for config ' + cfgname + '. You may have to use the --rescan option'}])

      # Compiling api if asked for
      if 'api' in cfg['options']:
          compile_api(cfgs, cfgname, False)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty():
      print '\n\nHummm ... I could not complete my work.\n'\
         +'~'*72+'\n'+ xcpts.exceptMessages()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print '\n\nMy work is done\n\n'
      sys.exit(0)
