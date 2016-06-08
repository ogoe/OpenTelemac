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
import time
from os import path, sep, walk, chdir, remove, environ
import ConfigParser
from multiprocessing.sharedctypes import Value,Array
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_CompileTELEMAC,cleanConfig
from parsers.parserFortran import scanSources,getPrincipalWrapNames
# ~~> dependencies towards other pytel/modules
from utils.files import createDirectories,putFileContent,isNewer
from utils.messages import MESSAGES,filterMessage,banner
from utils.progressbar import ProgressBar

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#
def trimTree(name,lname,lst,rebuild):
   liborder = []
   lrank = {}
   _ = getTree(name,lname,lst,[],lrank,rebuild)
   libdws = [lrank.keys()]
   while lrank != {}:
      for libdw in libdws:
         for lib in libdw:
            if lrank[lib]['up'] == [] and lib not in liborder: liborder.insert(0,lib)
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


def putScanContent(fle,root,content):
   lines = []
   if 'general' in content:
      lines.append('[general]'+'\n'+'path: '+content['general']['path'].replace(root,'<root>').replace(sep,'|')+'\n'+'module: '+content['general']['module'])
      lines.append('liborder: '+' '.join(content['general']['liborder']))
      lines.append('name: '+content['general']['name'])
   for lib in sorted(content.keys()):
      if lib == 'general': continue
      lines.append('\n['+lib+']'+'\n'+'path: '+content[lib]['path'].replace(root,'<root>').replace(sep,'|')+'\n'+'files: '+'\n  '.join(content[lib]['files']))
   putFileContent(fle,lines)
   return

def getScanContent(fle,root,bypass):
   content = {}
   if not path.exists(fle): raise Exception([{'name':'getScanContent','msg':'Could not find the cmdf-scan file: '+fle+'\n     ... you may have to use the --rescan option'}])
   # ~~ Read Configuration File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgfile = ConfigParser.RawConfigParser()
   try:
      cfgfile.read(fle)
   except Exception as e:
      raise Exception([filterMessage({'name':'getScanContent','msg':'Could not read the required parameters in the cmdf-scan file: '+fle+'\n     ... you may have to use the --rescan option'},e,bypass)])
   # ~~ Read General ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   try:
      general = dict(cfgfile.items('general'))
   except Exception as e:
      raise Exception([filterMessage({'name':'getScanContent','msg':'Could not find the general section in the cmdf-scan file: '+fle},e,bypass)])
   if not 'path' in general: raise Exception([{'name':'getScanContent','msg':'Could not find the key path in the general section of the cmdf-scan file:'+fle}])
   general['path'] = general['path'].replace('<root>',root).replace('|',sep)
   if not 'module' in general: raise Exception([{'name':'getScanContent','msg':'Could not find the key module in the general section of the cmdf-scan file:'+fle}])
   if not 'liborder' in general: raise Exception([{'name':'getScanContent','msg':'Could not find the key liborder in the general section of the cmdf-scan file:'+fle}])
   if not 'name' in general: raise Exception([{'name':'getScanContent','msg':'Could not find the key name in the general section of the cmdf-scan file:'+fle}])
   general['liborder'] = general['liborder'].split()
   content.update({'general':general})
   # ~~ Filter all libraries and files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for lib in cfgfile.sections():
      if lib == 'general': continue
      if lib not in content['general']['liborder']: raise Exception([{'name':'getScanContent','msg':'Found a section not in the [general] liborder key ' + lib + ' of the cmdf-scan file:'+fle}])
      content.update({lib:dict(cfgfile.items(lib))})
      if not 'path' in content[lib]: raise Exception([{'name':'getScanContent','msg':'Could not find the key path in the section ' + lib + ' of the cmdf-scan file:'+fle}])
      content[lib]['path'] = content[lib]['path'].replace('<root>',root).replace('|',sep)
      if not 'files' in content[lib]: raise Exception([{'name':'getScanContent','msg':'Could not find the key files in the section ' + lib + ' of the cmdf-scan file:'+fle}])
      content[lib]['files'] = content[lib]['files'].replace('\n',' ').replace('  ',' ').split()
   for lib in content['general']['liborder']:
      if lib not in content: raise Exception([{'name':'getScanContent','msg':'The reference ' + lib + ' in the [general] liborder key is not defined in your cmdf-scan file:'+fle}])
   return content

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
   mes = MESSAGES(size=10)
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

   print '   - completed: .../sources/mascaret/Deriv/adstack.c'
   HOMERES['HOMERE_MASCARET']['add'].append(('adstack.o','mascaret'))

def createObjFiles(cfg,oname,oprog,odict,ocfg,mes,tasks,bypass):
   # ~~ Assumes that the source filenames are in lower case ~~~~~~~~
   Root,Suffix = path.splitext(path.basename(oname))

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjDir = cfg['MODULES'][odict['libname']]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ocfg+sep+'lib')
   createDirectories(ObjDir)
   chdir(ObjDir)

   # ~~ Removes exisitng objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if odict['type'] == 'M' :
      ModFile = path.join(ObjDir,Root + cfg['SYSTEM']['sfx_mod'])
      if path.exists(ModFile): remove(ModFile)
   ObjFile = path.join(ObjDir,Root + cfg['SYSTEM']['sfx_obj'])
   if path.exists(ObjFile): remove(ObjFile)

   # ~~ creation of the module (according to makefile.wnt + systel.ini):
   # ~~ ifort.exe /c /Ot /names:uppercase /convert:big_endian /extend_source:132 /include:..\..\..\postel3d\1 declarations_postel3d.f
   cmd = cfg['MODULES'][odict['libname']]['xobj']
   incs = cfg['MODULES'][odict['libname']]['incs']
   cmd = cmd.replace('<incs>',incs)
   mods = ''
   for mod in HOMERES[oprog]['deps']:
      mods = mods + cfg['MODULES'][odict['libname']]['mods'].replace('<config>',cfg['MODULES'][mod]['path']).replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ocfg+sep+'lib') + ' '
   cmd = cmd.replace('<mods>',mods)
   cmd = cmd.replace('<f95name>',path.join(odict['path'],oname))
   cmd = cmd.replace('<config>',ObjDir).replace('<root>',cfg['root'])

   if debug : print cmd
   # ~~> remove gosts
   out = mes.cleanCmd(tasks)
   task = mes.startCmd( tasks,(cmd,bypass,Array('c',' '*10000),Value('i',0)),path.join(odict['path'],oname).replace(path.dirname(cfg['root']),'...') )
   if odict['type'] == 'M': out.extend( mes.flushCmd(tasks) )
   # ~~> and remove .f from objList
   odict['time'] = 1
   return out

def createLibFiles(cfg,lname,lcfg,lprog,mes,tasks,bypass):
   # ~~ Assumes that all objects are in <config> ~~~~~~~~~~~~~~~~~~~
   # ~~ recreates the lib regardless ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibDir = cfg['MODULES'][path.basename(lname)]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+lcfg+sep+'lib')
   chdir(LibDir)

   # LibFile is now created directly within prg[0]'s directory - /!\ hopefuly, the directory exists
   LibFile = path.join(cfg['MODULES'][lname]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+lcfg+sep+'lib'),lprog.lower() + cfg['SYSTEM']['sfx_lib'])

   # ~~ Lists all objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for obj,lib in HOMERES[item]['add'] :
      Root,Suffix = path.splitext(path.basename(obj))
      if lib == lname: ObjFiles = ObjFiles + (Root.lower()+cfg['SYSTEM']['sfx_obj']+' ')
   for obj,lib in HOMERES[item]['tag'] :
      if lib == lname: ObjFiles = ObjFiles + (path.basename(obj).lower()+cfg['SYSTEM']['sfx_obj']+' ')

   # ~~ is linkage necessary ? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if cfg['COMPILER']['REBUILD'] > 0 and cfg['COMPILER']['REBUILD'] < 2 and path.exists(LibFile): remove(LibFile)
   if path.exists(LibFile):
      refresh = False
      for o in ObjFiles.split(): refresh = refresh or ( isNewer(o,LibFile) == 0 ) or ( not path.exists(o) )
      if refresh: remove(LibFile)
   if path.exists(LibFile): return True

   # ~~ creation of the librairies (according to makefile.wnt + systel.ini):
   # ~~ xilink.exe -lib /nologo /out:postel3dV5P9.lib declarations_postel3d.obj coupeh.obj lecdon_postel3d.obj postel3d.obj coupev.obj lecr3d.obj pre2dh.obj pre2dv.obj ecrdeb.obj nomtra.obj homere_postel3d.obj point_postel3d.obj
   cmd = cfg['MODULES'][path.basename(lname)]['xlib']
   cmd = cmd.replace('<objs>',ObjFiles)
   cmd = cmd.replace('<libname>',LibFile)

   if debug : print cmd
   mes.startCmd( tasks,(cmd,bypass,Array('c',' '*10000),Value('i',0)),LibFile.replace(path.dirname(cfg['root']),'...') )
   return False

def createExeFiles(cfg,ename,ecfg,eprog,mes,bypass):

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibDir = cfg['MODULES'][eprog]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib')
   chdir(LibDir)
   ExeDir = cfg['root']+sep+'builds'+sep+ecfg+sep+'bin'
   createDirectories(ExeDir)
   cmd = cfg['MODULES'][eprog]['xexe']

   # ~~ Removes existing executables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'homere' in ename.lower() or 'systeme' in ename.lower():
      if '<exename>' in cfg['MODULES'][eprog]['xexe']:
         ExeFile = path.join(ExeDir,eprog + cfg['SYSTEM']['sfx_exe'])
         cmd = cmd.replace('<exename>',ExeFile).replace('<config>',LibDir).replace('<root>',cfg['root'])
      elif '<libname>' in cfg['MODULES'][eprog]['xexe']:     # dynamic or static library
         ExeFile = path.join(ExeDir,eprog + cfg['SYSTEM']['sfx_lib'])
         cmd = cmd.replace('<libname>',ExeFile).replace('<config>',LibDir).replace('<root>',cfg['root'])
      else:
         raise Exception([{'name':'createExeFiles','msg':'Your execute commande does not include a <exename> or a <libname>. I do not know what to do with it.'}])
      ObjCmd = path.join(LibDir,eprog + '.cmdo')
      ExeCmd = path.join(LibDir,eprog + '.cmdx')
   else:
      if '<exename>' in cfg['MODULES'][eprog]['xexe']:
         ExeFile = path.join(ExeDir,ename + cfg['SYSTEM']['sfx_exe'])
         cmd = cmd.replace('<exename>',ExeFile).replace('<config>',LibDir).replace('<root>',cfg['root'])
      elif '<libname>' in cfg['MODULES'][eprog]['xexe']:     # dynamic or static library
         ExeFile = path.join(ExeDir,ename + cfg['SYSTEM']['sfx_lib'])
         cmd = cmd.replace('<libname>',ExeFile).replace('<config>',LibDir).replace('<root>',cfg['root'])
      ObjCmd = path.join(LibDir,ename + '.cmdo')
      ExeCmd = path.join(LibDir,ename + '.cmdx')
   if cfg['COMPILER']['REBUILD'] > 0 and path.exists(ExeFile): remove(ExeFile)

   # ~~ Lists all system libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibFiles = ''
   for lib in HOMERES[ename.upper()]['deps'][:len(HOMERES[ename.upper()]['deps'])-1]:   # /!\ [1:] to create the exe from local objs.
      l = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib'),ename.lower()+cfg['SYSTEM']['sfx_lib'])
      if not path.exists(l): raise Exception([{'name':'createExeFiles','msg':'Library missing:\n      '+l}])
      LibFiles = l + ' ' + LibFiles
   lib = HOMERES[ename.upper()]['deps'][len(HOMERES[ename.upper()]['deps'])-1]
   libFile = path.join(cfg['MODULES'][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib'),ename.lower()+cfg['SYSTEM']['sfx_lib'])
   if not path.exists(libFile): raise Exception([{'name':'createExeFiles','msg':'Library missing:\n      '+libFile}])

   # ~~ Add external libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'libs' in cfg['MODULES'][eprog]:
      for lib in cfg['MODULES'][eprog]['libs'].split():
         # FD@EDF : temporary removal of the following action
         # => interesting but we should also support LDFLAGS as -lpthread -lmpich -lmed...
         #if not path.exists(lib): raise Exception([{'name':'createExeFiles','msg':'External library missing:\n      '+lib}])
         LibFiles = LibFiles + lib + ' '

   # ~~ Lists local objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for obj,lib in HOMERES[ename.upper()]['add'] :
      Root,Suffix = path.splitext(path.basename(obj))
      if lib == eprog and obj.lower()+cfg['SYSTEM']['sfx_obj'] not in ObjFiles.split():
         o = Root.lower()+cfg['SYSTEM']['sfx_obj']
         if not path.exists(o): raise Exception([{'name':'createExeFiles','msg':'Object missing:\n      '+o}])
         ObjFiles = ObjFiles + o + ' '
   #if ObjFiles.strip() == '' and path.exists(ExeFile): return True
   for obj,lib in HOMERES[ename.upper()]['tag'] :
      if lib == eprog and (path.basename(obj)).lower()+cfg['SYSTEM']['sfx_obj'] not in ObjFiles.split():
         o = (path.basename(obj)).lower()+cfg['SYSTEM']['sfx_obj']
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
   cmd = cmd.replace('<objs>',ObjFiles)

   xocmd = cfg['MODULES'][eprog]['xobj']
   xocmd = xocmd.replace('<incs>',cfg['MODULES'][eprog]['incs'])
   mods = ''
   for mod in HOMERES[ename.upper()]['deps']:
      mods = mods + cfg['MODULES'][eprog.lower()]['mods'].replace('<config>',cfg['MODULES'][mod]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+ecfg+sep+'lib')) + ' '
   xocmd = xocmd.replace('<mods>',mods)
   # <f95name> ... still to be replaced
   xocmd = xocmd.replace('<config>',LibDir).replace('<root>',cfg['root'])

   LibFiles = libFile + ' ' + LibFiles
   xecmd = cfg['MODULES'][eprog]['xexe']
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
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configname",
                      type="string",
                      dest="configName",
                      default='',
                      help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_option("-f", "--configfile",
                      type="string",
                      dest="configFile",
                      default='',
                      help="specify configuration file, default is systel.cfg" )
   parser.add_option("-r", "--rootdir",
                      type="string",
                      dest="rootDir",
                      default='',
                      help="specify the root, default is taken from config file" )
   parser.add_option("-m", "--modules",
                      type="string",
                      dest="modules",
                      default='',
                      help="specify the list modules, default is taken from config file" )
   parser.add_option("-b","--bypass",
                      action="store_true",
                      dest="bypass",
                      default=False,
                      help="will bypass execution failures and try to carry on (final report at the end)" )
   parser.add_option("--rescan",
                      action="store_true",
                      dest="rescan",
                      default=False,
                      help="will redo the scan of sources for an update of all the cmdf files" )
   parser.add_option("--clean",
                      action="store_true",
                      dest="cleanup",
                      default=False,
                      help="will erase all object, executable libraries from folder on the selected configs/modules" )
   parser.add_option("-j",
                      type="string",
                      dest="ncsize",
                      default='0',
                      help="set the number of core used for the parallel compilation of objects" )
   options, args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # path to the root
   PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   if options.rootDir != '': PWD = options.rootDir
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
   svnban = 'unknown revision'
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
      if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
      if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
      # parsing for proper naming
      cfg = parseConfig_CompileTELEMAC(cfgs[cfgname])
      print '\n\n'+'\n'.join(banner(cfgname))
      print 'Scanning the source code for:\n'+'~'*72+'\n'
      print '    +> configuration: ' +  cfgname
      if 'brief' in cfgs[cfgname]: print '    +> '+'\n    |  '.join(cfgs[cfgname]['brief'].split('\n'))
      print '    +> root:          ' +  cfgs[cfgname]['root']
      print '    +> modules:       ' +  cfgs[cfgname]['modules'] + '\n\n'+'~'*72+'\n'

      if options.cleanup:
         try: cleanConfig(cfg,cfgname)
         except Exception as e:
           xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> could not clean up your configuration: '+ cfgname},e,options.bypass)])

      #Liborder in the cmdf file is incorrect using fixed order instead
      #TODO: Solve order error when we compile telemac3d telemac2d is put before bief
      #DONE: the error on the order, but has to be tested -- replace LIBDEPS by MAKSYSTEL['deps']...'liborder' in the loop below
      #TODO: Tested still not working even if we rename all the duplicated variable/functions names
      #LIBDEPS = ['special', 'parallel', 'mumps', 'damocles','hermes', 'bief', \
      #           'partel', 'gretel', 'diffsel', 'splitsel', 'postel3d', 'waqtel',\
      #           'dredgesim', 'sisyphe', 'artemis', 'tomawac', 'stbtel', \
      #           'telemac2d', 'telemac3d', 'estel3d', 'mascaret', 'api', 'ad']
      # Only if we ask for a scan
      if options.rescan:
# ~~ Scans all source files to build a relation database ~~~~~~~~~~~
         fic,mdl,sbt,fct,prg,dep,all_file = scanSources(cfgname,cfg,BYPASS)

# ~~ Builds the Call Tree for each main program ~~~~~~~~~~~~~~~~~~~~
         HOMERES = {}
         print '\nUpdating your cmdf file for compilation without scan\n'+'~'*72+'\n'
         for item in prg:

            if prg[item][0] in cfg['COMPILER']['MODULES']:
               print '      +> '+item

# ~~ Builds the Call Tree for each main program ~~~~~~~~~~~~~~~~~~~~
               debug = False; rebuild = cfg['COMPILER']['REBUILD']
               MAKSYSTEL = {'add':[],'tag':[],'deps':[]}
               MAKSYSTEL['deps'] = trimTree(item,prg[item][0],all_file,rebuild)
               HOMERES.update({item:MAKSYSTEL})
# ~~ Prepare the cmdf file to avoid future scans ~~~~~~~~~~~~~~~~~~~
               ForDir = cfg['MODULES'][prg[item][0]]['path']
               if 'homere' in item.lower() or 'systeme' in item.lower():
                  ForCmd = path.join(ForDir,prg[item][0] + '.cmdf')
               else:
                  ForCmd = path.join(ForDir,item.lower() + '.cmdf')
               #TODO: Remove that loop when scan order is rectified
               #fixedLibOrder=[]
               #for lib in LIBDEPS:
               #  if lib in MAKSYSTEL['deps']: fixedLibOrder.append(lib)
               #TODO: Replace fixedLibOrder by MAKSYSTEL['deps']
               #FileList = {'general':{'path':cfg['MODULES'][prg[item][0]]['path'],'name':item,'module':prg[item][0],'liborder':fixedLibOrder}}
               FileList = {'general':{'path':cfg['MODULES'][prg[item][0]]['path'],'name':item,'module':prg[item][0],'liborder':MAKSYSTEL['deps']}}
               for obj,lib in HOMERES[item]['add']:
                  try:
                     fic = all_file[lib][path.splitext(path.basename(obj.replace('|',sep)))[0].upper()]
                  except  Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> missmatch between Fortran name and file name for: '+path.splitext(obj)[0].upper()},e,options.bypass)])
                  if not lib in FileList: FileList.update({lib:{'path':fic['path'],'files':[]}})
                  FileList[lib]['files'].append(fic['file'])
               for obj,lib in HOMERES[item]['tag']:
                  try:
                     fic = all_file[lib][path.splitext(path.basename(obj.replace('|',sep)))[0].upper()]
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

         options.rescan = False
         cfg = parseConfig_CompileTELEMAC(cfgs[cfgname])

# ~~ Scans all cmdf files found in all modules ~~~~~~~~~~~~~~~~~~~~~
      cmdfFiles = {}; HOMERES = {}; found = False
      rebuild = cfg['COMPILER']['REBUILD']
      for mod in cfg['COMPILER']['MODULES']:
         cmdfFiles.update({mod:{}})
         if mod in cfg['MODULES']:
            found = found or ( cfg['MODULES'][mod]['cmdfs'] != [] )
            for cmdFile in cfg['MODULES'][mod]['cmdfs']:   # make sure the key cmdfs exists
               try: cmdf = getScanContent(cmdFile,cfg['root'],options.bypass)
               except Exception as e:
                  xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> Scanning the cmdf file: '+path.basename(cmdFile)},e,options.bypass)])
               cmdfFiles[mod].update({cmdf['general']['name']:cmdf})
# ~~ Look whether .o older than .f ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for item in cmdfFiles[mod]:
            print '\n\nCompiling the program ' + item + ' and dependents\n'+'~'*72+'\n'
            MAKSYSTEL = {'add':[],'tag':[],'deps':cmdfFiles[mod][item]['general']['liborder']}
            HOMERES.update({item:MAKSYSTEL})
            for lib in MAKSYSTEL['deps']:
               for fle in cmdfFiles[mod][item][lib]['files'] :
                  #In case the file is in a subfolder of the module replace the | that defines the separator by the os separator
                  fle = fle.replace('|',sep)
                  srcName = cmdfFiles[mod][item][lib]['path']+sep+fle
                  p = cmdfFiles[mod][item][lib]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+cfgname+sep+'lib')
                  createDirectories(p)
                  objName = p + sep + path.splitext(path.basename(fle))[0] + cfg['SYSTEM']['sfx_obj']
                  try:
                     if (isNewer(srcName,objName) == 1) and rebuild < 2:
                        HOMERES[item]['tag'].append((path.splitext(fle)[0],lib))
                     else:
                        HOMERES[item]['add'].append((fle,lib))
                  except Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> Could not find the following file for compilation: '+path.basename(srcName)+'\n         ... so it may have to be removed from the following cmdf file: '+cmdFile},e,options.bypass)])
# ~~ Parallel log files
            tasks = []
            mes = MESSAGES(size=10,ncsize=int(options.ncsize))
# ~~ Creates modules and objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if HOMERES[item]['add'] == []: print '      +> There is no need to compile any object'
            else:
               ibar = 0; pbar = ProgressBar(maxval=len(HOMERES[item]['add'])).start()
               for obj,lib in HOMERES[item]['add'] :
                  out = createObjFiles(cfg,obj,item, \
                     {'libname':lib,'type':getPrincipalWrapNames(path.join(cmdfFiles[mod][item][lib]['path'],obj))[0][0], \
                      'path':cmdfFiles[mod][item][lib]['path']}, \
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
# ~~ Creates libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            foundLib = True
            for lib in HOMERES[item]['deps']:
               f = createLibFiles(cfg,lib,cfgname,item,mes,tasks,options.bypass)
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
# ~~ Creates executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            try:
               foundExe = createExeFiles(cfg,item.lower(),cfgname,mod,mes,options.bypass)
            except Exception as e:
               xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating executable: '+ item.lower()},e,options.bypass)])
            if foundExe: print '      +> There is no need to create the associate executable'
      if not found: xcpts.addMessages([{'name':'compileTELEMAC::main:','msg':'Could not find any cmdf file for config ' + cfgname + '. You may have to use the --rescan option'}])

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty():
      print '\n\nHummm ... I could not complete my work.\n'\
      '~'*72+'\n'+ xcpts.exceptMessages()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print '\n\nMy work is done\n\n'
      sys.exit(0)
