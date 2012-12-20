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
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path, sep, walk, chdir, remove, environ
import ConfigParser
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_CompileTELEMAC
from parsers.parserFortran import scanSources
# ~~> dependencies towards other pytel/modules
from utils.files import createDirectories,getFileContent,putFileContent,addFileContent,isNewer
from utils.messages import MESSAGES,filterMessage

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def getTree(name,lname,list,level,rebuild):
   # ~~ Recursive tree Build ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   level = level + 1
   # ~~ prints the tree to screen:
   #print "#  "*level + name + "  > "  + lname
   time = list[lname][name]['time']
   #if debug: print '===> use',list[lname][name]['uses']
   for use in list[lname][name]['uses']:
      libname = lname
      if list[lname].get(use) == None:
         for lib in list.keys():
            if lib != lname and list[lib].get(use) != None: libname = lib
      if list[libname].get(use) != None:
         tTree,rTree = getTree(use,libname,list,level,rebuild)
         if rebuild < 3: time = time * tTree
         if rTree > list[lname][name]['rank']: list[lname][name]['rank'] = rTree
   #if debug: print '===> call',list[lname][name]['calls']
   for call in list[lname][name]['calls'].keys():
      libname = lname
      if list[lname].get(call) == None:
         for lib in list.keys():
            if lib != lname and list[lib].get(call) != None:
               libname = lib
      if list[libname].get(call) != None:
         tTree,rTree = getTree(call.strip(),libname,list,level,rebuild)
         if rebuild < 3: time = time * tTree
         if rTree > list[lname][name]['rank']: list[lname][name]['rank'] = rTree
   #if debug and list[lname][name]['functions'] != []: print '===> fcts',name,list[lname][name]['functions']
   for function in list[lname][name]['functions']:
      libname = lname
      if list[lname].get(function) == None:
         for lib in list.keys():
            if lib != lname and list[lib].get(function) != None:
               libname = lib
      if list[libname].get(function) != None:
         tTree,rTree = getTree(function.strip(),libname,list,level,rebuild)
         if rebuild < 3: time = time * tTree
         if rTree > list[lname][name]['rank']: list[lname][name]['rank'] = rTree
   list[lname][name]['time'] = time
   rank = list[lname][name]['rank']
   if lname not in MAKSYSTEL['deps'].keys(): MAKSYSTEL['deps'].update({lname:rank})
   elif rank>MAKSYSTEL['deps'][lname]: MAKSYSTEL['deps'][lname]=rank
   #if MAKSYSTEL.get(rank) == None: MAKSYSTEL.update({rank:{'add':[],'tag':[]}})
   if time == 0:
      #if [name,lname] not in MAKSYSTEL[rank]['add']: MAKSYSTEL[rank]['add'].append([name,lname])
      if [list[lname][name]['file'],lname] not in MAKSYSTEL['add']: MAKSYSTEL['add'].append([list[lname][name]['file'],lname])
   else:
      #if [name,lname] not in MAKSYSTEL[rank]['tag']: MAKSYSTEL[rank]['tag'].append([name,lname])
      if [name,lname] not in MAKSYSTEL['tag']: MAKSYSTEL['tag'].append([name,lname])
   #print "|  "*rank + name + "  > "  + lname
   #ndu print to check 0s and 1s print "|  "*level + name + '  (' + str(time) + ')'

   return list[lname][name]['time'],rank+1


def putScanContent(file,content):
   lines = []
   if content.has_key('general'):
      lines.append('[general]'+'\n'+'path: '+content['general']['path']+'\n'+'module: '+content['general']['module'])
      lines.append('liborder: '+' '.join(content['general']['liborder']))
      lines.append('version: '+content['general']['version']+'\n'+'name: '+content['general']['name'])
   for lib in content.keys():
      if lib == 'general': continue
      lines.append('\n['+lib+']'+'\n'+'path: '+content[lib]['path']+'\n'+'files: '+'\n  '.join(content[lib]['files']))
   putFileContent(file,lines)
   return

def getScanContent(file,bypass):
   content = {}
   # ~~ Read Configuration File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgfile = ConfigParser.RawConfigParser()
   try:
      cfgfile.read(file)
   except Exception as e:
      raise Exception([filterMessage({'name':'getScanContent','msg':'Could not read the required parameters in the cmdf-scan file: '+file},e,bypass)])
   # ~~ Read General ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   try:
      general = dict(cfgfile.items('general'))
   except Exception as e:
      raise Exception([filterMessage({'name':'getScanContent','msg':'Could not find the general section in the cmdf-scan file: '+file},e,bypass)])
   if not general.has_key('path'): raise Exception([{'name':'getScanContent','msg':'Could not find the key path in the general section of the cmdf-scan file:'+file}])
   if not general.has_key('module'): raise Exception([{'name':'getScanContent','msg':'Could not find the key module in the general section of the cmdf-scan file:'+file}])
   if not general.has_key('liborder'): raise Exception([{'name':'getScanContent','msg':'Could not find the key liborder in the general section of the cmdf-scan file:'+file}])
   if not general.has_key('version'): raise Exception([{'name':'getScanContent','msg':'Could not find the key version in the general section of the cmdf-scan file:'+file}])
   if not general.has_key('name'): raise Exception([{'name':'getScanContent','msg':'Could not find the key name in the general section of the cmdf-scan file:'+file}])
   general['liborder'] = general['liborder'].split()
   content.update({'general':general})
   # ~~ Filter all libraries and files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for lib in cfgfile.sections():
      if lib == 'general': continue
      if lib not in content['general']['liborder']: raise Exception([{'name':'getScanContent','msg':'Found a section not in the [general] liborder key ' + lib + ' of the cmdf-scan file:'+file}])
      content.update({lib:dict(cfgfile.items(lib))})
      if not content[lib].has_key('path'): raise Exception([{'name':'getScanContent','msg':'Could not find the key path in the section ' + lib + ' of the cmdf-scan file:'+file}])
      if not content[lib].has_key('files'): raise Exception([{'name':'getScanContent','msg':'Could not find the key files in the section ' + lib + ' of the cmdf-scan file:'+file}])
      content[lib]['files'] = content[lib]['files'].replace('\n',' ').replace('  ',' ').split()
   for lib in content['general']['liborder']:
      if lib not in content.keys(): raise Exception([{'name':'getScanContent','msg':'The reference ' + lib + ' in the [general] liborder key is not defined in your cmdf-scan file:'+file}])
   return content

def createObjFiles(oname,oprog,odict,ocfg,bypass):
   # ~~ Assumes that the source filenames are in lower case ~~~~~~~~
   Root,Suffix = path.splitext(oname)

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjDir = path.join(cfg['MODULES'][odict['libname']]['path'],ocfg)
   createDirectories(ObjDir)
   chdir(ObjDir)

   # ~~ Removes exisitng objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if odict['type'] == 'M' :
      ModFile = path.join(ObjDir,Root + cfg['SYSTEM']['sfx_mod'])
      if path.exists(ModFile): remove(ModFile)
   ObjFile = path.join(ObjDir,Root + cfg['SYSTEM']['sfx_obj'])
   if path.exists(ObjFile): remove(ObjFile)

   # ~~ creation of the module (according to makefile.wnt + systel.ini):
   # ~~ ifort.exe /c /Ot /names:uppercase /convert:big_endian /extend_source:132 /include:..\..\..\postel3d\postel3d_V5P9\1 declarations_postel3d.f
   cmd = cfg['MODULES'][odict['libname']]['xobj']
   incs = cfg['MODULES'][odict['libname']]['incs']
   cmd = cmd.replace('<incs>',incs)
   mods = ''
   for mod in HOMERES[oprog]['deps']:
      mods = mods + path.join(cfg['MODULES'][odict['libname']]['mods'].replace('<config>',cfg['MODULES'][mod]['path']),ocfg) + ' '
   #mods = mods + path.join(cfg['MODULES'][odict['libname']]['mods'].replace('<config>',cfg['MODULES'][odict['libname']]['path']),ocfg) + ' '
   cmd = cmd.replace('<mods>',mods)
   cmd = cmd.replace('<f95name>',path.join(odict['path'],oname))
   cmd = cmd.replace('<config>',ObjDir).replace('<root>',cfg['root'])

   if debug : print cmd
   mes = MESSAGES(size=10)
   try:
      tail,code = mes.runCmd(cmd,bypass)
   except Exception as e:
      raise Exception([filterMessage({'name':'createObjFiles','msg':'something went wrong, I am not sure why. Please verify your compiler installation or the python script itself.'},e,bypass)])
   if code != 0: raise Exception([{'name':'createObjFiles','msg':'could not compile your FORTRAN (runcode='+str(code)+').\n      '+tail}])
   if odict['type'] == 'M': print '   - created ' + ObjFile + ' and ' + ModFile
   else: print '   - created ' + ObjFile
   odict['time'] = 1
   #and remove .f from objList
   return

def createLibFiles(lname,lcfg,lprog,bypass):
   # ~~ Assumes that all objects are in <config> ~~~~~~~~~~~~~~~~~~~
   # ~~ recreates the lib regardless ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibDir = path.join(cfg['MODULES'][lname]['path'],lcfg)
   chdir(LibDir)

   # LibFile is now created directly within prg[0]'s directory - /!\ hopefuly, the directory exists
   LibFile = path.join(path.join(cfg['MODULES'][lprog]['path'],lcfg),lname + cfg['version'] + cfg['SYSTEM']['sfx_lib'])

   # ~~ Lists all objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for obj,lib in HOMERES[item]['add'] :
      Root,Suffix = path.splitext(obj)
      if lib == lname: ObjFiles = ObjFiles + (Root.lower()+cfg['SYSTEM']['sfx_obj']+' ')
   for obj,lib in HOMERES[item]['tag'] :
      if lib == lname: ObjFiles = ObjFiles + (obj.lower()+cfg['SYSTEM']['sfx_obj']+' ')

   # ~~ is linkage necessary ? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if cfg['COMPILER']['REBUILD'] > 0 and cfg['COMPILER']['REBUILD'] < 3 and path.exists(LibFile): remove(LibFile)
   if path.exists(LibFile):
      refresh = False
      for o in ObjFiles.split(): refresh = refresh or ( isNewer(o,LibFile) == 0 ) or ( not path.exists(o) )
      if refresh: remove(LibFile)
   if path.exists(LibFile): return True

   # ~~ creation of the librairies (according to makefile.wnt + systel.ini):
   # ~~ xilink.exe -lib /nologo /out:postel3dV5P9.lib declarations_postel3d.obj coupeh.obj lecdon_postel3d.obj postel3d.obj coupev.obj lecr3d.obj pre2dh.obj pre2dv.obj ecrdeb.obj nomtra.obj homere_postel3d.obj point_postel3d.obj
   cmd = cfg['MODULES'][lname]['xlib']
   cmd = cmd.replace('<objs>',ObjFiles)
   cmd = cmd.replace('<libname>',LibFile)

   if debug : print cmd
   mes = MESSAGES(size=10)
   try:
      tail,code = mes.runCmd(cmd,bypass)
   except Exception as e:
      raise Exception([filterMessage({'name':'createLibFiles','msg':'something went wrong, I am not sure why. Please verify your compilation or the python script itself.'},e,bypass)])
   if code != 0: raise Exception([{'name':'createLibFiles','msg':'Could not pack your library (runcode='+str(code)+').\n      '+tail}])
   print '   - created ' + LibFile
   #ModDir = path.join(cfg['MODULES'][prg[item][0]]['path'],lcfg)       # moves all the libraries relevant to a TELEMAC model
   #if path.exists(path.join(ModDir,path.basename(LibFile))): remove(path.join(ModDir,path.basename(LibFile)))
   #shutil.move(LibFile,ModDir)                                     # in the cfgdir (ModDir) of that model
   return

def createExeFiles(ename,ecfg,eprog,bypass):

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ExeDir = path.join(cfg['MODULES'][eprog.lower()]['path'],ecfg)
   chdir(ExeDir)
   
   # ~~ Removes existing executables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'homere' in ename or 'systeme' in ename:
      ExeFile = path.join(ExeDir,eprog + cfg['version'] + cfg['SYSTEM']['sfx_exe'])
      LibFile = path.join(ExeDir,eprog + cfg['version'] + cfg['SYSTEM']['sfx_lib'])
      ObjCmd = path.join(ExeDir,eprog + cfg['version'] + '.cmdo')
      ExeCmd = path.join(ExeDir,eprog + cfg['version'] + '.cmdx')
   else:
      ExeFile = path.join(ExeDir,ename + cfg['SYSTEM']['sfx_exe'])
      LibFile = path.join(ExeDir,ename + cfg['SYSTEM']['sfx_lib'])
      ObjCmd = path.join(ExeDir,ename + '.cmdo')
      ExeCmd = path.join(ExeDir,ename + '.cmdx')
   #if cfg['COMPILER']['REBUILD'] > 0 and path.exists(ExeFile): remove(ExeFile)

   # ~~ Lists all system libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibFiles = ''
   for lib in HOMERES[ename.upper()]['deps'][1:]:   # /!\ [1:] to create the exe from local objs.
      #ModDir = path.join(cfg['MODULES'][eprog.lower()]['path'],ecfg)
      l = path.join(ExeDir,lib.lower()+cfg['version']+cfg['SYSTEM']['sfx_lib'])
      if not path.exists(l): raise Exception([{'name':'createExeFiles','msg':'Library missing:\n      '+l}])
      LibFiles = LibFiles + l + ' '

   # ~~ Add external libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for lib in cfg['MODULES'][eprog]['libs'].split():
      # FD@EDF : temporary removal of the following action
      # => interesting but we should also support LDFLAGS as -lpthread -lmpich -lmed...
      #if not path.exists(lib): raise Exception([{'name':'createExeFiles','msg':'External library missing:\n      '+lib}])
      LibFiles = LibFiles + lib + ' '

   # ~~ Lists local objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for obj,lib in HOMERES[ename.upper()]['add'] :
      Root,Suffix = path.splitext(obj)
      if lib == eprog and obj.lower()+cfg['SYSTEM']['sfx_obj'] not in ObjFiles.split():
         o = Root.lower()+cfg['SYSTEM']['sfx_obj']
         if not path.exists(o): raise Exception([{'name':'createExeFiles','msg':'Object missing:\n      '+o}])
         ObjFiles = ObjFiles + o + ' '
   #if ObjFiles.strip() == '' and path.exists(ExeFile): return True
   for obj,lib in HOMERES[ename.upper()]['tag'] :
      if lib == eprog and obj.lower()+cfg['SYSTEM']['sfx_obj'] not in ObjFiles.split():
         o = obj.lower()+cfg['SYSTEM']['sfx_obj']
         if not path.exists(o): raise Exception([{'name':'createExeFiles','msg':'Object missing:\n      '+o}])
         ObjFiles = ObjFiles + o +' '

   # ~~ is executable necessary ? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if path.exists(ExeFile):
      if cfg['COMPILER']['REBUILD'] > 0 and cfg['COMPILER']['REBUILD'] < 3: remove(ExeFile)
   if path.exists(ExeFile):
      if cfg['COMPILER']['REBUILD'] > 2 or cfg['COMPILER']['REBUILD'] == 0:
         refresh = False
         for o in ObjFiles.split(): refresh = refresh or ( isNewer(o,ExeFile) == 0 )
         for l in LibFiles.split(): refresh = refresh or ( isNewer(l,ExeFile) == 0 )
         if refresh: remove(ExeFile)
   if path.exists(ExeFile): return
   
   # ~~ creation of the exe (according to makefile.wnt + systel.ini):
   # ~~ xilink.exe /stack:536870912 /out:postel3dV5P9.exe declarations_postel3d.obj coupeh.obj lecdon_postel3d.obj postel3d.obj coupev.obj lecr3d.obj pre2dh.obj pre2dv.obj ecrdeb.obj nomtra.obj homere_postel3d.obj point_postel3d.obj ..\..\..\bief\bief_V5P9\1\biefV5P9.lib ..\..\..\damocles\damo_V5P9\1\damoV5P9.lib ..\..\..\paravoid\paravoid_V5P9\1\paravoidV5P9.lib ..\..\..\special\special_V5P9\1\specialV5P9.lib
   cmd = cfg['MODULES'][eprog]['xexe']
   cmd = cmd.replace('<libs>',LibFiles)
   cmd = cmd.replace('<objs>',ObjFiles)
   cmd = cmd.replace('<exename>',ExeFile).replace('<config>',ExeDir).replace('<root>',cfg['root'])
   
   xocmd = cfg['MODULES'][eprog]['xobj']
   xocmd = xocmd.replace('<incs>',cfg['MODULES'][eprog]['incs'])
   mods = ''
   for mod in HOMERES[ename.upper()]['deps']:
      mods = mods + path.join(cfg['MODULES'][eprog]['mods'].replace('<config>',cfg['MODULES'][mod]['path']),ecfg) + ' '
   xocmd = xocmd.replace('<mods>',mods)
   # <f95name> ... still to be replaced
   xocmd = xocmd.replace('<config>',ExeDir).replace('<root>',cfg['root'])
   
   xecmd = cfg['MODULES'][eprog]['xexe']
   xecmd = xecmd.replace('<libs>',LibFile + ' ' + LibFiles)
   # <exename> and <objs> ... still to be replaced
   xecmd = xecmd.replace('<config>',ExeDir).replace('<root>',cfg['root'])
   
   if debug : print cmd
   mes = MESSAGES(size=10)
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
   print '   - created ' + ExeFile
   putFileContent(ObjCmd,[xocmd])
   putFileContent(ExeCmd,[xecmd])
   return

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
   parser.add_option("-x","--noscan",
                      action="store_true",
                      dest="noscan",
                      default=False,
                      help="will bypass the scan of sources by checking only the cmdf files present" )
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
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)

   for cfgname in cfgs.keys():
      # still in lower case
      if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
      if options.version != '': cfgs[cfgname]['version'] = options.version
      if options.modules != '': cfgs[cfgname]['modules'] = options.modules
      # parsing for proper naming
      cfg = parseConfig_CompileTELEMAC(cfgs[cfgname])
      print '\n\nScanning the source code for:\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      print '    +> configuration: ' +  cfgname
      print '    +> root:          ' +  cfgs[cfgname]['root']
      print '    +> version:       ' +  cfgs[cfgname]['version']
      print '    +> modules:       ' +  cfgs[cfgname]['modules']
      print '    +> options:       ' +  cfgs[cfgname]['options'] + '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

      if options.noscan:
# ~~ Scans all cmdf files found in all modules ~~~~~~~~~~~~~~~~~~~~~
         cmdfFiles = {}; HOMERES = {} 
         for mod in cfg['COMPILER']['MODULES']:
            cmdfFiles.update({mod:{}})
            for verpath,vernames,file in walk(path.join(cfgs[cfgname]['root'],mod)) : break
            for vername in vernames:
               if cfgs[cfgname]['version'] in vername:
                  for p,d,filenames in walk(path.join(verpath,vername+sep+cfgname)) : break
                  for file in filenames:
                     if path.splitext(file)[1] == '.cmdf':
                        try: cmdf = getScanContent(path.join(p,file),options.bypass)
                        except Exception as e:
                           xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> Scanning the cmdf file: '+path.basename(file)},e,options.bypass)])
                        cmdfFiles[mod].update({cmdf['general']['name']:cmdf})
            MAKSYSTEL = {'add':[],'tag':[],'deps':cmdf['general']['liborder']}
# ~~ Look whether .o older than .f ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            for item in cmdfFiles[mod].keys():
               HOMERES.update({item:MAKSYSTEL})
               for lib in cmdfFiles[mod][item].keys():
                  if lib == 'general': continue
                  for file in cmdfFiles[mod][item][lib]['files'] :
                     srcName = cmdfFiles[mod][item][lib]['path'] + sep + file
                     objName = path.dirname(cmdfFiles[mod][item][lib]['path']) + sep + cfgname +sep + path.splitext(file)[0] + cfg['SYSTEM']['sfx_obj']
                     if isNewer(srcName,objName) == 1:
                        HOMERES[item]['tag'].append((path.splitext(file)[0],lib))
                     else:
                        HOMERES[item]['add'].append((file,lib))
# ~~ Creates modules and objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               for obj,lib in HOMERES[item]['add'] :
                  Root,Suffix = path.splitext(obj)
                  try:
                     createObjFiles(obj.lower(),item,{'libname':lib,'type':'','path':cmdfFiles[mod][item][lib]['path']},cfgname,options.bypass)
                  except Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating objects: '+path.basename(obj)},e,options.bypass)])
# ~~ Creates libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               for lib in HOMERES[item]['deps']:
                  try:
                     createLibFiles(lib,cfgname,mod,options.bypass)
                  except Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating library: '+path.basename(lib)},e,options.bypass)])
# ~~ Creates executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               try:
                  createExeFiles(item.lower(),cfgname,mod,options.bypass)
               except Exception as e:
                  xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating executable: '+ item.lower()},e,options.bypass)])

      else:
# ~~ Scans all source files to build a relation database ~~~~~~~~~~~
         fic,mdl,sbt,fct,prg,dep,all = scanSources(cfgname,cfg,BYPASS)

# ~~ Builds the Call Tree for each main program ~~~~~~~~~~~~~~~~~~~~
         HOMERES = {}
         for item in prg.keys() :
            if prg[item][0] in cfg['COMPILER']['MODULES']:
               
# ~~ Builds the Call Tree for each main program ~~~~~~~~~~~~~~~~~~~~
               print '\n\nBuilding the who calls who tree for ' + item + ' and dependents\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
               debug = False; rebuild = cfg['COMPILER']['REBUILD']
               MAKSYSTEL = {'add':[],'tag':[],'deps':{}}
               t,r = getTree(item,prg[item][0],all,0,rebuild)
               #del MAKSYSTEL['deps'][prg[item][0]]
               MAKSYSTEL['deps'] = sorted(MAKSYSTEL['deps'],key=MAKSYSTEL['deps'].get,reverse=True) # /!\ careful of MAKSYSTEL['deps'][1:]
               HOMERES.update({item:MAKSYSTEL})
# ~~ Prepare the cmdf file to avoid future scans ~~~~~~~~~~~~~~~~~~~
               print '\nUpdating your cmdf file for compilation with no scan for ' + item + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
               ForDir = path.join(cfg['MODULES'][prg[item][0]]['path'],cfgname)
               if 'homere' in item.lower() or 'systeme' in item.lower():
                  ForCmd = path.join(ForDir,prg[item][0] + cfgs[cfgname]['version'] + '.cmdf')
               else:
                  ForCmd = path.join(ForDir,item.lower() + cfgs[cfgname]['version'] + '.cmdf')
               FileList = {'general':{'path':cfg['MODULES'][prg[item][0]]['path'],'version':cfgs[cfgname]['version'],'name':item,'module':prg[item][0],'liborder':MAKSYSTEL['deps']}}
               for obj,lib in HOMERES[item]['add']:
                  fic = all[lib][path.splitext(obj)[0].upper()]
                  if not FileList.has_key(lib): FileList.update({lib:{'path':fic['path'],'files':[]}})
                  FileList[lib]['files'].append(fic['file'])
               for obj,lib in HOMERES[item]['tag']:
                  fic = all[lib][path.splitext(obj)[0].upper()]
                  if not FileList.has_key(lib): FileList.update({lib:{'path':fic['path'],'files':[]}})
                  FileList[lib]['files'].append(fic['file'])
               if True or not path.exists(ForCmd) or rebuild == 2: putScanContent(ForCmd,FileList)
               else:
                  FixeList = getScanContent(ForCmd,options.bypass)
                  for lib in FileList.keys():
                     if lib == 'general': continue
                     if FixeList.has_key(lib):
                        for fic in FileList[lib]['files']:
                           if fic not in FixeList[lib]['files']: FixeList[lib]['files'].append(fic)
                  putScanContent(ForCmd,FixeList)

# ~~ Creates modules and objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               for obj,lib in HOMERES[item]['add'] :
                  Root,Suffix = path.splitext(obj)
                  try:
                     createObjFiles(obj.lower(),item,all[lib][Root.upper()],cfgname,options.bypass)
                  except Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating objects: '+path.basename(obj)},e,options.bypass)])

# ~~ Creates libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               for lib in HOMERES[item]['deps']:
                  try:
                     createLibFiles(lib.lower(),cfgname,prg[item][0],options.bypass)
                  except Exception as e:
                     xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating library: '+path.basename(lib)},e,options.bypass)])

# ~~ Creates executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               try:
                  createExeFiles(item.lower(),cfgname,prg[item][0],options.bypass)
               except Exception as e:
                  xcpts.addMessages([filterMessage({'name':'compileTELEMAC::main:\n      +> creating executable: '+item.lower()},e,options.bypass)])

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty():
      print '\n\nHummm ... I could not complete my work.\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n' \
      + xcpts.exceptMessages()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else: print '\n\nMy work is done\n\n'

   sys.exit()
