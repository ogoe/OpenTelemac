"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from config import OptionParser,parseConfigFile, parseConfig_CompileTELEMAC
from parserFortran import scanSources
from os import path, chdir, mkdir, remove, system, environ
from utils import createDirectories,putFileContent
import shutil
import sys

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def getTree(name,lname,list,level):
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
         tTree,rTree = getTree(use,libname,list,level)
         time = time * tTree
         if rTree > list[lname][name]['rank']: list[lname][name]['rank'] = rTree
   #if debug: print '===> call',list[lname][name]['calls']
   for call in list[lname][name]['calls'].keys():
      libname = lname
      if list[lname].get(call) == None:
         for lib in list.keys():
            if lib != lname and list[lib].get(call) != None:
               libname = lib
      if list[libname].get(call) != None:
         tTree,rTree = getTree(call.strip(),libname,list,level)
         time = time * tTree
         if rTree > list[lname][name]['rank']: list[lname][name]['rank'] = rTree
   #if debug and list[lname][name]['functions'] != []: print '===> fcts',name,list[lname][name]['functions']
   for function in list[lname][name]['functions']:
      libname = lname
      if list[lname].get(function) == None:
         for lib in list.keys():
            if lib != lname and list[lib].get(function) != None:
               libname = lib
      if list[libname].get(function) != None:
         tTree,rTree = getTree(function.strip(),libname,list,level)
         time = time * tTree
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


def createObjFiles(oname,oprog,odict,ocfg):
   # ~~ Assumes that the source filenames are in lower case ~~~~~~~~
   Root,Suffix = path.splitext(oname)

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   chdir(odict['path'])
   ObjDir = path.join(path.dirname(odict['path']),ocfg)
   createDirectories(ObjDir)

   # ~~ Removes exisitng objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if odict['type'] == 'M' :
      ModFile = path.join(ObjDir,Root + cfgs[ocfg]['SYSTEM']['SFX_MOD'])
      if path.exists(ModFile): remove(ModFile)
   ObjFile = path.join(ObjDir,Root + cfgs[ocfg]['SYSTEM']['SFX_OBJ'])
   if path.exists(ObjFile): remove(ObjFile)

   # ~~ creation of the module (according to makefile.wnt + systel.ini):
   # ~~ ifort.exe /c /Ot /names:uppercase /convert:big_endian /extend_source:132 /include:..\..\..\postel3d\postel3d_V5P9\1 declarations_postel3d.f
   cmd = cfgs[ocfg]['COMPILER']['CMD_OBJ']
   incs = cfgs[ocfg]['MODULES'][odict['libname']]['incs']
   cmd = cmd.replace('<incs>',incs)
   mods = ''
   for mod in HOMERES[oprog]['deps']:
      mods = mods + path.join(cfgs[ocfg]['MODULES'][odict['libname']]['mods'].replace('<config>',cfgs[ocfg]['MODULES'][mod]['path']),ocfg) + ' '
   #mods = mods + path.join(cfgs[ocfg]['MODULES'][odict['libname']]['mods'].replace('<config>',cfgs[ocfg]['MODULES'][odict['libname']]['path']),ocfg) + ' '
   cmd = cmd.replace('<mods>',mods)
   cmd = cmd.replace('<f95name>',oname)
   cmd = cmd.replace('<config>',ObjDir).replace('<root>',cfgs[ocfg]['TELDIR'])

   if debug : print cmd
   failure = system(cmd)
   if not failure:
      if odict['type'] == 'M' :
         print '   - created ' + ObjFile + ' and ' + ModFile
         shutil.move(Root.lower()+cfgs[ocfg]['SYSTEM']['SFX_OBJ'],ObjDir)
         shutil.move(Root.lower()+cfgs[ocfg]['SYSTEM']['SFX_MOD'],ObjDir)
      else :
         print '   - created ' + ObjFile
         shutil.move(Root.lower()+cfgs[ocfg]['SYSTEM']['SFX_OBJ'],ObjDir)
      #change time
      #and remove .f from objList
      return True
   else: return False

def createLibFiles(lname,lcfg,lprog):
   # ~~ Assumes that all objects are in <config> ~~~~~~~~~~~~~~~~~~~
   # ~~ recreates the lib regardless ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibDir = path.join(cfgs[lcfg]['MODULES'][lname]['path'],lcfg)
   chdir(LibDir)

   # LibFile is now created directly within prg[0]'s directory - /!\ hopefuly, the directory exists
   LibFile = path.join(path.join(cfgs[lcfg]['MODULES'][lprog]['path'],lcfg),lname + cfgs[lcfg]['TELVER'] + cfgs[lcfg]['SYSTEM']['SFX_LIB'])
   if cfgs[lcfg]['COMPILER']['REBUILD'] > 0 and path.exists(LibFile): remove(LibFile)

   # ~~ Lists all objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for obj,lib in HOMERES[item]['add'] :
      Root,Suffix = path.splitext(obj)
      if lib == lname: ObjFiles = ObjFiles + (Root.lower()+cfgs[lcfg]['SYSTEM']['SFX_OBJ']+' ')
   if ObjFiles.strip() == '' and path.exists(LibFile): return True
   for obj,lib in HOMERES[item]['tag'] :
      if lib == lname: ObjFiles = ObjFiles + (obj.lower()+cfgs[lcfg]['SYSTEM']['SFX_OBJ']+' ')

   # ~~ creation of the librairies (according to makefile.wnt + systel.ini):
   # ~~ xilink.exe -lib /nologo /out:postel3dV5P9.lib declarations_postel3d.obj coupeh.obj lecdon_postel3d.obj postel3d.obj coupev.obj lecr3d.obj pre2dh.obj pre2dv.obj ecrdeb.obj nomtra.obj homere_postel3d.obj point_postel3d.obj
   cmd = cfgs[lcfg]['COMPILER']['CMD_LIB']
   cmd = cmd.replace('<objs>',ObjFiles)
   cmd = cmd.replace('<libname>',LibFile)

   if debug : print cmd
   failure = system(cmd)
   if not failure:
      print '   - created ' + LibFile
      #ModDir = path.join(cfgs[lcfg]['MODULES'][prg[item][0]]['path'],lcfg)       # moves all the libraries relevant to a TELEMAC model
      #if path.exists(path.join(ModDir,path.basename(LibFile))): remove(path.join(ModDir,path.basename(LibFile)))
      #shutil.move(LibFile,ModDir)                                     # in the cfgdir (ModDir) of that model
      return True
   else: return False

def createExeFiles(ename,ecfg,eprog):

   # ~~ Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ExeDir = path.join(cfgs[ecfg]['MODULES'][eprog.lower()]['path'],ecfg)
   chdir(ExeDir)
   
   # ~~ Removes existing executables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'homere' in ename:
      ExeFile = path.join(ExeDir,eprog + cfgs[ecfg]['TELVER'] + cfgs[ecfg]['SYSTEM']['SFX_EXE'])
      LibFile = path.join(ExeDir,eprog + cfgs[ecfg]['TELVER'] + cfgs[ecfg]['SYSTEM']['SFX_LIB'])
      ObjCmd = path.join(ExeDir,eprog + cfgs[ecfg]['TELVER'] + '.cmdo')
      ExeCmd = path.join(ExeDir,eprog + cfgs[ecfg]['TELVER'] + '.cmdx')
   else:
      ExeFile = path.join(ExeDir,ename + cfgs[ecfg]['SYSTEM']['SFX_EXE'])
      LibFile = path.join(ExeDir,ename + cfgs[ecfg]['SYSTEM']['SFX_LIB'])
      ObjCmd = path.join(ExeDir,ename + '.cmdo')
      ExeCmd = path.join(ExeDir,ename + '.cmdx')
   if cfgs[ecfg]['COMPILER']['REBUILD'] > 0 and path.exists(ExeFile): remove(ExeFile)

   # ~~ Lists all libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LibFiles = ''
   for lib in HOMERES[ename.upper()]['deps'][1:]:   # /!\ [1:] to create the exe from local objs.
      #ModDir = path.join(cfgs[ecfg]['MODULES'][eprog.lower()]['path'],ecfg)
      LibFiles = LibFiles + path.join(ExeDir,lib.lower()+cfgs[ecfg]['TELVER']+cfgs[ecfg]['SYSTEM']['SFX_LIB']) + ' '
   LibFiles = LibFiles + ' ' + cfgs[ecfg]['MODULES'][eprog]['libs']     # parallel executable: adding MPI library

   # ~~ Lists local objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ObjFiles = ''
   for obj,lib in HOMERES[ename.upper()]['add'] :
      Root,Suffix = path.splitext(obj)
      if lib == eprog and obj.lower()+cfgs[ecfg]['SYSTEM']['SFX_OBJ'] not in ObjFiles.split(): ObjFiles = ObjFiles + (Root.lower()+cfgs[ecfg]['SYSTEM']['SFX_OBJ']+' ')
   if ObjFiles.strip() == '' and path.exists(ExeFile): return True
   for obj,lib in HOMERES[ename.upper()]['tag'] :
      if lib == eprog and obj.lower()+cfgs[ecfg]['SYSTEM']['SFX_OBJ'] not in ObjFiles.split(): ObjFiles = ObjFiles + (obj.lower()+cfgs[ecfg]['SYSTEM']['SFX_OBJ']+' ')

   # ~~ creation of the exe (according to makefile.wnt + systel.ini):
   # ~~ xilink.exe /stack:536870912 /out:postel3dV5P9.exe declarations_postel3d.obj coupeh.obj lecdon_postel3d.obj postel3d.obj coupev.obj lecr3d.obj pre2dh.obj pre2dv.obj ecrdeb.obj nomtra.obj homere_postel3d.obj point_postel3d.obj ..\..\..\bief\bief_V5P9\1\biefV5P9.lib ..\..\..\damocles\damo_V5P9\1\damoV5P9.lib ..\..\..\paravoid\paravoid_V5P9\1\paravoidV5P9.lib ..\..\..\special\special_V5P9\1\specialV5P9.lib
   cmd = cfgs[ecfg]['COMPILER']['CMD_EXE']
   cmd = cmd.replace('<libs>',LibFiles)
   cmd = cmd.replace('<objs>',ObjFiles)
   cmd = cmd.replace('<exename>',ExeFile).replace('<config>',ExeDir).replace('<root>',cfgs[ecfg]['TELDIR'])

   xocmd = cfgs[ecfg]['COMPILER']['CMD_OBJ']
   xocmd = xocmd.replace('<incs>',cfgs[ecfg]['MODULES'][eprog]['incs'])
   mods = ''
   for mod in HOMERES[ename.upper()]['deps']:
      mods = mods + path.join(cfgs[ecfg]['MODULES'][eprog]['mods'].replace('<config>',cfgs[ecfg]['MODULES'][mod]['path']),ecfg) + ' '
   xocmd = xocmd.replace('<mods>',mods)
   # <f95name> ... still to be replaced
   xocmd = xocmd.replace('<config>',ExeDir).replace('<root>',cfgs[ecfg]['TELDIR'])

   xecmd = cfgs[ecfg]['COMPILER']['CMD_EXE']
   xecmd = xecmd.replace('<libs>',LibFile + ' ' + LibFiles)
   # <exename> and <objs> ... still to be replaced
   xecmd = xecmd.replace('<config>',ExeDir).replace('<root>',cfgs[ecfg]['TELDIR'])

   if debug : print cmd
   failure = system(cmd)
   if not failure:
      print '   - created ' + ExeFile
      putFileContent(ObjCmd,[xocmd])
      putFileContent(ExeCmd,[xecmd])
      return True
   else:
      if path.exists(ObjCmd): remove(ObjCmd)
      if path.exists(ExeCmd): remove(ExeCmd)
      return False

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

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
   options, args = parser.parse_args()
   for cfgname in parseConfigFile(options.configFile).keys():
      cfgs = parseConfig_CompileTELEMAC(cfgname)

      for cfg in cfgs:
# ~~ Scans all source files to build a relation database ~~~~~~~~~~~
         print '\n\nScanning the source code\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         fic,mdl,sbt,fct,prg,dep,all = scanSources(cfg,cfgs[cfg])

# ~~ Builds the Call Tree for each main program ~~~~~~~~~~~~~~~~~~~~
         HOMERES = {}
         for item in prg.keys() :
            if prg[item][0] in cfgs[cfg]['COMPILER']['MODULES']:

# ~~ Builds the Call Tree for each main program ~~~~~~~~~~~~~~~~~~~~
               print '\n\nBuilding the who calls who tree for ' + item + ' and dependents\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
               debug = False
               MAKSYSTEL = {'add':[],'tag':[],'deps':{}}
               t,r = getTree(item,prg[item][0],all,0)
               debug = True
               #del MAKSYSTEL['deps'][prg[item][0]]
               MAKSYSTEL['deps'] = sorted(MAKSYSTEL['deps'],key=MAKSYSTEL['deps'].get,reverse=True)
               HOMERES.update({item:MAKSYSTEL})

# ~~ Creates modules and objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               for obj,lib in HOMERES[item]['add'] :
                  Root,Suffix = path.splitext(obj)
                  if not createObjFiles(obj.lower(),item,all[lib][Root.upper()],cfg):
                     sys.exit()

# ~~ Creates libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               for lib in HOMERES[item]['deps']:
                  if not createLibFiles(lib.lower(),cfg,prg[item][0]):
                     sys.exit()

# ~~ Creates executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               if not createExeFiles(item.lower(),cfg,prg[item][0]):
                  sys.exit()
