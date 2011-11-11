#!/usr/bin/env python
"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
"""@details
   A new TELEMAC config file has been designed to be more generic
   while simplifying the user input when dealing with Compilers,
   Zippers and other platform specific programs.
   The same TELEMAC config file is used to either Compile or Run
   the TELEMAC system, on a specific platform for a specific
   compiler, or even to translate the sources or the CAS files of
   a simulation or to create the Doxygen documentation. It is a
   multi-purpose config file.

   The structure os a TELEMAC config file always starts with the
   section:
      [Configurations]
   within which only one key is expected, 'configs', the list of
   configurations to be acted upon. Here are a few instances:
       configs: wintel32s wintel32p wing9532s wintel64s
       configs: source.gb
       configs: ubunsun32p ubunsun32s ubugfor32s
   The value field of the key 'configs' is space-delimited. Each
   sub-value will then be used as a pointer to its own section in
   the same TELEMAC config file. For instance:
      [wintel32s] ...
      [source.gb] ...
      [ubugfor32s] ...
   The name of each configuration is user difined.

   The rest of the TELEMAC config file is a list of these
   configurations, each of which follows the structure defined
   here below.

   Each configuration has a number of key/values, some of which
   are mandatory while others are dependant upon the action carried
   out for that configuration. For instance, for the translation of
   the source code (from French to English) the configuration is as
   follows:
      [sources.gb]
      root:       D:\systel\v6p0r8
      version:    v6p0
      modules:    parallel artemis telemac2d
   - The key 'root' defines the directory path to the base of the
   TELEMAC system files.
   - The key 'version' defines the version number, assuming this
     number fits the template root\*\*_version\
   - the key 'module' defines the name of the modules upon which
     the action of the configuration is carried out. The module
   names that are not recognised as part of the TELEMAC system
   (following the template root\module\*_version\) are
   ignored. Three additonal names are however allowed:
   - system: which is replaced by the list of all available modules
   - clean: which trigger the action to be carried out as if it was
     the first time
   - update: which trigger the action only to update the parts
     that are necessary.
   For instance, the compilation of the entire TELEMAC system,
   removing all existing object and libraries would simply write:
      [wintel32s]
      root:       D:\systel\v6p0r8
      version:    v6p0
      modules:    clean system
   Note that the order of appearance of the names of the module has
   no bearing on the action. For the compilation of modules of the
   TELEMAC system, for instance, the python defines the true tree
   of dependencies.

   For the compilation of (parts of) the TELEMAC system, the
   following key are also required:
      cmd_obj:   ifort.exe /c /Ot /iface:cref /iface:nomixed_str_len_arg /nologo /names:uppercase /convert:big_endian /extend_source:132 <mods> <incs> <f95name>
      cmd_lib:   xilib.exe /nologo /out:<libname> <?libname?> <objs>
      cmd_exe:   xilink.exe /nologo /subsystem:console /stack:536870912 /out:<exename> <objs> <libs>
   and
      sfx_lib:         .lib
      sfx_obj:         .obj
      sfx_mod:         .mod
      sfx_exe:         .exe
   The 'cmd_' keys are commands, written in full as executed in a
   terminal windows. Within each command, a few tags are defined,
   such as <libname>, which are used by the compiler to replace by
   the appropriate value.
   The 'sfx_' keys are used to define the suffix of each type of
   file. A library would have a '.lib' extension on windows while
   having a '.a' extension on Linux, for instance. The suffix can
   also be empty, which is appropriate for executables on Linux,
   for instance, as opposed to '.exe' on windows.

   Note that sfx_zip is used to define what type of ZIPPER will be called,
   depending on the platform. So .zip will launch a windows-like zip archive
   while .gztar will launch the stadard tarball and gzip tools, if installed.

   For the compilation of (parts of) the TELEMAC system, the
   following key are optional, but there to provide flexibility:
      mods_all:        -M<config>
      libs_all:        <root>\XDMF\XdmfUtils.a
   and
      incs_parallel:   -I/usr/include/mpich2
      libs_parallel:   /usr/lib/metis-4.0/libmetis.a
      libs_sisyphe:    /usr/lib/libfmpich.so
   While the '_all' are use commonly accorss all modules, it is
   possible to also specify specific dependencies, such as
   libs_parallel, for which the libmetix only applies to the
   executables of the parallel module (partel, gretel, etc.). The
   first 4 letters, are incs, libs, or mods and define how the
   compiler should link to include, library and module files.
"""
"""@history 30/04/2011 -- Sebastien Bourban: Upgrade made to
         parseConfig_CompileTELEMAC, which does not use the GLOBAL
         variable anymore. The configuration is passed in argument.
"""
"""@history 30/04/2011 -- Sebastien Bourban: Upgrade made to
         the others parseConfig_***TELEMAC just as previously done
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#

import ConfigParser
from optparse import OptionParser
import re
from os import path, walk, listdir, environ
import sys

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#
"""
   Contains the entire configuration tree for all user configurations.
   By default the name is systel.cfg stored locally
"""
CONFIGS = {}

# _____                        _____________________________________
# ____/ Configuration Toolbox /____________________________________/
#
"""
   Read the content of the config file and extract all cfgs,
   and their key/values -- Returns a dictionary of all configs in
   the files that are highlighted in [Configurations]
"""
def getConfigs(file):
   # ~~ Read Configuration File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgfile = ConfigParser.RawConfigParser()
   try:
      cfgfile.read(file)
   except:
      parser.error("Could not access required parameters in config file")
      sys.exit()
   # ~~ Read Config Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if cfgfile.get('Configurations','configs') == '' :
      print '\nPlease specify Configurations in config file \n'
      sys.exit()
   # ~~ Loads Configurations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   configs = {}
   for cfg in cfgfile.get('Configurations','configs').split() :
      configs.update({cfg:dict(cfgfile.items(cfg))}) # convert all keys in lower case !

   return configs

"""
   Get the name of the config file from command line arguments
   and store its rough content in a dict -- Returns the dict
   set globals CONFIGS
"""
def parseConfigFile(file):
   #
   # ~~ Parse CFG File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #
   if file == '': file = options.configFile
   print "... parsing configuration file: " + file
   configDict = getConfigs(file)
   if configDict == {}:
      print '\nPlease specify configuration in config file \n'
      sys.exit()
   globals()["CONFIGS"] = configDict
   return configDict

"""
   Get the value of a key from the config cfg. Further, test if
   the key is there and if it is empty -- Return the value
"""
def getConfigKey(cfg,key,there,empty):
   # ~~ Get the root of the System ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if there and not cfg.has_key(key):
      print ('\nKey %s required in configuration \n' % (key))
      sys.exit()
   if empty and cfg[key] == '':
      print ('\nKey %s required non empty field \n' % (key))
      sys.exit()
   return cfg[key]

# _____                  ___________________________________________
# ____/ TELEMAC Toolbox /__________________________________________/
#
"""
   Extract all the information required for
   the Compilation of TELEMAC
   Requires: root, version,
   Optional: mods_, incs_, libs_, ... and options
"""
def parseConfig_CompileTELEMAC(cfg):
   #  cfg is the dictionary of either  wintel32s wintel32p wing9532s wing9532p ...
   cfgTELEMAC = {}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(cfg,'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit()
   cfgTELEMAC.update({'TELDIR':get})
   # Get telver:
   # TELEMAC version number, to build relative path names to \sources\ etc...
   # using the template ... teldir\*\*telver\
   get = getConfigKey(cfg,'version',True,True).lower()
   cfgTELEMAC.update({'TELVER':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['TELDIR'],cfgTELEMAC['TELVER'])})
   # Get libs_all: ... libs_artemis: ... mods_all: ... etc.
   # for every module in the list of modules to account for
   # specific external includes for all or each module
   for mod in cfgTELEMAC['MODULES'].keys():
      cfgTELEMAC['MODULES'][mod].update({'mods':getEXTERNALs(cfg,'mods',mod)})
      cfgTELEMAC['MODULES'][mod].update({'incs':getEXTERNALs(cfg,'incs',mod)})
      cfgTELEMAC['MODULES'][mod].update({'libs':getEXTERNALs(cfg,'libs',mod)})

   cfgTELEMAC.update({'COMPILER':{}})
   # Get modules: user list of module
   # in which 'system' means all existing modules,
   # and in which 'update' means a rebuild of the lib and exe
   # and in which 'clean' means a rebuild of the obj, lib and exe
   # and Get options: for the switches such as parallel, openmi, mumps, etc.
   get,tbd = parseUserModules(cfg,cfgTELEMAC['MODULES'])
   cfgTELEMAC['COMPILER'].update({'MODULES':get.split()})
   cfgTELEMAC['COMPILER'].update({'REBUILD':tbd})
   for mod in get.split():
      if mod not in cfgTELEMAC['MODULES'].keys(): del cfgTELEMAC['COMPILER']['MODULES'][mod]
   # Get cmd_obj: ... cmd_lib: ... cmd_exe: ...
   # the compiler dependent command lines to create obj, lib and exe
   # respectively
   get = getCOMPILER(cfg)
   cfgTELEMAC['COMPILER'].update(get)

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,False)[1:]})

   # Get system's suffixes for obj, lib, mod, and exe
   system = {}
   system.update({'SFX_OBJ':getConfigKey(cfg,'sfx_obj',True,False).lower()})
   system.update({'SFX_EXE':getConfigKey(cfg,'sfx_exe',True,False).lower()})
   system.update({'SFX_LIB':getConfigKey(cfg,'sfx_lib',True,False).lower()})
   system.update({'SFX_MOD':getConfigKey(cfg,'sfx_mod',True,False).lower()})
   cfgTELEMAC.update({'SYSTEM':system})

   return cfgTELEMAC

"""
   Extract all the information required for
   the Translation of the source code of the TELEMAC system
"""
def parseConfig_TranslateTELEMAC(cfg):
   #  cfg is either  sources.gb ...
   cfgTELEMAC = {cfg:{}}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(CONFIGS[cfg],'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit()
   cfgTELEMAC[cfg].update({'TELDIR':get})
   # Get telver:
   # TELEMAC version number, to build relative path names to \sources\ etc...
   # using the template ... teldir\*\*telver\
   get = getConfigKey(CONFIGS[cfg],'version',True,True).lower()
   cfgTELEMAC[cfg].update({'TELVER':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC[cfg].update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC[cfg]['TELDIR'],cfgTELEMAC[cfg]['TELVER'])})
   cfgTELEMAC[cfg].update({'COMPILER':{}})
   # Get cmplr_mod: user list of module
   get,tbd = parseUserModules(CONFIGS[cfg],cfgTELEMAC[cfg]['MODULES'])
   cfgTELEMAC[cfg]['COMPILER'].update({'MODULES':get.split()})
   for mod in get.split():
      if mod not in cfgTELEMAC[cfg]['MODULES'].keys(): del cfgTELEMAC[cfg]['COMPILER']['MODULES'][mod]

   return cfgTELEMAC

"""
   Extract all the information required for
   the Translation of CAS files when running TELEMAC
"""
def parseConfig_TranslateCAS(cfg):
   #  cfg is either  sources.gb ...
   cfgTELEMAC = {cfg:{}}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(CONFIGS[cfg],'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit()
   cfgTELEMAC[cfg].update({'TELDIR':get})
   # Get telver:
   # TELEMAC version number, to build relative path names to \sources\ etc...
   # using the template ... teldir\*\*telver\
   get = getConfigKey(CONFIGS[cfg],'version',True,True).lower()
   cfgTELEMAC[cfg].update({'TELVER':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC[cfg].update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC[cfg]['TELDIR'],cfgTELEMAC[cfg]['TELVER'])})
   cfgTELEMAC[cfg].update({'COMPILER':{}})
   # Get cmplr_mod: user list of module
   get,tbd = parseUserModules(CONFIGS[cfg],cfgTELEMAC[cfg]['MODULES'])
   cfgTELEMAC[cfg]['COMPILER'].update({'MODULES':get.split()})
   for mod in get.split():
      if mod not in cfgTELEMAC[cfg]['MODULES'].keys(): del cfgTELEMAC[cfg]['COMPILER']['MODULES'][mod]

   # Get tellng:
   # TELEMAC language code, to know the default language
   # tellng = 1, French; tellng = 2, English
   get = getConfigKey(CONFIGS[cfg],'language',True,True)
   cfgTELEMAC[cfg].update({'TELVER':get})

   return cfgTELEMAC

"""
   Extract all the information required for
   the Documentation of TELEMAC preparing for Doxygen
"""
def parseConfig_DoxygenTELEMAC(cfg):
   #  cfg is either  sources.gb ...
   cfgTELEMAC = {}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(cfg,'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit()
   cfgTELEMAC.update({'TELDIR':get})
   # Get telver:
   # TELEMAC version number, to build relative path names to \sources\ etc...
   # using the template ... teldir\*\*telver\
   get = getConfigKey(cfg,'version',True,True).lower()
   cfgTELEMAC.update({'TELVER':get})
   # Get destination doxydocs: ...
   get = getConfigKey(cfg,'doxydocs',True,True).lower()
   cfgTELEMAC.update({'DOXYDOCS':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['TELDIR'],cfgTELEMAC['TELVER'])})

   cfgTELEMAC.update({'COMPILER':{}})
   # Get modules: user list of module
   # in which 'system' means all existing modules,
   # and in which 'update' means an update only of the source files and tags
   # and in which 'clean' means a rebuild of all source files and tags
   # and Get options: for the switches such as parallel, openmi, mumps, etc.
   get,tbd = parseUserModules(cfg,cfgTELEMAC['MODULES'])
   cfgTELEMAC['COMPILER'].update({'MODULES':get.split()})
   cfgTELEMAC['COMPILER'].update({'REBUILD':tbd})
   for mod in get.split():
      if mod not in cfgTELEMAC['MODULES'].keys(): del cfgTELEMAC['COMPILER']['MODULES'][mod]

   return cfgTELEMAC

"""
   Extract all the information required for the Extraction
   of the relevant binaries and files for each configuration
"""
def parseConfig_CompactTELEMAC(cfg):
   #  cfg is either  sources.gb ...
   cfgTELEMAC = {}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(cfg,'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit()
   cfgTELEMAC.update({'TELDIR':get})
   # Get telver:
   # TELEMAC version number, to build relative path names to \sources\ etc...
   # using the template ... teldir\*\*telver\
   get = getConfigKey(cfg,'version',True,True).lower()
   cfgTELEMAC.update({'TELVER':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['TELDIR'],cfgTELEMAC['TELVER'])})

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,True)[1:]})

   return cfgTELEMAC

"""
   Extract all the information required for the validation
   of the relevant modules for each configuration
   The principal assumption is that the validation cases are
   either under:
     + val_root\\module\\mod_version\\rnn_*
     + teldir\\module\\mod_version\\val_root\\rnn_*
   where 'r' is the rank.
   If the 'val_root' key is not in the config, the default
   path is assumed to be based on the first option, with
   val_root = teldir\\..\\validation, i.e.
     + teldir\\..\\validation\\module\\mod_version\\rnn_*
"""
def parseConfig_ValidateTELEMAC(cfg):
   #  cfg is either  wintel32s wintel32p wing9532s wing9532p ...
   cfgTELEMAC = {}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(cfg,'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit()
   cfgTELEMAC.update({'TELDIR':get})
   # Get telver:
   # TELEMAC version number, to build relative path names to \sources\ etc..
   # using the template ... teldir\*\*telver\
   get = getConfigKey(cfg,'version',True,True).lower()
   cfgTELEMAC.update({'TELVER':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['TELDIR'],cfgTELEMAC['TELVER'])})
   # Get libs_all: ... libs_artemis: ... mods_all: ... etc.
   # for every module in the list of modules to account for
   # specific external includes for all or each module
   for mod in cfgTELEMAC['MODULES'].keys():
      cfgTELEMAC['MODULES'][mod].update({'mods':getEXTERNALs(cfg,'mods',mod)})
      cfgTELEMAC['MODULES'][mod].update({'incs':getEXTERNALs(cfg,'incs',mod)})
      cfgTELEMAC['MODULES'][mod].update({'libs':getEXTERNALs(cfg,'libs',mod)})

   cfgTELEMAC.update({'VALIDATION':{}})
   # Get ranks: user list of ranks to filter the list of validation cases
   # in which 'all' means all possible ranks,
   # in which '<n' means all ranks less than n,
   # in which 'n' means all ranks equal to n,
   # in which '>n' means all ranks greater than n,
   # where 'n', '>n', and '<n' can be combined if necessary
   val_ranks = parseValidationRanks(cfg)
   # Get validation: user list of module and there associated directories
   # in which 'system' means all existing modules,
   # and in which 'update' means a continuation, ignoring previously completed runs
   # and in which 'clean' means a re-run of all validation tests
   if not cfg.has_key('val_root'):
      val_root = path.realpath(path.join(cfgTELEMAC['TELDIR'],'validation'))
      if not path.isdir(val_root):
         print '\nNot able to find your validation set from the path: ' + val_root + '\n'
         print ' ... check the val_root key in your configuration file'
         sys.exit()
   else:
      val_root = cfg['val_root'].replace('<root>',cfgTELEMAC['TELDIR'])
   val_found = path.isdir(val_root)
   get,tbd = parseUserModules(cfg,cfgTELEMAC['MODULES'])
   cfgTELEMAC.update({'REBUILD':tbd})
   for mod in get.split():
      if mod in cfgTELEMAC['MODULES'].keys():
         if val_found:
            val_dir = cfgTELEMAC['MODULES'][mod]['path'].replace(cfgTELEMAC['TELDIR'],val_root)
         else:
            val_dir = val_root.replace('<modpath>',cfgTELEMAC['MODULES'][mod]['path'])
         if path.isdir(val_dir):
            val_mod = getFiles_ValidationTELEMAC(val_dir,val_ranks)
            if val_mod != {}:
               cfgTELEMAC['VALIDATION'].update({mod:{'path':path.realpath(val_dir)}})
               cfgTELEMAC['VALIDATION'][mod].update(val_mod)

   # Get path_parallel: for parallel option
   # the parallel dependent command line executables (partel, gretel, ...)
   get = getPARALLEL(cfg)
   if get != {}: cfgTELEMAC.update({'PARALLEL':get})
   # Get mpi_cpulist and mpi_cmdexec: for mpi option
   # .. in theory, mpi could be replaced by something else (?)
   get = getMPI(cfg)
   if get != {}: cfgTELEMAC.update({'MPI':get})

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,True)[1:]})

   # Get system's suffixes for obj, lib, mod, and exe
   system = {}
   system.update({'SFX_OBJ':getConfigKey(cfg,'sfx_obj',True,False).lower()})
   system.update({'SFX_EXE':getConfigKey(cfg,'sfx_exe',True,False).lower()})
   system.update({'SFX_LIB':getConfigKey(cfg,'sfx_lib',True,False).lower()})
   system.update({'SFX_MOD':getConfigKey(cfg,'sfx_mod',True,False).lower()})
   cfgTELEMAC.update({'SYSTEM':system})

   return cfgTELEMAC

"""
   Extract all the information required for the launch and execution
   of TELEMAC based on one CAS file, for each configuration
"""
def parseConfig_RunningTELEMAC(cfg):
   #  cfg is either  wintel32s wintel32p wing9532s wing9532p ...
   cfgTELEMAC = {}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(cfg,'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit()
   cfgTELEMAC.update({'TELDIR':get})
   # Get telver:
   # TELEMAC version number, to build relative path names to \sources\ etc...
   # using the template ... teldir\*\*telver\
   get = getConfigKey(cfg,'version',True,True).lower()
   cfgTELEMAC.update({'TELVER':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['TELDIR'],cfgTELEMAC['TELVER'])})
   # Get libs_all: ... libs_artemis: ... mods_all: ... etc.
   # for every module in the list of modules to account for
   # specific external includes for all or each module
   for mod in cfgTELEMAC['MODULES'].keys():
      cfgTELEMAC['MODULES'][mod].update({'mods':getEXTERNALs(cfg,'mods',mod)})
      cfgTELEMAC['MODULES'][mod].update({'incs':getEXTERNALs(cfg,'incs',mod)})
      cfgTELEMAC['MODULES'][mod].update({'libs':getEXTERNALs(cfg,'libs',mod)})

   get,tbd = parseUserModules(cfg,cfgTELEMAC['MODULES'])
   cfgTELEMAC.update({'REBUILD':tbd})
   # You do not need 'get' at this stage

   # Get path_parallel: for parallel option
   # the parallel dependent command line executables (partel, gretel, ...)
   get = getPARALLEL(cfg)
   if get != {}: cfgTELEMAC.update({'PARALLEL':get})
   # Get mpi_cpulist and mpi_cmdexec: for mpi option
   # .. in theory, mpi could be replaced by something else (?)
   get = getMPI(cfg)
   if get != {}: cfgTELEMAC.update({'MPI':get})

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,True)[1:]})

   # Get system's suffixes for obj, lib, mod, and exe
   system = {}
   system.update({'SFX_OBJ':getConfigKey(cfg,'sfx_obj',True,False).lower()})
   system.update({'SFX_EXE':getConfigKey(cfg,'sfx_exe',True,False).lower()})
   system.update({'SFX_LIB':getConfigKey(cfg,'sfx_lib',True,False).lower()})
   system.update({'SFX_MOD':getConfigKey(cfg,'sfx_mod',True,False).lower()})
   cfgTELEMAC.update({'SYSTEM':system})

   return cfgTELEMAC

"""
   Get the root of the validation directory and walk from there
   through the directory structure to identify modules with the template
   val_root\module_name\*\*.xml
"""
def getFiles_ValidationTELEMAC(root,ranks):
   validation = {}

   for dirpath,dirnames,filenames in walk(root) : break
   for dir in dirnames:
      if dir[0] in str(ranks):
         val = { dir:[] }
         for valpath,valnames,filenames in walk(path.join(dirpath,dir)) : break
         for file in filenames:
            if path.splitext(file)[1] == '.xml' : val[dir].append(file)
         if val[dir] != []: validation.update(val)

   return validation

"""
   Walk through the directory structure available from the root
   and identifies modules with the template
   teldir\module_name\*telver
"""
def getFolders_ModulesTELEMAC(root,dirname):
   modules = {}
   for moddir in listdir(root) :
      if not (moddir[0] == '.' or path.isfile(path.join(root,moddir))) :
         modroot = path.join(root,moddir.lower())
         if path.exists(modroot) :
            for subdir in listdir(modroot) :
               if dirname.lower() + ' ' in subdir.lower() + ' ' and not path.isfile(path.join(modroot,subdir)) :
                  dir = path.join(modroot,subdir)
                  if not modules.has_key(moddir):
                     modules.update({moddir:{'path':dir}})
   return modules

"""
   Etract links to user defined external dependencies
   whether a lib, a mod or an include following the template
   key ext_all and ext_..., with ext = mods, incs, libs
"""
def getEXTERNALs(cfgDict,ext,mod): # key ext_all and ext_..., with ext = mods, incs, libs
   # ~~ Loads External Dependencies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   extList = ''
   if cfgDict.has_key(ext+'_all'): extList = cfgDict[ext+'_all']
   if cfgDict.has_key(ext+'_'+mod): extList = extList + ' ' + cfgDict[ext+'_'+mod]
   return extList

"""
   Extract full user defined comand line
   for the compilation, linkage and execution of the sources
"""
def getCOMPILER(cfgDict):
   # ~~ Loads Compiler Commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   compiler = {}
   cmd_obj = ''
   if cfgDict.has_key('cmd_obj'): cmd_obj = cfgDict['cmd_obj']
   compiler.update({'CMD_OBJ':cmd_obj})
   cmd_lib = ''
   if cfgDict.has_key('cmd_lib'): cmd_lib = cfgDict['cmd_lib']
   compiler.update({'CMD_LIB':cmd_lib})
   cmd_exe = ''
   if cfgDict.has_key('cmd_exe'): cmd_exe = cfgDict['cmd_exe']
   compiler.update({'CMD_EXE':cmd_exe})
   return compiler

"""
   Extract full user defined comand line
   for the treatment of the option 'parallel'
"""
def getPARALLEL(cfgDict):
   # ~~ Loads Compiler Commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   parallel = {}
   if cfgDict.has_key('path_parallel'): parallel.update({'PATH':cfgDict['path_parallel']})
   return parallel

"""
   Extract full user defined comand line
   for the treatment of the option 'mpi'
"""
def getMPI(cfgDict):
   # ~~ Loads Compiler Commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mpi = {}
   if cfgDict.has_key('mpi_hosts'): mpi.update({'HOSTS':cfgDict['mpi_hosts']})
   if cfgDict.has_key('mpi_cmdexec'): mpi.update({'EXEC':cfgDict['mpi_cmdexec']})
   return mpi

"""
   Read the list of user defined modules for action -- Certain
   keyword such as clean, update, system will trigger additonal
   behaviours:
    - clean: rebuilt = 1, rebuild object, libs and executables
    - update: rebuilt = 2, rebuild libs and executables
    - system (or nothing): include all modules
   Will alos read the specif option and build the list of modules
   according to the stwiches, i.e. parallel, openmi, xdmf, etc.
"""
def parseUserModules(cfgDict,modules):
   # ~~ Loads To-Be Compiled ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   compiler = {}; typeBuild = 0; userList = ''
   if cfgDict.has_key('modules'):
      userList = cfgDict['modules']
   else:
      userList = 'clean ' + ' '.join(modules.keys())
   # ~~ Loads To-Be Compiled ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   word = r'\s*(?P<before>.*)\s*(?P<this>(\b(%s)\b))\s*(?P<after>.*)\s*\Z'
   proc = re.match(re.compile(word%('clean'),re.I),userList)
   if proc :
      userList = proc.group('before') + ' ' + proc.group('after')
      typeBuild = 2
   proc = re.match(re.compile(word%('update'),re.I),userList)
   if proc :
      userList = proc.group('before') + ' ' + proc.group('after')
      typeBuild = 1
   # ~~ Remove unwanted ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   word = r'(?P<before>.*?)\s*?-(?P<this>(%s)\b)\s*(?P<after>.*)\s*\Z'
   for mod in modules.keys():
      proc = re.match(re.compile(word%(mod),re.I),userList)
      if proc :
         userList = proc.group('before') + ' ' + proc.group('after')
         del modules[mod]
   # ~~ Check forgotten ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for mod in userList.split():
      if '-' == mod[0:1]:
         print '\nCould not find the following module:',mod[1:]
         sys.exit()
   # ~~ Deal with all ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'system' in userList : userList = ' '.join(modules.keys())
   # ~~ Activates parallel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if cfgDict.has_key('options'):
      if 'parallel' in cfgDict['options'].lower():
         userList = userList.replace('paravoid','')
         if modules.has_key('paravoid'): del modules['paravoid']
      else:
         userList = userList.replace('parallel','')
         if modules.has_key('parallel'): del modules['parallel']
   else:
      userList = userList.replace('parallel','')
      if modules.has_key('parallel'): del modules['parallel']
   # ~~ Activates openmi ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #if cfgDict.has_key('options'):
   #   if 'openmi' in cfgDict['options'].lower():
   return userList,typeBuild

"""
   Read the list of user defined ranks used as a filter of the
   validation test case directory names in the form rnn_*,
   where r takes values from 0 to 9 inclusive. Special keywords
   such as all, >, or < will trigger additonal behaviours:
    - all: includes all numbers from 0 to 9
    - <n: includes all numbers below n
    - >n: includes all numbers above n
    - n: includes the number n
"""
def parseValidationRanks(cfgDict):

   # ~~ Key not here ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if not cfgDict.has_key('val_rank'): return range(10)

   # ~~ 'all' in key ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   userList = cfgDict['val_rank']
   word = r'(?P<before>.*)\s*(?P<this>(\b(%s)\b))\s*(?P<after>.*)\s*\Z'
   proc = re.match(re.compile(word%('all'),re.I),userList)
   if proc : return range(10)

   ranks = []
   # ~~ '<' and '>' in key ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   word = r'(?P<before>.*?)\s*(?P<symbol>([><]|))(?P<this>\d\b)\s*(?P<after>.*)\s*\Z'
   while 1:
      ul = userList
      proc = re.match(re.compile(word,re.I),userList)
      if proc :
         if proc.group('symbol') == '<' :
            ranks.extend(range(0,int(proc.group('this'))))
         elif proc.group('symbol') == '>' :
            ranks.extend(range(int(proc.group('this'))+1,10))
         else:
            ranks.append(int(proc.group('this')))
         userList = proc.group('before') + proc.group('after')
      if ul == userList: break

   # ~~ sorting out duplicates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # there must be a better way (with tuples maybe ?)
   for u in range(10):
      while ranks.count(u) > 1: ranks.remove(u)

   return sorted(ranks)

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
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
                      default='',
                      help="specify the root, default is taken from config file" )
   parser.add_option("-v", "--version",
                      type="string",
                      dest="version",
                      default='',
                      help="specify the version number, default is taken from config file" )
   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.dirname(options.configFile)
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         for dirpath,dirnames,filenames in walk(dircfg) : break
         for file in filenames :
            head,tail = path.splitext(file)
            if tail == '.cfg' : print '    +> ',file
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile)
   cfgnames = cfgs.keys()
   if options.configName != '':
      if options.configName not in cfgnames:
         print '\nNot able to find your configuration in the configuration file: ' + options.configFile + '\n'
         print ' ... use instead:'
         for cfgname in cfgnames : print '    +> ',cfgname
         sys.exit()
      cfgnames = [options.configName]

   print '\n' + options.configFile + '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
#  /!\  for testing purposes ... no real use
   for cfgname in cfgnames:
      # still in lower case
      if options.rootDir != '': cfgs[cfgname]['root'] = options.rootDir
      if options.version != '': cfgs[cfgname]['version'] = options.version
      # parsing for proper naming
      cfg = parseConfig_CompileTELEMAC(cfgs[cfgname])

      print '\n'+cfgname + ': \n    '
      print '    +> root:    ',cfgs[cfgname]['root']
      print '    +> version: ',cfgs[cfgname]['version']
      print '    +> module:  ',' / '.join(cfg['MODULES'])
   
   sys.exit()
