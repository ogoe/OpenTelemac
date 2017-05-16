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
"""@history 30/04/2011 -- Sebastien E. Bourban
         Upgrade made to parseConfig_CompileTELEMAC, which does not use the
         GLOBAL variable anymore. The configuration is passed in argument.
"""
"""@history 30/04/2011 -- Sebastien E. Bourban
         Upgrade made to the others parseConfig_***TELEMAC just as
         previously done
"""
"""@history 19/06/2012 -- Yoann Audouin
         Bug corrected, replace the '<root>' in the output from getEXTERNALs
         by the actual root of the system
"""
"""@history 04/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Allowing users to define own a new section, "general", with own user
      keys as well as own user keys within each configuration.
      User keys are then replaced throughout all other values whenever [userkey]
      is used, such as (where fflags is an arbitrary user key):
fflags:     -c -O3 -ffixed-line-length-132 -fconvert=big-endian -frecord-marker=4
cmd_obj:    gfortran [fflags] <mods> <incs> <f95name>
   Note that if -c or USETELCFG is set, the replacement of user keys will only
      be done for that configuration
"""
"""@history 05/12/2012 -- Sebastien E. Bourban
   Allowing the user to set keys such as cmd_exe, cmd_obj and cmd_lib per
      module (for instance cmd_exe_parallel). For this, cfgTELEMAC['COMPILER']
      has been removed and replaced with a COMPILER for each module, just as
      for the incs, libs and mods.
   Also works where _all is not set, i.e. incs or incs_all work the same magic.
"""
"""@history 12/12/2012 -- Sebastien E. Bourban
   Addition of the --delete option to purge unwanted configuration-related
      sub-directories. Note that it will not remove the configuration from the
      config file.
   If the unwanted configuration does not exist in the config file anymore, it
      will read the [general] section, trying to guess the root and the version at
      the very least. One can also use -r and -v to inform of these values.
"""
"""@history 25/12/2014 -- Sebastien E. Bourban
   'version' is not mandatroy anymore.
   It has been removed from having to be in the configuration file.
"""
"""@brief
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
      root:       C:\opentelemac\v8p0
      modules:    parallel artemis telemac2d
   - The key 'root' defines the directory path to the base of the
   TELEMAC system files.

   - the key 'module' defines the name of the modules upon which
     the action of the configuration is carried out. The module
   names that are not recognised as part of the TELEMAC system
   (following the template root\module) are
   ignored. Three additonal names are however allowed:
   - system: which is replaced by the list of all available modules
   - clean: which trigger the action to be carried out as if it was
     the first time
   - update: which trigger the action only to update the parts
     that are necessary.
   For instance, the compilation of the entire TELEMAC system,
   removing all existing object and libraries would simply write:
      [wintel32s]
      root:       C:\opentelemac\v8p0
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
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import re
import sys
from os import path, walk, listdir, environ, sep, remove
from socket import gethostname
import ConfigParser
from argparse import ArgumentParser,RawDescriptionHelpFormatter
from utils.files import removeDirectories,putFileContent
from utils.messages import MESSAGES,filterMessage,banner

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
def getConfigs(file,name,bypass=False):
   # ~~ Read Configuration File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgfile = ConfigParser.RawConfigParser()
   try:
      cfgfile.read(file)
   except Exception as e:
      raise Exception([filterMessage({'name':'getConfigs','msg':' ... Some of the lines may not start with a comment #.'},e,bypass)])
   # ~~ Read Config Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgs = cfgfile.get('Configurations','configs')
   if cfgs == '' :
      print '\nPlease specify appropriate configuration names for key [Configurations] in the config file\n'
      sys.exit(1)
   # ~~ Filter Config Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgnames = cfgs.split()
   if name != '':
      if name not in cfgnames:
         print '\nNot able to find your configuration [' + name + '] in the configuration file: ' + file
         if bypass: print ' ... will try to gess the configuration from the general keys and move on ...'
         else:
            print '\n ... use instead:'
            for cfg in cfgnames : print '    +> ',cfg
            sys.exit(1)

      cfgnames = [name]
   # ~~ Verify presence of configs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for cfg in cfgnames:
      if cfg not in cfgfile.sections():
         print '\nNot able to find the configuration [' + cfg + '] in the configuration file: ' + file
         sys.exit(1)
   # ~~ Read General ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   try:
      general = dict(cfgfile.items('general'))
   except:
      general = {}
   # ~~ Loads Configurations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   configs = {}
   for cfg in cfgnames :
      try:
         configs.update({cfg:dict(cfgfile.items(cfg))}) # convert all keys in lower case !
      except:
         configs.update({cfg:{}})

   return general,configs

"""
   Get the name of the config file from command line arguments
   and store its rough content in a dict -- Returns the dict
   set globals CONFIGS
"""
def parseConfigFile(file,name,bypass=False):
   # ~~ Parse CFG File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print "... parsing configuration file: " + file
   generalDict,configDict = getConfigs(file,name,bypass)

   # ~~ Replacing user keys throughout ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   key_sqbrack = re.compile(r'(?P<brack>\[[\w_.-~=+]*?\])') #,re.I)
   for cfgname in configDict:
      # ~~> making sure cfgname also includes all keys from general
      for genkey in generalDict :
         if not genkey in configDict[cfgname]: configDict[cfgname].update({genkey:generalDict[genkey]})
      # ~~> replacing [key] by its value
      for cfgkey in configDict[cfgname]:
         for k in re.findall(key_sqbrack,configDict[cfgname][cfgkey]):
            key = k.strip('[]')
            if configDict[cfgname].has_key(key): configDict[cfgname][cfgkey] = configDict[cfgname][cfgkey].replace(k,configDict[cfgname][key])
            elif environ.has_key(key): configDict[cfgname][cfgkey] = configDict[cfgname][cfgkey].replace(k,environ[key])
            else:
               print '... Could not find your special key ',k,' in key ',cfgkey,' of configuration ',cfgname
   globals()["CONFIGS"] = configDict
   return configDict

"""
   Get the value of a key from the config cfg. Further, test if
   the key is there and if it is empty -- Return the value
"""
def getConfigKey(cfg,key,there,empty):
   # ~~ Get the root of the System ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if there and not key in cfg:
      print ('\nKey %s required in configuration (or on the command line as an option) \n' % (key))
      sys.exit(1)
   if empty and cfg[key] == '':
      print ('\nKey %s required non empty field \n' % (key))
      sys.exit(1)
   if not key in cfg: return ''
   return cfg[key]

def getScanContent(fle,root,bypass):
   """
   @title: Parser of the cmdf file.
   @param:
    - fle:    name of the cmdf file (with its path) to be parsed
    - root:   root of the system to replace reference to <root>
    - bypass: whether exception raised should be bypassed or not
   @return
    - cmdf dictionary
   @brief:
   File names and path are neutrally separated with the | separator.
   The cmdf file has a primary section called [general] with the following
   mandatory keys:
   [general]
    - path: <root>|sources|artemis, or <root>|sources|utils|bief
      the location of the source file, which is top of the tree of calls
      also the location of the cmdf file itself
    - module: artemis, or partel, or passive
      the module name used in the cfg and cmdf dictionaries to refer to
      a particular module of the system, noting that one module can have
      multiple tree tops, such as artemis.f, solve.f or partel_para.f
    - liborder: special hermes parllel ... module
      the ordered list of dependencies, which will be used to compile and
      link all libraries together in order of dependencies
    - name: not so useful at the n=moment (?)
   Optionaly, the [general] section can also include reference to other
   cmdf file, considered as external, with their own compilation options.
    - extrenal: passive.solve, or the name of a module defined in the cfg
      or cmdf dictionaries.
   The [general] section is then followed by other section taking their
   names in liborder [special], [hemes], [parallel], ... [module]. Each of
   these secondary section has two keys:
   [bief]
    - path: <root>|sources|utils|bief
      the common location of all files included in the tree of calls
    - files: bief_def.f
      bief.f
      ...
      the list of files included in the tree fo calls that are in the path,
      noting the indent with one file per line, and that file names could also
      include subdirectory path, relative to the common path location
   """
   # TODO: try removing root, leaving in th various paths references to <root>

   # ~~> cmdf format follows the raw config parser standard
   cfgfile = ConfigParser.RawConfigParser()
   # ~~> return content, as a dictionary of sections and their keys
   content = {}

   # ~~ Parse CMDF file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   try: cfgfile.read(fle)
   except Exception as e:
      raise Exception([filterMessage({'name':'getScanContent','msg':'Could not read the required parameters in the cmdf-scan file: '+fle+'\n     ... you may have to use the --rescan option'},e,bypass)])

   # ~~ Interpret [general] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   try: general = dict(cfgfile.items('general'))
   except Exception as e:
      raise Exception([filterMessage({'name':'getScanContent','msg':'Could not find the general section in the cmdf-scan file: '+fle},e,bypass)])

   # ~~> mandatory keys
   if not 'liborder' in general: raise Exception([{'name':'getScanContent','msg':'Could not find the key liborder in the general section of the cmdf-scan file:'+fle}])
   general['liborder'] = general['liborder'].split()
   if not 'path' in general: raise Exception([{'name':'getScanContent','msg':'Could not find the key path in the general section of the cmdf-scan file:'+fle}])
   general['path'] = general['path'].replace('<root>',root).replace('|',sep)
   if not 'module' in general: raise Exception([{'name':'getScanContent','msg':'Could not find the key module in the general section of the cmdf-scan file:'+fle}])
   #if not general['module'] in general['liborder']: raise Exception([{'name':'getScanContent','msg':'Could not find the key '+general['module']+' in the liborder of the cmdf-scan file:'+fle}])
   if not 'name' in general: raise Exception([{'name':'getScanContent','msg':'Could not find the key name in the general section of the cmdf-scan file:'+fle}])
   # ~~> optional keys
   #general['external']
   # ~~> final content
   content.update({'general':general})

   # ~~ Interpret all other sections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for lib in cfgfile.sections():
      # ~~> besides general
      if lib == 'general': continue
      # ~~> lib has to be in liborder
      if lib not in content['general']['liborder']: raise Exception([{'name':'getScanContent','msg':'Found a section not in the [general] liborder key ' + lib + ' of the cmdf-scan file:'+fle}])
      # ~~> content[lib] includes all keys of lib ...
      content.update({lib:dict(cfgfile.items(lib))})
      # ~~> ... of which path (may include <root> and | separators)
      if not 'path' in content[lib]: raise Exception([{'name':'getScanContent','msg':'Could not find the key path in the section ' + lib + ' of the cmdf-scan file:'+fle}])
      content[lib]['path'] = content[lib]['path'].replace('<root>',root).replace('|',sep)
      # ~~> ... and files (TODO: should | separators be replaced at this stage ?)
      if not 'files' in content[lib]: raise Exception([{'name':'getScanContent','msg':'Could not find the key files in the section ' + lib + ' of the cmdf-scan file:'+fle}])
      content[lib]['files'] = content[lib]['files'].replace('\n',' ').replace('  ',' ').split()
   # ~~> last check on possibly missing libs
   for lib in content['general']['liborder']:
      if lib not in content: raise Exception([{'name':'getScanContent','msg':'The reference ' + lib + ' in the [general] liborder key is not defined in your cmdf-scan file:'+fle}])

   # ~~ retrun ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   return content

"""
   <title>: Writer of the cmdf file.
   <args>:
    - fle:    name of the cmdf file (with its path) to be saved as
    - root:   root of the system to replace reference to <root>
    - bypass: whether exception raised should be bypassed or not
   <return>
    -
   <brief>:
   See format description at getScanContent()
"""
def putScanContent(fle,root,content):
   # TODO: try removing root, leaving in th various paths references to <root>

   # ~~> return lines to be stored
   lines = []

   # ~~ Write-up [general] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'general' in content:
      lines.append('[general]'+'\n'+'path: '+content['general']['path'].replace(root,'<root>').replace(sep,'|')+'\n'+'module: '+content['general']['module'])
      lines.append('liborder: '+' '.join(content['general']['liborder']))
      lines.append('name: '+content['general']['name'])
      if 'external' in content['general']: lines.append('external: '+content['general']['external'])

   # ~~ Write-up [sections] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #for lib in content['general']['liborder']: TODO: replace the following 2 line by this one
   for lib in sorted(content.keys()):
      if lib == 'general': continue
      lines.append('\n['+lib+']'+'\n'+'path: '+content[lib]['path'].replace(root,'<root>').replace(sep,'|')+'\n'+'files: '+'\n  '.join(content[lib]['files']))

   # ~~ Store to cmdf file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent(fle,lines)

   return

# _____                  ___________________________________________
# ____/ TELEMAC Toolbox /__________________________________________/
#
"""
   Extract all the information required for
   the Compilation of TELEMAC
   Requires: root,
   Optional: mods_, incs_, libs_, ... and options
"""
def parseConfig_CompileTELEMAC(cfg,rescan,bypass):
   #  ~~> cfg is the dictionary of either configuration such as wintel32s wintel32p wing9532s wing9532p ...
   cfgTELEMAC = {}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(cfg,'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'root':path.normpath(get)})
   # Get pytel: ...
   # or the absolute path to the root of the python scripts
   get = getConfigKey(cfg,'pytel',False,False)
   if get != '' and not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'pytel':path.normpath(get)})
   # Get version if present
   get = getConfigKey(cfg,'version',False,False).lower()
   cfgTELEMAC.update({'version':get})
   # Get options for printing purposes
   get = getConfigKey(cfg,'options',False,False).lower()
   cfgTELEMAC.update({'options':get})
   # Get cmd_obj_c for mascaret
   get = getConfigKey(cfg,'cmd_obj_c',False,False)
   cfgTELEMAC.update({'cmd_obj_c':get})

   # ~~ List all files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #
   #  Search all files within the <root> following a special template
   #  following
   #     <root>|sources|module-name or <root>|sources|utils|sub-module-name
   #  All files means here, all files with extensions:
   #    .cmdf or .f. f90 .F .F90 or .dico files
   #  /!\ hard coded template.
   #  TODO: remove requirement for fix template
   #
   get = getFolders_ModulesTELEMAC(cfgTELEMAC['root'],rescan,bypass)
   cfgTELEMAC.update({'MODULES':get})
   #  ... at this stage, you may not have found any or all cmdf files, and
   #  thus not know yet about the tags.
   #
   #  Get the configuration key tag_... for special effects,
   #  These will be compiled together with the main compilation as one.
   #  The odd ones are here only to counteract the add ones, in case the main
   #  should be compiled as a self comtained program. If absent, the main
   #  would be incomplete and dependent on the compilation of the add ones.
   #
   tags,get = getTAGs('tag',cfg,cfgTELEMAC['MODULES'])
   cfgTELEMAC.update({'ODDONES':tags})
   cfgTELEMAC['MODULES'].update(get)
   #
   #  Get the configuration key add_... for special effects,
   #  added to the main compilation as a separate tree with its own
   #  compilation options. The main program requires this additional tree
   #  to be complete, and therefore linked here as external.
   #
   adds,get = getTAGs('add',cfg,cfgTELEMAC['MODULES'])
   cfgTELEMAC.update({'ADDONES':adds})
   cfgTELEMAC['MODULES'].update(get)
   #  ... at this stage, you still do not know where are the files referred
   #  to in the tags even thouh getFolders_ModulesTELEMAC() has listed all
   #  on the system. Later on parserFortran() will be going through that.


   # ~~ Compilation options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #
   #  Get libs_all: ... libs_artemis: ... libs_passive: ... mods_all: ... etc.
   #  for every module in the list of modules to account for
   #  specific external includes for all or each module
   for mod in cfgTELEMAC['MODULES']:
      for ext in ['mods','incs','libs']:
         cfgTELEMAC['MODULES'][mod].update({ext:addEXTERNALs(cfg,ext,mod).replace('<root>',cfgTELEMAC['root'])})
   # Get cmd_obj: ... cmd_lib: ... cmd_exe: ...
   # the compiler dependent command lines to create obj, lib and exe
   for mod in cfgTELEMAC['MODULES']:
      for ext in ['obj','lib','exe','pyf','pyd']:
         cfgTELEMAC['MODULES'][mod].update({'x'+ext:getEXTERNALs(cfg,ext,mod).replace('<root>',cfgTELEMAC['root'])})

   cfgTELEMAC.update({'COMPILER':{}})
   #  Get modules: user list of module
   #  in which 'system' means all existing modules,
   #  and in which 'update' means a rebuild of the lib and exe
   #  and in which 'clean' means a rebuild of the obj, lib and exe
   #  and Get options: for the switches such as parallel, openmi, mumps, etc.
   get,tbd = parseUserModules(cfg,cfgTELEMAC['MODULES'])
   get = get.split()
   #  Add extra modules for special effects as priority items (insert(0,))
   #  where can be passive, for instance
   for mod in cfgTELEMAC['ADDONES']:
      if mod not in get: get.insert(0,mod)

   cfgTELEMAC['COMPILER'].update({'MODULES':get})
   cfgTELEMAC['COMPILER'].update({'REBUILD':tbd})
   for mod in get:
      if mod not in cfgTELEMAC['MODULES']:
         print ('\nThe following module does not exist %s \n' % (mod))
         sys.exit(1)
   # TODO: check if you need this
   #for mod in get:
   #   if mod not in cfgTELEMAC['MODULES']:
   #      del cfgTELEMAC['COMPILER']['MODULES'][cfgTELEMAC['COMPILER']['MODULES'].index(mod)]
   #print cfgTELEMAC['COMPILER']
   #sys.exit()

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,False)[1:]})

   # Get system's suffixes for obj, lib, mod, and exe
   system = {}
   system.update({'sfx_obj':getConfigKey(cfg,'sfx_obj',True,False).lower()})
   system.update({'sfx_pyf':getConfigKey(cfg,'sfx_pyf',False,False).lower()})
   system.update({'sfx_pyd':getConfigKey(cfg,'sfx_pyd',False,False).lower()})
   system.update({'sfx_exe':getConfigKey(cfg,'sfx_exe',False,False).lower()})
   system.update({'sfx_lib':getConfigKey(cfg,'sfx_lib',True,False).lower()})
   system.update({'sfx_mod':getConfigKey(cfg,'sfx_mod',True,False).lower()})
   cfgTELEMAC.update({'SYSTEM':system})

   # tagged fields for cmdx and cmdo
   cfgTELEMAC.update({'TRACE':{}})
   for k in cfg:
      if k[0:4] in ['root','libs']:
          get = getConfigKey(cfg,k,False,False)
          if get != '': cfgTELEMAC['TRACE'].update({k:get})

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
      sys.exit(1)
   cfgTELEMAC[cfg].update({'root':path.normpath(get)})
   # Get pytel: ...
   # or the absolute path to the root of the python scripts
   get = getConfigKey(cfg,'pytel',False,False)
   if get != '' and not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'pytel':path.normpath(get)})
   # Get version if present
   get = getConfigKey(CONFIGS[cfg],'version',False,False).lower()
   cfgTELEMAC[cfg].update({'version':get})
   # Get options for printing purposes
   get = getConfigKey(cfg,'options',False,False).lower()
   cfgTELEMAC.update({'options':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC[cfg].update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC[cfg]['root'],False,False)})
   cfgTELEMAC[cfg].update({'COMPILER':{}})
   # Get cmplr_mod: user list of module
   get,tbd = parseUserModules(CONFIGS[cfg],cfgTELEMAC[cfg]['MODULES'])
   cfgTELEMAC[cfg]['COMPILER'].update({'MODULES':get.split()})
   for mod in get.split():
      if mod not in cfgTELEMAC[cfg]['MODULES']:
         print ('\nThe following module does not exist %s \n' % (mod))
         sys.exit(1)
   for mod in get.split():
      if mod not in cfgTELEMAC[cfg]['MODULES']:
         del cfgTELEMAC[cfg]['COMPILER']['MODULES'][cfgTELEMAC[cfg]['COMPILER']['MODULES'].index(mod)]

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
      sys.exit(1)
   cfgTELEMAC[cfg].update({'root':path.normpath(get)})
   # Get pytel: ...
   # or the absolute path to the root of the python scripts
   get = getConfigKey(cfg,'pytel',False,False)
   if get != '' and not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'pytel':path.normpath(get)})
   # Get version if present
   get = getConfigKey(CONFIGS[cfg],'version',False,False).lower()
   cfgTELEMAC[cfg].update({'version':get})
   # Get options for printing purposes
   get = getConfigKey(cfg,'options',False,False).lower()
   cfgTELEMAC.update({'options':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC[cfg].update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC[cfg]['root'],False,False)})
   cfgTELEMAC[cfg].update({'COMPILER':{}})
   # Get cmplr_mod: user list of module
   get,tbd = parseUserModules(CONFIGS[cfg],cfgTELEMAC[cfg]['MODULES'])
   cfgTELEMAC[cfg]['COMPILER'].update({'MODULES':get.split()})
   for mod in get.split():
      if mod not in cfgTELEMAC[cfg]['MODULES']:
         print ('\nThe following module does not exist %s \n' % (mod))
         sys.exit(1)
   for mod in get.split():
      if mod not in cfgTELEMAC[cfg]['MODULES']:
        del cfgTELEMAC[cfg]['COMPILER']['MODULES'][cfgTELEMAC[cfg]['COMPILER']['MODULES'].index(mod)]

   # Get tellng:
   # TELEMAC language code, to know the default language
   # tellng = 1, French; tellng = 2, English
   get = getConfigKey(CONFIGS[cfg],'language',True,True)
   cfgTELEMAC[cfg].update({'language':get})

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
      sys.exit(1)
   cfgTELEMAC.update({'root':path.normpath(get)})
   # Get pytel: ...
   # or the absolute path to the root of the python scripts
   get = getConfigKey(cfg,'pytel',False,False)
   if get != '' and not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'pytel':path.normpath(get)})
   # Get destination doxydocs: ...
   get = getConfigKey(cfg,'doxydocs',True,True).lower()
   cfgTELEMAC.update({'doxydocs':get})
   # Get doxygen command: ...
   get = getConfigKey(cfg,'cmd_doxygen',True,True).lower()
   cfgTELEMAC.update({'cmd_doxygen':get})
   # Get options for printing purposes
   get = getConfigKey(cfg,'options',False,False).lower()
   cfgTELEMAC.update({'options':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['root'],False,False)})

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
      if mod not in cfgTELEMAC['MODULES']:
         print ('\nThe following module does not exist %s \n' % (mod))
         sys.exit(1)
   for mod in get.split():
      if mod not in cfgTELEMAC['MODULES']:
        del cfgTELEMAC['COMPILER']['MODULES'][cfgTELEMAC['COMPILER']['MODULES'].index(mod)]

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,False)[1:]})

   # Get system's suffixes for obj, lib, mod, and exe
   system = {}
   #system.update({'sfx_obj':getConfigKey(cfg,'sfx_obj',True,False).lower()})
   #system.update({'sfx_exe':getConfigKey(cfg,'sfx_exe',True,False).lower()})
   #system.update({'sfx_lib':getConfigKey(cfg,'sfx_lib',True,False).lower()})
   #system.update({'sfx_mod':getConfigKey(cfg,'sfx_mod',True,False).lower()})
   cfgTELEMAC.update({'SYSTEM':system})

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
      sys.exit(1)
   cfgTELEMAC.update({'root':path.normpath(get)})
   # Get pytel: ...
   # or the absolute path to the root of the python scripts
   get = getConfigKey(cfg,'pytel',False,False)
   if get != '' and not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'pytel':path.normpath(get)})
   # Get options for printing purposes
   get = getConfigKey(cfg,'options',False,False).lower()
   cfgTELEMAC.update({'options':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['root'],False,False)})

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,True)[1:]})

   return cfgTELEMAC

"""
   Extract all the information required for the validation
   of the relevant modules for each configuration
   The principal assumption is that the validation cases are
   either under:
     + val_root\\*
     + teldir\\examples\\module\\val_root\\*
   If the 'val_root' key is not in the config, the default
   path is assumed to be based on the second option
"""
def parseConfig_ValidateTELEMAC(cfg):
   #  cfg is either  wintel32s wintel32p wing9532s wing9532p ...
   cfgTELEMAC = {}
   # Get teldir: ...
   # or the absolute path to the TELEMAC root
   get = getConfigKey(cfg,'root',True,True)
   if not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'root':path.normpath(get)})
   # Get pytel: ...
   # or the absolute path to the root of the python scripts
   get = getConfigKey(cfg,'pytel',True,False)
   if get != '' and not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'pytel':path.normpath(get)})
   # Get version if present
   get = getConfigKey(cfg,'version',False,False).lower()
   cfgTELEMAC.update({'version':get})
   # Get options for printing purposes
   get = getConfigKey(cfg,'options',False,False).lower()
   cfgTELEMAC.update({'options':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['root'],False,False)})
   # Get libs_all: ... libs_artemis: ... mods_all: ... etc.
   # for every module in the list of modules to account for
   # specific external includes for all or each module
   for mod in cfgTELEMAC['MODULES']:
      cfgTELEMAC['MODULES'][mod].update({'mods':addEXTERNALs(cfg,'mods',mod).replace('<root>',cfgTELEMAC['root'])})
      cfgTELEMAC['MODULES'][mod].update({'incs':addEXTERNALs(cfg,'incs',mod).replace('<root>',cfgTELEMAC['root'])})
      cfgTELEMAC['MODULES'][mod].update({'libs':addEXTERNALs(cfg,'libs',mod).replace('<root>',cfgTELEMAC['root'])})

   cfgTELEMAC.update({'VALIDATION':{}})
   # Get ranks: user list of ranks to filter the list of validation cases
   # in which 'all' means all possible ranks,
   # in which '<n' means all ranks less than n,
   # in which 'n' means all ranks equal to n,
   # in which '>n' means all ranks greater than n,
   # where 'n', '>n', and '<n' can be combined if necessary
   if not 'val_rank' in cfg: val_ranks = range(10)
   else: val_ranks = parseValidationRanks(cfg['val_rank'])
   # Get validation: user list of module and there associated directories
   # in which 'system' means all existing modules,
   # and in which 'update' means a continuation, ignoring previously completed runs
   # and in which 'clean' means a re-run of all validation tests
   if not 'val_root' in cfg:
      val_root = path.realpath(path.join(cfgTELEMAC['root'],'examples'))
      if not path.isdir(val_root):
         print '\nNot able to find your validation set from the path: ' + val_root + '\n'
         print ' ... check the val_root key in your configuration file'
         sys.exit(1)
   else:
      val_root = cfg['val_root'].replace('<root>',cfgTELEMAC['root'])
   _, examples, _ = walk(val_root).next()
   get,tbd = parseUserModules(cfg,cfgTELEMAC['MODULES'])
   cfgTELEMAC.update({'REBUILD':tbd})
   # Removing python27 from examples if "system" or "python27" are not in cfg
   if not ("system" in cfg['modules'] or "python27" in cfg['modules']):
       examples.remove("python27")
   for mod in (' '.join(examples)).split():   # /!\ other ways to copy ?
      if mod not in get and mod in cfgTELEMAC['MODULES']: examples.remove(mod)
   for mod in examples:
      val_dir = path.join(val_root,mod)
      val_mod = getFiles_ValidationTELEMAC(val_dir,val_ranks)
      if val_mod != {}:
         cfgTELEMAC['VALIDATION'].update({mod:{'path':path.realpath(val_dir)}})
         cfgTELEMAC['VALIDATION'][mod].update(val_mod)

   # Get path_parallel for partel
   cfgTELEMAC.update({'PARTEL':getPARTEL(cfg)})
   # Get mpi_cpulist and mpi_cmdexec: for mpi option
   # .. in theory, mpi could be replaced by something else (?)
   cfgTELEMAC.update({'MPI':getMPI(cfg)})
   # Get hpc_cmdexec and hpc_infile for hpc option
   # .. in theory, bsub could be replaced by another queueing system
   cfgTELEMAC.update({'HPC':getHPC(cfg)})

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,True)[1:]})

   # Get system's suffixes for obj, lib, mod, and exe
   system = {}
   system.update({'sfx_obj':getConfigKey(cfg,'sfx_obj',True,False).lower()})
   system.update({'sfx_exe':getConfigKey(cfg,'sfx_exe',True,False).lower()})
   system.update({'sfx_lib':getConfigKey(cfg,'sfx_lib',True,False).lower()})
   system.update({'sfx_mod':getConfigKey(cfg,'sfx_mod',True,False).lower()})
   cfgTELEMAC.update({'SYSTEM':system})

   # tagged fields for cmdx and cmdo
   cfgTELEMAC.update({'TRACE':{}})
   for k in cfg:
      if k[0:4] in ['root','libs']:
          get = getConfigKey(cfg,k,False,False)
          if get != '': cfgTELEMAC['TRACE'].update({k:get})

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
      sys.exit(1)
   cfgTELEMAC.update({'root':path.normpath(get)})
   # Get pytel: ...
   # or the absolute path to the root of the python scripts
   get = getConfigKey(cfg,'pytel',False,False)
   if get != '' and not path.exists(get):
      print ('\nThe following directory does not exist %s \n' % (get))
      sys.exit(1)
   cfgTELEMAC.update({'pytel':path.normpath(get)})
   # Get version if present
   get = getConfigKey(cfg,'version',False,False).lower()
   cfgTELEMAC.update({'version':get})
   # Get options for printing purposes
   get = getConfigKey(cfg,'options',False,False).lower()
   cfgTELEMAC.update({'options':get})

   # Deduce the actual list of modules existing within the root teldir,
   # identified by matching the directory structure to the template
   # teldir\module_name\*telver\
   cfgTELEMAC.update({'MODULES':getFolders_ModulesTELEMAC(cfgTELEMAC['root'],False,False)})

   get,tbd = parseUserModules(cfg,cfgTELEMAC['MODULES'])
   cfgTELEMAC.update({'REBUILD':tbd})
   # You do not need 'get' at this stage

   # Get path_parallel: for parallel option
   # the parallel dependent command line executables (partel, gretel, ...)
   cfgTELEMAC.update({'PARTEL':getPARTEL(cfg)})
   # Get mpi_cpulist and mpi_cmdexec: for mpi option
   # .. in theory, mpi could be replaced by something else (?)
   cfgTELEMAC.update({'MPI':getMPI(cfg)})
   # Get hpc_cmdexec and hpc_infile for hpc option
   # .. in theory, bsub could be replaced by another queueing system
   cfgTELEMAC.update({'HPC':getHPC(cfg)})

   # Get command_zip: and command_piz:
   # the command lines to zip/unzip respectively
   cfgTELEMAC.update({'ZIPPER':getConfigKey(cfg,'sfx_zip',True,True)[1:]})

   # Get system's suffixes for obj, lib, mod, and exe
   system = {}
   system.update({'sfx_obj':getConfigKey(cfg,'sfx_obj',True,False).lower()})
   system.update({'sfx_exe':getConfigKey(cfg,'sfx_exe',True,False).lower()})
   system.update({'sfx_lib':getConfigKey(cfg,'sfx_lib',True,False).lower()})
   system.update({'sfx_mod':getConfigKey(cfg,'sfx_mod',True,False).lower()})
   cfgTELEMAC.update({'SYSTEM':system})

   # tagged fields for cmdx and cmdo
   cfgTELEMAC.update({'TRACE':{}})
   for k in cfg:
      if k[0:4] in ['root','libs']:
          get = getConfigKey(cfg,k,False,False)
          if get != '': cfgTELEMAC['TRACE'].update({k:get})

   return cfgTELEMAC

"""
   Get the root of the validation directory and walk from there
   through the directory structure to identify modules with the template
   val_root\module_name\*\*.xml
"""
def getFiles_ValidationTELEMAC(root,ranks):
   validation = {}

   dirpath, dirnames, _ = walk(root).next()
   for ddir in dirnames:
      val = { ddir:[] }
      _, _, filenames = walk(path.join(dirpath,ddir)).next()
      for fle in filenames:
         if path.splitext(fle)[1] == '.xml' : val[ddir].append(fle)
      if val[ddir] != []: validation.update(val)

   return validation

"""
   Walk through the directory structure available from the root
   and identifies modules with the template:
      sources|module-name or sources|utils|sub-module-name
"""
def getFolders_ModulesTELEMAC(root,rescan,bypass):
   #
   # note that the references to the cmdf will be stored with their path.
   # the dico and the fortran files will be stored without their path.
   #
   modules = {}
   sources = path.join(root,'sources')
   if not path.exists(sources):
      raise Exception([{'name':'getFolders_ModulesTELEMAC','msg':'... I could not locate the source code in '+sources+'\n     ... you may not have told me where the root is on your system.'}])

   for moddir in listdir(sources) :
      # ~~> excluding files and .svn or other hidden directories
      if moddir[0] == '.' or path.isfile(path.join(sources,moddir)): continue
      # ~~> moddir in lower case please
      modroot = path.join(sources,moddir.lower())
      if not path.exists(modroot): continue

      # ~~ Walk through ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dirpath, dirnames, filenames = walk(modroot).next()
      # ~~> One level in modroot
      for fle in filenames:
         f = path.splitext(fle)

         if f[len(f)-1].lower() in ['.dico']:   # /!\ you found an executable module
            if not moddir in modules: modules.update({moddir:{'path':dirpath,'dico':fle,'cmdfs':[]}})
            else: modules[moddir].update({'dico':fle})

         if f[len(f)-1].lower() in ['.cmdf']:   # /!\ you found at least one cmdf file
            if rescan: remove(path.join(dirpath,fle))
            else:
               cmdf = getScanContent(path.join(dirpath,fle),root,bypass)
               genmod = cmdf['general']['module']
               if dirpath != cmdf['general']['path']:
                  raise Exception([{'name':'getFolders_ModulesTELEMAC','msg':'... this cmdf file '+fle+' is out of place\n     ... check its internal path.'}])
               if not genmod in modules: modules.update({genmod:{'path':dirpath,'cmdfs':[fle]}})
               elif not 'cmdfs' in modules[genmod]: modules[genmod].update({'cmdfs':[fle]})
               else: modules[genmod]['cmdfs'].append(fle)
         if f[len(f)-1].lower() in ['.f','.f90']:      # /!\ you found a source file
            if not moddir in modules: modules.update({moddir:{'path':dirpath,'files':[fle],'cmdfs':[]}})
            elif not 'files' in modules[moddir]: modules[moddir].update({'files':[fle]})
            elif fle not in modules[moddir]['files']: modules[moddir]['files'].append(fle)

      # ~~> Higher levels in modroot
      for subdir in dirnames:
         if subdir[0] == '.': continue # .svn directories filter for old releases
         for subpath, subnames, filenames in walk(path.join(modroot,subdir)):
            if moddir not in ['utils']:
               for fle in filenames :
                  f = path.splitext(fle)
                  if f[len(f)-1].lower() in ['.f','.f90']:      # /!\ you found a source file
                     if not moddir in modules: modules.update({moddir:{'path':subpath,'files':[fle],'cmdfs':[]}})
                     elif not 'files' in modules[moddir]: modules[moddir].update({'files':[fle]})
                     else: modules[moddir]['files'].append(fle)
            else:
               for fle in filenames:
                  f = path.splitext(fle)
                  if f[len(f)-1].lower() in ['.dico']:   # /!\ you found an executable module
                     if not subdir in modules: modules.update({subdir:{'path':subpath,'dico':fle,'cmdfs':[]}})
                     else: modules[subdir].update({'dico':fle})
                  if f[len(f)-1].lower() in ['.cmdf']:   # /!\ you found at least one cmdf file
                     if rescan: remove(path.join(subpath,fle))
                     else:
                        cmdf = getScanContent(path.join(subpath,fle),root,bypass)
                        genmod = cmdf['general']['module']
                        if subpath != cmdf['general']['path']:
                           raise Exception([{'name':'getFolders_ModulesTELEMAC','msg':'... this cmdf file '+fle+' is out of place\n     ... check its internal path.'}])
                        if not genmod in modules: modules.update({genmod:{'path':subpath,'cmdfs':[fle]}})
                        elif not 'cmdfs' in modules[genmod]: modules[genmod].update({'cmdfs':[fle]})
                        else: modules[genmod]['cmdfs'].append(fle)
                  if f[len(f)-1].lower() in ['.f','.f90']:      # /!\ you found a source file
                     if not subdir in modules: modules.update({subdir:{'path':subpath,'files':[fle],'cmdfs':[]}})
                     elif not 'files' in modules[subdir]: modules[subdir].update({'files':[fle]})
                     elif fle not in modules[subdir]['files']: modules[subdir]['files'].append(fle)

   return modules

"""
   Etract links to user defined external dependencies
   whether a lib, a mod or an include following the template
   key ext_all and ext_..., with ext = mods, incs, libs
"""
def addEXTERNALs(cfgDict,ext,mod): # key ext_all and ext_..., with ext = mods, incs, libs, cmd_*
   # ~~ Loads External Dependencies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   extList = ''
   if ext in cfgDict: extList = cfgDict[ext]
   ext_all = ext + '_all'
   if ext_all in cfgDict: extList += ' ' + cfgDict[ext_all]
   ext_mod = ext + '_' + mod
   if ext_mod in cfgDict: extList += ' ' + cfgDict[ext_mod]
   return extList

"""
   Etract links to user defined external dependencies
   whether a lib, a mod or an include following the template
   key ext_all and ext_..., with ext = cmd_obj, cmd_lib, cmd_exe
"""
def getEXTERNALs(cfgDict,ext,mod): # key ext_all and ext_..., with ext = mods, incs, libs, cmd_*
   # ~~ Loads External Dependencies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   extList = ''
   if 'cmd_' + ext in cfgDict: extList = cfgDict['cmd_'+ext]
   ext_all = 'cmd_' + ext + '_all'
   if ext_all in cfgDict: extList = cfgDict[ext_all]
   ext_mod = 'cmd_' + ext + '_' + mod
   if ext_mod in cfgDict: extList = cfgDict[ext_mod]
   return extList

"""
   Extract full user defined comand line
   for the treatment of the tags, where tag_(name) produce a special effect on (name)
"""

def getTAGs(key,cfgDict,sources):
   # ~~ Loads tag and creates exception ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   modules = {}
   tags = {}
   for k in cfgDict:
      if k.split('_')[0] != key: continue
      tag = '_'.join(k.split('_')[1:])
      foundFiles = cfgDict[k].split()
      tags.update({tag:[]})
      for fle in foundFiles:
         if tag not in modules: modules.update({tag:{'path':'','cmdfs':[],'files':[]}})
         if tag in sources:
            modules[tag]['path'] = sources[tag]['path']
            modules[tag]['cmdfs'] = sources[tag]['cmdfs']
         if fle not in modules[tag]['files']: modules[tag]['files'].append(fle)
         tags[tag].append(fle)

   return tags,modules

"""
   Extract full user defined comand line
   for the treatment of the PARTEL in parallel
"""
def getPARTEL(cfgDict):
   # ~~ Loads Compiler Commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   partel = {}
   if cfgDict.has_key('par_path'): partel.update({'PATH':cfgDict['par_path']})
   if cfgDict.has_key('par_cmdexec'): partel.update({'EXEC':cfgDict['par_cmdexec']})
   return partel

"""
   Extract full user defined comand line
   for the treatment of the option 'mpi'
"""
def getMPI(cfgDict):
   # ~~ Loads Compiler Commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mpi = {}
   if cfgDict.has_key('mpi_cmdexec'):
      mpi.update({'EXEC':cfgDict['mpi_cmdexec']})
      if cfgDict.has_key('mpi_infile'): mpi.update({'INFILE':cfgDict['mpi_infile']})
      mpi.update({'HOSTS':gethostname().split('.')[0]}) # /!\ defaulting on  the local hostname
      if cfgDict.has_key('mpi_hosts'):
         if len(cfgDict['mpi_hosts'].split()) > 0: mpi['HOSTS'] = cfgDict['mpi_hosts']
      mpi.update({'HOSTFILE': 'MPI_HOSTFILE'})
   return mpi

"""
   Extract full user defined comand line
   for the treatment of the option 'hpc', Here bsub is used as an example
"""
def getHPC(cfgDict):
   # ~~ Loads Compiler Commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   hpc = {}
   if cfgDict.has_key('hpc_cmdexec'):
      hpc.update({'EXCODE':cfgDict['hpc_cmdexec']})
      if cfgDict.has_key('hpc_stdin'): hpc.update({'STDIN':['HPC_STDIN',cfgDict['hpc_stdin'].replace(r'\n','\n')]})
   if cfgDict.has_key('hpc_runcode'):
      hpc.update({'PYCODE':cfgDict['hpc_runcode']})
      if cfgDict.has_key('hpc_stdin'): hpc.update({'STDIN':['HPC_STDIN',cfgDict['hpc_stdin'].replace(r'\n','\n')]})
   if cfgDict.has_key('hpc_depend'):
      hpc.update({'DEPEND':cfgDict['hpc_depend']})
   return hpc

"""
   Read the list of user defined modules for action -- Certain
   keyword such as clean, update, system will trigger additonal
   behaviours:
    - clean: rebuilt = 1, rebuild object, libs and executables
    - update: rebuilt = 2, rebuild libs and executables
    - modify: rebuilt = 3, compile only the necessary files and rebuild libs and executables
    - system (or nothing): include all modules
   Will alos read the specif option and build the list of modules
   according to the stwiches, i.e. parallel, openmi, xdmf, etc.
"""
def parseUserModules(cfgDict,modules):
   # ~~ Loads To-Be Compiled ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   typeBuild = 0; userList = ''
   userList = 'clean ' + ' '.join(modules.keys())
   if 'modules' in cfgDict:
      if cfgDict['modules'].strip() != '': userList = cfgDict['modules']
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
   proc = re.match(re.compile(word%('modify'),re.I),userList)
   if proc :
      userList = proc.group('before') + ' ' + proc.group('after')
      typeBuild = 3
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
         sys.exit(1)
   # ~~ Deal with all ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if 'system' in userList : userList = ' '.join(modules.keys())

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
def parseValidationRanks(userList):

   # ~~ 'all' in key ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

def cleanConfig(cfg,cfgname):

   cfgDir = path.join(path.join(cfg['root'],'builds'),cfgname)
   if path.exists(cfgDir):
      removeDirectories(cfgDir)
      print '\n    +> build deleted!'
   else: print '\n ... Found no build to delete!'


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
List active configurations:\n
1. check if your system points to a valid configuration file
2. parse your configuration file and display the briefs of your active configurations
      '''))
   parser.add_argument(\
      "-c", "--configname",metavar="config name",
      #nargs="*" ... TODO: try supporting this
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
      "--clean",action="store_true",
      dest="configDelete",default=False,
      help="remove the directories in relation to the named configuration(s)" )
   options = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # The path to the root relates to the script launched, which implies
   # that the user environment knows which to run
   # (this script is stored under .../scripts/python27/)
   #PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   PWD = path.dirname(path.dirname( path.dirname(__file__)) )
   # if the appropriate command line option is used, then reset rootDir
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
      dircfg = path.dirname(options.configFile)
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         _, _, filenames = walk(dircfg).next()
         for fle in filenames:
            head,tail = path.splitext(fle)
            if tail == '.cfg' :
               print '    +> ',fle
      sys.exit(1)

   cfgs = parseConfigFile(options.configFile,options.configName,False)

   for cfgname in cfgs:
      print '\n\n'+'~'*72+'\n'
      # still in lower case
      if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
      if options.rootDir != '': cfgs[cfgname]['root'] = PWD
      # parsing for proper naming
      cfg = parseConfig_CompileTELEMAC(cfgs[cfgname],False,False)

      print cfgname + ': \n    '
      if 'brief' in cfgs[cfgname]: print '\n    +> '+'\n    |  '.join(cfgs[cfgname]['brief'].split('\n')) + '\n'
      print '    +> root:    ',cfg['root']
      print '    +> module:  ',' / '.join(cfg['MODULES'].keys())
      if options.configDelete: cleanConfig(cfg,cfgname)

   print '\n\n'+'~'*72+'\n'
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
