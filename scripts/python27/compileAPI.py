#!/usr/bin/env python
"""@author Y. Audouin
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
"""@brief This scripts compiles the API library and executable
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
import re
import sys
import time
import shutil
from subprocess import call
from os import path, sep, walk, chdir, remove, environ, mkdir, system, symlink
import argparse
# ~~> dependencies towards the root of pytel
from config import parseConfigFile,parseConfig_CompileTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.messages import MESSAGES,filterMessage,banner
from utils.progressbar import ProgressBar


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Y. Audouin"
__date__ ="$04-may-2016 14:04:00$"

def mycopy(src,dst):
   """
   Custom copy that will do a link is symlink is available a copy otherwise

   @param src The file to copy
   @param dst The destiantion where to copy the file
   """
   if path.exists(dst):
       remove(dst)
   symlink(src,dst)

def compile_API(cfgs,cfgname,fcompiler,user_fortran):
   print 'Compiling the API \n'+'~'*72+'\n'
   api_dir = path.join(cfgs[cfgname]['root'],'builds',cfgname,'wrap_api')
   lib_dir = path.join(cfgs[cfgname]['root'],'builds',cfgname,'lib')
   if not path.exists(api_dir):
      mkdir(api_dir)
      mkdir(api_dir+sep+'lib')
      mkdir(api_dir+sep+'src')
      mkdir(api_dir+sep+'include')
   # Copying libraries
   mycopy(path.join(lib_dir,'telemac2d','homere_api.so'),\
                   path.join(api_dir,'lib','libtelemac2d.so'))
   mycopy(path.join(lib_dir,'api','homere_api.so'),\
                   path.join(api_dir,'lib','libapi_telemac2d.so'))
   mycopy(path.join(lib_dir,'sisyphe','homere_api.so'),\
                   path.join(api_dir,'lib','libsisyphe.so'))
   mycopy(path.join(lib_dir,'nestor','homere_api.so'),\
                   path.join(api_dir,'lib','libnestor.so'))
   mycopy(path.join(lib_dir,'tomawac','homere_api.so'),\
                   path.join(api_dir,'lib','libtomawac.so'))
   mycopy(path.join(lib_dir,'waqtel','homere_api.so'),\
                   path.join(api_dir,'lib','libwaqtel.so'))
   mycopy(path.join(lib_dir,'utils','bief','homere_api.so'),\
                   path.join(api_dir,'lib','libbief.so'))
   mycopy(path.join(lib_dir,'utils','special','homere_api.so'),\
                   path.join(api_dir,'lib','libspecial.so'))
   mycopy(path.join(lib_dir,'utils','parallel','homere_api.so'),\
                   path.join(api_dir,'lib','libparallel.so'))
   mycopy(path.join(lib_dir,'utils','damocles','homere_api.so'),\
                   path.join(api_dir,'lib','libdamocles.so'))
   mycopy(path.join(lib_dir,'utils','hermes','homere_api.so'),\
                   path.join(api_dir,'lib','libhermes.so'))
   mycopy(path.join(lib_dir,'utils','gretel','homere_api.so'),\
                   path.join(api_dir,'lib','libgretel.so'))
   mycopy(path.join(lib_dir,'utils','partel','homere_api.so'),\
                   path.join(api_dir,'lib','libpartel.so'))
   # Copying Modules
   for root, dirs, files in walk(lib_dir):
      for ffile in files:
         if ffile.endswith("mod"):
            mycopy(path.join(root,ffile),
                            path.join(api_dir,'include',ffile))

   # Copying Sources
   mycopy(path.join(cfgs[cfgname]['root'],'sources','api','api_handle_var_t2d.f'),\
                   path.join(api_dir,'src','api_handle_var_t2d.f90'))
   mycopy(path.join(cfgs[cfgname]['root'],'sources','api','api_handle_var_sis.f'),\
                   path.join(api_dir,'src','api_handle_var_sis.f90'))
   mycopy(path.join(cfgs[cfgname]['root'],'sources','api','api_handle_error.f'),\
                   path.join(api_dir,'src','api_handle_error.f90'))
   mycopy(path.join(cfgs[cfgname]['root'],'sources','api','api_interface.f'),\
                   path.join(api_dir,'src','api_interface.f90'))
   # Generating Py wrapper using f2py
   chdir(api_dir+sep+'lib')
   source = '<apiSrc>api_handle_var_t2d.f90 <apiSrc>api_handle_var_sis.f90 '
   source += ' <apiSrc>api_handle_error.f90 <apiSrc>api_interface.f90'
   skipSource = 'get_boolean_t2d_d get_double_t2d_d get_integer_t2d_d get_string_t2d_d '
   skipSource += 'get_var_size_t2d_d set_boolean_t2d_d set_double_t2d_d set_integer_t2d_d set_string_t2d_d '
   skipSource += 'get_boolean_sis_d get_double_sis_d get_integer_sis_d get_string_sis_d '
   skipSource += 'get_var_size_sis_d set_boolean_sis_d set_double_sis_d set_integer_sis_d set_string_sis_d'
   pyfFile = 'api.pyf'
   if path.exists(pyfFile):
      remove(pyfFile)
   # First step of call to f2py
   print 'f2py -h %s -m _api %s skip: %s :'%(pyfFile,
             source.replace('<apiSrc>',api_dir+sep+'src'+sep),skipSource)
   try:
      retcode = call('f2py -h %s -m _api %s skip: %s :'%(pyfFile,
             source.replace('<apiSrc>',api_dir+sep+'src'+sep),skipSource),shell=True)
      if retcode != 0:
         print 'Error during first part of f2py return code:',retcode
         sys.exit(1)
   except OSError as e:
      print 'Error during first part of f2py'
      sys.exit(1)

   # Compile user Fortran
   # TODO: Replace .so by extension from config
   ld_flags = cfgs[cfgname]['libs_all'].replace('<root>',cfgs[cfgname]['root']).replace('\n',' ')
   if user_fortran != '':
      if fcompiler == 'gfortran':
         command = "gfortran %s -shared -fconvert=big-endian -o %s %s -I%s -fPIC" % \
                   (user_fortran,"libuser_fortran.so",ld_flags,api_dir+sep+'include')
         print '+> Compiling User fortran with gfortran'
         print command
         ret = system(command)
         if ret != 0:
            raise Exception("Cannot compile user fortran file %s" % user_fortran)
      elif fcompiler == 'intel':
         #TODO: For Cedric Add intel compilation
         print 'Intel compilation of user_fortran not implemented yet'
         return

   # Second step of call to f2py
   lib_cmd = ld_flags
   lib_cmd += ' -L'+api_dir+sep+'lib'
   if user_fortran != '':
      lib_cmd += ' ' + '-luser_fortran'
   lib_cmd += ' -ltelemac2d -lapi_telemac2d -lwaqtel -lbief -lspecial -lnestor'
   lib_cmd += ' -lsisyphe -ltomawac -lparallel -ldamocles -lhermes -lgretel -lpartel'
   print 'f2py -c %s --fcompiler=%s -I%s %s '%(pyfFile,args.fcompiler,api_dir+sep+'include',lib_cmd)
   try:
      retcode = call('f2py -c %s --fcompiler=%s -I%s %s '%(pyfFile,\
                                                           args.fcompiler,
                                                           api_dir+sep+'include',
                                                           lib_cmd),
                     shell=True)
      if retcode != 0:
         raise Exception('Error during second part of f2py return code:'+str(retcode))
   except OSError as e:
      raise Exception('Error during second part of f2py',e)

def compile_obj(fortranFile,cfg,cfgName):
   """
   Compile a fortran using configuration information

   @param fortranFile The file to be compiled
   @param cfg The configuration object
   @param cfgName The name of the configuration to compile the file with
   """
   root,suffix = path.splitext(path.basename(fortranFile))
   cmd = cfg['cmd_obj']
   incs = cfg['incs_all']
   cmd = cmd.replace('<incs>',incs)
   mods = cfg['mods_all'].replace('<config>',
          path.join(cfg['root'], 'builds', cfgName, 'wrap_api', 'include'))
   cmd = cmd.replace('<mods>',mods)
   cmd = cmd.replace('<f95name>',fortranFile)
   print cmd
   try:
      retcode = call(cmd,shell=True)
      if retcode != 0:
         raise Exception("Cannot compile fortran file %s" % fortranFile)
   except OSError as e:
      raise Exception("Cannot compile fortran file %s" % fortranFile)

def compile_exe(fortranFile,cfg,cfgName,user_fortran=''):
   """
   Compile a fortran using configuration information

   @param fortranFile The file to be compiled
   @param cfg The configuration object
   @param cfgName The name of the configuration to compile the file with
   """
   root,suffix = path.splitext(path.basename(fortranFile))
   cmd = cfg['cmd_exe']
   libs = cfg['libs_all']+' -L'+path.join(cfg['root'],'builds',cfgName,'wrap_api','lib')+" "
   deps_t2d = ['special', 'parallel', 'damocles', 'hermes',
           'bief', 'partel', 'gretel', 'waqtel', 'sisyphe',
           'tomawac', 'telemac2d', 'api_telemac2d']
   for lib in deps_t2d:
      libs += "-l"+lib+" "
   cmd = cmd.replace('<libs>',libs)
   objs = root+cfg['sfx_obj']
   if user_fortran != '':
      print 'user_fortran',user_fortran
      root2,suffix2 = path.splitext(path.basename(user_fortran))
      objs += ' ' +  root2+ cfg['sfx_obj']
   cmd = cmd.replace('<objs>',objs)
   cmd = cmd.replace('<exename>',root+cfg['sfx_exe'])
   print cmd
   try:
      retcode = call(cmd,shell=True)
      if retcode != 0:
         raise Exception("Cannot compile fortran file %s" % fortranFile)
   except OSError as e:
      raise Exception("Cannot compile fortran file %s" % fortranFile)

def compile_API_exe(programFile,cfgs,cfgname,user_fortran):
   """
   Compile the Fortran program file with the Telemac-Mascaret libraries
   and compile the user Fortran file if there is one

   @param programFile The program fortran
   @param cfgs Configurations informations
   @param cfgname Name of the configuration used here
   @param user_fortran The user Fortran file
   """
   cfg = cfgs[cfgname]
   if user_fortran != '':
      compile_obj(user_fortran,cfg,cfgname)
   # Compile the programfile.o
   compile_obj(programFile,cfg,cfgname)
   compile_exe(programFile,cfg,cfgname,user_fortran=user_fortran)

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
   parser = argparse.ArgumentParser(description='Compile Telemac-Mascaret API '\
                                                'and/or executable using the API')
   parser.add_argument("-c","--configname",
             dest='configName',
             default="",
             help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_argument("-f","--configfile",
             dest='configFile',
             default="",
             help="specify configuration file, default is systel.cfg" )
   parser.add_argument("-r","--rootdir",
             dest='rootDir',
             default="",
             help="specify the root, default is taken from config file" )
   parser.add_argument("--fcompiler",
             dest="fcompiler",
             default='gfortran',
             help="spectify the option to give to f2py, default is gfortran" )
   parser.add_argument("--user-fortran",
             dest="user_fortran",
             default='',
             help="give a user fortran to recompile the API with, default is None")
   parser.add_argument("--exe",
             dest="exeFile",
             default="",
             help="Compile the program exeFile")
   args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # path to the root
   HOMETEL = ''
   if 'HOMETEL' in environ: HOMETEL = environ['HOMETEL']
   if args.rootDir == '': args.rootDir = HOMETEL
   # user configuration name
   USETELCFG = ''
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   if args.configName == '': args.configName = USETELCFG
   # user configuration file
   SYSTELCFG = path.join(HOMETEL,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if args.configFile != '': SYSTELCFG = args.configFile
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   args.configFile = SYSTELCFG

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   if not path.isfile(args.configFile):
      print '\nNot able to get to the configuration file: ' + args.configFile + '\n'
      dircfg = path.abspath(path.dirname(args.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         _, _, filenames = walk(dircfg).next()
         for fle in filenames :
            head,tail = path.splitext(fle)
            if tail == '.cfg' :
               print '    +> ',fle
      raise Exception('Error in configuration file')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(args.configFile,args.configName)

   for cfgname in cfgs:
      # still in lower case
      if not cfgs[cfgname].has_key('root'):
         cfgs[cfgname]['root'] = HOMETEL
      # parsing for proper naming
      cfg = parseConfig_CompileTELEMAC(cfgs[cfgname])
      print '\n\n'+'\n'.join(banner(cfgname))

      # Check if we are compiling a fortran or recompiling the API
      if args.exeFile != "":
         compile_API_exe(args.exeFile,cfgs,cfgname,args.user_fortran)
      else:
         compile_API(cfgs,cfgname,args.fcompiler,args.user_fortran)

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
