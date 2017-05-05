#!/usr/bin/env python
r"""@author Y. Audouin

   @note ... this work is based on a collaborative effort between
  .________.                                                          ,--.
  |        |                                                      .  (  (
  |,-.    /   HR Wallingford                EDF - LNHE           / \_ \_/ .--.
  /   \  /    Howbery Park,                 6, quai Watier       \   )   /_   )
   ,.  `'     Wallingford, Oxfordshire      78401 Cedex           `-'_  __ `--
  /  \   /    OX10 8BA, United Kingdom      Chatou, France        __/ \ \ `.
 /    `-'|    www.hrwallingford.com         innovation.edf.com   |    )  )  )
!________!                                                        `--'   `--

   @brief This scripts compiles the API library and executable
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
import re
import sys
import shutil
from subprocess import call, STDOUT, check_output, CalledProcessError
from os import path, sep, walk, chdir, remove, environ, mkdir, \
               listdir, getcwd
import argparse
# ~~> dependencies towards the root of pytel
from config import parseConfigFile
# ~~> dependencies towards other pytel/modules
from utils.messages import MESSAGES, filterMessage, banner


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__ = "Y. Audouin"
__date__ = "$04-may-2016 14:04:00$"

def mycopy(src, dst):
    """
    Custom copy that will remove the destination first if it is there

    @param src The file to copy
    @param dst The destiantion where to copy the file
    """
    if path.exists(dst):
        remove(dst)
    shutil.copy2(src, dst)

def get_api_incs_flags(cfgs, cfgname):
    """
    Retuns the string for incs_flags for api

    @param cfgs Configuration structure
    @param cfgname Name of the configuration

    @returns the string
    """
    api_dir = path.join(cfgs[cfgname]['root'], 'builds', cfgname, 'wrap_api')

    incs_flags = cfgs[cfgname]['incs_all'].replace('<root>',
                                                   cfgs[cfgname]['root'])\
                                          .replace('\n', ' ')
    incs_flags += ' -I'+api_dir+sep+'include'

    return incs_flags

def get_api_ld_flags(cfgs, cfgname):
    """
    Retuns the string for ld_flags for api

    @param cfgs Configuration structure
    @param cfgname Name of the configuration

    @returns the string
    """
    api_dir = path.join(cfgs[cfgname]['root'], 'builds', cfgname, 'wrap_api')

    ld_flags = cfgs[cfgname]['libs_all'].replace('<root>',
                                                 cfgs[cfgname]['root'])\
                                        .replace('\n', ' ')
    ld_flags += ' -L'+api_dir+sep+'lib'
    ld_flags += ' -ltelemac2d -lapi -lwaqtel -lbief -lspecial -lnestor'
    ld_flags += ' -lsisyphe -ltomawac -lparallel -ldamocles'
    ld_flags += ' -lhermes -lgretel -lpartel'

    return ld_flags

def compile_princi_lib(princi_file, cfgname, cfgs, incs_flags, ld_flags):
    """
       Compiling user fortran as a library

       @param user_fortran Name of the user_fortran
       @param cfgname Name of the configuration
       @param cfgs Configuration structure
       @param incs_flags Include flags for compilation
       @param ld_flags Linking flags for compilation
    """
    if not path.exists(princi_file):
        raise Exception([{
            'name':'compile_princi_lib',
            'msg':'could not find your FORTRAN: '+princi_file}])

    user_fortran = []
    # in case of a folder getting list of files
    if path.isdir(princi_file):
        list_files = listdir(princi_file)
        for fle in list_files:
            if re.match("^m[0-9]+.*", fle) and \
               fle.lower().endswith((".f", ".f90")):
                user_fortran.append(path.join(princi_file, fle))
        # Adding the other files
        for fle in list_files:
            if fle not in user_fortran and \
               fle.lower().endswith((".f", ".f90")):
                user_fortran.append(path.join(princi_file, fle))
    else:
        user_fortran = [princi_file]
    # Building linking commands
    command = cfgs[cfgname]['cmd_lib'].replace('<libname>',
                                               "libuser_fortran" + \
                                                cfgs[cfgname]['sfx_lib'])\
                                      .replace('<objs>', ' '.join(user_fortran))
    command += ' ' + incs_flags + ' ' + ld_flags

    mes = MESSAGES(size=10)
    print "command",command
    try:
        tail, code = mes.runCmd(command, False)
    except Exception as execpt:
        raise Exception([filterMessage(\
                {'name':'compile_princi_lib',
                 'msg':'something went wrong for no reason. \
                        Please verify your compiler installation.'
                }, execpt, False)])
    if code != 0:
        raise Exception([{
            'name':'compile_princi_lib',
            'msg':'could not compile your FORTRAN \
                   (runcode='+str(code)+').\n      '+tail}])

def copy_src_api(api_dir, src_list, src_dir):
    """
    Copying sources in src_list from src_dir in api folder renaming them in .f90
    And build a string containing list of names

    @param api_dir Path to the api directory
    @param src_list List of files to copy
    @param src_dir Path of the sources

    @returns String containing list of new names for f2py
    """

    source = ''
    for src in src_list:
        root, ext = path.splitext(src)
        # Copying source in wrap_api folder and
        # changin extension into .f90
        mycopy(path.join(src_dir, src), \
               path.join(api_dir, 'src', root+'.f90'))
        # Building list of sources
        source += path.join(api_dir, 'src', root+'.f90') + ' '

    return source

def generate_api(cfgs, cfgname):
    """
    Builds the structure for an Python API

    @param cfgs List of configurations info
    @param cfgname Name of the configuration for which we compile the API

    @returns source list for api and for hermes
    """
    api_dir = path.join(cfgs[cfgname]['root'], 'builds', cfgname, 'wrap_api')
    lib_dir = path.join(cfgs[cfgname]['root'], 'builds', cfgname, 'lib')
    if not path.exists(api_dir):
        mkdir(api_dir)
        mkdir(api_dir+sep+'lib')
        mkdir(api_dir+sep+'src')
        mkdir(api_dir+sep+'include')
    # Copying libraries
    dyn_ext = cfgs[cfgname]['sfx_lib']
    list_libs = ['telemac2d', 'api', 'sisyphe', 'nestor', 'tomawac', \
                 'waqtel', 'utils|bief', 'utils|special', 'utils|parallel', \
                 'utils|hermes', 'utils|gretel', 'utils|partel', 'utils|damocles']
    for lib in list_libs:
        lib_name = lib.split('|')[-1]
        mycopy(path.join(lib_dir, lib.replace('|',sep), 'homere_api'+dyn_ext), \
                         path.join(api_dir, 'lib', 'lib'+lib_name+dyn_ext))

    # Copying Modules
    for root, _, files in walk(lib_dir):
        for ffile in files:
            if ffile.endswith("mod"):
                mycopy(path.join(root, ffile),
                       path.join(api_dir, 'include', ffile))

    # Copying sources for t2d and sis
    src_list = []
    src_list.append("api_handle_var_t2d.f")
    src_list.append("api_handle_var_sis.f")
    src_list.append("api_handle_error.f")
    src_list.append("api_interface.f")
    src_dir = path.join(cfgs[cfgname]['root'], 'sources', 'api')
    source_api = copy_src_api(api_dir, src_list, src_dir)
    # Copying sources for hermes
    src_list = []
    src_list.append("close_bnd.f")
    src_list.append("close_mesh.f")
    src_list.append("get_bnd_connectivity.f")
    src_list.append("get_bnd_ipobo.f")
    src_list.append("get_bnd_nelem.f")
    src_list.append("get_bnd_npoin.f")
    src_list.append("get_bnd_numbering.f")
    src_list.append("get_bnd_value.f")
    src_list.append("get_data_ntimestep.f")
    src_list.append("get_data_nvar.f")
    src_list.append("get_data_time.f")
    src_list.append("get_data_value.f")
    src_list.append("get_data_var_list2.f")
    src_list.append("get_mesh_connectivity.f")
    src_list.append("get_mesh_coord.f")
    src_list.append("get_mesh_date.f")
    src_list.append("get_mesh_dimension.f")
    src_list.append("get_mesh_l2g_numbering.f")
    src_list.append("get_mesh_nelem.f")
    src_list.append("get_mesh_nplan.f")
    src_list.append("get_mesh_npoin.f")
    src_list.append("get_mesh_npoin_per_element.f")
    src_list.append("get_mesh_nptir.f")
    src_list.append("get_mesh_title.f")
    src_list.append("open_bnd.f")
    src_list.append("open_mesh.f")
    src_list.append("set_bnd.f")
    src_list.append("set_mesh.f")
    src_list.append("set_header.f")
    src_list.append("add_data.f")
    src_list.append("transfer_group_info.f")
    src_dir = path.join(cfgs[cfgname]['root'], 'sources', 'utils', 'hermes')
    source_hermes = copy_src_api(api_dir, src_list, src_dir)

    return source_api, source_hermes

def compile_api_f2py(name, api_dir, source_list, skip_source, ld_flags, fcompiler, silent):
    """
    Running f2py to generate Python wrapper

    @param name Name of the wrapper
    @param api_dir Path to the api folder
    @param source_list List of source for the api
    @param skip_source List of function to skip
    @param ld_flags Linking flags
    @param fcompiler Name of the compiler
    """

    # Generating Py wrapper using f2py
    pyf_file = path.join(api_dir, 'lib', name+'.pyf')
    if path.exists(pyf_file):
        remove(pyf_file)
    if skip_source != '':
        skip_source = 'skip: ' + skip_source + ' :'
    # First step of call to f2py
    cmd = 'f2py --quiet -h %s -m _%s %s %s'\
            %(pyf_file,
              name,
              source_list,
              skip_source)
    try:
        output = check_output(cmd, shell=True, stderr=STDOUT)
    except CalledProcessError as execpt:
        print 'Error during first part of f2py for ',name, execpt.returncode
        print execpt.output
        sys.exit(1)
    if not silent:
        print output
    print "    ~> First part of f2py for %s passed"%name

    pwd = getcwd()
    chdir(path.join(api_dir, 'lib'))
    # Second step of call to f2py
    cmd = 'f2py --quiet -c %s --fcompiler=%s -I%s %s '\
              %(pyf_file, fcompiler, path.join(api_dir,'include'), ld_flags)
    try:
        output = check_output(cmd, shell=True, stderr=STDOUT)
    except CalledProcessError as execpt:
        print 'Error during second part of f2py for ',name, execpt.returncode
        print execpt.output
        sys.exit(1)
    if not silent:
        print output
    print "    ~> Second part of f2py of %s passed"%name
    chdir(pwd)

def compile_api(cfgs, cfgname, silent, fcompiler="gnu95"):
    """
       Compiling the APIs for Telemac-Mascaret

       @param cfgs List of configurations info
       @param cfgname Name of the configuration for which we compile the API
    """
    print '\nCompiling the API \n'+'~'*72+'\n'

    source_api, source_hermes = generate_api(cfgs, cfgname)
    print "    ~> Wrap_api built"

    skip_source = 'get_boolean_t2d_d get_double_t2d_d '
    skip_source += 'get_integer_t2d_d get_string_t2d_d '
    skip_source += 'get_var_size_t2d_d set_boolean_t2d_d set_double_t2d_d '
    skip_source += 'set_integer_t2d_d set_string_t2d_d '
    skip_source += 'get_boolean_sis_d get_double_sis_d '
    skip_source += 'get_integer_sis_d get_string_sis_d '
    skip_source += 'get_var_size_sis_d set_boolean_sis_d set_double_sis_d '
    skip_source += 'set_integer_sis_d set_string_sis_d'
    ld_flags = get_api_ld_flags(cfgs, cfgname)
    api_dir = path.join(cfgs[cfgname]['root'], 'builds', cfgname, 'wrap_api')
    print "    ~> Compiling Modules api"
    compile_api_f2py('api', api_dir, source_api, skip_source, ld_flags, fcompiler, silent)
    print "    ~> Compiling hermes api"
    compile_api_f2py('hermes', api_dir, source_hermes, '', ld_flags, fcompiler, silent)

def compile_obj(fortran_file, cfg, cfgname):
    """
       Compile a fortran using configuration information

       @param fortran_file The file to be compiled
       @param cfg The configuration object
       @param cfgname The name of the configuration to compile the file with
    """
    cmd = cfg['cmd_obj']
    incs = cfg['incs_all']
    cmd = cmd.replace('<incs>', incs)
    mods = cfg['mods_all'].replace('<config>',
                                   path.join(cfg['root'],
                                             'builds',
                                             cfgname,
                                             'wrap_api',
                                             'include'))
    cmd = cmd.replace('<mods>', mods)
    cmd = cmd.replace('<f95name>', fortran_file)
    print cmd
    try:
        retcode = call(cmd, shell=True, stderr=STDOUT)
    except CalledProcessError:
        raise Exception("Cannot compile fortran file %s error: %d" \
                          % (fortran_file, retcode))

def compile_exe(fortran_file, cfg, cfgname, user_fortran=''):
    """
    Compile a fortran using configuration information

    @param fortran_file The file to be compiled
    @param cfg The configuration object
    @param cfgname The name of the configuration to compile the file with
    """
    root, _ = path.splitext(path.basename(fortran_file))
    cmd = cfg['cmd_exe']
    libs = cfg['libs_all'] + ' -L'+path.join(cfg['root'], 'builds',
                                             cfgname, 'wrap_api', 'lib')\
           + " "
    deps_t2d = ['special', 'parallel', 'damocles', 'hermes',
                'bief', 'partel', 'gretel', 'waqtel', 'nestor', 'sisyphe',
                'tomawac', 'telemac2d', 'api']
    for lib in deps_t2d:
        libs += "-l"+lib+" "
    cmd = cmd.replace('<libs>', libs)
    objs = root + cfg['sfx_obj']
    if user_fortran != '':
        root2, _ = path.splitext(path.basename(user_fortran))
        objs += ' ' +  root2 + cfg['sfx_obj']
    cmd = cmd.replace('<objs>', objs)
    cmd = cmd.replace('<exename>', root+cfg['sfx_exe'])
    print cmd
    try:
        retcode = call(cmd, shell=True, stderr=STDOUT)
    except CalledProcessError:
        raise Exception("Cannot compile exe fortran file %s error: %d" \
                          % (fortran_file, retcode))

def compile_api_exe(program_file, cfgs, cfgname, user_fortran):
    """
    Compile the Fortran program file with the Telemac-Mascaret libraries
    and compile the user Fortran file if there is one

    @param program_file The program fortran
    @param cfgs Configurations informations
    @param cfgname Name of the configuration used here
    @param user_fortran The user Fortran file
    """
    cfg = cfgs[cfgname]
    if user_fortran != '':
        compile_obj(user_fortran, cfg, cfgname)
    # Compile the program_file.o
    compile_obj(program_file, cfg, cfgname)
    compile_exe(program_file, cfg, cfgname, user_fortran=user_fortran)

def build_config(config_name, config_file, root_dir):
    """
       Builds the configuration object

       @param config_name Name of the telemac configuration
       @param config_file Name of the configuration file
       @param root_dir Path to the root folder of telemac

       @retuns The configuration object
    """

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # path to the root
    hometel = ''
    if 'HOMETEL' in environ:
        hometel = environ['HOMETEL']
    if root_dir == '':
        root_dir = hometel
    # user configuration name
    usetelcfg = ''
    if 'USETELCFG' in environ:
        usetelcfg = environ['USETELCFG']
    if config_name == '':
        config_name = usetelcfg
    # user configuration file
    systelcfg = path.join(hometel, 'configs')
    if 'SYSTELCFG' in environ:
        systelcfg = environ['SYSTELCFG']
    if config_file != '':
        systelcfg = config_file
    if path.isdir(systelcfg):
        systelcfg = path.join(systelcfg, 'systel.cfg')
    config_file = systelcfg

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
    if not path.isfile(config_file):
        print '\nNot able to get to the configuration file: '\
              + config_file + '\n'
        dircfg = path.abspath(path.dirname(config_file))
        if path.isdir(dircfg):
            print ' ... in directory: ' + dircfg + '\n ... use instead: '
            _, _, filenames = walk(dircfg).next()
            for fle in filenames:
                _, tail = path.splitext(fle)
                if tail == '.cfg':
                    print '    +> ', fle
        raise Exception('Error in configuration file')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
    cfgs = parseConfigFile(config_file, config_name)
    for cfgname in cfgs:
        # still in lower case
        if 'root' not in cfgs[cfgname]:
            cfgs[cfgname]['root'] = root_dir

    return cfgs

def compileAPI(config_name, config_file, root_dir, fcompiler, user_fortran,
         exe_file, silent):
    """
       Main function

       @param config_name Name of the telemac configuration
       @param config_file Name of the configuration file
       @param root_dir Path to the root folder of telemac
       @param fcompiler Fortran compiler to use
       @param user_fortran Name of the user_fortran
       @param exe_file Name of the file to compile
       @param silent Deactivates f2py listing
    """


    cfgs = build_config(config_name, config_file, root_dir)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xcptss = MESSAGES()

    for cfgname in cfgs:
        print '\n\n'+'\n'.join(banner(cfgname))

        # Check if we are compiling a fortran or recompiling the API
        if exe_file != "":
            compile_api_exe(exe_file, cfgs, cfgname, user_fortran)
        else:
            compile_api(cfgs, cfgname, fcompiler, user_fortran,
                        silent)
    return xcptss


if __name__ == "__main__":
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
    PARSER = argparse.ArgumentParser(\
                    description='Compile Telemac-Mascaret API '\
                                 'and/or executable using the API')
    PARSER.add_argument(\
              "-c", "--configname",
              dest='configName',
              default="",
              help="specify configuration name, default is randomly \
                    found in the configuration file")
    PARSER.add_argument(\
              "-f", "--configfile",
              dest='configFile',
              default="",
              help="specify configuration file, default is systel.cfg")
    PARSER.add_argument(\
              "-r", "--rootdir",
              dest='rootDir',
              default="",
              help="specify the root, default is taken from config file")
    PARSER.add_argument(\
              "--fcompiler",
              dest="fcompiler",
              default='gfortran',
              help="spectify the option to give to f2py, default is gfortran")
    PARSER.add_argument(\
              "--user-fortran",
              dest="user_fortran",
              default='',
              help="give a user fortran to recompile the API with,\
                    default is None")
    PARSER.add_argument(\
              "--exe",
              dest="exeFile",
              default="",
              help="Compile the program exeFile")
    PARSER.add_argument(\
              "--silent",
              dest='silent',
              action='store_true',
              help="Compile the program exeFile")
    ARGS = PARSER.parse_args()

# Running main function
    XCPTS = compileAPI(ARGS.configName, ARGS.configFile, ARGS.rootDir,
                 ARGS.fcompiler, ARGS.user_fortran, ARGS.exeFile, ARGS.silent)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if XCPTS.notEmpty():
        print '\n\nHummm ... I could not complete my work.\n'\
        '~'*72+'\n'+ XCPTS.exceptMessages()
        sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else:
        print '\n\nMy work is done\n\n'
        sys.exit(0)
