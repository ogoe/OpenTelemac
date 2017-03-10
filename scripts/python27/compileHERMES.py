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
from utils.files import isNewer


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
        if isNewer(src, dst) == 0:
            remove(dst)
            shutil.copy2(src, dst)
    else:
        shutil.copy2(src, dst)


def compile_api(cfgs, cfgname, fcompiler, silent):
    """
    Compiling the API for Telemac-Mascaret

    @param cfgs List of configurations info
    @param cfgname Name of the configuration for which we compile the API
    @param fcompiler Fortran Compiler to use
    @param silent Deactivates f2py listing
    """
    print 'Compiling the API \n'+'~'*72+'\n'
    api_dir = path.join(cfgs[cfgname]['root'], 'builds', cfgname, 'wrap_api')
    lib_dir = path.join(cfgs[cfgname]['root'], 'builds', cfgname, 'lib')
    if not path.exists(api_dir):
        mkdir(api_dir)
        mkdir(api_dir+sep+'lib')
        mkdir(api_dir+sep+'src')
        mkdir(api_dir+sep+'include')
    # Copying libraries
    mycopy(path.join(lib_dir, 'utils', 'special', 'homere_api.so'), \
                     path.join(api_dir, 'lib', 'libspecial.so'))
    mycopy(path.join(lib_dir, 'utils', 'parallel', 'homere_api.so'), \
                     path.join(api_dir, 'lib', 'libparallel.so'))
    mycopy(path.join(lib_dir, 'utils', 'hermes', 'homere_api.so'), \
                     path.join(api_dir, 'lib', 'libhermes.so'))
    # Copying Modules
    for root, _, files in walk(lib_dir):
        for ffile in files:
            if ffile.endswith("mod"):
                mycopy(path.join(root, ffile),
                       path.join(api_dir, 'include', ffile))

    # Copying Sources
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

    source = ''
    for src in src_list:
        root, ext = path.splitext(src)
        # Copying source in wrap_api folder and
        # changin extension into .f90
        mycopy(path.join(src_dir, src), \
               path.join(api_dir, 'src', root+'.f90'))
        # Building list of sources
        source += '<apiSrc>' + root + '.f90 '

    print "    ~> Wrap_hermes built"

    pyf_file = path.join(api_dir, 'lib', 'hermes.pyf')
    if path.exists(pyf_file):
        remove(pyf_file)
    # First step of call to f2py
    cmd = 'f2py --quiet -h %s -m _hermes %s :'\
            %(pyf_file,
              source.replace('<apiSrc>', path.join(api_dir, 'src') + sep))
    try:
        output = check_output(cmd, shell=True, stderr=STDOUT)
    except CalledProcessError as execpt:
        print 'Error during first part of f2py ', execpt.returncode
        print execpt.output
        sys.exit(1)
    if not silent:
        print output
    print "    ~> First part of f2py passed"

    ld_flags = cfgs[cfgname]['libs_all'].replace('<root>',
                                                 cfgs[cfgname]['root'])\
                                        .replace('\n', ' ')
    ld_flags += ' -L'+api_dir+sep+'lib'
    ld_flags += ' -lspecial -lparallel -lhermes '

    pwd = getcwd()
    chdir(path.join(api_dir, 'lib'))
    # Second step of call to f2py
    cmd = 'f2py --quiet -c %s --fcompiler=%s -I%s %s '\
              %(pyf_file, fcompiler, api_dir+sep+'include', ld_flags)
    try:
        output = check_output(cmd, shell=True, stderr=STDOUT)
    except CalledProcessError as execpt:
        print 'Error during second part of f2py ', execpt.returncode
        print execpt.output
        sys.exit(1)
    if not silent:
        print output
    print "    ~> Second part of f2py passed"
    chdir(pwd)

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

def main(config_name, config_file, root_dir, fcompiler, silent):
    """
    Main function

    @param config_name Name of the telemac configuration
    @param config_file Name of the configuration file
    @param root_dir Path to the root folder of telemac
    @param fcompiler Fortran compiler to use
    @param silent Deactivates f2py listing
    """


    cfgs = build_config(config_name, config_file, root_dir)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xcptss = MESSAGES()

    for cfgname in cfgs:
        print '\n\n'+'\n'.join(banner(cfgname))

        compile_api(cfgs, cfgname, fcompiler, silent)
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
              "--silent",
              dest='silent',
              action='store_true',
              help="Compile the program exeFile")
    ARGS = PARSER.parse_args()

# Running main function
    XCPTS = main(ARGS.configName, ARGS.configFile, ARGS.rootDir,
                 ARGS.fcompiler, ARGS.silent)

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
