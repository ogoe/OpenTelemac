#!/usr/bin/env python
"""@author Yoann Audouin
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

# ~~> dependencies towards standard python
import re
import sys
import shutil
from subprocess import *
from os import path,walk,sep,environ
from argparse import ArgumentParser,RawDescriptionHelpFormatter
# ~~> dependencies towards other modules
from config import parseConfigFile,parseConfig_RunningTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.messages import MESSAGES,filterMessage,banner


def createMascaretFiles(cfg,cas):
   """
      Creates if not there the following files in the current folder:
      FichierCas.txt That contains the name of the steering file
      Abaques.txt ???
      Controle.txt ???

      param cfg Configuration object
      param cas Name of the cas file given as argument
   """
   # Create the FichierCas.txt if it does not exist
   if not path.isfile("FichierCas.txt"):
      print '~+> Creating FichierCas.txt'
      with open("FichierCas.txt",'w') as fobj:
         fobj.write("'"+cas+"'\n")
   # If already there checking that we have the right name inside
   else:
      print '~+> Checking FichierCas.txt'
      with open("FichierCas.txt",'rw') as fobj:
        casFile = fobj.readline()
      if casFile.strip("'\n") != cas:
         raise Exception([{'name':'createMascaretFiles',
                           'msg':'Incorrect CAS file \nIn FichierCas.txt: '\
                                 +casFile.strip("'\n")+\
                                 '\nIn argument      : '+cas}])

   # Copying the abaque file if necessary
   if not path.isfile("Abaques.txt"):
      print '~+> Copying Abaques.txt'
      shutil.copyfile(path.join(cfg['root'],'sources','mascaret','data',
                                'Abaques.txt'),
                      "Abaques.txt")
   # Copying the controle file if necessary
   if not path.isfile("Controle.Txt"):
      print '~+> Copying Controle.txt'
      shutil.copyfile(path.join(cfg['root'],'sources','mascaret','data',
                                'Controle.txt'),
                      "Controle.txt")


def main():
   """
      Main function that runs the mascaret executable in the current folder
   """

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n\
Run the mascaret executable in the current folder, given a CAS file.
      '''))
   parser.add_argument( "args",nargs='*' )
   # ~~> Environment
   parser.add_argument(\
      "-c", "--configname",dest="configName",default='',
      help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_argument(\
      "-f", "--configfile",dest="configFile",default='',
      help="specify configuration file, default is systel.cfg" )
   parser.add_argument(\
      "-r", "--rootdir",dest="rootDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_argument(\
      "-s", "--sortiefile",action="store_true",dest="sortieFile",default=False,
      help="specify whether there is a sortie file, default is no" )
   options = parser.parse_args()

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
   options.bypass = False

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
# ~~~~ Works for one configuration only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
   if len(options.args) < 1:
      print '\nThe name of the CAS file is required\n'
      parser.print_help()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cas = options.args[0]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)
   cfgname = cfgs.iterkeys().next()

   xcpts = MESSAGES()

   # still in lower case
   if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
   if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
   # parsing for proper naming
   cfg = parseConfig_RunningTELEMAC(cfgs[cfgname])

   try:
      createMascaretFiles(cfg,cas)
   except Exception as e:
      xcpts.addMessages(filterMessage({'name':'_____________\nruncode::main:\n'},
                        e,options.bypass))


   mascaretExe = cfg['root'] + sep + 'builds'+ sep + cfgname + sep + 'bin' + \
                 sep + 'mascaret' + cfgs[cfgname]['sfx_exe'] + ' FichierCas.txt'
   try:
      tail,code = xcpts.runCmd(mascaretExe,options.bypass)
   except Exception as e:
      xcpts.addMessages(filterMessage({'name':'processExecutable',
                                      'msg':'something went wrong for no reason. \
                                             Please verify your compiler installation.'
                                     },e,options.bypass))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty() or code != 0:
      print '\n\nHummm ... I could not complete my work.\n'+'~'*72\
      + xcpts.exceptMessages()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print '\n\nMy work is done\n\n'
      sys.exit(0)

if __name__ == "__main__":
    main()
