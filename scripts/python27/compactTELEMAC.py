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
      Python interpreter added for linux calls. This is a temporary solution
      as "/usr/bin/env" is not strickly portable cross operating systems
"""
"""@history 05/07/2011 -- Sebastien E. Bourban
      Copy only the relevant CFG file
"""
"""@history 05/07/2011 -- Sebastien E. Bourban
      Addition of a new option:
      -a archive_name
      - if the -a option is not present, the archive files are named after
      each available configuration
      - if the -a option is present, and it is just a name, then the archives
      files are named after each available configuration and further packaged
      within archive_name
      - if the -a option is present, and it is a name including a path, then
      the archives files are named after each available configuration and
      further packaged within the archive_name (at the defined location)
"""
"""@history 27/01/2012 -- Sebastien E. Bourban
         A new option (--modules) added to the command line, which if present
         will reset the value of the key in the configuration file.
         This development was triggered by Christophe Coulet (Artelia-Sogreah)
         who asked about it on the open TELEMAC forum.
"""
"""@history 04/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Simplifying call to parseConfigFile, which now takes two arguments
      options.configFile, and options.configName and return one or more
      valid configurations in an array. Testing for validity is now done
      within config.py
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
from os import path,walk,environ,sep
import sys
from shutil import copytree,ignore_patterns
from argparse import ArgumentParser,RawDescriptionHelpFormatter
# ~~> dependencies towards the root of pytel
from config import parseConfigFile, parseConfig_CompactTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.files import createDirectories, removeDirectories, zip, copyFile
from utils.messages import MESSAGES,banner

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n\
Compact the TELEMAC system files, into various archived:\n
1. archiving sources if necessary
2. archiving examples if necessary
3. archiving binaries if necessary
4. ...
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
      "-a", "--archiveName",metavar="archive name",
      dest="archiveName",default='',
      help="specify the archive name, default is taken as the config name" )
   parser.add_argument(\
      "-m", "--modules",metavar="modules",
      dest="modules",default='',
      help="specify the list modules, default is taken from config file" )
   parser.add_argument(\
      "--src",action="store_true",
      dest="srcOnly",default=False,
		    help="create a zip containing only the sources i.e. the "\
         "bare minimum to use telemac-mascaret" )
   parser.add_argument(\
      "--examples",action="store_true",
      dest="examplesOnly",default=False,
		    help="create a zip containing only the sources i.e. the "\
         "bare minimum to use telemac-mascaret" )
   options = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # The path to the root relates to the script launched, which implies
   # that the user environment knows which to run
   # (this script is stored under .../scripts/python27/)
   #PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   PWD = path.dirname(path.dirname( path.dirname(__file__)) )
   # if the appropriate command line option is used, then reset rootDir
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
   mes = MESSAGES()  # compact takes its version number from the SVN revision
   version = path.basename(PWD)
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
   else:
      if svnurl != '': print '\n'.join(banner(svnurl.split('/')[-1]))
      if svnrev != '': print '\n'.join(banner('rev. #'+svnrev))
      version = svnurl.split('/')[-1]+'-'+svnrev

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
# ~~~~ Works for only one common root and zipper ~~~~~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)
   pt = ''
   if options.rootDir != '':
      pt = path.abspath(options.rootDir)
      if not path.exists(pt):
         print '\nNot able to find your root directory: ' + pt + '\n'
         sys.exit(1)
      for cfgname in cfgs: cfgs[cfgname]['root'] = pt
   zt = ''
   for cfgname in cfgs:
      if cfgs[cfgname].has_key('root'):
         if pt == '': pt = cfgs[cfgname]['root']
         elif pt != cfgs[cfgname]['root']:
            print '\nThis script ony works with one common root for all your configurations\n'
            sys.exit(1)
      else: cfgs[cfgname].update({'root':PWD})
      cfgs[cfgname]['pytel'] = PYT
      if cfgs[cfgname].has_key('sfx_zip'):
         if zt == '': zt = cfgs[cfgname]['sfx_zip']
         elif zt != cfgs[cfgname]['sfx_zip']:
            print '\nThis script ony works with one common sfx_zip for all your configurations\n'
            sys.exit(1)
   if pt == '': pt = PWD
   if zt == '':
      print '\nAt least one configuration should have a sfx_zip\n'
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ srcOnlly is independent of config ~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if options.srcOnly:
      # ~~> create the archive directory
      if options.archiveName != '': archiveName = options.archiveName
      else: archiveName = 'otm_'+version+'-src'
      print '\n\nArchive ' + archiveName + '\n'+'~'*72+'\n'
      pc = path.join(pt,archiveName)
      if path.exists(pc): removeDirectories(pc)
      createDirectories(pc)
      # ~~> copy the content of the following dirs into the archive directory
      dirs = ['optionals','scripts','sources','documentation','configs']
      for pid in dirs:
         pi = path.join(pt,pid)
         po = pi.replace(pt,pc)
         copytree(pi,po,ignore=ignore_patterns('.svn','*.pyc'))
         print '    +> '+pi
      # ~~> copy the following files into the archive directory
      files = ['NEWS.txt','README.txt']
      for pid in files:
         pi = path.join(pt,pid)
         po = pi.replace(pt,pc)
         copyFile(pi,po)
         print '    +> '+pi
      # ~~> prepare an empty diretory for future builds
      pid = path.join(pt,'builds')
      po = pid.replace(pt,pc)
      createDirectories(po)
      # ~~> zipping the archive directory
      print '\n... now packaging ' + archiveName
      zip(archiveName,pc,zt)
      # ~~> cleaning the archive directory
      print '\n... now cleaning '
      removeDirectories(pc)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ examplesOnly is independent of config ~~~~~~~~~~~~~~~~~~~~~~~
   elif options.examplesOnly:
      # ~~> create the archive directory
      if options.archiveName != '': archiveName = options.archiveName
      else: archiveName = 'otm_'+version+'-examples'
      print '\n\nArchive ' + archiveName + '\n'+'~'*72+'\n'
      pc = path.join(pt,archiveName)
      if path.exists(pc): removeDirectories(pc)
      createDirectories(pc)
      # ~~> copy the content of the following dir into the archive directory
      dirs = ['examples']
      for pid in dirs:
         pi = path.join(pt,pid)
         po = pi.replace(pt,pc)
         copytree(pi,po,ignore=ignore_patterns('.svn','*.pyc'))
         print '    +> '+pi
      # ~~> zipping the archive directory
      print '\n... now packaging ' + archiveName
      zip(archiveName,pc,zt)
      # ~~> cleaning the archive directory
      print '\n... now cleaning '
      removeDirectories(pc)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   else:
      for cfgname in cfgs:
         if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
         # parsing for proper naming
         cfg = parseConfig_CompactTELEMAC(cfgs[cfgname])
         print '\n\nScanning the following:\n'+'~'*72+'\n'
         print '    +> configuration: ' +  cfgname
         if 'brief' in cfgs[cfgname]: print '\n    +> '+'\n    |  '.join(cfgs[cfgname]['brief'].split('\n')) + '\n'
         print '    +> root:          ' +  cfgs[cfgname]['root']
         print '    +> modules:       ' +  cfgs[cfgname]['modules'] + '\n\n'+'~'*72+'\n'

   # ~~ Scans all source files to build a relation database ~~~~~~~~~~~
         if cfg['MODULES'] == {}:
            print '\nNot able to find any modules within your root directory ' + cfgs[cfgname]['root'] + '\n'
            sys.exit(1)

         # ~~> create the archive directory
         if options.archiveName != '': archiveName = options.archiveName
         else: archiveName = 'otm_'+version+'-builds-'+cfgname
         print '\n\nArchive ' + archiveName + '\n'+'~'*72+'\n'
         pc = path.join(pt,archiveName)
         if path.exists(pc): removeDirectories(pc)
         createDirectories(pc)
         # ~~> copy the content of the following dir into the archive directory
         dirs = ['builds'+sep+cfgname,'scripts','sources','configs']
         for pid in dirs:
            pi = path.join(pt,pid)
            po = pi.replace(pt,pc)
            copytree(pi,po,ignore=ignore_patterns('.svn','*.pyc'))
            print '    +> '+pi
         # ~~> zipping the archive directory
         print '\n... now packaging ' + cfgname
         zip(cfgname,pc,cfg['ZIPPER'])
         # ~~> cleaning the archive directory
         print '\n... now cleaning '
         removeDirectories(pc)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
