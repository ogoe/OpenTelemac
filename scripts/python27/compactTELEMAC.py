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
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_CompactTELEMAC
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
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USETELCFG = ''
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   SYSTELCFG = path.join(PWD,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ banners ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mes = MESSAGES()  # compact takes its version number from the SVN revision
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
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
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
   parser.add_option("-a", "--archiveName",
                      type="string",
                      dest="archiveName",
                      default='',
                      help="specify the archive name, default is taken as the config name" )
   parser.add_option("-m", "--modules",
                      type="string",
                      dest="modules",
                      default='',
                      help="specify the list modules, default is taken from config file" )
   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         _, _, filenames = walk(dircfg).next()
         for fle in filenames :
            head,tail = path.splitext(fle)
            if tail == '.cfg' : print '    +> ',fle
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)

   for cfgname in cfgs:
      # still in lower case
      if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
      if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
      if not path.exists(cfgs[cfgname]['root']):
         print '\nNot able to find your root directory: ' + cfgs[cfgname]['root'] + '\n'
         sys.exit(1)
      if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
      # parsing for proper naming
      cfg = parseConfig_CompactTELEMAC(cfgs[cfgname])
      print '\n\nScanning the source code for:\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      print '    +> configuration: ' +  cfgname
      if 'brief' in cfgs[cfgname]: print '    +> '+'\n    |  '.join(cfgs[cfgname]['brief'].split('\n'))
      print '    +> root:          ' +  cfgs[cfgname]['root']
      print '    +> modules:       ' +  cfgs[cfgname]['modules'] + '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

# ~~ Scans all source files to build a relation database ~~~~~~~~~~~
      print '\n\nConfiguration ' + cfgname + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      if cfg['MODULES'] == {}:
         print '\nNot able to find any modules within your root directory ' + cfgs[cfgname]['root'] + '\n'
         sys.exit(1)
      
      pt = cfg['root']
      pc = path.join(pt,cfgname)
      if options.archiveName != '':
         if path.dirname(options.archiveName) != '':
            pc = path.join(options.archiveName,cfgname)
            archive = options.archiveName
         else:
            pc = path.join(path.join(pt,options.archiveName),cfgname)
            archive = path.join(pt,options.archiveName)

      dirs = ['builds'+sep+cfgname,'scripts','sources']
      for pid in dirs:
         pi = path.join(pt,pid)
         po = pi.replace(pt,pc)
         copytree(pi,po,ignore=ignore_patterns('.svn','*.pyc'))
         print '    +> '+pi

      pid = path.join(pt,'configs')
      if path.exists(pid):
         po = pid.replace(pt,pc)
         createDirectories(po)
         copyFile(options.configFile,po)
         print '... finally copying ' + options.configFile

      print '\n... now packaging ' + cfgname
      zip(cfgname,pc,cfg['ZIPPER'])

      print '\n... now cleaning '
      removeDirectories(pc)

   if options.archiveName != '':
      print '\n... now packaging ' + cfgname + ' into ' + archive
      zip(path.basename(archive),archive,cfg['ZIPPER']) # /!\ use the last cfg value

      print '\n... now cleaning ' + archive
      removeDirectories(archive)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
