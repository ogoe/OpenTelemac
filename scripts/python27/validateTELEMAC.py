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
"""@history 17/04/2011 -- Sebastien E. Bourban
         Updated to the latest runcode, which includes POSTEL and COUPLAGE
"""
"""@history 28/04/2011 -- Sebastien E. Bourban
         Now supports SYSTELCFG as a directory (old Perl version, to which
         systel.cfg is added) or as a file.
"""
"""@history 30/04/2011 -- Sebastien E. Bourban
         Upgrade made to config parsing to include the option to reset
         the version and the root from the command line option:
         -v <version>, reset the version read in the config file with this
         -r <root>, reset the root path read in the config file with this
"""
"""@history 05/07/2011 -- Sebastien Bourban
         Python interpreter added for linux calls. This is a temporary
         solution as "/usr/bin/env" is not strickly portable cross
         operating systems
"""
"""@history 21/08/2011 -- David Roscoe and Sebastien Bourban
         Addition of a program of validation in the form of a local XML
         file. This XML file sets the validation instructions for every
         test cases.
"""
"""@history 27/01/2012 -- Sebastien E. Bourban
         A new option (--modules) added to the command line, which if present
         will reset the value of the key in the configuration file.
         This development was triggered by Christophe Coulet (Artelia-Sogreah)
         who asked about it on the open TELEMAC forum.
"""
"""@history 19/03/2012 -- Sebastien E. Bourban
         A new option (--screen) added to the command line, in order for
         Jenkins to use any Xwindows backend on its virtual boxes.
"""
"""@history 18/06/2012 -- Sebastien E. Bourban & Fabien Decung
         Calls to sys.exit() and os.system() have been progressively captured
         into a try/except statement to better manage errors.
         This, however, assumes that all errors are anticipated.
"""
"""@history 28/08/2012 -- Sebastien E. Bourban
         A new option (--rank) added to the command line, in order for
         Jenkins to select ranking depending on the day of the week.
"""
"""@history 04/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Simplifying call to parseConfigFile, which now takes two arguments
      options.configFile, and options.configName and return one or more
      valid configurations in an array. Testing for validity is now done
      within config.py
"""
"""@history 15/02/2013 -- Juliette Parisi
         Validation Reports written, only with the bypass option.
         The new class REPORT has been created to group the specifics.
"""
"""@history 07/03/2013 -- Juliette Parisi
         New options created : --runcase, --postprocess, --casting to run
         respectively the simulation, the post process steps and the validation
         casting. A validation summary is produced for each step.
		 Before running one the step, the previous step Validation Summary is read
		 in order to run only successful cases in the next step.
"""
"""@history 28/04/2013 -- Juliette Parisi and Sebastien Bourban
   Re-work of the options --runcase, --postprocess, --casting in
      order to combine them with previous --action, --draw options.
   This should make the whole process more generic.
   Wiki documentation remains to be done.
"""
"""@history 23/09/2014 -- Sebastien E. Bourban and Yoann Audoin
   The content of the log files from GRETEL and PARTEL are now reported
   in the error report.
"""
"""@history 01/10/2015 -- Sebastien Bourban
   Simplification of the rank and of the options --action, --draw --cast --save,
      into just the one value for the rank.
   The rank is now defined at the top of the XML validation file.
"""
"""@brief
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import re
import time
from os import path,walk,environ,remove
from argparse import ArgumentParser,RawDescriptionHelpFormatter
from copy import deepcopy
# ~~> dependencies towards the root of pytel
from config import parseConfigFile, parseConfig_ValidateTELEMAC
# ~~> dependencies towards other pytel/modules
from parsers.parserXML import runXML
from utils.messages import MESSAGES,filterMessage,reprMessage,banner
from utils.files import checkSymLink,moveFile2File,putFileContent

# _____                        _____________________________________
# ____/ Primary Class: REPORT /____________________________________/
#

"""
 The principal object in REPORT is self.reports.
 It has the following structure:
   { "repname" :
      { "xmlfile1":{},"xmlfile2":{},... }
   }
 where each "xmlfile" dict has the following keys:
   [ '', '', '' ]
 and where the various "root" are roots to a particular branch of the system.

"""
class REPORT:

   # Constant definition
   comment = '#'
   delimiter = ','
   # Arbitrary associations between report headers and the return of the XML keys
   heads = [
      'XML Name',
      'Author',
      'Contact',
      'Total Duration (s)',
      'Action Name',
      'XML Path',
      'Action Type',
      'Action Failed',
      'Action Warned',
      'Action Rank',
      'Action Updated',
      'Action Result',
      'Action Meta-Data' ]
   hkeys = [
      'file',
      'author',
      'contact',
      'duration',
      'xref',
      'path',
      'type',
      'fail',
      'warn',
      'rank',
      'updt',
      'value',
      'title' ]

   """
      Initialisation
      - repname: a user defined name, which will distinguish one report from another
   """
   def __init__(self,repname):
      self.reports = {}
      # > fileFields makes up the name of the report file, stored at the root of the system
      self.fileFields = [ '[version]', repname, time.strftime("%Y-%m-%d-%Hh%Mmin%Ss", time.localtime(time.time())) ]

   """
      Adds a new report to the list of reports.
      The reference key for that additional report is the repname
      - root: the root of a particular system branch
   """
   def add(self,repname,root,version):
      if repname == '': return {}

      # ~~> Current file name
      self.fileFields[0] = version
      self.fileFields[1] = repname
      fileName = path.join( root, '_'.join(self.fileFields)+'.csv' )
      if not repname in self.reports: self.reports.update({ repname:{ 'head':[],'core':{},'file':[] } })
      if fileName not in self.reports[repname]['file']: self.reports[repname]['file'].append(fileName)
      # ~~> Possible existing file name
      dirpath, _, files = walk(root).next()
      for file in files:
         n,e = path.splitext(file)
         if e != '.csv': continue
         n = n.split('_')
         if len(n) < 3: continue
         if n[0] == version and repname in n[1]:
            if path.join(dirpath,file) != fileName:
               print '      +> Moving existing: ' + n[2] + ' to ' + path.basename(fileName)
               try:
                  self.reports[repname].update({ 'head':self.loadHead(path.join(dirpath,file)) })
                  self.reports[repname].update({ 'core':self.loadCore(path.join(dirpath,file)) })
                  moveFile2File(path.join(dirpath,file),fileName)
               except:
                  print '      ... I could not make sense of your previous report: ' + file + ' so I deleted it '
                  remove(path.join(dirpath,file))

      return self.reports[repname]['core']

   def loadHead(self,fileName):
      r = []
      file = open(fileName,'r')
      for line in file:
         if line[0] == self.comment: r.append(line.strip())
         else: break
      file.close()
      return r

   def loadCore(self,fileName):
      r = {}
      headrow = []
      corerow = ''
      # ~~> Opening
      file = open(fileName,'r')
      # ~~> Parsing head row
      for line in file:
         if line[0] == self.comment: continue
         if headrow == []: headrow = line.split(self.delimiter)
         else:
            corerow = line.replace('"','').split(self.delimiter)
            caseName = corerow[self.hkeys.index('file')]
            if caseName not in r: r.update({ caseName:{ 'casts':[] } })           # name of the xml file
            r[caseName]['file'] = path.join(corerow[self.hkeys.index('path')],caseName)
            r[caseName]['duration'] = float(corerow[self.hkeys.index('duration')])
            cast = {}
            cast.update({ 'type': corerow[self.hkeys.index('type')] })
            if corerow[self.hkeys.index('fail')] != '': cast.update({ 'fail': ( 'true' in corerow[self.hkeys.index('fail')].lower() ) })
            if corerow[self.hkeys.index('warn')] != '': cast.update({ 'warn': ( 'true' in corerow[self.hkeys.index('warn')].lower() ) })
            if corerow[self.hkeys.index('updt')] != '': cast.update({ 'updt': ( 'true' in corerow[self.hkeys.index('updt')].lower() ) })
            cast.update({ 'value': corerow[self.hkeys.index('value')] })
            cast.update({ 'rank': corerow[self.hkeys.index('rank')] })
            cast.update({ 'title': corerow[self.hkeys.index('title')] })
            cast.update({ 'xref': corerow[self.hkeys.index('xref')] })
            r[caseName]['casts'].append( cast )
      # ~~> closure
      file.close()
      return r

   def dump(self):   # /!\ TODO:(JCP) Update with CSV call functionalities (?)
      if self.reports == {}: return
      contents = {}
      # ~~> Copy the default header in all files
      for repName in self.reports:
         for fileName in self.reports[repName]['file']:
            if fileName not in contents:
               content = deepcopy(self.reports[repName]['head'])
               content.append(self.delimiter.join(self.heads))
               contents.update({ fileName:content })
      # ~~> Line template for those absenties
      emptyline = []
      for head in self.heads: emptyline.append('')
      # ~~> Copy the core passed/failed vallues
      for repName in sorted(self.reports):                                  # repname  (could be "Validation-Summary")
         for fileName in sorted(self.reports[repName]['file']):             # filename (root dependant)
            for caseName in sorted(self.reports[repName]['core']):          # case name (could bumpflu.xml)
               for cast in self.reports[repName]['core'][caseName]['casts']:
                  line = deepcopy(emptyline)
                  for key in cast: line[self.hkeys.index(key)] = '"'+str(cast[key])+'"'
                  line[self.hkeys.index('file')] = caseName
                  line[self.hkeys.index('path')] = path.dirname(self.reports[repName]['core'][caseName]['file'])
                  line[self.hkeys.index('duration')] = str(self.reports[repName]['core'][caseName]['duration'])
                  contents[fileName].append( self.delimiter.join(line) )
      for fileName in contents:
         putFileContent(fileName,contents[fileName])

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$11-Mar-2010 13:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Used to validate the TELEMAC system against a benchmark of test cases.
      '''))
   parser.add_argument( "args",nargs='*' )
   parser.add_argument(\
      "-c", "--configname",dest="configName",default='',
      help="specify configuration name, default is the first found in the configuration file" )
   parser.add_argument(\
      "-f", "--configfile",dest="configFile",default='',
      help="specify configuration file, default is systel.cfg" )
   parser.add_argument(\
      "-r", "--rootdir",dest="rootDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_argument(\
      "-v", "--version",dest="version",default='',
      help="specify the report version number, default is an empty string" )
   parser.add_argument(\
      "-m", "--modules",dest="modules",default='',
      help="specify the list modules, default is taken from config file" )
   parser.add_argument(\
      "-s", "--screen",action="store_true",dest="display",default=False,
      help="specify whether to display on screen or save silently" )
   parser.add_argument(\
      "-w", "--workdirectory",dest="wDir",default='',
      help="specify whether to re-run within a defined subdirectory" )
   parser.add_argument(\
      "--jobname",dest="jobname",default='job_unamed',
      help="specify a jobname for HPC queue tracking" )
   parser.add_argument(\
      "--queue",dest="hpc_queue",default='',
      help="specify a queue for HPC queue tracking" )
   parser.add_argument(\
      "--walltime",dest="walltime",default='01:00:00',
      help="specify a walltime for HPC queue tracking" )
   parser.add_argument(\
      "--email",dest="email",default='s.bourban@hrwallingford.com',
      help="specify an e-mail adress to warn when HPC job is finished" )
   parser.add_argument(\
      "--hosts",dest="hosts",default='',
      help="specify the list of hosts available for parallel mode, ';' delimited" )
   parser.add_argument(\
      "--ncsize",dest="ncsize",default='',
      help="the number of processors forced in parallel mode" )
   parser.add_argument(\
      "--nctile",dest="nctile",default='',
      help="the number of core per node. ncsize/nctile is the number of compute nodes" )
   parser.add_argument(\
      "--ncnode",dest="ncnode",default='',
      help="the number of of nodes. ncsize = ncnode*nctile is the total number of compute nodes" )
   parser.add_argument(\
      "--mpi",action="store_true",dest="mpi",default=False,
      help="make sure the mpi command is executed, ignoring any hpc command" )
   parser.add_argument(\
      "--split",action="store_true",dest="split",default=False,
      help="will only do the trace (and the split in parallel) if option there" )
   parser.add_argument(\
      "--merge",action="store_true",dest="merge",default=False,
      help="will only do the output copying (and recollection in parallel) if option there" )
   parser.add_argument(\
      "--run",action="store_true",dest="run",default=False,
      help="will only run the simulation if option there" )
   parser.add_argument(\
      "-b","--bypass",action="store_true",dest="bypass",default=False,
      help="will bypass execution failures and try to carry on (final report at the end)" )
# Combine with all filters above, "rank" now controls everything and Jenkins can control "rank"
   parser.add_argument(\
      "-k","--rank",dest="rank",default='0',
      help='''1 integer only (--rank 0 is the default)
         \n
         *** Options *** \t\t\t\t\n
         2: will run the test cases of rank 2
         \n
         6: will run the test cases of rank 2 and 3 (2x3=6)
         \n
         30: will run the test cases of rank 2, 3 and 5 (2x3x5=30)
         \t\t\t\n
         210: will run the test cases of rank 2, 3, 5 and 7 (2x3x5x7=210)
         \t\t\t\n
         2310: will run the test cases of rank 2, 3, 5, 7 and 11 (2x3x5x7x11=2310)
         \t\t\n
         0: will run everything, including the test cases with issues
         \t\t\t\t\n
         *** Key *** \t\t\t\t\t\n
         Rank 2: The individual test case takes less than 10sec to run
         \t\t\t\t\n
         Rank 3: The individual test case takes between 10sec and 1min to run
         \t\t\n
         Rank 5: The individual test case takes between 1min and 10min to run
         \t\t\n
         Rank 7: The individual test case takes between 10min and 1h to run
         \t\t\t\n
         Rank 11: The individual test case takes more than 1h to run
         \t\t\t\t\n
         Rank 13: Problematic test cases''' )
   parser.add_argument(\
      "--valrootdir",dest="val_root",default='',
      help="specify the directory in which to search the validation cases, default is taken from config file" )
   parser.add_argument(\
      "--runOnly",action="store_true",dest="runOnly",default=False,
      help="Only do the action type" )
   parser.add_argument(\
      "--todos",dest="todos",default='',
      help="filter specific actions from the XML file, and will only do these by default" )
   parser.add_argument(\
      "--report",dest="report",default='',
      help="will create a report summary of with your chosen middle name" )
   parser.add_argument(\
      "--clean",action="store_true",dest="cleanup",default=False,
      help="will erase all object, executable, result files from subfolders on the selected configs/modules" )
   # ~~> Other
   parser.add_argument(\
      "--use-link",action="store_true",dest="use_link",default=False,
      help="Will use link instead of copy in the temporary folder (Unix system only)" )
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

   # /!\ please do not reset the users environemnt
   # environ['HOMETEL'] = PWD
   # user configuration name
   USETELCFG = ''
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   if options.configName != '': USETELCFG = options.configName
   # /!\ please do not reset the users environemnt
   #else: environ['USETELCFG'] = options.configName
   # user configuration file
   SYSTELCFG = path.join(PWD,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if options.configFile != '': SYSTELCFG = options.configFile
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   options.configFile = SYSTELCFG
   #PYTELPATH = path.dirname(sys.argv[0])
   #if options.rootDir != '': PYTELPATH = path.join(path.join(options.rootDir,'scripts'),'python27')
   #if 'PYTELPATH' not in environ:
   #   environ.update({'PYTELPATH':PYTELPATH}) # do you need this ?
   #   sys.path.append( PYTELPATH ) # clever you !

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ banners ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mes = MESSAGES()  # validation takes its version number from the SVN revision
   VERSION = 'report'
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
      if options.version != '': svnrev += options.version
   if svnrev != '': VERSION += '-svn'+svnrev
   if svnurl != '': VERSION += '-'+svnurl.split('/')[-1]
   if svnrev+svnurl == '':
      print '\n'.join(banner('unknown revision'))
   else:
      if svnurl != '': print '\n'.join(banner(svnurl.split('/')[-1]))
      if svnrev != '': print '\n'.join(banner('rev. #'+svnrev))

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
            if tail == '.cfg' : print '    +> ',fle
      sys.exit(1)
   # Checking if symlink is available
   if (options.use_link and not checkSymLink(options.use_link)):
      print '\nThe symlink option is only available on Linux systems. Remove the option and try again'
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,USETELCFG)
   # /!\ yo need to validate all configuration in the config file
   # If USETELCFG is not defined in the environment setting USETELCFG to the first config
   # /!\ please do not reset the users environemnt
   #if 'USETELCFG' not in environ: environ['USETELCFG'] = cfgs.iterkeys().next()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Forces not to use any Xwindows backend for Jenkins ~~~~~~~~~~
   if not options.display:
      import matplotlib.pyplot as plt
      plt.switch_backend('agg')
      from mayavi import mlab
      mlab.options.offscreen = True

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()

# ~~~~ Reporting summary ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   reports = REPORT(options.report)

   if options.args != []:
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Turning XML / config loops inside out ~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nScanning XML files and configurations\n'+'~'*72+'\n'
      xmls = {}
      for cfgname in cfgs:
         # still in lower case
         if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
         cfgs[cfgname]['pytel'] = PYT
         root = cfgs[cfgname]['root']
         if options.rootDir != '': cfgs[cfgname]['root'] = PWD
         if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
         cfgs[cfgname]['display'] = options.display
         # parsing for proper naming
         cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
         print '    +> '+cfgname+': ' + ', '.join(cfg['VALIDATION'].keys())
         #cfg.update({ 'PWD':PWD })
         if options.cleanup: cfg['REBUILD'] = 2

         for xmlFile in options.args:
            if not path.isfile(xmlFile):
               print '\nNot able to find your XML file: ' + xmlFile
               print ' ... maybe something wrong with your command line'
               sys.exit(1)
            if not xmlFile in xmls: xmls.update({xmlFile:{}})
            xmls[xmlFile].update({cfgname:{'cfg':cfg,'options':options}})

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for xmlFile in xmls:
         print '\n\nFocused validation on ' + xmlFile + '\n'+'~'*72+'\n'
         try:
            report = reports.add(options.report,root,VERSION)
         except Exception as e:
            xcpts.addMessages([filterMessage({'name':'runXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)])
            break
         r = []
         if xmlFile.replace(root,'') in report: r = report[xmlFile.replace(root,'')]['casts']
         # /!\ please do not reset the users environemnt
         #environ['USETELCFG'] = cfgname
         try:
            tic = time.time()
            r = runXML(path.realpath(xmlFile),xmls[xmlFile],r,options.bypass,options.runOnly)
            toc = time.time()
            #if r == []: r = [{ 'fail':False, 'warn':False, 'value':1, 'title':'My work is done' }]
            report.update({ xmlFile.replace(root,''):
               { 'duration':toc-tic, 'file':path.realpath(xmlFile), 'casts':r } })
         except Exception as e:
            mes = filterMessage({'name':'runXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)
            xcpts.addMessages([mes])
            report.update({ xmlFile.replace(root,''):
               { 'duration':0, 'file':path.realpath(xmlFile),
                  'casts':[{ 'xref':'*****', 'fail':True, 'warn':True, 'updt':True, 'rank':-1, 'value':0, 'title':reprMessage([mes]).replace('\n','') }]
               } })

   else:
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Turning XML / config loops inside out ~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nScanning XML files and configurations\n'+'~'*72+'\n'
      xmls = {}; nxmls = 0
      for cfgname in cfgs:
         # still in lower case
         if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
         cfgs[cfgname]['pytel'] = PYT
         root = cfgs[cfgname]['root']
         if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
         root = cfgs[cfgname]['root']
         if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
         cfgs[cfgname]['display'] = options.display
         if options.val_root != '':
            cfgs[cfgname]['val_root'] = options.val_root
         # parsing for proper naming
         cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
         print '    +> '+cfgname+': ' + ', '.join(cfg['VALIDATION'].keys())
         #cfg.update({ 'PWD':PWD })
         if options.cleanup: cfg['REBUILD'] = 2
         # gathering XMLs
         for codeName in cfg['VALIDATION']:
            xmlKeys = cfg['VALIDATION'][codeName]
            if not codeName in xmls: xmls.update({codeName:{}})
            for key in xmlKeys:
               if key != 'path':
                  if not key in xmls[codeName]: xmls[codeName].update({key:{}})
                  xmlDir = path.join(xmlKeys['path'],key)
                  for xmlFile in xmlKeys[key]:
                     xmlPath = path.join(xmlDir,xmlFile)
                     if not xmlPath in xmls[codeName][key]: xmls[codeName][key].update({xmlPath:{}})
                     nxmls += 1
                     xmls[codeName][key][xmlPath].update({cfgname: { 'cfg':cfg, 'options':options } })
      nxmls = nxmls / len(cfgs)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nLooping through all XML files ...\n'+'~'*72+'\n'
      ixmls = 0
      for codeName in xmls:
         for key in xmls[codeName]:
            for xmlFile in xmls[codeName][key]:
               ixmls += 1
               print '\n\nValidation < '  + str(ixmls) + '/' + str(nxmls) + ' > of ' + key + ' of module ' + codeName
               print '     XML file: ' + xmlFile + '\n'+'~'*72
               try:
                  report = reports.add(options.report,root,VERSION)
               except Exception as e:
                  xcpts.addMessages([filterMessage({'name':'runXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)])
                  break
               r = []
               if path.realpath(xmlFile).replace(root,'') in report: r = report[path.realpath(xmlFile).replace(root,'')]['casts']
               try:
                  tic = time.time()
                  r = runXML(xmlFile,xmls[codeName][key][xmlFile],r,options.bypass,options.runOnly)
                  toc = time.time()
                  #if r == []: r = [{ 'fail':False, 'warn':False, 'value':1, 'title':'My work is done' }]
                  report.update({ path.realpath(xmlFile).replace(root,''):
                     { 'duration':toc-tic, 'file':path.realpath(xmlFile), 'casts':r } })
               except Exception as e:
                     mes = filterMessage({'name':'_____________\nrunXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)
                     xcpts.addMessages([mes])
                     report.update({ path.realpath(xmlFile).replace(root,''):
                        { 'duration':0, 'file':path.realpath(xmlFile),
                           'casts':[{ 'xref':'*****', 'fail':True, 'warn':True, 'updt':True, 'rank':-1, 'value':0, 'title':reprMessage([mes]).replace('\n','') }]
                        } })

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Writes the Validation Report in a CSV file ~~~~~~~~~~~~~~~~~~~~
   reports.dump()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty():
      print '\n\nHummm ... I could not complete my work.\n'\
      '~'*72 + '\n' + xcpts.exceptMessages()
      sys.exit(1)


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print '\n\nMy work is done\n\n'
      sys.exit(0)
