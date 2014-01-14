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
         New options created : --runcase, --postprocess, --criteria to run
         respectively the simulation, the post process steps and the validation
         criteria. A validation summary is produced for each step.
		 Before running one the step, the previous step Validation Summary is read
		 in order to run only successful cases in the next step.
"""
"""@history 28/04/2013 -- Juliette Parisi and Sebastien Bourban
   Re-work of the options --runcase, --postprocess, --criteria in
      order to combine them with previous --action, --draw options.
   This should make the whole process more generic.
   Wiki documentation remains to be done.
"""
"""@brief
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import time
from os import path,walk,environ
from copy import deepcopy
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
# ~~> dependencies towards other pytel/modules
from parsers.parserXML import runXML
from utils.messages import MESSAGES,filterMessage
from utils.files import checkSymLink,moveFile2File,putFileContent

# _____                         ____________________________________
# ____/ Primary Class: REPORTs /___________________________________/
# TODO: Differentiate reports for action
#      - needs implementation in runXML
#      - need a report.tic() and report.toc()

class REPORT:

   # Constant definition
   comment = '#'
   delimiter = ','

   def __init__(self,repname):
      self.reports = {}
      self.fileFields = [ '[version]', repname, time.strftime("%Y-%m-%d-%Hh%Mmin%Ss", time.localtime(time.time())) ]
      self.fileHeader = []

   def add(self,root,repname,module,version):
      if repname == '': return
      # ~~> Current file name
      self.fileFields[0] = version
      self.fileFields[1] = repname
      if not repname in self.reports: self.reports.update({ repname:{} })
      self.fileName = path.join( root, '_'.join(self.fileFields)+'.csv' )
      # ~~> Possible existing file name
      dirpath, _, filenames = walk(root).next()
      for filename in filenames:
         n,e = path.splitext(filename)
         n = n.split('_')
         if e =='.csv' and n[0] == version and repname in n[1]:
            if path.join(dirpath,filename) != self.fileName:
               print '      +> Moving existing: ' + n[2] + ' to ' + path.basename(self.fileName)
               moveFile2File(path.join(dirpath,filename),self.fileName)
               self.loadHead(self.fileName)
               self.reports[repname].update(self.loadCore(self.fileName))
      # ~~> Storage facility
      if not module in self.reports[repname]: self.reports[repname].update({ module:{}, 'file':self.fileName })
   
   def update(self,repname,module,caseName,caseValue):
      if repname == '': return
      if repname in self.reports:
         if module in self.reports[repname]:
            self.reports[repname][module].update({ caseName:caseValue })

   def loadHead(self,fileName):
      self.fileHeader = []
      File = open(fileName,'r')
      for line in File:
         if line[0] == self.comment: self.fileHeader.append(line.strip())
         else: break
      File.close()

   def loadCore(self,fileName):
      report = {}
      headrow = ''
      # ~~> Opening
      File = open(fileName,'r')
      # ~~> Parsing head row
      for line in File:
         if line[0] == self.comment: continue
         if headrow == '': headrow = line.strip().split(self.delimiter)
         else:
            corerow = line.strip().split(self.delimiter)
            if corerow[0] not in report: report.update({ corerow[0]:{}, 'file':fileName })
            report[corerow[0]].update({ corerow[1]:{} })
            for i,k in zip(range(len(headrow)),corerow[2:]):
               if k != '': report[corerow[0]][corerow[1]].update({ headrow[i+2]:k })
      # ~~> closure
      File.close()
      return report

   def dump(self):   # /!\ TODO:(JCP) Update with CSV call functionalities (?)
      if self.reports == {}: return
      contents = {}
      headrows = ['Module','Case']
      # ~~> Copy the default header
      for n in self.reports:
         if self.reports[n]['file'] not in contents:
            content = deepcopy(self.fileHeader)
            contents.update({ self.reports[n]['file']:content })
      # ~~> Identify all entry names / keys for the header row
      for n in self.reports:                        # repname (could be "Validation-Summary")
         for m in self.reports[n]:                  # codename (could be stbtel)
            if m == 'file': continue
            for z in self.reports[n][m]:            # case name (could adcirc)
               if z == 'Case': continue
               for k in self.reports[n][m][z]:      # reported keys (one of which is Duration)
                  if k not in headrows: headrows.append(k)
         contents[self.reports[n]['file']].append(self.delimiter.join(headrows))
      # ~~> Line template for those absenties
      emptyline = []
      for k in headrows: emptyline.append('')
      # ~~> Copy the core passed/failed vallues
      for n in self.reports:
         for m in self.reports[n]:
            if m == 'file': continue
            for z in self.reports[n][m]:
               if z == 'Case': continue
               for k in self.reports[n][m][z]:
                  line = deepcopy(emptyline)
                  line[0] = m; line[1] = z
                  line[headrows.index(k)] = str(self.reports[n][m][z][k])
               contents[self.reports[n]['file']].append( self.delimiter.join(line) )
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
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   USETELCFG = ''
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   SYSTELCFG = path.join(PWD,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configname",type="string",dest="configName",default=USETELCFG,
      help="specify configuration name, default is the first found in the configuration file" )
   parser.add_option("-f", "--configfile",type="string",dest="configFile",default=SYSTELCFG,
      help="specify configuration file, default is systel.cfg" )
   parser.add_option("-r", "--rootdir",type="string",dest="rootDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_option("-v", "--version",type="string",dest="version",default='',
      help="specify the version number, default is taken from config file" )
   parser.add_option("-m", "--modules",type="string",dest="modules",default='',
      help="specify the list modules, default is taken from config file" )
   parser.add_option("-s", "--screen",action="store_true",dest="display",default=False,
      help="specify whether to display on screen or save silently" )
   parser.add_option("-w", "--workdirectory",type="string",dest="wDir",default='',
      help="specify whether to re-run within a defined subdirectory" )
   parser.add_option("--jobname",type="string",dest="jobname",default='job_unamed',
      help="specify a jobname for HPC queue tracking" )
   parser.add_option("--queue",type="string",dest="hpc_queue",default='',
      help="specify a queue for HPC queue tracking" )
   parser.add_option("--walltime",type="string",dest="walltime",default='01:00:00',
      help="specify a walltime for HPC queue tracking" )
   parser.add_option("--email",type="string",dest="email",default='s.bourban@hrwallingford.com',
      help="specify an e-mail adress to warn when HPC job is finished" )
   parser.add_option("--hosts",type="string",dest="hosts",default='',
      help="specify the list of hosts available for parallel mode, ';' delimited" )
   parser.add_option("--ncsize",type="string",dest="ncsize",default='',
      help="the number of processors forced in parallel mode" )
   parser.add_option("--nctile",type="string",dest="nctile",default='',
      help="the number of core per node. ncsize/nctile is the number of compute nodes" )
   parser.add_option("--ncnode",type="string",dest="ncnode",default='',
      help="the number of of nodes. ncsize = ncnode*nctile is the total number of compute nodes" )
   parser.add_option("--mpi",action="store_true",dest="mpi",default=False,
      help="make sure the mpi command is executed, ignoring any hpc command" )
   parser.add_option("--split",action="store_true",dest="split",default=False,
      help="will only do the trace (and the split in parallel) if option there" )
   parser.add_option("--merge",action="store_true",dest="merge",default=False,
      help="will only do the output copying (and recollection in parallel) if option there" )
   parser.add_option("--run",action="store_true",dest="run",default=False,
      help="will only run the simulation if option there" )
   parser.add_option("-b","--bypass",action="store_true",dest="bypass",default=False,
      help="will bypass execution failures and try to carry on (final report at the end)" )
# Combine with all filters above, "rank" now controls everything and Jenkins can control "rank"
   parser.add_option("-k","--rank",type="string",dest="rank",default='',
      help="5 integers joined by point (--rank 0.0.0.0 is all by default)" )
# These filters reset "rank" to do just one or more things
   parser.add_option("--act",type="string",dest="action",default='',
      help="filter specific actions from the XML file, and will only do these by default" )
   parser.add_option("--draw",type="string",dest="drawing",default='',
      help="filter specific drawings from the XML file, and will only do these by default" )
   parser.add_option("--get",type="string",dest="extraction",default='',
      help="filter specific data extractions from the XML file and will only do these by default" )
   parser.add_option("--test",type="string",dest="criteria",default='',
      help="filter specific drawing actions from the XML file, and will only do these" )
   parser.add_option("--report",type="string",dest="report",default='',
      help="will create a report summary of with your chosen middle name" )
   parser.add_option("--clean",action="store_true",dest="cleanup",default=False,
      help="will erase all object, executable, result files from subfolders on the selected configs/modules" )
   # ~~> Other
   parser.add_option("--use-link",action="store_true",dest="use_link",default=False,
      help="Will use link instead of copy in the temporary folder (Unix system only)" )
	  
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
   # Checking if symlink is available
   if (options.use_link and not checkSymLink(options.use_link)):
      print '\nThe symlink option is only available on Linux systems. Remove the option and try again'
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~ Manage options to run only ranked actions ~~~~~~~~~~~~~~~~~~~~
#
# if no option are set, then all will be available
#   as a results: todos = {'test': {'todo': '', 'rank': 0}, 'draw': {'todo': '', 'rank': 0}, 'get': {'todo': '', 'rank': 0}, 'act': {'todo': '', 'rank': 0}}
#   where if rank=0, it can be divided by all prime numbers
#
# if one option is set, for instance --act prnci, then all not set will be turned off
#   as a result: todos = {'test': {'todo': '', 'rank': -1}, 'draw': {'todo': '', 'rank': -1}, 'get': {'todo': '', 'rank': -1}, 'act': {'todo': 'princi', 'rank': 0}}
#   where 1 or -1 is used to turn off an option as it cannot be divided by any prime number
#
# if --rank is used, then all are sreset accordingly, for instance --rank 2.0.1.1
#   as a result: todos = {'test': {'todo': 'none', 'rank': 1}, 'draw': {'todo': '', 'rank': 0}, 'get': {'todo': 'none', 'rank': 1}, 'act': {'todo': '', 'rank': 2}}
#   where the numbers are associated to act,draw,get and test respectively
#
   if options.action + options.drawing + options.extraction + options.criteria == '' :
      # rank = 0 can be divided by all prime numbers
      options.todos = { 'act':{'rank':0,'todo':options.action},
         'draw':{'rank':0,'todo':options.drawing},
         'get':{'rank':0,'todo':options.extraction},
         'test':{'rank':0,'todo':options.criteria} }
   # else, only those options will be avaiable at their specific rank
   else:
      # rank =1 or -1 cannot be divided by any prime number
      options.todos = { 'act':{'rank':-1,'todo':''},
         'draw':{'rank':-1,'todo':''},
         'get':{'rank':-1,'todo':''},
         'test':{'rank':-1,'todo':''} }
      if options.action != '': options.todos['act'] = {'rank':0,'todo':options.action}
      if options.drawing != '': options.todos['draw'] = {'rank':0,'todo':options.drawing}
      if options.extraction != '': options.todos['get'] = {'rank':0,'todo':options.extraction}
      if options.criteria != '': options.todos['test'] = {'rank':0,'todo':options.criteria}
   # however, if rank is present, then it takes over
   if options.rank != '':
      rank = options.rank.split('.')
      if len(rank) == 1: rank = [ rank[0],rank[0],rank[0],rank[0] ]
      if len(rank) != 4:
         print '\nNot able to decode you rank: ' + options.rank
         print ' ... it should be something like 2.3.1.0, where the numbers are associated to act,draw,get and test respectively,'
         print '     or it could be just one integer, in which case rank will associate the integer to act,draw,get and test.'
         sys.exit(1)
      options.todos['act']['rank'] = int(rank[0])
      if abs(options.todos['act']['rank']) == 1:  options.todos['act']['todo'] = 'none'
      options.todos['draw']['rank'] = int(rank[1])
      if abs(options.todos['draw']['rank']) == 1:  options.todos['draw']['todo'] = 'none'
      options.todos['get']['rank'] = int(rank[2])
      if abs(options.todos['get']['rank']) == 1:  options.todos['get']['todo'] = 'none'
      options.todos['test']['rank'] = int(rank[3])
      if abs(options.todos['test']['rank']) == 1:  options.todos['test']['todo'] = 'none'

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Forces not to use any Xwindows backend for Jenkins ~~~~~~~~~~
   if not options.display:
      import matplotlib.pyplot as plt
      plt.switch_backend('agg')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()
# ~~~~ Reporting summary ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   report = REPORT(options.report)

   if args != []:
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Turning XML / config loops inside out ~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nScanning XML files and configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      xmls = {}
      for cfgname in cfgs:
         # still in lower case
         if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
         if options.version != '': cfgs[cfgname]['version'] = options.version
         if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
         cfgs[cfgname]['display'] = options.display
         # parsing for proper naming
         cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
         print '    +> '+cfgname+': ' + ', '.join(cfg['VALIDATION'].keys())
         cfg.update({ 'PWD':PWD })
         if options.cleanup: cfg['REBUILD'] = 2

         for xmlFile in args:
            if not path.isfile(xmlFile):
               print '\nNot able to find your XML file: ' + xmlFile
               print ' ... maybe something wrong with your command line'
               sys.exit(1)
            if not xmlFile in xmls: xmls.update({xmlFile:{}})
            xmls[xmlFile].update({cfgname:{'cfg':cfg,'options':options}})

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for xmlFile in xmls:
         print '\n\nFocused validation on ' + xmlFile + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         try:
            runXML(path.realpath(xmlFile),xmls[xmlFile],options.bypass)
         except Exception as e:
            xcpts.addMessages([filterMessage({'name':'runXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)])

   else:
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Turning XML / config loops inside out ~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nScanning XML files and configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      xmls = {}; nxmls = 0
      for cfgname in cfgs:
         # still in lower case
         if options.rootDir != '': 
            cfgs[cfgname]['root'] = path.abspath(options.rootDir)
            root = path.abspath(options.rootDir)
         else : root = cfgs[cfgname]['root']
         if options.version != '': cfgs[cfgname]['version'] = options.version
         if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
         cfgs[cfgname]['display'] = options.display
         # parsing for proper naming
         cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
         print '    +> '+cfgname+': ' + ', '.join(cfg['VALIDATION'].keys())
         cfg.update({ 'PWD':PWD })
         if options.cleanup: cfg['REBUILD'] = 2
         # gathering XMLs
         for codeName in cfg['VALIDATION']:
            report.add(root,options.report,codeName,cfgs[cfgname]['version'])
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
      print '\n\nLooping through all XML files ...\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      ixmls = 0
      for codeName in xmls:
         for key in xmls[codeName]:
            for xmlFile in xmls[codeName][key]:
               ixmls += 1
               print '\n\nValidation < '  + str(ixmls) + '/' + str(nxmls) + ' > of ' + key + ' of module ' + codeName
               print '     XML file: ' + xmlFile + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
               try:
                  tic = time.time()
                  r = runXML(xmlFile,xmls[codeName][key][xmlFile],options.bypass)
                  toc = time.time()
                  r.update({'Duration (s)':toc-tic})
                  report.update(options.report,codeName,key,r)
               except Exception as e:
                  mes = filterMessage({'name':'_____________\nrunXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)
                  xcpts.addMessages([mes])
                  report.update(options.report,codeName,key,{'Failure':'x'}) #\n'+reprMessage([mes]))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         
# ~~~~ Writes the Validation Report in a CSV file ~~~~~~~~~~~~~~~~~~~~
      report.dump()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty():
      print '\n\nHummm ... I could not complete my work.\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n' \
      + xcpts.exceptMessages()


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else: print '\n\nMy work is done\n\n'

   
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
   sys.exit(0)
