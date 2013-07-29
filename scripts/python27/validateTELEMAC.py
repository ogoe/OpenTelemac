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
from utils.files import moveFile2File,putFileContent

# _____                         ____________________________________
# ____/ Primary Class: REPORTs /___________________________________/
# TODO: Differentiate reports for action
#      - needs implementation in runXML
#      - need a report.tic() and report.toc()

class REPORT:

   # Constant definition
   comment = '#'
   delimiter = ','

   def __init__(self):
      self.reports = {}
      self.fileFields = [ '[version]', 'Validation-Summary', time.strftime("%Y-%m-%d-%Hh%Mmin%Ss", time.localtime(time.time())) ]
      self.fileHeader = []

   def add(self,root,repname,module,version):
      # ~~> Current file name
      self.fileFields[0] = version
      self.fileFields[1] = repname
      if not self.reports.has_key(repname): self.reports.update({ repname:{} })
      self.fileName = path.join( root, '_'.join(self.fileFields)+'.csv' )
      # ~~> Possible existing file name
      for dirpath,dirnames,filenames in walk(root) : break
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
      if not self.reports[repname].has_key(module): self.reports[repname].update({ module:{}, 'file':self.fileName })
   
   def update(self,repname,module,caseName,caseValue):
      if self.reports.has_key(repname):
         if self.reports[repname].has_key(module):
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
      # ~~> Opening
      File = open(fileName,'r')
      # ~~> Parsing
      for line in File:
         if line[0] == self.comment: continue
         row = line.strip().split(self.delimiter)
         if not report.has_key(row[0]): report.update({ row[0]:{}, 'file':fileName })
         report[row[0]].update({row[1]:row[2]})
      # ~~> closure
      File.close()
      return report

   def dump(self):
      contents = {}
      for n in self.reports.keys():
         if self.reports[n]['file'] not in contents.keys():
            content = deepcopy(self.fileHeader)
            content.append('Module'+self.delimiter+'Case'+self.delimiter+'Duration')
            contents.update({ self.reports[n]['file']:content })
      for n in self.reports.keys():
         for m in self.reports[n].keys():
            if m == 'file': continue
            for z in self.reports[n][m].keys():
               if z == 'Case': continue
               contents[self.reports[n]['file']].append( m+self.delimiter+z+self.delimiter+str(self.reports[n][m][z]) )
      for fileName in contents.keys():
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
   if environ.has_key('USETELCFG'): USETELCFG = environ['USETELCFG']
   PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   SYSTELCFG = path.join(PWD,'configs')
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
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
   parser.add_option("--hosts",type="string",dest="hosts",default='',
      help="specify the list of hosts available for parallel mode, ';' delimited" )
   parser.add_option("--ncsize",type="string",dest="ncsize",default='',
      help="the number of processors forced in parallel mode" )
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
   parser.add_option("--report",action="store_true",dest="report",default=False,
      help="will create a report summary if option is present" )
	  
   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         for dirpath,dirnames,filenames in walk(dircfg) : break
         for file in filenames :
            head,tail = path.splitext(file)
            if tail == '.cfg' : print '    +> ',file
      sys.exit()

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
         sys.exit()
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
   if options.report: report = REPORT()

   if args != []:
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Turning XML / config loops inside out ~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nScanning XML files and configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      xmls = {}
      for cfgname in cfgs.keys():
         # still in lower case
         if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
         if options.version != '': cfgs[cfgname]['version'] = options.version
         if options.modules != '': cfgs[cfgname]['modules'] = options.modules
         cfgs[cfgname]['display'] = options.display
         # parsing for proper naming
         cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
         cfg.update({ 'PWD':PWD })

         for xmlFile in args:
            if not path.isfile(xmlFile):
               print '\nNot able to find your XML file: ' + xmlFile
               print ' ... maybe something wrong with your command line'
               sys.exit()
            if not xmls.has_key(xmlFile): xmls.update({xmlFile:{}})
            xmls[xmlFile].update({cfgname:{'cfg':cfg,'options':options}})

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for xmlFile in xmls.keys():
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
      for cfgname in cfgs.keys():
         # still in lower case
         if options.rootDir != '': 
            cfgs[cfgname]['root'] = path.abspath(options.rootDir)
            root = path.abspath(options.rootDir)
         else : root = cfgs[cfgname]['root']
         if options.version != '': cfgs[cfgname]['version'] = options.version
         if options.modules != '': cfgs[cfgname]['modules'] = options.modules
         cfgs[cfgname]['display'] = options.display
         # parsing for proper naming
         cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
         cfg.update({ 'PWD':PWD })
         # gathering XMLs
         for codeName in cfg['VALIDATION'].keys():
            if options.report: report.add(root,'Validation-Summary',codeName,cfgs[cfgname]['version'])
            xmlKeys = cfg['VALIDATION'][codeName]
            if not xmls.has_key(codeName): xmls.update({codeName:{}})
            for key in xmlKeys.keys():
               if key != 'path':
                  if not xmls[codeName].has_key(key): xmls[codeName].update({key:{}})
                  xmlDir = path.join(xmlKeys['path'],key)
                  for xmlFile in xmlKeys[key]:
                     xmlPath = path.join(xmlDir,xmlFile)
                     if not xmls[codeName][key].has_key(xmlPath): xmls[codeName][key].update({xmlPath:{}})
                     nxmls += 1
                     xmls[codeName][key][xmlPath].update({cfgname: { 'cfg':cfg, 'options':options } })
      nxmls = nxmls / len(cfgs.keys())

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nLooping through all XML files ... on %s\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n' % (time.ctime(time.time()))
      ixmls = 0
      for codeName in xmls.keys():
         for key in xmls[codeName]:
            for xmlFile in xmls[codeName][key]:
               ixmls += 1
               print '\n\nValidation < '  + str(ixmls) + '/' + str(nxmls) + ' > of ' + key + ' of module ' + codeName
               print '     XML file: ' + xmlFile + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
               try:
                  tic = time.time()
                  runXML(xmlFile,xmls[codeName][key][xmlFile],options.bypass)
                  toc = time.time()
                  if options.report: report.update('Validation-Summary',codeName,key,toc-tic)
               except Exception as e:
                  mes = filterMessage({'name':'_____________\nrunXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)
                  xcpts.addMessages([mes])
                  if options.report: report.update('Validation-Summary',codeName,key,'failed') #\n'+reprMessage([mes]))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         
# ~~~~ Writes the Validation Report in a CSV file ~~~~~~~~~~~~~~~~~~~~
      if options.report:
         print '\n\nWritting Validation Summary Report ... on    %s\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n' % (time.ctime(time.time()))
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
   sys.exit()
