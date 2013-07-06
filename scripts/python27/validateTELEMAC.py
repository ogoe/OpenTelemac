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
from numpy import dtype,loadtxt
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
# ~~> dependencies towards other pytel/modules
from parsers.parserXML import runXML
from utils.messages import MESSAGES,filterMessage
from utils.files import moveFile2File,putFileContent

# _____                         ____________________________________
# ____/ Primary Class: REPORTs /___________________________________/
#
class REPORT:

   # Version number
   version = ''
   # Name of the report: [ root, version, '_ValidationSummary_', action/reportName, date, .csv ]
   fileFields = [ '[version]', 'ValidationSummary', '[name]' , '[date]' ]
   # Collumns of the report
   dtReport = dtype([('Case','S30'),('Module','S15'),('Status','S30'),('Duration','S30')])
   # Constant definition
   comment = '#'
   delimiter = ','
   # File content
   headers = []
   columns = []
   
   def __init__(self, root, version, reportName ):
      # ~~> Current file name
      self.fileFields[0] = version
      self.fileFields[2] = reportName
      self.fileFields[3] = time.strftime("%Y-%m-%d-%Hh%Mmin%Ss", time.localtime(time.time()))
      self.fileName = path.join( root, '_'.join(self.fileFields)+'.csv' )
      # ~~> Possible existing file name
      for dirpath,dirnames,filenames in walk(root) : break
      for filename in filenames:
         n,e = path.splitext(filename)
         if e =='.csv' and n.split('_')[0] == version and n.split('_')[2] == reportName:
            print '      +> Moving existing: ' + n.split('_')[3] + ' to ' + self.fileName
            moveFile2File(filename,self.fileName)
      # ~~> Possible existing file name
      if path.exists(self.fileName):
         print ' loading '
         #self.headers = getHeaders(self.fileName)
         #self.columns = loadtxt(self.fileName, dtype=self.dtReport, comments=self.comment, skiprows=len(self.headers), delimiter=self.delimiter)
      else:
         print '      +> Creating: ' + path.basename(self.fileName)
         self.putContent(['try'])

   # Headers are the first few lines (if any) that start with '#'
   def getHeaders(self,fileName):
      File = open(fileName,'r')
      head = []
      for line in file:
         if line[0] == self.comment: head.append(line)
         else: break
      file.close()
      return head

   def putHeaders(self):
      putFileContent(self.fileName,dtReport)

   def putContent(self,content):
      putFileContent(self.fileName,content)

class actionREPORT(REPORT):
   def __init__(self, root, version): REPORT.__init__(self, root,version, 'Action')
   
class extractREPORT(REPORT):
   def __init__(self, root, version): REPORT.__init__(self, root,version, 'Extract')

class drawREPORT(REPORT):
   def __init__(self, root, version): REPORT.__init__(self, root,version, 'Plot')

class checkREPORT(REPORT):
   def __init__(self, root, version): REPORT.__init__(self, root,version, 'Check')


class REPORTS:
   
   def __init__(self):
      self.reports = {}

   def add(self,root,version):
      if not self.reports.has_key(root): self.reports.update({root:{}})
      self.reports[root].update({'act':actionREPORT(root,version)})
      self.reports[root].update({'get':extractREPORT(root,version)})
      self.reports[root].update({'draw':drawREPORT(root,version)})
      self.reports[root].update({'test':checkREPORT(root,version)})
   
   def filterRanks(self):
      return

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
   PWD = path.dirname(sys.argv[0])
   SYSTELCFG = path.join(path.dirname(PWD),'config')
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
   # if no option are set, then all will be available
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
   if options.report: report = REPORTS()

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
         # Reporting per configuration (/root)
         if options.report:
            report.add(root,cfgs[cfgname]['version'])
            print report.reports
         sys.exit()
         # gathering XMLs
         for codeName in cfg['VALIDATION'].keys():
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

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nLooping through all XML files ... on %s\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n' % (time.ctime(time.time()))
      ixmls = 0
      for codeName in xmls.keys():
         for key in xmls[codeName]:
            for xmlFile in xmls[codeName][key]:
               ixmls += 1
               print '\n\nValidation of ' + key + ' of module ' + codeName
               print '     XML file: < '  + str(ixmls) + '/' + str(nxmls) + ' > ' + xmlFile + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
               #try:
               tic = time.time()
               runXML(xmlFile,xmls[codeName][key][xmlFile],options.bypass)
               toc = time.time()
               ttime = toc-tic
               if options.report: report.update()
               #except Exception as e:
               #   status.append('failed')
               #   xcpts.addMessages([filterMessage({'name':'_____________\nrunXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)])

   """
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         
# ~~~~ Writes the Validation Report in a CSV file ~~~~~~~~~~~~~~~~~~~~
      print '\n\nWritting Validation Report ... on    %s\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n' % (time.ctime(time.time()))

      columns = []
      if options.version != '':
         if options.postprocess == False and options.criteria == False : 
            SummaryFile  = path.join(root,'Revision#'+ options.version + '_' + 'ValidationSummaryRun.csv')
         elif options.postprocess == True and options.criteria == False : 
            SummaryFile = path.join(root,'Revision#'+ options.version + '_' + 'ValidationSummaryPostProcess.csv')
         elif options.criteria == True and options.postprocess == False :
            SummaryFile = path.join(root,'Revision#'+ options.version + '_' + 'ValidationSummaryCriteria.csv')
         elif options.criteria == True and options.postprocess == True and options.runcase == True: 
            SummaryFile = path.join(root,'Revision#'+ options.version + '_' + 'ValidationSummary.csv')
      else : 
         if options.postprocess == False and options.criteria == False :
            SummaryFile  = path.join(root,d.isoformat() + '_' + 'ValidationSummaryRun.csv')
         elif options.postprocess == True and options.criteria == False : 
            SummaryFile = path.join(root,d.isoformat() + '_' + 'ValidationSummaryPostProcess.csv')
         elif options.criteria == True and options.postprocess == False: 
            SummaryFile = path.join(root,d.isoformat() + '_' + 'ValidationSummaryCriteria.csv')
         elif options.criteria == True and options.postprocess == True and options.runcase == True:
            SummaryFile = path.join(root,d.isoformat() + '_' + 'ValidationSummary.csv')
            
      columns = [cas,module,status]
      
      try : pass #putDataCSV(SummaryFile,columns)
      except : xcpts.addMessages(['I could not write properly the Validation Summary, something went wrong'])
   """

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
