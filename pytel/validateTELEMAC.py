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
"""
"""@history 07/03/2013 -- Juliette Parisi
         New options created : --runcase, --postprocess, --criteria to run
         respectively the simulation, the post process steps and the validation
         criteria. A validation summary is produced for each step.
		 Before running one the step, the previous step Validation Summary is read
		 in order to run only successful cases in the next step.
"""
"""@brief
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import time
from datetime import date
from os import path,walk,environ
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
# ~~> dependencies towards other pytel/modules
from parsers.parserXML import runXML
from parsers.parserCSV import putDataCSV,getValidationSummary
from utils.messages import MESSAGES,filterMessage

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
   parser.add_option("-a", "--action",type="string",dest="do",default='',
      help="filter specific process actions from the XML file (none can be used)" )
   parser.add_option("-d", "--draw",type="string",dest="draw",default='',
      help="filter specific drawing actions from the XML file (none can be used)" )
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
   parser.add_option("-k","--rank",type="string",dest="rank",default='',
      help="the suite of validation ranks (all by default)" )
   parser.add_option("--revision",type="string",dest="revision",default='',
      help="will use the SVN revision number in the Validation Summary Name (useful for Jenkins)" )   
   parser.add_option("--runcase",action="store_true",dest="runcase",default='',
      help="will only do the actions from the xml file" )
   parser.add_option("-p","--postprocess",action="store_true",dest="postprocess",default='',
      help="will only do the extracts and plots from the xml file" )
   parser.add_option("--criteria",action="store_true",dest="criteria",default='',
      help="will only do the criteria from the xml file" )
	  
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
# ~~~ Manage options to run only cases or only Post processing ~~~~~
# (Running only the validation criteria has not been implemented yet )

   if options.runcase == '' and options.postprocess == ''and options.criteria == '': 
      options.runcase = True
      options.postprocess = True
      options.criteria = True
   elif options.runcase == True : 
      options.postprocess = False 
      options.criteria = False
   elif options.postprocess == True :
      options.runcase = True 
      options.criteria = False
   elif options.criteria == True :
      options.runcase = False
      options.postprocess = False
	  
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Forces not to use any Xwindows backend for Jenkins ~~~~~~~~~~
   if not options.display:
      import matplotlib.pyplot as plt
      plt.switch_backend('agg')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()

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
            runXML(path.realpath(xmlFile),xmls[xmlFile],options.bypass,options.runcase,options.postprocess,options.criteria)
         except Exception as e:
            xcpts.addMessages([filterMessage({'name':'runXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)])

   else:
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Turning XML / config loops inside out ~~~~~~~~~~~~~~~~~~~~~~~
      print '\n\nScanning XML files and configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      xmls = {}
# ~~~~ Variables needed to generate the Validattion report ~~~~~~~~~
      cas=[]
      casrun=[]
      cas.append('TestCase')
      module=[]
      module.append('Module')
      status=[]
      status.append('Duration(s)')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
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
         if options.rank != '': cfgs[cfgname]['val_rank'] = options.rank
         cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
         cfg.update({ 'PWD':PWD })

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
                     xmls[codeName][key][xmlPath].update({cfgname: { 'cfg':cfg, 'options':options } })
      
      # Reading last step Validation Summary in order to run only successful 
      # cases in the next step
      
      d = date.today()
 
      if options.revision != '': 
         if options.postprocess == True and options.criteria == False :
            LastSummary = path.join(root,'Revision#'+ options.revision + '_' + 'ValidationSummaryRun.csv')
         elif options.postprocess == False and options.criteria == True :
            LastSummary = path.join(root,'Revision#'+ options.revision + '_' + 'ValidationSummaryPostProcess.csv')
         else : LastSummary = ''
      else :
         if options.postprocess == True and options.criteria == False :
            LastSummary = path.join(root,d.isoformat() + '_' + 'ValidationSummaryRun.csv')
         elif options.postprocess == False and options.criteria == True :
            LastSummary = path.join(root,d.isoformat() + '_' + 'ValidationSummaryPostProcess.csv')
         else : LastSummary = ''
      
      if LastSummary != '':
         Summary = getValidationSummary(LastSummary)
         Lastcas = Summary['TestCase']
         Laststatus = Summary['Status']        
      
      # ~~> Print summary of test cases which will be run
      i=0
      for codeName in xmls.keys():
         print '    +> ',codeName
         for key in xmls[codeName]:
            if LastSummary != '':
            # If one test case failed or has not been run in the last step,
            # that test case won't be run either in the next step
               cas.append(key)
               module.append(codeName)
               if Laststatus[i]== 'failed' or Laststatus[i]== 'NotRun' : continue
               else :
                  print '    |    +> ',key
                  casrun.append(key)
                  for xmlFile in xmls[codeName][key]:
                     print '    |    |    +> ',path.basename(xmlFile),xmls[codeName][key][xmlFile].keys()
               i += 1      
            else : 
               print '    |    +> ',key
               cas.append(key)
               casrun.append(key)
               module.append(codeName)
               for xmlFile in xmls[codeName][key]:
                  print '    |    |    +> ',path.basename(xmlFile),xmls[codeName][key][xmlFile].keys()
               
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print "%s" % (time.ctime(time.time()))
      i=0
      cpt = 0
      for codeName in xmls.keys():
         for key in xmls[codeName]:
            if LastSummary != '':
               if Laststatus[i]== 'failed' or Laststatus[i]== 'NotRun' :
                  status.append('NotRun')
                  continue
               else :
                  cpt += 1
                  print '\n\nValidation of ' + key + ' of module ' + codeName + ' '+ str(cpt) + '/' + str(len(casrun)) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
                  for xmlFile in xmls[codeName][key]:        

                     try:
                        tic = time.time()
                        runXML(xmlFile,xmls[codeName][key][xmlFile],options.bypass,options.runcase,options.postprocess,options.criteria)
                        toc = time.time()
                        ttime = toc-tic
                        status.append(ttime)
                     except Exception as e: 
                        status.append('failed')
                        xcpts.addMessages([filterMessage({'name':'_____________\nrunXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)])
               i += 1
            else :
               cpt += 1
               print '\n\nValidation of ' + key + ' of module ' + codeName + ' '+ str(cpt) + '/' + str(len(casrun)) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
               for xmlFile in xmls[codeName][key]:        
                     try:
                        tic = time.time()
                        runXML(xmlFile,xmls[codeName][key][xmlFile],options.bypass,options.runcase,options.postprocess,options.criteria)
                        toc = time.time()
                        ttime = toc-tic
                        status.append(ttime)
                     except Exception as e: 
                        status.append('failed')
                        xcpts.addMessages([filterMessage({'name':'_____________\nrunXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)])
                                
      print "%s" % (time.ctime(time.time()))
      
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         
# ~~~~ Writes the Validation Report in a CSV file ~~~~~~~~~~~~~~~~~~~~
      print '\n\nWritting Validation Report.\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

      columns = []
      if options.revision != '': 
         if options.postprocess == False and options.criteria == False : 
            SummaryFile  = path.join(root,'Revision#'+ options.revision + '_' + 'ValidationSummaryRun.csv')
         elif options.postprocess == True and options.criteria == False : 
            SummaryFile = path.join(root,'Revision#'+ options.revision + '_' + 'ValidationSummaryPostProcess.csv')
         elif options.criteria == True and options.postprocess == False :
            SummaryFile = path.join(root,'Revision#'+ options.revision + '_' + 'ValidationSummaryCriteria.csv')
         elif options.criteria == True and options.postprocess == True and options.runcase == True: 
            SummaryFile = path.join(root,'Revision#'+ options.revision + '_' + 'ValidationSummary.csv')
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
      
      try : putDataCSV(SummaryFile,columns)
      except : xcpts.addMessages(['I could not write properly the Validation Summary, something went wrong'])


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
