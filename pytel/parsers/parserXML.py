"""@author David H. Roscoe and Sebastien E. Bourban
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
"""@history 15/08/2011 -- Sebastien E. Bourban:
         Major re-work of this XML parser
"""
"""@history 16/03/2011 -- Sebastien E. Bourban:
         Development of new classes to handle ACTIONS and PLOTS
"""
"""@history 19/03/2011 -- Sebastien E. Bourban:
         Addition of a figure name for the non display option and
         handling of the backend display switch option for Jenkins's
         virtual boxes
"""
"""@history 19/05/2012 -- Fabien Decung:
         For partial compatibility issues with Python 2.6.6, replaced
         iter() by findall()
"""
"""@history 18/06/2012 -- Sebastien E. Bourban & Fabien Decung
         Calls to sys.exit() and os.system() have been progressively captured
         into a try/except statement to better manage errors.
         This, however, assumes that all errors are anticipated.
"""
"""@history 19/03/2011 -- Sebastien E. Bourban:
         Now capable of running/tranlating, etc. coupled simulations
         "links" has been added to the active aciton list.
"""
"""@history 08/03/2013 -- Juliette Parisi:
         Added the new extract Class in order to run post processing steps
		       as running executables, other python scripts, reading and writting
		       csv files ...
"""
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import time
import os
import subprocess
from os import path,walk
from optparse import Values
import sys
from socket import gethostname
from scipy import linalg as la
from itertools import izip_longest
# ~~> dependencies from within pytel/parsers
from parserKeywords import scanDICO,scanCAS,readCAS,translateCAS, getKeyWord,setKeyValue, getIOFilesSubmit
from parserSortie import getLatestSortieFiles
from parserStrings import parseArrayPaires
from parserCSV import getVariableCSV,get2VariablesCSV,putDataCSV,addColumnCSV
# ~~> dependencies towards the root of pytel
from runcode import runCAS,checkConsistency,compilePRINCI
# ~~> dependencies towards other pytel/modules
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) ) # clever you !
from utils.files import getFileContent,putFileContent,createDirectories,copyFile,moveFile, matchSafe, getTheseFiles
from utils.messages import filterMessage, MESSAGES
from mtlplots.plotTELEMAC import Figure

# _____                           __________________________________
# ____/ Specific TELEMAC Toolbox /_________________________________/
#
#   Global dictionnaries to avoid having to read these more than once
#   The keys are the full path to the dictionnaries and therefore
#      allows for <root> and <version> to change
DICOS = {}

def getDICO(cfg,code):

   dicoFile = path.join(path.join(cfg['MODULES'][code]['path'],'lib'),code+cfg['version']+'.dico')
   if dicoFile not in DICOS.keys():
      print '    +> register this DICO file: ' + dicoFile
      frgb,dico = scanDICO(dicoFile)
      idico,odico = getIOFilesSubmit(frgb,dico)
      globals()['DICOS'].update({dicoFile:{ 'frgb':frgb, 'dico':dico, 'input':idico, 'output':odico }})

   return dicoFile

# _____                      _______________________________________
# ____/ General XML Toolbox /______________________________________/
#
"""
   Will read the xml's XML keys based on the template do.
   +: those with None are must have
   +: those without None are optional and reset if there
"""
def getXMLKeys(xml,do):

   xcpt = []                            # try all keys for full report
   done = do.copy()                     # shallow copy is here sufficient
   for key in done.keys():
      if not key in xml.keys():
         if done[key] == None:
            xcpt.append({'name':'getXMLKeys','msg':'cannot find the key: '+key})
      else:
         done[key] = xml.attrib[key]
   if xcpt != []: raise Exception(xcpt) # raise full report

   return done

def setSafe(casFile,cas,idico,odico,safe):

   copyFile(casFile,safe)   # TODO: look at relative paths
   wDir = path.dirname(casFile)

   # ~~> process sortie files if any
   sacFile = path.join(safe,casFile)
   sortieFiles = getLatestSortieFiles(sacFile)

   # ~~> process input / output
   iFS = []; oFS = []
   for k in cas.keys():
      if idico.has_key(k):
         copyFile(path.join(wDir,eval(cas[k][0])),safe)
         ifile = path.join(safe,eval(cas[k][0]))
         iFS.append([k,[ifile],idico[k]])
         #if not path.isfile(ifile):
         #   print '... file does not exist ',ifile
         #   sys.exit()
      if odico.has_key(k):
         ofile = path.join(safe,eval(cas[k][0]))
         oFS.append([k,[ofile],odico[k]])

   return sortieFiles,iFS,oFS

# _____                        _____________________________________
# ____/ Primary Class: ACTION /____________________________________/
#
"""
   In the classes below, the double quoted keys refer to the keys
      of the XML file. Contrarily, the single quoted keys in
      because they are internal to the python scripts.
   In the XML file, you can have multiple actions and each action
      will be associated with multiple configurations:
      did.keys() => array [xref] for each action
      did[xref].keys() => array [cfgname] for each possible configuration
      did[xref][cfgname].keys() =>
         - 'target', basename of the CAS file
         - 'cas', scanned CAS file
         - 'code', name of the module
         - 'title', title of the action (xref)
         - 'cmaps', refer to the directory 'ColourMaps' for colour plotting
         - 'input'
         - 'output'
"""
class ACTION:

   def __init__(self,xmlFile,title='',bypass=True):
      if title != '': self.active["title"] = title
      self.bypass = bypass
      self.active = { 'path':path.dirname(xmlFile),'safe':'','cfg':'','dico':'',
         "target": None, "code": None, "xref": None, "do": None,
         "title": '', "ncsize":''}
      self.dids = {}

   def addAction(self,actions):
      try:
         i = getXMLKeys(actions,self.active)
      except Exception as e:
         raise Exception([filterMessage({'name':'ACTION::addACTION'},e,self.bypass)])  # only one item here
      else:
         self.active = i
      if self.dids.has_key(self.active["xref"]):
         raise Exception([{'name':'ACTION::addACTION','msg':'you are getting me confused, this xref already exists: '+self.active["xref"]}])
      self.dids.update({ self.active["xref"]:{} })
      self.code = self.active["code"]
      return self.active["target"]

   def addCFG(self,cfgname,cfg):
      self.active['cfg'] = cfgname
      if not self.active["code"] in cfg['MODULES'].keys():
         print '... do not know about:' + self.active["code"] + ' in configuration:' + cfgname
         return False
      self.active['safe'] = path.join( path.join(self.active['path'],self.active["xref"]),cfgname )
      self.dids[self.active["xref"]].update( { cfgname: {
         'target': self.active["target"],
         'safe': self.active['safe'],
         'code': self.active["code"],
         "links": {},
         'path': self.active['path'],
         'title': self.active["title"],
         'cmaps': path.join(cfg['PWD'],'ColourMaps')
         } } )
      return True

   def updateCFG(self,d): self.dids[self.active["xref"]][self.active['cfg']].update( d )

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #   Available actions
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   available = "translate;run;compile"

   def compareCAS(self): return
   def comparePRINCI(self): return

   # ~~ Translate the CAS file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   def translateCAS(self,rebuild):
      if not "translate" in self.available.split(';'): return
      xref = self.active["xref"]; cfgname = self.active['cfg']
      active = self.dids[xref][cfgname]
      # ~~> principal CAS file
      casFile = path.join(active['path'],active["target"])
      sacFile = path.join(active['safe'],active["target"])
      oneup = path.dirname(active['safe'])            # copied one level up
      if matchSafe(casFile,active["target"]+'.??',oneup,rebuild):
         print '     +> translate cas file: ' + active["target"]
         casfr,casgb = translateCAS(sacFile,DICOS[active['dico']]['frgb'])  #/!\ removes comments at end of lines
         moveFile(casfr,oneup)
         moveFile(casgb,oneup)
      # ~~> associated CAS files
      for mod in active["links"]:
         link = active["links"][mod]
         casFile = path.join(active['path'],link['target'])
         sacFile = path.join(active['safe'],link['target'])
         if matchSafe(casFile,link['target']+'.??',oneup,rebuild):
            print '     +> translate cas file: ' + link['target']
            casfr,casgb = translateCAS(sacFile,DICOS[link['dico']]['frgb'])  #/!\ removes comments at end of lines
            moveFile(casfr,oneup)
            moveFile(casgb,oneup)

   # ~~ Compile the PRINCI file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   def compilePRINCI(self,cfg,rebuild):
      if not "compile" in self.available.split(';'): return
      xref = self.active["xref"]; cfgname = self.active['cfg']
      active = self.dids[xref][cfgname]
      confirmed = False
      # ~~> principal PRINCI file
      value,default = getKeyWord('FICHIER FORTRAN',active['cas'],DICOS[active['dico']]['dico'],DICOS[active['dico']]['frgb'])
      princiFile = ''; princiSafe = ''
      if value != []:       # you do not need to compile the default executable
         princiFile = path.join(active['path'],eval(value[0]))
         if path.exists(princiFile):
            exeFile = path.join(active['safe'],path.splitext(eval(value[0]))[0] + cfg['SYSTEM']['sfx_exe'])
            if not path.exists(exeFile) or cfg['REBUILD'] == 0:
               print '     +> compiling princi file: ' + path.basename(princiFile)
               copyFile(princiFile,active['safe'])
               print '*********copying '+princiFile+' '+active['safe']
               princiSafe = path.join(active['safe'],path.basename(princiFile))
               confirmed = True
         else:
            raise Exception([{'name':'ACTION::compilePRINCI','msg':'I could not find your PRINCI file: '+princiFile}])
      # ~~> associated PRINCI file
      for mod in active["links"]:
         link = active["links"][mod]
         value,default = getKeyWord('FICHIER FORTRAN',link['cas'],DICOS[link['dico']]['dico'],DICOS[link['dico']]['frgb'])
         princiFilePlage = ''
         if value != []:       # you do not need to compile the default executable
            princiFilePlage = path.join(active['path'],eval(value[0]))
            if path.exists(princiFilePlage):
               if princiSafe != '':
                  print '*********adding content of '+path.basename(princiFilePlage)+' to '+princiSafe
                  putFileContent(princiSafe,getFileContent(princiSafe)+['']+getFileContent(princiFilePlage))
               else:
                  print '     +> compiling princi file: ' + path.basename(princiFilePlage)
                  exeFile = path.join(active['safe'],path.splitext(eval(value[0]))[0] + cfg['SYSTEM']['sfx_exe'])
                  princiSafe = path.join(active['safe'],path.basename(princiFilePlage))
                  print '*********copying '+path.basename(princiFilePlage)+ ' ' + active['safe']
                  copyFile(princiFilePlage,active['safe'])
               confirmed = True
            else:
               raise Exception([{'name':'ACTION::compilePRINCI','msg':'I could not find your PRINCI file: '+princiFilePlage}])
      if confirmed:
         try:
            compilePRINCI(princiSafe,active["code"],self.active['cfg'],cfg,self.bypass)
         except Exception as e:
            raise Exception([filterMessage({'name':'ACTION::compilePRINCI'},e,self.bypass)])  # only one item here
         #moveFile(exeFile,active['safe'])
         print '       ~> compilation successful ! created: ' + path.basename(exeFile)
      #else: you may wish to retrieve the executable for later analysis

   # ~~ Run the CAS file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   def runCAS(self,options,cfg,rebuild):
      if not "run" in self.available.split(';'): return
      
      # ~~> prepare options as if run from command line
      specs = Values()
      setattr(specs,'configName',options.configName)
      setattr(specs,'configFile', options.configFile)
      setattr(specs,'sortieFile',True)
      setattr(specs,'tmpdirectory',True)
      setattr(specs,'rootDir', options.rootDir)
      setattr(specs,'version', options.version)
      setattr(specs,'wDir', options.wDir)
      setattr(specs,'compileonly', False)
      if options.hosts != '': setattr(specs,'hosts', options.hosts)
      else: setattr(specs,'hosts', gethostname().split('.')[0])
      setattr(specs,'split', options.split)
      setattr(specs,'run', options.run)
      setattr(specs,'merge', options.merge)
      if options.ncsize != '' and self.active["ncsize"] != '': self.active["ncsize"] = options.ncsize
      setattr(specs,'ncsize', self.active["ncsize"])
      setattr(specs,'nctile', '1')    # default but should not be used for validation
      setattr(specs,'bypass',self.bypass)

      # In case we are not in a parallel configuration 
      # we do not run test with ncsize greater than one
      if self.active['ncsize'] <> '1' \
         and self.active['ncsize'] <> '0' \
         and self.active['ncsize'] <> '' \
         and ('parallel' not in cfg['options']): return
      # ~~> check on sorties and run
      casFile = path.join(self.active['path'],self.active["target"])
      sacFile = path.join(self.active['safe'],self.active["target"])
      sortieFiles = getLatestSortieFiles(sacFile)
      outputs = self.dids[self.active["xref"]][self.active['cfg']]['output']
      if matchSafe(casFile,self.active["target"]+'_*??h??min??s*.sortie',self.active['safe'],rebuild):
         print '     +> running cas file: ' + self.active["target"]
         for k in outputs: matchSafe('',path.basename(k[1][0]),self.active['safe'],2)
         try:
            sortieFiles = runCAS(self.active['cfg'],cfg,self.active["code"],sacFile,specs)
         except Exception as e:
            raise Exception([filterMessage({'name':'ACTION::runCAS'},e,self.bypass)])  # only one item here
      if sortieFiles != []: self.updateCFG({ 'sortie': sortieFiles })
      
class META:

   def __init__(self,title='',bypass=True):
      if title != '': self.active["title"] = title
      self.bypass = bypass

# _____                      _______________________________________
# ____/ Primary Class: EXTRACT /______________________________________/
#
class EXTRACT:

   def __init__(self,xmlFile,title='',bypass=True):
      if title != '': self.active["title"] = title
      self.bypass = bypass
      self.active = {'path':path.dirname(xmlFile),"xref": None,'cfg':'' ,'safe':''}
      self.tasking = {}
      self.dids = {}

   def addExtract(self,actions):
      try:
         i = getXMLKeys(actions,self.active)
      except Exception as e:
         raise Exception([filterMessage({'name':'EXTRACT::addEXTRACT'},e,self.bypass)])  # only one item here
      else:
         self.active = i
      if self.dids.has_key(self.active["xref"]):
         raise Exception([{'name':'EXTRACT::addExtract','msg':'you are getting me confused, this xref already exists: '+self.active["xref"]}])
      self.dids.update({ self.active["xref"]:{} })
      print '\n    +> Extract :', self.active["xref"]
      return 

   def addCFG(self,cfgname,cfg):
      self.active['cfg'] = cfgname
      self.active['safe'] = path.join( path.join(self.active['path'],self.active["xref"]),cfgname )
      self.dids[self.active["xref"]].update( { cfgname: {'tasks':{} } } )
      return True
   
   def addtask(self,task):
      tasking = { 'safe':'',"xref": '',"target": None,
                      "code": '', "do": None,"title": None,"dependency":'',
                      "reference":'',"ModelData":'',"ExactData":''}
      cfgname = self.active['cfg']
      try:
         self.tasking = getXMLKeys(task,tasking)
      except Exception as e:
         raise Exception([filterMessage({'name':'EXTRACT::addcondition'},e,self.bypass)])  # only one item here
      self.dids[self.active["xref"]][self.active['cfg']]['tasks'] = self.tasking
      self.tasking['safe'] = self.active['safe']
      return self.tasking

   def updateCFG(self,d): self.dids[self.active["xref"]][self.active['cfg']].update( d )

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #   Available Extracts
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   def readCSV(self,task):
      xref = self.active["xref"]; cfgname = self.active['cfg']
      active = self.dids[xref][cfgname]
      if self.tasking["reference"] != '':
         csvFile = path.join(self.dids[self.tasking["reference"]][cfgname]['tasks']['safe'],self.tasking["target"])
      else : csvFile = path.join(self.active['path'],self.tasking["target"])
      print '    +> reading your csv file :', self.tasking["target"]
      self.dids[xref][cfgname]['csv']= [csvFile]
      return csvFile
      
   def runPYTHON(self,task):
      xref = self.active["xref"]; cfgname = self.active['cfg']
      active = self.dids[xref][cfgname]
      pythonFile = path.join(self.active['path'],self.tasking["target"])
      copyFile(pythonFile,self.tasking['safe'])
      for file in self.tasking['dependency'].split(';'):
         if self.tasking["reference"] != '':
            depfile = path.join(self.dids[self.tasking["reference"]][cfgname]['tasks']['safe'],file)
            copyFile(depfile,self.active['safe'])
         else : copyFile(file,self.active['safe'])
      os.chdir(self.active['safe'])

      print '       +> running your python script :', self.tasking["target"],'\n'
      if os.sep == '/': 
         cmd = 'python '+ self.tasking["target"]
         os.system(cmd)
      else : subprocess.check_call(self.tasking["target"],shell = True)
      
      if self.tasking["code"]=="postel3d":
         self.dids[xref][cfgname]['output']= []
         for filenames in walk(self.active['safe']) :
            for file in filenames[2] :
               if 'old' not in file.split('.'):
                  if 'hor' in file.split('_'): 
                     self.dids[xref][cfgname]['output'].append(path.join(filenames[0],file))
                  if 'ver' in file.split('_'): 
                     self.dids[xref][cfgname]['output'].append(path.join(filenames[0],file))   
               elif 'old' in file.split('.') : continue      
      return
      
   def CalcL2error(self,task):   
      xref = self.active["xref"]; cfgname = self.active['cfg']
      active = self.dids[xref][cfgname]
      file = self.tasking["target"]
      print '       +> Calculating L2 error :', self.tasking["title"],'\n'
      if self.tasking["reference"] != '':
         file = path.join(self.dids[self.tasking["reference"]][cfgname]['tasks']['safe'],file)
      var1 = self.tasking["ModelData"].lower()
      var2 = self.tasking["ExactData"].lower()
      VarModel,VarExperiment = get2VariablesCSV(file,var1,var2)
      L2error = (la.norm(VarModel-VarExperiment))/(la.norm(VarExperiment))
      columns = [[self.tasking["title"],'None',L2error]]
      file2 = path.join(self.active['safe'], xref +'.csv')
      if path.exists(file2): 
         columns = [[self.tasking["title"],'None',L2error]]
         addColumnCSV(file2,columns[0])
      else : putDataCSV(file2,columns)
      return
      
# _____                      _______________________________________
# ____/ Primary Class: PLOT /______________________________________/
#            

class PLOT:

   def __init__(self,title='',bypass=True):
      if title != '': self.drawing["title"] = title
      self.bypass = bypass
      self.drawing = {}; self.layering = {}
      self.active = { 'type':'', 'xref':'', 'roi':'' }
      self.dids = {}

   def addPlotType(self,plot):   # types: plot1d, plot2d, plot3d, plotpv, ...
      self.dids.update({plot:{}})
      self.active['type'] = plot
      return

   def addDraw(self,draw):       # plots: plot1d, plot2d, plot3d, plotpv, ...
      drawing = { "type":'', "xref":'', "deco": 'line', "size":'[10;10]',
         "time": '[-1]', "extract": '', "vars": '', "roi": '',
         "title": '', "config": 'distinct', 'outFormat': 'png',
         'layers':[],"columns":''}     # draw includes an array of layers
      try:
         i = getXMLKeys(draw,drawing)
      except Exception as e:
         raise Exception([filterMessage({'name':'PLOT::addDraw'},e,self.bypass)])
      else:
         self.drawing = i
      self.active['xref'] = self.drawing["xref"]
      self.active['roi'] = self.drawing["roi"]
      if self.dids[self.active['type']].has_key(self.drawing["xref"]):
         raise Exception([{'name':'PLOT::addDRAW','msg':'you are getting me confused, this xref already exists: '+self.drawing["xref"]}])
      self.dids[self.active['type']].update({self.drawing["xref"]:self.drawing})
      return

   def addLayer(self,layer):
      # ~~> set default from the upper drawer
      layering = { "vars":self.drawing["vars"], "time":self.drawing["time"],
         "extract":self.drawing["extract"], "target":'',
         "title":'', "config":self.drawing["config"],
         "columns":self.drawing["columns"]}
      # ~~> reset from layer
      try:
         self.layering = getXMLKeys(layer,layering)
      except Exception as e:
         raise Exception([filterMessage({'name':'PLOT::addLayer'},e,self.bypass)])
      # ~~> filling-in remaining gaps
      self.layering = self.distributeMeta(self.layering)
      return self.layering["target"]

   def targetLayer(self,layers):
      self.layering.update({ 'fileName': layers })
      self.dids[self.active['type']][self.active['xref']]['layers'].append(self.layering)
      return

   def distributeMeta(self,layer):
      # ~~> distribute decoration
      vars = layer["vars"].split(';')
      for i in range(len(vars)):
         if ':' not in vars[i]: vars[i] = vars[i] + ':' + self.drawing["deco"]
      layer["vars"] = ';'.join(vars)
      return layer

   def update(self,d): self.dids[self.active['type']][self.active['xref']].update( d )

   def findLayerConfig(self,dido,src):
      layers = {}
      oneFound = False
      for cfg in dido.keys():
         cfgFound = False
         if dido[cfg].has_key(src):
            layers.update({ cfg:[dido[cfg][src],'',src] })
            cfgFound = True
         if not cfgFound and dido[cfg].has_key('input'):
            for i,j,k in dido[cfg]['input']:
               k = k.split(';')
               if src in k[1]:               # filename, fileForm, fileType
                  # /!\ Temporary fix because TOMAWAC's IOs names are not yet standard TELEMAC
                  if k[5] =='SCAL': k[5] = k[1]
                  # \!/
                  layers.update({ cfg:[j,k[3],k[5]] })
                  cfgFound  = True
         if not cfgFound and dido[cfg].has_key('output'):
            if dido[cfg]['code'] == 'postel3d':
               for file in dido[cfg]['output']:
                  if src == path.basename(file) :
                     layers.update({ cfg:[[file],'','SELAFIN'] })
                     cfgFound = True 
            else :
               for i,j,k in dido[cfg]['output']:
                  k = k.split(';')
                  if src in k[1]:               # filename, fileForm, fileType
                     # /!\ Temporary fix because TOMAWAC's IOs names are not yet standard TELEMAC
                     if k[5] =='SCAL': k[5] = k[1]
                     # \!/
                     layers.update({ cfg:[j,k[3],k[5]] })
                     cfgFound = True
         oneFound = oneFound or cfgFound
      if not oneFound:
         raise Exception([{'name':'PLOT::findLayerConfig','msg':'did not find the file to draw: '+src}])
      return layers
   
   
   
   
   
class CRITERIA:

   def __init__(self,xmlFile,title='',bypass=True):
      if title != '': self.active["title"] = title
      self.bypass = bypass
      self.variabling = {}; self.conditionning = {}
      self.active = { 'xref':None}
      self.dids = {}

   def addCriteria(self,criteria):
      try:
         i = getXMLKeys(criteria,self.active)
      except Exception as e:
         raise Exception([filterMessage({'name':'ACTION::addCriteria'},e,self.bypass)])  # only one item here
      else:
         self.active = i
      if self.dids.has_key(self.active["xref"]):
         raise Exception([{'name':'CRITERIA::addCriteria','msg':'you are getting me confused, this xref already exists: '+self.active["xref"]}])
      self.dids.update({ self.active["xref"]:{} })
      print '\n    +> Validation Criteria :', self.active["xref"]
      return 
   
   def addCFG(self,cfgname,cfg):
      self.active['cfg'] = cfgname
      self.dids[self.active["xref"]].update( { cfgname: {'variables':{},'condition':{} } } )
      return True

   def addvariable(self,variable):
      variabling = {"vars": None, "target":None, "path":''}
      cfgname = self.active['cfg']
      try:
         self.variabling = getXMLKeys(variable,variabling)
      except Exception as e:
         raise Exception([filterMessage({'name':'CRITERIA::addvariable'},e,self.bypass)])  # only one item here
           
      self.dids[self.active["xref"]][self.active['cfg']]['variables'].update({self.variabling["vars"]:{}})
      active = self.dids[self.active["xref"]][self.active['cfg']]['variables']
          
      folder,file = self.variabling["target"].split(':')
      pathfolder = path.join(os.getcwd(),folder)
      pathcfg = path.join(pathfolder,cfgname)
      pathfile = path.join(pathcfg,file) 
      
      active[self.variabling["vars"]]['path'] = [pathfile]
      self.variabling['path'] = [pathfile]
      return
   
   def addcondition(self,condition,NBR):
      conditionning = {"do": None, "success":None, "failure":None, "warning":'',
                        "result":[]}
      cfgname = self.active['cfg']
      try:
         self.conditionning = getXMLKeys(condition,conditionning)
      except Exception as e:
         raise Exception([filterMessage({'name':'CRITERIA::addcondition'},e,self.bypass)])  # only one item here
           
      self.dids[self.active["xref"]][self.active['cfg']]['condition'][NBR] = self.conditionning
      return self.conditionning

   def updateCFG(self,d): self.dids[self.active["xref"]][self.active['cfg']].update( d )
   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #   Available Operations for criteria
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   def compare(self,condition):
      variables = self.dids[self.active["xref"]][self.active['cfg']]['variables']
      
      ######### Success condition ################################
      NameVar1,operator,NameVar2 = condition["success"].split(':')
      
      print '         +> comparing :', NameVar1, '&', NameVar2

      if NameVar1.isalpha() == False : var1 = float(NameVar1)
      else :     
         PathVar1 = variables[NameVar1]['path'][0]
         var1 =getVariableCSV(PathVar1,NameVar1)
      if NameVar2.isalpha() == False : var2 = float(NameVar2)
      else :     
         PathVar2 = variables[NameVar2]['path'][0]
         var2 = getVariableCSV(PathVar2,NameVar2)
      
      if operator == 'LE' :
         if var1 <= var2 : condition["result"].append('success')
      elif operator == 'LT' :
         if var1 < var2 : condition["result"].append('success')   
      elif operator == 'GE' :
         if var1 >= var2 : condition["result"].append('success')
      elif operator == 'GT' :
         if var1 > var2 : condition["result"].append('success')
      
         
      ######### Failure condition ################################
      NameVar1,operator,NameVar2 = condition["failure"].split(':')

      if NameVar1.isalpha() == False : var1 = float(NameVar1)
      else :     
         PathVar1 = variables[NameVar1]['path'][0]
         var1 =getVariableCSV(PathVar1,NameVar1)
      if NameVar2.isalpha() == False : var2 = float(NameVar2)
      else :     
         PathVar2 = variables[NameVar2]['path'][0]
         var2 = getVariableCSV(PathVar2,NameVar2)
         
      if operator == 'LE' :
         if var1 <= var2 : condition["result"].append('failed')
      elif operator == 'LT' :
         if var1 < var2 : condition["result"].append('failed')   
      elif operator == 'GE' :
         if var1 >= var2 : condition["result"].append('failed')
      elif operator == 'GT' :
         if var1 > var2 : condition["result"].append('failed')   
         
      ######### Warning condition ################################
      if condition["result"] == [] : condition["result"].append('warning')  
      
      return

# _____                     ________________________________________
# ____/ XML Parser Toolbox /_______________________________________/
#
"""
   Assumes that the directory ColourMaps is in PWD (i.e. ~root/pytel.)
"""
def runXML(xmlFile,xmlConfig,bypass,runcase,postprocess,validationCriteria):

   xcpt = []                            # try all keys for full report

   # ~~ Parse xmlFile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   import xml.etree.ElementTree as XML
   print '... reading XML test specification file: ' + path.basename(xmlFile)
   f = open(xmlFile,'r')
   xmlTree = XML.parse(f)  # may need to try first and report error
   xmlRoot = xmlTree.getroot()
   f.close()

   # ~~ Meta data process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   title = ""
   #meta = META(title)
   #print '\n... acquiring meta data'
   display = False

   # ~~ Action analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if runcase != False :
      do = ACTION(xmlFile,title,bypass)
      first = True
      for action in xmlRoot.findall("action"):
         if first:
            print '\n... looping through the todo list'
            first = False

         # ~~ Step 1. Common check for keys and CAS file ~~~~~~~~~~~~~~
         try:
            doadd = do.addAction(action)
         except Exception as e:
            xcpt.append(filterMessage({'name':'runXML','msg':'add todo to the list'},e,bypass))
            continue    # bypass rest of the loop
         else:
            casFile = path.join(do.active['path'],doadd)
            if not path.isfile(casFile):
               xcpt.append({'name':'runXML','msg':'could not find your CAS file'+path.basename(casFile)})
               continue    # bypass rest of the loop

         # ~~ Step 2. Loop over configurations ~~~~~~~~~~~~~~~~~~~~~~~~
         for cfgname in xmlConfig.keys():
            cfg = xmlConfig[cfgname]['cfg']
            if not do.addCFG(cfgname,cfg): continue

            # ~~> Parse DICO File and its IO Files default (only once)
            dicoFile = getDICO(cfg,do.active["code"])
            do.updateCFG({'dico':dicoFile})
            dico = DICOS[dicoFile]['dico']
            frgb = DICOS[dicoFile]['frgb']
            cas = readCAS(scanCAS(casFile),dico,frgb)
            if do.active["ncsize"] != '': setKeyValue('PROCESSEURS PARALLELES',cas,frgb,int(do.active["ncsize"]))
            do.updateCFG({'cas':cas})
            if not checkConsistency(cas,dico,frgb,cfg): continue

            # ~~> Create the safe
            createDirectories(do.active['safe'])
            idico = DICOS[dicoFile]['input']
            odico = DICOS[dicoFile]['output']
            # ~~> Define config-split storage
            sortieFiles,iFS,oFS = setSafe(casFile,cas,idico,odico,do.active['safe'])   # TODO: look at relative paths
            if sortieFiles != []: do.updateCFG({ 'sortie': sortieFiles })
            do.updateCFG({ 'input':iFS })
            do.updateCFG({ 'output':oFS })

            # ~~> Case of coupling
            cplages,defaut = getKeyWord('COUPLING WITH',cas,dico,frgb)
            links = {}
            for cplage in cplages:
               for mod in cfg['MODULES'].keys():
                  if mod in cplage.lower():
                     # ~~> Extract the CAS File name
                     casFilePlage,defaut = getKeyWord(mod.upper()+' STEERING FILE',cas,dico,frgb)
                     if casFilePlage == []: casFilePlage = defaut[0]
                     else: casFilePlage = eval(casFilePlage[0])
                     casFilePlage = path.join(path.dirname(casFile),casFilePlage)
                     if not path.isfile(casFilePlage): raise Exception([{'name':'runCAS','msg':'missing coupling CAS file for '+mod+': '+casFilePlage}])
                     # ~~> Read the DICO File
                     dicoFilePlage = getDICO(cfg,mod)
                     dicoPlage = DICOS[dicoFilePlage]['dico']
                     frgbPlage = DICOS[dicoFilePlage]['frgb']
                     # ~~> Read the coupled CAS File
                     casPlage = readCAS(scanCAS(casFilePlage),dicoPlage,frgbPlage)
                     # ~~> Fill-in the safe
                     idicoPlage = DICOS[dicoFilePlage]['input']
                     odicoPlage = DICOS[dicoFilePlage]['output']
                     sortiePlage,iFSPlage,oFSPlage = setSafe(casFilePlage,casPlage,idicoPlage,odicoPlage,do.active['safe'])   # TODO: look at relative paths
                     links.update({mod:{}})
                     links[mod].update({ 'code':mod, 'target':path.basename(casFilePlage),
                        'cas':casPlage, 'frgb':frgbPlage, 'dico':dicoFilePlage,
                        'iFS':iFSPlage, 'oFS':oFSPlage, 'sortie':sortiePlage })
                     if sortiePlage != []: links[mod].update({ 'sortie':sortiePlage })
            if links != {}: do.updateCFG({ "links":links })

            # ~~ Step 3. Complete all actions ~~~~~~~~~~~~~~~~~~~~~~~~~
            # options.do takes: translate;run;compile and none
            doable = xmlConfig[cfgname]['options'].do
            if doable == '': doable = do.active["do"]
            if doable == '': doable = do.available
            display = display or xmlConfig[cfgname]['options'].display

            # ~~> Action type A. Translate the CAS file
            if "translate" in doable.split(';'):
               try:
                  do.translateCAS(cfg['REBUILD'])
               except Exception as e:
                  xcpt.append(filterMessage({'name':'runXML','msg':'   +> translate'},e,bypass))

            # ~~> Action type B. Analysis of the CAS file
            # TODO:
            # - comparison with DEFAULT values of the DICTIONARY
            #if "cas" in doable.split(';'):
            # - comparison of dictionnaries betwen configurations
            #if "dico" in doable.split(';'):

            # ~~> Action type C. Analysis of the PRINCI file
            # TODO:
            # - comparison with standard source files
            #if "princi" in doable.split(';'):
            #   out = diffTextFiles( f,t )
            # - comparison of called subroutines between configurations

            # ~~> Action type D. Compilation of PRINCI file
            # Contrary to the other step, Step 8 is completed where the original CAS file is
            # (for no particularly good reason)
           # YA: Removed because generating error when coupling copying the merged fortran file 
           # as the main module file
           # runcode is compiling the code anyway so what is the point of this action?
           # TODO:
           #    See what to do of this action
           #if "compile" in doable.split(';'):
           #   try:
           #      do.compilePRINCI(cfg,cfg['REBUILD'])
           #   except Exception as e:
           #      xcpt.append(filterMessage({'name':'runXML','msg':'   +> compile'},e,bypass))

            # ~~> Action type E. Running CAS files
            #cpu=[]
            if "run" in doable.split(';'):
               try:
                  do.runCAS(xmlConfig[cfgname]['options'],cfg,cfg['REBUILD'])
               except Exception as e:
                  xcpt.append(filterMessage({'name':'runXML','msg':'   +> run'},e,bypass))
           

   # ~~ Extraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # did has all the IO references and the latest sortie files
   if postprocess != False :
      doextract = EXTRACT(xmlFile,title,bypass)
      first = True
      for extract in xmlRoot.findall("extract"):
         if first:
            print '\n... looping through the extract todo list'
            first = False

     # ~~ Step 1. Common check for keys and CAS file ~~~~~~~~~~~~~~
         try:
            doadd = doextract.addExtract(extract)
         except Exception as e:
            xcpt.append(filterMessage({'name':'runXML','msg':'add todo to the list'},e,bypass))
            continue
            
         for cfgname in xmlConfig.keys():
            extractcfg = xmlConfig[cfgname]['cfg']
            if not doextract.addCFG(cfgname,extractcfg): continue
         
            createDirectories(doextract.active['safe'])
            
            for task in extract.findall("task"):
               try:
                   doaddtask = doextract.addtask(task)
               except Exception as e:
                  xcpt.append(filterMessage({'name':'runXML','msg':'add todo to the list'},e,bypass))
                  continue
         
               for actionXREF in do.dids.keys(): racine = do.dids[actionXREF][cfgname]['path']
               
               if "readCSV" in doaddtask["do"]:
                  os.chdir(racine)
                  try:
                     csvFile = doextract.readCSV(doaddtask)
                  except Exception as e:
                     xcpt.append(filterMessage({'name':'runXML','msg':'   +> readCSV'},e,bypass))

               if "runPYTHON" in doaddtask["do"]:
                  os.chdir(racine)
                  try:
                     if doextract.tasking["code"] == "postel3d" :
                        ResultFile1 = do.dids[actionXREF][cfgname]['output'][1][1][0]
                        ResultFile2 = do.dids[actionXREF][cfgname]['output'][0][1][0]
                        copyFile(ResultFile1,doextract.active['safe'])
                        copyFile(ResultFile2,doextract.active['safe'])
                     elif do.dids[actionXREF][cfgname]['links']!= {} :
                        for code in do.dids[actionXREF][cfgname]['links'].keys():
                           ResultFile = do.dids[actionXREF][cfgname]['links'][code]['oFS'][0][1][0]
                           copyFile(ResultFile,doextract.active['safe'])
                     else : 
                        ResultFile = do.dids[actionXREF][cfgname]['output'][0][1][0]
                        copyFile(ResultFile,doextract.active['safe'])
                     doextract.runPYTHON(doextract.dids)
                  except Exception as e:
                     xcpt.append(filterMessage({'name':'runXML','msg':'   +> runPYTHON'},e,bypass))  

               if "L2error" in doaddtask["do"]:
                  os.chdir(racine)
                  try:
                     doextract.CalcL2error(doaddtask)
                  except Exception as e:
                     xcpt.append(filterMessage({'name':'runXML','msg':'   +> CalcL2error'},e,bypass))


      # ~~ Gathering targets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      plot = PLOT(title,bypass)
      first = True
      for typePlot in ["plot1d","plot2d","plot3d","plotpv"]:
         plot.addPlotType(typePlot)
         for ploting in xmlRoot.findall(typePlot):
            if first:
               print '\n... gathering targets through the plot list'
               first = False
            # ~~ Step 1. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~
            try:
               plot.addDraw(ploting)
            except Exception as e:
               xcpt.append(filterMessage({'name':'runXML','msg':'add plot to the list'},e,bypass))
               continue   # bypass the rest of the for loop

            # ~~ Step 2. Cumul layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            for layer in ploting.findall("layer"):
               try:
                  target = plot.addLayer(layer)
               except Exception as e:
                  xcpt.append(filterMessage({'name':'runXML','msg':'add layer to the list'},e,bypass))
                  continue   # bypass the rest of the for loop

               # ~~> round up targets and their configurations
               xref,src = target.split(':')
               if doextract.dids.has_key(xref): 
                  try:
                     findlayer = plot.findLayerConfig(doextract.dids[xref],src)
                  except Exception as e:
                     xcpt.append(filterMessage({'name':'runXML','msg':'could not find reference to draw the extract: '+xref},e))
                     continue    # bypass the rest of the for loop
                  else : 
                     plot.targetLayer(findlayer)
               elif do.dids.has_key(xref):
                  try:
                     findlayer = plot.findLayerConfig(do.dids[xref],src)
                  except Exception as e:
                     xcpt.append(filterMessage({'name':'runXML','msg':'could not find reference to draw the action: '+xref},e))
                     continue    # bypass the rest of the for loop
                  else : 
                     plot.targetLayer(findlayer)
               else : xcpt.append({'name':'runXML','msg':'could not find reference to draw the action: '+xref})      
            plot.update(plot.drawing)

      # ~~ Matrix distribution by plot types ~~~~~~~~~~~~~~~~~~~~~~~~~~
      # /!\ configurations cannot be called "together" or "distinct" or "oneofall"
      first = True
      for typePlot in plot.dids.keys():
         if first:
            print '\n... plotting figures'
            first = False

         for xref in plot.dids[typePlot]:
            print '    +> reference: ',xref,' of type ',typePlot

            draw = plot.dids[typePlot][xref]
            xlayers = ''   # now done with strings as arrays proved to be too challenging
            for layer in draw["layers"]:
               if layer['config'] == 'together':
                  xys = []
                  for x in xlayers.split('|'): xys.append( (x+';'+':'.join( layer['fileName'].keys() )).strip(';') )
                  xlayers = '|'.join(xys)
               elif layer['config'] == 'distinct':
                  ylayers = layer['fileName'].keys()
                  xys = []
                  for i in range(len(ylayers)):
                     for x in xlayers.split('|'): xys.append( (x+';'+ylayers[i]).strip(';') )
                  xlayers = '|'.join(xys)
               elif layer['config'] == 'oneofall':
                  xys = []; cfg = layer['fileName'].keys()[0]     #/!\ you are sure to have at least one (?)
                  for x in xlayers.split('|'): xys.append( (x+';'+cfg).strip(';') )
                  xlayers = '|'.join(xys)
               else:
                  if layer['config'] in layer['fileName'].keys():
                     xys = []
                     for x in xlayers.split('|'): xys.append( (x+';'+layer['config']).strip(';') )
                  xlayers = '|'.join(xys)

            nbFig = 0; alayers = xlayers.split('|')
            for cfglist in alayers:
               # ~~> Figure name
               if len(alayers) == 1:
                  figureName = '.'.join([xref.replace(' ','_'),draw['outFormat']])
               else:
                  nbFig += 1
                  figureName = '.'.join([xref.replace(' ','_'),str(nbFig),draw['outFormat']])
               print '       ~> saved as: ',figureName
               figureName = path.join(path.dirname(xmlFile),figureName)
               # ~~> Figure size
               draw["size"] = parseArrayPaires(draw["size"])
               # ~~> Create Figure
               figure = Figure(typePlot,draw,display,figureName)

               for layer,cfgs in zip(draw["layers"],cfglist.split(';')):
                  for cfg in cfgs.split(':'):
                     for file in layer['fileName'][cfg][0]:
                        # ~~ 1d plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        if typePlot == "plot1d":
                           #print typePlot,' ... drawing'
                           figure.draw( layer['fileName'][cfg][2], { 'file': file,
                              'vars': layer["vars"], 'extract':parseArrayPaires(layer["extract"]),
                              'type': draw['type'], 'time':parseArrayPaires(layer["time"])[0],#})
                              'columns':layer["columns"]})

                        # ~~ 2d plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        if typePlot == "plot2d":  # so far no plot type, but this may change
                           figure.draw( layer['fileName'][cfg][2], { 'file': file,
                              'roi': parseArrayPaires(draw['roi']),
                              'vars': layer["vars"], 'extract':parseArrayPaires(layer["extract"]),
                              'type': draw['type'], 'time':parseArrayPaires(layer["time"])[0] } )


               figure.show()
               
   # ~~ Validation Criteria ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #
   racine = path.split(xmlFile)[0]
   os.chdir(racine)
   if validationCriteria != False : 
      docriteria = CRITERIA(xmlFile,title,bypass)
      first = True
      for criteria in xmlRoot.findall("criteria"):
         if first:
            print '\n... looping through the Criteria todo list'
            first = False

     # ~~ Step 1. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~
         try:
            doadd = docriteria.addCriteria(criteria)
         except Exception as e:
            xcpt.append(filterMessage({'name':'runXML','msg':'add todo to the list'},e,bypass))
            continue
            
         for cfgname in xmlConfig.keys():
            criteriacfg = xmlConfig[cfgname]['cfg']
            if not docriteria.addCFG(cfgname,criteriacfg): continue
         
            for vars in criteria.findall("variable"):
               try:
                  doaddvariable = docriteria.addvariable(vars)
               except Exception as e:
                  xcpt.append(filterMessage({'name':'runXML','msg':'add todo to the list'},e,bypass))
                  continue
            
            conditionNBR = 1
            for condition in criteria.findall("condition"):               
               try:
                  doaddcondition = docriteria.addcondition(condition,conditionNBR)
                  conditionNBR += 1
               except Exception as e:
                  xcpt.append(filterMessage({'name':'runXML','msg':'add todo to the list'},e,bypass))
                  continue  

               if "compare" in doaddcondition["do"]:
                  try:
                     docriteria.compare(doaddcondition)
                  except Exception as e:
                     xcpt.append(filterMessage({'name':'runXML','msg':'   +> compare'},e,bypass))

         
         for criteria in docriteria.dids.keys() :        
            for cfg in docriteria.dids[criteria]:
               for NBR in docriteria.dids[criteria][cfg]['condition']:
                  if docriteria.dids[criteria][cfg]['condition'][NBR]['result'][0] == 'success' : continue
                  else :
                     if docriteria.dids[criteria][cfg]['condition'][NBR]['result'][0] == 'warning' :
                        print '\n!!!!!  Warning about the follwing condition %s !!!!!!!!\n' % docriteria.dids[criteria][cfg]['condition'][NBR]['success']
                     else :
                        #raise Exception([{'name':'CRITERIA','msg':'the following criteria failed: '+criteria}])
                        raise Exception([{'name':'CRITERIA','msg':'the following condition failed: '+docriteria.dids[criteria][cfg]['condition'][NBR]['success']}])
        



   """
            # ~~> plot3d
               if plot["extract"] == '': plot["extract"] = "minmax"
   """
   # ~~ Error management ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpt != []:  # raise full report
      raise Exception({'name':'runXML','msg':'in xmlFile: '+xmlFile,'tree':xcpt})
   
   return 

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$2-Aug-2011 11:51:36$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
