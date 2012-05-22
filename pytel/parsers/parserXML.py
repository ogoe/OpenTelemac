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
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from os import path
from optparse import Values
import sys
from socket import gethostname
# ~~> dependencies from within pytel/parsers
from parserKeywords import scanDICO,scanCAS,readCAS,translateCAS, getKeyWord,setKeyValue, getIOFilesSubmit
from parserSortie import getLatestSortieFiles
from parserStrings import parseArrayPaires
# ~~> dependencies towards the root of pytel
from runcode import runCAS,checkConsistency,compilePRINCI
# ~~> dependencies towards other pytel/modules
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) ) # clever you !
from utils.files import createDirectories,copyFile,moveFile, matchSafe
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
      iFS,oFS = getIOFilesSubmit(frgb,dico)
      globals()['DICOS'].update({dicoFile:{ 'frgb':frgb, 'dico':dico, 'input':iFS, 'output':oFS }})

   return dicoFile,DICOS[dicoFile]

# _____                      _______________________________________
# ____/ General XML Toolbox /______________________________________/
#
"""
   Will read the xml's XML keys based on the template do.
   +: those with None are must have
   +: those without None are optional and reset if there
"""
def getXMLKeys(xml,do):

   done = do.copy()               # shallow copy is here sufficient
   for key in done.keys():
      if not key in xml.keys():
         if done[key] == None:
            print '... cannot find the key: ' + key
            sys.exit()
      else:
         done[key] = xml.attrib[key]

   return done

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
   active = { 'path':'','safe':'','casFile':'','cfg':'','dico':'',
      "target": None, "code": None, "xref": None, "do": None,
      "title": '', "ncsize":'' }
   dids = {}

   def __init__(self,title=''):
      if title != '': self.active["title"] = title

   def addAction(self,actions):
      self.active = getXMLKeys(actions,self.active)
      if self.dids.has_key(self.active["xref"]):
         print '... you are getting me confused, this xref already exists:',self.active["xref"]
         sys.exit()
      self.dids.update({ self.active["xref"]:{} })
      self.code = self.active["code"]
      return self.active["target"]

   def addCAS(self,casFile):
      self.active['path'] = path.dirname(casFile)
      self.active['casFile'] = casFile
      return casFile

   def addCFG(self,cfgname,cfg):
      self.active['cfg'] = cfgname
      if not self.active["code"] in cfg['MODULES'].keys():
         print '... do not know about:' + self.active["code"] + ' in configuration:' + cfgname
         return False
      self.active['safe'] = path.join( path.join(self.active['path'],self.active["xref"]),cfgname )
      self.update( { cfgname: {
         'target': self.active["target"],
         'safe': self.active['safe'],
         'code': self.active["code"],
         'title': self.active["title"],
         'cmaps': path.join(cfg['PWD'],'ColourMaps')
         } } )
      return True

   def setSafe(self,dico):

      # ~~> aliases
      xref = self.active["xref"]; cfgname = self.active['cfg']
      cas = self.dids[xref][cfgname]['cas']
      safe = self.active['safe']

      # ~~> create the safe
      casFile = path.join(self.active['path'],self.active["target"])
      createDirectories(self.active['safe'])
      copyFile(casFile,self.active['safe'])   # TODO: look at relative paths

      # ~~> process sortie files if any
      sacFile = path.join(self.active['safe'],self.active["target"])
      sortieFiles = getLatestSortieFiles(sacFile)
      if sortieFiles != []: self.updateCFG({ 'sortie': sortieFiles })

      # ~~> process input / output
      iFS = []; oFS = []
      for k in cas.keys():
         if dico['input'].has_key(k):
            copyFile(path.join(self.active['path'],eval(cas[k][0])),safe)
            ifile = path.join(safe,eval(cas[k][0]))
            iFS.append([k,[ifile],dico['input'][k]])
            #if not path.isfile(ifile):
            #   print '... file does not exist ',ifile
            #   sys.exit()
         if dico['output'].has_key(k):
            ofile = path.join(safe,eval(cas[k][0]))
            oFS.append([k,[ofile],dico['output'][k]])
      self.updateCFG({ 'input':iFS })
      self.updateCFG({ 'output':oFS })

      return

   def updateCFG(self,d): self.dids[self.active["xref"]][self.active['cfg']].update( d )
   def update(self,c): self.dids[self.active["xref"]].update( c )

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #   Available actions
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   available = "translate;run;compile"

   def compareCAS(self): return
   def comparePRINCI(self): return

   # ~~ Translate the CAS file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   def translateCAS(self,dico,rebuild):
      if not "translate" in self.available.split(';'): return
      casFile = path.join(self.active['path'],self.active["target"])
      sacFile = path.join(self.active['safe'],self.active["target"])
      oneup = path.dirname(self.active['safe'])            # copied one level up
      if matchSafe(casFile,self.active["target"]+'.??',oneup,rebuild):
         print '     +> translate cas file: ' + self.active["target"]
         casfr,casgb = translateCAS(sacFile,dico['frgb'])  #/!\ removes comments at end of lines
         moveFile(casfr,oneup)
         moveFile(casgb,oneup)

   # ~~ Compile the PRINCI file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   def compilePRINCI(self,dico,cfg,rebuild):
      if not "compile" in self.available.split(';'): return
      xref = self.active["xref"]; cfgname = self.active['cfg']
      value,default = getKeyWord('FICHIER FORTRAN',self.dids[xref][cfgname]['cas'],dico['dico'],dico['frgb'])
      princiFile = ''
      if value != []:       # you do not need to compile the default executable
         princiFile = path.join(self.active['path'],eval(value[0]))
         if path.exists(princiFile):
            exeFile = path.join(self.active['safe'],path.splitext(eval(value[0]))[0] + cfg['SYSTEM']['sfx_exe'])
            if not path.exists(exeFile) or cfg['REBUILD'] == 0:
               print '     +> compiling princi file: ' + path.basename(princiFile)
               copyFile(princiFile,self.active['safe'])
               compilePRINCI(princiFile,self.active["code"],self.active['cfg'],cfg)
               moveFile(exeFile,self.active['safe'])
         else:
            print '... I could not find your PRINCI file:',princiFile
            sys.exit()
            #else: you may wish to retrieve the executable for later analysis

   # ~~ Run the CAS file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   def runCAS(self,options,cfg,rebuild):
      if not "run" in self.available.split(';'): return
      
      # ~~> prepare options as if run from command line
      specs = Values()
      setattr(specs,'configName',options.configName)
      setattr(specs,'configFile', options.configFile)
      setattr(specs,'sortieFile',True)
      setattr(specs,'tmpdirectory','')
      setattr(specs,'rootDir', options.rootDir)
      setattr(specs,'version', options.version)
      setattr(specs,'wDir', options.wDir)
      setattr(specs,'compileonly', False)
      if options.hosts != '': setattr(specs,'hosts', options.hosts)
      else: setattr(specs,'hosts', gethostname().split('.')[0])
      setattr(specs,'split', options.split)
      setattr(specs,'run', options.run)
      setattr(specs,'merge', options.merge)
      if options.ncsize != '': self.active["ncsize"] = options.ncsize
      setattr(specs,'ncsize', self.active["ncsize"])

      # ~~> check on sorties and run
      casFile = path.join(self.active['path'],self.active["target"])
      sacFile = path.join(self.active['safe'],self.active["target"])
      sortieFiles = getLatestSortieFiles(sacFile)
      outputs = self.dids[self.active["xref"]][self.active['cfg']]['output']
      if matchSafe(casFile,self.active["target"]+'_*??h??min??s*.sortie',self.active['safe'],rebuild):
         print '     +> running cas file: ' + self.active["target"]
         for k in outputs: matchSafe('',path.basename(k[1][0]),self.active['safe'],2)
         sortieFiles = runCAS(self.active['cfg'],cfg,self.active["code"],sacFile,specs)
      if sortieFiles != []: self.updateCFG({ 'sortie': sortieFiles })

class META:

   def __init__(self,title=''):

      return

#class EXTRACT:
# _____                      _______________________________________
# ____/ Primary Class: PLOT /______________________________________/
#
class PLOT:
   drawing = {}; layering = {}; active = { 'type':'', 'xref':'', 'roi':'' }; dids = {}

   def __init__(self,title=''):
      if title != '': self.drawing["title"] = title

   def addPlotType(self,plot):   # types: plot1d, plot2d, plot3d, plotpv, ...
      self.dids.update({plot:{}})
      self.active['type'] = plot
      return

   def addDraw(self,draw):       # plots: plot1d, plot2d, plot3d, plotpv, ...
      drawing = { "type":'', "xref":'', "deco": 'line', "size":'[10;10]',
         "time": '[-1]', "extract": '', "vars": '', "roi": '',
         "title": '', "config": 'distinct', 'outFormat': 'png',
         'layers':[] }     # draw includes an array of layers
      self.drawing = getXMLKeys(draw,drawing)
      self.active['xref'] = self.drawing["xref"]
      self.active['roi'] = self.drawing["roi"]
      if self.dids[self.active['type']].has_key(self.drawing["xref"]):
         print '... this xref already exists:',self.drawing["xref"]
         sys.exit()
      self.dids[self.active['type']].update({self.drawing["xref"]:self.drawing})
      return

   def addLayer(self,layer):
      # ~~> set default from the upper drawer
      layering = { "vars":self.drawing["vars"], "time":self.drawing["time"],
         "extract":self.drawing["extract"], "target":'',
         "title":'', "config":self.drawing["config"] }
      # ~~> reset from layer
      self.layering = getXMLKeys(layer,layering)
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
         print '... did not find the file to draw: ',src
         sys.exit() # this should not be a problem anymore
      return layers
# _____                     ________________________________________
# ____/ XML Parser Toolbox /_______________________________________/
#
"""
   Assumes that the directory ColourMaps is in PWD (i.e. ~root/pytel.)
"""
def runXML(xmlFile,xmlConfig):

   # ~~ Parse xmlFile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   import xml.etree.ElementTree as XML
   print '\n... reading XML test specification file: ' + path.basename(xmlFile)
   f = open(xmlFile,'r')
   xmlTree = XML.parse(f)  # may need to try first and report error
   xmlRoot = xmlTree.getroot()
   f.close()

   # ~~ Meta data process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   title = ""
   meta = META(title)
   print '\n... acquiring meta data'
   display = False

   # ~~ Action analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   do = ACTION(title)
   print '\n... looping through the todo list'
   for action in xmlRoot.findall("action"):

      # ~~ Step 1. Common check for keys and CAS file ~~~~~~~~~~~~~~
      casFile = do.addCAS(path.join(path.dirname(xmlFile),do.addAction(action)))

      # ~~ Step 2. Loop over configurations ~~~~~~~~~~~~~~~~~~~~~~~~
      for cfgname in xmlConfig.keys():
         cfg = xmlConfig[cfgname]['cfg']
         if not do.addCFG(cfgname,cfg): contine

         # ~~> Parse DICO File and default IO Files (only once)
         dicoFile,dico = getDICO(cfg,do.active["code"])
         do.updateCFG({'dico':dicoFile})
         cas = readCAS(scanCAS(casFile),dico['dico'],dico['frgb'])
         if do.active["ncsize"] != '': setKeyValue('PROCESSEURS PARALLELES',cas,dico['frgb'],int(do.active["ncsize"]))
         do.updateCFG({'cas':cas})
         if not checkConsistency(cas,dico['dico'],dico['frgb'],cfg): continue

         # ~~> Define config-split storage
         do.setSafe(dico)   # TODO: look at relative paths
         
         # ~~ Step 3. Complete all actions ~~~~~~~~~~~~~~~~~~~~~~~~~
         # options.do takes: translate;run;compile and none
         doable = xmlConfig[cfgname]['options'].do
         if doable == '': doable = do.active["do"]
         if doable == '': doable = do.available
         display = display or xmlConfig[cfgname]['options'].display

         # ~~> Action type A. Translate the CAS file
         if "translate" in doable.split(';'): do.translateCAS(dico,cfg['REBUILD'])

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
         if "compile" in doable.split(';'): do.compilePRINCI(dico,cfg,cfg['REBUILD'])

         # ~~> Action type E. Running CAS files
         if "run" in doable.split(';'): do.runCAS(xmlConfig[cfgname]['options'],cfg,cfg['REBUILD'])

   # ~~ Extraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # did has all the IO references and the latest sortie files

   # ~~ Gathering targets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   plot = PLOT(title)
   print '\n... gathering targets through the plot list'
   for typePlot in ["plot1d","plot2d","plot3d","plotpv"]:
      plot.addPlotType(typePlot)
      for ploting in xmlRoot.findall(typePlot):
         # ~~ Step 1. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~
         plot.addDraw(ploting)

         # ~~ Step 2. Cumul layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for layer in ploting.findall("layer"):
            target = plot.addLayer(layer)

            # ~~> round up targets and their configurations
            xref,src = target.split(':')
            if not do.dids.has_key(xref):
               print '... could not find reference to draw the action: ' + xref
               sys.exit()

            # ~~> store layer and its configuration(s)
            plot.targetLayer(plot.findLayerConfig(do.dids[xref],src))
         plot.update(plot.drawing)

   # ~~ Matrix distribution by plot types ~~~~~~~~~~~~~~~~~~~~~~~~~~
   # /!\ configurations cannot be called "together" or "distinct" or "oneofall"
   print '\n... plotting figures'
   for typePlot in plot.dids.keys():
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
               figureName = '.'.join([xref,draw['outFormat']])
            else:
               nbFig += 1
               figureName = '.'.join([xref,str(nbFig),draw['outFormat']])
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
                           'type': draw['type'], 'time':parseArrayPaires(layer["time"])[0] } )

                     # ~~ 2d plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     if typePlot == "plot2d":  # so far no plot type, but this may change
                        #print typePlot,' ... drawing'
                        figure.draw( layer['fileName'][cfg][2], { 'file': file,
                           'roi': parseArrayPaires(draw['roi']),
                           'vars': layer["vars"], 'extract':parseArrayPaires(layer["extract"]),
                           'type': draw['type'], 'time':parseArrayPaires(layer["time"])[0] } )


            figure.show()

   """
            # ~~> plot3d
               if plot["extract"] == '': plot["extract"] = "minmax"
   """
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
