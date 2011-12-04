"""@author David H. Roscoe and Sebastien E. Bourban
"""
"""@note ... this work is based on a collaboration effort between
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
# ~~> dependencies from within pytel/parsers
from parserKeywords import scanDICO, translateCAS, getKeyWord, getIOFilesSubmit
from parserSortie import getLatestSortieFiles
from parserStrings import parseArrayPaires
# ~~> dependencies towards the root of pytel
from runcode import runCAS, scanCAS, checkConsistency, compilePRINCI
# ~~> dependencies towards other pytel/modules
from utils.files import createDirectories,copyFile,moveFile, checkSafe,matchSafe
from mtlplots.plotTELEMAC import openFigure,closeFigure,drawFigure1D,drawFigure1DV,drawFigure2D

# _____                     ________________________________________
# ____/ XML Parser Toolbox /_______________________________________/
#

"""
   Will read keys XML Keys based on the template do.
   +: those with None are must have
   +: those without None are optional and reset if there
"""
def getXMLKeys(xml,do):

   for key in do.keys():
      if not key in xml.keys():
         if do[key] == None:
            print '... cannot find the key: ' + key
            sys.exit()
      else:
         do[key] = xml.attrib[key]

   return do

"""
   Assumes that the directory ColourMaps is in PWD (i.e. ~root/pytel.)
"""
def runXML(xmlFile,xmlConfig):

   # ~~ Parse xmlFile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   import xml.etree.ElementTree as XML
   print '... reading XML test specification file: ' + path.basename(xmlFile)
   f = open(xmlFile,'r')
   xmlTree = XML.parse(f)  # may need to try first and report error
   xmlRoot = xmlTree.getroot()
   f.close()

   # ~~ Meta data process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   title = ""

   # ~~ Action analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   didList = {}; dicos = {}
   print '... looping first through the todo list'
   for action in xmlRoot.iter("action"):
      did = {}

      # ~~ Step 1a. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~~~
      do = { "target": None, "code": None, "xref": None, "do": None,
             "title": title }
      do = getXMLKeys(action,do)
      # ~~> check existing module name
      for cfgname in xmlConfig.keys():
         if not do["code"] in xmlConfig[cfgname]['cfg']['MODULES'].keys():
            print '... do not know about:' + do["code"]
            sys.exit()

      # ~~ Step 1b. Common pre-processing of CAS file ~~~~~~~~~~~~~~
      casFile = path.join(path.dirname(xmlFile),do["target"])
      cas = scanCAS(casFile)

      # ~~ Step 2. Loop over configurations ~~~~~~~~~~~~~~~~~~~~~~~~
      for cfgname in xmlConfig.keys():
         cfg = xmlConfig[cfgname]['cfg']

         did.update({ cfgname:{ 'code':do['code'], 'cas':cas,
            'cmaps': path.join(cfg['PWD'],'ColourMaps') } })

         # options.process takes: translate;run;compile and none
         options = xmlConfig[cfgname]['options']
         if options.do == '': options.do = "translate;run;compile"
         # you need to build didList with references to files ... if "none" in options.do.split(';'): break #/!\ because none comes from command line

         # ~~> Parse DICO File and default IO Files (only once)
         dicoFile = path.join(path.join(cfg['MODULES'][do["code"]]['path'],'lib'),do["code"]+cfg['TELVER']+'.dico')
         if dicoFile not in dicos.keys():
            print '... reading DICO file: ' + dicoFile
            frgb,dico = scanDICO(dicoFile)
            iFS,oFS = getIOFilesSubmit(frgb,dico)
            dicos.update({dicoFile:{ 'frgb':frgb, 'dico':dico, 'input':iFS, 'output':oFS }})
         dico = dicos[dicoFile]
         did[cfgname].update({ 'dico':dicoFile })

         # ~~> Consistency checks
         if not checkConsistency(cas,dico['dico'],dico['frgb'],cfg):
            #print '... configuration ' + cfgname + ' inconsistent with CAS file: ',casFile
            continue
         #print '... configuration ' + cfgname + ' consistent with CAS file: ',casFile

         # ~~> Define config-split storage
         safe = path.join(path.join(path.dirname(xmlFile),do["xref"]),cfgname)
         createDirectories(safe)
         copyFile(casFile,safe)   # TODO: look at relative paths
         sacFile = path.join(safe,path.basename(casFile))
         did[cfgname].update({ 'path': safe  })

         # ~~ Step 3. Setting default options ~~~~~~~~~~~~~~~~~~~~~~
         specs = Values()
         setattr(specs,'configName',options.configName)
         setattr(specs,'configFile', options.configFile)
         setattr(specs,'sortieFile',True)
         setattr(specs,'rootDir', options.rootDir)
         setattr(specs,'version', options.version)
         setattr(specs,'tmpdirectory', True)
         setattr(specs,'compileonly', False)

         # ~~ Step 4. Pre-processing IO files ~~~~~~~~~~~~~~~~~~~~~~
         did[cfgname].update({'input':[]})
         did[cfgname].update({'output':[]})
         for k in cas.keys():
            if dico['input'].has_key(k):
               copyFile(path.join(path.dirname(casFile),cas[k][0]),safe)
               ifile = path.join(safe,cas[k][0])
               did[cfgname]['input'].append([k,[ifile],dico['input'][k]])
               #if not path.isfile(ifile):
               #   print '... file does not exist ',ifile
               #   sys.exit()
            if dico['output'].has_key(k):
               ofile = path.join(safe,cas[k][0])
               did[cfgname]['output'].append([k,[ofile],dico['output'][k]])

         # ~~ Step 5. Analysis of the CAS file ~~~~~~~~~~~~~~~~~~~~~
         # TODO:
         # - comparison with DEFAULT values of the DICTIONARY
         #if "cas" in options.do.split(';') and "cas" in do["do"].split(';'):

         # ~~ Step 6. Translate the CAS file ~~~~~~~~~~~~~~~~~~~~~~~
         if "translate" in options.do.split(';') and "translate" in do["do"].split(';'):
            if matchSafe(casFile,path.basename(casFile)+'.??',path.dirname(safe),cfg['REBUILD']):
               print '     +> translate cas file: ' + path.basename(sacFile)
               casfr,casgb = translateCAS(sacFile,dico['frgb'])  #/!\ removes comments at end of lines
               moveFile(casfr,path.dirname(safe))    # copied one level up
               moveFile(casgb,path.dirname(safe))    # copied one level up

         # ~~ Step 7. Analysis of the PRINCI file ~~~~~~~~~~~~~~~~~~
         #if "princi" in options.do.split(';') and "princi" in do["do"].split(';'):
         #   out = diffTextFiles( f,t )
         # TODO:
         # - comparison with standard source files

         # ~~ Step 8.- Compilation of PRINCI file ~~~~~~~~~~~~~~~~~~
         # Contrary to the other step, Step 8 is completed where the original CAS file is
         # (for no particularly good reason)
         value,default = getKeyWord('FICHIER FORTRAN',cas,dico['dico'],dico['frgb'])
         princiFile = ''
         if value != []:
            princiFile = path.join(path.dirname(casFile),value[0])
            if "compile" in options.do.split(';') and "compile" in do["do"].split(';'):
               if path.exists(princiFile):
                  if checkSafe(princiFile,safe,cfg['REBUILD']):
                     print '     +> compiling princi file: ' + path.basename(princiFile)
                     copyFile(princiFile,safe)
                     efile = path.join(path.dirname(casFile),compilePRINCI(princiFile,do["code"],cfgname,cfg))
                     if checkSafe(efile,safe,2): moveFile(efile,safe)
            #else: you may wish to retrieve the executable for later analysis

         # ~~ Step 9.- Running CAS files ~~~~~~~~~~~~~~~~~~~~~~~~~~~
         sortieFiles = getLatestSortieFiles(sacFile)
         if "run" in options.do.split(';') and "run" in  do["do"].split(';'):
            if matchSafe(casFile,path.basename(casFile)+'_*??h??min??s*.sortie',safe,0): #cfg['REBUILD']):
               print '     +> running cas file: ' + path.basename(casFile)
               for k in did[cfgname]['output']: matchSafe('',path.basename(k[1][0]),safe,2)
               specs.compileonly = False
               specs.sortieFile = True
               sortieFiles = runCAS(cfgname,cfg,do["code"],sacFile,specs)
         if sortieFiles != []: did[cfgname].update({ 'sortie': sortieFiles })

      # ~~> Store action completed
      didList.update({ do["xref"]: did })

   # ~~ Extraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # dicos is correct
   # did has all the IO references and the latest sortie files

   # ~~ Gathering targets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   drawList = {}
   print '... looping through the plot list'
   for typePlot in ["plot1d","plot2d","plot3d","plotpv"]:
      drawList.update({typePlot: [] })
      for plot in xmlRoot.iter(typePlot):
         did = {}
         # ~~ Step 1. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~
         draw = { "type":'', "support": '', "extract": '', "vars": '',
                 "title": title, "config": 'distinct',
                 'layers':[] }
         draw = getXMLKeys(plot,draw)

         # ~~ Step 2. Cumul layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for layer in plot.iter("layer"):
            # ~~> check keys
            do = { "vars": draw["vars"], # change of plan: "support": draw["support"],
                   "extract": draw["extract"], "target":'',
                   "title":'', 'outFormat': 'png', "config": draw["config"] }
            do = getXMLKeys(layer,do)
            # ~~> distribute support
            support = do["vars"].split(';')
            for i in range(len(support)):
               if ':' not in support[i]: support[i] = support[i] + ':' + draw["support"]
            do["vars"] = ';'.join(support)

            # ~~> round up targets and their configurations
            xref,src = do["target"].split(':')
            if not didList.has_key(xref):
               print '... could not find reference to draw: ' + xref
               sys.exit()
            if not did.has_key(xref): did.update({xref:{}})

            oneFound = False
            for cfg in didList[xref].keys():
               cfgFound = False
               if not did[xref].has_key('fileName'): did[xref].update({'fileName':{}})
               if didList[xref][cfg].has_key(src):
                  #if did[xref]['fileName'].has_key(cfg) :
                  #   print '... only one ID can be targeted per configuration: ' + do["target"]
                  #   sys.exit()
                  did[xref]['fileName'].update({ cfg:[didList[xref][cfg][src],'',src] })
                  do.update({ 'fileName': did[xref]['fileName'] })
                  cfgFound = True
               if not cfgFound and didList[xref][cfg].has_key('input'):
                  for i,j,k in didList[xref][cfg]['input']:
                     k = k.split(';')
                     if src in k[1]:               # filename, fileForm, fileType
                        did[xref]['fileName'].update({ cfg:[j,k[3],k[5]] })
                        do.update({ 'fileName': did[xref]['fileName'] })
                        cfgFound  = True
               if not cfgFound and didList[xref][cfg].has_key('output'):
                  for i,j,k in didList[xref][cfg]['output']:
                     k = k.split(';')
                     if src in k[1]:               # filename, fileForm, fileType
                        did[xref]['fileName'].update({ cfg:[j,k[3],k[5]] })
                        do.update({ 'fileName': did[xref]['fileName'] })
                        cfgFound = True
               oneFound = oneFound or cfgFound
               #if plot.has_key('colourmap'):
               #   cmapPlot = path.join(plot['cmaps'],subentry.attrib['colourmap'])
               #   if path.exists(cmapPlot): sortie.update({ 'cmapPlot': cmapPlot })
            if not oneFound:
               print '... did not find the file to draw: ' + xref
               sys.exit() # this should not be a problem anymore

            draw["layers"].append(do)
         drawList[typePlot].append(draw)

   # ~~ Matrix distribution by plot types ~~~~~~~~~~~~~~~~~~~~~~~~~~
   # /!\ configurations cannot be called "together" or "distinct" or "oneofall"
   for draw in drawList.keys():
      for plot in drawList[draw]:

         xlayers = ''   # now done with strings as arrays proved to be too challenging
         for layer in plot["layers"]:
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

         for cfglist in xlayers.split('|'):
            fig = openFigure(plot)    #/!\ fig represents here both plt and fig in fig = plt.figure()
            for layer,cfgs in zip(plot["layers"],cfglist.split(';')):
               for cfg in cfgs.split(':'):
                  for file in layer['fileName'][cfg][0]:

                     # ~~ 1d plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     if draw == "plot1d" and plot['type'] == "history":
                        drawFigure1D( layer['fileName'][cfg][2], { 'file': file,
                           'vars': layer["vars"], 'extract':parseArrayPaires(layer["extract"])
                           },fig )
                     if draw == "plot1d" and plot['type'] == "v-section":
                        drawFigure1DV( layer['fileName'][cfg][2], { 'file': file,
                           'vars': layer["vars"], 'extract':parseArrayPaires(layer["extract"])
                           },fig )

                     # ~~ 2d plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     if draw == "plot2d":  # so far no plot type, but this may change
                        drawFigure2D( layer['fileName'][cfg][2], { 'file': file,
                           'vars': layer["vars"] },fig )

            closeFigure(fig)

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
