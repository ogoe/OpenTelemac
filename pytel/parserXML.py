"""@brief
"""
"""@author David H. Roscoe and Sebastien E. Bourban
"""
"""@details

"""
"""@history 15/08/2011 -- Sebastien E. Bourban:
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#

import re
from os import path,chdir,system,remove
import numpy as np
from optparse import Values
from utils import getFileContent
from plots import setFigure1D,getFigure1D, setFigure2D,getFigure2D
from runcode import runCAS, scanCAS, processExecutable
from parserSortie import getValueProfileSortie
from parserSELAFIN import getValueProfilesSLF,getMeshElementSLF
from parserKeywords import scanDICO, translateCAS, getKeyWord, getSubmitWord, getIOFilesSubmit
import sys

# _____                          ___________________________________
# ____/ General Parsing Toolbox /__________________________________/
#    
sqr_brack = re.compile(r'[,;]?\s*?\[(?P<brack>[\d;.\s+-dDeE]*?)\]',re.I)
var_doublep = re.compile(r'(?P<number>\b(|[^a-zA-Z(,])(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+)(\b|[^a-zA-Z,)])))',re.I)
var_integer = re.compile(r'(?P<number>\b(|[^a-zA-Z(,])(?:(\d+)(\b|[^a-zA-Z,)])))',re.I)
def parseArray(s):

   z = []  # /!\ only pairs of points allowed for now
   for brack in re.findall(sqr_brack,s):
      p = []
      for v in brack.split(';'):
         proci = re.match(var_integer,v)
         procd = re.match(var_doublep,v)
         if procd:
            p.append(float(procd.group('number')))
         elif proci:
            p.append(float(proci.group('number')))
         else:
            print '... could not parse the array: ' + s
            sys.exit()
      z.append(p)
      
   return np.array(z)
# _____                                       ______________________
# ____/ General Plotting Preparation Toolbox /_____________________/
#    
def doPlot1D(xmlPlot,plot,did):
   """
   TODO: Allow multiple-variables
   TODO: Differentiate 2D from 3D SELAFINs
   """
   figout = setFigure1D(plot)
   dos = []
   
   for layer in xmlPlot.iter("layer"):

      # ~~> check keys
      do = { "vars": None, "support": plot["support"], "source": plot["source"],
                   "extract": plot["extract"], "title": plot["title"] }
      do = getXMLKeys(layer,do)   # /!\ do not forget about plot['type']

      # ~~> round up sources
      for source in do["source"].split(';'):
         xref,src = source.split(':')
         if not did.has_key(xref):
            print '... could not find reference to draw: ' + xref
            sys.exit()

         # sortie file
         if src == "sortie":
            if not did[xref].has_key('sortie'):
               print '... did not find the sortie file to draw: ' + xref
               continue  # this is not a problem, just move on
            do.update({
               'fileName': did[xref]['sortie'],
               'fileForm': src,
               'outFormat': 'png',
               'plotFunction': getValueProfileSortie,
               'plotArguments': [ did[xref]['sortie'],do["support"],do["vars"] ] })
            dos.append(do)

         # TELEMAC DICO defined file
         else:
            fileName = ''
            for i,j,k in did[xref]['input']:
               k = k.split(';')
               if src in k[1]:
                  fileName = j; fileForm = k[3]; fileType = k[5]
            for i,j,k in did[xref]['output']:
               k = k.split(';')
               if src in k[1]:
                  fileName = j; fileForm = k[3]; fileType = k[5]
            if fileName != '':
               do.update({
                  'fileName': fileName, 'fileForm': fileForm, 'fileType': fileType,
                  'support': do["support"], 'extract': parseArray(do["extract"]), 'vars': do["vars"],
                  'typePlot': plot['type'],
                  'outFormat': 'png',
                  'plotFunction': getValueProfilesSLF,
                  'plotArguments': [ fileName,parseArray(do["extract"]),do["vars"] ] })
                        #if plot.has_key('colourmap'):
               #   cmapPlot = path.join(plot['cmaps'],subentry.attrib['colourmap'])
               #   if path.exists(cmapPlot): sortie.update({ 'cmapPlot': cmapPlot })
               dos.append(do)

   return getFigure1D(dos,figout)

def doPlot2D(xmlPlot,plot,did):
   #figout = setFigure2D(plot)
   dos = []
   
   for layer in xmlPlot.iter("layer"):
      
      # ~~> check keys
      do = { "vars": None, "support": plot["support"], "source": plot["source"],
                   "extract": plot["extract"], "title": plot["title"] }
      do = getXMLKeys(layer,do)

      # ~~> only one source available (you should use the layers tag to add other displays)
      xref,src = do["source"].split(':')
      if not did.has_key(xref):
         print '... could not find reference to draw: ' + xref
         sys.exit()

      # TELEMAC DICO defined file
      fileName = ''
      for i,j,k in did[xref]['input']:
         k = k.split(';')
         if src in k[1]:
            fileName = j; fileForm = k[3]; fileType = k[5]
      for i,j,k in did[xref]['output']:
         k = k.split(';')
         if src in k[1]:
            fileName = j; fileForm = k[3]; fileType = k[5]
      if fileName != '':
         do.update({
            'fileName': fileName, 'fileForm': fileForm, 'fileType': fileType,
            'support': do["support"], 'vars': do["vars"],
            'outFormat': 'png',
            'plotFunction': getMeshElementSLF,
            'plotArguments': fileName })
            #if subentry.attrib.has_key('colourmap'):
            #         cmapPlot = path.join(x['cmaps'],subentry.attrib['colourmap'])
            #         if path.exists(cmapPlot): geo.update({ 'cmapPlot': cmapPlot })
         dos.append(do)

   return getFigure2D(dos)
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
def runXML(cfgName,cfg,module,xmlFile,options):
   didList = {}      # clear each time
   import xml.etree.ElementTree as XML
   # options.process takes: translate;run;compile and none
   if options.do == '': options.do = "run" #"translate;run;compile"
   # options.draw takes: plot1;plot2d;plot3d;plotpv and none
   if options.draw == '': options.draw = "plot1d"
   # meta data process
   title = ""

   # ~~ Parse xmlFile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '... reading XML test specification file: ' + path.basename(xmlFile)
   f = open(xmlFile,'r')
   xmlTree = XML.parse(f)  # may need to try first and report error
   xmlRoot = xmlTree.getroot()
   f.close()
   
   # ~~ Step 1. Individual analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '... individual analysis'
   if "none" not in options.do.split(';'):
      didList.update({"action":{}})
      for action in xmlRoot.iter("action"):
         did = {}

         # ~~ Step 1a. Check keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         
         do = { "cas": None, "code": None, "xref": None, "do": None,
                "title": title }
         do = getXMLKeys(action,do)
         # ~~> module name
         if not do["code"] in cfg['MODULES'].keys():
            print '... do not know about:' + do["code"]
            sys.exit()
         did.update({'code':do['code']})

         # ~~ Step 1b. Pre-processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

         # ~~> CAS file
         casFile = path.join(path.dirname(xmlFile),do["cas"])
         cas = scanCAS(casFile)
         frgb,dico = scanDICO(path.join(path.join(cfg['MODULES'][do["code"]]['path'],'lib'),do["code"]+cfg['TELVER']+'.dico'))
         did.update({ 'cas':cas,'dico':dico, 'frgb':frgb, 'path': path.dirname(casFile) })
         # ~~> options
         specs = Values()
         setattr(specs,'configName',options.configName)
         setattr(specs,'configFile', options.configFile)
         setattr(specs,'sortieFile',True)
         setattr(specs,'rootDir', options.rootDir)
         setattr(specs,'version', options.version)
         setattr(specs,'tmpdirectory', True)
         setattr(specs,'compileonly', False)
         # ~~> file IOs
         iFS,oFS = getIOFilesSubmit(frgb,dico)
         did.update({'input':[]})
         did.update({'output':[]})
         for k in cas.keys():
            if iFS.has_key(k):
               ifile = path.join(path.dirname(casFile),cas[k][0])
               did['input'].append([k,ifile,iFS[k]])
               #if not path.isfile(ifile):
               #   print '... file does not exist ',ifile
               #   sys.exit()
            if oFS.has_key(k):
               ofile = path.join(path.dirname(casFile),cas[k][0])
               did['output'].append([k,ofile,oFS[k]])
         # ~~ Step 1c. Processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

         # ~~> Analysis CAS files
         #if "cas" in options.do.split(';') and "cas" in do["do"].split(';'):

         # ~~> Translate CAS files
         if "translate" in options.do.split(';') and "translate" in do["do"].split(';'):
            print '     +> translate cas file: ' + path.basename(casFile)
            translateCAS(casFile,frgb)  #/!\ removes comments at end of lines

         # ~~> Analysis PRINCI files
         #if "princi" in options.do.split(';') and "princi" in do["do"].split(';'):

         # ~~> Compilation PRINCI files
         if "compile" in options.do.split(';') and "compile" in do["do"].split(';'):
            value,default = getKeyWord('FICHIER FORTRAN',cas,dico,frgb)
            if value != []:
               princiFile = path.join(path.dirname(casFile),value[0])
               print '     +> compiling princi file: ' + path.basename(princiFile)
               if path.exists(princiFile):
                  objFile = path.join(path.join(cfg['MODULES'][do["code"]]['path'],cfgName),module+cfg['TELVER']+'.cmdo')
                  exeFile = path.join(path.join(cfg['MODULES'][do["code"]]['path'],cfgName),module+cfg['TELVER']+'.cmdx')
                  if not path.exists(objFile) or not path.exists(exeFile):
                     print '... could not find:' + exeFile
                     print '~~~> you may need to compile your system with the configuration: ' + cfgName
                     sys.exit()
                  objCmd = getFileContent(objFile)[0]
                  exeCmd = getFileContent(exeFile)[0]
                  chdir(path.dirname(princiFile))
                  princiFile = path.basename(princiFile)
                  objFile = path.splitext(princiFile)[0] + cfg['SYSTEM']['SFX_OBJ']
                  exeFile = path.splitext(princiFile)[0] + cfg['SYSTEM']['SFX_EXE']
                  if path.exists(exeFile): remove(exeFile)
                  if not processExecutable(exeFile,objFile,princiFile,objCmd,exeCmd,''):
                     print '... could not compile:' + princiFile
                     sys.exit()

         # ~~> Running CAS files
         if "run" in options.do.split(';') and "run" in  do["do"].split(';'):
            print '     +> running cas file: ' + path.basename(casFile)
            specs.compileonly = False
            #sortieFile = runCAS(cfgName,cfg,do["code"],casFile,specs)
            sortieFile = 'C:\home\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\t2d_bumpflu_v1p0.cas_2011-10-05-09h10min47s.sortie'
            specs.sortieFile = True
            if sortieFile != None: did.update({ 'sortie': sortieFile })

         # ~~> Store action completed
         did.update({ 'cmaps': path.join(cfg['PWD'],'ColourMaps') })
         didList["action"].update({ do["xref"]: did })
   
   # ~~ Step 2. Draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for typePlot in options.draw.split(';'):
      print '... individual drawings: ' + typePlot
      didList.update({typePlot:[]})
      for xmlPlot in xmlRoot.iter(typePlot):
         did = {}

         # ~~ Step 2a. Check keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         plot = { "type": '', "support": '', "extract": '', "source": '', "title": title }
         plot = getXMLKeys(xmlPlot,plot)

         # ~~ Step 2b. Plot distribution ~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for source in plot["source"].split(';'):
            plot["source"] = source

            # ~~> plot1d
            if typePlot == "plot1d":
               if plot["type"] == '': plot["type"] = "history"
               doPlot1D(xmlPlot,plot,didList["action"])

            # ~~> plot2d
            if typePlot == "plot2d":
               doPlot2D(xmlPlot,plot,didList["action"])

            # ~~> plot3d
            if typePlot == "plot3d":
               if plot["extract"] == '': plot["extract"] = "minmax"
               doPlot3D(xmlPlot,plot,didList["action"])

            # ~~> plot2d
            if typePlot == "plotpv":
               doPlotPV(xmlPlot,plot,didList["action"])

         #for layer in draw.iter("layer"):
            
         #   do = { "vars": None, "support": None, "source": None,
         #          "extract: '', ""title": valTitle }
         #   do = getXMLKeys(layer,do)

            # ~~> draw sortie variable
         #   doPlot1d(draw,did_xref)

            # Plot of GEO files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         #   if drawType == "plot2d" and "plot2d" in options.draw.split(';'):
         #      doPlot2dDraw(entry,action[processXref])

         """for subentry in entry.findall("do"):
                  # mesh plot2d
                  if "mesh" in subentry.attrib['action'].split(';'):
                     for xref in subentry.attrib['xref'].split(';'):
                        for x in action[processXref]:
                           if xref == x['xref']:
                              value,default = getKeyWord('FICHIER DE GEOMETRIE',cas,dico,frgb)
                              if value != []:
                                 mesh = {
                                    'fileName': path.join(path.dirname(casFile),value[0]),
                                    'typePlot': "mesh",
                                    'outFormat': entry.attrib['format'] }
                                 print '     +> plotting mesh of GEO file: ' + path.basename(mesh['fileName'])
                                 figout = plot2dMeshSLF(mesh)

                  # bathy contour plot2d
                  if "bathy" in subentry.attrib['action'].split(';'):
                     for xref in subentry.attrib['xref'].split(';'):
                        for x in action[processXref]:
                           if xref == x['xref']:
                              value,default = getKeyWord('FICHIER DE GEOMETRIE',cas,dico,frgb)
                              if value != []:
                                 geo = {
                                    'fileName': path.join(path.dirname(casFile),value[0]),
                                    'typePlot': "bathy",
                                    'varsPlot':["BOTTOM","FOND"],
                                    'outFormat': entry.attrib['format'] }
                                 if subentry.attrib.has_key('colourmap'):
                                    cmapPlot = path.join(path.join(cfg['PWD'],'ColourMaps'),subentry.attrib['colourmap'])
                                    if path.exists(cmapPlot): geo.update({ 'cmapPlot': cmapPlot })
                                 print '     +> plotting BOTTOM of GEO file: ' + path.basename(geo['fileName'])
                                 figout = plot2dMeshSLF(geo)"""

         # Plot of REF files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      #actionList["process"].update(action)

   # ~~ Step 2. Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~ Step 3. Plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '... comparative analysis'
   #processes = xmlRoot.findall("twoses")
   #if processes != None:
   #   actionList.update({"twoses":{}})
   #   action = {}
   #   for process in processes:
   #      processXref = process.attrib["xref"]
   #      action.update({ processXref:[] })
   #      for entry in process.findall("do"):

   #         # Pre-process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #         codeNames = entry.attrib["xref"].split(';')
            
   #         print codeNames
   sys.exit()

   # ~~ Remove XML dependencies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #for entry in entryList:
   #   actionList.append({'name'   :entry.attrib["name"],
   #      'cas'    :entry.attrib["cas"],
#         "exe"    :entry.attrib["exe"],
#         "folder" :entry.attrib["folder"],
#         "CASfile"    :entry.attrib["CASfile"],
#         "REFfile"    :entry.attrib["REFfile"],
#         "out"    :entry.attrib["out"]
   #      })

   print actionList
   return actionList

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$2-Aug-2011 11:51:36$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    testlist = gather_tests('test_runCAS.txt','TestCase')
    testlist2 = gather_tests('test_runCAS.txt','Analysis')
    # Go through each of the exe's
    for i,CurrentTEST in enumerate(testlist):

        # Retrive stem name of the current exe
        TestName        = CurrentTEST["name"]
        CurrentTESTexe  = CurrentTEST["exe"]
        CurrentTESTfolder = CurrentTEST["folder"]
        CurrentTESTcasfile = CurrentTEST["CASfile"]
        CurrentTESTreffile = CurrentTEST["REFfile"]
        CurrentTESTout = CurrentTEST["out"]

        CurrentTESTcmd  = CurrentTEST["exe"] + " " + CurrentTEST["args"] + " " + CurrentTEST["CASfile"]

        # Change directory to validation test folder
        TestFolder = 'C:\\opentelemac\\trunk\\telemac2d\\tel2d_v6p1\\validation\\' + CurrentTESTfolder
        if not path.exists(TestFolder):
            print 'FAILED: test area folder cannot be found: %s'%(TestFolder)
        else:
            chdir( TestFolder )

        print "\n\n---------------------------------------------------------------\n\
               ---------------------------------------------------------------\n\
               ----------------- RUNNING VALIDATION TEST %d ------------------\n\
               ---------------------------------------------------------------\n\
               ---------------------------------------------------------------\n\n" %(i+1)

        system(CurrentTESTcmd)

        print "\n\n---------------------------------------------------------------\n\
               ---------------------------------------------------------------\n\
               ---------------- VALIDATION TEST %d COMPLETE ------------------\n\
               ---------------------------------------------------------------\n\
               ---------------------------------------------------------------\n\n" %(i+1)







"""
        for j,CurrentPLOT in enumerate(testlist2):
            PlotName = CurrentPLOT["name"]
            CurrentPLOTexe  = CurrentPLOT["exe"]
            CurrentPLOTargs  = CurrentPLOT["args"]
            CurrentPLOTfolder = CurrentPLOT["folder"]
            CurrentPLOTcasfile = CurrentPLOT["CASfile"]
            CurrentPLOTreffile = CurrentPLOT["REFfile"]
            CurrentPLOTout = CurrentPLOT["out"]

            CurrentPLOTcmd = CurrentPLOTexe + " " + CurrentTEST["out"] + " " + CurrentPLOTargs

            # Change directory to validation test folder
            PlotFolder = 'Z:\\model\\TMP-Python\\Contour\\src'
            if not os.path.exists(PlotFolder):
                print 'FAILED: test area folder cannot be found: %s'%(PlotFolder)
            else:
                os.chdir( PlotFolder )

            os.system(CurrentPLOTcmd)
"""
