"""@brief
"""
"""@author David H. Roscoe and Sebastien E. Bourban
"""
"""@details

"""
"""@history 15/08/2011 -- Sebastien Bourban:
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#

from os import path,chdir,system,remove
from optparse import Values
from utils import getFileContent
from plots import plotTimeSeriesSortie,plot2dMeshSLF
from runcode import runCAS, scanCAS, processExecutable
from parserKeywords import scanDICO, translateCAS, getKeyWord, getIOFilesSubmit
import sys

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#
def doSortieDraw(entry,did):

   for subentry in entry.findall("do"):
      for xref in subentry.attrib['xref'].split(';'):
         for x in did:
            if x.has_key('xref'):
               #if not x.has_key('sortie'):
               #   print '... could not find sortie file for: ' + x
               #   sys.exit()
               if x['sortie'] == None:
                  print '... could not find sortie file for: ' + xref
                  break #sys.exit()
               sortie = { 'fileName': x['sortie'], 'typePlot': subentry.attrib["action"], 'outFormat': entry.attrib['format'] }
               figout = plotTimeSeriesSortie(sortie)

   return figout

def doPlot2dDraw(entry,did):

   for subentry in entry.findall("do"):
      for xref in subentry.attrib['xref'].split(';'):
         for x in did:
            if xref == x['xref']:
               value,default = getKeyWord('FICHIER DE GEOMETRIE',x['cas'],x['dico'],x['frgb'])
               if value != []:
                  geo = {
                     'fileName': path.join(x['path'],value[0]),
                     'typePlot': subentry.attrib['action'],
                     'varsPlot':["BOTTOM","FOND"],
                     'outFormat': entry.attrib['format'] }
                  if vars != []:
                     geo.update({ 'varsPlot': vars })
                     if subentry.attrib.has_key('colourmap'):
                        cmapPlot = path.join(x['cmaps'],subentry.attrib['colourmap'])
                        if path.exists(cmapPlot): geo.update({ 'cmapPlot': cmapPlot })
                  print '     +> plotting from the GEO file: ' + path.basename(geo['fileName'])
                  figout = plot2dMeshSLF(geo)

   return
# _____                     ________________________________________
# ____/ XML Parser Toolbox /_______________________________________/
#
"""
   Assumes that the directory ColourMaps is in PWD (i.e. ~root/pytel.)
"""
def runXML(cfgName,cfg,module,xmlFile,options):
   actionList = {}      # clear each time
   import xml.etree.ElementTree as XML
   # options.process takes: translate;run;compile and none
   if options.process == '': options.process = "run"
   # options.draw takes: sortie;plot2d;plot3d and none
   if options.draw == '': options.draw = "plot2d"

   # ~~ Parse xmlFile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '... reading XML test specification file: ' + path.basename(xmlFile)
   f = open(xmlFile,'r')
   xmlTree = XML.parse(f)
   xmlRoot = xmlTree.getroot()
   f.close()
   
   # ~~ Step 1. Individual analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '... individual analysis'
   processes = xmlRoot.findall("process")
   if processes != None:
      actionList.update({"process":{}})
      action = {}
      for process in processes:
         processXref = process.attrib["xref"]
         action.update({ processXref:[] })

   # ~~ Step 1a. dos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for entry in process.findall("do"):
            if "none" in options.process.split(';'): break

            # Pre-process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            codeName = entry.attrib["code"]
            if not codeName in cfg['MODULES'].keys():
               print '... do not know about:' + codeName
               sys.exit()
            casFile = path.join(path.dirname(xmlFile),entry.attrib["cas"])
            cas = scanCAS(casFile)
            frgb,dico = scanDICO(path.join(path.join(cfg['MODULES'][codeName]['path'],'lib'),codeName+cfg['TELVER']+'.dico'))
            do = { 'path': path.dirname(casFile), 'cas': cas, 'title': entry.attrib["title"], 'code': codeName, 'xref': entry.attrib["xref"], 'cmaps': path.join(cfg['PWD'],'ColourMaps'), 'dico':dico, 'frgb':frgb }
            # Options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            specs = Values()
            setattr(specs,'configName',options.configName)
            setattr(specs,'configFile', options.configFile)
            setattr(specs,'sortieFile',True)
            setattr(specs,'rootDir', options.rootDir)
            setattr(specs,'version', options.version)
            setattr(specs,'tmpdirectory', True)
            setattr(specs,'compileonly', False)
            # File IOs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            iFS,oFS = getIOFilesSubmit(frgb,dico)
            do.update({'input':[]})
            do.update({'output':[]})
            for k in cas.keys():
               if iFS.has_key(k):
                  ifile = path.join(path.dirname(casFile),cas[k][0])
                  do['input'].append([k,ifile,iFS[k]])
                  #if not path.isfile(ifile):
                  #   print '... file does not exist ',ifile
                  #   sys.exit()
               if oFS.has_key(k):
                  ofile = path.join(path.dirname(casFile),cas[k][0])
                  do['output'].append([k,ofile,oFS[k]])

            # Analysis CAS files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            # Tranlate CAS files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if "translate" in options.process.split(';') and "translate" in entry.attrib['action']:
               print '     +> tranlate cas file: ' + path.basename(casFile)
               translateCAS(casFile,frgb)  #/!\ removes comments at end of lines
               # no need to update do

            # Analysis PRINCI files ~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Compilation PRINCI files ~~~~~~~~~~~~~~~~~~~~~~~
            if "compile" in options.process.split(';') and "compile" in entry.attrib['action']:
               value,default = getKeyWord('FICHIER FORTRAN',cas,dico,frgb)
               if value != []:
                  princiFile = path.join(path.dirname(casFile),value[0])
                  print '     +> compiling princi file: ' + path.basename(princiFile)
                  if path.exists(princiFile):
                     objFile = path.join(path.join(cfg['MODULES'][codeName]['path'],cfgName),module+cfg['TELVER']+'.cmdo')
                     exeFile = path.join(path.join(cfg['MODULES'][codeName]['path'],cfgName),module+cfg['TELVER']+'.cmdx')
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
               # no need to update do

            # Running CAS files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if "run" in options.process.split(';') and "run" in entry.attrib['action']:
               print '     +> running cas file: ' + path.basename(casFile)
               specs.compileonly = False
               specs.sortieFile = True
               sortieFile = '' #runCAS(cfgName,cfg,codeName,casFile,specs)
               do.update({ 'sortie': sortieFile })

            # Store actions completed ~~~~~~~~~~~~~~~~~~~~~~~~
            action[processXref].append(do)

   # ~~ Step 1b. draws ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for entry in process.findall("draw"):
            # Pre-process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            drawType = entry.attrib["type"]

            # Draw the sortie file ~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if drawType == "sortie" and "sortie" in options.draw.split(';'):
               doSortieDraw(entry,action[processXref])

            # Plot of GEO files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if drawType == "plot2d" and "plot2d" in options.draw.split(';'):
               doPlot2dDraw(entry,action[processXref])

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

      sys.exit()
      actionList["process"].update(action)

   # ~~ Step 2. Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~ Step 3. Plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '... comparative analysis'
   processes = xmlRoot.findall("twoses")
   if processes != None:
      actionList.update({"twoses":{}})
      action = {}
      for process in processes:
         processXref = process.attrib["xref"]
         action.update({ processXref:[] })
         for entry in process.findall("do"):

            # Pre-process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            codeNames = entry.attrib["xref"].split(';')
            
            print codeNames
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
