"""@brief
"""
"""@author David H. Roscoe and Sebastien E. Bourban
"""
"""@history 30/04/2011 -- Sebastien Bourban: 
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#

from os import path
import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm                                  # used for colour maps
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.collections as collections                # used for collections
from utils import getFileContent
from parserSortie import getTimeProfile,getNameOfStudy,getVolumeProfile
from parserSELAFIN import getHeaderSLF,getCoreValueSLF

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#
def getColourMap(fileName):

   pointList = []
   red = []; green = []; blue = []
   f = open(fileName,'r')

   # just in case of file access, parser problems:
   try:
      import xml.etree.ElementTree as XML
      xmlTree = XML.parse(f)
      xmlRoot = xmlTree.getroot()
      f.close()
   except:
      print "... Could not access/read expected colour map file content: " + fileName
      f.close()
      sys.exit()

   for entry in xmlRoot.findall("Point"):
      red.append((float(entry.attrib["x"]),float(entry.attrib["r"]),float(entry.attrib["r"])))
      blue.append((float(entry.attrib["x"]),float(entry.attrib["b"]),float(entry.attrib["b"])))
      green.append((float(entry.attrib["x"]),float(entry.attrib["g"]),float(entry.attrib["g"])))

   return { 'blue': blue, 'red': red, 'green': green }
# _____                   __________________________________________
# ____/ Plotting Toolbox /_________________________________________/
#

def plotTimeSeriesSortie(sortie):

   # ~~ Extract data
   content = getFileContent(sortie['fileName'])
   title = getNameOfStudy(content)
   i,x = getTimeProfile(content)
   xname = 'Time (s)'
   if sortie['typePlot'] == "mass":
      y,i,i = getVolumeProfile(content)
      yname = 'Volume (m3)'
      figout = '.'.join([path.splitext(sortie['fileName'])[0],sortie['typePlot'],sortie['outFormat']])

   # ~~ Plot data
   plt.figure()
   plt.subplot(111)
   plt.title(title)
   plt.xlabel(xname)
   plt.ylabel(yname)
   plt.plot(x[1:],y)
   plt.savefig(figout)

   return figout

def plot2dMeshSLF(geometry):

   f = open(geometry['fileName'],'rb')

   # ~~ Extract header data
   title,numbers,vars,mesh = getHeaderSLF(f)
   NELEM3,NPOIN3,NDP,NPLAN = numbers
   IKLE,IPOBO,MESHX,MESHY = mesh

   xmin = xmax = MESHX[0]
   ymin = ymax = MESHY[0]
   for m in MESHX[1:]:
      xmin = min(m,xmin)
      xmax = max(m,xmax)
   for m in MESHY[1:]:
      ymin = min(m,ymin)
      ymax = max(m,ymax)

   elements = []
   for e in IKLE:
      element = []
      for n in e: element.append((MESHX[n-1],MESHY[n-1]))
      elements.append(element)

   # ~~ Extract variable data
   ivar = 0
   if geometry['typePlot'] == "bathy":
      NBV1,NBV2,VARNAMES,VARUNITS = vars
      for i in range(NBV1+NBV2):
         if VARNAMES[i].strip() in geometry['varsPlot']:
            ivar = i  # /!\ You only choose one variable
      VARNOD = getCoreValueSLF(f,ivar,NPOIN3)
      VARELE = []
      for e in IKLE:
         ze = 0.
         for n in e: ze = ze + VARNOD[n-1]
         VARELE.append( ze / ( float(len(e))+1. ) )
      zmin = VARELE[0]; zmax = VARELE[0]
      for z in VARELE[1:]:
         zmin = min(z,zmin)
         zmax = max(z,zmax)

   # ~~ Prepare for plot
   if geometry['typePlot'] == "mesh":
      colection = collections.PolyCollection(
         elements, cmap=cm.jet,
         edgecolors = 'face', facecolors = 'none')
      val = np.array([0])
      figout = '.'.join([path.splitext(geometry['fileName'])[0],geometry['typePlot'],geometry['outFormat']])
   if geometry['typePlot'] == "bathy":
      colormap = cm.jet
      if geometry.has_key('cmapPlot'):
         colormap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
      colection = collections.PolyCollection(
         elements, cmap=colormap, norm = plt.Normalize(zmin,zmax),
         linewidth=0, antialiaseds=0)
      val = np.array(VARELE)
      figout = '.'.join([path.splitext(geometry['fileName'])[0],geometry['typePlot'],geometry['outFormat']])

   colection.set_array(val)       # each triangle colour dependent on its value from its verticies

   # ~~ Plot data
   fig = plt.figure()       # initialises the figure
   mp = fig.gca()           # not sure what this does, but seems needed, think it initialises axis on plot
   mp.add_collection(colection)   # adds, or plots our collection
   mp.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
   mp.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
   mp.axis('equal')         # sets both axis scale to be equal
   #mp.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title
   if geometry.has_key('cmapPlot'): fig.colorbar(colection)     # sets up colourbar
   #plt.show()
   plt.savefig(figout)      # saves output plot to .png file

   f.close()
   return figout

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   sys.exit()
