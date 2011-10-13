"""@brief
"""
"""@author David H. Roscoe and Sebastien E. Bourban
"""
"""@history 30/08/2011 -- Sebastien E. Bourban: 
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

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#
def setFigure1D(plot):

   plt.figure()
   plt.subplot(111)
   plt.title(plot["title"])

   return plt

"""

   ~ plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
     could be used to restrict the view to a user-defined zoom
"""
def getFigure1D(dos,plt):
# TODO: use the names to label the curves
# TODO: save the plot in a file name.png
   for do in dos:

      xout,yout = do['plotFunction']( do['plotArguments'] )
      xname,x0 = xout
      plt.xlabel(xname)
      dim = len(yout) - 1
      if dim == 1:
         n0,y0 = yout
         plt.ylabel(n0)
         plt.plot(x0,y0)
         plt.grid(True)
         #plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
      elif dim == 2:
         n0,n1,y0 = yout
         for i in range(len(y0)):
            #plt.ylabel(n0[i])
            plt.plot(x0,y0[i])
         plt.grid(True)
      elif dim == 3:
         n0,n1,n2,y0 = yout
         for i in range(len(y0)):
            for j in range(len(y0[i])):
               #plt.ylabel(n0[i])
               plt.plot(x0,y0[i][j])
         plt.grid(True)

   plt.show()
   #plt.savefig(fig)

   return

def setFigure2D(plot):

   #plt.figure()
   
   return plt

def getFigure2D(dos):
# TODO: use the names to label the curves
# TODO: save the plot in a file name.png
   fig = plt.figure()       # initialises the figure
   for do in dos:

      minmax,eout = do['plotFunction']( do['plotArguments'] )
      xmin,xmax,ymin,ymax = minmax
      # Collections
      colection = collections.PolyCollection(
         eout, cmap=cm.jet, antialiaseds=0, # norm=plt.Normalize()
         edgecolors = 'k', linewidth=1, facecolors = 'none')
      #colection.set_array(val)       # each triangle colour dependent on its value from its verticies
      # ~~ Plot data
      #ex: fig = plt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
      mp = fig.gca()           # not sure what this does, but seems needed, think it initialises axis on plot
      mp.add_collection(colection)   # adds, or plots our collection
      mp.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
      mp.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
      plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
      mp.axis('equal')         # sets both axis scale to be equal
      #mp.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title

   plt.show()
   #figout = '.'.join([path.splitext(geometry['fileName'])[0],"mesh",geometry['outFormat']])
   #plt.savefig(figout)      # saves output plot to .png file

   return

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

   figout = []
   # ~~ Extract data
   content = getFileContent(sortie['fileName'])
   title = getNameOfStudy(content)
   i,x = getTimeProfile(content)
   y1,y2,y3 = getValueProfile(content)

   # ~~ Plot Volumes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if "voltotal" in sortie['varsPlot'].split(';'):
      fig = '.'.join([path.splitext(sortie['fileName'])[0],sortie['varsPlot'],sortie['outFormat']])
      figout.append(fig)
      plt.figure()
      plt.subplot(111)
      plt.title(title)
      plt.xlabel(x['name'])
      plt.ylabel(y1['name'])
      plt.plot(x['profile'],y1['profile'])
      plt.show()
      plt.savefig(fig)

   # ~~ Plot Fluxes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if "volfluxes" in sortie['varsPlot'].split(';'):
      fig = '.'.join([path.splitext(sortie['fileName'])[0],sortie['varsPlot'],sortie['outFormat']])
      figout.append(fig)
      plt.figure()
      plt.subplot(111)
      plt.title(title)
      plt.xlabel(x['name'])
      for i in range(len(y2)):
         plt.ylabel(y2[i]['name']+str(i))
         plt.plot(x['profile'][1:],y2[i]['profile'])
      plt.show()
      plt.savefig(fig)

   # ~~ Plot Errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if "volerror" in sortie['varsPlot'].split(';'):
      fig = '.'.join([path.splitext(sortie['fileName'])[0],sortie['varsPlot'],sortie['outFormat']])
      figout.append(fig)
      plt.figure()
      plt.subplot(111)
      plt.title(title)
      plt.xlabel(x['name'])
      plt.ylabel(y3['name'])
      plt.plot(x['profile'][1:],y3['profile'])
      plt.show()
      plt.savefig(fig)

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
   # ~~ Plot the mesh ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if "mesh" in geometry['typePlot'].split(';'):
      # Extract mesh connectivity
      elements = []
      for e in IKLE:
         element = []
         for n in e: element.append((MESHX[n-1],MESHY[n-1]))
         elements.append(element)
      # Collections
      colection = collections.PolyCollection(
         elements, cmap=cm.jet,
         edgecolors = 'face', facecolors = 'none')
      #colection.set_array(val)       # each triangle colour dependent on its value from its verticies
      # ~~ Plot data
      fig = plt.figure()       # initialises the figure
      #ex: fig = plt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
      mp = fig.gca()           # not sure what this does, but seems needed, think it initialises axis on plot
      mp.add_collection(colection)   # adds, or plots our collection
      mp.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
      mp.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
      mp.axis('equal')         # sets both axis scale to be equal
      #mp.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title
      plt.show()
      figout = '.'.join([path.splitext(geometry['fileName'])[0],"mesh",geometry['outFormat']])
      plt.savefig(figout)      # saves output plot to .png file

   # ~~ Plot the content of GEO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if "bathy" in geometry['typePlot'].split(';'):
      # Extract variable data
      NBV1,NBV2,VARNAMES,VARUNITS = vars
      for i in range(NBV1+NBV2):
         if geometry.has_key('varsPlot'):
            if not VARNAMES[i].strip() in geometry['varsPlot']: continue
         VARSOR = getCoreValueSLF(f,i,NPOIN3)
         # ~~ Plot data
         colourmap = cm.jet
         if geometry.has_key('cmapPlot'):
            colourmap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
         zmin = VARSOR[0]; zmax = VARSOR[0]
         for z in VARSOR[1:]:
            zmin = min(z,zmin)
            zmax = max(z,zmax)
         fig = plt.figure()       # initialises the figure
         #ex: fig = plt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
         mp = fig.gca()           # not sure what this does, but seems needed, think it initialises axis on plot
         cs = plt.tricontour(MESHX,MESHY,IKLE, VARSOR, linewidths=0.5, colors='k')
         #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
         plt.tricontourf(MESHX,MESHY,IKLE, VARSOR, cmap=colourmap)
         # adds numbers along the iso-contours
         plt.clabel(cs,fontsize=9,inline=1)
         mp.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
         mp.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
         mp.axis('equal')         # sets both axis scale to be equal
         #mp.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title
         #if geometry.has_key('cmapPlot'): fig.colorbar(colection)     # sets up colourbar
         #if geometry.has_key('cmapPlot'): fig.colorbar(colormap)     # sets up colourbar
         plt.show()
         figout = '.'.join([path.splitext(geometry['fileName'])[0],"bathy",geometry['outFormat']])
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
