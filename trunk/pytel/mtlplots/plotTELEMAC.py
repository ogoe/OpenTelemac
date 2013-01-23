#!/usr/bin/env python
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
"""@history 30/08/2011 -- Sebastien E. Bourban:
         Addition of a reference to time (or time frame rather) passed on
         from the XML and extracted while reading the files.
"""
"""@history 27/03/2012 -- Sebastien E. Bourban:
         Quad elements from TOMAWAC's SPECTRAL files are now supported,
         with each quad split into triangles.
"""
"""@history 28/03/2012 -- Sebastien E. Bourban:
         2D Vector plots are now supported with the ability to extract
         (re-interpolate) vectors on a regular mesh, given the roi
         (region-of-interest) and the extract (="[nx,ny]").
"""
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
from struct import unpack
import numpy as np
# FD@EDF : Forces not to use any Xwindows backend for Jenkins (see utils/progressbar.py)
try:
    hide_default = not sys.stderr.isatty()
except AttributeError: # output does not support isatty()
    hide_default = True
if hide_default:
   # Use of Agg must be done before importing matplotlib.pyplot
   import matplotlib as mpl
   mpl.use('Agg')
import matplotlib.pyplot as plt
# ~~> dependencies from within pytel/parsers
from myplot1d import drawHistoryLines,drawPolylineLines
from myplot2d import drawMesh2DElements,drawMeshLines, \
   drawLabeledTriContours,drawColouredTriMaps, \
   drawColouredTriVects
# ~~> dependencies towards other pytel/modules
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) ) # clever you !
from utils.files import getFileContent
from parsers.parserSortie import getValueHistorySortie
from parsers.parserSELAFIN import SELAFIN,getValueHistorySLF,parseSLF,getValuePolylineSLF,subsetVariablesSLF
from samplers.meshes import crossMesh,xysLocateMesh
from parsers.parserStrings import parseArrayPaires

# _____                   __________________________________________
# ____/ Plotting Toolbox /_________________________________________/
#
# TODO: Investigate the following:
#import matplotlib.rcParams as rcparams
#rcparams['xtick.direction'] = 'out'
#rcparams['ytick.direction'] = 'out'
#rcparams['contour.negative_linestyle'] = 'solid'
#    pyplot.figure(figsize=(20,2.5))
#    pyplot.hold(True)

deco = {
   'title': 'OpenTELEMAC',
   'dpi': 100,
   'grid': True,
   'crax.xlim': 0.02,
   'crax.ylim': 0.02,
   '1d-line-colours':[],
   '1d-line-symbols':[],
   'contour.levels' : '12',
   'contour.major.style' : 'solid',
   'contour.minor.style' : 'dashed',
   'contour.major.color' : 'black',
   'contour.minor.color' : 'gray',
   }
hrwd = { 
'plot_config_version' : 'R1r13',
'hrw_logo' : 'hr_wallingford.png',
'x_margin' : '0.02',
'y_margin' : '0.02',
'margins.x' : '0.02',
'margins.y' : '0.02',
'client_logo' : 'client_company.png',
'plot.boundary.padding' : '0.05',
'contour.nlevels' : '20',
'cmap.preset' : 'jet',
'cmap.user.min_max' : 'False',
'cmap.user.min' : '138',
'cmap.user.max' : '150',
'contour_line_color ' : 'black',
'contour_line_color2' : 'gray',
'inline_label_fmt' : '1.1f',
'inline_label_size' : '9',
'colorbar.label.fmt' : '1.3f',
'plot_xlabel' : 'x-axis (m)',
'plot_ylabel' : 'y-axis (m)',
'scatter.marker.type' : 'o',
'scatter.marker.size' : '100',
'scatter.marker.alpha' : '0.5',
'scatter.marker.outlines' : 'False',
'mesh_line_color_rgba' : '0.0,0.0,0.6,1.0',
'quiver_line_color_rgba' : '0.0,0.0,0.1,1.0',
'filled_triangle_alpha' : '0.99',
'trimesh_contour_inline_labels' : 'True',
'filled_curvilinear_alpha' : '1.0',
'use_image_bound_box' : 'False',
'image_retint' : 'False',
'image_retint_cmap' : 'gray',
'trimesh_overlap_zo' : 'True',
'trimesh_labels_density' : '1188',
'vector_regrid_density' : '80',
'outline_grid_dimension' : '10',
'anno.footer.height' : '1.0',
'anno.footer.width' : '1.0',
'anno.footer.placement' : 'bottom',
'anno.title.font.size' : '18',
'anno.subtitle.font.size' : '11',
'anno.footer.font.size' : '11',
'anno.num_logos' : '0',
'anno.num_labels' : '1',
'legend.location' : 'best',
'quiver.legend.location' : 'lower right',
'anno.label1.value' : 'My Plot',
'anno.label1.name' : 'Plot Title ',
'anno.label1.font.size' : '18 ',
'anno.label1.enabled' : 'True',
'anno.label1.font.family' : 'helvetica',
'anno.label1.rotation' : '0',
'anno.label1.rect' : '0.000000,0.900000,1.000000,0.100000,center,bottom', 
'anno.label1.font.style' : 'normal',
'anno.label1.option' : 'headlineString ',
'anno.label1.font.weight' : 'bold',
'anno.label1.placement' : 'plot'
}
#
#    Get current axes instance with plt.gca(), and current figure
#       with plt.gcf()
# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

"""
   This function is inspired from its equivalent function called stratPlot
      in the HR Wallingford library of python scripts.
   It takes one argument, plot, a dico with the following keys:
      - a title key, or a string which will be used as title of the figure
      - a type key, or a keyword or a list of keywords in [ line,rose ]
      - a dpi key, or an integer defining the picture resolution
   It returns a tuple made of a
      - FigureCanvas, or container class for the Figure instance
      - Figure, or container for one or more Axes instances
      - Axes, or the rectangular areas to hold the basic elements, such as
        lines, text, and so on

   For information:
      fig = plt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
"""
def openFigure(plot):

   # ~~> figure definition and resolution
   #  > plt.figure(dpi=pltdefault['dpi'])
   #dpi = pltdefault['dpi']
   if plot.has_key('dpi'): dpi = plot['dpi']
   #  > plt.figure(figsize=(width,height))
   figsize = plot["size"][0]
   #  > plt.figure() returns a Figure, where we can add one or more Axes instances
   fig = plt.figure(figsize=figsize)

   # ~~> add_subplot, or ax definition
   # fig.add_subplot(111) returns an Axes instance, where we can plot and this is
   #   also the reason why we call the variable referring to that instance ax.
   #   This is a common way to add an Axes to a Figure, but add_subplot() does a
   #   bit more: it adds a subplot. So far we have only seen a Figure with one Axes
   #   instance, so only one area where we can draw, but Matplotlib allows more than one
   fig.add_subplot(111)
   if plot.has_key('title'):
      plt.title(plot['title'])
   else:
      plt.title(pltdefault['title'])

   # ~~> types of plots
   if plot.has_key('type'):
      if len(plot['type']) > 0 and ((plot['type'][1] == 'line')or(plot['type'][1] == 'rose')):
         fig.add_subplot(adjustable='datalim')
      else:
         fig.add_subplot(aspect='equal')
   else:
      fig.add_subplot(aspect='equal')

   return (plt,fig)

"""
   ~ plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
     could be used to restrict the view to a user-defined zoom
"""
# _____                        _____________________________________
# ____/ Primary Class: FIGURE /____________________________________/
#
class Figure1D:
# TODO: use the names to label the curves

   def draw(self,type,what,fig):
      
      if 'sortie' in type.lower():
         # ~~> Load data
         sortie = getFileContent(what['file'])
         # ~~> Extract data
         data = getValueHistorySortie(sortie,what['vars'])
         # ~~> Deco
         # ~~> Draw data
         drawHistoryLines(plt,data,deco)

      elif 'SELAFIN' in type.upper():
         # ~~> Load data
         slf = SELAFIN(what['file'])

         if what['type'] == 'history':
            # ~~> Extract data
            vars = subsetVariablesSLF(what["vars"],slf.VARNAMES)
            support,tree,neighbours = xysLocateMesh(what["extract"],slf.IKLE,slf.MESHX,slf.MESHY)
            data = getValueHistorySLF(slf.file,slf.tags,what['time'],(support[1],slf.IKLE[support[1]],support[2]),slf.TITLE,slf.NVAR,slf.NPOIN3,vars)
            # ~~> Deco
            if what.has_key('roi'):
               if what['roi'] != []: deco['roi'] = what['roi']
            # ~~> Draw data
            drawHistoryLines(plt,data,deco)
            
         elif what['type'] == 'v-section':
            # ~~> Extract data
            vars = subsetVariablesSLF(what["vars"],slf.VARNAMES)
            support,tree,neighbours = crossMesh(what["extract"],slf.IKLE,slf.MESHX,slf.MESHY)
            data = getValuePolylineSLF(slf.file,slf.tags,what['time'],(support[0],slf.IKLE[support[1]],support[2]),slf.TITLE,slf.NVAR,slf.NPOIN3,vars)
            # ~~> Deco
            deco['roi'] = [ [np.min(slf.MESHX),np.min(slf.MESHY)], [np.max(slf.MESHX),np.max(slf.MESHY)] ]
            if what.has_key('roi'):
               if what['roi'] != []: deco['roi'] = what['roi']
            # ~~> Draw data
            drawPolylineLines(plt,data,deco)

         else: print '... do not know how to draw this type: ' + what['type']

      else:
         print '... do not know how to draw this format: ' + type

class Figure2D:
   def draw(self,type,what,fig):

      # ~~> Load data
      slf = SELAFIN(what['file'])

      # /!\ WACLEO: Temporary fix because TOMAWAC's IOs names are not yet standard TELEMAC
      if 'WACLEO' in type.upper() or \
         'SELAFIN' in type.upper():

         # ~~> Extract data
         elements = None; edges = []; edgexy = []

         # ~~> Deco
         xmin = np.min(slf.MESHX); xmax = np.max(slf.MESHX)
         ymin = np.min(slf.MESHY); ymax = np.max(slf.MESHY)
         if what.has_key('roi'):
            if what['roi'] != []:
               xmin = min(what['roi'][0][0],what['roi'][1][0])
               xmax = max(what['roi'][0][0],what['roi'][1][0])
               ymin = min(what['roi'][0][1],what['roi'][1][1])
               ymax = max(what['roi'][0][1],what['roi'][1][1])
         deco['roi'] = [[xmin,ymin],[xmax,ymax]]

         for var in what["vars"].split(';'):
            v,t = var.split(':')

            if "mesh" in t:
               # ~~> Extract mesh connectivity
               if elements == None: elements = np.dstack((slf.MESHX[slf.IKLE],slf.MESHY[slf.IKLE]))
               # ~~> Draw (works with triangles and quads)
               drawMesh2DElements(plt,elements,deco)

            elif "wire" in t:
               # ~~> Extract unique edges and outline /!\ assumes all same clowise orientation
               if edges == []:
                  for e in slf.IKLE:
                     for n in range(slf.NDP):
                        if (e[n],e[(n+1)%slf.NDP]) not in edges: edges.append((e[(n+1)%slf.NDP],e[n]))
                  # ~~> Assemble wires
                  for e in edges:
                     edgexy.append(( (slf.MESHX[e[0]],slf.MESHY[e[0]]) , (slf.MESHX[e[1]],slf.MESHY[e[1]]) ))
               # ~~> Draw (works with triangles and quads)
               drawMeshLines(plt,edgexy,deco)

            else:
               # ~~> Extract variable data
               VARSORS = []
               frame = 0
               if what.has_key('time'):
                  frame = int(what['time'][0])
                  if frame < 0: frame = max( 0, len(slf.tags['cores']) + frame )
               slf.file.seek(slf.tags['cores'][frame])
               slf.file.read(4+4+4)
               for ivar in range(slf.NVAR):
                  slf.file.read(4)
                  if v.upper() in slf.VARNAMES[ivar].strip():
                     VARSORS.append(np.asarray(unpack('>'+str(slf.NPOIN3)+'f',slf.file.read(4*slf.NPOIN3))))
                  else:
                     slf.file.read(4*slf.NPOIN3)
                  slf.file.read(4)
               # ~~> Multi-variables calculations
               MESHX = np.array(slf.MESHX); MESHY = np.array(slf.MESHY)
               if len(VARSORS) > 1:
                  if "arrow" in t or "angle" in t:
                     if what['extract'] != []:
                        dx = (xmax-xmin)/what['extract'][0][0]
                        dy = (ymax-ymin)/what['extract'][0][1]
                        grid = np.meshgrid(np.arange(xmin, xmax+dx, dx),np.arange(ymin, ymax+dy, dy))
                        MESHX = np.concatenate(grid[0]); MESHY = np.concatenate(grid[1])
                        support,tree,neighbours = xysLocateMesh(np.dstack((MESHX,MESHY))[0],slf.IKLE,slf.MESHX,slf.MESHY)
                        le,ln,bn = support[1],slf.IKLE[support[1]],support[2]
                        VARSOR = [np.zeros(len(le),np.float32),np.zeros(len(le),np.float32)]
                        for xy in range(len(bn)):
                           if le[xy] >= 0:
                              VARSOR[0][xy] = bn[xy][0]*VARSORS[0][ln[xy][0]] + bn[xy][1]*VARSORS[0][ln[xy][1]] + bn[xy][2]*VARSORS[0][ln[xy][2]]
                              VARSOR[1][xy] = bn[xy][0]*VARSORS[1][ln[xy][0]] + bn[xy][1]*VARSORS[1][ln[xy][1]] + bn[xy][2]*VARSORS[1][ln[xy][2]]
                  else:
                     t = "map"
                     VARSOR = np.sqrt(np.sum(np.power(np.dstack(VARSORS[0:2])[0],2),axis=1))
               else:
                  VARSOR = VARSORS[0]
               # ~~> Element types
               if slf.NDP == 3: IKLE = np.array(slf.IKLE)
               elif slf.NDP == 4:
                  # ~~> split each quad into triangles
                  IKLE = np.delete(np.concatenate((slf.IKLE,np.roll(slf.IKLE,2,axis=1))),np.s_[3::],axis=1)
               # ~~> Draw (multiple options possible)
               if "map" in t: drawColouredTriMaps(plt,(slf.MESHX,slf.MESHY,IKLE,VARSOR),deco)
               if "label" in t: drawLabeledTriContours(plt,(slf.MESHX,slf.MESHY,slf.IKLE,VARSOR),deco)
               if "arrow" in t: drawColouredTriVects(plt,(MESHX,MESHY,VARSOR,False),deco)
               if "angle" in t: drawColouredTriVects(plt,(MESHX,MESHY,VARSOR,True),deco)

      else:
         print '... do not know how to draw this format: ' + type

      slf.file.close()

      return


class Figure(Figure1D,Figure2D):
   plt = None; fig = None
   
   def __init__(self,type,plot,display,figureName):
      self.typePlot = type
      self.figureName = figureName
      self.display = display
      #/!\ fig represents here plt as well as fig in fig = plt.figure()
      self.plt,self.fig = openFigure(plot)

   def draw(self,type,what):
      if self.typePlot == "plot1d":
         Figure1D.draw(self,type,what,(self.plt,self.fig))
      elif self.typePlot == "plot2d":
         Figure2D.draw(self,type,what,(self.plt,self.fig))
         #try:
         #   import plt.tricontour
         #   tricontour_loaded = True
         #except:
         #   tricontour_loaded = False
         #if tricontour_loaded:
         #   Figure2D.draw(self,type,what,(self.plt,self.fig))
         #else:
         #   print '\ntricontour not available on your system'
         #   print ' ... unable to plot 2D figures'
      else:
         print '... do not know how to draw this type: ' + type
         sys.exit()

   def show(self):
      if self.display: self.plt.show()
      else: self.plt.savefig(self.figureName)


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

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Debuging drawFigure1D ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if False:
   # ~~ sortie file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'sortie' }
      figure = Figure('plot1d',draw,True,'')

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\1\\wing95s\\t2d_bumpflu_v1p0.cas_2011-11-23-13h40min12s.sortie'
      figure.draw( draw['type'], { 'file':fileName, 'type': 'history',
         'time':parseArrayPaires("[0;-1]")[0], 'vars':'voltotal:time' })

      figure.show()

   if False:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN','size':[[10,5]] }
      figure = Figure('plot1d',draw,True,'')

      fileName = 'C:\\home\\opentelemac\\trunk\\telemac2d\\tel2d_v6p2\\validation\\011_bumpflu\\f2d_bumpflu.slf'
      figure.draw( draw['type'], { 'file':fileName, 'type': 'history',
         'extract': parseArrayPaires('[10;1][0;1]'),
         'time':parseArrayPaires("[0;-1]")[0],'vars': 'surface libre:time' })

      figure.show()

   if True:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN','size':[[10,5]] }
      figure = Figure('plot1d',draw,True,'')

      fileName = 'C:\\home\\opentelemac\\trunk\\telemac2d\\tel2d_v6p2\\validation\\011_bumpflu\\f2d_bumpflu.slf'
      figure.draw( draw['type'], { 'file':fileName, 'type': 'v-section',
         'extract': parseArrayPaires('[0.0;1.0][20.0;1.0]'),
         'time':parseArrayPaires("[-1]")[0],'vars': 'surface libre:distance;fond:distance' })

      figure.show()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Debuging drawFigure2D ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if False:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN' }
      figure = Figure('plot2d',draw,True,'')

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\1\\wintels\\geo_bumpflu_v1p0.slf'
      figure.draw( draw['type'], { 'file':fileName,
         'time':parseArrayPaires("[-1]")[0],'vars': 'fond:map' })

      figure.show()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Debuging drawFigure2D ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if False:
   # ~~ SPECTRAL file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'WACLEO' }
      figure = Figure('plot2d',draw,True,'')

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\pwc_cali071-darwin_v6p2w1_2009-02-08-00.spc'
      figure.draw( draw['type'], { 'file':fileName,
         'time':parseArrayPaires("[-1]")[0],'vars': 'f01:map' })

      figure.show()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
"""
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

"""
