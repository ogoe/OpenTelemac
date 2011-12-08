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
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from struct import unpack
import numpy as np
import matplotlib.pyplot as plt
# ~~> dependencies from within pytel/parsers
from myplot1d import drawHistoryLines,drawPolylineLines
from myplot2d import drawLabeledContours,drawMeshTriangles,drawMeshLines,drawColouredMaps
# ~~> dependencies towards other pytel/modules
from utils.files import getFileContent
from parsers.parserSortie import getValueHistorySortie
from parsers.parserSELAFIN import getValueHistorySLF,parseSLF,crossLocateMeshSLF,xyLocateMeshSLF,getValuePolylineSLF,subsetVariablesSLF
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
   'roi':[0,0,1,1],
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
   # plt.figure() returns a Figure, where we can add one or more Axes instances
   fig = plt.figure()
   #if plot.has_key('dpi'):
   #   plt.figure(dpi=plot['dpi'])
   #else:
   #   plt.figure(dpi=pltdefault['dpi'])
   # TODO: use matplotlib.backends.backend_qt4agg.FigureCanvasQTAgg for future GUI

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

def closeFigure((plt,fig)):

   plt.show()
   #plt.savefig(fig)

   return

"""
   ~ plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
     could be used to restrict the view to a user-defined zoom
"""
def drawFigure1D(type,what,(plt,fig)):
# TODO: use the names to label the curves
# TODO: save the plot in a file name.png

   if 'sortie' in type.lower():
      drawHistoryLines(plt,getValueHistorySortie(getFileContent(what['file']),
         what["vars"]),deco)

   elif 'SELAFIN' in type.upper():

      # ~~ Extract data
      f = open(what['file'],'rb')
      tags,TITLE,numbers,vars,mesh = parseSLF(f)
      NELEM3,NPOIN3,NDP,NPLAN = numbers
      NBV1,NBV2,VARNAMES,VARUNITS = vars
      IKLE,IPOBO,MESHX,MESHY = mesh

      if what['type'] == 'history':
         if what.has_key('roi'): deco['roi'] = what['roi']

         drawHistoryLines(plt,getValueHistorySLF(f,tags,what['time'],
            xyLocateMeshSLF(what["extract"],NELEM3,IKLE,MESHX,MESHY),
            TITLE,(NBV1+NBV2),NPOIN3,
            subsetVariablesSLF(what["vars"],VARNAMES)),deco)

      elif what['type'] == 'v-section':
         if what.has_key('roi'):
            deco['roi'] = what['roi']
         else:
            deco['roi'] = [ np.min(MESHX),np.max(MESHX), np.min(MESHY),np.max(MESHY) ]

         drawPolylineLines(plt,getValuePolylineSLF(f,tags,what['time'],
            crossLocateMeshSLF(what["extract"],NELEM3,IKLE,MESHX,MESHY),
            TITLE,(NBV1+NBV2),NPOIN3,
            subsetVariablesSLF(what["vars"],VARNAMES)),deco)

      else:
         print '... do not know how to draw this type: ' + what['type']

      f.close()

   else:
      print '... do not know how to draw this format: ' + type

   return

def drawFigure2D(type,what,(plt,fig)):
# TODO: use the names to label the curves
# TODO: save the plot in a file name.png

   if 'SELAFIN' in type.upper():

      # ~~ Extract data
      f = open(what['file'],'rb')
      tags,title,numbers,vars,mesh = parseSLF(f)
      NELEM3,NPOIN3,NDP,NPLAN = numbers
      NBV1,NBV2,VARNAMES,VARUNITS = vars
      IKLE,IPOBO,MESHX,MESHY = mesh

      if what.has_key('roi'):
         deco['roi'] = what['roi']
      else:
         deco['roi'] = [ np.min(MESHX),np.max(MESHX), np.min(MESHY),np.max(MESHY) ]
      elements = []; edges = []; edgexy = []

      for var in what["vars"].split(';'):
         v,t = var.split(':')

         if "mesh" in t:
            # ~~> Extract mesh connectivity
            if elements == []:
               for e in IKLE:
                  element = []
                  for n in e: element.append((MESHX[n],MESHY[n]))
                  elements.append(element)
            # ~~> Draw
            drawMeshTriangles(plt,elements,deco)

         elif "wire" in t:
            # ~~> Extract unique edges and outline /!\ assumes all same clowise orientation
            if edges == []:
               for e in IKLE:
                  if (e[0],e[1]) not in edges: edges.append((e[1],e[0]))
                  if (e[1],e[2]) not in edges: edges.append((e[2],e[1]))
                  if (e[2],e[0]) not in edges: edges.append((e[0],e[2]))
               # ~~> Assemble wires
               for e in edges:
                  edgexy.append(( (MESHX[e[0]],MESHY[e[0]]) , (MESHX[e[1]],MESHY[e[1]]) ))
            # ~~> Draw
            drawMeshLines(plt,edgexy,deco)
            
         else:
            # ~~> Extract variable data
            VARSOR = np.zeros(NPOIN3)
            f.seek(tags['cores'][0])
            f.read(4+4+4)
            for ivar in range(NBV1+NBV2):
               f.read(4)
               if v.upper() in VARNAMES[ivar].strip():
                  VARSOR = np.asarray(unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3)))
                  break
               else:
                  f.read(4*NPOIN3)
               f.read(4)
            # ~~> Draw (multiple options possible)
            if "map" in t: drawColouredMaps(plt,(MESHX,MESHY,IKLE,VARSOR),deco)
            if "label" in t: drawLabeledContours(plt,(MESHX,MESHY,IKLE,VARSOR),deco)

      f.close()
   else:
      print '... do not know how to draw this format: ' + type

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

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Debuging drawFigure1D ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if True:
   # ~~ sortie file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'sortie' }
      fig = openFigure(draw)

      type = 'sortie'
      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\1\\wing95s\\t2d_bumpflu_v1p0.cas_2011-11-23-13h40min12s.sortie'
      drawFigure1D( draw['type'], { 'file':fileName, 'type': 'history',
         'time':parseArrayPaires("[0;-1]")[0], 'vars':'voltotal:time' }, fig)

      closeFigure(fig)

   if True:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN' }
      fig = openFigure(draw)

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\1\\wing95s\\r2d_bumpflu_v1p0.slf'
      drawFigure1D( draw['type'], { 'file':fileName, 'type': 'history',
         'extract': parseArrayPaires('[10;1][0;1]'),
         'time':parseArrayPaires("[0;-1]")[0],'vars': 'surface libre:time' }, fig)

      closeFigure(fig)

   if True:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN' }
      fig = openFigure(draw)

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\1\\wing95s\\r2d_bumpflu_v1p0.slf'
      drawFigure1D( draw['type'], { 'file':fileName, 'type': 'v-section',
         'extract': parseArrayPaires('[0.0;1.0][20;1.0]'),
         'time':parseArrayPaires("[-1]")[0],'vars': 'surface libre:distance;fond:distance' }, fig)

      closeFigure(fig)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Debuging drawFigure2D ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if True:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN' }
      fig = openFigure(draw)

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\1\\wintels\\geo_bumpflu_v1p0.slf'
      drawFigure2D( draw['type'], { 'file':fileName,
         'time':parseArrayPaires("[-1]")[0],'vars': 'fond:map'
         }, fig)

      closeFigure(fig)

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