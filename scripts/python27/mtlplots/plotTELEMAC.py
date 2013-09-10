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
         (re-interpolate) vectors on a regular mesh, given the
         extract (="[xmin;ymin][xmax;ymax][nx;ny]").
"""
"""@history 08/03/2013 -- Juliette Parisi:
         Plot1D from csv file
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
from parsers.parserCSV import CSV
from parsers.parserSortie import getValueHistorySortie
from parsers.parserSELAFIN import SELAFIN,getValueHistorySLF,getValuePolylineSLF,subsetVariablesSLF,getValuePolyplanSLF
from samplers.meshes import xysLocateMesh,sliceMesh
from parsers.parserStrings import parseArrayFrame,parseArrayPoint,parseArrayGrid

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

def decoStandard(userdeco):
   for key in deco.keys():
      if not userdeco.has_key(key): userdeco.update({key:deco[key]})
   return userdeco

def getKDTree(MESHX,MESHY,IKLE):
   from scipy.spatial import cKDTree
   isoxy = np.column_stack((np.sum(MESHX[IKLE],axis=1)/3.0,np.sum(MESHY[IKLE],axis=1)/3.0))
   return cKDTree(isoxy)

def getMPLTri(MESHX,MESHY,IKLE):
   from matplotlib.tri import Triangulation
   mpltri = Triangulation(MESHX,MESHY,IKLE).get_cpp_triangulation()
   return mpltri.get_neighbors(),mpltri.get_edges()

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
   figsize = parseArrayFrame(plot["size"])
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
# _____                          ___________________________________
# ____/ Primary Classes: Drawer /__________________________________/
#
class Drawer1D:

   def __init__(self,(plt,fig)):
      self.plt = plt
      self.fig = fig

   def doHistoryLines(self,data,deco): drawHistoryLines(self.plt,data,deco)
   def doPolylineLines(self,data,deco): drawPolylineLines(self.plt,data,deco)

   def show(self): self.plt.show()
   def savefig(self,fileName): self.plt.savefig(fileName)

class Drawer2D:

   def __init__(self,(plt,fig)):
      self.plt = plt
      self.fig = fig

   def doMesh2DElements(self,elements,deco): drawMesh2DElements(self.plt,elements,deco)
   def doMeshLines(self,edgexy,deco): drawMeshLines(self.plt,edgexy,deco)
   def doColouredTriMaps(self,data,deco): drawColouredTriMaps(self.plt,data,deco)
   def doLabeledTriContours(self,data,deco): drawLabeledTriContours(self.plt,data,deco)
   def doColouredTriVects(self,data,deco): drawColouredTriVects(self.plt,data,deco)

   def show(self): self.plt.show()
   def savefig(self,fileName): self.plt.savefig(fileName)

# _____                         ____________________________________
# ____/ Primary Classes:Dumper /___________________________________/
#

class Dumper1D:
   
   def __init__(self):
      self.csv = CSV()

   def doHistoryLines(self,data,deco):
      time,values = data
      self.csv.addColumns(time,values)
   def doPolylineLines(self,data,deco):
      dist,values = data
      self.csv.addColumns(dist,values)
   
   def putFileContent(self,fileName): self.csv.putFileContent(fileName)

class Dumper2D:

   def __init__(self):
      self.slf = SELAFIN('')

   def doMesh2DElements(self,elements,deco): pass #dumpMesh2DElements(plt,elements,deco)
   def doMeshLines(self,edgexy,deco): pass #dumpMeshLines(plt,edgexy,deco)

   def doColouredTriMaps(self,(x,y,ikle,z),deco):
      #if self.slf.NBV1 == 0:
      #   self.slf.NDP3 = 3
      #   self.slf.NELEM3 = len(ikle)
      #   self.slf.NPOIN3 = len(x)
      #   self.slf.NPLAN = 1
      #   #self.slf.IPARAM
      #   self.slf.IKLE3 = ikle
      #   #self.slf.IPOBO
      #   self.slf.MESHX = x
      #   self.slf.MESHY = y
      #self.slf.NBV1 += len(z)
      #for ivar in range(len(z)):
      #   iname = str(len(self.slf.VARNAMES))+'?       '
      #   self.slf.VARNAMES.append(iname[0:8])
      #   self.slf.VARNAMES.append('?       ')
      #self.slf.NVAR = self.slf.NBV1
      #self.slf.VARINDEX = range(self.slf.NVAR)
      pass

   def doLabeledTriContours(self,data,deco): pass #dumpLabeledTriContours(plt,data,deco)
   def doColouredTriVects(self,data,deco): pass #dumpColouredTriVects(plt,data,deco)

   def putFileContent(self,fileName): self.slf.putFileContent(fileName)

# _____                        _____________________________________
# ____/ Primary Class: FIGURE /____________________________________/
#
class Figure:
   do = None

   def __init__(self,typl,plot,figureName,display=False):
      if typl == 'plot1d':
         self.do = Drawer1D(openFigure(plot))
      elif typl == 'plot2d':
         self.do = Drawer2D(openFigure(plot))
      elif typl == 'save1d':
         self.do = Dumper1D()
      elif typl == 'save2d':
         self.do = Dumper2D()
      else:
         print '... do not know how to Figure this one out: ' + typl
         sys.exit()
      self.typePlot = typl
      self.figureName = figureName
      self.display = display

   def show(self):
      if self.display: self.do.show()
      else: self.do.savefig(self.figureName)

   def dump(self):
      self.do.putFileContent(self.figureName)

class Figure1D(Figure):
   # TODO: use the names to label the curves

   def draw(self,typl,what):

      if 'sortie' in typl.lower():
         # ~~> Load data
         # what['file']: has only one file
         sortie = getFileContent(what['file'])
         # ~~> Extract data
         # what['vars']: list of pairs variables:support delimited by ';' (/!\ support is ignored)
         data = getValueHistorySortie(sortie,what['vars'])
         # ~~> Draw/Dump data
         # what['deco']: TODO
         self.do.doHistoryLines(data,decoStandard(what["deco"]))

      elif 'csv' in typl.lower():
         # ~~> Load data
         # what['file']: has only one file
         csv = CSV()
         csv.getFileContent(what['file'])
         # ~~> Extract data
         # what['vars']: list of pairs variables:support delimited by ';' (/!\ support is ignored)
         t,data = csv.getColumns(what["vars"])
         # ~~> Draw/Dump data
         # what['deco']: TODO
         self.do.doHistoryLines((t,[('experiment',data[0],data[1])]),decoStandard(what["deco"]))

      elif 'SELAFIN' in typl.upper() or 'slf' in typl.lower():
         # ~~> Load data
         slf = SELAFIN(what['file'])
         slf.setKDTree()
         slf.setMPLTri()

         if what['type'] == 'history':   # ~~> 1D time history from 2D or 3D results
            # ~~> Extract data
            # what['vars']: list of pairs variables:support delimited by ';' (/!\ support is ignored)
            vars = subsetVariablesSLF(what["vars"],slf.VARNAMES)
            # what['time']: list of frames or (times,) delimited by ';'
            t = parseArrayFrame(what['time'],len(slf.tags['cores']))
            # what['extract']: could be list delimited by ';', of:
            #    + points (x;y),
            #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by ';'
            #      where n is a plan number and d is depth from that plane (or the surface by default)
            support2d = []; zps = []
            for xyi,zpi in parseArrayPoint(what["extract"],slf.NPLAN):
               if type(xyi) == type(()): support2d.append( xysLocateMesh(xyi,slf.IKLE2,slf.MESHX,slf.MESHY,slf.tree,slf.neighbours) )
               else: support2d.append( xyi )
               zps.append( zpi )
            support3d = zip(support2d,zps)
            # - support2d[i][0] is either the node or the triplet of nodes for each element including (x,y)
            # - support2d[i][1] is the plan or depth definition
            data = getValueHistorySLF(slf.file,slf.tags,t,support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars)
            # ~~> Draw/Dump data
            self.do.doHistoryLines((('Time (s)',slf.tags['times'][t]),[('history','not used','not used',data)]),decoStandard(what["deco"]))
            
         elif what['type'] == 'v-section':
            # ~~> Extract data
            # what['vars']: list of pairs variables:support2d delimited by ';'
            vars = subsetVariablesSLF(what["vars"],slf.VARNAMES)
            # what['time']: list of frames or (times,) delimited by ';'
            t = parseArrayFrame(what['time'],len(slf.tags['cores']))
            # what['extract']: could be list delimited by ';', of:
            #    + points (x;y),
            #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by ';'
            #      where n is a plan number and d is depth from that plane (or the surface by default)
            xyo = []; zpo = []
            for xyi,zpi in parseArrayPoint(what["extract"],slf.NPLAN):
               if type(xyi) == type(()): xyo.append(xyi)
               else: xyo.append( (slf.MESHX[xyi],slf.MESHY[xyi]) )
               for p in zpi:                         # /!\ common deinition of plans
                  if p not in zpo: zpo.append(p)     # /!\ only allowing plans for now
            xys,support2d = sliceMesh(xyo,slf.IKLE2,slf.MESHX,slf.MESHY,slf.tree)
            # - support2d[i][0] is either the douplets of nodes for each edges crossing with the polyline
            # - support2d[i][1] is the plan or depth definition
            support3d = []
            for s2d in support2d: support3d.append( (s2d,zpo) )   # common vertical definition to all points
            data = getValuePolylineSLF(slf.file,slf.tags,t,support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars)
            # Distance d-axis
            distot = 0.0
            d = [ distot ]
            for xy in range(len(xys)-1):
               distot += np.sqrt( np.power(xys[xy+1][0]-xys[xy][0],2) + np.power(xys[xy+1][1]-xys[xy][1],2) )
               d.append(distot)
            # ~~> Draw/Dump data
            self.do.doPolylineLines((('Distance (m)',d),[('v-section',vars[1],t,zpo,data)]),decoStandard(what["deco"]))

         else: print '... do not know how to draw this type: ' + what['type']

      else:
         print '... do not know how to draw this format: ' + typl

class Figure2D(Figure):

   def draw(self,typl,what):

      # /!\ WACLEO: Temporary fix because TOMAWAC's IOs names are not yet standard TELEMAC
      if 'WACLEO' in typl.upper() or \
         'SELAFIN' in typl.upper() or \
         'slf' in typl.lower():

         # ~~> Load data
         slf = SELAFIN(what['file'])
         slf.setKDTree()
         slf.setMPLTri()

         # ~~> Extract data
         elements = None; edges = []; edgexy = []

         if what['type'] == 'v-section':
            
            # ~~> Extract variable data for only one time frame
            # what['time']: list of frames or (times,) delimited by ';'
            t = parseArrayFrame(what['time'],len(slf.tags['cores']))
            if len(t) != 1: print '... the time definition should only have one frame in this case. It is: ',what['time'],'. I will assume you wish to plot the last frame.'
            t = [ t[len(t)-1] ]
            # what['vars']: list of pairs variables:support2d delimited by ';', include 'Z' I hope
            vars = []; vtypes = []
            for var in what["vars"].split(';'):
               v,vtype = var.split(':')
               vars.append( v ); vtypes.append( vtype )
            vars = subsetVariablesSLF(';'.join(vars),slf.VARNAMES)
            # what['extract']: could be list delimited by ';', of:
            #    + points (x;y),
            #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by ';'
            #      where n is a plan number and d is depth from that plane (or the surface by default)
            xyo = []; zpo = []
            for xyi,zpi in parseArrayPoint(what["extract"],slf.NPLAN):
               if type(xyi) == type(()): xyo.append(xyi)
               else: xyo.append( (slf.MESHX[xyi],slf.MESHY[xyi]) )
               for p in zpi:                         # /!\ common deinition of plans
                  if p not in zpo: zpo.append(p)     # /!\ only allowing plans for now

            # ~~> Extract horizontal mesh
            xys,support2d = sliceMesh(xyo,slf.IKLE2,slf.MESHX,slf.MESHY,slf.tree)
            support3d = []
            for s2d in support2d: support3d.append( (s2d,zpo) )   # common vertical definition to all points
            data = getValuePolylineSLF(slf.file,slf.tags,t,support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars)
            # Distance d-axis
            distot = 0.0
            d = [ distot ]
            for xy in range(len(xys)-1):
               distot += np.sqrt( np.power(xys[xy+1][0]-xys[xy][0],2) + np.power(xys[xy+1][1]-xys[xy][1],2) )
               d.append(distot)
            MESHX = np.repeat(d,len(zpo))
            # ~~>  Extract Z
            varz = subsetVariablesSLF('z',slf.VARNAMES)
            MESHZ = 10*np.ravel( getValuePolylineSLF(slf.file,slf.tags,t,support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,varz)[0][0].T )
            # ~~>  Connect
            IKLE = []
            for j in range(len(d)-1):
               for i in range(len(zpo)-1):
                  IKLE.append([ i+j*len(zpo),i+(j+1)*len(zpo),i+1+j*len(zpo) ])
                  IKLE.append([ i+1+j*len(zpo),i+(j+1)*len(zpo),i+1+(j+1)*len(zpo) ])
            IKLE = np.array(IKLE)

            # ~~> Possible re-sampling
            support2d = []
            if what["sample"] != '':
               supMESHX = []; supMESHZ = []
               supIKLE = []
               for grid in parseArrayGrid(what["sample"],[(min(MESHX),min(MESHZ)),(max(MESHX),max(MESHZ))]):
                  mx,my = np.meshgrid(np.linspace(grid[0][0], grid[1][0], grid[2][0]+1),np.linspace(grid[0][1], grid[1][1], grid[2][1]+1))
                  # splitting quad into triangles
                  dimy = len(mx)
                  dimx = len(mx[0])
                  for j in range(dimy-1):
                     for i in range(dimx-1):
                        supIKLE.append([ i+j*dimx,i+1+j*dimx,i+(j+1)*dimx ])
                        supIKLE.append([ i+1+j*dimx,i+1+(j+1)*dimx,i+(j+1)*dimx ])
                  mx = np.concatenate(mx); my = np.concatenate(my)
                  supMESHX.extend(mx)
                  supMESHZ.extend(my)
                  for xyo in zip(mx,my): support2d.append( xysLocateMesh(xyo,IKLE,MESHX,MESHZ,getKDTree(MESHX,MESHZ,IKLE),getMPLTri(MESHX,MESHZ,IKLE)[0]) )
               IKLE = np.array(supIKLE)
               MESHX = np.array(supMESHX)
               MESHZ = np.array(supMESHZ)

            # ~~> Loop on variables
            for var in what["vars"].split(';'):
               v,vtype = var.split(':')

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               if "mesh" in vtype:
                  # ~~> Extract mesh connectivity
                  if elements == None: elements = np.dstack((MESHX[IKLE],MESHZ[IKLE]))
                  # ~~> Draw/Dump (works with triangles and quads)
                  self.do.doMesh2DElements(elements,decoStandard(what["deco"]))

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               elif "wire" in vtype:

                  if support2d != []:
                     print '... you cannot "wire" and "sample" at the same time yet with this layer: ',var
                     sys.exit()
                  # ~~> Extract unique edges and outline /!\ assumes all same clowise orientation
                  if edgexy == []:
                     # ~~> Bottom row
                     for j in range(len(d)-1): edgexy.append(((MESHX[j*len(zpo)],MESHZ[j*len(zpo)]),(MESHX[(j+1)*len(zpo)],MESHZ[(j+1)*len(zpo)])))
                     # ~~> Left column
                     for i in range(len(zpo)-1): edgexy.append(((MESHX[i],MESHZ[i]),(MESHX[i+1],MESHZ[i+1])))
                     # ~~> Top rows and Right columns
                     for j in range(len(d)-1):
                        for i in range(len(zpo)-1):
                           edgexy.append(((MESHX[i+1+(j+1)*len(zpo)],MESHZ[i+1+(j+1)*len(zpo)]),(MESHX[i+1+j*len(zpo)],MESHZ[i+1+j*len(zpo)])))
                           edgexy.append(((MESHX[i+(j+1)*len(zpo)],MESHZ[i+(j+1)*len(zpo)]),(MESHX[i+1+(j+1)*len(zpo)],MESHZ[i+1+(j+1)*len(zpo)])))
                  # ~~> Draw/Dump (works with triangles and quads)
                  self.do.doMeshLines(edgexy,decoStandard(what["deco"]))

               else:
                  vars = subsetVariablesSLF(v,slf.VARNAMES)
                  VARSORS = []
                  for ivar in range(len(vars[0])): VARSORS.append( np.ravel(data[ivar][0].T) )

                  # ~~> Re-sampling
                  if support2d != []:
                     data = np.zeros((len(vars[0]),len(support2d)),dtype=np.float64)
                     for ivar in range(len(vars[0])):
                        for ipt in range(len(support2d)):
                           ln,bn = support2d[ipt]
                           data[ivar][ipt] = 0.0
                           for inod in range(len(bn)):                  # /!\ node could be outside domain
                              if ln[inod] >=0: data[ivar][ipt] += bn[inod]*VARSORS[ivar][ln[inod]]
                     VARSORS = data

                  # ~~> Multi-variables calculations
                  if len(VARSORS) > 1:
                     if "map" in vtype: VARSORS = [ np.sqrt(np.sum(np.power(np.dstack(VARSORS[0:3:2])[0],2),axis=1)) ]
                  # ~~> Draw/Dump (multiple options possible)
                  if "map" in vtype: self.do.doColouredTriMaps((MESHX,MESHZ,IKLE,VARSORS[0]),decoStandard(what["deco"]))
                  if "label" in vtype: self.do.doLabeledTriContours((MESHX,MESHZ,IKLE,VARSORS[0]),decoStandard(what["deco"]))
                  if "arrow" in vtype: self.do.doColouredTriVects((MESHX,MESHZ,VARSORS,False),decoStandard(what["deco"]))
                  if "angle" in vtype: self.do.doColouredTriVects((MESHX,MESHZ,VARSORS,True),decoStandard(what["deco"]))
                  
         else: # if what['type'] != 'v-section'

            # ~~> Possible re-sampling
            support2d = []
            if what["sample"] != '':
               MESHX = []; MESHY = []
               IKLE = []
               for grid in parseArrayGrid(what["sample"],[(min(slf.MESHX),min(slf.MESHY)),(max(slf.MESHX),max(slf.MESHY))]):
                  mx,my = np.meshgrid(np.linspace(grid[0][0], grid[1][0], grid[2][0]+1),np.linspace(grid[0][1], grid[1][1], grid[2][1]+1))
                  # splitting quad into triangles
                  dimy = len(mx)
                  dimx = len(mx[0])
                  for j in range(dimy-1):
                     for i in range(dimx-1):
                        IKLE.append([ i+j*dimx,i+1+j*dimx,i+(j+1)*dimx ])
                        IKLE.append([ i+1+j*dimx,i+1+(j+1)*dimx,i+(j+1)*dimx ])
                  mx = np.concatenate(mx); my = np.concatenate(my)
                  MESHX.extend(mx)
                  MESHY.extend(my)
                  for xyo in zip(mx,my): support2d.append( xysLocateMesh(xyo,slf.IKLE2,slf.MESHX,slf.MESHY,slf.tree,slf.neighbours) )
               IKLE = np.array(IKLE)
               MESHX = np.array(MESHX)
               MESHY = np.array(MESHY)

            # ~~> Default MESH
            else:
               MESHX = np.array(slf.MESHX); MESHY = np.array(slf.MESHY)
               # work on the mesh of 2D triangles
               if slf.NDP2 == 3: IKLE = np.array(slf.IKLE2)
               # split each quad into triangles
               elif slf.NDP2 == 4: IKLE = np.delete(np.concatenate((slf.IKLE2,np.roll(slf.IKLE2,2,axis=1))),np.s_[3::],axis=1)

            # ~~> Loop on variables
            for var in what["vars"].split(';'):
               v,vtype = var.split(':')

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               if "mesh" in vtype:
                  # ~~> Extract mesh connectivity
                  if elements == None: elements = np.dstack((MESHX[IKLE],MESHY[IKLE]))
                  # ~~> Draw/Dump (works with triangles and quads)
                  self.do.doMesh2DElements(elements,decoStandard(what["deco"]))

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               elif "wire" in vtype:
                  # ~~> Extract unique edges and outline /!\ assumes all same clowise orientation
                  if edges == []:
                     for e in IKLE:
                        for n in range(slf.NDP2):
                           if (e[n],e[(n+1)%slf.NDP2]) not in edges: edges.append((e[(n+1)%slf.NDP2],e[n]))
                     # ~~> Assemble wires
                     for e in edges:
                        edgexy.append(( (MESHX[e[0]],MESHY[e[0]]) , (MESHX[e[1]],MESHY[e[1]]) ))
                  # ~~> Draw/Dump (works with triangles and quads)
                  self.do.doMeshLines(edgexy,decoStandard(what["deco"]))

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               else:
                  # ~~> Extract variable data for only one time frame and one plane
                  # what['time']: list of frames or (times,) delimited by ';'
                  t = parseArrayFrame(what['time'],len(slf.tags['cores']))
                  if len(t) != 1: print '... the time definition should only have one frame in this case. It is: ',what['time'],'. I will assume you wish to plot the last frame.'
                  t = [ t[len(t)-1] ]
                  # what['vars']: list of pairs variables:support2d delimited by ';'
                  vars = subsetVariablesSLF(v,slf.VARNAMES)
                  # what['extract']: could be list delimited by ';', of:
                  #    + points (x;y),
                  #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by ';'
                  #      where n is a plan number and d is depth from that plane (or the surface by default)
                  xyo = []; zpo = []
                  for xyi,zpi in parseArrayPoint(what["extract"],slf.NPLAN):
                     if xyi == [] or type(xyi) == type(()): xyo.append(xyi)
                     else: xyo.append( (slf.MESHX[xyi],slf.MESHY[xyi]) )
                     for p in zpi:                         # /!\ common deinition of plans
                        if p not in zpo: zpo.append(p)     # /!\ only allowing plans for now
                  if len(zpo) != 1: print '... the vertical definition should only have one plan in this case. It is: ',what['extract'],'. I will assume you wish to plot the higher plane.'
                  # could be more than one variables including v, but only one time frame t and one plan
                  data = getValuePolyplanSLF(slf.file,slf.tags,t,zpo,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars)
                  VARSORS = []
                  for ivar in range(len(data)): VARSORS.append( data[ivar][0][0] )

                  # ~~> Re-sampling
                  if support2d != []:
                     data = np.zeros((len(vars[0]),len(support2d)),dtype=np.float64)
                     for ivar in range(len(vars[0])):
                        for ipt in range(len(support2d)):
                           ln,bn = support2d[ipt]
                           data[ivar][ipt] = 0.0
                           for inod in range(len(bn)):                  # /!\ node could be outside domain
                              if ln[inod] >=0: data[ivar][ipt] += bn[inod]*VARSORS[ivar][ln[inod]]
                     VARSORS = data

                  # ~~> Multi-variables calculations
                  if len(VARSORS) > 1:
                     if "map" in vtype: VARSORS = [ np.sqrt(np.sum(np.power(np.dstack(VARSORS[0:2])[0],2),axis=1)) ]
                  # ~~> Draw/Dump (multiple options possible)
                  if "map" in vtype: self.do.doColouredTriMaps((MESHX,MESHY,IKLE,VARSORS[0]),decoStandard(what["deco"]))
                  if "label" in vtype: self.do.doLabeledTriContours((MESHX,MESHY,IKLE,VARSORS[0]),decoStandard(what["deco"]))
                  if "arrow" in vtype: self.do.doColouredTriVects((MESHX,MESHY,VARSORS,False),decoStandard(what["deco"]))
                  if "angle" in vtype: self.do.doColouredTriVects((MESHX,MESHY,VARSORS,True),decoStandard(what["deco"]))

      else:
         print '... do not know how to draw this format: ' + typl

      slf.file.close()

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

   if False:
   # ~~ sortie file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'sortie' }
      figure = Figure('plot1d',draw,True,'')

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\1\\wing95s\\t2d_bumpflu_v1p0.cas_2011-11-23-13h40min12s.sortie'
      figure.draw( draw['type'], { 'file':fileName, 'type': 'history',
         'time':parseArrayFrame("[0;-1]")[0], 'vars':'voltotal:time' })

      figure.show()

   if False:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN','size':[[10,5]] }
      figure = Figure('plot1d',draw,True,'')

      fileName = 'C:\\home\\opentelemac\\trunk\\telemac2d\\tel2d_v6p2\\validation\\011_bumpflu\\f2d_bumpflu.slf'
      figure.draw( draw['type'], { 'file':fileName, 'type': 'history',
         'extract': parseArrayPoint('[10;1][0;1]'),
         'time':parseArrayFrame("[0;-1]")[0],'vars': 'surface libre:time' })

      figure.show()

   if True:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN','size':[[10,5]] }
      figure = Figure('plot1d',draw,True,'')

      fileName = 'C:\\home\\opentelemac\\trunk\\telemac2d\\tel2d_v6p2\\validation\\011_bumpflu\\f2d_bumpflu.slf'
      figure.draw( draw['type'], { 'file':fileName, 'type': 'v-section',
         'extract': parseArrayPoint('[0.0;1.0][20.0;1.0]'),
         'time':parseArrayFrame("[-1]")[0],'vars': 'surface libre:distance;fond:distance' })

      figure.show()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Debuging drawFigure2D ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if False:
   # ~~ SELAFIN file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'SELAFIN' }
      figure = Figure('plot2d',draw,True,'')

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\1\\wintels\\geo_bumpflu_v1p0.slf'
      figure.draw( draw['type'], { 'file':fileName,
         'time':parseArrayFrame("[-1]")[0],'vars': 'fond:map' })

      figure.show()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Debuging drawFigure2D ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if False:
   # ~~ SPECTRAL file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      draw = { 'title':'openTELEMAC','type':'WACLEO' }
      figure = Figure('plot2d',draw,True,'')

      fileName = 'C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\pwc_cali071-darwin_v6p2w1_2009-02-08-00.spc'
      figure.draw( draw['type'], { 'file':fileName,
         'time':parseArrayFrame("[-1]")[0],'vars': 'f01:map' })

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
