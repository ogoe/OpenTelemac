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
import numpy as np
# ~~> dependencies from within pytel/parsers
from myplot1d import Drawer1D
from myplot2d import Drawer2D
from myplot3d import Drawer3D
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
decoFigure = {
   'data':{ 'title': 'openTELEMAC' },
   'look':{ 'savefig.dpi': 100, 'savefig.format':"png", 'grid': True, 'crax.xlim': 0.02, 'crax.ylim': 0.02 }
   }
decoLayer = {
   'data':{ },
   'look':{ '1d-line-colours':[], '1d-line-symbols':[], 'contour.levels' : '12',
      'contour.major.style' : 'solid', 'contour.minor.style' : 'dashed', 'contour.major.color' : 'black', 'contour.minor.color' : 'gray' }
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

def getKDTree(MESHX,MESHY,IKLE):
   from scipy.spatial import cKDTree
   isoxy = np.column_stack((np.sum(MESHX[IKLE],axis=1)/3.0,np.sum(MESHY[IKLE],axis=1)/3.0))
   return cKDTree(isoxy)

def getMPLTri(MESHX,MESHY,IKLE):
   from matplotlib.tri import Triangulation
   mpltri = Triangulation(MESHX,MESHY,IKLE).get_cpp_triangulation()
   return mpltri.get_neighbors(),mpltri.get_edges()

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

class Dumper3D:
   pass

# _____                        _____________________________________
# ____/ Primary Class: FIGURE /____________________________________/
#
class Figure:
   do = None

   def __init__(self,typl,plot,figureName,display=False):
      if typl == 'plot1d':
         self.do = Drawer1D(plot)
      elif typl == 'plot2d':
         self.do = Drawer2D(plot)
      elif typl == 'plot3d':
         self.do = Drawer3D(plot)
      elif typl == 'save1d':
         self.do = Dumper1D() # meta data with plot['deco']
      elif typl == 'save2d':
         self.do = Dumper2D() # meta data with plot['deco']
      elif typl == 'save3d':
         self.do = Dumper3D() # meta data with plot['deco']
      else:
         print '... do not know how to Figure this one out: ' + typl
         sys.exit(1)
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
         self.do.doHistoryLines(data,what["deco"])

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
         self.do.doHistoryLines((t,[('experiment',data[0],data[1])]),what["deco"])

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
            pAP = parseArrayPoint(what["extract"],slf.NPLAN)
            for xyi,zpi in pAP:
               if type(xyi) == type(()): support2d.append( xysLocateMesh(xyi,slf.IKLE2,slf.MESHX,slf.MESHY,slf.tree,slf.neighbours) )
               else: support2d.append( xyi )
               zps.append( zpi )
            support3d = zip(support2d,zps)
            # - support2d[i][0] is either the node or the triplet of nodes for each element including (x,y)
            # - support2d[i][1] is the plan or depth definition
            data = getValueHistorySLF(slf.file,slf.tags,t,support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars)
            # ~~> Draw/Dump data
            self.do.doHistoryLines((('Time (s)',slf.tags['times'][t]), \
               [('',[n.replace(' ','_').replace(',',';') for n in vars[1]],[(str(n[0])+':'+str(m)).replace(' ','').replace(',',';') for n in pAP for m in n[1]],data)]),what["deco"])

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
            self.do.doPolylineLines((('Distance (m)',d),[('v-section',vars[1],t,zpo,data)]),what["deco"])

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
                  self.do.doMesh2DElements(elements,what["deco"])

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               elif "wire" in vtype:

                  if support2d != []:
                     print '... you cannot "wire" and "sample" at the same time yet with this layer: ',var
                     sys.exit(1)
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
                  self.do.doMeshLines(edgexy,what["deco"])

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
                  if "map" in vtype: self.do.doColouredTriMaps((MESHX,MESHZ,IKLE,VARSORS[0]),what["deco"])
                  if "label" in vtype: self.do.doLabeledTriContours((MESHX,MESHZ,IKLE,VARSORS[0]),what["deco"])
                  if "arrow" in vtype: self.do.doColouredTriVects((MESHX,MESHZ,VARSORS,False),what["deco"])
                  if "angle" in vtype: self.do.doColouredTriVects((MESHX,MESHZ,VARSORS,True),what["deco"])
                  
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
                  self.do.doMesh2DElements(elements,what["deco"])

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
                  self.do.doMeshLines(edgexy,what["deco"])

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
                  if "map" in vtype: self.do.doColouredTriMaps((MESHX,MESHY,IKLE,VARSORS[0]),what["deco"])
                  if "label" in vtype: self.do.doLabeledTriContours((MESHX,MESHY,IKLE,VARSORS[0]),what["deco"])
                  if "arrow" in vtype: self.do.doColouredTriVects((MESHX,MESHY,VARSORS,False),what["deco"])
                  if "angle" in vtype: self.do.doColouredTriVects((MESHX,MESHY,VARSORS,True),what["deco"])

      else:
         print '... do not know how to draw this format: ' + typl

      slf.file.close()

      return

class Figure3D(Figure):

   def draw(self,typl,what):
      pass

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
      sys.exit(1)

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

   sys.exit(0)
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
         if 'varsPlot' in geometry:
            if not VARNAMES[i].strip() in geometry['varsPlot']: continue
         VARSOR = getCoreValueSLF(f,i,NPOIN3)
         # ~~ Plot data
         colourmap = cm.jet
         if 'cmapPlot'in geometry:
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
         #if 'cmapPlot' in geometry: fig.colorbar(colection)     # sets up colourbar
         #if 'cmapPlot' in geometry: fig.colorbar(colormap)     # sets up colourbar
         plt.show()
         figout = '.'.join([path.splitext(geometry['fileName'])[0],"bathy",geometry['outFormat']])
         plt.savefig(figout)      # saves output plot to .png file

   f.close()
   return figout



### MATPLOTLIBRC FORMAT

# This is a sample matplotlib configuration file - you can find a copy
# of it on your system in
# site-packages/matplotlib/mpl-data/matplotlibrc.  If you edit it
# there, please note that it will be overwritten in your next install.
# If you want to keep a permanent local copy that will not be
# overwritten, place it in HOME/.matplotlib/matplotlibrc (unix/linux
# like systems) and C:\Documents and Settings\yourname\.matplotlib
# (win32 systems).
#
# This file is best viewed in a editor which supports python mode
# syntax highlighting. Blank lines, or lines starting with a comment
# symbol, are ignored, as are trailing comments.  Other lines must
# have the format
#    key : val # optional comment
#
# Colors: for the color values below, you can either use - a
# matplotlib color string, such as r, k, or b - an rgb tuple, such as
# (1.0, 0.5, 0.0) - a hex string, such as ff00ff or #ff00ff - a scalar
# grayscale intensity such as 0.75 - a legal html color name, eg red,
# blue, darkslategray

#### CONFIGURATION BEGINS HERE

# the default backend; one of GTK GTKAgg GTKCairo GTK3Agg GTK3Cairo
# CocoaAgg MacOSX Qt4Agg TkAgg WX WXAgg Agg Cairo GDK PS PDF SVG
# Template
# You can also deploy your own backend outside of matplotlib by
# referring to the module name (which must be in the PYTHONPATH) as
# 'module://my_backend'
backend      : qt4agg

# If you are using the Qt4Agg backend, you can choose here
# to use the PyQt4 bindings or the newer PySide bindings to
# the underlying Qt4 toolkit.
#backend.qt4 : PyQt4        # PyQt4 | PySide

# Note that this can be overridden by the environment variable
# QT_API used by Enthought Tool Suite (ETS); valid values are
# "pyqt" and "pyside".  The "pyqt" setting has the side effect of
# forcing the use of Version 2 API for QString and QVariant.

# The port to use for the web server in the WebAgg backend.
# webagg.port : 8888

# If webagg.port is unavailable, a number of other random ports will
# be tried until one that is available is found.
# webagg.port_retries : 50

# When True, open the webbrowser to the plot that is shown
# webagg.open_in_browser : True

# if you are running pyplot inside a GUI and your backend choice
# conflicts, we will automatically try to find a compatible one for
# you if backend_fallback is True
#backend_fallback: True

#interactive  : False
#toolbar      : toolbar2   # None | toolbar2  ("classic" is deprecated)
#timezone     : UTC        # a pytz timezone string, eg US/Central or Europe/Paris

# Where your matplotlib data lives if you installed to a non-default
# location.  This is where the matplotlib fonts, bitmaps, etc reside
#datapath : /home/jdhunter/mpldata


### LINES
# See http://matplotlib.org/api/artist_api.html#module-matplotlib.lines for more
# information on line properties.
#lines.linewidth   : 1.0     # line width in points
#lines.linestyle   : -       # solid line
#lines.color       : blue    # has no affect on plot(); see axes.color_cycle
#lines.marker      : None    # the default marker
#lines.markeredgewidth  : 0.5     # the line width around the marker symbol
#lines.markersize  : 6            # markersize, in points
#lines.dash_joinstyle : miter        # miter|round|bevel
#lines.dash_capstyle : butt          # butt|round|projecting
#lines.solid_joinstyle : miter       # miter|round|bevel
#lines.solid_capstyle : projecting   # butt|round|projecting
#lines.antialiased : True         # render lines in antialised (no jaggies)

### PATCHES
# Patches are graphical objects that fill 2D space, like polygons or
# circles.  See
# http://matplotlib.org/api/artist_api.html#module-matplotlib.patches
# information on patch properties
#patch.linewidth        : 1.0     # edge width in points
#patch.facecolor        : blue
#patch.edgecolor        : black
#patch.antialiased      : True    # render patches in antialised (no jaggies)

### FONT
#
# font properties used by text.Text.  See
# http://matplotlib.org/api/font_manager_api.html for more
# information on font properties.  The 6 font properties used for font
# matching are given below with their default values.
#
# The font.family property has five values: 'serif' (e.g., Times),
# 'sans-serif' (e.g., Helvetica), 'cursive' (e.g., Zapf-Chancery),
# 'fantasy' (e.g., Western), and 'monospace' (e.g., Courier).  Each of
# these font families has a default list of font names in decreasing
# order of priority associated with them.  When text.usetex is False,
# font.family may also be one or more concrete font names.
#
# The font.style property has three values: normal (or roman), italic
# or oblique.  The oblique style will be used for italic, if it is not
# present.
#
# The font.variant property has two values: normal or small-caps.  For
# TrueType fonts, which are scalable fonts, small-caps is equivalent
# to using a font size of 'smaller', or about 83% of the current font
# size.
#
# The font.weight property has effectively 13 values: normal, bold,
# bolder, lighter, 100, 200, 300, ..., 900.  Normal is the same as
# 400, and bold is 700.  bolder and lighter are relative values with
# respect to the current weight.
#
# The font.stretch property has 11 values: ultra-condensed,
# extra-condensed, condensed, semi-condensed, normal, semi-expanded,
# expanded, extra-expanded, ultra-expanded, wider, and narrower.  This
# property is not currently implemented.
#
# The font.size property is the default font size for text, given in pts.
# 12pt is the standard value.
#
#font.family         : sans-serif
#font.style          : normal
#font.variant        : normal
#font.weight         : medium
#font.stretch        : normal
# note that font.size controls default text sizes.  To configure
# special text sizes tick labels, axes, labels, title, etc, see the rc
# settings for axes and ticks. Special text sizes can be defined
# relative to font.size, using the following values: xx-small, x-small,
# small, medium, large, x-large, xx-large, larger, or smaller
#font.size           : 12.0
#font.serif          : Bitstream Vera Serif, New Century Schoolbook, Century Schoolbook L, Utopia, ITC Bookman, Bookman, Nimbus Roman No9 L, Times New Roman, Times, Palatino, Charter, serif
#font.sans-serif     : Bitstream Vera Sans, Lucida Grande, Verdana, Geneva, Lucid, Arial, Helvetica, Avant Garde, sans-serif
#font.cursive        : Apple Chancery, Textile, Zapf Chancery, Sand, cursive
#font.fantasy        : Comic Sans MS, Chicago, Charcoal, Impact, Western, fantasy
#font.monospace      : Bitstream Vera Sans Mono, Andale Mono, Nimbus Mono L, Courier New, Courier, Fixed, Terminal, monospace

### TEXT
# text properties used by text.Text.  See
# http://matplotlib.org/api/artist_api.html#module-matplotlib.text for more
# information on text properties

#text.color          : black

### LaTeX customizations. See http://www.scipy.org/Wiki/Cookbook/Matplotlib/UsingTex
#text.usetex         : False  # use latex for all text handling. The following fonts
                              # are supported through the usual rc parameter settings:
                              # new century schoolbook, bookman, times, palatino,
                              # zapf chancery, charter, serif, sans-serif, helvetica,
                              # avant garde, courier, monospace, computer modern roman,
                              # computer modern sans serif, computer modern typewriter
                              # If another font is desired which can loaded using the
                              # LaTeX \usepackage command, please inquire at the
                              # matplotlib mailing list
#text.latex.unicode : False # use "ucs" and "inputenc" LaTeX packages for handling
                            # unicode strings.
#text.latex.preamble :  # IMPROPER USE OF THIS FEATURE WILL LEAD TO LATEX FAILURES
                            # AND IS THEREFORE UNSUPPORTED. PLEASE DO NOT ASK FOR HELP
                            # IF THIS FEATURE DOES NOT DO WHAT YOU EXPECT IT TO.
                            # preamble is a comma separated list of LaTeX statements
                            # that are included in the LaTeX document preamble.
                            # An example:
                            # text.latex.preamble : \usepackage{bm},\usepackage{euler}
                            # The following packages are always loaded with usetex, so
                            # beware of package collisions: color, geometry, graphicx,
                            # type1cm, textcomp. Adobe Postscript (PSSNFS) font packages
                            # may also be loaded, depending on your font settings

#text.dvipnghack : None      # some versions of dvipng don't handle alpha
                             # channel properly.  Use True to correct
                             # and flush ~/.matplotlib/tex.cache
                             # before testing and False to force
                             # correction off.  None will try and
                             # guess based on your dvipng version

#text.hinting : 'auto' # May be one of the following:
                       #   'none': Perform no hinting
                       #   'auto': Use freetype's autohinter
                       #   'native': Use the hinting information in the
                       #             font file, if available, and if your
                       #             freetype library supports it
                       #   'either': Use the native hinting information,
                       #             or the autohinter if none is available.
                       # For backward compatibility, this value may also be
                       # True === 'auto' or False === 'none'.
#text.hinting_factor : 8 # Specifies the amount of softness for hinting in the
                         # horizontal direction.  A value of 1 will hint to full
                         # pixels.  A value of 2 will hint to half pixels etc.

#text.antialiased : True # If True (default), the text will be antialiased.
                         # This only affects the Agg backend.

# The following settings allow you to select the fonts in math mode.
# They map from a TeX font name to a fontconfig font pattern.
# These settings are only used if mathtext.fontset is 'custom'.
# Note that this "custom" mode is unsupported and may go away in the
# future.
#mathtext.cal : cursive
#mathtext.rm  : serif
#mathtext.tt  : monospace
#mathtext.it  : serif:italic
#mathtext.bf  : serif:bold
#mathtext.sf  : sans
#mathtext.fontset : cm # Should be 'cm' (Computer Modern), 'stix',
                       # 'stixsans' or 'custom'
#mathtext.fallback_to_cm : True  # When True, use symbols from the Computer Modern
                                 # fonts when a symbol can not be found in one of
                                 # the custom math fonts.

#mathtext.default : it # The default font to use for math.
                       # Can be any of the LaTeX font names, including
                       # the special name "regular" for the same font
                       # used in regular text.

### AXES
# default face and edge color, default tick sizes,
# default fontsizes for ticklabels, and so on.  See
# http://matplotlib.org/api/axes_api.html#module-matplotlib.axes
#axes.hold           : True    # whether to clear the axes by default on
#axes.facecolor      : white   # axes background color
#axes.edgecolor      : black   # axes edge color
#axes.linewidth      : 1.0     # edge linewidth
#axes.grid           : False   # display grid or not
#axes.titlesize      : large   # fontsize of the axes title
#axes.labelsize      : medium  # fontsize of the x any y labels
#axes.labelweight    : normal  # weight of the x and y labels
#axes.labelcolor     : black
#axes.axisbelow      : False   # whether axis gridlines and ticks are below
                              # the axes elements (lines, text, etc)
#axes.formatter.limits : -7, 7 # use scientific notation if log10
                               # of the axis range is smaller than the
                               # first or larger than the second
#axes.formatter.use_locale : False # When True, format tick labels
                                   # according to the user's locale.
                                   # For example, use ',' as a decimal
                                   # separator in the fr_FR locale.
#axes.formatter.use_mathtext : False # When True, use mathtext for scientific
                                     # notation.
#axes.unicode_minus  : True    # use unicode for the minus symbol
                               # rather than hyphen.  See
                               # http://en.wikipedia.org/wiki/Plus_and_minus_signs#Character_codes
#axes.color_cycle    : b, g, r, c, m, y, k  # color cycle for plot lines
                                            # as list of string colorspecs:
                                            # single letter, long name, or
                                            # web-style hex
#axes.xmargin        : 0  # x margin.  See `axes.Axes.margins`
#axes.ymargin        : 0  # y margin See `axes.Axes.margins`

#polaraxes.grid      : True    # display grid on polar axes
#axes3d.grid         : True    # display grid on 3d axes

### TICKS
# see http://matplotlib.org/api/axis_api.html#matplotlib.axis.Tick
#xtick.major.size     : 4      # major tick size in points
#xtick.minor.size     : 2      # minor tick size in points
#xtick.major.width    : 0.5    # major tick width in points
#xtick.minor.width    : 0.5    # minor tick width in points
#xtick.major.pad      : 4      # distance to major tick label in points
#xtick.minor.pad      : 4      # distance to the minor tick label in points
#xtick.color          : k      # color of the tick labels
#xtick.labelsize      : medium # fontsize of the tick labels
#xtick.direction      : in     # direction: in, out, or inout

#ytick.major.size     : 4      # major tick size in points
#ytick.minor.size     : 2      # minor tick size in points
#ytick.major.width    : 0.5    # major tick width in points
#ytick.minor.width    : 0.5    # minor tick width in points
#ytick.major.pad      : 4      # distance to major tick label in points
#ytick.minor.pad      : 4      # distance to the minor tick label in points
#ytick.color          : k      # color of the tick labels
#ytick.labelsize      : medium # fontsize of the tick labels
#ytick.direction      : in     # direction: in, out, or inout


### GRIDS
#grid.color       :   black   # grid color
#grid.linestyle   :   :       # dotted
#grid.linewidth   :   0.5     # in points
#grid.alpha       :   1.0     # transparency, between 0.0 and 1.0

### Legend
#legend.fancybox      : False  # if True, use a rounded box for the
                               # legend, else a rectangle
#legend.isaxes        : True
#legend.numpoints     : 2      # the number of points in the legend line
#legend.fontsize      : large
#legend.borderpad     : 0.5    # border whitespace in fontsize units
#legend.markerscale   : 1.0    # the relative size of legend markers vs. original
# the following dimensions are in axes coords
#legend.labelspacing  : 0.5    # the vertical space between the legend entries in fraction of fontsize
#legend.handlelength  : 2.     # the length of the legend lines in fraction of fontsize
#legend.handleheight  : 0.7     # the height of the legend handle in fraction of fontsize
#legend.handletextpad : 0.8    # the space between the legend line and legend text in fraction of fontsize
#legend.borderaxespad : 0.5   # the border between the axes and legend edge in fraction of fontsize
#legend.columnspacing : 2.    # the border between the axes and legend edge in fraction of fontsize
#legend.shadow        : False
#legend.frameon       : True   # whether or not to draw a frame around legend
#legend.scatterpoints : 3 # number of scatter points

### FIGURE
# See http://matplotlib.org/api/figure_api.html#matplotlib.figure.Figure
#figure.figsize   : 8, 6    # figure size in inches
#figure.dpi       : 80      # figure dots per inch
#figure.facecolor : 0.75    # figure facecolor; 0.75 is scalar gray
#figure.edgecolor : white   # figure edgecolor
#figure.autolayout : False  # When True, automatically adjust subplot
                            # parameters to make the plot fit the figure
#figure.max_open_warning : 20  # The maximum number of figures to open through
                               # the pyplot interface before emitting a warning.
                               # If less than one this feature is disabled.

# The figure subplot parameters.  All dimensions are a fraction of the
# figure width or height
#figure.subplot.left    : 0.125  # the left side of the subplots of the figure
#figure.subplot.right   : 0.9    # the right side of the subplots of the figure
#figure.subplot.bottom  : 0.1    # the bottom of the subplots of the figure
#figure.subplot.top     : 0.9    # the top of the subplots of the figure
#figure.subplot.wspace  : 0.2    # the amount of width reserved for blank space between subplots
#figure.subplot.hspace  : 0.2    # the amount of height reserved for white space between subplots

### IMAGES
#image.aspect : equal             # equal | auto | a number
#image.interpolation  : bilinear  # see help(imshow) for options
#image.cmap   : jet               # gray | jet etc...
#image.lut    : 256               # the size of the colormap lookup table
#image.origin : upper             # lower | upper
#image.resample  : False

### CONTOUR PLOTS
#contour.negative_linestyle :  dashed # dashed | solid

### Agg rendering
### Warning: experimental, 2008/10/10
#agg.path.chunksize : 0           # 0 to disable; values in the range
                                  # 10000 to 100000 can improve speed slightly
                                  # and prevent an Agg rendering failure
                                  # when plotting very large data sets,
                                  # especially if they are very gappy.
                                  # It may cause minor artifacts, though.
                                  # A value of 20000 is probably a good
                                  # starting point.
### SAVING FIGURES
#path.simplify : True   # When True, simplify paths by removing "invisible"
                        # points to reduce file size and increase rendering
                        # speed
#path.simplify_threshold : 0.1  # The threshold of similarity below which
                                # vertices will be removed in the simplification
                                # process
#path.snap : True # When True, rectilinear axis-aligned paths will be snapped to
                  # the nearest pixel when certain criteria are met.  When False,
                  # paths will never be snapped.
#path.sketch : None # May be none, or a 3-tuple of the form (scale, length,
                    # randomness).
                    # *scale* is the amplitude of the wiggle
                    # perpendicular to the line (in pixels).  *length*
                    # is the length of the wiggle along the line (in
                    # pixels).  *randomness* is the factor by which
                    # the length is randomly scaled.

# the default savefig params can be different from the display params
# e.g., you may want a higher resolution, or to make the figure
# background white
#savefig.dpi         : 100      # figure dots per inch
#savefig.facecolor   : white    # figure facecolor when saving
#savefig.edgecolor   : white    # figure edgecolor when saving
#savefig.format      : png      # png, ps, pdf, svg
#savefig.bbox        : standard # 'tight' or 'standard'.
#savefig.pad_inches  : 0.1      # Padding to be used when bbox is set to 'tight'
#savefig.jpeg_quality: 95       # when a jpeg is saved, the default quality parameter.
#savefig.directory   : ~        # default directory in savefig dialog box,
                                # leave empty to always use current working directory

# tk backend params
#tk.window_focus   : False    # Maintain shell focus for TkAgg

# ps backend params
#ps.papersize      : letter   # auto, letter, legal, ledger, A0-A10, B0-B10
#ps.useafm         : False    # use of afm fonts, results in small files
#ps.usedistiller   : False    # can be: None, ghostscript or xpdf
                                          # Experimental: may produce smaller files.
                                          # xpdf intended for production of publication quality files,
                                          # but requires ghostscript, xpdf and ps2eps
#ps.distiller.res  : 6000      # dpi
#ps.fonttype       : 3         # Output Type 3 (Type3) or Type 42 (TrueType)

# pdf backend params
#pdf.compression   : 6 # integer from 0 to 9
                       # 0 disables compression (good for debugging)
#pdf.fonttype       : 3         # Output Type 3 (Type3) or Type 42 (TrueType)

# svg backend params
#svg.image_inline : True       # write raster image data directly into the svg file
#svg.image_noscale : False     # suppress scaling of raster data embedded in SVG
#svg.fonttype : 'path'         # How to handle SVG fonts:
#    'none': Assume fonts are installed on the machine where the SVG will be viewed.
#    'path': Embed characters as paths -- supported by most SVG renderers
#    'svgfont': Embed characters as SVG fonts -- supported only by Chrome,
#               Opera and Safari

# docstring params
#docstring.hardcopy = False  # set this when you want to generate hardcopy docstring

# Set the verbose flags.  This controls how much information
# matplotlib gives you at runtime and where it goes.  The verbosity
# levels are: silent, helpful, debug, debug-annoying.  Any level is
# inclusive of all the levels below it.  If your setting is "debug",
# you'll get all the debug and helpful messages.  When submitting
# problems to the mailing-list, please set verbose to "helpful" or "debug"
# and paste the output into your report.
#
# The "fileo" gives the destination for any calls to verbose.report.
# These objects can a filename, or a filehandle like sys.stdout.
#
# You can override the rc default verbosity from the command line by
# giving the flags --verbose-LEVEL where LEVEL is one of the legal
# levels, eg --verbose-helpful.
#
# You can access the verbose instance in your code
#   from matplotlib import verbose.
#verbose.level  : silent      # one of silent, helpful, debug, debug-annoying
#verbose.fileo  : sys.stdout  # a log filename, sys.stdout or sys.stderr

# Event keys to interact with figures/plots via keyboard.
# Customize these settings according to your needs.
# Leave the field(s) empty if you don't need a key-map. (i.e., fullscreen : '')

#keymap.fullscreen : f               # toggling
#keymap.home : h, r, home            # home or reset mnemonic
#keymap.back : left, c, backspace    # forward / backward keys to enable
#keymap.forward : right, v           #   left handed quick navigation
#keymap.pan : p                      # pan mnemonic
#keymap.zoom : o                     # zoom mnemonic
#keymap.save : s                     # saving current figure
#keymap.quit : ctrl+w, cmd+w         # close the current figure
#keymap.grid : g                     # switching on/off a grid in current axes
#keymap.yscale : l                   # toggle scaling of y-axes ('log'/'linear')
#keymap.xscale : L, k                # toggle scaling of x-axes ('log'/'linear')
#keymap.all_axes : a                 # enable all axes

# Control location of examples data files
#examples.directory : ''   # directory to look in for custom installation

###ANIMATION settings
#animation.writer : ffmpeg         # MovieWriter 'backend' to use
#animation.codec : mp4             # Codec to use for writing movie
#animation.bitrate: -1             # Controls size/quality tradeoff for movie.
                                   # -1 implies let utility auto-determine
#animation.frame_format: 'png'     # Controls frame format used by temp files
#animation.ffmpeg_path: 'ffmpeg'   # Path to ffmpeg binary. Without full path
                                   # $PATH is searched
#animation.ffmpeg_args: ''         # Additional arguments to pass to ffmpeg
#animation.avconv_path: 'avconv'   # Path to avconv binary. Without full path
                                   # $PATH is searched
#animation.avconv_args: ''         # Additional arguments to pass to avconv
#animation.mencoder_path: 'mencoder'
                                   # Path to mencoder binary. Without full path
                                   # $PATH is searched
#animation.mencoder_args: ''       # Additional arguments to pass to mencoder

"""
