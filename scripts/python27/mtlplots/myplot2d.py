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
"""@history 30/08/2011 -- Sebastien E. Bourban 
"""
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
#import matplotlib
import numpy as np
# ~~> matplotlib and pyplot
import matplotlib as mpl
try: hide_default = not sys.stderr.isatty()
except AttributeError: hide_default = True # output does not support isatty()
if hide_default: mpl.use('Agg') # Use of Agg must be done before importing matplotlib.pyplot
import matplotlib.pyplot as plt
import matplotlib.cm as cm                                  # used for colour maps
#from matplotlib.colors import LinearSegmentedColormap
import matplotlib.collections as collections                # used for collections
# ~~> dependencies towards other pytel/modules
from parsers.parserSELAFIN import SELAFIN,getValuePolylineSLF,subsetVariablesSLF,getValuePolyplanSLF
from samplers.meshes import xysLocateMesh,sliceMesh
from parsers.parserStrings import parseArrayFrame,parseArrayPoint,parseArrayPaires,parseArrayGrid

# _____                         ____________________________________
# ____/ Primary Method:Extract /___________________________________/
#

def getKDTree(MESHX,MESHY,IKLE):
   from scipy.spatial import cKDTree
   isoxy = np.column_stack((np.sum(MESHX[IKLE],axis=1)/3.0,np.sum(MESHY[IKLE],axis=1)/3.0))
   return cKDTree(isoxy)

def getMPLTri(MESHX,MESHY,IKLE):
   from matplotlib.tri import Triangulation
   mpltri = Triangulation(MESHX,MESHY,IKLE).get_cpp_triangulation()
   return mpltri.get_neighbors(),mpltri.get_edges()

def whatTimeSLF(instr,ctimes):
   # instr ~ what['time']: list of frames or (times,) delimited by ';'
   # ctimes ~ slf.tags['cores']: list of time
   t = parseArrayFrame(instr,len(ctimes))
   if len(t) != 1: print '... the time definition should only have one frame in this case. It is: ',instr,'. I will assume you wish to plot the last frame.'
   return [ t[len(t)-1] ]
   
def whatVarsSLF(instr,vnames):
# instr ~ what['vars']: list of pairs "variable:support" delimited by ';'
# vnames ~ slf.VARNAMES: list of variables names from the SELAFIN file
   vars = []; vtypes = []
   for var in instr.split(';'):
      v,vtype = var.split(':')
      vars.append( v ); vtypes.append( vtype )
   return subsetVariablesSLF(';'.join(vars),vnames),vtypes

def whatSample(inspl,box):
   grids = []
   for grid in parseArrayGrid(inspl,box):
      if grid[0][0] == grid[1][0]:
         print '... same min(x)=',grid[0][0],' and max(x)=',grid[1][0],' values in: ',inspl,'. I cannot create a box.'
         continue # TODO: add exception
      if grid[0][1] == grid[1][1]:
         print '... same min(y)=',grid[0][1],' and max(y)=',grid[1][1],' values in: ',inspl,'. I cannot create a box.'
         continue # TODO: add exception
      mx,my = np.meshgrid(np.linspace(grid[0][0], grid[1][0], grid[2][0]+1),np.linspace(grid[0][1], grid[1][1], grid[2][1]+1))
      grids.append((len(mx[0]),len(mx),np.concatenate(mx),np.concatenate(my)))
   return grids

def setQuad(grids):
   MESHX = []; MESHY = []
   IKLE = []
   for dimx,dimy,x,y in grids:
      IKLE.extend(([ i+j*dimx,i+1+j*dimx,i+1+(j+1)*dimx,i+(j+1)*dimx ] for j in range(dimy-1) for i in range(dimx-1) ))
      MESHX.extend(x)
      MESHY.extend(y)
   return np.array(IKLE),np.array(MESHX),np.array(MESHY)

def splitQuad2Triangle(IKLE):
   # split each quad into triangles
   return np.delete(np.concatenate((IKLE,np.roll(IKLE,2,axis=1))),np.s_[3::],axis=1)

def extractVSectionSELAFIN(what,slf):

   # what['extract']: could be list delimited by ';', of:
   #    + points (x;y),
   #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by ';'
   #      where n is a plan number and d is depth from that plane (or the surface by default)
   xyo = []; zpo = []
   for xyi,zpi in parseArrayPoint(what["extract"],slf.NPLAN):
      if xyi == []:
         print '... I could not find anything to extract in "',what["extract"].strip(),'" as support for the cross section.'
         sys.exit(1)
      if type(xyi) == type(()): xyo.append(xyi)
      else: xyo.append( (slf.MESHX[xyi],slf.MESHY[xyi]) )
      for p in zpi:                         # /!\ common deinition of plans
         if p not in zpo: zpo.append(p)     # /!\ only allowing plans for now

   # ~~> Extract horizontal cross MESHX
   xys,support2d = sliceMesh(xyo,slf.IKLE2,slf.MESHX,slf.MESHY,slf.tree)
   support3d = []
   for s2d in support2d: support3d.append( (s2d,zpo) )   # common vertical definition to all points
   # Distance d-axis
   distot = 0.0
   d = [ distot ]
   for xy in range(len(xys)-1):
      distot += np.sqrt( np.power(xys[xy+1][0]-xys[xy][0],2) + np.power(xys[xy+1][1]-xys[xy][1],2) )
      d.append(distot)
   MESHX = np.repeat(d,len(zpo))
   
   # ~~>  Extract MESHZ for only one time frame
   varz = subsetVariablesSLF('z',slf.VARNAMES)
   t = whatTimeSLF(what['time'],slf.tags['cores'])
   MESHZ = np.ravel( getValuePolylineSLF(slf.file,slf.tags,t,support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,varz)[0][0].T )

   # ~~>  Connect with IKLE, keeping quads
   IKLE = []
   for j in range(len(d)-1):
      for i in range(len(zpo)-1):
         IKLE.append([ i+j*len(zpo),i+(j+1)*len(zpo),i+1+(j+1)*len(zpo),i+1+j*len(zpo) ])
   IKLE = np.array(IKLE)

   return IKLE,MESHX,MESHZ, support3d

def extractPlansSELAFIN(what,slf):

   # what['extract']: could be list delimited by ';', of:
   #    + empty spatial location [],
   #    + bundled within []#n or []@d#n, delimited by ';'
   #      where n is a plan number and d is depth from that plane (or the surface by default)
   xyo = []; zpo = []
   for xyi,zpi in parseArrayPoint(what["extract"],slf.NPLAN):
      #if xyi != []:
      #   print '... I will assume that all 2D nodes are considered at this stage.'
      #   sys.exit(1)
      for p in zpi:                         # /!\ common deinition of plans
         if p not in zpo: zpo.append(p)     # /!\ only allowing plans for now

   return slf.IKLE2,slf.MESHX,slf.MESHY, zpo

def extractVarsorSELAFIN(what,var,slf):
   
   t = whatTimeSLF(what['time'],slf.tags['cores'])
   # what['vars']: list of pairs variables:support2d delimited by ';'
   vars = subsetVariablesSLF(var,slf.VARNAMES)
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

   return VARSORS

# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def drawMesh2DElements(myplt,elements):

   #  *2DElements: draw individual elements polygons  (triangle or quads)
   # ~~> Focus on current subplot / axes instance
   crax = myplt.gca()
   # ~~>  Collections
   colection = collections.PolyCollection(
      elements, cmap=cm.jet, antialiaseds=0, # norm=myplt.Normalize()
      edgecolors = 'k', linewidth=1, facecolors = 'none')
      #colection.set_array(val)       # each triangle colour dependent on its value from its verticies
   # ~~> Plot data
   #ex: fig = myplt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
   crax.add_collection(colection)   # adds, or plots our collection
   
   return

def drawMeshLines(myplt,edges):

   #  *Lines: draw individual edges
   # TODO: Find a way to do colours properly -- and complete drawColouredMeshLines
   # ~~> Focus on current subplot / axes instance
   crax = myplt.gca()
   # ~~>  Collections
   colection = collections.LineCollection(
      edges, antialiaseds=0,  # cmap=cm.jet ,norm=self.plt.Normalize(),
      linewidth=1 ) #colors = 'k',
   #colection.set_zorder(deco['zorder'])
   #colection.set_array(val)       # each element colour dependent on its value from its verticies
   # ~~> Plot data
   #ex: fig = self.plt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
   crax.add_collection(colection, autolim=True)   # adds, or plots our collection

   return

def drawColouredTriMaps(myplt,(x,y,ikle,z)):

   # ~~> Plot data
   colourmap = cm.jet
   #if geometry.has_key('cmapPlot'):
   #   colourmap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
   zmin = np.min(z); zmax = np.max(z)
   cs = myplt.tricontour(x,y,ikle, z, linewidths=0.5, colors='k')
   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   myplt.tricontourf(x,y,ikle, z, cmap=colourmap)
   # adds numbers along the iso-contours
   myplt.clabel(cs,fontsize=9,inline=1)

   return

def drawLabeledTriContours(myplt,(x,y,ikle,z)):

   # Convert negative dashed default contour lines by solids
   # matplotlib.rcParams['contour.negative_linestyle'] = 'solid'

   # contour levels based on z values
   zmin = np.min(z); zmax = np.max(z)
   levels = np.arange(zmin,zmax,(zmax-zmin)/8)

   # + the value (or array) after the z are the levels
   #    ... there are 6 levels by default
   # + colors takes the z-values by default.
   #    colors='k' set the contours to black
   # TODO:   colors=('r','green','blue',(1,1,0),'#afeeee','0.5')
   # + linewidths defines the width of the contour lines
   #    linewidths=0.5, sets all contours' width to 0.5
   #    linewidths=np.arange(.5,4,.5), sets a variation in the width
   # ex: self.plt.tricontour(x,y,ikle, z, linewidths=0.5, colors='k')
   # ex:                     , linewidths=np.arange(.5, 4, .5),
   # ex: colors=('r', 'green', 'blue', (1,1,0), '#afeeee', '0.5')
   try:
      cs = myplt.tricontour( x,y,ikle, z , levels, linewidths=np.arange(.5,4,.5))
   except:
      print '\ntricontour not available on your system'
      print ' ... unable to plot 2D figures'

   # + inline of clabel() can be either:
   #    0 (the label is written on top of the contour)
   #    1 (the label makes a whole in the contour)
   # + colour is the same as cs by default, but it can be reset
   # + levels[1::2], one every two labels shown
   # + fmt='%1.1f',
   # ex: self.plt.clabel(cs, inline=1, fontsize=6, colors='k')
   # ex: self.plt.clabel(cs, levels[1::2], fmt='%1.1f', inline=1, fontsize=9)
   #   np.arange(-1.2, 1.6, 0.2)
   #levels = np.asarray([-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025])
   myplt.clabel(cs, levels[1::2], fmt='%1.3f', inline=1, fontsize=9, colors='k')

   # Extras: thicken the zero contour.
   myplt.setp( cs.collections[6], linewidth=4 )
   # TODO: investigate use of extent ( extent=(-3,3,-2,2) )

   # make a colorbar for the contour lines
   cb = myplt.colorbar(cs, shrink=0.8, extend='both')
   #self.plt.hot()  # Now change the colormap for the contour lines and colorbar
   #self.plt.flag()

   # We can still add a colorbar for the image, too.
   #cbi = self.plt.colorbar(im, orientation='horizontal', shrink=0.8)

   # This makes the original colorbar look a bit out of place,
   # so let's improve its position.

   #l,b,w,h = self.plt.gca().get_position().bounds
   #ll,bb,ww,hh = CB.ax.get_position().bounds
   #CB.ax.set_position([ll, b+0.1*h, ww, h*0.8])

   # Extras: Images
   #im = self.plt.imshow(Z, interpolation='bilinear', origin='lower',
   #                cmap=cm.gray, extent=(-3,3,-2,2))
   #levels = np.arange(-1.2, 1.6, 0.2)
   #CS = self.plt.contour(Z, levels,origin='lower',linewidths=2,extent=(-3,3,-2,2))
   #CB = self.plt.colorbar(CS, shrink=0.8, extend='both')
   #CBI = self.plt.colorbar(im, orientation='horizontal', shrink=0.8)
   #l,b,w,h = self.plt.gca().get_position().bounds
   #ll,bb,ww,hh = CB.ax.get_position().bounds
   #CB.ax.set_position([ll, b+0.1*h, ww, h*0.8])

   return

def drawColouredTriVects(myplt,(x,y,uv,normalised)):

   # ~~> Plot data
   colourmap = cm.jet
   #if geometry.has_key('cmapPlot'):
   #   colourmap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
   # get vector magnitude, i.e norm-2
   z = np.sqrt(np.sum(np.power(np.dstack(uv[0:2])[0],2),axis=1))
   zmin = np.min(z); zmax = np.max(z)
   if not normalised : cs = myplt.quiver(x,y,uv[0],uv[1], cmap=colourmap )
   else: cs = myplt.quiver(x,y,uv[0],uv[1], cmap=colourmap, norm=myplt.Normalize(zmin,zmax))
   cs.set_array(z)
   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   # adds numbers along the iso-contours

   return

# _____                      _______________________________________
# ____/ Primary Method:Deco /______________________________________/
#

def deco(myplt,upar,x0,y0):

   # ~~ Ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #crax.axis('equal')         # sets both axis scale to be equal
   crax = myplt.gca()
   crax.set_aspect('equal')

   # ~~ Axis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # TODO: Replace white margins
   if not upar.has_key("roi"):
      xmin = np.min(x0); xmax = np.max(x0)
      ymin = np.min(y0); ymax = np.max(y0)
      if upar.has_key("setroi"):
         xgap = max(upar["setroi"][1],xmax) - min(upar["setroi"][0],xmin)
         xmin -= 0.02*xgap; xmax += 0.02*xgap
         ygap = max(upar["setroi"][3],ymax) - min(upar["setroi"][2],ymin)
         ymin -= 0.02*ygap; ymax += 0.02*ygap
         upar["setroi"] = [xmin,xmax,ymin,ymax]
      else:
         xgap = xmax-xmin
         xmin -= 0.02*xgap; xmax += 0.02*xgap
         ygap = ymax-ymin
         ymin -= 0.02*ygap; ymax += 0.02*ygap
         upar.update({ "setroi":[xmin,xmax,ymin,ymax] })
   if upar.has_key("setroi"): myplt.axis(upar["setroi"])

   #curax.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title

   #if geometry.has_key('cmapPlot'): fig.colorbar(colection)     # sets up colourbar
   #if geometry.has_key('cmapPlot'): fig.colorbar(colormap)     # sets up colourbar

   return

# _____                         ____________________________________
# ____/ Primary Classes:Dumper /___________________________________/
#

class Dumper2D:

   def __init__(self,task):
      self.data = None
      if True: # TODO: raise exception
         print '... do not know how to save in this format: ' + task['outFormat']

   def add(self,typl,what):
      # ~~> SELAFIN file
      pass
   
   def save(self,fileName):
      # ~~> SELAFIN file
      pass

# _____                          ___________________________________
# ____/ Primary Classes: Figure /__________________________________/
#

class Figure2D:

   def __init__(self,plot):

      # ~~~ melting the pot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.mpar = {} # mpar, contains the matplotlib parameters (defaults)
      self.upar = {} # upar, contains the user parameters (from the XML)
      for name in ['look','data']:
         if plot['deco'].has_key(name): self.upar.update(plot['deco'][name][0])

      # ~~~ special conversions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Replaces the relevat mpar by the upar values
      for key in self.upar.keys(): # /!\ the .keys() is necessary
         if key in mpl.rcParams:
            if type(mpl.rcParams[key]) == type([]):
               if type(mpl.rcParams[key][0]) == type(1) or type(mpl.rcParams[key][0]) == type(1.0):
                  self.mpar[key] = parseArrayFrame(self.upar[key])
                  del self.upar[key]
               #elif type(mpl.rcParams[key][0]) == type(""): pass
               else:
                  print '... I did not know ',type(mpl.rcParams[key]),' could be an acceptable type'
                  sys.exit(1)
            elif type(mpl.rcParams[key]) == type(1):
               self.mpar[key] = int(self.upar[key])
               del self.upar[key]
            elif type(mpl.rcParams[key]) == type(1.0):
               self.mpar[key] = float(self.upar[key])
               del self.upar[key]
            elif type(mpl.rcParams[key]) == type(""):
               self.mpar[key] = self.upar[key]
               del self.upar[key]
            else:
               print '... I did not know ',type(mpl.rcParams[key]),' could be an acceptable type'
               sys.exit(1)
         elif self.upar[key] != '':
            if key == "roi": self.upar[key] = parseArrayPaires(self.upar[key])
            elif key == "dpi":
               self.mpar.update({ 'savefig.dpi': int(self.upar[key]) })
               self.mpar.update({ 'figure.dpi': int(self.upar[key]) })
               del self.upar[key]
            elif key == "size":
               self.mpar.update({ 'figure.figsize': parseArrayFrame(self.upar[key]) })
               del self.upar[key]
            else: del self.upar[key]

      # ~~> by default, there is no grid in 1D
      #self.mpar.update({ 'grid.alpha':1.0 })

      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      mpl.rcParams.update(self.mpar) # TODO: do this when you draw
      # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      fig = plt.figure() # TODO: do this when you draw
      # ~~> add_subplot, or ax definition
      fig.add_subplot(111)
      # fig.add_subplot(111) returns an Axes instance, where we can plot and this is
      #   also the reason why we call the variable referring to that instance ax.
      #   This is a common way to add an Axes to a Figure, but add_subplot() does a
      #   bit more: it adds a subplot. So far we have only seen a Figure with one Axes
      #   instance, so only one area where we can draw, but Matplotlib allows more than one
      
      # ~~> user params
      if plot.has_key('title'): plt.title(plot['title'])
      # ~~> type of plot
      if plot.has_key('type'):
         if len(plot['type']) > 0 and ((plot['type'][1] == 'line')or(plot['type'][1] == 'rose')):
            fig.add_subplot(adjustable='datalim')
         else:
            fig.add_subplot(aspect='equal')
      else: fig.add_subplot(aspect='equal')
      # ~~> self.plt.axis
      if self.upar.has_key("roi"):
         xmin = min(self.upar["roi"][0][0],self.upar["roi"][1][0])
         xmax = max(self.upar["roi"][0][0],self.upar["roi"][1][0])
         ymin = min(self.upar["roi"][0][1],self.upar["roi"][1][1])
         ymax = max(self.upar["roi"][0][1],self.upar["roi"][1][1])
         plt.axis([xmin,xmax,ymin,ymax])

      self.plt = plt 
      self.fig = fig

   def add(self,typl,what):

      # /!\ WACLEO: Temporary fix because TOMAWAC's IOs names are not yet standard TELEMAC
      if 'WACLEO' in typl.upper() or \
         'SELAFIN' in typl.upper() or \
         'slf' in typl.lower():

         # ~~> Load data
         slf = SELAFIN(what['file'])
         slf.setKDTree()
         slf.setMPLTri()
         
         if what['type'] == 'v-section':
            
            # ~~> Extract data
            elements = []
            IKLE4,MESHX,MESHZ, support3d = extractVSectionSELAFIN(what,slf)
            IKLE3 = splitQuad2Triangle(IKLE4)
            vars,vtypes = whatVarsSLF(what['vars'],slf.VARNAMES)
            time = whatTimeSLF(what['time'],slf.tags['cores'])
            tree = getKDTree(MESHX,MESHZ,IKLE3)
            tria = getMPLTri(MESHX,MESHZ,IKLE3)[0]
            data = getValuePolylineSLF(slf.file,slf.tags,time,support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars)

            # ~~> Possible sampling of the data
            if what["sample"] != '':
               supMESHX = MESHX; supMESHZ = MESHZ
               MESHX = []; MESHZ = []
               IKLE4 = []; support2d = []
               grids = whatSample(what["sample"],[(min(supMESHX),min(supMESHZ)),(max(supMESHX),max(supMESHZ))])
               for dimx,dimy,x,y in grids:
                  for xyi in np.dstack((x,y))[0]:
                     support2d.append( xysLocateMesh(xyi,IKLE3,supMESHX,supMESHZ,tree,tria) )
                  IKLE4.extend(([ i+j*dimx,i+1+j*dimx,i+1+(j+1)*dimx,i+(j+1)*dimx ] for j in range(dimy-1) for i in range(dimx-1) ))
                  MESHX.extend(x)
                  MESHZ.extend(y)
               IKLE4 = np.asarray(IKLE4)
               IKLE3 = splitQuad2Triangle(IKLE4)
               MESHX = np.asarray(MESHX)
               MESHZ = np.asarray(MESHZ)

            # ~~> Loop on variables
            for v,vtype in zip(vars,vtypes):

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
                  if elements == []: elements = np.dstack((MESHX[IKLE4],MESHZ[IKLE4]))
                  # ~~> Draw/Dump (works with triangles and quads)
                  drawMesh2DElements(self.plt,elements)
                  deco(self.plt,self.upar,MESHX,MESHZ)
               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               else:
                  VARSORS = []
                  for ivar in range(len(vars[0])): VARSORS.append( np.ravel(data[ivar][0].T) )

                  # ~~> Re-sampling
                  if what["sample"] != '':
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
                  if "map" in vtype: drawColouredTriMaps(self.plt,(MESHX,MESHZ,IKLE3,VARSORS[0]))
                  if "label" in vtype: drawLabeledTriContours(self.plt,(MESHX,MESHZ,IKLE3,VARSORS[0]))
                  if "arrow" in vtype: drawColouredTriVects(self.plt,(MESHX,MESHZ,VARSORS,False))
                  if "angle" in vtype: drawColouredTriVects(self.plt,(MESHX,MESHZ,VARSORS,True))
                  deco(self.plt,self.upar,np.ravel(MESHX[IKLE3]),np.ravel(MESHZ[IKLE3]))

         else: # if what['type'] != 'v-section'

            # ~~> Extract data
            elements = []
            IKLE3,MESHX,MESHY, zpo = extractPlansSELAFIN(what,slf)
            if slf.NDP2 == 4: IKLE3 = splitQuad2Triangle(IKLE3)
            IKLE4 = IKLE3
            vars,vtypes = whatVarsSLF(what['vars'],slf.VARNAMES)
            time = whatTimeSLF(what['time'],slf.tags['cores'])
            tree = slf.tree
            tria = slf.neighbours
            data = getValuePolyplanSLF(slf.file,slf.tags,time,zpo,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars)

            # ~~> Possible re-sampling
            support2d = []
            if what["sample"] != '':
               supMESHX = MESHX; supMESHY = MESHY
               MESHX = []; MESHY = []
               IKLE4 = []; support2d = []
               grids = whatSample(what["sample"],[(min(supMESHX),min(supMESHY)),(max(supMESHX),max(supMESHY))])
               for dimx,dimy,x,y in grids:
                  for xyi in np.dstack((x,y))[0]:
                     support2d.append( xysLocateMesh(xyi,IKLE3,supMESHX,supMESHY,tree,tria) )
                  IKLE4.extend(([ i+j*dimx,i+1+j*dimx,i+1+(j+1)*dimx,i+(j+1)*dimx ] for j in range(dimy-1) for i in range(dimx-1) ))
                  MESHX.extend(x)
                  MESHY.extend(y)
               IKLE4 = np.asarray(IKLE4)
               IKLE3 = splitQuad2Triangle(IKLE4)
               MESHX = np.asarray(MESHX)
               MESHY = np.asarray(MESHY)
            
            # ~~> Loop on variables
            for var in what["vars"].split(';'):
               v,vtype = var.split(':')

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               # ~~> Draw triangles and quads
               if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
                  if elements == []: elements = np.dstack((MESHX[IKLE4],MESHY[IKLE4]))
                  # ~~> Draw/Dump (works with triangles and quads)
                  drawMesh2DElements(self.plt,elements)
                  deco(self.plt,self.upar,MESHX,MESHY)

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               # ~~> Extract variable data for only one time frame and one plane
               else:
                  VARSORS = extractVarsorSELAFIN(what,v,slf)
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
                  if "map" in vtype: drawColouredTriMaps(self.plt,(MESHX,MESHY,IKLE3,VARSORS[0]))
                  elif "label" in vtype: drawLabeledTriContours(self.plt,(MESHX,MESHY,IKLE3,VARSORS[0]))
                  elif "arrow" in vtype: drawColouredTriVects(self.plt,(MESHX,MESHY,VARSORS,False))
                  elif "angle" in vtype: drawColouredTriVects(self.plt,(MESHX,MESHY,VARSORS,True))
                  else: print '... do not know how to draw this SELAFIN type: ' + vtype
                  deco(self.plt,self.upar,np.ravel(MESHX[IKLE3]),np.ravel(MESHY[IKLE3]))

         slf.file['hook'].close()

      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to extract from this format: ' + typl

   def show(self): self.plt.show()
   def save(self,fileName): self.plt.savefig(fileName)

# _____               ______________________________________________
# ____/ Mesh Toolbox /_____________________________________________/
#
#drawMesh* applies to mesh of triangles / quads
#   - by default this is drawn as a uni-colour wireframe
#   - see also drawCoulouredMesh*
#Note: triplot could be used but would not be valid for quads

   def drawColouredMeshLines(self,edges,deco): pass

# _____               ______________________________________________
# ____/ Grid Toolbox /_____________________________________________/
#
#   drawGrid* applies to regular i,j-grids
#      - by default this is drawn as a uni-colour wireframe
#      - see also drawCoulouredGrid*
#   *Cells: draw individual cell squares

#   drawGrid* applies to regular i,j-grids
#      - by default this is drawn as a uni-colour wireframe
#      - see also drawGridContours, drawColouredGridContours, drawLabeledGridContours, etc.
#   *Contours: draw iso-value contours

def drawGridContours(plt,(x,y,z),deco):

   # ~~> Focus on current subplot / axes instance
   #crax = plt.gca()

   # ~~ Split contours in major/minor classes ~~~~~~~~~~~~~~~~~~~~~~

   # ~~> Draw major contours
   cs1 = plt.contour(x,y,z, levels[::2], colors = deco['contour.major.color'], hold='on')
   for c in cs1.collections:
      c.set_linestyle(deco['contour.major.style'])
      c.set_zorder(zorder)

   # ~~> Draw minor contours
   cs2 = plt.contour(x,y,z, levels[1::2], colors = deco['contour.minor.color'], hold='on')
   for c in cs2.collections:
      c.set_linestyle(deco['contour.minor.style'])
      c.set_zorder(zorder)

    # label every 4th level
   inlineLabelSize = int(hrwd['inline.label.size'])
   CS4 = plt.clabel(CS2, CS2.levels[1::2],
                    inline=1,
                    fmt='%'+hrwd['inline.label.fmt'],
                    fontsize=inlineLabelSize)

   return

# _____               ______________________________________________
# ____/ Mesh Toolbox /_____________________________________________/
#
#   Contour plot of a Z based on a triangular mesh a with labels.

def drawColouredQuadMaps(plt,(nelem,npoin,ndp,nplan),(x,y,ikle,z),deco):

   # ~~> Focus on current subplot / axes instance
   crax = plt.gca()
   # ~~> Plot data
   colourmap = cm.jet
   #if 'cmapPlot' in geometry:
   #   colourmap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
   zmin = np.min(z); zmax = np.max(z)

   nx = npoin-nelem
   ny = int(npoin/nx)
   mesh = np.column_stack((x,y))
   print nx*ny,len(x),len(mesh)
   msh = collections.QuadMesh(nx-1,ny-1,mesh,True,shading='gouraud')
   #msh.set_array(np.zeros(self.x_cells*self.y_cells))
   #msh.set_array(np.array(self.FD.GetTimestepData(0)))
   #msh.set_clim(0.0, 1.0)
   #axis.axis([0, self.x_max, 0, self.y_top])
   #plt.colorbar(self.cax)
   ###!!! I have tried cax and msh, and various combos
   #toolbar.show()
   #canvas.draw()
   msh.set_array(z)
   crax.add_collection(msh)

   #cs = plt.tricontour(x,y,ikle, z, linewidths=0.5, colors='k')
   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   #plt.tricontourf(x,y,ikle, z, cmap=colourmap)
   # adds numbers along the iso-contours
   #plt.clabel(cs,fontsize=9,inline=1)
   xmin = min(deco['roi'][0][0],deco['roi'][1][0])
   xmax = max(deco['roi'][0][0],deco['roi'][1][0])
   ymin = min(deco['roi'][0][1],deco['roi'][1][1])
   ymax = max(deco['roi'][0][1],deco['roi'][1][1])
   crax.set_xlim(xmin-0.5,xmax+0.5)   # sets x axis limits, default 0-1
   crax.set_ylim(ymin-0.5,ymax+0.5)   # sets y axis limits, default 0-1
   crax.axis('equal')         # sets both axis scale to be equal


   #mp.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title
   #if 'cmapPlot' in geometry: fig.colorbar(colection)     # sets up colourbar
   #if 'cmapPlot' in geometry: fig.colorbar(colormap)     # sets up colourbar

   return


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

   sys.exit(0)
