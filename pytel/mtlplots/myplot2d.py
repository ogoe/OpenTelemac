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
import matplotlib.cm as cm                                  # used for colour maps
#from matplotlib.colors import LinearSegmentedColormap
import matplotlib.collections as collections                # used for collections

# _____               ______________________________________________
# ____/ Grid Toolbox /_____________________________________________/
#
"""
   drawGrid* applies to regular i,j-grids
      - by default this is drawn as a uni-colour wireframe
      - see also drawCoulouredGrid*
   *Cells: draw individual cell squares
"""

"""
   drawGrid* applies to regular i,j-grids
      - by default this is drawn as a uni-colour wireframe
      - see also drawGridContours, drawColouredGridContours, drawLabeledGridContours, etc.
   *Contours: draw iso-value contours
"""
def drawGridContours(plt,(x,y,z),deco):

   # ~~> Focus on current subplot / axes instance
   #crax = plt.gca()

   # ~~ Split contours in major/minor classes ~~~~~~~~~~~~~~~~~~~~~~

   # ~~> Draw major contours
   cs1 = plt.contour(x,y,z, levels[::2], colors = deco['contour.major.color'], hold='on')
   for c in cs2.collections:
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
"""
   drawMesh* applies to mesh of triangles / quads
      - by default this is drawn as a uni-colour wireframe
      - see also drawCoulouredMesh*
   Note: triplot could be used but would not be valid for quads
"""

#  *2DElements: draw individual elements polygons  (triangle or quads)
def drawMesh2DElements(plt,elements,deco):

   # ~~> Focus on current subplot / axes instance
   crax = plt.gca()
   # ~~>  Collections
   colection = collections.PolyCollection(
      elements, cmap=cm.jet, antialiaseds=0, # norm=plt.Normalize()
      edgecolors = 'k', linewidth=1, facecolors = 'none')
      #colection.set_array(val)       # each triangle colour dependent on its value from its verticies
   # ~~> Plot data
   #ex: fig = plt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
   crax.add_collection(colection)   # adds, or plots our collection
   xmin = deco['roi'][0][0]; xmax = deco['roi'][1][0]
   ymin = deco['roi'][0][1]; ymax = deco['roi'][1][1]
   if deco.has_key('crax.xlim'):
      xmin -= deco['crax.xlim']; xmax += deco['crax.xlim']
   if deco.has_key('crax.ylim'):
      ymin -= deco['crax.ylim']; ymin += deco['crax.ylim']
   crax.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
   crax.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
   #plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
   crax.axis('equal')         # sets both axis scale to be equal
   #curax.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title

   return

#  *Lines: draw individual edges
# TODO: Find a way to do colours properly -- and complete drawColouredMeshLines

def drawMeshLines(plt,edges,deco):

   # ~~> Focus on current subplot / axes instance
   crax = plt.gca()
   # ~~>  Collections
   colection = collections.LineCollection(
      edges, antialiaseds=0, # cmap=cm.jet, norm=plt.Normalize()
      linewidth=1 ) #colors = 'k', 
   #colection.set_zorder(deco['zorder'])

   #colection.set_array(val)       # each element colour dependent on its value from its verticies

   # ~~> Plot data
   #ex: fig = plt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
   crax.add_collection(colection, autolim=True)   # adds, or plots our collection
   xmin = deco['roi'][0][0]; xmax = deco['roi'][1][0]
   ymin = deco['roi'][0][1]; ymax = deco['roi'][1][1]
   if deco.has_key('crax.xlim'):
      xmin -= deco['crax.xlim']; xmax += deco['crax.xlim']
   if deco.has_key('crax.ylim'):
      ymin -= deco['crax.ylim']; ymin += deco['crax.ylim']
   crax.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
   crax.set_ylim(ymim,ymax)   # sets y axis limits, default 0-1
   crax.axis('equal')         # sets both axis scale to be equal

   return

def drawColouredMeshLines(plt,edges,deco):

   return

"""
   Contour plot of a Z based on a triangular mesh a with labels.
"""
def drawLabeledTriContours(plt,(x,y,ikle,z),deco):

   # ~~> Focus on current subplot / axes instance
   crax = plt.gca()

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
   # ex: plt.tricontour(x,y,ikle, z, linewidths=0.5, colors='k')
   # ex:                     , linewidths=np.arange(.5, 4, .5),
   # ex: colors=('r', 'green', 'blue', (1,1,0), '#afeeee', '0.5')
   cs = plt.tricontour( x,y,ikle, z , levels, linewidths=np.arange(.5,4,.5))
   
   # + inline of clabel() can be either:
   #    0 (the label is written on top of the contour)
   #    1 (the label makes a whole in the contour)
   # + colour is the same as cs by default, but it can be reset
   # + levels[1::2], one every two labels shown
   # + fmt='%1.1f',
   # ex: plt.clabel(cs, inline=1, fontsize=6, colors='k')
   # ex: plt.clabel(cs, levels[1::2], fmt='%1.1f', inline=1, fontsize=9)
   #   np.arange(-1.2, 1.6, 0.2)
   #levels = np.asarray([-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025])
   plt.clabel(cs, levels[1::2], fmt='%1.3f', inline=1, fontsize=9, colors='k')

   # Extras: thicken the zero contour.
   plt.setp( cs.collections[6], linewidth=4 )
   # TODO: investigate use of extent ( extent=(-3,3,-2,2) )

   # make a colorbar for the contour lines
   cb = plt.colorbar(cs, shrink=0.8, extend='both')
   #plt.hot()  # Now change the colormap for the contour lines and colorbar
   #plt.flag()

   # We can still add a colorbar for the image, too.
   #cbi = plt.colorbar(im, orientation='horizontal', shrink=0.8)

   # This makes the original colorbar look a bit out of place,
   # so let's improve its position.

   #l,b,w,h = plt.gca().get_position().bounds
   #ll,bb,ww,hh = CB.ax.get_position().bounds
   #CB.ax.set_position([ll, b+0.1*h, ww, h*0.8])

   # Extras: Images
   #im = plt.imshow(Z, interpolation='bilinear', origin='lower',
   #                cmap=cm.gray, extent=(-3,3,-2,2))
   #levels = np.arange(-1.2, 1.6, 0.2)
   #CS = plt.contour(Z, levels,origin='lower',linewidths=2,extent=(-3,3,-2,2))
   #CB = plt.colorbar(CS, shrink=0.8, extend='both')
   #CBI = plt.colorbar(im, orientation='horizontal', shrink=0.8)
   #l,b,w,h = plt.gca().get_position().bounds
   #ll,bb,ww,hh = CB.ax.get_position().bounds
   #CB.ax.set_position([ll, b+0.1*h, ww, h*0.8])

   return

def drawColouredTriMaps(plt,(x,y,ikle,z),deco):

   # ~~> Focus on current subplot / axes instance
   crax = plt.gca()
   # ~~> Plot data
   colourmap = cm.jet
   #if geometry.has_key('cmapPlot'):
   #   colourmap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
   zmin = np.min(z); zmax = np.max(z)
   cs = plt.tricontour(x,y,ikle, z, linewidths=0.5, colors='k')
   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   plt.tricontourf(x,y,ikle, z, cmap=colourmap)
   # adds numbers along the iso-contours
   plt.clabel(cs,fontsize=9,inline=1)
   xmin = deco['roi'][0][0]; xmax = deco['roi'][1][0]
   ymin = deco['roi'][0][1]; ymax = deco['roi'][1][1]
   if deco.has_key('crax.xlim'):
      xmin -= deco['crax.xlim']; xmax += deco['crax.xlim']
   if deco.has_key('crax.ylim'):
      ymin -= deco['crax.ylim']; ymin += deco['crax.ylim']
   crax.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
   crax.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
   crax.axis('equal')         # sets both axis scale to be equal
   #mp.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title
   #if geometry.has_key('cmapPlot'): fig.colorbar(colection)     # sets up colourbar
   #if geometry.has_key('cmapPlot'): fig.colorbar(colormap)     # sets up colourbar

   return

def drawColouredTriVects(plt,(x,y,uv,normalised),deco):

   # ~~> Focus on current subplot / axes instance
   crax = plt.gca()
   # ~~> Plot data
   colourmap = cm.jet
   #if geometry.has_key('cmapPlot'):
   #   colourmap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
   # get vector magnitude, i.e norm-2
   z = np.sqrt(np.sum(np.power(np.dstack(uv[0:2])[0],2),axis=1))
   zmin = np.min(z); zmax = np.max(z)
   if not normalised : cs = plt.quiver(x,y,uv[0],uv[1], cmap=colourmap )
   else: cs = plt.quiver(x,y,uv[0],uv[1], cmap=colourmap, norm=plt.Normalize(zmin,zmax))
   cs.set_array(z)
   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   # adds numbers along the iso-contours
   xmin = deco['roi'][0][0]; xmax = deco['roi'][1][0]
   ymin = deco['roi'][0][1]; ymax = deco['roi'][1][1]
   if deco.has_key('crax.xlim'):
      xmin -= deco['crax.xlim']; xmax += deco['crax.xlim']
   if deco.has_key('crax.ylim'):
      ymin -= deco['crax.ylim']; ymin += deco['crax.ylim']
   crax.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
   crax.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
   crax.axis('equal')         # sets both axis scale to be equal
   #mp.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title
   #if geometry.has_key('cmapPlot'): fig.colorbar(colection)     # sets up colourbar
   #if geometry.has_key('cmapPlot'): fig.colorbar(colormap)     # sets up colourbar

   return

"""
def drawColouredQuadMaps(plt,(nelem,npoin,ndp,nplan),(x,y,ikle,z),deco):

   # ~~> Focus on current subplot / axes instance
   crax = plt.gca()
   # ~~> Plot data
   colourmap = cm.jet
   #if geometry.has_key('cmapPlot'):
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
   #if geometry.has_key('cmapPlot'): fig.colorbar(colection)     # sets up colourbar
   #if geometry.has_key('cmapPlot'): fig.colorbar(colormap)     # sets up colourbar

   return
"""

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

   sys.exit()
