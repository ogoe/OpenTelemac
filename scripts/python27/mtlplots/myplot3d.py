"""@author Juliette C.E. Parisi and Sebastien E. Bourban
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
"""@history 02/12/2013 -- Juliette C.E. Parisi and Sebastien E. Bourban
"""
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from copy import deepcopy
import numpy as np
# ~~> mayavi
from mayavi import mlab
from tvtk.api import tvtk
# ~~> dependencies towards other mtlplots
from plotTELEMAC import getColourMap
# ~~> dependencies towards other pytel/modules
from samplers.mycast import Caster
from parsers.parserSELAFIN import SELAFIN
from parsers.parserStrings import parseArrayFrame

# _____                        _____________________________________
# ____/ Primary Mayavi Object /____________________________________/
#

def typeUnstructuredGrid(xyz,ikle):

    cellUG = tvtk.CellArray()
    cellUG.set_cells( len(ikle),np.insert(ikle,0,6, axis=1).ravel() )
    typeUG = tvtk.UnstructuredGrid( points=xyz )
    typeUG.set_cells( 13*np.ones(len(ikle)), 7*np.arange(len(ikle)), cellUG )
    
    return typeUG

# _____               ______________________________________________
# ____/ Default DECO /_____________________________________________/
#

decoDefault = {
   "size":'', "dpi":'', "ratio2d": '', "title": '', "roi": '', "type":''
}

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def mapDecoDefault(decoUser,default):
   
   # ~~~ melting the pot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mpar = {}                    # mpar, contains the matplotlib parameters (defaults)
   upar = deepcopy(default) # upar, contains the user parameters (from the XML)
   for name in decoUser:
      if name not in ['look','data']: upar.update( { name:decoUser[name] } )
   for name in decoUser:
      if name in ['look','data']: upar.update(decoUser[name][0])

   # ~~~ special conversions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Replaces the relevat mpar by the upar values
   for key in upar.keys(): # /!\ the .keys() is necessary
      if key in mpl.rcParams.keys():
         if type(mpl.rcParams[key]) == type(upar[key]):
            mpar[key] = deepcopy(upar[key])
            del upar[key]
         else:
            if type(mpl.rcParams[key]) == type([]):
               if type(mpl.rcParams[key][0]) == type(1) or type(mpl.rcParams[key][0]) == type(1.0):
                  mpar[key] = parseArrayFrame(upar[key])
                  del upar[key]
               if type(mpl.rcParams[key][0]) == type("") or type(mpl.rcParams[key][0]) == type(unicode('')):
                  print upar[key].strip('[]')
                  mpar[key] = [ s.strip() for s in upar[key].strip('[]').replace(';',',').split(',') ]
                  del upar[key]
               else:
                  print '... I did not know ',type(mpl.rcParams[key]),' for key:',key,'. Could be an acceptable type'
                  sys.exit()
            elif type(mpl.rcParams[key]) == type(True):
               mpar[key] = ( upar[key].lower() == 'true' )
               del upar[key]
            elif type(mpl.rcParams[key]) == type(1):
               mpar[key] = int(upar[key])
               del upar[key]
            elif type(mpl.rcParams[key]) == type(1.0):
               mpar[key] = float(upar[key])
               del upar[key]
            elif type(mpl.rcParams[key]) == type("") or type(mpl.rcParams[key][0]) == type(unicode('')):
               mpar[key] = upar[key]
               del upar[key]
            else:
               print '... I did not know ',type(mpl.rcParams[key]),' for key:',key,'. Could be an acceptable type'
               sys.exit()
      elif key == "dpi":
         if upar[key] != '':
            mpar.update({ 'savefig.dpi': int(upar[key]) })
            mpar.update({ 'figure.dpi': int(upar[key]) })
         del upar[key]
      elif key == "size":
         if upar[key] != '':
            mpar.update({ 'figure.figsize': parseArrayFrame(upar[key]) })
         del upar[key]

   return mpar,upar

# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def drawColouredTriMaps(myplt,deco,(x,y,z,ikle,v)):

   # ~~> Line Width
   #linewidths = decoDefault['lines.linewidth']
   #if deco.has_key('linewidths'): linewidths = float(deco['linewidths'])
   # ~~> Line Colour
   #colors = decoDefault['lines.color']
   #if deco.has_key('colors'): colors = deco['colors']
   # ~~> Line Labels
   #fontsize = 9        #/!\ do find and set a default
   #if deco.has_key('fontsize'): fontsize = int(deco['fontsize'])
   #inline = 1          #/!\ do find and set a default
   #if deco.has_key('inline'): inline = int(deco['inline'])
   #fmt='%1.1f'         #/!\ do find and set a default
   #if deco.has_key('fmt'): fmt = deco['fmt']  

   # ~~> Colour maps
   #cmap = cm.jet       #/!\ do find and set a default
   #if deco.has_key('cmap'): cmap = mpl.colors.LinearSegmentedColormap('user',getColourMap(deco['cmap']))

   # ~~> Unstructured 3D Mesh
   # points as the shape of [ [x1,y1,z1],...[xn,yn,zn] ]
   points = np.dstack((x,y,z))[0]
   
   struct = typeUnstructuredGrid(points,ikle)
   struct.point_data.scalars = v.ravel()
   struct.point_data.scalars.name = 'z'

   #myplt.tricontourf(x,y,ikle, z, cmap=cmap)
   # ~~> Iso-contours and Labels
   #zmin = np.min(z); zmax = np.max(z)
   #if zmin < zmax:
   #   cs = myplt.tricontour(x,y,ikle, z, linewidths=linewidths, colors=colors)
   #   myplt.clabel(cs,fmt=fmt,fontsize=fontsize,inline=inline)
   #   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   out = myplt.outline(color=(0, 0, 0))
   out.actor.actor.scale = np.array([ 1.,    1.,    10.])
   surf = myplt.pipeline.surface( struct, opacity=0.5)
   surf.actor.actor.scale = np.array([ 1.,    1.,    10.])
   #vitesse = mlab.pipeline.vectors(bathy,mask_points=5)
   #vitesse.actor.actor.scale = array([ 1.,    1.,    500.])
   #vitesse.glyph.glyph.scale_factor = 1500.0
   #edges = myplt.pipeline.surface(myplt.pipeline.extract_edges(surf),color=(0.5, 0.5, 0.5),opacity=0.1 )
   #edges.actor.actor.scale = np.array([ 1.,    1.,    10.])

   return

def drawColouredTriVects(myplt,deco,(x,y,z,uvw,normalised)):

   # ~~> Unstructured 3D Mesh
   # points as the shape of [ [x1,y1,z1],...[xn,yn,zn] ]
   points = np.dstack((x,y,z))[0]
   
   struct = typeUnstructuredGrid(points,ikle)
   struct.point_data.vectors = uvw[0].ravel()
   struct.point_data.vectors.name = 'velocity'

   # ~~> Plot data
   #colourmap = cm.jet
   #if geometry.has_key('cmapPlot'):
   #   colourmap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
   # get vector magnitude, i.e norm-2
   #z = np.sqrt(np.sum(np.power(np.dstack(uvw[0:2])[0],2),axis=1))
   #zmin = np.min(z); zmax = np.max(z)
   #if not normalised : cs = myplt.quiver(x,y,uvw[0],uvw[1], cmap=colourmap )
   #else: cs = myplt.quiver(x,y,uvw[0],uvw[1], cmap=colourmap, norm=myplt.Normalize(zmin,zmax))
   #cs.set_array(z)
   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   # adds numbers along the iso-contours

   return

# _____                      _______________________________________
# ____/ Primary Method:Deco /______________________________________/
#

# _____                         ____________________________________
# ____/ Primary Classes:Dumper /___________________________________/
#

class Dumper3D(Caster):

   def __init__(self,caster,dump):
      Caster.__init__(self,{'object':caster.object,'obdata':caster.obdata})
      self.obtype = dump['outFormat']    # the type of file, 'slf' most probably
      self.oudata = None          # the loaded SELAFIN object itself, most probably

   def add(self,typl,what):
      Caster.add(self,typl,what)
      
      # ~~> output from for 3D file
      if self.obtype == 'slf':
         if not self.oudata: self.oudata = SELAFIN()
      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to write to this format: ' + self.obtype
         sys.exit(1)

   def save(self,fileName):
      self.oudata.putFileContent(fileName)

# _____                          ___________________________________
# ____/ Primary Classes: Figure /__________________________________/
#

class Figure3D(Caster):

   def __init__(self,caster,plot):
      Caster.__init__(self,{'object':caster.object,'obdata':caster.obdata})

      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #self.mpar,self.upar = mapDecoDefault( plot['deco'],decoDefault )
      #mpl.rcParams.update(self.mpar)
      # ~~> by default, there is no grid in 1D
      #self.mpar.update({ 'grid.alpha':1.0 })

      # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      fig = mlab.figure(bgcolor=(1, 1, 1))

      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #mpl.rcdefaults()
      
      # ~~> user params
      #if self.upar['title'] != '': mlab.title(self.upar['title'])
      # ~~> type of plot
      #if self.upar['type'] != '':
      #   if self.upar['type'][1] == 'line' or self.upar['type'][1] == 'rose':
      #      fig.add_subplot(adjustable='datalim')
      #   else:
      #      fig.add_subplot(aspect='equal')
      #else: fig.add_subplot(aspect='equal')
      # ~~> region of interes and defaault margins
      #if self.upar["roi"] != '': self.upar["roi"] = [ parseArrayPaires(self.upar["roi"]), [0,0,0,0] ]

      self.plt = mlab
      self.fig = fig      

   def add(self,typl,what):
      Caster.add(self,typl,what)

      if 'SELAFIN' in typl.upper() or \
         'slf' in typl.lower():

         # TODO: range of plans and resample within a 2d and a 3d box.
         if what['type'].split(':')[1] == '':
         
            cast = self.get(typl,what)
            elements = cast.support
            VARSORS = cast.values

            # ~~> Loop on variables
            for vtype in what['vars'].split(';'):
               vtype = vtype.split(':')[1]

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               # ~~> Draw triangles and quads
               if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
                  pass
                  # ~~> Draw/Dump (works with triangles and quads)
               #   drawMesh2DElements(self.plt,what['deco'],elements)
               #   deco(self.plt,self.upar,np.ravel(elements.T[0]),np.ravel(elements.T[1]))

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               # ~~> Extract variable data for only one time frame and one plane
               else:
                  MESHX,MESHY,MESHZ,IKLE3 = elements
                  # ~~> Multi-variables calculations
                  #if len(VARSORS) > 1:
                  #   if "map" in vtype: VARSORS = [ np.sqrt(np.sum(np.power(np.dstack(VARSORS[0:2])[0],2),axis=1)) ]
                  # ~~> Draw/Dump (multiple options possible)
                  if "map" in vtype: drawColouredTriMaps(self.plt,what['deco'],(MESHX,MESHY,MESHZ,IKLE3,VARSORS[0]))
                  #elif "label" in vtype: drawLabeledTriContours(self.plt,what['deco'],(MESHX,MESHY,IKLE3,VARSORS[0]))
                  elif "arrow" in vtype or "vector" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHY,MESHZ,VARSORS,False))
                  #elif "angle" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHY,VARSORS,True))
                  #else: print '... do not know how to draw this SELAFIN type: ' + vtype
                  #deco(self.plt,self.upar,np.ravel(MESHX[IKLE3]),np.ravel(MESHY[IKLE3]))


   def show(self):
      self.fig.scene.isometric_view()
      self.plt.show()
   def save(self,fileName): self.plt.savefig(fileName)

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Juliette C.E. Parisi; Sebastien E. Bourban"
__date__ ="$01-Dec-2013 08:51:29$"

if __name__ == "__main__":

   sys.exit()
