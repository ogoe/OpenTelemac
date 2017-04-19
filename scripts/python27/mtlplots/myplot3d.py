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
from os import path, environ, stat, mkdir
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
from parsers.parserStrings import parseArrayFrame, parseArrayPaires

# _____                        _____________________________________
# ____/ Allows interactive view /__________________________________/
#

@mlab.show
def nothing():
   return

# _____                        _____________________________________
# ____/ Primary Mayavi Object /____________________________________/
#

def typeUnstructuredGrid(xyz,ikle):

   cellUG = tvtk.CellArray()
   cellUG.set_cells( len(ikle),np.insert(ikle,0,6, axis=1).ravel() )
   typeUG = tvtk.UnstructuredGrid( points=xyz )
   typeUG.set_cells( 13*np.ones(len(ikle)), 7*np.arange(len(ikle)), cellUG )

   return typeUG

# _____                                _____________________________
# ____/ Primary Mayavi Object (Plane) /____________________________/
#

def typeUnstructuredGrid_Plane(xyz,ikle):

   cellUG = tvtk.CellArray()
   cellUG.set_cells( len(ikle),np.insert(ikle,0,3, axis=1).ravel() )
   typeUG = tvtk.UnstructuredGrid( points=xyz )
   typeUG.set_cells( 5*np.ones(len(ikle)), 7*np.arange(len(ikle)), cellUG )

   return typeUG

# _____               ______________________________________________
# ____/ Default DECO /_____________________________________________/
#

decoDefault = {
   "dpi":'', "ratio2d": '', "title": '', "roi": '', "type":'',
   "azimuth":'', "elevation":'', "distance":'',
   "focalx":'', "focaly":'', "focalz":'',
   "interactive":'',
   "size":'(10;10)'
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

      """ Skip most of this
      if key in mpl.rcParams.keys():
         if type(mpl.rcParams[key]) == type(upar[key]):
            mpar[key] = deepcopy(upar[key])
            del upar[key]
         else:
            if type(mpl.rcParams[key]) == type([]):
               if type(mpl.rcParams[key][0]) == type(1) or type(mpl.rcParams[key][0]) == type(1.0):
                  mpar[key] = parseArrayFrame(upar[key])
                  del upar[key]
               elif type(mpl.rcParams[key][0]) == type("") or type(mpl.rcParams[key][0]) == type(unicode('')):
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
      """

      if key == "size":
         if upar[key] != '':
            mpar.update({ 'figure_size': parseArrayPaires(upar[key])[0]})
         del upar[key]

   return mpar,upar

# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def drawColouredTriMaps(myplt,decoUser,vtype,(x,y,z,ikle,v)):

   opacity = 0.5
   if decoUser.has_key('opacity'):
      if decoUser['opacity'] != '':
         opacity = float(decoUser['opacity'])

   # ~~> Unstructured 3D Mesh
   # points as the shape of [ [x1,y1,z1],...[xn,yn,zn] ]
   points = np.dstack((x,y,z))[0]

   struct = typeUnstructuredGrid(points,ikle)
   struct.point_data.scalars = v.ravel()
   struct.point_data.scalars.name = 'z'

   surf = myplt.pipeline.surface( struct, opacity=opacity)
   surf.actor.mapper.interpolate_scalars_before_mapping = True
   surf.actor.property.lighting = False

   colours = surf.parent

   if decoUser.has_key('colour_range'):
      if decoUser['colour_range'] != '':
         colour_range = parseArrayPaires(decoUser['colour_range'])[0][:]
         colours.scalar_lut_manager.data_range = colour_range

   if decoUser.has_key('number_colours'):
      if decoUser['number_colours'] != '':
         number_colours = int(decoUser['number_colours'])
         colours.scalar_lut_manager.number_of_colors = number_colours
         colours.scalar_lut_manager.number_of_labels = number_colours + 1

   if decoUser.has_key('z_scale'):
      if decoUser['z_scale'] != '':
         z_scale = float(decoUser['z_scale'])
         surf.actor.actor.scale = np.array([1.0, 1.0, z_scale])

   out = myplt.outline(color=(0, 0, 0))

   if "contour" in vtype:
      surf.enable_contours = True
      if decoUser.has_key('number_colours'):
         if decoUser['number_colours'] != '':
            number_colours = int(decoUser['number_colours'])
            surf.contour.number_of_contours = number_colours

   for key in decoUser:
      try:
         exec(str(key)+' = '+str(decoUser[key]))
      except:
         pass

   return

def drawColouredTriVects(myplt,decoUser,vtype,(x,y,z,ikle,uvw,normalised)):

   # ~~> Unstructured 3D Mesh
   # points as the shape of [ [x1,y1,z1],...[xn,yn,zn] ]
   points = np.dstack((x,y,z))[0]

   struct = typeUnstructuredGrid(points,ikle)
   struct.point_data.vectors = uvw[0].T
   struct.point_data.vectors.name = 'velocity'

   vel = myplt.pipeline.vectors( struct )

   if decoUser.has_key('z_scale'):
      if decoUser['z_scale'] != '':
         z_scale = float(decoUser['z_scale'])
         vel.actor.actor.scale = np.array([1.0, 1.0, z_scale])

   colours = vel.parent

   if decoUser.has_key('colour_range'):
      if decoUser['colour_range'] != '':
         colour_range = parseArrayPaires(decoUser['colour_range'])[0][:]
         colours.scalar_lut_manager.data_range = colour_range

   if decoUser.has_key('number_colours'):
      if decoUser['number_colours'] != '':
         number_colours = int(decoUser['number_colours'])
         colours.scalar_lut_manager.number_of_colors = number_colours
         colours.scalar_lut_manager.number_of_labels = number_colours + 1

   out = myplt.outline(color=(0, 0, 0))

   if "streamline" in vtype:

      vel.glyph.visible = False
      vel.actor.actor.visibility = False
      vel.visible = False

      norm = myplt.pipeline.extract_vector_norm( struct )
      stream = myplt.pipeline.streamline( norm )

      stream.seed.widget = stream.seed.widget_list[2]
      stream.seed.widget.enabled = False

      if decoUser.has_key('z_scale'):
         if decoUser['z_scale'] != '':
            z_scale = float(decoUser['z_scale'])
            stream.actor.actor.scale = np.array([1.0, 1.0, z_scale])

      colours = stream.parent

      if decoUser.has_key('colour_range'):
         if decoUser['colour_range'] != '':
            colour_range = parseArrayPaires(decoUser['colour_range'])[0][:]
            colours.scalar_lut_manager.data_range = colour_range

      if decoUser.has_key('number_colours'):
         if decoUser['number_colours'] != '':
            number_colours = int(decoUser['number_colours'])
            colours.scalar_lut_manager.number_of_colors = number_colours
            colours.scalar_lut_manager.number_of_labels = number_colours + 1

   for key in decoUser:
      try:
         exec(str(key)+' = '+str(decoUser[key]))
      except:
         pass

   return

def drawColouredTriMaps_Plane(myplt,decoUser,vtype,(x,y,z,ikle,v)):

   opacity = 0.5
   if decoUser.has_key('opacity'):
      if decoUser['opacity'] != '':
         opacity = float(decoUser['opacity'])

   # ~~> Unstructured 3D Mesh
   # points as the shape of [ [x1,y1,z1],...[xn,yn,zn] ]

   points = np.dstack((x,y,z))[0]

   struct = typeUnstructuredGrid_Plane(points,ikle)
   struct.point_data.scalars = v.ravel()
   struct.point_data.scalars.name = 'z'

   surf = myplt.pipeline.surface( struct, opacity=opacity)
   surf.actor.mapper.interpolate_scalars_before_mapping = True
   surf.actor.property.lighting = False

   if decoUser.has_key('z_scale'):
      if decoUser['z_scale'] != '':
         z_scale = float(decoUser['z_scale'])
         surf.actor.actor.scale = np.array([1.0, 1.0, z_scale])

   colours = surf.parent

   if decoUser.has_key('colour_range'):
      if decoUser['colour_range'] != '':
         colour_range = parseArrayPaires(decoUser['colour_range'])[0][:]
         colours.scalar_lut_manager.data_range = colour_range

   if decoUser.has_key('number_colours'):
      if decoUser['number_colours'] != '':
         number_colours = int(decoUser['number_colours'])
         colours.scalar_lut_manager.number_of_colors = number_colours
         colours.scalar_lut_manager.number_of_labels = number_colours + 1

   for key in decoUser:
      try:
         exec(str(key)+' = '+str(decoUser[key]))
      except:
         pass

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
      self.obtype = dump['saveas']    # the type of file, 'slf' most probably
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
      self.mpar,self.upar = mapDecoDefault( plot['deco'],decoDefault )

      #mpl.rcParams.update(self.mpar)
      # ~~> by default, there is no grid in 1D
      #self.mpar.update({ 'grid.alpha':1.0 })

      # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      fig = mlab.figure(bgcolor=(1, 1, 1),size=self.mpar['figure_size'])

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
         if what['type'].split(':')[1] == 'i-surface':

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

                  if "map" in vtype or "contour" in vtype:
                    drawColouredTriMaps(self.plt,what['deco'],vtype,(MESHX,MESHY,MESHZ,IKLE3,VARSORS[0]))

                  #elif "label" in vtype: drawLabeledTriContours(self.plt,what['deco'],(MESHX,MESHY,IKLE3,VARSORS[0]))

                  elif "arrow" in vtype or "vector" in vtype or "streamline" in vtype:
                    drawColouredTriVects(self.plt,what['deco'],vtype,(MESHX,MESHY,MESHZ,IKLE3,VARSORS,False))

                  #elif "angle" in vtype: drawColouredTriVects(self.plt,what['deco'],vtype,(MESHX,MESHY,VARSORS,True))
                  #else: print '... do not know how to draw this SELAFIN type: ' + vtype
                  #deco(self.plt,self.upar,np.ravel(MESHX[IKLE3]),np.ravel(MESHY[IKLE3]))

         elif what['type'].split(':')[1] == 'p-section':

            cast = self.get(typl,what)
            elements = cast.support
            VARSORS = cast.values
            vtype = what['vars'].split(':')[1]

            MESHX,MESHY,MESHZ,IKLE3 = elements

            if "map" in vtype:
              drawColouredTriMaps_Plane(self.plt,what['deco'],vtype,(MESHX,MESHY,MESHZ,IKLE3,VARSORS[0]))

         elif what['type'].split(':')[1] == 'v-section':

            cast = self.get(typl,what)
            elements = cast.support
            VARSORS = cast.values
            vtype = what['vars'].split(':')[1]

            MESHX,MESHY,MESHZ,IKLE3 = elements

            if "map" in vtype:
              drawColouredTriMaps_Plane(self.plt,what['deco'],vtype,(MESHX,MESHY,MESHZ,IKLE3,VARSORS[0]))

   def show(self):
      self.fig.scene.isometric_view()
      self.plt.show()
      self.plt.close()

   def save(self,fileName):
      default_view = mlab.view()
      azimuth = default_view[0]
      elevation = default_view[1]
      distance = default_view[2]
      focalpoint = default_view[3]

      if self.upar['azimuth']!='':
         azimuth = float(self.upar['azimuth'])
      if self.upar['elevation']!='':
         elevation = float(self.upar['elevation'])
      if self.upar['distance']!='':
         distance = float(self.upar['distance'])
      if self.upar['focalx']!='' and self.upar['focaly']!='' and self.upar['focalz']!='':
         focalpoint = (float(self.upar['focalx']),
                       float(self.upar['focaly']),
                       float(self.upar['focalz']))

      mlab.view(azimuth = azimuth,
                elevation = elevation,
                distance = distance,
                focalpoint = focalpoint)

      if self.upar['interactive']!='':
         nothing()

      try:
         stat(path.split(fileName)[0])
      except:
         mkdir(path.split(fileName)[0])
      self.plt.savefig(fileName)
      self.plt.close()

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Juliette C.E. Parisi; Sebastien E. Bourban"
__date__ ="$01-Dec-2013 08:51:29$"

if __name__ == "__main__":

   sys.exit()
