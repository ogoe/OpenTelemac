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
from os import path, environ, stat, mkdir
from copy import deepcopy
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
# ~~> dependencies towards other mtlplots
from plotTELEMAC import getColourMap
# ~~> dependencies towards other pytel/modules
from samplers.mycast import Caster,whatVarsSLF
from parsers.parserSELAFIN import SELAFIN,SELAFINS
from parsers.parserStrings import parseArrayFrame, parseArrayPaires

# _____               ______________________________________________
# ____/ Default DECO /_____________________________________________/
#

decoDefault = {
   "size":'(10;10)', "aspect":'1', "dpi":'', "ratio2d": '', "title": '', "roi": '', "type":'', "set":'',
   "background":'(1.0,0.90196,0.6)',
   ### LINES
   # See http://matplotlib.org/api/artist_api.html#module-matplotlib.lines for more
   # information on line properties.
   'lines.linewidth'          : 0.5,            # line width in points
   'lines.linestyle'          : '-',            # solid line
   'lines.color'              : 'blue',         # has no affect on plot(); see axes.color_cycle
   'lines.marker'             : None,           # the default marker
   'lines.markeredgewidth'    : 0.5,            # the line width around the marker symbol
   'lines.markersize'         : 6,              # markersize, in points
   'lines.dash_joinstyle'     : 'miter',        # miter|round|bevel
   'lines.dash_capstyle'      : 'butt',         # butt|round|projecting
   'lines.solid_joinstyle'    : 'miter',        # miter|round|bevel
   'lines.solid_capstyle'     : 'projecting',   # butt|round|projecting
   'lines.antialiased'        : True,           # render lines in antialised (no jaggies)
   ### AXES
   # default face and edge color, default tick sizes,
   # default fontsizes for ticklabels, and so on.  See
   # http://matplotlib.org/api/axes_api.html#module-matplotlib.axes
   'axes.hold'               : True,           # whether to clear the axes by default on
   'axes.facecolor'          : 'white',        # axes background color
   'axes.edgecolor'          : 'black',        # axes edge color
   'axes.linewidth'          : 1.0,            # edge linewidth
   'axes.grid'               : False,          # display grid or not
   'axes.titlesize'          : 'large',        # fontsize of the axes title
   'axes.labelsize'          : 'medium',       # fontsize of the x any y labels
   'axes.labelweight'        : 'normal',       # weight of the x and y labels
   'axes.labelcolor'         : 'black',
   'axes.axisbelow'          : False,          # whether axis gridlines and ticks are below
                                               # the axes elements (lines, text, etc)
   'axes.formatter.limits'   : [-7, 7],        # use scientific notation if log10
                                               # of the axis range is smaller than the
                                               # first or larger than the second
   'axes.formatter.use_locale' : False,        # When True, format tick labels
                                               # according to the user's locale.
                                               # For example, use ',' as a decimal
                                               # separator in the fr_FR locale.
   'axes.formatter.use_mathtext' : False,      # When True, use mathtext for scientific
                                               # notation.
   'axes.unicode_minus'      : True,           # use unicode for the minus symbol
                                               # rather than hyphen.  See
                                               # http://en.wikipedia.org/wiki/Plus_and_minus_signs#Character_codes
   'axes.color_cycle'        : [ 'b', 'g', 'r', 'c', 'm', 'y', 'k' ],
                                               # color cycle for plot lines
                                               # as list of string colorspecs:
                                               # single letter, long name, or
                                               # web-style hex
   'axes.xmargin'            : 0,              # x margin.  See `axes.Axes.margins`
   'axes.ymargin'            : 0,              # y margin See `axes.Axes.margins`
   'polaraxes.grid'          : True,           # display grid on polar axes
   'axes3d.grid'             : True,           # display grid on 3d axes
   ### TICKS
   # see http://matplotlib.org/api/axis_api.html#matplotlib.axis.Tick
   'xtick.major.size'         : 4,             # major tick size in points
   'xtick.minor.size'         : 2,             # minor tick size in points
   'xtick.major.width'        : 0.5,           # major tick width in points
   'xtick.minor.width'        : 0.5,           # minor tick width in points
   'xtick.major.pad'          : 4,             # distance to major tick label in points
   'xtick.minor.pad'          : 4,             # distance to the minor tick label in points
   'xtick.color'              : 'k',           # color of the tick labels
   'xtick.labelsize'          : 'medium',      # fontsize of the tick labels
   'xtick.direction'          : 'in',          # direction: in, out, or inout
   'ytick.major.size'         : 4,             # major tick size in points
   'ytick.minor.size'         : 2,             # minor tick size in points
   'ytick.major.width'        : 0.5,           # major tick width in points
   'ytick.minor.width'        : 0.5,           # minor tick width in points
   'ytick.major.pad'          : 4,             # distance to major tick label in points
   'ytick.minor.pad'          : 4,             # distance to the minor tick label in points
   'ytick.color'              : 'k',           # color of the tick labels
   'ytick.labelsize'          : 'medium',      # fontsize of the tick labels
   'ytick.direction'          : 'in',          # direction: in, out, or inout
   ### GRIDS
   'grid.color'               : 'black',       # grid color
   'grid.linestyle'           : ':',           # dotted
   'grid.linewidth'           : 0.5,           # in points
   'grid.alpha'               : 1.0,           # transparency, between 0.0 and 1.0
   ### LEGEND
   'legend.fancybox'           : False,        # if True, use a rounded box for the
                                               # legend, else a rectangle
   'legend.isaxes'             : True,
   'legend.numpoints'          : 2,            # the number of points in the legend line
   'legend.fontsize'           : 'large',
   'legend.borderpad'          : 0.5,          # border whitespace in fontsize units
   'legend.markerscale'        : 1.0,          # the relative size of legend markers vs. original
                                               # the following dimensions are in axes coords
   'legend.labelspacing'       : 0.5,          # the vertical space between the legend entries in fraction of fontsize
   'legend.handlelength'       : 2.,           # the length of the legend lines in fraction of fontsize
   'legend.handleheight'       : 0.7,          # the height of the legend handle in fraction of fontsize
   'legend.handletextpad'      : 0.8,          # the space between the legend line and legend text in fraction of fontsize
   'legend.borderaxespad'      : 0.5,          # the border between the axes and legend edge in fraction of fontsize
   'legend.columnspacing'      : 2.,           # the border between the axes and legend edge in fraction of fontsize
   'legend.shadow'             : False,
   'legend.frameon'            : True,         # whether or not to draw a frame around legend
   'legend.scatterpoints'      : 3             # number of scatter points
}

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def mapDecoDefault(decoUser,default):

   # ~~~ melting the pot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mpar = {}                    # mpar, contains the matplotlib parameters (defaults)
   upar = deepcopy(default)     # upar, contains the user parameters (from the XML)
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
               elif type(mpl.rcParams[key][0]) == type("") or type(mpl.rcParams[key][0]) == type(unicode('')):
                  print upar[key].strip('[]')
                  mpar[key] = [ s.strip() for s in upar[key].strip('[]').replace(';',',').split(',') ]
                  del upar[key]
               else:
                  print '... I did not know ',type(mpl.rcParams[key]),' for key:',key,'. Could be an acceptable type'
                  sys.exit(1)
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
               sys.exit(1)
      elif key == "dpi":
         if upar[key] != '':
            mpar.update({ 'savefig.dpi': int(upar[key]) })
            mpar.update({ 'figure.dpi': int(upar[key]) })
         del upar[key]
      elif key == "size":
         if upar[key] != '':
            mpar.update({ 'figure.figsize': parseArrayPaires(upar[key])[0]})
         del upar[key]
      elif key == "roi":
         if upar[key] != '':
            pairs = parseArrayPaires(upar[key])
            if len(pairs) == 2: pairs.extend([(0.,0.),(0.,0.)])
            elif len(pairs) != 4:
               print '... could not interprete roi ('+upar[key]+'): ' + pairs
               sys.exit(1)
            upar[key] = pairs

   return mpar,upar

# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def drawGeo(myplt,decoUser,fname):

   import gdal
   import Image
   image1 = Image.open(fname)
   dataset = gdal.Open(fname)
   cols = dataset.RasterXSize
   rows = dataset.RasterYSize
   geotransform = dataset.GetGeoTransform()
   x1 = geotransform[0]
   y2 = geotransform[3]
   dx = geotransform[1]
   dy = geotransform[5]
   x2 = x1 + dx*cols
   y1 = y2 + dy*rows
   myplt.imshow(image1.transpose(1),extent=[x1,x2,y1,y2])

   return

def drawImage(myplt,decoUser,fname):

   import Image
   image1 = Image.open(fname)

   if decoUser.has_key('extent'):
      extent = eval(decoUser['extent'])
      myplt.imshow(image1.transpose(1),extent=extent)
   else:
      myplt.imshow(image1.transpose(1))

   return

def drawMesh2DElements(myplt,decoUser,elements):

   #  *2DElements: draw individual elements polygons  (triangle or quads)
   # ~~> Focus on current subplot / axes instance
   crax = myplt.gca()
   # ~~>  Collections
#   colection = collections.PolyCollection(
#      elements, cmap=cm.jet, antialiaseds=0, # norm=myplt.Normalize()
#      edgecolors = 'k', linewidth=1, facecolors = 'none')

   colection = collections.PolyCollection(elements)

      #colection.set_array(val)       # each triangle colour dependent on its value from its verticies
   # ~~> Plot data
   #ex: fig = myplt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in inches
   crax.add_collection(colection)   # adds, or plots our collection

   return

def drawColouredTriMaps(myplt,decoUser,(x,y,ikle,z)):

   # ~~> Line Width
   linewidths = decoDefault['lines.linewidth']
   if decoUser.has_key('linewidths'): linewidths = float(decoUser['linewidths'])
   # ~~> Line Colour
   colors = decoDefault['lines.color']
   if decoUser.has_key('colors'): colors = decoUser['colors']
   # ~~> Line Labels
   fontsize = 9        #/!\ do find and set a default
   if decoUser.has_key('fontsize'): fontsize = int(decoUser['fontsize'])
   inline = 1          #/!\ do find and set a default
   if decoUser.has_key('inline'): inline = int(decoUser['inline'])
   fmt=u'%1.1f'         #/!\ do find and set a default
   if decoUser.has_key('fmt'): fmt = decoUser['fmt']

   # ~~> Colour maps
   if decoUser.has_key('cmap') and decoUser['cmap'].split('.')[-1] == 'xml':
      cmap = mpl.colors.LinearSegmentedColormap('user',getColourMap(decoUser['cmap']))
      decoUser['cmap'] = cmap

   # ~~> Colour maps
   for key in decoUser:
      try:
         decoUser[key] = eval(decoUser[key])
      except:
         pass

   if decoUser.has_key('levels'): decoUser['levels'] = sorted(decoUser['levels'])

   cs = myplt.tricontourf(x,y,ikle, z, **decoUser)

#   if (decoUser.has_key('colourbar')):
#      if (decoUser['colourbar'] == "yes"):
#         # make a colour bar
#         cb = eval("myplt."+"colorbar"+"()")

   # ~~> Iso-contours and Labels
#   zmin = np.min(z); zmax = np.max(z)
#   if zmin < zmax:
#      cs = myplt.tricontour(x,y,ikle, z, linewidths=linewidths, colors=colors)
#      myplt.clabel(cs,fmt=fmt,fontsize=fontsize,inline=inline)
#      myplt.clabel(cs,fontsize=fontsize,inline=inline)
      #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')

   return

def drawLabeledTriContours(myplt,decoUser,(x,y,ikle,z)):

   # Convert negative dashed default contour lines by solids
   # matplotlib.rcParams['contour.negative_linestyle'] = 'solid'

   # contour levels based on z values
   zmin = np.min(z); zmax = np.max(z)

   for key in decoUser:
     try:
       decoUser[key] = eval(decoUser[key])
     except:
       pass

   if zmin < zmax:
      cs = myplt.tricontour(x,y,ikle, z, **decoUser)
      myplt.clabel(cs, **decoUser)

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
#   try:
#      cs = myplt.tricontour( x,y,ikle, z , levels, linewidths=np.arange(.5,4,.5))
#   except:
#      print '\ntricontour not available on your system'
#      print ' ... unable to plot 2D figures'

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
#   myplt.clabel(cs, levels[1::2], fmt='%1.3f', inline=1, fontsize=9, colors='k')

   # Extras: thicken the zero contour.
#   myplt.setp( cs.collections[6], linewidth=4 )
   # TODO: investigate use of extent ( extent=(-3,3,-2,2) )

   # make a colorbar for the contour lines
#   cb = myplt.colorbar(cs, shrink=0.8, extend='both')
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

def drawColouredTriVects(myplt,decoUser,(x,y,uv,normalised)):

   # ~~> Colour maps
   if decoUser.has_key('cmap') and decoUser['cmap'].split('.')[-1] == 'xml':
      cmap = mpl.colors.LinearSegmentedColormap('user',getColourMap(decoUser['cmap']))
      decoUser['cmap'] = cmap

   # ~~> Plot data
   for key in decoUser:
     if ((key == 'units') or (key == 'angles') or (key == 'scale_units')):
       pass
     else:
       try:
         decoUser[key] = eval(decoUser[key])
       except:
         pass

   vector_keys = [ 'units', 'angles', 'scale', 'scale_units', 'width', \
      'headwidth', 'headlength', 'headaxislength', 'minshaft', 'minlength', \
      'pivot', 'color', 'cmap' ]
   decoVector = {}

   for key in decoUser:
     if (key in vector_keys):
       decoVector[key] = decoUser[key]

   z = np.sqrt(np.sum(np.power(np.dstack(uv[0:2])[0],2),axis=1))
   zmin = np.min(z); zmax = np.max(z)
   if not normalised :
     cs = myplt.quiver(x,y,uv[0],uv[1], **decoVector )
   else:
     cs = myplt.quiver(x,y,uv[0],uv[1], norm=myplt.Normalize(zmin,zmax), **decoVector )

#   print 'units', cs.units
#   print 'pivot', cs.pivot
#   print 'color', cs.color

   cs.set_array(z)

#   if (decoUser.has_key('colourbar')):
#      if (decoUser['colourbar'] == "yes"):
#      # make a colour bar
#         cb = myplt.colorbar(cs, shrink=1.0, extend='both')

   key_x = 0.8
   key_y = 0.05
   key_length = 1.0
   key_label = ''

   if (decoUser.has_key('key_x')):
      key_x = float(decoUser['key_x'])
   if (decoUser.has_key('key_y')):
      key_y = float(decoUser['key_y'])
   if (decoUser.has_key('key_length')):
      key_length = float(decoUser['key_length'])
   if (decoUser.has_key('key_label')):
      key_label = decoUser['key_label']

   if (decoUser.has_key('key')):
      if (decoUser['key'] == "yes"):
      # make a key
         cb = myplt.quiverkey(cs,key_x,key_y,key_length,key_label)

   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   # adds numbers along the iso-contours

   return

# _____                         ____________________________________
# ____/ Primary Casts:SELAFINS /___________________________________/
#
class dumpSELAFIN(SELAFINS):

   # ~~> Standard SELAFINS file
   #def __init__(self,f):
   #   SELAFINS.__init__(self,f)

   def add(self,slf):
      if self.slf == None: self.slf = slf
      self.slfs.append(slf)
      self.suite = self.isSuite() # True if there is only one slf
      self.merge = self.isMerge() # True if there is only one slf

# _____                      _______________________________________
# ____/ Primary Method:Deco /______________________________________/
#

def deco(myplt,upar,dpar):

   # ~~ Ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #crax.axis('equal')         # sets both axis scale to be equal
   #crax = myplt.gca()
   #crax.set_aspect('equal')

   # ~~ General keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if upar['set'] != '':
      for set in upar['set'].split(';'):
         if set[-1] != ')': set += '()'
         eval("myplt."+set)

   # ~~ Axis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if upar["roi"] != '':
      (xmin,ymin),(xmax,ymax),(emar,nmar),(wmar,smar) = upar["roi"]
      xgap = xmax - xmin
      xmin -= wmar*xgap
      xmax += emar*xgap
      ygap = ymax - ymin
      ymin -= smar*ygap
      ymax += nmar*ygap
      myplt.axis([xmin,xmax,ymin,ymax])
   elif dpar["roi"] != '':
      roi,mar = dpar["roi"]
      xmin,ymin,xmax,ymax = roi
      emar,nmar,wmar,smar = mar
      xgap = xmax - xmin
      xmin -= wmar*xgap
      xmax += emar*xgap
      ygap = ymax - ymin
      ymin -= smar*ygap
      ymax += nmar*ygap
      myplt.axis([xmin,xmax,ymin,ymax])

   #curax.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title

   #if geometry.has_key('cmapPlot'): fig.colorbar(colection)     # sets up colourbar
   #if geometry.has_key('cmapPlot'): fig.colorbar(colormap)     # sets up colourbar

   return

# _____                         ____________________________________
# ____/ Primary Classes:Dumper /___________________________________/
#

class Dumper2D(Caster):

   def __init__(self,caster,dump):
      Caster.__init__(self,{'object':caster.object,'obdata':caster.obdata})
      self.obtype = dump['saveas']    # the type of file, 'slf' most probably
      self.oudata = None          # the loaded SELAFIN object itself, most probably
      #self.obdump = dumpSELAFIN()

   def add(self,typl,what):
      Caster.add(self,typl,what)

      # ~~> output from for 2D file
      if self.obtype == 'slf':
         #self.obdump.add(self.object[what['file']])
         cast = self.get(typl,what)
         support = cast.support
         values = cast.values
         if len(support) != 3:
            print '... not enough information to save as 2d variable'
            sys.exit(1)
         obj = self.object[what['file']]
         # ~~ SELAFIN header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         if not self.oudata:
            self.oudata = SELAFIN('')
            # create the out header
            self.oudata.TITLE = '' # TODO: pass it on from what and deco
            self.oudata.NBV1 = 0
            self.oudata.VARNAMES = []; self.oudata.VARUNITS = []
            self.oudata.IPARAM = obj.IPARAM
            self.oudata.IPARAM[6] = 1 # 3D being forced to 2D
            self.oudata.NDP2 = len(support[2][0])
            if np.all([obj.IKLE2,support[2]]):
               self.oudata.IKLE2 = support[3]; self.oudata.IPOB2 = np.zeros(len(supoort[0]),dtype=np.int)
               self.oudata.MESHX = support[0]
               self.oudata.MESHY = support[1]
            else:
               self.oudata.IKLE2 = obj.IKLE2; self.oudata.IPOB2 = obj.IPOB2 # IPOBO missing from support
               self.oudata.MESHX = obj.MESHX
               self.oudata.MESHY = obj.MESHY
            self.oudata.NELEM2 = len(self.oudata.IKLE2); self.oudata.NPOIN2 = len(self.oudata.MESHX)
            self.oudata.NELEM3 = self.oudata.NELEM2; self.oudata.NPOIN3 = self.oudata.NPOIN2; self.oudata.NDP3 = self.oudata.NDP2; self.oudata.NPLAN = 1
         vars,vtypes = whatVarsSLF(what['vars'],obj.VARNAMES)
         self.oudata.NBV1 = self.oudata.NBV1 + len(vars[0])
         self.oudata.NBV2 = 0; self.oudata.NVAR = self.oudata.NBV1 + self.oudata.NBV2
         self.oudata.CLDNAMES = []; self.oudata.CLDUNITS = []
         self.oudata.VARINDEX = range(self.oudata.NVAR)
         for ivar,ival in zip(vars[0],range(len(vars[0]))):
            self.oudata.VARNAMES.append(obj.VARNAMES[ivar])
            self.oudata.VARUNITS.append(obj.VARUNITS[ivar])
            self.obdata.update({obj.VARNAMES[ivar]:[values[ival]]})
         if max(self.oudata.IPARAM[9],obj.IPARAM[9]) >0:
            if self.oudata.DATETIME != obj.DATETIME: self.oudata.IPARAM[9] = 0
         if self.oudata.NELEM2 != obj.NELEM2 or self.oudata.NPOIN2 != obj.NPOIN2:
            print '... mismatch between the 2D sizes of layers of a same save2d object '
            sys.exit(1)
         self.oudata.IKLE3 = self.oudata.IKLE2; self.oudata.IPOB3 = self.oudata.IPOB2

      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to write to this format: ' + self.obtype
         sys.exit(1)

   def save(self,fileName):
      # gather common information for the final header
      if self.obtype == 'slf':
         self.oudata.fole = {}
         self.oudata.fole.update({ 'name': fileName })
         self.oudata.fole.update({ 'endian': ">" })    # "<" means little-endian, ">" means big-endian
         self.oudata.fole.update({ 'float': ('f',4) }) #'f' size 4, 'd' = size 8
         self.oudata.fole.update({ 'hook': open(fileName,'wb') })
         self.oudata.appendHeaderSLF()
         self.oudata.appendCoreTimeSLF(0.0) # TODO: recover track of time
         for ivar in self.oudata.VARNAMES:
            self.oudata.appendCoreVarsSLF(self.obdata[ivar])
         self.oudata.fole['hook'].close()

      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to write to this format: ' + self.obtype
         sys.exit(1)

# _____                          ___________________________________
# ____/ Primary Classes: Figure /__________________________________/
#

class Figure2D(Caster):

   def __init__(self,caster,plot):
      Caster.__init__(self,{'object':caster.object,'obdata':caster.obdata})

      myplt = deepcopy(plot)
      # ~~~ special keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for key in ['size','roi']:
         if key in myplt.keys():
            myplt['deco'].update({ key:plot[key] })
            del myplt[key]

      # ~~~ special case for set ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if 'set' in myplt.keys():
         if 'look' in myplt['deco'].keys():
            for l in myplt['deco']['look']:
               if 'set' in l:
                  if myplt['set'] not in l['set'].split(';'):
                     l['set'] = myplt['set']+';'+l['set']
               else:
                  l.update({ 'set':myplt['set'] })
         else:
            myplt['deco'].update({ 'look' : [{'set':myplt['set']}] })
         del  myplt['set']

      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.dpar = {}
      self.mpar,self.upar = mapDecoDefault( myplt['deco'],decoDefault )
      mpl.rcParams.update(self.mpar)
      # ~~> by default, there is no grid in 1D
      #self.mpar.update({ 'grid.alpha':1.0 })

      # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      fig = plt.figure(figsize=self.mpar['figure.figsize']) # TODO: do this when you draw
      # ~~> add_subplot, or ax definition

      background = self.upar['background']
      try:
        background = eval(background)
      except:
        pass

      fig.add_subplot(111,axisbg=background)
      ax1, = fig.get_axes()
      # fig.add_subplot(111) returns an Axes instance, where we can plot and this is
      #   also the reason why we call the variable referring to that instance ax.
      #   This is a common way to add an Axes to a Figure, but add_subplot() does a
      #   bit more: it adds a subplot. So far we have only seen a Figure with one Axes
      #   instance, so only one area where we can draw, but Matplotlib allows more than one

      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#      mpl.rcdefaults()

      # ~~> user params
      if self.upar['title'] != '': plt.title(self.upar['title'])
      # ~~> type of plot
      if self.upar['type'] != '':
         if self.upar['type'][1] == 'line' or self.upar['type'][1] == 'rose':
            ax1.set_adjustable('datalim')
         else:
            ax1.set_aspect(self.upar['aspect'])
      else: ax1.set_aspect(self.upar['aspect'])
      # ~~> region of interes and defaault margins
      self.dpar.update({ "roi":[[],[0.02,0.02,0.02,0.02]] })

      self.plt = plt
      self.fig = fig

   def add(self,typl,what):
      Caster.add(self,typl,what)

      if len(what['vars'].split(';')) > 1: # TODO: raise exception
         print '... do not know support multiple variables anymore: ' + what['vars']
         sys.exit(1)

      if 'tif' in typl.lower():
         vtype = what['vars'].split(':')[1]
         if "geo" in vtype: drawGeo(self.plt,what['deco'],what['file'])
         if "image" in vtype: drawImage(self.plt,what['deco'],what['file'])

      elif 'jpg' in typl.lower() or 'gif' in typl.lower()  \
        or 'png' in typl.lower() or 'bmp' in typl.lower():
         vtype = what['vars'].split(':')[1]
         if "image" in vtype: drawImage(self.plt,what['deco'],what['file'])

      elif what['type'].split(':')[1] == 'v-section':

         cast = self.get(typl,what)
         elements = cast.support
         VARSORS = cast.values
         vtype = what['vars'].split(':')[1]

         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
            # ~~> Draw/Dump (works with triangles and quads)
            drawMesh2DElements(self.plt,what['deco'],elements)
            x0 = np.ravel(elements.T[0]); y0 = np.ravel(elements.T[1])
            if self.dpar["roi"][0] == []: self.dpar["roi"][0] = [ np.min(x0),np.min(y0),np.max(x0),np.max(y0) ]
            else: self.dpar["roi"][0] = [ min(self.dpar["roi"][0][0],np.min(x0)),min(self.dpar["roi"][0][1],np.min(y0)),max(self.dpar["roi"][0][2],np.max(x0)),max(self.dpar["roi"][0][3],np.max(y0)) ]
            # TODO: colour the mesh according to cast.values

         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         else:
            MESHX,MESHZ,IKLE3 = elements
            # ~~> Multi-variables calculations
            if len(VARSORS) > 1:
               if "map" in vtype: VARSORS = [ np.sqrt(np.sum(np.power(np.dstack(VARSORS[0:3:2])[0],2),axis=1)) ]
            # ~~> Draw/Dump (multiple options possible)
            if "map" in vtype: drawColouredTriMaps(self.plt,what['deco'],(MESHX,MESHZ,IKLE3,VARSORS[0]))
            if "label" in vtype: drawLabeledTriContours(self.plt,what['deco'],(MESHX,MESHZ,IKLE3,VARSORS[0]))
            if "arrow" in vtype or "vector" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHZ,VARSORS,False))
            if "angle" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHZ,VARSORS,True))
            x0 = np.ravel(MESHX[IKLE3]); y0 = np.ravel(MESHZ[IKLE3])
            if self.dpar["roi"][0] == []: self.dpar["roi"][0] = [ np.min(x0),np.min(y0),np.max(x0),np.max(y0) ]
            else: self.dpar["roi"][0] = [ min(self.dpar["roi"][0][0],np.min(x0)),min(self.dpar["roi"][0][1],np.min(y0)),max(self.dpar["roi"][0][2],np.max(x0)),max(self.dpar["roi"][0][3],np.max(y0)) ]


      elif what['type'].split(':')[1] == 'p-section':

         cast = self.get(typl,what)
         elements = cast.support
         VARSORS = cast.values
         vtype = what['vars'].split(':')[1]

         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         # ~~> Draw triangles and quads
         if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
            # ~~> Draw/Dump (works with triangles and quads)
            drawMesh2DElements(self.plt,what['deco'],elements)
            x0 = np.ravel(elements.T[0]); y0 = np.ravel(elements.T[1])
            if self.dpar["roi"][0] == []: self.dpar["roi"][0] = [ np.min(x0),np.min(y0),np.max(x0),np.max(y0) ]
            else: self.dpar["roi"][0] = [ min(self.dpar["roi"][0][0],np.min(x0)),min(self.dpar["roi"][0][1],np.min(y0)),max(self.dpar["roi"][0][2],np.max(x0)),max(self.dpar["roi"][0][3],np.max(y0)) ]
            # TODO: colour the mesh according to cast.values

         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         # ~~> Extract variable data for only one time frame and one plane
         else:
            MESHX,MESHY,IKLE3 = elements
            # ~~> Multi-variables calculations
            if len(VARSORS) > 1:
               if "map" in vtype: VARSORS = [ np.sqrt(np.sum(np.power(np.dstack(VARSORS[0:2])[0],2),axis=1)) ]
            # ~~> Draw/Dump (multiple options possible)
            if "map" in vtype: drawColouredTriMaps(self.plt,what['deco'],(MESHX,MESHY,IKLE3,VARSORS[0]))
            elif "label" in vtype: drawLabeledTriContours(self.plt,what['deco'],(MESHX,MESHY,IKLE3,VARSORS[0]))
            elif "arrow" in vtype or "vector" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHY,VARSORS,False))
            elif "angle" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHY,VARSORS,True))
            else: print '... do not know how to draw this SELAFIN type: ' + vtype
            x0 = np.ravel(MESHX[IKLE3]); y0 = np.ravel(MESHY[IKLE3])
            if self.dpar["roi"][0] == []: self.dpar["roi"][0] = [ np.min(x0),np.min(y0),np.max(x0),np.max(y0) ]
            else: self.dpar["roi"][0] = [ min(self.dpar["roi"][0][0],np.min(x0)),min(self.dpar["roi"][0][1],np.min(y0)),max(self.dpar["roi"][0][2],np.max(x0)),max(self.dpar["roi"][0][3],np.max(y0)) ]

         """# /!\ WACLEO: Temporary fix because TOMAWAC's IOs names are not yet standard TELEMAC
      if 'WACLEO' in typl.upper() or \
         'SELAFIN' in typl.upper() or \
         'slf' in typl.lower():

         if what['type'].split(':')[1] == 'v-section':

            # ~~> Loop on variables
            for vtype in what['vars'].split(';'):
               vtype = vtype.split(':')[1]

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
                  cast = self.get(typl,what)
                  elements = cast.support
                  # ~~> Draw/Dump (works with triangles and quads)
                  drawMesh2DElements(self.plt,what['deco'],elements)
                  x0 = np.ravel(elements.T[0]); y0 = np.ravel(elements.T[1])
                  if self.dpar["roi"][0] == []: self.dpar["roi"][0] = [ np.min(x0),np.min(y0),np.max(x0),np.max(y0) ]
                  else: self.dpar["roi"][0] = [ min(self.dpar["roi"][0][0],np.min(x0)),min(self.dpar["roi"][0][1],np.min(y0)),max(self.dpar["roi"][0][2],np.max(x0)),max(self.dpar["roi"][0][3],np.max(y0)) ]
               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               else:
                  cast = self.get(typl,what)
                  MESHX,MESHZ,IKLE3 = cast.support
                  VARSORS = cast.values
                  # ~~> Multi-variables calculations
                  if len(VARSORS) > 1:
                     if "map" in vtype: VARSORS = [ np.sqrt(np.sum(np.power(np.dstack(VARSORS[0:3:2])[0],2),axis=1)) ]
                  # ~~> Draw/Dump (multiple options possible)
                  if "map" in vtype: drawColouredTriMaps(self.plt,what['deco'],(MESHX,MESHZ,IKLE3,VARSORS[0]))
                  if "label" in vtype: drawLabeledTriContours(self.plt,what['deco'],(MESHX,MESHZ,IKLE3,VARSORS[0]))
                  if "arrow" in vtype or "vector" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHZ,VARSORS,False))
                  if "angle" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHZ,VARSORS,True))
                  x0 = np.ravel(MESHX[IKLE3]); y0 = np.ravel(MESHZ[IKLE3])
                  if self.dpar["roi"][0] == []: self.dpar["roi"][0] = [ np.min(x0),np.min(y0),np.max(x0),np.max(y0) ]
                  else: self.dpar["roi"][0] = [ min(self.dpar["roi"][0][0],np.min(x0)),min(self.dpar["roi"][0][1],np.min(y0)),max(self.dpar["roi"][0][2],np.max(x0)),max(self.dpar["roi"][0][3],np.max(y0)) ]

         elif what['type'].split(':')[1] == 'p-section':

            # ~~> Loop on variables
            for vtype in what['vars'].split(';'):
               vtype = vtype.split(':')[1]

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               # ~~> Draw triangles and quads
               if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
                  cast = self.get(typl,what)
                  elements = cast.support
                  # ~~> Draw/Dump (works with triangles and quads)
                  drawMesh2DElements(self.plt,what['deco'],elements)
                  x0 = np.ravel(elements.T[0]); y0 = np.ravel(elements.T[1])
                  if self.dpar["roi"][0] == []: self.dpar["roi"][0] = [ np.min(x0),np.min(y0),np.max(x0),np.max(y0) ]
                  else: self.dpar["roi"][0] = [ min(self.dpar["roi"][0][0],np.min(x0)),min(self.dpar["roi"][0][1],np.min(y0)),max(self.dpar["roi"][0][2],np.max(x0)),max(self.dpar["roi"][0][3],np.max(y0)) ]

               # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               # ~~> Extract variable data for only one time frame and one plane
               else:
                  cast = self.get(typl,what)
                  MESHX,MESHY,IKLE3 = cast.support
                  VARSORS = cast.values
                  # ~~> Multi-variables calculations
                  if len(VARSORS) > 1:
                     if "map" in vtype: VARSORS = [ np.sqrt(np.sum(np.power(np.dstack(VARSORS[0:2])[0],2),axis=1)) ]
                  # ~~> Draw/Dump (multiple options possible)
                  if "map" in vtype: drawColouredTriMaps(self.plt,what['deco'],(MESHX,MESHY,IKLE3,VARSORS[0]))
                  elif "label" in vtype: drawLabeledTriContours(self.plt,what['deco'],(MESHX,MESHY,IKLE3,VARSORS[0]))
                  elif "arrow" in vtype or "vector" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHY,VARSORS,False))
                  elif "angle" in vtype: drawColouredTriVects(self.plt,what['deco'],(MESHX,MESHY,VARSORS,True))
                  else: print '... do not know how to draw this SELAFIN type: ' + vtype
                  x0 = np.ravel(MESHX[IKLE3]); y0 = np.ravel(MESHY[IKLE3])
                  if self.dpar["roi"][0] == []: self.dpar["roi"][0] = [ np.min(x0),np.min(y0),np.max(x0),np.max(y0) ]
                  else: self.dpar["roi"][0] = [ min(self.dpar["roi"][0][0],np.min(x0)),min(self.dpar["roi"][0][1],np.min(y0)),max(self.dpar["roi"][0][2],np.max(x0)),max(self.dpar["roi"][0][3],np.max(y0)) ]
         """

      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to do this type of extraction: ' + what['type'].split(':')[1]

      # ~~> unkonwn
      #else: # TODO: raise exception
      #   print '... do not know how to draw from this format: ' + typl

      #if (what['deco'].has_key('set')):
      #   cb = eval("self.plt."+what['deco']['set']+"()")

   def show(self):
      deco(self.plt,self.upar,self.dpar)
      self.plt.show()
      self.plt.close()

   def save(self,fileName):
      deco(self.plt,self.upar,self.dpar)
      try:
         stat(path.split(fileName)[0])
      except:
         mkdir(path.split(fileName)[0])
      self.plt.savefig(fileName, bbox_inches='tight')
      self.plt.close()

# _____               ______________________________________________
# ____/ Mesh Toolbox /_____________________________________________/
#
#drawMesh* applies to mesh of triangles / quads
#   - by default this is drawn as a uni-colour wireframe
#   - see also drawCoulouredMesh*
#Note: triplot could be used but would not be valid for quads

   def drawColouredMeshLines(self,edges,decoUser): pass

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

def drawGridContours(plt,decoUser,(x,y,z)):

   # ~~> Focus on current subplot / axes instance
   #crax = plt.gca()

   # ~~ Split contours in major/minor classes ~~~~~~~~~~~~~~~~~~~~~~

   # ~~> Draw major contours
   cs1 = plt.contour(x,y,z, levels[::2], colors = decoUser['contour.major.color'], hold='on')
   for c in cs1.collections:
      c.set_linestyle(decoUser['contour.major.style'])
      c.set_zorder(zorder)

   # ~~> Draw minor contours
   cs2 = plt.contour(x,y,z, levels[1::2], colors = decoUser['contour.minor.color'], hold='on')
   for c in cs2.collections:
      c.set_linestyle(decoUser['contour.minor.style'])
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

def drawColouredQuadMaps(plt,decoUser,(nelem,npoin,ndp,nplan),(x,y,ikle,z)):

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
   xmin = min(decoUser['roi'][0][0],decoUser['roi'][1][0])
   xmax = max(decoUser['roi'][0][0],decoUser['roi'][1][0])
   ymin = min(decoUser['roi'][0][1],decoUser['roi'][1][1])
   ymax = max(decoUser['roi'][0][1],decoUser['roi'][1][1])
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
