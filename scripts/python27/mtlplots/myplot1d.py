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

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from copy import deepcopy
import numpy as np
# ~~> matplotlib and pyplot
import matplotlib as mpl
try: hide_default = not sys.stderr.isatty()
except AttributeError: hide_default = True # output does not support isatty()
if hide_default: mpl.use('Agg') # Use of Agg must be done before importing matplotlib.pyplot
import matplotlib.pyplot as plt
# ~~> dependencies towards other mtlplots
from plotTELEMAC import getColourMap
# ~~> dependencies towards other pytel/modules
from samplers.mycast import Caster
from parsers.parserCSV import CSV
from parsers.parserStrings import parseArrayFrame, parseArrayPaires

# _____               ______________________________________________
# ____/ Default DECO /_____________________________________________/
#

decoDefault = {
   "size":'(10;10)', "aspect":'1', "dpi":'', "ratio2d": '', "title": '', "roi": '', "type":'',
   ### LINES
   # See http://matplotlib.org/api/artist_api.html#module-matplotlib.lines for more
   # information on line properties.
   'lines.linewidth'          : 1.0,            # line width in points
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
                  mpar[key] =  parseArrayPaires(upar[key])[0]
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
      elif key == "size":
         if upar[key] != '':
            mpar.update({ 'figure.figsize': parseArrayPaires(upar[key])[0]})
         del upar[key]

   return mpar,upar

# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def drawHistoryLines(myplt,decoUser,x,ys):

   axes = myplt.axis()
   # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xname,x0 = x
   myplt.xlabel(xname)
   ynames,y0 = ys
   dim = len(ynames) - 1
   # ynames[0] is an extra meta data
   if dim == 1:
      n0 = ynames[1]
      for i0 in range(len(n0)): # each variables
         myplt.ylabel(n0[i0])
         myplt.plot(x0,y0[i0],**decoUser)
   elif dim == 2:
      # ~~> This is called for 1d:history, for instance
      n0,n1 = ynames[1:]
      for i1 in range(len(n1)):    # each location
         for i0 in range(len(n0)): # each variables
            #myplt.ylabel(str(n1[])) you could label each curve in time and plan
            myplt.plot(x0,y0[i0][i1],**decoUser)
   elif dim == 3:
      # ~~> This is called for 1d:v-section, for instance
      n0,n1,n2 = ynames[1:]
      for i2 in range(len(n2)):       # each plan
         for i1 in range(len(n1)):    # each time
            for i0 in range(len(n0)): # each variables
               #myplt.ylabel(str(n1[])) you could label each curve in time and plan
               myplt.plot(x0,y0[i0][i1][i2],**decoUser)
   elif dim == 4:
      n0,n1,n2,n3 = ynames[1:]
      for i3 in range(len(n3)):
         for i2 in range(len(n2)):
            for i1 in range(len(n1)):
               for i0 in range(len(n0)):
                  #myplt.ylabel(str(n1[]))
                  myplt.plot(x0,y0[i0][i1][i2][i3],**decoUser)

   return x0,y0

def drawPolylineLines(myplt,decoUser,x,ys):

   axes = myplt.axis()
   # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xname,x0 = x
   myplt.xlabel(xname)
   ynames,y0 = ys
   dim = len(ynames) - 1
   # ynames[0] is an extra meta data
   if dim == 1:
      n0 = ynames[1]
      for i0 in range(len(n0)): # each variables
         myplt.ylabel(n0[i0])
         myplt.plot(x0,y0[i0],**decoUser)
   elif dim == 2:
      n0,n1 = ynames[1:]
      for i1 in range(len(n1)):    # each time
         for i0 in range(len(n0)): # each variables
            #myplt.ylabel(n0[i])
            myplt.plot(x0,y0[i0][i1],**decoUser)
            #myplot.plot[i].updte()
   elif dim == 3:
      # ~~> This is called for 1d:v-section, for instance
      n0,n1,n2 = ynames[1:]
      for i2 in range(len(n2)):       # each plan
         for i1 in range(len(n1)):    # each time
            for i0 in range(len(n0)): # each variables
               #myplt.ylabel(str(n1[])) you could label each curve in time and plan
               myplt.plot(x0,y0[i0][i1][i2],**decoUser)
   elif dim == 4:
      n0,n1,n2,n3 = ynames[1:]
      for i3 in range(len(n3)):
         for i2 in range(len(n2)):
            for i1 in range(len(n1)):
               for i0 in range(len(n0)):
                  #myplt.ylabel(str(n1[]))
                  myplt.plot(x0,y0[i0][i1][i2][i3],**decoUser)

   return x0,y0

# _____                      _______________________________________
# ____/ Primary Method:Deco /______________________________________/
#

def deco(myplt,upar,x0,y0):

   # ~~ Axis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # TODO: Replace white margins with user parameter
   if upar["roi"] != '':
      roi,mar = upar["roi"]
      (xmin,ymin),(xmax,ymax) = roi
      emar,nmar,wmar,smar = mar
      xgap = xmax - xmin
      xmin -= emar*xgap; xmax += wmar*xgap
      ygap = ymax - ymin
      ymin -= smar*ygap; ymax += nmar*ygap
      myplt.axis([xmin,xmax,ymin,ymax])
   else:
      xmin = np.min(x0); xmax = np.max(x0)
      ymin = np.min(y0); ymax = np.max(y0)
      if upar.has_key("setroi"):
         xmin = min(upar["setroi"][0][0][0],xmin)
         ymin = min(upar["setroi"][0][0][1],ymin)
         xmax = max(upar["setroi"][0][1][0],xmax)
         ymax = max(upar["setroi"][0][1][1],ymax)
         upar["setroi"] = [ [[xmin,ymin],[xmax,ymax]],[0.02,0.02,0.02,0.02] ]
         xgap = xmax - xmin
         xmin -= upar["setroi"][1][2]*xgap
         xmax += upar["setroi"][1][0]*xgap
         ygap = ymax - ymin
         ymin -= upar["setroi"][1][3]*ygap
         ymax += upar["setroi"][1][1]*ygap
      else:
         upar.update({ "setroi":[ [[xmin,ymin],[xmax,ymax]],[0.02,0.02,0.02,0.02] ] })
         xgap = xmax - xmin
         xmin -= upar["setroi"][1][2]*xgap
         xmax += upar["setroi"][1][0]*xgap
         ygap = ymax - ymin
         ymin -= upar["setroi"][1][3]*ygap
         ymax += upar["setroi"][1][1]*ygap
      myplt.axis([xmin,xmax,ymin,ymax])

   return

# _____                         ____________________________________
# ____/ Primary Classes:Dumper /___________________________________/
#

class Dumper1D(Caster):

   def __init__(self,caster,dump):
      Caster.__init__(self,{'object':caster.object,'obdata':caster.obdata})
      self.obtype = dump['outFormat']
      self.oudata = None

   def add(self,typl,what):
      Caster.add(self,typl,what)

      # ~~> only csv is recognised fr now
      if self.obtype != 'csv': # TODO: raise exception
         print '... do not know how to write to this format: ' + self.obtype
         sys.exit(1)

      # ~~> initialisation
      if not self.oudata: self.oudata = CSV()

      # ~~> write-up
      cast = self.get(typl,what)

      # ~~> 1D time history from 2D or 3D results
      if what['type'].split(':')[1] == 'history' or 'sortie' in typl.lower():
         #try: # if instance unit exist
         self.oudata.addColumns(( cast.unit,cast.support ),( cast.function,cast.values ))
         #except:
         #   self.oudata.addColumns(( 'unit',cast.support ),( ('history','function'),cast.values ))

      # ~~> 1D vertical cross section from 2D or 3D results
      elif what['type'].split(':')[1] == 'v-section' or 'csv' in typl.lower():
         #cast = self.get(typl,what)
         try: # if instance unit exist
            self.oudata.addColumns(( cast.unit,cast.support ),( cast.function,cast.values ))
         except:
            dim = len(cast.values.shape)
            fct = ['v-section']
            if dim > 1: fct.append([ 'VAR'+str(ivar) for ivar in range(cast.values.shape[0]) ])
            if dim > 2: fct.append([ str(itim) for itim in range(cast.values.shape[1]) ])
            if dim > 3: fct.append([ str(ilay) for ilay in range(cast.values.shape[2]) ])
            self.oudata.addColumns(( 'unit',cast.support ),( fct,cast.values ))

      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to extract from this format: ' + typl
         sys.exit(1)

   def save(self,fileName):
      self.oudata.putFileContent(fileName)

# _____                          ___________________________________
# ____/ Primary Classes: Figure /__________________________________/
#
class Figure1D(Caster):

   def __init__(self,caster,plot):
      Caster.__init__(self,{'object':caster.object,'obdata':caster.obdata})

      # ~~~ special case for size ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if 'size' in plot.keys():
         if 'look' in plot['deco'].keys():
            for l in plot['deco']['look']:
               l.update({'size':plot['size']})
         else: plot['deco'].update({'look':[{'size':plot['size']}]})
         del plot['size']
      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.mpar,self.upar = mapDecoDefault( plot['deco'],decoDefault )
      mpl.rcParams.update(self.mpar)
      # ~~> by default, there is no grid in 1D
      #self.mpar.update({ 'grid.alpha':1.0 })

      # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      fig = plt.figure()
      # ~~> add_subplot, or ax definition
      fig.add_subplot(111)
      # fig.add_subplot(111) returns an Axes instance, where we can plot and this is
      #   also the reason why we call the variable referring to that instance ax.
      #   This is a common way to add an Axes to a Figure, but add_subplot() does a
      #   bit more: it adds a subplot. So far we have only seen a Figure with one Axes
      #   instance, so only one area where we can draw, but Matplotlib allows more than one

      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      mpl.rcdefaults()

      # ~~> user params
      if self.upar['title'] != '': plt.title(self.upar['title'])
      # ~~> type of plot
      if self.upar['type'] != '':
         if self.upar['type'][1] == 'line' or self.upar['type'][1] == 'rose':
            fig.add_subplot(adjustable='datalim')
         else:
            fig.add_subplot(aspect='equal')
      else: fig.add_subplot(aspect='equal')
      # ~~> region of interes and defaault margins
      if self.upar["roi"] != '': self.upar["roi"] = [ parseArrayPaires(self.upar["roi"]), [0,0,0,0] ]

      self.plt = plt
      self.fig = fig

   def add(self,typl,what):
      Caster.add(self,typl,what)

      if len(what['vars'].split(';')) > 1: # TODO: raise exception
         print '... do not know support multiple variables anymore: ' + what['vars']
         sys.exit(1)

      # ~~> 1D time history from 2D or 3D results
      if what['type'].split(':')[1] == 'history' or 'sortie' in typl.lower():
         cast = self.get(typl,what)
         #try: # if instance unit exist
         x0,y0 = drawHistoryLines(self.plt,what['deco'],( cast.unit,cast.support ),( cast.function,cast.values ))
         deco(self.plt,self.upar,x0,y0)
         #except:
         #   x0,y0 = drawHistoryLines(self.plt,( 'unit',cast.support ),( ('history','function'),cast.values ))
         #   deco(self.plt,self.upar,x0,y0)

      # ~~> 1D time history from 2D or 3D results
      elif what['type'].split(':')[1] == 'v-section' or 'csv' in typl.lower():
         cast = self.get(typl,what)
         try: # if instance unit exist
            x0,y0 = drawPolylineLines(self.plt,what['deco'],( cast.unit,cast.support ),( cast.function,cast.values ))
            #deco(self.plt,self.upar,x0,y0)
         except:
            dim = len(cast.values.shape)
            fct = ['v-section']
            if dim > 1: fct.append([ 'VAR'+str(ivar) for ivar in range(cast.values.shape[0]) ])
            if dim > 2: fct.append([ str(itim) for itim in range(cast.values.shape[1]) ])
            if dim > 3: fct.append([ str(ilay) for ilay in range(cast.values.shape[2]) ])
            x0,y0 = drawPolylineLines(self.plt,what['deco'],( 'unit',cast.support ),( fct,cast.values ))
            #deco(self.plt,self.upar,x0,y0)

      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to draw from this format: ' + typl
         sys.exit(1)

   def show(self):
      self.plt.show()
      self.plt.close()

   def save(self,fileName):
      self.plt.savefig(fileName)
      self.plt.close()


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

   sys.exit(0)
