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
import numpy as np
# ~~> matplotlib and pyplot
import matplotlib as mpl
try: hide_default = not sys.stderr.isatty()
except AttributeError: hide_default = True # output does not support isatty()
if hide_default: mpl.use('Agg') # Use of Agg must be done before importing matplotlib.pyplot
import matplotlib.pyplot as plt
# ~~> dependencies towards other pytel/modules
from utils.files import getFileContent
from parsers.parserCSV import CSV
from parsers.parserSortie import getValueHistorySortie
from parsers.parserSELAFIN import SELAFIN,getValueHistorySLF,getValuePolylineSLF,subsetVariablesSLF
from samplers.meshes import xysLocateMesh,sliceMesh
from parsers.parserStrings import parseArrayFrame,parseArrayPoint,parseArrayPaires

# _____               ______________________________________________
# ____/ Default DECO /_____________________________________________/
#

decoDefault = {
   ### LINES
   # See http://matplotlib.org/api/artist_api.html#module-matplotlib.lines for more
   # information on line properties.
   'lines' : {
      'linewidth'          : 1.0,            # line width in points
      'linestyle'          : '-',            # solid line
      'color'              : 'blue',         # has no affect on plot(); see axes.color_cycle
      'marker'             : None,           # the default marker
      'markeredgewidth'    : 0.5,            # the line width around the marker symbol
      'markersize'         : 6,              # markersize, in points
      'dash_joinstyle'     : 'miter',        # miter|round|bevel
      'dash_capstyle'      : 'butt',         # butt|round|projecting
      'solid_joinstyle'    : 'miter',        # miter|round|bevel
      'solid_capstyle'     : 'projecting',   # butt|round|projecting
      'antialiased'        : True            # render lines in antialised (no jaggies)
   },
   ### AXES
   # default face and edge color, default tick sizes,
   # default fontsizes for ticklabels, and so on.  See
   # http://matplotlib.org/api/axes_api.html#module-matplotlib.axes
   'axes' : {
      'hold'               : True,           # whether to clear the axes by default on
      'facecolor'          : 'white',        # axes background color
      'edgecolor'          : 'black',        # axes edge color
      'linewidth'          : 1.0,            # edge linewidth
      'grid'               : False,          # display grid or not
      'titlesize'          : 'large',        # fontsize of the axes title
      'labelsize'          : 'medium',       # fontsize of the x any y labels
      'labelweight'        : 'normal',       # weight of the x and y labels
      'labelcolor'         : 'black',
      'axisbelow'          : False,          # whether axis gridlines and ticks are below
                                             # the axes elements (lines, text, etc)
      'formatter.limits'   : [-7, 7],        # use scientific notation if log10
                                             # of the axis range is smaller than the
                                             # first or larger than the second
      'formatter.use_locale' : False,        # When True, format tick labels
                                             # according to the user's locale.
                                             # For example, use ',' as a decimal
                                             # separator in the fr_FR locale.
      'formatter.use_mathtext' : False,      # When True, use mathtext for scientific
                                             # notation.
      'unicode_minus'      : True,           # use unicode for the minus symbol
                                             # rather than hyphen.  See
                                             # http://en.wikipedia.org/wiki/Plus_and_minus_signs#Character_codes
      'color_cycle'        : [ 'b', 'g', 'r', 'c', 'm', 'y', 'k' ],
                                             # color cycle for plot lines
                                             # as list of string colorspecs:
                                             # single letter, long name, or
                                             # web-style hex
      'xmargin'            : 0,              # x margin.  See `axes.Axes.margins`
      'ymargin'            : 0               # y margin See `axes.Axes.margins`
   },
   'polaraxes.grid'        : True,           # display grid on polar axes
   'axes3d.grid'           : True,           # display grid on 3d axes
   ### TICKS
   # see http://matplotlib.org/api/axis_api.html#matplotlib.axis.Tick
   'xtick' : {
      'major.size'         : 4,              # major tick size in points
      'minor.size'         : 2,              # minor tick size in points
      'major.width'        : 0.5,            # major tick width in points
      'minor.width'        : 0.5,            # minor tick width in points
      'major.pad'          : 4,              # distance to major tick label in points
      'minor.pad'          : 4,              # distance to the minor tick label in points
      'color'              : 'k',            # color of the tick labels
      'labelsize'          : 'medium',       # fontsize of the tick labels
      'direction'          : 'in'            # direction: in, out, or inout
   },
   'ytick' : {
      'major.size'         : 4,              # major tick size in points
      'minor.size'         : 2,              # minor tick size in points
      'major.width'        : 0.5,            # major tick width in points
      'minor.width'        : 0.5,            # minor tick width in points
      'major.pad'          : 4,              # distance to major tick label in points
      'minor.pad'          : 4,              # distance to the minor tick label in points
      'color'              : 'k',            # color of the tick labels
      'labelsize'          : 'medium',       # fontsize of the tick labels
      'direction'          : 'in'            # direction: in, out, or inout
   },
   ### GRIDS
   'grid' : {
      'color'              : 'black',        # grid color
      'linestyle'          : ':',            # dotted
      'linewidth'          : 0.5,            # in points
      'alpha'              : 1.0             # transparency, between 0.0 and 1.0
   },
   ### LEGEND
   'legend' : {
      'fancybox'           : False,          # if True, use a rounded box for the
                                             # legend, else a rectangle
      'legend.isaxes'      : True,
      'numpoints'          : 2,              # the number of points in the legend line
      'fontsize'           : 'large',
      'borderpad'          : 0.5,            # border whitespace in fontsize units
      'markerscale'        : 1.0,            # the relative size of legend markers vs. original
                                             # the following dimensions are in axes coords
      'labelspacing'       : 0.5,            # the vertical space between the legend entries in fraction of fontsize
      'handlelength'       : 2.,             # the length of the legend lines in fraction of fontsize
      'handleheight'       : 0.7,            # the height of the legend handle in fraction of fontsize
      'handletextpad'      : 0.8,            # the space between the legend line and legend text in fraction of fontsize
      'borderaxespad'      : 0.5,            # the border between the axes and legend edge in fraction of fontsize
      'columnspacing'      : 2.,             # the border between the axes and legend edge in fraction of fontsize
      'shadow'             : False,
      'frameon'            : True,           # whether or not to draw a frame around legend
      'scatterpoints'      : 3               # number of scatter points
   }
}

# _____                         ____________________________________
# ____/ Primary Method:Extract /___________________________________/
#

def extractSortie(what):

   # ~~> Load data
   # what['file']: has only one file
   sortie = getFileContent(what['file'])
   # ~~> Extract data
   # what['vars']: list of pairs variables:support delimited by ';' (/!\ support is ignored)
   return getValueHistorySortie(sortie,what['vars'])


def extractCSV(what):
   # ~~> Load data
   # what['file']: has only one file
   csv = CSV()
   csv.getFileContent(what['file'])
   # ~~> Extract data
   # what['vars']: list of pairs variables:support delimited by ';' (/!\ support is ignored)
   t,data = csv.getColumns(what["vars"])
   return t, [('experiment',data[0],data[1])]

def extractHistorySELAFIN(what):

   # ~~> Load data
   slf = SELAFIN(what['file'])
   slf.setKDTree()
   slf.setMPLTri()

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
   return ('Time (s)',slf.tags['times'][t]), \
      [('',[n.replace(' ','_').replace(',',';') for n in vars[1]],[(str(n[0])+':'+str(m)).replace(' ','').replace(',',';') for n in pAP for m in n[1]],data)]

def extractPolylineSELAFIN(what):
   
   # ~~> Load data
   slf = SELAFIN(what['file'])
   slf.setKDTree()
   slf.setMPLTri()

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
   return ('Distance (m)',d), \
      [('v-section',vars[1],t,zpo,data)]


# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def drawHistoryLines(myplt,x,ys):

   # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xname,x0 = x
   myplt.xlabel(xname)
   for y in ys:
      dim = len(y) - 1
      if dim == 1:
         n0,y0 = y
         myplt.ylabel(n0)
         myplt.plot(x0,y0)
         #myplt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
      elif dim == 2:
         n0,n1,y0 = y
         for i in range(len(y0)):
            #myplt.ylabel(n0[i])
            myplt.plot(x0,y0[i])
      elif dim == 3:
         n0,n1,n2,y0 = y
         for i in range(len(y0)):
            for j in range(len(y0[i])):
               #myplt.ylabel(n0[i])
               myplt.plot(x0,y0[i][j])
      elif dim == 4:
         n0,n1,n2,n4,y0 = y
         for i in range(len(y0)):
            for j in range(len(y0[i])):
               for k in range(len(y0[i][j])):
                  myplt.plot(x0,y0[i][j][k])

   return x0,y0

def drawPolylineLines(myplt,x,ys):

   axes = myplt.axis()
   # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xname,x0 = x
   myplt.xlabel(xname)
   for y in ys:
      dim = len(y) - 1
      if dim == 1:
         n0,y0 = y
         myplt.ylabel(n0)
         myplt.plot(x0,y0)
         #myplt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
      elif dim == 2:
         n0,n1,y0 = y
         for i in range(len(y0)):
            #myplt.ylabel(n0[i])
            myplt.plot(x0,y0[i])
      elif dim == 3:
         n0,n1,n2,y0 = y
         for i in range(len(y0)):
            for j in range(len(y0[i])):
               #myplt.ylabel(str(n1[i]))
               myplt.plot(x0,y0[i][j])
      elif dim == 4:
         n0,n1,n2,n3,y0 = y
         for i in range(len(y0)):
            for j in range(len(y0[i])):
               for k in range(len(y0[i][j])):
                  myplt.plot(x0,y0[i][j][k])

   return x0,y0

# _____                      _______________________________________
# ____/ Primary Method:Deco /______________________________________/
#

def deco(myplt,upar,x0,y0):

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

   return

# _____                         ____________________________________
# ____/ Primary Classes:Dumper /___________________________________/
#

class Dumper1D:
   
   def __init__(self,task):
      self.data = None
      if task['outFormat'] == 'csv': self.data = CSV()
      else: # TODO: raise exception
         print '... do not know how to save in this format: ' + task['outFormat']
   
   def add(self,typl,what):
      # ~~> sortie file
      if 'sortie' in typl.lower():
         time,values = extractSortie(what)
         self.data.addColumns(time,values)
      # ~~> csv file
      elif 'csv' in typl.lower():
         dist,values = extractCSV(what)
         self.data.addColumns(dist,values)
      # ~~> SELAFIN file
      if 'SELAFIN' in typl.upper() or 'slf' in typl.lower():
         if what['type'] == 'history':   # ~~> 1D time history from 2D or 3D results
            time,values = extractHistorySELAFIN(what)
            self.data.addColumns(time,values)
         elif what['type'] == 'v-section':
            dist,values = extractPolylineSELAFIN(what)
            self.data.addColumns(dist,values)
         else: print '... do not know how to draw this SELAFIN type: ' + what['type']
      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to extract from this format: ' + typl

   def save(self,fileName): self.data.putFileContent(fileName)

# _____                          ___________________________________
# ____/ Primary Classes: Figure /__________________________________/
#
class Figure1D:

   def __init__(self,plot):

      # ~~~ melting the pot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.mpar = {} # mpar, contains the matplotlib parameters (defaults)
      self.upar = {} # upar, contains the user parameters (from the XML)
      for name in ['look','data']:
         if plot['deco'].has_key(name): self.upar.update(plot['deco'][name][0])

      # ~~~ special conversions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Replaces the relevat mpar by the upar values
      for key in self.upar.keys(): # /!\ the .keys() is necessary
         if key in mpl.rcParams.keys():
            if type(mpl.rcParams[key]) == type([]):
               if type(mpl.rcParams[key][0]) == type(1) or type(mpl.rcParams[key][0]) == type(1.0):
                  self.mpar[key] = parseArrayFrame(self.upar[key])
                  del self.upar[key]
               #elif type(mpl.rcParams[key][0]) == type(""): pass
               else:
                  print '... I did not know ',type(mpl.rcParams[key]),' could be an acceptable type'
                  sys.exit()
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
               sys.exit()
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

      if 'sortie' in typl.lower():
         time,values = extractSortie(what)
         x0,y0 = drawHistoryLines(self.plt,time,values)
         deco(self.plt,self.upar,x0,y0)

      elif 'csv' in typl.lower():
         dist,values = extractCSV(what)
         x0,y0 = drawPolylineLines(self.plt,dist,values)
         deco(self.plt,self.upar,x0,y0)

      elif 'SELAFIN' in typl.upper() or 'slf' in typl.lower():
         if what['type'] == 'history':   # ~~> 1D time history from 2D or 3D results
            time,values = extractHistorySELAFIN(what)
            x0,y0 = drawHistoryLines(self.plt,time,values)
            deco(self.plt,self.upar,x0,y0)
         elif what['type'] == 'v-section':
            dist,values = extractPolylineSELAFIN(what)
            x0,y0 = drawPolylineLines(self.plt,dist,values)
            deco(self.plt,self.upar,x0,y0)
         else: print '... do not know how to draw this SELAFIN type: ' + what['type']

      # ~~> unkonwn
      else: # TODO: raise exception
         print '... do not know how to extract from this format: ' + typl

   def show(self): self.plt.show()
   def save(self,fileName): self.plt.savefig(fileName)


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

   sys.exit(0)
