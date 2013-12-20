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
from parsers.parserStrings import parseArrayFrame,parseArrayPaires

# _____                          ___________________________________
# ____/ Primary Classes: Drawer /__________________________________/
#
class Drawer1D:

   def __init__(self,plot):

      # ~~~ melting the pot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.mpar = {}
      self.upar = {}
      for name in ['look','data']:
         if plot['deco'].has_key(name): self.upar.update(plot['deco'][name][0])

      # ~~~ special conversions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

   def doHistoryLines(self,(x,ys),deco):

      # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      xname,x0 = x
      self.plt.xlabel(xname)
      for y in ys:
         dim = len(y) - 1
         if dim == 1:
            n0,y0 = y
            self.plt.ylabel(n0)
            self.plt.plot(x0,y0)
            #self.plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
         elif dim == 2:
            n0,n1,y0 = y
            for i in range(len(y0)):
               #self.plt.ylabel(n0[i])
               self.plt.plot(x0,y0[i])
         elif dim == 3:
            n0,n1,n2,y0 = y
            for i in range(len(y0)):
               for j in range(len(y0[i])):
                  #self.plt.ylabel(n0[i])
                  self.plt.plot(x0,y0[i][j])
         elif dim == 4:
            n0,n1,n2,n4,y0 = y
            for i in range(len(y0)):
               for j in range(len(y0[i])):
                  for k in range(len(y0[i][j])):
                     self.plt.plot(x0,y0[i][j][k])

      # ~~ Deco ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # ~~> plt.axis
      if not self.upar.has_key("roi"):
         xmin = np.min(x0); xmax = np.max(x0)
         ymin = np.min(y0); ymax = np.max(y0)
         if not self.upar.has_key("setroi"):
            xgap = xmax-xmin
            xmin -= 0.02*xgap; xmax += 0.02*xgap
            ygap = ymax-ymin
            ymin -= 0.02*ygap; ymax += 0.02*ygap
            self.upar.update({ "setroi":[xmin,xmax,ymin,ymax] })
         else:
            xgap = max(self.upar["setroi"][1],xmax) - min(self.upar["setroi"][0],xmin)
            xmin -= 0.02*xgap; xmax += 0.02*xgap
            ygap = max(self.upar["setroi"][3],ymax) - min(self.upar["setroi"][2],ymin)
            ymin -= 0.02*ygap; ymax += 0.02*ygap
            self.upar["setroi"] = [xmin,xmax,ymin,ymax]
         self.plt.axis(self.upar["setroi"])

      return

   def doPolylineLines(self,(x,ys),deco):

      axes = self.plt.axis()
      # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      xname,x0 = x
      self.plt.xlabel(xname)
      for y in ys:
         dim = len(y) - 1
         if dim == 1:
            n0,y0 = y
            self.plt.ylabel(n0)
            self.plt.plot(x0,y0)
            #self.plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
         elif dim == 2:
            n0,n1,y0 = y
            for i in range(len(y0)):
               #self.plt.ylabel(n0[i])
               self.plt.plot(x0,y0[i])
         elif dim == 3:
            n0,n1,n2,y0 = y
            for i in range(len(y0)):
               for j in range(len(y0[i])):
                  #self.plt.ylabel(str(n1[i]))
                  self.plt.plot(x0,y0[i][j])
         elif dim == 4:
            n0,n1,n2,n3,y0 = y
            for i in range(len(y0)):
               for j in range(len(y0[i])):
                  for k in range(len(y0[i][j])):
                     self.plt.plot(x0,y0[i][j][k])

      # ~~ Deco ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # ~~> plt.axis # TODO: Replace white margins
      if not self.upar.has_key("roi"):
         xmin = np.min(x0); xmax = np.max(x0)
         ymin = np.min(y0); ymax = np.max(y0)
         if not self.upar.has_key("setroi"):
            xgap = xmax-xmin
            xmin -= 0.02*xgap; xmax += 0.02*xgap
            ygap = ymax-ymin
            ymin -= 0.02*ygap; ymax += 0.02*ygap
            self.upar.update({ "setroi":[xmin,xmax,ymin,ymax] })
         else:
            xgap = max(self.upar["setroi"][1],xmax) - min(self.upar["setroi"][0],xmin)
            xmin -= 0.02*xgap; xmax += 0.02*xgap
            ygap = max(self.upar["setroi"][3],ymax) - min(self.upar["setroi"][2],ymin)
            ymin -= 0.02*ygap; ymax += 0.02*ygap
            self.upar["setroi"] = [xmin,xmax,ymin,ymax]
         self.plt.axis(self.upar["setroi"])
      
      return

   def show(self): self.plt.show()
   def savefig(self,fileName): self.plt.savefig(fileName)

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

   sys.exit(0)
