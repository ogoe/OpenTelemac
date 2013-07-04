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
#import matplotlib.pyplot.legend as legend

# _____                   __________________________________________
# ____/ Plotting Toolbox /_________________________________________/
#

def drawHistoryLines(plt,(x,ys),deco):

   # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xname,x0 = x
   plt.xlabel(xname)
   for y in ys:
      dim = len(y) - 1
      if dim == 1:
         n0,y0 = y
         plt.ylabel(n0)
         plt.plot(x0,y0)
         #plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
      elif dim == 2:
         n0,n1,y0 = y
         for i in range(len(y0)):
            #plt.ylabel(n0[i])
            plt.plot(x0,y0[i])
      elif dim == 3:
         n0,n1,n2,y0 = y
         for i in range(len(y0)):
            for j in range(len(y0[i])):
               #plt.ylabel(n0[i])
               plt.plot(x0,y0[i][j])

   # ~~ Deco ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print deco
   # ~~> plt.grid
   if deco.has_key('grid'): plt.grid(deco['grid'])
   # ~~> plt.axis
   xmin = np.min(x0); xmax = np.max(x0)
   xgap = xmax-xmin
   xmin -= 0.1*xgap; xmax += 0.1*xgap
   ymin = np.min(y0); ymax = np.max(y0)
   ygap = ymax-ymin
   ymin -= 0.1*ygap; ymax += 0.1*ygap
   if deco.has_key('roi'):
      if deco['roi'] != '':
         xmin = min(deco['roi'][0][0],deco['roi'][1][0])
         xmax = max(deco['roi'][0][0],deco['roi'][1][0])
         ymin = min(deco['roi'][0][1],deco['roi'][1][1])
         ymax = max(deco['roi'][0][1],deco['roi'][1][1])
   #crax = plt.gca()
   #crax.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
   #crax.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
   plt.axis([xmin,xmax,ymin,ymax])

   return


def drawPolylineLines(plt,(x,ys),deco):

   # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xname,x0 = x
   plt.xlabel(xname)
   for y in ys:
      dim = len(y) - 1
      if dim == 1:
         n0,y0 = y
         plt.ylabel(n0)
         plt.plot(x0,y0)
         #plt.axis([np.min(x0),np.max(x0),np.min(y0),np.max(y0)])
      elif dim == 2:
         n0,n1,y0 = y
         for i in range(len(y0)):
            #plt.ylabel(n0[i])
            plt.plot(x0,y0[i])
      elif dim == 3:
         n0,n1,n2,y0 = y
         for i in range(len(y0)):
            for j in range(len(y0[i])):
               #plt.ylabel(str(n1[i]))
               plt.plot(x0,y0[i][j])

   # ~~ Deco ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> plt.grid
   if deco.has_key('grid'): plt.grid(deco['grid'])
   # ~~> plt.axis
   xmin = np.min(x0); xmax = np.max(x0)
   xgap = xmax-xmin
   xmin -= 0.1*xgap; xmax += 0.1*xgap
   ymin = np.min(y0); ymax = np.max(y0)
   ygap = ymax-ymin
   ymin -= 0.1*ygap; ymax += 0.1*ygap
   if deco.has_key('roi'):
      if deco['roi'] != '':
         xmin = min(deco['roi'][0][0],deco['roi'][1][0])
         xmax = max(deco['roi'][0][0],deco['roi'][1][0])
         ymin = min(deco['roi'][0][1],deco['roi'][1][1])
         ymax = max(deco['roi'][0][1],deco['roi'][1][1])
   #crax = plt.gca()
   #crax.set_xlim(xmin,xmax)   # sets x axis limits, default 0-1
   #crax.set_ylim(ymin,ymax)   # sets y axis limits, default 0-1
   plt.axis([xmin,xmax,ymin,ymax])

   return

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

   sys.exit()
