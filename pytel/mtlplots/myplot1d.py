"""@author David H. Roscoe and Sebastien E. Bourban
"""
"""@note ... this work is based on a collaboration effort between
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

# _____                   __________________________________________
# ____/ Plotting Toolbox /_________________________________________/
#

def drawHistoryLines(plt,(x,ys),deco):

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
      if deco['grid']: plt.grid(True)

   return


def drawPolylineLines(plt,(x,ys),deco):

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

      if deco['grid']: plt.grid(True)

   return

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$19-Jul-2011 08:51:29$"

if __name__ == "__main__":

   sys.exit()
