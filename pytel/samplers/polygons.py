"""@author Sebastien E. Bourban
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
"""@history 26/12/2011 -- Sebastien E. Bourban
"""
"""@brief
      Tools for trivial polygon operations
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import numpy as np
# ~~> dependencies towards other pytel/modules
from utils.geometry import getConeAngle,isClose,getNorm2

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#
def joinSegments(polyLines):

   polyGones = []
   while polyLines != []:
      # ~~> starting point
      e = polyLines[0]
      a,b = e[0],e[len(e)-1]
      # ~~> case of closed line
      if a == b:
         polyGones.append(e[1:])
         polyLines.pop(0)
         continue
      # ~~> iterative process
      iline = 1
      ei = polyLines[iline]
      while b != ei[0]:
         iline += 1
         ei = polyLines[iline]
      # ~~> merging the two segments
      e.extend(ei[1:])
      polyLines[0] = e
      polyLines.pop(iline)

   return polyGones

def smoothSubdivise(poly,type,weight):

   ptwice = np.zeros([2*len(poly)-1+type,2])
   # ~~> save original set
   for i in range(len(poly)): ptwice[2*i] = poly[i]
   # ~~> include intermediates
   for i in range(len(poly)-1): ptwice[2*i+1]  = ( poly[i]+poly[i+1] )/2.
   if type!=0: ptwice[2*len(poly)-1]  = ( poly[0]+poly[len(poly)-1] )/2.
   # ~~> weighted-average of the original
   for i in range(len(poly)-1)[1:]: ptwice[2*i] = weight*ptwice[2*i] + (1-weight)*( ptwice[2*i-1]+ptwice[2*i+1] )/2.
   if type!=0:
      ptwice[0] = weight*ptwice[0] + (1-weight)*( ptwice[len(ptwice)-1]+ptwice[1] )/2.
      ptwice[len(ptwice)-2] = weight*ptwice[len(ptwice)-2] + (1-weight)*( ptwice[len(ptwice)-1]+ptwice[len(ptwice)-3] )/2.

   return ptwice,type

def removeDuplicates(poly,type):
   found = True
   while found:
      i = 0; found = False
      while i < len(poly)-1:
         if isClose( poly[i],poly[i+1],size=10 ):
            found = True
            poly = np.delete(poly,i+1,0)
         i += 1
   if len(poly) == 1: return [],0
   elif len(poly) == 2: return poly,0
   else:
      if type != 0:
         if isClose( poly[len(poly)-1],poly[0],size=10 ): poly = np.delete(poly,len(poly)-1,0)
      if len(poly) < 3: return poly,0
      return poly,type

def subsampleDistance(poly,type,dist):

   found = True
   while found:
      i = 0; found = False
      while i < len(poly)-1:
         if dist > getNorm2( poly[i],poly[i+1] ):
            poly[i] = ( poly[i]+poly[i+1] )/2.
            poly = np.delete(poly,i+1,0)
            found = True
         i += 1
   if len(poly) == 1: return [],0
   elif len(poly) == 2: return poly,0
   else:
      if type!=0:
         if dist > getNorm2( poly[len(poly)-1],poly[0] ):
            poly[len(poly)-1] = ( poly[len(poly)-1]+poly[0] )/2.
            poly = np.delete(poly,0,0)
      if len(poly) < 3: return poly,0
      return poly,type

def subsampleAngle(poly,type,angle):

   found = True
   while found:
      i = 0; found = False
      while i < len(poly)-2:
         if angle > 180*abs( abs(getConeAngle( poly[i],poly[i+1],poly[i+2] )) - np.pi )/np.pi:
            poly = np.delete(poly,i+1,0)
            found = True
         i += 2
   if len(poly) < 3: return poly,0
   return poly,type

def isClockwise(poly):
   wise = 0
   for i in range(len(poly)):
      wise += ( poly[(i+1)%len(poly)][0]-poly[i][0] ) \
         *( poly[(i+2)%len(poly)][1]-poly[(i+1)%len(poly)][1] ) \
         - ( poly[(i+1)%len(poly)][1]-poly[i][1] ) \
         * ( poly[(i+2)%len(poly)][0]-poly[(i+1)%len(poly)][0] )
   return wise < 0


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$15-Nov-2011 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
