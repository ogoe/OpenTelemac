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
from os import path
from argparse import ArgumentParser,RawDescriptionHelpFormatter
import numpy as np
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
# ~~> dependencies towards other pytel/modules
from utils.geometry import getConeAngle,isClose,getNorm2
from utils.progressbar import SubProgressBar
from parsers.parserKenue import InS

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

def smoothSubdivise(poly,vals,type,weight):

   ptwice = np.zeros((2*len(poly)-1+type,2))
   #vtwice = np.zeros((2*len(poly)-1+type,2,len(lavs)))
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

   return ptwice,vals,type

def removeDuplicates(poly,type): # /!\ does not work anymore
   found = True
   while found:
      i = 0; found = False
      while i < len(poly)-1:
         if isClose( poly[i],poly[i+1],size=10 ):
            found = True
            poly = np.delete(poly,i+1,0)
         i += 1
   if len(poly) == 1: return [],0
   elif len(poly) == 2: return [],0 #poly,0
   else:
      if type != 0:
         if isClose( poly[len(poly)-1],poly[0],size=10 ): poly = np.delete(poly,len(poly)-1,0)
      if len(poly) < 3: return [],0 #poly,0
      return poly,type

def removeDuplilines(poly,vals,type): # /!\ does not work anymore
   p = []; t = []; v = []; stencil = 1000
   sbar = SubProgressBar(maxval=len(poly)).start()
   found = False
   for i in range(len(poly)):
      for j in range(len(poly))[i+2:i+2+stencil]:
         if isClose( poly[i],poly[j] ):
            ia = (i+1)%len(poly)
            ib = (j-1)%len(poly)
            if isClose( poly[ia],poly[ib] ):
               poly1,ptmp1,poly2,ptmp2,poly3 = np.split(poly,[i,ia,ib,j])
               p.append(np.concatenate((poly1, poly3), axis=0))
               if type == 1: t.append(1)
               p.append(poly2)
               t.append(1)
               found = True
         if found: break
      if found: break
      sbar.update(i)
   sbar.finish()
   if p == []:
      p.append(poly); t.append(type)
   return p,v,t

def removeDuplangles(poly,vals,type): # /!\ does not work anymore

   found = True
   while found:
      i = 0; found = False
      while i < len(poly)-3:
         if 1 > 180*abs( getConeAngle( poly[i],poly[i+1],poly[i+2] ) )/np.pi:
            poly = np.delete(poly,i+1,0)
            found = True
         if 1 > 180*abs( getConeAngle( poly[i+1],poly[i+2],poly[i+3] ) )/np.pi:
            poly = np.delete(poly,i+2,0)
            found = True
         i += 2
   if len(poly) < 3: return [],0 #poly,0
   return poly,type

def subsampleDistance(poly,vals,type,dist):

   found = True
   while found:
      i = 0; found = False
      while i < len(poly)-1:
         if dist > getNorm2( poly[i],poly[i+1] ):
            poly[i] = ( poly[i]+poly[i+1] )/2.
            poly = np.delete(poly,i+1,0)
            vals[i] = ( vals[i]+vals[i+1] )/2.
            vals = np.delete(vals,i+1,0)
            found = True
         i += 1
   if len(poly) == 1: return [],[],0
   elif len(poly) == 2: return [],[],0 #poly,0
   else:
      if type!=0:
         if dist > getNorm2( poly[len(poly)-1],poly[0] ):
            poly[len(poly)-1] = ( poly[len(poly)-1]+poly[0] )/2.
            poly = np.delete(poly,0,0)
            vals[len(vals)-1] = ( vals[len(vals)-1]+vals[0] )/2.
            vals = np.delete(vals,0,0)
      if len(poly) < 3: return [],[],0 #poly,0
      return poly,vals,type

def subsampleAngle(poly,vals,type,angle):

   found = True
   while found:
      i = 0; found = False
      while i < len(poly)-4:
         if angle > 180*abs( abs(getConeAngle( poly[i],poly[i+1],poly[i+2] )) - np.pi )/np.pi:
            poly = np.delete(poly,i+1,0)
            vals = vals.pop(i+1)
            found = True
         if angle > 180*abs( abs(getConeAngle( poly[i+1],poly[i+2],poly[i+3] )) - np.pi )/np.pi:
            poly = np.delete(poly,i+2,0)
            vals = vals.pop(i+2)
            found = True
         i += 2
   if len(poly) < 3: return [],[],0 #poly,vals,0
   return poly,vals,type

def isClockwise(poly):
   # assumes that poly does not duplicate points
   wise = 0
   for i in range(len(poly)):
      z = ( poly[(i+1)%len(poly)][0]-poly[i][0] ) \
         *( poly[(i+2)%len(poly)][1]-poly[(i+1)%len(poly)][1] ) \
         - ( poly[(i+1)%len(poly)][1]-poly[i][1] ) \
         * ( poly[(i+2)%len(poly)][0]-poly[(i+1)%len(poly)][0] )
      if z > 0: wise += 1
      elif z < 0: wise -= 1
   return wise < 0

def makeClockwise(poly,vals):
   # TODO -- you need to also flip the values
   if not isClockwise(poly): return np.flipud(poly),vals[::-1]
   return poly,vals

def makeAntiClockwise(poly,vals):
   # TODO -- you need to also flip the values
   if isClockwise(poly): return np.flipud(poly),vals[::-1]
   return poly,vals

def getArea(poly):
   # assumes that poly does not duplicate points
   x,y = poly.T
   area = np.sum( x[:-1]*y[1:] - x[1:]*y[:-1] )
   return abs( area + ( x[-1]*y[0] - x[0]*y[-1] ) )/2.0

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#

"""
   Polygons is a lightweight structure to hold polygon information,
      whether to support i2s/i3s or shape or other file types.
   Polygons repalces the previous object InS
   Note that the derived class of Polygons have to implement the methods
      parseContent and putContent
"""
class Polygons:

   def __init__(self):
      self.object = None
      self.coordinates = { 'type':None }   # important for coordinate conversions and distance calculations

      # self.poly = []      # list of several polygons, each being defined as a pairs of x,y
      # self.vals = []      # list of several polygons, each being defined as the values for the corresponding nodes
      # self.type = []      # an tuples of integers for each polygon, where for the
                          #    first value: 0 = open; 1 = closed clockwise; 2 = closed anti-clockwise; 3 = ...
                          #    second value: 0 = soft line; 1 = hard line; 2 = ...
      # self.atrbut = []    # a list of attributs for each polygon, common to all nodes on the polygon
      self.npoly = 0
      self.npoin = 0      # /!\ the poly does not duplicate the first and last node for closed contours

   def parseContent(self,fileName):
      # file parsing is based on the name of the extension
      _,tail = path.splitext(fileName)
      # ~~> Case of a Kenue type i2s/i3s file
      if tail in ['.i2s','.i3s']: self.object = InS(fileName)
      self.npoly = self.object.npoly
      # ~~> Case of a Arc-GIS type shap file (which comes with additional related files)
      #elif tail == '.shp':
      #   head,fileType,self.npoin,self.poly,self.vals,self.type,self.atrbut = getShp(self.fileName)
      # ~~> Sort out the poly types
      for ip in range(self.object.npoly):
         if self.object.type[ip] > 0:
            if isClockwise(self.object.poly[ip]): self.object.type[ip] = 1
            else: self.object.type[ip] = 2
      return self.object.head

   def putContent(self,fileName,head=[]):
      # ~~> all object should have a putContent method
      self.object.putContent(fileName,head)

   def getAreas(self,select=[]):
      if select == []: select = range(self.npoly)
      # TODO: take sperical coordinate into account
      areas = np.zeros(len(select), dtype=np.float)
      for ip,i in zip(select,range(len(select))):
         # ~~> compute the area only for closed polygons
         if self.object.type[ip] in [1,2]: areas[i] = getArea(self.object.poly[ip])
      return areas

   def getLengths(self,select=[]):
      # TODO
      return np.zeros(len(select),dtype=float)

   def sortByAreas(self):
      s = np.sort( np.array( zip(np.argsort(self.getAreas())[::-1],np.arange(self.npoly)), dtype=[('s',int),('i',int)] ), order='s')
      for i in range(len(s)):
         if s['s'][i] > s['i'][i]:
            self.object.poly.insert(s['i'][i], self.object.poly.pop(s['s'][i]))
            self.object.type.insert(s['i'][i], self.object.type.pop(s['s'][i]))
            self.object.vals.insert(s['i'][i], self.object.vals.pop(s['s'][i]))

   def makeAntiClockwise(self,select=[]):
      if select == []: select = range(self.npoly)
      for ip in select:
         # ~~> make anti-clockwise only closed clockwise polygons
         if self.object.type[ip] in [1,2]:
            self.object.poly[ip],self.object.vals[ip] = makeAntiClockwise(self.object.poly[ip],self.object.vals[ip])
            self.object.type[ip] = 2

   def makeClockwise(self,select=[]):
      if select == []: select = range(self.npoly)
      for ip in select:
         # ~~> make clockwise only closed anti-clockwise polygons
         if self.object.type[ip] in [1,2]:
            self.object.poly[ip],self.object.vals[ip] = makeClockwise(self.object.poly[ip],self.object.vals[ip])
            self.object.type[ip] = 1

   def smoothSubdivise(self,select=[],weigth=0.5):
      if select == []: select = range(self.npoly)
      for ip in select:
         # ~~> make clockwise only closed anti-clockwise polygons
         self.object.poly[ip],self.object.vals[ip],_ = smoothSubdivise(self.object.poly[ip],self.object.vals[ip],self.object.type[ip],weigth)

   def subsampleDistance(self,select=[],distance=1000.0):
      if select == []: select = range(self.npoly)
      for ip in select:
         # ~~> make clockwise only closed anti-clockwise polygons
         self.object.poly[ip],self.object.vals[ip],_ = subsampleDistance(self.object.poly[ip],self.object.vals[ip],self.object.type[ip],distance)

   def subsampleAngle(self,select=[],angle=15.0):
      if select == []: select = range(self.npoly)
      for ip in select:
         # ~~> make clockwise only closed anti-clockwise polygons
         self.object.poly[ip],self.object.vals[ip],_ = subsampleAngle(self.object.poly[ip],self.object.vals[ip],self.object.type[ip],angle)

   def sph2ll(self,(long0,lat0)):
      radius  = 6371000.
      long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
      const = np.tan( lat0/2. + np.pi/4. )
      for poly in self.object.poly:
         for ip in range(len(poly)):
            poly[ip][0] = np.rad2deg( poly[ip][0]/radius + long0 )
            poly[ip][1] = np.rad2deg( 2.*np.arctan( const*np.exp(poly[ip][1]/radius) ) - np.pi/2. )

   def ll2sph(self,(long0,lat0)):
      radius  = 6371000.
      long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
      const = np.tan( lat0/2. + np.pi/4. )
      for poly in self.object.poly:
         for ip in range(len(poly)):
            poly[ip][0] = radius * ( np.deg2rad(poly[ip][0]) - long0 )
            poly[ip][1] = radius * ( np.log( np.tan( np.deg2rad(poly[ip][1])/2. + np.pi/4. ) ) - np.log( const ) )

   def getBBox(self):
      if len(self.poly) == 0: return 0.,0.,0.,0.
      xmin = xmax = self.object.poly[0][0][0]
      ymin = ymax = self.object.poly[0][0][1]
      for poly in self.object.poly:
         xp,yp = poly.T
         xmin = min( xmin,min(xp) )
         xmax = max( xmax,max(xp) )
         ymin = min( ymin,min(yp) )
         ymax = max( ymax,max(yp) )
      return xmin,ymin,xmax,ymax

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$15-Nov-2011 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Command line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n'+'~'*72+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Testing ...
      '''))
   parser.add_argument( "args",nargs='*' )
   parser.add_argument("--sph2ll",dest="sph2ll",default=None,help="convert from spherical to longitude-latitude" )
   options = parser.parse_args()
   if len(options.args) < 1:
      print '\nAt least a code name (and its associated inputs) are required\n'
      parser.print_help()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   insFile = options.args[0]

   p = Polygons()
   head = p.parseContent(insFile)

   p.subsampleDistance(distance=250.)
   p.smoothSubdivise(weigth=0.7)
   p.subsampleAngle(angle=12.0)

   p.putContent(options.args[1],head)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)

"""
Early work by S.E.Bourban ... will be replaced by more recent work from M.S.Turnbull

def cutAngleJoinSplit(poly,angle,dist,stencil):

   d2 = np.power(dist,2)     # ~~> d square will save some calculation time
   a2 = angle * np.pi / 180.
   remo = []                 # ~~> left over
   found = True
   while found:
      found = False
      iline = 0
      for s in range(stencil)[:int(2*stencil/3):-2]:   # ~~> here you start wit hthe larger stencil first
         iline = iline%len(poly)
         while iline < len(poly):
            #ibar = min((iline-stencil)%len(line)*len(line)/l0Line+stencil*l0Line,(maxStencil-4)*l0Line)
            # ~~> takes points on either sides (stencil)
            a,b,c = poly[(iline-s)%len(poly)],poly[iline%len(poly)],poly[(iline+s)%len(poly)]
            # ~~> calculates the "vision" angle -- (+):inlet; (-):headland assuming anti-clockwise
            cosac = getConeAngle( a,b,c )
            if abs(cosac) < a2 and cosac > 0:
               if getDistancePointToLine( c,b,a ) < dist or getDistancePointToLine( a,b,c ) < dist:
                  remo.append(poly[(iline-s)%len(poly):(iline+s)%len(poly)+1])
                  iline += stencil
               #   should use split / join
               #   poly = np.delete(poly,iline%len(line))
               #   print 'I can delete the following',a,b,c
               #   sys.exit(1)
               #   found += 1
               #   continue
            iline += 1
            #pbar.update(ibar)
         #pbar.write('Areas of interst found: '+str(found),ibar)
      #if found == 0: break
      #pbar.finish()

   return poly,remo




   def removeDuplicates(self):
      ibar = 0; pbar = ProgressBar(maxval=self.npoin).start()
      ip = 0
      while ip < len(self.poly):
         ibar += len(self.poly[ip])
         lb = len(self.poly[ip])
         self.poly[ip],self.type[ip] = pl_removeDuplicates(self.poly[ip],self.type[ip])
         la = len(self.poly[ip])
         if la < lb: pbar.write('    +> removed '+str(lb-la)+' points of '+str(lb)+' from polygon '+str(ip+1),ibar)
         if self.poly[ip] == []:
            pbar.write('    +> removed entire polygon '+str(ip+1),ibar)
            self.poly.pop(ip)
            self.type.pop(ip)
         else: ip += 1
         pbar.update(ibar)
      pbar.finish()
      return self.poly,self.type

   def removeDuplilines(self):
      ibar = 0; pbar = ProgressBar(maxval=self.npoin).start()
      ip = 0
      while ip < len(self.poly):
         ibar += len(self.poly[ip])
         p,t = pl_removeDuplilines(self.poly[ip],self.type[ip])
         pbar.trace() # /!\ the call requires sub-progress bar
         if len(p) > 1:
            pbar.maxval -= len(self.poly[ip])
            ibar -= len(self.poly[ip])
            self.poly.pop(ip)
            self.type.pop(ip)
            for po,to in zip(p,t):
               pbar.maxval += len(po)
               self.poly.append(po)
               self.type.append(to)
         else:
            ip += 1
         pbar.update(ibar)
      pbar.finish()
      return self.poly,self.type

   def removeDuplangles(self):
      ibar = 0; pbar = ProgressBar(maxval=self.npoin).start()
      ip = 0
      while ip < len(self.poly):
         ibar += len(self.poly[ip])
         lb = len(self.poly[ip])
         self.poly[ip],self.type[ip] = pl_removeDuplangles(self.poly[ip],self.type[ip])
         la = len(self.poly[ip])
         if la < lb: pbar.write('    +> removed '+str(lb-la)+' points of '+str(lb)+' from polygon '+str(ip+1),ibar)
         if self.poly[ip] == []:
            pbar.write('    +> removed entire polygon '+str(ip+1),ibar)
            self.poly.pop(ip)
            self.type.pop(ip)
         else: ip += 1
         pbar.update(ibar)

      pbar.finish()
      return self.poly,self.type


"""
