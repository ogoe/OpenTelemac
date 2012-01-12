"""@author Christopher J. Cawthorn and Sebastien E. Bourban
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
"""@brief
         Tools for handling SELAFIN files and TELEMAC binary related in python
"""
"""@details
         Contains read/write functions for binary (big-endian) SELAFIN files
"""
"""@history 07/12/2011 -- Sebastien E. Bourban:
         Addition of 2 new methods (getVariablesAt and getNeighboursSLF)
         and modifications to others to:
         + replace NBV1+NBV2 by NVAR and varsNumbers by varsIndexes
         + and correct a bug in subsetVariablesSLF
"""
"""@history 07/01/2012 -- Sebastien E. Bourban:
         Implementation of the SELAFIN class and of the putSLF function.
         A number of methods are available to the SELAFIN object to modify
            or transform its content, which are then called upon in putSLF.
"""
"""@history 07/01/2012 -- Sebastien E. Bourban:
         Distinguishing between VARIABLES and CLANDESTINES.
         Also, now learned how mix multiple classes' methods.
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from struct import unpack,pack
import sys
from os import path
import numpy as np
# ~~> dependencies towards other modules
from config import OptionParser
#np.set_printoptions(precision=16)
# ~~> dependencies towards other pytel/modules
from utils.geometry import isInsideTriangle,getBarycentricWeights,getSegmentIntersection
from utils.progressbar import ProgressBar

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def getHeaderParametersSLF(f):

   # ~~ Read title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   TITLE = unpack('>80s',f.read(80))[0]
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read TITLE'
      sys.exit()

   # ~~ Read NBV(1) and NBV(2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   NBV1,NBV2 = unpack('>ii',f.read(8))
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read NBVs'
      sys.exit()

   # ~~ Read variable names and units ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   VARNAMES = []; VARUNITS = []
   for i in range(NBV1):
     l = unpack('>i',f.read(4))[0]
     VARNAMES.append(unpack('>16s',f.read(16))[0])
     VARUNITS.append(unpack('>16s',f.read(16))[0])
     chk = unpack('>i',f.read(4))[0]
     if l!=chk:
        print '... Cannot read VARNAMES/VARUNITS['+str(i)+']'
        sys.exit()
   CLDNAMES = []; CLDUNITS = []
   for i in range(NBV2):
     l = unpack('>i',f.read(4))[0]
     CLDNAMES.append(unpack('>16s',f.read(16))[0])
     CLDUNITS.append(unpack('>16s',f.read(16))[0])
     chk = unpack('>i',f.read(4))[0]
     if l!=chk:
        print '... Cannot read CLDNAMES/CLDUNITS['+str(i)+']'
        sys.exit()

   # ~~ Read IPARAM array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   IPARAM = np.asarray( unpack('>10i',f.read(40)) )
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read IPARAM'
      sys.exit()

   # ~~ Read DATE/TIME array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   DATETIME = [1972,07,13,17,15,13]
   if IPARAM[9] == 1:
      l = unpack('>i',f.read(4))[0]
      DATETIME = np.asarray( unpack('>6i',f.read(4*6)) )
      chk = unpack('>i',f.read(4))[0]
      if l!=chk:
         print '... Cannot read DATE and TIME'
         sys.exit()

   # ~~ Read NELEM3, NPOIN3, NDP, NPLAN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   NELEM3,NPOIN3,NDP,NPLAN = unpack('>4i',f.read(16))
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read NELEM3 etc.'
      sys.exit()

   return TITLE, NBV1,VARNAMES,VARUNITS, NBV2,CLDNAMES,CLDUNITS, IPARAM, DATETIME, NELEM3,NPOIN3,NDP,NPLAN

def getHeaderMeshSLF(f,NELEM3,NPOIN3,NDP,NPLAN):

   # ~~ Read the IKLE array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   #IKLE = unpack('>'+str(NELEM3*NDP)+'i',f.read(4*NELEM3*NDP))
   IKLE = []
   for i in range(NELEM3):
      IKLE.append(unpack('>'+str(NDP)+'i',f.read(4*NDP)))
   #   for j in range(NDP):
   #      IKLE[i].append(unpack('>i',f.read(4))[0])
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read IKLE3'
      sys.exit()

   # ~~ Read the IPOBO array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   IPOBO = unpack('>'+str(NPOIN3)+'i',f.read(4*NPOIN3))
   #IPOBO = []
   #for i in range(NPOIN3):
   #   IPOBO.append(unpack('>i',f.read(4))[0])
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read IPOBO'
      sys.exit()

   # ~~ Read the x-coordinates of the nodes ~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   MESHX = unpack('>'+str(NPOIN3/NPLAN)+'f',f.read(4*NPOIN3/NPLAN))
   #MESHX = []
   #for i in range(NPOIN3):
   #   MESHX.append(unpack('>f',f.read(4))[0])
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read MESHX'

   # ~~ Read the y-coordinates of the nodes ~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   MESHY = unpack('>'+str(NPOIN3/NPLAN)+'f',f.read(4*NPOIN3/NPLAN))
   #MESHY = []
   #for i in range(NPOIN3):
   #   MESHY.append(unpack('>f',f.read(4))[0])
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read MESHY'

   return np.asarray(IKLE)-1,np.asarray(IPOBO),np.asarray(MESHX),np.asarray(MESHY)

def getTimeHistorySLF(f,NVAR,NPOIN3):

   ATs = []; ATt = []
   while True:
      try:
         ATt.append(f.tell())
         # ~~ Read AT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         l = unpack('>i',f.read(4))[0]
         ATs.append(unpack('>f',f.read(4))[0])
         chk = unpack('>i',f.read(4))[0]
         if l!=chk: print 'Error reading AT'
         # ~~ Skip Values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         f.read( NVAR*(4+4*NPOIN3+4) )
         
      except:
         ATt.pop(len(ATt)-1)   # since the last record failed the try
         break

   return ATt, np.asarray(ATs)

def getVariablesAt( f,tags,frame,NVAR,NPOIN3,varsIndexes ):

   z = np.zeros((len(varsIndexes),NPOIN3))
   if frame < len(tags['cores']) and frame >= 0:
      f.seek(tags['cores'][frame])
      f.read(4+4+4)
      for ivar in range(NVAR):
         f.read(4)
         if ivar in varsIndexes:
            z[varsIndexes.index(ivar)] = unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
         else:
            f.read(4*NPOIN3)
         f.read(4)

   return z

def parseSLF(f):
   
   tags = { }; f.seek(0)
   
   tags.update({ 'meta': f.tell() })
   TITLE,NBV1,VARNAMES,VARUNITS,NBV2,CLDNAMES,CLDUNITS,IPARAM,DATETIME,NELEM3,NPOIN3,NDP,NPLAN = getHeaderParametersSLF(f)

   tags.update({ 'mesh': f.tell() })
   IKLE,IPOBO,MESHX,MESHY = getHeaderMeshSLF(f,NELEM3,NPOIN3,NDP,NPLAN)

   ATtags,ATs = getTimeHistorySLF(f,NBV1+NBV2,NPOIN3)
   tags.update({ 'cores': ATtags })
   tags.update({ 'times': ATs })

   return tags, TITLE,DATETIME,IPARAM,(NELEM3,NPOIN3,NDP,NPLAN),(NBV1,VARNAMES,VARUNITS,NBV2,CLDNAMES,CLDUNITS),(IKLE,IPOBO,MESHX,MESHY)

def subsetVariablesSLF(vars,ALLVARS):
   ids = []; names = []
   
   v = vars.split(';')
   for ivar in range(len(v)):
      vi = v[ivar].split(':')[0]
      for jvar in range(len(ALLVARS)):
         if vi.lower() in ALLVARS[jvar].strip().lower():
            ids.append(jvar)
            names.append(ALLVARS[jvar].strip())
   if not len(ids) == len(vars.split(';')):
      print "... Could not find ",vars," in ",ALLVARS
      sys.exit()

   return ids,names

def getValueHistorySLF( f,tags,time,(le,ln,bn),TITLE,NVAR,NPOIN3,(varsIndexes,varsName) ):

   # ~~ Subset time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   subset = time  # /!\ History requires 2 values
   if time[0] < 0: subset = [ 0,max( 0, len(tags['cores']) + time[0] ) ]
   if time[1] < 0: subset = [ time[0],max( 0, len(tags['cores']) + time[1] ) ]

   # ~~ Extract time profiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   z = np.zeros((len(varsIndexes),len(bn),len(tags['cores'])))
   for t in range(len(tags['cores'])):
      if t < subset[0] or t > subset[1]: continue
      f.seek(tags['cores'][t])
      f.read(4+4+4)
      for ivar in range(NVAR):
         f.read(4)
         if ivar in varsIndexes:
            VARSOR = unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
            for xy in range(len(bn)):
               z[varsIndexes.index(ivar)][xy][t] = bn[xy][0]*VARSOR[ln[xy][0]] + bn[xy][1]*VARSOR[ln[xy][1]] + bn[xy][2]*VARSOR[ln[xy][2]]
         else:
            f.read(4*NPOIN3)
         f.read(4)

   return ('Time (s)',tags['times']),[(TITLE,varsName,le,z)]

def getEdgesSLF(IKLE):

   edges = []
   ibar = 0; pbar = ProgressBar(maxval=(len(IKLE))).start()
   for e in IKLE:
      pbar.update(ibar); ibar += 1
      if (e[0],e[1]) not in edges: edges.append((e[1],e[0]))
      if (e[1],e[2]) not in edges: edges.append((e[2],e[1]))
      if (e[2],e[0]) not in edges: edges.append((e[0],e[2]))
   pbar.finish()

   return edges

def getNeighboursSLF(IKLE):

   neighbours = {}; ne = []; insiders = {}
   print '    +> start listing neighbours of edges'
   ibar = 0; pbar = ProgressBar(maxval=(3*len(IKLE))).start()
   for e,i in zip(IKLE,range(len(IKLE))):
      nk = neighbours.keys(); ne.append({})
      for k in [0,1,2]:
         pbar.update(ibar); ibar += 1
         ne[i].update({ (e[k],e[(k+1)%3]):-1, (e[(k+1)%3],e[k]):-1 })
         if (e[k],e[(k+1)%3]) not in nk: neighbours.update({ (e[(k+1)%3],e[k]):i })
         else:
            j = neighbours[(e[k],e[(k+1)%3])]
            ne[i][(e[k],e[(k+1)%3])] = j
            ne[i][(e[(k+1)%3],e[k])] = j
            ne[j][(e[k],e[(k+1)%3])] = i
            ne[j][(e[(k+1)%3],e[k])] = i
            insiders.update({(e[k],e[(k+1)%3]):[i,j]})
            del neighbours[(e[k],e[(k+1)%3])]
   pbar.write('    +> listing neighbours of edges completed',ibar)
   pbar.finish()

   return ne,neighbours,insiders

"""
   An accuracy has been introduced because Python does not seem to be accurate
      with sums and multiplications
"""
def consecutive( p1,p2 ):

   if ( p2 == [] or p1 == [] ): return False
   s = max(p1[0]+p2[0],p1[1]+p2[1])
   accuracy = np.power(10.0, -5+np.floor(np.log10(s)))

   return np.sqrt( np.power(p1[0]-p2[0],2) + np.power(p1[1]-p2[1],2)) < accuracy

"""
   This function return the element number for the triangle including xyo=(xo,yo)
      or -1 if the (xo,yo) is outside the mesh
   It should be noted that (xo,yo) are arrays so only one search is necessary for
      multiple pairs
   Return: locate, and array of integers of size len(xyo)
"""
def xyLocateMeshSLF(xyo,NELEM,IKLE,MESHX,MESHY):
   
   locate = - np.ones((len(xyo)), dtype=np.int)
   locatn = - np.ones((len(xyo),3), dtype=np.int)
   bryctr = np.zeros((len(xyo),3))
   
   for e in range(NELEM):
      p = [ IKLE[e][0], IKLE[e][1], IKLE[e][2] ]
      for io in range(len(xyo)):
         b0 = isInsideTriangle( xyo[io],(MESHX[p[0]],MESHY[p[0]]),(MESHX[p[1]],MESHY[p[1]]),(MESHX[p[2]],MESHY[p[2]]) )
         if b0 != []:
            locate[io] = e; locatn[io] = p; bryctr[io] = b0

   return locate,locatn,bryctr

def dichoLocateMeshSLF(rank,(e1,e2),(p1,p2),NELEM,IKLE,MESHX,MESHY):
   
   # ~~ Split in two sub-segments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   x0 = 0.5 * ( p1[0]+p2[0] ); y0 = 0.5 * ( p1[1]+p2[1] )
   p0 = [ x0,y0 ]
   e12 = [e1,e2]; p = []; e = []
   
   # ~~ Position the middle point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # ~~> just in case e1 is your match
   f0 = False
   if e1 >= 0:
      br = isInsideTriangle( np.asarray(p0), (MESHX[IKLE[e1][0]],MESHY[IKLE[e1][0]]),(MESHX[IKLE[e1][1]],MESHY[IKLE[e1][1]]),(MESHX[IKLE[e1][2]],MESHY[IKLE[e1][2]]) )
      if br != []:
         e0 = e1
         f0 = True
   # ~~> just in case e2 is your match
   if not f0 and e2 >= 0:
      br = isInsideTriangle( np.asarray(p0), (MESHX[IKLE[e2][0]],MESHY[IKLE[e2][0]]),(MESHX[IKLE[e2][1]],MESHY[IKLE[e2][1]]),(MESHX[IKLE[e2][2]],MESHY[IKLE[e2][2]]) )
      if br != []:
         e0 = e2
         f0 = True
   # ~~> eventually search through the whole grid
   if not f0:
      le,ln,br = xyLocateMeshSLF(np.asarray([p0]),NELEM,IKLE,MESHX,MESHY)
      e0 = le[0]
      f0 = True

   # ~~ Limit the number of useless dichotomies ~~~~~~~~~~~~~~~~~~~~

   # ~~> should you not have found a mesh position
   if not f0:
      rank = rank + 1
      if rank > 3: return p,e
   # ~~> should the split be ridicoulously small
   else:
      sizem = 0.3 * min( max(MESHX[IKLE[e0]])-min(MESHX[IKLE[e0]]), max(MESHY[IKLE[e0]])-min(MESHY[IKLE[e0]]) )
      siz10 = max( max(p1[0],p0[0])-min(p1[0],p0[0]), max(p1[1],p0[1])-min(p1[1],p0[1]) )
      siz02 = max( max(p0[0],p2[0])-min(p0[0],p2[0]), max(p0[1],p2[1])-min(p0[1],p2[1]) )
      if sizem > min(siz10,siz02): rank = rank + 1
      if rank > 3: return p,e

   e10 = []; e02 = []
   p10 = []; p02 = []

   # ~~ First sub-segment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # ~~> if e1 == e0, you are about to go out ...
   if e1 == e0 and e1 >= 0:
      pi = []
      for i in [0,1,2]:
         pi.extend( getSegmentIntersection( ( MESHX[IKLE[e0][i%3]],MESHY[IKLE[e0][i%3]] ),
            ( MESHX[IKLE[e0][(i+1)%3]],MESHY[IKLE[e0][(i+1)%3]] ),
            ( p1[0],p1[1] ), ( p2[0],p2[1] ) ) )
      if pi == []: return p,e
      elif len(pi) == 1:
         p.extend(pi)
         e.append(e0)
      else:
         p00 = [ 0.5*( pi[0][0]+pi[1][0] ), 0.5*( pi[0][1]+pi[1][1] ) ]
         for i in [0,1,2]:
            p.extend( getSegmentIntersection( ( MESHX[IKLE[e0][i%3]],MESHY[IKLE[e0][i%3]] ),
               ( MESHX[IKLE[e0][(i+1)%3]],MESHY[IKLE[e0][(i+1)%3]] ),
               ( p1[0],p1[1] ), ( p00[0],p00[1] ) ) )
         e.append(e0)
         for i in [0,1,2]:
            p.extend( getSegmentIntersection( ( MESHX[IKLE[e0][i%3]],MESHY[IKLE[e0][i%3]] ),
               ( MESHX[IKLE[e0][(i+1)%3]],MESHY[IKLE[e0][(i+1)%3]] ),
               ( p00[0],p00[1] ), ( p2[0],p2[1] ) ) )
         e.append(e0)
   # ~~> further split required
   elif e1 >= 0 or e0 >= 0:
      p10,e10 = dichoLocateMeshSLF(rank,(e1,e0),(p1,p0),NELEM,IKLE,MESHX,MESHY)

   for pj in p10: #p.append(pi)
      if p != []:
         if not consecutive(pj,p[len(p)-1]):
            p.append(pj)
            e.append(e10.pop(0))
         else: e10.pop(0)
      else:
         p.append(pj)
         e.append(e10.pop(0))
   if len(e10) > 0: print e10

   # ~~ Second sub-segment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # ~~> if e1 == e0, you are about to go out ...
   if e2 == e0 and e2 >= 0:
      pi = []
      for i in [0,1,2]:
         pi.extend( getSegmentIntersection( ( MESHX[IKLE[e0][i%3]],MESHY[IKLE[e0][i%3]] ),
            ( MESHX[IKLE[e0][(i+1)%3]],MESHY[IKLE[e0][(i+1)%3]] ),
            ( p1[0],p1[1] ), ( p2[0],p2[1] ) ) )
      if pi == []: return p,e
      elif len(pi) == 1:
         p.extend(pi)
         e.append(e0)
      else:
         p00 = [ 0.5*( pi[0][0]+pi[1][0] ), 0.5*( pi[0][1]+pi[1][1] ) ]
         for i in [0,1,2]:
            p.extend( getSegmentIntersection( ( MESHX[IKLE[e0][i%3]],MESHY[IKLE[e0][i%3]] ),
               ( MESHX[IKLE[e0][(i+1)%3]],MESHY[IKLE[e0][(i+1)%3]] ),
               ( p1[0],p1[1] ), ( p00[0],p00[1] ) ) )
         e.append(e0)
         for i in [0,1,2]:
            p.extend( getSegmentIntersection( ( MESHX[IKLE[e0][i%3]],MESHY[IKLE[e0][i%3]] ),
               ( MESHX[IKLE[e0][(i+1)%3]],MESHY[IKLE[e0][(i+1)%3]] ),
               ( p00[0],p00[1] ), ( p2[0],p2[1] ) ) )
         e.append(e0)
   # ~~> further split required
   elif e2 >= 0 or e0 >= 0:
      p02,e02 = dichoLocateMeshSLF(rank,(e0,e2),(p0,p2),NELEM,IKLE,MESHX,MESHY)

   for pj in p02: #p.append(pi)
      if p != []:
         if not consecutive(pj,p[len(p)-1]):
            p.append(pj)
            e.append(e02.pop(0))
         else: e02.pop(0)
      else:
         p.append(pj)
         e.append(e02.pop(0))
   if len(e02) > 0: print e02

   return p,e

def crossLocateMeshSLF(polyline,NELEM,IKLE,MESHX,MESHY):

   # ~~ Nodes of the polyline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   polyle,polyln,polybr = xyLocateMeshSLF(polyline,NELEM,IKLE,MESHX,MESHY)

   # ~~ Intersection nodes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ipt = []; iet = []; ibr = []
   for li in range(len(polyline)-1):

      # ~~> Intersected nodes on the polyline ~~~~~~~~~~~~~~~~~~~~~~
      p,e = dichoLocateMeshSLF(0,(-1,-1),(polyline[li],polyline[li+1]),NELEM,IKLE,MESHX,MESHY)

      # ~~> First node on the sub-polyline ~~~~~~~~~~~~~~~~~~~~~~~~~
      ei = polyle[li]
      if ei >= 0:
         pi = polyline[li]
         if p != []:
            if not consecutive(pi,p[0]):
               ipt.append(pi); iet.append([IKLE[ei][0],IKLE[ei][1],IKLE[ei][2]])
               ibr.append( getBarycentricWeights(pi,(MESHX[IKLE[ei][0]],MESHY[IKLE[ei][0]]),(MESHX[IKLE[ei][1]],MESHY[IKLE[ei][1]]),(MESHX[IKLE[ei][2]],MESHY[IKLE[ei][2]])) )
         else:
            ipt.append(pi); iet.append([IKLE[ei][0],IKLE[ei][1],IKLE[ei][2]])
            ibr.append( getBarycentricWeights(pi,(MESHX[IKLE[ei][0]],MESHY[IKLE[ei][0]]),(MESHX[IKLE[ei][1]],MESHY[IKLE[ei][1]]),(MESHX[IKLE[ei][2]],MESHY[IKLE[ei][2]])) )

      # ~~> You may duplicate the nodes at either end ~~~~~~~~~~~~~~
      for pi,ei in zip(p,e):
         ipt.append(pi); iet.append([IKLE[ei][0],IKLE[ei][1],IKLE[ei][2]])
         ibr.append( getBarycentricWeights(pi,(MESHX[IKLE[ei][0]],MESHY[IKLE[ei][0]]),(MESHX[IKLE[ei][1]],MESHY[IKLE[ei][1]]),(MESHX[IKLE[ei][2]],MESHY[IKLE[ei][2]])) )

   # ~~> Last node on the polyline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ei = polyle[len(polyle)-1]
   if ei >= 0:
      pi = polyline[len(polyle)-1]
      if ipt != []:
         if not consecutive(pi,ipt[len(ipt)-1]):
            ipt.append(pi); iet.append([IKLE[ei][0],IKLE[ei][1],IKLE[ei][2]])
            ibr.append( getBarycentricWeights(pi,(MESHX[IKLE[ei][0]],MESHY[IKLE[ei][0]]),(MESHX[IKLE[ei][1]],MESHY[IKLE[ei][1]]),(MESHX[IKLE[ei][2]],MESHY[IKLE[ei][2]])) )

   return ipt,iet,ibr

def getValuePolylineSLF(f,tags,time,(p,ln,bn),TITLE,NVAR,NPOIN3,(varsIndexes,varsName)):
   # TODO: you may have one or two values, the later defining an animation

   # ~~ Subset time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   subset = [ time[0] ]
   if time[0] < 0: subset = [ max( 0, len(tags['cores']) + time[0] ) ]

   # ~~ Extract time profiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   z = np.zeros((len(varsIndexes),len(p))) #,len(tags['cores'])))
   # ~~ Extract distances along ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   d = 0.0
   x = [ d ]
   for xy in range(len(p)-1):
      d = d + np.sqrt( np.power(p[xy+1][0]-p[xy][0],2) + np.power(p[xy+1][1]-p[xy][1],2) )
      x.append(d)
   # ~~ Extract data along line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #for t in range(len(tags['cores'])): ... see the TODO above
   t = int(subset[0])
   f.seek(tags['cores'][t])
   f.read(4+4+4)
   for ivar in range(NVAR):
      f.read(4)
      if ivar in varsIndexes:
         VARSOR = unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
         for xy in range(len(p)):
            z[varsIndexes.index(ivar)][xy] = bn[xy][0]*VARSOR[ln[xy][0]] + bn[xy][1]*VARSOR[ln[xy][1]] + bn[xy][2]*VARSOR[ln[xy][2]]
      else:
         f.read(4*NPOIN3)
      f.read(4)

   return ('Distance (m)',x),[(TITLE,varsName,z)]

def putSLF(slf):

   putHeaderSLF(slf)
   # ~~ Write time varying variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for t in range(len(slf.tags['times'])):
      appendCoreTimeSLF(slf,t)
      VARSOR = slf.getVALUES(t)
      appendCoreVarsSLF(slf,VARSOR)

   return

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/^\~~~~~~~~
#                                                 Header \_/
#

def putHeaderSLF(slf):
   f = slf.fole

   # ~~ Write title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i80si',80,slf.TITLE,80))

  # ~~ Write NBV(1) and NBV(2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>iiii',4+4,slf.NBV1,slf.NBV2,4+4))

   # ~~ Write variable names and units ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for i in range(slf.NBV1):
      f.write(pack('>i',32))
      f.write(pack('>16s',slf.VARNAMES[i]))
      f.write(pack('>16s',slf.VARUNITS[i]))
      f.write(pack('>i',32))
   for i in range(slf.NBV2):
      f.write(pack('>i',32))
      f.write(pack('>16s',slf.CLDNAMES[i]))
      f.write(pack('>16s',slf.CLDUNITS[i]))
      f.write(pack('>i',32))

   # ~~ Write IPARAM array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i',4*10))
   for i in range(len(slf.IPARAM)): f.write(pack('>i',slf.IPARAM[i]))
   f.write(pack('>i',4*10))

   # ~~ Write DATE/TIME array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if slf.IPARAM[9] == 1:
      f.write(pack('>i',4*6))
      for i in range(6): f.write(pack('>i',slf.DATETIME[i]))
      f.write(pack('>i',4*6))

   # ~~ Write NELEM3, NPOIN3, NDP, NPLAN ~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>6i',4*4,slf.NELEM3,slf.NPOIN3,slf.NDP,slf.NPLAN,4*4))

   # ~~ Write the IKLE array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i',4*slf.NELEM3*slf.NDP))
   for i in range(slf.NELEM3):
      for j in range(slf.NDP): f.write(pack('>i',slf.IKLE[i][j]+1))
   f.write(pack('>i',4*slf.NELEM3*slf.NDP))

   # ~~ Write the IPOBO array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i',4*slf.NPOIN3))
   for i in range(slf.NPOIN3): f.write(pack('>i',slf.IPOBO[i]))
   f.write(pack('>i',4*slf.NPOIN3))

   # ~~ Write the x-coordinates of the nodes ~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i',4*slf.NPOIN3))
   for i in range(slf.NPOIN3): f.write(pack('>f',slf.MESHX[i]))
   f.write(pack('>i',4*slf.NPOIN3))

   # ~~ Write the y-coordinates of the nodes ~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i',4*slf.NPOIN3))
   for i in range(slf.NPOIN3): f.write(pack('>f',slf.MESHY[i]))
   f.write(pack('>i',4*slf.NPOIN3))

   return

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/^\~~~~~~~~
#                                                   Core \_/
#

def appendCoreTimeSLF(slf,t):
   f = slf.fole

   # Print time record
   f.write(pack('>ifi',4,slf.tags['times'][t],4))

   return

def appendCoreVarsSLF(slf,VARSOR):
   f = slf.fole

   # Print variable records
   for v in range(len(VARSOR)):
      f.write(pack('>i',4*slf.NPOIN3))
      for j in range(slf.NPOIN3): f.write(pack('>f',VARSOR[v][j]))
      f.write(pack('>i',4*slf.NPOIN3))

   return

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#
class SELAFIN:

   DATETIME = [1972,07,13,17,24,27]  # ... needed here ecause optional in SLF
   
   def __init__(self,fileName):
      self.fileName = fileName
      self.file = open(fileName,'rb')
      self.tags,self.TITLE,self.DATETIME,self.IPARAM,numbers,vars,mesh = parseSLF(self.file)
      self.NELEM3,self.NPOIN3,self.NDP,self.NPLAN = numbers
      self.NBV1,self.VARNAMES,self.VARUNITS,self.NBV2,self.CLDNAMES,self.CLDUNITS = vars
      self.NVAR = self.NBV1 + self.NBV2
      self.VARINDEX = range(self.NVAR)
      self.IKLE,self.IPOBO,self.MESHX,self.MESHY = mesh

   def getVALUES(self,t):
      return getVariablesAt( self.file,self.tags,t,self.NVAR,self.NPOIN3,self.VARINDEX )

   def putContent(self,fileName):
      self.fole = open(fileName,'wb')
      putHeaderSLF(self)
      for t in range(len(self.tags['times'])):
         appendCoreTimeSLF(self,t)
         appendCoreVarsSLF(self,self.getVALUES(t))
      self.fole.close()

   def __del__(self): self.file.close()

class SELAFINS:

   def __init__(self):
      self.slfs = []
      self.slf = None

   def add(self,fileName):
      slf = SELAFIN(fileName)
      if self.slf == None: self.slf = slf
      self.slfs.append(slf)
      self.suite = self.isSuite()
      self.merge = self.isMerge()

   def isSuite(self):
      same = True
      for slf in self.slfs[1:]:
         for v in slf.VARNAMES: same = same and ( v in self.slf.VARNAMES )
         for v in slf.CLDNAMES: same = same and ( v in self.slf.CLDNAMES )
      return same

   def isMerge(self):
      same = True
      for slf in self.slfs[1:]:
         max = 1.e-5 + np.max( slf.tags['times'] ) + np.max( self.slf.tags['times'] )
         accuracy = np.power(10.0, -5+np.floor(np.log10(max)))
         same = same and ( accuracy > \
            np.max( slf.tags['times'] - self.slf.tags['times'] ) - np.min( slf.tags['times'] - self.slf.tags['times'] ) )
      return same

   def pop(self,index=0):
      index = max( 0,min(index,len(self.slfs)-1) )
      return slfs.pop(self.slfs[index])

   def putContent(self,fileName): # TODO: files also have to have the same header
      if self.suite and self.merge:
         if len(self.slfs) == 2:  # /!\ difference only between two files
            self.slf.fole = open(fileName,'wb')
            putHeaderSLF(self.slf)
            for t in range(len(self.slf.tags['times'])):
               appendCoreTimeSLF(self.slf,t)
               appendCoreVarsSLF(self.slf,self.slf.getVALUES(t)-self.slfs[1].getVALUES(t))
         else: SELAFIN.putContent(self.slf,fileName) # just a copy
      elif self.suite:
         self.slf.fole = open(fileName,'wb')
         putHeaderSLF(self.slf)
         for t in range(len(self.slf.tags['times'])):
            time = self.slf.tags['times'][t]
            appendCoreTimeSLF(self.slf,t)
            appendCoreVarsSLF(self.slf,self.slf.getVALUES(t))
         for slf in self.slfs:
            slf.fole = self.slf.fole
            for t in range(len(slf.tags['times'])):
               if slf.tags['times'][t] > time:
                  time = slf.tags['times'][t]
                  appendCoreTimeSLF(slf,t)
                  appendCoreVarsSLF(slf,slf.getVALUES(t))
         self.slf.fole.close()
      elif self.merge:
         self.slf.fole = open(fileName,'wb')
         for slf in self.slfs[1:]:
            slf.fole = self.slf.fole
            idvars = []
            for v in range(len(slf.VARNAMES)):
               if v not in self.slf.VARNAMES:
                  idvars.append(v)
                  self.slf.VARNAMES.append(slf.VARNAMES[v])
                  self.slf.VARUNITS.append(slf.VARUNITS[v])
            for v in range(len(slf.CLDNAMES)):
               if v not in self.slf.CLDNAMES:
                  idvars.append(v+slf.NBV1)
                  self.slf.CLDNAMES.append(slf.CLDNAMES[v])
                  self.slf.CLDUNITS.append(slf.CLDUNITS[v])
            slf.VARINDEX = idvars
            self.slf.NBV1 = len(self.slf.VARNAMES)
            self.slf.NBV2 = len(self.slf.CLDNAMES)
         putHeaderSLF(self.slf)
         for t in range(len(self.slf.tags['times'])):
            appendCoreTimeSLF(self.slf,t)
            appendCoreVarsSLF(self.slf,self.slf.getVALUES(t))
            for slf in self.slfs[1:]:
               appendCoreVarsSLF(self.slf,slf.getVALUES(t))
         self.slf.fole.close()

      else:
         print "Does not know how to merge your files. Try either:"
         print "    + to make sure your files have the same time support"
         print "    + to make sure your files have the same variables"
         sys.exit()

   def __del__(self):
      if self.slf != None: SELAFIN.__del__(self.slf)
      if self.slfs != []:
         for slf in self.slfs: SELAFIN.__del__(slf)


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Christopher J. Cawthor and Sebastien E. Bourban"
__date__ ="$09-Sep-2011 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
