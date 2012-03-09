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
"""@history 01/02/2012 -- Sebastien E. Bourban:
         Massively speeding up the read and write of SELAFIN files.
         Also, following discussion with Laure C. Grignon (HRW), addition of
         the getSERIES() method to the SELAFIN class, which will eventually
         replace or at least accelerate getValueHistorySLF
"""
"""@history 20/02/2012 -- Sebastien E. Bourban:
         A new class has been added to handle the information contained in
         CONLIM files: class CONLIM.
         This class has two methods: __init__ and putContent
         + __init__: reads the content of a standard CONLIM file
         + putContent writes both the content to a standard CONLIM file
           or a parallel CONLIM file.
"""
"""@history 29/02/2012 -- Sebastien E. Bourban:
         A new class has been added to replace the time-consuming part of
         PARTEL: class splitSELAFIN.
         This class has a number of methods but in particular:
         __init__ and putContent
         + __init__: reads the content of a standard CONLIM file, a SELAFIN file
           and if there, a SEQUENCE file from METIS (PARTEL_PRELIM)
         + putContent writes first all SELAFINs and then go on preparing the
           information necessary for the CONLIMs split before printing.
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from struct import unpack,pack
import sys
import numpy as np
# ~~> dependencies towards other modules
#np.set_printoptions(precision=16)
# ~~> dependencies towards other pytel/modules
from utils.geometry import isClose,isInsideTriangle,getBarycentricWeights,getSegmentIntersection
from utils.progressbar import ProgressBar
from utils.files import getFileContent,putFileContent

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def getHeaderParametersSLF(f):

   # ~~ Read title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l,TITLE,chk = unpack('>i80si',f.read(4+80+4))
   if l!=chk:
      print '... Cannot read the TITLE of your SELAFIN file'
      sys.exit()

   # ~~ Read NBV(1) and NBV(2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l,NBV1,NBV2,chk = unpack('>iiii',f.read(4+8+4))
   # ~~ Read variable names and units ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   VARNAMES = []; VARUNITS = []
   for i in range(NBV1):
     l,vn,vu,chk = unpack('>i16s16si',f.read(4+16+16+4))
     VARNAMES.append(vn)
     VARUNITS.append(vu)
   CLDNAMES = []; CLDUNITS = []
   for i in range(NBV2):
     l,vn,vu,chk = unpack('>i16s16si',f.read(4+16+16+4))
     CLDNAMES.append(vn)
     CLDUNITS.append(vu)

   # ~~ Read IPARAM array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   d = unpack('>12i',f.read(4+40+4))
   IPARAM = np.asarray( d[1:11] )

   # ~~ Read DATE/TIME array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   DATETIME = [1972,07,13,17,15,13]
   if IPARAM[9] == 1:
      d = unpack('>8i',f.read(4+24+4))
      DATETIME = np.asarray( d[1:9] )

   # ~~ Read NELEM3, NPOIN3, NDP, NPLAN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l,NELEM3,NPOIN3,NDP,NPLAN,chk = unpack('>6i',f.read(4+16+4))

   return TITLE, NBV1,VARNAMES,VARUNITS, NBV2,CLDNAMES,CLDUNITS, IPARAM, DATETIME, NELEM3,NPOIN3,NDP,NPLAN

def getHeaderMeshSLF(f,NELEM3,NPOIN3,NDP,NPLAN):

   # ~~ Read the IKLE array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.seek(4,1)
   IKLE = np.array( unpack('>'+str(NELEM3*NDP)+'i',f.read(4*NELEM3*NDP)) ).reshape((NELEM3,NDP))
   f.seek(4,1)

   # ~~ Read the IPOBO array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.seek(4,1)
   IPOBO = np.asarray( unpack('>'+str(NPOIN3)+'i',f.read(4*NPOIN3)) )
   f.seek(4,1)

   # ~~ Read the x-coordinates of the nodes ~~~~~~~~~~~~~~~~~~
   f.seek(4,1)
   MESHX = np.asarray( unpack('>'+str(NPOIN3/NPLAN)+'f',f.read(4*NPOIN3/NPLAN)) )
   f.seek(4,1)

   # ~~ Read the y-coordinates of the nodes ~~~~~~~~~~~~~~~~~~
   f.seek(4,1)
   MESHY = np.asarray( unpack('>'+str(NPOIN3/NPLAN)+'f',f.read(4*NPOIN3/NPLAN)) )
   f.seek(4,1)

   return IKLE-1,IPOBO,MESHX,MESHY

def getTimeHistorySLF(f,NVAR,NPOIN3):

   ATs = []; ATt = []
   while True:
      try:
         ATt.append(f.tell())
         # ~~ Read AT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         f.seek(4,1)
         ATs.append(unpack('>f',f.read(4))[0])
         f.seek(4,1)
         # ~~ Skip Values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         f.seek(NVAR*(4+4*NPOIN3+4),1)
         
      except:
         ATt.pop(len(ATt)-1)   # since the last record failed the try
         break

   return ATt, np.asarray(ATs)

def getVariablesAt( f,tags,frame,NVAR,NPOIN3,varsIndexes ):

   z = np.zeros((len(varsIndexes),NPOIN3))
   if frame < len(tags['cores']) and frame >= 0:
      f.seek(tags['cores'][frame])
      f.seek(4+4+4,1)
      for ivar in range(NVAR):
         f.seek(4,1)
         if ivar in varsIndexes:
            z[varsIndexes.index(ivar)] = unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
         else:
            f.seek(4*NPOIN3,1)
         f.seek(4,1)

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
      print "... Could not find ",v[ivar]," in ",ALLVARS
      print "   +> may be you forgot to switch name spaces into underscores in your command ?"
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
      f.seek(4+4+4,1)
      for ivar in range(NVAR):
         f.seek(4,1)
         if ivar in varsIndexes:
            VARSOR = unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
            for xy in range(len(bn)):
               z[varsIndexes.index(ivar)][xy][t] = bn[xy][0]*VARSOR[ln[xy][0]] + bn[xy][1]*VARSOR[ln[xy][1]] + bn[xy][2]*VARSOR[ln[xy][2]]
         else:
            f.seek(4*NPOIN3,1)
         f.seek(4,1)

   return ('Time (s)',tags['times']),[(TITLE,varsName,le,z)]

def getEdgesSLF(IKLE):

   edges = []
   ibar = 0; pbar = ProgressBar(maxval=len(IKLE)).start()
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
         if not isClose(pj,p[len(p)-1]):
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
         if not isClose(pj,p[len(p)-1]):
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
            if not isClose(pi,p[0]):
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
         if not isClose(pi,ipt[len(ipt)-1]):
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

def putHeaderSLF(slf): #TODO: optimise for speed, use of struct.Struct
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
   f.write(pack('>'+str(slf.NELEM3*slf.NDP)+'i',*(n+1 for e in slf.IKLE for n in e)))
   f.write(pack('>i',4*slf.NELEM3*slf.NDP))

   # ~~ Write the IPOBO array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i',4*slf.NPOIN3))
   f.write(pack('>'+str(slf.NPOIN3)+'i',*(slf.IPOBO)))
   f.write(pack('>i',4*slf.NPOIN3))

   # ~~ Write the x-coordinates of the nodes ~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i',4*slf.NPOIN3))
   f.write(pack('>'+str(slf.NPOIN3)+'f',*(slf.MESHX)))
   f.write(pack('>i',4*slf.NPOIN3))

   # ~~ Write the y-coordinates of the nodes ~~~~~~~~~~~~~~~~~~~~~~~
   f.write(pack('>i',4*slf.NPOIN3))
   f.write(pack('>'+str(slf.NPOIN3)+'f',*(slf.MESHY)))
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
   for v in VARSOR:
      f.write(pack('>i',4*slf.NPOIN3))
      f.write(pack('>'+str(slf.NPOIN3)+'f',*(v)))
      f.write(pack('>i',4*slf.NPOIN3))

   return

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#

class CONLIM:

   def __init__(self,fileName):
      self.fileName = fileName
      # ~~> Columns of integers and floats
      DTYPE = np.dtype=[('lih','<i4'),('liu','<i4'),('liv','<i4'), \
                          ('h','<f4'),('u','<f4'),('v','<f4'),('au','<f4'), \
         ('lit','<i4'),   ('t','<f4'),('at','<f4'),('bt','<f4'), \
         ('n','<i4'),('c','<i4') ]

      if fileName != '':
         # ~~> Number of boundary points ( tuple() necessary for dtype parsing )
         #core = [ tuple((' '.join(c.strip().split()[0:13])+' 0 0.0 0.0 0').split()) for c in getFileContent(fileName) ]
         core = [ tuple(c.strip().split()[0:13]) for c in getFileContent(fileName) ]
         self.NPTFR = len(core)
         self.BOR = np.array( core, DTYPE )
         # ~~> Dictionary of KFRGL
         self.KFRGL = dict(zip( self.BOR['n']-1,range(self.NPTFR) ))
         
         # ~~> Filtering indices
         self.INDEX = np.array( range(self.NPTFR),dtype=np.int )

   def setNUMLIQ(self,closedContours):

      # ~~> Part linkages
      self.NPTIR = {}
      self.IFAPAR = {}

      # ~~> Extra columns
      DTYPE = np.dtype=[('is','<i4'),('xs','<f4'),('ys','<f4'),('lq','<i4') ]
      core = [ tuple(' 0 0.0 0.0 0'.split()) for c in range(self.NPTFR) ]
      self.POR = np.array( core, DTYPE )

      # ~~> Initiates NUMLIQ
      solids = self.BOR['lih'] == 2
      self.NFRLIQ = 0 # liquid boundary numbers start at 1

      # ~~> Finds TELEMAC's south-east corner


      # ~~> Counts NUMLIQ
      for contour in closedContours: # for the domain boundary and eavery islands
         # ~~> look for a solid point
         inode = 0
         while not solids[self.KFRGL[contour[inode]]] and inode < len(contour): inode += 1
         if inode == len(contour): # No solid boundary found in contour
            self.NFRLIQ += 1
            for i in contour: self.POR['lq'][self.KFRGL[i]] = self.NFRLIQ
            continue # go to next contour
         # ~~> list all liquid boundaries on contour
         wassolid = True
         for i in contour: # this makes sure you go around contour only once
            if not solids[self.KFRGL[contour[inode%len(contour)]]]:
               if wassolid:
                  self.NFRLIQ += 1
                  wassolid = False
               self.POR['lq'][self.KFRGL[contour[inode%len(contour)]]] = self.NFRLIQ
            else: wassolid = True
            inode += 1

      return

   def putContent(self,fileName):

      # standard part of the CONLIM
      core = []
      for ifr in range(self.NPTFR):
         if self.INDEX[ifr] != 0:
            c = ( ' '.join(['{0[' + str(i) + ']}' for i in range(len(self.BOR[ifr]))]) ).format(self.BOR[ifr])
            if self.NPTIR != {} and self.IFAPAR != {}:
               c += ( ' '+' '.join(['{0[' + str(i) + ']}' for i in range(len(self.POR[ifr]))]) ).format(self.POR[ifr])
            core.append( c )

      if self.NPTIR != {} and self.IFAPAR != {}:
      # parrallel part 1 of the CONLIM -- format with i7 /!\
         ntr = self.NPTIR.keys()
         ntr.sort()
         core.append(str(len(ntr)))
         for itr in ntr:
            c = self.NPTIR[itr]
            c.sort()
            c.extend([-1,-1,-1,-1,-1,-1,-1,-1,-1,-1])
            s = repr(itr+1).rjust(7) + ' ' + \
               repr(c[0]).rjust(7) + ' ' + repr(c[1]).rjust(7) + ' ' + repr(c[2]).rjust(7) + ' ' + \
               repr(c[3]).rjust(7) + ' ' + repr(c[4]).rjust(7) + ' ' + repr(c[5]).rjust(7) + ' ' + \
               repr(c[6]).rjust(7) + ' ' + repr(c[7]).rjust(7) + ' ' + repr(c[8]).rjust(7)
            core.append( s )
      # parrallel part 2 of the CONLIM -- format with i9 /!\
         ntr = self.IFAPAR.keys()
         ntr.sort()
         core.append(str(len(ntr)))
         for itr in ntr:
            c = self.IFAPAR[itr]
            s = repr(itr+1).rjust(9) + ' ' + \
               repr(c[1]).rjust(9) + ' ' + repr(c[3]).rjust(9) + ' ' + repr(c[5]).rjust(9) + ' ' + \
               repr(c[0]+1).rjust(9) + ' ' + repr(c[2]+1).rjust(9) + ' ' + repr(c[4]+1).rjust(9)
            core.append( s )

      putFileContent(fileName,core)

class SELAFIN:

   DATETIME = [1972,07,13,17,24,27]  # ... needed here ecause optional in SLF
   
   def __init__(self,fileName):
      self.fileName = fileName
      if fileName != '':
         self.file = open(fileName,'rb')
         self.tags,self.TITLE,self.DATETIME,self.IPARAM,numbers,vars,mesh = parseSLF(self.file)
      else:
         self.tags = []; self.TITLE = ''; self.IPARAM = []
         numbers = ( 0,0,0,0 ); vars = ( 0,[],[],0,[],[] )
         mesh = ( [],[],[],[] )
      self.NELEM3,self.NPOIN3,self.NDP,self.NPLAN = numbers
      self.NBV1,self.VARNAMES,self.VARUNITS,self.NBV2,self.CLDNAMES,self.CLDUNITS = vars
      self.NVAR = self.NBV1 + self.NBV2
      self.VARINDEX = range(self.NVAR)
      self.IKLE,self.IPOBO,self.MESHX,self.MESHY = mesh

   def getVALUES(self,t):
      return getVariablesAt( self.file,self.tags,t,self.NVAR,self.NPOIN3,self.VARINDEX )

   def putContent(self,fileName):
      self.fole = open(fileName,'wb')
      ibar = 0; pbar = ProgressBar(maxval=len(self.tags['times'])).start()
      putHeaderSLF(self)
      for t in range(len(self.tags['times'])):
         ibar += 1
         appendCoreTimeSLF(self,t)
         appendCoreVarsSLF(self,self.getVALUES(t))
         pbar.update(ibar)
      self.fole.close()
      pbar.finish()

   def getSERIES( self,nodes ):
      f = self.file
      varsIndexes = self.VARINDEX

      # ~~ Ordering the nodes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      onodes = np.sort(np.array( zip(range(len(nodes)),nodes), dtype=[ ('0',int),('1',int) ] ),order='1')

      # ~~ Extract time profiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      z = np.zeros((len(varsIndexes),len(nodes),len(self.tags['cores'])))
      f.seek(self.tags['cores'][0])
      for t in range(len(self.tags['cores'])):
         f.seek(self.tags['cores'][t])
         f.seek(4+4+4,1)
         for ivar in range(self.NVAR):

            f.seek(4,1)
            if ivar in varsIndexes:
               jnod = onodes[0]
               f.seek(4*(jnod[1]-1),1)
               z[varsIndexes.index(ivar),jnod[0],t] = unpack('>f',f.read(4))[0]
               for inod in onodes[1:]:
                  f.seek(4*(inod[1]-jnod[1]-1),1)
                  z[varsIndexes.index(ivar),inod[0],t] = unpack('>f',f.read(4))[0]
                  jnod = inod
               f.seek(4*self.NPOIN3-4*jnod[1],1)
            else:
               f.seek(4*self.NPOIN3,1)
            f.seek(4,1)

      return z

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
         accuracy = np.power(10.0, -5+np.floor(np.log10(max)))  #/!\ max always positive
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
            ibar = 0; pbar = ProgressBar(maxval=len(self.slf.tags['times'])).start()
            putHeaderSLF(self.slf)
            for t in range(len(self.slf.tags['times'])):
               ibar += 1
               appendCoreTimeSLF(self.slf,t)
               appendCoreVarsSLF(self.slf,self.slf.getVALUES(t)-self.slfs[1].getVALUES(t))
               pbar.update(ibar)
            pbar.finish()
         else: SELAFIN.putContent(self.slf,fileName) # just a copy
      elif self.suite:
         self.slf.fole = open(fileName,'wb')
         ibar = 0; pbar = ProgressBar(maxval=len(self.slf.tags['times'])).start()
         putHeaderSLF(self.slf)
         for t in range(len(self.slf.tags['times'])):
            ibar += 1
            time = self.slf.tags['times'][t]
            appendCoreTimeSLF(self.slf,t)
            appendCoreVarsSLF(self.slf,self.slf.getVALUES(t))
            pbar.update(ibar)
         pbar.finish()
         for slf in self.slfs:
            slf.fole = self.slf.fole
            ibar = 0; pbar = ProgressBar(maxval=len(slf.tags['times'])).start()
            for t in range(len(slf.tags['times'])):
               if slf.tags['times'][t] > time:
                  ibar += 1
                  time = slf.tags['times'][t]
                  appendCoreTimeSLF(slf,t)
                  appendCoreVarsSLF(slf,slf.getVALUES(t))
                  pbar.update(ibar)
            pbar.finish()
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
         ibar = 0; pbar = ProgressBar(maxval=len(self.slf.tags['times'])).start()
         putHeaderSLF(self.slf)
         for t in range(len(self.slf.tags['times'])):
            ibar += 1
            appendCoreTimeSLF(self.slf,t)
            appendCoreVarsSLF(self.slf,self.slf.getVALUES(t))
            for slf in self.slfs[1:]:
               appendCoreVarsSLF(self.slf,slf.getVALUES(t))
            pbar.update(ibar)
         pbar.finish()
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
