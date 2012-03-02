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
from os import path
import numpy as np
# ~~> dependencies towards other modules
#np.set_printoptions(precision=16)
# ~~> dependencies towards other pytel/modules
from utils.geometry import isClose,isInsideTriangle,getBarycentricWeights,getSegmentIntersection
from utils.progressbar import ProgressBar
from utils.files import getFileContent,putFileContent
#from parsers.parserKenue import putInS

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

class splitSELAFIN():

   def __init__(self,SLFfileName,CLMfileName,SEQfileName='',splitCONLIM=False):

      print '\n... Acquiring global files'
      # ~~> Acquire global CONLIM file
      print '    +> CONLIM file'
      self.clm = CONLIM(CLMfileName)
      self.isCONLIM = splitCONLIM

      # ~~> Acquire global SELAFIN file
      print '    +> SELAFIN file'
      self.slf = SELAFIN(SLFfileName)

      # ~~> Acquire global SELAFIN file
      if SEQfileName != '':
         print '    +> SEQUENCE file'
         self.NSPLIT = self.getSplitFromSequence(np.array( getFileContent(SEQfileName), dtype='<i4' ))
         """
         self.slf.fole = open(SEQfileName+'.slf','wb')
         putHeaderSLF(self.slf)
         appendCoreTimeSLF(self.slf,0)
         VARSOR = self.slf.getVALUES(0)
         for v in range(self.slf.NVAR): VARSOR[v] = self.NSPLIT
         appendCoreVarsSLF(self.slf,VARSOR)
         self.slf.fole.close()
         """
      else:
         self.NSPLIT = self.getSplitFromNodeValues('PROCESSORS')

      self.NPARTS,self.PNIKLE,self.PINTER,self.PNHALO,self.PNODDS,self.IPOBO = \
         self.elementSplit( self.clm.KFRGL,self.NSPLIT )
      self.slfn = self.copyCommonData()


   #   Make a copy of common information for sub-meshes
   def copyCommonData(self):

      SLFn = SELAFIN('')
      #   Meta data
      SLFn.TITLE = self.slf.TITLE
      SLFn.file = self.slf.file
      SLFn.IPARAM = self.slf.IPARAM
      #   Time
      SLFn.DATETIME = self.slf.DATETIME
      SLFn.tags = self.slf.tags
      #   Variables
      SLFn.NBV1 = self.slf.NBV1
      SLFn.VARNAMES = self.slf.VARNAMES
      SLFn.VARUNITS = self.slf.VARUNITS
      SLFn.NBV2 = self.slf.NBV2
      SLFn.CLDNAMES = self.slf.CLDNAMES
      SLFn.CLDUNITS = self.slf.CLDUNITS
      SLFn.NVAR = self.slf.NVAR
      SLFn.VARINDEX = range(self.slf.NVAR)
      #   Unchanged numbers
      SLFn.NPLAN = self.slf.NPLAN
      SLFn.NDP = self.slf.NDP

      return SLFn

   #   Split based on a sequence of parts, one for each element (result from METIS)
   def getSplitFromSequence(self,KSPLIT):

      NSPLIT = np.zeros( self.slf.NPOIN3 ,dtype=np.int )
      NPARTS = len(np.unique(KSPLIT))
      for part in range(NPARTS):
         k = np.compress(KSPLIT==(NPARTS-part),range(len(self.slf.IKLE)))
         NSPLIT[self.slf.IKLE[k]] = KSPLIT[k]
      return NSPLIT-1

   #   Split based on the variable PROCESSORS, defined at the nodes
   def getSplitFromNodeValues(self,var):

      # ~~> Filter for 'PROCESSORS' as input to the getVariablesAt method
      i,vn = subsetVariablesSLF(var,self.slf.VARNAMES)
      if i == []:
         print '... Could not find ',var,', you may need another split method'
         sys.exit()
      # ~~> NSPLIT is the interger value of the variable PROCESSORS (time frame 0)
      NSPLIT = np.array( \
         getVariablesAt( self.slf.file,self.slf.tags,0,self.slf.NVAR,self.slf.NPOIN3,i )[0], \
         dtype=np.int)

      return NSPLIT

   #   Split based on the variable PROCESSORS, defined at the nodes
   def elementSplit(self,KFRGL,NSPLIT):

      # ~~> NPARTS is the number of parts /!\ does not check continuity vs. missing parts
      NPARTS = len(np.unique(NSPLIT))

      # ~~> Join up the global boundary nodes with the halo elements
      IPOBO = np.zeros(self.slf.NPOIN3,dtype=np.int)
      IPOBO[KFRGL.keys()] = np.array(KFRGL.values(),dtype=np.int)+1  # this is so the nonzero search is easier

      # ~~> PNIKLE hold the PROCESSORS number at the element level (min of 3 nodes)
      PNIKLE = np.zeros(len(self.slf.IKLE),dtype=np.int)
      PNHALO = dict([ (i,[]) for i in range(NPARTS) ])
      PNODDS = dict([ (i,[]) for i in range(NPARTS) ])
      PINTER = dict([ (i,[]) for i in range(NPARTS) ])
      print '\n... Splitting by elements in ',NPARTS,' parts\n'
      pbar = ProgressBar(maxval=len(self.slf.IKLE)).start()
      for k in range(len(self.slf.IKLE)):
         e = self.slf.IKLE[k]
         PNIKLE[k] = min( NSPLIT[e] )
         # Case 1: you are at an internal boundary element
         if PNIKLE[k] != max( NSPLIT[e] ):
            for p1,p2 in zip([0,1,2],[1,2,0]):
               if NSPLIT[e[p1]] != PNIKLE[k] and NSPLIT[e[p2]] != PNIKLE[k]:
                  PINTER[PNIKLE[k]].append([e[p1],e[p2]])
                  if NSPLIT[e[p1]] == NSPLIT[e[p2]]:
                     PINTER[NSPLIT[e[p1]]].append([e[p2],e[p1]])
         # Case 2: you may be at an external boundary element
         if np.count_nonzero( IPOBO[e] ) > 1:
            for p1,p2 in zip([0,1,2],[1,2,0]):
               if IPOBO[e[p1]] != 0 and IPOBO[e[p2]] != 0: # multiplier is not possible
                  if IPOBO[e[p1]] + 1 == IPOBO[e[p2]]: PNHALO[PNIKLE[k]].append([e[p1],e[p2]])
                  else: PNODDS[PNIKLE[k]].append([e[p1],e[p2]])
         pbar.update(k)
      pbar.finish()

      return NPARTS,PNIKLE,PINTER,PNHALO,PNODDS,IPOBO

   def getIKLE(self,npart):

      # ~~> get IKLE for that part ... still with global element numbers
      GIKLE = np.compress( self.PNIKLE==npart,self.slf.IKLE,axis=0 )
      KELLG = np.compress( self.PNIKLE==npart,range(len(self.slf.IKLE)),axis=0 )
      # ~~> KNOLG(NPOIN3) gives the global node number such that
      #   for i = 1,NPOIN3: Fwrite(i) = Fread(KNOLG(i)) and is ordered
      ENUM = np.ravel(GIKLE)
      KNOLG,indices = np.unique( ENUM, return_index=True )
      KNOGL = dict(zip( [ ENUM[i] for i in indices ],range(len(KNOLG)) ))
      LIKLE = - np.ones_like(GIKLE,dtype=np.int)
      pbar = ProgressBar(maxval=len(GIKLE)).start()
      for k in range(len(GIKLE)):
         LIKLE[k] = [ KNOGL[GIKLE[k][0]], KNOGL[GIKLE[k][1]], KNOGL[GIKLE[k][2]] ]
         pbar.update(k)
      pbar.finish()

      return LIKLE,KELLG,KNOLG

   def joinSegments(self,polyLines):

      polyGones = []
      maxbar = max(len(polyLines),1)
      pbar = ProgressBar(maxval=maxbar).start()
      while polyLines != []:
         # ~~> starting point
         e = polyLines[0]
         le = len(e)
         a,b = e[0],e[len(e)-1]
         # ~~> case of closed line
         if a == b:
            polyGones.append(e[0:len(e)]) # /!\ here you keep the duplicated point
            polyLines.pop(0)
            continue
         # ~~> iterative process
         for ei,iline in zip(polyLines[1:],range(len(polyLines))[1:]):
            # ~~> merging the two segments
            if b == ei[0]:
               polyLines[0] = e[0:len(e)]     # copy !
               polyLines[0].extend(ei[1:])
               polyLines.pop(iline)
               break
            if a == ei[len(ei)-1]:
               polyLines[0] = ei[0:len(ei)]   # copy !
               polyLines[0].extend(e[1:])
               polyLines.pop(iline)
               break
         # ~~> completed search
         if le == len(polyLines[0]):
            polyGones.append(e[0:len(e)])
            polyLines.pop(0)
         pbar.update(maxbar-len(polyLines))
      pbar.finish()

      return polyGones

   def tetrisOddSegments(self,main,odds):

      polyGones = []
      lo = len(odds)
      while main != []:
         # ~~> starting point
         e = main[0]
         le = len(e)
         a,b = e[0],e[len(e)-1]
         # ~~> case of closed line
         if a == b:
            polyGones.append(e[0:len(e)]) # /!\ here you keep the duplicated point
            main.pop(0)
            continue
         # ~~> iterative process
         for ei,iline in zip(odds,range(len(odds))):
            # ~~> merging the two segments
            if b == ei[0]:
               main[0] = e[0:len(e)]
               main[0].extend(ei[1:])
               odds.pop(iline)
               break
            if a == ei[len(ei)-1]:
               main[0] = ei[0:len(ei)]
               main[0].extend(e[1:])
               odds.pop(iline)
               break
         # ~~> completed search
         if le == len(main[0]):
            polyGones.append(e[0:len(e)])
            main.pop(0)

      # ~~> removing the over-constrained elements
      for p in polyGones:
         if len(p) > 3:
            j = 2
            while j < len(p):
               if p[j-2] == p[j]:
                  p.pop(j-2)
                  p.pop(j-2)
               j += 1

      return polyGones

   #   Filter poly according to IPOBO on that part.
   #   ~> gloseg: is the ensemble of either closed islands or
   #      open external boundary segments
   #   Note: filtering now seems to mean that to have done a lot of work for nothing
   def globalSegments(self,poly):
      gloseg = []
      for p in poly:
         pA = p[0]; pZ = p[len(p)-1]; closed = False
         if pA == pZ and self.IPOBO[pA] != 0: closed = True
         iA = 0; iZ = 0
         ploseg = []
         for i in p:
            if self.IPOBO[i] != 0: # moves the counter along for external points
               iZ += 1
            elif iZ != 0: # you have just found the end of an external segment
               ploseg.append(p[iA:iA+iZ])
               iA += iZ+1
               iZ = 0
            else:
               iA += 1
         if iZ != 0:
            if closed and len(ploseg) > 0:
               i = p[iA:iA+iZ]
               i.extend(ploseg[0][1:]) # remove duplicate
               ploseg[0] = i
            else: ploseg.append(p[iA:iA+iZ])
         gloseg.extend(ploseg)
      return gloseg

   def putContent(self):

      # ~~> Preliminary set up for LIKLE, KNOLG and KEMLG by parts
      LIKLE = dict([ (i,[]) for i in range(self.NPARTS) ])
      KELLG = dict([ (i,[]) for i in range(self.NPARTS) ])
      KNOLG = dict([ (i,[]) for i in range(self.NPARTS) ])

      print '... Split of the mesh connectivity'
      for part in range(self.NPARTS):
         print '    +> re-ordering IKLE for part ',part+1
         LIKLE[part],KELLG[part],KNOLG[part] = self.getIKLE(part)

      print '\n... Split of the boundary connectivity'
      # ~~> Assemble internal and external segments
      polyCLOSED = dict([ (i,[]) for i in range(self.NPARTS) ])
      polyFILTER = dict([ (i,[]) for i in range(self.NPARTS) ])
      polyGLOSED = []
      for part in range(self.NPARTS): # this could be done in parallel

         print '    +> Joining up boundary segments for part: ',part+1
         # ~~> Joining up boundaries for sub-domains
         print '       ~> main internal segments'
         self.PINTER[part] = self.joinSegments(self.PINTER[part])
         print '       ~> main external segments'
         self.PNHALO[part].extend(self.PINTER[part])
         polyHALO = self.joinSegments(self.PNHALO[part])
         print '       ~> odd segments'
         polyODDS = self.joinSegments(self.PNODDS[part])
         print '       ~> stitching with the odd ones'
         polyGones = self.tetrisOddSegments(polyHALO,polyODDS)
         print '       ~> final closure'
         polyCLOSED[part] = self.joinSegments(polyGones)

         # ~~> Building up the entire picture
         polyFILTER[part] = self.globalSegments(polyCLOSED[part])
         polyGLOSED.extend( polyFILTER[part] )

      # ~~> Joining up boundaries for the global domain (Note: seems counter productive but is not)
      polyGLOSED = self.joinSegments(polyGLOSED)

      print '\n... Storing the global liquid boundary numbering (NUMLIQ)'
      # ~~> Implying NUMLIQ and the number NFRLIQ based on the joined-up lines
      self.clm.setNUMLIQ(polyGLOSED)

      """# ~~> Convert node numbers into x,y
      for p in range(self.NPARTS):
         polyXY = []
         for pg in range(len(self.PINTER[p])):
            pxy = []
            for pt in range(len(self.PINTER[p][pg])):
               n = self.PINTER[p][pg][pt]
               pxy.append([ self.slf.MESHX[n],self.slf.MESHY[n] ])
            polyXY.append(pxy)
         # ~~> Write polygons to double check
         file = 'test1-'+str(p).strip()+'.i2s'
         putInS(file,[],'i2s',polyXY)

      # ~~> Convert node numbers into x,y
      polyXY = []
      for pg in range(len(polyGLOSED)):
         pxy = []
         for pt in range(len(polyGLOSED[pg])):
            n = polyGLOSED[pg][pt]
            pxy.append([ self.slf.MESHX[n],self.slf.MESHY[n] ])
         polyXY.append(pxy)
      # ~~> Write polygons to double check
      file = 'test1.i2s'
      putInS(file,[],'i2s',polyXY)"""

      IFAPAR = dict([ (i,{}) for i in range(self.NPARTS) ])

      # ~~> CONLIM file: Preliminary set up of ISEG for all parts
      ISEG = {}
      #   Organising ISEG for easier call: part 1
      for part in range(self.NPARTS):
         for i in polyFILTER[part]:
            if i[0] == i[len(i)-1]: continue                # /!\ you are here adding one !
            if i[0] in ISEG.keys(): ISEG[i[0]].update({ part:i[1]+1 })
            else: ISEG.update({ i[0]:{ part:i[1]+1 } })
            if i[len(i)-1] in ISEG.keys(): ISEG[i[len(i)-1]].update({ part:-i[len(i)-2]-1 })
            else: ISEG.update({ i[len(i)-1]:{ part:-i[len(i)-2]-1 } })
      #   Switching parts of ISEG for final call: part 2
      for i in ISEG.keys():
         if len(ISEG[i]) != 2:
            print '... You have a boundary node surounded with more than two boundary segments: ',i
            sys.exit()
         parts = ISEG[i].keys()
         ISEG[i] = { parts[0]:ISEG[i][parts[1]], parts[1]:ISEG[i][parts[0]] }

      # ~~> CONLIM file: Preliminary set up of NPTIR for all parts
      NPTIR = dict([ (i,{}) for i in range(self.NPARTS) ])
      for part in range(self.NPARTS):
         for p in self.PINTER[part]: NPTIR[part].update( dict([ (i,[]) for i in p ]) )
      parts = range(self.NPARTS)
      while parts != []:
         part = parts[0]
         parts.pop(0)
         for ip in NPTIR[part].keys():
            for ipart in parts:
               if ip in NPTIR[ipart].keys():
                  NPTIR[part][ip].append(ipart)
                  NPTIR[ipart][ip].append(part)

      # ~~> Extension for parallel file names
      fmtn = '00000' + str(self.NPARTS-1)
      fmtn = fmtn[len(fmtn)-5:]

      print '... Split of the SELAFIN file'
      for part in range(self.NPARTS):
         fmti = '00000' + str(part)
         fmti = fmti[len(fmti)-5:]
         print '    +> part ',part+1,' of ',self.NPARTS

         self.slfn.IKLE = LIKLE[part]
         self.slfn.NELEM3 = len(LIKLE[part])
         self.slfn.NPOIN3 = len(KNOLG[part])
         # ~~> IPARAM has two new values: 8:NPTFR and 9:NPTIR
         self.slfn.IPARAM[7] = len(np.unique(np.concatenate(polyFILTER[part])))
         self.slfn.IPARAM[8] = len(NPTIR[part])
         # ~~> IPOBO (or IRAND) converted into KNOLG[part]
         self.slfn.IPOBO = KNOLG[part]+1

         print '       ~> filtering the MESH'
         # ~~> GEO file: MESH coordinates
         self.slfn.MESHX = np.zeros(self.slfn.NPOIN3,dtype=np.float32)
         self.slfn.MESHY = np.zeros(self.slfn.NPOIN3,dtype=np.float32)
         self.slfn.MESHX = self.slf.MESHX[KNOLG[part]]
         self.slfn.MESHY = self.slf.MESHY[KNOLG[part]]

         # ~~> GEO file: File names
         fileRoot,fileExts = path.splitext(self.slf.fileName)
         self.slfn.fileName = fileRoot+fmtn+'-'+fmti+fileExts

         # ~~> GEO file: Printing
         print '       ~> printing: ',self.slfn.fileName
         self.slfn.fole = open(self.slfn.fileName,'wb')
         putHeaderSLF(self.slfn)
         LVARSOR = np.zeros((self.slfn.NVAR,self.slfn.NPOIN3),dtype=np.float32)
         for t in range(len(self.slf.tags['times'])):
            appendCoreTimeSLF(self.slfn,t)
            VARSOR = self.slf.getVALUES(t)
            for v in range(self.slfn.NVAR): LVARSOR[v] = VARSOR[v][KNOLG[part]]
            appendCoreVarsSLF(self.slfn,LVARSOR)
         self.slfn.fole.close()

      if not self.isCONLIM: return
      
      print '\n... Connect elements across internal boundaries (IFAPAR)'
      for part in range(self.NPARTS):
         print '    +> part ',part+1,' of ',self.NPARTS
         # ~~> CONLIM file: Preliminary set up of PEHALO elements accross internal boundaries
         PEHALO = {}; SEHALO = {}
         #   Step 1: find out about the primary elements and loop through IKLE
         self.NSPLIT *= 0
         MASKER = NPTIR[part].keys()
         self.NSPLIT[MASKER] += 1

         print '       ~> Assembling primary elements with other side'
         # Sub Step 1: Assembling all edges from the other sides
         maxbar = 0; ibar = 0
         for ip in range(self.NPARTS): maxbar += len(LIKLE[ip])
         pbar = ProgressBar(maxval=maxbar).start()
         for otherpart in range(self.NPARTS):
            if otherpart == part: continue        # all parts are still positive at this stage
            for k in range(len(LIKLE[otherpart])):
               ibar += 1
               e = self.slf.IKLE[KELLG[otherpart][k]]
               if np.count_nonzero( self.NSPLIT[e] ) < 2: continue
               for p1,p2 in zip([1,2,0],[0,1,2]):    # reverse order because looking from the other side
                  if self.NSPLIT[e[p1]] > 0 and self.NSPLIT[e[p2]] > 0:
                     if not PEHALO.has_key((e[p1],e[p2])): PEHALO.update({ (e[p1],e[p2]):[0,[]] })
                     PEHALO[(e[p1],e[p2])][1].append(k)
                     PEHALO[(e[p1],e[p2])][1].append(otherpart)
               pbar.update(ibar)
         # Sub Step 2: Assembling all edges from the primary side (there are three times more of them)
         for k in range(len(LIKLE[part])):
            ibar += 1
            j = KELLG[part][k]
            e = self.slf.IKLE[j]
            if np.count_nonzero( self.NSPLIT[e] ) < 2: continue
            for p1,p2,p3 in zip([0,1,2],[1,2,0],[2,0,1]):
               if self.NSPLIT[e[p1]] > 0 and self.NSPLIT[e[p2]] > 0:
                  if PEHALO.has_key((e[p1],e[p2])):  # the good side opposes the dark side
                     PEHALO[(e[p1],e[p2])][0] = k    # /!\ can you be smatter about the edges' orientation ?
                     if self.NSPLIT[e[p3]] == 0: self.NSPLIT[e[p3]] = -1
                     if self.NSPLIT[e[p3]] == -1:
                        if not SEHALO.has_key((e[p1],e[p3])): SEHALO.update({ (e[p1],e[p3]):[] })
                        SEHALO[(e[p1],e[p3])].append(k)
                        if not SEHALO.has_key((e[p2],e[p3])): SEHALO.update({ (e[p2],e[p3]):[] })
                        SEHALO[(e[p2],e[p3])].append(k)
                     else: # self.NSPLIT[e[p3]] must be 2 !
                        if not SEHALO.has_key((e[p3],e[p1])): SEHALO.update({ (e[p3],e[p1]):[] })
                        if k not in SEHALO[(e[p3],e[p1])]: SEHALO[(e[p3],e[p1])].append(k)
                        if not SEHALO.has_key((e[p2],e[p3])): SEHALO.update({ (e[p2],e[p3]):[] })
                        if k not in SEHALO[(e[p2],e[p3])]: SEHALO[(e[p2],e[p3])].append(k)
                     if self.PNIKLE[j] >= 0: self.PNIKLE[j] = -(self.PNIKLE[j]+1)     # /!\ This is very dangerous but necessary
            pbar.update(ibar)
         pbar.finish()
         # Sub Step 3: Final clean up of the other side ? no need but check later for (ei)[0] == 0
         #   Step 2: find out about the secondary elements on IKLE ( local LIKLE ? )
         print '       ~> Assembling secondary elements of that side'
         pbar = ProgressBar(maxval=len(LIKLE[part])).start()
         for k in range(len(LIKLE[part])):
            j = KELLG[part][k]
            e = self.slf.IKLE[j]
            if self.PNIKLE[j] != part: continue
            if np.count_nonzero( self.NSPLIT[e] ) < 2: continue
            for i in [0,1,2]:
               ii = (i+1)%3
               if self.NSPLIT[e[i]] > 0 and self.NSPLIT[e[ii]] < 0 and SEHALO.has_key((e[i],e[ii])): SEHALO[(e[i],e[ii])].append(k) # correct orientation
               if self.NSPLIT[e[i]] > 0 and self.NSPLIT[e[ii]] > 0 and SEHALO.has_key((e[ii],e[i])): SEHALO[(e[ii],e[i])].append(k) # opposite orientation
               ii = (i+2)%3
               if self.NSPLIT[e[i]] > 0 and self.NSPLIT[e[ii]] < 0 and SEHALO.has_key((e[i],e[ii])): SEHALO[(e[i],e[ii])].append(k) # correct orientation
               if self.NSPLIT[e[i]] > 0 and self.NSPLIT[e[ii]] > 0 and SEHALO.has_key((e[i],e[ii])): SEHALO[(e[i],e[ii])].append(k) # opposite orientation
            if self.PNIKLE[j] < 0: self.PNIKLE[j] = -self.PNIKLE[j] - 1    # /!\ back to a safe place
            pbar.update(k)
         pbar.finish()
         #   Step 3: finally cross reference information between SEHALO and PEHALO
         print '       ~> Combining sides surrounding the halo-elements'
         for ie in PEHALO.keys():
            if PEHALO[ie][0] == 0: continue
            k = PEHALO[ie][0]      # element number in its local part numbering
            if not IFAPAR[part].has_key(k): IFAPAR[part].update({ k:[-2,-1,-2,-1,-2,-1] })
            j = KELLG[part][k]
            e = self.slf.IKLE[j]
            for p1,p2 in zip([0,1,2],[1,2,0]):
               if SEHALO.has_key((e[p1],e[p2])):
                  if len(SEHALO[(e[p1],e[p2])]) > 1:
                     if SEHALO[(e[p1],e[p2])][0] == k: IFAPAR[part][k][2*p1] = SEHALO[(e[p1],e[p2])][1]
                     if SEHALO[(e[p1],e[p2])][1] == k: IFAPAR[part][k][2*p1] = SEHALO[(e[p1],e[p2])][0]
                     IFAPAR[part][k][1+2*p1] = part
               if SEHALO.has_key((e[p2],e[p1])):
                  if len(SEHALO[(e[p2],e[p1])]) > 1:
                     if SEHALO[(e[p2],e[p1])][0] == k: IFAPAR[part][k][2*p1] = SEHALO[(e[p2],e[p1])][1]
                     if SEHALO[(e[p2],e[p1])][1] == k: IFAPAR[part][k][2*p1] = SEHALO[(e[p2],e[p1])][0]
                     IFAPAR[part][k][1+2*p1] = part
               if ie == (e[p1],e[p2]):
                  IFAPAR[part][k][2*p1] = PEHALO[ie][1][0]
                  IFAPAR[part][k][1+2*p1] = PEHALO[ie][1][1]

      # ~~> CONLIM file: Write to file ... pfuuuuuh ... this is it !
      print '\n... Split of the CONLIM files'
      for part in range(self.NPARTS):
         fmti = '00000' + str(part)
         fmti = fmti[len(fmti)-5:]

         print '    +> part: ',part+1,' of ',self.NPARTS
         # ~~> CONLIM file: Set the filter
         INDEX = np.zeros_like(self.clm.INDEX,dtype=np.int)
         for contour in polyFILTER[part]:
            # ~~> Closed contour: no need to change ISEG
            if contour[0] == contour[len(contour)-1]:
               for c in contour[1:]: INDEX[self.clm.KFRGL[c]] = self.clm.KFRGL[c]+1
            # ~~> Open contour: need to change ISEG with neighbours
            else:
               for c in contour[0:]: INDEX[self.clm.KFRGL[c]] = self.clm.KFRGL[c]+1
               iA = self.clm.KFRGL[contour[0]]
               self.clm.POR['is'][iA] = ISEG[contour[0]][part]
               self.clm.POR['xs'][iA] = self.slf.MESHX[abs(ISEG[contour[0]][part])-1]  # /!\ MESHX start at 0
               self.clm.POR['ys'][iA] = self.slf.MESHY[abs(ISEG[contour[0]][part])-1]  # /!\ MESHY start at 0
               iA = self.clm.KFRGL[contour[len(contour)-1]]
               self.clm.POR['is'][iA] = ISEG[contour[len(contour)-1]][part]
               self.clm.POR['xs'][iA] = self.slf.MESHX[abs(ISEG[contour[len(contour)-1]][part])-1]
               self.clm.POR['ys'][iA] = self.slf.MESHY[abs(ISEG[contour[len(contour)-1]][part])-1]
         self.clm.INDEX = INDEX

         # ~~> CONLIM file: Set the NPTIR and CUTs
         self.clm.NPTIR = NPTIR[part]

         # ~~> CONLIM file: Set the IFAPAR
         self.clm.IFAPAR = IFAPAR[part]

         # ~~> CONLIM file
         fileRoot,fileExts = path.splitext(self.clm.fileName)
         print '       ~> printing: ',fileRoot+fmtn+'-'+fmti+fileExts
         self.clm.putContent(fileRoot+fmtn+'-'+fmti+fileExts)

      return

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
