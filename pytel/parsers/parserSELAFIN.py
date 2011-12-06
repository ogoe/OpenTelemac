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
         Contains getSLF and putSLF, which read/write python variables into
         binary (big-endian) SELAFIN files
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from struct import unpack
import sys
import numpy as np
#np.set_printoptions(precision=16)
# ~~> dependencies towards other pytel/modules
from utils.geometry import isInsideTriangle,getBarycentricWeights,getSegmentIntersection

def getHeaderParametersSLF(f):

   # ~~ Read title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   TITLE = unpack('>80s',f.read(80))[0]
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read TITLE'
      sys.exit()

   # ~~ Read NBV(1) and NBV(2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   NBV1,NBV2 = unpack('>ii',f.read(8))
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read NBVs'
      sys.exit()

   # ~~ Read variable names and units ~~~~~~~~~~~~~~~~~~~~~~~~
   VARNAMES = []; VARUNITS = []
   for i in range(NBV1+NBV2):
     l = unpack('>i',f.read(4))[0]
     VARNAMES.append(unpack('>16s',f.read(16))[0])
     VARUNITS.append(unpack('>16s',f.read(16))[0])
     chk = unpack('>i',f.read(4))[0]
     if l!=chk:
        print '... Cannot read VARNAMES/VARUNITS['+str(i)+']'
        sys.exit()

   # ~~ Read IPARAM array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   IPARAM = unpack('>10i',f.read(40))
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read IPARAM'
      sys.exit()

   # ~~ Read DATE/TIME array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #if IPARAM[9] == 1:
   #   l = unpack('>i',f.read(4))[0]

   # ~~ Read NELEM3, NPOIN3, NDP, NPLAN ~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   NELEM3,NPOIN3,NDP,NPLAN = unpack('>4i',f.read(16))
   chk = unpack('>i',f.read(4))[0]
   if l!=chk:
      print '... Cannot read NELEM3 etc.'
      sys.exit()

   return TITLE, NBV1,NBV2, VARNAMES,VARUNITS, IPARAM, NELEM3,NPOIN3,NDP,NPLAN

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

def parseSLF(f):
   
   tags = { }; f.seek(0)
   
   tags.update({ 'meta': f.tell() })
   TITLE,NBV1,NBV2,VARNAMES,VARUNITS,IPARAM,NELEM3,NPOIN3,NDP,NPLAN = getHeaderParametersSLF(f)

   tags.update({ 'mesh': f.tell() })
   IKLE,IPOBO,MESHX,MESHY = getHeaderMeshSLF(f,NELEM3,NPOIN3,NDP,NPLAN)

   ATtags,ATs = getTimeHistorySLF(f,NBV1+NBV2,NPOIN3)
   tags.update({ 'cores': ATtags })
   tags.update({ 'times': ATs })

   return tags, TITLE,(NELEM3,NPOIN3,NDP,NPLAN),(NBV1,NBV2,VARNAMES,VARUNITS),(IKLE,IPOBO,MESHX,MESHY)

def subsetVariablesSLF(vars,VARNAMES):
   ids = []; names = []

   for ivar in range(len(VARNAMES)):
      for v in vars.split(';'):
         vi = v.split(':')[0]
         if vi.lower() in VARNAMES[ivar].strip().lower():
            ids.append(ivar)
            names.append(VARNAMES[ivar].strip())
   if not len(ids) == len(vars.split(';')):
      print "... Could not find ",vars," in ",VARNAMES
      sys.exit()

   return ids,names

def getValueHistorySLF( f,tags,time,(le,ln,bn),TITLE,NBV1,NBV2,NPOIN3,(varsNber,varsName) ):

   # ~~ Subset time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   subset = time  # /!\ History requires 2 values
   if time[0] < 0: subset = [ 0,max( 0, len(tags['cores']) + time[0] ) ]
   if time[1] < 0: subset = [ time[0],max( 0, len(tags['cores']) + time[1] ) ]

   # ~~ Extract time profiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   z = np.zeros((len(varsNber),len(bn),len(tags['cores'])))
   for t in range(len(tags['cores'])):
      if t < subset[0] or t > subset[1]: continue
      f.seek(tags['cores'][t])
      f.read(4+4+4)
      for ivar in range(NBV1+NBV2):
         f.read(4)
         if ivar in varsNber:
            VARSOR = unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
            for xy in range(len(bn)):
               z[varsNber.index(ivar)][xy][t] = bn[xy][0]*VARSOR[ln[xy][0]] + bn[xy][1]*VARSOR[ln[xy][1]] + bn[xy][2]*VARSOR[ln[xy][2]]
         else:
            f.read(4*NPOIN3)
         f.read(4)

   return ('Time (s)',tags['times']),[(TITLE,varsName,le,z)]

def getMeshElementSLF(fileName,vars):
   
   var,sup = vars.split(':')
   # ~~ Extract data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f = open(fileName,'rb')
   TITLE,NBV1,NBV2,VARNAMES,VARUNITS,IPARAM,NELEM3,NPOIN3,NDP,NPLAN = getHeaderParametersSLF(f)
   IKLE,IPOBO,MESHX,MESHY = getHeaderMeshSLF(f,NELEM3,NPOIN3,NDP,NPLAN)
   # ~~ Close data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.close()
   # ~~ min-max ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xmin = np.min(MESHX); xmax = np.max(MESHX)
   ymin = np.min(MESHY); ymax = np.max(MESHY)
   # ~~ Plot the mesh ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elements = []
   for e in IKLE:
      element = []
      for n in e:
          element.append((MESHX[n],MESHY[n]))
      elements.append(element)

   return (xmin,xmax,ymin,ymax), np.asarray(elements)

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
   
   locate = - np.ones((len(xyo),), dtype=np.int)
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

def getValuePolylineSLF(f,tags,time,(p,ln,bn),TITLE,NBV1,NBV2,NPOIN3,(varsNber,varsName)):
   # TODO: you may have one or two values, the later defining an animation

   # ~~ Subset time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   subset = [ time[0] ]
   if time[0] < 0: subset = [ max( 0, len(tags['cores']) + time[0] ) ]

   # ~~ Extract time profiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   z = np.zeros((len(varsNber),len(p))) #,len(tags['cores'])))
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
   for ivar in range(NBV1+NBV2):
      f.read(4)
      if ivar in varsNber:
         VARSOR = unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
         for xy in range(len(p)):
            z[varsNber.index(ivar)][xy] = bn[xy][0]*VARSOR[ln[xy][0]] + bn[xy][1]*VARSOR[ln[xy][1]] + bn[xy][2]*VARSOR[ln[xy][2]]
      else:
         f.read(4*NPOIN3)
      f.read(4)

   return ('Distance (m)',x),[(TITLE,varsName,z)]
"""

def getSLF(filename):
    # getSelafin: Read a binary (big-endian) Selafin file
    #                 and returns the data in several variables
    
    f = open(filename,'rb')

    TITLE,NBV1,NBV2,VARNAMES,VARUNITS,IPARAM,NELEM3,NPOIN3,NDP,NPLAN = getHeaderSLF(f)
    IKLE,IPOBO,MESHX,MESHY = getHeaderMesh(f,NELEM3,NPOIN3,NDP,NPLAN)

    f.close()


    return TITLE,NBV1,NBV2,VARNAMES,VARUNITS,IPARAM,NELEM3,NPOIN3,NDP,IKLE,IPOBO,MESHX,MESHY,VARSOR,TIME,NTIME

"""
def putSLF(TITLE,NBV1,NBV2,VARNAMES,VARUNITS,IPARAM,NELEM3,NPOIN3,NDP,IKLE,IPOBO,MESHX,MESHY,VARSOR,TIME,NTIME,selfile):
    # Writes a Selafin file (binary, big-endian) for the supplied data.
    
    #File takes the form of a list of Fortran records complete with checksums:
    #(length)[data_entry](length)

    from struct import pack

    sf = open(selfile,'wb')

    # Write padding and title
    l=80 
    sf.write(pack('>i80si',l,TITLE,l))

    # Write NBV1 and NBV2
    l=8
    sf.write(pack('>iiii',l,NBV1,NBV2,l))

    # Write VARNAMES and VARUNITS
    l=32
    for i in range(NBV1+NBV2):
        sf.write(pack('>i',l))
        sf.write(pack('>16s',VARNAMES[i]))
        sf.write(pack('>16s',VARUNITS[i]))
        sf.write(pack('>i',l))

    # Write IPARAM array
    l=40
    sf.write(pack('>i',l))
    sf.write(pack('>10i',*IPARAM))
    sf.write(pack('>i',l))

    # Write NELEM3, NPOIN3, NDP, 1
    l=16
    sf.write(pack('>6i',l,NELEM3,NPOIN3,NDP,1,l))

    # Write IKLE array
    l = NELEM3*NDP*4
    sf.write(pack('>i',l))
    for i in range(NELEM3):
        for j in range(NDP):
            sf.write(pack('>i',IKLE[i][j]))
    sf.write(pack('>i',l))    

    # Write IPOBO array
    l = 4*NPOIN3
    sf.write(pack('>i',l))
    for i in range(NPOIN3):
        sf.write(pack('>i',IPOBO[i]))
    sf.write(pack('>i',l))

    # Write X and Y values
    l=4*NPOIN3
    sf.write(pack('>i',l))
    for i in range(NPOIN3):
        sf.write(pack('>f',MESHX[i]))
    sf.write(pack('>i',l))
    sf.write(pack('>i',l))
    for i in range(NPOIN3):
        sf.write(pack('>f',MESHY[i]))
    sf.write(pack('>i',l))

    # Now loop to add temporal data
    for t in range(NTIME):
        # Print time record
        l = 4
        sf.write(pack('>ifi',l,TIME[t],l))
        # Print variable records
        for i in range(NBV1+NBV2):
            l = 4*NPOIN3
            sf.write(pack('>i',l))
            for j in range(NPOIN3):
                sf.write(pack('>f',VARSOR[t][i][j]))
            sf.write(pack('>i',l))

    sf.close()
    return

def diffSLF():
   return


# Example use: 
if __name__ == "__main__":
    import sys
    TITLE,NBV1,NBV2,VARNAMES,VARUNITS,IPARAM,NELEM3,NPOIN3,NDP,IKLE,IPOBO,MESHX,MESHY,VARSOR,TIME,NTIME = getSLF(sys.argv[1])
    putSLF(TITLE,NBV1,NBV2,VARNAMES,VARUNITS,IPARAM,NELEM3,NPOIN3,NDP,IKLE,IPOBO,MESHX,MESHY,VARSOR,TIME,NTIME,sys.argv[2])

