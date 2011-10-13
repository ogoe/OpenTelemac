"""@brief Tools for handling SELAFIN files and TELEMAC binary related in python
"""
"""@author Christopher J. Cawthorn and Sebastien E. Bourban
"""
"""@details Contains getSLF and putSLF, which
            read/write python variables into
            binary (big-endian) SELAFIN files
"""
"""@history 09/06/2011 Original Version
"""
from struct import unpack
from os import path
import sys
import numpy as np

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

def getTimeProfileSLF(f,NVAR,NPOIN3):

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

   ATtags,ATs = getTimeProfileSLF(f,NBV1+NBV2,NPOIN3)
   tags.update({ 'core': ATtags })

   return tags, TITLE,(NELEM3,NPOIN3,NDP,NPLAN),(NBV1,NBV2,VARNAMES,VARUNITS),(IKLE,IPOBO,MESHX,MESHY), ATs

def getValueProfilesSLF( args ):

   fileName,ext,var = args
   # ~~ Extract data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f = open(fileName,'rb')
   tags,title,nbrs,vars,mesh,x = parseSLF(f)
   NELEM3,NPOIN3,NDP,NPLAN = nbrs
   NBV1,NBV2,VARNAMES,VARUNITS = vars
   IKLE,IPOBO,MESHX,MESHY = mesh
   # ~~ Find sample locations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   le,ln,bn = locateXY_inMeshSLF( ext, NELEM3,IKLE,MESHX,MESHY )
   # ~~ Find variable indices ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   varsNber = []; varsName = []
   for ivar in range(len(VARNAMES)):
      for v in var.split(';'):
         if v in VARNAMES[ivar].strip():
            varsNber.append(ivar)
            varsName.append(VARNAMES[ivar].strip())
   if not len(varsNber) == len(var.split(';')):
      print "... Could not find all variables: "
      sys.exit()
   # ~~ Extract time profiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   z = np.zeros((len(varsNber),len(bn),len(tags['core'])))
   for t in range(len(tags['core'])):
      f.seek(tags['core'][t])
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
   # ~~ Close data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   f.close()
   return ('Time (s)',x),(title,varsName,le,z)

def getMeshElementSLF(args):
   
   fileName = args
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
   This function return the element number for the triangle including xyo=(xo,yo)
      or -1 if the (xo,yo) is outside the mesh
   It should be noted that (xo,yo) are arrays so only one search is necessary for
      multiple pairs
   Return: locate, and array of integers of size len(xyo)
"""
def locateXY_inMeshSLF(xyo,NELEM,IKLE,MESHX,MESHY):
   
   locate = - np.ones((len(xyo),), dtype=np.int)
   locatn = - np.ones((len(xyo),3), dtype=np.int)
   bryctr = np.zeros((len(xyo),3))
   
   for e in range(NELEM):
      # barycentric coordinates
      p = [ IKLE[e][0], IKLE[e][1], IKLE[e][2] ]
      det = ( MESHY[p[1]]-MESHY[p[2]] ) * ( MESHX[p[0]]-MESHX[p[2]] ) - \
            ( MESHY[p[0]]-MESHY[p[2]] ) * ( MESHX[p[1]]-MESHX[p[2]] )
      for io in range(len(xyo)):
         xo,yo = xyo[io]
         l1 = ( ( MESHY[p[1]]-MESHY[p[2]] ) * (    xo      -MESHX[p[2]] ) + \
                (     yo     -MESHY[p[2]] ) * ( MESHX[p[2]]-MESHX[p[1]] ) )/det
         l2 = ( ( MESHY[p[2]]-MESHY[p[0]] ) * (    xo      -MESHX[p[2]] ) + \
                (     yo     -MESHY[p[2]] ) * ( MESHX[p[0]]-MESHX[p[2]] ) )/det
         l3 = 1.0 - l2 - l1
         if l1 >= 0.0 and l1 <= 1.0 and l2 >= 0.0 and l2 <= 1.0 and l3 >= 0.0 and l3 <= 1.0 :
            locate[io] = e
            locatn[io] = p
            bryctr[io] = [ l1, l2, l3 ]

   return locate,locatn,bryctr

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

