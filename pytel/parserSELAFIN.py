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

def getHeaderSLF(f):
   """ getSelafin: Read a binary (big-endian) Selafin file
                     and returns the data in several variables
   """
   TITLE,NBV1,NBV2,VARNAMES,VARUNITS,IPARAM,NELEM3,NPOIN3,NDP,NPLAN = getHeaderParametersSLF(f)
   IKLE,IPOBO,MESHX,MESHY = getHeaderMeshSLF(f,NELEM3,NPOIN3,NDP,NPLAN)

   return TITLE,(NELEM3,NPOIN3,NDP,NPLAN),(NBV1,NBV2,VARNAMES,VARUNITS),(IKLE,IPOBO,MESHX,MESHY)

def getCoreValueSLF(f,ivar,NPOIN3):
   
   VARSOR = []
   # ~~ Read AT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   l = unpack('>i',f.read(4))[0]
   time = unpack('>f',f.read(4))[0]
   chk = unpack('>i',f.read(4))[0]
   if l!=chk: print 'Error reading geo'

   # ~~ Read AT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for i in range(ivar):
      l= unpack('>i',f.read(4))[0]
      unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
      chk =unpack('>i',f.read(4))[0]
      if l!=chk:
         print 'Error reading VARSOR['+str(i)+']'
   l= unpack('>i',f.read(4))[0]
   VARSOR = unpack('>'+str(NPOIN3)+'f',f.read(4*NPOIN3))
   chk =unpack('>i',f.read(4))[0]
   if l!=chk:
      print 'Error reading VARSOR['+str(ivar)+']'

   return np.asarray(VARSOR)
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

