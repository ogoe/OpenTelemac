"""@author Sebastien E. Bourban
"""
"""@note ... this work is based on a collaborative effort between
  .________.
  |        |
  |,-.    /   HR Wallingford
  /   \  /    Howbery Park,
   ,.  `'     Wallingford, Oxfordshire
  /  \   /    OX10 8BA, United Kingdom
 /    `-'|    www.hrwallingford.com
!________!
"""
"""@brief
         Tools for handling MSH files when created by the mesh generator GMSH
"""
"""@details
         Contains read/write functions for binary and asci MSH files
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from struct import unpack
import re
import sys
from os import path
import numpy as np
from matplotlib.tri import Triangulation
# ~~> dependencies towards the root of pytel
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) ) # clever you !
from config import OptionParser
# ~~> dependencies towards other pytel/modules
from parsers.parserSELAFIN import SELAFIN
from parsers.parserKenue import InS
from utils.files import putFileContent

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#

class GEO(InS):

   def __init__(self,fileName):

      # ~~> possibly empty i2s
      InS.__init__(self,fileName)

      # TODO: You may need to account for the fact that more than one polygon
      #       is an outside domain boundary
      # ~~> sort out by the area to build the order map
      self.sortByArea()
      # ~~> make the first / larger polygon anti-clockwise
      self.makeAntiClockwise(select=[0])
      # ~~> make the other polygons clockwise
      self.makeClockwise(select=range(len(self.poly))[1:])

      # ~~> initialisasing counters
      self.ipoin = [0]; self.iline = [0]
      self.iloop = [0]; self.isurf = [0]

   def writePolygon(self,poly):
      # TODO: values / resolution
      geo = []
      ipoin = self.ipoin[-1]
      for i,xy in zip(range(len(poly)),poly):
         geo.append('Point('+str(i+ipoin+1)+') = { '+str(xy[0])+','+str(xy[1])+',0,2000 };')
      # Lines
      iline = self.iline[-1]
      for i in range(len(poly))[:-1]:
         geo.append('Line('+str(i+iline+1)+') = { '+str(i+iline+1)+','+str(i+iline+2)+' };')
      geo.append('Line('+str(len(poly)+iline)+') = { '+str(len(poly)+iline)+','+str(iline+1)+' };')
      # Line Loop
      iloop = self.iloop[-1]
      geo.append('Line Loop('+str(len(poly)+iloop+1)+') = {'+','.join([ str(i+iloop+1) for i in range(len(poly))])+' };')
      # next set of entities
      self.ipoin.append(ipoin+len(poly))
      self.iline.append(iline+len(poly)+1)
      self.iloop.append(iloop+len(poly)+1)
      return geo

   def putContent(self,fileName):

      geo = []
      # assuming the first polygon is the outside domain
      ip = self.order[0]
      geo.extend(self.writePolygon(self.poly[ip]))
      # add the other polygons
      for ip in self.order[1:]: geo.extend(self.writePolygon(self.poly[ip]))

      # make up surface with wholes
      psurf = self.isurf[-1]
      geo.append('Plane Surface('+str(psurf+1)+') = {'+','.join([ str(i) for i in self.iloop[1:] ])+'};')
      self.isurf.append(psurf+1)

      # write up
      putFileContent(fileName,geo)


class MSH(SELAFIN):

   mshkeys = { "MeshFormat":'', "Nodes":'', "Elements":[], \
       "PhysicalName":'', "Periodic":'', "NodeData":'', "ElementData":'', "ElementNodeData":'', "InterpolationScheme":'' }

   frst_keys = re.compile(r'[$](?P<key>[^\s]+)\s*\Z',re.I)
   last_keys = re.compile(r'[$]End(?P<key>[^\s]+)\s*\Z',re.I)

   def __init__(self,fileName):

      # ~~> empty SELAFIN
      SELAFIN.__init__(self,'')
      self.DATETIME = []

      # ~~> variables
      self.TITLE = ''
      self.NBV1 = 1
      self.NVAR = self.NBV1
      self.VARINDEX = range(self.NVAR)
      self.VARNAMES = ['BOTTOM          ']
      self.VARUNITS = ['M               ']

      # ~~ Openning files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.file = {}
      self.file.update({ 'name': fileName })
      self.file.update({ 'endian': ">" })    # "<" means little-endian, ">" means big-endian
      self.file.update({ 'integer': ('i',4) }) #'i' size 4
      self.file.update({ 'float': ('f',4) }) #'f' size 4, 'd' = size 8
      self.file.update({ 'hook': open(fileName,'rt') })
      file = iter(self.file['hook'])

      # ~~ Read/Write dimensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Note:
      #    The section MeshFormat is mandatory
      line = file.next()
      proc = re.match(self.frst_keys,line)
      if proc:
         if proc.group('key') != "MeshFormat":
            print '... Could not recognise your MSH file format. Missing MeshFormat key.'
            sys.exit(1)
         line = file.next().split()
         if line[0] != "2.2":
            print '... Could not read your MSH file format. Only the version 2.2 is allowed.'
            sys.exit(1)
         fileType = int(line[1])
         if fileType == 1:
            print '... I have never done this before. Do check it works'
            line = file.next()
            l,isize,chk = unpack('>i',line.read(4+4+4))
         floatSize = int(line[2])
         if floatSize == 8: self.file['float'] = ('d',8)
      line = file.next()
      proc = re.match(self.last_keys,line)
      if proc:
         if proc.group('key') != "MeshFormat":
            print '... Could not complete reading the header of you MSH file format. Missing EndMeshFormat key.'
            sys.exit(1)

      # ~~ Loop on sections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      while True:
         try: line = file.next()
         except: break
         proc = re.match(self.frst_keys,line)
         if not proc:
            print '... Was expecting a new Section starter. Found this instead: ',line
            sys.exit(1)
         key = proc.group('key')

      # ~~ Section Nodes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         if key == "Nodes":
            print '     +> mesh x,y,z'
            NPOIN = int(file.next())
            if self.file['float'][0] == 'd':
               MESHX = np.zeros(NPOIN,dtype=np.float64)
               MESHY = np.zeros(NPOIN,dtype=np.float64)
               MESHZ = np.zeros(NPOIN,dtype=np.float64)
            else:
               MESHX = np.zeros(NPOIN,dtype=np.float)
               MESHY = np.zeros(NPOIN,dtype=np.float)
               MESHZ = np.zeros(NPOIN,dtype=np.float)
            #map_nodes = []
            for i in range(NPOIN):
               line = file.next().split()
               #map_nodes.append(int(line[0]))
               MESHX[i] = np.float(line[1])
               MESHY[i] = np.float(line[2])
               MESHZ[i] = np.float(line[3])
            # TODO: renumbering nodes according to map_nodes ?
            #map_nodes = np.asarray(map_nodes)
            self.NPOIN2 = NPOIN
            self.MESHX = MESHX
            self.MESHY = MESHY
            self.MESHZ = MESHZ

            line = file.next()

      # ~~ Section Nodes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         elif proc.group('key') == "Elements":
            print '     +> renumbered connectivity'
            NELEM = int(file.next())
            IKLE2 = - np.ones((NELEM,3),dtype=np.int)
            for i in range(NELEM):
               line = file.next().split()
               if int(line[1]) != 2: continue
               e = line[ int(line[2])+3: ]
               IKLE2[i] = [ np.int(e[0]),np.int(e[1]),np.int(e[2]) ]

            self.IKLE2 = IKLE2[np.not_equal(*(np.sort(IKLE2).T[0::2]))] - 1
            self.NELEM2 = len(self.IKLE2)

            line = file.next()
            # TODO: fitting the unique node numbers with map_nodes ?

      # ~~ Unnecessary section ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         else:
            while True:
               line = file.next()
               if re.match(self.last_keys,line): break

      proc = re.match(self.last_keys,line)
      if proc:
         if proc.group('key') != key:
            print '... Could not complete reading the header of you MSH file format. Missing ',key,' end key.'
            sys.exit(1)

      # ~~> sizes
      print '     +> sizes'
      self.NDP3 = 3; self.NDP2 = 3; self.NPLAN = 1
      self.NELEM3 = self.NELEM2; self.NPOIN3 = self.NPOIN2
      self.IKLE3 = self.IKLE2
      self.IPARAM = [ 0,0,0,0,0,0,            1,     0,0,0 ]

      print '     +> boundaries'
      # ~~> establish neighborhood
      neighbours = Triangulation(self.MESHX,self.MESHY,self.IKLE3).get_cpp_triangulation().get_neighbors()
      # ~~> build the enssemble of boundary segments
      ebounds = []
      #print '        - identify'
      #pbar = ProgressBar(maxval=self.NELEM3).start()
      #for i in range(self.NELEM3):
      #   if neighbours[i,0] < 0: ebounds.append([self.IKLE3[i][0],self.IKLE3[i][1]])
      #   if neighbours[i,1] < 0: ebounds.append([self.IKLE3[i][1],self.IKLE3[i][2]])
      #   if neighbours[i,2] < 0: ebounds.append([self.IKLE3[i][2],self.IKLE3[i][0]])
      #   pbar.update(i)
      #pbar.finish()
      # ~~> assemble the enssemble of boundary segments
      #print '        - assemble'
      #pbounds = polygons.joinSegments(ebounds)
      # ~~> define IPOBO from an arbitrary start point
      #print '        - set'
      self.IPOB3 = np.ones(self.NPOIN3,dtype=np.int)
      #self.IPOB3 = np.zeros(self.NPOIN3,dtype=np.int)
      #iptfr = 0
      #for p in pbounds:
      #   for n in p[1:]:
      #      iptfr += 1
      #      self.IPOB3[n] = iptfr
      self.IPOB2 = self.IPOB3

   def putContent(self,fileName,showbar=True):

      # ~~> new SELAFIN writer
      self.fole = {}
      self.fole.update({ 'hook': open(fileName,'wb') })
      self.fole.update({ 'name': fileName })
      self.fole.update({ 'endian': ">" })     # big endian
      self.fole.update({ 'float': ('f',4) })  # single precision

      print '     +> Write SELAFIN header'
      self.appendHeaderSLF()

      print '     +> Write SELAFIN core'
      self.appendCoreTimeSLF(0.0)
      self.appendCoreVarsSLF([self.MESHZ])
      self.fole['hook'].close()

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$11-Nov-2015 17:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+'~'*72+'\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   options, args = parser.parse_args()
   if len(args) < 1:
      print '\nThe name of and action is required, together with associated arguments\n'
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = args[0]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of MSH to SELAFIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if codeName == 'msh2slf':

      # ~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if len(args) != 2:
         print '.. One MSH file name is required\n'
         parser.print_help()
         sys.exit(1)
      fileName = args[1]
      if not path.exists(fileName):
         print '... Could not file your MSH file: ',fileName
         sys.exit(1)

      # ~~ Parse the MSH file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '      ~> scanning your MSH file: ',path.basename(fileName)
      msh = MSH(fileName)
      head,tail = path.splitext(fileName)
      # ~~ Convert to SELAFIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '      ~> converting it to a SELAFIN: ',path.basename(fileName)
      msh.putContent(head+'.slf')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of i2s/i3s to GEO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'ins2geo':

      # ~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if len(args) != 2:
         print '... One i2s/i3s file name is required\n'
         parser.print_help()
         sys.exit(1)
      fileName = args[1]
      if not path.exists(fileName):
         print '... Could not file your i2s/i3s file: ',fileName
         sys.exit(1)

      # ~~ Parse the i2s/i3s file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '      ~> scanning your MSH file: ',path.basename(fileName)
      geo = GEO(fileName)
      head,tail = path.splitext(fileName)
      # ~~ Convert to GEO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '      ~> converting it to a SELAFIN: ',path.basename(fileName)
      geo.putContent(head+'.geo')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of UNKNOWN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print '\nDo not know what to do with this code name: ',codeName
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
