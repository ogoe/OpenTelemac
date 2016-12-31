"""@author Juliette Paraisi and Sebastien Bourban
"""
"""@note ... this work was carried out by
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

"""
"""@details

"""
"""@history 12/12/2014 -- Sebastien E. Bourban

"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
import numpy as np
from matplotlib.tri import Triangulation
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
from config import OptionParser
from parsers.parserSELAFIN import SELAFIN
from utils.progressbar import ProgressBar
from converters import convertUTM as utm
from samplers import polygons

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#

class GEBCO(SELAFIN):

   def __init__(self,fname,vals=(None,None)):

      # ~~> empty SELAFIN
      SELAFIN.__init__(self,'')
      self.DATETIME = []

      # ~~> variables
      self.TITLE = ''
      self.NBV1 = 1 # bathymetry only
      self.NVAR = self.NBV1
      self.VARINDEX = range(self.NVAR)
      self.VARNAMES = ['BOTTOM          ']
      self.VARUNITS = ['M               ']

      print '     +> header'
      # ~~> load header (ASC type)
      gebcofile = open(fname,'r')
      # ~~
      gline = []
      gline.append(gebcofile.readline().split())
      if gline[-1][0] == "ncols": NX1D = int(gline[-1][1])
      else:
         print '.. Could not read this file format. Key ncols expected here.'
         sys.exit(1)
      gline.append(gebcofile.readline().split())
      if gline[-1][0] == "nrows": NY1D = int(gline[-1][1])
      else:
         print '.. Could not read this file format. Key nrows expected here.'
         sys.exit(1)
      gline.append(gebcofile.readline().split())
      if gline[-1][0] == "xllcorner": xllcorner = np.float(gline[-1][1])
      else:
         print '.. Could not read this file format. Key xllcorner expected here.'
         sys.exit(1)
      gline.append(gebcofile.readline().split())
      if gline[-1][0] == "yllcorner": yllcorner = np.float(gline[-1][1])
      else:
         print '.. Could not read this file format. Key yllcorner expected here.'
         sys.exit(1)
      gline.append(gebcofile.readline().split())
      if gline[-1][0] == "cellsize":
         xdim = np.float(gline[-1][1])
         ydim = xdim
      elif gline[-1][0] in ["xdim","dx"]:
         xdim = np.float(gline[-1][1])
         gline.append(gebcofile.readline().split())
         if gline[-1][0] in ["ydim","dy"]: ydim = np.float(gline[-1][1])
         else:
            print '.. Could not read this file format. Key ydim expected here.'
            sys.exit(1)
      else:
         print '.. Could not read this file format. Key cellsize or xdim expected here.'
         sys.exit(1)
      gline.append(gebcofile.readline().split())
      if gline[-1][0] == "NODATA_value": NODATA_value = int(gline[-1][1])
      else:
         print '.. Could not read this file format. Key NODATA_value expected here.'
         sys.exit(1)
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gebcofile.close()

      print '     +> bathymetry'
      # ~~> load ASCII content, ignoring the header lines
      z = np.loadtxt(fname, skiprows=len(gline)).T.ravel()
      print '     +> filtered connectivity'
      # ~~> temporary IKLE
      aval = min(z) - 1
      if vals[0] != None: aval = float(vals[0])
      bval = max(z) + 1
      if vals[1] != None: bval = float(vals[1])
      ielem = 0; pbar = ProgressBar(maxval=2*(NX1D-1)*(NY1D-1)).start()
      ikle3 = - np.ones((2*(NX1D-1)*(NY1D-1),3),dtype=np.int)
      for i in range(1,NX1D):
         for j in range(1,NY1D):
            ipoin = (i-1)*NY1D + j - 1
            # ~~> first triangle
            if ( aval < z[ipoin] < bval ) and \
               ( aval < z[ipoin + NY1D] < bval ) and \
               ( aval < z[ipoin + 1] < bval ):
               ikle3[ielem] = [ ipoin, ipoin + 1, ipoin + NY1D ]
            ielem = ielem + 1
            pbar.update(ielem)
            # ~~> second triangle
            if ( aval < z[ipoin + NY1D] < bval ) and \
               ( aval < z[ipoin + NY1D + 1] < bval ) and \
               ( aval < z[ipoin + 1] < bval ):
               ikle3[ielem] = [ ipoin + NY1D, ipoin + 1, ipoin + NY1D + 1 ]
            ielem = ielem + 1
            pbar.update(ielem)
      pbar.finish()

      print '     +> renumbered connectivity'
      # ~~> intermediate connectivity
      GIKLE = ikle3[np.not_equal(*(np.sort(ikle3).T[0::2]))]
      KNOLG = np.unique( np.ravel(GIKLE) )
      KNOGL = dict(zip( KNOLG,range(len(KNOLG)) ))
      # ~~> final connectivity
      self.IKLE3 = - np.ones_like(GIKLE,dtype=np.int)
      pbar = ProgressBar(maxval=len(GIKLE)).start()
      for k in range(len(GIKLE)):
         self.IKLE3[k] = [ KNOGL[GIKLE[k][0]], KNOGL[GIKLE[k][1]], KNOGL[GIKLE[k][2]] ]
         pbar.update(k)
      pbar.finish()

      print '     +> mesh x,y,z'
      # ~~> defines grid
      x = xllcorner + xdim * np.arange(NX1D,dtype=np.float) - xdim/2.
      y = yllcorner - ydim * np.arange(NY1D,dtype=np.float) + ydim * NY1D - ydim/2.
      self.MESHX = np.tile(x,NY1D).reshape(NY1D,NX1D).T.ravel()[KNOLG]
      self.MESHY = np.tile(y,NX1D)[KNOLG]
      self.z = z[KNOLG]

      print '     +> sizes'
      # ~~> sizes
      self.NPLAN = 1
      self.NDP2 = 3
      self.NDP3 = self.NDP2
      self.NPOIN2 = len(self.MESHX)
      self.NPOIN3 = self.NPOIN2
      self.NELEM2 = len(self.IKLE3)
      self.NELEM3 = self.NELEM2
      self.IPARAM = [ 0,0,0,0,0,0,            1,     0,0,0 ]

      print '     +> boundaries'
      # ~~> establish neighborhood
      neighbours = Triangulation(self.MESHX,self.MESHY,self.IKLE3).get_cpp_triangulation().get_neighbors()
      # ~~> build the enssemble of boundary segments
      ebounds = []
      print '        - identify'
      pbar = ProgressBar(maxval=self.NELEM3).start()
      for i in range(self.NELEM3):
         if neighbours[i,0] < 0: ebounds.append([self.IKLE3[i][0],self.IKLE3[i][1]])
         if neighbours[i,1] < 0: ebounds.append([self.IKLE3[i][1],self.IKLE3[i][2]])
         if neighbours[i,2] < 0: ebounds.append([self.IKLE3[i][2],self.IKLE3[i][0]])
         pbar.update(i)
      pbar.finish()
      # ~~> assemble the enssemble of boundary segments
      print '        - assemble'
      pbounds = polygons.joinSegments(ebounds)
      # ~~> define IPOBO from an arbitrary start point
      print '        - set'
      self.IPOB3 = np.zeros(self.NPOIN3,dtype=np.int)
      iptfr = 0
      for p in pbounds:
         for n in p[1:]:
            iptfr += 1
            self.IPOB3[n] = iptfr
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
      self.appendCoreVarsSLF([self.z])
      self.fole['hook'].close()

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Juliette Parisi"
__date__ ="$12-Oct-2015 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("--above",type="string",dest="abval",default=None,help="select only the values above" )
   parser.add_option("--below",type="string",dest="beval",default=None,help="select only the values below" )
   parser.add_option("--sph2ll",type="string",dest="sph2ll",default=None,help="convert from spherical to longitude-latitude" )
   parser.add_option("--ll2sph",type="string",dest="ll2sph",default=None,help="convert from longitude-latitude to spherical" )
   parser.add_option("--ll2utm",type="string",dest="ll2utm",default=None,help="convert from longitude-latitude to UTM" )
   parser.add_option("--utm2ll",type="string",dest="utm2ll",default=None,help="convert from UTM to longitude-latitude" )
   parser.add_option("--X+?",type="string",dest="axp",default="0",help="adds to the MESHX" )
   options, args = parser.parse_args()

   # rootName
   if len(args) != 1:
      print '... only one file name is necessary to capture the processed dataset.\n\n'
      sys.exit(1)
   rootName = args[-1]
   head,tail = path.splitext(rootName)

   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '\nLoading the GEBCO file\n'
   gebco2slf = GEBCO(rootName,(options.abval,options.beval))

   gebco2slf.MESHX = gebco2slf.MESHX + float(options.axp)
   if options.sph2ll != None:
      radius  = 6371000.
      long0,lat0 = options.sph2ll.split(":")
      long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
      const = np.tan( lat0/2. + np.pi/4. )
      gebco2slf.MESHX = np.rad2deg( gebco2slf.MESHX/radius + long0 )
      gebco2slf.MESHY = np.rad2deg( 2.*np.arctan( const*np.exp(gebco2slf.MESHY/radius) ) - np.pi/2. )
   if options.ll2sph != None:
      radius  = 6371000.
      long0,lat0 = options.ll2sph.split(":")
      long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
      gebco2slf.MESHX = radius * ( np.deg2rad(gebco2slf.MESHX) - long0 )
      gebco2slf.MESHY = radius * ( np.log( np.tan( np.deg2rad(gebco2slf.MESHY)/2. + np.pi/4. ) ) \
                                     - np.log( np.tan( lat0/2. + np.pi/4. ) ) )
   if options.ll2utm != None:
      zone = int(options.ll2utm)
      gebco2slf.MESHX,gebco2slf.MESHY,zone = utm.fromLatLong(gebco2slf.MESHX,gebco2slf.MESHY)
   if options.utm2ll != None:
      zone = int(options.utm2ll)
      gebco2slf.MESHX,gebco2slf.MESHY = utm.toLatLong(gebco2slf.MESHX,gebco2slf.MESHY,zone)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Convert to SELAFIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '\nConverting into SELAFIN\n'
   gebco2slf.putContent(head+'.slf')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
