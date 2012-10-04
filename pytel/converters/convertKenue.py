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
"""@brief
         Tools for handling conversions to-from Kenue types of files
"""
"""@details
         Contains read/write functions for Kenue files
"""
"""@history 07/01/2012 -- Sebastien E. Bourban:
         Implementation of the i2s/i3s => shape files.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
import numpy as np
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
from config import OptionParser
# ~~> dependencies towards other modules
from parsers.parserKenue import InS,getInS
import shapefile
# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#

class InS2Shp(InS):

   def __init__(self,fileName):
      InS.__init__(self,fileName)
      print '~~>',fileName
      if self.fileName != '':
         self.head,self.fileType,self.npoin,self.poly,self.type,self.atrbut = getInS(self.fileName)

   def sph2ll(self,(long0,lat0)):
      radius  = 6371000.
      long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
      const = np.tan( lat0/2. + np.pi/4. )
      print self.poly
      sys.exit()
      for poly in self.poly:
         for ip in range(len(poly)):
            poly[ip] = [np.rad2deg( poly[ip][0]/radius + long0 ),np.rad2deg( 2.*np.arctan( const*np.exp(poly[ip][1]/radius) ) - np.pi/2. )]

   def putContent(self,fileName):
      f = shapefile.Writer()
      i = 1                     # attributes starts at one
      for poly in self.poly:
         f.poly(shapeType = 5, parts = [poly])
         if i == 1:
            for k in self.atrbut.keys(): f.field(self.atrbut[k][0], 'C', '40')
         a = []
         for k in self.atrbut.keys(): a.append(self.atrbut[k][i])
         f.record(*a)
         i += 1
      f.save(path.basename(path.splitext(fileName)[0]))
      return

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$13-Jan-2012 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("--sph2ll",type="string",dest="sph2ll",default=None,help="convert from spherical to longitude-latitude" )
   options, args = parser.parse_args()
   if len(args) < 1:
      print '\nAt least a code name (and its associated inputs) are required\n'
      parser.print_help()
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = args[0]

   if codeName in ['i2s2shp'] :
      insFiles = args[1:]
      for insFile in insFiles:

         insFile = path.realpath(insFile)
         print '\n\nSmoothing ' + path.basename(insFile) + ' within ' + path.dirname(insFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         ins = InS2Shp( insFile )
         if options.sph2ll != None: ins.sph2ll(options.sph2ll.split(":"))

         ins.putContent( insFile )


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
