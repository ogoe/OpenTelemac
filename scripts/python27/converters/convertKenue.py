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
from argparse import ArgumentParser,RawDescriptionHelpFormatter
import numpy as np
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
# ~~> dependencies towards other modules
from parsers.parserKenue import InS
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

   def putContent(self,fileName):
      f = shapefile.Writer()
      i = 1                     # attributes starts at one
      for poly in self.poly:
         f.poly(shapeType = 5, parts = [poly])
         if i == 1:
            for k in self.atrbut: f.field(self.atrbut[k][0], 'C', '40')
         a = []
         for k in self.atrbut: a.append(self.atrbut[k][i])
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
   print '\n\nInterpreting command line options\n'+'~'*72+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Vaiours operations are carried out on Blue Kenue type files
      '''))
   parser.add_argument( "args",default='',nargs='*' )
   parser.add_argument(\
      "--sph2ll",dest="sph2ll",default=None,
      help="convert from spherical to longitude-latitude" )
   options = parser.parse_args()
   if len(options.args) < 1:
      print '\nAt least a code name (and its associated inputs) are required\n'
      parser.print_help()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = options.args[0]

   if codeName in ['i2s2shp'] :
      insFiles = options.args[1:]
      for insFile in insFiles:

         insFile = path.realpath(insFile)
         print '\n\nSmoothing ' + path.basename(insFile) + ' within ' + path.dirname(insFile) + '\n'+'~'*72+'\n'
         ins = InS2Shp( insFile )
         if options.sph2ll != None: ins.sph2ll(options.sph2ll.split(":"))

         ins.putContent( insFile )


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
