#!/usr/bin/env python
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
         Tools for handling KENUE files and related in python
"""
"""@details
         Contains a number of functions for KENUE type files
         that used to be written in Fortran.
"""
"""@history 07/01/2012 -- Sebastien E. Bourban:
         Implementation of the InS class and of its smoothing functions.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
from argparse import ArgumentParser,RawDescriptionHelpFormatter
# ~~> dependencies towards other modules
# ~~> dependencies towards other modules
from parsers.parserKenue import InS
from utils.files import moveFile
# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                    _________________________________________
# ____/ Secondary Classes /________________________________________/
#

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$13-Jan-2012 08:51:29$"

""" Options for each code name
      ins in.i2s out.i2s
      + '--replace': replace the input file by the output file, in which case
        multiple input files can be used
"""

def main(action=None):
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n'+'~'*72+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Tools for handling KENUE files and related in python
      '''))
   parser.add_argument( "args",nargs='*' )
   # valid for i2s / i3s
   parser.add_argument(\
      "--replace",action="store_true",dest="freplace",default=False,
      help="if present, the output file will eventualy replace the input file" )
   parser.add_argument(\
      "--duplicates",action="store_true",dest="fduplicates",default=False,
      help="if present, remove duplicate points" )
   parser.add_argument(\
      "--duplangles",action="store_true",dest="fduplangles",default=False,
      help="if present, remove return angles" )
   parser.add_argument(\
      "--subdivise",dest="fsubdivise",default=None,
      help="if present, use the subdivise method (first)" )
   parser.add_argument(\
      "--subsample",dest="fsubsample",default=None,
      help="if present, use the subsample method (distance=..;angle=.." )
   parser.add_argument(\
      "--clockwise",action="store_true",dest="fclock",default=False,
      help="if present, anticlockwise polylines will be converted clockwise" )
   parser.add_argument(\
      "--aclockwise",action="store_true",dest="faclock",default=False,
      help="if present, clockwise polylines will be converted anticlockwise" )
   parser.add_argument(\
      "--sph2ll",dest="sph2ll",default=None,
      help="convert from spherical to longitude-latitude" )
   parser.add_argument(\
      "--ll2sph",dest="ll2sph",default=None,
      help="convert from longitude-latitude to spherical" )

   options = parser.parse_args()
   if not action is None:
       options.args.insert(0, action)
   if len(options.args) < 1:
      print '\nThe name of one file at least is required\n'
      parser.print_help()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = options.args[0]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of I2S / I3S ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if codeName in ['i2s','i3s'] :

      if not options.freplace:
         if len(options.args) != 3:
            print '\nThe code ',codeName,' (without --replace) uses a minimum of 2 argumensts, aside from the options\n'
            parser.print_help()
            sys.exit(1)
         insFiles = [ options.args[1] ]
         outFile = options.args[2]
      else:
         insFiles = options.args[1:]
         outFile = "smooth-tmp.i2s"

      for insFile in insFiles:

         insFile = path.realpath(insFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         print '\n\nProcessing ' + path.basename(insFile) + ' within ' + path.dirname(insFile) + '\n'+'~'*72+'\n'
         ins = InS( insFile )
         if options.sph2ll != None: ins.sph2ll(options.sph2ll.split(":"))
         if options.ll2sph != None: ins.ll2sph(options.ll2sph.split(":"))
         if options.fclock:
            print '\nMake closed loops clockwise'
            ins.makeClockwise()
         if options.faclock:
            print '\nMake closed loops anti-clockwise'
            ins.makeAntiClockwise()
         #if options.fduplicates:
         #   print '\nRemove duplicates'
         #   ins.removeDuplicates()
         #if options.fduplangles:
         #   print '\nRemove return angles'
         #   ins.removeDuplangles()
         if options.fsubdivise != None:
            print '\nSubdivise and average'
            ins.smoothSubdivise(float(options.fsubdivise))
         if options.fsubsample != None:
            distance = ''; angle = ''
            for dw in options.fsubsample.split(';'):
               if "dist" in dw.split('=')[0]: distance = dw.split('=')[1]
               if "angl" in dw.split('=')[0]: angle = dw.split('=')[1]
            if distance != '':
               print '\nSubsample based on proximity'
               ins.smoothSubsampleDistance(float(distance))
            if angle != '':
               print '\nSubsample based on flatness'
               ins.smoothSubsampleAngle(float(angle))

         ins.putContent( outFile )

         if options.freplace: moveFile(outFile,insFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of UNKNOWN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print '\nDo not know what to do with this code name: ',codeName
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'
   sys.exit(0)

if __name__ == "__main__":
    main(None)

