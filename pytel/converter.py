#!/usr/bin/python
"""@author Yoann Audouin
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
"""@history 15/02/2013 -- Sebastien E. Bourban
         Adding the file in pytel
"""
"""@brief
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import system, remove
import ConfigParser
from optparse import OptionParser

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#
__author__="Yoann Audouin"
__date__ ="$21-Sep-2012 16:51:09$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # define the different file extension and the format name associated
   formats = {"slf":"SERAFIN",
              "srf":"SERAFIN",
              "sel":"SERAFIN",
              "med":"MED",
              "vtk":"VTK",
              "unv":"UNV",
              "cgns":"CGNS"}
   # Define a parser for the program options
   parser = OptionParser("Usage: %prog input-file-name -o output-file-name [options]\n"+
         "Example: runStbtel.py coarse.slf -b coarse.cli -o coarse.med --debug\n"+
         "Where coarse.slf is the mesh in SERAFIN fornat,\n"+
         "      coarse.cli is the boundary conditions file and\n"+
         "      coarse.med the converted mesh in MED format.")
   # output name option
   parser.add_option("-o","--output-file",
             type="string",
             dest="outputFile",
             default="output.med",
             help="name of the output file also defines the output format")
   # the boundary file option
   parser.add_option("-b","--boundary-file",
             type="string",
             dest="boundaryFile",
             default="",
             help="name of the output file")
   # the log file option
   parser.add_option("-l","--log-file",
             type="string",
             dest="logFile",
             default="",
             help="name of the log file")
   # option for converting distributed mesh
   parser.add_option("-n","--ndomains",
             type="int",
             dest="ndomains",
             default=1,
             help="number of sub-domains of the distributed mesh")
   # the silent option define if display stbtel informations
   parser.add_option("-s","--silent",
             action="store_true",
             dest="silent",
             default=False,
             help="disable stbtel output informations")
   # specail option to read/write selafin with bouble precision
   parser.add_option("","--selafin-double-precision",
             action="store_true",
             dest="isdble",
             default=False,
             help="state that the selafin file is in double precision")
   # the debug mode option
   parser.add_option("","--debug",
             action="store_true",
             dest="debug",
             default=False,
             help="Enable debug mode which displays more informations during run time")
   
   # reading the options
   options, args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Identifying input and output informations ~~~~~~~~~~~~~~~~~~~
   if len(args) < 1:
       print("\nMissing input file\n")
       parser.print_help()
       sys.exit(-1)
   # Getting input and output file names
   inputFile = args[0]
   outputFile = options.outputFile
   # Finding the input format by checking the extension
   n = len(inputFile)
   i = inputFile.find('.')
   inputExtension = inputFile[i+1:n]
   if not inputExtension in formats.keys():
        print("The input file extension is unknown")
        print("Known associations :")
        print(formats)
        sys.exit(-1)
   inputFormat = formats[inputExtension]
   # Finding the output format
   n = len(outputFile)
   i = outputFile.find('.')
   outputExtension = outputFile[i+1:n]
   if not outputExtension in formats.keys():
        print("The output file extension is unknown")
        print("Known associations :")
        print(formats)
        sys.exit(-1)
   outputFormat = formats[outputExtension]
   
   # if double precision parameter enable check that input or output is selafin
   if options.isdble:
     print("\n*** Working with double precision serafin ***\n")
     if not (outputFormat == 'SERAFIN' or inputFormat == 'SERAFIN'):
        print("serafin double precision parameter available only")
        print("if input or output format is selafin")
        sys.exit(-1)
   # Loop on the number of domains
   ndomains = options.ndomains
   for idom in range(0,ndomains):
      # build the extension added to each file of the distributed mesh by partel
      if ndomains == 1:
        # Nothing if we are dealling with a non-distributed mesh
        extens=''
      else:
          # the string of format 00000-00000
          extens=str(ndomains-1).zfill(5)+'-'+str(idom).zfill(5)
      # Writting the steering file
      fichier = open("stb"+extens+".cas","w")
      fichier.write("CONVERTER : YES\n")
      if options.debug:
          fichier.write("DEBUG : YES\n")
      fichier.write("INPUT FILE FORMAT : "+inputFormat+"\n")
      fichier.write("INPUT FILE : "+inputFile+extens+"\n")
      if options.boundaryFile != "":
          fichier.write("BOUNDARY FILE : "+options.boundaryFile+extens+"\n")
      if options.logFile != "":
          fichier.write("LOG FILE : "+options.logFile+extens+"\n")
      fichier.write("OUTPUT FILE FORMAT : "+outputFormat+"\n")
      fichier.write("OUTPUT FILE : "+outputFile+extens+"\n")
      if options.isdble:
          fichier.write("SELAFIN IN DOUBLE PRECISION : YES\n")
      # if 
      if outputFormat == "SERAFIN":
          fichier.write("OUTPUT BOUNDARY FILE : "+outputFile[0:(n-3)]+"cli"+extens+"\n")
      # If th output is in UNV format add the name of the lof file
      if outputFormat == "UNV":
          fichier.write("OUTPUT LOG FILE : "+outputFile[0:(n-3)]+"log"+extens+"\n")
      fichier.close()
      # Running stbtel
      system("stbtel.py stb"+extens+".cas")
      # Remove the case file
      remove("stb"+extens+".cas")
   
   sys.exit()
