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
from os import remove
import subprocess as sp
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
   # output fomrat
   parser.add_option("","--input-format",
             type="string",
             dest="inputFormat",
             default="",
             help="name of the input format, overwrites input detected by extension")
   # output fomrat
   parser.add_option("","--output-format",
             type="string",
             dest="outputFormat",
             default="",
             help="name of the output format, overwrites output detected by extension")
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
     sys.exit(1)
   # Getting input and output file names
   inputFile = args[0]
   outputFile = options.outputFile
   if options.inputFormat == "":
     # Finding the input format by checking the extension
     n = len(inputFile)
     i = inputFile.find('.')
     inputExtension = inputFile[i+1:n]
     if not inputExtension in formats:
       print("The input file extension is unknown")
       print("Known associations :")
       print(formats)
       sys.exit(1)
     inputFormat = formats[inputExtension]
   if options.outputFormat == "":
     # Finding the output format
     n = len(outputFile)
     i = outputFile.find('.')
     outputExtension = outputFile[i+1:n]
     if not outputExtension in formats:
       print("The output file extension is unknown")
       print("Known associations :")
       print(formats)
       sys.exit(1)
     outputFormat = formats[outputExtension]
   
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
     fichier = open("stb%s.cas" % extens,"w")
     fichier.write("CONVERTER : 'YES'\n")
     if options.debug:
       fichier.write("DEBUG : 'YES'\n")
     fichier.write("INPUT FILE FORMAT : '%s'\n" % inputFormat)
     fichier.write("INPUT FILE : '%s%s'\n" % (inputFile, extens))
     if options.boundaryFile != "":
       fichier.write("BOUNDARY FILE : '%s%s'\n" % (options.boundaryFile, extens))
     if options.logFile != "":
       fichier.write("LOG FILE : '%s%s'\n" % (options.logFile, extens))
     fichier.write("OUTPUT FILE FORMAT : '%s'\n" % outputFormat)
     fichier.write("OUTPUT FILE : '%s%s'\n" % (outputFile, extens))
     # if the output is serafin format we add the boundary file 
     if outputFormat == "SERAFIN":
       fichier.write("OUTPUT BOUNDARY FILE : '%scli%s'\n" % (outputFile[0:(n-3)], extens))
     # If th output is in UNV format add the name of the log file
     if outputFormat == "UNV":
       fichier.write("OUTPUT LOG FILE : '%slog%s'\n" % (outputFile[0:(n-3)], extens))
     fichier.close()
     # Running stbtel
     sp.call(["stbtel.py", "stb%s.cas" % extens])
     # Remove the case file
     remove("stb%s.cas" % extens)

   print '\n\n'+'~'*72
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
