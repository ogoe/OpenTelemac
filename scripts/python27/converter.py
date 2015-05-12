#!/usr/bin/env python
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
"""@brief Run a converions of mesh files using stbtel 
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import os
import subprocess as sp
from optparse import OptionParser

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Yoann Audouin"
__date__ ="$21-Sep-2012 16:51:09$"


casCanvas = \
"""
/
/ CONVERSION OF MESH FILE USING STBTEL
/
CONVERTER = YES
DEBUG = {debug}
/
/ INPUT FILE INFORMATION
/
INPUT FILE FORMAT : '{inputFormat}'
INPUT FILE : '{inputFile}'
BOUNDARY CONDITION IN SERAFIN FORMAT : {srfBnd}
{inAdditionalFile}
/
/ OUTPUT FILE INFORMATION
/
OUTPUT FILE FORMAT : '{outputFormat}'
OUTPUT FILE : '{outputFile}'
{outAdditionalFile}
"""

def build_cas(options,extens,inputFormat,inputFile,outputFormat,outputFile):
   """
   Build the steering file for stbtel
   """
   # Building canvas for steering file
   debug = 'YES' if options.debug else 'NO'
   srfBnd = 'YES' if options.srfBnd else 'NO'
   # Additional files
   ## input files
   inAdditionalFile = ''
   if options.boundaryFile:
      inAdditionalFile += "BOUNDARY FILE : '%s' \n" % (options.boundaryFile + extens)
   if options.logFile:
      inAdditionalFile += "LOG FILE : '%s' \n" % (options.logFile + extens)
   ## Output files
   outAdditionalFile = ''
   if outputFormat == "SERAFIN":
      outAdditionalFile += "OUTPUT BOUNDARY FILE : '%s'\n" % (outputFile[:-3] + 'cli' + extens)
   # If th output is in UNV format add the name of the log file
   if outputFormat == "UNV":
      outAdditionalFile += "OUTPUT LOG FILE : '%s'\n" % (outputFile[:-3]+'log'+extens)
   
   return casCanvas.format(
             debug=debug,  
             inputFormat=inputFormat,
             inputFile=inputFile+extens, 
             srfBnd=srfBnd, 
             inAdditionalFile=inAdditionalFile, 
             outputFormat=outputFormat,
             outputFile=outputFile+extens, 
             outAdditionalFile=outAdditionalFile 
             )

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
   parser.add_option("", "--output-boundary-file",
             type="string",
             dest="outBoundaryFile",
             default="",
             help="name of the output boundary file")
   # the boundary file option
   parser.add_option("-b","--boundary-file",
             type="string",
             dest="boundaryFile",
             default="",
             help="name of the boundary file")
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
   # Option to tell stbtel to read the boundary conidtion from the boundary file
   parser.add_option("","--srf-bnd",
             action="store_true",
             dest="srfBnd",
             default=False,
             help="tell stbtel to read the boundary conidtion from the boundary file")
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
   # root directory
   parser.add_option("-r", "--rootdir",
                     type = "string",
                     dest = "rootDir",
                     default = None,
                     help="specify the root, default is taken from config file")

   # reading the options
   options, args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Identifying input and output informations ~~~~~~~~~~~~~~~~~~~
   if len(args) < 1:
      parser.print_help()
      parser.error("Missing input file\n")
   # Getting input and output file names
   inputFile = args[0]
   outputFile = options.outputFile
   if not options.inputFormat:
      # Finding the input format by checking the extension
      i = inputFile.rfind('.')
      inputExtension = inputFile[i+1:]
      if not inputFile.endswith(tuple(formats.keys())):
         parser.error("The input file extension is unknown\n Known are : %r"
                      % formats)
      else:
         inputFormat = formats[inputExtension]
   else:
      inputFormat = options.inputFormat
   # If no format was given as option try to indentify format from extension
   if not options.outputFormat:
      # Finding the output format
      i = outputFile.rfind('.')
      outputExtension = outputFile[i+1:]
      if not outputFile.endswith(tuple(formats.keys())):
         parser.error("The output file extension is unknown\n Known are : %r"
                      % formats)
      else:
         outputFormat = formats[outputExtension]
   else:
      outputFormat = options.outputFormat
   
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
     
      cas = build_cas(options, extens, inputFormat, inputFile, 
                      outputFormat, outputFile)
      # Writting the steering file
      casName = 'stb'+extens+".cas"
      with open(casName,"w") as fobj:
         fobj.write(cas)
      # Running stbtel
      path_stbtel = "stbtel.py"
      if options.rootDir is not None:
        path_stbtel = os.path.join(options.rootDir, "scripts", "python27", path_stbtel)
      stbtel_args = [path_stbtel, casName]
      if options.rootDir is not None:
         stbtel_args += ["-r", options.rootDir]
      print "Calling:", " ".join(stbtel_args)
      rc = sp.call(stbtel_args)

      
      if rc != 0:
         sys.exit(rc)
      else:
         # Remove the case file
         os.remove(casName)


   print '\n\n'+'~'*72
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
