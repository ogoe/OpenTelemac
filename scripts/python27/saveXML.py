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
   Simpler tools for storing save items from XML files directly
      (as opposed to through the validateTELEMAC.py)
"""
"""@details
"""
"""@history 30/09/2014 -- Sebastien E. Bourban:
         Initial implementation.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
# ~~> dependencies towards other modules
from config import OptionParser
# ~~> dependencies towards other modules
from utils.messages import MESSAGES,filterMessage
from parsers.parserXML import runXML
# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$30-Sep-2014 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-b","--bypass",action="store_true",dest="bypass",default=False,
      help="will bypass execution failures and try to carry on (final report at the end)" )
   parser.add_option("--clean",action="store_true",dest="cleanup",default=False,
      help="will erase all object, executable, result files from subfolders on the selected configs/modules" )

   options, args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~ Manage options to run only ranked actions ~~~~~~~~~~~~~~~~~~~~
#
   # rank = 0 can be divided by all prime numbers
   options.todos = { 'save':{'rank':0,'todo':''},
      'act':{'rank':-1,'todo':''},'draw':{'rank':-1,'todo':''},'cast':{'rank':-1,'todo':''} }

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Trying to fit it with validateTELEMAC ~~~~~~~~~~~~~~~~~~~~~~~
   cfg = { 'REBUILD':0 }
   if options.cleanup: cfg.update({ 'REBUILD':2 })
   options.display = False
   xmls = { 'xml':{ 'cfg':cfg, 'options':options }}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()

   if args != []:
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Running the XML commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for xmlFile in args:
         print '\nFocused validation on ' + xmlFile + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         if not path.isfile(xmlFile):
            print '\nNot able to find your XML file: ' + xmlFile
            print ' ... maybe something wrong with your command line'
            sys.exit(1)
         try:
            print xmls['xml']['cfg'].keys()
            runXML(path.realpath(xmlFile),xmls,[],options.bypass)
         except Exception as e:
            mes = filterMessage({'name':'runXML::main:\n      '+path.dirname(xmlFile)},e,options.bypass)
            xcpts.addMessages([mes])

   else:
      print '\nThis program requires at least one XMP file to play with ...'
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty():
      print '\n\nHummm ... I could not complete my work.\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n' \
      + xcpts.exceptMessages()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else: print '\n\nMy work is done\n\n'
   
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
   sys.exit(0)
