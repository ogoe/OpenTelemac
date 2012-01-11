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
         Tools for handling Kenue native files in python.
         Kenue and its related software (Blue Kenue, Greem Kenue, )
         are property of the NRC Canadian Hydrualics Centre
"""
"""@details
         Contains getI2S, getI3S and putI2S, putI3S, which read/write
         python variables into ASCII I2S and I3S files
"""
"""@history 26/12/2011 -- Sebastien E. Bourban:
         First trial at parsing I2S and I3S
"""
"""@history 09/01/2012 -- Sebastien E. Bourban:
         Addition of XY and XYZ parsing
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import re
import numpy as np
from os import path
# ~~> dependencies towards other pytel/modules
from utils.files import getFileContent,putFileContent
from utils.progressbar import ProgressBar

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#
ken_header = re.compile(r'[#:]')

asc_FileType = re.compile(r':FileType\s(?P<type>\b\w\w\w\b)') #\s(?P<after>[^\Z]*)\Z')

var_integer = re.compile(r'(?P<number>\b\d+\b)(?P<after>[^\Z]*)\Z')
var_doublep = re.compile(r'(?P<number>\b(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+)(\b|[^a-zA-Z,)])))(?P<after>[^\Z]*)\Z')

rint = r'\b\d+\b'
var_1int = re.compile(r'\s(?P<number>'+rint+r')\s')
rdbl = r'[+-]*\b((?:(\d+)\b)|(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+)))'
var_1dbl = re.compile(r'\s*(?P<number>'+rdbl+r')\s*')
var_2dbl = re.compile(r'\s*(?P<number1>'+rdbl+r')'+r'[\s,;]*(?P<number2>'+rdbl+r')\s*')
var_3dbl = re.compile(r'\s*(?P<number1>'+rdbl+r')'+r'[\s,;]*(?P<number2>'+rdbl+r')'+r'[\s,;]*(?P<number3>'+rdbl+r')\s*')

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def cleanSpaces(istr): # same as in parserFortran
   return istr.strip().replace('  ',' ').replace('  ',' ')

# _____                      _______________________________________
# ____/ Toolbox for I2S/I3S /______________________________________/
#

def getInS(file):
   # TODO: Read the whole header, for the time being head is copied
   #       over
   # TODO: Read variables depending on type and on a list

   # ~~ Get all ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   core = getFileContent(file)

   # ~~ Parse head ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   icore = 0
   while re.match(ken_header,core[icore]):
      # ~~> instruction FileType
      proc = re.match(asc_FileType,core[icore])
      if proc: fileType = proc.group('type').lower()
      # ... more instruction coming ...
      icore += 1
   head = core[0:icore]
   # /!\ icore starts the body

   # ~~ Parse body ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # This is also fairly fast, so you might not need a progress bar
   poly = []; type = [] #; pbar = ProgressBar(maxval=len(core)).start()
   while icore < len(core):
      # ~~> polygon head
      proc = re.match(var_1int,core[icore])
      if not proc:
         print '\nCould not parse the following polyline header: '+core[icore]
         sys.exit()
      nrec = int(proc.group('number')); icore += 1 #; pbar.update(icore)
      xyi = []
      for irec in range(nrec):
         proc = re.match(var_2dbl,core[icore+irec]+' ')
         if not proc:
            print '\nCould not parse the following polyline record: '+core[icore+irec+1]
            sys.exit()
         xyi.append([float(proc.group('number1')),float(proc.group('number2'))])
      if xyi != []:
         accuracy = np.power(10.0, -5+np.floor(np.log10(xyi[0][0]+xyi[0][1])))
         distance = np.sqrt( ( np.power((xyi[0][0]-xyi[len(xyi)-1][0]),2)+np.power((xyi[0][1]-xyi[len(xyi)-1][1]),2) ) )
         cls = False
         if accuracy > distance :
            xyi.pop()
            cls = True
         poly.append(xyi)
         type.append(cls)
      icore += nrec
   #pbar.finish()

   return head,fileType,poly,type

def putInS(file,head,fileType,poly,type):

   # ~~ Write head ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if head != []: core = head
   else: core = [':FileType '+fileType+' ASCII EnSim 1.0',
      ':Application BlueKenue', ':Version 3.2.24',
      ':WrittenBy sebourban', ':CreationDate Thu, Dec 08, 2011 02:47 PM',
      ':Name ' + path.basename(file),
      ':AttributeUnits 1 m',
      ':EndHeader' ]

   # ~~ Write body ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for ip,it in zip(poly,type):
      il = len(ip)
      if it: il += 1
      core.append(str(il)+' 0')  #TODO: you should use proper values
      if fileType == 'i2s':
         for xyi in ip: core.append(str(xyi[0])+' '+str(xyi[1]))
         if it: core.append(str(ip[0][0])+' '+str(ip[0][1]))
      elif fileType == 'i3s':
         for xyi in ip: core.append(str(xyi[0])+' '+str(xyi[1])+' '+str(xyi[2]))
         if it: core.append(str(ip[0][0])+' '+str(ip[0][1])+' '+str(ip[0][2]))

   # ~~ Put all ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent(file,core)

   return

# _____                  ___________________________________________
# ____/ Toolbox for XYZ /__________________________________________/
#

def getXYn(file):
   # TODO: Read the whole header, for the time being head is copied
   #       over
   # TODO: Read multiple variables depending on type and on a list

   # ~~ Get all ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   core = getFileContent(file)

   # ~~ Parse head ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   icore = 0; fileType = None
   while re.match(ken_header,core[icore]):
      # ~~> instruction FileType
      proc = re.match(asc_FileType,core[icore])
      if proc: fileType = proc.group('type').lower()
      # ... more instruction coming ...
      icore += 1
   head = core[0:icore]
   if fileType == None:
      proc = re.match(var_3dbl,core[icore]+' ')
      if not proc:
         proc = re.match(var_2dbl,core[icore]+' ')
         if not proc:
            print '\nCould not parse the first record: '+core[icore]
            sys.exit()
         else: fileType = 'xy'
      else: fileType = 'xyz'

   # /!\ icore starts the body

   # ~~ Parse body ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # This is also fairly fast, so you might not need a progress bar
   xyz = [] #; pbar = ProgressBar(maxval=len(core)).start()
   while icore < len(core):
      if fileType == 'xy':
         proc = re.match(var_2dbl,core[icore]+' ')
         if not proc:
            print '\nCould not parse the following xyz record: '+core[icore]
            sys.exit()
         xyz.append([float(proc.group('number1')),float(proc.group('number2'))])
      elif fileType == 'xyz':
         proc = re.match(var_3dbl,core[icore]+' ')
         if not proc:
            print '\nCould not parse the following xyz record: '+core[icore]
            sys.exit()
         xyz.append([float(proc.group('number1')),float(proc.group('number2')),float(proc.group('number3'))])
      icore += 1
   #pbar.finish()

   return head,fileType,xyz

def putXYn(file,head,fileType,xyz):

   # ~~ Write head ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   core = head
   #if head != []: core = head
   #<else: core = [':FileType '+fileType+' ASCII EnSim 1.0',
   #   ':Application BlueKenue', ':Version 3.2.24',
   #   ':WrittenBy sebourban', ':CreationDate Thu, Dec 08, 2011 02:47 PM',
   #   ':Name ' + path.basename(file),
   #   ':AttributeUnits 1 m',
   #   ':EndHeader' ]

   # ~~ Write body ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if fileType == 'xy':
      for i in xyz: core.append(str(xyz[0])+' '+str(xyz[1]))
   elif fileType == 'xyz':
      for i in xyz: core.append(str(xyz[0])+' '+str(xyz[1])+' '+str(xyz[2]))

   # ~~ Put all ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent(file,core)

   return
