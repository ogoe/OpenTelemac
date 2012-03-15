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
         Tools for handling SELAFIN files and TELEMAC binary related in python
"""
"""@details
         Contains a number of functions for binary (big-endian) SELAFIN files
         that used to be written in Fortran.
"""
"""@history 07/01/2012 -- Sebastien E. Bourban:
         Implementation of the SELAFIN class and of the putSLF function.
         A number of methods are available to the SELAFIN object to modify
            or transform its content, which are then called upon in putSLF.
"""
"""@history 09/01/2012 -- Sebastien E. Bourban:
         Re-Implementation of the what was a series of Fortran programs used
         to modify, extract and calculate stuff based on the SELAFIN format.
         These include:
         - scan (just writes information about the SLF content on screen)
         - chop (chop the time profile and the variables to a sub-number)
         - merge (merge 2 results into one file, either alining time or
           pulling variables together)
"""
"""@history 01/02/2012 -- Michelle Woodward, Laure C. Grignon and Sebastien E. Bourban:
         Addition of a new SELAFIN class to add calculated variables to a
         existing SELAFIN file.
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
import numpy as np
# ~~> dependencies towards other modules
from config import OptionParser
# ~~> dependencies towards other modules
from parsers.parserSELAFIN import SELAFIN,SELAFINS,putHeaderSLF,appendCoreTimeSLF,appendCoreVarsSLF,getVariablesAt,subsetVariablesSLF
from parsers.parserFortran import cleanQuotes
from parsers.parserLQD import LQD
from utils.files import moveFile
# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                    _________________________________________
# ____/ Secondary Classes /________________________________________/
#
class chopSELAFIN(SELAFIN):

   chopFrom = 0; chopStep = 1; chopStop = -1

   def __init__(self,f, times=None,vars=None):
      SELAFIN.__init__(self,f)
      if vars != None: self.updateVARS(vars)
      if times != None:
         tfrom,tstep,tstop = times
         self.updateTIMES(tfrom,tstep,tstop)

   def updateVARS(self,vars):
      ALLVARS = self.VARNAMES; ALLVARS.extend(self.CLDNAMES)
      ALLUNITS = self.VARUNITS; ALLUNITS.extend(self.CLDUNITS)
      self.VARINDEX = subsetVariablesSLF(vars,ALLVARS)[0]
      NBV1 = 0; VARNAMES = []; VARUNITS = []; NBV2 = 0; CLDNAMES = []; CLDUNITS = []
      for i in self.VARINDEX:
         if i <= self.NBV1:
            NBV1 += 1
            VARNAMES.append(ALLVARS[i])
            VARUNITS.append(ALLUNITS[i])
         else:
            NBV2 += 1
            VARNAMES.append(ALLVARS[i])
            VARUNITS.append(ALLUNITS[i])
      self.NBV1 = NBV1; self.VARNAMES = VARNAMES; self.VARUNITS = VARUNITS
      self.NBV2 = NBV2; self.CLDNAMES = CLDNAMES; self.CLDUNITS = CLDUNITS

   def updateTIMES(self,tfrom,tstep,tstop): # /!\ starts from 0!
      if tfrom > 0: tfrom -= 1
      if tstop > 0: tstop -= 1
      ltime = len(self.tags['times'])
      if tstop < 0: tstop = max(0,ltime+tstop)
      if tfrom < 0: tfrom = max(0,ltime+tfrom)
      tfrom = min(ltime-1,tfrom)             # /!\ you could stop the update here
      tstop = max(min(ltime-1,tstop),tfrom)  # /!\ you could stop the update here
      self.chopFrom = tfrom
      self.chopStep = max(1,tstep)
      self.chopStop = tstop
      ATt = []; ATs = []
      for t in range(ltime)[self.chopFrom:self.chopStop+1:self.chopStep]:
         ATs.append(self.tags['times'][t])
         ATt.append(self.tags['cores'][t])
      self.tags['times'] = np.asarray(ATs)
      self.tags['cores'] = ATt

class scanSELAFIN(SELAFIN,chopSELAFIN):

   def __init__(self,f, times=None,vars=None):
      SELAFIN.__init__(self,f)
      if vars != None: chopSELAFIN.updateVARS(self,vars)
      if times != None:
         tfrom,tstep,tstop = times
         chopSELAFIN.updateTIMES(self,tfrom,tstep,tstop)

   def printHeader(self):
      print "TITLE        :    <" + self.TITLE + ">"
      if self.IPARAM[9] == 1:    # /!\ needs proper formatting
         print "DATE / TIME  : ", \
         self.DATETIME[2],"-",self.DATETIME[1],"-",self.DATETIME[0]," ", \
         self.DATETIME[3],":",self.DATETIME[4],":",self.DATETIME[5]
      if len(self.VARNAMES) > 0: print "VARIABLES    :\n   - " + "\n   - ".join(v+u for v,u in zip(self.VARNAMES,self.VARUNITS))
      if len(self.CLDNAMES) > 0: print "CLANDESTINES :\n   - " + "\n   - ".join(v+u for v,u in zip(self.CLDNAMES,self.CLDUNITS))
      print "NUMBERs      :"
      print "   - NPLAN* = ",self.IPARAM[6],"\n   - NPTFR* = ",self.IPARAM[7],"\n   - IFACE* = ",self.IPARAM[8]
      print "   - NELEM3 = ",self.NELEM3,"\n   - NPOIN3 = ",self.NPOIN3,"\n   - NDP    = ",self.NDP,"\n   - NPLAN  = ",self.NPLAN
      print "MESH         : / min: [ ",np.min(self.MESHX),";",np.min(self.MESHY),"]  / max: [ ",np.max(self.MESHX),";",np.max(self.MESHY),"]"

   def printCore(self):
      for v in range(self.NBV1):
         print "VARIABLE     : ",self.VARNAMES[v]
         for t in range(len(self.tags['times'])):
            VARSOR = getVariablesAt( self.file,self.tags,t,self.NVAR,self.NPOIN3,[self.VARINDEX[v]] )
            print "    / TIME: ",self.tags['times'][t],"/ min:",np.min(VARSOR[0]),"/ max:",np.max(VARSOR[0])
      for v in range(self.NBV2):
         print "CLANDESTINE  : ",self.CLDNAMES[v]
         for t in range(len(self.tags['times'])):
            VARSOR = getVariablesAt( self.file,self.tags,t,self.NVAR,self.NPOIN3,[self.VARINDEX[v+self.NBV1]] )
            print "    / TIME: ",self.tags['times'][t],"/ min:",np.min(VARSOR[0]),"/ max:",np.max(VARSOR[0])

   def printTimeSummary(self):
      print "NUMBER OF TIMES : ",len(self.tags['times'])
      print "FIRST TIME STEP : ",self.tags['times'][0]
      print "LAST TIME STEP  : ",self.tags['times'][len(self.tags['times'])-1]
      if len(self.tags['times']) > 1: print "TIME STEP       : ", \
         ( self.tags['times'][len(self.tags['times'])-1] - self.tags['times'][0] )/( len(self.tags['times'])-1 )
      else: print "ONLY ONE TIME FRAME"

class alterSELAFIN(SELAFIN,chopSELAFIN):

   alterZp = 0; alterZm = 1; alterZnames = []

   def __init__(self,f, times=None,vars=None):
      SELAFIN.__init__(self,f)
      if vars != None: chopSELAFIN.updateVARS(self,vars)
      if times != None:
         tfrom,tstep,tstop = times
         chopSELAFIN.updateTIMES(self,tfrom,tstep,tstop)

   def updateTITLE(self,title=None):
      if title != None: self.TITLE = ( title + 80*' ' )[0:80]

   def updateDATETIME(self,date=None,time=None):
      if date != None:
         self.DATETIME[0],self.DATETIME[1],self.DATETIME[2] = int(date[2]),int(date[1]),int(date[0])
         self.IPARAM[9] = 1
      if time != None:
         self.DATETIME[3] = int(time[0])
         if len(time) > 1: self.DATETIME[4] = int(time[1])
         if len(time) > 2: self.DATETIME[5] = int(time[2])
         self.IPARAM[9] = 1

   def updateVARS(self,vars=None):
      if vars != None:
         for vn in vars.split(';'):
            v,n = vn.split('=')
            for iv in range(len(self.VARNAMES)):
               if v.lower() in self.VARNAMES[iv].lower(): self.VARNAMES[iv] = n.upper()
            for iv in range(len(self.CLDNAMES)):
               if v.lower() in self.CLDNAMES[iv].lower(): self.CLDNAMES[iv] = n.upper()

   def switchVARS(self):
      x = self.VARINDEX[0:self.NBV1]
      self.VARINDEX = self.VARINDEX[self.NBV1:]
      self.VARINDEX.extend(x)
      x = self.NBV1
      self.NBV1 = self.NBV2; self.NBV2 = x
      x = self.VARNAMES
      self.VARNAMES = self.CLDNAMES; self.CLDNAMES = x
      x = self.VARUNITS
      self.VARUNITS = self.CLDUNITS; self.CLDUNITS = x

   def updateMESH(self,mX=1,pX=0,mY=1,pY=0):
      self.MESHX = mX * self.MESHX + pX
      self.MESHY = mY * self.MESHY + pY

   def updateTIMES(self,mT=1,pT=0):
      self.tags['times'] = mT * self.tags['times'] + pT

   def updateVALUES(self,vars=None,mZ=1,pZ=0):
      if vars != None:
         self.alterZm = mZ; self.alterZp = pZ; self.alterZnames = vars.split(';')

   def getVALUES(self,t):
      VARSOR = SELAFIN.getVALUES(self,t)
      for v in self.alterZnames:
         for iv in range(len(self.VARNAMES)):
            if v.lower() in self.VARNAMES[iv].lower(): VARSOR[iv] = self.alterZm * VARSOR[iv] + self.alterZp
         for iv in range(len(self.CLDNAMES)):
            if v.lower() in self.CLDNAMES[iv].lower(): VARSOR[iv+slf.NBV1] = self.alterZm * VARSOR[iv+slf.NBV1] + self.alterZp
      return VARSOR

class calcsSELAFIN(SELAFIN):

   def __init__(self,f, times=None,vars=None):
      SELAFIN.__init__(self,f)
      self.calcs = []

   def calcWaterDepth(self):
      self.VARNAMES.append("WATER DEPTH     ")
      self.VARUNITS.append("M               ")
      self.NBV1 += 1
      args = subsetVariablesSLF("FREE SURFACE;BOTTOM",self.VARNAMES)[0]
      def calc(vars,ivars): return [vars[ivars[0]]-vars[ivars[1]]]
      self.calcs.append( [ calc, args ] )

   def calcKineticEnergy(self):
      self.VARNAMES.append("KINETIC ENERGY  ")
      self.VARUNITS.append("?               ")
      self.NBV1 += 1
      args = subsetVariablesSLF("VELOCITY U;VELOCITY V",self.VARNAMES)[0]
      def calc(vars,ivars): return [np.power( (np.power(vars[ivars[0]],2)+np.power(vars[ivars[1]],2) ),(3.0/2.0) )]
      self.calcs.append( [ calc, args ] )

   def putContent(self,fileName):
      self.fole = open(fileName,'wb')
      putHeaderSLF(self)
      for t in range(len(self.tags['times'])):
         appendCoreTimeSLF(self,t)
         vars = self.getVALUES(t)
         appendCoreVarsSLF(self,vars)
         for fct,args in self.calcs: appendCoreVarsSLF(self,fct(vars,args))
      self.fole.close()

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$09-Jan-2012 08:51:29$"

""" Options for each code name
      scan [--core] *.slf [*.slf]
      + '--core': print statistics on all variables, for each time step with
                 the core of all SELAFIN file present in args
      chop [--from] [--step] [--stop] in.slf out.slf
      + '--from': first frame included in out.slf
      + '--step': step used to extract the appropriate frame for out.slf
      + '--stop': last frame included in out.slf (unless step jumps over it)
        frame numbers (from and stop) being numbered from 0
      + '--vars': list those variables that are being extracted (all if empty)
      + '--replace': replace the input file by the output file, in which case
        multiple input files can be used
      alter [--title] [--date] [--time] ...
"""

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   # valid for scan, chop and alter
   parser.add_option("-v", "--vars",type="string",dest="xvars",default=None,help="specify which variables should remain (';'-delimited)" )
   # valid for scan and chop
   parser.add_option("-c","--core",action="store_true",dest="core",default=False,help="scan: specify whether to print statistics on the core variables" )
   parser.add_option("-f", "--from",type="string",dest="tfrom",default="1",help="chop: specify the first frame included" )
   parser.add_option("-s", "--stop",type="string",dest="tstop",default="-1",help="chop: specify the last frame included (negative from the end)" )
   parser.add_option("-d", "--step",type="string",dest="tstep",default="1",help="specify the step for the extraction of frames" )
   # valid for chop, alter and merge
   parser.add_option("-r", "--replace",action="store_true",dest="freplace",default=False,help="if present, the output file will eventualy replace the input file" )
   # valid for alter
   parser.add_option("--title",type="string",dest="atitle",default=None,help="set the title of the SLF" )
   parser.add_option("--reset",action="store_true",dest="areset",default=False,help="reset AT to zero second" )
   parser.add_option("--date",type="string",dest="adate",default=None,help="set the start date of the SLF (dd-mm-yyyy)" )
   parser.add_option("--time",type="string",dest="atime",default=None,help="set the start time of the SLF (hh:mm:ss)" )
   parser.add_option("--switch",action="store_true",dest="aswitch",default=False,help="switch between VARIABLES and CLANDESTINES" )
   parser.add_option("--name",type="string",dest="aname",default=None,help="change the name of a VARIABLE: 'OLD VAR=NEW VAR'" )
   parser.add_option("--T+?",type="string",dest="atp",default="0",help="adds to the ATs" )
   parser.add_option("--T*?",type="string",dest="atm",default="1",help="scales the ATs" )
   parser.add_option("--X+?",type="string",dest="axp",default="0",help="adds to the MESHX" )
   parser.add_option("--X*?",type="string",dest="axm",default="1",help="scales the MESHX" )
   parser.add_option("--Y+?",type="string",dest="ayp",default="0",help="adds to the MESHY" )
   parser.add_option("--Y*?",type="string",dest="aym",default="1",help="scales the MESHY" )
   parser.add_option("--Z?",type="string",dest="azname",default=None,help="will filter Z+ znd Z* operations on that VARIABLE name" )
   parser.add_option("--Z+?",type="string",dest="azp",default="0",help="adds to the VARIABLE" )
   parser.add_option("--Z*?",type="string",dest="azm",default="1",help="scales the VARIABLE" )
   # valid for merge

   options, args = parser.parse_args()
   if len(args) < 1:
      print '\nThe name of one SELAFIN file at least is required\n'
      parser.print_help()
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = args[0]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of SCAN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if codeName == 'scan':

      slfFiles = args[1:]
      for slfFile in slfFiles:

         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         print '\n\nScanning ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         vars = options.xvars
         if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
         slf = scanSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars )
         slf.printHeader()
         if options.core: slf.printCore()
         else: slf.printTimeSummary()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of CHOP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'chop':

      if not options.freplace:
         if len(args) != 3:
            print '\nThe code "chop" (without --replace) uses a minimum of 2 argumensts, aside from the options\n'
            parser.print_help()
            sys.exit()
         slfFiles = [ args[1] ]
         outFile = args[2]
      else:
         slfFiles = args[1:]
         outFile = "chop-tmp.slf"

      for slfFile in slfFiles:

         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         print '\n\nChoping ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         vars = options.xvars
         if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
         slf = chopSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars )

         slf.putContent( outFile )

         if options.freplace: moveFile(outFile,slfFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of ALTER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'alter':

      if not options.freplace:
         if len(args) != 3:
            print '\nThe code "alter" (without --replace) uses a minimum of 2 argumensts, aside from the options\n'
            parser.print_help()
            sys.exit()
         slfFiles = [ args[1] ]
         outFile = args[2]
      else:
         slfFiles = args[1:]
         outFile = "chop-tmp.slf"

      for slfFile in slfFiles:

         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         print '\n\nAltering ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         vars = options.xvars
         if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
         slf = alterSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars )
         if options.atitle != None: slf.updateTITLE(options.atitle)
         if options.areset: slf.updateTIMES(pT=-slf.tags['times'][0])
         if options.adate != None: slf.updateDATETIME(date=options.adate.split('-'))
         if options.atime != None: slf.updateDATETIME(time=options.atime.split(':'))
         if options.aswitch: slf.switchVARS()
         if options.aname != None: slf.updateVARS( options.aname )
         slf.updateTIMES( mT=float(options.atm),pT=float(options.atp) )
         slf.updateMESH( mX=float(options.axm),pX=float(options.axp),mY=float(options.aym),pY=float(options.ayp) )
         if options.azname != None: slf.updateVALUES( options.azname, mZ=float(options.azm),pZ=float(options.azp) )

         slf.putContent( outFile )
         
         if options.freplace: moveFile(outFile,slfFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of MERGE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'merge':

      if len(args) < 3:
         print '\nThe code "merge" uses a minimum of 2 argumensts, aside from the options\n'
         parser.print_help()
         sys.exit()

      slfFiles = args[1:len(args)-1]
      outFile = args[len(args)-1]
      
      slfs = SELAFINS()
      print '\n\nMerging into ' + path.basename(outFile) + ' within ' + path.dirname(outFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      for slfFile in slfFiles:
         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         slfs.add( slfFile )

      slfs.putContent(outFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of DIFF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'diff':

      if len(args) < 3:
         print '\nThe code "diff" uses a minimum of 3 argumensts, aside from the options\n'
         parser.print_help()
         sys.exit()

      slfFiles = args[1:len(args)-1]
      print slfFiles
      outFile = args[len(args)-1]
      print outFile

      slfs = SELAFINS()
      print '\n\nDifferences into ' + path.basename(outFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      for slfFile in slfFiles:
         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         slfs.add( slfFile )

      slfs.putContent(outFile)


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of SAMPLE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'sample':
      if len(args) < 3:
         print '\nThe code "sample" uses a minimum of 3 argumensts, aside from the options\n'
         parser.print_help()
         sys.exit()

      slfFile = args[1]
      outFile = args[2]
      nodList = []
      for nod in args[3].split(): nodList.append(int(nod))

      slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
      print '\n\nChoping ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
      vars = options.xvars
      if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
      slf = chopSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars )

      lqd = LQD( vars=[zip(slf.VARNAMES,slf.VARUNITS),nodList], date=slf.DATETIME, times=slf.tags['times'], series=slf.getSERIES(nodList) )
      lqd.putContent( outFile )

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of UNKNOWN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print '\nDo not know what to do with this code name: ',codeName
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
