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
"""@history 01/02/2012 -- Sebastien E. Bourban:
         Addition of the new code "spec" to read and print to screen the core content
         of a TOMAWAC SPECTRAL file.
"""
"""@history 05/04/2012 -- Sebastien E. Bourban:
         Addition of the new SELAFIN class to expand on calcs with the crunch.
         While calcsSELAFIN calculates a time varying variables, crunchSELAFIN
         calculates variables aggregated over time resulting in one time frame.
         Examples of calculations already implemented include: SURFACE RANGE ;
         TIME OF PEAK and SURFACE AT PEAK ; MAXIMUM SPEED ; and
         RESIDUAL VELOCITIES
"""
"""@history 05/04/2012 -- Sebastien E. Bourban:
         Additional capability implemented for all SLF objects when used
         to process multiple simulation results (when produced by TELEMAC
         in parallel mode). The input will be the root of all files, and
         the output is re-collected into one.
         Option --parallel is required to activate this feature.
         Note that doing operation on one sub-file only, to produce one sub-file
         is not covered here as it could be done already.
"""
"""@history 07/02/2013 -- Sebastien E. Bourban:
         Additional capability implemented for subdivision of the mesh of
         all SLF objects. A little bit like STBTEL but hopefully easier
         to use from the command line. Linear interpolation is used for
         new (in-between) points.
"""
"""@history 25/12/2016 -- Noemie Durand:
         Additional capability implemented for ARTEMIS simulations:
         Free surface elevation animations can now be produced
         over the model area.
         New class transfSELAFIN(PARAFINS,alterSELAFIN) implemented
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
import numpy as np
from argparse import ArgumentParser,RawDescriptionHelpFormatter
# ~~> dependencies towards other modules
# ~~> dependencies towards other modules
from parsers.parserSELAFIN import SELAFIN,SELAFINS,PARAFINS,subsetVariablesSLF
from parsers.parserFortran import cleanQuotes
from parsers.parserLQD import LQD
from parsers.parserKenue import InS
from utils.files import moveFile
from utils.progressbar import ProgressBar
from samplers.meshes import subdivideMesh4,subdivideMesh3,tessellatePoly
from converters import convertUTM as utm

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                    _________________________________________
# ____/ Secondary Classes /________________________________________/
#

class chopSELAFIN(PARAFINS):

   chopFrom = 0; chopStep = 1; chopStop = -1

   def __init__(self,f, times=None,vars=None,root=None):
      PARAFINS.__init__(self,f,root)
      if vars != None: self.updateVARS(vars)
      if times != None:
         tfrom,tstep,tstop = times
         self.updateTIMES(tfrom,tstep,tstop)

   def updateVARS(self,vars):
      ALLVARS = self.slf.VARNAMES; ALLVARS.extend(self.slf.CLDNAMES)
      ALLUNITS = self.slf.VARUNITS; ALLUNITS.extend(self.slf.CLDUNITS)
      self.slf.VARINDEX = subsetVariablesSLF(vars,ALLVARS)[0]
      NBV1 = 0; VARNAMES = []; VARUNITS = []; NBV2 = 0; CLDNAMES = []; CLDUNITS = []
      for i in self.slf.VARINDEX:
         if i <= self.slf.NBV1:
            NBV1 += 1
            VARNAMES.append(ALLVARS[i])
            VARUNITS.append(ALLUNITS[i])
         else:
            NBV2 += 1
            VARNAMES.append(ALLVARS[i])
            VARUNITS.append(ALLUNITS[i])
      self.slf.NBV1 = NBV1; self.slf.VARNAMES = VARNAMES; self.slf.VARUNITS = VARUNITS
      self.slf.NBV2 = NBV2; self.slf.CLDNAMES = CLDNAMES; self.slf.CLDUNITS = CLDUNITS

   def updateTIMES(self,tfrom,tstep,tstop): # /!\ starts from 0!
      if tfrom > 0: tfrom -= 1
      if tstop > 0: tstop -= 1
      ltime = len(self.slf.tags['times'])
      if tstop < 0: tstop = max(0,ltime+tstop)
      if tfrom < 0: tfrom = max(0,ltime+tfrom)
      tfrom = min(ltime-1,tfrom)             # /!\ you could stop the update here
      tstop = max(min(ltime-1,tstop),tfrom)  # /!\ you could stop the update here
      self.chopFrom = tfrom
      self.chopStep = max(1,tstep)
      self.chopStop = tstop
      ATt = []; ATs = []
      for t in range(ltime)[self.chopFrom:self.chopStop+1:self.chopStep]:
         ATs.append(self.slf.tags['times'][t])
         ATt.append(self.slf.tags['cores'][t])
      self.slf.tags['times'] = np.asarray(ATs)
      self.slf.tags['cores'] = ATt

class scanSELAFIN(PARAFINS,chopSELAFIN): # /!\ does not support PARAFINS yet -- because of the print order of print Core

   def __init__(self,f, times=None,vars=None):
      PARAFINS.__init__(self,f)
      if vars != None: chopSELAFIN.updateVARS(self,vars)
      if times != None:
         tfrom,tstep,tstop = times
         chopSELAFIN.updateTIMES(self,tfrom,tstep,tstop)

   def printHeader(self):
      if self.slf.file['endian'] == ">": print 'This file appears to be coded in "big endian"'
      else: print 'This SELAFIN file appears to be coded in "little endian"'
      if self.slf.file['float'] == ('f',4): print 5*' '+'and the floats are assumed to be SINGLE PRECISION\n'
      else: print 5*' '+'and the floats are assumed to be DOUBLE PRECISION\n'
      print "TITLE        :    <" + self.slf.TITLE + ">"
      if self.slf.IPARAM[9] == 1:    # /!\ needs proper formatting
         print "DATE / TIME  : ", \
         self.slf.DATETIME[2],"-",self.slf.DATETIME[1],"-",self.slf.DATETIME[0]," ", \
         self.slf.DATETIME[3],":",self.slf.DATETIME[4],":",self.slf.DATETIME[5]
      if len(self.slf.VARNAMES) > 0: print "VARIABLES    :\n   - " + "\n   - ".join(v+u for v,u in zip(self.slf.VARNAMES,self.slf.VARUNITS))
      if len(self.slf.CLDNAMES) > 0: print "CLANDESTINES :\n   - " + "\n   - ".join(v+u for v,u in zip(self.slf.CLDNAMES,self.slf.CLDUNITS))
      print "NUMBERs      :"
      print "   - NPLAN* = ",self.slf.IPARAM[6],"\n   - NPTFR* = ",self.slf.IPARAM[7],"\n   - IFACE* = ",self.slf.IPARAM[8]
      print "   - NELEM3 = ",self.slf.NELEM3,"\n   - NPOIN3 = ",self.slf.NPOIN3,"\n   - NDP3   = ",self.slf.NDP3,"\n   - NPLAN  = ",self.slf.NPLAN
      if self.slf.NPLAN > 1: print "   - NELEM2 = ",self.slf.NELEM2,"\n   - NPOIN2 = ",self.slf.NPOIN2,"\n   - NDP2   = ",self.slf.NDP2
      if self.slf.NPOIN2 > 0: print "MESH         : / min: [ ",np.min(self.slf.MESHX),";",np.min(self.slf.MESHY),"]  / max: [ ",np.max(self.slf.MESHX),";",np.max(self.slf.MESHY),"]"
      print "ARRAYs       :"
      print "   - IKLE  : / min: [ ",np.min(self.slf.IKLE3)+1,"]  / max: [ ",np.max(self.slf.IKLE3)+1,"]",self.slf.IKLE3+1
      print "   - IPOBO : / min: [ ",np.min(self.slf.IPOB3),"]  / max: [ ",np.max(self.slf.IPOB3),"]",self.slf.IPOB3

   def printCore(self):
      for v in range(self.slf.NBV1):
         print "VARIABLE     : ",self.slf.VARNAMES[v]
         for t in range(len(self.slf.tags['times'])):
            VARSOR = self.slf.getVariablesAt( t,[self.slf.VARINDEX[v]] )
            if self.slf.NPOIN2 > 0: print "    / TIME: ",self.slf.tags['times'][t],"/ min:",np.min(VARSOR[0]),"/ max:",np.max(VARSOR[0])
      for v in range(self.slf.NBV2):
         print "CLANDESTINE  : ",self.slf.CLDNAMES[v]
         for t in range(len(self.slf.tags['times'])):
            VARSOR = self.slf.getVariablesAt( t,[self.slf.VARINDEX[v+self.slf.NBV1]] )
            if self.slf.NPOIN2 > 0: print "    / TIME: ",self.slf.tags['times'][t],"/ min:",np.min(VARSOR[0]),"/ max:",np.max(VARSOR[0])

   def printTimeSummary(self):
      print "NUMBER OF TIMES : ",len(self.slf.tags['times'])
      print "FIRST TIME STEP : ",self.slf.tags['times'][0]
      print "LAST TIME STEP  : ",self.slf.tags['times'][len(self.slf.tags['times'])-1]
      if len(self.slf.tags['times']) > 1: print "TIME STEP       : ", \
         ( self.slf.tags['times'][len(self.slf.tags['times'])-1] - self.slf.tags['times'][0] )/( len(self.slf.tags['times'])-1 )
      else: print "ONLY ONE TIME FRAME"

class alterSELAFIN(PARAFINS,chopSELAFIN):

   alterZp = 0; alterZm = 1; alterZnames = []

   def __init__(self,f, times=None,vars=None,root=None):
      PARAFINS.__init__(self,f,root)
      if vars != None: chopSELAFIN.updateVARS(self,vars)
      if times != None:
         tfrom,tstep,tstop = times
         chopSELAFIN.updateTIMES(self,tfrom,tstep,tstop)

   def alterTITLE(self,title=None):
      if title != None: self.slf.TITLE = ( title + 80*' ' )[0:80]

   def alterDATETIME(self,date=None,time=None):
      if date != None:
         self.slf.DATETIME[0],self.slf.DATETIME[1],self.slf.DATETIME[2] = int(date[2]),int(date[1]),int(date[0])
         self.slf.IPARAM[9] = 1
      if time != None:
         self.slf.DATETIME[3] = int(time[0])
         if len(time) > 1: self.slf.DATETIME[4] = int(time[1])
         if len(time) > 2: self.slf.DATETIME[5] = int(time[2])
         self.slf.IPARAM[9] = 1

   def alterVARS(self,vars=None):
      if vars != None:
         for vn in vars.split(':'):
            v,n = vn.split('=')
            for iv in range(len(self.slf.VARNAMES)):
               if v.lower() in self.slf.VARNAMES[iv].lower(): self.slf.VARNAMES[iv] = n.upper()
            for iv in range(len(self.slf.CLDNAMES)):
               if v.lower() in self.slf.CLDNAMES[iv].lower(): self.slf.CLDNAMES[iv] = n.upper()

   def switchVARS(self):
      x = self.slf.VARINDEX[0:self.slf.NBV1]
      self.slf.VARINDEX = self.slf.VARINDEX[self.slf.NBV1:]
      self.slf.VARINDEX.extend(x)
      x = self.slf.NBV1
      self.slf.NBV1 = self.slf.NBV2; self.slf.NBV2 = x
      x = self.slf.VARNAMES
      self.slf.VARNAMES = self.slf.CLDNAMES; self.slf.CLDNAMES = x
      x = self.slf.VARUNITS
      self.slf.VARUNITS = self.slf.CLDUNITS; self.slf.CLDUNITS = x

   def alterMESH(self,mX=1,pX=0,mY=1,pY=0):
      self.slf.MESHX = mX * self.slf.MESHX + pX
      self.slf.MESHY = mY * self.slf.MESHY + pY

   def alterTIMES(self,mT=1,pT=0):
      self.slf.tags['times'] = mT * self.slf.tags['times'] + pT

   def alterVALUES(self,vars=None,mZ=1,pZ=0):
      self.slf.alterVALUES(vars,mZ,pZ)

class calcsSELAFIN(PARAFINS,alterSELAFIN):

   def __init__(self,f, times=None,vars=None,root=None):
      PARAFINS.__init__(self,f,root)
      self.calcs = []
      if times != None:
         tfrom,tstep,tstop = times
         alterSELAFIN.updateTIMES(self,tfrom,tstep,tstop)

   def calcWaterDepth(self):
      self.slf.VARNAMES.append("WATER DEPTH     ")
      self.slf.VARUNITS.append("M               ")
      self.slf.NBV1 += 1
      args = subsetVariablesSLF("FREE SURFACE;BOTTOM",self.slf.VARNAMES)[0]
      def calc(vars,ivars): return [vars[ivars[0]]-vars[ivars[1]]]
      self.calcs.append( [ calc, args ] )

   def calcKineticEnergy(self):
      self.slf.VARNAMES.append("KINETIC ENERGY  ")
      self.slf.VARUNITS.append("?               ")
      self.slf.NBV1 += 1
      args = subsetVariablesSLF("VELOCITY U;VELOCITY V",self.slf.VARNAMES)[0]
      def calc(vars,ivars): return [np.power( np.square(vars[ivars[0]])+np.square(vars[ivars[1]]),(3.0/2.0) )]
      self.calcs.append( [ calc, args ] )

   def putContent(self,fileName):
      self.slf.fole.update({ 'hook': open(fileName,'wb') })
      self.slf.file['name'] = fileName
      pbar = ProgressBar(maxval=len(self.slf.tags['times'])).start()
      self.slf.appendHeaderSLF()
      # ~~> Time stepping
      for t in range(len(self.slf.tags['times'])):
         self.slf.appendCoreTimeSLF(t)
         vars = self.getPALUES(t)
         self.slf.appendCoreVarsSLF(vars)
         for fct,args in self.calcs: self.slf.appendCoreVarsSLF(fct(vars,args))
         pbar.update(t)
      pbar.finish()
      self.slf.fole['hook'].close()

class transfSELAFIN(PARAFINS,alterSELAFIN):

   def __init__(self,f, times=None,vars=None,root=None, points=None):
      PARAFINS.__init__(self,f,root)
      self.calcs = []
      # ~~> history has no impact on potential resampling of self.slf.tags['times']
      if times == ( 1, 1, -1):
         self.history = np.arange(2006.07,2108.75,0.34,dtype=float)
      else:
         tfrom,tstep,tstop = times
         self.history = np.arange(tfrom,tstop,tstep,dtype=float)
      # ~~> extraction restricted to points
      if points != None:
         pass

   def calcFreeSurfaceFromARTEMIS(self):
      # ~~> Dependencies
      #     needs all the components to recreate the free surface signal
      args = range(self.slf.NBV1)
      # ~~> New variable name
      calcs = { 'vars':[["WAVE SURFACE    ","M               "]]}
      # ~~> Initial value 0
      def init(vars,ivars,t0,ti): return [np.zeros(self.slf.NPOIN3,dtype=np.float64)]
      calcs.update( { 'init':( init, args ) } )
      # ~~> Computation of free surface
      def calc(vars,ivars,ttime,ti,lastt,vari):
         # work in double precision
         vars64 = np.asarray(vars, dtype=np.float64)
         for dirN in range(len(vars64)/2):
            # dfr is an artefact to avoid phase locking
            dfr = np.float64(0.04/ti)*np.float64(dirN+1-(len(vars64)/2+1)/2)
            for ipoin in range(self.slf.NPOIN3):
               vari[0][ipoin] += vars64[ivars[2*dirN]][ipoin]                                              \
                              * np.cos(-2.0*np.pi*np.float64(1.0/ti+dfr)*np.float64(ttime)+vars64[ivars[2*dirN+1]][ipoin]) \
                              / (2.0*np.sqrt(2.0))
         return vari

      calcs.update( { 'calc':( calc, args ) } )
      # ~~> Conclusion step
      def stop(t0,ti,vari): return vari
      calcs.update( { 'stop':stop } )
      # ~~> Store
      self.calcs.append( calcs )

   def putContent(self,fileName):
      # ~~> Output file header
      self.slf.fole.update({ 'hook': open(fileName,'wb') })
      self.slf.fole['name'] = fileName
      self.slf.NBV1 = 0; self.slf.NBV2 = 0; self.slf.VARNAMES = []; self.slf.VARUNITS = []
      for calc in self.calcs:
         for cname,cunit in calc['vars']:
            self.slf.VARNAMES.append(cname)
            self.slf.VARUNITS.append(cunit)
            self.slf.NBV1 += 1
      self.slf.appendHeaderSLF()

      # ~~> Time stepping
      print '\n      > Input signal based on:'
      print '      - ',self.slf.NVAR/2,' direction(s)'
      print '      - for the following periods:',self.slf.tags['times']
      # ~~> Time stepping
      print '\n      > Going through time (',len(self.history),'time steps) :'
      pbar = ProgressBar(maxval=len(self.history)).start()

      for ti in range(len(self.history)):
         ttime = self.history[ti]
         #pbar.write('         - '+str(ttime)+' s',ti)
         # ~~> Initialise vari to 0 for each time step
         vari = []
         t0 = self.slf.tags['times'][0]
         for calc,icalc in zip(self.calcs,range(len(self.calcs))):
            fct,args = calc['init']
            vari.append(fct(vari,args,t0,t0))
         # ~~> Sweeps through wave periods, adding up all components of free surface into "vari" for a given time
         #     in this particular case self.slf.tags['times'] holds wave periods, not times
         for tp in range(len(self.slf.tags['times'])):
            vars = self.getPALUES(tp)
            for calc,icalc in zip(self.calcs,range(len(self.calcs))):
               fct,args = calc['calc']
               vari[icalc] = fct(vars,args,ttime,self.slf.tags['times'][tp],self.slf.tags['times'][len(self.slf.tags['times'])-1],vari[icalc])
         # ~~> Print time record
         self.slf.appendCoreTimeSLF(float(ttime))
         for calc,icalc in zip(self.calcs,range(len(self.calcs))):
            fct = calc['stop']
            self.slf.appendCoreVarsSLF(fct(t0,ttime,vari[icalc]))
         pbar.update(ti)

      pbar.finish()
      self.slf.fole['hook'].close()

class crunchSELAFIN(PARAFINS,alterSELAFIN):

   def __init__(self,f, times=None,vars=None,root=None):
      PARAFINS.__init__(self,f,root)
      self.calcs = []
      if times != None:
         tfrom,tstep,tstop = times
         alterSELAFIN.updateTIMES(self,tfrom,tstep,tstop)

   def calcPeakTimeModuloM2(self):
      # ~~> Dependancies
      args = subsetVariablesSLF("FREE SURFACE",self.slf.VARNAMES)[0]
      # ~~> New variable name
      calcs = { 'vars':[["TIME OF PEAK    ","H               "],["SURFACE AT PEAK ","M               "]]}
      # ~~> Initial value for new variable
      def init(vars,ivars,t0,ti): return [np.zeros(self.slf.NPOIN3,dtype=np.float32),np.zeros(self.slf.NPOIN3,dtype=np.float32)]
      calcs.update( { 'init':( init, args ) } )
      # ~~> Calculation for new variable
      def calc(vars,ivars,t0,ti,vari):
         for ipoin in range(self.slf.NPOIN3):
            if vars[ivars[0]][ipoin] > vari[1][ipoin]:
               vari[0][ipoin] = ( (ti-t0)/3600.0 )%12.42  # Modulo M2 (this could be an argument)
               vari[1][ipoin] = vars[ivars[0]][ipoin]
         return vari
      calcs.update( { 'calc':( calc, args ) } )
      # ~~> Conclusion step for new variable
      def stop(t0,ti,vari): return vari
      calcs.update( { 'stop':stop } )
      # ~~> Store
      self.calcs.append( calcs )

   def calcSurfaceRange(self):
      # ~~> Dependancies
      args = subsetVariablesSLF("FREE SURFACE",self.slf.VARNAMES)[0]
      # ~~> New variable name
      calcs = { 'vars':[["SURFACE RANGE   ","M               "]]}
      # ~~> Initial value for new variable
      def init(vars,ivars,t0,ti): return [np.array(vars[ivars[0]],copy=True),np.array(vars[ivars[0]],copy=True)]
      calcs.update( { 'init':( init, args ) } )
      # ~~> Calculation for new variable
      def calc(vars,ivars,t0,ti,vari): return [np.minimum(vars[ivars[0]],vari[0]),np.maximum(vars[ivars[0]],vari[1])]
      calcs.update( { 'calc':( calc, args ) } )
      # ~~> Conclusion step for new variable
      def stop(t0,ti,vari): return [vari[1]-vari[0]]
      calcs.update( { 'stop':stop } )
      # ~~> Store
      self.calcs.append( calcs )

   def calcResidualVelocity(self):
      # ~~> Dependancies
      args = subsetVariablesSLF("VELOCITY U;VELOCITY V",self.slf.VARNAMES)[0]
      # ~~> New variable name
      calcs = { 'vars':[["RESIDUAL U      ","M/S             "],["RESIDUAL V      ","M/S             "]]}
      # ~~> Initial value for new variable
      def init(vars,ivars,t0,ti): return [np.zeros(self.slf.NPOIN3,dtype=np.float32),np.zeros(self.slf.NPOIN3,dtype=np.float32)]
      calcs.update( { 'init':( init, args ) } )
      # ~~> Calculation for new variable
      def calc(vars,ivars,t0,ti,vari):
         vari[0] += vars[ivars[0]]
         vari[1] += vars[ivars[1]]
         return vari
      calcs.update( { 'calc':( calc, args ) } )
      # ~~> Conclusion step for new variable
      def stop(t0,ti,vari): return vari/(ti-t0)
      calcs.update( { 'stop':stop } )
      # ~~> Store
      self.calcs.append( calcs )

   def calcMaximumSpeed(self):
      # ~~> Dependancies
      args = subsetVariablesSLF("VELOCITY U;VELOCITY V",self.slf.VARNAMES)[0]
      # ~~> New variable name
      calcs = { 'vars':[["MAXIMUM SPEED   ","M/S             "]]}
      # ~~> Initial value for new variable
      def init(vars,ivars,t0,ti): return [np.zeros(self.slf.NPOIN3,dtype=np.float32)]
      calcs.update( { 'init':( init, args ) } )
      # ~~> Calculation for new variable
      def calc(vars,ivars,t0,ti,vari): return [np.maximum(vari[0],np.power( (np.power(vars[ivars[0]],2)+np.power(vars[ivars[1]],2) ),(1.0/2.0) ))]
      calcs.update( { 'calc':( calc, args ) } )
      # ~~> Conclusion step for new variable
      def stop(t0,ti,vari): return vari
      calcs.update( { 'stop':stop } )
      # ~~> Store
      self.calcs.append( calcs )

   def putContent(self,fileName):
      # ~~> Sweep through time steps, saving "vari"
      vari = []; initiate = True
      pbar = ProgressBar(maxval=len(self.slf.tags['times'])).start()
      t0 = self.slf.tags['times'][0]
      for t in range(len(self.slf.tags['times'])):
         vars = self.getPALUES(t)
         if initiate:
            for calc,icalc in zip(self.calcs,range(len(self.calcs))):
               fct,args = calc['init']
               vari.append(fct(vars,args,t0,self.slf.tags['times'][t]))
            initiate = False
         else:
            for calc,icalc in zip(self.calcs,range(len(self.calcs))):
               fct,args = calc['calc']
               vari[icalc] = fct(vars,args,t0,self.slf.tags['times'][t],vari[icalc])
         pbar.update(t)
      pbar.finish()
      # ~~> Header
      self.slf.fole.update({ 'hook': open(fileName,'wb') })
      self.slf.fole['name'] = fileName
      self.slf.NBV1 = 0; self.slf.NBV2 = 0; self.slf.VARNAMES = []; self.slf.VARUNITS = []
      for calc in self.calcs:
         for cname,cunit in calc['vars']:
            self.slf.VARNAMES.append(cname)
            self.slf.VARUNITS.append(cunit)
            self.slf.NBV1 += 1
      self.slf.appendHeaderSLF()
      # ~~> Core
      self.slf.appendCoreTimeSLF(0)
      for calc,icalc in zip(self.calcs,range(len(self.calcs))):
         fct = calc['stop']
         self.slf.appendCoreVarsSLF(fct(t0,t,vari[icalc]))
      self.slf.fole['hook'].close()

class subSELAFIN(SELAFIN): # TODO with 3D

   def __init__(self,f):
      SELAFIN.__init__(self,f)
      self.IKLE2,self.MESHX,self.MESHY,self.IPOB2,self.INTERP,self.INTERP3 = subdivideMesh4(self.IKLE2,self.MESHX,self.MESHY)
      #self.IKLE2,self.MESHX,self.MESHY,self.IPOB2,self.INTERP2,self.INTERP3 = subdivideMesh3(self.IKLE2,self.MESHX,self.MESHY)

   def putContent(self,fileName):
      # ~~> Doubling the number of NPLAN
      nplo = self.NPLAN
      if self.NPLAN > 1: self.NPLAN = 2 * self.NPLAN - 1
      npln = self.NPLAN
      if self.IPARAM[6] > 1: self.IPARAM[6] = self.NPLAN
      # ~~> Getting the new size of NPOIN2 from MESHX
      np2o = self.NPOIN2
      self.NPOIN2 = len(self.MESHX)
      np2n = self.NPOIN2
      # ~~> Setting the new size of NPOIN3
      np3o = self.NPOIN3
      self.NPOIN3 = self.NPLAN * self.NPOIN2
      np3n = self.NPOIN3
      # ~~> Getting the new size of NELEM2 from self.IKLE2
      nel2o = self.NELEM2
      self.NELEM2 = len(self.IKLE2)
      nel2n = self.NELEM2
      # ~~> Setting the new size of NELEM3
      nel3o = self.NELEM3
      if self.NPLAN > 1: self.NELEM3 = (self.NPLAN-1)*self.NELEM2
      else: self.NELEM3 = self.NELEM2
      nel3n = self.NELEM3
      # ~~> Connecting
      if self.NPLAN > 1:
         self.IPOB3 = np.ravel(np.add(np.repeat(self.IPOB2,self.NPLAN).reshape((self.NPOIN2,self.NPLAN)),self.NPOIN2*np.arange(self.NPLAN)).T)
         self.IKLE3 = \
            np.repeat(self.NPOIN2*np.arange(self.NPLAN-1),self.NELEM2*self.NDP3).reshape((self.NELEM2*(self.NPLAN-1),self.NDP3)) + \
            np.tile(np.add(np.tile(self.IKLE2,2),np.repeat(self.NPOIN2*np.arange(2),self.NDP2)),(self.NPLAN-1,1))
      else:
         self.IPOB3 = self.IPOB2
         self.IKLE3 = self.IKLE2
      # ~~> Filing
      self.fole.update({ 'hook': open(fileName,'wb') })
      self.fole['name'] = fileName
      self.appendHeaderSLF()
      pbar = ProgressBar(maxval=len(self.tags['times'])).start()
      # ~~> Time stepping
      varx = np.zeros((self.NVAR,self.NPOIN3),np.float32)
      for t in range(len(self.tags['times'])):
         self.appendCoreTimeSLF(t)
         self.NPOIN3 = np3o           #\
         vars = self.getVALUES(t)     #|+ game of shadows
         self.NPOIN3 = np3n           #/
         for iv in range(self.NVAR):  # TODO:(JPC), convert to numpy calculations
            for iplan in range(nplo):
               varx[iv][0+2*iplan*np2n:np2o+2*iplan*np2n] = vars[iv][0+iplan*np2o:np2o+iplan*np2o]
               varx[iv][np2o+2*iplan*np2n:np2o+2*iplan*np2n+np2n-np2o] = np.sum(vars[iv][self.INTERP+iplan*np2o],axis=1)/2.
            for iplan in range(nplo-1):
               varx[iv][(2*iplan+1)*np2n:(2*iplan+2)*np2n] = ( varx[iv][2*iplan*np2n:(2*iplan+1)*np2n]+varx[iv][(2*iplan+2)*np2n:(2*iplan+3)*np2n] )/2.
         self.appendCoreVarsSLF(varx)
         pbar.update(t)
      pbar.finish()
      self.fole['hook'].close()

class scanSPECTRAL(scanSELAFIN):

   def printHeader(self):
      scanSELAFIN.printHeader(self)
      nd = self.slf.NPOIN3-self.slf.NELEM3
      nf = self.slf.NPOIN3/nd
      print "FREQUENCIES ",nf," / min: [ ",self.slf.MESHY[0],"]  / max: [ ",self.slf.MESHY[self.slf.NPOIN3-nd],"]"
      print ' '.join([ repr(i) for i in self.slf.MESHY[0::nd] ])
      print "DIRECTIONS  ",nd," / angle: ",360./nd

   def printCore(self,na):
      for v in range(self.slf.NBV1):
         print "VARIABLE     : ",self.slf.VARNAMES[v]
         for t in range(len(self.slf.tags['times'])):
            VARSOR = self.slf.getVariablesAt( t,[self.slf.VARINDEX[v]] )
            print "    / TIME: ",self.slf.tags['times'][t]
            # na significant figures
            accuracy = np.power(10.0, -na+np.floor(np.log10(abs(np.max(VARSOR)))))
            print '\nFACTOR ', accuracy
            #spe = np.reshape(np.array((VARSOR/accuracy),dtype=np.int),(self.NPOIN3-self.NELEM3,-1)) # by direction
            spe = np.reshape(np.array((VARSOR/accuracy),dtype=np.int),(self.slf.NPOIN3/(self.slf.NPOIN3-self.slf.NELEM3),-1)) # by frequency
            for s in spe: print ' '.join([ repr(i).rjust(na+1) for i in s ])
            print '\n'
      for v in range(self.slf.NBV2):
         print "CLANDESTINE  : ",self.slf.CLDNAMES[v]
         for t in range(len(self.slf.tags['times'])):
            VARSOR = self.slf.getVariablesAt( t,[self.slf.VARINDEX[v+self.slf.NBV1]] )
            print "    / TIME: ",self.slf.tags['times'][t]
            # na significant figures
            accuracy = np.power(10.0, -na+np.floor(np.log10(abs(np.max(VARSOR)))))
            print '\nFACTOR ', accuracy
            #spe = np.reshape(np.array((VARSOR/accuracy),dtype=np.int),(self.NPOIN3-self.NELEM3,-1)) # by direction
            spe = np.reshape(np.array((VARSOR/accuracy),dtype=np.int),(self.slf.NPOIN3/(self.slf.NPOIN3-self.slf.NELEM3),-1)) # by frequency
            for s in spe: print ' '.join([ repr(i).rjust(na+1) for i in s ])
            print '\n'

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

def main(action=None):
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n'+'~'*72+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Tools for handling SELAFIN files and TELEMAC binary related in python\n
Possible actions:\n
   scan        will print information about the SELAFIN, such as variables, their vales etc.
   spec        will print information about a spectral file (also SELAFIN), such as
                  frequencies, periodes, etc.
   chop        will chop a SELAFIN given a new set of time range and step (but alter is better)
   alter       will alter a SELAFIN file, choping or modifying time, converting its
                  coordinates, extracting variables, etc.
   merge       will merge two files together, whether they are continuous simulations
                  (same variables) or putting variables together (same time definition)
   subdivide   will subdivide a mesh by one iteration (splitting all triangles in four others)
      '''))
   #parser.add_argument(
   #   dest="action", type=str,
   #   choices=set(("scan", "spec", "chop", "alter", "merge", "subdivide")),
   #   help="""Choose one in the list below:
   parser.add_argument( "args",nargs='*' )
   # valid for scan, chop and alter
   parser.add_argument(\
      "-v", "--vars",dest="xvars",default=None,
      help="specify which variables should remain (','-delimited)" )
   # valid for scan and chop
   parser.add_argument(\
      "-c","--core",action="store_true",dest="core",default=False,
      help="scan: specify whether to print statistics on the core variables" )
   parser.add_argument(\
      "-f", "--from",dest="tfrom",default="1",
      help="chop: specify the first frame included" )
   parser.add_argument(\
      "-s", "--stop",dest="tstop",default="-1",
      help="chop: specify the last frame included (negative from the end)" )
   parser.add_argument(\
      "-d", "--step",dest="tstep",default="1",
      help="specify the step for the extraction of frames" )
   # valid for chop, alter and merge
   parser.add_argument(\
      "-r", "--replace",action="store_true",dest="freplace",default=False,
      help="if present, the output file will eventualy replace the input file" )
   # valid for alter
   parser.add_argument(\
      "--title",dest="atitle",default=None,
      help="set the title of the SLF" )
   parser.add_argument(\
      "--reset",action="store_true",dest="areset",default=False,
      help="reset AT to zero second" )
   parser.add_argument(\
      "--date",dest="adate",default=None,
      help="set the start date of the SLF (dd-mm-yyyy)" )
   parser.add_argument(\
      "--time",dest="atime",default=None,
      help="set the start time of the SLF (hh:mm:ss)" )
   parser.add_argument(\
      "--endian",action="store_true",dest="eswitch",default=False,
      help="switch between endian encoddings" )
   parser.add_argument(\
      "--float",action="store_true",dest="fswitch",default=False,
      help="switch between DOUBLE and SINGLE precision float" )
   parser.add_argument(\
      "--switch",action="store_true",dest="aswitch",default=False,
      help="switch between VARIABLES and CLANDESTINES" )
   parser.add_argument(\
      "--name",dest="aname",default=None,
      help="change the name of a VARIABLE: 'OLD VAR=NEW VAR'" )
   parser.add_argument(\
      "--T+?",dest="atp",default="0",
      help="adds to the ATs" )
   parser.add_argument(\
      "--T*?",dest="atm",default="1",
      help="scales the ATs" )
   parser.add_argument(\
      "--sph2ll",dest="sph2ll",default=None,
      help="convert from spherical to longitude-latitude" )
   parser.add_argument(\
      "--ll2sph",dest="ll2sph",default=None,
      help="convert from longitude-latitude to spherical" )
   parser.add_argument(\
      "--ll2utm",dest="ll2utm",default=None,
      help="convert from longitude-latitude to UTM" )
   parser.add_argument(\
      "--utm2ll",dest="utm2ll",default=None,
      help="convert from UTM to longitude-latitude" )
   parser.add_argument(\
      "--X+?",dest="axp",default="0",
      help="adds to the MESHX" )
   parser.add_argument(\
      "--X*?",dest="axm",default="1",
      help="scales the MESHX" )
   parser.add_argument(\
      "--Y+?",dest="ayp",default="0",
      help="adds to the MESHY" )
   parser.add_argument(\
      "--Y*?",dest="aym",default="1",
      help="scales the MESHY" )
   parser.add_argument(\
      "--Z?",dest="azname",default=None,
      help="will filter Z+ znd Z* operations on that VARIABLE name" )
   parser.add_argument(\
      "--Z+?",dest="azp",default="0",
      help="adds to the VARIABLE" )
   parser.add_argument(\
      "--Z*?",dest="azm",default="1",
      help="scales the VARIABLE" )
   parser.add_argument(\
      "--accuracy",dest="accuracy",default="5",
      help="significant figures for text display" )
   # valid for transf / TODO: make this valid for all
   parser.add_argument(\
      "--points",dest="points",default=None,
      help="extract data only at those locations" )
   parser.add_argument(\
      "--nodes",dest="nodes",default=None,
      help="extract data only at those nodes" )
   # valid for all
   parser.add_argument(\
      "--parallel",action="store_true",dest="parallel",default=False,
      help="if option there, will assume input files have not been recollected, in which case you also need one example of the global file" )

   options = parser.parse_args()
   # Adding the action first if called from alter/scan/chop.py
   if not action is None:
       options.args.insert(0,action)
   if len(options.args) < 1:
      print '\nThe name of one SELAFIN file at least is required\n'
      parser.print_help()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = options.args[0]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of SCAN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if codeName == 'scan':

      slfFiles = options.args[1:]
      for slfFile in slfFiles:

         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         if not path.exists(slfFile):
            print '\nCould not find the file named: ',slfFile
            sys.exit(1)
         print '\n\nScanning ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n'+'~'*72+'\n'
         vars = options.xvars
         if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
         slf = scanSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars )
         slf.printHeader()
         if options.core: slf.printCore()
         else: slf.printTimeSummary()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of SPECTRAL file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'spec':

      slfFiles = options.args[1:]
      for slfFile in slfFiles:

         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         if not path.exists(slfFile):
            print '\nCould not find the file named: ',slfFile
            sys.exit(1)
         print '\n\nScanning ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n'+'~'*72+'\n'
         vars = options.xvars
         if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
         slf = scanSPECTRAL( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars )
         slf.printHeader()
         if options.core: slf.printCore(int(options.accuracy))
         else: slf.printTimeSummary()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of CHOP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'chop':

      rootFile = None
      if not options.freplace:
         if not options.parallel:
            if len(options.args) != 3:
               print '\nThe code "chop" (without --replace) here requires 2 file names\n'
               parser.print_help()
               sys.exit(1)
            slfFiles = [ options.args[1] ]
            outFile = options.args[2]
         else:
            if len(options.args) != 4:
               print '\nThe code "chop" (without --replace) here requires 2 file names and 1 file root name for the partition\n'
               parser.print_help()
               sys.exit(1)
            slfFiles = [ options.args[1] ]
            rootFile = options.args[2]
            outFile = options.args[3]
      else:
         slfFiles = options.args[1:]
         outFile = "chop-tmp.slf"

      for slfFile in slfFiles:

         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         if not path.exists(slfFile):
            print '\nCould not find the file named: ',slfFile
            sys.exit(1)
         print '\n\nChoping ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n'+'~'*72+'\n'
         vars = options.xvars
         if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
         slf = chopSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars, root=rootFile )
         if options.eswitch: slf.alterEndian()
         if options.fswitch: slf.alterFloat()

         slf.putContent( outFile )

         if options.freplace: moveFile(outFile,slfFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of ALTER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'alter':

      rootFile = None
      if not options.freplace:
         if not options.parallel:
            if len(options.args) != 3:
               print '\nThe code "alter" (without --replace) requires 2 file names\n'
               parser.print_help()
               sys.exit(1)
            slfFiles = [ options.args[1] ]
            outFile = options.args[2]
         else:
            if len(options.args) != 4:
               print '\nThe code "alter" (without --replace) here requires 2 file names and 1 file root name for the partition\n'
               parser.print_help()
               sys.exit(1)
            slfFiles = [ options.args[1] ]
            rootFile = options.args[2]
            outFile = options.args[3]
      else:
         slfFiles = options.args[1:]
         outFile = "chop-tmp.slf"

      for slfFile in slfFiles:

         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         if not path.exists(slfFile):
            print '\nCould not find the file named: ',slfFile
            sys.exit(1)
         print '\n\nAltering ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n'+'~'*72+'\n'
         vars = options.xvars
         if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
         slf = alterSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars, root=rootFile  )
         if options.atitle != None: slf.alterTITLE(options.atitle)
         if options.areset: slf.alterTIMES(pT=-slf.slf.tags['times'][0])
         if options.adate != None: slf.alterDATETIME(date=options.adate.split('-'))
         if options.atime != None: slf.alterDATETIME(time=options.atime.split(':'))
         if options.aswitch: slf.switchVARS()
         if options.eswitch: slf.alterEndian()
         if options.fswitch: slf.alterFloat()
         if options.aname != None: slf.alterVARS( options.aname )
         slf.alterTIMES( mT=float(options.atm),pT=float(options.atp) )
         slf.alterMESH( mX=float(options.axm),pX=float(options.axp),mY=float(options.aym),pY=float(options.ayp) )
         if options.azname != None: slf.alterVALUES( options.azname, mZ=float(options.azm),pZ=float(options.azp) )
         if options.sph2ll != None:
            radius  = 6371000.
            long0,lat0 = options.sph2ll.split(":")
            long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
            const = np.tan( lat0/2. + np.pi/4. )
            slf.slf.MESHX = np.rad2deg( slf.slf.MESHX/radius + long0 )
            slf.slf.MESHY = np.rad2deg( 2.*np.arctan( const*np.exp(slf.slf.MESHY/radius) ) - np.pi/2. )
         if options.ll2sph != None:
            radius  = 6371000.
            long0,lat0 = options.ll2sph.split(":")
            long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
            slf.slf.MESHX = radius * ( np.deg2rad(slf.slf.MESHX) - long0 )
            slf.slf.MESHY = radius * ( np.log( np.tan( np.deg2rad(slf.slf.MESHY)/2. + np.pi/4. ) ) \
                                     - np.log( np.tan( lat0/2. + np.pi/4. ) ) )
         if options.ll2utm != None:
            zone = int(options.ll2utm)
            slf.slf.MESHX,slf.slf.MESHY,zone = utm.fromLatLong(slf.slf.MESHX,slf.slf.MESHY,zone)
         if options.utm2ll != None:
            zone = int(options.utm2ll)
            slf.slf.MESHX,slf.slf.MESHY = utm.toLatLong(slf.slf.MESHX,slf.slf.MESHY,zone)

         slf.putContent( outFile )

         if options.freplace: moveFile(outFile,slfFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of MERGE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'merge':

      rootFile = None
      if not options.parallel:
         if len(options.args) < 4:
            print '\nThe code "merge" requires at leat 2 file names, aside from the options\n'
            parser.print_help()
            sys.exit(1)
         slfFiles = options.args[1:len(options.args)-1]
         outFile = options.args[len(options.args)-1]

         slfs = SELAFINS()
         print '\n\nMerging into ' + path.basename(outFile) + ' within ' + path.dirname(outFile) + '\n'+'~'*72+'\n'
         for slfFile in slfFiles:
            slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
            if not path.exists(slfFile):
               print '\nCould not find the file named: ',slfFile
               sys.exit(1)
            slfs.add( slfFile )

         slfs.putContent(outFile)

      else:
         if len(options.args) != 4:
            print '\nThe code "merge" here requires 2 file names and 1 file root name for the partition\n'
            parser.print_help()
            sys.exit(1)
         slfFile = options.args[1]
         rootFile = options.args[2]
         outFile = options.args[3]

         print '\n\nMerging into ' + path.basename(outFile) + ' within ' + path.dirname(outFile) + '\n'+'~'*72+'\n'
         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         if not path.exists(slfFile):
            print '\nCould not find the file named: ',slfFile
            sys.exit(1)

         vars = options.xvars
         if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
         slf = alterSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars, root=rootFile  )
         if options.atitle != None: slf.alterTITLE(options.atitle)
         if options.areset: slf.alterTIMES(pT=-slf.tags['times'][0])
         if options.adate != None: slf.alterDATETIME(date=options.adate.split('-'))
         if options.atime != None: slf.alterDATETIME(time=options.atime.split(':'))
         if options.aswitch: slf.switchVARS()
         if options.eswitch: slf.alterEndian()
         if options.fswitch: slf.alterFloat()
         if options.aname != None: slf.alterVARS( options.aname )
         slf.alterTIMES( mT=float(options.atm),pT=float(options.atp) )
         slf.alterMESH( mX=float(options.axm),pX=float(options.axp),mY=float(options.aym),pY=float(options.ayp) )
         if options.azname != None: slf.alterVALUES( options.azname, mZ=float(options.azm),pZ=float(options.azp) )

         slf.putContent( outFile )

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of DIFF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'diff':

      if len(options.args) < 3:
         print '\nThe code "diff" uses a minimum of 3 argumensts, aside from the options\n'
         parser.print_help()
         sys.exit(1)
      slfFiles = options.args[1:len(options.args)-1]
      outFile = options.args[len(options.args)-1]

      slfs = SELAFINS()
      print '\n\nDifferences into ' + path.basename(outFile) + '\n'+'~'*72+'\n'
      for slfFile in slfFiles:
         slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
         if not path.exists(slfFile):
            print '\nCould not find the file named: ',slfFile
            sys.exit(1)
         slfs.add( slfFile )

      slfs.putContent(outFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of SAMPLE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'sample':
      rootFile = None
      if not options.parallel:
         if len(options.args) < 4:
            print '\nThe code "sample" requires at least 2 file names and one series of node numbers\n'
            parser.print_help()
            sys.exit(1)
         slfFile = options.args[1]
         outFile = options.args[2]
         nodList = []
         for nod in options.args[3].split(" "): nodList.append(int(nod))
      else:
         if len(options.args) != 5:
            print '\nThe code "sample" here requires 2 file names, 1 file root name for the partition and 1 series of node numbers\n'
            parser.print_help()
            sys.exit(1)
         slfFile = options.args[1]
         rootFile = options.args[2]
         outFile = options.args[3]
         nodList = []
         for nod in options.args[4].split(" "): nodList.append(int(nod))

      slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
      if not path.exists(slfFile):
         print '\nCould not find the file named: ',slfFile
         sys.exit(1)
      print '\n\nSample ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n'+'~'*72+'\n'
      vars = options.xvars
      if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
      slf = chopSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars, root=rootFile )

      lqd = LQD( vars=[zip(slf.slf.VARNAMES,slf.slf.VARUNITS),nodList], date=slf.slf.DATETIME, times=slf.slf.tags['times'], series=slf.getSERIES(nodList) )
      lqd.putContent( outFile )

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of CALCS and CRUNCH ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'calcs' or codeName == 'crunch' or codeName == 'transf':
      rootFile = None
      if not options.parallel:
         if len(options.args) < 3:
            print '\nThe code "calcs" requires 2 file names\n'
            parser.print_help()
            sys.exit(1)
         slfFile = options.args[1]
         outFile = options.args[2]
      else:
         if len(options.args) != 4:
            print '\nThe code "calcs" requires 2 file names and 1 root file name for parallel inputs\n'
            parser.print_help()
            sys.exit(1)
         slfFile = options.args[1]
         rootFile = options.args[2]
         outFile = options.args[3]

      slfFile = path.realpath(slfFile)  #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
      if not path.exists(slfFile):
         print '\nCould not find the file named: ',slfFile
         sys.exit(1)
      print '\n\nCalculations for ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n'+'~'*72+'\n'
      vars = options.xvars; calcList = []
      if options.xvars != None:
         vars = cleanQuotes(options.xvars.replace('_',' '))
         calcList = vars.split(':')
      if codeName == 'calcs':
         slf = calcsSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), root=rootFile )
         print '   ~> Assembling the following variables together into the file:'
         for calc in calcList:
            if calc.upper() in "WATER DEPTH":
               print '      +> WATER DEPTH'
               slf.calcWaterDepth()
            if calc.upper() in "KINETIC ENERGY":
               print '      +> KINETIC ENERGY'
               slf.calcKineticEnergy()
      elif codeName == 'transf':
         slf = transfSELAFIN( slfFile, times = (float(options.tfrom),float(options.tstep),float(options.tstop)), root=rootFile )
         print '   ~> Computing an animation for the following variable(s):'
         for calc in calcList:
            if calc.upper() in "WAVE SURFACE":
               print '      +> WAVE SURFACE'
               #if options.points != None:
               slf.calcFreeSurfaceFromARTEMIS()
            #if calc.upper() in "TIDE SURFACE":
            #   print '      +> TIDE SURFACE and CURRENTS'
            #   slf.calcFreeSurfaceFromTPXO()
      elif codeName == 'crunch':
         slf = crunchSELAFIN( slfFile, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), root=rootFile )
         print '   ~> Assembling the following variables into the file:'
         for calc in calcList:
            if calc.upper() in "SURFACE RANGE":
               print '      +> SURFACE RANGE'
               slf.calcSurfaceRange()
            if calc.upper() in "MAXIMUM SPEED":
               print '      +> MAXIMUM SPEED'
               slf.calcMaximumSpeed()
            if calc.upper() in "TIME OF PEAK":
               print '      +> TIME OF PEAK'
               slf.calcPeakTimeModuloM2()
            if calc.upper() in "RESIDUAL U":
               print '      +> RESIDUAL U'
               slf.calcResidualVelocity()
      slf.alterTIMES( mT=float(options.atm),pT=float(options.atp) )
      slf.alterMESH( mX=float(options.axm),pX=float(options.axp),mY=float(options.aym),pY=float(options.ayp) )
      if options.azname != None: slf.alterVALUES( options.azname, mZ=float(options.azm),pZ=float(options.azp) )
      if options.eswitch: slf.alterEndian()
      if options.fswitch: slf.alterFloat()

      slf.putContent(outFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of SUBDIVIDE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'subdivide':

      if not options.freplace:
         if len(options.args) != 3:
            print '\nThe code "subdivide" (without --replace) here requires 2 file names\n'
            sys.exit(1)
         slfFile = options.args[1]
         outFile = options.args[2]
      else:
         if len(options.args) != 2:
            print '\nThe code "subdivide" (with --replace) here requires 1 file name at a time\n'
            sys.exit(1)
         slfFile = options.args[1]
         outFile = "subdivide-tmp.slf"

      slfFile = path.realpath(slfFile)
      if not path.exists(slfFile):
         print '\nCould not find the file named: ',slfFile
         sys.exit(1)
      print '\n\nSubdividing ' + path.basename(slfFile) + ' within ' + path.dirname(slfFile) + '\n'+'~'*72+'\n'
      slf = subSELAFIN( slfFile )
      slf.putContent( outFile )

      if options.freplace: moveFile(outFile,slfFile)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of TESSELLATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   elif codeName == 'tessellate':
      if not options.freplace:
         if len(options.args) != 3:
            print '\nThe code "tessellate" here requires one i2s/i3s file and one output slf file\n'
            sys.exit(1)
         i3sFile = options.args[1]
         outFile = options.args[2]
      else:
         if len(options.args) != 2:
            print '\nThe code "tessellate" here requires one i2s/i3s file\n'
            sys.exit(1)
         i3sFile = options.args[1]
         head,tail = path.splitext(i3sFile)
         outFile = head+'.slf'

      i3sFile = path.realpath(i3sFile)
      if not path.exists(i3sFile):
         print '\nCould not find the file named: ',i3sFile
         sys.exit(1)

      print '\n\nTessellating ' + path.basename(i3sFile) + ' within ' + path.dirname(i3sFile) + '\n'+'~'*72+'\n'
      i2s = InS( i3sFile )
      IKLE2,IPOB2,MESHX,MESHY = tessellatePoly(i2s,debug=True)

      print '\n\nWriting down the SELAFIN file ' + path.basename(outFile) + '\n'+'~'*72+'\n'
      slf = SELAFIN('')
      slf.TITLE = ''
      slf.NPLAN = 1
      slf.NDP2 = 3; slf.NDP3 = 3
      slf.NBV1 = 1; slf.NVAR = 1
      slf.VARINDEX = 1
      slf.VARNAMES = ['BOTTOM          ']
      slf.VARUNITS = ['M               ']
      slf.IKLE2 = IKLE2; slf.IKLE3 = slf.IKLE2
      slf.MESHX = MESHX; slf.MESHY = MESHY
      slf.NPOIN2 = i2s.npoin; slf.NPOIN3 = slf.NPOIN2
      slf.NELEM2 = slf.IKLE2.shape[0]; slf.NELEM3 = slf.NELEM2
      slf.IPARAM = [ 0,0,0,0,0,0,            1,     0,0,0 ]
      slf.IPOB2 = IPOB2; slf.IPOB3 = slf.IPOB2
      slf.fole = {'hook': open(outFile,'wb'), 'endian': ">" , 'float': ('f',4),'name':outFile }
      slf.tags['times'] = [1]
      if options.sph2ll != None:
         radius  = 6371000.
         long0,lat0 = options.sph2ll.split(":")
         long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
         const = np.tan( lat0/2. + np.pi/4. )
         slf.MESHX = np.rad2deg( slf.MESHX/radius + long0 )
         slf.MESHY = np.rad2deg( 2.*np.arctan( const*np.exp(slf.MESHY/radius) ) - np.pi/2. )
      if options.ll2sph != None:
         radius  = 6371000.
         long0,lat0 = options.ll2sph.split(":")
         long0 = np.deg2rad(float(long0)); lat0 = np.deg2rad(float(lat0))
         slf.MESHX = radius * ( np.deg2rad(slf.MESHX) - long0 )
         slf.MESHY = radius * ( np.log( np.tan( np.deg2rad(slf.MESHY)/2. + np.pi/4. ) ) \
                                     - np.log( np.tan( lat0/2. + np.pi/4. ) ) )
      if options.ll2utm != None:
         zone = int(options.ll2utm)
         slf.MESHX,slf.MESHY,zone = utm.fromLatLong(slf.MESHX,slf.MESHY)
      if options.utm2ll != None:
         zone = int(options.utm2ll)
         slf.MESHX,slf.MESHY = utm.toLatLong(slf.MESHX,slf.MESHY,zone)
      slf.appendHeaderSLF(); slf.appendCoreTimeSLF(0)
      slf.appendCoreVarsSLF([np.zeros(slf.NPOIN2)])
      slf.fole['hook'].close()

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
   main()
