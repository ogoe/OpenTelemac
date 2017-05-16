#!/usr/bin/env python
"""@author Sebastien E. Bourban and Noemie Durand
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
   Tools for handling conversions to-from JCOPE2 server files
"""
"""@details
   Contains server read functions to convert to SELAFIN file
"""
"""@history 30/04/2013 -- Sebastien E. Bourban
   Frist draft, also the final version.
"""
"""@history 11/09/2013 -- Noemie Durand
   Modification of the 3D output file, so the elevation of the top
     layer is the same as the elevation in the 2D file.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
from argparse import ArgumentParser,RawDescriptionHelpFormatter
import time
from datetime import datetime,timedelta
import numpy as np
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
from parsers.parserSELAFIN import SELAFIN
from utils.progressbar import ProgressBar
# ~~> dependencies towards other modules
try:
   from pydap.client import open_url
except:
   print '... you are in bad luck !'
   print '  ~>  you need the pydap library unzipped locally'
   sys.exit(1)


# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#
# Current Experiments
#    => url = 'http://apdrc.soest.hawaii.edu/dods/public_data/FRA-JCOPE2'
# Other Experiments
#    => url = 'http://apdrc.soest.hawaii.edu/dapper/public_data/topography/io_bathy/modified_etopo2.nc'

class JCOPE2():


   def __init__(self,dates):

      # ~~~~ Initialisation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.moddates = [ datetime(*dates[0]),datetime(*dates[1]) ]
      jcope2vars = ['el','t','s','u','v']
      jcope2date = [ 1993,1,1 ] # /!\ unknown convertion of time records into dates
      jcope2root = 'http://apdrc.soest.hawaii.edu/dods/public_data/FRA-JCOPE2'

      # ~~~~ Time records ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Extract JCOPE2 time records\n'
      self.experiments = []
      experiment = {} # /!\ only one time period covered at this stage
      for jvar in jcope2vars:
         jcope2url = jcope2root+'/'+jvar
         jcope2data = open_url(jcope2url)
         NIT = jcope2data['time'].shape[0]
         print '        x '+str(NIT)+' records from '+jcope2url,
         ITs = []
         ATs = []
         for itime in range(NIT):
            d = datetime(jcope2date[0],jcope2date[1],jcope2date[2])+timedelta(itime)
            if itime == 0: print ' from: ',str(d),
            if itime == NIT-1: print ' to: ',str(d)
            if self.moddates[0] <= d and d <= self.moddates[1]:
               ITs.append(itime)
               ATs.append(d)
         if ITs != []:
            for ivar in jcope2data.keys():
               if ivar not in [ 'time', 'lev', 'lat', 'lon' ]: experiment.update( {ivar:jcope2data[ivar]} )
         else:
            print '... I could not find the time to do your work'
            print '  ~>  you may need to select a different time period'
            sys.exit(1)
      self.experiments.append((experiment,NIT,ITs,ATs))
      print '\n'


   def getHeaderJCOPE2(self,bounds):

      # ~~> inheritence
      self.slf3d = SELAFIN('')     # slf3d
      self.slf2d = SELAFIN('')     # slf2d surface

      print '     +> Set SELAFIN Variables'
      self.slf3d.TITLE = ''
      self.slf3d.NBV1 = 6
      self.slf3d.NVAR = 6
      self.slf3d.VARINDEX = range(self.slf3d.NVAR)
      self.slf3d.VARNAMES = ['ELEVATION Z     ', \
         'SALINITY        ','TEMPERATURE     ', \
         'VELOCITY U      ','VELOCITY V      ','VELOCITY W      ']
      self.slf3d.VARUNITS = ['M               ', \
         '                ','                ', \
         'M/S             ','M/S             ','M/S             ']
      self.slf2d.TITLE = self.slf3d.TITLE
      self.slf2d.NBV1 = self.slf3d.NBV1 - 1
      self.slf2d.NVAR = self.slf2d.NBV1
      self.slf2d.VARINDEX = range(self.slf2d.NVAR)
      self.slf2d.VARNAMES = self.slf3d.VARNAMES[0:-1]
      self.slf2d.VARUNITS = self.slf3d.VARUNITS[0:-1]

      # ~~~~ Grid coordinates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # ~~> the whole of the 2D grid sizes
      print '     +> Extract JCOPE2 sizes'
      # /!\ 't' gives me access to NPLAN in 3D
      jcope2data = self.experiments[0][0]['temp']
      NX1D = jcope2data['lon'].shape[0]
      NY1D = jcope2data['lat'].shape[0]
      print '     +> Extract JCOPE2 mesh'
      lonX1D = jcope2data['lon'].data[0:NX1D]
      latY1D = jcope2data['lat'].data[0:NY1D]
      # ~~> no correction for lat,lon
      # ~~> subset for the SELAFIN
      print '     +> Set SELAFIN mesh'
      self.jcope2ilon = np.where(( lonX1D >= bounds[0][1] )*( lonX1D <= bounds[1][1] ))[0]
      self.jcope2ilat = np.where(( latY1D >= bounds[0][0] )*( latY1D <= bounds[1][0] ))[0]
      x = lonX1D[ self.jcope2ilon ]
      y = latY1D[ self.jcope2ilat ]
      NX1D = len(x)
      NY1D = len(y)

      # ~~~~ MESH sizes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Set SELAFIN sizes'
      # ~~> 3D
      self.slf3d.NPLAN = jcope2data['lev'].shape[0]
      self.ZPLAN = jcope2data['lev'][0:self.slf3d.NPLAN][::-1] # I do not know any other way
      self.slf3d.NDP2 = 3
      self.slf3d.NDP3 = 6
      self.slf3d.NPOIN2 = NX1D * NY1D
      self.slf3d.NPOIN3 = self.slf3d.NPOIN2 * self.slf3d.NPLAN
      self.slf3d.NELEM2 = 2*( NX1D-1 )*( NY1D-1 )
      self.slf3d.NELEM3 = self.slf3d.NELEM2 * ( self.slf3d.NPLAN-1 )
      self.slf3d.IPARAM = [ 0,0,0,0,0,0, self.slf3d.NPLAN, 0,0,0 ]
      # ~~> 2D
      self.slf2d.NPLAN = 1
      self.slf2d.NDP2 = self.slf3d.NDP2
      self.slf2d.NDP3 = self.slf2d.NDP2
      self.slf2d.NPOIN2 = self.slf3d.NPOIN2
      self.slf2d.NPOIN3 = self.slf2d.NPOIN2
      self.slf2d.NELEM2 = self.slf3d.NELEM2
      self.slf2d.NELEM3 = self.slf2d.NELEM2
      self.slf2d.IPARAM = [ 0,0,0,0,0,0,            1,     0,0,0 ]

      # ~~~~ Connectivity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Set the default SELAFIN IKLE 3D'
      ielem = 0; pbar = ProgressBar(maxval=self.slf3d.NELEM3).start()
      self.slf3d.IKLE3 = np.zeros((self.slf3d.NELEM3,self.slf3d.NDP3),dtype=np.int)
      for k in range(1,self.slf3d.NPLAN):
         for i in range(1,NX1D):
            for j in range(1,NY1D):
               ipoin = (i-1)*NY1D + j - 1 + (k-1)*self.slf3d.NPOIN2
               # ~~> first prism
               self.slf3d.IKLE3[ielem][0] = ipoin
               self.slf3d.IKLE3[ielem][1] = ipoin + NY1D
               self.slf3d.IKLE3[ielem][2] = ipoin + 1
               self.slf3d.IKLE3[ielem][3] = ipoin + self.slf3d.NPOIN2
               self.slf3d.IKLE3[ielem][4] = ipoin + NY1D + self.slf3d.NPOIN2
               self.slf3d.IKLE3[ielem][5] = ipoin + 1 + self.slf3d.NPOIN2
               ielem = ielem + 1
               pbar.update(ielem)
               # ~~> second prism
               self.slf3d.IKLE3[ielem][0] = ipoin + NY1D
               self.slf3d.IKLE3[ielem][1] = ipoin + NY1D + 1
               self.slf3d.IKLE3[ielem][2] = ipoin + 1
               self.slf3d.IKLE3[ielem][3] = ipoin + NY1D + self.slf3d.NPOIN2
               self.slf3d.IKLE3[ielem][4] = ipoin + NY1D + 1 + self.slf3d.NPOIN2
               self.slf3d.IKLE3[ielem][5] = ipoin + 1 + self.slf3d.NPOIN2
               ielem = ielem + 1
               pbar.update(ielem)
      pbar.finish()
      self.slf2d.IKLE3 = np.compress( [ True,True,True,False,False,False ], self.slf3d.IKLE3[0:self.slf3d.NELEM2], axis=1 ) #.reshape((self.slf3d.NELEM2,self.slf3d.NDP2))

      # ~~~~ Boundaries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Set SELAFIN IPOBO'
      pbar = ProgressBar(maxval=NX1D+NY1D).start()
      IPOB2 = np.zeros(self.slf3d.NPOIN2,dtype=np.int)
      # ~~> along the x-axis (lon)
      for i in range(NX1D):
         ipoin = i*NY1D
         IPOB2[ipoin] = i + 1
         ipoin = i*NY1D -1
         IPOB2[ipoin] = 2*NX1D+(NY1D-2) - i
         pbar.update(i)
      # ~~> along the y-axis (alt)
      for i in range(1,NY1D):
         ipoin = i
         IPOB2[ipoin] = 2*NX1D + 2*(NY1D-2) -i + 1
         ipoin = NY1D*(NX1D-1) + i
         IPOB2[ipoin] = NX1D + i
         pbar.update(i+NX1D)
      pbar.finish()

      # ~~~~ Connectivity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # /!\ 'el' gives me access to the real mesh removing elements with -99 values
      print '     +> Mask the non-values from the SELAFIN IKLE'
      jcope2data = self.experiments[0][0]['el']
      var = np.swapaxes( jcope2data['el'].data[0,0,self.jcope2ilat[0]:self.jcope2ilat[-1]+1,self.jcope2ilon[0]:self.jcope2ilon[-1]+1][0], 1,2).ravel()
      # ~> the elements you wish to keep
      MASK2 = self.slf2d.IKLE3[np.where( np.sum(np.in1d(self.slf2d.IKLE3,np.compress( var > -99, np.arange(len(var)) )).reshape(self.slf2d.NELEM2,self.slf2d.NDP2),axis=1) == 3 )]

      self.slf2d.NELEM2 = len(MASK2)
      self.slf2d.NELEM3 = self.slf2d.NELEM2
      self.slf3d.NELEM2 = self.slf2d.NELEM2
      self.slf3d.NELEM3 = self.slf3d.NELEM2 * ( self.slf3d.NPLAN-1 )

      # ~~> re-numbering IKLE2 as a local connectivity matrix
      KNOLG,indices = np.unique( np.ravel(MASK2), return_index=True )
      KNOGL = dict(zip( KNOLG,range(len(KNOLG)) ))
      self.MASK2 = np.in1d( np.arange(len(var)),KNOLG )
      self.MASK3 = np.tile(self.MASK2,self.slf3d.NPLAN)
      self.slf2d.IKLE2 = - np.ones_like(MASK2,dtype=np.int)
      for k in range(len(MASK2)):
         self.slf2d.IKLE2[k] = [ KNOGL[MASK2[k][0]], KNOGL[MASK2[k][1]], KNOGL[MASK2[k][2]] ]

      self.slf3d.NPOIN2 = len(KNOLG)
      self.slf3d.NPOIN3 = self.slf3d.NPOIN2 * self.slf3d.NPLAN
      self.slf2d.NPOIN2 = self.slf3d.NPOIN2
      self.slf2d.NPOIN3 = self.slf2d.NPOIN2

      # ~~> re-connecting the upper floors
      self.slf2d.IKLE3 = self.slf2d.IKLE2
      self.slf3d.IKLE2 = self.slf2d.IKLE2
      self.slf3d.IKLE3 = \
         np.repeat(self.slf2d.NPOIN2*np.arange(self.slf3d.NPLAN-1),self.slf2d.NELEM2*self.slf3d.NDP3).reshape((self.slf2d.NELEM2*(self.slf3d.NPLAN-1),self.slf3d.NDP3)) + \
         np.tile(np.add(np.tile(self.slf2d.IKLE2,2),np.repeat(self.slf2d.NPOIN2*np.arange(2),self.slf3d.NDP2)),(self.slf3d.NPLAN-1,1))

      # ~~~~ Boundaries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.slf2d.IPOB2 = IPOB2[self.MASK2]
      self.slf2d.IPOB3 = self.slf2d.IPOB2
      self.slf3d.IPOB2 = self.slf2d.IPOB2
      self.slf3d.IPOB3 = np.ravel(np.add(np.repeat(self.slf2d.IPOB2,self.slf3d.NPLAN).reshape((self.slf2d.NPOIN2,self.slf3d.NPLAN)),self.slf2d.NPOIN2*np.arange(self.slf3d.NPLAN)).T)

      # ~~~~ Mesh ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Set SELAFIN mesh'
      self.slf3d.MESHX = np.tile(x,NY1D).reshape(NY1D,NX1D).T.ravel()[self.MASK2] + 0.042
      self.slf3d.MESHY = np.tile(y,NX1D)[self.MASK2] + 0.042
      self.slf2d.MESHX = self.slf3d.MESHX
      self.slf2d.MESHY = self.slf3d.MESHY

   def putContent(self,rootName,only2D):

      nbar = 0
      for e in self.experiments: nbar += len(e[2])
      ilat = [ self.jcope2ilat[0], self.jcope2ilat[-1]+1 ]
      ilon = [ self.jcope2ilon[0], self.jcope2ilon[-1]+1 ]

      # ~~~~ Time records ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Extract JCOPE2 time records'
      self.slf3d.tags = { 'times':[] }
      # ~~~~ Start Date and Time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.slf3d.tags['times'] = 86400.0 * np.arange(nbar)
      self.slf2d.tags = { 'times':self.slf3d.tags['times'] }
      self.slf3d.DATETIME = self.experiments[-1][3][0].timetuple()[0:6]
      self.slf2d.DATETIME = self.slf3d.DATETIME
      self.slf3d.IPARAM[9] = 1
      self.slf2d.IPARAM[9] = 1

      #a = np.arange(40).reshape(5,8)[::-1].ravel()  # 5 plans, 8 points
      print '     +> Write SELAFIN headers'
      if not only2D:
         self.slf3d.fole = {}
         self.slf3d.fole.update({ 'hook': open('t3d_'+rootName,'wb') })
         self.slf3d.fole.update({ 'name': 't3d_'+rootName })
         self.slf3d.fole.update({ 'endian': ">" })     # big endian
         self.slf3d.fole.update({ 'float': ('f',4) })  # single precision
         self.slf3d.appendHeaderSLF()
      self.slf2d.fole = {}
      self.slf2d.fole.update({ 'hook': open('t2d_'+rootName,'wb') })
      self.slf2d.fole.update({ 'name': 't2d_'+rootName })
      self.slf2d.fole.update({ 'endian': ">" })     # big endian
      self.slf2d.fole.update({ 'float': ('f',4) })  # single precision
      self.slf2d.appendHeaderSLF()
      # ~~~~ Time loop(s) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #var3d = np.zeros(self.slf3d.NPOIN3,dtype=np.float)
      #var2d = np.zeros(self.slf2d.NPOIN3,dtype=np.float)
      print '     +> Write SELAFIN cores'
      ibar = 0; pbar = ProgressBar(maxval=6*nbar).start()
      for e in self.experiments:
         jcope2data = e[0]
         i1 = min(e[2]); i2 = max(e[2])+1
         for t in range(i1,i2):
            # ~~> time stamp
            pbar.write('        x '+str(e[3][t-i1]),6*ibar+0)
            pbar.update(6*ibar+0)
            if not only2D: self.slf3d.appendCoreTimeSLF(ibar)
            self.slf2d.appendCoreTimeSLF(ibar)

            # ~~> ELEVATION
            var2d = np.swapaxes( jcope2data['el']['el'].data[t,0,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2).ravel()[self.MASK2]
            self.slf2d.appendCoreVarsSLF([var2d])
            if not only2D:
               var3d = - np.tile(self.ZPLAN,self.slf3d.NPOIN2).reshape(self.slf3d.NPOIN2,self.slf3d.NPLAN).T.ravel()
               var3d[self.slf3d.NPOIN3-self.slf3d.NPOIN2:] = var2d
               self.slf3d.appendCoreVarsSLF([var3d])
            pbar.write('             - elevation',6*ibar+1)
            pbar.update(6*ibar+1)

            # ~~> SALINITY
            if only2D: var = np.swapaxes( jcope2data['salt']['salt'].data[t,0:1,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            else: var = np.swapaxes( jcope2data['salt']['salt'].data[t,0:self.slf3d.NPLAN,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            var2d = var[0].ravel()[self.MASK2]
            self.slf2d.appendCoreVarsSLF([var2d])
            if not only2D:
               var3d = var[::-1].ravel()[self.MASK3]
               for ipoin in range(self.slf3d.NPOIN2):
                  for iplan in range(self.slf3d.NPLAN-1,0,-1):
                     if var3d[ipoin+(iplan-1)*self.slf3d.NPOIN2] < -99.0: var3d[ipoin+(iplan-1)*self.slf3d.NPOIN2] = var3d[ipoin+iplan*self.slf3d.NPOIN2]
               self.slf3d.appendCoreVarsSLF([var3d])
               pbar.write('             - salinity',6*ibar+2)
            pbar.update(6*ibar+2)

            # ~~> TEMPERATURE
            if only2D: var = np.swapaxes( jcope2data['temp']['temp'].data[t,0:1,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            else: var = np.swapaxes( jcope2data['temp']['temp'].data[t,0:self.slf3d.NPLAN,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            var2d = var[0].ravel()[self.MASK2]
            self.slf2d.appendCoreVarsSLF([var2d])
            if not only2D:
               var3d = var[::-1].ravel()[self.MASK3]
               for ipoin in range(self.slf3d.NPOIN2):
                  for iplan in range(self.slf3d.NPLAN-1,0,-1):
                     if var3d[ipoin+(iplan-1)*self.slf3d.NPOIN2] < -99.0: var3d[ipoin+(iplan-1)*self.slf3d.NPOIN2] = var3d[ipoin+iplan*self.slf3d.NPOIN2]
               self.slf3d.appendCoreVarsSLF([var3d])
               pbar.write('             - temperature',6*ibar+3)
            pbar.update(6*ibar+3)

            # ~~> VELOCITY U
            if only2D: var = np.swapaxes( jcope2data['u']['u'].data[t,0:1,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            else: var = np.swapaxes( jcope2data['u']['u'].data[t,0:self.slf3d.NPLAN,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            var2d = var[0].ravel()[self.MASK2]
            self.slf2d.appendCoreVarsSLF([var2d])
            if not only2D:
               var3d = var[::-1].ravel()[self.MASK3]
               for ipoin in range(self.slf3d.NPOIN2):
                  for iplan in range(self.slf3d.NPLAN-1,0,-1):
                     if var3d[ipoin+(iplan-1)*self.slf3d.NPOIN2] < -99.0: var3d[ipoin+(iplan-1)*self.slf3d.NPOIN2] = var3d[ipoin+iplan*self.slf3d.NPOIN2]
               self.slf3d.appendCoreVarsSLF([var3d])
            pbar.write('             - u-velocity',6*ibar+4)
            pbar.update(6*ibar+4)

            # ~~> VELOCITY V
            if only2D: var = np.swapaxes( jcope2data['v']['v'].data[t,0:1,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            else: var = np.swapaxes( jcope2data['v']['v'].data[t,0:self.slf3d.NPLAN,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            var2d = var[0].ravel()[self.MASK2]
            self.slf2d.appendCoreVarsSLF([var2d])
            if not only2D:
               var3d = var[::-1].ravel()[self.MASK3]
               for ipoin in range(self.slf3d.NPOIN2):
                  for iplan in range(self.slf3d.NPLAN-1,0,-1):
                     if var3d[ipoin+(iplan-1)*self.slf3d.NPOIN2] < -99.0: var3d[ipoin+(iplan-1)*self.slf3d.NPOIN2] = var3d[ipoin+iplan*self.slf3d.NPOIN2]
               self.slf3d.appendCoreVarsSLF([var3d])
            pbar.write('             - v-velocity',6*ibar+5)
            pbar.update(6*ibar+5)

            # ~~> VELOCITY W
            if not only2D:
               var3d = 0. * var3d
               self.slf3d.appendCoreVarsSLF([var3d])

            ibar += 1

      pbar.finish()
      if not only2D: self.slf3d.fole['hook'].close()
      self.slf2d.fole['hook'].close()

   def __del__(self): pass


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$13-July-2014 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n'+72*'~'+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Download JCOPE2 data into a SELAFIN file\n
Example 1: Extract about 80 days of 2D results only from January 2, 1993.
> convertJCOPE2.py --from 1993-01-02 --stop 1993-03-25 --bl 34,140 --tr 41,147 -r jcope2-80d.slf --2d\n
Example 2: Extract 4 months from both 2D and 3D dataset
> convertJCOPE2.py --from 1993-01-02 --stop 1993-05-02 --bl 34,140 --tr 41,147 -r jcope2-4m1993.slf
> convertJCOPE2.py --from 1993-01-02 --stop 1993-05-02 --bl 10,108 --tr 62,180 -r visu-jcope2-4m1993.slf
      '''))
   parser.add_argument(\
      "-r", "--root",dest="rootName",default='jcope2.slf',required=True,
      help="root name used for the output" )
   parser.add_argument(\
      "-f", "--from",dest="tfrom",default=None,required=True,
      help="specify the first date included (1972-13-07)" )
   parser.add_argument(\
      "-s", "--stop",dest="tstop",default=None,required=True,
      help="specify the last date included (1980-12-31)" )
   parser.add_argument(\
      "--bl",dest="blcorner",default=None,required=True,
      help="specify the bottom left corner (30,130)" )
   parser.add_argument(\
      "--tr",dest="trcorner",default=None,required=True,
      help="specify the top right corner (64,170)" )
   parser.add_argument(\
      "--2d",action="store_true",dest="t2d",default=False,
      help="if there, produces on the 2D file" )
   options = parser.parse_args()

   # Arbitrary 6-day period
   period = [[],[]]
   if options.tfrom != None:
      for i in options.tfrom.split('-'): period[0].append(int(i))
   else:
      print '... could not find your from date. Please use --from option (- delimited, no spaces).\n\n'
      sys.exit(1)
   if options.tstop != None:
      for i in options.tstop.split('-'): period[1].append(int(i))
   else:
      print '... could not find your stop date. Please use --stop option (- delimited, no spaces).\n\n'
      sys.exit(1)

   # arbitrary box (small pieve of the atlantic side of Mexico)
   modelbox = [[],[]]
   if options.blcorner != None:
      for i in options.blcorner.split(','): modelbox[0].append(int(i))
   else:
      print '... could not find your bounding box bottom left corner. Please use --bl option (, delimited, no spaces).\n\n'
      sys.exit(1)
   if options.trcorner != None:
      for i in options.trcorner.split(','): modelbox[1].append(int(i))
   else:
      print '... could not find your bounding box top right corner. Please use --tr option (, delimited, no spaces).\n\n'
      sys.exit(1)

   # rootName
   rootName = options.rootName

   # 2D file only
   only2D = options.t2d

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Convert to SELAFIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '\nProcessing header (mesh, connectivity, etc.)\n'
   jc2slf = JCOPE2(period)
   jc2slf.getHeaderJCOPE2(modelbox)

   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '\nProcessing core variables (time record, variables, etc.)\n'
   tic = time.time()
   print '\nExtraction start time:   '+time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(tic))
   jc2slf.putContent(rootName,only2D)
   toc = time.time()
   print '\nExtraction end time:     '+time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(toc))
   print '___________\nDuration:     '+str(int(toc-tic))+' seconds\n'

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
