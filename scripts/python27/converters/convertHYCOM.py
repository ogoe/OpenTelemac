"""@author Juliette C.E. Parisi and Sebastien E. Bourban
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
   Tools for handling conversions to-from HYCOM server files
"""
"""@details
   Contains server read functions to convert to SELAFIN file
"""
"""@history 30/04/2011 -- Laure C. Grignon
   First attempt to extract HYCOM data and dump it into an ASCII file
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
import time
from datetime import datetime
import numpy as np
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
from config import OptionParser
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
#    91.0 (21-Aug-2013 to Present)
#    => url = 'http://tds.hycom.org/thredds/dodsC/GLBa0.08/expt_91.0'
#    90.9 (3-Jan-2011 to 20-Aug-2013)
#    => url = 'http://tds.hycom.org/thredds/dodsC/GLBa0.08/expt_90.9'
#    90.8 (7-May-2009 to 2-Jan-2011)
#    => url = 'http://tds.hycom.org/thredds/dodsC/GLBa0.08/expt_90.8'
#    90.6 (18-Sep-2008 to 6-May-2009)
#    => url = 'http://tds.hycom.org/thredds/dodsC/GLBa0.08/expt_90.6'
# Legacy Experiments
#    74.2 (Jun-2007 to Oct-2008)
#    90.3 (Apr-2007 to Sep-2008)
#    90.2 (Jan-2007 to Apr-2007)
#    60.5 (Nov-2003 to Dec-2006)

class HYCOM():


   def __init__(self,dates):

      # ~~~~ Initialisation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.moddates = [ datetime(*dates[0]),datetime(*dates[1]) ]

      # ~~~~ Time records ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Extract HYCOM time records\n'
      hycomurls = [ \
         'http://tds.hycom.org/thredds/dodsC/GLBa0.08/expt_91.0', \
         'http://tds.hycom.org/thredds/dodsC/GLBa0.08/expt_90.9', \
         'http://tds.hycom.org/thredds/dodsC/GLBa0.08/expt_90.8', \
         'http://tds.hycom.org/thredds/dodsC/GLBa0.08/expt_90.6' 
            ]
      self.experiments = []
      for hycomurl in hycomurls:
         hycomdata = open_url(hycomurl)
         NIT = hycomdata['Date'].shape[0]
         print '        x '+str(NIT)+' records from '+hycomurl,
         ITs = []
         ATs = []
         for hycomdate,itime in zip(hycomdata['Date'][0:NIT],range(NIT)):
            d = datetime(int(str(hycomdate)[0:4]),int(str(hycomdate)[4:6]),int(str(hycomdate)[6:8]))
            if itime == 0: print ' from: ',str(d),
            if itime == NIT-1: print ' to: ',str(d)
            if self.moddates[0] <= d and d <= self.moddates[1]:
               ITs.append(itime)
               ATs.append(d)
         if ITs != []: self.experiments.append((hycomdata,NIT,ITs,ATs))
      print '\n'


   def getHeaderHYCOM(self,bounds):

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
         'G/L             ','DEGREES         ', \
         'M/S             ','M/S             ','M/S             ']
      self.slf2d.TITLE = self.slf3d.TITLE
      self.slf2d.NBV1 = self.slf3d.NBV1 + 1; self.slf2d.NVAR = self.slf3d.NVAR + 1
      self.slf2d.VARINDEX = range(self.slf2d.NVAR)
      self.slf2d.VARNAMES = self.slf3d.VARNAMES[0:-1]
      self.slf2d.VARNAMES.append('EMP             ')
      self.slf2d.VARNAMES.append('QTOT            ')
      self.slf2d.VARUNITS = self.slf3d.VARUNITS[0:-1]
      self.slf2d.VARUNITS.append('???             ')
      self.slf2d.VARUNITS.append('???             ')
      # ~~> server access,
      #     get the grid and header from the latest experiment
      self.hycomdata = self.experiments[0][0]

      # ~~~~ Grid coordinates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # ~~> the whole of the 2D grid sizes
      print '     +> Extract HYCOM sizes'
      NX1D = self.hycomdata['X'].shape[0]
      NY1D = self.hycomdata['Y'].shape[0]
      print '     +> Extract HYCOM mesh'
      lonX1D = self.hycomdata['Longitude']['Longitude'].data[0,0:NX1D].ravel()%360
      latY1D = self.hycomdata['Latitude']['Latitude'].data[0:NY1D,0].ravel()
      # ~~> lat,lon correction
      for i in range(NX1D):
         if( lonX1D[i] > 180 ): lonX1D[i] = lonX1D[i] - 360.0
      for i in range(2172,NY1D):
         latY1D[i] = 47.0 + ( i-2172 )/18.0
      # ~~> subset for the SELAFIN
      print '     +> Set SELAFIN mesh'
      self.hycomilon = np.where(( lonX1D >= bounds[0][1] )*( lonX1D <= bounds[1][1] ))[0]
      self.hycomilat = np.where(( latY1D >= bounds[0][0] )*( latY1D <= bounds[1][0] ))[0]
      x = lonX1D[ self.hycomilon ]
      y = latY1D[ self.hycomilat ]
      NX1D = len(x)
      NY1D = len(y)

      # ~~~~ MESH sizes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Set SELAFIN sizes'
      # ~~> 3D
      self.slf3d.NPLAN = self.hycomdata['Depth'].shape[0]
      self.ZPLAN = self.hycomdata['Depth'][0:self.slf3d.NPLAN][::-1] # I do not know any other way
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

      print '     +> Set SELAFIN mesh'
      self.slf3d.MESHX = np.tile(x,NY1D).reshape(NY1D,NX1D).T.ravel()
      self.slf3d.MESHY = np.tile(y,NX1D)
      self.slf2d.MESHX = self.slf3d.MESHX[0:self.slf2d.NPOIN2]
      self.slf2d.MESHY = self.slf3d.MESHY[0:self.slf2d.NPOIN2]

      # ~~~~ Connectivity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Set SELAFIN IKLE'
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
      self.slf3d.IPOB3 = np.zeros(self.slf3d.NPOIN3,dtype=np.int)
      # ~~> along the x-axis (lon)
      for i in range(NX1D):
         for k in range(1,self.slf3d.NPLAN+1):
            ipoin = i*NY1D  + (k-1)*(2*NX1D+2*NY1D-4) 
            self.slf3d.IPOB3[ipoin] = i + 1 + (k-1)*(2*NX1D+2*NY1D-4)
            ipoin = i*NY1D -1 + (k-1)*(2*NX1D+2*NY1D-4)
            self.slf3d.IPOB3[ipoin] = 2*NX1D+(NY1D-2) - i  + (k-1)*(2*NX1D+2*NY1D-4)
         pbar.update(i)
      # ~~> along the y-axis (alt)
      for i in range(1,NY1D):
         for k in range(1,self.slf3d.NPLAN+1):
            ipoin = i + (k-1)*(2*NX1D+2*NY1D-4)
            self.slf3d.IPOB3[ipoin] = 2*NX1D + 2*(NY1D-2) -i + 1 + (k-1)*(2*NX1D+2*NY1D-4)
            ipoin = NY1D*(NX1D-1) + i + (k-1)*(2*NX1D+2*NY1D-4)
            self.slf3d.IPOB3[ipoin] = NX1D + i + (k-1)*(2*NX1D+2*NY1D-4)
         pbar.update(i+NX1D)
      pbar.finish()
      self.slf2d.IPOB3 = self.slf3d.IPOB3[0:self.slf3d.NPOIN2]
      

   def putContent(self,rootName,only2D):

      nbar = 0
      for e in self.experiments: nbar += len(e[2])
      ilat = [ self.hycomilat[0], self.hycomilat[-1]+1 ]
      ilon = [ self.hycomilon[0], self.hycomilon[-1]+1 ]

      # ~~~~ Time records ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      print '     +> Extract HYCOM time records'
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
      var3d = np.zeros(self.slf3d.NPOIN3,dtype=np.float)
      var2d = np.zeros(self.slf2d.NPOIN3,dtype=np.float)
      print '     +> Write SELAFIN cores'
      ibar = 0; pbar = ProgressBar(maxval=10*nbar).start()
      for e in self.experiments[::-1]:
         hycomdata = e[0]
         i1 = min(e[2]); i2 = max(e[2])+1
         for t in range(i1,i2):
            # ~~> time stamp
            pbar.write('        x '+str(e[3][t-i1]),10*ibar+0)
            pbar.update(10*ibar+0)
            if not only2D: self.slf3d.appendCoreTimeSLF(ibar)
            self.slf2d.appendCoreTimeSLF(ibar)

            # ~~> HYCOM variable extraction ( 1L:times, 33L:layers, yyL:NY1D, xxL:NX1D )

            # ~~> ELEVATION
            v2d = np.swapaxes( hycomdata['ssh']['ssh'].data[t,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 0,1).ravel()
            var2d = np.where( v2d < 10000, v2d, 0.0 )
            self.slf2d.appendCoreVarsSLF([var2d])
            if not only2D:
               var3d = - np.tile(self.ZPLAN,self.slf3d.NPOIN2).reshape(self.slf3d.NPOIN2,self.slf3d.NPLAN).T.ravel()
               self.slf3d.appendCoreVarsSLF([var3d])
            pbar.write('             - ssh',10*ibar+1)
            pbar.update(10*ibar+1)

            # ~~> SALINITY
            v2d = np.swapaxes( hycomdata['surface_salinity_trend']['surface_salinity_trend'].data[t,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 0,1).ravel()
            var2d = np.where( v2d < 10000, v2d, 0.0 )
            self.slf2d.appendCoreVarsSLF([var2d])
            pbar.write('             - surface_salinity_trend',10*ibar+2)
            pbar.update(10*ibar+2)
            if not only2D:
               var = np.swapaxes( hycomdata['salinity']['salinity'].data[t,0:self.slf3d.NPLAN,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
               v3d = var[::-1].ravel()
               var3d = np.where( v3d < 10000, v3d, 0.0 )
               self.slf3d.appendCoreVarsSLF([var3d])
               pbar.write('             - salinity',10*ibar+3)
            pbar.update(10*ibar+3)

            # ~~> TEMPERATURE
            v2d = np.swapaxes( hycomdata['surface_temperature_trend']['surface_temperature_trend'].data[t,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 0,1).ravel()
            var2d = np.where( v2d < 10000, v2d, 0.0 )
            self.slf2d.appendCoreVarsSLF([var2d])
            pbar.write('             - surface_temperature_trend',10*ibar+4)
            pbar.update(10*ibar+4)
            if not only2D:
               var = np.swapaxes( hycomdata['temperature']['temperature'].data[t,0:self.slf3d.NPLAN,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
               v3d = var[::-1].ravel()
               var3d = np.where( v3d < 10000, v3d, 0.0 )
               self.slf3d.appendCoreVarsSLF([var3d])
               pbar.write('             - temperature',10*ibar+5)
            pbar.update(10*ibar+5)

            # ~~> VELOCITY U
            if only2D: var = np.swapaxes( hycomdata['u']['u'].data[t,0:1,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            else: var = np.swapaxes( hycomdata['u']['u'].data[t,0:self.slf3d.NPLAN,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            v2d = var[0].ravel()
            var2d = np.where( v2d < 10000, v2d, 0.0 )
            self.slf2d.appendCoreVarsSLF([var2d])
            if not only2D:
               v3d = var[::-1].ravel()
               var3d = np.where( v3d < 10000, v3d, 0.0 )
               self.slf3d.appendCoreVarsSLF([var3d])
            pbar.write('             - u-velocity',10*ibar+6)
            pbar.update(10*ibar+6)

            # ~~> VELOCITY V
            if only2D: var = np.swapaxes( hycomdata['v']['v'].data[t,0:1,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            else: var = np.swapaxes( hycomdata['v']['v'].data[t,0:self.slf3d.NPLAN,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 1,2)
            v2d = var[0].ravel()
            var2d = np.where( v2d < 10000, v2d, 0.0 )
            self.slf2d.appendCoreVarsSLF([var2d])
            if not only2D:
               v3d = var[::-1].ravel()
               var3d = np.where( v3d < 10000, v3d, 0.0 )
               self.slf3d.appendCoreVarsSLF([var3d])
            pbar.write('             - v-velocity',10*ibar+7)
            pbar.update(10*ibar+7)

            # ~~> VELOCITY W
            if not only2D:
               var3d = 0. * var3d
               self.slf3d.appendCoreVarsSLF([var3d])

            # ~~> EMP ???
            v2d = np.swapaxes( hycomdata['emp']['emp'].data[t,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 0,1).ravel()
            var2d = np.where( v2d < 10000, v2d, 0.0 )
            self.slf2d.appendCoreVarsSLF([var2d])
            pbar.write('             - emp',10*ibar+8)
            pbar.update(10*ibar+8)

            # ~~> TEMPERATURE
            v2d = np.swapaxes( hycomdata['qtot']['qtot'].data[t,ilat[0]:ilat[1],ilon[0]:ilon[1]][0], 0,1).ravel()
            var2d = np.where( v2d < 10000, v2d, 0.0 )
            self.slf2d.appendCoreVarsSLF([var2d])
            pbar.write('             - qtot',10*ibar+9)
            pbar.update(10*ibar+9)
            
            ibar += 1

      pbar.finish()
      if not only2D: self.slf3d.fole['hook'].close()
      self.slf2d.fole['hook'].close()

   def __del__(self): pass


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Juliette C.E. Parisi"
__date__ ="$13-Nov-2013 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-r", "--root",type="string",dest="rootName",default='hycom.slf',help="root name used for the output" )
   parser.add_option("-f", "--from",type="string",dest="tfrom",default=None,help="specify the first date included (1972-13-07)" )
   parser.add_option("-s", "--stop",type="string",dest="tstop",default=None,help="specify the last date included (1980-12-31)" )
   parser.add_option("--bl",type="string",dest="blcorner",default=None,help="specify the bottom left corner (25,-117)" )
   parser.add_option("--tr",type="string",dest="trcorner",default=None,help="specify the top right corner (27,-110)" )
   parser.add_option("--2d",action="store_true",dest="t2d",default=False,help="if there, produces on the 2D file" )
   options, args = parser.parse_args()

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
   hy2slf = HYCOM(period)
   hy2slf.getHeaderHYCOM(modelbox)

   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '\nProcessing core variables (time record, variables, etc.)\n'
   tic = time.time()
   print '\nExtraction start time:   '+time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(tic))
   hy2slf.putContent(rootName,only2D)
   toc = time.time()
   print '\nExtraction end time:     '+time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(toc))
   print '___________\nDuration:     '+str(int(toc-tic))+' seconds\n'

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
