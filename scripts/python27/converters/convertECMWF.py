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
   Tools for handling conversions to-from ECMWF server files
"""
"""@details
   Contains server read functions to convert to SELAFIN file
"""
"""@history 12/12/2014 -- Sebastien E. Bourban
   Complete write-up of the script to produce 2D SELAFIN files.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
from datetime import datetime
import time
import numpy as np
from scipy.io import netcdf
import urllib
import urllib2
import httplib
import traceback
from argparse import ArgumentParser,RawDescriptionHelpFormatter
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
from parsers.parserSELAFIN import SELAFIN
from utils.progressbar import ProgressBar

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#
# When using the following user ID to access the ECMWF computing
#    facilities it is with the understanding that you acknowledge
#    the terms and conditions detailed at:
#    http://www.ecmwf.int/en/forecasts/software-and-tools
#
# username: s.bourban@hrwallingford.com
# password: Lxtc14
#
config = {
    "url"   : "https://api.ecmwf.int/v1",
    "key"   : "70f6a4499dddb7d17545f9bd3cf5ef3f",
    "email" : "s.bourban@hrwallingford.com",
    "password"   : "Lxtc14",
}
# ECMWF Re-Analysis, Keys to retrieve requests
#
#  'dataset' (those if general license), "interim" being the default:
#     era15  - ECMWF Global Reanalysis Data - ERA-15 (Jan 1979 - Dec 1993)
#     era20c  -	Reanalysis of the 20th-century using surface observations only (Jan 1900 - Dec 2010)
#     era20cmv0  -	ERA-20CM: Ensemble of climate model integrations (Experimental version)
#     era40  -	ECMWF Global Reanalysis Data - ERA-40 (Sep 1957 - Aug 2002)
#     eraclim  -	ERA-20CM: Ensemble of climate model integrations
#     icoads  -	ICOADS v2.5.1 with interpolated 20CR feedback
#     interim  -	ECMWF Global Reanalysis Data - ERA Interim (Jan 1979 - present)
#     ispd  -	ISPD v2.2
#     yotc  -	YOTC (Year of Tropical Convection)
#  'step', "6" being the default:
#     "24/to/120/by/24", ...
#  'number'  : "all",
#  'levtype' : "sl",
#  'date'    : "20071001",
#  'time'    : "00",
#  'origin'  : "all",
#  'type'    : "pf",
#  'param'   : "tp",
#  'area'    : "70/-130/30/-60",
#  'grid'    : "2/2",
#  'target'  : "data.grib" # wil not be used anymore ... since directly into SELAFIN
#

# _____                  ___________________________________________
# ____/ API Classes /__________________________________________/
#
# (C) Copyright 2012-2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.
#

try:
   import json
except:
   import simplejson as json

class RetryError(Exception):
   def __init__(self, code, text):
      self.code = code
      self.text = text
   def __str__(self): return "%d %s" % (self.code, self.text)

class APIException(Exception):
   def __init__(self, value):
      self.value = value
   def __str__(self): return repr(self.value)

def robust(func):

   def wrapped(*args,**kwargs):
      tries = 0
      while True:
         try:
            return func(*args,**kwargs)
         except urllib2.HTTPError, e:
            print "WARNING: httplib2.HTTPError received %s" % (e)
            if e.code < 500: raise
            tries += 1
            if tries > 10: raise
            time.sleep(60)
         except httplib.BadStatusLine, e:
            print "WARNING: httplib.BadStatusLine received %s" % (e)
            tries += 1
            if tries > 10: raise
            time.sleep(60)
         except urllib2.URLError, e:
            print "WARNING: httplib2.URLError received %s %s" % (e.errno, e)
            tries += 1
            if tries > 10: raise
            time.sleep(60)
         except APIException:
            raise
         except RetryError, e:
            print "WARNING: HTTP received %s" % (e.code)
            print e.text
            tries += 1
            if tries > 10: raise
            time.sleep(60)
         except:
            print "Unexpected error:", sys.exc_info()[0]
            print traceback.format_exc()
            raise

   return wrapped

SAY = True
class Ignore303(urllib2.HTTPRedirectHandler):

   def redirect_request(self, req, fp, code, msg, headers, newurl):
      if code in [301, 302]:
         # We want the posts to work even if we are redirected
         if code == 301:
            global SAY, URL
            if SAY:
               o = req.get_full_url()
               n = newurl
               while o != URL and len(o) and len(n) and o[-1] == n[-1]:
                  o = o[0:-1]
                  n = n[0:-1]
               print
               print "*** ECMWF API has moved"
               print "***   OLD: %s" % o
               print "***   NEW: %s" % n
               print "*** Please update your ~/.ecmwfapirc file"
               print
               SAY = False
         data = None
         if req.has_data(): data = req.get_data()
         return urllib2.Request(newurl, data=data, headers = req.headers,
                                origin_req_host=req.get_origin_req_host(), unverifiable=True)
      return None

   def http_error_303(self, req, fp, code, msg, headers):
      infourl = urllib.addinfourl(fp, headers, req.get_full_url())
      infourl.status = code
      infourl.code = code
      return infourl

class Connection(object):

   def __init__(self, email = None, key = None, verbose = False, quiet = False):
      self.email    = email
      self.key      = key
      self.retry    = 5
      self.location = None
      self.done     = False
      self.value    = True
      self.offset   = 0
      self.verbose  = verbose
      self.quiet    = quiet
      self.status   = None

   @robust
   def call(self, url, payload = None, method = "GET"):

      if self.verbose: print method, url

      headers = { "Accept" : "application/json", "From" : self.email, "X-ECMWF-KEY" : self.key }

      opener = urllib2.build_opener(Ignore303)

      data = None
      if payload is not None:
         data = json.dumps(payload)
         data.encode('utf-8')
         headers["Content-Type"] = "application/json";

      url = "%s?offset=%d&limit=500" % (url, self.offset)
      req = urllib2.Request(url=url, data=data, headers=headers)
      if method:
         req.get_method = lambda: method

      error = False
      try:
         try:
            res  = opener.open(req)
         except urllib2.HTTPError,e:
            # It seems that some version of urllib2 are buggy
            if e.code <= 299: res = e
            else: raise
      except urllib2.HTTPError,e:
         print e
         error = True
         res   = e
         # 502: Proxy Error
         # 503: Service Temporarily Unavailable
         if e.code >= 500: raise RetryError(e.code, e.read())

      self.retry    = int(res.headers.get("Retry-After", self.retry))
      code          = res.code
      if code in [201, 202]: self.location = res.headers.get("Location",    self.location)

      if self.verbose:
         print "Code", code
         print "Content-Type", res.headers.get("Content-Type")
         print "Content-Length", res.headers.get("Content-Length")
         print "Location", res.headers.get("Location")

      body = res.read()
      res.close()

      if code in [204]:
         self.last = None
         return None
      else:
         try:
            self.last  =  json.loads(body)
         except Exception, e:
            self.last = { "error" : "%s: %s" % (e, body) }
            error = True

      if self.verbose: print json.dumps(self.last,indent=4)

      self.status = self.last.get("status", self.status)

      if self.verbose: print "Status", self.status

      if "messages" in self.last:
         for n in self.last["messages"]:
            if not self.quiet: print n
            self.offset += 1

      if code == 200 and self.status == "complete":
         self.value = self.last
         self.done  = True
         if isinstance(self.value, dict) and "result" in self.value: self.value = self.value["result"]

      if code in [303]:
         self.value = self.last
         self.done  = True

      if "error" in self.last: raise APIException("ecmwf.API error 1: %s" % (self.last["error"],) )

      if error: raise APIException("ecmwf.API error 2: %s" % (res, ) )

      return self.last

   def submit(self, url, payload): self.call(url, payload, "POST")

   def POST(self, url, payload): return self.call(url, payload, "POST")

   def GET(self, url): return self.call(url, None, "GET")

   def wait(self):
      if self.verbose: print "Sleeping %s second(s)" % (self.retry)
      time.sleep(self.retry)
      self.call(self.location, None, "GET")

   def ready(self): return self.done

   def result(self): return self.value

   def cleanup(self):
      try:
         if self.location: self.call(self.location, None, "DELETE")
      except:
         pass

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#

class ECMWF():

   def __init__(self,dataset,dates,request):

      # ~~> inheritence
      self.slf2d = SELAFIN('')     # surface
      self.slf2d.DATETIME = dates[0]
      self.typ = request['stream']

      # ~> Initialisation
      self.moddates = [ datetime(*dates[0]),datetime(*dates[1]) ]
      status = ''
      self.request = request

      # ~> Establish connection
      self.connection = Connection(config['email'], config['key'], quiet = True, verbose = False)
      # ~> Verify connection
      user = self.connection.call("%s/%s" % (config['url'], "who-am-i"))
      print '   ~> access through username: %s\n' % (user["full_name"] or "user '%s'" % user["uid"],)
      # ~> Request dataset
      self.connection.submit("%s/%s/requests" % (config['url'], dataset), request)
      status = self.connection.status
      print '   ~> request has been',status
      # ~> Wait for remote processing
      while not self.connection.ready():
         if status != self.connection.status:
            status = self.connection.status
            print '   ~> request remains',status,'...'
         self.connection.wait()
      # ~> Request completed
      print '   ~> request is now',self.connection.status
      self.connection.cleanup()

   def downloadECMWF(self):

      result = self.connection.result()
      fileName = self.request.get("target")

      # ~> tries connecting 3 times before stopping
      tries = 0
      while True:

         # ~> downloading file by blocks
         http = urllib2.urlopen(result["href"])
         f = open(fileName,"wb")
         ibar = 0; pbar = ProgressBar(maxval=result["size"]).start()
         while True:
            chunk = http.read(1024*1024)
            if not chunk: break
            f.write(chunk)
            ibar += len(chunk)
            pbar.update(ibar)
         f.flush()
         f.close()
         pbar.finish()
         # ~> have I got everything ?
         if ibar == result["size"]: break
         if tries == 3:
            print "    ... exhausted the number of download trials.\nYou may wish to attempt this again later."
            sys.exit()
         print "    ... trying to download the data once more ..."
         tries += 1

   def appendHeaderECMWF(self,ecmwfdata):

      # ~~> variables
      self.slf2d.TITLE = ''
      self.slf2d.NBV1 = len(ecmwfdata.variables) - 3 # less longitude, latitude and time
      self.slf2d.NVAR = self.slf2d.NBV1
      self.slf2d.VARINDEX = range(self.slf2d.NVAR)
      if self.typ == 'wave':
         self.slf2d.VARNAMES = ['WAVE HEIGHT     ', \
            'WAVE PERIOD     ','WAVE DIRECTION  ']
         self.slf2d.VARUNITS = ['M               ', \
            'S               ','DEGREES         ']
      else:
         self.slf2d.VARNAMES = ['SURFACE PRESSURE', \
            'WIND VELOCITY U ','WIND VELOCITY V ', \
            'AIR TEMPERATURE ']
         self.slf2d.VARUNITS = ['UI              ', \
            'M/S             ','M/S             ', \
            'DEGREES         ']

      # ~~> 2D grid
      x = ecmwfdata.variables['longitude'][:]
      NX1D = len(x)
      y = ecmwfdata.variables['latitude'][:]
      NY1D = len(y)
      self.slf2d.MESHX = np.tile(x,NY1D).reshape(NY1D,NX1D).T.ravel()
      self.slf2d.MESHY = np.tile(y,NX1D)
      # ~~> lat,lon correction
      for i in range(NX1D):
         if( self.slf2d.MESHX[i] > 180 ): self.slf2d.MESHX[i] = self.slf2d.MESHX[i] - 360.0
      #for i in range(2172,NY1D):
      #   self.slf2d.MESHY[i] = 47.0 + ( i-2172 )/18.0
      self.slf2d.NPLAN = 1
      self.slf2d.NDP2 = 3
      self.slf2d.NDP3 = self.slf2d.NDP2
      self.slf2d.NPOIN2 = NX1D * NY1D
      self.slf2d.NPOIN3 = self.slf2d.NPOIN2
      self.slf2d.NELEM2 = 2*( NX1D-1 )*( NY1D-1 )
      self.slf2d.NELEM3 = self.slf2d.NELEM2
      self.slf2d.IPARAM = [ 0,0,0,0,0,0,            1,     0,0,0 ]

      # ~~> Connectivity
      ielem = 0; pbar = ProgressBar(maxval=self.slf2d.NELEM3).start()
      self.slf2d.IKLE3 = np.zeros((self.slf2d.NELEM3,self.slf2d.NDP3),dtype=np.int)
      for i in range(1,NX1D):
         for j in range(1,NY1D):
            ipoin = (i-1)*NY1D + j - 1
            # ~~> first triangle
            self.slf2d.IKLE3[ielem][0] = ipoin
            self.slf2d.IKLE3[ielem][1] = ipoin + NY1D
            self.slf2d.IKLE3[ielem][2] = ipoin + 1
            ielem = ielem + 1
            pbar.update(ielem)
            # ~~> second triangle
            self.slf2d.IKLE3[ielem][0] = ipoin + NY1D
            self.slf2d.IKLE3[ielem][1] = ipoin + NY1D + 1
            self.slf2d.IKLE3[ielem][2] = ipoin + 1
            ielem = ielem + 1
            pbar.update(ielem)
      pbar.finish()

      # ~~> Boundaries
      pbar = ProgressBar(maxval=NX1D+NY1D).start()
      self.slf2d.IPOB3 = np.zeros(self.slf2d.NPOIN3,dtype=np.int)
      # ~~> along the x-axis (lon)
      for i in range(NX1D):
         ipoin = i*NY1D
         self.slf2d.IPOB3[ipoin] = i + 1
         ipoin = i*NY1D -1
         self.slf2d.IPOB3[ipoin] = 2*NX1D+(NY1D-2) - i
         pbar.update(i)
      # ~~> along the y-axis (alt)
      for i in range(1,NY1D):
         ipoin = i
         self.slf2d.IPOB3[ipoin] = 2*NX1D + 2*(NY1D-2) -i + 1
         ipoin = NY1D*(NX1D-1) + i
         self.slf2d.IPOB3[ipoin] = NX1D + i
         pbar.update(i+NX1D)
      pbar.finish()

      # ~~~~ Time records ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ATs = ecmwfdata.variables['time'][:]
      self.slf2d.tags = { 'times': 3600 * ( ATs-ATs[0] ) } # time record in hours
      # self.slf2d.DATETIME = period[0] ... already set
      self.slf2d.appendHeaderSLF()

   def appendCoreTimeECMWF(self,t): self.slf2d.appendCoreTimeSLF(t)

   def appendCoreVarsECMWF(self,ecmwfdata,itime):
      # Note: this is how you get to the attributes ...
      # ecmwfdata.variables['sp'].ncattrs()
      # in particular ...
      # ecmwfdata.variables['sp'].units
      # ecmwfdata.variables['sp'].missing_value

      if self.type == 'wave':
         # ~~> SIGNIFICANT WAVE PERIOD == 'mwp'
         var2d = np.swapaxes( ecmwfdata.variables['mwp'][itime][:], 0,1).ravel()
         varof = ecmwfdata.variables['mwp'].add_offset
         varsf = ecmwfdata.variables['mwp'].scale_factor
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> MEAN WAVE DIRECTION == 'mwd'
         var2d = np.swapaxes( ecmwfdata.variables['mwd'][itime][:], 0,1).ravel()
         varof = ecmwfdata.variables['mwd'].add_offset
         varsf = ecmwfdata.variables['mwd'].scale_factor
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> MEAN WAVE DIRECTION == 'mwd'
         var2d = np.swapaxes( ecmwfdata.variables['mwd'][itime][:], 0,1).ravel()
         varof = ecmwfdata.variables['mwd'].add_offset
         varsf = ecmwfdata.variables['mwd'].scale_factor
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

      else:
         # ~~> SURFACE PRESSURE == 'sp'
         var2d = np.swapaxes( ecmwfdata.variables['sp'][itime][:], 0,1).ravel()
         varof = ecmwfdata.variables['sp'].add_offset
         varsf = ecmwfdata.variables['sp'].scale_factor
         #print ecmwfdata.variables['sp'].units
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> WIND VELOCITY U == 'u10'
         var2d = np.swapaxes( ecmwfdata.variables['u10'][itime][:], 0,1).ravel()
         varof = ecmwfdata.variables['u10'].add_offset
         varsf = ecmwfdata.variables['u10'].scale_factor
         #print ecmwfdata.variables['u10'].units
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> WIND VELOCITY V == 'v10'
         var2d = np.swapaxes( ecmwfdata.variables['v10'][itime][:], 0,1).ravel()
         varof = ecmwfdata.variables['v10'].add_offset
         varsf = ecmwfdata.variables['v10'].scale_factor
         #print ecmwfdata.variables['v10'].units
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> AIR TEMPERATURE == 't2m'
         var2d = np.swapaxes( ecmwfdata.variables['t2m'][itime][:], 0,1).ravel()
         varof = ecmwfdata.variables['t2m'].add_offset
         varsf = ecmwfdata.variables['t2m'].scale_factor
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof-273.15])  # Kelvin to Celsius

   def putContent(self,fileName,showbar=True):

      # ~~> netcdf reader
      ecmwfdata = netcdf.netcdf_file(self.request.get("target"), 'r')

      # ~~> new SELAFIN writer
      self.slf2d.fole = {}
      self.slf2d.fole.update({ 'hook': open(fileName,'wb') })
      self.slf2d.fole.update({ 'name': fileName })
      self.slf2d.fole.update({ 'endian': ">" })     # big endian
      self.slf2d.fole.update({ 'float': ('f',4) })  # single precision

      print '     +> Write SELAFIN header'
      self.appendHeaderECMWF(ecmwfdata)

      print '     +> Write SELAFIN core'
      ibar = 0
      if showbar: pbar = ProgressBar(maxval=len(self.slf2d.tags['times'])).start()
      for t in range(len(self.slf2d.tags['times'])):
         self.appendCoreTimeECMWF(t)
         self.appendCoreVarsECMWF(ecmwfdata,ibar)
         ibar += 1
         if showbar: pbar.update(ibar)
      self.slf2d.fole['hook'].close()
      if showbar: pbar.finish()


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$12-Dec-2014 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n'+72*'~'+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Download ECMWF data into a SELAFIN file\n
Examples 1:
> convertECMWF.py --from 2011-02-15 --stop 2011-06-15 --bl 34,140 --tr 41,147 ecmwf-4m.slf
      '''))
   parser.add_argument(\
      "rootName",default='',
      help="specify the root name of the resulting SELAFIN file." )
   parser.add_argument(\
      "-f", "--from",dest="tfrom",default=None,required=True,
      help="specify the first date included (1972-13-07)" )
   parser.add_argument(\
      "-s", "--stop",dest="tstop",default=None,required=True,
      help="specify the last date included (1980-12-31)" )
   parser.add_argument(\
      "--bl",dest="blcorner",default=None,required=True,
      help="specify the bottom left corner (25,-117)" )
   parser.add_argument(\
      "--tr",dest="trcorner",default=None,required=True,
      help="specify the top right corner (27,-110)" )
   parser.add_argument(\
     "--dataset",dest="dataset",default='oper',
     help="type of dataset requested either 'oper' (atmospheric) or 'wave' (waves), etc.")
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
   #if len(args) != 1:
   #   print '... only one file name is necessary to capture the processed dataset.\n\n'
   #   sys.exit(1)
   rootName = options.rootName #args[-1]
   head,tail = path.splitext(rootName)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Making a request ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   req = {
          'dataset' : "interim",
          'step'    : "0",
          'number'  : "all",
          'levtype' : "sfc",
          'date'    : "2011-02-15/to/2011-06-15",
          'time'    : "00/06/12/18",
          # 'origin'  : "all",
          'type'    : "an",
          'param'   : "134.128/165.128/166.128/167.128", # because these are ambigous: "ap/u10/v10/t2m"
          'area'    : "41/140/34/147",
          'grid'    : "0.125/0.125",
          'target'  : head+'.nc',
          'format'  : 'netcdf',
          'class'   : "ei",
          'stream'  : options.dataset
          }
   req['date'] = options.tfrom + '/to/' + options.tstop
   req['area'] = options.blcorner.replace(',','/') + '/' + options.trcorner.replace(',','/')
   if options.dataset == 'wave': req['param'] = "swh/mwp/mwd"
   else: req['param'] = "134.128/165.128/166.128/167.128"

   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '\nMaking an ECMWF request\n'
   ecmwf2slf = ECMWF("datasets/%s" % (req['dataset']),period,req)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Downloading the NetCDF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Unfortunately, I did not manage to access the NetCDF file remotely

   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '\nHaving to download the ECMWF file first\n'

   ecmwf2slf.downloadECMWF()
   print "   ~> download completed."

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Convert to SELAFIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   print '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '\nConverting netcdf file into SELAFIN\n'
   ecmwf2slf.putContent(rootName)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
