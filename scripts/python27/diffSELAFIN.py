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
         Reporting on differences between two SELAFIN files
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
from runSELAFIN import scanSELAFIN
from parsers.parserFortran import cleanQuotes

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                    _________________________________________
# ____/ Secondary Classes /________________________________________/
#

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$21-Sep-2016 13:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+72*'~'+'\n'
   parser = OptionParser("usage: %prog [options] file1.slf file2.slf\n ... use -h for more help.")
   # ~~> Uselessly set to True as default ... may change in the future
   # ~~> The real switches
   parser.add_option("--head",action="store_true",dest="head",default=False,help="Will print a statiscal differences between two SELARING files" )
   parser.add_option("--core",action="store_true",dest="core",default=False,help="Will print a statiscal differences between two SELARING files" )
   #parser.add_option("--full",action="store_true",dest="full",default=False,help="Will create pictures of the difference between files" )
   parser.add_option("--scan",action="store_true",dest="scan",default=False,help="Will print an individual summary for each file" )
   parser.add_option("-v", "--vars",type="string",dest="xvars",default=None,help="specify which variables should be differentiated (':'-delimited)" )
   parser.add_option("-f", "--from",type="string",dest="tfrom",default="1",help="specify the first frame included in the differentiation" )
   parser.add_option("-s", "--stop",type="string",dest="tstop",default="-1",help="specify the last frame included (negative from the end) in the differentiation" )
   parser.add_option("-d", "--step",type="string",dest="tstep",default="1",help="specify the step for the extraction of frames for the differentiation" )
   parser.add_option("-e", "--epsilon",type="string",dest="epsilon",default="0",help="specify the threshold for which values are assumed the same" )
   parser.add_option("-b","--bypass",action="store_true",dest="bypass",default=False,help="Will bypass certain mismatches between files" )
   options, args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Double checks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if len(args) != 2:
      print '\nThe names of two SELAFIN files are required\n\n'
      parser.print_help()
      sys.exit(1)
   slfFile1 = args[0]
   if not path.exists(slfFile1):
      print '\nCould not find the file named: ',slfFile1
      sys.exit(1)
   slfFile2 = args[1]
   if not path.exists(slfFile2):
      print '\nCould not find the file named: ',slfFile2
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Initial scan ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   vars = options.xvars
   if options.xvars != None: vars = cleanQuotes(options.xvars.replace('_',' '))
   slf1 = scanSELAFIN( slfFile1, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars )
   slf2 = scanSELAFIN( slfFile2, times = (int(options.tfrom),int(options.tstep),int(options.tstop)), vars  = vars )

   if options.scan:
      print '\n\nFirst file: '+slfFile1+'\n'+72*'~'+'\n'
      slf1.printHeader()
      slf1.printTimeSummary()
      print '\n\nSecond file: '+slfFile2+'\n'+72*'~'+'\n'
      slf2.printHeader()
      slf2.printTimeSummary()

   comparable = True

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Header differences ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if options.head:
      print '\n\nHeader differences: \n'+72*'~'+'\n'

      # ~~> File formats
      if slf1.slf.file['endian'] != slf2.slf.file['endian']:
         print '\n  <> File ENDIANs:\n'
         if slf1.slf.file['endian'] == ">": print '     + '+slfFile1+' is BIG ENDIAN'
         else: print '     + '+slfFile1+' is LITTLE ENDIAN'
         if slf2.slf.file['endian'] == ">": print '     + '+slfFile2+' is BIG ENDIAN'
         else: print '     + '+slfFile2+' is LITTLE ENDIAN'
      if slf1.slf.file['float'] != slf2.slf.file['float']:
         print '\n  <> File FLOATs:\n'
         if slf1.slf.file['float'] == ('d',8): print '     + '+slfFile1+' is DOUBLE PRECISION'
         else: print '     + '+slfFile1+' is SINGLE PRECISION'
         if slf2.slf.file['float'] == ('d',8): print '     + '+slfFile2+' is DOUBLE PRECISION'
         else: print '     + '+slfFile2+' is SINGLE PRECISION'

   # ~~> File contents
   mes = '\n  <> List of variable names:\n'
   found = False
   cmn_vars = []
   mes = mes + '\n     + '+slfFile1
   for ivar in range(len(slf1.slf.VARNAMES)):
      if slf1.slf.VARNAMES[ivar] in slf2.slf.VARNAMES:
         mes = mes + '\n        = '+slf1.slf.VARNAMES[ivar]
         cmn_vars.append(slf1.slf.VARNAMES[ivar])
      else:
         mes = mes + '\n        * '+slf1.slf.VARNAMES[ivar]
         found = True
   mes = mes + '\n     + '+slfFile2
   for ivar in range(len(slf2.slf.VARNAMES)):
      if slf2.slf.VARNAMES[ivar] in slf1.slf.VARNAMES:
         mes = mes + '\n        = '+slf2.slf.VARNAMES[ivar]
      else:
         mes = mes + '\n        * '+slf2.slf.VARNAMES[ivar]
         found = True
   if found and options.head: print mes
   if len(cmn_vars) == 0:
      comparable = False
      print '\n  /!\\ no common variables. The files are not comparables.\n'
   for ivar in range(len(slf1.slf.VARNAMES)):
      if slf1.slf.VARNAMES[ivar] not in cmn_vars: slf1.slf.VARINDEX.pop(ivar)
   for ivar in range(len(slf2.slf.VARNAMES)):
      if slf2.slf.VARNAMES[ivar] not in cmn_vars: slf2.slf.VARINDEX.pop(ivar)

   # ~~> File reference dates and times
   if options.head:
      if max( np.array(slf1.slf.DATETIME) - np.array(slf2.slf.DATETIME) ) > 0:
         print '\n  <> Different reference dates:'
         print '     + '+slfFile1+': ',slf1.slf.DATETIME
         print '     + '+slfFile2+': ',slf2.slf.DATETIME

   # ~~> File time frames
   mes = '\n  <> List of time frames:\n'
   found = False
   times0 = []
   times1 = []
   times2 = []
   # ~~> check if sorted times
   it1 = 1
   if len(slf1.slf.tags['times']) > 1:
      for it1 in range(len(slf1.slf.tags['times']))[1:]:
         if slf1.slf.tags['times'][it1] <= slf1.slf.tags['times'][it1-1]: break
      if slf1.slf.tags['times'][it1] > slf1.slf.tags['times'][it1-1]: it1 += 1
   it2 = 1
   if len(slf2.slf.tags['times']) > 1:
      for it2 in range(len(slf2.slf.tags['times']))[1:]:
         if slf2.slf.tags['times'][it2] <= slf2.slf.tags['times'][it2-1]: break
      if slf2.slf.tags['times'][it2] > slf2.slf.tags['times'][it2-1]: it2 += 1
   # ~~> correct if not bypassed
   if options.bypass and len(slf1.slf.tags['times']) == len(slf2.slf.tags['times']):
      times0 = range(len(slf1.slf.tags['times']))
   else:
      d = np.setdiff1d(slf1.slf.tags['times'][:it1],slf2.slf.tags['times'][:it2])
      if len(d):
         found = True
         mes = mes + '\n     + frames only in '+slfFile1+' : '+', '.join(['{0:.2f}'.format(i) for i in d])
      d = np.setdiff1d(slf2.slf.tags['times'][:it2],slf1.slf.tags['times'][:it1])
      if len(d):
         found = True
         mes = mes + '\n     + frames only in '+slfFile2+' : '+', '.join(['{0:.2f}'.format(i) for i in d])
      d = np.intersect1d(slf1.slf.tags['times'][:it1],slf2.slf.tags['times'][:it2])
      if len(d):
         mes = mes + '\n     + frames in both files: '+', '.join([str(i) for i in d])
         times1 = np.searchsorted(slf1.slf.tags['times'][:it1],d)
         slf1.slf.tags['times'] = slf1.slf.tags['times'][times1]
         for t in range(len(slf1.slf.tags['cores']))[it1:]:
            slf1.slf.tags['cores'].remove(slf1.slf.tags['cores'][-1])
         for t in range(len(slf1.slf.tags['cores']))[::-1]:
            if t not in times1: slf1.slf.tags['cores'].remove(slf1.slf.tags['cores'][t])
         times2 = np.searchsorted(slf2.slf.tags['times'][:it2],d)
         slf2.slf.tags['times'] = slf2.slf.tags['times'][times2]
         for t in range(len(slf2.slf.tags['cores']))[it2:]:
            slf2.slf.tags['cores'].remove(slf2.slf.tags['cores'][-1])
         for t in range(len(slf2.slf.tags['cores']))[::-1]:
            if t not in times2: slf2.slf.tags['cores'].remove(slf2.slf.tags['cores'][t])
         times0 = range(len(slf2.slf.tags['times']))
         if options.head: print mes
      else:
         comparable = False
         print '\n  /!\\ no common time frames. The files are not comparables.\n'
      times0 = range(len(slf1.slf.tags['times'])) # ... for instance
   if found and options.head: print mes

   # ~~> File geometries
   mes = ''
   if slf1.slf.NPOIN2 != slf2.slf.NPOIN2:
      mes = mes + '     + NPOIN2 = '+str(slf1.slf.NPOIN2)+' in '+slfFile1
      mes = mes + '     * NPOIN2 = '+str(slf2.slf.NPOIN2)+' in '+slfFile2
      mes = mes + '\n'
   if slf1.slf.NPLAN != slf2.slf.NPLAN:
      mes = mes + '     + NPLAN = '+str(slf1.slf.NPLAN)+' in '+slfFile1
      mes = mes + '     * NPLAN = '+str(slf2.slf.NPLAN)+' in '+slfFile2
      mes = mes + '\n'
   if mes != '':
      if options.head: print '\n  <> Gemetry:\n'+mes
      comparable = False
      print '\n  /!\\ different geometries. The files are not comparables.\n'

   if options.head:
      # ~~> File trangulations
      d = slf1.slf.IKLE2 - slf2.slf.IKLE2
      if len(np.argwhere( d > [0,0,0] )) > 0:
         print '\n  <> 2D Triangulation:\n'
         print '     + number of mismatches: ',len(np.argwhere( d == [0,0,0] ).T[0])
         print '     + mismatched elements: ',np.argwhere( d == [0,0,0] ).T[0][::3]
         #comparable = False
         #print '\n  /!\\ different triangulation. The files are not comparables.\n'

   if options.head:
      # ~~> File geo-localisation
      d = np.sqrt( np.power((slf1.slf.MESHX-slf2.slf.MESHX),2) + np.power((slf1.slf.MESHY-slf2.slf.MESHY),2) )
      e = np.argwhere( d > float(options.epsilon) ).ravel()
      if len(e) > 0:
         print '\n  <> Geo-Localisation:\n'
         print '     + maximum distance between points : ',max(d[e])
         print '     + number of points above clocelyness threshold : '+str(len(e))+' (i.e. '+str(100*len(e)/len(d))+'% of points)'
         print '     + node numbers : ',np.arange(slf1.slf.NPOIN3)[e]
         #comparable = False
         #print '\n  /!\\ different geo-localisations. The files are not comparables.\n'

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Core differences ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if options.core and comparable:
      print '\n\nCore differences: \n'+72*'~'+'\n'

      found = False
      for t in times0:
         for ivar in range(len(slf1.slf.VARINDEX)):
            jvar = slf2.slf.VARNAMES.index(slf1.slf.VARNAMES[ivar])
            d = np.absolute(slf1.slf.getVariablesAt( t,[slf1.slf.VARINDEX[ivar]] ) - slf2.slf.getVariablesAt( t,[slf2.slf.VARINDEX[jvar]] )).ravel()
            e = np.argwhere( d > float(options.epsilon) ).ravel()
            if len(e) > 0:
               found = True
               print '\n  <> Frame: '+str(t)+' (times: '+'{0:.2f}'.format(slf1.slf.tags['times'][t])+' / '+'{0:.2f}'.format(slf2.slf.tags['times'][t])+'), Variable: '+slf1.slf.VARNAMES[ivar]+'\n'
               print '     + max difference: ',max(d[e])
               np.arange(slf1.slf.NPOIN3)[e]
               print '     + number of values above threshold : '+str(len(e))+' (i.e. '+str(100*len(e)/len(d))+'% of points)'
               print '     + node numbers :          ',np.arange(slf1.slf.NPOIN3)[e]
               print '     + values at those nodes : ',d[np.arange(slf1.slf.NPOIN3)[e]]
      if not found: print '  <> None to the epsilon: ',options.epsilon

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
