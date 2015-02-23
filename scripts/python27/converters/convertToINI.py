#!/usr/bin/env python
"""@author Juliette C.E. Parisi, Michael S. Turnbull and Sebastien E. Bourban
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
      Beware the the variables number / names are set in the code below.
      Creates an initial condition file from a global model (SLF form)
        by interpolating on a given GEO model domain.
"""
"""@history 02/12/2013 -- Juliette C.E. Parisi
      Created draft script to write the binary liquid boundary file
         on the basis of the HYCOM global model results
"""
"""@history 14/02/2014 -- Michael S. Turnbull
      CFurther adaption and correction for application to HYCOM
"""
"""@history 11/11/2014 -- Sebastien E. Bourban
      Heavy modifictaions to make it generic and in order to
         correctly fill-in the IPOBO and the IKLE in both 2D and 3D
         based on fancy numpy programing.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
import numpy as np
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
from utils.progressbar import ProgressBar

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#
__author__="Juliette C.E. Parisi"
__date__ ="$02-Dec-2013 15:09:48$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Dependencies towards other modules ~~~~~~~~~~~~~~~~~~~~~~~~~~
   from config import OptionParser
   from parsers.parserSELAFIN import CONLIM,SELAFIN,subsetVariablesSLF,getValueHistorySLF
   from samplers.meshes import xysLocateMesh

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   options, args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ slf new mesh ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   geoFile = args[0]

# Find corresponding (x,y) in corresponding new mesh
   print '   +> getting hold of the GEO file and of its bathymetry'
   geo = SELAFIN(geoFile)
   xys = np.vstack( (geo.MESHX,geo.MESHY) ).T
   bat = geo.getVariablesAt( 0,subsetVariablesSLF("BOTTOM: ",geo.VARNAMES)[0] )[0]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ slf existing res ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   slfFile = args[1]
   slf = SELAFIN(slfFile)
   slf.setKDTree()
   slf.setMPLTri()

   print '   +> support extraction'
   # Extract triangles and weights in 2D
   support2d = []
   ibar = 0; pbar = ProgressBar(maxval=len(xys)).start()
   for xyi in xys:
      support2d.append(xysLocateMesh(xyi,slf.IKLE2,slf.MESHX,slf.MESHY,slf.tree,slf.neighbours))
      ibar+=1
      pbar.update(ibar)
   pbar.finish()
   # Extract support in 3D
   support3d = zip(support2d,len(xys)*[range(slf.NPLAN)])

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ writes INI header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   iniFile = args[2]
   ini = SELAFIN('')
   ini.fole = {}
   ini.fole.update({ 'hook': open(iniFile,'wb') })
   ini.fole.update({ 'name': iniFile})
   ini.fole.update({ 'endian': ">" })     # big endian
   ini.fole.update({ 'float': ('f',4) })  # single precision
   
   # Meta data and variable names
   ini.TITLE = ''
   ini.NBV1 = 5
   # /!\ ELEVATION has to be the first variable
   # (for possible vertical re-interpolation within TELEMAC)
   ini.VARNAMES = ['ELEVATION Z     ', \
                   'VELOCITY U      ','VELOCITY V      ', \
                   'SALINITY        ','TEMPERATURE     ' ]
   ini.VARUNITS = ['M               ', \
                   'M/S             ','M/S             ', \
                   '                ','                ' ]
   ini.NVAR = ini.NBV1
   ini.VARINDEX = range(ini.NVAR)
   
   # Sizes and mesh connectivity
   ini.NPLAN = slf.NPLAN
   ini.NDP2 = 3
   ini.NDP3 = 6
   ini.NPOIN2 = geo.NPOIN2
   ini.NPOIN3 = geo.NPOIN2*ini.NPLAN
   ini.NELEM2 = geo.NELEM2
   ini.NELEM3 = ini.NELEM2*(ini.NPLAN-1)
   
   print '   +> setting connectivity'
   ini.IKLE3 = \
      np.repeat(geo.NPOIN2*np.arange(ini.NPLAN-1),geo.NELEM2*ini.NDP3).reshape((geo.NELEM2*(ini.NPLAN-1),ini.NDP3)) + \
      np.tile(np.add(np.tile(geo.IKLE2,2),np.repeat(geo.NPOIN2*np.arange(2),geo.NDP2)),(ini.NPLAN-1,1))
   ini.IPOB3 = np.ravel(np.add(np.repeat(geo.IPOB2,ini.NPLAN).reshape((geo.NPOIN2,ini.NPLAN)),geo.NPOIN2*np.arange(ini.NPLAN)).T)
   ini.IPARAM = [0,0,0,0,0,0,ini.NPLAN,0,0,0]
   
   # Mesh coordinates
   ini.MESHX = geo.MESHX
   ini.MESHY = geo.MESHY
   
   print '   +> writing header'
   # Write header
   ini.appendHeaderSLF()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ writes INI core ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   print '   +> setting variables'
   # TIME and DATE extraction
   iniDATES = slf.DATETIME
   iniTIMES = slf.tags['times']
   ini.tags['times'] = slf.tags['times']
   # VARIABLE extraction
   vars = subsetVariablesSLF("ELEVATION Z: ;VELOCITY U: ;VELOCITY V: ;SALINITY: ;TEMPERATURE: ",slf.VARNAMES)

   # Read / Write data for first time step
   zeros = np.zeros((ini.NPOIN3,1),dtype=np.float)

   print '   +> extracting variables'
   data = getValueHistorySLF( slf.file,slf.tags,[0],support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars )
   
   # special case for TEMPERATURE and SALINITY
   data[3] = np.maximum( data[3],zeros )
   data[4] = np.maximum( data[4],zeros )
   print '   +> correcting variables'
   # duplicate values below bottom
   d = np.reshape(np.transpose(np.reshape(np.ravel(data),(ini.NVAR,ini.NPOIN2,ini.NPLAN)),(0,2,1)),(ini.NVAR,ini.NPOIN3))
   #for ipoin in range(ini.NPOIN2):
   #   for iplan in range(ini.NPLAN-1,0,-1):
   #      for ivar in range(ini.NVAR)[1:]:  # except for Z
   #         if bat[ipoin] > d[0][ipoin+(iplan-1)*ini.NPOIN2]:
   #            d[ivar][ipoin+(iplan-1)*ini.NPOIN2] = d[ivar][ipoin+iplan*ini.NPOIN2]
   #      if d[3][ipoin+(iplan-1)*ini.NPOIN2] < 28.0:
   #         d[3][ipoin+(iplan-1)*ini.NPOIN2] = max(d[3][ipoin+iplan*ini.NPOIN2],28.0)
   print '   +> writing variables'
   ini.appendCoreTimeSLF( 0 )
   ini.appendCoreVarsSLF( d )

   # Close iniFile
   ini.fole['hook'].close()   
   
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)

