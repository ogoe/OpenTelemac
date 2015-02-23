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
      Beware the the variables number / names are set in the code below.
      Creates an atmospheric condition file from a global model (SLF form)
        by interpolating on a given GEO model domain.
"""
"""@history 16/12/2014 -- Sebastien E. Bourban
      Created from convertINI.py and createBND.py
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
__author__="S.E.Bourban"
__date__ ="$16-Dec-2014 15:09:48$"

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
   print '   +> getting hold of the GEO file'
   geo = SELAFIN(geoFile)
   xys = np.vstack( (geo.MESHX,geo.MESHY) ).T

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
# ~~~~ writes ATM header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   atmFile = args[2]
   atm = SELAFIN('')
   atm.fole = {}
   atm.fole.update({ 'hook': open(atmFile,'wb') })
   atm.fole.update({ 'name': atmFile})
   atm.fole.update({ 'endian': ">" })     # big endian
   atm.fole.update({ 'float': ('f',4) })  # single precision
   
   # Meta data and variable names
   atm.TITLE = ''
   atm.NBV1 = 4
   atm.VARNAMES = ['SURFACE PRESSURE', \
      'WIND VELOCITY U ','WIND VELOCITY V ', \
      'AIR TEMPERATURE ']
   atm.VARUNITS = ['UI              ', \
      'M/S             ','M/S             ', \
      'DEGREES         ']
   atm.NVAR = atm.NBV1
   atm.VARINDEX = range(atm.NVAR)
   
   # Sizes and mesh connectivity
   atm.NPLAN = slf.NPLAN          # it should be 2D but why the heack not ...
   atm.NDP2 = slf.NDP2
   atm.NDP3 = slf.NDP3
   atm.NPOIN2 = geo.NPOIN2
   atm.NPOIN3 = geo.NPOIN2*atm.NPLAN
   atm.NELEM2 = geo.NELEM2
   
   print '   +> setting connectivity'
   if atm.NPLAN > 1:
      atm.NELEM3 = geo.NELEM2*(atm.NPLAN-1)
      atm.IKLE2 = geo.IKLE2
      atm.IKLE3 = \
         np.repeat(geo.NPOIN2*np.arange(atm.NPLAN-1),geo.NELEM2*atm.NDP3).reshape((geo.NELEM2*(atm.NPLAN-1),atm.NDP3)) + \
         np.tile(np.add(np.tile(geo.IKLE2,2),np.repeat(geo.NPOIN2*np.arange(2),geo.NDP2)),(atm.NPLAN-1,1))
      atm.IPOB2 = geo.IPOB2
      atm.IPOB3 = np.ravel(np.add(np.repeat(geo.IPOB2,atm.NPLAN).reshape((geo.NPOIN2,atm.NPLAN)),geo.NPOIN2*np.arange(atm.NPLAN)).T)
   else:
      atm.NELEM3 = geo.NELEM2
      atm.IKLE2 = geo.IKLE2
      atm.IKLE3 = geo.IKLE3
      atm.IPOB2 = geo.IPOB2
      atm.IPOB3 = geo.IPOB3
   atm.IPARAM = [0,0,0,0,0,0,atm.NPLAN,0,0,0]
   
   # Mesh coordinates
   atm.MESHX = geo.MESHX
   atm.MESHY = geo.MESHY
   
   print '   +> writing header'
   # Write header
   atm.appendHeaderSLF()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ writes ATM core ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   print '   +> setting variables'
   # TIME and DATE extraction
   atmDATES = slf.DATETIME
   atmTIMES = slf.tags['times']
   atm.tags['times'] = slf.tags['times']
   # VARIABLE extraction
   vars = subsetVariablesSLF("SURFACE PRESSURE: ;WIND VELOCITY U: ;WIND VELOCITY V: ;AIR TEMPERATURE: ",slf.VARNAMES)

   # Read / Write data, one time step at a time to support large files
   pbar = ProgressBar(maxval=len(slf.tags['times'])).start()
   for t in range(len(slf.tags['times'])):

      data = getValueHistorySLF( slf.file,slf.tags,[t],support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars )
      # special cases ?
      atm.appendCoreTimeSLF( t )
      atm.appendCoreVarsSLF( np.reshape(np.transpose(np.reshape(np.ravel(data),(atm.NVAR,atm.NPOIN2,atm.NPLAN)),(0,2,1)),(atm.NVAR,atm.NPOIN3)) )
      pbar.update(t)
   pbar.finish()

   # Close atmFile
   atm.fole['hook'].close()   
   
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)

