#!/usr/bin/env python
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
      Beware the the variables number / names are set in the code below.
      Creates a binary liquid boundary file from a global model (SLF form)
        by interpolating on a given GEO model domain. The time series in
        the BND file are extracted only at liquid nodes as defined in the
        CONLIM file.
"""
"""@history 02/12/2013 -- Juliette C.E. Parisi
      Created draft script to write the binary liquid boundary file
         on the basis of the HYCOM global model results
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
   from argparse import ArgumentParser,RawDescriptionHelpFormatter
   from parsers.parserSELAFIN import CONLIM,SELAFIN,subsetVariablesSLF,getValueHistorySLF
   from samplers.meshes import xysLocateMesh

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n'+'~'*72+'\n'
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
A script to map 2D or 3D outter model results into a SELAFIN, onto the
   spatially and time varying boundary of a spatially contained SELAFIN file
   of your choosing (your MESH).
      '''))
   parser.add_argument( "args",default='',nargs=4 )
   options = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ cli+slf new mesh ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cliFile = options.args[0]
   if not path.exists(cliFile):
      print '... the provided cliFile does not seem to exist: '+cliFile+'\n\n'
      sys.exit(1)
   geoFile = options.args[1]
   if not path.exists(cliFile):
      print '... the provided geoFile does not seem to exist: '+geoFile+'\n\n'
      sys.exit(1)

   # Read the new CLI file to get boundary node numbers
   print '   +> getting hold of the CONLIM file and of its liquid boundaries'
   cli = CONLIM(cliFile)
   # Keeping only open boundary nodes
   BOR = np.extract( cli.BOR['lih'] != 2, cli.BOR['n'] )

   # Find corresponding (x,y) in corresponding new mesh
   print '   +> getting hold of the GEO file and of its bathymetry'
   geo = SELAFIN(geoFile)
   xys = np.vstack( (geo.MESHX[BOR-1],geo.MESHY[BOR-1]) ).T
   bat = geo.getVariablesAt( 0,subsetVariablesSLF("BOTTOM: ",geo.VARNAMES)[0] )[0]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ slf existing res ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   slfFile = options.args[2]
   if not path.exists(cliFile):
      print '... the provided slfFile does not seem to exist: '+slfFile+'\n\n'
      sys.exit(1)
   slf = SELAFIN(slfFile)
   slf.setKDTree()
   slf.setMPLTri()

   print '   +> support extraction'
   # Extract triangles and weigths in 2D
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
# ~~~~ writes BND header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   bndFile = options.args[3]
   bnd = SELAFIN('')
   bnd.fole = {}
   bnd.fole.update({ 'hook': open(bndFile,'wb') })
   bnd.fole.update({ 'name': bndFile})
   bnd.fole.update({ 'endian': ">" })     # big endian
   bnd.fole.update({ 'float': ('f',4) })  # single precision

   # Meta data and variable names
   bnd.TITLE = ''
   bnd.NBV1 = 5
   # /!\ ELEVATION has to be the first variable
   # (for possible vertical re-interpolation within TELEMAC)
   bnd.VARNAMES = ['ELEVATION Z     ', \
                   'VELOCITY U      ','VELOCITY V      ', \
                   'SALINITY        ','TEMPERATURE     ' ]
   bnd.VARUNITS = ['M               ', \
                   'M/S             ','M/S             ', \
                   '                ','                ' ]
   bnd.NVAR = bnd.NBV1
   bnd.VARINDEX = range(bnd.NVAR)

   # Sizes and mesh connectivity
   bnd.NPLAN = slf.NPLAN
   bnd.NDP2 = 2
   bnd.NDP3 = 4
   bnd.NPOIN2 = len(BOR)
   bnd.NPOIN3 = bnd.NPOIN2*slf.NPLAN
   bnd.IPARAM = [0,0,0,0,0,0,bnd.NPLAN,0,0,0]
   bnd.IPOB2 = BOR   # /!\ Note that IPOBO keeps the original numbering
   print '   +> masking and setting connectivity'
   # Set the array that only includes elements of geo.IKLE2 with at least two nodes in BOR
   MASK = geo.IKLE2[np.where( np.sum(np.in1d(geo.IKLE2,np.sort(BOR-1)).reshape(geo.NELEM2,geo.NDP2),axis=1) == 2 )]
   IKLE2 = np.ravel(MASK)[np.in1d(MASK,np.sort(BOR-1))].reshape(len(MASK),2) # this IKLE2 keeps the original numbering
   # ~~> re-numbering IKLE2 as a local connectivity matrix
   KNOLG,indices = np.unique( np.ravel(IKLE2), return_index=True )
   KNOGL = dict(zip( KNOLG,range(len(KNOLG)) ))
   bnd.IKLE2 = - np.ones_like(IKLE2,dtype=np.int)
   for k in range(len(IKLE2)):
      bnd.IKLE2[k] = [ KNOGL[IKLE2[k][0]], KNOGL[IKLE2[k][1]] ]    # /!\ bnd.IKLE2 has a local numbering, fit to the boundary elements
   # Last few numbers
   bnd.NELEM2 = len(bnd.IKLE2)
   if slf.NPLAN > 1: bnd.NELEM3 = bnd.NELEM2*(slf.NPLAN-1)
   else: bnd.NELEM3 = bnd.NELEM2
   # 3D structures
   if slf.NPLAN > 1:
      bnd.IPOB3 = np.ravel(np.add(np.repeat(bnd.IPOB2,slf.NPLAN).reshape((bnd.NPOIN2,slf.NPLAN)),bnd.NPOIN2*np.arange(slf.NPLAN)).T)
      bnd.IKLE3 = \
         np.repeat(bnd.NPOIN2*np.arange(slf.NPLAN-1),bnd.NELEM2*bnd.NDP3).reshape((bnd.NELEM2*(slf.NPLAN-1),bnd.NDP3)) + \
         np.tile(np.add(np.tile(bnd.IKLE2,2),np.repeat(bnd.NPOIN2*np.arange(2),bnd.NDP2)),(slf.NPLAN-1,1))
   else:
      bnd.IPOB3 = bnd.IPOB2
      bnd.IKLE3 = bnd.IKLE2
   # Mesh coordinates
   bnd.MESHX = geo.MESHX[BOR-1]
   bnd.MESHY = geo.MESHY[BOR-1]

   print '   +> writing header'
   # Write header
   bnd.appendHeaderSLF()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ writes BND core ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   print '   +> setting variables'
   # TIME and DATE extraction
   bndDATES = slf.DATETIME
   bndTIMES = slf.tags['times']
   bnd.tags['times'] = slf.tags['times']
   # VARIABLE extraction
   vars = subsetVariablesSLF("ELEVATION Z: ;VELOCITY U: ;VELOCITY V: ;SALINITY: ;TEMPERATURE: ",slf.VARNAMES)

   # Read / Write data, one time step at a time to support large files
   print '   +> reading / writing variables'
   pbar = ProgressBar(maxval=len(slf.tags['times'])).start()
   zeros = np.zeros((bnd.NPOIN3,1),dtype=np.float)
   for t in range(len(slf.tags['times'])):
      data = getValueHistorySLF( slf.file,slf.tags,[t],support3d,slf.NVAR,slf.NPOIN3,slf.NPLAN,vars )
      # special case for TEMPERATURE and SALINITY
      data[3] = np.maximum( data[3],zeros )
      data[4] = np.maximum( data[4],zeros )
      d = np.reshape(np.transpose(np.reshape(np.ravel(data),(bnd.NVAR,bnd.NPOIN2,bnd.NPLAN)),(0,2,1)),(bnd.NVAR,bnd.NPOIN3))
      #for ipoin in range(bnd.NPOIN2):
      #   for iplan in range(bnd.NPLAN-1,0,-1):
      #      for ivar in range(bnd.NVAR)[1:]:  # except for Z
      #         if bat[BOR[ipoin]-1] > d[0][ipoin+(iplan-1)*bnd.NPOIN2]:
      #            d[ivar][ipoin+(iplan-1)*bnd.NPOIN2] = d[ivar][ipoin+iplan*bnd.NPOIN2]
      #      if d[3][ipoin+(iplan-1)*bnd.NPOIN2] < 28.0:
      #         d[3][ipoin+(iplan-1)*bnd.NPOIN2] = max(d[3][ipoin+iplan*bnd.NPOIN2],28.0)
      bnd.appendCoreTimeSLF( t )
      bnd.appendCoreVarsSLF( d )
      pbar.update(t)
   pbar.finish()

   # Close bndFile
   bnd.fole['hook'].close()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)

