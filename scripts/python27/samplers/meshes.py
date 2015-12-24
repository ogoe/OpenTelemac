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
"""@history 12/12/2012 -- Sebastien E. Bourban
     Many methods developped for application to meshes. The latest
        one being about subdivision of meshes.
"""
"""@brief
      Tools for sampling and interpolating through triangular meshes
"""
"""@details
         Contains ...
"""
"""@history 20/06/2013 -- Sebastien E. Bourban
      A new method, sliceMesh, now replaces crossMesh and all of the
         Ray Tracing algorithms. The later will remain for the fame and
         maybe for future uses, but sliveMesh should now be used.
"""
"""@history 10/11/2015 -- Juliette Parisi and Sebastien E. Bourban
      A new method, tessellatePoly, has now been implmented.
      It returns a MESH (including the IKLE and IPOBO) of the waters within
         the largest polygon, having taken out all islands.
      TODO:
       - It should be capable of taking in open lines to constrain the
       mesh to these.
       - It should be parallelised fairly easily.
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
import numpy as np
import math
from scipy.spatial import cKDTree
import matplotlib.path as mplPath
from scipy.spatial import Delaunay
from matplotlib.tri import Triangulation
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
from config import OptionParser
# ~~> dependencies towards other pytel/modules
from parsers.parserSELAFIN import SELAFIN
from utils.progressbar import ProgressBar
from utils.geometry import isCCW,getSegmentIntersection,getBarycentricWeights,isInsideTriangle,getDistancePointToLine
from samplers.polygons import isClockwise,joinSegments

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def nearLocateMesh(xyo,IKLE,MESHX,MESHY,tree=None):
   """
   Requires the scipy.spatial and the matplotlib.tri packages to be loaded.
    - Will use already computed tree or re-create it if necessary.
    - Will use already computed neighbourhood or re-create it if necessary.
   This function return the element number for the triangle including xyo=(xo,yo)
      or -1 if the (xo,yo) is outside the mesh
   Return: the element, the barycentric weights, and the tree and the neighbourhood if computed
   """
   # ~~> Create the KDTree of the iso-barycentres
   if tree == None:
      isoxy = np.column_stack((np.sum(MESHX[IKLE],axis=1)/3.0,np.sum(MESHY[IKLE],axis=1)/3.0))
      tree = cKDTree(isoxy)
   # ~~> Find the indices corresponding to the nearest elements to the points
   inear = -1
   for d,i in zip(*tree.query(xyo,8)):
      ax,bx,cx = MESHX[IKLE[i]]
      ay,by,cy = MESHY[IKLE[i]]
      w = isInsideTriangle( xyo,(ax,ay),(bx,by),(cx,cy),nomatter=True )
      if w != []: return i,w,tree
      if inear < 0:
         inear = i
         dnear = d
      if dnear > d:
         inear = i
         dnear = d

   # ~~> Find the indices and weights corresponding to the element containing the point
   ax,bx,cx = MESHX[IKLE[inear]]
   ay,by,cy = MESHY[IKLE[inear]]

   return inear,isInsideTriangle( xyo,(ax,ay),(bx,by),(cx,cy),nomatter=False ),tree

def dichoLocateMesh(rank,e1,xy1,e2,xy2,IKLE,MESHX,MESHY,tree):
   """
   Will find at least one point between xy1 and xy2 that is within the mesh
   """
   # ~~ Position the middle point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xyo = [ ( xy1[0]+xy2[0] )/2.0,( xy1[1]+xy2[1] )/2.0 ]
   eo,bo,tree = nearLocateMesh(xyo,IKLE,MESHX,MESHY,tree)
   if bo != []: return True,eo,xyo,bo

   # ~~ Limit the number of useless dichotomies ~~~~~~~~~~~~~~~~~~~~
   rank = rank + 1
   if rank > 3: return False,eo,xyo,bo

   # ~~ Sub-segments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   found,ej,xyj,bj = dichoLocateMesh(rank,e1,xy1,eo,xyo,IKLE,MESHX,MESHY,tree)
   if found: return found,ej,xyj,bj
   found,ej,xyj,bj = dichoLocateMesh(rank,eo,xyo,e2,xy2,IKLE,MESHX,MESHY,tree)
   if found: return found,ej,xyj,bj

   return False,eo,xyo,bo

def xyTraceMesh(inear,xyi,xyo,IKLE,MESHX,MESHY,neighbours=None):
   """
   Requires the matplotlib.tri package to be loaded.
    - Will use already computed neighbourhood or re-create it if necessary.
   This function return the element number for the triangle including xyo=(xo,yo)
      or -1 if the (xo,yo) is outside the mesh. It creates the neighbouring connectivity
      map and ray-traces from xyi to xyo
   Return: whether xyo was found within the mesh, the (nearest) element, the
      associated barycentric weights, and the neighbourhood if computed
   """
   if neighbours == None:
      neighbours = Triangulation(MESHX,MESHY,IKLE).get_cpp_triangulation().get_neighbors()
   found,ray = traceRay2XY(IKLE,MESHX,MESHY,neighbours,inear,xyi,inear,xyo)

   return found,ray,neighbours

def subdivideMesh(IKLE,MESHX,MESHY):
   """
   Requires the matplotlib.tri package to be loaded.
    - Will use already computed edges or re-create it if necessary.
   This function return a new tuple IKLE,MESHX,MESHY where each triangle has been
      subdivided in 4.
   """
   # ~~> Singling out edges
   from matplotlib.tri import Triangulation
   edges = Triangulation(MESHX,MESHY,IKLE).get_cpp_triangulation().get_edges()

   # ~~> Memory allocation for new MESH
   IELEM = len(IKLE); IPOIN = len(MESHX); IEDGE = len(edges)
   JKLE = np.zeros((IELEM*4,3),dtype=np.int)       # you subdivide every elements by 4
   MESHJ = np.zeros((IEDGE,2),dtype=np.int)        # you add one point on every edges

   # ~~> Lookup tables for node numbering on common edges
   pa,pb = edges.T
   k1b,k1a = np.sort(np.take(IKLE,[0,1],axis=1)).T
   indx1 = np.searchsorted(pa,k1a)
   jndx1 = np.searchsorted(pa,k1a,side='right')
   k2b,k2a = np.sort(np.take(IKLE,[1,2],axis=1)).T
   indx2 = np.searchsorted(pa,k2a)
   jndx2 = np.searchsorted(pa,k2a,side='right')
   k3b,k3a = np.sort(np.take(IKLE,[2,0],axis=1)).T
   indx3 = np.searchsorted(pa,k3a)
   jndx3 = np.searchsorted(pa,k3a,side='right')

   # ~~> Building one triangle at a time /!\ Please get this loop parallelised
   j = 0
   for i in range(IELEM):
      k1 = indx1[i]+np.searchsorted(pb[indx1[i]:jndx1[i]],k1b[i])
      k2 = indx2[i]+np.searchsorted(pb[indx2[i]:jndx2[i]],k2b[i])
      k3 = indx3[i]+np.searchsorted(pb[indx3[i]:jndx3[i]],k3b[i])
      # ~~> New connectivity JKLE
      JKLE[j] = [IKLE[i][0],IPOIN+k1,IPOIN+k3]
      JKLE[j+1] = [IKLE[i][1],IPOIN+k2,IPOIN+k1]
      JKLE[j+2] = [IKLE[i][2],IPOIN+k3,IPOIN+k2]
      JKLE[j+3] = [IPOIN+k1,IPOIN+k2,IPOIN+k3]
      # ~~> New interpolation references for values and coordinates
      MESHJ[k1] = [IKLE[i][0],IKLE[i][1]]
      MESHJ[k2] = [IKLE[i][1],IKLE[i][2]]
      MESHJ[k3] = [IKLE[i][2],IKLE[i][0]]
      j += 4

   # ~~> Reset IPOBO while you are at it
   MESHX = np.resize(MESHX,IPOIN+IEDGE)
   MESHY = np.resize(MESHY,IPOIN+IEDGE)
   MESHX[IPOIN:] = np.sum(MESHX[MESHJ],axis=1)/2.
   MESHY[IPOIN:] = np.sum(MESHY[MESHJ],axis=1)/2.
   neighbours = Triangulation(MESHX,MESHY,JKLE).get_cpp_triangulation().get_neighbors()
   JPOBO = np.zeros(IPOIN+IEDGE,np.int)
   for n in range(IELEM*4):
      s1,s2,s3 = neighbours[n]
      e1,e2,e3 = JKLE[n]
      if s1 < 0:
        JPOBO[e1] = e1+1
        JPOBO[e2] = e2+1
      if s2 < 0:
        JPOBO[e2] = e2+1
        JPOBO[e3] = e3+1
      if s3 < 0:
        JPOBO[e3] = e3+1
        JPOBO[e1] = e1+1

   return JKLE,MESHX,MESHY,JPOBO,MESHJ

def traceRay2XY(IKLE,MESHX,MESHY,neighbours,ei,xyi,en,xyn):
   """
   This assumes that you cannot go back on your ray.
   """
   # ~~> latest addition to the ray
   ax,bx,cx = MESHX[IKLE[en]]
   ay,by,cy = MESHY[IKLE[en]]
   bi = getBarycentricWeights( xyi,(ax,ay),(bx,by),(cx,cy) )
   pnt = {'n':1, 'xy':[xyi], 'e':[en], 'b':[bi],
      'd':[np.power(xyi[0]-xyn[0],2) + np.power(xyi[1]-xyn[1],2)]}

   # ~~> convergence on distance to target xyn
   accuracy = np.power(10.0, -5+np.floor(np.log10(abs(ax+bx+cx+ay+by+cy))))
   if pnt['d'][0] < accuracy: return True,pnt

   # ~~> get the ray through to the farthest neighbouring edges
   ks = []; ds = []
   for k in [0,1,2]:
      xyj = getSegmentIntersection( (MESHX[IKLE[en][k]],MESHY[IKLE[en][k]]),(MESHX[IKLE[en][(k+1)%3]],MESHY[IKLE[en][(k+1)%3]]),xyi,xyn )
      if xyj == []: continue         # there are no intersection with that edges
      ej = neighbours[en][k]
      if ej == ei: continue          # you should not back track on your ray
      xyj = xyj[0]
      dij = np.power(xyi[0]-xyj[0],2) + np.power(xyi[1]-xyj[1],2)
      ks.append(k)
      ds.append(dij)
   if ds != []:
      k = ks[np.argmax(ds)]
      ej = neighbours[en][k]
      xyj = getSegmentIntersection( (MESHX[IKLE[en][k]],MESHY[IKLE[en][k]]),(MESHX[IKLE[en][(k+1)%3]],MESHY[IKLE[en][(k+1)%3]]),xyi,xyn )[0]
      djn = np.power(xyn[0]-xyj[0],2) + np.power(xyn[1]-xyj[1],2)

      # ~~> Possible recursive call
      if True or djn > accuracy:    # /!\ this may be a problem
         if ej < 0:
            # you have reach the end of the line
            bj = getBarycentricWeights( xyj,(ax,ay),(bx,by),(cx,cy) )
            pnt['n'] += 1; pnt['xy'].insert(0,xyj); pnt['e'].insert(0,en); pnt['b'].insert(0,bj); pnt['d'].insert(0,djn)
            return djn<accuracy,pnt
         else:
            found,ray = traceRay2XY(IKLE,MESHX,MESHY,neighbours,en,xyj,ej,xyn)
            ray['n'] += 1; ray['xy'].append(xyi); ray['e'].append(en); ray['b'].append(bi); ray['d'].append(dij)
            return found,ray

   # ~~> convergence on having found the appropriate triangle
   bn = isInsideTriangle( xyn,(ax,ay),(bx,by),(cx,cy) )
   if bn != []:
      pnt['n'] += 1; pnt['xy'].insert(0,xyn); pnt['e'].insert(0,en); pnt['b'].insert(0,bn); pnt['d'].insert(0,0.0)
      return True,pnt

   # ~~> you should not be here !
   return False,pnt

def xysLocateMesh(xyo,IKLE,MESHX,MESHY,tree=None,neighbours=None):

   # ~~> get to the nearest element
   oet = -1; obr = [0.0,0.0,0.0]
   eo,bo,tree = nearLocateMesh(np.array(xyo),IKLE,MESHX,MESHY,tree)
   if bo == []:
      found,ray,neighbours = xyTraceMesh(eo,[np.sum(MESHX[IKLE[eo]])/3.0,np.sum(MESHY[IKLE[eo]])/3.0],xyo,IKLE,MESHX,MESHY,neighbours)
      if found:
         obr = ray['b'][ray['n']]
         oet = ray['e'][ray['n']]
   else:
      obr = bo
      oet = eo

   if oet == -1: return [-1,-1,-1],obr
   return IKLE[oet],obr

def crossMesh(polyline,IKLE,MESHX,MESHY,tree=None,neighbours=None):
   """
   """
   # ~~ Intersection nodes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ipt = []; iet = []; ibr = []

   # ~~ Locate nodes of the polyline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xyo = polyline[0]
   eo,bo,tree = nearLocateMesh(xyo,IKLE,MESHX,MESHY,tree)

   for i in range(len(polyline)-1):
      xyi = polyline[i+1]
      ei,bi,tree = nearLocateMesh(xyi,IKLE,MESHX,MESHY,tree)
      if bo == [] and bi == []:
         rank = 0
         found,ej,xyj,bj = dichoLocateMesh(rank,eo,xyo,ei,xyi,IKLE,MESHX,MESHY,tree)
         if not found:
            print '... Could not find easily an intersection with the mesh'
            sys.exit(1)
         found,rayo,neighbours = xyTraceMesh(ej,xyj,xyo,IKLE,MESHX,MESHY,neighbours)
         #print 'raya'
         for j in range(rayo['n'])[:-1]:
            #print rayo['e'][j],rayo['xy'][j]
            ipt.append(rayo['xy'][j]); iet.append(rayo['e'][j]); ibr.append(rayo['b'][j])
         found,rayi,neighbours = xyTraceMesh(ej,xyj,xyi,IKLE,MESHX,MESHY,neighbours)
         #print 'rayb'
         for j in range(rayi['n'])[(rayi['n']-1)::-1]:
            #print rayi['e'][j],rayi['xy'][j]
            ipt.append(rayi['xy'][j]); iet.append(rayi['e'][j]); ibr.append(rayi['b'][j])
      elif bi == [] and bo != []:
         found,rayi,neighbours = xyTraceMesh(eo,xyo,xyi,IKLE,MESHX,MESHY,neighbours)
         #print 'rayc'
         for j in range(rayi['n'])[(rayi['n']-1)::-1]:
            #print rayi['e'][j],rayi['xy'][j]
            ipt.append(rayi['xy'][j]); iet.append(rayi['e'][j]); ibr.append(rayi['b'][j])
      elif bi != [] and bo == []:
      # it is necessary to reverse the ray for a case with first end outside
         found,rayo,neighbours = xyTraceMesh(ei,xyi,xyo,IKLE,MESHX,MESHY,neighbours)
         #print 'rayd'
         for j in range(rayo['n']): #[(rayo['n']-1)::-1]:
            #print rayo['e'][j],rayo['xy'][j]
            ipt.append(rayo['xy'][j]); iet.append(rayo['e'][j]); ibr.append(rayo['b'][j])
      else:
         found,rayi,neighbours = xyTraceMesh(eo,xyo,xyi,IKLE,MESHX,MESHY,neighbours)
         #print 'rayi',rayi
         for j in range(rayi['n'])[(rayi['n']-1)::-1]:
            #print rayi['e'][j],rayi['xy'][j]
            ipt.append(rayi['xy'][j]); iet.append(rayi['e'][j]); ibr.append(rayi['b'][j])

      xyo = xyi; bo = bi; eo = ei

   return (ipt,iet,ibr),tree,neighbours


def sliceMesh(polyline,IKLE,MESHX,MESHY,tree=None):
   """
   A new method to slice through a triangular mesh (replaces crossMesh)
   """
   from matplotlib.tri import Triangulation
   xys = []
   douplets = []
   # ~~> Calculate the minimum mesh resolution
   dxy = math.sqrt(min(np.square(np.sum(np.fabs(MESHX[IKLE]-MESHX[np.roll(IKLE,1)]),axis=1)/3.0) + \
            np.square(np.sum(np.fabs(MESHY[IKLE]-MESHY[np.roll(IKLE,1)]),axis=1)/3.0)))
   accuracy = np.power(10.0, -8+np.floor(np.log10(dxy)))

   xyo = np.array(polyline[0])
   for i in range(len(polyline)-1):
      xyi = np.array(polyline[i+1])
      dio = math.sqrt(sum(np.square(xyo-xyi)))

      # ~~> Resample the line to that minimum mesh resolution
      rsmpline = np.dstack((np.linspace(xyo[0],xyi[0],num=int(dio/dxy)),np.linspace(xyo[1],xyi[1],num=int(dio/dxy))))[0]
      nbpoints = len(rsmpline)
      nbneighs = min( 8,len(IKLE) )
      # ~~> Filter closest 8 elements (please create a good mesh) as a halo around the polyline
      halo = np.zeros((nbpoints,nbneighs),dtype=np.int)
      for i in range(nbpoints):
         d,e = tree.query(rsmpline[i],nbneighs)
         halo[i] = e
      halo = np.unique(halo)

      # ~~> Get the intersecting halo (on a smaller mesh connectivity)
      edges = Triangulation(MESHX,MESHY,IKLE[halo]).get_cpp_triangulation().get_edges()

      # ~~> Last filter, all nodes that are on the polyline
      olah = []
      nodes = np.unique(edges)
      for node in nodes:  # TODO(jcp): replace by numpy calcs
         if getDistancePointToLine((MESHX[node],MESHY[node]),xyo,xyi) < accuracy: olah.append(node)
      ijsect = zip(olah,olah)
      xysect = [(MESHX[i],MESHY[i]) for i in olah]
      lmsect = [ (1.0,0.0) for i in range(len(ijsect)) ]
      mask = np.zeros((len(edges),2),dtype=bool)
      for i in olah:
         mask = np.logical_or( edges == i  , mask )
      edges = np.compress(np.logical_not(np.any(mask,axis=1)),edges,axis=0)

      # ~~> Intersection with remaining edges
      for edge in edges:
         xyj = getSegmentIntersection( (MESHX[edge[0]],MESHY[edge[0]]),(MESHX[edge[1]],MESHY[edge[1]]),xyo,xyi )
         if xyj != []:
            ijsect.append(edge)     # nodes from the mesh
            xysect.append(tuple(xyj[0]))   # intersection (xo,yo)
            lmsect.append((xyj[1],1.0-xyj[1]))   # weight along each each

      # ~~> Final sorting along keys x and y
      xysect = np.array(xysect, dtype=[('x', '<f4'), ('y', '<f4')])
      xysort = np.argsort(xysect, order=('x','y'))

      # ~~> Move on to next point
      for i in xysort:
         xys.append( xysect[i] )
         douplets.append( (ijsect[i],lmsect[i]) )
      xyo = xyi

   return xys,douplets

def tessellatePoly(i2s,debug=True):
   """
   A new method to tessellate a series of polygons, the biggest one
      being the outter domain, while the smaller ones are islands.
   The method start by building a Delaunay triangulation of all points
      before correcting and swapping those edges that do not belong.
   """

   # ~~ Ensuring Clockwisines ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if debug: print '     +> Ensuring clockwisiness of input polygons'
   if debug: print '        - Distinguishing the outter polygon'
   pol_areas = np.zeros(len(i2s.poly), dtype=np.float)
   if debug: pbar = ProgressBar(maxval=i2s.npoin).start()
   for ip in range(len(i2s.poly)):
      if i2s.type[ip] == 1:
         x,y = i2s.poly[ip].T
         pol_areas[ip] = abs( np.sum( x[:-1]*y[1:] - x[1:]*y[:-1] ) )/2.0
      if debug: pbar.update(ip)
   if debug: pbar.finish()
   pol_main = np.argmax(pol_areas)
   if debug: print '        - Forcing clockwise / anti-clockwise prerties'
   if debug: pbar = ProgressBar(maxval=i2s.npoin).start()
   if i2s.type[pol_main] == 1:
      if isClockwise(i2s.poly[pol_main]): i2s.poly[pol_main] = np.flipud(i2s.poly[pol_main])
      for ip in range(len(i2s.poly)):
         if i2s.type[ip] == 1 and ip != pol_main:
            if not isClockwise(i2s.poly[ip]): i2s.poly[ip] = np.flipud(i2s.poly[ip])
         if debug: pbar.update(ip)
   if debug: pbar.finish()

   # ~~ Sorting out polygons' edges ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if debug: print '     +> Sorting out egdes of the input polygons'
   MESHX = np.zeros(i2s.npoin)
   MESHY = np.zeros(i2s.npoin)
   pol_edges = []
   pol_nbres = np.zeros(i2s.npoin, dtype=np.int)
   cpt = 0
   if debug: pbar = ProgressBar(maxval=i2s.npoin).start()
   for ip in np.arange(len(i2s.poly),dtype=np.int)[np.argsort(pol_areas)][::-1]:
      edges = []
      # number the edges
      lpt = len(i2s.poly[ip])
      MESHX[cpt:cpt+lpt] = i2s.poly[ip].T[0][:lpt]
      MESHY[cpt:cpt+lpt] = i2s.poly[ip].T[1][:lpt]
      pol_nbres[cpt:cpt+lpt] = ip
      if i2s.type[ip] == 0:
         edges = np.arange(len(i2s.poly[ip]),dtype=np.int) + cpt
      elif i2s.type[ip] == 1:
         edges = np.arange(len(i2s.poly[ip])+1,dtype=np.int) + cpt
         edges[-1] = edges[0]
      cpt += len(i2s.poly[ip])
      if debug: pbar.update(cpt)
      pol_edges.extend(zip(edges[:-1],edges[1:]))
      pol_areas[ip] = 0.0
      if i2s.type[ip] == 1: pol_areas[ip] = \
         abs( np.sum( MESHX[edges[:-1]]*MESHY[edges[1:]] - MESHX[edges[1:]]*MESHY[edges[:-1]] ) )/2.0
   if debug: pbar.finish()
   pol_main = np.argmax(pol_areas)
   pol_edges = np.array( zip(*np.sort(np.asarray( pol_edges )).T), dtype=[ ('h',int),('t',int) ] )
   pol_edges = pol_edges[np.argsort(pol_edges,order=('h','t'))]

   # ~~ Delaunay triangulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if debug: print '     +> Delaunay triangulation of all points (outside and inside)'
   IKLE2 = Delaunay(np.dstack((MESHX,MESHY))[0]).vertices

   # ~~ Constraining the triangulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if debug: print '     +> Constraining the triangulation to the input polygon'
   # Note:
   #   Because the nodes in the polygon are ordered, it should be easy to
   #   identify when an ( edges[0] != edges[0]-1 ), and check whether it is
   #   a poly-closing edge or an edge to be swapped.
   #   /!\ that get_edges() is ordered, but not necessarily oriented

   if debug: print '        - Extracting edges from the triangulation'
   tri = Triangulation(MESHX,MESHY,IKLE2).get_cpp_triangulation()
   tri_neigh = tri.get_neighbors()
   tri_edges = np.asarray( zip(*np.sort(tri.get_edges()).T), dtype=[ ('h',int),('t',int) ] )
   tri_edges = tri_edges[np.argsort(tri_edges,order=('h','t'))]

   if debug: print '        - Searching for missing edges in the triangulation'
   ipe = 0; ite = 0
   swp_edges = []
   if debug: pbar = ProgressBar(maxval=len(pol_edges)).start()
   while ipe < len(pol_edges):
      edge = pol_edges[ipe]
      k = np.searchsorted(tri_edges['h'][ite:],edge[0])
      ite += k
      if debug: pbar.update(ipe)
      if ite == len(tri_edges):     # remaining edges not in the triangulation
         swp_edges.extend(pol_edges[ipe:])
         ipe = len(pol_edges)
      elif edge == tri_edges[ite]:  # found it first trial
         ite += 1
         ipe += 1
      elif edge[0] < tri_edges[ite][0]:
         swp_edges.append(edge)
         ipe += 1
         ite = np.searchsorted(tri_edges['h'],edge[0])
      else:
         ite += 1
   if debug: pbar.finish()

   if debug: print '        - Swapping the edges to follow input polygons'
   # Note:
   #   A ray is traced along the missing edges so to break through and rebuild
   #      the triangulation incorporating the missing edge. If there is only one
   #      edge crossing the missing edge, then a simple swap is done. Otherwise,
   #      it re-create two separate triangulations on either side of the edge
   #   n1 and n2 are the two nodes of the input polygon defining the missing
   #      edge in the triangulation
   #   ei and en are the elements including n1 and n2 respectively, the
   #      opposit segement of which intersect with the missing edge
   cpt = 0
   if debug: pbar = ProgressBar(maxval=len(swp_edges)).start()
   for n1,n2 in swp_edges:

      eis = np.extract(np.any(np.equal(n1,IKLE2),axis = 1),np.arange(len(IKLE2)))
      for ei in eis:
         ki1,ki2 = np.setxor1d([n1],IKLE2[ei])
         seg1 = mplPath.Path([[MESHX[n1],MESHY[n1]],[MESHX[n2],MESHY[n2]]])
         seg2 = mplPath.Path([[MESHX[ki1],MESHY[ki1]],[MESHX[ki2],MESHY[ki2]]])
         if seg1.intersects_path(seg2) == 1: break
      ens = np.extract(np.any(np.equal(n2,IKLE2),axis = 1),np.arange(len(IKLE2)))
      for en in ens:
         kn1,kn2 = np.setxor1d([n2],IKLE2[en])
         seg1 = mplPath.Path([[MESHX[n1],MESHY[n1]],[MESHX[n2],MESHY[n2]]])
         seg2 = mplPath.Path([[MESHX[kn1],MESHY[kn1]],[MESHX[kn2],MESHY[kn2]]])
         if seg1.intersects_path(seg2) == 1: break
      if debug: pbar.update(cpt)
      cpt += 1
      # ~~> if no segment crossing, continue (the problem may have corrected itself)
      if len(np.intersect1d(eis,ens) == 2): continue
      # ~~> if only one segment crossing, swap it
      elif ei in tri_neigh[en] and en in tri_neigh[ei]:
         if isCCW( (MESHX[n1],MESHY[n1]),(MESHX[n2],MESHY[n2]),(MESHX[ki1],MESHY[ki1]) ): IKLE2[ei] = [ n1, n2, ki1 ]
         else: IKLE2[ei] = [ ki1, n2, n1 ]
         if isCCW( (MESHX[n1],MESHY[n1]),(MESHX[n2],MESHY[n2]),(MESHX[ki2],MESHY[ki2]) ): IKLE2[en] = [ n1, n2, ki2 ]
         else: IKLE2[en] = [ ki2, n2, n1 ]
         ejs = [ ei,en ]
      # ~~> if more than one segment crossing, rebuild two local triangulations on either side of the missing edge
      else:
         ejs = [ ei ]     # elements sliced by the missing edge
         ki1s = [ n1,ki1 ]   # nodes on one side
         ki2s = [ n1,ki2 ]   # nodes on the other side
         ej = ei
         while ej != en:
            # the missing edge goes between ki1 and ki2 shared between ei and ej
            for ej in tri_neigh[ei]:
               if ej < 0: continue
               if ki1 in IKLE2[ej] and ki2 in IKLE2[ej]: break
            ejs.append(ej)
            kj0 = np.setxor1d([ki1,ki2],IKLE2[ej])[0]  # there shall be only one
            seg1 = mplPath.Path([[MESHX[n1],MESHY[n1]],[MESHX[n2],MESHY[n2]]])
            seg2 = mplPath.Path([[MESHX[ki1],MESHY[ki1]],[MESHX[kj0],MESHY[kj0]]])
            if seg1.intersects_path(seg2) == 1:
               if kj0 in ki2s: sys.exit()
               ki2s.append( kj0 )
               ki2 = kj0
            else:
               if kj0 in ki1s: sys.exit()
               ki1s.append( kj0 )
               ki1 = kj0
            ei = ej
         if n2 not in ki1s: ki1s.append(n2)
         ki1s = np.array(ki1s)
         if n2 not in ki2s: ki2s.append(n2)
         ki2s = np.array(ki2s)

         # ~~> new triangulations on either side of the missing edge
         # side one: assumed convex
         sub1_ikle2 = Delaunay(np.dstack((MESHX[ki1s],MESHY[ki1s]))[0]).vertices
         # if side one not convex
         isoxy = np.column_stack((np.sum(MESHX[ki1s[sub1_ikle2]],axis=1)/3.0,np.sum(MESHY[ki1s[sub1_ikle2]],axis=1)/3.0))
         polyPath = mplPath.Path(np.dstack((MESHX[ki1s],MESHY[ki1s]))[0])
         sub1_ikle2 = sub1_ikle2[polyPath.contains_points(isoxy)]
         # side two: assumed convex
         sub2_ikle2 = Delaunay(np.dstack((MESHX[ki2s],MESHY[ki2s]))[0]).vertices
         # if side two not convex
         isoxy = np.column_stack((np.sum(MESHX[ki2s[sub2_ikle2]],axis=1)/3.0,np.sum(MESHY[ki2s[sub2_ikle2]],axis=1)/3.0))
         polyPath = mplPath.Path(np.dstack((MESHX[ki2s],MESHY[ki2s]))[0])
         sub2_ikle2 = sub2_ikle2[polyPath.contains_points(isoxy)]

         # ~~> like for like replacement
         IKLE2[ejs] = np.concatenate((ki1s[sub1_ikle2],ki2s[sub2_ikle2]))

      # ~~> finding out the new neighbouring state
      halo = np.unique(np.concatenate((ejs,tri_neigh[ejs].ravel())))
      insiders = {}; bounders = {}
      for e,i in zip(IKLE2[halo],halo):
         nk = bounders.keys()
         for k in [0,1,2]:
            if (e[k],e[(k+1)%3]) not in nk: bounders.update({ (e[(k+1)%3],e[k]):i })
            else:
               j = bounders[(e[k],e[(k+1)%3])]
               insiders.update({(e[k],e[(k+1)%3]):[i,j]})
               del bounders[(e[k],e[(k+1)%3])]
      for e,i in zip(IKLE2[halo],halo):
         for k in [0,1,2]:
            if (e[k],e[(k+1)%3]) in insiders:
               a,b = insiders[(e[k],e[(k+1)%3])]
               if a == i: tri_neigh[i][k] = b
               if b == i: tri_neigh[i][k] = a
            if (e[(k+1)%3],e[k]) in insiders:
               a,b = insiders[(e[(k+1)%3],e[k])]
               if a == i: tri_neigh[i][k] = b
               if b == i: tri_neigh[i][k] = a

   if debug: pbar.finish()

   # ~~ Removing outside elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if debug: print '     +> Removing the outside elements'

   if debug: print '        - Computing iso-barycentres'
   isoxy = np.column_stack((np.sum(MESHX[IKLE2],axis=1)/3.0,np.sum(MESHY[IKLE2],axis=1)/3.0))

   if debug: print '        - Identifying and removing who is out'
   # pol_main is the domain boundary. The others are islands
   tri_inside = np.ones(len(IKLE2),dtype=bool)
   tri_mask = np.arange(len(IKLE2),dtype=np.int)[np.equal(*pol_nbres[np.sort(IKLE2).T[0::2]])]
   PKLE2 = pol_nbres[IKLE2.T[0]]
   cpt = 0
   if debug: pbar = ProgressBar(maxval=len(i2s.poly)).start()
   for ip in np.arange(len(i2s.poly),dtype=np.int)[np.argsort(pol_areas)][::-1]:
      polyPath = mplPath.Path(i2s.poly[ip])
      pol_mask = np.equal(ip,PKLE2[tri_mask])
      t = tri_mask[pol_mask]
      if ip == pol_main: tri_inside[t] = polyPath.contains_points(isoxy[t])
      else: tri_inside[t] = np.logical_not(polyPath.contains_points(isoxy[t]))
      tri_mask = tri_mask[np.logical_not(pol_mask)]
      if debug: pbar.update(cpt)
      cpt += 1
   if debug: pbar.finish()
   IKLE2 = IKLE2[tri_inside]

   if debug: print '     +> set IPOBO'
   IPOB2 = np.arange(i2s.npoin, dtype=np.int) + 1

   return IKLE2,IPOB2,MESHX,MESHY

def filterMeshResolution(MESHX,MESHY,IKLE2,RESOLUT,factor,debug=True):

   tree = cKDTree(np.column_stack(MESHX,MESHY))

   if debug: pbar = ProgressBar(maxval=len(RESOLUT)).start()
   while True:
      found = 0
      for io in range(len(RESOLUT)):
         xyo = [MESHX[io],MESHY[io]]
         for d,i in zip(*tree.query(xyo,50)):
            if RESOLUT[i] > RESOLUT[io] + d * factor:
               found += 1
               RESOLUT[i] = RESOLUT[io] + d * factor
         if debug: pbar.update(max(len(RESOLUT)-found,0))
      if found == 0: break
   if debug: pbar.finish()

def filterPolyResolution(i2s,factor,debug=True):

   IKLE2,IPOB2,MESHX,MESHY = tessellatePoly(i2s,debug)
   tree = cKDTree(np.column_stack(MESHX,MESHY))

   if debug: pbar = ProgressBar(maxval=len(i2s.npoin)).start()
   while True:
      found = 0
      for io in range(len(i2s.vals)):
         xyo = [MESHX[io],MESHY[io]]
         for d,i in zip(*tree.query(xyo,50)):
            if RESOLUT[i] > RESOLUT[io] + d * factor:
               found += 1
               RESOLUT[i] = RESOLUT[io] + d * factor
         if debug: pbar.update(max(len(RESOLUT)-found,0))
      if found == 0: break
   if debug: pbar.finish()


def crossCheckBoundaries(MESHX,MESHY,IKLE2,IPOB0,debug=True):

   if debug: print '     +> removing cross boundaries'
   # ~~> establish neighborhood
   tri_neigh = Triangulation(MESHX,MESHY,IKLE2).get_cpp_triangulation().get_neighbors()
   crosspts = np.where(IPOB0==4)[0]
   sout = - np.ones(len(crosspts),dtype=np.int)
   eout = np.extract(np.any(IPOB0[IKLE2]==4,axis=1),np.arange(len(IKLE2)))
   for i in range(len(crosspts)):
      if debug: print '        - node: ',crosspts[i],' ( ',MESHX[crosspts[i]],' , ',MESHY[crosspts[i]],' )'
      jout = IKLE2[eout[np.where(IKLE2[eout]==crosspts[i])[0]]]
      if len(jout) == 2: kout = 0
      else: # how about if you have more than 3 nodes connecting ?
         kout = 0
         if len(np.intersect1d(jout[0],jout[1])) == 2:
            if len(np.intersect1d(jout[1],jout[2])) == 1: kout = 2
         elif len(np.intersect1d(jout[1],jout[2])) == 1: kout = 1
      sout[i] = eout[np.where(IKLE2[eout]==crosspts[i])[0][kout]]
      # removing only one of the elements
   mask = np.ones(len(IKLE2),dtype=np.bool)
   mask[sout] = False

   return IKLE2[mask]

def getIPOBO(MESHX,MESHY,IKLE2,debug=True):

   if debug: print '     +> sorting boundary nodes'
   # ~~> establish neighborhood
   tri_neigh = Triangulation(MESHX,MESHY,IKLE2).get_cpp_triangulation().get_neighbors()
   ebounds = []
   if debug: print '        - identify'
   pbar = ProgressBar(maxval=len(IKLE2)).start()
   for i in range(len(IKLE2)):
      if tri_neigh[i,0] < 0: ebounds.append([IKLE2[i][0],IKLE2[i][1]])
      if tri_neigh[i,1] < 0: ebounds.append([IKLE2[i][1],IKLE2[i][2]])
      if tri_neigh[i,2] < 0: ebounds.append([IKLE2[i][2],IKLE2[i][0]])
      pbar.update(i)
   pbar.finish()
   # ~~> assemble the enssemble of boundary segments
   if debug: print '        - assemble'
   pbounds = joinSegments(ebounds)
   # ~~> define IPOBO from an arbitrary start point
   if debug: print '        - set'
   IPOB2 = np.zeros(len(MESHX),dtype=np.int)
   iptfr = 0
   for p in pbounds:
      for n in p[1:]:
         iptfr += 1
         IPOB2[n] = iptfr

   return IPOB2,pbounds

def showBoundaryNodes(MESHX,MESHY,IKLE2,debug=True):

   if debug: print '     +> highlight boundary nodes'
   # ~~> establish neighborhood
   tri_neigh = Triangulation(MESHX,MESHY,IKLE2).get_cpp_triangulation().get_neighbors()
   # ~~> check boundary nodes
   IPOB0 = np.zeros(len(MESHX),dtype=np.int)
   pbar = ProgressBar(maxval=len(IKLE2)).start()
   for i in range(len(IKLE2)):
      if tri_neigh[i,0] < 0:
         IPOB0[IKLE2[i][0]] += 1
         IPOB0[IKLE2[i][1]] += 1
      if tri_neigh[i,1] < 0:
         IPOB0[IKLE2[i][1]] += 1
         IPOB0[IKLE2[i][2]] += 1
      if tri_neigh[i,2] < 0:
         IPOB0[IKLE2[i][2]] += 1
         IPOB0[IKLE2[i][0]] += 1
      pbar.update(i)
   pbar.finish()

   return IPOB0

def showNodeConnections(MESHX,MESHY,IKLE2,debug=True):

   if debug: print '     +> count node connections'
   # ~~> establish neighborhood
   tri_edges = Triangulation(MESHX,MESHY,IKLE2).get_cpp_triangulation().get_edges()
   # ~~> count node connections
   IPOB1 = np.zeros(len(MESHX),dtype=np.int)
   for e in tri_edges: IPOB1[e] += 1

   return IPOB1

def mergeMin4Nodes(MESHX,MESHY,IKLE2,WHERE,debug=True):

   if debug: print '     +> removing nodes connected to 4 or less others'
   # ~~> preparing masks
   mask_IKLE2 = np.ones(len(IKLE2),dtype=bool)
   mask_NPOIN = np.ones(len(MESHX),dtype=bool)
   for ipoin in WHERE:
      # ~~> elements surounding the problematic node
      w = np.where( IKLE2 == ipoin )[0]
      # ~~> nodes surounding the problematic node
      n1 = np.setdiff1d(np.unique(IKLE2[w]),[ipoin])[0]
      # ~~> two elements are to be masked
      for e in w:
         if n1 in IKLE2[e]: mask_IKLE2[e] = False
         else:
            for i in range(3):
               if IKLE2[e][i] == ipoin: IKLE2[e][i] = n1
      mask_NPOIN[ipoin] = False # you need renumbering
      # TODO: search for the opposing edges before joining -- or swap if needed

   return mask_NPOIN,IKLE2[mask_IKLE2]


def cleaveMax7Nodes(MESHX,MESHY,IKLE2,WHERE,debug=True):

   if debug: print '     +> cleaving nodes connected of '
   # ~~> preparing masks
   add_IKLE2 = []
   add_MESHX = []
   add_MESHY = []
   for ipoin in WHERE:
      # ~~> elements surounding the problematic node
      w = np.where( IKLE2 == ipoin )[0]
      edges = Triangulation(MESHX,MESHY,IKLE2[w]).get_cpp_triangulation().get_edges()
      edge  = edges[0]
      # ~~> average distance between all nodes
      dist = 0.; dx = 0.; dy = 0.
      for edge in edges:
         dxmin = max(MESHX[edge]) - min(MESHX[edge])
         dymin = max(MESHY[edge]) - min(MESHY[edge])
         dist += math.sqrt( dxmin*dxmin + dymin*dymin )/len(edges)
         dx += dxmin/len(edges)
         dy += dymin/len(edges)
      print dist,dx,dy
      # ~~> nodes surounding the problematic node
      n = np.setdiff1d(np.unique(IKLE2[w]),[ipoin])
      xc = np.sum(MESHX[n])/len(n)
      yc = np.sum(MESHY[n])/len(n)
      print xc,yc,xc+dist*dx/2./math.sqrt(dx*dx+dy*dy),yc+dist*dy/2./math.sqrt(dx*dx+dy*dy)

      sys.exit()

      dxmin = min( MESHX[n][1:] - MESHX[n][:-1] )
      dymin = min( MESHY[n][1:] - MESHY[n][:-1] )
      dist = math.sqrt( dxmin*dxmin + dymin*dymin )
      dxmax = abs(max( MESHX[n][1:] - MESHX[n][:-1] ))
      dymax = abs(max( MESHY[n][1:] - MESHY[n][:-1] ))
      print ipoin,xc,yc,dist,dxmax,dymax
      # ~~> two elements are to be added
      #print ipoin,w,n
   sys.exit()
   pass

def removeDuplicateNodes(MESHX,MESHY,IKLE2,alpha,debug=True):
   # /!\ just checking ...
   # TODO: implement the removal / merge of the duplicated nodes

   if debug: print '     +> checking for duplicated nodes'
   tree = cKDTree(np.column_stack((MESHX,MESHY)))

   found = False
   if debug: pbar = ProgressBar(maxval=len(MESHX)).start()
   for j in range(len(MESHX)): # TODO: parallel implementation

      # ~~> subset of points (one closest nodes should be OK)
      i = np.setdiff1d(tree.query([MESHX[j],MESHY[j]],2)[1],[j])[0]
      dist = math.sqrt( ( MESHX[j]-MESHX[i] )*( MESHX[j]-MESHX[i] ) + ( MESHY[j]-MESHY[i] )*( MESHY[j]-MESHY[i] ) )
      if dist < alpha:
         found = True
         pbar.write( '        - distance between nodes:' + str(j) + ' and ' + str(i) + ' is ' + repr(dist),j )

      if debug: pbar.update(j)
   if debug: pbar.finish()

   #if found:
   return MESHX,MESHY,IKLE2


def removeExtraNodes(MESHX,MESHY,IKLE2,debug=True):

   if debug: print '     +> removing extra nodes'
   # ~~> KNOLG(NPOIN3) gives the global node number such that
   #   for i = 1,NPOIN3: Fwrite(i) = Fread(KNOLG(i)) and is ordered
   KNOLG = np.sort( np.unique( np.ravel(IKLE2), return_index=True )[0] )
   KNOGL = dict(zip( KNOLG,range(len(KNOLG)) ))
   LIKLE = - np.ones_like(IKLE2,dtype=np.int)
   if debug:
      print '        - renumbering connectivity'
      pbar = ProgressBar(maxval=len(IKLE2)).start()
   for k in range(len(IKLE2)):
      LIKLE[k] = [ KNOGL[IKLE2[k][0]], KNOGL[IKLE2[k][1]], KNOGL[IKLE2[k][2]] ]
      if debug: pbar.update(k)
   if debug:
      pbar.finish()
      print '        - removing extra nodes from mesh'
      pbar = ProgressBar(maxval=len(IKLE2)).start()
   for k in KNOLG:
      MESHX[KNOGL[k]] = MESHX[k]
      MESHY[KNOGL[k]] = MESHY[k]
      if debug: pbar.update(k)
   if debug: pbar.finish()

   return MESHX[:len(KNOLG)],MESHY[:len(KNOLG)],LIKLE

def mapThinPlateSpline(MESHX,MESHY,BATHX,BATHY,BATHZ,npoin,alpha,debug=True):

   if debug: print '     +> creating a proximity tree'
   tree = cKDTree(np.column_stack((BATHX,BATHY)))
   # ~~> mask for the proximity points
   mask = np.zeros(npoin,dtype=np.int)

   VARSOR = np.zeros(len(MESHX),dtype=np.float)

   if debug:
      print '     +> thin plate spline mapping'
      pbar = ProgressBar(maxval=len(MESHX)).start()
   A = np.zeros((npoin+3,npoin+3),dtype=np.float)
   row = np.zeros(npoin+3,dtype=np.float)
   for j in range(len(MESHX)): # TODO: parallel implementation

      # ~~> subset of points
      mask = tree.query([MESHX[j],MESHY[j]],npoin)[1]   # indices
      # ~~> weight
      dt = 0.
      for i in range(npoin):
         dt += np.sum( 2*np.sqrt(np.power(BATHX[mask][i]-BATHX[mask][i+1:],2) + np.power(BATHY[mask][i]-BATHY[mask][i+1:],2)) )
      dt = dt / (npoin*npoin)

      # ~~> main matrix
      A = A * 0.              # resetting A
      # ~~> upper triangular elements
      for i in range(npoin):
         row = row * 0.       # reseting row
         xi = BATHX[mask][i] * np.ones(npoin-(i+1))
         yi = BATHY[mask][i] * np.ones(npoin-(i+1))
         ra = np.sqrt(np.square(BATHX[mask][(i+1):]-xi)+np.square(BATHY[mask][(i+1):]-yi))
         ra[np.where(ra==0)[0]] = 0.001
         rowslice = np.square(ra)*np.log(ra) # / 2.0 ?
         row[i] = alpha * dt * dt
         row[(i+1):(i+1+len(rowslice))] = rowslice
         row[(npoin):] = np.array([1,BATHX[mask][i],BATHY[mask][i]])
         A[i] = row

      # ~~> build a symetric matrix of A
      M = np.mat(A)
      M = M + M.T

      # ~~> second member
      b = np.append(BATHZ[mask],[0,0,0])

      # ~~> linear algebra
      a = np.linalg.solve(M,b)

      # ~~> resulting interpolation
      ra=np.sqrt(np.square(BATHX[mask]-MESHX[j]*np.ones(npoin))+np.square(BATHY[mask]-MESHY[j]*np.ones(npoin)))
      # /!\ must check for NaN from calculating log(0) ... Thank you David Roscoe !
      VARSOR[j] = a[-3]+a[-2]*MESHX[j]+a[-1]*MESHY[j]+np.nansum(np.square(ra)*a[:npoin]*np.log(ra))

      if debug: pbar.update(j)
   if debug: pbar.finish()

   return VARSOR


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$12-Dec-2012 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Command line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nInterpreting command line options\n'+'~'*72+'\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-x","--crosses",action="store_true",dest="xpts",default=False,help="check and remove cross boundary points" )
   parser.add_option("-4","--minnode",action="store_true",dest="mnod",default=False,help="check and merge interior nodes with less than 5 neighbours" )
   parser.add_option("-8","--maxnode",action="store_true",dest="xnod",default=False,help="check and cleave interior nodes with more than 7 neighbours" )
   parser.add_option("--dupnode",action="store_true",dest="dupnode",default=False,help="check and remove duplicated nodes" )
   parser.add_option("--map",type="string",dest="map",default=None,help="name of the points for mapping (thin plate spline)" )
   parser.add_option("-n","--cloudsize",type="string",dest="npoin",default=None,help="size of the proximity cloud (thin plate spline)" )
   parser.add_option("-r","--smoothing",type="string",dest="alpha",default=None,help="smoothing factor (thin plate spline)" )
   options, args = parser.parse_args()
   if len(args) < 1:
      print '\nAt least a code name (and its associated inputs) are required\n'
      parser.print_help()
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   slfFile = args[0]
   outFile = args[1]
   slf = SELAFIN(slfFile)

   # ~~> Monitoring changes
   updt_IPOBO = False
   updt_NPOIN = False

   # ~~> check and remove boundary nodes where boundary lines intersect
   if options.xpts:
      # IPOB0 is 0 for inside node, or the noumber of boundary segment attached
      # np.sum((IPOB0[slf.IKLE2]>0),axis=1) > 1 highlights elements with at least two boundary nodes
      IPOB0 = showBoundaryNodes(slf.MESHX,slf.MESHY,slf.IKLE2,debug=True)
      if max(IPOB0) > 2:
         slf.IKLE2 = crossCheckBoundaries(slf.MESHX,slf.MESHY,slf.IKLE2,IPOB0,debug=True)
         updt_IPOBO = True
         slf.IKLE3 = slf.IKLE2
         slf.NELEM2 = len(slf.IKLE2)
         slf.NELEM3 = slf.NELEM2

   # ~~> remove inside nodes that have 4 or less connecting nodes
   if options.mnod:
      # IPOB0 is 0 for inside node, or the noumber of boundary segment attached
      IPOB0 = showBoundaryNodes(slf.MESHX,slf.MESHY,slf.IKLE2,debug=True)
      # IPOB1 counts the number of connections around a node
      IPOB1 = showNodeConnections(slf.MESHX,slf.MESHY,slf.IKLE2,debug=True)
      if np.any(np.logical_and(IPOB1<5,IPOB0==0)):
         mask_NPOIN,slf.IKLE2 = mergeMin4Nodes(slf.MESHX,slf.MESHY,slf.IKLE2,np.where(np.logical_and(IPOB1<5,IPOB0==0))[0],debug=True)
         updt_IPOBO = True
         updt_NPOIN = True
         slf.IKLE3 = slf.IKLE2
         slf.NELEM2 = len(slf.IKLE2)
         slf.NELEM3 = slf.NELEM2

   # ~~> cleave inside nodes that have 8 or more connecting nodes
   if options.xnod:
      # IPOB0 is 0 for inside node, or the noumber of boundary segment attached
      IPOB0 = showBoundaryNodes(slf.MESHX,slf.MESHY,slf.IKLE2,debug=True)
      # IPOB1 counts the number of connections around a node
      IPOB1 = showNodeConnections(slf.MESHX,slf.MESHY,slf.IKLE2,debug=True)
      if np.any(np.logical_and(IPOB1>7,IPOB0==0)):
         mask_NPOIN,slf.IKLE2 = cleaveMax7Nodes(slf.MESHX,slf.MESHY,slf.IKLE2,np.where(np.logical_and(IPOB1>7,IPOB0==0))[0],debug=True)
         updt_IPOBO = True
         updt_NPOIN = True
         slf.IKLE3 = slf.IKLE2
         slf.NELEM2 = len(slf.IKLE2)
         slf.NELEM3 = slf.NELEM2

   # ~~> check and remove duplicated nodes
   if options.dupnode:
      alpha = 1.0 # minimum resolution allowed
      if options.alpha: alpha = float(options.alpha)
      slf.MESHX,slf.MESHY,slf.IKLE2 = removeDuplicateNodes(slf.MESHX,slf.MESHY,slf.IKLE2,alpha,debug=True)
      updt_IPOBO = True
      updt_NPOIN = True
      slf.NPOIN2 = len(slf.MESHX)
      slf.NPOIN3 = slf.NPOIN2
      slf.IKLE3 = slf.IKLE2

   # ~~> remove singled out nodes, and renumber IKLE accordingly
   if updt_NPOIN:
      slf.MESHX,slf.MESHY,slf.IKLE2 = removeExtraNodes(slf.MESHX,slf.MESHY,slf.IKLE2,debug=True)
      slf.NPOIN2 = len(slf.MESHX)
      slf.NPOIN3 = slf.NPOIN2
      slf.IKLE3 = slf.IKLE2

   if updt_IPOBO:
      # ~~> Effective way to limit the search and sort for boundary nodes
      # IPOB0 is 0 form inside node, or the noumber of boundary segment attached
      # np.sum((IPOB0[slf.IKLE2]>0),axis=1) > 1 highlights elements with at least two boundary nodes
      IPOB0 = showBoundaryNodes(slf.MESHX,slf.MESHY,slf.IKLE2,debug=True)
      mask = np.sum((IPOB0[slf.IKLE2]>0),axis=1) > 1
      slf.IPOB2,_ = getIPOBO(slf.MESHX,slf.MESHY,slf.IKLE2[mask],debug=True)
      slf.IPOB3 = slf.IPOB2

   VARSOR = np.zeros(len(slf.MESHX),dtype=np.float)
   if options.map:
      map = SELAFIN(options.map)
      npoin = min( 20,map.NPOIN2 )
      if options.npoin: npoin = min( int(options.npoin),map.NPOIN2 )
      alpha = 0.2
      if options.alpha: alpha = float(options.alpha)
      VARSOR = mapThinPlateSpline(slf.MESHX,slf.MESHY,map.MESHX,map.MESHY,map.getVariablesAt(0,[0])[0],npoin,alpha,debug=True)

   #slf.putContent(outFile)

   # ~~> new SELAFIN writer
   slf.fole = {}
   slf.fole.update({ 'hook': open(outFile,'wb') })
   slf.fole.update({ 'name': outFile })
   slf.fole.update({ 'endian': ">" })     # big endian
   slf.fole.update({ 'float': ('f',4) })  # single precision

   print '     +> Write SELAFIN header'
   slf.appendHeaderSLF()

   print '     +> Write SELAFIN core'
   slf.appendCoreTimeSLF(0.0)
   slf.appendCoreVarsSLF([VARSOR])
   slf.fole['hook'].close()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
