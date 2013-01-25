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
"""
"""@brief
      Tools for sampling and interpolating through triangular meshes
"""
"""@details
         Contains ...
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path
import numpy as np
# ~~> dependencies towards other pytel/modules
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
from parsers.parserSELAFIN import SELAFIN
from utils.geometry import getSegmentLineIntersection,getSegmentIntersection,getBarycentricWeights,isInsideTriangle
from utils.files import putFileContent

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
      from scipy.spatial import cKDTree
      isoxy = np.column_stack((np.sum(MESHX[IKLE],axis=1)/3.0,np.sum(MESHY[IKLE],axis=1)/3.0))
      tree = cKDTree(isoxy)
   # ~~> Find the indices corresponding to the nearest elements to the points
   dnear,inear = tree.query(xyo)

   # ~~> Find the indices and weights corresponding to the element containing the point
   ax,bx,cx = MESHX[IKLE[inear]]
   ay,by,cy = MESHY[IKLE[inear]]

   return inear,isInsideTriangle( xyo,(ax,ay),(bx,by),(cx,cy) ),tree

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
      from matplotlib.tri import Triangulation
      neighbours = Triangulation(MESHX,MESHY,IKLE).get_cpp_triangulation().get_neighbors()
   found,ray = traceRay2XY(IKLE,MESHX,MESHY,neighbours,inear,xyi,inear,xyo)

   return found,ray,neighbours


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

def xysLocateMesh(xys,IKLE,MESHX,MESHY,tree=None,neighbours=None):
   # ~~ Re-sampling nodes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ipt = []; iet = []; ibr = []
   
   for xyo in xys:
      # ~~> get to the nearest element
      oet = -1; obr = [0.0,0.0,0.0]
      eo,bo,tree = nearLocateMesh(xyo,IKLE,MESHX,MESHY,tree)
      if bo == []:
         found,ray,neighbours = xyTraceMesh(eo,[np.sum(MESHX[IKLE[eo]])/3.0,np.sum(MESHY[IKLE[eo]])/3.0],xyo,IKLE,MESHX,MESHY,neighbours)
         if found:
            obr = ray['b'][ray['n']]
            oet = ray['e'][ray['n']]
      else:
         obr = bo
         oet = eo
      ipt.append(xyo); iet.append(oet); ibr.append(obr)

   return (ipt,iet,ibr),tree,neighbours

def crossMesh(polyline,IKLE,MESHX,MESHY,tree=None,neighbours=None):
   """
   """
   # ~~ Intersection nodes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ipt = []; iet = []; ibr = []
   
   # ~~ Locate nodes of the polyline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xelms = []; xbrys = []
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
            sys.exit()
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

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$12-Dec-2012 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()
