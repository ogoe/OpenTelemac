#  Copyright (C) 2012-2015 EDF
#
#  This file is part of SALOME HYDRO module.
#
#  SALOME HYDRO module is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  SALOME HYDRO module is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with SALOME HYDRO module.  If not, see <http://www.gnu.org/licenses/>.

import math
from itertools import izip,cycle

EPS = 10**-9

def is_in_polygon(x,y,poly):
    """
    Returns True if the point (x,y) is in the polygon poly
    Warning the polygon must be distinct (no indentical points)
    """
    angle = -1.0
    while True:
        angle += 1.0
        if(angle > 360.0):
          # Special case of a point on the contour
          return True
        a = math.cos(angle*math.pi/180.0)
        b = math.sin(angle*math.pi/180.0)
        nsect = 0
        # Loop on all the segment of the polygon
        # TODO: Pythonize that loop
        n = len(poly)
        next_angle = False
        for i in range(0,n):
            xdep,ydep = poly[i]
            # next element
            xarr,yarr = poly[(i+1) % n]
            # Case the point is on the polygon
            if (abs(x-xdep)<EPS) and (abs(y-ydep)<EPS):
                nsect = 1
                break            
            det = a*(ydep-yarr) - b*(xdep-xarr)
            if abs(det) < EPS:
               next_angle = True
               break

            mu      = ( (xdep-x)*(ydep-yarr)-(ydep-y)*(xdep-xarr) ) / det
            llambda = (    a    *(ydep-y   )-    b   *(xdep-x   ) ) / det
            # if the intersection point is a vertex, increases the angle
            # otherwise the point would be counted twice instead of just once
            if ((abs(x+a*mu-xdep) <= EPS and abs(y+b*mu-ydep) <= EPS) or\
                (abs(x+a*mu-xarr) <= EPS and abs(y+b*mu-yarr) <= EPS)):
                next_angle = True
                break
            if(mu >= -EPS and llambda >= -EPS and llambda <= (1.0+EPS)):
                nsect += 1
        # If     
        if not next_angle:
            break

    return (nsect % 2) == 1

def get_list_points_in_polygon(t2d_inst, poly):
    """
    Returns the list of points from the mesh that are in the polygon poly

    t2d_inst : is the Telemac2D instance
    poly : is a list of (x,y) coordiantes defining the polygon
    """
    # Get the number of point in the mesh
    npoin = t2d_inst.get_integer('MODEL.NPOIN')
    point_in_polygon = []
 
    # Loop on all mesh points
    for i in range(1, npoin + 1):
        # Get the cooridnates of point i
        x = t2d_inst.get_double('MODEL.X', i)
        y = t2d_inst.get_double('MODEL.Y', i)
        # If point is in polygon add it to the list of points
        if is_in_polygon(x,y,poly):
            point_in_polygon.append(i)
    return point_in_polygon

if __name__ == "__main__":
    l = [(0.0,0.0),(0.0,1.0),(1.0,2.0),(2.0,2.0),(2.0,0.0)]
    print is_in_polygon(1.0,1.0,l),True
    print is_in_polygon(0.0,0.0,l),True
    print is_in_polygon(-1.0,1.0,l),False
    print is_in_polygon(2.0,0.0,l),True
