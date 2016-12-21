#!/usr/bin/python
# -*- coding: UTF-8 -*-
import os, sys, re

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from   mpl_toolkits.mplot3d import Axes3D
import matplotlib.tri as mtri
import mtlplots

from mtlplots.plotTELEMAC import getColourMap

relativeColorMapsPath = os.path.join('scripts','python27','ColourMaps')
homePath=os.environ['HOMETEL']

def showNpArray(name, ar ) :           
    print "%s(%d)=%s" %(name, len(ar), ar)
def showVal(name, ar ) :           
    print "%s=%s" %(name,  ar)
 
def myPlot2dPerspective3d( data, codeName, caseName, timeIndexs=[0], nameIndex=0,  colorMapFileName='jet.xml', ext=".png", zlimits=None  ) :
    """\
exeaple :    
    codeName="telemac2d"
    caseName="gouttedo"    
    """
    print "myPlot2dPerspective3d ..."
    
    colorsPath = os.path.join( homePath, relativeColorMapsPath,colorMapFileName)            
    cmap = matplotlib.colors.LinearSegmentedColormap('user',getColourMap(colorsPath))        
        
    trvPath = os.path.join( homePath,'examples',codeName,caseName)    
    
    values = data.values
    names = data.names
    support = data.support
    time  = data.time
    print "time array element bumber", len(time)
    x =  support[0]
    y =  support[1]
    
    triang = mtri.Triangulation(x, y)
    
    indexes =  support[2]
    
    fig = plt.figure()    
    
    subPlotNumber = len(timeIndexs)
    
    subPlotIndex = 0    
    for timeIndex in timeIndexs :
        subPlotIndex += 1
        
        ax = fig.add_subplot(  subPlotNumber , 1,subPlotIndex, projection='3d')    
        xtime = time[timeIndex]
        showVal( "time", xtime )
        timeValues = values[ timeIndex ]
        nameValues = timeValues[ nameIndex ]
        z = nameValues            
        ax.autoscale(enable=True, axis='both', tight=True)
        #ax.set_xlabel('x',fontsize=10)
        #ax.set_ylabel('y',fontsize=10)
        #ax.set_zlabel('z',fontsize=10)
        if zlimits != None :
            ax.set_zlim( zlimits[0], zlimits[1])
        ax.plot_trisurf(triang, z, cmap=cmap, linewidth=0.0, antialiased=True, edgecolor='none')
        
        #ax.set_xlabel("%s at t=%1.1fs" % (names[nameIndex].strip(),time[timeIndex] ) ) 
        #ax.suptitle("%s at t=%1.1fs" % (names[nameIndex].strip(),time[timeIndex] ), verticalalignment="baseline", fontsize=12)
    
    plt.savefig( trvPath + os.sep + "img/img%s_time%d_name%d_pers3d%s" % (caseName, timeIndex, nameIndex, ext), dpi=200, bbox_inches='tight')
    print "... myPlot2dPerspective3d"
if __name__ == u'__main__' :    
    pass