#!/usr/bin/env python
"""Provides a function to plot the validation example for YEN from Telemac csv-files

We use the python built in csv-reader. There is no CSV standard we asume that there 
is a Telemac standard

Please notice: 
    - The evolution must be devided by 0.0544
    - The following lines are needed on a server without graphical screen  
        import matplotlib as mpl
        mpl.use('Agg')

"""
import matplotlib as mpl
mpl.use('Agg')
from os import getcwd,path,sep
import sys
import csv
import numpy as np
import matplotlib.pyplot as plt 
import matplotlib.tri as tri

################################################################################
#####    Will allow to use pytel scripts from the test case folder    ##########
################################################################################
actualPath =  getcwd()
pathsplit = actualPath.split(sep)
pytelpath = ""
pytelpath += sep

for i in range(len(pathsplit)):
    if "example" in pathsplit[i]:
        break
    if len(pathsplit[i]) > 0:
        pytelpath = path.join(pytelpath,pathsplit[i])
 
pytelpath = path.join(pytelpath,'scripts','python27')
sys.path.append(pytelpath)

#hardcoded workarround for paths
#sys.path.append("/panfs/sw/apps/telemac/v6p3_wels/wels/scripts/python27")

################################################################################
#####    Dependencies towards other pytel/modules                     ##########
################################################################################

import parsers.parserSELAFIN as parserSlf

__authors__="Rebekka Kopmann, Leopold Stadler"
__maintainer__ = "Rebekka Kopmann"
__version__ = "1.0"



def getPositionOfVariable(varnames,searchName):

   for i,name in enumerate(varnames):
      if name.strip() == searchName:
         return i

   print "Exit Variable %s not found in varnames" %(searchName)
   sys.exit()

   return

if __name__ == "__main__":

   print "debug",getcwd()

   if len(sys.argv) < 2:
      sys.exit("No arguments please give an input file and outputname")

   #########################################################################
   #
   # factor is used for scaling (evolution/h0_factor = evolutionCut)
   # See the paper of Yen et al. 1995
   #########################################################################
    
   selafinfile = sys.argv[1]
   slf = parserSlf.SELAFIN(selafinfile) 
   mesh = np.array(slf.IKLE3)
   triang = tri.Triangulation(slf.MESHX,slf.MESHY,mesh)
   timePos = len(slf.tags["times"]) -1

   values = slf.getVALUES(timePos)
   varPos = getPositionOfVariable(slf.VARNAMES,'EVOLUTION')
   evolution = values[varPos]
   evolution_interpolator = tri.LinearTriInterpolator(triang,evolution)

   #########################################################################
   #
   # Read the reference file
   #
   #########################################################################
   h0_factor = 0.0544
    
   #load profile 90 
   data_profile_90 =  np.genfromtxt("yen_90profilexy-koor.dat",names=["x","y","z"])
   profileRef90X = data_profile_90["x"]
   profileRef90Y = data_profile_90["y"]
   profileRef90Z = data_profile_90["z"]
   evolutionCut_90 = evolution_interpolator.__call__(profileRef90X,profileRef90Y)/h0_factor

   distance_profile_90 = profileRef90Y - 46.1371

   #load profile 180 
   data_profile_180 =  np.genfromtxt("yen_180profilexy-koor.dat",names=["x","y","z"])
   profileRef180X = data_profile_180["x"]
   profileRef180Y = data_profile_180["y"]
   profileRef180Z = data_profile_180["z"]
   evolutionCut_180 = evolution_interpolator.__call__(profileRef180X,profileRef180Y)/h0_factor

   distance_profile_180 = profileRef180X - 104.682


   #########################################################################
   #
   # Plot with matplotlib
   #
   #########################################################################

   #plot 90
   plt.plot(distance_profile_90, profileRef90Z, "o-",color="darkorange", label = "Reference")
   plt.plot(distance_profile_90, evolutionCut_90,"<--",color="green", label = "Simulation")
   plt.xlabel("Distance [m]")
   plt.ylabel("Evolution [m]")
   plt.grid()
   plt.xlim([0.0,1.0])
   plt.legend()
   title = sys.argv[2] +"_90"
   plt.title(title)
   plt.savefig(title + ".pdf")
   plt.savefig(title + ".png")
   plt.clf()

   #plot 180
   plt.plot(distance_profile_180, profileRef180Z, "o-",color="darkorange", label = "Reference")
   plt.plot(distance_profile_180, evolutionCut_180,"<--",color="green", label = "Simulation")
   plt.xlabel("Distance [m]")
   plt.ylabel("Evolution [m]")
   plt.grid()
   plt.xlim([0.0,1.0])
   plt.legend()
   title = sys.argv[2] + "_180"
   plt.title(title)
   plt.savefig(title + ".pdf")
   plt.savefig(title + ".png")
   plt.clf()


