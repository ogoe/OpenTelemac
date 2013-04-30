#!/usr/bin/env python
#

__author__="jcp"
__date__ ="$23-Apr-2013 10:24:01$"

from os import getcwd,path,sep
import sys
from numpy import array,ones
import matplotlib.pyplot as plt 

################################################################################
#####    Will allow to use pytel scripts from the test case folder    ##########
################################################################################

YouAreHere =  getcwd()
pathsplit = YouAreHere.split(sep)
p = pathsplit[0]+sep
for i in range(len(pathsplit[0:-7])):
   i+=1
   p = path.join(p,pathsplit[i])
pytelpath = path.join(p,'pytel') 
sys.path.append(pytelpath)

################################################################################
#####    Dependencies towards other pytel/modules                     ##########
################################################################################

from parsers.parserSELAFIN import SELAFIN,subsetVariablesSLF,getValuePolylineSLF
from parsers.parserCSV import putDataCSV,getDataCSV
from samplers.meshes import crossMesh

################################################################################
#####                    MAIN PROGRAM                                 ##########
################################################################################

if __name__ == "__main__":
    
    ############################################################################
    #####                    Importing data                               ######
    ############################################################################

    # Cross section from Selafin file
      
    slf = SELAFIN("sis_sandpit.slf")
    
    variable = 'bottom:line'
    coordinates = array([[50.0,0.5],[130.0,0.5]])
    timef = [20]
    
    vars = subsetVariablesSLF(variable,slf.VARNAMES)
    support,tree,neighbours = crossMesh(coordinates,slf.IKLE,slf.MESHX,slf.MESHY)
    xf,yf = getValuePolylineSLF(slf.file,slf.tags,timef,(support[0],slf.IKLE[support[1]],support[2]),slf.TITLE,slf.NVAR,slf.NPOIN3,vars)
 
    Xaxis = xf[1]
    BedFinal = yf[0][3][0][0]
    
    # Experiment data from CSV file
    # always write the variable name in lower case

    file = 'experiment_data.csv'
    data = getDataCSV(file,4)
    
    x = data['x']
    BedInitial = data['bedinitial']*10
    BedMeasured = data['bedmeasured']*10

    ############################################################################
    #####                        CALCS                                    ######
    ############################################################################
     
    o = ones(82)*50
    Xmodel = Xaxis + o
    
    ############################################################################
    #####             Wrtting data in CSV file                            ######
    ############################################################################
    
    # the first two values of a column must be the variable name and the 
    # variable unit (without any space)
    
    ModelBed = ['ModelBedLevel','m'] + BedFinal.tolist()
    ExpBed = ['MeasuredBedLevel','m'] + BedMeasured.tolist()
        
    columns = [ModelBed,ExpBed]
    putDataCSV('Bed_level.csv',columns)
    
    ############################################################################
    #####                      PLOTS                                      ######
    ############################################################################
    
    fig = plt.figure()
    
    plt.plot(x*10,BedInitial,'k',label='Initial bed')
    plt.plot(x*10,BedMeasured, 'b', label='Measured')
    plt.plot(Xmodel,BedFinal, 'r', label='Modelled')
    plt.xlabel('location(m)')
    plt.ylabel('Bed level (m)')

    plt.legend(loc='lower right')
    plt.title('Comparison of model and experiment morphology' )
    filename = 'Sandpit_2_profiles'+ '.png'
    plt.savefig(filename, dpi=100)
    print 'Wrote file', filename
    plt.clf()
    
    sys.exit() 
	
	
	
	
	
