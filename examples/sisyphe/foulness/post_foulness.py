#!/usr/bin/env python
#

__author__="jcp"
__date__ ="$23-Apr-2013 10:24:01$"

from os import getcwd,path,sep
import sys
from numpy import power
import matplotlib.pyplot as plt 

################################################################################
#####    Will allow to use pytel scripts from the test case folder    ##########
################################################################################

YouAreHere =  getcwd()
pathsplit = YouAreHere.split(sep)
p = pathsplit[0]+sep
for i in range(len(pathsplit[0:-6])):
   i+=1
   p = path.join(p,pathsplit[i])
pytelpath = path.join(path.join(p,'scripts') ,'python27')
sys.path.append(pytelpath)

################################################################################
#####    Dependencies towards other pytel/modules                     ##########
################################################################################

from parsers.parserSELAFIN import SELAFIN
from parsers.parserCSV import CSV

################################################################################
#####                    MAIN PROGRAM                                 ##########
################################################################################

def putDataCSV(file,columns):
   if path.exists(file): remove(file)
   csvF = open(file,'wb')
   csvF.write('#\n')
   csvF.write('#\n')
   for j in range(len(columns[0])):
      for i in range(len(columns)):
         if i == (len(columns)-1) : csvF.write(str(columns[i][j])+'\n')
         else : csvF.write(str(columns[i][j])+',')
   csvF.close()
   return

if __name__ == "__main__":
    
    ############################################################################
    #####                    Importing data                               ######
    ############################################################################

    # Times Series from Selafin file
    # 5113 is the node number
    # 0,1,2,8 are the variable indexes
      
    slf = SELAFIN("sis_foulness.slf")
	   
    series = slf.getSERIES([5113],[0,1,2,8])
    	
    u = series[0][0]
    v = series[1][0]
    h = series[2][0]
    QSsuspension = series[3][0]

    # Experiment data from CSV file
    # always write the variable name in lower case

    csv = CSV()
    csv.getFileContent('fielddata.csv')
    t,QSexp = csv.getColumns('qs')
       
    ############################################################################
    #####                        CALCS                                    ######
    ############################################################################
       
    speed = power( (power(u,2) + power(v,2)),(1.0/2.0) )
    k = (power(speed,3))/h
    QSmodel = QSsuspension * 2650000.0
    QSexpEBB = QSexp[1][0][15:26]*h[4:15]* speed[4:15]
    QSexpFLOOD = QSexp[1][0][3:14]*h[15:26]* speed[15:26]
    
    ############################################################################
    #####             Writing data in CSV file                           ######
    ############################################################################
    
    # the first two values of a column must be the variable name and the 
    # variable unit (without any space)
    
    velocityEBB = ['Velocity','m/s'] + speed[4:15].tolist()
    velocityFLOOD = ['Velocity','m/s'] + speed[15:26].tolist()
    depthEBB = ['Depth','m']+ h[4:15].tolist()
    depthFLOOD = ['Depth','m']+ h[15:26].tolist()
    kEBB =['u^3/h','(m^2)/(s^3)']+ k[4:15].tolist()
    kFLOOD =['u^3/h','(m^2)/(s^3)']+ k[15:26].tolist()
    QSMEBB = ['SedimentTransportModel','kg/ms']+ QSmodel[4:15].tolist()
    QSMFLOOD = ['SedimentTransportModel','kg/ms']+ QSmodel[15:26].tolist()
    QSEEBB = ['SedimentTransportExperiment','kg/ms'] +QSexpEBB.tolist()
    QSEFLOOD = ['SedimentTransportExperiment','kg/ms'] +QSexpFLOOD.tolist()
    
    columnsEBB = [velocityEBB,depthEBB,kEBB,QSMEBB,QSEEBB]
    columnsFLOOD = [velocityFLOOD,depthFLOOD,kFLOOD,QSMFLOOD,QSEFLOOD]
    putDataCSV('transport_rates_ebb.csv',columnsEBB)
    putDataCSV('transport_rates_flood.csv',columnsFLOOD)

    ############################################################################
    #####                      PLOTS                                      ######
    ############################################################################
    
    fig = plt.figure()

    plt.plot(k[4:15],QSmodel[4:15],'b',label='Model Ebb', linewidth=2.0)
    plt.plot(k[4:15],QSexpEBB, 'b:o', label='Measured Ebb', linewidth=2.0)
    plt.plot(k[15:26],QSmodel[15:26],'k',label='Model Flood', linewidth=2.0)
    plt.plot(k[15:26],QSexpFLOOD, 'k:o', label='Measured Flood', linewidth=2.0)
    plt.xlabel(r'$ \frac{u^3}{h} (\frac{m^2}{s^3}) $')
    plt.ylabel('Sediment Transport' r'$ ( \frac{kg}{ms}) $')
    
    plt.legend(loc='upper left')
    plt.title('Comparison of model and experiment concentrations' )
    filename = 'Foulness'+ '.png'
    plt.savefig(filename, dpi=100)
    print 'Wrote file', filename
    plt.clf()
    
    
    sys.exit() 
	
	
	
	
	