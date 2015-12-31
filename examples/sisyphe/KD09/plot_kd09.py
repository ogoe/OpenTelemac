#! /usr/bin/python

# To change this template, choose Tools | Templates
# and open the template in the editor.

__author__="DMK"
__date__ ="$31-Oct-2012 17:12:01$"

import matplotlib.pyplot as plt                             # used for matplotlib
import os
import sys

def get_data(filename):
    x  = []
    g1 = []
    g2 = []
    
    tfile = open(filename, 'r')
    
    for line in tfile:
       if line.split()[0]!= '#' :
          lineparts = line.split(',')	
          x.append(lineparts[0])
          g1.append(lineparts[1])
          g2.append(lineparts[2])
       else : continue  
        
    tfile.close()
    return x,g1,g2

if __name__ == "__main__":
    
    ## Fig 1
    x,a1,n1 = get_data('kd09bed.csv')
      
    fig = plt.figure()

    plt.plot(x[2:],n1[2:],'r',label='Analytical KD09')
    plt.plot(x[2:],a1[2:], 'b', label='Telemac-Sisyphe')
    plt.xlabel('Distance(m)')
    plt.ylabel('Bed level (m)')

    plt.legend(loc='lower right')
    plt.title('Comparison of Telemac-Sisyphe and KD09 analytical solution at t=1s' )
    filename = 'KD09_Bed_profile_1s'+ '.png'
    plt.savefig(filename, dpi=100)
    print 'Wrote file', filename
    plt.clf()
    
    
    ## Fig 2
    x,a1,n1 = get_data('kd09dep.csv')
      
    fig = plt.figure()

    plt.plot(x[2:],n1[2:],'r',label='Analytical KD09')
    plt.plot(x[2:],a1[2:], 'b', label='Telemac-Sisyphe')
    plt.xlabel('Distance(m)')
    plt.ylabel('Water Depth (m)')

    plt.legend(loc='lower left')
    plt.title('Comparison of Telemac-Sisyphe and KD09 analytical solution at t=1s' )
    filename = 'KD09_Depth_profile_1s'+ '.png'
    plt.savefig(filename, dpi=100)
    print 'Wrote file', filename
    plt.clf()
