#! /usr/bin/python

# To change this template, choose Tools | Templates
# and open the template in the editor.

__author__="jcp"
__date__ ="$31-Oct-2012 17:12:01$"

import matplotlib.pyplot as plt                             # used for matplotlib
import os
import sys

def get_data(filename):
    t = []
    g1 = []
    g2 = []
    g3 = []    
    g4 = [] 
    tfile = open(filename, 'r')
    
    for line in tfile:
        lineparts = line.split(',')
        t.append(lineparts[0])
        g1.append(lineparts[1])
        g2.append(lineparts[2])
        g3.append(lineparts[3])
        g4.append(lineparts[4])
        
    tfile.close()
    return t,g1,g2,g3,g4

if __name__ == "__main__":

    t,m1,m2,d1,d2 = get_data('Profiles.csv')
      
    #--------------------------
    #---  Plot Gauge 1    -----
    #--------------------------
    fig = plt.figure()

    plt.plot(t[2:],d1[2:],'k',label='Initial bed')
    plt.plot(t[2:],d2[2:], 'b', label='Measured')
    plt.plot(t[2:],m2[2:], 'r', label='Modelled')
    plt.xlabel('location(m)')
    plt.ylabel('Bed level (m)')

    plt.legend(loc='lower right')
    plt.title('Comparison of model and experiment morphology' )
    filename = 'Sandpit_2_profiles'+ '.png'
    plt.savefig(filename, dpi=100)
    print 'Wrote file', filename
    plt.clf()
