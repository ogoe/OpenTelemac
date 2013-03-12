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
      if line.split()[0]!= '#' :
         lineparts = line.split(',')	
         t.append(lineparts[0])
         g1.append(lineparts[1])
         g2.append(lineparts[2])
         g3.append(lineparts[3])
         g4.append(lineparts[4])
      else : continue   

   tfile.close()	
   return t,g1,g2,g3,g4

if __name__ == "__main__":
    os.system('sisvali_foulness.exe')
    u,h,k,st_m,st_d = get_data('transport_rates.csv')
      
    #--------------------------
    #---  Plot Gauge 1    -----
    #--------------------------
    fig = plt.figure()

    plt.plot(k[6:18],st_m[6:18],'b',label='Model Ebb', linewidth=2.0)
    plt.plot(k[6:18],st_d[6:18], 'b:o', label='Measured Ebb', linewidth=2.0)
    plt.plot(k[18:28],st_m[18:28],'k',label='Model Flood', linewidth=2.0)
    plt.plot(k[18:28],st_d[18:28], 'k:o', label='Measured Flood', linewidth=2.0)
    plt.xlabel(r'$ \frac{u^3}{h} (\frac{m^2}{s^3}) $')
    plt.ylabel('Sediment Transport' r'$ ( \frac{kg}{ms}) $')

    plt.legend(loc='upper left')
    plt.title('Comparison of model and experiment concentrations' )
    filename = 'Foulness'+ '.png'
    plt.savefig(filename, dpi=100)
    print 'Wrote file', filename
    plt.clf()
