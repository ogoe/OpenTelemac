#!/usr/bin/env python
"""@author Juliette Parisi
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
"""@history 17/01/2013 -- Juliette Parisi
         [putDataCsv] : writes date in a CSV file
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from os import path,remove
import sys
import numpy as np
from numpy import dtype, loadtxt

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
   
def addColumnCSV(file,column):
   data = loadtxt(file, dtype= 'S30', comments='#', skiprows=2, delimiter=',')
   data2 =  data.tolist()
   if len(data.shape) > 1 :
      for row in range(len(data2)):
         data2[row].append(column[row])
      csvF = open(file,'wb')
      csvF.write('#\n')
      csvF.write('#\n')
      for i in range(len(data2)):
         for j in range(len(data2[0])):
            if j == (len(data2[0])-1) : csvF.write(str(data2[i][j])+'\n')
            else : csvF.write(str(data2[i][j])+',')
      csvF.close() 
   else :
      data2 = [data2]
      data2.append(column)
      csvF = open(file,'wb')
      csvF.write('#\n')
      csvF.write('#\n')
      for j in range(len(data2[0])):
         for i in range(len(data2)):
            if i == (len(data2)-1) : csvF.write(str(data2[i][j])+'\n')
            else : csvF.write(str(data2[i][j])+',')
      csvF.close() 
   return
    
def getDataCSV(file,firstline):
   csvF = open(file,'r')
   for line in csvF:
      if line.startswith('#'): continue
      else :
         header = line.lower().replace(',',' ')
         variables = header.split()
         break
   csvF.close()
   
   fields = zip(variables, ['f8']*len(variables))
   fields_dtype = dtype(fields)
   
   data = loadtxt(file, dtype=fields_dtype, comments='#', skiprows=firstline, delimiter=',')
   return data

def getVariableCSV(file,var):
   csvF = open(file,'r')
   for line in csvF:
      if line.startswith('#'): continue
      else :
         header = line.lower().replace(',',' ')
         variables = header.split()
         break
   csvF.close()
   
   fields = zip(variables, ['f8']*len(variables))
   fields_dtype = dtype(fields)
   
   data = loadtxt(file, dtype=fields_dtype, comments='#', skiprows=4, delimiter=',')
   return data[var]

def get2VariablesCSV(file,var1,var2):
   csvF = open(file,'r')
   for line in csvF:
      if line.startswith('#'): continue
      else :
         header = line.lower().replace(',',' ')
         variables = header.split()
         break
   csvF.close()
   
   fields = zip(variables, ['f8']*len(variables))
   fields_dtype = dtype(fields)
   data = loadtxt(file, dtype=fields_dtype, comments='#', skiprows=4, delimiter=',')
   return (data[var1],data[var2])

def getValidationSummary(file):
   dt = dtype([('TestCase','S30'),('Module','S15'),('Status','S30')])
   data = loadtxt(file, dtype=dt, comments='#', skiprows=1, delimiter=',')
   return data

if __name__ == "__main__":
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Loading comand line options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()