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

def putDataCSV(file,columns):
   if path.exists(file): remove(file)
   csvF = open(file,'wb')
   for j in range(len(columns[0])):
      for i in range(len(columns)):
         csvF.write(str(columns[i][j])+',')
      csvF.write('\n') 
   csvF.close()

def getColumnCSV(file,(xNBR,yNBR)):
   columnX = []
   columnY = []
   ilines = []
   csvF = open(file,'r')
   for line in csvF:
      ilines.append(line.split(','))
   csvF.close()   
   for rows in range(4,len(ilines)):
      columnX.append(ilines[rows][xNBR].rstrip('\n'))
      columnY.append(ilines[rows][yNBR].rstrip('\n'))
   return (columnX,columnY)
       
def getDataCSV(file,firstline,(xNBR,yNBR)):
   column1 = []
   column2 = []
   ilines = []
   csvF = open(file,'r')
   for line in csvF:
      ilines.append(line.split(','))
   csvF.close()   
   for rows in range(firstline,len(ilines)):
      column1.append(ilines[rows][xNBR].rstrip('\n'))
      column2.append(ilines[rows][yNBR].rstrip('\n'))
   return (column1,column2)



if __name__ == "__main__":
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Loading comand line options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit()