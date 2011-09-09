"""@brief
"""
"""@author David H. Roscoe and Sebastien E. Bourban
"""
"""@details

"""
"""@history 15/08/2011 -- Sebastien Bourban:
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#

#from os import path,chdir,system
import sys
import re
import matplotlib.pyplot as plt
from utils import getFileContent

# _____                             ________________________________
# ____/ Global Regular Expressions /_______________________________/
#
# _____                        _____________________________________
# ____/ Sortie Parser Toolbox /____________________________________/
#
"""
   Returns the time profile in iteration and in seconds, read from the TELEMAC sortie file
"""
sortie_time = re.compile(r'\s*ITERATION\s+(?P<iteration>\d+)\s+(TEMPS|TIME)[\s:]*'
            + r'(?P<others>.*?)'
            + r'(?P<number>\b((?:(\d+)\b)|(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+))))\s+S\s*(|\))'
            + r'\s*\Z',re.I)

def getTimeProfile(sortieLines):
   iter = []; time = []
   for line in sortieLines:
      proc = re.match(sortie_time,line)
      if proc:
         iter.append(int(proc.group('iteration')))
         time.append(float(proc.group('number')))
   return iter,time

"""
   Returns the name of the study, read from the TELEMAC sortie file
   +> If no name, returns NO NAME
   +> If not found, returns NAME NO FOUND
"""
sortie_nameofstudy = re.compile(r'\s*(?P<before>.*?)(NAME OF THE STUDY|TITRE DE L\'ETUDE)[\s:]*(?P<after>.*?)\s*\Z',re.I)
#sortie_title = re.compile(r'\s*(?P<before>.*?)(TITLE|TITRE)[\s=]*(?P<after>.*?)\s*\Z',re.I)

def getNameOfStudy(sortieLines):
   for line in range(len(sortieLines)):
      proc = re.match(sortie_nameofstudy,sortieLines[line])
      if proc:
         if sortieLines[line+1].strip() == '': return 'NO NAME'
         return sortieLines[line+1].strip()
      #proc = re.match(sortie_title,sortieLines[line])
      #if proc:
      #   if proc.group('after').strip() == '': return 'NO NAME'
      #   return proc.group('after').strip()
   return 'NAME NOT FOUND'
"""
   Returns the time series of volumes, read from the TELEMAC sortie file
"""
sortie_volume = re.compile(r'\s*(VOLUME IN THE DOMAIN|VOLUME DANS LE DOMAINE)[\s:]*'
            + r'\s+(?P<number>\b([-+]|)((?:(\d+)\b)|(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+))))\s+'
            + r'(?P<after>.*?)\s*\Z',re.I)
sortie_flux = re.compile(r'\s*(FLUX BOUNDARY|FLUX FRONTIERE)\s+\d+:\s*'
            + r'(?P<number>[+-]*\b((?:(\d+)\b)|(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+))))(.\s|\s)+'
            + r'(?P<after>.*?)\s*\Z',re.I)
sortie_errorvolume = re.compile(r'\s*(RELATIVE ERROR IN VOLUME|ERREUR RELATIVE EN VOLUME)'
            + r'(?P<others>.*?)'
            + r'(?P<number>[+-]*\b((?:(\d+)\b)|(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+))))'
            + r'\s*\Z',re.I)

def getVolumeProfile(sortieLines):
   volumes = []; fluxes = []; errors = []
   iLine = 0
   while iLine < len(sortieLines):
      proc = re.match(sortie_volume,sortieLines[iLine])
      if proc:
         volumes.append(float(proc.group('number')))
         flux = []
         while 1:
            iLine = iLine + 1
            proc = re.match(sortie_flux,sortieLines[iLine])
            if proc:
               flux.append(float(proc.group('number')))
            else: break
         fluxes.append(flux)
         proc = re.match(sortie_errorvolume,sortieLines[iLine])
         if proc:
            errors.append(float(proc.group('number')))
      iLine = iLine + 1

   return volumes,fluxes,errors

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="David H. Roscoe; Sebastien E. Bourban"
__date__ ="$2-Aug-2011 11:51:36$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #sortieFile = "C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\t2d_bumpflu_v1p0.cas_2011-08-29-08h29min55s.sortie"
   sortieFile = "C:\\opentelemac\\validation\\telemac2d\\tel2d_v6p2\\011_bumpflu\\t2d_calib2010-11_extended_v6p0r1_run1.sortie.log"
   sortie = { 'fileName': sortieFile, 'typePlot': "mass", 'outFormat': '.png' }
   figout = plots.plotTimeSeriesSortie(sortie)
   print '     +> Capture produced: ' + figout

   print 'my job is done'