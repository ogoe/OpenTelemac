"""@brief
"""
"""@author Sebastien E. Bourban, Noemie Durand and Alain Weisgerber
"""
"""@history 28/04/2011 -- Sebastien Bourban: Now supports SYSTELCFG
         as a directory (old Perl version, to which systel.cfg is added)
         or as a file.
"""
"""@history 30/04/2011 -- Sebastien Bourban: Upgrade made to config parsing
         to include the option to reset the version and the root from the
         command line option:
         -v <version>, reset the version read in the config file with this
         -r <root>, reset the root path read in the config file with this
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from os import path,walk,mkdir,getcwd,chdir,remove,rmdir,listdir
from distutils.archive_util import make_zipfile, make_archive
from distutils.dep_util import newer
import sys
import shutil

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def addToList(list,name,value):
   if list.get(name) != None:
      if value not in list[name]:
         list[name].append(value)
   else:
      list.update({name:[value]})
   return list

"""
   Make a list of all files in root that have the extension in [ext]
   -- Return the list
"""
def getTheseFiles(root,exts):
   root = root.strip()
   files = []
   if path.exists(root) :
      for dirpath,dirnames,filenames in walk(root) : break
      for file in filenames :
         for ext in exts :
            head,tail = path.splitext(file)
            if tail.lower() == ext.lower() :
               files.append(path.join(dirpath,file))
   return files

"""

"""
def getFileContent(file):
   ilines = []
   SrcF = open(file,'r')
   for line in SrcF:
      ilines.append(line)
   SrcF.close()
   return ilines

"""

"""
def putFileContent(file,lines):
   SrcF = open(file,'wb')
   SrcF.write(('\n'.join(lines)).replace('\r','').replace('\n\n','\n'))
   SrcF.close()
   return

"""

"""
def createDirectories(po):
   pr = po; pd = []
   while not path.isdir(pr):
      pd.append(path.basename(pr))
      pr = path.dirname(pr)
   while pd != []:
      pr = path.join(pr,pd.pop())
      mkdir(pr)
   return

"""
"""
def copyFiles(pi,po):
   for f in listdir(pi):
      if path.isfile(path.join(pi,f)): shutil.copy(path.join(pi,f),po)
   return
"""
"""
def copyFile(fi,po):
   if path.isfile(fi): shutil.copy(fi,po)
   return

"""
   Walk through the directory structure available from the root
   and removes everything in it, including the root
"""
def removeDirectories(root):
   for p, pdirs, pfiles in walk(root, topdown=False):
       for f in pfiles: remove(path.join(p,f))
       for d in pdirs: rmdir(path.join(p,d))
   rmdir(root)
   
"""
    bname is a the root directory to be archived --
    Return the name of the archive, zname, with its full path --
    form can be either 'zip', 'gztar' ... read from the config file
"""
def zip(zname,bname,form):
   cpath = getcwd()
   chdir(path.dirname(bname))
   zipfile = make_archive(zname,form,base_dir=path.basename(bname))
   chdir(cpath)
   return zipfile

"""
   Evaluate whether one file is more recent than the other
   Return 1 is ofile exists and is more recent than nfile, 0 otherwise
"""
def isNewer(nfile,ofile):
   if newer(nfile,ofile): return 0
   return 1

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   sys.exit()
