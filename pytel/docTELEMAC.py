#!/usr/bin/python
"""@author Yoann Audouin
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
"""@history 15/02/2013 -- Sebastien E. Bourban
         Adding the file in pytel
"""
"""@brief
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import system, getcwd, chdir, remove, walk, sep, environ, path
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.messages import MESSAGES,filterMessage

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#
__author__="Yoann Audouin"
__date__ ="$21-Sep-2012 16:51:09$"


def cleanDoc(docDir,fullclean):
   for dirpath,dirnames,files in walk(docDir): break
   for file in files :
      if file.endswith(".aux"): remove(file)
      if file.endswith(".out"): remove(file)
      if file.endswith(".toc"): remove(file)
      if file.endswith(".log"): remove(file)
      if file.endswith("~"): remove(file)
      if fullclean and file.endswith(".pdf"): remove(file)

def compiletex(texfile,version):
   mes = MESSAGES(size=10)
   print 'currentDir = '+getcwd()
   try:
     tail, code = mes.runCmd("pdflatex --jobname="+texfile+"_"+version+" "+texfile+".tex",False)
   except Exception as e:
      raise Exception([filterMessage({'name':'compiletex','msg':'something went wrong, I am not sure why.'},e,bypass)])
   if code != 0: raise Exception([{'name':'compiletex','msg':'could not compile your tex files (runcode='+str(code)+').\n      '+tail}])
   try:
     tail, code = mes.runCmd("pdflatex --jobname="+texfile+"_"+version+" "+texfile+".tex",False)
   except Exception as e:
      raise Exception([filterMessage({'name':'compiletex','msg':'something went wrong, I am not sure why.'},e,bypass)])
   if code != 0: raise Exception([{'name':'compiletex','msg':'could not compile your tex files (runcode='+str(code)+').\n      '+tail}])
   
if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   USETELCFG = ''
   if environ.has_key('USETELCFG'): USETELCFG = environ['USETELCFG']
   PWD = path.dirname(sys.argv[0])
   SYSTELCFG = path.join(path.dirname(PWD),'config')
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configname",
                     type="string",
                     dest="configName",
                     default=USETELCFG,
                     help="specify configuration name, default is the first found in the configuration file" )
   parser.add_option("-f", "--configfile",
                     type="string",
                     dest="configFile",
                     default=SYSTELCFG,
                     help="specify configuration file, default is systel.cfg" )
   parser.add_option("-r", "--rootdir",
                     type="string",
                     dest="rootDir",
                     default='',
                     help="specify the root, default is taken from config file" )
   parser.add_option("-v", "--version",
                     type="string",
                     dest="version",
                     default='',
                     help="specify the version number, default is taken from config file" )
   parser.add_option("-m", "--modules",
                     type="string",
                     dest="modules",
                     default='',
                     help="specify the list modules, default is taken from config file" )
   parser.add_option("--validation",
                     action="store_true",
                     dest="validation",
                     default=True,
                     help="Will generate only the validation documentation" )
   parser.add_option("--clean",
                     action="store_true",
                     dest="cleanup",
                     default=False,
                     help="Will remove all temporary file generated by pdflatex" )
   parser.add_option("--fullclean",
                     action="store_true",
                     dest="fullcleanup",
                     default=False,
                     help="Same as clean but removes the pdf as well" )

   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         for dirpath,dirnames,filenames in walk(dircfg) : break
         for file in filenames :
            head,tail = path.splitext(file)
            if tail == '.cfg' : print '    +> ',file
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Compile the valiation documentation
   currentDir = getcwd()
   for cfgname in cfgs.keys():
      # still in lower case
      if options.rootDir != '': 
         cfgs[cfgname]['root'] = path.abspath(options.rootDir)
         root = path.abspath(options.rootDir)
      else : root = cfgs[cfgname]['root']  
      if options.version != '': cfgs[cfgname]['version'] = options.version
      if options.modules != '': cfgs[cfgname]['modules'] = options.modules
      cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
      cfg.update({ 'PWD':PWD })
      # Look on all the modules for validation
      if options.validation :
         for codeName in cfg['VALIDATION'].keys():
           docDir = cfg['MODULES'][codeName]['path'] + sep + 'documentation' + sep + 'validation'
           chdir(docDir)
           if options.cleanup or options.fullcleanup:
             print '\nCleaning up folder '+docDir+'\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
             cleanDoc(docDir,options.fullcleanup)
           else:
             print '\nCompilation of the validation documentation for '+ codeName +'\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
             # Removing pdflatex temporary files
             cleanDoc(docDir,False)
             # Compiling the texfile
             compiletex(codeName+"_validation",cfgs[cfgname]['version'])

      
   print '\n\nMy work is done\n\n'

   sys.exit()
