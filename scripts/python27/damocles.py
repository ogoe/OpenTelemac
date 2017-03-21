#!/usr/bin/python
"""@author Yoann Audouin

   @note ... this work is based on a collaborative effort between
  .________.                                                          ,--.
  |        |                                                      .  (  (
  |,-.    /   HR Wallingford                EDF - LNHE           / \_ \_/ .--.
  /   \  /    Howbery Park,                 6, quai Watier       \   )   /_   )
   ,.  `'     Wallingford, Oxfordshire      78401 Cedex           `-'_  __ `--
  /  \   /    OX10 8BA, United Kingdom      Chatou, France        __/ \ \ `.
 /    `-'|    www.hrwallingford.com         innovation.edf.com   |    )  )  )
!________!                                                        `--'   `--

   @history 15/02/2013 -- Y. Audouin
         Adding the file in pytel

   @brief Scripts to Manipulate the dictionary using damocles
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import chdir, remove, walk, environ, path, linesep
# ~~> dependencies towards the root of pytel
from config import OptionParser, parseConfigFile, \
                   parseConfig_ValidateTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.messages import MESSAGES
from utils.files import getFileContent


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#
__author__ = "Yoann Audouin"
__date__ = "$21-Sep-2012 16:51:09$"

def runDamocles(exePath,paramFile,logFile=''):
   """
      Running the damocles executable
      param exePath Path the damocles executable
      param paramFile Path to the input aprameters file
      param logFile Redirecting ouput to that file if present
   """
   if not path.exists(exePath):
      print "You need to compile damocles to use it..."
      sys.exit(1)
   # Run Fortran program
   mes = MESSAGES(size=10)
   # TODO: Error handling when damocles crashes
   try:
      if logFile == '':
         print "%s < %s " % (exePath,paramFile)
         tail, code = mes.runCmd("%s < %s" % (exePath,paramFile), False)
      else:
         print "%s < %s > %s" % (exePath,paramFile,logFile)
         tail, code = mes.runCmd("%s < %s > %s" % (exePath,paramFile,logFile), False)
   except OSError as exc:
      print exc.message
      sys.exit(1)
   if code !=0:
      raise Exception([
            {'name':'damocles',
             'msg':'Could not execute damocles'\
                   +'\n\nHere is the log:\n'
                   +'\n'.join(getFileContent(logFile))
            }])

def genDump(exePath,inputDict,outputDict):
   """
      Run damocles to generate a reordered dictionary
      param exePath Path to the damocles executable
      param inputDict Input Telemac dictionary
      param ouputDict Resorted dictionary
   """
   paramFile = path.join(path.dirname(inputDict),'damo.par')
   with open(paramFile,'w') as f:
      f.write('DUMP'+'\n')
      f.write(inputDict+'\n')
      f.write(outputDict)
   runDamocles(exePath, paramFile)
   remove(paramFile)

def genCata(codeName,exePath,inputDict,inputDep,cataName,enumName,tsPath):
   """
      Run damocles to generate an eficas catalogue
      param exePath Path to the damocles executable
      param inputDict Input Telemac dictionary
      param inputDep Input Telemac depnedancies file
      param cataName Name of the eficas Catalogue
      param enumName Name of the enum for CHOIX
      param tsPath Path for where the ts file will be generated
   """
   paramFile = path.join(path.dirname(inputDict),'damo.par')
   with open(paramFile,'w') as f:
      f.write('CATA'+'\n')
      f.write(codeName+'\n')
      f.write(inputDict+'\n')
      f.write(inputDep+'\n')
      f.write(cataName+'\n')
      f.write(enumName+'\n')
      f.write(tsPath)
   runDamocles(exePath, paramFile)
   remove(paramFile)

def genLatex(exePath,inputDict,latexName,lng):
   """
      Run damocles to generate an LaTeX file for the reference manual
      param exePath Path to the damocles executable
      param inputDict Input Telemac dictionary
      param latexName Name of the LaTeX file
      param lng Language of the documentation
   """
   paramFile = path.join(path.dirname(inputDict),'damo.par')
   with open(paramFile,'w') as f:
      f.write('LATEX'+'\n')
      f.write(inputDict+'\n')
      f.write(latexName+'\n')
      f.write(lng)
   runDamocles(exePath, paramFile)
   remove(paramFile)

def main():
   """
      Main program for the execution of damocles
   """
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+'~'*72+'\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.\n"\
                    "By Default all the documentation are generated\n"\
                    "use the options --validation/reference/user/"\
                    "release to compile only one")
   parser.add_option("-c", "--configname",
                 type="string",
                 dest="configName",
                 default='',
                 help="specify configuration name, default is the "\
                     "first found in the configuration file" )
   parser.add_option("-f", "--configfile",
                 type="string",
                 dest="configFile",
                 default='',
                 help="specify configuration file, "\
                     "default is systel.cfg" )
   parser.add_option("-r", "--root_dir",
                 type="string",
                 dest="root_dir",
                 default='',
                 help="specify the root, default is "\
                     "taken from config file" )
   parser.add_option("-m", "--modules",
                 type="string",
                 dest="modules",
                 default='',
                 help="specify the list modules, default is "\
                     "taken from config file" )
   parser.add_option("--dump",
                 action="store_true",
                 dest="dump",
                 default=False,
                 help="Will dump a reformated dictionary" )
   parser.add_option("--eficas",
                 action="store_true",
                 dest="eficas",
                 default=False,
                 help="Will generate the eficas Catalogue from the dictionary" )
   parser.add_option("--latex",
                 action="store_true",
                 dest="latex",
                 default=False,
                 help="Will generate the LaTeX file for the reference manual" )

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   options, _ = parser.parse_args()
   # path to the root
   PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   if options.root_dir != '': PWD = options.root_dir
   # user configuration name
   USETELCFG = ''
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   if options.configName == '': options.configName = USETELCFG
   # user configuration file
   SYSTELCFG = path.join(PWD,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if options.configFile != '': SYSTELCFG = options.configFile
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   options.configFile = SYSTELCFG

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: %s\n' % \
           options.configFile
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         _, _, filenames = walk(dircfg).next()
         for fle in filenames:
            _, tail = path.splitext(fle)
            if tail == '.cfg' :
               print '    +> ', fle
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile, options.configName)
   cfgname = cfgs.iterkeys().next()
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # Defining which modules to use
   if options.modules is '':
      moduleList = ['artemis','postel3d','stbtel','sisyphe',
                   'telemac2d','telemac3d','tomawac','waqtel']
   else:
      moduleList = options.modules.split(';')
   # Identify Root value
   if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
   if options.root_dir != '':
      cfgs[cfgname]['root'] = path.abspath(options.root_dir)
      root = path.abspath(options.root_dir)
   else :
      root = cfgs[cfgname]['root']
   cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
   exePath = path.join(root,'builds',cfgname,\
             'bin','damocles'+\
             cfg['SYSTEM']['sfx_exe'])
   # Looping on all modules
   for module in moduleList:
      modulePath = path.join(root,
                             'sources',
                             module)
      if(options.dump):
         inputDict = path.join(modulePath, module+".dico")
         outputDict = path.join(modulePath, module+"2.dico")
         genDump(exePath, inputDict, outputDict)

      if(options.eficas):
         inputDict = path.join(modulePath,
                               module+".dico")
         inputDep = path.join(modulePath,
                               module+".dico.dep")
         fancyModule = module[0].upper()+module[1:]
         cataName = path.join(modulePath,
                              'eficas',
                              fancyModule+"_cata_auto.py")
         enumName = path.join(modulePath,
                              'eficas',
                              'enum_'+fancyModule+"_auto.py")
         tsPath = path.join(modulePath,
                              'eficas')
         genCata(module.upper(), exePath, inputDict, inputDep, cataName, \
                 enumName, tsPath+path.sep)

      if(options.latex):
         inputDict = path.join(modulePath, module+".dico")
         latexName = path.join(root,'documentation',module,'reference',\
                               'latex','Corpus.tex')
         # English only
         lng = '2'
         genLatex(exePath, inputDict, latexName, lng)
# ~~~~ Compile the valiation documentation

   print '\n\n'+'~'*72

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)

if __name__ == "__main__":
   main()
