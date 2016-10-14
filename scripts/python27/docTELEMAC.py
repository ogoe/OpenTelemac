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

   @history 15/02/2013 -- Sebastien E. Bourban
         Adding the file in pytel

   @brief Scripts to compile the telemac-mascaret documentation
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import chdir, remove, walk, sep, environ, path, linesep
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

def clean_doc(doc_dir, fullclean):
   """
       brief Remove latex temporary files

       param doc_dir Directory containing the main tex file
       param fullclean If Yes will remove the pdf file as well
   """
   _, _, files = walk(doc_dir).next()
   for fle in files:
      if fle.endswith((".bbl",".blg",".aux", ".out", ".toc", ".log",
                       ".nlo", "~", "idx", "ptc")):
         remove(fle)
      if fullclean and fle.endswith(".pdf"):
         remove(fle)

def compiletex(texfile, version):
   """
      brief Full procedure for compiling a LaTeX file
            .i.e pdflatex,bibtex,pdflatex,pdflatex
      param texfile Name of the main LaTex file
      param version Version of the code/documentation
   """

   # First compilation
   mes = MESSAGES(size=10)
   try:
      tail, code = mes.runCmd("pdflatex --jobname=%s_%s %s.tex" % \
                       (texfile, version, texfile), False)
   except OSError as exc:
      print exc.message
      sys.exit(1)

   if code != 0:
      print 'Latex compilation failed'
      print tail
      sys.exit(1)

   # Bibtex compilation
   try:
      tail, code = mes.runCmd("bibtex %s.aux" % \
                       (texfile+'_'+version), False)
   except OSError as exc:
      print exc.message
      sys.exit(1)

   if code != 0:
      print 'Bibtex compilation failed'
      print tail
      sys.exit(1)

   # Second compilation
   try:
      tail, code = mes.runCmd("pdflatex --jobname=%s_%s %s.tex" % \
                       (texfile, version, texfile), False)
   except OSError as exc:
      print exc.message
      sys.exit(1)

   if code != 0:
      print 'Latex compilation failed'
      print tail
      sys.exit(1)

   # Third compilation
   try:
      tail, code = mes.runCmd("pdflatex --jobname=%s_%s %s.tex" % \
                       (texfile, version, texfile), False)
   except OSError as exc:
      print exc.message
      sys.exit(1)

   if code != 0:
      print 'Latex compilation failed'
      print tail
      sys.exit(1)


#
def create_case_list_file(doc_dir, val_dir, cfg_val, cleanup):
   """
      brief Creates the CASELIST.tex which includes
      all the test cases tex file

      param doc_dir Path to directry containing the main LaTeX file
      param cfg_val list of path for the examples
      param cleanup If yes clean up the temporay files instead
                    of creating the CASELIST.Tex file
      return the list of cases that where missing the .tex file
   """
   case_list_file = doc_dir + sep + 'latex' + sep + 'CASELIST.tex'
   skipedCases = []
   if cleanup:
      if path.exists(case_list_file):
         remove(case_list_file)
   else:
      # Remove the file if it is already there
      if path.exists(case_list_file):
         remove(case_list_file)
      with open(case_list_file, 'w') as fobj:
         # Loop on all test cases
         for case in sorted(cfg_val):
            if not path.exists(path.join(val_dir,case,'doc',case+".tex")):
               skipedCases.append(case)
            else:
               txt = linesep + '\subincludefrom{' + val_dir + sep +\
                  case + sep + 'doc' +\
                  sep + '}{' + case + '}' + \
                  linesep + '\clearpage' + linesep
               fobj.write(txt)
   return skipedCases

def generate_ref_from_dict(exePath,dictionary,latexFile,lng,cleanup):
   """
      brief Generate the Latex file for the
            reference manual from the dictionary

      param exePath Path to homere_damocles executable
      param dictionary Path to the dictionary to read
      param latexFile Name of the outpu latex file that will
                      contain the reference manual
      param lng Language for the reference manual
                1: French
                2: English
   """
   #Building input parameter file
   paramFile = path.dirname(latexFile)+sep+'gen_ref.par'
   logFile = path.dirname(latexFile)+sep+'gen_ref.log'
   # Cleanup
   if(cleanup):
     if path.exists(paramFile):
        remove(paramFile)
     if path.exists(logFile):
        remove(logFile)
     if path.exists(latexFile):
        remove(latexFile)
   else:
      # Creating parameter file for damocles
      with open(paramFile,'w') as f:
         f.write(dictionary+'\n')
         f.write(latexFile+'\n')
         f.write(lng+'\n')
      # Removing LaTeX file if already there
      if path.exists(latexFile):
         remove(latexFile)
      # Run Fortran program
      mes = MESSAGES(size=10)
      # TODO: Error handling when damocles crashes
      try:
         print "%s < %s > %s" % (exePath,paramFile,logFile)
         tail, code = mes.runCmd("%s < %s > %s" % (exePath,paramFile,logFile), False)
      except OSError as exc:
         print exc.message
         sys.exit(1)
      if code !=0:
         raise Exception([
               {'name':'runPARTEL',
                'msg':'Could not generated data from dictionary '\
                      +'\n\nHere is the log:\n'
                      +'\n'.join(getFileContent(logFile))
               }])

def compile_doc(doc_dir, doc_name, version, cleanup, fullcleanup):
   """
      brief Compile the telemac-mascaret documentation

      param doc_dir Directory containing the main LaTeX file
      param doc_name Name of the main LaTeX file
      param version Version of the code/documentation
      param cleanup If yes remove temporary files
      param fullcleanup If yes does cleanup + remove pdf
   """
   chdir(doc_dir)
   if cleanup or fullcleanup:
      clean_doc(doc_dir, fullcleanup)
      print '   - Cleaned up folder '+doc_dir+'\n'
   else:
      # removing pdflatex temporary files
      clean_doc(doc_dir, False)
      # compiling the texfile
      compiletex(doc_name, version)

def main():
   """
      Main program for the compilation of the documentation of
      the telemac-mascaret system
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
   parser.add_option("-M", "--misc",
                 type="string",
                 dest="misc",
                 default='',
                 help="specify the list of misc documentation to compile, "\
                      "default is all of them" )
   parser.add_option("--validation",
                 action="store_true",
                 dest="validation",
                 default=False,
                 help="Will generate the validation documentation" )
   parser.add_option("--case-list",
                 type="string",
                 dest="caseList",
                 default='',
                 help="List of cas to include in the validation documentation"\
                      "separated by ',' (default all of them)" )
   parser.add_option("--reference",
                 action="store_true",
                 dest="reference",
                 default=False,
                 help="Will generate the reference documentation" )
   parser.add_option("--user",
                 action="store_true",
                 dest="user",
                 default=False,
                 help="Will generate the user documentation" )
   parser.add_option("--release",
                 action="store_true",
                 dest="release_note",
                 default=False,
                 help="Will generate the release note" )
   parser.add_option("--clean",
                 action="store_true",
                 dest="cleanup",
                 default=False,
                 help="Will remove all temporary file "\
                     "generated by pdflatex" )
   parser.add_option("--fullclean",
                 action="store_true",
                 dest="fullcleanup",
                 default=False,
                 help="Same as clean but removes the pdf as well" )

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
# ~~~~ Compile the valiation documentation
   doall = not (options.validation or options.user or options.reference or options.release_note )
   for cfgname in cfgs:
      # still in lower case
      if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
      if options.root_dir != '':
         cfgs[cfgname]['root'] = path.abspath(options.root_dir)
         root = path.abspath(options.root_dir)
      else :
         root = cfgs[cfgname]['root']
      # Get what i to be compiled
      # By default everything if something is defined compiling only that
      if options.modules != '':
         moduleList = options.modules.split(',')
      else:
         # all modules
         moduleList = ['artemis','postel3d','stbtel','sisyphe',
                       'telemac2d','telemac3d','tomawac','waqtel']
      if options.misc != '':
         miscList = options.misc.split(',')
         moduleList = []
      else:
         # all docs
         miscList = ['developer_guide','software_quality_plan',
	             'TelemacDocTemplate']
	 # If a module was specified  or a specific documentation for modules
	 # not compiling Misc documentation
         if options.modules != '' or not doall:
            miscList = []

      cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
      # Loop on all the modules

      # Get version in config if it exist use trunk otherwise
      version = cfgs[cfgname].get('version','trunk')

      # Initialise output message
      output_mess = '\n\n'
      # Look on all the modules for the documentation
      for code_name in moduleList:
         print '\nCompilation of the documentation for '+ code_name + \
               '\n'+'~'*72
	 # list of what to do for the module
         todo = []
         if (options.validation or doall):
            # Building Validation LaTeX file
            doc_dir = root + sep + 'documentation' + sep +\
                     code_name + sep + 'validation'
            chdir(doc_dir)
            if options.caseList != '':
                listOfCase = options.caseList.split(',')
            else:
                listOfCase = cfg['VALIDATION'][code_name].keys()
                listOfCase.remove('path')
            skipedCase = create_case_list_file(doc_dir, \
                                  cfg['VALIDATION'][code_name]['path'],\
                                  listOfCase,\
                                  options.cleanup or options.fullcleanup)
            for case in skipedCase:
               output_mess += '   - /!\ Missing LaTeX file for '+case+'\n'
            todo.append('validation')
         if (options.reference or doall):
            # Path to the dictionary
            dictionary = root + sep + 'sources' + sep + code_name +\
                         sep + code_name + '.dico'
            # Path to latex File
            latexFile = root + sep + 'documentation' + sep +\
                      code_name + sep + 'reference' + sep +\
                      'latex' + sep + 'Corpus.tex'
            # English only for now
            lng = '2'
            # Path to bin directory
            exePath = root + sep + 'builds' + sep + cfgname + sep +\
                      'bin' + sep + 'damocles' +\
                      cfg['SYSTEM']['sfx_exe']
            generate_ref_from_dict(exePath,dictionary,latexFile,lng,\
                                   options.cleanup or options.fullcleanup)
            todo.append('reference')
         if (options.user or doall):
            # Normal Compilation of a LaTeX file
            todo.append('user')
         if (options.release_note or doall):
            todo.append('release_note')
         for doc_type in todo:
            doc_dir = root + sep + 'documentation' + sep +\
                     code_name + sep + doc_type
            chdir(doc_dir)
            # Check if the file exist
            if path.exists(doc_dir + sep + code_name + "_" + doc_type + ".tex"):
               compile_doc(doc_dir, code_name+'_'+doc_type,
                           version,
                           options.cleanup, options.fullcleanup)
            else:
               print "   - Error for %s %s, %s.tex not found " % \
                    (path.basename(doc_dir), code_name+"_"+doc_type)
               sys.exit(1)
            if not (options.cleanup or options.fullcleanup):
               output_mess += '   - Created %s_%s_%s.pdf\n' % \
                          (code_name, doc_type, version)
      # List of the other documentation
      for doc in miscList:
         print '\nCompilation of the documentation for '+ doc + \
               '\n'+'~'*72
         doc_dir = root + sep + 'documentation' + sep +\
                  'Misc' + sep + doc
         chdir(doc_dir)
         if path.exists(doc_dir + sep + doc + ".tex"):
            compile_doc(doc_dir, doc,
                        version,
                        options.cleanup, options.fullcleanup)
         else:
            print "   - Error in %s, %s.tex not found " % \
                 (path.basename(doc_dir), doc)
            sys.exit(1)
         if not (options.cleanup or options.fullcleanup):
            output_mess += '   - Created %s_%s.pdf\n' % \
                       (doc, version)

   print output_mess
   print '\n\n'+'~'*72

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)

if __name__ == "__main__":
   main()
