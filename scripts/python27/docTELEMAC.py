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
   
   @brief
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


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#
__author__ = "Yoann Audouin"
__date__ = "$21-Sep-2012 16:51:09$"

def clean_doc(doc_dir, fullclean):
   """ Remove latex temporary files """
   _, _, files = walk(doc_dir).next()
   for fle in files:
      if fle.endswith((".aux", ".out", ".toc", ".log", ".nlo", "~")): 
         remove(fle)
      if fullclean and fle.endswith(".pdf"): 
         remove(fle)

def compiletex(texfile, version):
   """ Compile a tex file """
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

   try:
      tail, code = mes.runCmd("pdflatex --jobname=%s_%s %s.tex" % \
                       (texfile, version, texfile), False)
   except OSError as exc:
      print exc.message
      sys.exit(1)

   if code != 0:
      print 'Latex compilation failed'
      print tail

# 
def create_case_list_file(doc_dir, cfg_val):
   """ Creates the CASELIST.tex which includes i
      all the test cases tex file 
   """
   case_list_file = doc_dir + sep + 'latex' + sep + 'CASELIST.tex'
   # Remove the file if it is already there
   if path.exists(case_list_file): 
      remove(case_list_file)
   with open(case_list_file, 'w') as fobj:
      val_dir = cfg_val['path']
      # Loop on all test cases
      for case in cfg_val:
         # Skip the 'path' key
         if case != 'path':
            txt = linesep + '\subincludefrom{' + val_dir + sep +\
               case + sep + 'doc' +\
               sep + '}{' + case + '}' + \
               linesep + '\clearpage' + linesep
            fobj.write(txt)
   
def compile_doc(doc_dir, doc_type, code_name, version, cleanup, fullcleanup):
   """ Compile the telemac-mascaret documentation """
   chdir(doc_dir)
   if cleanup or fullcleanup:
      clean_doc(doc_dir, fullcleanup)
      print '   - Cleaned up folder '+doc_dir+'\n'
   else:
      # Check if the file exist
      if path.exists(doc_dir + sep + code_name + "_" + doc_type + ".tex"):
         # removing pdflatex temporary files
         clean_doc(doc_dir, False)
         # compiling the texfile
         compiletex(code_name + "_" + doc_type, version)
      else:
         print "   - Skipping %s, %s_%s.tex not found " % \
              (code_name, code_name, doc_type)

def main():
   """ Main code """
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+'~'*72+'\n'
   usetelcfg = ''
   if 'USETELCFG' in environ: 
      usetelcfg = environ['USETELCFG']
   pwd = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   systelcfg = path.join(pwd, 'configs')
   if 'SYSTELCFG' in environ: 
      systelcfg = environ['SYSTELCFG']
   if path.isdir(systelcfg): 
      systelcfg = path.join(systelcfg, 'systel.cfg')

   parser = OptionParser("usage: %prog [options] \nuse -h for more help.\n"\
                    "By Default all the documentation are generated\n"\
                    "use the options --validation/reference/user/"\
                    "release to compile only one")
   parser.add_option("-c", "--configname",
                 type="string",
                 dest="configName",
                 default=usetelcfg,
                 help="specify configuration name, default is the "\
                     "first found in the configuration file" )
   parser.add_option("-f", "--configfile",
                 type="string",
                 dest="configFile",
                 default=systelcfg,
                 help="specify configuration file, "\
                     "default is systel.cfg" )
   parser.add_option("-r", "--root_dir",
                 type="string",
                 dest="root_dir",
                 default='',
                 help="specify the root, default is "\
                     "taken from config file" )
   parser.add_option("-v", "--version",
                 type="string",
                 dest="version",
                 default='',
                 help="specify the version number, mandatory for "\
                     "documentation purposes" )
   parser.add_option("-m", "--modules",
                 type="string",
                 dest="modules",
                 default='',
                 help="specify the list modules, default is "\
                     "taken from config file" )
   parser.add_option("--validation",
                 action="store_true",
                 dest="validation",
                 default=False,
                 help="Will generate the validation documentation" )
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
   
   options, _ = parser.parse_args()
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
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Compile the valiation documentation
   doall = not (options.validation or options.user or options.reference or options.release )
   for cfgname in cfgs:
      # still in lower case
      if options.root_dir != '': 
         cfgs[cfgname]['root'] = path.abspath(options.root_dir)
         root = path.abspath(options.root_dir)
      else : 
         root = cfgs[cfgname]['root']  
      if options.version == '': 
         print '\nYou need a reference version for this documentation'
         sys.exit(1)
      cfgs[cfgname]['version'] = options.version
      if options.modules != '': 
         cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
      cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])
      # Loop on all the modules
      
      # Initialise output message
      output_mess = '\n\n'
      # Look on all the modules for the documentation
      for code_name in cfg['VALIDATION']:
         print '\nCompilation of the documentation for '+ code_name + \
               '\n'+'~'*72
         todo = []
         if (options.validation or doall):
            todo.append('validation')
         if (options.reference or doall):
            todo.append('reference')
         if (options.user or doall):
            todo.append('user')
         if (options.release_note or doall):
            todo.append('release_note')
         for doc_type in todo:
            doc_dir = root + sep + 'documentation' + sep +\
                     code_name + sep + doc_type
            chdir(doc_dir)
            create_case_list_file(doc_dir, cfg['VALIDATION'][code_name])
            compile_doc(doc_dir, doc_type, code_name, 
                        cfgs[cfgname]['version'],
                        options.cleanup, options.fullcleanup)
            if not (options.cleanup or options.fullcleanup): 
               output_mess += '   - Created %s_%s_%s.pdf\n' % \
                          (code_name, doc_type, cfgs[cfgname]['version'])
      
   print output_mess
   print '\n\n'+'~'*72

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'
   
   sys.exit(0)

if __name__ == "__main__":
   main()
