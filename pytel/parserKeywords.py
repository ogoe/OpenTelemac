"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
"""@details

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

from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
from utils import getFileContent
import re
from os import path,walk
import sys

debug = False

# _____               ______________________________________________
# ____/ Instructions /_____________________________________________/
#

# _____                 ____________________________________________
# ____/ Keyword parse  /___________________________________________/
#

key_comment = re.compile(r"(?P<before>([^'/]*'[^']*'[^'/]*|[^/]*)*){1}(?P<after>.*)",re.I)
continued = re.compile(r"(?P<before>[^']*)(?P<after>'[^']*)\s*\Z",re.I)
emptyline = re.compile(r'\s*\Z',re.I)

entryquote = re.compile(r'(?P<before>[^\'"]*)(?P<after>.*)\s*\Z',re.I)
exitsquote = re.compile(r"'(?P<before>(.*?[^']+|))'(?P<after>[^']+.*)\s*\Z",re.I)
exitdquote = re.compile(r'"(?P<before>(.*?[^"]+|))"(?P<after>[^"]+.*)\s*\Z',re.I)

key_none = re.compile(r'\s*&\w*\s+(?P<after>.*)',re.I)
key_equals = re.compile(r'(?P<key>[^=:]*)(?P<after>.*)',re.I)
val_equals = re.compile(r"[=:;]\s*(?P<val>('.*?'|[^\s;']*))\s*(?P<after>.*)",re.I)

dicokeys = ['AIDE','AIDE1','APPARENCE','CHOIX','CHOIX1','COMPORT','COMPOSE',
    'CONTROLE','DEFAUT','DEFAUT1','INDEX','MNEMO','NIVEAU','NOM','NOM1', \
    'RUBRIQUE','RUBRIQUE1','TAILLE','TYPE','SUBMIT']
# _____             ________________________________________________
# ____/ CAS FILES  /_______________________________________________/
#
def scanCAS(cas):
   keylist = []
   casLines = getFileContent(cas)
   # ~~ clean comments
   core = []
   for i in range(len(casLines)):
      line = casLines[i].replace('"""',"'''").replace('"',"'")
      proc = re.match(key_comment,line+'/')
      line = proc.group('before').strip() + ' '
      proc = re.match(emptyline,line)
      if not proc: core.append(line)
   casStream = (' '.join(core)).replace('  ',' ')
   # ~~ clean values to keep only the keys
   while casStream != '':
      # ~~ non-key
      proc = re.match(key_none,casStream)
      if proc:
         casStream = proc.group('after')
         continue
      # ~~ key
      proc = re.match(key_equals,casStream)
      if not proc:
         print '... hmmm, did not see this one coming ...'
         break
      kw = proc.group('key').strip()
      casStream = proc.group('after')   # still hold the separator
      # ~~ val
      proc = re.match(val_equals,casStream)
      if not proc:
         print 'no value to keyword ',kw
         sys.exit()
      val = []
      while proc:
         val.append(proc.group('val').replace("'",''))
         casStream = proc.group('after')   # still hold the separator
         proc = re.match(val_equals,casStream)
      keylist.append([kw,val])

   # ~~ sort out the groups, starting with 'NOM'
   keywords = {}
   while keylist != []:
      keywords.update({keylist[0][0]:keylist[0][1]})
      keylist.pop(0)

   return keywords

# _____              _______________________________________________
# ____/ DICO FILES  /______________________________________________/
#
"""
   keywords.keys() are in French. dico provides you with the translation
"""
def scanDICO(dicoFile):
   keylist = []
   dicoLines = getFileContent(dicoFile)
   # ~~ buddle continuations (long strings) and remove comments and empty lines
   core = []; i = -1
   while i < len(dicoLines) - 1:
      i = i + 1; line = ''
      l = dicoLines[i].strip()
      #proc = re.match(key_comment,l)
      #if proc: l = proc.group('before').strip() + ' '
      if l.strip()[0:1] == '/' : continue
      proc = re.match(emptyline,l)
      if proc: continue
      proc = re.match(key_none,l)
      if proc: continue
      proc = re.match(entryquote,l)
      line = proc.group('before')
      l = proc.group('after')
      while l != '':
         if l[0:1] == '"':
            proc = re.match(exitdquote,l+' ')
            if proc:
               line = line + "'" + proc.group('before').replace("'",'"') + "'"
               proc = re.match(entryquote,proc.group('after').strip())
               line = line + proc.group('before')
               l = proc.group('after').strip()
               print '>',l
            else:
               i = i + 1
               l = l.strip() + ' ' + dicoLines[i].strip()
         elif l[0:1] == "'":
            proc = re.match(exitsquote,l+' ')
            if proc:
               line = line + "'" + proc.group('before').replace("'",'"') + "'"
               proc = re.match(entryquote,proc.group('after').strip())
               line = line + proc.group('before')
               l = proc.group('after').strip()
            else:
               i = i + 1
               l = l.strip() + ' ' + dicoLines[i].strip()
      core.append(line)
   dicoStream = (' '.join(core)).replace('  ',' ').replace('""','"')
   # ~~ clean values to keep only the keys
   while dicoStream != '':
      # ~~ non-key
      proc = re.match(key_none,dicoStream)
      if proc:
         dicoStream = proc.group('after')
         continue
      # ~~ key
      proc = re.match(key_equals,dicoStream)
      if not proc: break
      kw = proc.group('key').strip()
      if kw not in dicokeys:
         print 'unknown key ',kw
         sys.exit()
      dicoStream = proc.group('after')   # still hold the separator
      # ~~ val
      proc = re.match(val_equals,dicoStream)
      if not proc:
         print 'no value to keyword ',kw
         sys.exit()
      val = []
      while proc:
         if proc.group('val')[0] == "'":
            val.append(proc.group('val')[1:len(proc.group('val'))-1])
         else:
            val.append(proc.group('val'))
         dicoStream = proc.group('after')   # still hold the separator
         proc = re.match(val_equals,dicoStream)
      keylist.append([kw,val])
   # ~~ sort out the groups, starting with 'NOM'
   dico = {'FR':{},'GB':{},'DICO':dicoFile}; keywords = {}
   while keylist != []:
      if keylist[0][0] != 'NOM' and keylist[1][0] != 'NOM1':
         print 'could not read NOM or NOM1 from ',keylist[0][1]
         sys.exit()
      dico['FR'].update({keylist[0][1][0]:keylist[1][1][0]})
      dico['GB'].update({keylist[1][1][0]:keylist[0][1][0]})
      key = keylist[0][1][0]
      words = {'NOM':keylist[0][1]}; keylist.pop(0)
      while keylist != []:
         if keylist[0][0] == 'NOM': break
         words.update({keylist[0][0]:keylist[0][1]})
         keylist.pop(0)
      keywords.update({key:words})

   return dico,keywords

"""
   getIOFilesSubmit returns both French and English keys + SUBMIT actions

"""
def getIOFilesSubmit(frgb,dico):
   iFiles = {}; oFiles = {}
   for key in dico.keys():
      if dico[key].has_key('SUBMIT'):
         if 'LIT' in dico[key]['SUBMIT'][0]:
            iFiles.update({key:dico[key]['SUBMIT'][0]})
            iFiles.update({frgb['FR'][key]:dico[key]['SUBMIT'][0]})
         elif 'ECR' in dico[key]['SUBMIT'][0]:
            oFiles.update({key:dico[key]['SUBMIT'][0]})
            oFiles.update({frgb['FR'][key]:dico[key]['SUBMIT'][0]})
         else:
            if 'void' not in dico[key]['SUBMIT'][0]:
               print '... hmm, this is embarassing. I do not know what to do with ', key
               sys.exit()

   return iFiles,oFiles

def getKeyWord(key,cas,dico,frgb):

   value = []; defaut = []
   if key in frgb['GB'].keys():
      defaut = dico[frgb['GB'][key]]['DEFAUT1']
      if key in cas.keys(): value = cas[key]
      elif frgb['GB'][key] in cas.keys(): value = cas[frgb['GB'][key]]
   if key in frgb['FR'].keys():
      defaut = dico[key]['DEFAUT']
      if key in cas.keys(): value = cas[key]
      elif frgb['FR'][key] in cas.keys(): value = cas[frgb['FR'][key]]

   return value,defaut

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   CFGNAME = ''
   SYSTELCFG = 'systel.cfg'
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configname",
                      type="string",
                      dest="configName",
                      default=CFGNAME,
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
   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.dirname(options.configFile)
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         for dirpath,dirnames,filenames in walk(dircfg) : break
         for file in filenames :
            head,tail = path.splitext(file)
            if tail == '.cfg' : print '    +> ',file
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile)
   cfgnames = cfgs.keys()
   if options.configName != '':
      if options.configName not in cfgnames:
         print '\nNot able to find your configuration in the configuration file: ' + options.configFile + '\n'
         print ' ... use instead:'
         for cfgname in cfgnames : print '    +> ',cfgname
         sys.exit()
      cfgnames = [options.configName]

   #  /!\  for testing purposes ... no real use
   for cfgname in cfgnames:
      # still in lower case
      if options.rootDir != '': cfgs[cfgname]['root'] = options.rootDir
      if options.version != '': cfgs[cfgname]['version'] = options.version
      # parsing for proper naming
      cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])

      debug = True

      for mod in cfg['VALIDATION'].keys():
# ~~ Scans all CAS files to launch validation ~~~~~~~~~~~~~~~~~~~~~~
         print '\n\nConfiguration ' + cfgname + ', Module '+ mod + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         print '... reading module dictionary'
         frgb,dico = scanDICO(path.join(path.join(cfg['MODULES'][mod]['path'],'lib'),mod+cfg['TELVER']+'.dico'))
         for casFile in cfg['VALIDATION'][mod]:
            print '... CAS file: ',casFile
            casKeys = scanCAS(casFile)
               #/!\ for testing purposes ... no real use.

   sys.exit()

