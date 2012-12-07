"""@author Sebastien E. Bourban and Noemie Durand
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
"""@history 28/04/2011 -- Sebastien E. Bourban
         Now supports SYSTELCFG as a directory (old Perl version, to which
         systel.cfg is added) or as a file.
"""
"""@history 30/04/2011 -- Sebastien E. Bourban
         Upgrade made to config parsing to include the option to reset
         the version and the root from the command line option:
         -v <version>, reset the version read in the config file with this
         -r <root>, reset the root path read in the config file with this
"""
"""@history 21/05/2012 -- Sebastien E. Bourban
         Addition of the method setKeyValue, in order to force keyword values,
         for instance, in the case of running scalar case with parallel
         Also, scanDICO now understands the type of each keyword.
"""
"""@history 10/07/2012 -- Christophe Coulet
         Addition of a specific test and management of long lines because some
         variables, such as the path could be greater than 72 characters.
         Update (FD,30/08/212) : '<73' changed to '<72' (bug in 015_bosse_mixte)
"""
"""@history 04/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Simplifying call to parseConfigFile, which now takes two arguments
      options.configFile, and options.configName and return one or more
      valid configurations in an array. Testing for validity is now done
      within config.py
"""
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import re
from os import path,walk,environ
import sys
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_ValidateTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.files import getFileContent,putFileContent

# _____               ______________________________________________
# ____/ Instructions /_____________________________________________/
#

debug = False

# _____                 ____________________________________________
# ____/ Keyword parse  /___________________________________________/
#

key_comment = re.compile(r"(?P<before>([^'/]*'[^']*'[^'/]*|[^/]*)*){1}(?P<after>.*)",re.I)
continued = re.compile(r"(?P<before>[^']*)(?P<after>'[^']*)\s*\Z",re.I)
emptyline = re.compile(r'\s*\Z',re.I)

entryquote = re.compile(r'(?P<before>[^\'"]*)(?P<after>.*)\s*\Z',re.I)
exitsquote = re.compile(r"'(?P<before>(.*?[^']+|))'(?P<after>[^']+.*)\s*\Z",re.I)
exitdquote = re.compile(r'"(?P<before>(.*?[^"]+|))"(?P<after>[^"]+.*)\s*\Z',re.I)

key_none = re.compile(r'\s*(?P<key>&\w*)\s+(?P<after>.*)',re.I)
key_equals = re.compile(r'(?P<key>[^=:]*)(?P<after>.*)',re.I)
val_equals = re.compile(r"[=:;]\s*(?P<val>('.*?'|[^\s;']*))\s*(?P<after>.*)",re.I)

key_word = r'\s*(?P<this>(%s))\s*(?P<after>.*)\s*\Z'
val_word = r"\s*[=:;]\s*(?P<this>('.*?'|%s))\s*(?P<after>.*)\s*\Z"

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
      line = casLines[i].replace('"""',"'''").replace('"',"'").replace("''",'"')
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

def readCAS(keywords,dico,frgb):

   outwords = keywords.copy()       # shallow copy is here sufficient
   for key,value in keywords.iteritems():
      kw = key
      if kw not in dico.keys(): kw = frgb['GB'][kw]
      if dico[kw]['TYPE'][0] == 'LOGIQUE':
         vals = []
         for val in value:
            if val.upper() in ['YES','Y','TRUE','OUI','O','VRAI']: vals.append('TRUE')
            if val.upper() in ['NO','N','FALSE','NON','N','FAUX']: vals.append('FALSE')
         outwords.update({key:vals})
      elif dico[kw]['TYPE'][0] in ['ENTIER','INTEGER']:
         vals = []
         for val in value: vals.append(int(val))
         outwords.update({key:vals})
      elif dico[kw]['TYPE'][0] in ['REEL','REAL']:
         vals = []
         for val in value: vals.append(float(val.lower().replace('d','e')))
         outwords.update({key:vals})
      else:
         vals = []
         for val in value: vals.append(repr(val))
         outwords.update({key:vals})

   return outwords

def rewriteCAS(cas):

   lines = []
   for key,val in cas.iteritems():

      # ~~~> Check if final size more than 72 characters
      if len(' ' + key + ' : ' + str(val[0])) < 73:
         line = ''; lcur = ' ' + key + ' : ' + str(val[0])
      else:
         line = ' ' + key + ' :\n'
         if len('    ' + str(val[0])) < 73: lcur = '    ' + str(val[0])
         else:
            lcur = ''
            for i in range(len(str(val[0]))/65+1):
               lcur = lcur + '    ' + ( str(val[0])+65*' ' )[65*i:65*i+65] + '\n'
      for v in val[1:]:
         if len(lcur + ';'+str(v)) < 72:
            lcur = lcur + ';'+str(v)
         else:
            if len(lcur) < 72:
               line = line + lcur + ';\n'
               lcur = '    '+str(v)
            else:  '... warning: CAS file cannot read this value: ',lcur
      lines.append(line+lcur)

   lines.append('')
   return lines

def translateCAS(cas,frgb):
   casLines = getFileContent(cas)

   core = []
   for i in range(len(casLines)):
      # ~~> scan through to remove all comments
      casLines[i] = casLines[i].replace('"""',"'''").replace('"',"'")
      proc = re.match(key_comment,casLines[i]+'/')
      head = proc.group('before').strip()
      # ~~> special keys starting with '&'
      if not re.match(key_none,head+' '): core.append(head)
   casStream = ' '.join(core)

   frLines = []; gbLines = []
   for i in range(len(casLines)):

      # ~~> split comments
      casLines[i] = casLines[i].replace('"""',"'''").replace('"',"'")
      proc = re.match(key_comment,casLines[i]+'/')
      head = proc.group('before').strip()
      tail = proc.group('after').rstrip('/').strip()  # /!\ is not translated
      # ~~ special keys starting with '&'
      p = re.match(key_none,head+' ')
      if p:
         head = ''
         tail = casLines[i].strip()
      frline = head
      gbline = head

      if head != '' and casStream == '':
         raise Exception([{'name':'translateCAS','msg':'could not translate this cas file after the line:\n'+head}])
      # ~~> this is a must for multiple keywords on one line
      while casStream != '':
         proc = re.match(key_equals,casStream)
         if not proc:
            print '... hmmm, did not see this one coming ...'
            break
         kw = proc.group('key').strip()
         if kw not in head: break  # move on to next line

         # ~~> translate the keyword
         head = head.replace(kw,'',1)
         if kw.upper() in frgb['GB'].keys(): frline = frline.replace(kw,frgb['GB'][kw],1)
         if kw.upper() in frgb['FR'].keys(): gbline = gbline.replace(kw,frgb['FR'][kw],1)

         # ~~> look for less obvious keywords
         casStream = proc.group('after')   # still hold the separator
         proc = re.match(val_equals,casStream)
         if not proc:
            raise Exception([{'name':'translateCAS','msg':'no value to keyword: '+kw}])
         while proc:
            casStream = proc.group('after')   # still hold the separator
            proc = re.match(val_equals,casStream)

      # final append
      if frline != '': frline = ' ' + frline
      frLines.append(frline + tail)
      if gbline != '': gbline = ' ' + gbline
      gbLines.append(gbline + tail)

   # ~~ print FR and GB versions of the CAS file
   putFileContent(cas+'.fr',frLines)
   putFileContent(cas+'.gb',gbLines)

   return cas+'.fr',cas+'.gb'

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
         print 'unknown key ',kw,proc.group('after'),dicoStream
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
      dico['FR'].update({keylist[0][1][0].replace('"',"'"):keylist[1][1][0].replace('"',"'")})
      dico['GB'].update({keylist[1][1][0].replace('"',"'"):keylist[0][1][0].replace('"',"'")})
      key = keylist[0][1][0].replace('"',"'")
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
               print '... hmm, this is embarrassing. I do not know what to do with ', key
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

def getSubmitWord(key,cas,iFS,oFS):

   value = []
   for i in iFS.keys():
      if key == iFS[i].split(';')[1]:
         if i in cas.keys(): value = cas[i]
   for i in oFS.keys():
      if key == oFS[i].split(';')[1]:
         if i in cas.keys(): value = cas[i]

   return value

def setKeyValue(key,cas,frgb,value):

   if key in frgb['GB'].keys():
      if key in cas.keys(): cas[key] = [value]
      elif frgb['GB'][key] in cas.keys(): cas[frgb['GB'][key]] = [value]
      else: cas.update({key:[value]})
   if key in frgb['FR'].keys():
      if key in cas.keys(): cas[key] = [value]
      elif frgb['FR'][key] in cas.keys(): cas[frgb['FR'][key]] = [value]
      else: cas.update({key:[value]})

   return True

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   USETELCFG = ''
   if environ.has_key('USETELCFG'): USETELCFG = environ['USETELCFG']
   SYSTELCFG = 'systel.cfg'
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
   parser.add_option("-k","--rank",type="string",dest="rank",default='all',
      help="the suite of validation ranks (all by defult)" )
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
   cfgs = parseConfigFile(options.configFile,options.configName)

   #  /!\  for testing purposes ... no real use
   for cfgname in cfgs.keys():
      # still in lower case
      if options.rootDir != '': cfgs[cfgname]['root'] = options.rootDir
      if options.version != '': cfgs[cfgname]['version'] = options.version
      # parsing for proper naming
      if options.rank != '': cfgs[cfgname]['val_rank'] = options.rank
      cfg = parseConfig_ValidateTELEMAC(cfgs[cfgname])

      debug = True

      for mod in cfg['VALIDATION'].keys():
# ~~ Scans all CAS files to launch validation ~~~~~~~~~~~~~~~~~~~~~~
         print '\n\nConfiguration ' + cfgname + ', Module '+ mod + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
         print '... reading module dictionary'
         frgb,dico = scanDICO(path.join(path.join(cfg['MODULES'][mod]['path'],'lib'),mod+cfg['version']+'.dico'))
         for casFile in cfg['VALIDATION'][mod]:
            print '... CAS file: ',casFile
            casKeys = readCAS(scanCAS(casFile),dico,frgb)
               #/!\ for testing purposes ... no real use.

   sys.exit()

