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
      Upgrade made to config parsing to include the option to reset the
      version and the root from the command line option:
      -v <version>, reset the version read in the config file with this
      -r <root>, reset the root path read in the config file with this
"""
"""@history 04/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Simplifying call to parseConfigFile, which now takes two arguments
      options.configFile, and options.configName and return one or more
      valid configurations in an array. Testing for validity is now done
      within config.py
"""
"""@history 10/01/2013 -- Yoann Audouin
   ScanSources goes through subdirectories as well now ignore
   hidden directories
   Adding scan of .F and .F90 files as well
"""
"""@history 06/02/2013 -- Sebastien E. Bourban
   Adding the functionality of displaying changes (html/diff) made
      to a PRINCI file by comparing individual subroutines to their
      original version.
   Further capability to compare changes made between 2 PRINCI files.
"""
"""@history 01/07/2013 -- Sebastien E. Bourban and Yoann Audoin
   Upgrade to the new structure
"""
"""@history 13/07/2013 -- Sebastien E. Bourban
   Now deals with DECLARATIONS first before identifying unkonwn externals
"""
"""@history 23/09/2014 -- Sebastien E. Bourban and Yoann Audoin
   The content of the log files from GRETEL and PARTEL are now reported
   in the error report.
"""
"""@brief
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import re
import sys
from optparse import Values
from copy import copy,deepcopy
from os import path,walk,remove, environ, sep
from argparse import ArgumentParser,RawDescriptionHelpFormatter
# ~~> dependencies towards the root of pytel
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) ) # clever you !
from config import parseConfigFile, parseConfig_CompileTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.files import getTheseFiles,isNewer,addToList,addFileContent,getFileContent,putFileContent,diffTextFiles
from utils.progressbar import ProgressBar

debug = False

# _____               ______________________________________________
# ____/ Instructions /_____________________________________________/
#

listINSTRUCTION = ['ALLOCATE','ASSIGN',  \
   'BACKSPACE','BLOCK DATA',  \
   'CALL','CASE','CLOSE','COMMON','CYCLE','CONTINUE',  \
   'DATA','DEALLOCATE','DEFAULT','DO',  \
   'ELSE','ELSEIF','ENDIF','ENDDO','ENDSELECT','END','ENDFILE','EQUIVALENCE','EXIT',  \
   'FORMAT',  \
   'GO','TO','GOTO',  \
   'IF','IMPLICIT NONE','INCLUDE','INQUIRE','INTERFACE',  \
   'MULTIPLE',  \
   'NAMELIST','NULLIFY',  \
   'OPEN',  \
   'PRINT',  \
   'READ','REWIND','RETURN',  \
   'SELECT','STOP','SAVE', \
   'THEN', 'USE',  \
   'WHILE','WHERE','WRITE' ]

listINTRINSIC = [ \
   'ABS', 'ACCESS','ACHAR','ACOS','ACOSH','ADJUSTL','ADJUSTR','AIMAG','AINT','ALARM','ALL', \
   'ALLOCATED','AND','ANINT','ANY','ASIN','ASINH','ASSOCIATED','ATAN','ATAN2','ATANH', \
   'BESJ0','BESJ1','BESJN','BESY0','BESY1','BESYN','BIT_SIZE','BTEST', \
   'CEILING','CHAR','CHDIR','CHMOD','CMPLX','COMMAND_ARGUMENT_COUNT','CONJG','COS','COSH','COUNT','CPU_TIME','CSHIFT','CTIME', \
   'DATE_AND_TIME','DBLE','DCMPLX','DFLOAT','DIGITS','DIM','DOT_PRODUCT','DPROD','DREAL','DTIME', \
   'DMAX1','DMIN1','DMOD','DSQRT','DSIN','DCOS','DTAN','DABS','DATAN','DATAN2','DEXP','DLOG','DSINH','DCOSH','DTANH', \
   'EOSHIFT','EPSILON','ERF','ERFC','ETIME','EXIT','EXP','EXPONENT', \
   'FDATE','FGET','FGETC','FLOAT','FLOOR','FLUSH','FNUM','FPUT','FPUTC','FRACTION','FREE','FSEEK','FSTAT','FTELL', \
   'GERROR','GETARG','GET_COMMAND','GET_COMMAND_ARGUMENT','GETCWD','GETENV','GET_ENVIRONMENT_VARIABLE','GETGID','GETLOG','GETPID','GETUID','GMTIME', \
   'HOSTNM','HUGE', \
   'IACHAR','IAND','IARGC','IBCLR','IBITS','IBSET','ICHAR','IDATE','IEOR','IERRNO', \
   'INDEX','IDINT','INT','INT2','INT8','IOR','IRAND','ISATTY','ISHFT','ISHFTC','ITIME', \
   'KILL','KIND', \
   'LBOUND','LEN','LEN_TRIM','LGE','LGT','LINK','LLE','LLT','LNBLNK','LOC','LOG','LOG10','LOGICAL','LONG','LSHIFT','LSTAT','LTIME', \
   'MALLOC','MATMUL','MAX','MAX0','MAXEXPONENT','MAXLOC','MAXVAL','MCLOCK','MCLOCK8','MERGE','MIN','MIN0','MINEXPONENT','MINLOC','MINVAL','MOD','MODULO','MOVE_ALLOC','MVBITS', \
   'NEAREST','NEW_LINE','NINT','NOT','NULL', \
   'OR', \
   'PACK','PERROR','PRECISION','PRESENT','PRODUCT', \
   'RADIX','RANDOM_NUMBER','RANDOM_SEED','RAND','RANGE','RAN','REAL','RENAME','REPEAT','RESHAPE','RRSPACING','RSHIFT', \
   'SCALE','SCAN','SECNDS','SECOND','SELECTED_INT_KIND','SELECTED_REAL_KIND','SET_EXPONENT','SHAPE','SIGN','SIGNAL','SIN','SINH','SIZE', \
   'SLEEP','SNGL','SPACING','SPREAD','SQRT','SRAND','STAT','SUM','SYMLNK','SYSTEM','SYSTEM_CLOCK', \
   'TAN','TANH','TIME','TIME8','TINY','TRANSFER','TRANSPOSE','TRIM','TTYNAM', \
   'UBOUND','UMASK','UNLINK','UNPACK','VERIFY','XOR' ]

# _____                             ________________________________
# ____/ Global Regular Expressions /_______________________________/
#
"""
  Softer regular expression to ensure scanning of any sources
"""
#?beforethisafter=r'\s*(?P<before>%s(?=\s*(\b(%s)\b)))'+ \
#?                          r'\s*(?P<this>(\b(%s)\b))'+ \
#?                          r'\s*(?P<after>%s)\s*\Z'
beforethisafter=r'(?P<before>%s(?=\s?(\b(%s)\b)))'+ \
                          r'\s?(?P<this>(\b(%s)\b))'+ \
                          r'\s?(?P<after>%s)\Z'

#?emptyline = re.compile(r'\s*\Z') #,re.I)
emptyline = re.compile(r'\Z') #,re.I)

f77comment = re.compile(r'[C!#*]') #,re.I)
f77continu2 = re.compile(r'(\s{5}\S)(?P<line>.*)') #,re.I)
#?f90comment = re.compile(r'(?P<line>([^"]*"[^"]*"[^"!]*|[^\']*\'[^\']*\'[^\'!]*|[^!]*))!{1}(?P<rest>.*)') #,re.I)
f90comment = re.compile(r'(?P<line>([^"]*"[^"]*"[^"!]*|[^\']*\'[^\']*\'[^\'!]*|[^!]*))!{1}(?P<rest>[^\Z]*)') #,re.I)
#?f90continu1 = re.compile(r'(?P<line>.*)&\s*\Z') #,re.I)
f90continu1 = re.compile(r'(?P<line>.*)&\Z') #,re.I)
#?f90continu2 = re.compile(r'(\s*&\s*)(?P<line>.*)&\s*\Z') #,re.I)
f90continu2 = re.compile(r'(&\s?)(?P<line>.*)&\Z') #,re.I)
#?f90continu3 = re.compile(r'(\s*&\s*)(?P<line>.*)') #,re.I)
f90continu3 = re.compile(r'(&\s?)(?P<line>.*)') #,re.I)

var_dquots = re.compile(r'(?P<dquot>".*?")') #,re.I)
var_squots = re.compile(r"(?P<squot>'.*?')") #,re.I)
var_bracks = re.compile(r'(?P<brack>\([\w,*\s+-/:%]*?\))') #,re.I)

# no (\+|\-)? to capture the sign if there ... different from the utils version
#?var_doublep = re.compile(r'\s*(?P<before>.*?)(?P<number>\b(|[^a-zA-Z(,])(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+)(\b|[^a-zA-Z,)])))(?P<after>.*?)\s*\Z') #,re.I)
var_doublep = re.compile(r'(?P<before>.*?)(?P<number>\b(|[^a-zA-Z(,])(?:(\d+(|\.)\d*[dDeE](\+|\-)?\d+|\d+\.\d+)(\b|[^a-zA-Z,)])))(?P<after>.*?)\Z') #,re.I)
#?var_integer = re.compile(r'\s*(?P<before>.*?)(?P<number>\b(|[^a-zA-Z(,])(?:(\d+)(\b|[^a-zA-Z,)])))(?P<after>.*?)\s*\Z') #,re.I)
var_integer = re.compile(r'(?P<before>.*?)(?P<number>\b(|[^a-zA-Z(,])(?:(\d+)(_\w+|\b|[^a-zA-Z,)])))(?P<after>.*?)\Z') #,re.I)
f90logic = '(FALSE|TRUE|EQV|EQ|NE|GT|LT|GE|LE|OR|AND|XOR)'
#?var_logical = re.compile(r'.*?(?P<logic>\.\s*?%s\s*?\.).*?'%(f90logic)) #,re.I)
var_logical = re.compile(r'.*?(?P<logic>\.\s?%s\s?\.)'%(f90logic)) #,re.I)
#?var_pointer = re.compile(r'\s*(?P<before>.*?)(?P<this>%\w+)(?P<after>.*?)\s*\Z') #,re.I)
var_pointer = re.compile(r'(?P<before>.*?)(?P<this>%\w+)(?P<after>.*?)\Z') #,re.I)
#?var_word = re.compile(r'\s*(?P<before>.*?)(?P<word>\b\w+?\b)(?P<after>.*?)\s*\Z') #,re.I)
var_word = re.compile(r'(?P<before>.*?)(?P<word>\b\w+?\b)(?P<after>.*?)\Z') #,re.I)

var_only = re.compile(r'\s*ONLY\s*:(?P<after>.*?)\Z') #,re.I)
#?var_assocs = re.compile(r'\s*(?P<before>.*?)(?P<name>\s*\b\w+?\b\s*=\s*)(?P<after>.*?)\s*\Z') #,re.I)
var_assocs = re.compile(r'(?P<before>.*?)(?P<name>\s?\b\w+?\b\s?=\s?)(?P<after>.*?)\Z') #,re.I)
#?var_equals = re.compile(r'\s*(?P<before>.*?)(?P<name>\s*\b\w+?\b)(?P<value>\s*=[^,]*)(?P<after>.*?)\s*\Z') #,re.I)
var_equals = re.compile(r'(?P<before>.*?)(?P<name>\s?\b\w+?\b)(?P<value>\s?=[^,]*)(?P<after>.*?)\Z') #,re.I)
#?var_operand = re.compile(r'\s*(?P<before>.*?)(?=[^\s])\W+(?P<after>.*?)\s*\Z') #,re.I)
var_operand = re.compile(r'(?P<before>.*?)(?=[^\s])\W+(?P<after>.*?)\Z') #,re.I)
#?argnames = re.compile(r'\s*(?P<name>\b\w+\b)\s*?(|\((?P<args>.*)\))\s*\Z') #,re.I)
argnames = re.compile(r'(?P<name>\b\w+\b)\s*(|\((?P<args>.*)\))\Z') #,re.I)

#?itf_title = re.compile(r'\s*?\bINTERFACE\b(|\s+?(?P<name>\w+).*?)\s*\Z') #,re.I)
itf_title = re.compile(r'\bINTERFACE\b(|\s(?P<name>\w+).*?)\Z') #,re.I)
#?itf_uless = re.compile(r'\s*?\bMODULE\s+?PROCEDURE\s+?(?P<name>\w+).*?\s*\Z') #,re.I)
itf_uless = re.compile(r'\bMODULE\sPROCEDURE\s(?P<name>\w+).*?\Z') #,re.I)
#?itf_close = re.compile(r'\s*?\bEND\s+INTERFACE\b(|\s+?(?P<name>\w+).*?)\s*\Z') #,re.I)
itf_close = re.compile(r'\bEND\sINTERFACE\b(|\s(?P<name>\w+).*?)\Z') #,re.I)
#?use_title = re.compile(r'\s*?\bUSE\b\s+?(?P<name>\b\w+\b)\s*(|,\s*(?P<after>.*?))\s*\Z') #,re.I)
use_title = re.compile(r'\bUSE\b\s(?P<name>\b\w+\b)\s?(|,\s?(?P<after>.*?))\Z') #,re.I)
#?xtn_title = re.compile(r'.*?\bEXTERNAL\b(.*?::)?\s*?(?P<vars>.*?)\s*\Z') #,re.I)
xtn_title = re.compile(r'.*?\bEXTERNAL\b(.*?::)?\s?(?P<vars>.*?)\Z') #,re.I)
#?itz_title = re.compile(r'\s*?\bINTRINSIC\b(.*?::)?\s*?(?P<vars>.*?)\s*\Z') #,re.I)
itz_title = re.compile(r'\bINTRINSIC\b([^:]?::)?\s?(?P<vars>.*?)\Z') #,re.I)
eqv_title = re.compile(r'\bEQUIVALENCE\b([^:]?::)?\s?(?P<vars>.*?)\Z') #,re.I)
#?implictNone = re.compile(r'\s*?\bIMPLICIT\s+NONE\b\s*\Z') #,re.I)
implictNone = re.compile(r'\bIMPLICIT\sNONE\b\Z') #,re.I)
#?inc_title = re.compile(r'\s*?\bINCLUDE\b\s*(?P<file>.*?)\s*\Z') #,re.I)
inc_title = re.compile(r'\bINCLUDE\b\s?(?P<file>.*?)\Z') #,re.I)
#?cmn_title = re.compile(r'\s*?\bCOMMON\b\s*?/\s*(?P<name>\w*)\s*/\s*?(?P<after>.*?)\s*\Z') #,re.I)
cmn_title = re.compile(r'\bCOMMON\b\s?/\s?(?P<name>\w*)\s?/\s?(?P<after>.*?)\Z') #,re.I)
#?ctn_title = re.compile(r'\s*?\bCONTAINS\b\s*\Z') #,re.I)
ctn_title = re.compile(r'\bCONTAINS\b\Z') #,re.I)
#?def_title = re.compile(r'\s*?\bTYPE\b\s+?(?P<name>\b\w+\b)\s*\Z') #,re.I)
def_title = re.compile(r'\bTYPE\b([^:]?::)?\s(?P<name>\b\w+\b)\Z') #,re.I)
#?def_close = re.compile(r'\s*?\bEND TYPE\b\s+?(?P<name>\b\w+\b)\s*\Z') #,re.I)
def_close = re.compile(r'\bEND TYPE\b(|\s(?P<name>\b\w+\b))\Z') #,re.I)

#?als_core = re.compile(r'(?P<before>.*?)(?P<alias>\b\w([\w%]|\([\w(,:)%]*\))*)\s*=>\s*(?P<link>\b\w+?\b)(?P<after>.*?)\s*\Z') #,re.I)
als_core = re.compile(r'(?P<before>.*?)(?P<alias>\b\w([\w%]|\([\w(,:)%]*\))*)\s?=>\s?(?P<link>\b\w+?\b)(?P<after>.*?)\Z') #,re.I)
#?var_format = re.compile(r'\s*(?P<before>.*?)\d+?\s+?\bFORMAT\b(?P<after>.*?)\s*\Z') #,re.I)
var_format = re.compile(r'(?P<before>.*?)\d+?\s\bFORMAT\b(?P<after>.*?)\Z') #,re.I)

#?cls_title = re.compile(r".*?\bCALL\b\s+?(?P<name>\b\w+\b)\s*?(|\((?P<args>[\w\s\*\+\-\/=,%('.:)]*)\))\s*\Z") #,re.I)
cls_title = re.compile(r".*?\bCALL\b\s(?P<name>\b\w+\b)\s?(|\((?P<args>[\w\s\*\+\-\/=,%('.:)]*)\))\Z") #,re.I)
#?fct_title = re.compile(r"\s*(?P<before>.*?)(?P<name>\b\w+\b)\s*?\((?P<args>[\w\s\*\+\-\/=,%('.:)]*)\)(?P<after>.*?)\s*\Z") #,re.I)
fct_title = re.compile(r"(?P<before>.*?)(?P<name>\b\w+\b)\s?\((?P<args>[\w\s\*\+\-\/=,%('.:)]*)\)(?P<after>.*?)\Z") #,re.I)
#?f90types = '(CHARACTER|LOGICAL|INTEGER|REAL|COMPLEX|DOUBLE\s*(PRECISION\s*(COMPLEX|)|COMPLEX))\s*?(\**\s*?\d+|\**\(.*?\))?|TYPE\s*\([\w\s,=(*)]*?\)'
f90types = '(CHARACTER|LOGICAL|INTEGER|REAL|COMPLEX|DOUBLE\s?(PRECISION\s?(COMPLEX|)|COMPLEX))\s?(\**\s?\d+|\**\(.*?\))?|TYPE\s?\([\w\s,=(*)]*?\)'
#?f90xport = '(PUBLIC|PRIVATE|SAVE|PARAMETER|DATA|SEQUENCE)\s*?'
f90xport = '(PUBLIC|PRIVATE|SAVE|PARAMETER|DATA|SEQUENCE)'
#?f95_name = re.compile(r"\s*(?P<name>\b\w+\b)\s*?:\s*(?P<after>.*?)\s*\Z") #,re.I)
f95_name = re.compile(r"(?P<name>\b\w+\b)\s?:\s?(?P<after>.*?)\Z") #,re.I)
#?typ_args = re.compile(r'\s*?(.*?::)?\s*?(?P<vars>.*?)\s*\Z',re.I)
typ_args = re.compile(r'(.*?::)?\s?(?P<vars>.*?)\Z') #,re.I)
#?typ_name = re.compile(r'\s*?(?P<type>(%s))\s*?\Z'%(f90types)) #,re.I)
typ_name = re.compile(r'(?P<type>(%s))\Z'%(f90types)) #,re.I)
#?typ_title = re.compile(r'\s*?(?P<type>(%s))\s*?(?P<after>.*?)\s*\Z'%(f90types)) #,re.I)
typ_title = re.compile(r'(?P<type>(%s))\s?(?P<after>.*?)\Z'%(f90types)) #,re.I)
#?typ_xport = re.compile(r'\s*?(?P<type>(%s))\s*?(?P<after>.*?)\s*\Z'%(f90xport)) #,re.I)
typ_xport = re.compile(r'(?P<type>(%s))\s?(?P<after>.*?)\Z'%(f90xport)) #,re.I)

#?pcl_title = re.compile(r'\s*?((?P<type>[\w\s(=*+-/)]*?)|)\b(?P<object>(PROGRAM|FUNCTION|SUBROUTINE|MODULE))\b\s*(?P<after>.*?)\s*?(\bRESULT\b\s*?(?P<result>\w+[\w\s]*)|)\s*\Z') #,re.I)
pcl_title = re.compile(r'((?P<type>[\w\s(=*+-/)]*?)|)\b(?P<object>(PROGRAM|FUNCTION|PURE FUNCTION|RECURSIVE FUNCTION|SUBROUTINE|RECURSIVE SUBROUTINE|MODULE|BLOCK DATA))\b\s+(?P<after>.*?)\s*(\bRESULT\b[\s\(]*(?P<result>\w+[\w\s]*)\)?|)\Z') #,re.I)
#?pcl_close = re.compile(r'\s*?\bEND\b(|\s+?(?P<object>(PROGRAM|FUNCTION|SUBROUTINE|MODULE))(|\s+?(?P<name>\w+?)))\s*\Z') #,re.I)
pcl_close = re.compile(r'\bEND\b(|\s(?P<object>(PROGRAM|FUNCTION|RECURSIVE FUNCTION|RECURSIVE SUBROUTINE|SUBROUTINE|MODULE))(|\s(?P<name>\w+?)))\Z') #,re.I)

"""
  Ruled regular expression to ensure proper coding for the TELEMAC system
"""
#rle_indent = re.compile(r'')
#rle_comment = re.compile(r'[!#]')
#f77comment = re.compile(r'[C!#*]') #,re.I)

# _____                         ____________________________________
# ____/ FORTRAN Parser Toolbox /___________________________________/
#

def cleanSpaces(istr):
   return istr.strip().replace('  ',' ').replace('  ',' ')

def cleanAssoc(istr):
   while 1:
      ostr = istr
      proc = re.match(var_assocs,ostr)
      if proc:
         istr = proc.group('before')+proc.group('after')
      if ostr == istr: break
   return cleanSpaces(ostr)

def cleanBracks(istr):
   while 1:
      ostr = istr
      for brack in re.findall(var_bracks,ostr):
         istr = istr.replace(brack,'')
      if ostr == istr: return cleanSpaces(ostr)

def cleanEquals(istr):
   while 1:
      ostr = istr
      proc = re.match(var_equals,ostr)
      if proc:
         istr = proc.group('before')+proc.group('name')+proc.group('after')
      if ostr == istr: break
   return cleanSpaces(ostr)

def cleanFormatted(istr):
   proc = re.match(var_format,istr)
   if proc: istr = ''
   return istr.strip()

def cleanInstruction(istr,list):
   ostr = ''
   while 1:
      proc = re.match(var_word,istr)
      if proc:
         ostr = ostr + ' ' + proc.group('before')
         istr = proc.group('after')
         instr = proc.group('word')
         if instr not in list: ostr = ostr + ' ' + instr
      else: break
   return cleanSpaces(ostr)

def cleanLogicals(istr):
   while 1:
      ostr = istr
      proc = re.match(var_logical,ostr)
      if proc:
         istr = istr.replace(proc.group('logic'),' ')
      if ostr == istr: break
   return cleanSpaces(ostr)

def cleanNumbers(istr):
   while 1:
      ostr = istr
      proc = re.match(var_doublep,ostr)
      if proc:
         istr = proc.group('before')+proc.group('after')
      if ostr == istr: break
   while 1:
      ostr = istr
      proc = re.match(var_integer,ostr)
      if proc:
         istr = proc.group('before')+proc.group('after')
      if ostr == istr: break
   return cleanSpaces(ostr)

def cleanOperators(istr):
   while 1:
      ostr = istr
      proc = re.match(var_operand,ostr)
      if proc:
         istr = proc.group('before')+' '+proc.group('after')
      if ostr == istr: break
   return cleanSpaces(ostr)

def cleanPointers(istr):
   while 1:
      ostr = istr
      proc = re.match(var_pointer,ostr)
      if proc:
         istr = proc.group('before')+proc.group('after')
      if ostr == istr: break
   return cleanSpaces(ostr)

def cleanQuotes(istr):
   istr = istr.replace("'''","'").replace('"""',"'")
   # ~~> Deal with single quotes
   while 1:
      ostr = istr
      for quote in re.findall(var_squots,ostr):
         istr = istr.replace(quote,"''")
      if ostr == istr: break
   # ~~> Deal with double quotes (replace by sigle quotes)
   while 1:
      ostr = istr
      for quote in re.findall(var_dquots,ostr):
         istr = istr.replace(quote,"''")
      if ostr == istr: break
   # ~~> Remove the internal apostrophies
   ostr = ostr.replace("''''","''").replace("''''","''")
   return ostr

def cleanEmptyQuotes(istr):
   return istr.replace("''","").replace('"',"")

def cleanEmptySpaces(istr):
   return istr.replace(',,',',')

def refactorName(istr,nami,namo):
   n = re.subn(r"(\b)(%s)(\b)"%(nami),namo,istr)[1]
   while n > 0: istr,n = re.subn(r"(\b)(%s)(\b)"%(nami),namo,istr)
   return istr

def parseAlias(lines):
   listAlias = []; count = 0
   core = []; core.extend(lines)
   for line in lines :
      line = cleanQuotes(line)
      proc = re.match(als_core,line)
      if proc:
         alias = proc.group('alias').strip()
         if not re.match(var_pointer,alias):
            if alias not in listAlias: listAlias.append(alias)
         core[count] = proc.group('before') + ' ' + proc.group('link') + proc.group('after')
      count = count + 1
   return core,listAlias

def parseArgs(ilist):
   return cleanPointers(cleanNumbers(cleanLogicals(cleanEquals(cleanBracks(cleanQuotes(ilist)))))).replace(' ','').split(',')

def parseImplicitNone(lines):
   core = []; core.extend(lines)
   if lines == []: return lines,False
   line = lines[0]
   proc = re.match(implictNone,line)
   if proc :
      core.pop(0)
      return core,True
   return core,False

def parseDeclarations(lines):
   listCommon = []; listDeclar = []; listIntrinsic = []; listExternal = []; listType = {}
   core = []; core.extend(lines)
   ignoreblock = False
   for line in lines :
      headline = False

      # ~~ TYPE Definition ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if not ignoreblock:
         proc = re.match(def_title,line)
         if proc :
            ignoreblock = True
            name = proc.group('name')
            listType.update({name:[]})
            core.pop(0); headline = True; continue
      else:
         # /!\ assuming here you have no COMMON, INTRINSIC, INCLUDE, EXTERNAL,
         #     etc in the TYPE block
         proc = re.match(typ_title,core[0])
         if proc :
            if not re.match(xtn_title,proc.group('after')):
               proc = re.match(typ_args,proc.group('after'))
               if proc :
                  if proc.group('vars') != None:
                     for k in parseArgs(cleanEmptyQuotes(proc.group('vars'))):
                        if k != '': listType[name].append(k)
         proc = re.match(def_close,line)
         if proc :
            ignoreblock = False
            if proc.group('name') and debug: print 'No mane at TYPE END of TYPE ' + name
         core.pop(0); headline = True; continue

      # ~~ Common Declaration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      proc = re.match(cmn_title,line)
      if proc :
         listCommon.append([proc.group('name'),parseArgs(proc.group('after'))])
         core.pop(0); headline = True; continue

      # ~~ Private/Public Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      proc = re.match(typ_xport,line)
      if proc :
         #listCommon.append([proc.group('name'),parseArgs(proc.group('after'))])
         core.pop(0); headline = True; continue

      # ~~ INCLUDE Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      proc = re.match(inc_title,line) # you should parse the content from MainWrap
      if proc : core.pop(0); headline = True; continue

      # ~~ Type Declaration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      proc = re.match(typ_title,line)
      if proc :
         if not re.match(xtn_title,proc.group('after')):
            proc = re.match(typ_args,proc.group('after'))
            if proc :
               if proc.group('vars') != None:
                  for k in parseArgs(cleanEmptyQuotes(proc.group('vars'))):
                     if k != '': listDeclar.append(k)
               core.pop(0); headline = True; continue

      # ~~ Intrinsic Declaration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      proc = re.match(itz_title,line)
      if proc :
         if proc.group('vars') != None: listIntrinsic.extend(parseArgs(proc.group('vars')))
         core.pop(0); headline = True; continue

      # ~~ Equivalence Declaration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      proc = re.match(eqv_title,line)
      if proc :
         #if proc.group('vars') != None: ... parseArgs(proc.group('vars'))
         core.pop(0); headline = True; continue

      # ~~ External Declaration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      proc = re.match(xtn_title,line)
      if proc :
         if proc.group('vars') != None: listExternal.extend(parseArgs(proc.group('vars')))
         core.pop(0); headline = True; continue

      # ~~ Reached main core ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if not headline: break

   return core,{ 'cmn':listCommon, 'dec':listDeclar, 'itz':listIntrinsic, 'xtn':listExternal, 'als':[], 'typ':listType }

def parseUses(lines):
   listUses = {}; core = []; core.extend(lines)
   for line in lines :
      proc = re.match(use_title,line)
      if proc :
         name = proc.group('name')                                               # You should not find None here
         args = ''
         if proc.group('after') != None:                                         # Do you need to distinguish the ONLYs ?
            pa = proc.group('after')
            if pa != '':
               proc = re.match(var_only,pa)
               if proc :
                  args = proc.group('after').strip()
               else:
                  args = pa.strip()
         core.pop(0)
         addToList(listUses,name,args)
      else: break
   return core,listUses

def parseVars(ilist):
   return cleanPointers(cleanNumbers(cleanLogicals(cleanAssoc(cleanBracks(cleanQuotes(ilist)))))).replace(' ',',').split(',')

def parseCalls(lines):
   listCalls = {} #; count = 0
   core = []; core.extend(lines)
   for line in lines :
      proc = re.match(cls_title,cleanPointers(cleanQuotes(line))) # you might not want to clean Logical at this stage /!\
      if proc :
         name = proc.group('name'); args = []
         if proc.group('args') != None: args = parseVars(proc.group('args'))
         addToList(listCalls,name,args)
   return core,listCalls

def parseFunctions(lines):
   listFcts = set([]); listTags = []
   for line in lines :
      line = cleanQuotes(line)
      line = cleanPointers(line)
      proc = re.match(cls_title,cleanLogicals(line))
      if proc :
         line = ''
         if proc.group('args') != None: line = ' '.join(parseVars(proc.group('args')))
      line = cleanFormatted(line)
      proc = re.match(fct_title,line)
      if proc : line = proc.group('before')+proc.group('name')+'('+cleanAssoc(proc.group('args'))+')'+proc.group('after')
      line = cleanLogicals(line)
      line = cleanNumbers(line)
      proc = re.match(f95_name,line)
      if proc:
         line = proc.group('after')
         listTags.append(proc.group('name'))
      line = cleanInstruction(line,listTags)
      line = cleanOperators(line)
      line = cleanInstruction(line,listINSTRUCTION)
      line = cleanInstruction(line,listINTRINSIC)
      if line != '':
         listFcts = listFcts | set(line.split())
   return listFcts

def parsePrincipalWrap(lines):
# you could be parsing the INTERFACE / END INTERFACE as well
# and in the case of a module ... declarations / definitions
   core = []; core.extend(lines)
   face = []
   proc = re.match(pcl_title,lines[0])
   if proc :
      name = proc.group('after')
      resu = proc.group('result')
      objt = proc.group('object')
      typ = proc.group('type').strip()
      if ( typ != '' ):
         if not re.match(typ_name,typ):
            print 'Invalid header type ' + typ + ' ' + objt + ' ' + name + ( '\n|   '.join(lines[:5]) + '\n| ...' )
            sys.exit(1) #return [],[],[],lines
      proc = re.match(argnames,name)
      if proc :
         name = proc.group('name')
         args = []
         if proc.group('args') != None: args = parseArgs(proc.group('args'))
         #~~> header completed
         count = 0; block = 0
         ctain = 0; ltain = False; lface = False
         for line in lines[1:]:                                  # /!\ does not work with empty lines
            count = count + 1
            #~~> interface
            if lface:
               proc = re.match(itf_close,line)
               if proc:
                  lface = False
               else:
                  proc = re.match(itf_uless,line)
                  if not proc: face.append(line)
               core.pop(count)
               count = count - 1
               continue  # THIS IS TO IGNORE THE LOCAL VARIABLES
            else:
               proc = re.match(itf_title,line)
               if proc :
                  lface = True
                  core.pop(count)
                  count = count - 1
            #~~> contains
            if ltain: ctain = ctain - 1
            if not ltain:
               if re.match(ctn_title,line):
                  ltain = True
                  ctain = ctain - 1
            proc = re.match(pcl_close,line)
            if proc :
               block = block - 1
            else:
               proc = re.match(pcl_title,line)
               if proc :
                  t = proc.group('type').strip()
                  if ( t != '' ):
                     if not re.match(typ_name,t): continue
                  block = block + 1
            if block < 0 :
               if ltain: ctain = ctain + 1
               if proc.group('name') != name:
                  if proc.group('name') != None: print 'Different name at END ' + objt + ' ' + name
               if proc.group('object') != objt:
                  if proc.group('object') != None and proc.group('object') not in objt: print 'Different type at END ' + objt
               return core[1:count+ctain],[ objt[0:1], name, args, resu ],face,core[count+ctain+1:count],core[count+1:]
         # ~~> Acount for empty MODULE (including only INTERFACE and CONTAINS)
         if ltain and block == 0 and count+1 >= len(core)-1:
            if proc.group('name') != name:
               if debug: print 'Different name at END ' + objt + ' ' + name
            if proc.group('object') != objt:
               if debug and proc.group('object') not in objt: print 'Different type at END ' + objt
            return core[1:count+ctain],[ objt[0:1], name, args, resu ],face,core[count+ctain+1:count],core[count+2:]
      else:
         print 'Invalid header type for first line ' + lines[0]
         sys.exit(1)

   print 'Invalid header type for first line ' + lines[0]
   sys.exit(1)
   return # /!\ this return is just for python parsing

def parsePrincipalMain(lines,who,typ,name,args,resu):
   core = []; core.extend(lines)
   whi = deepcopy(who); whi['uses'] = {}; whi['vars'] = {}; whi['calls'] = {}; whi['called'] = [] #; whi['alias'] = {}
   whi['type'] = typ; whi['name'] = name; whi['args'] = args; whi['resu'] = resu

   # ~~ Lists aliases in File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   core,alias = parseAlias(core)

   # ~~ Lists uses in File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   core,uses = parseUses(core)
   for k in uses:
      whi['uses'].update({k:[]})
      for v in uses[k]: addToList(whi['uses'],k,v)

   # ~~ Imposes IMPLICIT NONE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   core,l = parseImplicitNone(core)
   if not l and whi['type'] != 'M' and debug:
      print 'No IMPLICIT NONE in ',whi['name'],whi['file']

   # ~~ Lists declarations in File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   core,decs = parseDeclarations(core)
   for dec in decs['xtn']:
      if dec in decs['dec']: decs['dec'].remove(dec)
   # ~~> remove common variables from decs (keps them in common)
   for dec in decs['cmn']:
      for k in dec[1]:
         if k in decs['dec']: decs['dec'].remove(k)
   # ~~> remove arguments from decs (keps them as arguments)
   for dec in args:
      if dec in decs['dec']: decs['dec'].remove(dec)
   #for k in uses:
   #   for v in decs['dec']:
   #      if v in uses[k][0]: decs['dec'].remove(dec)
   for k in decs:
      whi['vars'][k] = []
      if type(decs[k]) == type({}):
         addToList(whi['vars'],k,'')
         whi['vars'][k] = decs[k]
      else:
         for v in decs[k]: addToList(whi['vars'],k,v)
   whi['vars']['als'] = alias

   # ~~ Lists calls in File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   core,calls = parseCalls(core)
   for k in calls:
      if k in listINTRINSIC: continue
      whi['calls'][k] = []
      for v in calls[k]:
         addToList(whi['calls'],k,v) # still includes xtn calls

   # ~~ Lists functions in File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   fcts = []
   for fct in parseFunctions(core):
      if fct not in args:
         if fct not in whi['vars']['dec']:
            if fct not in whi['vars']['als']:
               found = False
               for cmn in whi['vars']['cmn']:
                  if fct in cmn[1]: found = True
               #for cmn in uses:
               #   if fct in uses[cmn][0]: found = True
               if not found and fct != name: fcts.append(fct)
   whi['functions'] = fcts # still includes xtn

   return name,whi,core

"""
   In order for multiple line of code to be interpreted, remove the
   continuation symbols so every line is self contained --
   This takes into account the possibility for f77 and f90
   continuation -- assumes that in between comments have been
   removed already
   Return the set of lines without continuation
"""

def delContinuedsF77(lines):
   # ~~ Assumes code without comments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cmds = []
   cmd  = ''
   for line in lines :
      proc1 = re.match(f77continu2,line)
      if proc1:
         #?cmd = cmd.rstrip() + proc1.group('line')
         cmd = ( cmd.rstrip() + proc1.group('line') ).strip().replace('  ',' ').replace('  ',' ') # /!\ Looking to save on upper space
      else:
         if cmd != '' : cmds.append(cmd)
         #?cmd = line
         cmd = line.strip().replace('  ',' ').replace('  ',' ') # /!\ Looking to save on upper space
   cmds.append(cmd)
   return cmds

def delContinuedsF90(lines):
   # ~~ Assumes code without comments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cmds = []
   cmd  = ''
   cnt = False
   for line in lines :
      proc2 = re.match(f90continu1,line)
      proc3 = re.match(f90continu2,line)
      proc4 = re.match(f90continu3,line)
      if proc3 :
         #?cmd = cmd.rstrip() + proc3.group('line')
         cmd = cleanSpaces( cmd + proc3.group('line') ) # /!\ Looking to save on upper space
         cnt = True
      elif proc2 :
         if not cnt:
            if cmd != '' : cmds.append(cmd)
            cmd = ''
         #?cmd = cmd.rstrip() + proc2.group('line')
         cmd = cleanSpaces( cmd + proc2.group('line') ) # /!\ Looking to save on upper space
         cnt = True
      elif proc4 :
         #?cmd = cmd.rstrip() + proc4.group('line')
         cmd = cleanSpaces( cmd + proc4.group('line') ) # /!\ Looking to save on upper space
         cnt = False
      else:
         if cnt:
            #?cmd = cmd.rstrip() + line
            cmd = cleanSpaces( cmd + line ) # /!\ Looking to save on upper space
         else:
            if cmd != '' : cmds.append(cmd)
            #?cmd = line
            cmd = cleanSpaces(line) # /!\ Looking to save on upper space
         cnt = False
   if cmd != '' : cmds.append(cmd)
   return cmds

"""
   In order for multiple lines of code to be interpreted, remove all
   comments form these, whether they include f77 or f09 comments --
   Return the command lines (without empty lines)
"""
def delComments(lines):
   # ~~ Also removes end lines and sets to UPPERCASE ~~~~~~~~~~~~~~~
   cmds = []
   for line in lines :
      line = cleanQuotes(line).rstrip().upper()
      proc1 = re.match(f77continu2,line) # no strip here
      proc = re.match(f77comment,line)   # no strip here
      if proc and not proc1 :
         cmd = ''
      else :
         while 1:
            cmd = line
            proc = re.match(f90comment,cmd)
            if proc: line = proc.group('line').rstrip()
            if cmd == line: break
      if cmd != '' :
         proc = re.match(emptyline,cmd)
         if not proc:
            cmds.append(cmd.replace('\n','').replace('\r',''))
   return cmds

"""
   Print to file the the scanning processes as an XML tree.

"""
def putScanContent(cfg,fileName,wcw):

   # ~~ High level root ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xmlIndents = 0
   xmlSources = ['<'+path.basename(cfg['root'])+'>']

   for mod in wcw:
      # ~~ For each module ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      xmlIndents += 3
      xmlSources.append(' '*xmlIndents+'<'+mod+' path="'+wcw[mod]['path']+'" >')

      for name in wcw[mod]:
         if name == 'path': continue
         whoi = wcw[mod][name]
         # ~~ For each file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         xmlIndents += 3
         xmlSources.append(' '*xmlIndents+'<'+name+' type="'+whoi['type']+'" file="'+whoi['file']+'">')

         # ~~> uses
         if whoi['uses'] != {}:
            xmlIndents += 3
            xml = ' '*xmlIndents+'<uses'
            for u in whoi['uses']: xml += ' '+u+'="'+','.join(whoi['uses'][u])+'"'
            xml += '/>'
            xmlSources.append(xml)
            xmlIndents -= 3
         # ~~> declarations
         if whoi['vars']['dec'] != [] or whoi['vars']['xtn'] != [] or whoi['args'] != []:
            xmlIndents += 3
            xml = ' '*xmlIndents+'<vars'
            if whoi['args'] != []: xml += ' args="'+','.join(whoi['args'])+'"'
            if whoi['vars']['dec'] != []: xml += ' dec="'+','.join(whoi['vars']['dec'])+'"'
            if whoi['vars']['xtn'] != []: xml += ' xtn="'+','.join(whoi['vars']['xtn'])+'"'
            xml += '/>'
            xmlSources.append(xml)
            xmlIndents -= 3
         # ~~> calls
         if whoi['calls'] != {}:
            xmlIndents += 3
            xml = ' '*xmlIndents+'<calls'
            for u in whoi['calls']: xml += ' '+u+'="'+str(len(whoi['calls'][u]))+'x"'
            xml += '/>'
            xmlSources.append(xml)
            xmlIndents -= 3
         # ~~> calls
         if whoi['called'] != {}:
            xmlIndents += 3
            xml = ' '*xmlIndents+'<called '+','.join(whoi['called'])+' />'
            xmlSources.append(xml)
            xmlIndents -= 3

         # ~~> function / unknown
         if whoi['functions'] != []:
            xmlIndents += 3
            xml = ' '*xmlIndents+'<unknowns'
            if whoi['functions'] != []: xml += ' functions="'+','.join(whoi['functions'])+'"'
            xml += '/>'
            xmlSources.append(xml)
            xmlIndents -= 3

         # ~~> contains
         if whoi['contains'] != []:
            xmlIndents += 3
            xmlSources.append(' '*xmlIndents+'<contains>')

            for u in whoi['contains']:
               xmlIndents += 3
               xmlSources.append(' '*xmlIndents+'<'+u+' />')
               xmlIndents -= 3
#                  xmlIndents += 3
#                  xmlSources.append(' '*xmlIndents+'<'+cname+' type="'+cw[0]+'">')
#                  if cw[0] == 'S': sbt = addToList(sbt,cname,whoi['libname'])# subroutine
#                  if cw[0] == 'F': fct = addToList(fct,cname,whoi['libname'])# function
#                  if whoc['uses'] != {}:
#                     xml = ' '*xmlIndents+'<uses'
#                     for k in whoc['uses']:
#                        for v in whoc['uses'][k]: addToList(whoi['uses'],k,v)
#                     #print whoc['uses']
#                  if whoc['vars'] != {} or whoc['args'] != []:
#                     xmlIndents += 3
#                     xml = ' '*xmlIndents+'<vars'
#                     if whoc['args'] != []: xml += ' args="'+','.join(whoc['args'])+'"'
#                     for k in whoc['vars']:
#                        if k != 'cmn':
#                           for v in whoc['vars'][k]: addToList(whoi['vars'],k,v)
#                           if whoc['vars'][k] != []: xml += ' '+k+'="'+','.join(whoc['vars'][k])+'"'
#                     xml += '/>'
#                     if '<vars/>' not in xml: xmlSources.append(xml)
#                     xmlIndents -= 3
#                  for k in deepcopy(whoc['calls']):
#                     if k in whoi['vars']['xtn']: whoc['calls'].pop(k)
#                     elif k in whoi['contains'] or k in whoi['calls']: whoc['calls'].pop(k)
#                     else: whoi['calls'].update({k:whoc['calls'][k]})
#                  if whoc['calls'] != {}:
#                     xmlIndents += 3
#                     xml = ' '*xmlIndents+'<calls'
#                     for k in whoc['calls']:
#                        for v in whoc['calls'][k]: addToList(whoi['calls'],k,v)
#                        xml += ' '+k+'="'+str(len(whoc['calls'][k]))+'x"'
#                     xml += '/>'
#                     xmlSources.append(xml)
#                     xmlIndents -= 3
#                  for k in deepcopy(whoc['functions']):
#                     if k in whoi['args'] or k in whoi['vars']['dec'] or k in whoi['vars']['xtn']: whoc['functions'].remove(k)
#                     elif k in whoi['contains'] or k in whoi['functions']: whoc['functions'].remove(k)
#                     else: whoi['functions'].append(k)
#                  if whoc['functions'] != []:
#                     xmlIndents += 3
#                     xml = ' '*xmlIndents+'<unknowns'
#                     xml += ' functions="'+','.join(whoc['functions'])+'"'
#                     xml += '/>'
#                     xmlSources.append(xml)
#                     xmlIndents -= 3
#
#                  xmlSources.append(' '*xmlIndents+'</'+cname+'>')
#                  xmlIndents -= 3

            xmlSources.append(' '*xmlIndents+'</contains>')
            xmlIndents -= 3

         xmlSources.append(' '*xmlIndents+'</'+name+'>')
         xmlIndents -= 3

      xmlSources.append(' '*xmlIndents+'</'+mod+'>')
      xmlIndents -= 3

   xmlSources.append('</'+path.basename(cfg['root'])+'>')
   putFileContent(fileName,xmlSources)

   return

def refactorSources(subset,cmdf,BYPASS):
   genmod = cmdf['general']['module']
   name = cmdf['general']['name'].split('.')[0]
   #print 'Refactoring tree top: ',genmod
   # ~~> scanning main names
   refnames = []
   numfiles = 0
   for mod in cmdf:
      if mod == 'general': continue
      numfiles += len(cmdf[mod]['files'])
      for fle in cmdf[mod]['files']:
         genpath = path.join(cmdf[mod]['path'],'.'+genmod)
         refnames.append( getPrincipalWrapNames(path.join(genpath,fle))[0][1] )
   # ~~> simplifying subset
   sub = []
   for s in subset: sub.append(path.basename(s[0]))
   # ~~> replacing main names
   ibar = 0; pbar = ProgressBar(maxval=numfiles).start()
   for mod in cmdf:
      if mod == 'general': continue
      for fle in cmdf[mod]['files']:
         ibar = ibar + 1; pbar.update(ibar)
         if fle not in sub: continue
         genpath = path.join(cmdf[mod]['path'],'.'+genmod)
         # read the file
         F = open(path.join(genpath,fle),'r')
         srci = ''.join(list(F))
         F.close()
         # refactor the file
         for refname in refnames: srci = refactorName(srci,refname,refname+'_'+name.upper())
         # write the file
         #print '  ... completed ',path.join(genpath,fle)
         F = open(path.join(genpath,fle),'wb')
         F.write(srci)
         F.close()
   pbar.finish()

def scanSources(cfgdir,cfg,BYPASS):
   fic = {}; mdl = {}; sbt = {}; fct = {}; prg = {}; top = {}; odd = {}; wcw = {}

   # ~~ Looking at each file individually ~~~~~~~~~~~~~~~~~~~~~~~~~~
   # TODO: do this in parallel
   for mod in cfg['MODULES'].keys():

      # Dealing with only one set of files
      #  by skipping the duplicate tagged files (active and passive)
      if mod in cfg['ADDONES'] or mod in cfg['ODDONES']: continue
      if 'files' not in cfg['MODULES'][mod].keys(): continue

      # Skipping masacaret module
      if mod == 'mascaret': continue

      wcw.update({mod:{'path':cfg['MODULES'][mod]['path']}})
      fic.update({mod:{}})
      # ~~ Scans the sources that are relevant to the model ~~~~~~~~
      #SrcDir = path.join(cfg['MODULES'][mod]['path'],'sources')     # assumes the sources are under ./sources
      SrcDir = cfg['MODULES'][mod]['path']                           # not anymore

      # In case of subdirectories loop on the subdirectories as well
      FileList = cfg['MODULES'][mod]['files']
      if len(FileList) == 0:
         print '... found an empty module: ' + mod
         sys.exit(1)
      ODir = path.join(cfg['MODULES'][mod]['path'],cfgdir)

      print '... now scanning ', path.basename(cfg['MODULES'][mod]['path'])

      ibar = 0; pbar = ProgressBar(maxval=len(FileList)).start()
      #/!\ if you need to print within this loop, you now need to use
      #    pbar.write(text,ibar) so the progress bar moves along
      for File in FileList :
         ibar = ibar + 1; pbar.update(ibar)
         File = path.join(SrcDir,File)

         if not mod in fic: fic.update({mod:{}})
         fic[mod].update({File:[]})
         who = { 'path':SrcDir, \
            'file':File.replace(SrcDir+sep,'').replace(sep,'|'), \
            'libname':mod,   \
            'type':'',       \
            'name':'',       \
            'args':[],       \
            'resu':'',       \
            'contains':[],   \
            'uses':{},       \
            'vars':{},       \
            'calls':{},      \
            'called':[],     \
            'functions':[],  \
            'rank':1,        \
            'time':0 }
         if path.isdir(ODir) :
            if cfg['COMPILER']['REBUILD'] == 2:
               who['time'] = 0
            else:
               who['time'] = isNewer(File,path.join(ODir,path.splitext(path.basename(File))[0] + cfg['SYSTEM']['sfx_obj'].lower()))

         SrcF = open(File,'r')
         if path.splitext(who['file'])[1].lower() in ['.f90','.f95']:
            flines = delContinuedsF90(delComments(SrcF))        # Strips the F90+ commented lines
         else:
            flines = delContinuedsF77(delComments(SrcF))        # Strips the F77 commented lines
         SrcF.close()                                           # and deletes the continuation characters

         core = flines
         found = False
         while core != [] and not found:                             # ignores what might be in the file after the main program

            # ~~ Parse Main Structure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            code,w,face,ctns,core = parsePrincipalWrap(core)
            name,whoi,rest = parsePrincipalMain(code,who,w[0],w[1],w[2],w[3])
            if w[0] == 'P': top = addToList(top,name,whoi['libname'])
            # others
            if w[0] == 'P': prg = addToList(prg,name,whoi['libname'])# main program
            if w[0] == 'P': found = BYPASS
            if w[0] == 'S': sbt = addToList(sbt,name,whoi['libname'])# subroutine
            if w[0] == 'M': mdl = addToList(mdl,name,whoi['libname'])# module
            if w[0] == 'F': fct = addToList(fct,name,whoi['libname'])# function
            fic[mod][File].append(name)
            # ~~> you may not need this afterall or do you ?
            while face != []:
               fcode,fw,ff,ft,face = parsePrincipalWrap(face)
               fname,whof,rest = parsePrincipalMain(fcode,who,fw[0],fw[1],fw[2],fw[3])
               for k in whof['uses']:
                  for v in whof['uses'][k]: addToList(whoi['uses'],k,v)
            while ctns != []:                                      # contains fcts & subs
               ccode,cw,cf,ct,ctns = parsePrincipalWrap(ctns)
               cname,whoc,rest = parsePrincipalMain(ccode,who,cw[0],cw[1],cw[2],cw[3])
               whoi['contains'].append(cname)
               # ~~> not unkown anymore
               if cname in whoi['functions']: whoi['functions'].remove(cname)
               if cname in whoi['calls']: whoi['calls'].pop(cname)
               if cw[0] == 'S': sbt = addToList(sbt,cname,whoi['libname'])# subroutine
               if cw[0] == 'F': fct = addToList(fct,cname,whoi['libname'])# function
               # ~~> transfer global context to identify unknowns
               for k in whoc['uses']:
                  for v in whoc['uses'][k]: addToList(whoi['uses'],k,v)
               #for k in whoc['vars']:
               #   if k != 'cmn':
               #      for v in whoc['vars'][k]: addToList(whoi['vars'],k,v)
               for k in deepcopy(whoc['calls']):
                  if k in whoi['vars']['xtn']: whoc['calls'].pop(k)
                  elif k in whoi['contains'] or k in whoi['calls']: whoc['calls'].pop(k)
                  else: whoi['calls'].update({k:whoc['calls'][k]})
               for k in whoc['calls']:
                  for v in whoc['calls'][k]: addToList(whoi['calls'],k,v)
               for k in deepcopy(whoc['functions']):
                  for v in whoi['vars']['cmn']:
                     if k in v[1]: whoc['functions'].remove(k)
                  if k in whoc['functions']:
                     if k in whoi['args'] or k in whoi['vars']['dec'] or k in whoi['vars']['xtn']: whoc['functions'].remove(k)
                     elif k in whoi['contains'] or k in whoi['functions']: whoc['functions'].remove(k)
                     else: whoi['functions'].append(k)

            whoi['vars'].update({'use':{}})
            wcw[mod].update({name:whoi})         # ~~ All ~~~~~~~~~~

            # separating the top of the tree from the odd ones
            for m in cfg['ODDONES']:
               if path.basename(File) in cfg['ODDONES'][m]:
                  odd = addToList(odd,name,m+'.'+name.lower())
                  # the combination m+name is unique, and therefore allows you to specify the path
                  if m not in cfg['ADDONES']:
                     top = addToList(top,name,m+'.'+name.lower())
                     wcw.update({m+'.'+name.lower():{'path':cfg['MODULES'][mod]['path'],name:whoi}})
            # /!\ tree tops now also include some of the ODD ONES (if there not also amongst the ADD ONES)
            for m in cfg['ADDONES']:
               if path.basename(File) in cfg['ADDONES'][m]:
                  top = addToList(top,name,m+'.'+name.lower())
                  # the combination m+name is unique, and therefore allows you to specify the path
                  wcw.update({m+'.'+name.lower():{'path':cfg['MODULES'][mod]['path'],name:whoi}})

      pbar.finish()

   # ~~: wcw.keys()
   #   holds the list of all primary and secondary libraries
   #   where source files were found. For each library, the list of files
   #   scanned is provided by cfg['MODULES'][mod]['files']
   # ~~: wcw[mod].keys()
   #   holds the 'path' and well as the list of names of all subroutines
   #   / functions / programs / modules contained in that mod (uppercase names)
   # ~~: wcw[mod][name].keys()
   #   holds a split of the analysis of the content of 'name' with the following
   #   fields:
   #  ['functions', 'libname', 'name', 'vars', 'contains', 'args', 'rank',
   #   'resu', 'uses', 'calls', 'file', 'time', 'path', 'type', 'called']

   # ~~ Cross-referencing CALLS together ~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # For those CALLs stored in 'calls' but not part of the system:
   #   move them from 'calls' to 'function' (outsiders remain)
   #
   for mod in wcw.keys():
      for name in copy(wcw[mod].keys()):
         if name == 'path': continue
         who = wcw[mod][name]
         for s in copy(who['calls'].keys()):
            # separate items in the top tree by declaring then as external
            if s in top.keys() and s not in odd.keys():
               del wcw[mod][name]['calls'][s]
            # calls bot in the list of subroutines msut be functions (?)
            elif s not in sbt:
               del wcw[mod][name]['calls'][s]
               wcw[mod][name]['functions'].append(s)

   # ~~ Cross-referencing FUNCTIONS together ~~~~~~~~~~~~~~~~~~~~~~~
   for mod in wcw.keys():
      for name in wcw[mod].keys():
         if name == 'path': continue
         who = wcw[mod][name]
         f,u = sortFunctions(who['functions'],who['vars']['use'],wcw,mdl,who['uses'])
         # /!\ because this is a dico-list, updating who updates wcw
         who['functions'] = f
         who['vars']['use'].update(u)

   for mod in wcw:
      for name in wcw[mod]:
         if name == 'path': continue
         who = wcw[mod][name]
         for f in who['vars']['xtn']:
            if f not in who['functions']:
               if debug: print f,' declared but not used in ',who['name']
               who['functions'].append(f)
         for f in fct:
            while f in who['functions']:
               who['functions'].remove(f)
               if f not in top.keys() or f in odd.keys(): who['calls'].update({f:[['']]})

   # ~~ Sort out referencing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Fill-in the 'called' category
   for mod in wcw.keys():
      for name in wcw[mod].keys():
         if name == 'path': continue
         for call in wcw[mod][name]['calls']:
            if call in fct:
               for u in fct[call]:
                  if call in wcw[u]:
                     wcw[u][call]['called'].append(name)

   if debug:
      putScanContent(cfg,cfg['root']+sep+'scanTELEMAC.xml',wcw)

   return fic,mdl,sbt,fct,prg,top,odd,wcw

def sortFunctions(ifcts,iuses,list,mods,xuses):
   #found = False
   ofcts = []; ofcts.extend(ifcts)
   for d in ifcts:
      for u in xuses:
         if u not in mods:
            continue
         if d in list[mods[u][0]][u]['vars']['dec']:
            ofcts.remove(d)
            addToList(iuses,u,d)
            break
         if d in list[mods[u][0]][u]['vars']['als']:
            ofcts.remove(d)
            addToList(iuses,u,d)
            break
   ifcts = ofcts
   for u in xuses:
      if u in mods and ifcts != []:
         ifcts,iuses = sortFunctions(ifcts,iuses,list,mods,list[mods[u][0]][u]['uses'])

   return ifcts,iuses


# _____                _____________________________________________
# ____/ Coding Rules  /____________________________________________/
#
def scanEnunciation(name):

   # ~~ get file content ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   lines = getFileContent(name)

   # ~~ scan indents ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # grep -ER -n $2 '^(\ ){9}[^\ ]|^(\ ){7}[^\ ]|^(\ ){15}[^\ ]' $1 --include=*.[fF]
   indent =[]

   # ~~ scan comments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # grep -ER -n $2 '^[^!\n0-9#\ ]' $1 --include=*.[fF]
   check =[]
   rle_indent = re.compile(r'')
   rle_comment = re.compile(r'[!#]')
   #f77comment = re.compile(r'[C!#*]') #,re.I)

   # ~~ proposed file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   difFile = name+'.mdf'
   putFileContent(difFile,lines)

   # ~~ differentiation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> default options
   options = Values()
   options.unified = False
   options.ndiff = False
   options.html = True
   options.ablines = 3
   options.context = False
   # ~~> differentiate
   print '\n\nDifferenciating:\n    +> ' + name + '\nand +> ' + difFile + '\n'+'~'*72+'\n'
   # ~~> use of writelines because diff is a generator
   diff = diffTextFiles(name,difFile,options)
   # ~~> write differentiator
   if options.html:
      htmlFile = path.splitext(difFile)[0]+'.html'
      of = open(htmlFile,'wb')
      of.writelines( diff )
      of.close()
      print '\n      ~> produced: ',htmlFile,'\n'
   #elif options.unified:
   #   diffFile = path.splitext(difFile)[0]+'.diff'
   #   of = open(diffFile,'wb')
   #   of.writelines( diff )
   #   of.close()
   #   print '\n      ~> produced: ',diffFile,'\n'
   #else:
   #   sys.stdout.writelines( diff )
   sys.exit()

   return name

"""
echo '*****************'
echo 'Comments error'
echo '*****************'
wc -l comments.log
echo '*****************'
echo 'Continuation line error'
echo '*****************'
grep -ER -n $2 '^(\ ){5}[^\&\ ]' $1 --include=*.[fF] > continuation.log
wc -l continuation.log
echo '*****************'
echo 'Lowercase error'
echo '*****************'
grep -ER -n $2 '^[^!#\"'\'']*[azertyuiopqsdfghjklmnbvcxw]' $1 --include=*.[fF] > lowercas.log
wc -l lowercas.log
echo '*****************'
echo 'Line too long error'
echo '*****************'
grep -ER -n $2 '^[^!]{73}' $1 --include=*.[fF] > linetoolong.log
wc -l linetoolong.log
echo '*****************'
echo 'Invalid character error'
echo '*****************'
grep -PR -n $2 '\t|\r' $1 --include=*.[fF] > invalidchar.log
wc -l invalidchar.log
"""

# _____                 ____________________________________________
# ____/ DOXYGEN parse  /___________________________________________/
#

doxycomment = re.compile(r'!>(?P<doxy>.*)\s*\Z') #,re.I)
doxy_tag = re.compile(r'!>\s*?@(?P<tag>[\w\[,\]]+)(?P<after>.*)\s*\Z') #,re.I)
doxy_title = re.compile(r'\s+(?P<title>.*)\s*\Z') #,re.I)

"""
   Parse a list of entry lines, removing the lines that are
   understood to be part of a tag -- the lines are removed
   from the 'core' iteratively -- Return one complete tag
"""
def getNextDoxyTag(core):
   ltag = []; tag = ''; name = ''
   found = False
   while core != []:
      proc = re.match(doxy_tag,core[0])
      if proc:
         if not found:
            tag = proc.group('tag')
            proc = re.match(doxy_title,proc.group('after'))
            if proc: name = proc.group('title')
            found = True
         else: break
      if found: ltag.append(core[0])
      core.pop(0)

   return tag,name,ltag,core

"""
   Parse a list of entry lines, removing the doxygen blocks
   listing them out by tag names -- the doxygen blocks are removed
   from the 'core' iteratively -- Return the list of tags by blocks
"""
def parseDoxyTags(core):
   tags = []
   while 1:
      tname,title,field,core = getNextDoxyTag(core)
      if tname =='': break
      tags.append([tname,title,field])
   return tags

# _____                 ____________________________________________
# ____/ FORTRAN parse  /___________________________________________/
#

#?varcomment = re.compile(r'[C!#*]\s*?[|!]\s*?(?P<vars>[\s\w,()]*)(?P<inout>(|[|!->=<\s]*[|!]))(?P<after>(|[^|!]*))\s*[|!]?\s*\Z') #,re.I)
varcomment = re.compile(r'[C!#*]\s*?[|!]\s*?(?P<vars>[\s\w,()]*)(?P<inout>(|[|!->=<\s]*[|!]))(?P<after>(|[^|!]*))\s*[|!]?\s*\Z') #,re.I)

def parseFortHeader(core):
   docs = []; vrs = {}
   while 1:
      line = delComments([core[0]])
      if line == []:
         proc = re.match(varcomment,core[0])
         if proc:
            #print core[0].rstrip()
            if proc.group('vars').strip() != '':
               var = proc.group('vars').strip()
               val = proc.group('after').strip()
               ino = proc.group('inout').strip()
               if ino == '!!' or ino == '||': ino = '<>'
               #print val
               vrs.update({var:[ino,[val]]})
            elif proc.group('after').strip() != '':
               #print vrs
               vrs[var][1].append(proc.group('after').strip())
            #print '##'+proc.group('vars')+'##','@@'+proc.group('inout')+'@@','>>'+proc.group('after')+'<<'
            #print var,vrs[var]
         core.pop(0)
         continue
      line = line[0].rstrip()
      proc = re.match(pcl_title,line)
      if proc:
         typ = proc.group('type').strip()
         if ( typ != '' ):
            if not re.match(typ_name,typ): break
         docs.append(core[0].rstrip())
         core.pop(0)
         continue
      proc = re.match(use_title,line)
      proc = proc or re.match(implictNone,line)
      proc = proc or re.match(itf_title,line)
      proc = proc or re.match(def_title,line)
      proc = proc or re.match(cmn_title,line)
      proc = proc or re.match(typ_xport,line)
      proc = proc or re.match(inc_title,line)
      proc = proc or re.match(typ_title,line)
      proc = proc or re.match(itz_title,line)
      proc = proc or re.match(xtn_title,line)
      if proc: break
      #proc = re.match(f77comment,line)
      #if not proc:
      docs.append(core[0].rstrip())
      core.pop(0)

   return docs,vrs,core

def getPrincipalWrapNames(difFile):
   # Filter most unuseful
   SrcF = open(difFile,'r')
   if path.splitext(path.basename(difFile))[1].lower() in ['.f90','.f95']:
      flines = delContinuedsF90(delComments(SrcF))        # Strips the F90+ commented lines
   else:
      flines = delContinuedsF77(delComments(SrcF))        # Strips the F77 commented lines
   SrcF.close()                                           # and deletes the continuation characters
   # Identify main items
   pFiles = []
   while flines != []:
      code,w,face,ctns,flines = parsePrincipalWrap(flines)
      pFiles.append([w[0],w[1]])
   return pFiles

def filterPrincipalWrapNames(uNames,sFiles):
   oFiles = {}
   for sFile in sFiles:
      SrcF = open(sFile,'r')
      if path.splitext(path.basename(sFile))[1].lower() in ['.f90','.f95']:
         flines = delContinuedsF90(delComments(SrcF))
      else:
         flines = delContinuedsF77(delComments(SrcF))
      SrcF.close()
      code,w,face,ctns,flines = parsePrincipalWrap(flines)
      if w[1] in uNames: oFiles.update({w[1]:sFile})
   return oFiles

# Note that the spaces are kept in the 'after' text for possible formatting
doxytags = '(brief|note|warning|history|bug|code)'
doxycomment = re.compile(r'\s*!(?P<name>%s\b)(?P<after>.*?)\s*\Z'%(doxytags)) #,re.I)
doxycommentadd = re.compile(r"\s*!\+(?P<after>.*?)\s*\Z") #,re.I)

def parseDoxyHeader(body):
   # It is now assumed that doxytags could be part of the main
   # core of the Fortran.
   tags = []; tfound = False; tcount = -1
   core = []; core.extend(body); bcount = -1
   nfound = False
   # ~~ Parse Doxygen Tags ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   while core != []:
      line = core[0].rstrip()
      bcount = bcount + 1

      # ~~> parsing the tags
      proc = re.match(doxycomment,line)
      if proc:
         tcount = tcount + 1
         tags.extend([[]])
         tags[tcount].append(proc.group('name'))
         tags[tcount].append([proc.group('after')])
         tfound = True
         core.pop(0)
         body.pop(bcount)
         bcount = bcount - 1
         continue
      proc = re.match(doxycommentadd,line)
      if proc and tfound:
         tags[tcount][1].append(proc.group('after'))
         core.pop(0)
         body.pop(bcount)
         bcount = bcount - 1
         continue
      tfound = False

      # ~~> parsing the name of the program for later reference
      if not nfound:
         proc = re.match(pcl_title,line)
         if proc :
            proc = re.match(var_word,proc.group('after'))
            if proc : name = proc.group('word')
            nfound = True
      core.pop(0)

   return name,tags

"""
   Split a set of lines (content of a file) into a Doxygen header
   the definition of the FORTRAN entity and the text between the
   the definition oan the core of the FORTRAN entity -- The wrap
   iteratively goes through all included entities and sub-entities
   - lines contains the content of the file
   - icount is the number of entities included (functions, subroutines, etc.)
"""
def parseDoxyWrap(lines):
   core = []; core.extend(lines)
   wrap = []; wrap.extend([[]])
   blevel = 0; bcount = 0

   # ~~ Split the content of the file by blocks ~~~~~~~~~~~~~~~~~~~~
   while core != []:
      line = core[0].rstrip()
      wrap[bcount].append(line)
      core.pop(0)

      proc = re.match(pcl_close,line)
      if proc:
         blevel = blevel - 1
         if blevel == 0:
            bcount = bcount + 1
            wrap.extend([[]])
         continue
      proc = re.match(pcl_title,line)
      if proc:
         blevel = blevel + 1
         continue

   # ~~ Clean the wrap ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # remove the the last item of the wrap,
   # which is empty since it gets set just after the pcl_close
   ##>wrap.pop(len(wrap)-1)

   # ~~ Parse the DOXYGEN tags ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   doxy = []; name = ''
   for w in wrap:
      if w != []:
         n,d = parseDoxyHeader(w)
         if name == '': name = n   #/!\ here takes the first name of the file
         doxy.extend([(name,n,d,w)])

   return doxy

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n'+'~'*72+'\n'
   USETELCFG = ''
   PWD = path.dirname(path.dirname(path.dirname(path.dirname(sys.argv[0]))))
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   SYSTELCFG = 'systel.cfg'
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Testing ...
      '''))
   parser.add_argument( "args",nargs='*' )
   parser.add_argument(\
      "-c", "--configname",dest="configName",default=USETELCFG,
      help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_argument(\
      "-f", "--configfile",dest="configFile",default=SYSTELCFG,
      help="specify configuration file, default is systel.cfg" )
   parser.add_argument(\
      "-r", "--rootdir",dest="rootDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_argument(\
      "-m", "--modules",dest="modules",default='',
      help="specify the list modules, default is taken from config file" )
   parser.add_argument(\
      "-x","--noscan",action="store_true",dest="noscan",default=False,
      help="will bypass the scan of sources by checking only the cmdf files present" )
   parser.add_argument(\
      "--context",action="store_true",dest="context",default=False,
      help='Produce a context format diff (default)')
   parser.add_argument(\
      "--unified",action="store_true",dest="unified",default=False,
      help='Produce a unified format diff')
   parser.add_argument(\
      "--html",action="store_true",dest="html",default=False,
      help='Produce HTML side by side diff (can use -c and -l in conjunction)')
   parser.add_argument(\
      "--ndiff",action="store_true",dest="ndiff",default=False,
      help='Produce a ndiff format diff')
   parser.add_argument(\
      "--ablines",dest="ablines",type=int,default=3,
      help='Set number of before/after context lines (default 3)')
   options = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         _, _, filenames = walk(dircfg).next()
         for fle in filenames :
            head,tail = path.splitext(fle)
            if tail == '.cfg' : print '    +> ',fle
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)
   cfgname = cfgs.iterkeys().next()
   if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
   if options.rootDir != '': cfgs[cfgname]['root'] = options.rootDir
   if options.modules != '': cfgs[cfgname]['modules'] = options.modules
   cfg = parseConfig_CompileTELEMAC(cfgs[cfgname],False,False)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if len(options.args) < 2:
      print '\nAn action name and at least one file are required\n'
      parser.print_help()
      sys.exit(1)
   codeName = options.args[0]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Comparison with standard PRINCI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if codeName == 'princi':
      print '\n\nScanning Fortran\n'+'~'*72+'\n'

      if len(options.args[1:]) >= 1:
         difFile = options.args[1]
         # ~~> Scans the first user PRINCI file
         print '      ~> scanning your PRINCI file: ',path.basename(difFile)
         pFiles = getPrincipalWrapNames(difFile)
         if pFiles == []:
            print '         ... nothing !'
            print '\n... This does not seem a Fortran file I can read.\n'
            sys.exit(1)
         else:
            print '        +> found:'
            for oType,oFile in pFiles: print '           - ',oFile

      if len(options.args[1:]) == 1: # if only one PRINCI ...
         # ~~> Get and store original version of files
         print '      ~> scanning the entire system: '
         oFiles = {}
         for mod in cfg['MODULES']: oFiles.update( filterPrincipalWrapNames( pFiles,
            getTheseFiles(cfg['MODULES'][mod]['path'],['.f','.f90','.F','.F90']) ) )
         if oFiles == {}:
            print '         ... nothing !'
            print '\n... Your program does not seem to be related to the system in this configuration.\n'
            sys.exit(1)
         else:
            print '        +> found:'
            for oFile in oFiles: print '           - ',path.basename(oFile)
         oriFile = path.splitext(difFile)[0]+'.original'+path.splitext(difFile)[1]
         putFileContent(oriFile,[])
         for p in pFiles:
            if p in oFiles:
               addFileContent(oriFile,getFileContent(oFiles[p]))

      elif len(options.args[1:]) == 2: # case of two PRINCI files ...
         oriFile = options.args[2]

      # ~~> Execute diff
      print '\n\nDifferenciating:\n    +> ' + oriFile + '\nand +> ' + difFile + '\n'+'~'*72+'\n'
      # ~~> use of writelines because diff is a generator
      diff = diffTextFiles(oriFile,difFile,options)
      remove(oriFile)
      if options.html:
         htmlFile = path.splitext(difFile)[0]+'.html'
         of = open(htmlFile,'wb')
         of.writelines( diff )
         of.close()
         print '\n      ~> produced: ',htmlFile,'\n'
      elif options.unified:
         diffFile = path.splitext(difFile)[0]+'.diff'
         of = open(diffFile,'wb')
         of.writelines( diff )
         of.close()
         print '\n      ~> produced: ',diffFile,'\n'
      else:
         sys.stdout.writelines( diff )


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Case of UNKNOWN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print '\nDo not know what to do with this code name: ',codeName
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)
