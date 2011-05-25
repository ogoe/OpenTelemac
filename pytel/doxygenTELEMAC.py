"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
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
from config import OptionParser,parseConfigFile, parseConfig_DoxygenTELEMAC
from parserFortran import scanSources, parseDoxyWrap, parseVars
from os import path, chdir, mkdir, remove, system
from utils import getFileContent, putFileContent, createDirectories
import sys

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def createDOXYGEN(ilines,lname,icount,list):

   # ~~ Parsers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   olines = []
   for name,doxy,docs,body,vars in parseDoxyWrap(ilines,icount):
      who = list[lname][name]
      #print name, lname, ifile
      #if name != 'BIEF_VALIDA': return

      # ~~ Final Header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # replace <br> by <br><br>

      #olines.extend(['C'+71*'#'+'\n'])
      # ~~ General
      #if doxy['file'] != []:
      #   olines.extend(doxy['file'])
      #line = '!> @code\n'
      #if who['type'] == 'S': line = line + '!>             SUBROUTINE\n'
      #if who['type'] == 'M': line = line + '!>             MODULE\n'
      #if who['type'] == 'P': line = line + '!>             PROGRAM\n'
      #if who['type'] == 'F': line = line + '!>             FUNCTION\n'
      #line = line + '!>      NAME : ' + who['name'] + '\n' \
      #              '!>    MODULE : ' + who['libname'] + '\n'
      #line = line + banner('!> ',who['libname'],'\n')
      #line = line + '!> @endcode\n'
      #olines.extend([line])

      # ~~ Module
      #olines.extend(['!>  @par Module: '+who['libname'+'\n']])

      # ~~ Function
      if doxy['fcts'] != []:
         olines.extend(['C'+71*'~'+'\n'])
         #~~~~~~~~~>
         #> changes function into brief
         doxy['fcts'][0] = doxy['fcts'][0].replace('@function','@brief  ') # makes use of Tooltips, but no title => Try formating with colour
         #> replace line break by its HTML tag (but note in the following @code part)
         count = 1
         while count < len(doxy['fcts']) - 1:
            if doxy['fcts'][count] == '!>':
               doxy['fcts'].pop(count)
               doxy['fcts'][count-1] = doxy['fcts'][count-1] + '<br>'
               count = count - 1
            count = count + 1
            if doxy['fcts'][count][0:9] == '!>  @code': break
         #~~~~~~~~~<
         olines.extend(doxy['fcts'])
         olines.extend(['C'+71*'~'+'\n'])

      # ~~ User Defined Information
      if doxy['note'] != [] or doxy['warn'] != [] or doxy['refs'] != [] or doxy['code'] != []:
         olines.extend(['C'+71*'~'+'\n'])
         if doxy['note'] != []:
            #doxy['note'][1].replace('!>','!><br>')
            olines.extend(doxy['note'])
         if doxy['bugs'] != []:
            #doxy['bugs'][1].replace('!>','!><br>')
            olines.extend(doxy['bugs'])
         if doxy['warn'] != []:
            #doxy['warn'][1].replace('!>','!><br>')
            olines.extend(doxy['warn'])
         if doxy['refs'] != []:
            #doxy['refs'][1].replace('!>','!><br>')
            olines.extend(doxy['refs'])
         if doxy['code'] != []:
            olines.extend(doxy['code'])
         olines.extend(['C'+71*'~'+'\n'])

      # ~~ Final Uses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['uses'] != {}:
         line = '!>  @par Use(s)\n!><br>' + ', '.join(sorted(who['uses'].keys()))
         olines.extend([line])

      # ~~ Final Variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['args'] != [] or who['vars']['use'] != {} or who['vars']['cmn'] != [] or who['vars']['dec'] != [] or who['vars']['als'] != []:
         line = '!>  @par Variable(s)\n!>  <br><table>'
         olines.extend([line])

         # ~~ Arguments
         if who['args'] != []:
            line = '!>     <tr><th> Argument(s)\n!>    </th><td> ' + ', '.join(sorted(who['args'])) + '\n!>   </td></tr>'
            olines.extend([line])
         # ~~ Uses
         if who['vars']['use'] != {}:
            line = '!>     <tr><th> Use(s)\n!>    </th><td>'
            for u in who['vars']['use']:
               uv = []
               for v in who['vars']['use'][u]:
                  uv.append('\n!> @link ' + u + '::' + v + ' ' + v + '@endlink')
               line = line + '<hr>\n!> ' + u + ' :<br>' + ', '.join(sorted(uv))
            line = line.replace('<td><hr>\n','<td>\n') + '\n!>   </td></tr>'
            olines.extend([line])

         # ~~ Commons
         if who['vars']['cmn'] != []:
            line = '!>     <tr><th> Common(s)\n!>    </th><td>'
            for u in who['vars']['cmn']:
               line = line + '<hr>\n!> ' + u[0] + ' : ' + ', '.join(u[1])
            line = line.replace('<td><hr>\n','<td>\n') + '\n!>   </td></tr>'
            olines.extend([line])

         # ~~ Declars
         if who['vars']['dec'] != []:
            line = '!>     <tr><th> Internal(s)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['dec'])) + '\n!>   </td></tr>'
            olines.extend([line])

         # ~~ Externals
         #if who['vars']['xtn'] != []:
         #   line = '!>     <tr><th> External(s)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['xtn'])) + '\n!>   </td></tr>'
         #   olines.extend([line])

         # ~~ Aliases
         if who['vars']['als'] != []:
            line = '!>     <tr><th> Alias(es)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['als'])) + '\n!>   </td></tr>'
            olines.extend([line])

         line = '!>     </table>\n'
         olines.extend([line])

      # ~~ Final Calls ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['calls'] != {} or who['functions'] != []:
         line = '!>  @par Call(s)\n!>  <br><table>'
         olines.extend([line])
         if who['calls'] != {}:
            line = '!>     <tr><th> Known(s)\n!>    </th><td> ' + '(), '.join(sorted(who['calls'].keys())) + '()\n!>   </td></tr>'
            olines.extend([line])
         if who['functions'] != []:
            line = '!>     <tr><th> Unknown(s)\n!>    </th><td> ' + ', '.join(sorted(who['functions'])) + '\n!>   </td></tr>'
            olines.extend([line])
         line = '!>     </table>\n'
         olines.extend([line])

      # ~~ Final Called ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['called'] != []:
         line = '!>  @par Called by\n!><br>' + '(), '.join(sorted(who['called'])) + '()\n'
         olines.extend([line])

      #if doxy['para'] != []:
         #olines.extend(['C'+71*'='+'\n'])
         #olines.extend(doxy['para'])
      #if doxy['resu'] != []:
         #olines.extend(['C'+71*'='+'\n'])
         #olines.extend(doxy['resu'])
      #olines.extend(['C'+71*'='+'\n'])
      #olines.extend(head['hist'])

      # ~~ Final History ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if doxy['hist'] != []:
         olines.extend(['C'+71*'~'+'\n'])
         found = False
         for line in doxy['hist']:
            if line == '!>  </tr>':
         #~~~~~~~~~>
               if not found:
                  olines.append('!>  @par Development history')
                  olines.append('!>   <br><table>')
                  olines.append('!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>')
                  olines.append('!>  <tr><td><center> 6.0                                       </center>')
                  olines.append('!>    </td><td> 21/08/2010')
                  olines.append('!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)')
                  olines.append('!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources')
                  olines.append('!>   </td></tr>')
                  olines.append('!>  <tr><td><center> 6.0                                       </center>')
                  olines.append('!>    </td><td> 13/07/2010')
                  olines.append('!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)')
                  olines.append('!>    </td><td> Translation of French comments within the FORTRAN sources into English comments')
                  olines.append('!>   </td></tr>')
                  found = True
         #~~~~~~~~~<
            else:
               if found: olines.append(line)
         olines.extend(['C'+71*'~'+'\n'])
         #olines.extend(doxy['hist'])

      # ~~ Extended Description
      if vars.keys() != '' or who['args'] != '':
         for a in who['args']:
            found = False
            for v in vars.keys():
               if a in parseVars(v): found = True
            if not found: vars.update({a:['',['']]})
         olines.extend(['!>  @par Details of primary variable(s)\n!>  <br><table>'])
         #olines.extend(['!>  <p><div id="dynsection-1" onclick="return toggleVisibility(this)" class="dynheader closed" style="cursor:pointer;"> \
         #   \n!>  <img id="dynsection-1-trigger" src="closed.png"> Here are details of primary variables:</div> \
         #   \n!>  <div id="dynsection-1-summary" class="dynsummary" style="display:block;"> \
         #   \n!>  </div> \
         #   \n!>  <div id="dynsection-1-content" class="dyncontent" style="display:none;">'])
         olines.extend(['!>\n!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>'])
         for v in sorted(vars.keys()):
            #print v,vars[v][1]
            ino = ['-','-','-']
            if '<' in vars[v][0]: ino[0] = '<'
            if '>' in vars[v][0]: ino[2] = '>'
            line = '!>          <tr><td>' + v + '\n!></td><td>' + ''.join(ino) + '</td><td>' + '\n!>                  '.join(vars[v][1]) + '\n!>    </td></tr>'
            olines.extend([line])
         olines.extend(['!>     </table>'])
         #olines.extend(['!>  </div></p>'])


      # ~~ Final Tempates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      olines.extend(['C\nC'+71*'#'+'\nC'])
      #~~~~~~~~~>
      for count in range(len(docs)):
         proc = re.match(f77continu2,docs[count])
         if proc: docs[count] = '     &'+docs[count][6:]
      #~~~~~~~~~<
      olines.extend(docs)

      # ~~ Final Tempates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      olines.extend(['C\nC'+71*'~'])
      for v in sorted(vars.keys()):
         name = v + '             '
         ino = ['-','-','-']
         if '<' in vars[v][0]: ino[0] = '<'
         if '>' in vars[v][0]: ino[2] = '>'
         line = 'C| ' + name[0:15] + '|' + ''.join(ino) + '| ' + '\nC|                |   | '.join(vars[v][1])
         olines.extend([line])
      olines.extend(['C'+71*'~'+'\nC'])

      # ~~ Final Tempates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~>
      for count in range(len(body)):
         proc = re.match(f77continu2,body[count])
         if proc: body[count] = '     &'+body[count][6:]
      #~~~~~~~~~<
      olines.extend(body)
      olines.extend(['C\nC'+71*'#'+'\nC'])

   return olines

def updateDOXYGEN(ilines,lname,icount,list):

   # ~~ Parsers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   olines = []
   for name,doxy,docs,body,vars in parseDoxyWrap(ilines,icount):
      who = list[lname][name]
      #print name, lname, ifile

      # ~~ Function
      if doxy['fcts'] != []:
         olines.extend(['C'+71*'~'+'\n'])
         #~~~~~~~~~>
         #> replace line break by its HTML tag (but note in the following @code part)
         count = 1
         while count < len(doxy['fcts']) - 1:
            if doxy['fcts'][count] == '!>':
               doxy['fcts'].pop(count)
               doxy['fcts'][count-1] = doxy['fcts'][count-1] + '<br>'
               count = count - 1
            count = count + 1
            if doxy['fcts'][count][0:9] == '!>  @code': break
         #~~~~~~~~~<
         # /!\ you probably need to account for several @code / @endcode and in betweens
         olines.extend(doxy['fcts'])
         olines.extend(['C'+71*'~'+'\n'])

      # ~~ User Defined Information
      if doxy['note'] != [] or doxy['warn'] != [] or doxy['refs'] != [] or doxy['code'] != []:
         olines.extend(['C'+71*'~'+'\n'])
         if doxy['note'] != []: olines.extend(doxy['note'])
         if doxy['bugs'] != []: olines.extend(doxy['bugs'])
         if doxy['warn'] != []: olines.extend(doxy['warn'])
         if doxy['refs'] != []: olines.extend(doxy['refs'])
         if doxy['code'] != []: olines.extend(doxy['code'])
         olines.extend(['C'+71*'~'+'\n'])

      # ~~ Final Uses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['uses'] != {}:
         line = '!>  @par Use(s)\n!><br>' + ', '.join(sorted(who['uses'].keys()))
         olines.extend([line])

      # ~~ Final Variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['args'] != [] or who['vars']['use'] != {} or who['vars']['cmn'] != [] or who['vars']['dec'] != [] or who['vars']['als'] != []:
         line = '!>  @par Variable(s)\n!>  <br><table>'
         olines.extend([line])

         # ~~ Arguments
         if who['args'] != []:
            line = '!>     <tr><th> Argument(s)\n!>    </th><td> ' + ', '.join(sorted(who['args'])) + '\n!>   </td></tr>'
            olines.extend([line])
         # ~~ Uses
         if who['vars']['use'] != {}:
            line = '!>     <tr><th> Use(s)\n!>    </th><td>'
            for u in who['vars']['use']:
               uv = []
               for v in who['vars']['use'][u]:
                  uv.append('\n!> @link ' + u + '::' + v + ' ' + v + '@endlink')
               line = line + '<hr>\n!> ' + u + ' :<br>' + ', '.join(sorted(uv))
            line = line.replace('<td><hr>\n','<td>\n') + '\n!>   </td></tr>'
            olines.extend([line])

         # ~~ Commons
         if who['vars']['cmn'] != []:
            line = '!>     <tr><th> Common(s)\n!>    </th><td>'
            for u in who['vars']['cmn']:
               line = line + '<hr>\n!> ' + u[0] + ' : ' + ', '.join(u[1])
            line = line.replace('<td><hr>\n','<td>\n') + '\n!>   </td></tr>'
            olines.extend([line])

         # ~~ Declars
         if who['vars']['dec'] != []:
            line = '!>     <tr><th> Internal(s)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['dec'])) + '\n!>   </td></tr>'
            olines.extend([line])

         # ~~ Externals
         #if who['vars']['xtn'] != []:
         #   line = '!>     <tr><th> External(s)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['xtn'])) + '\n!>   </td></tr>'
         #   olines.extend([line])

         # ~~ Aliases
         if who['vars']['als'] != []:
            line = '!>     <tr><th> Alias(es)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['als'])) + '\n!>   </td></tr>'
            olines.extend([line])

         line = '!>     </table>\n'
         olines.extend([line])

      # ~~ Final Calls ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['calls'] != {} or who['functions'] != []:
         line = '!>  @par Call(s)\n!>  <br><table>'
         olines.extend([line])
         if who['calls'] != {}:
            line = '!>     <tr><th> Known(s)\n!>    </th><td> ' + '(), '.join(sorted(who['calls'].keys())) + '()\n!>   </td></tr>'
            olines.extend([line])
         if who['functions'] != []:
            line = '!>     <tr><th> Unknown(s)\n!>    </th><td> ' + ', '.join(sorted(who['functions'])) + '\n!>   </td></tr>'
            olines.extend([line])
         line = '!>     </table>\n'
         olines.extend([line])

      # ~~ Final Called ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['called'] != []:
         line = '!>  @par Called by\n!><br>' + '(), '.join(sorted(who['called'])) + '()\n'
         olines.extend([line])

      # ~~ Final History ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if doxy['hist'] != []:
         olines.extend(['C'+71*'~'+'\n'])
         found = False
         for line in doxy['hist']:
            if line == '!>  </tr>':
         #~~~~~~~~~>
               if not found:
                  olines.append('!>  @par Development history')
                  olines.append('!>   <br><table>')
                  olines.append('!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>')
                  olines.append('!>  <tr><td><center> 6.0                                       </center>')
                  olines.append('!>    </td><td> 21/08/2010')
                  olines.append('!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)')
                  olines.append('!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources')
                  olines.append('!>   </td></tr>')
                  olines.append('!>  <tr><td><center> 6.0                                       </center>')
                  olines.append('!>    </td><td> 13/07/2010')
                  olines.append('!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)')
                  olines.append('!>    </td><td> Translation of French comments within the FORTRAN sources into English comments')
                  olines.append('!>   </td></tr>')
                  found = True
         #~~~~~~~~~<
            else:
               if found: olines.append(line)
         olines.extend(['C'+71*'~'+'\n'])
         #olines.extend(doxy['hist'])

      # ~~ Extended Description
      if vars.keys() != '' or who['args'] != '':
         for a in who['args']:
            found = False
            for v in vars.keys():
               if a in parseVars(v): found = True
            if not found: vars.update({a:['',['']]})
         olines.extend(['!>  @par Details of primary variable(s)\n!>  <br><table>'])
         #olines.extend(['!>  <p><div id="dynsection-1" onclick="return toggleVisibility(this)" class="dynheader closed" style="cursor:pointer;"> \
         #   \n!>  <img id="dynsection-1-trigger" src="closed.png"> Here are details of primary variables:</div> \
         #   \n!>  <div id="dynsection-1-summary" class="dynsummary" style="display:block;"> \
         #   \n!>  </div> \
         #   \n!>  <div id="dynsection-1-content" class="dyncontent" style="display:none;">'])
         olines.extend(['!>\n!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>'])
         for v in sorted(vars.keys()):
            #print v,vars[v][1]
            ino = ['-','-','-']
            if '<' in vars[v][0]: ino[0] = '<'
            if '>' in vars[v][0]: ino[2] = '>'
            line = '!>          <tr><td>' + v + '\n!></td><td>' + ''.join(ino) + '</td><td>' + '\n!>                  '.join(vars[v][1]) + '\n!>    </td></tr>'
            olines.extend([line])
         olines.extend(['!>     </table>'])
         #olines.extend(['!>  </div></p>'])


      # ~~ Final Tempates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      olines.extend(['C\nC'+71*'#'+'\nC'])
      #~~~~~~~~~>
      for count in range(len(docs)):
         proc = re.match(f77continu2,docs[count])
         if proc: docs[count] = '     &'+docs[count][6:]
      #~~~~~~~~~<
      olines.extend(docs)

      # ~~ Final Tempates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      olines.extend(['C\nC'+71*'~'])
      for v in sorted(vars.keys()):
         name = v + '             '
         ino = ['-','-','-']
         if '<' in vars[v][0]: ino[0] = '<'
         if '>' in vars[v][0]: ino[2] = '>'
         line = 'C| ' + name[0:15] + '|' + ''.join(ino) + '| ' + '\nC|                |   | '.join(vars[v][1])
         olines.extend([line])
      olines.extend(['C'+71*'~'+'\nC'])

      # ~~ Final Tempates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~>
      for count in range(len(body)):
         proc = re.match(f77continu2,body[count])
         if proc: body[count] = '     &'+body[count][6:]
      #~~~~~~~~~<
      olines.extend(body)
      olines.extend(['C\nC'+71*'#'+'\nC'])

   return olines

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
                      help="specify configuration name, default is randomly found in the configuration file" )
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
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile)
   cfgnames = cfgs.keys()
   cfgname = options.configName
   if options.configName == '':
      cfgname = cfgnames[0]
   if cfgname not in cfgnames:
      print '\nNot able to get to find your configurtaion in the configuration file: ' + options.configFile + '\n'
      print ' ... use instead:'
      for cfgname in cfgnames : print '    +> ',cfgname
      sys.exit()

   # still in lower case
   if options.rootDir != '': cfgs[cfgname]['root'] = options.rootDir
   if options.version != '': cfgs[cfgname]['version'] = options.version
   # parsing for proper naming
   cfg = parseConfig_DoxygenTELEMAC(cfgs[cfgname])

   debug = False

   # ~~ Scans all source files to build a relation database ~~
   print '\n\nScanning the source code\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   fic,mdl,sbt,fct,prg,dep,all = scanSources(cfgname,cfg)

   # ~~ Scann all source files to update Doxygen ~~~~~~~~~~~~~~~~
   for mod in fic.keys():
      print '\nModifying the DOXYGEN Headers for ' + mod + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      for ifile in fic[mod].keys():

         # ~~ Read the content of the source file ~~~~~~~~~~~~
         ilines = getFileContent(ifile)
         # ~~ Update its Doxygen content ~~~~~~~~~~~~~~~~~~~~~
         olines = updateDOXYGEN(ilines,mod,len(fic[mod][ifile]),all)
         # ~~ Make sure the distination exists ~~~~~~~~~~~~~~~
         ofile = ifile.replace(cfg['TELDIR'],path.join(cfg['TELDIR'],cfgname))
         createDirectories(path.dirname(ofile))
         # ~~ Write the content of the source file ~~~~~~~~~~~
         putFileContent(ofile,olines)

   sys.exit()

