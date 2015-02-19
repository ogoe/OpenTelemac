#!/usr/bin/env python
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
         Now supports SYSTELCFG as a directory (old Perl version, to
         which systel.cfg is added) or as a file.
"""
"""@history 30/04/2011 -- Sebastien E. Bourban
         Upgrade made to config parsing to include the option to reset
         the version and the root from the command line option:
         -v <version>, reset the version read in the config file with this
         -r <root>, reset the root path read in the config file with this
"""
"""@history 05/07/2011 -- Sebastien E. Bourban
         Python interpreter added for linux calls. This is a temporary
         solution as "/usr/bin/env" is not strickly portable cross
         operating systems
"""
"""@history 30/08/2011 -- Sebastien E. Bourban
         Completion of the scipt so it reads the agreed template and write
         the DOXYGEN tags. There are a number of extensions that can be used,
         in particular a scan of the source code for validity of declaration
         and memory allocation.
"""
"""@history 27/01/2012 -- Sebastien E. Bourban
         A new option (--modules) added to the command line, which if present
         will reset the value of the key in the configuration file.
         This development was triggered by Christophe Coulet (Artelia-Sogreah)
         who asked about it on the open TELEMAC forum.
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
import sys
import re
from os import path, walk, sep, chdir, environ
import subprocess as sp
# ~~> dependencies towards the root of pytel
from config import OptionParser,parseConfigFile, parseConfig_DoxygenTELEMAC
# ~~> dependencies towards other pytel/modules
from parsers.parserFortran import scanSources, parseDoxyWrap
from utils.files import getFileContent, putFileContent, createDirectories
from utils.messages import MESSAGES,banner

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def createDOXYGEN(ifile,ilines,lname,list):

   # ~~ Parsers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   olines = []

   print '    +>  ',path.basename(ifile),
   for name,subname,doxy,body in parseDoxyWrap(ilines):
      print subname, name,
      who = list[lname][subname]

      # ~~ General
      olines.append(ifile + '\n!')
      #line = '!> @code\n'
      #if who['type'] == 'S': line = line + '!>             SUBROUTINE\n'
      #if who['type'] == 'M': line = line + '!>             MODULE\n'
      #if who['type'] == 'P': line = line + '!>             PROGRAM\n'
      #if who['type'] == 'F': line = line + '!>             FUNCTION\n'
      #line = line + '!>      NAME : ' + who['name'] + '\n' \
      #              '!>    MODULE : ' + who['libname'] + '\n'
      #line = line + '!> @endcode\n'
      #olines.append(line)

      # ~~ Module
      olines.append('!> @par Module: '+who['libname']+'\n!')

      for d in doxy:
         # ~~ Brief
         if d[0] == 'brief':
            line = '!> @brief\n!> ' + '\n!> '.join(d[1])
            line.replace('\n!> \n','\n!> <br><br>\n')
            olines.append(line+'\n!')

         # ~~ User Defined Information
         if d[0] == 'note' or d[0] == 'warn' or d[0] == 'refs' or d[0] == 'code':
            if d[0] == 'note':
               line = '!> @note\n!> ' + '\n!> '.join(d[1])
               line.replace('\n!> \n','\n!> <br><br>\n')
               olines.append(line+'\n!')
            if d[0] == 'bug':
               line = '!> @bug\n!> ' + '\n!> '.join(d[1])
               line.replace('\n!> \n','\n!> <br><br>\n')
               olines.append(line+'\n!')
            if d[0] == 'warn':
               line = '!> @warning\n!> ' + '\n!> '.join(d[1])
               line.replace('\n!> \n','\n!> <br><br>\n')
               olines.append(line+'\n!')
            if d[0] == 'refs':
               line = '!> @reference\n!> ' + '\n!> '.join(d[1])
               line.replace('\n!> \n','\n!> <br><br>\n')
               olines.append(line+'\n!')
            if d[0] == 'code':
               line = '!> @code\n!> ' + '\n!> '.join(d[1])
               line.replace('\n!> \n','\n!> <br><br>\n')
               olines.append(line + '\n!> @endcode\n!')

      # ~~ Final Uses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['uses'] != {}:
         line = '!> @par Use(s)\n!><br>' + ', '.join(sorted(who['uses'].keys()))
         olines.append(line+'\n!')

      # ~~ Final Variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['args'] != [] or who['vars']['use'] != {} or who['vars']['cmn'] != [] or who['vars']['dec'] != [] or who['vars']['als'] != []:
         line = '!> @par Variable(s)\n!>  <br><table>'
         olines.append(line)

         # ~~ Arguments
         if who['args'] != []:
            line = '!>     <tr><th> Argument(s)\n!>    </th><td> ' + ', '.join(sorted(who['args'])) + '\n!>   </td></tr>'
            olines.append(line)
         # ~~ Uses
         if who['vars']['use'] != {}:
            line = '!>     <tr><th> Use(s)\n!>    </th><td>'
            for u in who['vars']['use']:
               uv = []
               for v in who['vars']['use'][u]:
                  uv.append('\n!> @link ' + u + '::' + v + ' ' + v + '@endlink')
               line = line + '<hr>\n!> ' + u + ' :<br>' + ', '.join(sorted(uv))
            line = line.replace('<td><hr>\n','<td>\n') + '\n!>   </td></tr>'
            olines.append(line)

         # ~~ Commons
         if who['vars']['cmn'] != []:
            line = '!>     <tr><th> Common(s)\n!>    </th><td>'
            for u in who['vars']['cmn']:
               line = line + '<hr>\n!> ' + u[0] + ' : ' + ', '.join(u[1])
            line = line.replace('<td><hr>\n','<td>\n') + '\n!>   </td></tr>'
            olines.append(line)

         # ~~ Declars
         if who['vars']['dec'] != []:
            line = '!>     <tr><th> Internal(s)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['dec'])) + '\n!>   </td></tr>'
            olines.append(line)

         # ~~ Externals
         #if who['vars']['xtn'] != []:
         #   line = '!>     <tr><th> External(s)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['xtn'])) + '\n!>   </td></tr>'
         #   olines.append(line)

         # ~~ Aliases
         if who['vars']['als'] != []:
            line = '!>     <tr><th> Alias(es)\n!>    </th><td> ' + ', '.join(sorted(who['vars']['als'])) + '\n!>   </td></tr>'
            olines.append(line)

         line = '!>     </table>\n!'
         olines.append(line)

      # ~~ Final Calls ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['calls'] != {} or who['functions'] != []:
         line = '!> @par Call(s)\n!>  <br><table>'
         olines.append(line)
         if who['calls'] != {}:
            line = '!>     <tr><th> Known(s)\n!>    </th><td> ' + '(), '.join(sorted(who['calls'].keys())) + '()\n!>   </td></tr>'
            olines.append(line)
         if who['functions'] != []:
            line = '!>     <tr><th> Unknown(s)\n!>    </th><td> ' + ', '.join(sorted(who['functions'])) + '\n!>   </td></tr>'
            olines.append(line)
         line = '!>     </table>\n!'
         olines.append(line)

      # ~~ Final Called ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if who['called'] != []:
         line = '!> @par Called by\n!><br>' + '(), '.join(sorted(who['called'])) + '()\n!'
         olines.append(line)

      # ~~ Final History ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for d in doxy:
         # ~~ Other pars
         if d[0] == 'para':
            line = '!> @par ' + '\n!> '.join(d[1])
            line.replace('\n!> \n','\n!> <br><br>\n')
            olines.append(line+'\n')
         # ~~ Result
         if d[0] == 'result':
            line = '!> @par Result ' + '\n!> '.join(d[1])
            olines.append(line+'\n!')

      # ~~ Final History ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      hist = False
      for d in doxy:
         if d[0] == 'history': hist = True
      if hist:
         olines.append('!> @par Development history')
         olines.append('!>   <br><table>')
         olines.append('!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>')
         for d in doxy:
            if d[0] == 'history':
               olines.append('!>  <tr><td><center> ' + d[1][2] +' </center>')
               olines.append('!>    </td><td> ' + d[1][1])
               olines.append('!>    </td><td> ' + d[1][0])
               if len(d[1]) > 3:
                  olines.append('!>    </td><td> ' + d[1][3])
               else:
                  olines.append('!>    </td><td> ')
                  print '\nHistory comment missing in: ' + subname + '\n'
               olines.append('!>   </td></tr>')
         olines.append('!>   </table>\n!')

      # ~~ Extended Description
      #if vars != {} or who['args'] != '':
      #   for a in who['args']:
      #      found = False
      #      for v in vars:
      #         if a in parseVars(v): found = True
      #      if not found: vars.update({a:['',['']]})
      #   olines.extend(['!>  @par Details of primary variable(s)\n!>  <br><table>'])
      #   #olines.extend(['!>  <p><div id="dynsection-1" onclick="return toggleVisibility(this)" class="dynheader closed" style="cursor:pointer;"> \
      #   #   \n!>  <img id="dynsection-1-trigger" src="closed.png"> Here are details of primary variables:</div> \
      #   #   \n!>  <div id="dynsection-1-summary" class="dynsummary" style="display:block;"> \
      #   #   \n!>  </div> \
      #   #   \n!>  <div id="dynsection-1-content" class="dyncontent" style="display:none;">'])
      #   olines.extend(['!>\n!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>'])
      #   for v in sorted(vars.keys()):
      #      #print v,vars[v][1]
      #      ino = ['-','-','-']
      #      if '<' in vars[v][0]: ino[0] = '<'
      #      if '>' in vars[v][0]: ino[2] = '>'
      #      line = '!>          <tr><td>' + v + '\n!></td><td>' + ''.join(ino) + '</td><td>' + '\n!>                  '.join(vars[v][1]) + '\n!>    </td></tr>'
      #      olines.extend([line])
      #   olines.extend(['!>     </table>'])
      #   #olines.extend(['!>  </div></p>'])

      #~~~~~~~~~>
      #for count in range(len(docs)):
      #   proc = re.match(f77continu2,docs[count])
      #   if proc: docs[count] = '     &'+docs[count][6:]
      #~~~~~~~~~<
      #olines.extend(docs)

      # ~~ Final Tempates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #olines.extend(['C\nC'+71*'~'])
      #for v in sorted(vars.keys()):
      #   name = v + '             '
      #   ino = ['-','-','-']
      #   if '<' in vars[v][0]: ino[0] = '<'
      #   if '>' in vars[v][0]: ino[2] = '>'
      #   line = 'C| ' + name[0:15] + '|' + ''.join(ino) + '| ' + '\nC|                |   | '.join(vars[v][1])
      #   olines.extend([line])
      #olines.extend(['C'+71*'~'+'\nC'])

      # ~~ Unchanged ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for line in body:
         olines.append(line)

   print ''
   return olines
"""
      <link rel="stylesheet" href="./images/design.css" type="text/css" media="screen,projection" />
      <link rel="stylesheet" href="./images/layout.css" type="text/css" media="screen,projection" />
"""

def replaceDOXYGEN(doxydocs):

   iTextList = [ ('<!DOCTYPE HTML PUBLIC','<BODY BGCOLOR="#FFFFFF">'),('<DIV class="div-page">','<hr>'),('<div class="header">','<div class="summary">'),('<hr>\n<div class="div-footer">','</html>'),('<dl class="user"><dt><b','>'),('</b></dt><d','d><br/>') ]
   oTextList = [ r"""<!DOCTYPE install PUBLIC "-//Joomla! 2.5//DTD template 1.0//EN" "http://www.joomla.org/xml/dtd/1.6/template-install.dtd">
   <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en" dir="ltr">
   <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8" /> 
      <meta name="keywords" content="open, telemac, mascaret, hydraulique, surface libre, houle, vague, hydraulic, free surface, wave, sediment" /> 
      <meta name="robots" content="index, follow" /> 
      <meta name="date" content="2013-07-24T16:49:04+0100"/> 
      <meta name="description" content="The open TELEMAC-MASCARET system is a set of software for free surface numerical modelling of:\n* 2D, 3D Hydrodynamics,\n* Sediment (sand and muds),\n* Waves." /> 
      <TITLE>The open TELEMAC-MASCARET system: 2D, 3D hydrodynamics sediment waves simulation system</TITLE> 
      <link rel="shortcut icon" href="./images/favicon.ico" type="image/vnd.microsoft.icon" /> 
      <script src="./images/jquery-1.4.2.min.js" type="text/javascript"></script>
      <link rel="stylesheet" href="./images/system.css" type="text/css" />
      <link rel="stylesheet" href="./images/position.css" type="text/css" media="screen,projection" />
      <link rel="stylesheet" href="./images/layout.css" type="text/css" media="screen,projection" />
      <link rel="stylesheet" href="./images/general.css" type="text/css" />
      <link rel="stylesheet" href="./images/principal.css" type="text/css" />
      <style type="text/css"> 
      #ahgalleryOTconsortium { margin-left: auto; margin-right: auto; margin-top: 0px !important; margin-bottom: 0px !important; width: 1000px; } 
      #ahgalleryOTconsortium ul.hover_block0, #ahgalleryOTconsortium ul.hover_block1, #ahgalleryOTconsortium ul.hover_block2 { display: block; overflow: hidden; padding-top: 20px; padding-left: 2px; background: transparent; margin-left: 2px; margin-top: 0 !important; margin-bottom: 0 !important; } 
      #ahgalleryOTconsortium ul.bottom_block { padding-bottom: 20px ; } 
      #ahgalleryOTconsortium ul.hover_block0 li.item, #ahgalleryOTconsortium ul.hover_block1 li.item, #ahgalleryOTconsortium ul.hover_block2 li.item { margin-left: 0; padding-left: 0; list-style:none; list-style-position: inside; float:left; background: transparent; width: 150px; position: relative; } 
      #ahgalleryOTconsortium ul.hover_block0 li a.teaser, #ahgalleryOTconsortium ul.hover_block1 li a.teaser , #ahgalleryOTconsortium ul.hover_block2 li a.teaser{ display: block; position: relative; overflow: hidden; height: 60px; width: 130px; padding: 1px; } 
      #ahgalleryOTconsortium ul.hover_block0 li div.teaser, #ahgalleryOTconsortium ul.hover_block1 li div.teaser , #ahgalleryOTconsortium ul.hover_block2 li div.teaser { display: block; position: relative; overflow: hidden; height: 60px; width: 140px; padding: 1px; } 
      #ahgalleryOTconsortium ul.hover_block0 li img.overlay, #ahgalleryOTconsortium ul.hover_block1 li img.overlay, #ahgalleryOTconsortium ul.hover_block2 li img.overlay { margin: 0; position: absolute; top: 5px; left: 0; border: 0; } 
      </style> 
      <script type="text/javascript" src="./images/hide.js"></script>
      <script type="text/javascript"> 
         window.addEvent(\'load\', function() { 
            new JCaption(\'img.caption\'); 
         }); 
         window.addEvent(\'domready\', function() { 
            $$(\'.hasTip\').each(function(el) { 
               var title = el.get(\'title\'); 
               if (title) { 
                  var parts = title.split(\'::\', 2); 
                  el.store(\'tip:title\', parts[0]); 
                  el.store(\'tip:text\', parts[1]); 
            }}); 
            var JTooltips = new Tips($$(\'.hasTip\'), { maxTitleChars: 50, fixed: false}); 
         }); 
      </script> 
      <link href="./images/tabsVTK.css" rel="stylesheet" type="text/css"/> 
      <link href="./images/searchVTK.css" rel="stylesheet" type="text/css"/> 
      <script type="text/javaScript" src="./images/searchVTK.js"></script> 
      <link href="./images/doxygenVTK.css" rel="stylesheet" type="text/css"/> 
      </HEAD>
   <BODY BGCOLOR="#FFFFFF">""",
      """<div id="all">
    <div id="header">
        <div class="logoheader">
            <h1 id="logo">open TELEMAC-MASCARET               <span class="header1">The mathematically superior suite of solvers</span></h1>
         </div>
         <div class="bar-top" >
         <ul class="menu">
          <li><a href="http://www.opentelemac.org/" ><img src="./images/OTM_Home-icon_15pix_212-118-0.png" alt="Home" /><span class="image-title">Home</span> </a></li>
       <li><a href="http://www.opentelemac.org/index.php/contact2"><img src="./images/OTM_Mail-icon_15pix_212-118-0.png" alt="Contact"/><span class="image-title">CONTACT</span> </a></li>
       <li><span class="separator"><img src="./images/OTM_transparent_15x080pix.png" /><img src="./images/OTM_transparent_15x080pix.png" /></span></li>
       <li><a href="http://www.opentelemac.org/index.php/community"><span class="image-title">COMMUNITY</span> </a></li>
       <li><a href="http://www.opentelemac.org/index.php/user-conference"><span class="image-title">CONFERENCE</span> </a></li>
       <li><a href="http://www.opentelemac.org/index.php/download"><span class="image-title">DOWNLOAD</span> </a></li>
          <li><a href="http://docs.opentelemac.org/" style="color:#333;background:#ffd1a3;padding:10px;"><span class="image-title">DOXY DOCS</span> </a></li>
       <li><a href="http://www.opentelemac.org/index.php/kunena"><span class="image-title">FORUM</span> </a></li>
          <li><a href="http://wiki.opentelemac.org/doku.php"><span class="image-title">WIKI</span> </a></li>
            </ul>
            </div>    
            </div>

      <!-- Generated by Doxygen 1.7.0 -->
      <script type="text/javascript">
      function hasClass(ele,cls) {
        return ele.className.match(new RegExp('(\\s|^)'+cls+'(\\s|$)'));
      }
      function addClass(ele,cls) {
        if (!this.hasClass(ele,cls)) ele.className += " "+cls;
      }
      function removeClass(ele,cls) {
        if (hasClass(ele,cls)) {
          var reg = new RegExp('(\\s|^)'+cls+'(\\s|$)');
          ele.className=ele.className.replace(reg,' ');
        }
      }
      function toggleVisibility(linkObj) {
       var base = linkObj.getAttribute('id');
       var summary = document.getElementById(base + '-summary');
       var content = document.getElementById(base + '-content');
       var trigger = document.getElementById(base + '-trigger');
       if ( hasClass(linkObj,'closed') ) {
         summary.style.display = 'none';
         content.style.display = 'block';
         trigger.src = 'open.png';
         removeClass(linkObj,'closed');
         addClass(linkObj,'opened');
       } else if ( hasClass(linkObj,'opened') ) {
         summary.style.display = 'block';
         content.style.display = 'none';
         trigger.src = 'closed.png';
         removeClass(linkObj,'opened');
         addClass(linkObj,'closed');
       }
       return false;
      }
      </script>
      <br>""",
      """<div  id="main" >
         <div  class="header" >
         <div  class="summary" >""",
         """<br>
      </div></div>
               <h2 class="unseen">Generated on Fri Aug 31 2013 18:12:58 by S.E.Bourban (HRW) using <A href="http://www.doxygen.org/index.html"><img class="footer" width=70px src="doxygen.png" alt="doxygen"/></A> 1.7.0</h2>
               <div  id="footer-outer" ><div id="footer-inner"><div id="bottom"><div class="box box1"><div class="moduletable">
               <script type="text/javascript">
              jQuery.noConflict();
        jQuery(document).ready(function($)
         {
          $('#ahgalleryOTconsortium ul.hover_block0 li.item').hover(function(){
           $(this).find('img.overlay').animate({top:'60px'},{queue:false,duration:500});
          }, function(){
           $(this).find('img.overlay').animate({top:'5px'},{queue:false,duration:500});
          });
        });
        </script>
               <div id="ahgalleryOTconsortium">
                  <ul class="hover_block0 bottom_block">
                     <li class="item"><a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.arteliagroup.com/" target="_blank"><img class="overlay" src="./images/Sogreah-Artelia.jpg" alt="" />
                        <span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">Artelia</span>
                        <span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span></a></li>
                     <li class="item"><a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.baw.de/de/index.php.html" target="_blank"><img class="overlay" src="./images/logo_baw.png" alt="" />
                        <span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">BundesAnstalt fur Wasserbau</span>
                        <span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span></a></li>
                     <li class="item"><a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.cetmef.equipement.gouv.fr/" target="_blank"><img class="overlay" src="./images/logo_cetmef_v2.png" alt="" />
                        <span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">CETMEF</span>
                        <span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span></a></li>
                     <li class="item"><a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.stfc.ac.uk/About%20STFC/45.aspx" target="_blank"><img class="overlay" src="./images/logo_Daresbury_v3.gif" alt="" />
                        <span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">Daresbury Laboratory</span>
                        <span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span></a></li>
                     <li class="item"><a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://research.edf.com/research-and-innovation-44204.html&amp;tab=44205" target="_blank"><img class="overlay" src="./images/logo_edfR&D.jpg" alt="" />
                        <span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">EDF R&D</span>
                        <span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span></a></li>
                     <li class="item"><a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.hrwallingford.com" target="_blank"><img class="overlay" src="./images/logo_HRW.png" alt="" />
                        <span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">HR Wallingford</span>
                        <span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span></a></li>
                  </ul></div>
        <div class="clr"></div>
       </div></div>
       </div></div>
               <div id="footer-sub"><div id="footer">
                  <ul class="menu">
                     <li class="item-187"><a href="http://www.opentelemac.org/index.php/forum-rules" >Forum rules</a></li>
                     <li class="item-111"><a href="http://www.opentelemac.org/index.php/licence" >Licence</a></li>
                     <li class="item-112"><a href="http://www.opentelemac.org/index.php/privacy" >Privacy</a></li>
                     <li class="item-112"><a href="http://www.opentelemac.org/index.php/terms-and-conditions" >Terms &amp; Conditions</a></li>
                  </ul>
               </div>
       </div></div>
      </body></html>""", '<dl class="user"><dt><h3><b>','</b></h3></dt><dd><br/>' ]

   text = r'(?P<before>[\s\S]*?)(?P<text>%s)(?P<after>[\s\S]*)'
   if path.exists(doxydocs):
      dirpath, _, filenames = walk(doxydocs).next()
      for fle in filenames :
         head,tail = path.splitext(fle)
         if tail == '.html' :
            print '    +> ',fle
            lines = ''.join(getFileContent(path.join(dirpath,fle)))
            proc = True
            while proc:
               proc = False
               for itext,otext in zip(iTextList,oTextList):
                  proc0 = re.match(re.compile(text%(itext[0]),re.I),lines)
                  if proc0:
                     a = proc0.group('before')
                     proc1 = re.match(re.compile(text%(itext[1]),re.I),proc0.group('after'))
                     if proc1:
                        proc = True
                        b = proc1.group('after')
                        lines = a + otext + b
                        #print 'replacing: ',proc0.group('text')+proc1.group('before')
            putFileContent(path.join(dirpath,fle),[lines])

   return

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   debug = False
   BYPASS = False  # /!\ Temporary bypass for subroutine within programs

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CFGNAME = 'doxydocs'
   PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   SYSTELCFG = path.join(PWD,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ banners ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mes = MESSAGES()  # runcode takes its version number from the CAS file
   svnrev = ''
   svnurl = ''
   svnban = 'unknown revision'
   try:
      key_equals = re.compile(r'(?P<key>[^:]*)(?P<after>.*)',re.I)
      tail,code = mes.runCmd('svn info '+PWD,True)
      for line in tail.split('\n'):
         proc = re.match(key_equals,line)
         if proc:
            if proc.group('key').strip() == 'Revision': svnrev = proc.group('after')[1:].strip()
            if proc.group('key').strip() == 'URL': svnurl = proc.group('after')[1:].strip()
   except:
      pass
   if svnrev+svnurl == '':
      print '\n'.join(banner('unknown revision'))
   else:
      if svnurl != '': print '\n'.join(banner(svnurl.split('/')[-1]))
      if svnrev != '': print '\n'.join(banner('rev. #'+svnrev))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   parser = OptionParser("usage: %prog [options] \nuse -h for more help.")
   parser.add_option("-c", "--configname",type="string",dest="configName",default=CFGNAME,
      help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_option("-f", "--configfile",type="string",dest="configFile",default=SYSTELCFG,
      help="specify configuration file, default is systel.cfg" )
   parser.add_option("-r", "--rootdir",type="string",dest="rootDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_option("-d", "--doxydir",type="string",dest="doxyDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_option("-m", "--modules",type="string",dest="modules",default='',
      help="specify the list modules, default is taken from config file" )
   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print ' ... in directory: ' + dircfg + '\n ... use instead: '
         _, _, filenames  = walk(dircfg).next()
         for fle in filenames :
            head,tail = path.splitext(fle)
            if tail == '.cfg' : print '    +> ',fle
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)
   cfgname = cfgs.iterkeys().next()

   # still in lower case
   if not cfgs[cfgname].has_key('root'): cfgs[cfgname]['root'] = PWD
   if options.rootDir != '': cfgs[cfgname]['root'] = path.abspath(options.rootDir)
   if options.modules != '': cfgs[cfgname]['modules'] = options.modules.replace(',',' ').replace(';',' ').replace('.',' ')
   if options.doxyDir == '':
      cfgs[cfgname].update({'doxydocs':path.join(cfgs[cfgname]['root'],'documentation'+sep+cfgname)})
   else:
      cfgs[cfgname].update({'doxydocs':options.doxyDir})
   if not path.isdir(cfgs[cfgname]['doxydocs']): createDirectories(cfgs[cfgname]['doxydocs'])
   # parsing for proper naming
   cfg = parseConfig_DoxygenTELEMAC(cfgs[cfgname])
   print '\n\nScanning the source code for:\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   print '    +> configuration: ' +  cfgname
   if 'brief' in cfgs[cfgname]: print '    +> '+'\n    |  '.join(cfgs[cfgname]['brief'].split('\n'))
   print '    +> root:          ' +  cfgs[cfgname]['root']
   print '    +> modules:       ' +  cfgs[cfgname]['modules'] + '\n\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'

   # ~~ Scans all source files to build a relation database ~~
   fic,mdl,sbt,fct,prg,dep,racine = scanSources(cfgname,cfg,BYPASS)

   # ~~ Scann all source files to update Doxygen ~~~~~~~~~~~~~~~~
   for mod in fic:
      print '\nCreating the DOXYGEN headers for ' + mod + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      for ifile in fic[mod]:

         # ~~ Read the content of the source file ~~~~~~~~~~~~
         ilines = getFileContent(ifile)
         # ~~ Update its Doxygen content ~~~~~~~~~~~~~~~~~~~~~
         olines = createDOXYGEN(ifile,ilines,mod,racine)
         # ~~ Make sure the distination exists ~~~~~~~~~~~~~~~
         ofile = ifile.replace(cfg['root'],cfg['doxydocs'])
         createDirectories(path.dirname(ofile))
         # ~~ Write the content of the source file ~~~~~~~~~~~
         putFileContent(ofile,olines)

   # ~~ Run Doxygen ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   chdir(cfg['doxydocs'])
   if sp.call(['doxygen']): sys.exit(1)

   # ~~ Scan all HTML files and replace template in phases
   replaceDOXYGEN(path.join(cfgs[cfgname]['doxydocs'],'html'))


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nMy work is done\n\n'

   sys.exit(0)

