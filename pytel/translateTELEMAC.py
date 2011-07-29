#!/usr/bin/env python
"""@brief
"""
"""@author Sebastien E. Bourban and Noemie Durand
"""
"""@history 05/07/2011 -- Sebastien Bourban: python interpreter added for
         linux calls. This is a temporary solution as "/usr/bin/env" is not
         strickly portable cross operating systems
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
from config import OptionParser,parseConfigFile, parseConfig_TranslateTELEMAC
from os import path,walk,mkdir,remove
from utils import getTheseFiles,getFileContent,putFileContent
import sys
import re
import urllib
from PyQt4 import QtGui
from PyQt4 import QtCore

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#
CLine = []; CChar = []; inPhraseList = []; Phrase = ''; TLine = ''

# _____                       ______________________________________
# ____/ File Editing Toolbox /_____________________________________/
#
emptyrest = re.compile(r'\s*\Z',re.I)
splitrest = re.compile(r'.*[-*=#~_%]{10,71}.*',re.I)
firstword = re.compile(r'[\s\W]*(?P<word>\b[\w]+).*',re.I)
f77comment = re.compile(r'([C!#*]){1}(?P<rest>.*)',re.I)
f90comment = re.compile(r'(?P<line>([^"]*"[^"]*"[^"!]*|[^\']*\'[^\']*\'[^\'!]*|[^!]*))!{1}(?P<rest>.*)',re.I)

def needsTranslating(line):
   part0 = line.replace('\n',''); part2 = ''
   # ~~ Comment at the begining of the line ~~~~~~~~~~~~~~~~~~~~~~~~
   proc1 = re.match(f77comment,part0)         # line of comment characters
   if proc1:
      part2 = proc1.group('rest')
      proc2 = re.match(splitrest,part2)
      proc3 = re.match(emptyrest,part2)
      if not ( proc2 or proc3 ):
         #word = re.match(firstword,part2).group('name')
         #space,word,rest = part2.partition(word)
         proc4 = re.match(firstword,part2)
         if proc4:
            word = proc4.group('word')
            space,word,rest = part2.partition(word)
            return [ '', 'C'+space, word+rest+'\n' ]       # part1=part0.replace(part2,'')

   # ~~ Comment in the middle of the line ~~~~~~~~~~~~~~~~~~~~~~~~~~
   proc1 = re.match(f90comment,part0)
   if proc1:
      part2 = proc1.group('rest')
      proc2 = re.match(splitrest,part2)
      proc3 = re.match(emptyrest,part2)
      if not ( proc2 or proc3 ):
         proc4 = re.match(firstword,part2)
         if proc4:
            word = proc4.group('word')
            space,word,rest = part2.partition(word)
            return [ proc1.group('line'), '!'+space, word+rest+'\n' ]  # line.replace ...

   # ~~ No comment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   return [ part0+'\n', '', '' ]

   #proc2 = re.match(f77comment2,line)        # line with only one comment character
   #proc3 = re.match(emptyline,line)          # empty line
   #proc4 = re.match(codeline,line)           # line of FORTRAN code
   #proc5 = re.match(f77continu,line)         # continuation line of FORTRAN code
   #proc6 = re.match(f77followup,line)        # 'xxx continue' line
   #if not (proc1 or proc2 or proc3 or proc4 or proc5 or proc6) :
   #   translate = True
   #return translate

def BabelFish(iText):
   # some special tag string that will not be in any dictionary, hide in () for
   # avoidance of phrase grammar interpretation
   plusSub = '(ZplusZ)'
   crSub   = '(ZcrZ)'

   # set language request
   #  en_fr = English to French
   #  en_de = English to German
   #  en_it = English to Italian
   #  en_es = English to Spanish
   #  en_nl = English to Dutch
   #  nl_en = Dutch to English
   #  fr_en = French to English

   translateMode = 'fr_en'

   # ~~ Feeds Babel Fish a text in the right format ~~~~~~~~~~~~~~~~
   inPhrase = crSub.join(iText)
   inPhrase = inPhrase.replace('+',plusSub) # hide real +
   inPhrase = inPhrase.replace("c'",'ce ').replace("C'",'CE ')
   inPhrase = inPhrase.replace("n'",'ne ').replace("N'",'NE ')
   inPhrase = inPhrase.replace("l'",'le ').replace("L'",'LE ')
   inPhrase = inPhrase.replace("d'",'de ').replace("D'",'DE ')
   inPhrase = inPhrase.replace(' ','+')

   babelfishRequestA ="""http://babelfish.yahoo.com/translate_txt"""
   babelfishRequestB ="""?ei=UTF-8&doit=done&fr=bf-res&intl=1&tt=urltext&trtext="""
   babelfishRequestC ="""&lp=%s&btnTrTxt=Translate"""%(translateMode)
   serverURL = babelfishRequestA + babelfishRequestB + inPhrase + babelfishRequestC

   f = urllib.urlopen(serverURL)
   lines = f.readlines()
   f.close()

   # ~~ Reads translation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   oText = []
   tagSearch= '<div id=\"result\">'
   for line in lines:
      if line.count(tagSearch) > 0:
         line = line.replace(tagSearch,'')
         line = line.replace('<div style=\"padding:0.6em;\">','')
         line = line.replace('</div>','')
         line = line.strip()
         line = line.replace(plusSub,'+')    # put back real +

         for item in line.split(crSub): # put back real cr, break up string at CRs
            oText.append(item.strip().upper())

   return oText

class dlgBox(QtGui.QWidget) :

   outFileName = ''
   leftFileNames = []
   waitLines = []; todoLines = []; doneLines = []; leftLines = []
   waitChars = []
   linguist = ''; translator = ''

   def __init__(self, parent=None) :
      QtGui.QWidget.__init__(self, parent)

      # ~~ Initialisation of locals ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.linguist = cfg
      self.translator = 'BabelFish'

      # ~~ Initialisation of files left ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for mod in MODULES.keys():
         self.leftFileNames.extend(MODULES[mod]['sources'])

      # ~~ Go to next text in the next file to be translated ~~~~~~~
      while 1:
         if not self.getNextFile():
            self.Exit()              #> no more input file available
         print self.outFileName
         if self.getNextText():
            break              #> next translation text in todoLines
         self.endOfFile()

      # ~~ Initialisation of GUI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.initUI()
      # ~~ Set initial text/boxes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.initTextUI()
      self.initCBoxUI()

   def getNextText(self) :
      while self.leftLines != []:
         # ~~ skips lines with only one comment character ~~~~~~~~~~
         # ~~ and FORTRAN code lines ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         translate = needsTranslating(self.leftLines[0].upper())
         if translate[1] != '':
            # ~~ concatenates a whole paragraph ~~~~~~~~~~~~~~~~~~~~
            self.leftLines.pop(0)
            self.todoLines.append([False,translate])
         else:
            if self.todoLines == []:
               self.leftLines.pop(0)
               self.doneLines.append(translate[0])
            else:
               break           #> exits once found the 1st paragraph
      self.waitLines = self.todoLines
      if self.todoLines == []: return False
      # ~~ todoLines holds a whole paragraph to improve translation
      return True

   def getNextFile(self) :
      # ~~ always 1st file in the list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      while 1:
         if self.leftFileNames == []: return False
         frf = self.leftFileNames.pop(0)
         frd = path.dirname(frf)
         gbd = path.join(path.dirname(frd),self.linguist)
         if not path.isdir(gbd) : mkdir(gbd)
         self.outFileName = path.join(gbd,path.basename(frf))
         if path.isfile(path.splitext(self.outFileName)[0]+'.noe'):
            frf = path.splitext(self.outFileName)[0]+'.noe'
            break
         if not path.isfile(self.outFileName):
            break

      self.doneLines = []
      self.leftLines = getFileContent(frf)

      if path.splitext(frf)[1] == '.noe': remove(frf)

      return True
      # ~~ end of getNextFile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   def endOfFile(self):
      reply = QtGui.QMessageBox.question(self, 'File Completed', "Is this your final version ?", QtGui.QMessageBox.Yes, QtGui.QMessageBox.No)
      if reply != QtGui.QMessageBox.Yes:
         self.outFileName = path.splitext(self.outFileName)[0]+'.noe'
      putFileContent(self.outFileName,self.doneLines)

   def initUI(self) :
      # ~~ Initialisation of user interface ~~~~~~~~~~~~~~~~~~~~~~~~
      self.setGeometry(0, 50, 1280, 720)
      self.setWindowTitle("TELEMAC translation UI")

      # ~~ Introduction label ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.fileLabel = QtGui.QLabel("<b>~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~<\b>",self)
      self.fileLabel.move(10,7)

      # ~~ Choice of translating tools ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      translatorCombo = QtGui.QComboBox(self)
      translatorCombo.addItem('BabelFish')
      translatorCombo.addItem('Google')
      translatorCombo.addItem('User Translation')
      translatorCombo.move(1110,0)
      self.connect(translatorCombo,QtCore.SIGNAL('activated(QString)'),self.chooseTranslator)

      # ~~ Translate, OK, Quit buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      location = 5
      self.goButton = QtGui.QPushButton('=>',self)
      self.goButton.setGeometry(650,location,30,20)
      self.connect(self.goButton,QtCore.SIGNAL('clicked()'),self.goTranslate)        # /!\ goTranslate
      self.okButton = QtGui.QPushButton('OK',self)
      self.okButton.setGeometry(750,location,30,20)
      self.connect(self.okButton,QtCore.SIGNAL('clicked()'),self.okGoToNext)
      self.quitButton = QtGui.QPushButton('Quit',self)
      self.quitButton.setGeometry(800,location,30,20)
      self.connect(self.quitButton,QtCore.SIGNAL('clicked()'),self.Exit)

      # ~~ Checks/unchecks all boxes at once ~~~~~~~~~~~~~~~~~~~~~~~
      location = 7
      self.allOnCB = QtGui.QCheckBox('',self)
      self.allOnCB.move((1280-30)/2-25+26,location)
      self.connect(self.allOnCB,QtCore.SIGNAL('stateChanged(int)'),self.allChecked)
      allOn = QtGui.QLabel("All on/off",self)
      allOn.move((1280-30)/2-25-42,7)
      self.CBgroup = QtGui.QButtonGroup(self)
      self.CBgroup.setExclusive(False)

      # ~~ Text box with text for translation ~~~~~~~~~~~~~~~~~~~~~~
      self.toTranslateEdit = QtGui.QTextEdit(self)
      self.toTranslateEdit.setGeometry(5,30,(1280-30)/2-5,720-5-10-20)

      # ~~ Result of translation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # the width of the translationEdit box corresponds to 72 characters
      #lineLength = QtGui.QLabel("123456789012345678901234567890123456789012345678901234567890123456789012",self)
      #lineLength.move(680,47)
      self.translationEdit = QtGui.QTextEdit(self)
      self.translationEdit.setGeometry((1280+10)/2,30,(1280-30)/2-5,720-5-10-20)
      #self.connect(self.translationEdit,QtCore.SIGNAL('textChanged()'),self.onChanged)

      # ~~ end of initialisation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   def goTranslate(self) :
      if self.translator == 'BabelFish' :
         frgbPack = []; self.waitLines = []
         idb = 0
         for index in range(len(self.todoLines)):
            if self.todoLines[index][0]:
               frgbPack.append(self.todoLines[index][1][2].replace('\n',''))
            else:
               if frgbPack == []:
                  self.waitLines.append(self.todoLines[index])
                  idb = index + 1
               else:
                  for gbline in BabelFish(frgbPack):
                     self.waitLines.append([True,[self.todoLines[idb][1][0],self.todoLines[idb][1][1],gbline]])
                     if idb < index: idb = idb + 1
                  self.waitLines.append(self.todoLines[index])
                  idb = index + 1
               frgbPack = []
         if frgbPack != []:
            for gbline in BabelFish(frgbPack):
               self.waitLines.append([True,[self.todoLines[idb][1][0],self.todoLines[idb][1][1],gbline]])
               if idb < index: idb = idb + 1
         frgbPack = []
      elif self.translator == 'Google' :
         self.Exit()   # Google toolbox not yet implemented here
      self.initTextUI()

   def initTextUI(self) :
      # ~~ Information ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.fileLabel.setText("<b>..."+self.outFileName[len(self.outFileName)-40:]+"</b>")

      # ~~ Prints the text for translation ~~~~~~~~~~~~~~~~~~~~~~~~~
      self.toTranslateEdit.setText(self.setContext(self.todoLines))
      # ~~ Result of translation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.translationEdit.setText(self.setContext(self.waitLines))

   def initCBoxUI(self) :
      # ~~ and associates check boxes to translate or not ~~~~~~~~~~
      for i in range(len(self.todoLines)) :
         location = 38+15+i*15
         if len(self.doneLines) > 5: location = location + 5*15 # + 13*5 to include 5-line above context
         else: location = location + len(self.doneLines)*15
         self.CB = QtGui.QCheckBox('',self)
         self.CB.move((1280-30)/2-25+26,location)
         self.CB.show()
         self.CBgroup.addButton(self.CB)
         self.connect(self.CB,QtCore.SIGNAL('stateChanged(int)'),self.checkTranslate)

   def getContent(self,tLines):
      # ~~ Extract the content out of the context
      for i in range(5):
         if len(self.doneLines) > i:
            tLines.pop(0)
      tLines.pop(0); tLines.pop(0); tLines.pop(0)
      for i in range(10):
         if len(self.leftLines) > i:
            tLines.pop(len(tLines)-1)
      tLines.pop(len(tLines)-1); tLines.pop(len(tLines)-1); tLines.pop(len(tLines)-1)

      return tLines

   def setContext(self,text):
      # ~~ Provide context - 5 lines before ~~~~~~~~~~~~~~~~~~~~~~~~
      head = ''
      for i in range(5):
         if len(self.doneLines) > i: #  size=+1
            head = self.html11(self.doneLines[len(self.doneLines)-i-1]) + head
      if len(self.doneLines) > 5: head = self.html00() + head
      else: head = self.html01() + head

      # ~~ Format main text ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      main = ''
      for line in text:
         if not line[0]: main = main + self.html29( self.html23(line[1][0])+self.html21(line[1][1]+line[1][2]) )
         else: main = main + self.html29( self.html23(line[1][0])+self.html22(line[1][1]+line[1][2]) )

      # ~~ Provide context - 10 lines after ~~~~~~~~~~~~~~~~~~~~~~~~
      foot = ''
      for i in range(10):
         if len(self.leftLines) > i:
            foot = foot + self.html11(self.leftLines[i])
      if len(self.leftLines) > 10: foot = foot + self.html00()
      else: foot = foot + self.html01()

      return self.html10(head) + self.html20(main) + self.html10(foot)

   def html00(self):
      return '<tr><td><font face="Courier New" color=#f99><b>(...)</b></font></td></tr>'
   def html01(self):
      return '<tr><td><font face="Courier New" color=#f99><b>~~~~~~~~~~~~~~~~~~</b></font></td></tr>'
   def html10(self,text):
      return '<table width="100%" border="0" cellpadding="0" cellspacing="0" bgcolor=#ffc>' + text + '</table>'
   def html11(self,text):
      return '<tr><td><font face="Courier New" color=#000>' + text.replace(' ','&nbsp;') + '</font></td></tr>'
   def html20(self,text):
      return '<table width="100%" border="0" cellpadding="0" cellspacing="0" bgcolor=#fff>' + text + '</table>'
   def html21(self,text):
      return '<font face="Courier New" color=#080><b>' + text.replace(' ','&nbsp;') + '</b></font>'
   def html22(self,text):
      return '<font face="Courier New" color=#56d><b>' + text.replace(' ','&nbsp;') + '</b></font>'
   def html23(self,text):
      return '<font face="Courier New" color=#000>' + text.replace(' ','&nbsp;') + '</font>'
   def html29(self,text):
      return '<tr><td>' + text + '</td></tr>'

   def okGoToNext(self) :
      # ~~ copies agreed text ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for line in self.getContent(str(self.translationEdit.toPlainText()).split('\n')) :
         self.doneLines.append((str(line)+'\n').upper())

      # ~~ clear texts and reset boxes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.waitLines = []
      self.todoLines = []
      for CB in self.CBgroup.buttons() :
         #CB.setChecked(False)
         self.CBgroup.removeButton(CB)
         CB.deleteLater()
      self.allOnCB.setChecked(False)

      # ~~ looks for next comment to be translated ~~~~~~~~~~~~~~~~~
      while 1:
         if self.getNextText():
            break              #> next translation text in todoLines
         self.endOfFile()
         if not self.getNextFile():
            self.Exit()              #> no more input file available
         if self.getNextText():
            break              #> next translation text in todoLines
         print self.outFileName
         self.endOfFile()

      # ~~ Set initial text boxes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.translationEdit.clear()
      self.toTranslateEdit.clear()
      self.initTextUI()
      self.initCBoxUI()

   def chooseTranslator(self,text) :
      self.translator = text

   def checkTranslate(self) :
      iline = -1
      for CB in self.CBgroup.buttons():
         iline = iline + 1
         if CB.isChecked(): self.todoLines[iline][0] = True
         else: self.todoLines[iline][0] = False

   def allChecked(self) :
      if self.allOnCB.isChecked():
         for CB in self.CBgroup.buttons(): CB.setChecked(True)
         for iline in range(len(self.todoLines)): self.todoLines[iline][0] = True
      else :
         for CB in self.CBgroup.buttons(): CB.setChecked(False)
         for iline in range(len(self.todoLines)): self.todoLines[iline][0] = False

   def Exit(self) :
      if self.doneLines != []:
         self.outFileName = path.splitext(self.outFileName)[0]+'.noe'
         poolLines = self.doneLines
         for line in self.waitLines:
            print '>'+line[1][0]+line[1][1]+line[1][2]
            poolLines.append(line[1][0]+line[1][1]+line[1][2])
         poolLines.extend(self.leftLines)
         print poolLines
         putFileContent(self.outFileName,poolLines)
      sys.exit()

class ConfirmSaveBox(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.setGeometry(300, 300, 250, 150)
        self.setWindowTitle('Confirm Save')

    def closeEvent(self, event):
        reply = QtGui.QMessageBox.question(self, 'Message',
            "Are you sure to quit?", QtGui.QMessageBox.Yes, QtGui.QMessageBox.No)

        if reply == QtGui.QMessageBox.Yes:
            event.accept()
        else:
            event.ignore()

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

if __name__ == "__main__":
   debug = False

# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print '\n\nLoading Options and Configurations\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   CFGNAME = ''
   PWD = path.dirname(path.dirname(sys.argv[0]))
   SYSTELCFG = path.join(PWD,'config')
   if environ.has_key('SYSTELCFG'): SYSTELCFG = environ['SYSTELCFG']
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
   options, args = parser.parse_args()
   if not path.isfile(options.configFile):
      print '\nNot able to get to the configuration file: ' + options.configFile + '\n'
      sys.exit()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgname = options.configName
   if options.configName == '':
      cfgname = parseConfigFile(options.configFile).keys()[0]
   if cfgname not in parseConfigFile(options.configFile).keys():
      print '\nNot able to get to find your configurtaion in the configuration file: ' + options.configFile + '\n'
      sys.exit()

   cfg = parseConfig_TranslateTELEMAC(cfgname)[cfgname]

   MODULES = {}
   for mod in cfg['MODULES'].keys():
      if mod in cfg['COMPILER']['MODULES']:
         files = getTheseFiles(path.join(cfg['MODULES'][mod]['path'],'sources'),['.f'])
         if files != []:
            MODULES.update({mod:cfg['MODULES'][mod]})
            MODULES[mod].update({'sources':files})

   app = QtGui.QApplication(sys.argv)
   translationUI = dlgBox()
   translationUI.show()
   app.exec_()

   """      for mod in cfg['COMPILER']['MODULES']:
# ~~ Scans all source files to build a relation database ~~~~~~~~~~~
         print '\n\nTranslation of module  ' + mod + '\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n'
   """
