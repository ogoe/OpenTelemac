#!/usr/bin/env python
# -*- coding: UTF-8 -*-
######################################################################
# Project Plateforme Hydro-Environnementale
# Author: Arthur Torossian
# Date:   03.09.2014
#         (c) 09/2014 EDF
######################################################################
import string, re, os, sys, getopt, time, shutil
  
######################################################################
# Help
######################################################################
pathapp, appname = os.path.split(__file__)
VERSION = u"0.1.1"
def help() :
    print (u"""
Help on %(app)s :
-------------------
* %(app)s version %(version)s
    %(app)s --medFile <medFilePath> 
            --groupFile <groupFile> 
            --bcdFile <bcdFile>
* Example :
            python groupFile_medFile_to_BoundaryConditionDataFile.py 
                                             --medFile riviere3D.med 
                                             --groupFile riviere3D.grp 
                                             --bcdFile riviere3D.bcd      
* Options :
    -h :
      Print this help.

    INPUTS:  
    --medFile <medFilePath>
      path to the med file
    --groupFile <groupFile>
      path to the group file

    OUTPUTS:       
    --bcdFile <bcdFile>
      path to the boundary conditions data file
 
""" % ({u"app":appname, u"version" :VERSION  })).decode(u'UTF-8')

######################################################################
# CParams
######################################################################
class CMedFile2BoundaryConditionDataFileException(Exception):
    pass
######################################################################
# CParams
######################################################################
class CParams :
    def Ok(self) :
        ok = True
        msg = u""
        if (self.medFile == None) :
            ok = False
            msg += u"med file is not given!\n"
        elif not os.path.isfile(self.medFile) :
            ok = False
            msg += u"the file '%s' does not exist!\n" % self.medFile             
                        
        if (self.groupFile == None) :
            ok = False
            msg += u"group file is not given!\n"
        elif not os.path.isfile(self.groupFile) :
            ok = False
            msg += u"the file '%s' does not exist!\n" % self.groupFile             
            
        if (self.bcdFile == None) :
            ok = False
            msg += u"boundary conditions data file is not given!\n"
                    
        return ok, msg
 
    def __init__(self):        
        self.medFile = None
        self.groupFile = None
        self.bcdFile = None
######################################################################
# Body
######################################################################
from med.medfile import MEDfileOpen, MEDfileClose, MED_ACC_RDONLY, \
                        MED_NODE , MED_DESCENDING_EDGE, MED_GEO_ALL, \
                        MED_NAME, MED_NO_CMODE, \
                        MED_SEG2, MED_TRIA3, \
                        MED_ALL_ENTITY_TYPE, MED_CONNECTIVITY, \
                        MED_DESCENDING, MED_CELL, MED_NODAL, \
                        MED_NO_GEOTYPE, MED_COORDINATE, \
                        MED_LNAME_SIZE, MEDCHAR, MEDINT, \
                        MED_COMMENT_SIZE, MED_FULL_INTERLACE, \
                        MED_UNDEF_INTERLACE, MEDFLOAT, \
                        MED_NO_INTERLACE, MED_NONE
                        
from med.medmesh import MEDnMesh, MEDmeshInfo, MEDmeshnAxis, \
                        MEDmeshnEntity, MED_NO_DT, MED_NO_IT, \
                        MEDmeshEntityFamilyNumberRd, \
                        MEDmeshEntityNumberRd, \
                        MEDmeshElementConnectivityRd, \
                        MEDmeshNodeCoordinateRd
                        
from med.medfamily import MEDnFamily, MEDnFamilyGroup, \
                        MEDnFamily23Attribute, MEDfamily23Info
                        
######################################################################  
familyIndexberPattern = re.compile("FAM_(.*?)_(.*)")
grpFileLinePattern = re.compile(
                    "\s*([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s*")

######################################################################
def fromfile(filename) :
    f = open(filename, "r")
    text = f.read()
    f.close()
    return text
######################################################################
def medGroupToStr(medGroup):
    gr = ''
    index = 0 
    while (index < MED_LNAME_SIZE) and (medGroup[index] != chr(0)) :
        gr += medGroup[index]
        index += 1
    return gr
######################################################################
class ClCondLimit :
    def __init__(self, LIHBOR, LIUBOR, LIVBOR, GRPNAMEV):
        self.LIHBOR = LIHBOR
        self.LIUBOR = LIUBOR
        self.LIVBOR = LIVBOR
        self.GRPNAMEV = GRPNAMEV
        self.family = list()
        #self.nodeIndexes = None
######################################################################
class CPoint3D:
    def __init__( self,x,y,z ):
        self.x = x 
        self.y = y
        self.z = z
        
def nodeCoordinatesToCPoint3D( nodeCoordinates, index, spaceDim ):
    addr = (index - 1) * spaceDim
    if spaceDim == 3 :
        pnt = CPoint3D( nodeCoordinates[ addr ], \
                        nodeCoordinates[ addr + 1 ], \
                        nodeCoordinates[ addr + 2 ] )
    elif spaceDim == 2 :
        pnt = CPoint3D( nodeCoordinates[ addr ], \
                        nodeCoordinates[ addr + 1 ], 0 )
    return pnt

class CLoop :
    def __init__( self ):
        self.name = None        
        self.borders = list()

    def revert(self):
        for border in self.borders :
            border.revertPointIndexes()
        index = self.borders        
        self.borders = index[::-1]
        
    def lastPntIndex( self ):
        last = None
        if len( self.borders) > 0 :
            last = self.borders[-1].lastPntIndex()
        return last
        
    def isLooped( self ):
        looped = False
        if len( self.borders) > 0 :
            b1 = self.borders[0]
            b2 = self.borders[-1]
            looped = b1.pointIndexes[0] == b2.pointIndexes[-1] 
        return looped
    
    def pointNb( self ):
        nb = 0
        for brd in self.borders :
            nb += len( brd.pointIndexes) - ( len( self.borders )-2 )
        return nb

    def toGnuPlotData(self, filename, nodeCoordinates, spaceDim ):
        datFile= open( filename, "w" )
        pnt1 = None
        pntIndex = 0
        borderIndex=0
        borderNb = len( self.borders )        
        for border in self.borders :            
            if borderIndex == 0 :
                    for pnt in border.pointIndexes :
                        if pntIndex == 0 :
                            pnt1 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt,spaceDim )
                        else :
                            pnt2 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt,spaceDim )                            
                            datFile.write("%s %s %s %s %s %s\n" % \
                                          ( pnt1.x, pnt1.y, pnt1.z, \
                                            pnt2.x-pnt1.x, \
                                            pnt2.y-pnt1.y, \
                                            pnt2.z-pnt1.z ))                       
                            pnt1 = pnt2    
                        pntIndex += 1
                    
            else :
                    for pnt in border.pointIndexes[ 1: ] :
                        if pntIndex == 0 :
                            pnt1 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt,spaceDim )
                        else :
                            pnt2 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt,spaceDim )                            
                            datFile.write("%s %s %s %s %s %s\n" % \
                                          ( pnt1.x, pnt1.y, pnt1.z, \
                                            pnt2.x-pnt1.x, \
                                            pnt2.y-pnt1.y, \
                                            pnt2.z-pnt1.z ))                       
                            pnt1 = pnt2    
                        
                        pntIndex += 1                        
                    
            borderIndex += 1            
        datFile.close()
        
    def isAntiClockWise( self, nodeCoordinates,spaceDim  ):
        sign = 0
        pnt1 = None
        pntIndex = 0
        borderIndex=0
        borderNb = len( self.borders )        
        for border in self.borders :            
            if borderIndex == 0 :
                    for pnt in border.pointIndexes :
                        if pntIndex == 0 :
                            pnt1 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt,spaceDim  )
                        else :
                            pnt2 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt,spaceDim  )
                            
                            sign += pnt1.x * pnt2.y - pnt1.y * pnt2.x       
                            pnt1 = pnt2    
                        pntIndex += 1
                    
            else :
                    for pnt in border.pointIndexes[ 1: ] :
                        if pntIndex == 0 :
                            pnt1 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt,spaceDim  )
                        else :
                            pnt2 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt,spaceDim  )
                            
                            sign += pnt1.x * pnt2.y - pnt1.y * pnt2.x       
                            pnt1 = pnt2                            
                        pntIndex += 1
                    
            borderIndex += 1
        return sign
        
######################################################################
class CBorder :
    def __init__(self, name, condLimit):
        self.name = name        
        self.condLimit = condLimit
        self.segments = list()
        self.pointIndexes = list()
        self.ordered = False
        
    def revertPointIndexes(self):
        index = self.pointIndexes
        self.pointIndexes = index[::-1]
            
    def lastPntIndex(self):
        last = None
        if len(self.pointIndexes)>0 :
            last = self.pointIndexes[-1]
        return last
        
    def isLooped(self):
        looped = False
        if len(self.pointIndexes)>1 :
            looped = self.pointIndexes[0] == self.pointIndexes[-1] 
        return looped
        
    def findSegIndexForPoint( self, pntIndex, seg2Connectivity):
        segIndex0 = None
        segPntIndex = None
        segNb = len( self.segments )
        
        segIndex = 0
        notFound = True
        
        while notFound and (segIndex < segNb ) :
            
            seg = self.segments[ segIndex ]
            pnt = seg2Connectivity[seg * 2 ]
            
            if pnt == pntIndex :
                notFound = False
                segIndex0 = segIndex
                segPntIndex = 0
            else :
                pnt = seg2Connectivity[ seg * 2 + 1 ]
                if pnt == pntIndex :
                    notFound = False
                    segIndex0 = segIndex
                    segPntIndex = 1
            segIndex += 1            
        return segIndex0, segPntIndex    

    def createOrderedPointIndexes( self, \
                                   segIndex, \
                                   segPntIndex, \
                                   seg2Connectivity ):
        """
segIndex and segPntIndex must point to the end points of the edge, otherwise no 
values are to be set (None).
        """            
        if not self.ordered :
            self.ordered = True
            segNb = len( self.segments )
            
            if segIndex == None :
                # we have to find one of two border points
                notFound = True
                segIndex1 = 0
                while notFound and ( segIndex1 < segNb ) :
                    seg = self.segments[ segIndex1 ]
                    pntIndex1 = 0
                    while notFound and ( pntIndex1 < 2 ) :
                        pnt1 = seg2Connectivity[ seg * 2 + pntIndex1 ]
                        pntCount = 0
                        segIndex2 = 0
                        while notFound and ( segIndex2 < segNb ) :
                            seg2 = self.segments[ segIndex2 ]
                            pntIndex2 = 0
                            while notFound and ( pntIndex2 < 2 ) :
                                pnt2 = seg2Connectivity[ seg2 * 2 + pntIndex2 ]
                                if pnt2 == pnt1 :
                                    pntCount += 1
                                if notFound :
                                    pntIndex2 += 1                    
                            if notFound :
                                segIndex2 += 1                       
                        if pntCount == 1 :
                            notFound = False 
                            segIndex =  segIndex1
                            segPntIndex = pntIndex1
                            
                        if notFound :
                            pntIndex1 += 1                    
                    if notFound :
                        segIndex1 += 1
                if notFound :
                    # the border is looped
                    segIndex =  0
                    segPntIndex = 0
            loopInc = 0
            while loopInc < segNb :
                loopInc += 1
                seg = self.segments[ segIndex ]
                pnt = seg2Connectivity[seg * 2 + segPntIndex ]
                self.pointIndexes.append( pnt )
                
                if loopInc == segNb :
                    if segPntIndex == 0 :
                        pnt2 = seg2Connectivity[seg * 2 + 1 ]
                    else :
                        pnt2 = seg2Connectivity[seg * 2  ]
                    self.pointIndexes.append( pnt2 )
                    
                        
                if segPntIndex == 0 :
                    pntToFind = seg2Connectivity[seg * 2 + 1 ]
                else :
                    pntToFind = seg2Connectivity[seg * 2  ]
                    
                segIndToFind = 0
                notFound = loopInc < segNb
                while notFound and ( segIndToFind < segNb ) :
                    if segIndToFind != segIndex :    
                        seg2 = self.segments[ segIndToFind ]
                        if seg2Connectivity[seg2 * 2] == pntToFind :
                            notFound = False 
                            segIndex = segIndToFind
                            segPntIndex = 0
                        elif seg2Connectivity[seg2 * 2 + 1 ] == \
                                                           pntToFind :
                            notFound = False 
                            segIndex = segIndToFind
                            segPntIndex = 1
                        else :
                            pass                        
                    segIndToFind += 1
                    
                                
    def toGnuPlotData(self, filename, seg2Connectivity, \
                                          nodeCoordinates, spaceDim ):
        datFile= open( filename, "w" )
        
        
        for seg in self.segments :
            pnt1Index = seg2Connectivity[seg * 2  ]
            pnt2Index = seg2Connectivity[seg * 2 + 1 ]
            pnt1 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt1Index, spaceDim )
            pnt2 = nodeCoordinatesToCPoint3D( \
                                              nodeCoordinates, \
                                              pnt2Index, spaceDim )
            datFile.write("%s %s %s %s %s %s\n" % (pnt1.x, pnt1.y, \
                                                   pnt1.z, \
                                                   pnt2.x-pnt1.x, \
                                                   pnt2.y-pnt1.y, \
                                                   pnt2.z-pnt1.z ))
            
        datFile.close()
                    
            
######################################################################
class CBoundaryLimitData :
    def __init__(self):
        self.spaceDimension = 0
        self.nodeNumber = None
        self.nodeCoordinates = None
        self.seg2nodeNumber = None
        self.seg2Connectivity = None        
        self.lftBtmBrdIndex = None
        self.lftBtmBrdSegIndex = None
        self.lftBtmBrdSegPntIndex = None
        self.borderList = list()
                 
######################################################################
def getConnectivity( medFileId, \
                     meshName, \
                     geotype, \
                     connectivityElemSize ):
    nodeNumber, chgt, trsf = MEDmeshnEntity(medFileId, meshName, \
                             MED_NO_DT, MED_NO_IT, MED_CELL, geotype,\
                             MED_CONNECTIVITY, MED_NODAL)
    connectivity = MEDINT(nodeNumber * connectivityElemSize)
    MEDmeshElementConnectivityRd( medFileId, meshName, MED_NO_DT, \
                                  MED_NO_IT, MED_CELL , geotype , \
                                  MED_NODAL, MED_FULL_INTERLACE , \
                                  connectivity)
    return nodeNumber, connectivity        
######################################################################
def loadGroupFile(groupFileName):
    limitConditions = dict()    
    lines = fromfile(groupFileName).split("\n")
    nb = int(lines[0])
    for line in lines[1:]:
        if(line != ''):
            LIHBOR, LIUBOR, LIVBOR, GRPNAMEV = \
                                  grpFileLinePattern.findall(line)[0]
            limitConditions[GRPNAMEV] = ClCondLimit( \
                                  LIHBOR, LIUBOR, LIVBOR, GRPNAMEV)
    return limitConditions
######################################################################
def loadMedFile(medFileName, limitConditions):
    nodeCoordinates = None

    leftBottomX = sys.float_info.max
    leftBottomY = sys.float_info.max
    
    boundaryLimitData = CBoundaryLimitData()
    
    ##################################################################
    medFileId = MEDfileOpen(medFileName, MED_ACC_RDONLY)    
    meshNumber = MEDnMesh(medFileId)

    if meshNumber > 1 :
        raise CMedFile2BoundaryConditionDataFileException( \
                                             "Med file must have only one mesh")
    
    for meshNum in range(1, meshNumber + 1):
        
        meshName, spaceDim, meshDim, meshtype, desc, dtunit, sort, \
                  nstep, repere, axisname, axisunit = \
                  MEDmeshInfo(medFileId , meshNum)
                  
        axisNumber = MEDmeshnAxis(medFileId, meshNum)
        
        meshName, spaceDimension, meshDimension, meshType, \
                  description, deltaTimeUnit, sortOrder, stepNumber, \
                  coordinateSystem, axisNames, axisUnit = \
                  MEDmeshInfo(medFileId , meshNum)
                  
        nodeNumber, chgt, trsf = MEDmeshnEntity(medFileId, meshName, \
                                 MED_NO_DT, MED_NO_IT, MED_NODE, \
                                 MED_NONE, MED_COORDINATE, \
                                 MED_NO_CMODE)
        
        nodeCoordinates = MEDFLOAT(nodeNumber * spaceDim)
        
        MEDmeshNodeCoordinateRd( medFileId, meshName, MED_NO_DT, \
                                 MED_NO_IT, MED_FULL_INTERLACE, \
                                 nodeCoordinates)
        
        nodeFamily = MEDINT(nodeNumber)
        
        nodeFamily = MEDmeshEntityFamilyNumberRd( medFileId, \
                     meshName, MED_NO_DT, MED_NO_IT, MED_NODE, \
                     MED_NO_GEOTYPE, nodeFamily)
        
        seg2nodeNumber, seg2Connectivity = \
                    getConnectivity( medFileId, meshName, MED_SEG2, 2)
                    
        seg2NodeFamily = MEDINT(seg2nodeNumber)
        
        seg2NodeFamily = MEDmeshEntityFamilyNumberRd( medFileId, \
                         meshName, MED_NO_DT, MED_NO_IT, MED_CELL, \
                         MED_SEG2, seg2NodeFamily)
        
        seg2NodeNumber = MEDINT(seg2nodeNumber)
        
        seg2NodeNumber = MEDmeshEntityNumberRd( medFileId, meshName, \
                         MED_NO_DT, MED_NO_IT, MED_CELL, MED_SEG2, \
                         seg2NodeNumber)
        
        seg3nodeNumber, seg3Connectivity = \
                         getConnectivity( medFileId, meshName, \
                                          MED_TRIA3, 3)
                         
        familyIndexber = MEDnFamily(medFileId, meshName)
        
        ##############################################################
        for familyIndex in xrange(1, familyIndexber + 1):
                        
          groupNumber = MEDnFamilyGroup(medFileId, meshName, \
                                        familyIndex)
          
          attributeNumber = MEDnFamily23Attribute( medFileId, \
                                                meshName, familyIndex)
           
          attide = MEDINT(attributeNumber)          
          attval = MEDINT(attributeNumber)
          attdes = MEDCHAR(MED_COMMENT_SIZE * attributeNumber + 1)
          groups = MEDCHAR(MED_LNAME_SIZE * groupNumber + 1)
                  
          nomfam, numfam, attide, attval, attdes, groups = \
                  MEDfamily23Info( medFileId, meshName, familyIndex, \
                                   attide, attval, attdes, groups)
                  
          famNumNameArray = familyIndexberPattern.findall( nomfam )
          
          familyNum = familyIndex 
          familyName = nomfam 
          if len(famNumNameArray) > 0 :
               familyNum, familyName = famNumNameArray[0]
          
          ############################################################     
          for groupNum in xrange(0, groupNumber):
                            
              groupName = medGroupToStr( \
                           groups[groupNum * MED_LNAME_SIZE:groupNum \
                                   * MED_LNAME_SIZE + MED_LNAME_SIZE])
                            
              if groupName in limitConditions :
                  condLimit = limitConditions[ groupName ]
                  if not familyIndex in condLimit.family :
                      condLimit.family.append(int(familyNum))
                      
        ##############################################################
        
        borderIndex = -1
        for borderName, limitCond in limitConditions.items() :
            borderIndex += 1            
            fam = limitCond.family
            border  = CBorder( borderName, limitCond )            
            boundaryLimitData.borderList.append( border )
             
            borderSegmentIndex = -1 
            for seg2 in range(0, seg2nodeNumber) :                                
                
                segfam = int(seg2NodeFamily[ seg2 ])                            
                if segfam in fam  :
                        
                    border.segments.append( seg2 )
                    borderSegmentIndex += 1
                    
                    point1Index = seg2Connectivity[ seg2 * 2 ]
                    point2Index = seg2Connectivity[ seg2 * 2 + 1 ] 
                    
                    addr1 = ( point1Index - 1) * spaceDim
                    addr2 = ( point2Index - 1) * spaceDim
                    
                    point1X = nodeCoordinates[ addr1 ]                        
                    point1Y = nodeCoordinates[ addr1+1 ]
                    
                    point2X = nodeCoordinates[ addr2 ]                        
                    point2Y = nodeCoordinates[ addr2+1 ]

                    if spaceDim == 3 :
                        point1Z = nodeCoordinates[ addr1+2 ]
                        point2Z = nodeCoordinates[ addr2+2 ]                        
                    elif spaceDim == 2 :
                        point1Z = 0
                        point2Z = 0                        
                    else :
                        pass

                    
                    if ( leftBottomX > point1X ) or \
                       (leftBottomY>point1Y):
                        
                        leftBottomX = point1X
                        leftBottomY = point1Y
                        
                        boundaryLimitData.lftBtmBrdIndex = borderIndex
                        boundaryLimitData.lftBtmBrdSegIndex = \
                                                    borderSegmentIndex
                        boundaryLimitData.lftBtmBrdSegPntIndex = 0
                         
                    if ( leftBottomX > point2X ) or \
                       ( leftBottomY > point2Y ):
                        
                        leftBottomX = point2X
                        leftBottomY = point2Y
                        
                        boundaryLimitData.lftBtmBrdIndex = borderIndex
                        boundaryLimitData.lftBtmBrdSegIndex = \
                                                    borderSegmentIndex                                                
                        boundaryLimitData.lftBtmBrdSegPntIndex = 1
                                        
        
    MEDfileClose(medFileId)
    
## Look for the nearest node to (leftBottomX,leftBottomY) following the axe(y=x)
#    diagDistance = sys.float_info.max
#    borderIndex = -1
#    for border in boundaryLimitData.borderList :
#        borderIndex += 1
#        borderSegmentIndex = -1
#        for seg in border.segments :
#            borderSegmentIndex += 1
#            point1Index = seg2Connectivity[ seg * 2 ]
#            point2Index = seg2Connectivity[ seg * 2 + 1 ] 
#
#            addr1 = ( point1Index - 1) * spaceDim
#            addr2 = ( point2Index - 1) * spaceDim
#
#            pointX = nodeCoordinates[ addr1 ]                        
#            pointY = nodeCoordinates[ addr1+1 ]            
#            diagX = (pointX-leftBottomX)+(pointX-leftBottomX)
#            diagY = (pointY-leftBottomY)+(pointY-leftBottomY)            
#            diagD= diagX*diagX+diagY*diagY
#            if diagD < diagDistance :
#                diagDistance = diagD
#                boundaryLimitData.lftBtmBrdIndex = borderIndex
#                boundaryLimitData.lftBtmBrdSegIndex = borderSegmentIndex                                                
#                boundaryLimitData.lftBtmBrdSegPntIndex = 0
#
#            pointX = nodeCoordinates[ addr2 ]
#            pointY = nodeCoordinates[ addr2+1 ]
#            diagX = (pointX-leftBottomX)+(pointY-leftBottomY)
#            diagY = diagX
#            diagD= diagX*diagX+diagY*diagY
#            if diagD < diagDistance :
#                diagDistance = diagD
#                boundaryLimitData.lftBtmBrdIndex = borderIndex
#                boundaryLimitData.lftBtmBrdSegIndex = borderSegmentIndex                                                
#                boundaryLimitData.lftBtmBrdSegPntIndex = 0
    
    
    ##################################################################
    boundaryLimitData.nodeNumber = nodeNumber
    boundaryLimitData.nodeCoordinates = nodeCoordinates
    boundaryLimitData.seg2nodeNumber = seg2nodeNumber
    boundaryLimitData.seg2Connectivity = seg2Connectivity
    boundaryLimitData.spaceDimension = spaceDimension
    return boundaryLimitData
           
######################################################################
def writeBcdLine( bcdFile, pntLineIndex, border, pnt, colorNumber  ):
    if pntLineIndex == 0 :
        comment = " # %s %d - %d" %( \
            border.name, \
            border.pointIndexes[0], \
            border.pointIndexes[-1])
    else :
      comment = ""
    lineText = "%d %d %d  " \
        +"0.000 0.000 0.000 0.000  2  " \
        +"0.000 0.000 0.000  %11d %11d%s\n"  
    bcdFile.write( lineText  % \
    ( int( border.condLimit.LIHBOR ),\
      int( border.condLimit.LIUBOR ),\
      int( border.condLimit.LIVBOR ),\
      pnt, \
      colorNumber,comment ) )
    
def saveToBcdFile( bcdFilePath, bldData ):
    loops =list()
    loop = CLoop()
    loops.append( loop )
    
    
    
    externalBorder = bldData.borderList[ \
                                    bldData.lftBtmBrdIndex ]    
    loop.borders.append( externalBorder )
    loop.name = externalBorder.name
    
    del bldData.borderList[ bldData.lftBtmBrdIndex ]
    externalBorder.createOrderedPointIndexes( \
                    None, \
                    None, \
                    bldData.seg2Connectivity )
    borderNb= len( bldData.borderList )
            
    while borderNb > 0 :
        borderIndex = 0
        notFound = not loop.isLooped()
         
        while notFound and ( borderIndex < borderNb ) :
            
            border2 = bldData.borderList[ borderIndex ]
                
            segIndex, segPntIndex = border2.findSegIndexForPoint( \
                                        loop.lastPntIndex() , \
                                        bldData.seg2Connectivity )
            
            if segIndex != None :
                
                loop.name += "_"+border2.name
                
                border2.createOrderedPointIndexes( segIndex, \
                               segPntIndex, bldData.seg2Connectivity )
                loop.borders.append( border2  )
                del bldData.borderList[ borderIndex ]
                borderNb= len( bldData.borderList )
                borderIndex = 0 
            else :
                borderIndex += 1
                
            notFound = not loop.isLooped()
                        
        borderNb= len( bldData.borderList )
        if borderNb > 0 :
            loop = CLoop()
            loops.append( loop )
                        
            border = bldData.borderList[0]
            
            loop.name = border.name 
            loop.borders.append( border )
            
            
            border.createOrderedPointIndexes( 0, 0, \
                                            bldData.seg2Connectivity )
                                            
                                            
            del bldData.borderList[ 0 ]
            borderNb= len( bldData.borderList )
        
                
    bcdFile = open( bcdFilePath, "w" )
    colorNumber = 1
    loopIndex=0
    for loop in loops :
        borderIndex=0
        sarea = loop.isAntiClockWise( bldData.nodeCoordinates, \
                                      bldData.spaceDimension )        
        if loopIndex==0 :
            if sarea < 0 :
               loop.revert()  
        else :
            if sarea > 0 :
               loop.revert()  
        
        borderNb = len( loop.borders )        
        for border in loop.borders :            
            
            if borderIndex == 0 :
                if borderIndex == borderNb-1 :
                    pntLineIndex = 0                
                    for pnt in border.pointIndexes[ :-1 ] :
                        writeBcdLine( bcdFile, pntLineIndex, border,\
                                      pnt, colorNumber  )                        
                        pntLineIndex += 1
                        colorNumber += 1
                else :
                    
                    pntLineIndex = 0                
                    for pnt in border.pointIndexes :
                        writeBcdLine( bcdFile, pntLineIndex, border,\
                                      pnt, colorNumber  )                        
                        pntLineIndex += 1
                        colorNumber += 1                
            else :
                if borderIndex == borderNb-1 :
                    
                    pntLineIndex = 0
                    for pnt in border.pointIndexes[ 1:-1 ] :                        
                        writeBcdLine( bcdFile, pntLineIndex, border,\
                                      pnt, colorNumber  )                        
                        pntLineIndex += 1
                        colorNumber += 1  
                else :
                    
                    pntLineIndex = 0
                    for pnt in border.pointIndexes[ 1: ] :
                        writeBcdLine( bcdFile, pntLineIndex, border,\
                                      pnt, colorNumber  )                        
                        pntLineIndex += 1
                        colorNumber += 1
                    
            borderIndex += 1
        loopIndex += 1
    bcdFile.close()
######################################################################  
def conversionToBcd(params) :
    limitConditions = loadGroupFile( params.groupFile )
    boundaryLimitData = loadMedFile( params.medFile, limitConditions)
    saveToBcdFile( params.bcdFile, boundaryLimitData )
 
######################################################################
# Main
######################################################################
if __name__ == u'__main__' :
    params = CParams()
    short = u"h"
    long = [ u"medFile=", u"groupFile=", u"bcdFile=" ]
    dohelp = 0
    opts, args = getopt.getopt(sys.argv[1:], short, long)
    if len(opts) > 0 :
        _opts = list()
        for opt, val in opts:
            _opts.append(opt)
        for opt, val in opts:
            if opt == u'-h' :
                dohelp = 1
                
            if opt == u'--medFile' :
                params.medFile = val
            if opt == u'--groupFile' :
                params.groupFile = val
                                
            if opt == u'--bcdFile' :
                params.bcdFile = val
            else :
                pass
    if dohelp :
        help()
    else :
        ok, msg = params.Ok()
        if (ok) :
            conversionToBcd(params)
        else :
            help()
            for ms in msg.split(u'\n') :
                if ms.strip() != "" : 
                    print u"ERROR : %s" % ms
            exit(1)
