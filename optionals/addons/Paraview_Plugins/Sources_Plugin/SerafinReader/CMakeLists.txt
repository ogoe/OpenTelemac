cmake_minimum_required(VERSION 3.4)

SET(ParaView_DIR /home/jfloyd/Downloads/Paraview/ParaView-v4.4.0-source/Build)

FIND_PACKAGE(ParaView REQUIRED)

INCLUDE(${PARAVIEW_USE_FILE})
 ADD_PARAVIEW_PLUGIN(SerafinReader "2.0" 
   SERVER_MANAGER_XML SerafinReader.xml
   SERVER_MANAGER_SOURCES vtkSerafinReader.cxx 
   REQUIRED_ON_SERVER)
