CMAKE_MINIMUM_REQUIRED(VERSION 2.4)

PROJECT(SerafinClientPlugin)

FIND_PACKAGE(Qt4)
IF(QT4_FOUND)

INCLUDE(${QT_USE_FILE})

INCLUDE_DIRECTORIES(
  ${VTK_INCLUDE_DIR}
  ${PARAVIEW_INCLUDE_DIRS}
  ${ParaView_SOURCE_DIR}/VTK/GUISupport/Qt
  ${pqComponents_SOURCE_DIR}
  ${pqComponents_BINARY_DIR}
  ${pqCore_SOURCE_DIR}
  ${pqCore_BINARY_DIR}
  ${QtWidgets_SOURCE_DIR}
  ${QtWidgets_BINARY_DIR}
  ${QtChart_SOURCE_DIR}
  ${QtChart_BINARY_DIR}
  ${QtTesting_SOURCE_DIR}
  ${QtTesting_BINARY_DIR}
  ${QtPython_SOURCE_DIR}
  ${QtPython_BINARY_DIR}
)

ADD_PARAVIEW_PLUGIN(SerafinClientPlugin "0.1" 
		    GUI_RESOURCES Resources/Serafin.qrc)

ENDIF(QT4_FOUND)