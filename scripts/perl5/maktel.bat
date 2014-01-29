@echo off
rem --------------------------------------------------
rem 
rem  Lancement de MAKE pour SYSTEL sous WINDOWS
rem 
rem ---------------------------DeltaCAD-Septembre98---
rem
rem
rem Please activate the following line if you are using INTEL or COMPAQ compiler
rem
@FOR /F "tokens=*" %%i in ('getsystelcfg') DO set mySYSTELCFG=%%i
echo mySYSTELCFG %mySYSTELCFG%
@FOR /F "tokens=*" %%i in ('getproject') DO set myHOMETEL=%%i
echo myHOMETEL %myHOMETEL%
@FOR /F "tokens=*" %%i in ('gethosttype') DO set myHOSTTYPE=%%i
set myDEST=%myHOMETEL%\builds\%myHOSTTYPE%
@if not exist %myDEST% mkdir %myDEST% %myDEST%\lib %myDEST%\bin
rem
rem Please activate the following line if you are using INTEL or COMPAQ compiler
rem
  nmake /f makefile.wnt %1 %2 %3
rem
rem Please activate the following line if you are using G95 compiler
rem
rem  nmake /f makefile.gfo %1 %2 %3
rem
rem Please activate the following line if you are using GFORTRAN compiler
rem
rem gfomake /f makefile.gfo %1 %2 %3
rem
rem   End of Maktel.bat
rem
rem   End of Maktel.bat
