@echo off
rem --------------------------------------------------
rem 
rem  Lancement de MAKE pour SYSTEL sous WINDOWS
rem 
rem ---------------------------DeltaCAD-Septembre98---
rem
IF NOT DEFINED SYSTELCFG SET SYSTELCFG=R:\tm52\config
rem
nmake /f makefile.wnt %1 %2 %3
