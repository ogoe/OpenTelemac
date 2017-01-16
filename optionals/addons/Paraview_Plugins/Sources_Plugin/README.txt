----------------------------------------
# ~~ TELEMAC SELAFIN PLUGIN IN PARAVIEW:
----------------------------------------
1) First release (F. Decung : 29/04/2012)

Please find the "sources" for the TELEMAC data plugin (Selafin format) in Paraview.
It has been initially developped by R. Nebauer (EDF) and H. Ozdoba (Internship).
Updated by John Floyd for version 4.x of Paraview

Prerequisites:
--------------
a) Paraview comes with CMAKE makefile (~CMakeLists.txt files). Freely available on both Linux and Windows.
b) The QT library is resquested for compiling the plugin sources. The library is often pre-installed on many Linux flavours.
However, on Windows, it is necessary to install this library before running CMAKE.
c) Third party libraries as ffmpeg and Qt are disabled here. I encountered some "bundle" errors.

Compilation :
-------------
a) The TELEMAC plugin sources (SerafinTELEMAC directory) must be placed in the "Plugins" directory.
b) run cmake .;make
c) Add the plugin in Paraview Tools->Manage Plugins-> Load New

