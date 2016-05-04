----------------------------------------
# ~~ TELEMAC SELAFIN PLUGIN IN PARAVIEW:
----------------------------------------
1) First release (F. Decung : 29/04/2012)

Please find the "sources" for the TELEMAC data plugin (Selafin format) in Paraview.
It has been initially developped by R. Nebauer (EDF) and H. Ozdoba (Internship).

Prerequisites:
--------------
a) Paraview comes with CMAKE makefile (~CMakeLists.txt files). Freely available on both Linux and Windows.
b) The QT library is resquested for compiling the plugin sources. The library is often pre-installed on many Linux flavours.
However, on Windows, it is necessary to install this library before running CMAKE.
c) Third party libraries as ffmpeg and Qt are disabled here. I encountered some "bundle" errors.

Compilation :
-------------
a) Paraview must be entirely compiled from scratch. Sources can be downloaded at www.paraview.org.
b) The TELEMAC plugin sources (SerafinTELEMAC directory) must be placed in the "Plugins" directory.
c) The CMakeLists.txt file in the Plugins directory must be edited. The following line must be added :
> "paraview_build_optional_plugin(SerafinTELEMAC "SerafinTELEMAC" SerafinTELEMAC ON)"
d) Create a 'build' directory at the same level as Paraview sources and perform an 'Out of Sources' configuration & compilation (see Script.sh).
> cmake ../ParaView-3.10.1/ -G "Unix Makefiles" \
-DBUILD_SHARED_LIBS=ON \
-DPARAVIEW_BUILD_PLUGIN_SerafinTELEMAC=ON \
-DCMAKE_INSTALL_PREFIX=./installed \
-DPARAVIEW_BUILD_QT_GUI=ON \
-DPARAVIEW_INSTALL_THIRD_PARTY_LIBRARIES=OFF
In this example, it will configure and create makefiles with the TELEMAC plugin, shared libs, no third Party libraries and will install it in "./installed".
You can also configure manually with the ccmake-gui (on Linux). You must sometimes access to advanced cache values (press t ~ toggle on Linux).
e) If, configure is succesfull, then run make && make install.

Global variables:
-----------------
It may be necessary sometimes to export the PV_PLUGIN_PATH variable
> export PV_PLUGIN_PATH=${PATH_TO_PARAVIEW_INSTALL_DIR}/lib/paraview...
or to configure in $HOME/.config/ParaView
