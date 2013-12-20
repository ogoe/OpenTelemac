# CMAKE : Configure with Unix Makefile, enabling shared libs and disabling Third Party Libraries (bundles error...)
cmake ../ParaView-3.10.1/ -G "Unix Makefiles" \
-DBUILD_SHARED_LIBS=ON \
-DPARAVIEW_BUILD_PLUGIN_SerafinTELEMAC=ON \
-DCMAKE_INSTALL_PREFIX=./installed \
-DPARAVIEW_BUILD_QT_GUI=ON \
-DPARAVIEW_INSTALL_THIRD_PARTY_LIBRARIES=OFF

# Parallel make & Install
make -j 4 && make install

# It may be necessary sometimes to export the PV_PLUGIN_PATH variable
# export PV_PLUGIN_PATH=${PATH_TO_PARAVIEW_INSTALL_DIR}/lib/paraview...
# or to configure in $HOME/.config/ParaView
