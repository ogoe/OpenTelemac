#------------------------------makefile------------------------DeltaCAD
#
# Makefile de TELEMAC-MASCARET pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   sources/stbtel.
#
# Modes supportes :
#        % make
#        % make  [all] [install] [clean]
#----------------------------------------------------Aout-1999-DeltaCAD
#
# definition des noms de fichiers
#
#
include makefile.in
#
MODULES= utils/special \
 utils/damocles \
 utils/parallel \
 utils/hermes \
 utils/bief \
 utils/diffsel \
 utils/splitsel \
 utils/partel \
 utils/gretel \
 nestor \
 artemis \
 tomawac \
 sisyphe \
 stbtel \
 postel3d \
 waqtel \
 telemac2d \
 telemac3d \
 api \
 utils/ad \
 mascaret
#

all: dist metis
	  @for module in ${MODULES}; do\
		  $(MAKE) install -C sources/$${module};\
			if [ $$? -ne 0 ] ; then \
			  echo "Compilation crashed";\
		    return ;\
		  fi;\
		done
		@echo "Compilation done"

clean: clean_metis
	  @for module in ${MODULES}; do\
		  $(MAKE) clean -C sources/$${module};\
		done

distclean:
	  @echo "Deleting $(DEST)"
		@rm -rf $(DEST)
		@rm -rf sources/utils/ad/list.txt \
			sources/mascaret/install \
      sources/mascaret/Fox/config.status \
      sources/mascaret/Fox/config.log \
      sources/mascaret/Fox/arch.make \
      optionals/metis-5.1.0/programs/cmake_install.cmake \
      optionals/metis-5.1.0/programs/Makefile \
      optionals/metis-5.1.0/programs/CMakeFiles/ \
      optionals/metis-5.1.0/libmetis/cmake_install.cmake \
      optionals/metis-5.1.0/libmetis/Makefile \
      optionals/metis-5.1.0/libmetis/CMakeFiles/ \
      optionals/metis-5.1.0/include/cmake_install.cmake \
      optionals/metis-5.1.0/include/Makefile \
      optionals/metis-5.1.0/include/CMakeFiles/ \
      optionals/metis-5.1.0/cmake_install.cmake \
      optionals/metis-5.1.0/Makefile \
      optionals/metis-5.1.0/CMakeFiles/ \
      optionals/metis-5.1.0/CMakeCache.txt

metis:
	  @cd ${HOMETEL}/optionals/metis-5.1.0 && cmake . && make
		@cp -v ${HOMETEL}/optionals/metis-5.1.0/libmetis/libmetis.a $(DEST)/lib

clean_metis:
	  @cd ${HOMETEL}/optionals/metis-5.1.0 && make clean

dist:
	  @if [ ! -d $(DEST) ] ; then \
			echo "Creating $(DEST) install directory";\
			mkdir $(DEST) $(DEST)/lib $(DEST)/bin $(DEST)/include;\
		fi

help:
	  @echo "Usage make [all] [clean|distclean] [install]"
		@echo "available options :"
		@echo "          all : compile everything (default)"
		@echo "          install : copy compiled file in install dir"
		@echo "          clean : delete local compiled object"
		@echo "          distclean : delete install directory"
		@echo "          metis : compile metis"
		@echo "          all : compile everything (default)"
		@echo "          all : compile everything (default)"
