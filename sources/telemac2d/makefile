#
# Makefile for Unix.
#
include $(HOMETEL)/makefile.in
# Get SRCS and LIBS
include makefile.am
#
LIBRARY=libtelemac2d.a
EXE=telemac2d

# Get list of files
TMP=$(SRCS:.f=.o)
OBJS=$(TMP:.F=.o)

LKLIB=$(LIBS) $(FC_LIBS)
FFLAGS=$(FCFLAGS) -I$(DEST)/include

LIBRARIES=$(LIBRARY)
EXES=$(EXE)
#
# Defining rules
#

all: $(LIBRARIES) $(EXES)

$(LIBRARY):	$(OBJS)
		@$(MESSAGE) "Archive $@ ..."
		@$(LIB_NAM) $@ $^

$(EXE):	$(OBJS)
		@$(MESSAGE) "Link executable $@ ..."
		@$(LD) $(LDFLAGS) -o $@ $^ $(LKLIB)

clean:
	  @$(MESSAGE) "Efface les objets, librairies, executables et core-file."
		@rm -f $(OBJS) $(LIBRARIES) $(EXES) *.mod *.MOD *.g90 core

install:	$(LIBRARIES) $(EXES)
	  @for lib in $(LIBRARIES); do \
		  rm -f $(DEST)/lib/$${lib};\
	    cp -v $${lib} $(DEST)/lib;\
	  done;
	  @for exe in $(EXES); do \
		  rm -f $(DEST)/bin/$${exe};\
	    cp -v $${exe} $(DEST)/bin;\
	  done;
