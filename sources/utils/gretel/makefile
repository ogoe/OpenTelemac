#
# Makefile for Unix.
#
include $(HOMETEL)/makefile.in
# Get SRCS and LIBS
include makefile.am
#
LIBRARY=libgretel.a
EXE_GRETEL=gretel
EXE_GREDELPTS=gretelpts
EXE_GREDELSEG=gretelseg
EXE_GREDELMET=gretelmet
EXE_GREDELHYD=gretelhyd

# Get list of files
TMP_GRETEL=$(SRCS_GRETEL:.f=.o)
OBJS_GRETEL=$(TMP_GRETEL:.F=.o)

TMP_GREDELPTS=$(SRCS_GREDELPTS:.f=.o)
OBJS_GREDELPTS=$(TMP_GREDELPTS:.F=.o)

TMP_GREDELSEG=$(SRCS_GREDELSEG:.f=.o)
OBJS_GREDELSEG=$(TMP_GREDELSEG:.F=.o)

TMP_GREDELMET=$(SRCS_GREDELMET:.f=.o)
OBJS_GREDELMET=$(TMP_GREDELMET:.F=.o)

TMP_GREDELHYD=$(SRCS_GREDELHYD:.f=.o)
OBJS_GREDELHYD=$(TMP_GREDELHYD:.F=.o)

LKLIB=$(LIBS) $(FC_LIBS)
FFLAGS=$(FCFLAGS) -I$(DEST)/include

LIBRARIES=$(LIBRARY)
EXES=$(EXE_GRETEL) $(EXE_GREDELPTS) $(EXE_GREDELSEG) $(EXE_GREDELMET) $(EXE_GREDELHYD)
#
# Defining rules
#

all: $(LIBRARIES) $(EXES)

$(LIBRARY):	$(OBJS)
		@$(MESSAGE) "Archive $@ ..."
		@$(LIB_NAM) $@ $^

$(EXE_GRETEL):	$(OBJS_GRETEL)
		@$(MESSAGE) "Link executable $@ ..."
		@$(LD) $(LDFLAGS) -o $@ $^ $(LKLIB)

$(EXE_GREDELPTS):	$(OBJS_GREDELPTS)
		@$(MESSAGE) "Link executable $@ ..."
		@$(LD) $(LDFLAGS) -o $@ $^ $(LKLIB)

$(EXE_GREDELSEG):	$(OBJS_GREDELSEG)
		@$(MESSAGE) "Link executable $@ ..."
		@$(LD) $(LDFLAGS) -o $@ $^ $(LKLIB)

$(EXE_GREDELMET):	$(OBJS_GREDELMET)
		@$(MESSAGE) "Link executable $@ ..."
		@$(LD) $(LDFLAGS) -o $@ $^ $(LKLIB)

$(EXE_GREDELHYD):	$(OBJS_GREDELHYD)
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
