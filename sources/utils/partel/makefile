#
# Makefile for Unix.
#
include $(HOMETEL)/makefile.in
# Get SRCS and LIBS
include makefile.am
#
LIBRARY_PARTEL=libpartel.a
EXE_PARTEL=partel

LIBRARY_PARTEL_PARA=libpartel_para.a
EXE_PARTEL_PARA=partel_para

LIBRARY_PARTEL_PRELIM=libpartel_prelim.a
EXE_PARTEL_PRELIM=partel_prelim

# Get list of files
TMP=$(SRCS_PARTEL:.f=.o)
OBJS_PARTEL=$(TMP:.F=.o)

TMP1=$(SRCS_PARTEL_PARA:.f=.o)
OBJS_PARTEL_PARA=$(TMP1:.F=.o)

TMP2=$(SRCS_PARTEL_PRELIM:.f=.o)
OBJS_PARTEL_PRELIM=$(TMP2:.F=.o)

OBJS= $(OBJS_PARTEL_PRELIM) $(OBJS_PARTEL_PARA) $(OBJS_PARTEL)

LKLIB=$(LIBS) $(FC_LIBS)
FFLAGS=$(FCFLAGS) -I$(DEST)/include

LIBRARIES=$(LIBRARY_PARTEL) $(LIBRARY_PARTEL_PARA) $(LIBRARY_PARTEL_PRELIM)
EXES=$(EXE_PARTEL) $(EXE_PARTEL_PARA) $(EXE_PARTEL_PRELIM)
#
# Defining rules
#

all: $(LIBRARIES) $(EXES)

$(LIBRARY_PARTEL):	$(OBJS_PARTEL)
		@$(MESSAGE) "Archive $@ ..."
		@$(LIB_NAM) $@ $^

$(EXE_PARTEL):	$(OBJS_PARTEL)
		@$(MESSAGE) "Link executable $@ ..."
		@$(LD) -o $@ $^ $(LKLIB) $(LDFLAGS)

$(LIBRARY_PARTEL_PARA):	$(OBJS_PARTEL_PARA)
		@$(MESSAGE) "Archive $@ ..."
		@$(LIB_NAM) $@ $^

$(EXE_PARTEL_PARA):	$(OBJS_PARTEL_PARA)
		@$(MESSAGE) "Link executable $@ ..."
		@$(LD) -o $@ $^ $(LKLIB) $(LDFLAGS)

$(LIBRARY_PARTEL_PRELIM):	$(OBJS_PARTEL_PRELIM)
		@$(MESSAGE) "Archive $@ ..."
		@$(LIB_NAM) $@ $^

$(EXE_PARTEL_PRELIM):	$(OBJS_PARTEL_PRELIM)
		@$(MESSAGE) "Link executable $@ ..."
		@$(LD) -o $@ $^ $(LKLIB) $(LDFLAGS)

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
