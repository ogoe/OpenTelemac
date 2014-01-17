#------------------------------makefile------------------------DeltaCAD
#
# Makefile de gretel pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   parallel*/.../sources.
#
# Modes supportes :
#        % maktel
#        % maktel  [all] [install] [menage]
#        % maktel  libdebug
#        % maktel  libprofile
#-----------------------------------------------------Juin-1999-DeltaCAD
#
# definition des noms de fichiers
#
NOM_GENERIQUE = gretel

VERSION	      = v7p0

###########################
# Definition des macros
#
# macros predefinies :
#	$@	la cible complete (partie avant les ':').
#	$?	ce qui n'est pas a jour par rapport a la cible (partie apres les ':').
#	$*	(regles implicites) partie fichier d'une dependence sans le suffixe.
#	$<	(regles implicites) la partie a jour : '.c' dans '.c.o:'
#	on peut utiliser les formes $(*D), $(@D) et $(<D) pour ne prendre que la partie
#	repertoire ou $(*F), $(@F) et $(<F) pour la partie fichier.
#
# Options du compilateur :
#
include $(SYSTELCFG)/cfgmak.mak
#
DIRINC           = $(PROJECT)/builds/$(DIRLIB)/lib

#
FCO           = $(FC_OPT_COMPIL)
LKO           = $(LK_OPT_NORMAL) $(LK_OPT_OTHERS)
FC            = $(FC_NAM)
FFLAGS        = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) 

MODE          =

GRETEL        = gretel$(VERSION).exe
GREDELPTS     = gredelpts$(VERSION).exe
GREDELSEG     = gredelseg$(VERSION).exe
GREDELMET     = gredelmet$(VERSION).exe
GREDELHYD     = gredelhyd$(VERSION).exe
JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo


DEST          =  $(PROJECT)/builds/$(DIRLIB)

LKLIB         = $(PROJECT)/builds/$(DIRLIB)/lib/bief$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/damo$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/parallel$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/special$(MODE)$(VERSION).a  \
                $(LK_LIB_SPECIAL) $(LIBS_MPI)

INSTALL       = cp

MAKEFILE      = makefile

#------------------------ Liste des fichiers
#
#------- Sources Fortran

SRCS_GRETEL= gretel_autop.f gretel_cpikle2.f gretel_read_dataset_elem.f gretel_read_dataset.f gretel_skip_header.f
SRCS_GREDELPTS= gredelpts_autop.f gredelpts_read_dataset.f
SRCS_GREDELSEG= gredelseg_autop.f gredelpts_read_dataset.f
SRCS_GREDELMET= gredelmet_autop.f gredel_fdnrst.f
SRCS_GREDELHYD= gredelhyd_autop.f

#
#------- Objets
#
TEMP = $(SRCS_GRETEL:.f=.o)
OBJS_GRETEL = $(TEMP:.F=.o)
TEMP = $(SRCS_GREDELPTS:.f=.o)
OBJS_GREDELPTS = $(TEMP:.F=.o)
TEMP = $(SRCS_GREDELSEG:.f=.o)
OBJS_GREDELSEG = $(TEMP:.F=.o)
TEMP = $(SRCS_GREDELMET:.f=.o)
OBJS_GREDELMET = $(TEMP:.F=.o)
TEMP = $(SRCS_GREDELHYD:.f=.o)
OBJS_GREDELHYD = $(TEMP:.F=.o)

PRINT	      = pr

SHELL	      = /bin/sh


#
# Definition des cibles
#

all:		$(GRETEL) $(GREDELPTS) $(GREDELSEG) $(GREDELMET) $(GREDELHYD)

$(GRETEL) : 	$(OBJS_GRETEL) $(OBJS_GREDELPTS) $(OBJS_GREDELSEG) $(OBJS_GREDELMET) $(OBJS_GREDELHYD) 
		$(MESSAGE) "Link de $(GRETEL)"
		$(FC) $(LKO) $(LK_OPT_OUTNAME) $(GRETEL) $(OBJS_GRETEL) $(LKLIB) 
		$(MESSAGE) "Link de $(GREDELPTS)"
		$(FC) $(LKO) $(LK_OPT_OUTNAME) $(GREDELPTS) $(OBJS_GREDELPTS) $(LKLIB) 
		$(MESSAGE) "Link de $(GREDELSEG)"
		$(FC) $(LKO) $(LK_OPT_OUTNAME) $(GREDELSEG) $(OBJS_GREDELSEG) $(LKLIB) 
		$(MESSAGE) "Link de $(GREDELMET)"
		$(FC) $(LKO) $(LK_OPT_OUTNAME) $(GREDELMET) $(OBJS_GREDELMET) $(LKLIB) 
		$(MESSAGE) "Link de $(GREDELHYD)"
		$(FC) $(LKO) $(LK_OPT_OUTNAME) $(GREDELHYD) $(OBJS_GREDELHYD) $(LKLIB) 
		@echo "Termine."

menage:;	$(MESSAGE) "Efface les objets, librairies, executables et core-file."
		@rm -f $(OBJS_GRETEL) $(OBJS_GREDELPTS) $(OBJS_GREDELSEG) $(OBJS_GREDELMET) $(OBJS_GREDELHYD) $(GRETEL) $(GREDELPTS) $(GREDELSEG) $(GREDELMET) $(GREDELHYD) *.g90 core

Menage:;	$(MESSAGE) "Efface les objets, bibliotheques et core-file."
		@rm -f $(OBJS_GRETEL) $(OBJS_GREDELPTS) $(OBJS_GREDELSEG) $(OBJS_GREDELMET) $(OBJS_GREDELHYD) core tags

depend:;	$(MESSAGE) "Met a jour le Makefile."
		@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

install:	$(GRETEL) $(GREDELPTS) $(GREDELSEG) $(GREDELMET) $(GREDELHYD)
		@if [ ! -d $(DEST)]; then \
		echo "creation du repertoire $(DEST)";mkdir $(DEST) $(DEST)/lib  $(DEST)/bin; fi
		$(MESSAGE) "Installe $(GRETEL) dans $(DEST)/bin."
		@$(INSTALL) $(GRETEL) $(DEST)/bin/$(GRETEL)
		$(MESSAGE) "Installe $(GREDELPTS) dans $(DEST)/bin."
		@$(INSTALL) $(GREDELPTS) $(DEST)/bin/$(GREDELPTS)
		$(MESSAGE) "Installe $(GREDELSEG) dans $(DEST)/bin."
		@$(INSTALL) $(GREDELSEG) $(DEST)/bin/$(GREDELSEG)
		$(MESSAGE) "Installe $(GREDELMET) dans $(DEST)/bin."
		@$(INSTALL) $(GREDELMET) $(DEST)/bin/$(GREDELMET)
		$(MESSAGE) "Installe $(GREDELHYD) dans $(DEST)/bin."
		@$(INSTALL) $(GREDELHYD) $(DEST)/bin/$(GREDELHYD)
		

#--- Librairie en DEBUG
libdebug:
		$(MESSAGE) "Mise a jour forcee de la bibli. en DEBUG"
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_DEBUG)" LKO="$(LK_OPT_DEBUG)" MODE=d
		@$(MAKE) -f $(MAKEFILE) install MODE=d

#--- Librairie en PROFILE
libprofile:
		$(MESSAGE) "Mise a jour forcee de la bibli. en PROFILE"
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_PROFILE)" LKO="$(LK_OPT_PROFILE)" MODE=p
		@$(MAKE) -f $(MAKEFILE) install MODE=p

#------- Regles de compilation des sources FORTRAN

.F.o:
	@echo "=> Compilation de $< :"
	@if [ ! -d $(DEST)/lib ]; then \
	echo "creation du repertoire $(DEST)";mkdir $(DEST) $(DEST)/lib $(DEST)/bin; fi
	$(FC) $(FFLAGS) $< ;
	@if [ -r *.mod ]; then \
	 echo "Transfert du module $(@F:.o=.mod) ";\
	 cp  *.mod $(DEST)/lib/.; /bin/rm *.mod; fi;
	@if [ -r *.MOD ]; then \
	 echo "Transfert du module $(@F:.o=.MOD) ";\
	 cp  *.MOD $(DEST)/lib/.; /bin/rm *.MOD; fi;

.f.o:
	@echo "=> Compilation de $< :"
	@if [ ! -d $(DEST)/lib ]; then \
	echo "creation du repertoire $(DEST)";mkdir $(DEST) $(DEST)/lib $(DEST)/bin; fi
	$(FC) $(FFLAGS) $< ;
	@if [ -r *.mod ]; then \
	 echo "Transfert du module $(@F:.o=.mod) ";\
	 cp  *.mod $(DEST)/lib/.; /bin/rm *.mod; fi;
	@if [ -r *.MOD ]; then \
	 echo "Transfert du module $(@F:.o=.MOD) ";\
	 cp  *.MOD $(DEST)/lib/.; /bin/rm *.MOD; fi;
