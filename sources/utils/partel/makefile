#------------------------------makefile------------------------DeltaCAD
#
# Makefile de PARTEL pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   utils/partel.
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
NOM_GENERIQUE = partel

VERSION	      = v7p1

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
include makefile.am
include $(SYSTELCFG)/cfgmak.mak
#
DIRINC           = $(PROJECT)/builds/$(DIRLIB)/lib

#
FCO           = $(FC_OPT_COMPIL)
LKO           = $(LK_OPT_NORMAL) $(LK_OPT_OTHERS)
FC            = $(FC_NAM)
FFLAGS        = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) 

MODE          =

PARTEL        = partel$(VERSION).exe
PARTEL_PARA   = partel_para$(VERSION).exe
PARTEL_PRELIM = partel_prelim$(VERSION).exe
JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo


DEST          =  $(PROJECT)/builds/$(DIRLIB)

LKLIB         = $(PROJECT)/builds/$(DIRLIB)/lib/bief$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/hermes$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/parallel$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/special$(MODE)$(VERSION).a \
                $(PROJECT)/optionals/metis-5.1.0/libmetis/libmetis.a \
                $(LIBS_MPI) $(LK_LIB_SPECIAL)

INSTALL	      = cp

MAKEFILE      = makefile

#------------------------ Liste des fichiers
#
#------- Objets
#
TEMP = $(SRCS_PARTEL:.F=.o)
OBJS_PARTEL = $(TEMP:.f=.o)
OBJS_PARTEL_PARA = $(SRCS_PARTEL_PARA:.F=.o) 
OBJS_PARTEL_PRELIM = $(SRCS_PARTEL_PRELIM:.F=.o)

PRINT	      = pr

SHELL	      = /bin/sh


#
# Definition des cibles
#

all:		$(PARTEL) $(PARTEL_PARA) $(PARTEL_PRELIM) 

$(PARTEL) : $(OBJS_PARTEL) 
		$(MESSAGE) "Link de $(PARTEL)"
		$(FC) $(LKO) $(LK_OPT_OUTNAME) $(PARTEL) $(OBJS_PARTEL) $(LKLIB) 

$(PARTEL_PRELIM) : $(OBJS_PARTEL_PRELIM)
		$(MESSAGE) "Link de $(PARTEL_PRELIM)"
		$(FC) $(LKO) $(LK_OPT_OUTNAME) $(PARTEL_PRELIM) $(OBJS_PARTEL_PRELIM) $(LKLIB) 
		@echo "Termine."

$(PARTEL_PARA) : $(OBJS_PARTEL_PARA)
		$(MESSAGE) "Link de $(PARTEL_PARA)"
		$(FC) $(LKO) $(LK_OPT_OUTNAME) $(PARTEL_PARA) $(OBJS_PARTEL_PARA) $(LKLIB) 

menage:;	$(MESSAGE) "Efface les objets, librairies, executables et core-file."
		@rm -f $(OBJS_PARTEL) $(OBJS_PARTEL_PARA) $(OBJS_PARTEL_PRELIM) $(PARTEL) $(PARTEL_PARA) $(PARTEL_PRELIM) *.g90 core

Menage:;	$(MESSAGE) "Efface les objets, bibliotheques et core-file."
		@rm -f $(OBJS_PARTEL) $(OBJS_PARTEL_PARA) $(OBJS_PARTEL_PRELIM) core tags

depend:;	$(MESSAGE) "Met a jour le Makefile."
		@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

install:	$(PARTEL) $(PARTEL_PARA) $(PARTEL_PRELIM)
		@if [ ! -d $(DEST) ]; then \
		echo "creation du repertoire $(DEST)";mkdir $(DEST) $(DEST)/lib  $(DEST)/bin; fi
		$(MESSAGE) "Installe $(PARTEL) dans $(DEST)/bin."
		@$(INSTALL) $(PARTEL) $(DEST)/bin/$(PARTEL)
		$(MESSAGE) "Installe $(PARTEL_PARA) dans $(DEST)/bin."
		@$(INSTALL) $(PARTEL_PARA) $(DEST)/bin/$(PARTEL_PARA)
		$(MESSAGE) "Installe $(PARTEL_PRELIM) dans $(DEST)/bin."
		@$(INSTALL) $(PARTEL_PRELIM) $(DEST)/bin/$(PARTEL_PRELIM)

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
