#------------------------------makefile------------------------DeltaCAD
#
# Makefile de TOMAWAC pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   sources/tomawac.
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
NOM_GENERIQUE = tomawac

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

LIBRARY       = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

EXEDEF        = $(NOM_GENERIQUE)$(MODE)$(VERSION).exe
LKLIB         = $(PROJECT)/builds/$(DIRLIB)/lib/bief$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/damo$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/hermes$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/parallel$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/special$(MODE)$(VERSION).a \
                $(LK_LIB_SPECIAL) $(LIBS_MPI)

JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo


DEST          =  $(PROJECT)/builds/$(DIRLIB)

INSTALL       = cp

MAKEFILE      = makefile

#------------------------ Liste des fichiers
#------- Objets
#
TMP = $(SRCS:.f=.o)
OBJS = $(TMP:.F=.o)


PRINT	      = pr

SHELL	      = /bin/sh


#
# Definition des cibles
#

all:		$(LIBRARY) $(EXEDEF) 

exedef:         $(EXEDEF)

$(LIBRARY):	$(OBJS)
		$(MESSAGE) "Archive $(LIBRARY) ..."
		@$(LIB_NAM) $(LIB_OPT_OUTNAME) $(LIBRARY) $(OBJS)
		@if [ $(LIB_RANLIB) ]; then $(LIB_RANLIB) $(LIBRARY) > /dev/null ; fi
		@echo "Termine."

$(EXEDEF):	$(OBJS)
		$(MESSAGE) "Link executable par defaut ..." ; \
		$(LK_NAM) $(LKO) $(LK_OPT_OUTNAME) $(NOM_GENERIQUE)$(MODE)$(VERSION).exe $(OBJS) $(LKLIB);
		@echo "Termine."

menage:;	$(MESSAGE) "Efface les objets, librairies, executables et core-file."
		@rm -f $(OBJS) *.a *.exe *.mod *.MOD *.g90 core

Menage:;	$(MESSAGE) "Efface les objets, bibliotheques et core-file."
		@rm -f $(OBJS) *$(VERSION).a core tags *.exe

depend:;	$(MESSAGE) "Met a jour le Makefile."
		@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

install:	$(LIBRARY) $(EXEDEF)
		@if [ ! -d $(DEST) ]; then \
		echo "creation du repertoire $(DEST)";mkdir $(DEST) $(DEST)/lib  $(DEST)/bin; fi
		@rm  -f $(DEST)/lib/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)/lib."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/lib/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)/lib); fi
		@$(MESSAGE) "Installe $(EXEDEF) dans $(DEST)/bin."
		@if [ $(DEST)/bin != . ]; then \
		(rm -f $(DEST)/bin/$(EXEDEF); $(INSTALL) $(EXEDEF) $(DEST)/bin); fi

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
