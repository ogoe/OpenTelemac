#------------------------------makefile------------------------DeltaCAD
#
# Makefile de PARALLEL pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   sources/utils/parallel.
#
# Modes supportes :
#        % maktel 
#        % maktel  [all] [install] [menage]
#        % maktel  libdebug
#        % maktel  libprofile
#-----------------------------------------------------Juin-2001-DeltaCAD
# definition des noms de fichiers
#
NOM_GENERIQUE = parallel

VERSION	      = v6p3

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

INCMPI        = $(PROJECT)/optionals/mpi/$(DIRLIB)/include

#
FCO           = $(FC_OPT_COMPIL)
LKO           = $(LK_OPT_NORMAL) $(LK_OPT_OTHERS) 
FC            = $(FC_NAM)
FFLAGS	      = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) $(FC_OPT_INCLUDE)$(INCMPI)

MODE          =

LIBRARY	      = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo

DEST	      = $(PROJECT)/builds/$(DIRLIB)
#
LKLIB         = $(PROJECT)/builds/$(DIRLIB)/lib/special$(MODE)$(VERSION).a \
                $(LIBS_MPI) 

INSTALL       = cp

MAKEFILE      = makefile

#------------------------ Liste des fichiers
#
#------- Sources Fortran

SRCS	      = interface_parallel.f \
                declarations_parallel.F \
		errpvm.F \
		org_charac_type1.F \
		oil_org_charac_type.F \
		org_charac_type_alg.F \
		p_dmax.F \
		p_dmin.F \
		p_dsum.F \
		p_exit.F \
		p_imax.F \
		p_imin.F \
		p_init.F \
		p_isum.F \
                p_isum_array.F \
                p_imax_array.F \
		p_lsum.F \
		p_mail.F \
		p_mpi_address.F \
		p_mpi_address2.F \
		p_mpi_address3.F \
		p_mpi_alltoall.F \
		p_mpi_alltoallv.F \
		p_mpi_alltoallv_alg.F \
		p_mpi_alltoallv_oil.F \
		p_mpi_alltoallv_i.F \
		p_mpi_type_commit.F \
		p_mpi_type_get_extent.F \
		p_mpi_type_free.F \
		p_mpi_type_create_struct.F \
		p_read.F \
		p_iread.F \
		p_iread_c.F \
		p_sync.F \
		p_time.F \
		p_wait_paraco.F \
		p_writ.F \
		p_iwrit.F \
		p_iwrit_c.F 
#
#------- Objets
#
TMP = $(SRCS:.f=.o)
OBJS = $(TMP:.F=.o)

PRINT	      = pr

SHELL	      = /bin/sh


#
# Definition des cibles
#

all:		$(LIBRARY) 

$(LIBRARY):	$(OBJS)
		$(MESSAGE) "Archive $(LIBRARY) ..."
		@$(LIB_NAM) $(LIB_OPT_OUTNAME) $(LIBRARY) $(OBJS)
		@if [ $(LIB_RANLIB) ]; then $(LIB_RANLIB) $(LIBRARY) > /dev/null ; fi
		@echo "Termine."

menage:;	$(MESSAGE) "Efface les objets, librairies, executables et core-file."
		@rm -f $(OBJS) *.a *.exe *.mod *.MOD *.g90 core

Menage:;	$(MESSAGE) "Efface les objets, bibliotheques et core-file."
		@rm -f $(OBJS) *$(VERSION).a core tags *.exe

depend:;	$(MESSAGE) "Met a jour le Makefile."
		@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

install:	$(LIBRARY) 
		@if [ ! -d $(DEST) ]; then \
		echo "creation du repertoire $(DEST)";mkdir $(DEST) $(DEST)/lib  $(DEST)/bin; fi
		@rm  -f $(DEST)/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)/lib."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/lib/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)/lib); fi
		
#--- Librairie en DEBUG
libdebug:
		$(MESSAGE) "Mise a jour forcee de la bibli. en DEBUG"
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_DEBUG)" MODE=d
		@$(MAKE) -f $(MAKEFILE) install MODE=d

#--- Librairie en PROFILE
libprofile:
		$(MESSAGE) "Mise a jour forcee de la bibli. en PROFILE"
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_PROFILE)" MODE=p
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
