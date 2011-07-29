#------------------------------makefile------------------------DeltaCAD
#
# Makefile de PARAVOID pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   paravoid*/.../sources.
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
NOM_GENERIQUE = paravoid

VERSION	      = v6p1

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
# reservé a la compilation sur CRAY ou FUJITSU
# include $(PROJECT)/cfgdef-$(DIRLIB)
#
DIRINC        = $(PROJECT)/include/$(SYSTELVERS)/$(DIRLIB)
#
FCO           = $(FC_OPT_COMPIL)
FC            = $(FC_NAM)
FFLAGS	      = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC)

MODE          =

LIBRARY	      = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo

DEST	      =  $(PROJECT)/paravoid/paravoid_$(VERSION)/$(DIRLIB)

INSTALL	      = cp

MAKEFILE      = makefile

SRCS= \
p_org_charac_type.f p_org_charac_type_4d.f p_org_fonction_type.f p_org_fonction_type_4d.f \
errpvm.f get_mpi_parameters.f  org_charac_type1.f p_dmax.f p_dmin.f  p_dsum.f p_exit.f \
p_imax.f p_imin.f p_init.f p_iread.f p_iread_c.f p_isum.f p_iwrit.f p_iwrit_c.f p_lsum.f p_mail.f \
p_mpi_address.f p_mpi_address2.f p_mpi_address3.f \
p_mpi_alltoall.f p_mpi_alltoallv.f p_mpi_type_commit.f p_mpi_type_free.f \
p_read.f p_sync.f p_wait_paraco.f p_writ.f build_global_bound.f \
p_mpi_alltoallv_toma1.f p_mpi_alltoallv_toma2.f p_mpi_alltoallv_toma3.f p_mpi_alltoallv_toma4.f

#
#------- Objets
#
OBJS = $(SRCS:.f=.o)

PRINT	      = pr

SHELL	      = /bin/sh

#
# DÅfinition des cibles
#

all:		create_workdir $(LIBRARY) delete_workdir

$(LIBRARY):	$(OBJS)
		$(MESSAGE) "Archive $(LIBRARY) ..."
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  DIR="`whoami`@`uname -n`:`pwd`" ; \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir/compilation ;\
		  $(AR_CIBLE) $(ARFLAG_CIBLE) $(LIBRARY) *.o ;\
		  rcp $(LIBRARY) $$DIR/. ";\
		else \
		  $(LIB_NAM) $(LIB_OPT_OUTNAME) $(LIBRARY) $(OBJS) ;\
		  if [ $(LIB_RANLIB) ]; then $(LIB_RANLIB) $(LIBRARY) > /dev/null ; fi ;\
		fi ;
		@echo "Termine."

menage:;	$(MESSAGE) "Efface les objets, librairies, executables et core-file."
		@rm -f $(OBJS) *.a *.exe *.mod *.g90 core

Menage:;	$(MESSAGE) "Efface les objets, bibliothÉques et core-file."
		@rm -f $(OBJS) *$(VERSION).a core tags

depend:;	$(MESSAGE) "Met È jour le Makefile."
		@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

install:	$(LIBRARY)
		@if [ ! -d ../$(DIRLIB) ]; then \
		echo "creation du repertoire $(DIRLIB)";mkdir ../$(DIRLIB); fi
		@rm  -f $(DEST)/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)); fi


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

#--- Librairie pour le CRAY
libcray:
		$(MESSAGE) "Generation de la bibliotheque pour le CRAY"
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_CRAY)" DIRLIB=unicos
		@$(MAKE) -f $(MAKEFILE) install DIRLIB=unicos

#--- Librairie pour le FUJITSU
libfuji:
		$(MESSAGE) "Generation de la bibliotheque pour le FUJITSU"
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_FUJI)" DIRLIB=fuji
		@$(MAKE) -f $(MAKEFILE) install DIRLIB=fuji

create_workdir :
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir ;  mkdir compilation " ; fi

delete_workdir :
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir ;  rm -rf compilation " ; fi

#------- Regles de compilation des sources FORTRAN

.f.o:
	@echo "=> Compilation de $< :"
	@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
          DIR="`whoami`@`uname -n`:`pwd`" ; \
          remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
	  "cd workdir/compilation ;\
	  rcp $$DIR/$(<F) . ;\
	  $(FC_CIBLE) $(FCFLAG_CIBLE) $(<F) ;\
	  rm $(*F).f ;\
	  rcp $(*F).o $$DIR/. ";\
	else \
	  $(FC) $(FFLAGS) $< ; fi;
