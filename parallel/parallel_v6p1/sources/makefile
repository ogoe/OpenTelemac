#------------------------------makefile------------------------DeltaCAD
#
# Makefile de PARALLEL pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   parallel*/.../sources.
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
#
VERSION	      = v6p1
#
#
# Options du compilateur :
#
include $(SYSTELCFG)/cfgmak.mak
#
DIRINC    = $(PROJECT)/parallel/parallel_$(VERSION)/$(DIRLIB)
#
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
# reservé a la compilation sur CRAY ou FUJITSU
# include $(PROJECT)/cfgdef-$(DIRLIB)
#
INCMPI        = $(PROJECT)/mpi/$(DIRLIB)/include
#
FCO           = $(FC_OPT_COMPIL)
LKO           = $(LK_OPT_NORMAL) $(LK_OPT_OTHERS) 
FC            = $(FC_MPI)
FC2           = $(FC_NAM)
FFLAGS	      = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) $(FC_OPT_INCLUDE)$(INCMPI)

MODE          =

LIBRARY	      = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo

PARTEL        = partel
GRETEL        = gretel
GREDELPTS     = gredelpts
GREDELSEG     = gredelseg
GREDELMET     = gredelmet
GREDELHYD     = gredelhyd
DIFFSEL       = diffsel

DEST	      = $(PROJECT)/parallel/parallel_$(VERSION)/$(DIRLIB)
#

LKLIB         = $(PROJECT)/parallel/parallel_$(VERSION)/$(DIRLIB)/libmetis.a 

INSTALL	      = cp

MAKEFILE      = makefile

SRCS	      = errpvm.f \
		get_mpi_parameters.f \
		org_charac_type1.f \
		p_org_charac_type.f \
		p_org_fonction_type.f \
		p_org_charac_type_4d.f \
		p_org_fonction_type_4d.f \
		p_dmax.f \
		p_dmin.f \
		p_dsum.f \
		p_exit.f \
		p_imax.f \
		p_imin.f \
		p_init.f \
		p_isum.f \
		p_lsum.f \
		p_mail.f \
		p_mpi_address.f \
		p_mpi_address2.f \
		p_mpi_address3.f \
		p_mpi_alltoall.f \
		p_mpi_alltoallv.f \
		p_mpi_type_commit.f \
		p_mpi_type_get_extent.f \
		p_mpi_type_free.f \
		p_mpi_type_create_struct.f \
		p_read.f \
		p_iread.f \
		p_iread_c.f \
		p_sync.f \
		p_time.f \
		p_wait_paraco.f \
		p_writ.f \
		p_iwrit.f \
		p_iwrit_c.f \
		p_mpi_alltoallv_toma1.f \
		p_mpi_alltoallv_toma2.f \
		p_mpi_alltoallv_toma3.f \
		p_mpi_alltoallv_toma4.f \
		build_global_bound.f

SRCS_PARTEL= partel.f
SRCS_GRETEL= gretel_autop.f
SRCS_GREDELPTS= gredelpts_autop.f
SRCS_GREDELSEG= gredelseg_autop.f
SRCS_GREDELMET= gredelmet_autop.f
SRCS_GREDELHYD= gredelhyd_autop.f
SRCS_DIFFSEL=diffsel.f

#
#------- Objets
#
OBJS = $(SRCS:.f=.o)
OBJS_PARTEL = $(SRCS_PARTEL:.f=.o)
OBJS_GRETEL = $(SRCS_GRETEL:.f=.o)
OBJS_GREDELPTS = $(SRCS_GREDELPTS:.f=.o)
OBJS_GREDELSEG = $(SRCS_GREDELSEG:.f=.o)
OBJS_GREDELMET = $(SRCS_GREDELMET:.f=.o)
OBJS_GREDELHYD = $(SRCS_GREDELHYD:.f=.o)
OBJS_DIFFSEL= $(SRCS_DIFFSEL:.f=.o)

PRINT	      = pr

SHELL	      = /bin/sh

#
# Definition des cibles
#

all:		$(LIBRARY) $(PARTEL) $(GRETEL) $(GREDELPTS) $(GREDELSEG) $(GREDELMET) $(GREDELHYD) $(DIFFSEL)

$(LIBRARY):	$(OBJS)
		$(MESSAGE) "Archive $(LIBRARY) ..."
		@$(LIB_NAM) $(LIB_OPT_OUTNAME) $(LIBRARY) $(OBJS)
		@if [ $(LIB_RANLIB) ]; then $(LIB_RANLIB) $(LIBRARY) > /dev/null ; fi
		@echo "Termine."

$(PARTEL) : $(OBJS_PARTEL) $(OBJS_GRETEL) $(OBJS_GREDELPTS) $(OBJS_GREDELSEG) $(OBJS_GREDELMET) $(OBJS_GREDELHYD) $(OBJS_DIFFSEL)
		$(MESSAGE) "Link de PARTEL/GRETEL/GREDELPTS/GREDELSEG/GREDELMET/GREDELHYD ..."
		$(FC2) $(LKO) $(LK_OPT_OUTNAME) $(PARTEL) $(OBJS_PARTEL) $(LKLIB) 
		$(FC2) $(LKO) $(LK_OPT_OUTNAME) $(GRETEL) $(OBJS_GRETEL) $(LKLIB) 
		$(FC2) $(LKO) $(LK_OPT_OUTNAME) $(GREDELPTS) $(OBJS_GREDELPTS) $(LKLIB) 
		$(FC2) $(LKO) $(LK_OPT_OUTNAME) $(GREDELSEG) $(OBJS_GREDELSEG) $(LKLIB) 
		$(FC2) $(LKO) $(LK_OPT_OUTNAME) $(GREDELMET) $(OBJS_GREDELMET) $(LKLIB) 
		$(FC2) $(LKO) $(LK_OPT_OUTNAME) $(GREDELHYD) $(OBJS_GREDELHYD) $(LKLIB) 
		$(FC2) $(LKO) $(LK_OPT_OUTNAME) $(DIFFSEL) $(OBJS_DIFFSEL)
		@echo "Termine."

menage:;	$(MESSAGE) "Efface les objets, librairies, executables et core-file."
		@rm -f $(OBJS) $(OBJS_PARTEL) $(OBJS_GRETEL) $(OBJS_GREDELPTS) $(OBJS_GREDELSEG) $(OBJS_GREDELMET) $(OBJS_GREDELHYD) $(OBJS_DIFFSEL) *.a $(PARTEL) $(GRETEL) $(GREDELPTS) $(GREDELSEG) $(GREDELMET) $(GREDELHYD) $(DIFFSEL) *.g90 core

Menage:;	$(MESSAGE) "Efface les objets, bibliotheques et core-file."
		@rm -f $(OBJS) *$(VERSION).a core tags

depend:;	$(MESSAGE) "Met a jour le Makefile."
		@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

install:	$(LIBRARY) $(PARTEL) $(GRETEL) $(GREDELPTS) $(GREDELSEG) $(GREDELMET) $(GREDELHYD)
		@if [ ! -d ../$(DIRLIB) ]; then \
		echo "creation du repertoire $(DIRLIB)";mkdir ../$(DIRLIB); fi
		@rm  -f $(DEST)/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)); fi
		$(MESSAGE) "Installe $(PARTEL)/$(GRETEL)/$(GREDELPTS)/$(GREDELSEG)/$(GREDELMET)/$(GREDELHYD) dans $(DEST)."
		@$(INSTALL) $(PARTEL) $(DEST)/$(PARTEL)
		@$(INSTALL) $(GRETEL) $(DEST)/$(GRETEL)
		@$(INSTALL) $(GREDELPTS) $(DEST)/$(GREDELPTS)
		@$(INSTALL) $(GREDELSEG) $(DEST)/$(GREDELSEG)
		@$(INSTALL) $(GREDELMET) $(DEST)/$(GREDELMET)
		@$(INSTALL) $(GREDELHYD) $(DEST)/$(GREDELHYD)
		@$(INSTALL) $(DIFFSEL) $(DEST)/$(DIFFSEL)

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
		remsh $(NAME_CRAY) -l $(USR_CRAY) " mkdir compilation "
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_CRAY)" DIRLIB=unicos
		@$(MAKE) -f $(MAKEFILE) install DIRLIB=unicos
		remsh $(NAME_CRAY) -l $(USR_CRAY) "rm -rf compilation "

#------- Modules
#

.f.o:
	@echo "=> Compilation de $< :"
	@if [ ! -d ../$(DIRLIB) ]; then \
	echo "creation du repertoire $(DIRLIB)";mkdir ../$(DIRLIB); fi
	@if [ "$(DIRLIB)" = "unicos" ]; then \
          DIR="`whoami`@`uname -n`:`pwd`" ; \
          remsh $(NAME_CRAY) -l $(USR_CRAY) \
	  "cd compilation ;\
	  rcp $$DIR/$(<F) . ;\
	  $(FC_CRAY) $(FCFLAG_CRAY) $(<F) ;\
	  rm $(*F).f ;\
	  rcp $(*F).o $$DIR/. ";\
	else \
	  $(FC) $(FFLAGS) $< ; fi;
	@if [ -r *.mod ]; then \
	 echo "Transfert du module $(@F:.o=.mod) ";\
	 cp  *.mod $(DEST)/.; /bin/rm *.mod; fi;
	@if [ -r *.MOD ]; then \
	 echo "Transfert du module $(@F:.o=.MOD) ";\
	 cp  *.MOD $(DEST)/.; /bin/rm *.MOD; fi;

