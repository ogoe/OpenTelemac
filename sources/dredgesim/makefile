#
# Makefile de TELEMAC2D pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   sources/telemac2d.
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
NOM_GENERIQUE = dredgesim

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

LIBRARY       = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

EXEDEF        = $(NOM_GENERIQUE)$(MODE)$(VERSION).exe
LKLIB         = $(PROJECT)/builds/$(DIRLIB)/lib/bief$(MODE)$(VERSION).a \
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
#
#------- Sources Fortran

SRCS= \
b_error.f90 b_constants.f90 b_carry.f90 b_time.f90 \
b_cmds.f90 b_file.f90 b_date.f90 b_datetime.f90 \
b_dim.f90 b_var.f90 b_phy.f90 b_att.f90 \
b_io_info.f90 b_grain.f90 l_criterion.f90 m_dredgesim_data.f90 \
m_dredgesim_output.f90 m_dredgesim_interpolations.f90 m_dredgesim_compute.f90 m_dredgesim_update.f90 \
m_dictionary_data.f90 m_stdat_types.f90 m_stringliste.f90 m_stdat_checks.f90 \
m_lesehilfe.f90 m_dic_checks.f90 m_felder.f90 m_diction_ein.f90 \
p_dictionary_ui.f90 m_dredgesim_steer.f90 m_dredgesim_errors.f90 \
b_omi_space.f90 b_omi_xyz.f90 b_omi_ind.f90 b_omi_ele.f90 \
b_omi_stamp.f90 b_omi_span.f90 b_omi_dim.f90 b_omi_unit.f90 \
b_omi_quant.f90 b_omi_arg.f90 b_omi_dope.f90 b_omi_exch.f90 \
m_h_grid_data.f90 b_point_2d.f90 m_h_grid_derive.f90 m_h_grid_omi.f90 \
m_h_grid_delft3d.f90 m_h_grid_gitter05.f90 m_h_grid_errors.f90 m_h_grid_selafin.f90 \
m_h_grid_convert.f90 m_h_grid_untrim.f90 m_h_grid_dimensions.f90 b_omi_exch_r.f90 \
p_h_grid_ui.f90 m_ipds_phydef.f90 m_ipds_phyval.f90 m_ipds_physet.f90 \
m_ipds_mespos.f90 m_ipds_region.f90 m_ipds_regphyset.f90 m_ipds_data.f90 \
m_ipds_errors.f90 m_ipds_readfile.f90 m_ipds_derive.f90 m_ipds_omi.f90 \
m_ipds_interpolation.f90 m_ipds_access.f90 io_ipds_ui.f90 \
p_dredgesim_ui.f90 ext_ds_dredge_node_index.f90 p_sisyphe_ui.f90

#------- Objets
#
TMP = $(SRCS:.f90=.o)
OBJS = $(TMP:.F90=.o)

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
		@rm  -f $(DEST)/lib/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		@$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)/lib."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/lib/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)/lib); fi

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

.SUFFIXES:
.SUFFIXES: .f .F .F90 .f90 .o .mod .MOD

.f90.o:
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

.F90.o:
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
