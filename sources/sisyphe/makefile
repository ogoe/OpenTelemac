#------------------------------makefile------------------------DeltaCAD
#
# Makefile de SISYPHE pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   sources/sisyphe
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
NOM_GENERIQUE = sisyphe
#
VERSION	      = v6p2

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
                $(PROJECT)/builds/$(DIRLIB)/lib/damo$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/mumps$(MODE)$(VERSION).a \
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
declarations_sisyphe.f         \
interface_sisyphe_bedload.f    \
interface_sisyphe_suspension.f \
interface_sisyphe.f            \
homere_sisyphe.f               \
bedload_main.f             qsform.f\
sis_arret.f                bedload_meyer.f            rescue_sisyphe.f\
bedload_bailard.f          diricl.f     ride.f\
bedload_bijker.f           entete_sisyphe.f\
bedload_nerbed_vf.f                     suspension_bilan.f\
bedload_calcdw.f           suspension_computation.f  \
bedload_dibwat.f           init_avai.f                suspension_dispersion.f \
bedload_diffin.f           bedload_solidischarge.f    init_compo.f\
layer.f  suspension_erosion.f   \
suspension_erosion_coh.f   suspension_flux_mixte.f \
bedload_direction.f        bedload_solvs_fe.f         init_constant.f\
suspension_depot.f\
bedload_solvs_vf.f         integ.f                    suspension_conv.f\
bedload_effpnt.f           bedload_soulsby.f          suspension_bijker.f\
suspension_fredsoe.f       suspension_vanrijn.f\
bedload_einst.f            flusec_sisyphe.f           fluxpr_sisyphe.f\
bedload_engel.f            bedload_vanrijn.f          suspension_listing.f\
bedload_engel_cc.f        bedload_seccurrent.f       bilan_sisyphe.f\
suspension_main.f        suspension_bilan_coh.f\
bedload_evol.f             calcuw.f     disimp.f      lecdon_sisyphe.f   \
bedload_formula.f          leclis.f                   suspension_rouse.f\
tobw_sisyphe.f   suspension_miles.f\
coefro_sisyphe.f           maskab.f                         tob_sisyphe.f\
bedload_hiding_factor.f    condim_sisyphe.f           mean_grain_size.f\
ride_vr.f\
bedload_hunz_meyer.f       condim_susp.f              noerod.f\
suspension_evol.f\
condis_sisyphe.f           nomvar_sisyphe.f           vitchu_sisyphe.f\
dredgesim_interface.f\
bedload_interact.f         conlit.f                   point_sisyphe.f  \
corstr_sisyphe.f           predes.f                         maxslope.f \
init_transport.f          init_mixte.f   init_compo_coh.f      init_zero.f\
init_sediment.f            ks_sisyphe.f               tassement_2.f\
sisyphe.f                 read_sections_sisyphe.f   suspension_sandflow.f \
rescue_sisyphe_notperma.f \
tassement.f               read_fic_conc.f            cgl.f \
db.f cvsp_add_fraction.f cvsp_add_section.f cvsp_alt.f \
cvsp_check_f.f cvsp_check_steady.f \
cvsp_compress_brut.f \
cvsp_compress_clean.f cvsp_compress_dp.f cvsp_init.f \
cvsp_init_from_layers.f 	cvsp_integrate_volume.f cvsp_main.f\
cvsp_make_actlay.f cvsp_outputfiles.f cvsp_p.f\
cvsp_rm_fraction.f cvsp_write_profile.f layers_p.f\
cvsp_output_init.f lecdon_split_outputpoints.f

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
