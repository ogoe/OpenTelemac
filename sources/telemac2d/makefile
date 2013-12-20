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
NOM_GENERIQUE = telemac2d

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

#
FCO           = $(FC_OPT_COMPIL)
LKO           = $(LK_OPT_NORMAL) $(LK_OPT_OTHERS)
FC            = $(FC_NAM)
FFLAGS        = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) 

MODE          =

LIBRARY       = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

EXEDEF        = $(NOM_GENERIQUE)$(MODE)$(VERSION).exe
LKLIB         = $(PROJECT)/builds/$(DIRLIB)/lib/sisyphe$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/dredgesim$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/tomawac$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/bief$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/damo$(MODE)$(VERSION).a \
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
friction_def.f\
declarations_telemac2d.f  interface_telemac2d.f  \
m_coupling_estel3d.f tpxo.f okada.f oilspill.f\
homere_telemac2d.f                     \
lecdon_telemac2d.f                     \
akepin.f assignstr.f astro.f  bilan.f  \
bilant1.f bord.f  bord_tidal_bc.f bord_tide.f bord_tide_legos.f breach.f buse.f  caldt.f  \
cdl.f cdlproj.f cdlzz.f cdl_hllc.f cdl_tch.f cdl_waf.f celerite.f chpcon.f clhuvt.f  \
clsing.f cltrac.f   coefmat.f coefro.f complim.f condin.f  condin_adj.f  \
conv_mercator_to_degdec.f conv_lambert_to_degdec.f  \
corfon.f corpor.f   cornor.f  correction_depth_2d.f  corstr.f  \
corvis.f cosake.f   cost_function.f cubeequation.f \
calcul_q_weir.f calcul_tang_w2.f collect_values.f \
debimp.f debsce.f   def_zones.f  \
difsou.f disper.f   dragcoeff.f dragfo.f entete.f exlim.f fdnrst.f filter_h.f flot.f \
flucin.f flucint.f fluhyd.f \
fluroe.f flusec_telemac2d.f flusew.f flusrc.f flutrac.f fluxe.f fluxzz.f flux_hllc.f flux_waf.f flux_tch.f \
flu_tchamen.f flu_zokagoa.f \
fluxpr_telemac2d.f fricti.f friction.f friction_bubble.f \
friction_lindner.f  friction_calc.f friction_quad.f \
friction_unif.f friction_zones.f friction_user.f \
friction_read.f friction_scan.f friction_init.f friction_choice.f \
gestio.f  gradnod.f gradnodt.f gradz.f grad_zconv.f \
homere_adj_t2d.f hpropa.f hyd_hllc.f hyd_waf.f interpol.f  \
href.f   incide.f   initstr.f intemp.f isitok.f  \
kepscl.f kepsil.f   kepsin.f lagran.f lecbus.f lecbreach.f \
lecsip.f lecsng.f  limiter.f loiden.f loinoy.f loi_w_inc.f  majtrac.f majzz.f \
marast.f masbas2d.f maskob.f   matbou.f mesures.f meteo.f  metgra.f \
newstr.f nodalf_pugh.f nodalf_schureman.f nodalupv_pugh.f nodalupv_schureman.f \
nomvar_telemac2d.f oil_flot.f output_telemac2d.f point_telemac2d.f point_adj_t2d.f  \
poros.f poro11.f \
prebor.f preres_telemac2d.f  propag.f propag_adj.f propin_telemac2d.f  \
prosou.f q.f  read_fic_curves.f read_fic_sources.f read_sections_telemac2d.f \
receding.f reinit.f rescue.f resolu.f  read_fic_frliq.f \
rotne0.f seg_neighbors.f siphon.f sl.f smagor.f smoothing_flux.f smtrac.f \
sorflo.f source_telemac2d.f  spectre.f sta_dis_cur.f steady.f tel4del.f telemac2d.f  testeur.f \
thomps.f tidal_model_t2d.f tr.f trsce.f  utimp_telemac2d.f \
valro.f  vistur.f vit.f     \
volfin.f vusce.f vvsce.f wall_friction.f wetdry.f wrihyd.f zeropsi.f zerophi.f  



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
		@$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)/lib."
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
