#
#      Principales commandes :
#
#      maktel menage  :  efface tous les executables et *.o
#      maktel all install : compile les sources nouvelles, fait l'édition
#                           de liens et copie les exécutables dans les dossiers
#                           correspondant au compilateur.
#
#
#      maktel all parallel : idem mais avec parallélisme.
#
#
#
#------------------------------makefile------------------------DeltaCAD
#
# Makefile de TELEMAC2D pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   telemac2d*/.../sources.
#
# Modes supportes :
#        % maktel 
#        % maktel  [all] [install] [menage] [parallel]
#        % maktel  libdebug
#        % maktel  libprofile
#-----------------------------------------------------Juin-1999-DeltaCAD
#
# definition des noms de fichiers
#
NOM_GENERIQUE = telemac2d
#
VERSION	      = v6p1
V_SISYPHE     = v6p1
V_TOMAWAC     = v6p1
V_BIEF 	      = v6p1
V_DAMOCLES    = v6p1
V_PARAVOID    = v6p1
V_PARALLEL    = v6p1
V_SPECIAL     = v6p1
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
# Options du compilateur :
#
include $(SYSTELCFG)/cfgmak.mak
# reservé a la compilation sur CRAY ou FUJITSU
#include $(PROJECT)/cfgdef-$(DIRLIB)
#
DIRINC           = $(PROJECT)/telemac2d/tel2d_$(VERSION)/$(DIRLIB)
DIRINC_SISYPHE   = $(PROJECT)/sisyphe/sisyphe_$(VERSION)/$(DIRLIB)
DIRINC_TOMAWAC   = $(PROJECT)/tomawac/toma_$(VERSION)/$(DIRLIB)
DIRINC_BIEF      = $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)
DIRINC_DAMOCLES  = $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)
DIRINC_PARAVOID  = $(PROJECT)/paravoid/paravoid_$(V_PARAVOID)/$(DIRLIB)
DIRINC_PARALLEL  = $(PROJECT)/parallel/parallel_$(V_PARALLEL)/$(DIRLIB)
DIRINC_SPECIAL   = $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)
#
#
#
FCO           = $(FC_OPT_COMPIL)
LKO           = $(LK_OPT_NORMAL) $(LK_OPT_OTHERS)
FC            = $(FC_NAM)
FFLAGS        = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) $(FC_OPT_INCLUDE)$(DIRINC_SISYPHE) $(FC_OPT_INCLUDE)$(DIRINC_TOMAWAC) $(FC_OPT_INCLUDE)$(DIRINC_BIEF) $(FC_OPT_INCLUDE)$(DIRINC_DAMOCLES) $(FC_OPT_INCLUDE)$(DIRINC_PARAVOID) $(FC_OPT_INCLUDE)$(DIRINC_SPECIAL)
#
FFLAGSPARA    = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) $(FC_OPT_INCLUDE)$(DIRINC_SISYPHE) $(FC_OPT_INCLUDE)$(DIRINC_TOMAWAC) $(FC_OPT_INCLUDE)$(DIRINC_BIEF) $(FC_OPT_INCLUDE)$(DIRINC_DAMOCLES) $(FC_OPT_INCLUDE)$(DIRINC_PARALLEL) $(FC_OPT_INCLUDE)$(DIRINC_SPECIAL)
#
MODE          =

LIBRARY       = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

EXEDEF        = $(NOM_GENERIQUE)$(MODE)$(VERSION).exe

LKLIB         = $(PROJECT)/sisyphe/sisyphe_$(V_SISYPHE)/$(DIRLIB)/sisyphe$(MODE)$(V_SISYPHE).a \
                $(PROJECT)/tomawac/toma_$(V_TOMAWAC)/$(DIRLIB)/tomawac$(MODE)$(V_TOMAWAC).a \
                $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)/bief$(MODE)$(V_BIEF).a \
                $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)/damo$(MODE)$(V_DAMOCLES).a \
                $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)/special$(MODE)$(V_SPECIAL).a \
                $(PROJECT)/paravoid/paravoid_$(V_PARAVOID)/$(DIRLIB)/paravoid$(MODE)$(V_PARAVOID).a \
                $(LK_LIB_SPECIAL) $(LIBMUMPSPAR)

EXEDEFPARA    = $(NOM_GENERIQUE)$(MODE)$(VERSION)_MP.exe
LKLIBPARA     = $(PROJECT)/sisyphe/sisyphe_$(V_SISYPHE)/$(DIRLIB)/sisyphe$(MODE)$(V_SISYPHE).a \
                $(PROJECT)/tomawac/toma_$(V_TOMAWAC)/$(DIRLIB)/tomawac$(MODE)$(V_TOMAWAC).a \
                $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)/bief$(MODE)$(V_BIEF).a \
                $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)/damo$(MODE)$(V_DAMOCLES).a \
                $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)/special$(MODE)$(V_SPECIAL).a \
                $(PROJECT)/parallel/parallel_$(V_PARALLEL)/$(DIRLIB)/parallel$(MODE)$(V_PARALLEL).a \
                $(LK_LIB_SPECIAL) $(LIBS_MPI) $(LIBMUMPSPAR)

JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo


DEST          =  $(PROJECT)/telemac2d/tel2d_$(VERSION)/$(DIRLIB)

INSTALL       = cp

MAKEFILE      = makefile

#------------------------ Liste des fichiers
#
#------- Sources Fortran

SRCS= \
friction_def.f\
declarations_telemac2d.f  interface_telemac2d.f  \
m_coupling_estel3d.f \
homere_telemac2d.f                     \
lecdon_telemac2d.f                     \
akepin.f assignstr.f astro.f  bilan.f  \
bilant1.f bord.f  bord_tidal_bc.f bord_tide.f  caldt.f  \
carafr.f cdl.f cdlproj.f cdlzz.f cdl_tch.f celerite.f chpcon.f clhuvt.f  \
clsing.f cltrac.f   coefmat.f coefro.f complim.f condin.f  condin_adj.f  \
conv_mercator_to_degdec.f conv_lambert_to_degdec.f  \
corfon.f corpor.f   cornor.f  correction_depth_2d.f  corstr.f  \
corvis.f cosake.f   cost_function.f cubeequation.f \
debimp.f debsce.f   def_zones.f  \
difsou.f disper.f   dragcoeff.f dragfo.f entete.f exlim.f fdnrst.f filter_h.f flot.f \
flucin.f flucint.f fluhyd.f \
fluroe.f flusec_telemac2d.f flusew.f flusrc.f flutrac.f fluxe.f fluxzz.f flux_tch.f flu_tchamen.f  \
flu_zokagoa.f \
fluxpr_telemac2d.f fricti.f friction.f friction_bubble.f \
friction_lindner.f  friction_calc.f friction_quad.f \
friction_unif.f friction_zones.f friction_user.f \
friction_read.f friction_scan.f friction_init.f friction_choice.f \
gestio.f  gradnod.f gradnodt.f gradz.f \
homere_adj_t2d.f hpropa.f interpol.f  \
href.f   incide.f   init_kin.f initstr.f intemp.f isitok.f  \
kepscl.f kepsil.f   kepsin.f lagran.f  \
lecsip.f lecsng.f   loiden.f loinoy.f  majtrac.f majzz.f \
marast.f masbas2d.f maskob.f   matbou.f mesures.f meteo.f  metgra.f \
newstr.f nodalf_pugh.f nodalupv_pugh.f nomvar_telemac2d.f output_telemac2d.f point_telemac2d.f point_adj_t2d.f  \
oil_spill_2d.f poros.f poro11.f \
prebor.f preres_telemac2d.f  propag.f propag_adj.f propin_telemac2d.f  \
prosou.f q.f  read_fic_curves.f read_fic_sources.f read_sections_telemac2d.f \
reinit.f rescue.f resolu.f  read_fic_frliq.f \
rotne0.f siphon.f sl.f smagor.f smoothing_flux.f smtrac.f \
sorflo.f source_telemac2d.f  spectre.f sta_dis_cur.f steady.f tel4del.f telemac2d.f  testeur.f \
thomps.f tidal_model_t2d.f tr.f trsce.f  utimp_telemac2d.f \
valro.f  vistur.f vit.f     \
volfin.f vusce.f vvsce.f wall_friction.f wetdry.f wrihyd.f zeropsi.f zerophi.f  



#------- Objets
#
OBJS = $(SRCS:.f=.o)


PRINT	      = pr

SHELL	      = /bin/sh


#
# Definition des cibles
#

all:		modules  $(LIBRARY) $(EXEDEF) delete_workdir
exedef:         $(EXEDEF)
exedefpara:	$(EXEDEFPARA)

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

$(EXEDEF):	$(OBJS)
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  if [ "$(DIRLIB)" = "unicos" ]; then \
		    $(MAKE) -f $(MAKEFILE) execray DIRLIB=unicos ; fi; \
		  if [ "$(DIRLIB)" = "fuji" ]; then \
		    $(MAKE) -f $(MAKEFILE) execray DIRLIB=fuji ; fi; \
		else \
		  echo "Link executable par defaut ..." ; \
		  $(LK_NAM) $(LKO) $(LK_OPT_OUTNAME) $(NOM_GENERIQUE)$(MODE)$(VERSION).exe $(OBJS) $(LKLIB) ; fi;
		@echo "Termine."

#---- we use LK_NAM in parallel case as well

$(EXEDEFPARA):	$(OBJS)
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  if [ "$(DIRLIB)" = "unicos" ]; then \
		    $(MAKE) -f $(MAKEFILE) execray DIRLIB=unicos ; fi; \
		  if [ "$(DIRLIB)" = "fuji" ]; then \
		    $(MAKE) -f $(MAKEFILE) execray DIRLIB=fuji ; fi; \
		else \
		  echo "Link parallel executable par defaut ..." ; \
		  $(LK_NAM) $(LKO) $(LK_OPT_OUTNAME) $(NOM_GENERIQUE)$(MODE)$(VERSION)_MP.exe $(OBJS) $(LKLIBPARA); fi;
		@echo "Termine."

menage:;	$(MESSAGE) "Efface les objets, librairies, executables et core-file."
		@rm -f $(OBJS) *.a *.exe *.mod *.MOD *.g90 core

Menage:;	$(MESSAGE) "Efface les objets, bibliotheques et core-file."
		@rm -f $(OBJS) *$(VERSION).a core tags *.exe

depend:;	$(MESSAGE) "Met a jour le Makefile."
		@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

install:	$(LIBRARY) $(EXEDEF)
		@if [ ! -d ../$(DIRLIB) ]; then \
		echo "creation du repertoire $(DIRLIB)";mkdir ../$(DIRLIB); fi
		@rm  -f $(DEST)/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		@$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)); fi
		@$(MESSAGE) "Installe $(EXEDEF) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(EXEDEF); $(INSTALL) $(EXEDEF) $(DEST)); fi

parallel:	$(LIBRARY) $(EXEDEFPARA)
		@if [ ! -d ../$(DIRLIB) ]; then \
		echo "creation du repertoire $(DIRLIB)";mkdir ../$(DIRLIB); fi
		@rm  -f $(DEST)/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		@$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)); fi
		@$(MESSAGE) "Installe $(EXEDEFPARA) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(EXEDEFPARA); $(INSTALL) $(EXEDEFPARA) $(DEST)); fi

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

#--- Librairie pour le CRAY
libcray:
		$(MESSAGE) "Generation de la bibliotheque pour le CRAY"
		@$(MAKE) -f $(MAKEFILE) transfert_modules DIRLIB=unicos
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_CRAY)" DIRLIB=unicos
		@$(MAKE) -f $(MAKEFILE) install DIRLIB=unicos

#--- Librairie pour le FUJITSU
libfuji:
		$(MESSAGE) "Generation de la bibliotheque pour le FUJITSU"
		@$(MAKE) -f $(MAKEFILE) transfert_modules DIRLIB=fuji
		@$(MAKE) -f $(MAKEFILE) FCO="$(FC_OPT_FUJI)" DIRLIB=fuji
		@$(MAKE) -f $(MAKEFILE) install DIRLIB=fuji

#--- Executable pour le CRAY et le Fuji
execray:
		$(MESSAGE) "Generation de l'executable"
		@echo "=> Transfert des bibliotheques "
		@for BIB in `ls $(LKLIB)`  ;\
		do \
		  DIR="`whoami`@`uname -n`:$$BIB" ; \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		   "rcp $$DIR workdir/compilation/. " ; \
		done ;
		@echo "=> Edition de liens "
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  DIR="`whoami`@`uname -n`:`pwd`" ; \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
	  	  "cd workdir/compilation ;\
		  $(FC_LKCIBLE) $(FC_FLAG_LKCIBLE) -o $(EXEDEF) $(OBJS) *.a $(FC_LIBCIBLE) ;\
		  rcp $(EXEDEF) $$DIR/. " ; fi
		@echo "=> Executable recupere "

#--- Transfert des modules
transfert_modules: create_workdir
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		for MOD in `ls $(DIRINC)`  ;\
		do \
		DIR="`whoami`@`uname -n`:$(DIRINC)/$$MOD" ; \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "rcp $$DIR workdir/compilation/.  ";\
		done ; fi;
# 
#------- Modules
#
MODS = friction_def.o declarations_telemac2d.o  interface_telemac2d.o
#
modules:	$(MODS)
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  DIR="`whoami`@`uname -n`:`pwd`" ; \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir/compilation ;\
		  rcp *.mod $$DIR/. ";\
		  mv *.mod $(DEST)/.; fi;
		@echo "=> Transfert des modules effectue "
#
create_workdir :
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir ;  mkdir compilation " ; fi
#
delete_workdir :
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir ;  rm -rf compilation " ; fi
#
#------- Regles de compilation des sources FORTRAN
#------- we use FC also in parallel case, with FFLAGS(!)
#
.f.o:
	@echo "=> Compilation de $< :"
	@if [ ! -d ../$(DIRLIB) ]; then \
	echo "creation du repertoire $(DIRLIB)";mkdir ../$(DIRLIB); fi
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
	@if [ -r *.mod ]; then \
	 echo "Transfert du module $(@F:.o=.mod) ";\
	 cp  *.mod $(DEST)/.; /bin/rm *.mod; fi;
	@if [ -r *.MOD ]; then \
	 echo "Transfert du module $(@F:.o=.MOD) ";\
	 cp  *.MOD $(DEST)/.; /bin/rm *.MOD; fi;
