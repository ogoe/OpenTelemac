#------------------------------makefile------------------------DeltaCAD
#
# Makefile de TELEMAC3D pour Unix.
#
# ATTENTION : doit �tre lanc� dans le repertoire adequat :
#                   telemac3d*/.../sources.
#
# Modes supportes :
#        % maktel 
#        % maktel  [all] [install] [menage]
#        % maktel  libdebug
#        % maktel  libprofile
#-----------------------------------------------------Oct-2001-DeltaCAD
#
# definition des noms de fichiers
#
NOM_GENERIQUE = telemac3d
#
VERSION	      = v6p1
V_T2D         = v6p1
V_BIEF        = v6p1
V_DAMOCLES    = v6p1
V_PARAVOID    = v6p1
V_SISYPHE     = v6p1
V_TOMAWAC     = v6p1
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
#
# reserve a la compilation sur CRAY
#include $(PROJECT)/cfgdef-$(DIRLIB)
#
DIRINC          = $(PROJECT)/telemac3d/tel3d_$(VERSION)/$(DIRLIB)
DIRINC_T2D      = $(PROJECT)/telemac2d/tel2d_$(V_T2D)/$(DIRLIB)
DIRINC_SISYPHE  = $(PROJECT)/sisyphe/sisyphe_$(V_SISYPHE)/$(DIRLIB)
DIRINC_TOMAWAC  = $(PROJECT)/tomawac/toma_$(V_TOMAWAC)/$(DIRLIB)
DIRINC_BIEF     = $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)
DIRINC_DAMOCLES = $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)
DIRINC_PARAVOID = $(PROJECT)/paravoid/paravoid_$(V_PARAVOID)/$(DIRLIB)
DIRINC_PARALLEL = $(PROJECT)/parallel/parallel_$(V_PARALLEL)/$(DIRLIB)
DIRINC_SPECIAL  = $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)
#
FCO           = $(FC_OPT_COMPIL)
LKO           = $(LK_OPT_NORMAL) $(LK_OPT_OTHERS)
FC            = $(FC_NAM)

FFLAGS	      = $(FCO) $(FC_OPT_OTHERS) \
                $(FC_OPT_INCLUDE)$(DIRINC)\
                $(FC_OPT_INCLUDE)$(DIRINC_T2D)\
                $(FC_OPT_INCLUDE)$(DIRINC_BIEF)\
                $(FC_OPT_INCLUDE)$(DIRINC_TOMAWAC)\
                $(FC_OPT_INCLUDE)$(DIRINC_SISYPHE)\
                $(FC_OPT_INCLUDE)$(DIRINC_DAMOCLES)\
                $(FC_OPT_INCLUDE)$(DIRINC_PARAVOID)\
                $(FC_OPT_INCLUDE)$(DIRINC_SPECIAL)

FFLAGSPARA    = $(FCO) $(FC_OPT_OTHERS) \
                $(FC_OPT_INCLUDE)$(DIRINC)\
                $(FC_OPT_INCLUDE)$(DIRINC_T2D)\
                $(FC_OPT_INCLUDE)$(DIRINC_BIEF)\
                $(FC_OPT_INCLUDE)$(DIRINC_TOMAWAC)\
                $(FC_OPT_INCLUDE)$(DIRINC_SISYPHE)\
                $(FC_OPT_INCLUDE)$(DIRINC_DAMOCLES)\
                $(FC_OPT_INCLUDE)$(DIRINC_PARALLEL)\
                $(FC_OPT_INCLUDE)$(DIRINC_SPECIAL)

MODE          =

LIBRARY	      = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

EXEDEF	      = $(NOM_GENERIQUE)$(MODE)$(VERSION).exe
EXEDEFPARA    = $(NOM_GENERIQUE)$(MODE)$(VERSION)_MP.exe

LKLIB         = $(PROJECT)/sisyphe/sisyphe_$(V_SISYPHE)/$(DIRLIB)/sisyphe$(MODE)$(V_SISYPHE).a \
                $(PROJECT)/tomawac/toma_$(V_TOMAWAC)/$(DIRLIB)/tomawac$(MODE)$(V_TOMAWAC).a \
                $(PROJECT)/telemac2d/tel2d_$(V_T2D)/$(DIRLIB)/telemac2d$(MODE)$(V_T2D).a \
                $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)/bief$(MODE)$(V_BIEF).a \
                $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)/damo$(MODE)$(V_DAMOCLES).a \
                $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)/special$(MODE)$(V_SPECIAL).a \
                $(PROJECT)/paravoid/paravoid_$(V_PARAVOID)/$(DIRLIB)/paravoid$(MODE)$(V_PARAVOID).a \
                $(LK_LIB_SPECIAL) $(LIBMUMPSPAR) 

LKLIBPARA     = $(PROJECT)/sisyphe/sisyphe_$(V_SISYPHE)/$(DIRLIB)/sisyphe$(MODE)$(V_SISYPHE).a \
                $(PROJECT)/tomawac/toma_$(V_TOMAWAC)/$(DIRLIB)/tomawac$(MODE)$(V_TOMAWAC).a \
                $(PROJECT)/telemac2d/tel2d_$(V_T2D)/$(DIRLIB)/telemac2d$(MODE)$(V_T2D).a \
                $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)/bief$(MODE)$(V_BIEF).a \
                $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)/damo$(MODE)$(V_DAMOCLES).a \
                $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)/special$(MODE)$(V_SPECIAL).a \
                $(PROJECT)/parallel/parallel_$(V_PARALLEL)/$(DIRLIB)/parallel$(MODE)$(V_PARALLEL).a \
                $(LK_LIB_SPECIAL) $(LIBS_MPI) $(LIBMUMPSPAR)

JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo


DEST	      =  $(PROJECT)/telemac3d/tel3d_$(VERSION)/$(DIRLIB)

INSTALL	      = cp

MAKEFILE      = makefile

#------------------------ Liste des fichiers
#
#------- Sources Fortran
#   
SRCS= \
declarations_telemac3d.f   interface_telemac3d.f \
homere_telemac3d.f lecdon_telemac3d.f \
point_telemac3d.f  preres_telemac3d.f    actuzf.f \
airwik2.f          airwik3.f  amr_plan.f bil3d.f    bissel.f \
bord3d.f           bypass_crushed_points_seg.f    bypass_crushed_points_ebe.f  calcg.f  calcot.f  check.f \
clsedi.f           coef.f     complete_ebe_fluint.f    condim.f   condis.f   corfon.f \
corpar.f           correction_depth_3d.f    coupe.f    cstkep.f   cvdf3d.f \
debimp3d.f         deri3d.f   dessed.f  diag_murd.f \
diff3d.f           drialg.f   dricv.f    driuti.f   drsurr.f \
erodc.f            erodnc.f   erode.f    extmsk.f  findksce.f flot3d.f flupri.f flused.f fluver_2.f \
flux3d.f           fonvas.f   fsgrad.f   gestdp.f   grad2d.f ifab3d.f  impsed.f    kepcl3.f \
kepicl.f           kepini.f   komcl3.f   lichek.f \
limi3d.f           longmb.f   longml.f   mask3d.f \
mass3d.f           massed.f   mesh_prop.f mittit.f \
murd3d.f           murd3d_pos.f          nomvar_telemac3d.f  nomvar_2d_in_3d.f  \
nuwave_p0.f        oil_spill_3d.f phstat.f   plane_bottom.f  precon.f   prediv.f  prepare_advection.f q3.f \
rhs_pressure.f     scope.f    sed3d.f    sflo3d.f   sl3.f      smago.f    smago3d.f \
soukep.f           soukom.f   source.f   sources_sinks.f source_trac.f stress.f suised.f sumver.f \
tassec.f           tassem.f   tbord.f    telemac3d.f t3d_debsce.f t3d_read_fic_curves.f t3d_trsce.f \
tfond.f            transf_zchar.f  tridw2.f  tridw3.f  trisou.f  tra_prof_z.f  tr3.f upwind.f upwindebe.f upwindseg.f \
utimp.f            vel_prof_z.f  velres.f   \
vermoy.f           viscke.f   viscko.f   visclip.f visclm.f \
viscos.f           vissma.f   vitchu.f   vit3.f \
wave_equation.f    wcturb.f   wstarw.f   wstar.f  

#
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
		  $(LK_NAM) $(LKO) $(LK_OPT_OUTNAME) $(NOM_GENERIQUE)$(MODE)$(VERSION).exe $(OBJS) $(LKLIB); fi;
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
		$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)); fi
		$(MESSAGE) "Installe $(EXEDEF) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(EXEDEF); $(INSTALL) $(EXEDEF) $(DEST)); fi

parallel:	$(LIBRARY) $(EXEDEFPARA)
		@if [ ! -d ../$(DIRLIB) ]; then \
		echo "creation du repertoire $(DIRLIB)";mkdir ../$(DIRLIB); fi
		@rm  -f $(DEST)/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)."
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)); fi
		$(MESSAGE) "Installe $(EXEDEFPARA) dans $(DEST)."
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

#--- Executable pour le CRAY
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
		  "rcp $$DIR workdir/compilation/. ";\
		done ; fi;
  
#------- Modules
#
MODS = declarations_telemac3d.o   interface_telemac3d.o

modules:	$(MODS)
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  DIR="`whoami`@`uname -n`:`pwd`" ; \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir/compilation ;\
		  rcp *.mod $$DIR/. ";\
		  mv *.mod $(DEST)/.; fi;
		@echo "=> Transfert des modules effectue "

create_workdir :
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir ;  mkdir compilation " ; fi

delete_workdir :
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir ;  rm -rf compilation " ; fi

#------- Regles de compilation des sources FORTRAN
#------- we use FC also in parallel case, with FFLAGS(!)

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
	 cp *.mod $(DEST)/.; /bin/rm *.mod; fi;
	@if [ -r *.MOD ]; then \
	 echo "Transfert du module $(@F:.o=.MOD) ";\
	 cp *.MOD $(DEST)/.; /bin/rm *.MOD; fi;

