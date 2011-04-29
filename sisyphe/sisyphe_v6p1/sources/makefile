#------------------------------makefile------------------------DeltaCAD
#
# Makefile de SISYPHE pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   sisyphe*/.../sources.
# maktel menage: supprime tous les .o
# maktel all install: recrée les .o et installation du nouvel exécutable
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
VERSION	      = v6p1
V_BIEF 	      = v6p1
V_DAMOCLES    = v6p1
V_PARAVOID    = v6p1
V_PARALLEL    = v6p1
V_SPECIAL     = v6p1

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
#
DIRINC =           $(PROJECT)/sisyphe/sisyphe_$(VERSION)/$(DIRLIB)
DIRINC_BIEF      = $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)
DIRINC_DAMOCLES  = $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)
DIRINC_PARAVOID  = $(PROJECT)/paravoid/paravoid_$(V_PARAVOID)/$(DIRLIB)
DIRINC_PARALLEL  = $(PROJECT)/parallel/parallel_$(V_PARALLEL)/$(DIRLIB)
DIRINC_SPECIAL   = $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)

#
FCO           = $(FC_OPT_COMPIL)
LKO           = $(LK_OPT_NORMAL) $(LK_OPT_OTHERS)
FC            = $(FC_NAM)
FFLAGS	      = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) \
                                        $(FC_OPT_INCLUDE)$(DIRINC_BIEF) \
                                        $(FC_OPT_INCLUDE)$(DIRINC_DAMOCLES) \
                                        $(FC_OPT_INCLUDE)$(DIRINC_PARAVOID) \
                                        $(FC_OPT_INCLUDE)$(DIRINC_SPECIAL)  

FFLAGSPARA	= $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC) \
                                        $(FC_OPT_INCLUDE)$(DIRINC_BIEF) \
                                        $(FC_OPT_INCLUDE)$(DIRINC_DAMOCLES) \
                                        $(FC_OPT_INCLUDE)$(DIRINC_PARALLEL) \
                                        $(FC_OPT_INCLUDE)$(DIRINC_SPECIAL)  

MODE          =

LIBRARY	      = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

EXEDEF	      = $(NOM_GENERIQUE)$(MODE)$(VERSION).exe
LKLIB         = $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)/bief$(MODE)$(V_BIEF).a \
                $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)/damo$(MODE)$(V_DAMOCLES).a \
                $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)/special$(MODE)$(V_SPECIAL).a \
                $(PROJECT)/paravoid/paravoid_$(V_PARAVOID)/$(DIRLIB)/paravoid$(MODE)$(V_PARAVOID).a \
                $(LK_LIB_SPECIAL) $(LIBMUMPSPAR)
                
EXEDEFPARA	= $(NOM_GENERIQUE)$(MODE)$(VERSION)_MP.exe
LKLIBPARA	= $(PROJECT)/bief/bief_$(V_BIEF)/$(DIRLIB)/bief$(MODE)$(V_BIEF).a \
                $(PROJECT)/damocles/damo_$(V_DAMOCLES)/$(DIRLIB)/damo$(MODE)$(V_DAMOCLES).a \
                $(PROJECT)/special/special_$(V_SPECIAL)/$(DIRLIB)/special$(MODE)$(V_SPECIAL).a \
                $(PROJECT)/parallel/parallel_$(V_PARALLEL)/$(DIRLIB)/parallel$(MODE)$(V_PARALLEL).a \
                $(LK_LIB_SPECIAL) $(LIBS_MPI) $(LIBMUMPSPAR)

JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo


DEST	      =  $(PROJECT)/sisyphe/sisyphe_$(VERSION)/$(DIRLIB)

INSTALL	      = cp

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
suspension_main.f\
bedload_evol.f             calcuw.f                   lecdon_sisyphe.f   \
bedload_formula.f          leclis.f                   suspension_rouse.f\
tobw_sisyphe.f\
coefro_sisyphe.f           maskab.f                         tob_sisyphe.f\
bedload_hiding_factor.f    condim_sisyphe.f           mean_grain_size.f\
ride_vr.f\
bedload_hunz_meyer.f       condim_susp.f              noerod.f\
suspension_evol.f\
condis_sisyphe.f           nomvar_sisyphe.f           vitchu_sisyphe.f\
dredgesim_interface.f\
bedload_interact.f         conlit.f                   point_sisyphe.f  \
corstr_sisyphe.f           predes.f                         maxslope.f \
init_transport.f           init_mixte.f               init_zero.f\
init_sediment.f            ks_sisyphe.f\
sisyphe.f                 read_sections_sisyphe.f rescue_sisyphe_notperma.f   tassement.f

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
		@echo "Termine lib."

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
		$(MESSAGE) "Generation de l'executable "
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
MODS = declarations_sisyphe.o   interface_sisyphe_bedload.f   interface_sisyphe_suspension.f   interface_sisyphe.f

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

