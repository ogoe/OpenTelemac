#------------------------------makefile------------------------DeltaCAD
#
# Makefile de TELEMAC3D pour Unix.
#
# ATTENTION : doit être lancé dans le répertoire adéquat :
#                   sources/telemac3d.
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
LKLIB         = $(PROJECT)/builds/$(DIRLIB)/lib/sisyphe$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/tomawac$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/telemac2d$(MODE)$(VERSION).a \
                $(PROJECT)/builds/$(DIRLIB)/lib/bief$(MODE)$(VERSION).a \
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
declarations_telemac3d.f   interface_telemac3d.f \
homere_telemac3d.f lecdon_telemac3d.f \
point_telemac3d.f  preres_telemac3d.f    actuzf.f \
airwik2.f          airwik3.f  amr_plan.f bil3d.f    bissel.f \
bord3d.f           bypass_crushed_points_seg.f    bypass_crushed_points_ebe.f  calcg.f  calcot.f  check.f \
clsedi.f           coef.f     complete_ebe_fluint.f    condim.f   condis.f   t3d_corfon.f \
correction_depth_3d.f    coupe.f    cstkep.f   cvdf3d.f \
debimp3d.f         debimp_3d.f           dessed.f  diag_murd.f \
diff3d.f           drialg.f   dricv.f    driuti.f   drsurr.f \
erodc.f            erodnc.f   erode.f    exchange_with_atmosphere.f \
extmsk.f           findksce.f flot3d.f flupri.f flused.f fluver_2.f \
flux3d.f           fonvas.f   fsgrad.f   gestdp.f   grad2d.f ifab3d.f ifab3dt.f impsed.f    kepcl3.f \
kepicl.f           kepini.f   komcl3.f   lichek.f \
limi3d.f           longml.f   make_zconv.f mask3d.f \
mass3d.f           massed.f   mesh_prop.f mittit.f \
murd3d.f           murd3d_pos.f          nomvar_telemac3d.f  nomvar_2d_in_3d.f  numliq_3d.f \
nuwave_p0.f        oil_spill_3d.f phstat.f   plane_bottom.f  preadv.f   prediv.f  prepare_advection.f q3.f \
rhs_pressure.f     scope.f    sed3d.f    sflo3d.f   sl3.f      smago.f    smago3d.f \
soukep.f           soukom.f   source.f   sources_sinks.f source_trac.f t3d_stress.f suised.f sumver.f \
tassec.f           tassem.f   tbord.f    telemac3d.f thomps_2dto3d.f thomps_bc.f t3d_debsce.f t3d_read_fic_curves.f t3d_trsce.f \
tfond.f            tidal_model_t3d.f     transf_zchar.f  tridw2.f  tridw3.f  trisou.f  tra_prof_z.f  tr3.f upwind.f upwindebe.f upwindseg.f \
utimp.f            vel_prof_z.f  velres.f   \
vermoy.f           viscke.f   viscko.f   visclip.f visclm.f \
viscos.f           vissma.f   vitchu.f   vit3.f \
wave_equation.f    wcturb.f   wstarw.f   wstar.f  

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
