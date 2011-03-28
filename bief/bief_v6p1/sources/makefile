#------------------------------makefile------------------------DeltaCAD
#
# Makefile de BIEF pour Unix.
#
# ATTENTION : doit être lancé dans le repertoire adéquat :
#                   bief*/.../sources.
#
# Modes supportes :
#        % maktel 
#        % maktel  [all] [install] [menage]
#        % maktel  libdebug
#        % maktel  libprofile
#-----------------------------------------------------Juin-1999-DeltaCAD
## definition des noms de fichiers
#
NOM_GENERIQUE = bief

VERSION = v6p1

DIRINC    = $(PROJECT)/bief/bief_$(VERSION)/$(DIRLIB)

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
# reserve a la compilation sur CRAY et FUJITSU, peut etre enleve sur toute autre machine
#include $(PROJECT)/cfgdef-$(DIRLIB)
#
#
FCO           = $(FC_OPT_COMPIL)
FC            = $(FC_NAM)
FFLAGS	      = $(FCO) $(FC_OPT_OTHERS) $(FC_OPT_INCLUDE)$(DIRINC)

MODE          =
LIBRARY	      = $(NOM_GENERIQUE)$(MODE)$(VERSION).a

JOURNAL       = journal
DATE          = `date '+%d/%m/%y %H:%M'`
MESSAGE       = @echo "$(PWD) $(LIBRARY) $(LOGNAME) $(DATE) $@ ($?)" >>$(JOURNAL);echo


DEST	      =  $(PROJECT)/bief/bief_$(VERSION)/$(DIRLIB)
INSTALL	      = cp
MAKEFILE      = makefile

SRCS= \
bief_def.f declarations_telemac.f bief.f \
streamline.f m_med.f \
addblo.f  allblo.f allblo_in_block.f             \
almesh.f     asmve1.f       attend.f  \
asmvec.f     asseg_3d.f     assex3.f      assve1.f      assvec.f \
as3_1111_q.f as3_1111_s.f   as3_1313_q.f  as3_1313_s.f  as3_1113.f    as3_1311.f    as3_1212_q.f  as3_1212_s.f \
as3_1112.f   as3_1211.f     as3_4141_s.f  as3_4141_q.f \
atanc.f  bief_allmat.f bief_allvec.f bief_allvec_in_block.f bief_close_files.f \
bief_desimp.f  bief_dim1_ext.f bief_dim2_ext.f bief_ininds.f bief_init.f \
bief_nbfel.f bief_nbmpts.f  bief_nbpel.f  bief_nbpts.f  bief_nbseg.f  bief_nbsegel.f \
bief_open_files.f bief_sum.f bief_suite.f  bief_valida.f \
bilant.f      caract.f      cflp11.f    cflp12.f \
cflpsi.f     cflvf.f        cg1112.f      cg1113.f      cgsqua.f    cgstab.f \
char11.f     char13.f       char41.f      charac.f      check_digits.f  check_dot.f chgdis.f  \
clip.f       cmpobj.f       comp_fac.f  comp_ikle.f \
comp_nh_com_seg.f           comp_seg.f    config_code.f   corlat.f  \
corrsl.f     corrxy.f       cpik12.f      cpik13.f      cpikle2.f \
cpikle3.f    cpstmt.f       cpstvc.f      create_dataset.f  create_dataset_serafin.f  crosfr.f      crsl11.f  \
crsl12.f     cvdftr.f       cvtrvf.f      cvtrvf_pos.f  cvtrvf_pos_2.f  dcpldu.f      decldu.f      decv11.f  desseg.f \
decv21.f     decvrt.f       derive.f      derlag.f  \
des11.f      des21.f        des41.f       descen.f  \
diffcl.f     diffin.f       dimens.f  \
diraux.f     diri01.f       diri04.f      diri09.f  \
dirich.f     dlduseg.f      dldu11.f      dldu21.f      dldu41.f  \
dmo.f        dot.f          dots.f        downup.f  \
dwnup1.f     ecrgeo.f       ecri2.f       elapse.f      elebd.f       eleb3d.f eleb3dt.f \
elebd31.f    eof.f          equnor.f      errmin.f      errmax.f    extens.f fasp.f  \
filp10.f     filp11.f       filp12.f      filpol.f      flux_ef_vf.f  flux_ef_vf_3d.f \
filter.f     find_in_sel.f  flusec.f      flux3dlim.f   flux_mask.f   fluxpr.f   \
fond.f       fonstr.f       front2.f      fropro.f      geoelt.f      gettri.f gettriebe.f gettriseg.f \
gmres.f      godown.f       godwn1.f      goup.f    \
goup1.f      gracjg.f       gradp.f       gsebe.f   \
gtsh11.f     gtsh13.f       gtsh41.f      hloc.f     hvf.f ielbor.f  \
impvec.f     inbief.f       inclu2.f      inclus.f  \
infcel.f     inpoly.f  \
interp.f     invmtx.f       jultim.f      kspg11.f   ksupg.f   \
latitu.f     leclim.f       lit.f         litenr.f   lubksb.f   ludcmp.f  lump.f  maskbd.f \
masktf.f     maskto.f       matrbl.f      matrix.f   matriy.f  \
matvct.f     matvec.f       maxi.f        mer11.f   \
mer21.f      mer41.f        mini.f        mvseg.f   \
mt01aa.f     mt01bb.f       mt01cc.f      mt01oo.f  mt01pp.f  mt01tt.f \
mt02aa.f     mt02aa_2.f     mt02bb.f      mt02cc.f  mt02pp.f  mt02pp_star.f  mt02pt.f mt02tt.f mt03aa.f  \
mt03bb.f     mt03cc.f       mt04aa.f      mt04bb.f  mt04cc.f  mt04pp.f  mt04tt.f \
mt05aa.f     mt05bb.f       mt05cc.f      mt05pp.f  mt05tt.f  mt06aa.f \
mt06bb.f     mt06cc.f       mt06ff.f      mt06ft.f  mt06ft2.f mt06oo.f  mt06oc.f  mt06pp.f  mt06tt.f  mt07aa.f  \
mt07bb.f     mt07cc.f       mt08aa.f      mt08ab.f  mt08ac.f  mt08ba.f \
mt08bb.f     mt08pp.f       mt08tt.f      mt11aa.f  mt11ab.f  mt11ac.f \
mt11ba.f     mt11bb.f       mt12aa.f      mt12ab.f  mt12ac.f \
mt12ba.f     mt12bb.f       mt13aa.f      mt13ab.f \
mt13ba.f     mt13bb.f       mt13cc.f      mt13ca.f  mt14pp.f  mt99aa.f \
mt99bb.f     mv0202.f       mv0303.f      mv0304.f  mv0306.f \
mult_interface_seg.f        mv0403.f      mv0404.f  mv0603.f      \
mv0606.f     mw0303.f       mxptel.f      mxptel31.f  normab.f    \
om.f         omseg.f        omsegbor.f    om0101.f  om1101.f  om1111.f  \
om1112.f     om1113.f       om1201.f      om1211.f  om1302.f  om1311.f  om2121.f  om3181.f \
om4111.f     om4121.f       om4141.f      om5111.f  om5161.f  opass.f \
os.f         osbd.f         osdb.f \
osdbif.f     ov.f           ovbd.f        ovd.f \
ovdb.f       ovd_2.f        ov_2.f        paraco.f \
paragl.f     parcom.f       parcom2.f     parcom2_seg.f parcom_bord.f parini.f  parmoy.f \
plante.f     positive_depths.f  pre_scaract_maillage.f  pre_scaract_thompson.f \
prebd4.f     prebd9.f       prebdt.f \
precd1.f     precd4.f       precd9.f      precdt.f  preverseg.f  preverebe.f \
proxim.f     ptel11.f       pttoel.f      puog.f \
puog1.f      p_dot.f        p_dots.f      readgeo1.f \
readgeo2.f   readgeo3.f     read_config.f read_submit.f remseg.f  rem11.f \
rem21.f      rem41.f        remont.f      rescjg.f \
sd_md.f      sd_mdi.f       sd_mdm.f      sd_mdp.f      sd_mdu.f \
sd_odrv.f    sd_sdrv.f      sd_snf.f      sd_sns.f      sd_sro.f \
sd_ssf.f     sd_strssd.f    sd_strtri.f   sd_strsg4.f   sd_fabcad.f sd_fabsg4.f  sd_solve_1.f   sd_solve_4.f \
sd_ndrv.f    sd_nnf.f       sd_nns.f      sd_nnt.f      sd_nsf.f \
segbor.f     share_3d_fluxes.f            skipgeo.f     slop10.f      slopes.f \
solaux.f     solve.f        somme.f       somme2.f      sortie.f \
stoseg.f     stoseg41.f     strche.f      suite_serafin.f surv11.f \
survol.f      time_in_seconds.f           tnomer.f       tracvf.f      trid3d.f      tsloc.f       topogr.f  tvf.f \
um1x.f       um1x04.f       um1x09.f       \
vc00aa.f     vc00bb.f       vc00cc.f      vc00pp.f      vc00pp2.f     vc00ft.f \
vc00ff.f     vc00tt.f       vc01aa.f       \
vc01bb.f     vc01ff.f       vc01ft.f      vc01ft2.f     vc01oo.f      vc01pp.f  vc01tt.f  vc01tt0.f \
vc02pp_star.f               vc03aa.f      vc03bb.f      vc04aa.f      vc04pp.f  vc05oo.f  vc05aa.f  vc04tt.f \
vc05ff.f     vc05ft.f       vc08aa.f      vc08bb.f      vc08cc.f  vc08pp.f  vc08tt.f \
vc09aa.f     vc10oo.f       vc11aa.f      vc11aa2.f \
vc11bb.f     vc11pp.f       vc11tt.f      vc11tt0.f \
vc13aa.f     vc13bb.f       vc13cc.f      vc13pp.f      vc13pp2.f     vc13tt.f  vc14aa.f \
vc15aa.f     vc16aa.f       vc18pp.f      vc19aa.f      vecle3.f  vecle4.f vecle6.f \
veclen.f     vector.f       vectos.f      vgfpsi.f  \
voisin.f     voisin31.f     waitfor.f     write_data.f write_data_serafin.f   write_mesh.f write_mesh_serafin.f
#
#------- Objets
#
OBJS = $(SRCS:.f=.o)


PRINT	      = pr
#
SHELL	      = /bin/sh
#
# Definition des cibles
#

all:		create_workdir modules  $(LIBRARY) delete_workdir

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
		@rm -f $(OBJS) *.a *.exe *.mod *.MOD *.g90 core

Menage:;	$(MESSAGE) "Efface les objets, bibliotheques et core-file."
		@rm -f $(OBJS) *$(VERSION).a core tags

depend:;	$(MESSAGE) "Met a jour le Makefile."
		@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

install:	$(LIBRARY)
		@if [ ! -d ../$(DIRLIB) ]; then \
		echo "creation du repertoire $(DIRLIB)";mkdir ../$(DIRLIB); fi
		@rm  -f $(DEST)/$(NOM_GENERIQUE)$(MODE)$(VERSION).a
		@$(MESSAGE) "Installe $(LIBRARY) dans $(DEST)."
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

 
#------- Modules
#
MODS =   bief_def.o declarations_telemac.o bief.o streamline.o

modules:	$(MODS)
		@if [ "$(DIRLIB)" = "unicos" -o "$(DIRLIB)" = "fuji" ]; then \
		  DIR="`whoami`@`uname -n`:`pwd`" ; \
		  remsh $(NAME_CIBLE) -l $(USR_CIBLE) \
		  "cd workdir/compilation ;\
		  rcp *.mod $$DIR/. ";\
		  mv *.mod $(DEST)/.; fi;
		@echo "=> Transfert des modules effectue !"

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

