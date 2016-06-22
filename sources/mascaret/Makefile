# Compiler and linker
FC=gfortran
CC=gcc
LD=gfortran
# Optimisation level
OPT = -O2
# Debug level
DBG =
INSTALL_DIR=${HOMETEL}/builds/${USETELCFG}
# Compilation and link options for a particular OS
ifdef SYSTEMROOT
# Windows OS
	MASC = mascaret.exe mascaret.dll
	FFLAGS = $(OPT) $(DBG) -I./Fox/objs/finclude/
	LDFLAGS = $(OPT) $(DBG) -I./Fox/objs/finclude/ -static -L./Fox/objs/lib -lFoX_dom -lFoX_sax -lFoX_common -lFoX_utils -lFoX_fsys
	LDFLAGSLIB = $(LDFLAGS)
else
	ifeq ($(shell uname),Linux)
# GNU/Linux OS
		MASC = mascaret mascaret.so
		FFLAGS = $(OPT) $(DBG) -I./Fox/objs/finclude/  -fPIC
		CFLAGS = $(OPT) $(DBG) -I./Fox/objs/finclude/  -fPIC
		LDFLAGS = $(OPT) $(DBG) -I./Fox/objs/finclude/ -static -L./Fox/objs/lib -lFoX_dom -lFoX_sax -lFoX_common -lFoX_utils -lFoX_fsys
		LDFLAGSLIB = $(OPT) $(DBG) -I./Fox/objs/finclude/ -L./Fox/objs/lib -lFoX_dom -lFoX_sax -lFoX_common -lFoX_utils -lFoX_fsys
	else
# Mac OSX
		MASC = mascaretOSX mascaret.dylib
		FFLAGS = $(OPT) $(DBG) -I./Fox/objs/finclude/
		LDFLAGS = $(OPT) $(DBG) -I./Fox/objs/finclude/ -L./Fox/objs/lib -lFoX_dom -lFoX_sax -lFoX_common -lFoX_utils -lFoX_fsys
		LDFLAGSLIB =$(LDFLAGS)
	endif
endif

.DEFAULT:
	-touch $@
all: $(MASC)

adbuffer.o: Deriv/adbuffer.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Deriv/adbuffer.f
adstack.o: Deriv/adstack.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c	Deriv/adstack.c
aintdc.o: Mascaret/aintdc.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/aintdc.f90
akiv.o: Mascaret/akiv.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/akiv.f90
akivm1.o: Mascaret/akivm1.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/akivm1.f90
alarg.o: Mascaret/alarg.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/alarg.f90
algop.o: Pretrait/algop.f90 m_message_c.o m_connect_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/algop.f90
apimascaret.o: API/cpp/apimascaret.c API/cpp/apimascaret.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -IAPI/cpp	API/cpp/apimascaret.c
bilan.o: Mascaret/bilan.f90 m_precision.o m_connect_t.o m_erreur_t.o m_confluent_t.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/bilan.f90
bilmas.o: Transdiffusion/bilmas.f90 m_precision.o m_constantes_tracer_t.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/bilmas.f90
bilvol.o: Casier/bilvol.f90 m_precision.o m_constantes_calcul_c.o m_casier_t.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_casier_i.o m_message_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/bilvol.f90
bissn1.o: Mascaret/bissn1.f90 m_precision.o m_erreur_t.o m_promat_i.o m_promvt_i.o m_invmat_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/bissn1.f90
bissnd.o: Mascaret/bissnd.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/bissnd.f90
bissnd_tracer.o: Transdiffusion/bissnd_tracer.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/bissnd_tracer.f90
borne.o: Mascaret/borne.f90 m_precision.o m_message_c.o m_erreur_t.o m_indice_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/borne.f90
boussi.o: Mascaret/boussi.f90 m_precision.o m_erreur_t.o m_parametre_c.o m_cele_i.o m_csur_i.o m_presd_i.o m_dichodm_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/boussi.f90
calage_n2qn1.o: Castor/calage_n2qn1.f90 m_precision.o m_parametre_c.o m_message_c.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_xindic_s.o m_strick_i.o m_direct_i.o m_simul_i.o m_share_var.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Castor/calage_n2qn1.f90
calaig.o: Planim/calaig.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/calaig.f90
calc_connect.o: Pretrait/calc_connect.f90 m_precision.o m_connect_t.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/calc_connect.f90
calc_maillage.o: Pretrait/calc_maillage.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_fichier_t.o m_maille_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_lec_maillage_i.o m_mailler_i.o m_traiter_erreur_i.o m_abs_abs_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/calc_maillage.f90
calc_pc_conflu.o: Sarap/calc_pc_conflu.f90 m_precision.o m_parametre_c.o m_message_c.o m_connect_t.o m_confluent_t.o m_erreur_t.o m_profil_t.o m_profil_plan_t.o m_traiter_erreur_i.o m_rhsbp_s.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/calc_pc_conflu.f90
calcfl.o: Mascaret/calcfl.f90 m_precision.o m_constantes_calcul_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/calcfl.f90
calcon.o: Mascaret/calcon.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_flusrc_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/calcon.f90
calcs_biomass.o: ModelesQualEau/calcs_biomass.f90 m_precision.o m_parametre_c.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_meteo_t.o m_erreur_t.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/calcs_biomass.f90
calcs_eutro.o: ModelesQualEau/calcs_eutro.f90 m_precision.o m_parametre_c.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_meteo_t.o m_erreur_t.o m_interpolation_s.o m_singularite_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/calcs_eutro.f90
calcs_mes.o: ModelesQualEau/calcs_mes.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/calcs_mes.f90
calcs_micropol.o: ModelesQualEau/calcs_micropol.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/calcs_micropol.f90
calcs_o2.o: ModelesQualEau/calcs_o2.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_singularite_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/calcs_o2.f90
calcs_rien.o: ModelesQualEau/calcs_rien.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/calcs_rien.f90
calcs_thermic.o: ModelesQualEau/calcs_thermic.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_meteo_t.o m_erreur_t.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/calcs_thermic.f90
calcsa.o: Transdiffusion/calcsa.f90 m_precision.o m_source_tracer_t.o m_apport_t.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_xindic_s.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/calcsa.f90
calcul.o: Rezo/calcul.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_message_c.o m_erreur_t.o m_connect_t.o m_traiter_erreur_i.o m_debitance_s.o m_rezomat_t.o m_apport_pluie_t.o m_liaison_t.o m_casier_t.o m_survol_i.o m_bilvol_i.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/calcul.f90
calcul_mascaret.o: API/f90/calcul_mascaret.f90 m_erreur_t.o m_loi_t.o m_modele_mascaret_t.o m_etat_mascaret_t.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_extremite_t.o m_fichier_t.o m_profil_t.o m_profil_plan_t.o m_section_t.o m_section_plan_t.o m_singularite_t.o m_zone_seche_t.o m_sauve_t.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_index_variable_c.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_intersect_i.o m_apimascaret_static.o m_planim_i.o m_planma_i.o m_post_i.o m_post_imp_i.o m_pretrait_interface_i.o m_qcl_i.o m_rezo_interface_i.o m_sarap_i.o m_clpluie_i.o m_post_casier_i.o m_post_imp_casier_i.o m_stock_i.o m_stock_rep_i.o m_traiter_erreur_i.o m_traiter_erreur_casier_i.o m_mascaret_interface_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/calcul_mascaret.f90
calcul_mascaret_condition_limite.o: API/f90/calcul_mascaret_condition_limite.f90 m_erreur_t.o m_loi_t.o m_modele_mascaret_t.o m_etat_mascaret_t.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_extremite_t.o m_fichier_t.o m_profil_t.o m_profil_plan_t.o m_section_t.o m_section_plan_t.o m_singularite_t.o m_zone_seche_t.o m_sauve_t.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_index_variable_c.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_intersect_i.o m_apimascaret_static.o m_planim_i.o m_planma_i.o m_post_i.o m_post_imp_i.o m_pretrait_interface_i.o m_qcl_i.o m_rezo_interface_i.o m_sarap_i.o m_clpluie_i.o m_post_casier_i.o m_post_imp_casier_i.o m_stock_i.o m_stock_rep_i.o m_traiter_erreur_i.o m_traiter_erreur_casier_i.o m_mascaret_interface_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/calcul_mascaret_condition_limite.f90
caldeb.o: Planim/caldeb.f90 m_precision.o m_parametre_c.o m_debitance_s.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/caldeb.f90
caldyg.o: Planim/caldyg.f90 m_precision.o m_dicho_i.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/caldyg.f90
caleta.o: Mascaret/caleta.f90 m_precision.o m_erreur_t.o m_parametre_c.o m_cele_i.o m_csur_i.o m_presd_i.o m_dichodm_i.o m_dichod_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/caleta.f90
calk.o: Transdiffusion/calk.f90 m_precision.o m_parametre_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/calk.f90
calvar.o: Mascaret/calvar.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_section_plan_t.o m_cele_i.o m_csurm1_i.o m_minmaj_i.o m_dicho_i.o m_dichom_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/calvar.f90
carac.o: Mascaret/carac.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_aintdc_i.o m_bissnd_i.o m_dydxsc_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/carac.f90
carac3.o: Mascaret/carac3.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_aintdc_i.o m_bissnd_i.o m_dydxsc_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/carac3.f90
ccl.o: Rezo/ccl.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_connect_t.o m_erreur_t.o m_extremite_t.o m_profil_plan_t.o m_profil_t.o m_rhsbp_s.o m_debitance_s.o m_loilim_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/ccl.f90
cele.o: Mascaret/cele.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_dicho_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/cele.f90
cfl2d.o: Mascaret/cfl2d.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/cfl2d.f90
cfl2d1.o: Mascaret/cfl2d1.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/cfl2d1.f90
chainage_rezo.o: Rezo/chainage_rezo.f90 m_connect_t.o m_extremite_t.o m_rezomat_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_liaison_t.o m_casier_t.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/chainage_rezo.f90
clipp.o: Mascaret/clipp.f90 m_precision.o m_erreur_t.o m_csur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/clipp.f90
clpluie.o: Casier/clpluie.f90 m_loi_t.o m_apport_pluie_t.o m_erreur_t.o m_interpolation_s.o m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/clpluie.f90
compt_chenaux.o: Planim/compt_chenaux.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/compt_chenaux.f90
conflu.o: Mascaret/conflu.f90 m_precision.o m_erreur_t.o m_confluent_t.o m_constantes_calcul_c.o m_calcon_i.o m_cfl2d_i.o m_cfl2d1_i.o m_csur_i.o m_inigeo_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/conflu.f90
cout.o: Castor/cout.f90 m_precision.o m_parametre_c.o m_message_c.o m_strick_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Castor/cout.f90
cqinj.o: Rezo/cqinj.f90 m_precision.o m_parametre_c.o m_message_c.o m_fichier_t.o m_deversoir_t.o m_apport_t.o m_erreur_t.o m_interpolation_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/cqinj.f90
create_mascaret.o: API/f90/create_mascaret.f90 m_apimascaret_static.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/create_mascaret.f90
critiq.o: Sarap/critiq.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_rhsbp_s.o m_traiter_erreur_i.o m_repar_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/critiq.f90
csur.o: Mascaret/csur.f90 m_precision.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/csur.f90
csurm1.o: Mascaret/csurm1.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/csurm1.f90
csurmd.o: Mascaret/csurmd.f90 m_precision.o m_erreur_t.o m_dichod_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/csurmd.f90
dcopy.o: Lapack/dcopy.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dcopy.f
debita.o: Mascaret/debita.f90 m_precision.o m_dicho_i.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/debita.f90
decbar.o: Mascaret/decbar.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/decbar.f90
decoder_geom.o: Pretrait/decoder_geom.f90 m_precision.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/decoder_geom.f90
decoder_geom_casier.o: Casier/decoder_geom_casier.f90 m_precision.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/decoder_geom_casier.f90
delete_mascaret.o: API/f90/delete_mascaret.f90 m_apimascaret_static.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/delete_mascaret.f90
dgbsv.o: Lapack/dgbsv.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dgbsv.f
dgbtf2.o: Lapack/dgbtf2.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dgbtf2.f
dgbtrf.o: Lapack/dgbtrf.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dgbtrf.f
dgbtrs.o: Lapack/dgbtrs.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dgbtrs.f
dgemm.o: Lapack/dgemm.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dgemm.f
dgemv.o: Lapack/dgemv.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dgemv.f
dger.o: Lapack/dger.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dger.f
dgtsl.o: Mascaret/dgtsl.f90 m_precision.o m_parametre_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/dgtsl.f90
dicho.o: Planim/dicho.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/dicho.f90
dichod.o: Mascaret/dichod.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/dichod.f90
dichodm.o: Mascaret/dichodm.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/dichodm.f90
dichom.o: Mascaret/dichom.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/dichom.f90
diff_jcout.o: Castor/diff_jcout.f90 m_precision.o m_parametre_c.o m_message_c.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_strick_i.o m_permat_i.o m_xindic_s.o m_share_var.o m_diff_z_cf12_fwd_sarap_var.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Castor/diff_jcout.f90
diff_z_cf12_bwd_sarap.o: Deriv/diff_z_cf12_bwd_sarap.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_erreur_t.o m_debitance_s_b.o m_debitance_s.o m_traiter_erreur_i.o m_profil_t.o m_singularite_t.o m_interpolation_s.o m_interpolation_s_b.o m_rhsbp_s.o m_profil_plan_t.o m_rhsbp_s_b.o m_repar_i_b.o m_connect_t.o m_rhsb1_s.o m_num_bief_s.o m_froude_s.o m_repar_i.o m_psing_i_b.o m_critiq_i.o m_critiq_i_b.o m_confluent_t.o m_extremite_t.o m_calc_pc_conflu_i_b.o m_permat_i.o m_permat_i_b.o m_qnode_i_b.o m_qrepar_i_b.o m_fichier_t.o m_deversoir_t.o m_apport_t.o m_cqinj_i.o m_cqinj_i_b.o m_persar_i.o m_persar_i_b.o m_sarap_i_b.o m_share_var.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Deriv/diff_z_cf12_bwd_sarap.f90
diff_z_cf12_fwd_sarap.o: Deriv/diff_z_cf12_fwd_sarap.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_erreur_t.o m_traiter_erreur_i.o m_debitance_s_d.o m_profil_t.o m_singularite_t.o m_interpolation_s.o m_interpolation_s_d.o m_rhsbp_s.o m_profil_plan_t.o m_rhsbp_s_d.o m_repar_i_d.o m_connect_t.o m_rhsb1_s.o m_num_bief_s.o m_froude_s.o m_repar_i.o m_psing_i_d.o m_critiq_i_d.o m_confluent_t.o m_extremite_t.o m_calc_pc_conflu_i_d.o m_permat_i_d.o m_qnode_i_d.o m_qrepar_i_d.o m_apport_t.o m_deversoir_t.o m_cqinj_i.o m_persar_i_d.o m_sarap_i_d.o m_share_var.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Deriv/diff_z_cf12_fwd_sarap.f90
diffu_tracer.o: Transdiffusion/diffu_tracer.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_bissnd_tracer_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/diffu_tracer.f90
direct.o: Castor/direct.f90 m_precision.o m_share_var.o m_parametre_c.o m_message_c.o m_rhsbp_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_cout_i.o m_strick_i.o m_permat_i.o m_cqinj_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Castor/direct.f90
dlaswp.o: Lapack/dlaswp.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dlaswp.f
dpdz.o: Rezo/dpdz.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_connect_t.o m_num_bief_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/dpdz.f90
dscal.o: Lapack/dscal.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dscal.f
dswap.o: Lapack/dswap.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dswap.f
dtbsv.o: Lapack/dtbsv.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dtbsv.f
dtrsm.o: Lapack/dtrsm.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/dtrsm.f
duplicate_etat.o: API/f90/duplicate_etat.f90 m_etat_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/duplicate_etat.f90
dydxsc.o: Mascaret/dydxsc.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/dydxsc.f90
erodev.o: Mascaret/erodev.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_alarg_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/erodev.f90
err_alloc.o: Super/err_alloc.f90 m_erreur_t.o m_message_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Super/err_alloc.f90
export_xml.o: API/f90/export_xml.f90 m_apimascaret_static.o m_mascaret_t.o m_modele_mascaret_t.o m_constantes_calcul_c.o m_apimascaret_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/export_xml.f90
fc1.o: Mascaret/fc1.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/fc1.f90
fconj.o: Mascaret/fconj.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/fconj.f90
fh.o: Mascaret/fh.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/fh.f90
fludev.o: Mascaret/fludev.f90 m_precision.o m_interpolation_s.o m_parametre_c.o m_erreur_t.o m_qsing_i.o m_erodev_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/fludev.f90
fluroe.o: Mascaret/fluroe.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/fluroe.f90
flusrc.o: Mascaret/flusrc.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/flusrc.f90
fperd.o: Mascaret/fperd.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/fperd.f90
free_all_save_etat_mascaret.o: API/f90/free_all_save_etat_mascaret.f90 m_apimascaret_static.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/free_all_save_etat_mascaret.f90
free_save_etat_mascaret.o: API/f90/free_save_etat_mascaret.f90 m_apimascaret_static.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/free_save_etat_mascaret.f90
frottd.o: Mascaret/frottd.f90 m_precision.o m_erreur_t.o m_dichodm_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/frottd.f90
frotte.o: Mascaret/frotte.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/frotte.f90
get_condition_limite.o: API/f90/get_condition_limite.f90 m_apimascaret_static.o m_loi_t.o m_modele_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/get_condition_limite.f90
get_desc_var_mascaret.o: API/f90/get_desc_var_mascaret.f90 m_apimascaret_static.o m_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/get_desc_var_mascaret.f90
get_erreur_mascaret.o: API/f90/get_erreur_mascaret.f90 m_apimascaret_static.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/get_erreur_mascaret.f90
get_taille_var_mascaret.o: API/f90/get_taille_var_mascaret.f90 m_apimascaret_static.o m_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/get_taille_var_mascaret.f90
get_type_var_mascaret.o: API/f90/get_type_var_mascaret.f90 m_apimascaret_static.o m_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/get_type_var_mascaret.f90
get_valeur_mascaret.o: API/f90/get_valeur_mascaret.f90 m_apimascaret_static.o m_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/get_valeur_mascaret.f90
godunov.o: Transdiffusion/godunov.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/godunov.f90
homere_mascaret.o: ./homere_mascaret.f90 m_precision.o m_barrage_t.o m_loi_t.o m_section_t.o m_section_plan_t.o m_zone_seche_t.o m_sauve_t.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_index_variable_c.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_intersect_i.o m_planim_i.o m_planma_i.o m_mascaret_i.o m_post_i.o m_post_imp_i.o m_post_casier_i.o m_post_imp_casier_i.o m_stock_i.o m_stock_rep_i.o m_pretrait_i.o m_qcl_i.o m_rezo_i.o m_sarap_i.o m_clpluie_i.o m_traiter_erreur_i.o m_traiter_erreur_casier_i.o m_cqinj_i.o m_xindic_s.o m_rhsbp_s.o m_share_var.o m_constantes_calage_t.o m_calage_n2qn1_i.o m_diff_z_cf12_fwd_sarap_var.o m_diff_z_cf12_bwd_sarap_var.o m_index_variable_tracer_c.o m_parametres_qualite_eau_t.o m_meteo_t.o m_constantes_tracer_t.o m_loi_tracer_t.o m_source_tracer_t.o m_cond_lim_tracer_t.o m_tracer_i.o m_pretrait_tracer_i.o m_qcl_tracer_i.o m_post_imp_tracer_i.o m_stock_tracer_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./homere_mascaret.f90
hyp1fa.o: Transdiffusion/hyp1fa.f90 m_precision.o m_parametre_c.o m_bissnd_tracer_i.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/hyp1fa.f90
idamax.o: Lapack/idamax.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/idamax.f
ieeeck.o: Lapack/ieeeck.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/ieeeck.f
ilaenv.o: Lapack/ilaenv.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/ilaenv.f
import_modele_mascaret.o: API/f90/import_modele_mascaret.f90 m_apimascaret_static.o m_constantes_calcul_c.o m_pretrait_interface_i.o m_intersect_i.o m_planim_i.o m_planma_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/import_modele_mascaret.f90
import_xml.o: API/f90/import_xml.f90 m_apimascaret_static.o m_mascaret_t.o m_modele_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/import_xml.f90
indice.o: Mascaret/indice.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/indice.f90
inigeo.o: Mascaret/inigeo.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_csur_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/inigeo.f90
init_abaque.o: API/f90/init_abaque.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/init_abaque.f90
init_etat_mascaret.o: API/f90/init_etat_mascaret.f90 m_apimascaret_static.o m_modele_mascaret_t.o m_etat_mascaret_t.o m_constantes_calcul_c.o m_lec_ligne_interface_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/init_etat_mascaret.f90
init_ligne_mascaret.o: API/f90/init_ligne_mascaret.f90 m_apimascaret_static.o m_modele_mascaret_t.o m_etat_mascaret_t.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/init_ligne_mascaret.f90
intersect.o: Intersect/intersect.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_message_c.o m_connect_t.o m_erreur_t.o m_extremite_t.o m_profil_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Intersect/intersect.f90
invmat.o: Mascaret/invmat.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/invmat.f90
iparmq.o: Lapack/iparmq.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/iparmq.f
kliaison.o: Casier/kliaison.f90 m_precision.o m_parametre_c.o m_message_c.o m_connect_t.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o m_traiter_erreur_casier_i.o m_message_casier_c.o m_num_bief_s.o m_interpolation_s.o m_rhsbp_s.o m_liaison_seuil_i.o m_liaison_chenal_i.o m_liaison_siphon_i.o m_liaison_orifice_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/kliaison.f90
ksing.o: Rezo/ksing.f90 m_precision.o m_parametre_c.o m_message_c.o m_connect_t.o m_erreur_t.o m_profil_t.o m_singularite_t.o m_interpolation_s.o m_traiter_erreur_i.o m_num_bief_s.o m_rhsbp_s.o m_sing2_i.o m_sing3_i.o m_sing10_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/ksing.f90
lec_abaque.o: Pretrait/lec_abaque.f90 m_precision.o m_message_c.o m_erreur_t.o m_fichier_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_abaque.f90
lec_apport.o: Pretrait/lec_apport.f90 m_precision.o m_apport_t.o m_connect_t.o m_erreur_t.o m_loi_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_apport.f90
lec_apport_pluie.o: Casier/lec_apport_pluie.f90 m_apport_pluie_t.o m_erreur_t.o m_loi_t.o m_traiter_erreur_casier_i.o m_traiter_erreur_i.o m_message_casier_c.o m_precision.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/lec_apport_pluie.f90
lec_barrage.o: Pretrait/lec_barrage.f90 m_precision.o m_barrage_t.o m_connect_t.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_barrage.f90
lec_casier.o: Casier/lec_casier.f90 m_casier_t.o m_erreur_t.o m_parametre_c.o m_fichier_t.o m_message_casier_c.o m_constantes_casier_c.o m_traiter_erreur_casier_i.o m_traiter_erreur_i.o m_lec_geom_casier2_i.o m_lec_geom_casier1_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/lec_casier.f90
lec_conc_ini_tracer.o: Pretrait/lec_conc_ini_tracer.f90 m_precision.o m_fichier_t.o m_erreur_t.o m_connect_t.o m_profil_t.o m_parametre_c.o m_message_c.o m_message_tracer_c.o m_constantes_calcul_c.o m_lec_fic_conc_ini_tracer_i.o m_interpolation_s.o m_abs_abs_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_conc_ini_tracer.f90
lec_confluent.o: Pretrait/lec_confluent.f90 m_precision.o m_confluent_t.o m_connect_t.o m_erreur_t.o m_message_c.o m_constantes_calcul_c.o m_parametre_c.o m_traiter_erreur_i.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_confluent.f90
lec_dever.o: Pretrait/lec_dever.f90 m_precision.o m_connect_t.o m_erreur_t.o m_deversoir_t.o m_loi_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_xindic_s.o m_abs_abs_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_dever.f90
lec_fic_conc_ini_tracer.o: Pretrait/lec_fic_conc_ini_tracer.f90 m_precision.o m_parametre_c.o m_message_c.o m_message_tracer_c.o m_erreur_t.o m_fichier_t.o m_traiter_erreur_i.o m_lire_chaine_s.o m_opt2fort_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_fic_conc_ini_tracer.f90
lec_fic_loi_tracer.o: Pretrait/lec_fic_loi_tracer.f90 m_precision.o m_lire_chaine_s.o m_fichier_t.o m_loi_tracer_t.o m_erreur_t.o m_message_c.o m_message_tracer_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_fic_loi_tracer.f90
lec_frottement.o: Pretrait/lec_frottement.f90 m_precision.o m_erreur_t.o m_parametre_c.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o m_abs_abs_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_frottement.f90
lec_geom.o: Pretrait/lec_geom.f90 m_precision.o m_message_c.o m_parametre_c.o m_constantes_calcul_c.o m_profil_t.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_lec_geom_v2p0_i.o m_lec_geom_v3p0_i.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_geom.f90
lec_geom_casier1.o: Casier/lec_geom_casier1.f90 m_precision.o m_parametre_c.o m_fichier_t.o m_casier_t.o m_erreur_t.o m_message_casier_c.o m_traiter_erreur_casier_i.o m_decoder_geom_casier_i.o m_lire_chaine_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/lec_geom_casier1.f90
lec_geom_casier2.o: Casier/lec_geom_casier2.f90 m_precision.o m_fichier_t.o m_casier_t.o m_erreur_t.o m_message_casier_c.o m_traiter_erreur_casier_i.o m_decoder_geom_casier_i.o m_lire_chaine_s.o m_planim_casier_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/lec_geom_casier2.f90
lec_geom_v2p0.o: Pretrait/lec_geom_v2p0.f90 m_precision.o m_parametre_c.o m_profil_t.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_geom_v2p0.f90
lec_geom_v3p0.o: Pretrait/lec_geom_v3p0.f90 m_precision.o m_parametre_c.o m_fichier_t.o m_profil_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_decoder_geom_i.o m_lire_chaine_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_geom_v3p0.f90
lec_hydrau.o: Pretrait/lec_hydrau.f90 m_precision.o m_parametre_c.o m_loi_t.o m_erreur_t.o m_fichier_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_lire_chaine_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_hydrau.f90
lec_liaison.o: Casier/lec_liaison.f90 m_precision.o m_liaison_t.o m_erreur_t.o m_profil_t.o m_message_casier_c.o m_constantes_casier_c.o m_traiter_erreur_casier_i.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/lec_liaison.f90
lec_ligne.o: Pretrait/lec_ligne.f90 m_precision.o m_fichier_t.o m_erreur_t.o m_profil_t.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_lec_ligne_lido_i.o m_lec_ligne_opt_i.o m_interpolation_s.o m_abs_abs_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_ligne.f90
lec_ligne_interface.o: API/f90/lec_ligne_interface.f90 m_precision.o m_fichier_t.o m_erreur_t.o m_profil_t.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_lec_ligne_lido_i.o m_lec_ligne_opt_i.o m_interpolation_s.o m_abs_abs_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/lec_ligne_interface.f90
lec_ligne_lido.o: Pretrait/lec_ligne_lido.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_fichier_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_ligne_lido.f90
lec_ligne_opt.o: Pretrait/lec_ligne_opt.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_fichier_t.o m_traiter_erreur_i.o m_lire_chaine_s.o m_opt2fort_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_ligne_opt.f90
lec_loi.o: Pretrait/lec_loi.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_loi_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_lec_hydrau_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_loi.f90
lec_loi_interface.o: API/f90/lec_loi_interface.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_loi_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_lec_hydrau_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/lec_loi_interface.f90
lec_loi_tracer.o: Pretrait/lec_loi_tracer.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_loi_tracer_t.o m_message_c.o m_message_tracer_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_lec_fic_loi_tracer_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_loi_tracer.f90
lec_maillage.o: Pretrait/lec_maillage.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_maille_t.o m_message_c.o m_constantes_calcul_c.o m_lire_chaine_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_maillage.f90
lec_meteo.o: ModelesQualEau/lec_meteo.f90 m_precision.o m_meteo_t.o m_erreur_t.o m_fichier_t.o m_message_c.o m_traiter_erreur_i.o m_lire_chaine_s.o m_constantes_calcul_tracer_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/lec_meteo.f90
lec_parphy.o: ModelesQualEau/lec_parphy.f90 m_precision.o m_parametres_qualite_eau_t.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModelesQualEau/lec_parphy.f90
lec_pcsing.o: Pretrait/lec_pcsing.f90 m_precision.o m_erreur_t.o m_connect_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_pcsing.f90
lec_planim.o: Pretrait/lec_planim.f90 m_precision.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_planim.f90
lec_reseau.o: Pretrait/lec_reseau.f90 m_precision.o m_erreur_t.o m_extremite_t.o m_loi_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_parametre_c.o m_traiter_erreur_i.o m_abs_abs_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_reseau.f90
lec_sing.o: Pretrait/lec_sing.f90 m_precision.o m_connect_t.o m_erreur_t.o m_loi_t.o m_profil_t.o m_singularite_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_sing.f90
lec_sorties.o: Pretrait/lec_sorties.f90 m_precision.o m_erreur_t.o m_message_c.o m_constantes_calcul_c.o m_index_variable_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_sorties.f90
lec_source.o: Pretrait/lec_source.f90 m_precision.o m_parametre_c.o m_source_tracer_t.o m_apport_t.o m_connect_t.o m_erreur_t.o m_loi_tracer_t.o m_profil_t.o m_message_c.o m_message_tracer_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_source.f90
lec_stockage.o: Pretrait/lec_stockage.f90 m_precision.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_stockage.f90
lec_zone_seche.o: Pretrait/lec_zone_seche.f90 m_precision.o m_connect_t.o m_erreur_t.o m_profil_t.o m_zone_seche_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/lec_zone_seche.f90
liaison_chenal.o: Casier/liaison_chenal.f90 m_precision.o m_parametre_c.o m_message_c.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/liaison_chenal.f90
liaison_orifice.o: Casier/liaison_orifice.f90 m_precision.o m_parametre_c.o m_liaison_chenal_i.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/liaison_orifice.f90
liaison_seuil.o: Casier/liaison_seuil.f90 m_precision.o m_parametre_c.o m_message_c.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/liaison_seuil.f90
liaison_siphon.o: Casier/liaison_siphon.f90 m_precision.o m_parametre_c.o m_liaison_chenal_i.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/liaison_siphon.f90
limite.o: Mascaret/limite.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_akiv_i.o m_akivm1_i.o m_carac_i.o m_carac3_i.o m_cele_i.o m_csur_i.o m_csurm1_i.o m_frotte_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/limite.f90
loilim.o: Rezo/loilim.f90 m_precision.o m_parametre_c.o m_message_c.o m_interpolation_s.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/loilim.f90
lsame.o: Lapack/lsame.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/lsame.f
m_abs_abs_s.o: Modules/m_abs_abs_s.f90 m_precision.o m_erreur_t.o m_message_c.o m_profil_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_abs_abs_s.f90
m_aintdc_i.o: ModulesMascaret/m_aintdc_i.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_aintdc_i.f90
m_akiv_i.o: ModulesMascaret/m_akiv_i.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_akiv_i.f90
m_akivm1_i.o: ModulesMascaret/m_akivm1_i.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_akivm1_i.f90
m_alarg_i.o: ModulesMascaret/m_alarg_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_alarg_i.f90
m_algop_i.o: Modules/m_algop_i.f90 m_message_c.o m_erreur_t.o m_connect_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_algop_i.f90
m_apimascaret_i.o: ModulesAPI/m_apimascaret_i.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_apimascaret_i.f90
m_apimascaret_static.o: ModulesAPI/m_apimascaret_static.f90 m_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_apimascaret_static.f90
m_apport_pluie_t.o: ModulesCasier/m_apport_pluie_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_apport_pluie_t.f90
m_apport_t.o: Modules/m_apport_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_apport_t.f90
m_barrage_t.o: Modules/m_barrage_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_barrage_t.f90
m_bilan_i.o: ModulesMascaret/m_bilan_i.f90 m_precision.o m_connect_t.o m_erreur_t.o m_confluent_t.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_bilan_i.f90
m_bilmas_i.o: ModulesTracer/m_bilmas_i.f90 m_precision.o m_constantes_tracer_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_bilmas_i.f90
m_bilvol_i.o: ModulesCasier/m_bilvol_i.f90 m_precision.o m_constantes_calcul_c.o m_casier_t.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_casier_i.o m_message_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_bilvol_i.f90
m_bissn1_i.o: ModulesMascaret/m_bissn1_i.f90 m_precision.o m_erreur_t.o m_promat_i.o m_promvt_i.o m_invmat_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_bissn1_i.f90
m_bissnd_i.o: ModulesMascaret/m_bissnd_i.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_bissnd_i.f90
m_bissnd_tracer_i.o: ModulesTracer/m_bissnd_tracer_i.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_bissnd_tracer_i.f90
m_borne_i.o: ModulesMascaret/m_borne_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_indice_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_borne_i.f90
m_boussi_i.o: ModulesMascaret/m_boussi_i.f90 m_precision.o m_erreur_t.o m_parametre_c.o m_cele_i.o m_csur_i.o m_presd_i.o m_dichodm_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_boussi_i.f90
m_calage_n2qn1_i.o: ModulesCastor/m_calage_n2qn1_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_xindic_s.o m_strick_i.o m_direct_i.o m_simul_i.o m_share_var.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_calage_n2qn1_i.f90
m_calaig_i.o: Modules/m_calaig_i.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_calaig_i.f90
m_calc_connect_i.o: Modules/m_calc_connect_i.f90 m_precision.o m_connect_t.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_calc_connect_i.f90
m_calc_maillage_i.o: Modules/m_calc_maillage_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_fichier_t.o m_maille_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_lec_maillage_i.o m_mailler_i.o m_traiter_erreur_i.o m_abs_abs_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_calc_maillage_i.f90
m_calc_pc_conflu_i.o: Modules/m_calc_pc_conflu_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_connect_t.o m_confluent_t.o m_erreur_t.o m_profil_t.o m_profil_plan_t.o m_traiter_erreur_i.o m_rhsbp_s.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_calc_pc_conflu_i.f90
m_calc_pc_conflu_i_b.o: ModulesDeriv/m_calc_pc_conflu_i_b.f90 m_precision.o m_parametre_c.o m_message_c.o m_connect_t.o m_confluent_t.o m_erreur_t.o m_profil_t.o m_profil_plan_t.o m_traiter_erreur_i.o m_rhsbp_s_b.o m_interpolation_s_b.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_calc_pc_conflu_i_b.f90
m_calc_pc_conflu_i_d.o: ModulesDeriv/m_calc_pc_conflu_i_d.f90 m_precision.o m_parametre_c.o m_message_c.o m_connect_t.o m_confluent_t.o m_erreur_t.o m_profil_t.o m_profil_plan_t.o m_traiter_erreur_i.o m_rhsbp_s_d.o m_interpolation_s_d.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_calc_pc_conflu_i_d.f90
m_calcfl_i.o: ModulesMascaret/m_calcfl_i.f90 m_precision.o m_constantes_calcul_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_calcfl_i.f90
m_calcon_i.o: ModulesMascaret/m_calcon_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_flusrc_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_calcon_i.f90
m_calcs_biomass_i.o: ModulesTracer/m_calcs_biomass_i.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_meteo_t.o m_erreur_t.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_calcs_biomass_i.f90
m_calcs_eutro_i.o: ModulesTracer/m_calcs_eutro_i.f90 m_precision.o m_parametre_c.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_meteo_t.o m_erreur_t.o m_interpolation_s.o m_singularite_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_calcs_eutro_i.f90
m_calcs_micropol_i.o: ModulesTracer/m_calcs_micropol_i.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_calcs_micropol_i.f90
m_calcs_o2_i.o: ModulesTracer/m_calcs_o2_i.f90 m_precision.o m_parametre_c.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_singularite_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_calcs_o2_i.f90
m_calcs_rien_i.o: ModulesTracer/m_calcs_rien_i.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_calcs_rien_i.f90
m_calcs_thermic_i.o: ModulesTracer/m_calcs_thermic_i.f90 m_precision.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_meteo_t.o m_erreur_t.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_calcs_thermic_i.f90
m_calcsa_i.o: ModulesTracer/m_calcsa_i.f90 m_precision.o m_source_tracer_t.o m_apport_t.o m_constantes_tracer_t.o m_parametres_qualite_eau_t.o m_xindic_s.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_calcsa_i.f90
m_calcul_i.o: Modules/m_calcul_i.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_message_c.o m_erreur_t.o m_connect_t.o m_traiter_erreur_i.o m_debitance_s.o m_rezomat_t.o m_apport_pluie_t.o m_liaison_t.o m_casier_t.o m_survol_i.o m_bilvol_i.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_calcul_i.f90
m_caldeb_i.o: Modules/m_caldeb_i.f90 m_precision.o m_parametre_c.o m_debitance_s.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_caldeb_i.f90
m_caldyg_i.o: Modules/m_caldyg_i.f90 m_precision.o m_dicho_i.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_caldyg_i.f90
m_caleta_i.o: ModulesMascaret/m_caleta_i.f90 m_precision.o m_erreur_t.o m_parametre_c.o m_cele_i.o m_csur_i.o m_presd_i.o m_dichodm_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_caleta_i.f90
m_calint_i.o: Modules/m_calint_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_calint_i.f90
m_calk_i.o: ModulesTracer/m_calk_i.f90 m_precision.o m_parametre_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_calk_i.f90
m_calvar_i.o: ModulesMascaret/m_calvar_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_section_plan_t.o m_cele_i.o m_csurm1_i.o m_minmaj_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_calvar_i.f90
m_carac3_i.o: ModulesMascaret/m_carac3_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_aintdc_i.o m_bissnd_i.o m_dydxsc_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_carac3_i.f90
m_carac_i.o: ModulesMascaret/m_carac_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_aintdc_i.o m_bissnd_i.o m_dydxsc_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_carac_i.f90
m_casier_t.o: ModulesCasier/m_casier_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_casier_t.f90
m_ccl_i.o: Modules/m_ccl_i.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_connect_t.o m_erreur_t.o m_extremite_t.o m_profil_plan_t.o m_profil_t.o m_rhsbp_s.o m_debitance_s.o m_loilim_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_ccl_i.f90
m_cele_i.o: ModulesMascaret/m_cele_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_dicho_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_cele_i.f90
m_cfl2d1_i.o: ModulesMascaret/m_cfl2d1_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_cfl2d1_i.f90
m_cfl2d_i.o: ModulesMascaret/m_cfl2d_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_cfl2d_i.f90
m_chainage_rezo_i.o: Modules/m_chainage_rezo_i.f90 m_connect_t.o m_extremite_t.o m_rezomat_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_liaison_t.o m_casier_t.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_chainage_rezo_i.f90
m_clipp_i.o: ModulesMascaret/m_clipp_i.f90 m_precision.o m_erreur_t.o m_csur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_clipp_i.f90
m_clpluie_i.o: ModulesCasier/m_clpluie_i.f90 m_loi_t.o m_apport_pluie_t.o m_erreur_t.o m_interpolation_s.o m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_clpluie_i.f90
m_coeffs_i.o: Modules/m_coeffs_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_coeffs_i.f90
m_compt_chenaux_i.o: Modules/m_compt_chenaux_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_compt_chenaux_i.f90
m_cond_lim_tracer_t.o: ModulesTracer/m_cond_lim_tracer_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_cond_lim_tracer_t.f90
m_conden_i.o: Modules/m_conden_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_conden_i.f90
m_conflu_i.o: ModulesMascaret/m_conflu_i.f90 m_precision.o m_erreur_t.o m_confluent_t.o m_constantes_calcul_c.o m_calcon_i.o m_cfl2d_i.o m_cfl2d1_i.o m_csur_i.o m_inigeo_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_conflu_i.f90
m_confluent_t.o: Modules/m_confluent_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_confluent_t.f90
m_connect_t.o: Modules/m_connect_t.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_connect_t.f90
m_constantes_calage_t.o: ModulesCastor/m_constantes_calage_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_constantes_calage_t.f90
m_constantes_calcul_c.o: Modules/m_constantes_calcul_c.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_constantes_calcul_c.f90
m_constantes_calcul_tracer_c.o: ModulesTracer/m_constantes_calcul_tracer_c.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_constantes_calcul_tracer_c.f90
m_constantes_casier_c.o: ModulesCasier/m_constantes_casier_c.f90 m_precision.o m_parametre_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_constantes_casier_c.f90
m_constantes_tracer_t.o: ModulesTracer/m_constantes_tracer_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_constantes_tracer_t.f90
m_contq_i.o: Modules/m_contq_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_connect_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_contq_i.f90
m_cout_i.o: ModulesCastor/m_cout_i.f90 m_precision.o m_parametre_c.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_cout_i.f90
m_cqinj_i.o: Modules/m_cqinj_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_fichier_t.o m_deversoir_t.o m_apport_t.o m_erreur_t.o m_interpolation_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_cqinj_i.f90
m_cqinj_i_b.o: ModulesDeriv/m_cqinj_i_b.f90 m_precision.o m_parametre_c.o m_message_c.o m_fichier_t.o m_deversoir_t.o m_apport_t.o m_erreur_t.o m_interpolation_s.o m_interpolation_s_b.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_cqinj_i_b.f90
m_critiq_i.o: Modules/m_critiq_i.f90 m_precision.o m_parametre_c.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_rhsbp_s.o m_traiter_erreur_i.o m_repar_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_critiq_i.f90
m_critiq_i_b.o: ModulesDeriv/m_critiq_i_b.f90 m_precision.o m_parametre_c.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_rhsbp_s_b.o m_traiter_erreur_i.o m_repar_i_b.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_critiq_i_b.f90
m_critiq_i_d.o: ModulesDeriv/m_critiq_i_d.f90 m_precision.o m_parametre_c.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_rhsbp_s_d.o m_traiter_erreur_i.o m_repar_i_d.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_critiq_i_d.f90
m_csur_i.o: ModulesMascaret/m_csur_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_csur_i.f90
m_csurm1_i.o: ModulesMascaret/m_csurm1_i.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_csurm1_i.f90
m_csurmd_i.o: ModulesMascaret/m_csurmd_i.f90 m_precision.o m_erreur_t.o m_dichod_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_csurmd_i.f90
m_date_s.o: Modules/m_date_s.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_date_s.f90
m_debita_i.o: ModulesMascaret/m_debita_i.f90 m_precision.o m_dicho_i.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_debita_i.f90
m_debitance_s.o: Modules/m_debitance_s.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_debitance_s.f90
m_debitance_s_b.o: ModulesDeriv/m_debitance_s_b.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_debitance_s_b.f90
m_debitance_s_d.o: ModulesDeriv/m_debitance_s_d.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_debitance_s_d.f90
m_decbar_i.o: ModulesMascaret/m_decbar_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_decbar_i.f90
m_decoder_geom_casier_i.o: ModulesCasier/m_decoder_geom_casier_i.f90 m_precision.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_decoder_geom_casier_i.f90
m_decoder_geom_i.o: Modules/m_decoder_geom_i.f90 m_precision.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_decoder_geom_i.f90
m_deversoir_t.o: Modules/m_deversoir_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_deversoir_t.f90
m_dicho_i.o: Modules/m_dicho_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_dicho_i.f90
m_dichod_i.o: ModulesMascaret/m_dichod_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_dichod_i.f90
m_dichodm_i.o: ModulesMascaret/m_dichodm_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_dichodm_i.f90
m_dichom_i.o: ModulesMascaret/m_dichom_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_dichom_i.f90
m_diff_jcout_i.o: ModulesCastor/m_diff_jcout_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_strick_i.o m_permat_i.o m_xindic_s.o m_share_var.o m_diff_z_cf12_fwd_sarap_var.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_diff_jcout_i.f90
m_diff_z_cf12_bwd_sarap_var.o: ModulesDeriv/m_diff_z_cf12_bwd_sarap_var.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_diff_z_cf12_bwd_sarap_var.f90
m_diff_z_cf12_fwd_sarap_var.o: ModulesDeriv/m_diff_z_cf12_fwd_sarap_var.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_diff_z_cf12_fwd_sarap_var.f90
m_diffu_tracer_i.o: ModulesTracer/m_diffu_tracer_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_bissnd_tracer_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_diffu_tracer_i.f90
m_direct_i.o: ModulesCastor/m_direct_i.f90 m_precision.o m_share_var.o m_parametre_c.o m_message_c.o m_rhsbp_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_cout_i.o m_strick_i.o m_permat_i.o m_cqinj_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_direct_i.f90
m_donnees_calage_t.o: ModulesCastor/m_donnees_calage_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_donnees_calage_t.f90
m_donnees_crues_calage_t.o: ModulesCastor/m_donnees_crues_calage_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_donnees_crues_calage_t.f90
m_dpdz_i.o: Modules/m_dpdz_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_connect_t.o m_num_bief_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_dpdz_i.f90
m_dydxsc_i.o: ModulesMascaret/m_dydxsc_i.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_dydxsc_i.f90
m_erodev_i.o: ModulesMascaret/m_erodev_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_alarg_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_erodev_i.f90
m_erreur_t.o: Modules/m_erreur_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_erreur_t.f90
m_etat_casier_t.o: ModulesAPI/m_etat_casier_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_etat_casier_t.f90
m_etat_liaison_t.o: ModulesAPI/m_etat_liaison_t.f90 m_precision.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_etat_liaison_t.f90
m_etat_mascaret_t.o: ModulesAPI/m_etat_mascaret_t.f90 m_precision.o m_etat_liaison_t.o m_etat_casier_t.o m_rezomat_t.o m_sauve_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_etat_mascaret_t.f90
m_extremite_t.o: Modules/m_extremite_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_extremite_t.f90
m_fc1_i.o: ModulesMascaret/m_fc1_i.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_fc1_i.f90
m_fconj_i.o: ModulesMascaret/m_fconj_i.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_fconj_i.f90
m_fh_i.o: ModulesMascaret/m_fh_i.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_fh_i.f90
m_fichier_t.o: Modules/m_fichier_t.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_fichier_t.f90
m_fludev_i.o: ModulesMascaret/m_fludev_i.f90 m_precision.o m_interpolation_s.o m_parametre_c.o m_erreur_t.o m_qsing_i.o m_erodev_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_fludev_i.f90
m_fluroe_i.o: ModulesMascaret/m_fluroe_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_fluroe_i.f90
m_flusrc_i.o: ModulesMascaret/m_flusrc_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_flusrc_i.f90
m_fperd_i.o: ModulesMascaret/m_fperd_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_fperd_i.f90
m_frottd_i.o: ModulesMascaret/m_frottd_i.f90 m_precision.o m_erreur_t.o m_dichodm_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_frottd_i.f90
m_frotte_i.o: ModulesMascaret/m_frotte_i.f90 m_precision.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_frotte_i.f90
m_froude_s.o: Modules/m_froude_s.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_erreur_t.o m_connect_t.o m_traiter_erreur_i.o m_num_bief_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_froude_s.f90
m_gaussj_i.o: Modules/m_gaussj_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_gaussj_i.f90
m_godunov_i.o: ModulesTracer/m_godunov_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_godunov_i.f90
m_hyp1fa_i.o: ModulesTracer/m_hyp1fa_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_hyp1fa_i.f90
m_index_variable_c.o: Modules/m_index_variable_c.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_index_variable_c.f90
m_index_variable_tracer_c.o: ModulesTracer/m_index_variable_tracer_c.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_index_variable_tracer_c.f90
m_indice_i.o: ModulesMascaret/m_indice_i.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_indice_i.f90
m_inigeo_i.o: ModulesMascaret/m_inigeo_i.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_csur_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_inigeo_i.f90
m_init_var_sortie_s.o: Modules/m_init_var_sortie_s.f90 m_precision.o m_index_variable_c.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_init_var_sortie_s.f90
m_init_var_sortie_tracer_s.o: ModulesTracer/m_init_var_sortie_tracer_s.f90 m_precision.o m_index_variable_tracer_c.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_init_var_sortie_tracer_s.f90
m_interpolation_s.o: Modules/m_interpolation_s.f90 m_precision.o m_parametre_c.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_interpolation_s.f90
m_interpolation_s_b.o: ModulesDeriv/m_interpolation_s_b.f90 m_precision.o m_parametre_c.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_interpolation_s_b.f90
m_interpolation_s_d.o: ModulesDeriv/m_interpolation_s_d.f90 m_precision.o m_parametre_c.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_interpolation_s_d.f90
m_intersect_i.o: Modules/m_intersect_i.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_message_c.o m_connect_t.o m_erreur_t.o m_extremite_t.o m_profil_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_intersect_i.f90
m_invmat_i.o: ModulesMascaret/m_invmat_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_invmat_i.f90
m_kliaison_i.o: ModulesCasier/m_kliaison_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_connect_t.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o m_traiter_erreur_casier_i.o m_message_casier_c.o m_num_bief_s.o m_interpolation_s.o m_rhsbp_s.o m_liaison_seuil_i.o m_liaison_chenal_i.o m_liaison_siphon_i.o m_liaison_orifice_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_kliaison_i.f90
m_ksing_i.o: Modules/m_ksing_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_connect_t.o m_erreur_t.o m_profil_t.o m_singularite_t.o m_interpolation_s.o m_traiter_erreur_i.o m_num_bief_s.o m_rhsbp_s.o m_sing2_i.o m_sing3_i.o m_sing10_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_ksing_i.f90
m_lec_abaque_i.o: Modules/m_lec_abaque_i.f90 m_precision.o m_message_c.o m_erreur_t.o m_fichier_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_abaque_i.f90
m_lec_apport_i.o: Modules/m_lec_apport_i.f90 m_precision.o m_apport_t.o m_connect_t.o m_erreur_t.o m_loi_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_apport_i.f90
m_lec_apport_pluie_i.o: ModulesCasier/m_lec_apport_pluie_i.f90 m_apport_pluie_t.o m_erreur_t.o m_loi_t.o m_traiter_erreur_casier_i.o m_traiter_erreur_i.o m_message_casier_c.o m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_lec_apport_pluie_i.f90
m_lec_barrage_i.o: Modules/m_lec_barrage_i.f90 m_precision.o m_barrage_t.o m_connect_t.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_barrage_i.f90
m_lec_casier_i.o: ModulesCasier/m_lec_casier_i.f90 m_casier_t.o m_erreur_t.o m_parametre_c.o m_fichier_t.o m_message_casier_c.o m_constantes_casier_c.o m_traiter_erreur_casier_i.o m_traiter_erreur_i.o m_lec_geom_casier2_i.o m_lec_geom_casier1_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_lec_casier_i.f90
m_lec_conc_ini_tracer_i.o: ModulesTracer/m_lec_conc_ini_tracer_i.f90 m_precision.o m_fichier_t.o m_erreur_t.o m_connect_t.o m_profil_t.o m_parametre_c.o m_message_c.o m_message_tracer_c.o m_constantes_calcul_c.o m_lec_fic_conc_ini_tracer_i.o m_interpolation_s.o m_abs_abs_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_lec_conc_ini_tracer_i.f90
m_lec_confluent_i.o: Modules/m_lec_confluent_i.f90 m_precision.o m_confluent_t.o m_connect_t.o m_erreur_t.o m_message_c.o m_constantes_calcul_c.o m_parametre_c.o m_traiter_erreur_i.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_confluent_i.f90
m_lec_dever_i.o: Modules/m_lec_dever_i.f90 m_precision.o m_connect_t.o m_erreur_t.o m_deversoir_t.o m_loi_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_xindic_s.o m_abs_abs_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_dever_i.f90
m_lec_fic_conc_ini_tracer_i.o: ModulesTracer/m_lec_fic_conc_ini_tracer_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_message_tracer_c.o m_erreur_t.o m_fichier_t.o m_traiter_erreur_i.o m_lire_chaine_s.o m_opt2fort_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_lec_fic_conc_ini_tracer_i.f90
m_lec_fic_loi_tracer_i.o: ModulesTracer/m_lec_fic_loi_tracer_i.f90 m_precision.o m_lire_chaine_s.o m_fichier_t.o m_loi_tracer_t.o m_erreur_t.o m_message_c.o m_message_tracer_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_lec_fic_loi_tracer_i.f90
m_lec_frottement_i.o: Modules/m_lec_frottement_i.f90 m_precision.o m_erreur_t.o m_parametre_c.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o m_abs_abs_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_frottement_i.f90
m_lec_geom_casier1_i.o: ModulesCasier/m_lec_geom_casier1_i.f90 m_precision.o m_parametre_c.o m_fichier_t.o m_casier_t.o m_erreur_t.o m_message_casier_c.o m_traiter_erreur_casier_i.o m_decoder_geom_casier_i.o m_lire_chaine_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_lec_geom_casier1_i.f90
m_lec_geom_casier2_i.o: ModulesCasier/m_lec_geom_casier2_i.f90 m_precision.o m_fichier_t.o m_casier_t.o m_erreur_t.o m_message_casier_c.o m_traiter_erreur_casier_i.o m_decoder_geom_casier_i.o m_lire_chaine_s.o m_planim_casier_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_lec_geom_casier2_i.f90
m_lec_geom_i.o: Modules/m_lec_geom_i.f90 m_precision.o m_message_c.o m_parametre_c.o m_constantes_calcul_c.o m_profil_t.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_lec_geom_v2p0_i.o m_lec_geom_v3p0_i.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_geom_i.f90
m_lec_geom_v2p0_i.o: Modules/m_lec_geom_v2p0_i.f90 m_precision.o m_parametre_c.o m_profil_t.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_geom_v2p0_i.f90
m_lec_geom_v3p0_i.o: Modules/m_lec_geom_v3p0_i.f90 m_precision.o m_parametre_c.o m_fichier_t.o m_profil_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_decoder_geom_i.o m_lire_chaine_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_geom_v3p0_i.f90
m_lec_hydrau_i.o: Modules/m_lec_hydrau_i.f90 m_precision.o m_parametre_c.o m_loi_t.o m_erreur_t.o m_fichier_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_lire_chaine_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_hydrau_i.f90
m_lec_liaison_i.o: ModulesCasier/m_lec_liaison_i.f90 m_precision.o m_liaison_t.o m_profil_t.o m_erreur_t.o m_message_casier_c.o m_constantes_casier_c.o m_traiter_erreur_casier_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_lec_liaison_i.f90
m_lec_ligne_i.o: Modules/m_lec_ligne_i.f90 m_precision.o m_fichier_t.o m_erreur_t.o m_profil_t.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_lec_ligne_lido_i.o m_lec_ligne_opt_i.o m_interpolation_s.o m_abs_abs_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_ligne_i.f90
m_lec_ligne_interface_i.o: ModulesAPI/m_lec_ligne_interface_i.f90 m_precision.o m_fichier_t.o m_erreur_t.o m_profil_t.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_lec_ligne_lido_i.o m_lec_ligne_opt_i.o m_interpolation_s.o m_abs_abs_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_lec_ligne_interface_i.f90
m_lec_ligne_lido_i.o: Modules/m_lec_ligne_lido_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_fichier_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_ligne_lido_i.f90
m_lec_ligne_opt_i.o: Modules/m_lec_ligne_opt_i.f90 m_precision.o m_message_c.o m_parametre_c.o m_erreur_t.o m_fichier_t.o m_traiter_erreur_i.o m_lire_chaine_s.o m_opt2fort_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_ligne_opt_i.f90
m_lec_loi_i.o: Modules/m_lec_loi_i.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_loi_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_lec_hydrau_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_loi_i.f90
m_lec_loi_interface_i.o: ModulesAPI/m_lec_loi_interface_i.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_loi_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_lec_hydrau_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_lec_loi_interface_i.f90
m_lec_loi_tracer_i.o: ModulesTracer/m_lec_loi_tracer_i.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_loi_tracer_t.o m_message_c.o m_message_tracer_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_lec_fic_loi_tracer_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_lec_loi_tracer_i.f90
m_lec_maillage_i.o: Modules/m_lec_maillage_i.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_maille_t.o m_message_c.o m_constantes_calcul_c.o m_lire_chaine_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_maillage_i.f90
m_lec_pcsing_i.o: Modules/m_lec_pcsing_i.f90 m_precision.o m_erreur_t.o m_connect_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_pcsing_i.f90
m_lec_planim_i.o: Modules/m_lec_planim_i.f90 m_precision.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_planim_i.f90
m_lec_reseau_i.o: Modules/m_lec_reseau_i.f90 m_precision.o m_erreur_t.o m_extremite_t.o m_loi_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_parametre_c.o m_traiter_erreur_i.o m_abs_abs_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_reseau_i.f90
m_lec_sing_i.o: Modules/m_lec_sing_i.f90 m_precision.o m_connect_t.o m_erreur_t.o m_loi_t.o m_profil_t.o m_singularite_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_sing_i.f90
m_lec_sorties_i.o: Modules/m_lec_sorties_i.f90 m_precision.o m_erreur_t.o m_message_c.o m_constantes_calcul_c.o m_index_variable_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_sorties_i.f90
m_lec_source_i.o: ModulesTracer/m_lec_source_i.f90 m_precision.o m_parametre_c.o m_source_tracer_t.o m_apport_t.o m_connect_t.o m_erreur_t.o m_loi_tracer_t.o m_profil_t.o m_message_c.o m_message_tracer_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_lec_source_i.f90
m_lec_stockage_i.o: Modules/m_lec_stockage_i.f90 m_precision.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_stockage_i.f90
m_lec_zone_seche_i.o: Modules/m_lec_zone_seche_i.f90 m_precision.o m_connect_t.o m_erreur_t.o m_profil_t.o m_zone_seche_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_abs_abs_s.o m_xindic_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lec_zone_seche_i.f90
m_liaison_chenal_i.o: ModulesCasier/m_liaison_chenal_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_liaison_chenal_i.f90
m_liaison_orifice_i.o: ModulesCasier/m_liaison_orifice_i.f90 m_precision.o m_parametre_c.o m_liaison_chenal_i.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_liaison_orifice_i.f90
m_liaison_seuil_i.o: ModulesCasier/m_liaison_seuil_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_liaison_seuil_i.f90
m_liaison_siphon_i.o: ModulesCasier/m_liaison_siphon_i.f90 m_precision.o m_parametre_c.o m_liaison_chenal_i.o m_liaison_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_liaison_siphon_i.f90
m_liaison_t.o: ModulesCasier/m_liaison_t.f90 m_precision.o m_constantes_casier_c.o m_liaisonrc_t.o m_liaisoncc_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_liaison_t.f90
m_liaisoncc_t.o: ModulesCasier/m_liaisoncc_t.f90 m_precision.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_liaisoncc_t.f90
m_liaisonrc_t.o: ModulesCasier/m_liaisonrc_t.f90 m_precision.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_liaisonrc_t.f90
m_limite_i.o: ModulesMascaret/m_limite_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_akiv_i.o m_akivm1_i.o m_carac_i.o m_carac3_i.o m_cele_i.o m_csur_i.o m_csurm1_i.o m_frotte_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_limite_i.f90
m_lire_chaine_s.o: Modules/m_lire_chaine_s.f90 m_fichier_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_lire_chaine_s.f90
m_loi_t.o: Modules/m_loi_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_loi_t.f90
m_loi_tracer_t.o: ModulesTracer/m_loi_tracer_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_loi_tracer_t.f90
m_loilim_i.o: Modules/m_loilim_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_interpolation_s.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_loilim_i.f90
m_maille_t.o: Modules/m_maille_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_maille_t.f90
m_mailler_i.o: Modules/m_mailler_i.f90 m_precision.o m_parametre_c.o m_constantes_calcul_c.o m_profil_t.o m_maille_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_mailler_i.f90
m_mascaret_i.o: ModulesMascaret/m_mascaret_i.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_section_plan_t.o m_singularite_t.o m_zone_seche_t.o m_sauve_t.o m_interpolation_s.o m_limite_i.o m_bilan_i.o m_borne_i.o m_calcfl_i.o m_calvar_i.o m_conflu_i.o m_cqinj_i.o m_csur_i.o m_csurm1_i.o m_decbar_i.o m_frottd_i.o m_precal_i.o m_pres_i.o m_presd_i.o m_resol_i.o m_valida_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_mascaret_i.f90
m_mascaret_interface_i.o: ModulesAPI/m_mascaret_interface_i.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_section_plan_t.o m_singularite_t.o m_zone_seche_t.o m_sauve_t.o m_interpolation_s.o m_limite_i.o m_bilan_i.o m_borne_i.o m_calcfl_i.o m_calvar_i.o m_conflu_i.o m_cqinj_i.o m_csur_i.o m_csurm1_i.o m_decbar_i.o m_frottd_i.o m_precal_i.o m_pres_i.o m_presd_i.o m_resol_i.o m_valida_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_mascaret_interface_i.f90
m_mascaret_t.o: ModulesAPI/m_mascaret_t.f90 m_modele_mascaret_t.o m_etat_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_mascaret_t.f90
m_matri_i.o: ModulesMascaret/m_matri_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_matri_i.f90
m_matria_i.o: ModulesMascaret/m_matria_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_matria_i.f90
m_message_c.o: Modules/m_message_c.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_message_c.f90
m_message_casier_c.o: ModulesCasier/m_message_casier_c.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_message_casier_c.f90
m_message_tracer_c.o: ModulesTracer/m_message_tracer_c.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_message_tracer_c.f90
m_meteo_t.o: ModulesTracer/m_meteo_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_meteo_t.f90
m_minmaj_i.o: ModulesMascaret/m_minmaj_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_minmaj_i.f90
m_modele_mascaret_t.o: ModulesAPI/m_modele_mascaret_t.f90 m_precision.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_fichier_t.o m_loi_t.o m_profil_t.o m_profil_plan_t.o m_section_t.o m_section_plan_t.o m_singularite_t.o m_zone_seche_t.o m_sauve_t.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_index_variable_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_modele_mascaret_t.f90
m_muscl_hancock_i.o: ModulesTracer/m_muscl_hancock_i.f90 m_precision.o m_parametre_c.o m_constantes_tracer_t.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_muscl_hancock_i.f90
m_node_tracer_t.o: ModulesTracer/m_node_tracer_t.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_node_tracer_t.f90
m_nulmat_i.o: ModulesMascaret/m_nulmat_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_nulmat_i.f90
m_num_bief_s.o: Modules/m_num_bief_s.f90 m_connect_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_num_bief_s.f90
m_opt2fort_i.o: Modules/m_opt2fort_i.f90 m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_opt2fort_i.f90
m_papy_i.o: Modules/m_papy_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_papy_i.f90
m_parametre_c.o: Modules/m_parametre_c.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_parametre_c.f90
m_parametres_qualite_eau_t.o: ModulesTracer/m_parametres_qualite_eau_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_parametres_qualite_eau_t.f90
m_parphy_i.o: ModulesTracer/m_parphy_i.f90 m_precision.o m_parametres_qualite_eau_t.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_parphy_i.f90
m_permat_i.o: Modules/m_permat_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_erreur_t.o m_connect_t.o m_rhsbp_s.o m_rhsb1_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_repar_i.o m_psing_i.o m_critiq_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_permat_i.f90
m_permat_i_b.o: ModulesDeriv/m_permat_i_b.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_erreur_t.o m_connect_t.o m_rhsbp_s.o m_rhsbp_s_b.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_repar_i.o m_repar_i_b.o m_psing_i_b.o m_critiq_i_b.o m_critiq_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_permat_i_b.f90
m_permat_i_d.o: ModulesDeriv/m_permat_i_d.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_erreur_t.o m_connect_t.o m_rhsbp_s.o m_rhsbp_s_d.o m_rhsb1_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_repar_i.o m_repar_i_d.o m_psing_i_d.o m_critiq_i_d.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_permat_i_d.f90
m_persar_i.o: Modules/m_persar_i.f90 m_precision.o m_message_c.o m_confluent_t.o m_connect_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_singularite_t.o m_calc_pc_conflu_i.o m_permat_i.o m_qnode_i.o m_qrepar_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_persar_i.f90
m_persar_i_b.o: ModulesDeriv/m_persar_i_b.f90 m_precision.o m_message_c.o m_confluent_t.o m_connect_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_singularite_t.o m_calc_pc_conflu_i_b.o m_permat_i.o m_permat_i_b.o m_qnode_i_b.o m_qrepar_i_b.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_persar_i_b.f90
m_persar_i_d.o: ModulesDeriv/m_persar_i_d.f90 m_precision.o m_message_c.o m_confluent_t.o m_connect_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_singularite_t.o m_calc_pc_conflu_i_d.o m_permat_i_d.o m_qnode_i_d.o m_qrepar_i_d.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_persar_i_d.f90
m_pladeb_i.o: Modules/m_pladeb_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o m_compt_chenaux_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_pladeb_i.f90
m_planim_casier_i.o: ModulesCasier/m_planim_casier_i.f90 m_precision.o m_casier_t.o m_erreur_t.o m_message_casier_c.o m_constantes_casier_c.o m_parametre_c.o m_traiter_erreur_casier_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_planim_casier_i.f90
m_planim_i.o: Modules/m_planim_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_traiter_erreur_i.o m_papy_i.o m_pladeb_i.o m_planma_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_planim_i.f90
m_planma_i.o: Modules/m_planma_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_plan_t.o m_profil_t.o m_section_plan_t.o m_section_t.o m_connect_t.o m_erreur_t.o m_traiter_erreur_i.o m_calaig_i.o m_caldeb_i.o m_caldyg_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_planma_i.f90
m_post_casier_i.o: ModulesCasier/m_post_casier_i.f90 m_casier_t.o m_liaison_t.o m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_post_casier_i.f90
m_post_i.o: Modules/m_post_i.f90 m_precision.o m_profil_t.o m_profil_plan_t.o m_rhsbp_s.o m_constantes_calcul_c.o m_index_variable_c.o m_parametre_c.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_fichier_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_post_i.f90
m_post_imp_casier_i.o: ModulesCasier/m_post_imp_casier_i.f90 m_precision.o m_casier_t.o m_liaison_t.o m_erreur_t.o m_fichier_t.o m_constantes_calcul_c.o m_message_casier_c.o m_traiter_erreur_casier_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_post_imp_casier_i.f90
m_post_imp_i.o: Modules/m_post_imp_i.f90 m_precision.o m_constantes_calcul_c.o m_index_variable_c.o m_parametre_c.o m_message_c.o m_apport_t.o m_erreur_t.o m_connect_t.o m_init_var_sortie_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_post_imp_i.f90
m_post_imp_tracer_i.o: ModulesTracer/m_post_imp_tracer_i.f90 m_precision.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_fichier_t.o m_connect_t.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_post_imp_tracer_i.f90
m_precal_i.o: ModulesMascaret/m_precal_i.f90 m_precision.o m_message_c.o m_section_plan_t.o m_confluent_t.o m_erreur_t.o m_cele_i.o m_csur_i.o m_inigeo_i.o m_minmaj_i.o m_dicho_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_precal_i.f90
m_precision.o: Modules/m_precision.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_precision.f90
m_prelido_i.o: Modules/m_prelido_i.f90 m_precision.o m_apport_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_fichier_t.o m_loi_t.o m_maille_t.o m_profil_t.o m_singularite_t.o m_zone_seche_t.o m_constantes_calcul_c.o m_index_variable_c.o m_message_c.o m_parametre_c.o m_lec_geom_i.o m_mailler_i.o m_lec_ligne_i.o m_lec_hydrau_i.o m_algop_i.o m_traiter_erreur_i.o m_xindic_s.o m_date_s.o m_abs_abs_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_prelido_i.f90
m_pres_i.o: ModulesMascaret/m_pres_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_pres_i.f90
m_presd_i.o: ModulesMascaret/m_presd_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_dichod_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_presd_i.f90
m_pretrait_i.o: Modules/m_pretrait_i.f90 m_precision.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_fichier_t.o m_loi_t.o m_profil_t.o m_singularite_t.o m_zone_seche_t.o m_donnees_crues_calage_t.o m_donnees_calage_t.o m_constantes_calage_t.o m_index_variable_c.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_algop_i.o m_calc_connect_i.o m_calc_maillage_i.o m_lec_apport_i.o m_lec_barrage_i.o m_lec_confluent_i.o m_lec_dever_i.o m_lec_frottement_i.o m_lec_geom_i.o m_lec_ligne_i.o m_lec_loi_i.o m_lec_pcsing_i.o m_lec_planim_i.o m_lec_reseau_i.o m_lec_sing_i.o m_lec_sorties_i.o m_lec_stockage_i.o m_lec_zone_seche_i.o m_traiter_erreur_i.o m_xindic_s.o m_date_s.o m_abs_abs_s.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_lec_casier_i.o m_lec_liaison_i.o m_lec_apport_pluie_i.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_pretrait_i.f90
m_pretrait_interface_i.o: ModulesAPI/m_pretrait_interface_i.f90 m_precision.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_fichier_t.o m_loi_t.o m_profil_t.o m_singularite_t.o m_zone_seche_t.o m_index_variable_c.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_algop_i.o m_calc_connect_i.o m_calc_maillage_i.o m_lec_apport_i.o m_lec_barrage_i.o m_lec_confluent_i.o m_lec_dever_i.o m_lec_frottement_i.o m_lec_geom_i.o m_lec_ligne_i.o m_lec_loi_interface_i.o m_lec_pcsing_i.o m_lec_planim_i.o m_lec_reseau_i.o m_lec_sing_i.o m_lec_sorties_i.o m_lec_stockage_i.o m_lec_zone_seche_i.o m_traiter_erreur_i.o m_xindic_s.o m_date_s.o m_abs_abs_s.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_lec_casier_i.o m_lec_liaison_i.o m_lec_apport_pluie_i.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_pretrait_interface_i.f90
m_pretrait_tracer_i.o: ModulesTracer/m_pretrait_tracer_i.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_fichier_t.o m_connect_t.o m_apport_t.o m_profil_t.o m_extremite_t.o m_traiter_erreur_i.o m_parametres_qualite_eau_t.o m_meteo_t.o m_source_tracer_t.o m_loi_tracer_t.o m_constantes_tracer_t.o m_cond_lim_tracer_t.o m_constantes_calcul_tracer_c.o m_lec_conc_ini_tracer_i.o m_lec_loi_tracer_i.o m_lec_source_i.o m_erreur_t.o m_message_c.o m_message_tracer_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_pretrait_tracer_i.f90
m_profil_plan_t.o: Modules/m_profil_plan_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_profil_plan_t.f90
m_profil_t.o: Modules/m_profil_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_profil_t.f90
m_promat_i.o: ModulesMascaret/m_promat_i.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_promat_i.f90
m_promvt_i.o: ModulesMascaret/m_promvt_i.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_promvt_i.f90
m_psing_i.o: Modules/m_psing_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_profil_t.o m_singularite_t.o m_interpolation_s.o m_rhsbp_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_psing_i.f90
m_psing_i_b.o: ModulesDeriv/m_psing_i_b.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_profil_t.o m_singularite_t.o m_interpolation_s.o m_interpolation_s_b.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_psing_i_b.f90
m_psing_i_d.o: ModulesDeriv/m_psing_i_d.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_profil_t.o m_singularite_t.o m_interpolation_s.o m_interpolation_s_d.o m_rhsbp_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_psing_i_d.f90
m_qcl_i.o: Modules/m_qcl_i.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_apport_t.o m_connect_t.o m_erreur_t.o m_extremite_t.o m_loi_t.o m_singularite_t.o m_traiter_erreur_i.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_qcl_i.f90
m_qcl_tracer_i.o: ModulesTracer/m_qcl_tracer_i.f90 m_precision.o m_constantes_calcul_c.o m_source_tracer_t.o m_cond_lim_tracer_t.o m_loi_tracer_t.o m_extremite_t.o m_interpolation_s.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_qcl_tracer_i.f90
m_qnode_i.o: Modules/m_qnode_i.f90 m_precision.o m_message_c.o m_connect_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_qnode_i.f90
m_qnode_i_b.o: ModulesDeriv/m_qnode_i_b.f90 m_precision.o m_connect_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_qnode_i_b.f90
m_qnode_i_d.o: ModulesDeriv/m_qnode_i_d.f90 m_precision.o m_connect_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_qnode_i_d.f90
m_qrepar_i.o: Modules/m_qrepar_i.f90 m_precision.o m_connect_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_interpolation_s.o m_rhsbp_s.o m_traiter_erreur_i.o m_repar_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_qrepar_i.f90
m_qrepar_i_b.o: ModulesDeriv/m_qrepar_i_b.f90 m_precision.o m_connect_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_interpolation_s_b.o m_rhsbp_s_b.o m_traiter_erreur_i.o m_repar_i_b.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_qrepar_i_b.f90
m_qrepar_i_d.o: ModulesDeriv/m_qrepar_i_d.f90 m_precision.o m_connect_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_interpolation_s_d.o m_rhsbp_s_d.o m_traiter_erreur_i.o m_repar_i_d.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_qrepar_i_d.f90
m_qsing_i.o: ModulesMascaret/m_qsing_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_alarg_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_qsing_i.f90
m_rempli_i.o: Modules/m_rempli_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_connect_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_rempli_i.f90
m_repar_i.o: Modules/m_repar_i.f90 m_precision.o m_parametre_c.o m_constantes_calcul_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_repar_i.f90
m_repar_i_b.o: ModulesDeriv/m_repar_i_b.f90 m_precision.o m_parametre_c.o m_constantes_calcul_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_repar_i_b.f90
m_repar_i_d.o: ModulesDeriv/m_repar_i_d.f90 m_precision.o m_parametre_c.o m_constantes_calcul_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_repar_i_d.f90
m_resequ_i.o: ModulesTracer/m_resequ_i.f90 m_precision.o m_parametre_c.o m_constantes_tracer_t.o m_bissnd_tracer_i.o m_hyp1fa_i.o m_godunov_i.o m_muscl_hancock_i.o m_erreur_t.o m_diffu_tracer_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_resequ_i.f90
m_resol_i.o: ModulesMascaret/m_resol_i.f90 m_precision.o m_erreur_t.o m_section_plan_t.o m_singularite_t.o m_clipp_i.o m_solvro_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_resol_i.f90
m_rezo_i.o: Modules/m_rezo_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_apport_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_rezomat_t.o m_interpolation_s.o m_rhsbp_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_calcul_i.o m_contq_i.o m_ccl_i.o m_cqinj_i.o m_ksing_i.o m_calc_pc_conflu_i.o m_dpdz_i.o m_repar_i.o m_constantes_casier_c.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_kliaison_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_rezo_i.f90
m_rezo_interface_i.o: ModulesAPI/m_rezo_interface_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_apport_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_rezomat_t.o m_interpolation_s.o m_rhsbp_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_calcul_i.o m_chainage_rezo_i.o m_singularite_rezo_i.o m_contq_i.o m_ccl_i.o m_cqinj_i.o m_ksing_i.o m_calc_pc_conflu_i.o m_dpdz_i.o m_repar_i.o m_constantes_casier_c.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_kliaison_i.o m_apimascaret_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesAPI/m_rezo_interface_i.f90
m_rezomat_t.o: Modules/m_rezomat_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_rezomat_t.f90
m_rhsb1_s.o: Modules/m_rhsb1_s.f90 m_precision.o m_profil_t.o m_erreur_t.o m_traiter_erreur_i.o m_parametre_c.o m_message_c.o m_profil_plan_t.o m_fichier_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_rhsb1_s.f90
m_rhsbp_s.o: Modules/m_rhsbp_s.f90 m_precision.o m_profil_t.o m_erreur_t.o m_traiter_erreur_i.o m_parametre_c.o m_message_c.o m_profil_plan_t.o m_fichier_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_rhsbp_s.f90
m_rhsbp_s_b.o: ModulesDeriv/m_rhsbp_s_b.f90 m_precision.o m_message_c.o m_parametre_c.o m_profil_t.o m_profil_plan_t.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_rhsbp_s_b.f90
m_rhsbp_s_d.o: ModulesDeriv/m_rhsbp_s_d.f90 m_precision.o m_message_c.o m_parametre_c.o m_profil_t.o m_profil_plan_t.o m_fichier_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_rhsbp_s_d.f90
m_rieman_i.o: ModulesMascaret/m_rieman_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_fc1_i.o m_solex_i.o m_zbrent_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_rieman_i.f90
m_sarap_i.o: Modules/m_sarap_i.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_apport_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_num_bief_s.o m_rhsbp_s.o m_froude_s.o m_traiter_erreur_i.o m_cqinj_i.o m_persar_i.o m_repar_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_sarap_i.f90
m_sarap_i_b.o: ModulesDeriv/m_sarap_i_b.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_apport_t.o m_confluent_t.o m_connect_t.o m_extremite_t.o m_singularite_t.o m_deversoir_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_rhsbp_s.o m_traiter_erreur_i.o m_num_bief_s.o m_froude_s.o m_cqinj_i.o m_cqinj_i_b.o m_persar_i.o m_persar_i_b.o m_repar_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_sarap_i_b.f90
m_sarap_i_d.o: ModulesDeriv/m_sarap_i_d.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_apport_t.o m_confluent_t.o m_connect_t.o m_extremite_t.o m_singularite_t.o m_deversoir_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_rhsbp_s.o m_traiter_erreur_i.o m_num_bief_s.o m_froude_s.o m_cqinj_i.o m_persar_i_d.o m_repar_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesDeriv/m_sarap_i_d.f90
m_sauve_maillage_i.o: Modules/m_sauve_maillage_i.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_maille_t.o m_message_c.o m_constantes_calcul_c.o m_lire_chaine_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_sauve_maillage_i.f90
m_sauve_t.o: Modules/m_sauve_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_sauve_t.f90
m_section_plan_t.o: Modules/m_section_plan_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_section_plan_t.f90
m_section_t.o: Modules/m_section_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_section_t.f90
m_seuil_i.o: ModulesMascaret/m_seuil_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_fh_i.o m_fconj_i.o m_zbrent_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_seuil_i.f90
m_share_var.o: Modules/m_share_var.f90 m_apport_t.o m_confluent_t.o m_connect_t.o m_extremite_t.o m_singularite_t.o m_deversoir_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_donnees_calage_t.o m_donnees_crues_calage_t.o m_constantes_calage_t.o m_fichier_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_share_var.f90
m_simul_i.o: ModulesCastor/m_simul_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_rhsbp_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_cqinj_i.o m_diff_jcout_i.o m_cout_i.o m_xindic_s.o m_share_var.o m_stock_calage_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_simul_i.f90
m_sing10_i.o: Modules/m_sing10_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_sing10_i.f90
m_sing2_i.o: Modules/m_sing2_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_sing2_i.f90
m_sing3_i.o: Modules/m_sing3_i.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_sing3_i.f90
m_singularite_rezo_i.o: Modules/m_singularite_rezo_i.f90 m_rezomat_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_singularite_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_singularite_rezo_i.f90
m_singularite_t.o: Modules/m_singularite_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_singularite_t.f90
m_solex_i.o: ModulesMascaret/m_solex_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_solex_i.f90
m_solvro_i.o: ModulesMascaret/m_solvro_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_singularite_t.o m_bissn1_i.o m_caleta_i.o m_debita_i.o m_matri_i.o m_matria_i.o m_fludev_i.o m_fluroe_i.o m_presd_i.o m_source_i.o m_vvpropi_i.o m_vvprop_i.o m_boussi_i.o m_dichom_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_solvro_i.f90
m_source_i.o: ModulesMascaret/m_source_i.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_csur_i.o m_frottd_i.o m_presd_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_source_i.f90
m_source_tracer_t.o: ModulesTracer/m_source_tracer_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_source_tracer_t.f90
m_stock_calage_i.o: ModulesCastor/m_stock_calage_i.f90 m_erreur_t.o m_fichier_t.o m_precision.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_stock_calage_i.f90
m_stock_i.o: Modules/m_stock_i.f90 m_precision.o m_connect_t.o m_fichier_t.o m_erreur_t.o m_casier_t.o m_liaison_t.o m_constantes_calcul_c.o m_index_variable_c.o m_parametre_c.o m_init_var_sortie_s.o m_traiter_erreur_i.o m_traiter_erreur_casier_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_stock_i.f90
m_stock_rep_i.o: Modules/m_stock_rep_i.f90 m_precision.o m_connect_t.o m_fichier_t.o m_erreur_t.o m_constantes_calcul_c.o m_index_variable_c.o m_parametre_c.o m_init_var_sortie_s.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_stock_rep_i.f90
m_stock_tracer_i.o: ModulesTracer/m_stock_tracer_i.f90 m_precision.o m_connect_t.o m_fichier_t.o m_erreur_t.o m_constantes_calcul_c.o m_index_variable_tracer_c.o m_parametre_c.o m_init_var_sortie_tracer_s.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_stock_tracer_i.f90
m_strick_i.o: ModulesCastor/m_strick_i.f90 m_precision.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o m_abs_abs_s.o m_share_var.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCastor/m_strick_i.f90
m_survol_i.o: ModulesCasier/m_survol_i.f90 m_precision.o m_casier_t.o m_erreur_t.o m_traiter_erreur_casier_i.o m_message_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_survol_i.f90
m_tracer_i.o: ModulesTracer/m_tracer_i.f90 m_precision.o m_parametre_c.o m_connect_t.o m_singularite_t.o m_fichier_t.o m_resequ_i.o m_bilmas_i.o m_constantes_tracer_t.o m_constantes_calcul_c.o m_source_tracer_t.o m_parametres_qualite_eau_t.o m_cond_lim_tracer_t.o m_constantes_calcul_tracer_c.o m_calcsa_i.o m_calcs_rien_i.o m_calcs_o2_i.o m_calcs_biomass_i.o m_calcs_eutro_i.o m_calcs_micropol_i.o m_calcs_thermic_i.o m_parphy_i.o m_meteo_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_tracer_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesTracer/m_tracer_i.f90
m_traiter_erreur_casier_i.o: ModulesCasier/m_traiter_erreur_casier_i.f90 m_message_casier_c.o m_erreur_t.o m_fichier_t.o m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesCasier/m_traiter_erreur_casier_i.f90
m_traiter_erreur_i.o: Modules/m_traiter_erreur_i.f90 m_message_c.o m_erreur_t.o m_fichier_t.o m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_traiter_erreur_i.f90
m_valida_i.o: ModulesMascaret/m_valida_i.f90 m_precision.o m_erreur_t.o m_rieman_i.o m_seuil_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_valida_i.f90
m_vvprop_i.o: ModulesMascaret/m_vvprop_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_vvprop_i.f90
m_vvpropi_i.o: ModulesMascaret/m_vvpropi_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_vvpropi_i.f90
m_xindic_s.o: Modules/m_xindic_s.f90 m_precision.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_xindic_s.f90
m_zbrent_i.o: ModulesMascaret/m_zbrent_i.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	ModulesMascaret/m_zbrent_i.f90
m_zone_seche_t.o: Modules/m_zone_seche_t.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Modules/m_zone_seche_t.f90
mailler.o: Pretrait/mailler.f90 m_precision.o m_parametre_c.o m_constantes_calcul_c.o m_profil_t.o m_maille_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/mailler.f90
mascaret.o: Mascaret/mascaret.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_section_plan_t.o m_singularite_t.o m_zone_seche_t.o m_sauve_t.o m_interpolation_s.o m_limite_i.o m_bilan_i.o m_borne_i.o m_calcfl_i.o m_calvar_i.o m_conflu_i.o m_csur_i.o m_csurm1_i.o m_cqinj_i.o m_decbar_i.o m_frottd_i.o m_precal_i.o m_pres_i.o m_presd_i.o m_cele_i.o m_resol_i.o m_valida_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/mascaret.f90
mascaret_interface.o: API/f90/mascaret_interface.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_section_plan_t.o m_singularite_t.o m_zone_seche_t.o m_sauve_t.o m_interpolation_s.o m_limite_i.o m_bilan_i.o m_borne_i.o m_calcfl_i.o m_calvar_i.o m_conflu_i.o m_csur_i.o m_csurm1_i.o m_cqinj_i.o m_decbar_i.o m_frottd_i.o m_precal_i.o m_pres_i.o m_presd_i.o m_resol_i.o m_valida_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/mascaret_interface.f90
matri.o: Mascaret/matri.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/matri.f90
matria.o: Mascaret/matria.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/matria.f90
minmaj.o: Mascaret/minmaj.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/minmaj.f90
muscl_hancock.o: Transdiffusion/muscl_hancock.f90 m_precision.o m_parametre_c.o m_constantes_tracer_t.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/muscl_hancock.f90
n2qn1.o: N2QN1/n2qn1.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	N2QN1/n2qn1.f
nulmat.o: Mascaret/nulmat.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/nulmat.f90
opt2fort.o: Pretrait/opt2fort.f90 m_erreur_t.o m_traiter_erreur_i.o m_message_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/opt2fort.f90
papy.o: Planim/papy.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/papy.f90
permat.o: Sarap/permat.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_erreur_t.o m_connect_t.o m_rhsbp_s.o m_rhsb1_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_repar_i.o m_psing_i.o m_critiq_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/permat.f90
persar.o: Sarap/persar.f90 m_precision.o m_message_c.o m_confluent_t.o m_connect_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_singularite_t.o m_calc_pc_conflu_i.o m_permat_i.o m_qnode_i.o m_qrepar_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/persar.f90
pladeb.o: Planim/pladeb.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o m_compt_chenaux_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/pladeb.f90
planim.o: Planim/planim.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_traiter_erreur_i.o m_papy_i.o m_pladeb_i.o m_planma_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/planim.f90
planim_casier.o: Casier/planim_casier.f90 m_precision.o m_casier_t.o m_erreur_t.o m_message_casier_c.o m_constantes_casier_c.o m_parametre_c.o m_traiter_erreur_casier_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/planim_casier.f90
planma.o: Planim/planma.f90 m_precision.o m_parametre_c.o m_message_c.o m_profil_plan_t.o m_profil_t.o m_section_plan_t.o m_section_t.o m_connect_t.o m_erreur_t.o m_traiter_erreur_i.o m_calaig_i.o m_caldeb_i.o m_caldyg_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Planim/planma.f90
post.o: Post/post.f90 m_precision.o m_profil_t.o m_profil_plan_t.o m_rhsbp_s.o m_constantes_calcul_c.o m_index_variable_c.o m_parametre_c.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_fichier_t.o m_debitance_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Post/post.f90
post_casier.o: Casier/post_casier.f90 m_casier_t.o m_liaison_t.o m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/post_casier.f90
post_imp.o: Post/post_imp.f90 m_precision.o m_constantes_calcul_c.o m_index_variable_c.o m_parametre_c.o m_message_c.o m_apport_t.o m_erreur_t.o m_connect_t.o m_init_var_sortie_s.o m_traiter_erreur_i.o m_fichier_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Post/post_imp.f90
post_imp_casier.o: Casier/post_imp_casier.f90 m_precision.o m_casier_t.o m_liaison_t.o m_erreur_t.o m_fichier_t.o m_constantes_calcul_c.o m_message_casier_c.o m_traiter_erreur_casier_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/post_imp_casier.f90
post_imp_tracer.o: Post/post_imp_tracer.f90 m_precision.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_fichier_t.o m_connect_t.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Post/post_imp_tracer.f90
precal.o: Mascaret/precal.f90 m_precision.o m_message_c.o m_section_plan_t.o m_confluent_t.o m_erreur_t.o m_cele_i.o m_csur_i.o m_inigeo_i.o m_minmaj_i.o m_dicho_i.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/precal.f90
pres.o: Mascaret/pres.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_dicho_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/pres.f90
presd.o: Mascaret/presd.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_dichod_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/presd.f90
pretrait.o: Pretrait/pretrait.f90 m_precision.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_fichier_t.o m_loi_t.o m_profil_t.o m_singularite_t.o m_zone_seche_t.o m_donnees_crues_calage_t.o m_donnees_calage_t.o m_constantes_calage_t.o m_index_variable_c.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_algop_i.o m_calc_connect_i.o m_calc_maillage_i.o m_lec_apport_i.o m_lec_barrage_i.o m_lec_confluent_i.o m_lec_dever_i.o m_lec_frottement_i.o m_lec_geom_i.o m_lec_ligne_i.o m_lec_loi_i.o m_lec_pcsing_i.o m_lec_planim_i.o m_lec_reseau_i.o m_lec_sing_i.o m_lec_sorties_i.o m_lec_stockage_i.o m_lec_zone_seche_i.o m_traiter_erreur_i.o m_xindic_s.o m_date_s.o m_abs_abs_s.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_lec_casier_i.o m_lec_liaison_i.o m_lec_apport_pluie_i.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/pretrait.f90
pretrait_interface.o: API/f90/pretrait_interface.f90 m_precision.o m_apport_t.o m_barrage_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_fichier_t.o m_loi_t.o m_profil_t.o m_singularite_t.o m_zone_seche_t.o m_index_variable_c.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_algop_i.o m_calc_connect_i.o m_calc_maillage_i.o m_lec_apport_i.o m_lec_barrage_i.o m_lec_confluent_i.o m_lec_dever_i.o m_lec_frottement_i.o m_lec_geom_i.o m_lec_ligne_i.o m_lec_loi_interface_i.o m_lec_pcsing_i.o m_lec_planim_i.o m_lec_reseau_i.o m_lec_sing_i.o m_lec_sorties_i.o m_lec_stockage_i.o m_lec_zone_seche_i.o m_traiter_erreur_i.o m_xindic_s.o m_date_s.o m_abs_abs_s.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_lec_casier_i.o m_lec_liaison_i.o m_lec_apport_pluie_i.o m_constantes_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/pretrait_interface.f90
pretrait_tracer.o: Pretrait/pretrait_tracer.f90 m_precision.o m_constantes_calcul_c.o m_parametre_c.o m_fichier_t.o m_connect_t.o m_apport_t.o m_profil_t.o m_extremite_t.o m_traiter_erreur_i.o m_parametres_qualite_eau_t.o m_meteo_t.o m_source_tracer_t.o m_loi_tracer_t.o m_constantes_tracer_t.o m_cond_lim_tracer_t.o m_constantes_calcul_tracer_c.o m_lec_conc_ini_tracer_i.o m_lec_loi_tracer_i.o m_lec_source_i.o m_erreur_t.o m_message_c.o m_message_tracer_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/pretrait_tracer.f90
print_stat_masc.o: Super/print_stat_masc.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Super/print_stat_masc.f90
promat.o: Mascaret/promat.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/promat.f90
promvt.o: Mascaret/promvt.f90 m_precision.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/promvt.f90
psing.o: Sarap/psing.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_profil_t.o m_singularite_t.o m_interpolation_s.o m_rhsbp_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/psing.f90
qcl.o: Qcl/qcl.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_apport_t.o m_connect_t.o m_erreur_t.o m_extremite_t.o m_loi_t.o m_singularite_t.o m_traiter_erreur_i.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Qcl/qcl.f90
qcl_tracer.o: Qcl/qcl_tracer.f90 m_precision.o m_constantes_calcul_c.o m_source_tracer_t.o m_cond_lim_tracer_t.o m_loi_tracer_t.o m_extremite_t.o m_interpolation_s.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Qcl/qcl_tracer.f90
qnode.o: Sarap/qnode.f90 m_precision.o m_message_c.o m_connect_t.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/qnode.f90
qrepar.o: Sarap/qrepar.f90 m_precision.o m_message_c.o m_connect_t.o m_profil_t.o m_profil_plan_t.o m_erreur_t.o m_interpolation_s.o m_rhsbp_s.o m_traiter_erreur_i.o m_repar_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/qrepar.f90
qsing.o: Mascaret/qsing.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_alarg_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/qsing.f90
repar.o: Sarap/repar.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_erreur_t.o m_debitance_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/repar.f90
resequ.o: Transdiffusion/resequ.f90 m_precision.o m_parametre_c.o m_constantes_tracer_t.o m_bissnd_tracer_i.o m_hyp1fa_i.o m_erreur_t.o m_diffu_tracer_i.o m_godunov_i.o m_muscl_hancock_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/resequ.f90
resol.o: Mascaret/resol.f90 m_precision.o m_erreur_t.o m_section_plan_t.o m_singularite_t.o m_clipp_i.o m_solvro_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/resol.f90
rezo.o: Rezo/rezo.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_apport_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_rezomat_t.o m_interpolation_s.o m_rhsbp_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_calcul_i.o m_chainage_rezo_i.o m_singularite_rezo_i.o m_contq_i.o m_ccl_i.o m_cqinj_i.o m_ksing_i.o m_calc_pc_conflu_i.o m_dpdz_i.o m_repar_i.o m_constantes_casier_c.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_kliaison_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/rezo.f90
rezo_interface.o: API/f90/rezo_interface.f90 m_precision.o m_parametre_c.o m_message_c.o m_constantes_calcul_c.o m_apport_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_rezomat_t.o m_interpolation_s.o m_rhsbp_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_calcul_i.o m_chainage_rezo_i.o m_singularite_rezo_i.o m_contq_i.o m_ccl_i.o m_cqinj_i.o m_ksing_i.o m_calc_pc_conflu_i.o m_dpdz_i.o m_repar_i.o m_constantes_casier_c.o m_casier_t.o m_liaison_t.o m_apport_pluie_t.o m_kliaison_i.o m_apimascaret_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/rezo_interface.f90
rieman.o: Mascaret/rieman.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_fc1_i.o m_solex_i.o m_zbrent_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/rieman.f90
sarap.o: Sarap/sarap.f90 m_precision.o m_constantes_calcul_c.o m_message_c.o m_parametre_c.o m_apport_t.o m_confluent_t.o m_connect_t.o m_deversoir_t.o m_erreur_t.o m_extremite_t.o m_profil_t.o m_profil_plan_t.o m_singularite_t.o m_num_bief_s.o m_rhsbp_s.o m_froude_s.o m_traiter_erreur_i.o m_cqinj_i.o m_persar_i.o m_repar_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Sarap/sarap.f90
sauve_maillage.o: Pretrait/sauve_maillage.f90 m_precision.o m_erreur_t.o m_fichier_t.o m_maille_t.o m_message_c.o m_constantes_calcul_c.o m_lire_chaine_s.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Pretrait/sauve_maillage.f90
save_etat_mascaret.o: API/f90/save_etat_mascaret.f90 m_apimascaret_static.o m_etat_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/save_etat_mascaret.f90
set_etat_mascaret.o: API/f90/set_etat_mascaret.f90 m_apimascaret_static.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/set_etat_mascaret.f90
set_taille_var_mascaret.o: API/f90/set_taille_var_mascaret.f90 m_apimascaret_static.o m_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/set_taille_var_mascaret.f90
set_valeur_mascaret.o: API/f90/set_valeur_mascaret.f90 m_apimascaret_static.o m_mascaret_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/set_valeur_mascaret.f90
seuil.o: Mascaret/seuil.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_fh_i.o m_fconj_i.o m_zbrent_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/seuil.f90
simul.o: Castor/simul.f90 m_precision.o m_parametre_c.o m_message_c.o m_rhsbp_s.o m_num_bief_s.o m_froude_s.o m_traiter_erreur_i.o m_cqinj_i.o m_diff_jcout_i.o m_cout_i.o m_xindic_s.o m_share_var.o m_stock_calage_i.o m_strick_i.o m_direct_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Castor/simul.f90
sing10.o: Rezo/sing10.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/sing10.f90
sing2.o: Rezo/sing2.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o m_interpolation_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/sing2.f90
sing3.o: Rezo/sing3.f90 m_precision.o m_parametre_c.o m_message_c.o m_erreur_t.o m_traiter_erreur_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/sing3.f90
singularite_rezo.o: Rezo/singularite_rezo.f90 m_rezomat_t.o m_erreur_t.o m_message_c.o m_traiter_erreur_i.o m_singularite_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Rezo/singularite_rezo.f90
solex.o: Mascaret/solex.f90 m_precision.o m_parametre_c.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/solex.f90
solvro.o: Mascaret/solvro.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_singularite_t.o m_bissn1_i.o m_boussi_i.o m_caleta_i.o m_debita_i.o m_matri_i.o m_matria_i.o m_fludev_i.o m_fluroe_i.o m_presd_i.o m_source_i.o m_vvpropi_i.o m_vvprop_i.o m_dichom_i.o m_csur_i.o m_csurm1_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/solvro.f90
source.o: Mascaret/source.f90 m_precision.o m_parametre_c.o m_erreur_t.o m_csur_i.o m_frottd_i.o m_presd_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/source.f90
stock.o: Stock/stock.F90 m_precision.o m_connect_t.o m_fichier_t.o m_erreur_t.o m_casier_t.o m_liaison_t.o m_constantes_calcul_c.o m_index_variable_c.o m_parametre_c.o m_init_var_sortie_s.o m_traiter_erreur_i.o m_traiter_erreur_casier_i.o m_message_c.o m_num_bief_s.o m_date_s.o m_message_casier_c.o m_constantes_casier_c.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	Stock/stock.F90
stock_calage.o: Castor/stock_calage.f90 m_erreur_t.o m_fichier_t.o m_precision.o m_constantes_calcul_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Castor/stock_calage.f90
stock_rep.o: Stock/stock_rep.f90 m_precision.o m_connect_t.o m_fichier_t.o m_erreur_t.o m_constantes_calcul_c.o m_index_variable_c.o m_parametre_c.o m_init_var_sortie_s.o m_traiter_erreur_i.o m_message_c.o m_date_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Stock/stock_rep.f90
stock_tracer.o: Stock/stock_tracer.f90 m_precision.o m_connect_t.o m_fichier_t.o m_erreur_t.o m_constantes_calcul_c.o m_index_variable_tracer_c.o m_parametre_c.o m_init_var_sortie_tracer_s.o m_traiter_erreur_i.o m_message_c.o m_num_bief_s.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Stock/stock_tracer.f90
strick.o: Castor/strick.f90 m_precision.o m_erreur_t.o m_profil_t.o m_message_c.o m_constantes_calcul_c.o m_traiter_erreur_i.o m_xindic_s.o m_abs_abs_s.o m_share_var.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Castor/strick.f90
survol.o: Casier/survol.f90 m_precision.o m_parametre_c.o m_casier_t.o m_erreur_t.o m_traiter_erreur_casier_i.o m_message_casier_c.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Casier/survol.f90
tracer.o: Transdiffusion/tracer.f90 m_precision.o m_parametre_c.o m_connect_t.o m_singularite_t.o m_fichier_t.o m_resequ_i.o m_bilmas_i.o m_constantes_tracer_t.o m_constantes_calcul_c.o m_source_tracer_t.o m_parametres_qualite_eau_t.o m_cond_lim_tracer_t.o m_constantes_calcul_tracer_c.o m_calcsa_i.o m_calcs_rien_i.o m_calcs_o2_i.o m_calcs_biomass_i.o m_calcs_eutro_i.o m_calcs_micropol_i.o m_calcs_thermic_i.o m_parphy_i.o m_meteo_t.o m_erreur_t.o m_traiter_erreur_i.o m_message_tracer_c.o m_message_c.o m_node_tracer_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Transdiffusion/tracer.f90
valida.o: Mascaret/valida.f90 m_precision.o m_erreur_t.o m_rieman_i.o m_seuil_i.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/valida.f90
version_mascaret.o: API/f90/version_mascaret.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	API/f90/version_mascaret.f90
vvprop.o: Mascaret/vvprop.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/vvprop.f90
vvpropi.o: Mascaret/vvpropi.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/vvpropi.f90
xerbla.o: Lapack/xerbla.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Lapack/xerbla.f
y12maf.o: Y12M/y12maf.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Y12M/y12maf.f
y12mbf.o: Y12M/y12mbf.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Y12M/y12mbf.f
y12mcf.o: Y12M/y12mcf.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Y12M/y12mcf.f
y12mdf.o: Y12M/y12mdf.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Y12M/y12mdf.f
zbrent.o: Mascaret/zbrent.f90 m_precision.o m_erreur_t.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	Mascaret/zbrent.f90
./m_maille_t.f90: Modules/m_maille_t.f90
	cp Modules/m_maille_t.f90 .
./m_caleta_i.f90: ModulesMascaret/m_caleta_i.f90
	cp ModulesMascaret/m_caleta_i.f90 .
./m_clipp_i.f90: ModulesMascaret/m_clipp_i.f90
	cp ModulesMascaret/m_clipp_i.f90 .
./m_post_imp_casier_i.f90: ModulesCasier/m_post_imp_casier_i.f90
	cp ModulesCasier/m_post_imp_casier_i.f90 .
./m_pretrait_interface_i.f90: ModulesAPI/m_pretrait_interface_i.f90
	cp ModulesAPI/m_pretrait_interface_i.f90 .
./m_index_variable_c.f90: Modules/m_index_variable_c.f90
	cp Modules/m_index_variable_c.f90 .
./intersect.f90: Intersect/intersect.f90
	cp Intersect/intersect.f90 .
./post.f90: Post/post.f90
	cp Post/post.f90 .
./ccl.f90: Rezo/ccl.f90
	cp Rezo/ccl.f90 .
./m_calc_connect_i.f90: Modules/m_calc_connect_i.f90
	cp Modules/m_calc_connect_i.f90 .
./lec_ligne_lido.f90: Pretrait/lec_ligne_lido.f90
	cp Pretrait/lec_ligne_lido.f90 .
./m_lec_frottement_i.f90: Modules/m_lec_frottement_i.f90
	cp Modules/m_lec_frottement_i.f90 .
./m_psing_i_d.f90: ModulesDeriv/m_psing_i_d.f90
	cp ModulesDeriv/m_psing_i_d.f90 .
./m_constantes_casier_c.f90: ModulesCasier/m_constantes_casier_c.f90
	cp ModulesCasier/m_constantes_casier_c.f90 .
./m_sing10_i.f90: Modules/m_sing10_i.f90
	cp Modules/m_sing10_i.f90 .
./fperd.f90: Mascaret/fperd.f90
	cp Mascaret/fperd.f90 .
./m_compt_chenaux_i.f90: Modules/m_compt_chenaux_i.f90
	cp Modules/m_compt_chenaux_i.f90 .
./mailler.f90: Pretrait/mailler.f90
	cp Pretrait/mailler.f90 .
./m_decoder_geom_casier_i.f90: ModulesCasier/m_decoder_geom_casier_i.f90
	cp ModulesCasier/m_decoder_geom_casier_i.f90 .
./dgbtrf.f: Lapack/dgbtrf.f
	cp Lapack/dgbtrf.f .
./m_apimascaret_static.f90: ModulesAPI/m_apimascaret_static.f90
	cp ModulesAPI/m_apimascaret_static.f90 .
./xerbla.f: Lapack/xerbla.f
	cp Lapack/xerbla.f .
./m_lec_geom_casier1_i.f90: ModulesCasier/m_lec_geom_casier1_i.f90
	cp ModulesCasier/m_lec_geom_casier1_i.f90 .
./matria.f90: Mascaret/matria.f90
	cp Mascaret/matria.f90 .
./m_constantes_tracer_t.f90: ModulesTracer/m_constantes_tracer_t.f90
	cp ModulesTracer/m_constantes_tracer_t.f90 .
./source.f90: Mascaret/source.f90
	cp Mascaret/source.f90 .
./dichod.f90: Mascaret/dichod.f90
	cp Mascaret/dichod.f90 .
./m_sarap_i_d.f90: ModulesDeriv/m_sarap_i_d.f90
	cp ModulesDeriv/m_sarap_i_d.f90 .
./m_parametre_c.f90: Modules/m_parametre_c.f90
	cp Modules/m_parametre_c.f90 .
./m_precision.f90: Modules/m_precision.f90
	cp Modules/m_precision.f90 .
./m_rhsbp_s_d.f90: ModulesDeriv/m_rhsbp_s_d.f90
	cp ModulesDeriv/m_rhsbp_s_d.f90 .
./sing3.f90: Rezo/sing3.f90
	cp Rezo/sing3.f90 .
./m_lec_ligne_lido_i.f90: Modules/m_lec_ligne_lido_i.f90
	cp Modules/m_lec_ligne_lido_i.f90 .
./delete_mascaret.f90: API/f90/delete_mascaret.f90
	cp API/f90/delete_mascaret.f90 .
./m_csurmd_i.f90: ModulesMascaret/m_csurmd_i.f90
	cp ModulesMascaret/m_csurmd_i.f90 .
./invmat.f90: Mascaret/invmat.f90
	cp Mascaret/invmat.f90 .
./m_post_imp_i.f90: Modules/m_post_imp_i.f90
	cp Modules/m_post_imp_i.f90 .
./m_fludev_i.f90: ModulesMascaret/m_fludev_i.f90
	cp ModulesMascaret/m_fludev_i.f90 .
./seuil.f90: Mascaret/seuil.f90
	cp Mascaret/seuil.f90 .
./m_lec_ligne_opt_i.f90: Modules/m_lec_ligne_opt_i.f90
	cp Modules/m_lec_ligne_opt_i.f90 .
./iparmq.f: Lapack/iparmq.f
	cp Lapack/iparmq.f .
./m_stock_rep_i.f90: Modules/m_stock_rep_i.f90
	cp Modules/m_stock_rep_i.f90 .
./m_qnode_i_b.f90: ModulesDeriv/m_qnode_i_b.f90
	cp ModulesDeriv/m_qnode_i_b.f90 .
./m_survol_i.f90: ModulesCasier/m_survol_i.f90
	cp ModulesCasier/m_survol_i.f90 .
./dgemm.f: Lapack/dgemm.f
	cp Lapack/dgemm.f .
./m_persar_i.f90: Modules/m_persar_i.f90
	cp Modules/m_persar_i.f90 .
./lec_frottement.f90: Pretrait/lec_frottement.f90
	cp Pretrait/lec_frottement.f90 .
./calcs_rien.f90: ModelesQualEau/calcs_rien.f90
	cp ModelesQualEau/calcs_rien.f90 .
./resol.f90: Mascaret/resol.f90
	cp Mascaret/resol.f90 .
./akiv.f90: Mascaret/akiv.f90
	cp Mascaret/akiv.f90 .
./m_lec_geom_casier2_i.f90: ModulesCasier/m_lec_geom_casier2_i.f90
	cp ModulesCasier/m_lec_geom_casier2_i.f90 .
./vvpropi.f90: Mascaret/vvpropi.f90
	cp Mascaret/vvpropi.f90 .
./m_sarap_i.f90: Modules/m_sarap_i.f90
	cp Modules/m_sarap_i.f90 .
./m_opt2fort_i.f90: Modules/m_opt2fort_i.f90
	cp Modules/m_opt2fort_i.f90 .
./caldeb.f90: Planim/caldeb.f90
	cp Planim/caldeb.f90 .
./m_source_tracer_t.f90: ModulesTracer/m_source_tracer_t.f90
	cp ModulesTracer/m_source_tracer_t.f90 .
./lec_conc_ini_tracer.f90: Pretrait/lec_conc_ini_tracer.f90
	cp Pretrait/lec_conc_ini_tracer.f90 .
./m_vvprop_i.f90: ModulesMascaret/m_vvprop_i.f90
	cp ModulesMascaret/m_vvprop_i.f90 .
./dgtsl.f90: Mascaret/dgtsl.f90
	cp Mascaret/dgtsl.f90 .
./m_lec_planim_i.f90: Modules/m_lec_planim_i.f90
	cp Modules/m_lec_planim_i.f90 .
./m_coeffs_i.f90: Modules/m_coeffs_i.f90
	cp Modules/m_coeffs_i.f90 .
./m_borne_i.f90: ModulesMascaret/m_borne_i.f90
	cp ModulesMascaret/m_borne_i.f90 .
./m_algop_i.f90: Modules/m_algop_i.f90
	cp Modules/m_algop_i.f90 .
./m_liaison_orifice_i.f90: ModulesCasier/m_liaison_orifice_i.f90
	cp ModulesCasier/m_liaison_orifice_i.f90 .
./m_qsing_i.f90: ModulesMascaret/m_qsing_i.f90
	cp ModulesMascaret/m_qsing_i.f90 .
./duplicate_etat.f90: API/f90/duplicate_etat.f90
	cp API/f90/duplicate_etat.f90 .
./m_critiq_i_b.f90: ModulesDeriv/m_critiq_i_b.f90
	cp ModulesDeriv/m_critiq_i_b.f90 .
./dgbsv.f: Lapack/dgbsv.f
	cp Lapack/dgbsv.f .
./m_singularite_rezo_i.f90: Modules/m_singularite_rezo_i.f90
	cp Modules/m_singularite_rezo_i.f90 .
./m_minmaj_i.f90: ModulesMascaret/m_minmaj_i.f90
	cp ModulesMascaret/m_minmaj_i.f90 .
./m_hyp1fa_i.f90: ModulesTracer/m_hyp1fa_i.f90
	cp ModulesTracer/m_hyp1fa_i.f90 .
./calcs_biomass.f90: ModelesQualEau/calcs_biomass.f90
	cp ModelesQualEau/calcs_biomass.f90 .
./fh.f90: Mascaret/fh.f90
	cp Mascaret/fh.f90 .
./post_casier.f90: Casier/post_casier.f90
	cp Casier/post_casier.f90 .
./bilmas.f90: Transdiffusion/bilmas.f90
	cp Transdiffusion/bilmas.f90 .
./m_apport_t.f90: Modules/m_apport_t.f90
	cp Modules/m_apport_t.f90 .
./m_zone_seche_t.f90: Modules/m_zone_seche_t.f90
	cp Modules/m_zone_seche_t.f90 .
./m_clpluie_i.f90: ModulesCasier/m_clpluie_i.f90
	cp ModulesCasier/m_clpluie_i.f90 .
./m_stock_calage_i.f90: ModulesCastor/m_stock_calage_i.f90
	cp ModulesCastor/m_stock_calage_i.f90 .
./m_conden_i.f90: Modules/m_conden_i.f90
	cp Modules/m_conden_i.f90 .
./bissn1.f90: Mascaret/bissn1.f90
	cp Mascaret/bissn1.f90 .
./import_modele_mascaret.f90: API/f90/import_modele_mascaret.f90
	cp API/f90/import_modele_mascaret.f90 .
./diff_jcout.f90: Castor/diff_jcout.f90
	cp Castor/diff_jcout.f90 .
./m_erodev_i.f90: ModulesMascaret/m_erodev_i.f90
	cp ModulesMascaret/m_erodev_i.f90 .
./m_sing2_i.f90: Modules/m_sing2_i.f90
	cp Modules/m_sing2_i.f90 .
./m_calcon_i.f90: ModulesMascaret/m_calcon_i.f90
	cp ModulesMascaret/m_calcon_i.f90 .
./m_valida_i.f90: ModulesMascaret/m_valida_i.f90
	cp ModulesMascaret/m_valida_i.f90 .
./m_cfl2d_i.f90: ModulesMascaret/m_cfl2d_i.f90
	cp ModulesMascaret/m_cfl2d_i.f90 .
./m_dichom_i.f90: ModulesMascaret/m_dichom_i.f90
	cp ModulesMascaret/m_dichom_i.f90 .
./m_cfl2d1_i.f90: ModulesMascaret/m_cfl2d1_i.f90
	cp ModulesMascaret/m_cfl2d1_i.f90 .
./zbrent.f90: Mascaret/zbrent.f90
	cp Mascaret/zbrent.f90 .
./m_bilvol_i.f90: ModulesCasier/m_bilvol_i.f90
	cp ModulesCasier/m_bilvol_i.f90 .
./calk.f90: Transdiffusion/calk.f90
	cp Transdiffusion/calk.f90 .
./save_etat_mascaret.f90: API/f90/save_etat_mascaret.f90
	cp API/f90/save_etat_mascaret.f90 .
./m_promvt_i.f90: ModulesMascaret/m_promvt_i.f90
	cp ModulesMascaret/m_promvt_i.f90 .
./post_imp_tracer.f90: Post/post_imp_tracer.f90
	cp Post/post_imp_tracer.f90 .
./boussi.f90: Mascaret/boussi.f90
	cp Mascaret/boussi.f90 .
./m_repar_i_b.f90: ModulesDeriv/m_repar_i_b.f90
	cp ModulesDeriv/m_repar_i_b.f90 .
./critiq.f90: Sarap/critiq.f90
	cp Sarap/critiq.f90 .
./free_save_etat_mascaret.f90: API/f90/free_save_etat_mascaret.f90
	cp API/f90/free_save_etat_mascaret.f90 .
./m_akiv_i.f90: ModulesMascaret/m_akiv_i.f90
	cp ModulesMascaret/m_akiv_i.f90 .
./m_calcs_thermic_i.f90: ModulesTracer/m_calcs_thermic_i.f90
	cp ModulesTracer/m_calcs_thermic_i.f90 .
./m_liaisoncc_t.f90: ModulesCasier/m_liaisoncc_t.f90
	cp ModulesCasier/m_liaisoncc_t.f90 .
./dichom.f90: Mascaret/dichom.f90
	cp Mascaret/dichom.f90 .
./m_lec_confluent_i.f90: Modules/m_lec_confluent_i.f90
	cp Modules/m_lec_confluent_i.f90 .
./stock_rep.f90: Stock/stock_rep.f90
	cp Stock/stock_rep.f90 .
./m_calcul_i.f90: Modules/m_calcul_i.f90
	cp Modules/m_calcul_i.f90 .
./calage_n2qn1.f90: Castor/calage_n2qn1.f90
	cp Castor/calage_n2qn1.f90 .
./m_kliaison_i.f90: ModulesCasier/m_kliaison_i.f90
	cp ModulesCasier/m_kliaison_i.f90 .
./m_fc1_i.f90: ModulesMascaret/m_fc1_i.f90
	cp ModulesMascaret/m_fc1_i.f90 .
./m_planim_i.f90: Modules/m_planim_i.f90
	cp Modules/m_planim_i.f90 .
./m_num_bief_s.f90: Modules/m_num_bief_s.f90
	cp Modules/m_num_bief_s.f90 .
./calcs_thermic.f90: ModelesQualEau/calcs_thermic.f90
	cp ModelesQualEau/calcs_thermic.f90 .
./lec_confluent.f90: Pretrait/lec_confluent.f90
	cp Pretrait/lec_confluent.f90 .
./m_persar_i_b.f90: ModulesDeriv/m_persar_i_b.f90
	cp ModulesDeriv/m_persar_i_b.f90 .
./m_lec_liaison_i.f90: ModulesCasier/m_lec_liaison_i.f90
	cp ModulesCasier/m_lec_liaison_i.f90 .
./m_bissn1_i.f90: ModulesMascaret/m_bissn1_i.f90
	cp ModulesMascaret/m_bissn1_i.f90 .
./inigeo.f90: Mascaret/inigeo.f90
	cp Mascaret/inigeo.f90 .
./m_confluent_t.f90: Modules/m_confluent_t.f90
	cp Modules/m_confluent_t.f90 .
./m_traiter_erreur_i.f90: Modules/m_traiter_erreur_i.f90
	cp Modules/m_traiter_erreur_i.f90 .
./m_section_plan_t.f90: Modules/m_section_plan_t.f90
	cp Modules/m_section_plan_t.f90 .
./ksing.f90: Rezo/ksing.f90
	cp Rezo/ksing.f90 .
./m_lec_sorties_i.f90: Modules/m_lec_sorties_i.f90
	cp Modules/m_lec_sorties_i.f90 .
./m_lec_maillage_i.f90: Modules/m_lec_maillage_i.f90
	cp Modules/m_lec_maillage_i.f90 .
./simul.f90: Castor/simul.f90
	cp Castor/simul.f90 .
./m_calk_i.f90: ModulesTracer/m_calk_i.f90
	cp ModulesTracer/m_calk_i.f90 .
./free_all_save_etat_mascaret.f90: API/f90/free_all_save_etat_mascaret.f90
	cp API/f90/free_all_save_etat_mascaret.f90 .
./bilan.f90: Mascaret/bilan.f90
	cp Mascaret/bilan.f90 .
./m_decbar_i.f90: ModulesMascaret/m_decbar_i.f90
	cp ModulesMascaret/m_decbar_i.f90 .
./calcs_eutro.f90: ModelesQualEau/calcs_eutro.f90
	cp ModelesQualEau/calcs_eutro.f90 .
./stock.F90: Stock/stock.F90
	cp Stock/stock.F90 .
./pretrait_tracer.f90: Pretrait/pretrait_tracer.f90
	cp Pretrait/pretrait_tracer.f90 .
./planim_casier.f90: Casier/planim_casier.f90
	cp Casier/planim_casier.f90 .
./psing.f90: Sarap/psing.f90
	cp Sarap/psing.f90 .
./m_pres_i.f90: ModulesMascaret/m_pres_i.f90
	cp ModulesMascaret/m_pres_i.f90 .
./frottd.f90: Mascaret/frottd.f90
	cp Mascaret/frottd.f90 .
./m_lec_apport_pluie_i.f90: ModulesCasier/m_lec_apport_pluie_i.f90
	cp ModulesCasier/m_lec_apport_pluie_i.f90 .
./m_dydxsc_i.f90: ModulesMascaret/m_dydxsc_i.f90
	cp ModulesMascaret/m_dydxsc_i.f90 .
./m_meteo_t.f90: ModulesTracer/m_meteo_t.f90
	cp ModulesTracer/m_meteo_t.f90 .
./qrepar.f90: Sarap/qrepar.f90
	cp Sarap/qrepar.f90 .
./m_diff_z_cf12_fwd_sarap_var.f90: ModulesDeriv/m_diff_z_cf12_fwd_sarap_var.f90
	cp ModulesDeriv/m_diff_z_cf12_fwd_sarap_var.f90 .
./lec_sing.f90: Pretrait/lec_sing.f90
	cp Pretrait/lec_sing.f90 .
./m_lec_barrage_i.f90: Modules/m_lec_barrage_i.f90
	cp Modules/m_lec_barrage_i.f90 .
./m_apport_pluie_t.f90: ModulesCasier/m_apport_pluie_t.f90
	cp ModulesCasier/m_apport_pluie_t.f90 .
./m_tracer_i.f90: ModulesTracer/m_tracer_i.f90
	cp ModulesTracer/m_tracer_i.f90 .
./m_indice_i.f90: ModulesMascaret/m_indice_i.f90
	cp ModulesMascaret/m_indice_i.f90 .
./m_akivm1_i.f90: ModulesMascaret/m_akivm1_i.f90
	cp ModulesMascaret/m_akivm1_i.f90 .
./calcs_mes.f90: ModelesQualEau/calcs_mes.f90
	cp ModelesQualEau/calcs_mes.f90 .
./qsing.f90: Mascaret/qsing.f90
	cp Mascaret/qsing.f90 .
./m_mailler_i.f90: Modules/m_mailler_i.f90
	cp Modules/m_mailler_i.f90 .
./m_calcs_micropol_i.f90: ModulesTracer/m_calcs_micropol_i.f90
	cp ModulesTracer/m_calcs_micropol_i.f90 .
./bissnd_tracer.f90: Transdiffusion/bissnd_tracer.f90
	cp Transdiffusion/bissnd_tracer.f90 .
./dydxsc.f90: Mascaret/dydxsc.f90
	cp Mascaret/dydxsc.f90 .
./m_sauve_t.f90: Modules/m_sauve_t.f90
	cp Modules/m_sauve_t.f90 .
./m_lec_sing_i.f90: Modules/m_lec_sing_i.f90
	cp Modules/m_lec_sing_i.f90 .
./m_constantes_calcul_tracer_c.f90: ModulesTracer/m_constantes_calcul_tracer_c.f90
	cp ModulesTracer/m_constantes_calcul_tracer_c.f90 .
./m_critiq_i.f90: Modules/m_critiq_i.f90
	cp Modules/m_critiq_i.f90 .
./m_extremite_t.f90: Modules/m_extremite_t.f90
	cp Modules/m_extremite_t.f90 .
./get_condition_limite.f90: API/f90/get_condition_limite.f90
	cp API/f90/get_condition_limite.f90 .
./get_desc_var_mascaret.f90: API/f90/get_desc_var_mascaret.f90
	cp API/f90/get_desc_var_mascaret.f90 .
./calaig.f90: Planim/calaig.f90
	cp Planim/calaig.f90 .
./m_diffu_tracer_i.f90: ModulesTracer/m_diffu_tracer_i.f90
	cp ModulesTracer/m_diffu_tracer_i.f90 .
./m_carac3_i.f90: ModulesMascaret/m_carac3_i.f90
	cp ModulesMascaret/m_carac3_i.f90 .
./algop.f90: Pretrait/algop.f90
	cp Pretrait/algop.f90 .
./m_message_tracer_c.f90: ModulesTracer/m_message_tracer_c.f90
	cp ModulesTracer/m_message_tracer_c.f90 .
./create_mascaret.f90: API/f90/create_mascaret.f90
	cp API/f90/create_mascaret.f90 .
./m_planim_casier_i.f90: ModulesCasier/m_planim_casier_i.f90
	cp ModulesCasier/m_planim_casier_i.f90 .
./planma.f90: Planim/planma.f90
	cp Planim/planma.f90 .
./m_donnees_crues_calage_t.f90: ModulesCastor/m_donnees_crues_calage_t.f90
	cp ModulesCastor/m_donnees_crues_calage_t.f90 .
./m_date_s.f90: Modules/m_date_s.f90
	cp Modules/m_date_s.f90 .
./lec_geom_v3p0.f90: Pretrait/lec_geom_v3p0.f90
	cp Pretrait/lec_geom_v3p0.f90 .
./m_lec_zone_seche_i.f90: Modules/m_lec_zone_seche_i.f90
	cp Modules/m_lec_zone_seche_i.f90 .
./m_resequ_i.f90: ModulesTracer/m_resequ_i.f90
	cp ModulesTracer/m_resequ_i.f90 .
./strick.f90: Castor/strick.f90
	cp Castor/strick.f90 .
./m_pretrait_i.f90: Modules/m_pretrait_i.f90
	cp Modules/m_pretrait_i.f90 .
./mascaret_interface.f90: API/f90/mascaret_interface.f90
	cp API/f90/mascaret_interface.f90 .
./m_diff_jcout_i.f90: ModulesCastor/m_diff_jcout_i.f90
	cp ModulesCastor/m_diff_jcout_i.f90 .
./m_chainage_rezo_i.f90: Modules/m_chainage_rezo_i.f90
	cp Modules/m_chainage_rezo_i.f90 .
./borne.f90: Mascaret/borne.f90
	cp Mascaret/borne.f90 .
./conflu.f90: Mascaret/conflu.f90
	cp Mascaret/conflu.f90 .
./ilaenv.f: Lapack/ilaenv.f
	cp Lapack/ilaenv.f .
./n2qn1.f: N2QN1/n2qn1.f
	cp N2QN1/n2qn1.f .
./m_papy_i.f90: Modules/m_papy_i.f90
	cp Modules/m_papy_i.f90 .
./adstack.c: Deriv/adstack.c
	cp Deriv/adstack.c .
./m_calaig_i.f90: Modules/m_calaig_i.f90
	cp Modules/m_calaig_i.f90 .
./m_simul_i.f90: ModulesCastor/m_simul_i.f90
	cp ModulesCastor/m_simul_i.f90 .
./m_debitance_s_d.f90: ModulesDeriv/m_debitance_s_d.f90
	cp ModulesDeriv/m_debitance_s_d.f90 .
./m_inigeo_i.f90: ModulesMascaret/m_inigeo_i.f90
	cp ModulesMascaret/m_inigeo_i.f90 .
./m_permat_i_b.f90: ModulesDeriv/m_permat_i_b.f90
	cp ModulesDeriv/m_permat_i_b.f90 .
./permat.f90: Sarap/permat.f90
	cp Sarap/permat.f90 .
./rezo.f90: Rezo/rezo.f90
	cp Rezo/rezo.f90 .
./lec_apport_pluie.f90: Casier/lec_apport_pluie.f90
	cp Casier/lec_apport_pluie.f90 .
./m_calc_pc_conflu_i_b.f90: ModulesDeriv/m_calc_pc_conflu_i_b.f90
	cp ModulesDeriv/m_calc_pc_conflu_i_b.f90 .
./lec_loi.f90: Pretrait/lec_loi.f90
	cp Pretrait/lec_loi.f90 .
./m_apimascaret_i.f90: ModulesAPI/m_apimascaret_i.f90
	cp ModulesAPI/m_apimascaret_i.f90 .
./get_taille_var_mascaret.f90: API/f90/get_taille_var_mascaret.f90
	cp API/f90/get_taille_var_mascaret.f90 .
./m_precal_i.f90: ModulesMascaret/m_precal_i.f90
	cp ModulesMascaret/m_precal_i.f90 .
./pladeb.f90: Planim/pladeb.f90
	cp Planim/pladeb.f90 .
./m_godunov_i.f90: ModulesTracer/m_godunov_i.f90
	cp ModulesTracer/m_godunov_i.f90 .
./m_source_i.f90: ModulesMascaret/m_source_i.f90
	cp ModulesMascaret/m_source_i.f90 .
./dscal.f: Lapack/dscal.f
	cp Lapack/dscal.f .
./m_repar_i_d.f90: ModulesDeriv/m_repar_i_d.f90
	cp ModulesDeriv/m_repar_i_d.f90 .
./m_caldyg_i.f90: Modules/m_caldyg_i.f90
	cp Modules/m_caldyg_i.f90 .
./m_froude_s.f90: Modules/m_froude_s.f90
	cp Modules/m_froude_s.f90 .
./clipp.f90: Mascaret/clipp.f90
	cp Mascaret/clipp.f90 .
./cout.f90: Castor/cout.f90
	cp Castor/cout.f90 .
./lec_sorties.f90: Pretrait/lec_sorties.f90
	cp Pretrait/lec_sorties.f90 .
./vvprop.f90: Mascaret/vvprop.f90
	cp Mascaret/vvprop.f90 .
./m_ksing_i.f90: Modules/m_ksing_i.f90
	cp Modules/m_ksing_i.f90 .
./lec_geom_casier1.f90: Casier/lec_geom_casier1.f90
	cp Casier/lec_geom_casier1.f90 .
./m_deversoir_t.f90: Modules/m_deversoir_t.f90
	cp Modules/m_deversoir_t.f90 .
./godunov.f90: Transdiffusion/godunov.f90
	cp Transdiffusion/godunov.f90 .
./m_rezo_i.f90: Modules/m_rezo_i.f90
	cp Modules/m_rezo_i.f90 .
./m_ccl_i.f90: Modules/m_ccl_i.f90
	cp Modules/m_ccl_i.f90 .
./m_liaison_seuil_i.f90: ModulesCasier/m_liaison_seuil_i.f90
	cp ModulesCasier/m_liaison_seuil_i.f90 .
./m_muscl_hancock_i.f90: ModulesTracer/m_muscl_hancock_i.f90
	cp ModulesTracer/m_muscl_hancock_i.f90 .
./m_matri_i.f90: ModulesMascaret/m_matri_i.f90
	cp ModulesMascaret/m_matri_i.f90 .
./m_calcs_o2_i.f90: ModulesTracer/m_calcs_o2_i.f90
	cp ModulesTracer/m_calcs_o2_i.f90 .
./presd.f90: Mascaret/presd.f90
	cp Mascaret/presd.f90 .
./m_calcsa_i.f90: ModulesTracer/m_calcsa_i.f90
	cp ModulesTracer/m_calcsa_i.f90 .
./m_cqinj_i.f90: Modules/m_cqinj_i.f90
	cp Modules/m_cqinj_i.f90 .
./m_diff_z_cf12_bwd_sarap_var.f90: ModulesDeriv/m_diff_z_cf12_bwd_sarap_var.f90
	cp ModulesDeriv/m_diff_z_cf12_bwd_sarap_var.f90 .
./resequ.f90: Transdiffusion/resequ.f90
	cp Transdiffusion/resequ.f90 .
./m_lec_fic_conc_ini_tracer_i.f90: ModulesTracer/m_lec_fic_conc_ini_tracer_i.f90
	cp ModulesTracer/m_lec_fic_conc_ini_tracer_i.f90 .
./m_permat_i.f90: Modules/m_permat_i.f90
	cp Modules/m_permat_i.f90 .
./solvro.f90: Mascaret/solvro.f90
	cp Mascaret/solvro.f90 .
./calvar.f90: Mascaret/calvar.f90
	cp Mascaret/calvar.f90 .
./csurm1.f90: Mascaret/csurm1.f90
	cp Mascaret/csurm1.f90 .
./m_calcs_rien_i.f90: ModulesTracer/m_calcs_rien_i.f90
	cp ModulesTracer/m_calcs_rien_i.f90 .
./m_casier_t.f90: ModulesCasier/m_casier_t.f90
	cp ModulesCasier/m_casier_t.f90 .
./m_calc_pc_conflu_i.f90: Modules/m_calc_pc_conflu_i.f90
	cp Modules/m_calc_pc_conflu_i.f90 .
./y12mcf.f: Y12M/y12mcf.f
	cp Y12M/y12mcf.f .
./frotte.f90: Mascaret/frotte.f90
	cp Mascaret/frotte.f90 .
./get_type_var_mascaret.f90: API/f90/get_type_var_mascaret.f90
	cp API/f90/get_type_var_mascaret.f90 .
./lec_geom_v2p0.f90: Pretrait/lec_geom_v2p0.f90
	cp Pretrait/lec_geom_v2p0.f90 .
./calc_maillage.f90: Pretrait/calc_maillage.f90
	cp Pretrait/calc_maillage.f90 .
./limite.f90: Mascaret/limite.f90
	cp Mascaret/limite.f90 .
./dtrsm.f: Lapack/dtrsm.f
	cp Lapack/dtrsm.f .
./dcopy.f: Lapack/dcopy.f
	cp Lapack/dcopy.f .
./cele.f90: Mascaret/cele.f90
	cp Mascaret/cele.f90 .
./calcon.f90: Mascaret/calcon.f90
	cp Mascaret/calcon.f90 .
./liaison_seuil.f90: Casier/liaison_seuil.f90
	cp Casier/liaison_seuil.f90 .
./m_bilmas_i.f90: ModulesTracer/m_bilmas_i.f90
	cp ModulesTracer/m_bilmas_i.f90 .
./m_donnees_calage_t.f90: ModulesCastor/m_donnees_calage_t.f90
	cp ModulesCastor/m_donnees_calage_t.f90 .
./rezo_interface.f90: API/f90/rezo_interface.f90
	cp API/f90/rezo_interface.f90 .
./m_permat_i_d.f90: ModulesDeriv/m_permat_i_d.f90
	cp ModulesDeriv/m_permat_i_d.f90 .
./calcul.f90: Rezo/calcul.f90
	cp Rezo/calcul.f90 .
./dger.f: Lapack/dger.f
	cp Lapack/dger.f .
./m_prelido_i.f90: Modules/m_prelido_i.f90
	cp Modules/m_prelido_i.f90 .
./post_imp.f90: Post/post_imp.f90
	cp Post/post_imp.f90 .
./lec_liaison.f90: Casier/lec_liaison.f90
	cp Casier/lec_liaison.f90 .
./cqinj.f90: Rezo/cqinj.f90
	cp Rezo/cqinj.f90 .
./cfl2d.f90: Mascaret/cfl2d.f90
	cp Mascaret/cfl2d.f90 .
./sing10.f90: Rezo/sing10.f90
	cp Rezo/sing10.f90 .
./m_post_imp_tracer_i.f90: ModulesTracer/m_post_imp_tracer_i.f90
	cp ModulesTracer/m_post_imp_tracer_i.f90 .
./dpdz.f90: Rezo/dpdz.f90
	cp Rezo/dpdz.f90 .
./m_node_tracer_t.f90: ModulesTracer/m_node_tracer_t.f90
	cp ModulesTracer/m_node_tracer_t.f90 .
./cfl2d1.f90: Mascaret/cfl2d1.f90
	cp Mascaret/cfl2d1.f90 .
./m_traiter_erreur_casier_i.f90: ModulesCasier/m_traiter_erreur_casier_i.f90
	cp ModulesCasier/m_traiter_erreur_casier_i.f90 .
./m_liaisonrc_t.f90: ModulesCasier/m_liaisonrc_t.f90
	cp ModulesCasier/m_liaisonrc_t.f90 .
./m_presd_i.f90: ModulesMascaret/m_presd_i.f90
	cp ModulesMascaret/m_presd_i.f90 .
./m_debita_i.f90: ModulesMascaret/m_debita_i.f90
	cp ModulesMascaret/m_debita_i.f90 .
./m_caldeb_i.f90: Modules/m_caldeb_i.f90
	cp Modules/m_caldeb_i.f90 .
./qnode.f90: Sarap/qnode.f90
	cp Sarap/qnode.f90 .
./m_pladeb_i.f90: Modules/m_pladeb_i.f90
	cp Modules/m_pladeb_i.f90 .
./promvt.f90: Mascaret/promvt.f90
	cp Mascaret/promvt.f90 .
./m_lec_apport_i.f90: Modules/m_lec_apport_i.f90
	cp Modules/m_lec_apport_i.f90 .
./bilvol.f90: Casier/bilvol.f90
	cp Casier/bilvol.f90 .
./fconj.f90: Mascaret/fconj.f90
	cp Mascaret/fconj.f90 .
./lec_maillage.f90: Pretrait/lec_maillage.f90
	cp Pretrait/lec_maillage.f90 .
./pres.f90: Mascaret/pres.f90
	cp Mascaret/pres.f90 .
./m_loilim_i.f90: Modules/m_loilim_i.f90
	cp Modules/m_loilim_i.f90 .
./m_profil_t.f90: Modules/m_profil_t.f90
	cp Modules/m_profil_t.f90 .
./m_sarap_i_b.f90: ModulesDeriv/m_sarap_i_b.f90
	cp ModulesDeriv/m_sarap_i_b.f90 .
./muscl_hancock.f90: Transdiffusion/muscl_hancock.f90
	cp Transdiffusion/muscl_hancock.f90 .
./m_qnode_i.f90: Modules/m_qnode_i.f90
	cp Modules/m_qnode_i.f90 .
./lec_dever.f90: Pretrait/lec_dever.f90
	cp Pretrait/lec_dever.f90 .
./m_message_casier_c.f90: ModulesCasier/m_message_casier_c.f90
	cp ModulesCasier/m_message_casier_c.f90 .
./survol.f90: Casier/survol.f90
	cp Casier/survol.f90 .
./set_taille_var_mascaret.f90: API/f90/set_taille_var_mascaret.f90
	cp API/f90/set_taille_var_mascaret.f90 .
./m_bissnd_tracer_i.f90: ModulesTracer/m_bissnd_tracer_i.f90
	cp ModulesTracer/m_bissnd_tracer_i.f90 .
./m_qcl_tracer_i.f90: ModulesTracer/m_qcl_tracer_i.f90
	cp ModulesTracer/m_qcl_tracer_i.f90 .
./dgemv.f: Lapack/dgemv.f
	cp Lapack/dgemv.f .
./m_cele_i.f90: ModulesMascaret/m_cele_i.f90
	cp ModulesMascaret/m_cele_i.f90 .
./m_persar_i_d.f90: ModulesDeriv/m_persar_i_d.f90
	cp ModulesDeriv/m_persar_i_d.f90 .
./m_qrepar_i.f90: Modules/m_qrepar_i.f90
	cp Modules/m_qrepar_i.f90 .
./flusrc.f90: Mascaret/flusrc.f90
	cp Mascaret/flusrc.f90 .
./m_lec_conc_ini_tracer_i.f90: ModulesTracer/m_lec_conc_ini_tracer_i.f90
	cp ModulesTracer/m_lec_conc_ini_tracer_i.f90 .
./persar.f90: Sarap/persar.f90
	cp Sarap/persar.f90 .
./m_etat_liaison_t.f90: ModulesAPI/m_etat_liaison_t.f90
	cp ModulesAPI/m_etat_liaison_t.f90 .
./calc_pc_conflu.f90: Sarap/calc_pc_conflu.f90
	cp Sarap/calc_pc_conflu.f90 .
./m_lec_geom_v3p0_i.f90: Modules/m_lec_geom_v3p0_i.f90
	cp Modules/m_lec_geom_v3p0_i.f90 .
./mascaret.f90: Mascaret/mascaret.f90
	cp Mascaret/mascaret.f90 .
./aintdc.f90: Mascaret/aintdc.f90
	cp Mascaret/aintdc.f90 .
./m_zbrent_i.f90: ModulesMascaret/m_zbrent_i.f90
	cp ModulesMascaret/m_zbrent_i.f90 .
./m_cond_lim_tracer_t.f90: ModulesTracer/m_cond_lim_tracer_t.f90
	cp ModulesTracer/m_cond_lim_tracer_t.f90 .
./dlaswp.f: Lapack/dlaswp.f
	cp Lapack/dlaswp.f .
./m_rieman_i.f90: ModulesMascaret/m_rieman_i.f90
	cp ModulesMascaret/m_rieman_i.f90 .
./opt2fort.f90: Pretrait/opt2fort.f90
	cp Pretrait/opt2fort.f90 .
./m_share_var.f90: Modules/m_share_var.f90
	cp Modules/m_share_var.f90 .
./m_rhsb1_s.f90: Modules/m_rhsb1_s.f90
	cp Modules/m_rhsb1_s.f90 .
./apimascaret.h: API/cpp/apimascaret.h
	cp API/cpp/apimascaret.h .
./singularite_rezo.f90: Rezo/singularite_rezo.f90
	cp Rezo/singularite_rezo.f90 .
./lec_geom_casier2.f90: Casier/lec_geom_casier2.f90
	cp Casier/lec_geom_casier2.f90 .
./m_lec_reseau_i.f90: Modules/m_lec_reseau_i.f90
	cp Modules/m_lec_reseau_i.f90 .
./init_ligne_mascaret.f90: API/f90/init_ligne_mascaret.f90
	cp API/f90/init_ligne_mascaret.f90 .
./lec_fic_conc_ini_tracer.f90: Pretrait/lec_fic_conc_ini_tracer.f90
	cp Pretrait/lec_fic_conc_ini_tracer.f90 .
./m_lec_casier_i.f90: ModulesCasier/m_lec_casier_i.f90
	cp ModulesCasier/m_lec_casier_i.f90 .
./dswap.f: Lapack/dswap.f
	cp Lapack/dswap.f .
./qcl_tracer.f90: Qcl/qcl_tracer.f90
	cp Qcl/qcl_tracer.f90 .
./m_lec_loi_tracer_i.f90: ModulesTracer/m_lec_loi_tracer_i.f90
	cp ModulesTracer/m_lec_loi_tracer_i.f90 .
./m_mascaret_t.f90: ModulesAPI/m_mascaret_t.f90
	cp ModulesAPI/m_mascaret_t.f90 .
./m_limite_i.f90: ModulesMascaret/m_limite_i.f90
	cp ModulesMascaret/m_limite_i.f90 .
./precal.f90: Mascaret/precal.f90
	cp Mascaret/precal.f90 .
./print_stat_masc.f90: Super/print_stat_masc.f90
	cp Super/print_stat_masc.f90 .
./m_pretrait_tracer_i.f90: ModulesTracer/m_pretrait_tracer_i.f90
	cp ModulesTracer/m_pretrait_tracer_i.f90 .
./set_etat_mascaret.f90: API/f90/set_etat_mascaret.f90
	cp API/f90/set_etat_mascaret.f90 .
./planim.f90: Planim/planim.f90
	cp Planim/planim.f90 .
./lsame.f: Lapack/lsame.f
	cp Lapack/lsame.f .
./m_invmat_i.f90: ModulesMascaret/m_invmat_i.f90
	cp ModulesMascaret/m_invmat_i.f90 .
./lec_ligne.f90: Pretrait/lec_ligne.f90
	cp Pretrait/lec_ligne.f90 .
./m_intersect_i.f90: Modules/m_intersect_i.f90
	cp Modules/m_intersect_i.f90 .
./lec_stockage.f90: Pretrait/lec_stockage.f90
	cp Pretrait/lec_stockage.f90 .
./m_critiq_i_d.f90: ModulesDeriv/m_critiq_i_d.f90
	cp ModulesDeriv/m_critiq_i_d.f90 .
./m_debitance_s_b.f90: ModulesDeriv/m_debitance_s_b.f90
	cp ModulesDeriv/m_debitance_s_b.f90 .
./m_flusrc_i.f90: ModulesMascaret/m_flusrc_i.f90
	cp ModulesMascaret/m_flusrc_i.f90 .
./m_fperd_i.f90: ModulesMascaret/m_fperd_i.f90
	cp ModulesMascaret/m_fperd_i.f90 .
./y12maf.f: Y12M/y12maf.f
	cp Y12M/y12maf.f .
./get_erreur_mascaret.f90: API/f90/get_erreur_mascaret.f90
	cp API/f90/get_erreur_mascaret.f90 .
./m_boussi_i.f90: ModulesMascaret/m_boussi_i.f90
	cp ModulesMascaret/m_boussi_i.f90 .
./akivm1.f90: Mascaret/akivm1.f90
	cp Mascaret/akivm1.f90 .
./m_mascaret_i.f90: ModulesMascaret/m_mascaret_i.f90
	cp ModulesMascaret/m_mascaret_i.f90 .
./dgbtf2.f: Lapack/dgbtf2.f
	cp Lapack/dgbtf2.f .
./m_dichod_i.f90: ModulesMascaret/m_dichod_i.f90
	cp ModulesMascaret/m_dichod_i.f90 .
./m_loi_tracer_t.f90: ModulesTracer/m_loi_tracer_t.f90
	cp ModulesTracer/m_loi_tracer_t.f90 .
./lec_loi_interface.f90: API/f90/lec_loi_interface.f90
	cp API/f90/lec_loi_interface.f90 .
./sauve_maillage.f90: Pretrait/sauve_maillage.f90
	cp Pretrait/sauve_maillage.f90 .
./repar.f90: Sarap/repar.f90
	cp Sarap/repar.f90 .
./m_alarg_i.f90: ModulesMascaret/m_alarg_i.f90
	cp ModulesMascaret/m_alarg_i.f90 .
./m_stock_tracer_i.f90: ModulesTracer/m_stock_tracer_i.f90
	cp ModulesTracer/m_stock_tracer_i.f90 .
./lec_fic_loi_tracer.f90: Pretrait/lec_fic_loi_tracer.f90
	cp Pretrait/lec_fic_loi_tracer.f90 .
./diffu_tracer.f90: Transdiffusion/diffu_tracer.f90
	cp Transdiffusion/diffu_tracer.f90 .
./rieman.f90: Mascaret/rieman.f90
	cp Mascaret/rieman.f90 .
./m_profil_plan_t.f90: Modules/m_profil_plan_t.f90
	cp Modules/m_profil_plan_t.f90 .
./m_liaison_chenal_i.f90: ModulesCasier/m_liaison_chenal_i.f90
	cp ModulesCasier/m_liaison_chenal_i.f90 .
./pretrait.f90: Pretrait/pretrait.f90
	cp Pretrait/pretrait.f90 .
./m_etat_mascaret_t.f90: ModulesAPI/m_etat_mascaret_t.f90
	cp ModulesAPI/m_etat_mascaret_t.f90 .
./m_message_c.f90: Modules/m_message_c.f90
	cp Modules/m_message_c.f90 .
./m_cout_i.f90: ModulesCastor/m_cout_i.f90
	cp ModulesCastor/m_cout_i.f90 .
./m_lec_geom_i.f90: Modules/m_lec_geom_i.f90
	cp Modules/m_lec_geom_i.f90 .
./m_fconj_i.f90: ModulesMascaret/m_fconj_i.f90
	cp ModulesMascaret/m_fconj_i.f90 .
./m_calc_pc_conflu_i_d.f90: ModulesDeriv/m_calc_pc_conflu_i_d.f90
	cp ModulesDeriv/m_calc_pc_conflu_i_d.f90 .
./m_planma_i.f90: Modules/m_planma_i.f90
	cp Modules/m_planma_i.f90 .
./m_matria_i.f90: ModulesMascaret/m_matria_i.f90
	cp ModulesMascaret/m_matria_i.f90 .
./m_conflu_i.f90: ModulesMascaret/m_conflu_i.f90
	cp ModulesMascaret/m_conflu_i.f90 .
./m_loi_t.f90: Modules/m_loi_t.f90
	cp Modules/m_loi_t.f90 .
./m_qrepar_i_d.f90: ModulesDeriv/m_qrepar_i_d.f90
	cp ModulesDeriv/m_qrepar_i_d.f90 .
./m_dpdz_i.f90: Modules/m_dpdz_i.f90
	cp Modules/m_dpdz_i.f90 .
./m_liaison_t.f90: ModulesCasier/m_liaison_t.f90
	cp ModulesCasier/m_liaison_t.f90 .
./m_resol_i.f90: ModulesMascaret/m_resol_i.f90
	cp ModulesMascaret/m_resol_i.f90 .
./dgbtrs.f: Lapack/dgbtrs.f
	cp Lapack/dgbtrs.f .
./lec_casier.f90: Casier/lec_casier.f90
	cp Casier/lec_casier.f90 .
./version_mascaret.f90: API/f90/version_mascaret.f90
	cp API/f90/version_mascaret.f90 .
./compt_chenaux.f90: Planim/compt_chenaux.f90
	cp Planim/compt_chenaux.f90 .
./m_calvar_i.f90: ModulesMascaret/m_calvar_i.f90
	cp ModulesMascaret/m_calvar_i.f90 .
./lec_pcsing.f90: Pretrait/lec_pcsing.f90
	cp Pretrait/lec_pcsing.f90 .
./m_debitance_s.f90: Modules/m_debitance_s.f90
	cp Modules/m_debitance_s.f90 .
./m_parametres_qualite_eau_t.f90: ModulesTracer/m_parametres_qualite_eau_t.f90
	cp ModulesTracer/m_parametres_qualite_eau_t.f90 .
./m_liaison_siphon_i.f90: ModulesCasier/m_liaison_siphon_i.f90
	cp ModulesCasier/m_liaison_siphon_i.f90 .
./diff_z_cf12_bwd_sarap.f90: Deriv/diff_z_cf12_bwd_sarap.f90
	cp Deriv/diff_z_cf12_bwd_sarap.f90 .
./calcfl.f90: Mascaret/calcfl.f90
	cp Mascaret/calcfl.f90 .
./lec_apport.f90: Pretrait/lec_apport.f90
	cp Pretrait/lec_apport.f90 .
./adbuffer.f: Deriv/adbuffer.f
	cp Deriv/adbuffer.f .
./m_lec_dever_i.f90: Modules/m_lec_dever_i.f90
	cp Modules/m_lec_dever_i.f90 .
./calc_connect.f90: Pretrait/calc_connect.f90
	cp Pretrait/calc_connect.f90 .
./m_aintdc_i.f90: ModulesMascaret/m_aintdc_i.f90
	cp ModulesMascaret/m_aintdc_i.f90 .
./dicho.f90: Planim/dicho.f90
	cp Planim/dicho.f90 .
./liaison_siphon.f90: Casier/liaison_siphon.f90
	cp Casier/liaison_siphon.f90 .
./calcul_mascaret.f90: API/f90/calcul_mascaret.f90
	cp API/f90/calcul_mascaret.f90 .
./m_lec_hydrau_i.f90: Modules/m_lec_hydrau_i.f90
	cp Modules/m_lec_hydrau_i.f90 .
./clpluie.f90: Casier/clpluie.f90
	cp Casier/clpluie.f90 .
./m_interpolation_s_b.f90: ModulesDeriv/m_interpolation_s_b.f90
	cp ModulesDeriv/m_interpolation_s_b.f90 .
./m_section_t.f90: Modules/m_section_t.f90
	cp Modules/m_section_t.f90 .
./m_lec_loi_i.f90: Modules/m_lec_loi_i.f90
	cp Modules/m_lec_loi_i.f90 .
./caleta.f90: Mascaret/caleta.f90
	cp Mascaret/caleta.f90 .
./stock_tracer.f90: Stock/stock_tracer.f90
	cp Stock/stock_tracer.f90 .
./lec_parphy.f90: ModelesQualEau/lec_parphy.f90
	cp ModelesQualEau/lec_parphy.f90 .
./m_repar_i.f90: Modules/m_repar_i.f90
	cp Modules/m_repar_i.f90 .
./set_valeur_mascaret.f90: API/f90/set_valeur_mascaret.f90
	cp API/f90/set_valeur_mascaret.f90 .
./m_decoder_geom_i.f90: Modules/m_decoder_geom_i.f90
	cp Modules/m_decoder_geom_i.f90 .
./lec_zone_seche.f90: Pretrait/lec_zone_seche.f90
	cp Pretrait/lec_zone_seche.f90 .
./valida.f90: Mascaret/valida.f90
	cp Mascaret/valida.f90 .
./m_solvro_i.f90: ModulesMascaret/m_solvro_i.f90
	cp ModulesMascaret/m_solvro_i.f90 .
./m_bilan_i.f90: ModulesMascaret/m_bilan_i.f90
	cp ModulesMascaret/m_bilan_i.f90 .
./m_psing_i_b.f90: ModulesDeriv/m_psing_i_b.f90
	cp ModulesDeriv/m_psing_i_b.f90 .
./liaison_orifice.f90: Casier/liaison_orifice.f90
	cp Casier/liaison_orifice.f90 .
./decoder_geom_casier.f90: Casier/decoder_geom_casier.f90
	cp Casier/decoder_geom_casier.f90 .
./tracer.f90: Transdiffusion/tracer.f90
	cp Transdiffusion/tracer.f90 .
./lec_loi_tracer.f90: Pretrait/lec_loi_tracer.f90
	cp Pretrait/lec_loi_tracer.f90 .
./m_qnode_i_d.f90: ModulesDeriv/m_qnode_i_d.f90
	cp ModulesDeriv/m_qnode_i_d.f90 .
./m_constantes_calage_t.f90: ModulesCastor/m_constantes_calage_t.f90
	cp ModulesCastor/m_constantes_calage_t.f90 .
./carac.f90: Mascaret/carac.f90
	cp Mascaret/carac.f90 .
./m_calint_i.f90: Modules/m_calint_i.f90
	cp Modules/m_calint_i.f90 .
./m_lec_fic_loi_tracer_i.f90: ModulesTracer/m_lec_fic_loi_tracer_i.f90
	cp ModulesTracer/m_lec_fic_loi_tracer_i.f90 .
./m_interpolation_s_d.f90: ModulesDeriv/m_interpolation_s_d.f90
	cp ModulesDeriv/m_interpolation_s_d.f90 .
./m_lec_geom_v2p0_i.f90: Modules/m_lec_geom_v2p0_i.f90
	cp Modules/m_lec_geom_v2p0_i.f90 .
./m_seuil_i.f90: ModulesMascaret/m_seuil_i.f90
	cp ModulesMascaret/m_seuil_i.f90 .
./y12mbf.f: Y12M/y12mbf.f
	cp Y12M/y12mbf.f .
./erodev.f90: Mascaret/erodev.f90
	cp Mascaret/erodev.f90 .
./m_calcs_biomass_i.f90: ModulesTracer/m_calcs_biomass_i.f90
	cp ModulesTracer/m_calcs_biomass_i.f90 .
./m_vvpropi_i.f90: ModulesMascaret/m_vvpropi_i.f90
	cp ModulesMascaret/m_vvpropi_i.f90 .
./m_post_i.f90: Modules/m_post_i.f90
	cp Modules/m_post_i.f90 .
./m_bissnd_i.f90: ModulesMascaret/m_bissnd_i.f90
	cp ModulesMascaret/m_bissnd_i.f90 .
./m_calage_n2qn1_i.f90: ModulesCastor/m_calage_n2qn1_i.f90
	cp ModulesCastor/m_calage_n2qn1_i.f90 .
./lec_abaque.f90: Pretrait/lec_abaque.f90
	cp Pretrait/lec_abaque.f90 .
./m_rezomat_t.f90: Modules/m_rezomat_t.f90
	cp Modules/m_rezomat_t.f90 .
./fludev.f90: Mascaret/fludev.f90
	cp Mascaret/fludev.f90 .
./m_calcfl_i.f90: ModulesMascaret/m_calcfl_i.f90
	cp ModulesMascaret/m_calcfl_i.f90 .
./m_index_variable_tracer_c.f90: ModulesTracer/m_index_variable_tracer_c.f90
	cp ModulesTracer/m_index_variable_tracer_c.f90 .
./lec_source.f90: Pretrait/lec_source.f90
	cp Pretrait/lec_source.f90 .
./y12mdf.f: Y12M/y12mdf.f
	cp Y12M/y12mdf.f .
./m_connect_t.f90: Modules/m_connect_t.f90
	cp Modules/m_connect_t.f90 .
./m_rhsbp_s.f90: Modules/m_rhsbp_s.f90
	cp Modules/m_rhsbp_s.f90 .
./m_calcs_eutro_i.f90: ModulesTracer/m_calcs_eutro_i.f90
	cp ModulesTracer/m_calcs_eutro_i.f90 .
./m_fichier_t.f90: Modules/m_fichier_t.f90
	cp Modules/m_fichier_t.f90 .
./calcs_micropol.f90: ModelesQualEau/calcs_micropol.f90
	cp ModelesQualEau/calcs_micropol.f90 .
./m_nulmat_i.f90: ModulesMascaret/m_nulmat_i.f90
	cp ModulesMascaret/m_nulmat_i.f90 .
./lec_barrage.f90: Pretrait/lec_barrage.f90
	cp Pretrait/lec_barrage.f90 .
./m_strick_i.f90: ModulesCastor/m_strick_i.f90
	cp ModulesCastor/m_strick_i.f90 .
./m_fh_i.f90: ModulesMascaret/m_fh_i.f90
	cp ModulesMascaret/m_fh_i.f90 .
./hyp1fa.f90: Transdiffusion/hyp1fa.f90
	cp Transdiffusion/hyp1fa.f90 .
./decbar.f90: Mascaret/decbar.f90
	cp Mascaret/decbar.f90 .
./m_lec_ligne_i.f90: Modules/m_lec_ligne_i.f90
	cp Modules/m_lec_ligne_i.f90 .
./m_qcl_i.f90: Modules/m_qcl_i.f90
	cp Modules/m_qcl_i.f90 .
./m_rempli_i.f90: Modules/m_rempli_i.f90
	cp Modules/m_rempli_i.f90 .
./fluroe.f90: Mascaret/fluroe.f90
	cp Mascaret/fluroe.f90 .
./m_xindic_s.f90: Modules/m_xindic_s.f90
	cp Modules/m_xindic_s.f90 .
./init_etat_mascaret.f90: API/f90/init_etat_mascaret.f90
	cp API/f90/init_etat_mascaret.f90 .
./liaison_chenal.f90: Casier/liaison_chenal.f90
	cp Casier/liaison_chenal.f90 .
./lec_geom.f90: Pretrait/lec_geom.f90
	cp Pretrait/lec_geom.f90 .
./m_calc_maillage_i.f90: Modules/m_calc_maillage_i.f90
	cp Modules/m_calc_maillage_i.f90 .
./fc1.f90: Mascaret/fc1.f90
	cp Mascaret/fc1.f90 .
./minmaj.f90: Mascaret/minmaj.f90
	cp Mascaret/minmaj.f90 .
./qcl.f90: Qcl/qcl.f90
	cp Qcl/qcl.f90 .
./m_lec_ligne_interface_i.f90: ModulesAPI/m_lec_ligne_interface_i.f90
	cp ModulesAPI/m_lec_ligne_interface_i.f90 .
./csur.f90: Mascaret/csur.f90
	cp Mascaret/csur.f90 .
./m_rhsbp_s_b.f90: ModulesDeriv/m_rhsbp_s_b.f90
	cp ModulesDeriv/m_rhsbp_s_b.f90 .
./m_frotte_i.f90: ModulesMascaret/m_frotte_i.f90
	cp ModulesMascaret/m_frotte_i.f90 .
./m_solex_i.f90: ModulesMascaret/m_solex_i.f90
	cp ModulesMascaret/m_solex_i.f90 .
./m_init_var_sortie_s.f90: Modules/m_init_var_sortie_s.f90
	cp Modules/m_init_var_sortie_s.f90 .
./lec_ligne_opt.f90: Pretrait/lec_ligne_opt.f90
	cp Pretrait/lec_ligne_opt.f90 .
./m_barrage_t.f90: Modules/m_barrage_t.f90
	cp Modules/m_barrage_t.f90 .
./m_psing_i.f90: Modules/m_psing_i.f90
	cp Modules/m_psing_i.f90 .
./m_sauve_maillage_i.f90: Modules/m_sauve_maillage_i.f90
	cp Modules/m_sauve_maillage_i.f90 .
./lec_reseau.f90: Pretrait/lec_reseau.f90
	cp Pretrait/lec_reseau.f90 .
./dtbsv.f: Lapack/dtbsv.f
	cp Lapack/dtbsv.f .
./indice.f90: Mascaret/indice.f90
	cp Mascaret/indice.f90 .
./m_interpolation_s.f90: Modules/m_interpolation_s.f90
	cp Modules/m_interpolation_s.f90 .
./calcul_mascaret_condition_limite.f90: API/f90/calcul_mascaret_condition_limite.f90
	cp API/f90/calcul_mascaret_condition_limite.f90 .
./m_fluroe_i.f90: ModulesMascaret/m_fluroe_i.f90
	cp ModulesMascaret/m_fluroe_i.f90 .
./m_rezo_interface_i.f90: ModulesAPI/m_rezo_interface_i.f90
	cp ModulesAPI/m_rezo_interface_i.f90 .
./matri.f90: Mascaret/matri.f90
	cp Mascaret/matri.f90 .
./m_erreur_t.f90: Modules/m_erreur_t.f90
	cp Modules/m_erreur_t.f90 .
./calcs_o2.f90: ModelesQualEau/calcs_o2.f90
	cp ModelesQualEau/calcs_o2.f90 .
./bissnd.f90: Mascaret/bissnd.f90
	cp Mascaret/bissnd.f90 .
./m_direct_i.f90: ModulesCastor/m_direct_i.f90
	cp ModulesCastor/m_direct_i.f90 .
./import_xml.f90: API/f90/import_xml.f90
	cp API/f90/import_xml.f90 .
./decoder_geom.f90: Pretrait/decoder_geom.f90
	cp Pretrait/decoder_geom.f90 .
./get_valeur_mascaret.f90: API/f90/get_valeur_mascaret.f90
	cp API/f90/get_valeur_mascaret.f90 .
./m_lec_pcsing_i.f90: Modules/m_lec_pcsing_i.f90
	cp Modules/m_lec_pcsing_i.f90 .
./lec_planim.f90: Pretrait/lec_planim.f90
	cp Pretrait/lec_planim.f90 .
./init_abaque.f90: API/f90/init_abaque.f90
	cp API/f90/init_abaque.f90 .
./m_contq_i.f90: Modules/m_contq_i.f90
	cp Modules/m_contq_i.f90 .
./m_cqinj_i_b.f90: ModulesDeriv/m_cqinj_i_b.f90
	cp ModulesDeriv/m_cqinj_i_b.f90 .
./m_sing3_i.f90: Modules/m_sing3_i.f90
	cp Modules/m_sing3_i.f90 .
./caldyg.f90: Planim/caldyg.f90
	cp Planim/caldyg.f90 .
./m_singularite_t.f90: Modules/m_singularite_t.f90
	cp Modules/m_singularite_t.f90 .
./carac3.f90: Mascaret/carac3.f90
	cp Mascaret/carac3.f90 .
./lec_ligne_interface.f90: API/f90/lec_ligne_interface.f90
	cp API/f90/lec_ligne_interface.f90 .
./m_abs_abs_s.f90: Modules/m_abs_abs_s.f90
	cp Modules/m_abs_abs_s.f90 .
./m_gaussj_i.f90: Modules/m_gaussj_i.f90
	cp Modules/m_gaussj_i.f90 .
./debita.f90: Mascaret/debita.f90
	cp Mascaret/debita.f90 .
./solex.f90: Mascaret/solex.f90
	cp Mascaret/solex.f90 .
./loilim.f90: Rezo/loilim.f90
	cp Rezo/loilim.f90 .
./m_lec_source_i.f90: ModulesTracer/m_lec_source_i.f90
	cp ModulesTracer/m_lec_source_i.f90 .
./m_dicho_i.f90: Modules/m_dicho_i.f90
	cp Modules/m_dicho_i.f90 .
./nulmat.f90: Mascaret/nulmat.f90
	cp Mascaret/nulmat.f90 .
./sarap.f90: Sarap/sarap.f90
	cp Sarap/sarap.f90 .
./m_lec_loi_interface_i.f90: ModulesAPI/m_lec_loi_interface_i.f90
	cp ModulesAPI/m_lec_loi_interface_i.f90 .
./dichodm.f90: Mascaret/dichodm.f90
	cp Mascaret/dichodm.f90 .
./chainage_rezo.f90: Rezo/chainage_rezo.f90
	cp Rezo/chainage_rezo.f90 .
./ieeeck.f: Lapack/ieeeck.f
	cp Lapack/ieeeck.f .
./m_frottd_i.f90: ModulesMascaret/m_frottd_i.f90
	cp ModulesMascaret/m_frottd_i.f90 .
./post_imp_casier.f90: Casier/post_imp_casier.f90
	cp Casier/post_imp_casier.f90 .
./err_alloc.f90: Super/err_alloc.f90
	cp Super/err_alloc.f90 .
./kliaison.f90: Casier/kliaison.f90
	cp Casier/kliaison.f90 .
./sing2.f90: Rezo/sing2.f90
	cp Rezo/sing2.f90 .
./m_carac_i.f90: ModulesMascaret/m_carac_i.f90
	cp ModulesMascaret/m_carac_i.f90 .
./m_etat_casier_t.f90: ModulesAPI/m_etat_casier_t.f90
	cp ModulesAPI/m_etat_casier_t.f90 .
./m_parphy_i.f90: ModulesTracer/m_parphy_i.f90
	cp ModulesTracer/m_parphy_i.f90 .
./m_lec_abaque_i.f90: Modules/m_lec_abaque_i.f90
	cp Modules/m_lec_abaque_i.f90 .
./m_init_var_sortie_tracer_s.f90: ModulesTracer/m_init_var_sortie_tracer_s.f90
	cp ModulesTracer/m_init_var_sortie_tracer_s.f90 .
./m_constantes_calcul_c.f90: Modules/m_constantes_calcul_c.f90
	cp Modules/m_constantes_calcul_c.f90 .
./m_promat_i.f90: ModulesMascaret/m_promat_i.f90
	cp ModulesMascaret/m_promat_i.f90 .
./papy.f90: Planim/papy.f90
	cp Planim/papy.f90 .
./lec_hydrau.f90: Pretrait/lec_hydrau.f90
	cp Pretrait/lec_hydrau.f90 .
./m_dichodm_i.f90: ModulesMascaret/m_dichodm_i.f90
	cp ModulesMascaret/m_dichodm_i.f90 .
./m_qrepar_i_b.f90: ModulesDeriv/m_qrepar_i_b.f90
	cp ModulesDeriv/m_qrepar_i_b.f90 .
./m_stock_i.f90: Modules/m_stock_i.f90
	cp Modules/m_stock_i.f90 .
./m_post_casier_i.f90: ModulesCasier/m_post_casier_i.f90
	cp ModulesCasier/m_post_casier_i.f90 .
./direct.f90: Castor/direct.f90
	cp Castor/direct.f90 .
./stock_calage.f90: Castor/stock_calage.f90
	cp Castor/stock_calage.f90 .
./pretrait_interface.f90: API/f90/pretrait_interface.f90
	cp API/f90/pretrait_interface.f90 .
./idamax.f: Lapack/idamax.f
	cp Lapack/idamax.f .
./m_modele_mascaret_t.f90: ModulesAPI/m_modele_mascaret_t.f90
	cp ModulesAPI/m_modele_mascaret_t.f90 .
./calcsa.f90: Transdiffusion/calcsa.f90
	cp Transdiffusion/calcsa.f90 .
./promat.f90: Mascaret/promat.f90
	cp Mascaret/promat.f90 .
./lec_meteo.f90: ModelesQualEau/lec_meteo.f90
	cp ModelesQualEau/lec_meteo.f90 .
./m_csur_i.f90: ModulesMascaret/m_csur_i.f90
	cp ModulesMascaret/m_csur_i.f90 .
./diff_z_cf12_fwd_sarap.f90: Deriv/diff_z_cf12_fwd_sarap.f90
	cp Deriv/diff_z_cf12_fwd_sarap.f90 .
./alarg.f90: Mascaret/alarg.f90
	cp Mascaret/alarg.f90 .
./m_csurm1_i.f90: ModulesMascaret/m_csurm1_i.f90
	cp ModulesMascaret/m_csurm1_i.f90 .
./m_lec_stockage_i.f90: Modules/m_lec_stockage_i.f90
	cp Modules/m_lec_stockage_i.f90 .
./m_lire_chaine_s.f90: Modules/m_lire_chaine_s.f90
	cp Modules/m_lire_chaine_s.f90 .
./csurmd.f90: Mascaret/csurmd.f90
	cp Mascaret/csurmd.f90 .
./apimascaret.c: API/cpp/apimascaret.c
	cp API/cpp/apimascaret.c .
./export_xml.f90: API/f90/export_xml.f90
	cp API/f90/export_xml.f90 .
./m_mascaret_interface_i.f90: ModulesAPI/m_mascaret_interface_i.f90
	cp ModulesAPI/m_mascaret_interface_i.f90 .
SRC = Mascaret/caleta.f90 Pretrait/lec_geom.f90 Planim/planim.f90 ModulesMascaret/m_dichodm_i.f90 ModulesCastor/m_calage_n2qn1_i.f90 Mascaret/vvprop.f90 Casier/lec_casier.f90 Pretrait/lec_confluent.f90 Lapack/iparmq.f ModulesCasier/m_liaison_orifice_i.f90 Mascaret/dichodm.f90 ModulesMascaret/m_caleta_i.f90 ModulesDeriv/m_qnode_i_b.f90 ModulesDeriv/m_rhsbp_s_d.f90 Mascaret/csur.f90 API/f90/set_valeur_mascaret.f90 ModulesTracer/m_calcs_rien_i.f90 Pretrait/lec_geom_v2p0.f90 Modules/m_apport_t.f90 ModulesDeriv/m_psing_i_d.f90 ModulesMascaret/m_qsing_i.f90 ModulesMascaret/m_nulmat_i.f90 Pretrait/lec_sorties.f90 Modules/m_extremite_t.f90 Mascaret/vvpropi.f90 Modules/m_lec_stockage_i.f90 ModulesCastor/m_donnees_crues_calage_t.f90 Modules/m_date_s.f90 Modules/m_abs_abs_s.f90 Modules/m_sing2_i.f90 ModulesCasier/m_liaisonrc_t.f90 ModulesMascaret/m_decbar_i.f90 API/f90/free_all_save_etat_mascaret.f90 Modules/m_prelido_i.f90 Modules/m_lec_frottement_i.f90 API/cpp/apimascaret.c Modules/m_calc_pc_conflu_i.f90 ModulesMascaret/m_bissn1_i.f90 Lapack/idamax.f ModulesCasier/m_lec_geom_casier1_i.f90 Pretrait/lec_planim.f90 Rezo/sing10.f90 ModulesMascaret/m_alarg_i.f90 ModulesMascaret/m_flusrc_i.f90 ModelesQualEau/calcs_micropol.f90 API/f90/version_mascaret.f90 Modules/m_caldyg_i.f90 Modules/m_debitance_s.f90 ModulesCastor/m_strick_i.f90 Mascaret/boussi.f90 ModulesAPI/m_etat_mascaret_t.f90 ModulesCasier/m_lec_liaison_i.f90 ModulesCasier/m_casier_t.f90 Pretrait/lec_reseau.f90 Modules/m_qnode_i.f90 ModulesMascaret/m_pres_i.f90 Modules/m_algop_i.f90 ModulesTracer/m_node_tracer_t.f90 Mascaret/fludev.f90 Casier/decoder_geom_casier.f90 Pretrait/decoder_geom.f90 ModulesMascaret/m_calcon_i.f90 Sarap/sarap.f90 Modules/m_lec_hydrau_i.f90 Modules/m_compt_chenaux_i.f90 ModulesMascaret/m_borne_i.f90 Transdiffusion/calcsa.f90 Modules/m_lec_sing_i.f90 Modules/m_stock_rep_i.f90 ModulesTracer/m_constantes_tracer_t.f90 Modules/m_chainage_rezo_i.f90 Pretrait/mailler.f90 Sarap/qrepar.f90 ModulesDeriv/m_calc_pc_conflu_i_b.f90 ModulesMascaret/m_csur_i.f90 ModulesMascaret/m_carac_i.f90 Modules/m_lec_pcsing_i.f90 Modules/m_ccl_i.f90 ModulesMascaret/m_fc1_i.f90 ModulesMascaret/m_promvt_i.f90 ModulesMascaret/m_frotte_i.f90 Modules/m_rhsbp_s.f90 ModulesMascaret/m_vvprop_i.f90 Mascaret/flusrc.f90 Mascaret/bissn1.f90 Rezo/cqinj.f90 API/f90/get_condition_limite.f90 Mascaret/erodev.f90 Modules/m_calint_i.f90 Mascaret/fc1.f90 ModulesTracer/m_diffu_tracer_i.f90 Deriv/diff_z_cf12_fwd_sarap.f90 Pretrait/lec_sing.f90 Mascaret/dydxsc.f90 Deriv/diff_z_cf12_bwd_sarap.f90 Castor/stock_calage.f90 Casier/liaison_chenal.f90 ModulesTracer/m_hyp1fa_i.f90 Mascaret/qsing.f90 ModulesMascaret/m_erodev_i.f90 Intersect/intersect.f90 Modules/m_rezo_i.f90 ModulesCastor/m_simul_i.f90 ModulesTracer/m_qcl_tracer_i.f90 Modules/m_parametre_c.f90 ModulesCasier/m_constantes_casier_c.f90 Planim/pladeb.f90 API/f90/set_etat_mascaret.f90 Mascaret/aintdc.f90 API/f90/init_etat_mascaret.f90 Pretrait/algop.f90 Modules/m_erreur_t.f90 Pretrait/calc_connect.f90 ModulesAPI/m_lec_loi_interface_i.f90 Sarap/psing.f90 Modules/m_calaig_i.f90 Mascaret/promat.f90 Modules/m_conden_i.f90 ModulesMascaret/m_aintdc_i.f90 ModelesQualEau/calcs_eutro.f90 Mascaret/bissnd.f90 ModulesMascaret/m_akiv_i.f90 ModulesTracer/m_calcsa_i.f90 ModulesMascaret/m_promat_i.f90 Modules/m_lec_loi_i.f90 Modules/m_lec_ligne_opt_i.f90 ModulesMascaret/m_csurmd_i.f90 ModulesCasier/m_decoder_geom_casier_i.f90 Mascaret/seuil.f90 Planim/caldeb.f90 Mascaret/limite.f90 Modules/m_rezomat_t.f90 API/f90/delete_mascaret.f90 Modules/m_num_bief_s.f90 ModulesDeriv/m_diff_z_cf12_bwd_sarap_var.f90 Castor/calage_n2qn1.f90 ModulesCasier/m_liaison_t.f90 Modules/m_persar_i.f90 ModulesDeriv/m_interpolation_s_d.f90 Modules/m_interpolation_s.f90 Rezo/ksing.f90 ModulesTracer/m_index_variable_tracer_c.f90 ModulesDeriv/m_permat_i_b.f90 ModulesTracer/m_cond_lim_tracer_t.f90 ModulesDeriv/m_qrepar_i_b.f90 Mascaret/cfl2d.f90 Lapack/ieeeck.f ModulesTracer/m_parametres_qualite_eau_t.f90 Rezo/chainage_rezo.f90 Modules/m_init_var_sortie_s.f90 Casier/liaison_seuil.f90 API/f90/rezo_interface.f90 Mascaret/frotte.f90 ModulesDeriv/m_critiq_i_b.f90 ModulesMascaret/m_precal_i.f90 ModulesDeriv/m_sarap_i_d.f90 API/f90/lec_ligne_interface.f90 Lapack/dgbtrs.f Castor/diff_jcout.f90 Transdiffusion/tracer.f90 Sarap/qnode.f90 Modules/m_qrepar_i.f90 Mascaret/dichod.f90 ModulesMascaret/m_debita_i.f90 Lapack/dtbsv.f ModulesDeriv/m_critiq_i_d.f90 API/f90/export_xml.f90 ModulesTracer/m_pretrait_tracer_i.f90 Mascaret/inigeo.f90 ModulesTracer/m_init_var_sortie_tracer_s.f90 Mascaret/solvro.f90 Pretrait/lec_dever.f90 ModulesTracer/m_calcs_o2_i.f90 ModulesMascaret/m_resol_i.f90 ModulesCasier/m_post_imp_casier_i.f90 Modules/m_lec_dever_i.f90 Mascaret/frottd.f90 ModulesTracer/m_lec_fic_conc_ini_tracer_i.f90 ModulesAPI/m_apimascaret_static.f90 Mascaret/solex.f90 Lapack/xerbla.f ModulesMascaret/m_cfl2d_i.f90 Mascaret/conflu.f90 Modules/m_lec_geom_v2p0_i.f90 Mascaret/matria.f90 ModulesMascaret/m_solvro_i.f90 Modules/m_papy_i.f90 Modules/m_rhsb1_s.f90 API/f90/pretrait_interface.f90 ModulesDeriv/m_diff_z_cf12_fwd_sarap_var.f90 ModulesDeriv/m_rhsbp_s_b.f90 ModulesMascaret/m_presd_i.f90 Mascaret/fperd.f90 Pretrait/pretrait_tracer.f90 Modules/m_contq_i.f90 Rezo/sing2.f90 ModulesCasier/m_post_casier_i.f90 ModulesMascaret/m_invmat_i.f90 API/f90/get_type_var_mascaret.f90 Modules/m_permat_i.f90 ModulesTracer/m_calcs_eutro_i.f90 Modules/m_sing3_i.f90 Sarap/calc_pc_conflu.f90 ModulesDeriv/m_cqinj_i_b.f90 Casier/post_imp_casier.f90 Mascaret/dgtsl.f90 Planim/dicho.f90 Modules/m_lec_planim_i.f90 API/f90/get_desc_var_mascaret.f90 Post/post_imp.f90 Mascaret/fh.f90 Casier/planim_casier.f90 ModulesTracer/m_tracer_i.f90 Lapack/dgbtrf.f Mascaret/nulmat.f90 ModulesMascaret/m_fconj_i.f90 ModulesMascaret/m_fh_i.f90 Modules/m_maille_t.f90 Mascaret/mascaret.f90 Sarap/permat.f90 Lapack/dgemm.f Modules/m_connect_t.f90 Lapack/dgbsv.f Mascaret/bilan.f90 Mascaret/cfl2d1.f90 ModelesQualEau/calcs_biomass.f90 API/f90/save_etat_mascaret.f90 ModulesMascaret/m_dichom_i.f90 Modules/m_froude_s.f90 ./homere_mascaret.f90 API/f90/free_save_etat_mascaret.f90 Modules/m_section_plan_t.f90 Modules/m_lec_zone_seche_i.f90 Transdiffusion/resequ.f90 ModulesMascaret/m_valida_i.f90 Pretrait/lec_barrage.f90 Modules/m_loilim_i.f90 Pretrait/lec_loi.f90 ModulesCasier/m_bilvol_i.f90 Pretrait/lec_fic_conc_ini_tracer.f90 ModulesMascaret/m_seuil_i.f90 Modules/m_singularite_t.f90 ModulesAPI/m_rezo_interface_i.f90 ModulesTracer/m_loi_tracer_t.f90 Castor/cout.f90 Modules/m_pretrait_i.f90 Planim/planma.f90 Sarap/critiq.f90 ModulesCasier/m_liaison_seuil_i.f90 ModulesCastor/m_stock_calage_i.f90 ModulesCastor/m_diff_jcout_i.f90 Modules/m_lire_chaine_s.f90 ModulesDeriv/m_qnode_i_d.f90 Rezo/ccl.f90 ModulesDeriv/m_persar_i_d.f90 Modules/m_sauve_t.f90 ModulesMascaret/m_indice_i.f90 Lapack/dlaswp.f Modules/m_index_variable_c.f90 ModulesMascaret/m_source_i.f90 ModulesTracer/m_constantes_calcul_tracer_c.f90 ModulesMascaret/m_fperd_i.f90 Pretrait/lec_conc_ini_tracer.f90 Modules/m_lec_apport_i.f90 Stock/stock_tracer.f90 ModulesCastor/m_donnees_calage_t.f90 Modules/m_deversoir_t.f90 Rezo/singularite_rezo.f90 Pretrait/lec_loi_tracer.f90 Lapack/lsame.f ModulesTracer/m_calcs_thermic_i.f90 ModulesCasier/m_clpluie_i.f90 ModulesCasier/m_liaison_siphon_i.f90 Castor/strick.f90 Y12M/y12mcf.f ModulesCasier/m_apport_pluie_t.f90 API/f90/set_taille_var_mascaret.f90 ModulesCasier/m_survol_i.f90 API/f90/create_mascaret.f90 API/f90/calcul_mascaret.f90 N2QN1/n2qn1.f ModulesTracer/m_bissnd_tracer_i.f90 API/f90/get_taille_var_mascaret.f90 Mascaret/akivm1.f90 ModulesTracer/m_meteo_t.f90 ModulesMascaret/m_limite_i.f90 API/f90/mascaret_interface.f90 Modules/m_barrage_t.f90 Planim/calaig.f90 Y12M/y12maf.f Super/print_stat_masc.f90 Deriv/adstack.c API/f90/get_valeur_mascaret.f90 ModulesMascaret/m_frottd_i.f90 Pretrait/lec_ligne.f90 ModulesMascaret/m_inigeo_i.f90 Modules/m_message_c.f90 ModulesDeriv/m_repar_i_d.f90 ModulesDeriv/m_interpolation_s_b.f90 Modules/m_mailler_i.f90 ModulesTracer/m_message_tracer_c.f90 Pretrait/lec_source.f90 ModulesTracer/m_lec_fic_loi_tracer_i.f90 Casier/lec_geom_casier2.f90 ModulesDeriv/m_persar_i_b.f90 ModulesCasier/m_message_casier_c.f90 Mascaret/minmaj.f90 ModulesCasier/m_lec_geom_casier2_i.f90 ModulesMascaret/m_solex_i.f90 Casier/lec_geom_casier1.f90 Pretrait/lec_ligne_lido.f90 API/f90/import_modele_mascaret.f90 Modules/m_loi_t.f90 ModelesQualEau/calcs_o2.f90 Modules/m_lec_sorties_i.f90 ModulesCasier/m_kliaison_i.f90 ModulesAPI/m_etat_liaison_t.f90 Pretrait/opt2fort.f90 Modules/m_lec_ligne_i.f90 ModulesMascaret/m_fluroe_i.f90 Modules/m_calc_maillage_i.f90 Qcl/qcl.f90 Modules/m_gaussj_i.f90 Qcl/qcl_tracer.f90 ModelesQualEau/calcs_mes.f90 Sarap/repar.f90 ModelesQualEau/lec_meteo.f90 Casier/clpluie.f90 Modules/m_caldeb_i.f90 Modules/m_coeffs_i.f90 ModulesTracer/m_stock_tracer_i.f90 Stock/stock.F90 ModulesTracer/m_source_tracer_t.f90 Lapack/ilaenv.f Modules/m_section_t.f90 Mascaret/cele.f90 Mascaret/resol.f90 Castor/direct.f90 ModulesAPI/m_modele_mascaret_t.f90 Mascaret/indice.f90 ModulesTracer/m_calk_i.f90 Modules/m_share_var.f90 ModulesCasier/m_liaisoncc_t.f90 Mascaret/borne.f90 Casier/lec_liaison.f90 Lapack/dscal.f Transdiffusion/calk.f90 Mascaret/clipp.f90 ModulesTracer/m_bilmas_i.f90 Rezo/loilim.f90 API/f90/import_xml.f90 Pretrait/lec_ligne_opt.f90 ModulesCasier/m_planim_casier_i.f90 ModulesMascaret/m_mascaret_i.f90 ModulesTracer/m_calcs_biomass_i.f90 ModulesCasier/m_traiter_erreur_casier_i.f90 ModulesTracer/m_lec_source_i.f90 Pretrait/lec_pcsing.f90 ModulesMascaret/m_clipp_i.f90 Y12M/y12mdf.f Lapack/dgbtf2.f ModulesMascaret/m_cele_i.f90 Modules/m_post_imp_i.f90 ModulesCastor/m_constantes_calage_t.f90 ModulesTracer/m_lec_conc_ini_tracer_i.f90 Modules/m_traiter_erreur_i.f90 Planim/caldyg.f90 Modules/m_psing_i.f90 Casier/lec_apport_pluie.f90 Pretrait/pretrait.f90 ModulesMascaret/m_matri_i.f90 Modules/m_decoder_geom_i.f90 Pretrait/lec_hydrau.f90 ModulesTracer/m_parphy_i.f90 Mascaret/pres.f90 Pretrait/lec_apport.f90 Mascaret/carac3.f90 ModulesTracer/m_calcs_micropol_i.f90 Stock/stock_rep.f90 ModulesMascaret/m_bilan_i.f90 Modules/m_stock_i.f90 Modules/m_singularite_rezo_i.f90 Mascaret/zbrent.f90 Mascaret/calcon.f90 Mascaret/promvt.f90 Modules/m_lec_ligne_lido_i.f90 ModulesDeriv/m_debitance_s_b.f90 Modules/m_fichier_t.f90 Modules/m_sarap_i.f90 Mascaret/fluroe.f90 ModulesAPI/m_mascaret_interface_i.f90 Casier/survol.f90 Pretrait/lec_geom_v3p0.f90 Lapack/dtrsm.f ModulesMascaret/m_csurm1_i.f90 Modules/m_cqinj_i.f90 Transdiffusion/muscl_hancock.f90 ModulesMascaret/m_matria_i.f90 Post/post_imp_tracer.f90 Pretrait/lec_fic_loi_tracer.f90 Mascaret/carac.f90 Modules/m_dpdz_i.f90 ModulesTracer/m_post_imp_tracer_i.f90 ModulesMascaret/m_minmaj_i.f90 Mascaret/csurmd.f90 Post/post.f90 ModulesDeriv/m_psing_i_b.f90 Pretrait/lec_zone_seche.f90 Rezo/sing3.f90 ModulesDeriv/m_debitance_s_d.f90 ModulesMascaret/m_dichod_i.f90 Casier/bilvol.f90 ModulesCastor/m_direct_i.f90 ModulesTracer/m_muscl_hancock_i.f90 ModulesDeriv/m_repar_i_b.f90 Lapack/dger.f ModulesMascaret/m_boussi_i.f90 Modules/m_profil_t.f90 Modules/m_lec_abaque_i.f90 Rezo/calcul.f90 Modules/m_qcl_i.f90 ModulesCasier/m_lec_casier_i.f90 Transdiffusion/diffu_tracer.f90 Modules/m_repar_i.f90 Lapack/dgemv.f ModulesCasier/m_liaison_chenal_i.f90 ModulesCasier/m_lec_apport_pluie_i.f90 API/f90/lec_loi_interface.f90 Lapack/dswap.f Modules/m_calc_connect_i.f90 ModulesDeriv/m_sarap_i_b.f90 ModulesCastor/m_cout_i.f90 Modules/m_rempli_i.f90 Mascaret/valida.f90 Mascaret/fconj.f90 ModulesMascaret/m_bissnd_i.f90 Mascaret/matri.f90 Deriv/adbuffer.f API/f90/calcul_mascaret_condition_limite.f90 Modules/m_profil_plan_t.f90 Modules/m_ksing_i.f90 Mascaret/invmat.f90 Mascaret/calcfl.f90 Sarap/persar.f90 ModulesMascaret/m_dydxsc_i.f90 Modules/m_constantes_calcul_c.f90 ModulesAPI/m_apimascaret_i.f90 ModulesAPI/m_etat_casier_t.f90 Modules/m_lec_geom_i.f90 Modules/m_lec_confluent_i.f90 Modules/m_lec_geom_v3p0_i.f90 Modules/m_opt2fort_i.f90 Mascaret/precal.f90 Modules/m_lec_barrage_i.f90 Modules/m_confluent_t.f90 Super/err_alloc.f90 ModulesMascaret/m_carac3_i.f90 Pretrait/calc_maillage.f90 Transdiffusion/bissnd_tracer.f90 ModelesQualEau/lec_parphy.f90 ModulesMascaret/m_cfl2d1_i.f90 API/f90/get_erreur_mascaret.f90 ModulesMascaret/m_zbrent_i.f90 API/f90/duplicate_etat.f90 Modules/m_planma_i.f90 API/f90/init_ligne_mascaret.f90 ModelesQualEau/calcs_thermic.f90 Transdiffusion/godunov.f90 Mascaret/akiv.f90 Modules/m_planim_i.f90 ModulesMascaret/m_calvar_i.f90 Modules/m_sauve_maillage_i.f90 Lapack/dcopy.f Transdiffusion/bilmas.f90 ModelesQualEau/calcs_rien.f90 Modules/m_lec_reseau_i.f90 ModulesMascaret/m_rieman_i.f90 Modules/m_zone_seche_t.f90 Y12M/y12mbf.f Modules/m_post_i.f90 Mascaret/source.f90 Pretrait/lec_maillage.f90 Casier/liaison_orifice.f90 Modules/m_sing10_i.f90 Mascaret/presd.f90 ModulesAPI/m_pretrait_interface_i.f90 ModulesMascaret/m_conflu_i.f90 Casier/liaison_siphon.f90 Mascaret/rieman.f90 Pretrait/lec_frottement.f90 Castor/simul.f90 Planim/papy.f90 Casier/post_casier.f90 Modules/m_critiq_i.f90 Mascaret/calvar.f90 Rezo/dpdz.f90 API/f90/init_abaque.f90 Rezo/rezo.f90 ModulesTracer/m_resequ_i.f90 ModulesTracer/m_lec_loi_tracer_i.f90 ModulesMascaret/m_akivm1_i.f90 Mascaret/dichom.f90 ModulesDeriv/m_permat_i_d.f90 ModulesAPI/m_lec_ligne_interface_i.f90 Transdiffusion/hyp1fa.f90 Modules/m_lec_maillage_i.f90 ModulesMascaret/m_fludev_i.f90 Modules/m_intersect_i.f90 Mascaret/csurm1.f90 Pretrait/lec_stockage.f90 ModulesMascaret/m_vvpropi_i.f90 ModulesDeriv/m_calc_pc_conflu_i_d.f90 Planim/compt_chenaux.f90 ModulesTracer/m_godunov_i.f90 Pretrait/sauve_maillage.f90 ModulesDeriv/m_qrepar_i_d.f90 Mascaret/debita.f90 Pretrait/lec_abaque.f90 Mascaret/alarg.f90 ModulesMascaret/m_calcfl_i.f90 Modules/m_xindic_s.f90 Modules/m_pladeb_i.f90 Modules/m_precision.f90 ModulesAPI/m_mascaret_t.f90 Casier/kliaison.f90 Mascaret/decbar.f90 Modules/m_dicho_i.f90 Modules/m_calcul_i.f90 API/cpp/apimascaret.h
OBJ = caleta.o lec_geom.o planim.o m_dichodm_i.o m_calage_n2qn1_i.o vvprop.o lec_casier.o lec_confluent.o iparmq.o m_liaison_orifice_i.o dichodm.o m_caleta_i.o m_qnode_i_b.o m_rhsbp_s_d.o csur.o set_valeur_mascaret.o m_calcs_rien_i.o lec_geom_v2p0.o m_apport_t.o m_psing_i_d.o m_qsing_i.o m_nulmat_i.o lec_sorties.o m_extremite_t.o vvpropi.o m_lec_stockage_i.o m_donnees_crues_calage_t.o m_date_s.o m_abs_abs_s.o m_sing2_i.o m_liaisonrc_t.o m_decbar_i.o free_all_save_etat_mascaret.o m_prelido_i.o m_lec_frottement_i.o apimascaret.o m_calc_pc_conflu_i.o m_bissn1_i.o idamax.o m_lec_geom_casier1_i.o lec_planim.o sing10.o m_alarg_i.o m_flusrc_i.o calcs_micropol.o version_mascaret.o m_caldyg_i.o m_debitance_s.o m_strick_i.o boussi.o m_etat_mascaret_t.o m_lec_liaison_i.o m_casier_t.o lec_reseau.o m_qnode_i.o m_pres_i.o m_algop_i.o m_node_tracer_t.o fludev.o decoder_geom_casier.o decoder_geom.o m_calcon_i.o sarap.o m_lec_hydrau_i.o m_compt_chenaux_i.o m_borne_i.o calcsa.o m_lec_sing_i.o m_stock_rep_i.o m_constantes_tracer_t.o m_chainage_rezo_i.o mailler.o qrepar.o m_calc_pc_conflu_i_b.o m_csur_i.o m_carac_i.o m_lec_pcsing_i.o m_ccl_i.o m_fc1_i.o m_promvt_i.o m_frotte_i.o m_rhsbp_s.o m_vvprop_i.o flusrc.o bissn1.o cqinj.o get_condition_limite.o erodev.o m_calint_i.o fc1.o m_diffu_tracer_i.o diff_z_cf12_fwd_sarap.o lec_sing.o dydxsc.o diff_z_cf12_bwd_sarap.o stock_calage.o liaison_chenal.o m_hyp1fa_i.o qsing.o m_erodev_i.o intersect.o m_rezo_i.o m_simul_i.o m_qcl_tracer_i.o m_parametre_c.o m_constantes_casier_c.o pladeb.o set_etat_mascaret.o aintdc.o init_etat_mascaret.o algop.o m_erreur_t.o calc_connect.o m_lec_loi_interface_i.o psing.o m_calaig_i.o promat.o m_conden_i.o m_aintdc_i.o calcs_eutro.o bissnd.o m_akiv_i.o m_calcsa_i.o m_promat_i.o m_lec_loi_i.o m_lec_ligne_opt_i.o m_csurmd_i.o m_decoder_geom_casier_i.o seuil.o caldeb.o limite.o m_rezomat_t.o delete_mascaret.o m_num_bief_s.o m_diff_z_cf12_bwd_sarap_var.o calage_n2qn1.o m_liaison_t.o m_persar_i.o m_interpolation_s_d.o m_interpolation_s.o ksing.o m_index_variable_tracer_c.o m_permat_i_b.o m_cond_lim_tracer_t.o m_qrepar_i_b.o cfl2d.o ieeeck.o m_parametres_qualite_eau_t.o chainage_rezo.o m_init_var_sortie_s.o liaison_seuil.o rezo_interface.o frotte.o m_critiq_i_b.o m_precal_i.o m_sarap_i_d.o lec_ligne_interface.o dgbtrs.o diff_jcout.o tracer.o qnode.o m_qrepar_i.o dichod.o m_debita_i.o dtbsv.o m_critiq_i_d.o export_xml.o m_pretrait_tracer_i.o inigeo.o m_init_var_sortie_tracer_s.o solvro.o lec_dever.o m_calcs_o2_i.o m_resol_i.o m_post_imp_casier_i.o m_lec_dever_i.o frottd.o m_lec_fic_conc_ini_tracer_i.o m_apimascaret_static.o solex.o xerbla.o m_cfl2d_i.o conflu.o m_lec_geom_v2p0_i.o matria.o m_solvro_i.o m_papy_i.o m_rhsb1_s.o pretrait_interface.o m_diff_z_cf12_fwd_sarap_var.o m_rhsbp_s_b.o m_presd_i.o fperd.o pretrait_tracer.o m_contq_i.o sing2.o m_post_casier_i.o m_invmat_i.o get_type_var_mascaret.o m_permat_i.o m_calcs_eutro_i.o m_sing3_i.o calc_pc_conflu.o m_cqinj_i_b.o post_imp_casier.o dgtsl.o dicho.o m_lec_planim_i.o get_desc_var_mascaret.o post_imp.o fh.o planim_casier.o m_tracer_i.o dgbtrf.o nulmat.o m_fconj_i.o m_fh_i.o m_maille_t.o mascaret.o permat.o dgemm.o m_connect_t.o dgbsv.o bilan.o cfl2d1.o calcs_biomass.o save_etat_mascaret.o m_dichom_i.o m_froude_s.o homere_mascaret.o free_save_etat_mascaret.o m_section_plan_t.o m_lec_zone_seche_i.o resequ.o m_valida_i.o lec_barrage.o m_loilim_i.o lec_loi.o m_bilvol_i.o lec_fic_conc_ini_tracer.o m_seuil_i.o m_singularite_t.o m_rezo_interface_i.o m_loi_tracer_t.o cout.o m_pretrait_i.o planma.o critiq.o m_liaison_seuil_i.o m_stock_calage_i.o m_diff_jcout_i.o m_lire_chaine_s.o m_qnode_i_d.o ccl.o m_persar_i_d.o m_sauve_t.o m_indice_i.o dlaswp.o m_index_variable_c.o m_source_i.o m_constantes_calcul_tracer_c.o m_fperd_i.o lec_conc_ini_tracer.o m_lec_apport_i.o stock_tracer.o m_donnees_calage_t.o m_deversoir_t.o singularite_rezo.o lec_loi_tracer.o lsame.o m_calcs_thermic_i.o m_clpluie_i.o m_liaison_siphon_i.o strick.o y12mcf.o m_apport_pluie_t.o set_taille_var_mascaret.o m_survol_i.o create_mascaret.o calcul_mascaret.o n2qn1.o m_bissnd_tracer_i.o get_taille_var_mascaret.o akivm1.o m_meteo_t.o m_limite_i.o mascaret_interface.o m_barrage_t.o calaig.o y12maf.o print_stat_masc.o adstack.o get_valeur_mascaret.o m_frottd_i.o lec_ligne.o m_inigeo_i.o m_message_c.o m_repar_i_d.o m_interpolation_s_b.o m_mailler_i.o m_message_tracer_c.o lec_source.o m_lec_fic_loi_tracer_i.o lec_geom_casier2.o m_persar_i_b.o m_message_casier_c.o minmaj.o m_lec_geom_casier2_i.o m_solex_i.o lec_geom_casier1.o lec_ligne_lido.o import_modele_mascaret.o m_loi_t.o calcs_o2.o m_lec_sorties_i.o m_kliaison_i.o m_etat_liaison_t.o opt2fort.o m_lec_ligne_i.o m_fluroe_i.o m_calc_maillage_i.o qcl.o m_gaussj_i.o qcl_tracer.o calcs_mes.o repar.o lec_meteo.o clpluie.o m_caldeb_i.o m_coeffs_i.o m_stock_tracer_i.o stock.o m_source_tracer_t.o ilaenv.o m_section_t.o cele.o resol.o direct.o m_modele_mascaret_t.o indice.o m_calk_i.o m_share_var.o m_liaisoncc_t.o borne.o lec_liaison.o dscal.o calk.o clipp.o m_bilmas_i.o loilim.o import_xml.o lec_ligne_opt.o m_planim_casier_i.o m_mascaret_i.o m_calcs_biomass_i.o m_traiter_erreur_casier_i.o m_lec_source_i.o lec_pcsing.o m_clipp_i.o y12mdf.o dgbtf2.o m_cele_i.o m_post_imp_i.o m_constantes_calage_t.o m_lec_conc_ini_tracer_i.o m_traiter_erreur_i.o caldyg.o m_psing_i.o lec_apport_pluie.o pretrait.o m_matri_i.o m_decoder_geom_i.o lec_hydrau.o m_parphy_i.o pres.o lec_apport.o carac3.o m_calcs_micropol_i.o stock_rep.o m_bilan_i.o m_stock_i.o m_singularite_rezo_i.o zbrent.o calcon.o promvt.o m_lec_ligne_lido_i.o m_debitance_s_b.o m_fichier_t.o m_sarap_i.o fluroe.o m_mascaret_interface_i.o survol.o lec_geom_v3p0.o dtrsm.o m_csurm1_i.o m_cqinj_i.o muscl_hancock.o m_matria_i.o post_imp_tracer.o lec_fic_loi_tracer.o carac.o m_dpdz_i.o m_post_imp_tracer_i.o m_minmaj_i.o csurmd.o post.o m_psing_i_b.o lec_zone_seche.o sing3.o m_debitance_s_d.o m_dichod_i.o bilvol.o m_direct_i.o m_muscl_hancock_i.o m_repar_i_b.o dger.o m_boussi_i.o m_profil_t.o m_lec_abaque_i.o calcul.o m_qcl_i.o m_lec_casier_i.o diffu_tracer.o m_repar_i.o dgemv.o m_liaison_chenal_i.o m_lec_apport_pluie_i.o lec_loi_interface.o dswap.o m_calc_connect_i.o m_sarap_i_b.o m_cout_i.o m_rempli_i.o valida.o fconj.o m_bissnd_i.o matri.o adbuffer.o calcul_mascaret_condition_limite.o m_profil_plan_t.o m_ksing_i.o invmat.o calcfl.o persar.o m_dydxsc_i.o m_constantes_calcul_c.o m_apimascaret_i.o m_etat_casier_t.o m_lec_geom_i.o m_lec_confluent_i.o m_lec_geom_v3p0_i.o m_opt2fort_i.o precal.o m_lec_barrage_i.o m_confluent_t.o err_alloc.o m_carac3_i.o calc_maillage.o bissnd_tracer.o lec_parphy.o m_cfl2d1_i.o get_erreur_mascaret.o m_zbrent_i.o duplicate_etat.o m_planma_i.o init_ligne_mascaret.o calcs_thermic.o godunov.o akiv.o m_planim_i.o m_calvar_i.o m_sauve_maillage_i.o dcopy.o bilmas.o calcs_rien.o m_lec_reseau_i.o m_rieman_i.o m_zone_seche_t.o y12mbf.o m_post_i.o source.o lec_maillage.o liaison_orifice.o m_sing10_i.o presd.o m_pretrait_interface_i.o m_conflu_i.o liaison_siphon.o rieman.o lec_frottement.o simul.o papy.o post_casier.o m_critiq_i.o calvar.o dpdz.o init_abaque.o rezo.o m_resequ_i.o m_lec_loi_tracer_i.o m_akivm1_i.o dichom.o m_permat_i_d.o m_lec_ligne_interface_i.o hyp1fa.o m_lec_maillage_i.o m_fludev_i.o m_intersect_i.o csurm1.o lec_stockage.o m_vvpropi_i.o m_calc_pc_conflu_i_d.o compt_chenaux.o m_godunov_i.o sauve_maillage.o m_qrepar_i_d.o debita.o lec_abaque.o alarg.o m_calcfl_i.o m_xindic_s.o m_pladeb_i.o m_precision.o m_mascaret_t.o kliaison.o decbar.o m_dicho_i.o m_calcul_i.o
OFF = Modules/m_maille_t.f90 ModulesMascaret/m_caleta_i.f90 ModulesMascaret/m_clipp_i.f90 ModulesCasier/m_post_imp_casier_i.f90 ModulesAPI/m_pretrait_interface_i.f90 Modules/m_index_variable_c.f90 Intersect/intersect.f90 Post/post.f90 Rezo/ccl.f90 Modules/m_calc_connect_i.f90 Pretrait/lec_ligne_lido.f90 Modules/m_lec_frottement_i.f90 ModulesDeriv/m_psing_i_d.f90 ModulesCasier/m_constantes_casier_c.f90 Modules/m_sing10_i.f90 Mascaret/fperd.f90 Modules/m_compt_chenaux_i.f90 Pretrait/mailler.f90 ModulesCasier/m_decoder_geom_casier_i.f90 Lapack/dgbtrf.f ModulesAPI/m_apimascaret_static.f90 Lapack/xerbla.f ModulesCasier/m_lec_geom_casier1_i.f90 Mascaret/matria.f90 ModulesTracer/m_constantes_tracer_t.f90 Mascaret/source.f90 Mascaret/dichod.f90 ModulesDeriv/m_sarap_i_d.f90 Modules/m_parametre_c.f90 Modules/m_precision.f90 ModulesDeriv/m_rhsbp_s_d.f90 Rezo/sing3.f90 Modules/m_lec_ligne_lido_i.f90 API/f90/delete_mascaret.f90 ModulesMascaret/m_csurmd_i.f90 Mascaret/invmat.f90 Modules/m_post_imp_i.f90 ModulesMascaret/m_fludev_i.f90 Mascaret/seuil.f90 Modules/m_lec_ligne_opt_i.f90 Lapack/iparmq.f Modules/m_stock_rep_i.f90 ModulesDeriv/m_qnode_i_b.f90 ModulesCasier/m_survol_i.f90 Lapack/dgemm.f Modules/m_persar_i.f90 Pretrait/lec_frottement.f90 ModelesQualEau/calcs_rien.f90 Mascaret/resol.f90 Mascaret/akiv.f90 ModulesCasier/m_lec_geom_casier2_i.f90 Mascaret/vvpropi.f90 Modules/m_sarap_i.f90 Modules/m_opt2fort_i.f90 Planim/caldeb.f90 ModulesTracer/m_source_tracer_t.f90 Pretrait/lec_conc_ini_tracer.f90 ModulesMascaret/m_vvprop_i.f90 Mascaret/dgtsl.f90 Modules/m_lec_planim_i.f90 Modules/m_coeffs_i.f90 ModulesMascaret/m_borne_i.f90 Modules/m_algop_i.f90 ModulesCasier/m_liaison_orifice_i.f90 ModulesMascaret/m_qsing_i.f90 API/f90/duplicate_etat.f90 ModulesDeriv/m_critiq_i_b.f90 Lapack/dgbsv.f Modules/m_singularite_rezo_i.f90 ModulesMascaret/m_minmaj_i.f90 ModulesTracer/m_hyp1fa_i.f90 ModelesQualEau/calcs_biomass.f90 Mascaret/fh.f90 Casier/post_casier.f90 Transdiffusion/bilmas.f90 Modules/m_apport_t.f90 Modules/m_zone_seche_t.f90 ModulesCasier/m_clpluie_i.f90 ModulesCastor/m_stock_calage_i.f90 Modules/m_conden_i.f90 Mascaret/bissn1.f90 API/f90/import_modele_mascaret.f90 Castor/diff_jcout.f90 ModulesMascaret/m_erodev_i.f90 Modules/m_sing2_i.f90 ModulesMascaret/m_calcon_i.f90 ModulesMascaret/m_valida_i.f90 ModulesMascaret/m_cfl2d_i.f90 ModulesMascaret/m_dichom_i.f90 ModulesMascaret/m_cfl2d1_i.f90 Mascaret/zbrent.f90 ModulesCasier/m_bilvol_i.f90 Transdiffusion/calk.f90 API/f90/save_etat_mascaret.f90 ModulesMascaret/m_promvt_i.f90 Post/post_imp_tracer.f90 Mascaret/boussi.f90 ModulesDeriv/m_repar_i_b.f90 Sarap/critiq.f90 API/f90/free_save_etat_mascaret.f90 ModulesMascaret/m_akiv_i.f90 ModulesTracer/m_calcs_thermic_i.f90 ModulesCasier/m_liaisoncc_t.f90 Mascaret/dichom.f90 Modules/m_lec_confluent_i.f90 Stock/stock_rep.f90 Modules/m_calcul_i.f90 Castor/calage_n2qn1.f90 ModulesCasier/m_kliaison_i.f90 ModulesMascaret/m_fc1_i.f90 Modules/m_planim_i.f90 Modules/m_num_bief_s.f90 ModelesQualEau/calcs_thermic.f90 Pretrait/lec_confluent.f90 ModulesDeriv/m_persar_i_b.f90 ModulesCasier/m_lec_liaison_i.f90 ModulesMascaret/m_bissn1_i.f90 Mascaret/inigeo.f90 Modules/m_confluent_t.f90 Modules/m_traiter_erreur_i.f90 Modules/m_section_plan_t.f90 Rezo/ksing.f90 Modules/m_lec_sorties_i.f90 Modules/m_lec_maillage_i.f90 Castor/simul.f90 ModulesTracer/m_calk_i.f90 API/f90/free_all_save_etat_mascaret.f90 Mascaret/bilan.f90 ModulesMascaret/m_decbar_i.f90 ModelesQualEau/calcs_eutro.f90 Stock/stock.F90 Pretrait/pretrait_tracer.f90 Casier/planim_casier.f90 Sarap/psing.f90 ModulesMascaret/m_pres_i.f90 Mascaret/frottd.f90 ModulesCasier/m_lec_apport_pluie_i.f90 ModulesMascaret/m_dydxsc_i.f90 ModulesTracer/m_meteo_t.f90 Sarap/qrepar.f90 ModulesDeriv/m_diff_z_cf12_fwd_sarap_var.f90 Pretrait/lec_sing.f90 Modules/m_lec_barrage_i.f90 ModulesCasier/m_apport_pluie_t.f90 ModulesTracer/m_tracer_i.f90 ModulesMascaret/m_indice_i.f90 ModulesMascaret/m_akivm1_i.f90 ModelesQualEau/calcs_mes.f90 Mascaret/qsing.f90 Modules/m_mailler_i.f90 ModulesTracer/m_calcs_micropol_i.f90 Transdiffusion/bissnd_tracer.f90 Mascaret/dydxsc.f90 Modules/m_sauve_t.f90 Modules/m_lec_sing_i.f90 ModulesTracer/m_constantes_calcul_tracer_c.f90 Modules/m_critiq_i.f90 Modules/m_extremite_t.f90 API/f90/get_condition_limite.f90 API/f90/get_desc_var_mascaret.f90 Planim/calaig.f90 ModulesTracer/m_diffu_tracer_i.f90 ModulesMascaret/m_carac3_i.f90 Pretrait/algop.f90 ModulesTracer/m_message_tracer_c.f90 API/f90/create_mascaret.f90 ModulesCasier/m_planim_casier_i.f90 Planim/planma.f90 ModulesCastor/m_donnees_crues_calage_t.f90 Modules/m_date_s.f90 Pretrait/lec_geom_v3p0.f90 Modules/m_lec_zone_seche_i.f90 ModulesTracer/m_resequ_i.f90 Castor/strick.f90 Modules/m_pretrait_i.f90 API/f90/mascaret_interface.f90 ModulesCastor/m_diff_jcout_i.f90 Modules/m_chainage_rezo_i.f90 Mascaret/borne.f90 Mascaret/conflu.f90 Lapack/ilaenv.f N2QN1/n2qn1.f Modules/m_papy_i.f90 Deriv/adstack.c Modules/m_calaig_i.f90 ModulesCastor/m_simul_i.f90 ModulesDeriv/m_debitance_s_d.f90 ModulesMascaret/m_inigeo_i.f90 ModulesDeriv/m_permat_i_b.f90 Sarap/permat.f90 Rezo/rezo.f90 Casier/lec_apport_pluie.f90 ModulesDeriv/m_calc_pc_conflu_i_b.f90 Pretrait/lec_loi.f90 ModulesAPI/m_apimascaret_i.f90 API/f90/get_taille_var_mascaret.f90 ModulesMascaret/m_precal_i.f90 Planim/pladeb.f90 ModulesTracer/m_godunov_i.f90 ModulesMascaret/m_source_i.f90 Lapack/dscal.f ModulesDeriv/m_repar_i_d.f90 Modules/m_caldyg_i.f90 Modules/m_froude_s.f90 Mascaret/clipp.f90 Castor/cout.f90 Pretrait/lec_sorties.f90 Mascaret/vvprop.f90 Modules/m_ksing_i.f90 Casier/lec_geom_casier1.f90 Modules/m_deversoir_t.f90 Transdiffusion/godunov.f90 Modules/m_rezo_i.f90 Modules/m_ccl_i.f90 ModulesCasier/m_liaison_seuil_i.f90 ModulesTracer/m_muscl_hancock_i.f90 ModulesMascaret/m_matri_i.f90 ModulesTracer/m_calcs_o2_i.f90 Mascaret/presd.f90 ModulesTracer/m_calcsa_i.f90 Modules/m_cqinj_i.f90 ModulesDeriv/m_diff_z_cf12_bwd_sarap_var.f90 Transdiffusion/resequ.f90 ModulesTracer/m_lec_fic_conc_ini_tracer_i.f90 Modules/m_permat_i.f90 Mascaret/solvro.f90 Mascaret/calvar.f90 Mascaret/csurm1.f90 ModulesTracer/m_calcs_rien_i.f90 ModulesCasier/m_casier_t.f90 Modules/m_calc_pc_conflu_i.f90 Y12M/y12mcf.f Mascaret/frotte.f90 API/f90/get_type_var_mascaret.f90 Pretrait/lec_geom_v2p0.f90 Pretrait/calc_maillage.f90 Mascaret/limite.f90 Lapack/dtrsm.f Lapack/dcopy.f Mascaret/cele.f90 Mascaret/calcon.f90 Casier/liaison_seuil.f90 ModulesTracer/m_bilmas_i.f90 ModulesCastor/m_donnees_calage_t.f90 API/f90/rezo_interface.f90 ModulesDeriv/m_permat_i_d.f90 Rezo/calcul.f90 Lapack/dger.f Modules/m_prelido_i.f90 Post/post_imp.f90 Casier/lec_liaison.f90 Rezo/cqinj.f90 Mascaret/cfl2d.f90 Rezo/sing10.f90 ModulesTracer/m_post_imp_tracer_i.f90 Rezo/dpdz.f90 ModulesTracer/m_node_tracer_t.f90 Mascaret/cfl2d1.f90 ModulesCasier/m_traiter_erreur_casier_i.f90 ModulesCasier/m_liaisonrc_t.f90 ModulesMascaret/m_presd_i.f90 ModulesMascaret/m_debita_i.f90 Modules/m_caldeb_i.f90 Sarap/qnode.f90 Modules/m_pladeb_i.f90 Mascaret/promvt.f90 Modules/m_lec_apport_i.f90 Casier/bilvol.f90 Mascaret/fconj.f90 Pretrait/lec_maillage.f90 Mascaret/pres.f90 Modules/m_loilim_i.f90 Modules/m_profil_t.f90 ModulesDeriv/m_sarap_i_b.f90 Transdiffusion/muscl_hancock.f90 Modules/m_qnode_i.f90 Pretrait/lec_dever.f90 ModulesCasier/m_message_casier_c.f90 Casier/survol.f90 API/f90/set_taille_var_mascaret.f90 ModulesTracer/m_bissnd_tracer_i.f90 ModulesTracer/m_qcl_tracer_i.f90 Lapack/dgemv.f ModulesMascaret/m_cele_i.f90 ModulesDeriv/m_persar_i_d.f90 Modules/m_qrepar_i.f90 Mascaret/flusrc.f90 ModulesTracer/m_lec_conc_ini_tracer_i.f90 Sarap/persar.f90 ModulesAPI/m_etat_liaison_t.f90 Sarap/calc_pc_conflu.f90 Modules/m_lec_geom_v3p0_i.f90 Mascaret/mascaret.f90 Mascaret/aintdc.f90 ModulesMascaret/m_zbrent_i.f90 ModulesTracer/m_cond_lim_tracer_t.f90 Lapack/dlaswp.f ModulesMascaret/m_rieman_i.f90 Pretrait/opt2fort.f90 Modules/m_share_var.f90 Modules/m_rhsb1_s.f90 API/cpp/apimascaret.h Rezo/singularite_rezo.f90 Casier/lec_geom_casier2.f90 Modules/m_lec_reseau_i.f90 API/f90/init_ligne_mascaret.f90 Pretrait/lec_fic_conc_ini_tracer.f90 ModulesCasier/m_lec_casier_i.f90 Lapack/dswap.f Qcl/qcl_tracer.f90 ModulesTracer/m_lec_loi_tracer_i.f90 ModulesAPI/m_mascaret_t.f90 ModulesMascaret/m_limite_i.f90 Mascaret/precal.f90 Super/print_stat_masc.f90 ModulesTracer/m_pretrait_tracer_i.f90 API/f90/set_etat_mascaret.f90 Planim/planim.f90 Lapack/lsame.f ModulesMascaret/m_invmat_i.f90 Pretrait/lec_ligne.f90 Modules/m_intersect_i.f90 Pretrait/lec_stockage.f90 ModulesDeriv/m_critiq_i_d.f90 ModulesDeriv/m_debitance_s_b.f90 ModulesMascaret/m_flusrc_i.f90 ModulesMascaret/m_fperd_i.f90 Y12M/y12maf.f API/f90/get_erreur_mascaret.f90 ModulesMascaret/m_boussi_i.f90 Mascaret/akivm1.f90 ModulesMascaret/m_mascaret_i.f90 Lapack/dgbtf2.f ModulesMascaret/m_dichod_i.f90 ModulesTracer/m_loi_tracer_t.f90 API/f90/lec_loi_interface.f90 Pretrait/sauve_maillage.f90 Sarap/repar.f90 ModulesMascaret/m_alarg_i.f90 ModulesTracer/m_stock_tracer_i.f90 Pretrait/lec_fic_loi_tracer.f90 Transdiffusion/diffu_tracer.f90 Mascaret/rieman.f90 Modules/m_profil_plan_t.f90 ModulesCasier/m_liaison_chenal_i.f90 Pretrait/pretrait.f90 ModulesAPI/m_etat_mascaret_t.f90 Modules/m_message_c.f90 ModulesCastor/m_cout_i.f90 Modules/m_lec_geom_i.f90 ModulesMascaret/m_fconj_i.f90 ModulesDeriv/m_calc_pc_conflu_i_d.f90 Modules/m_planma_i.f90 ModulesMascaret/m_matria_i.f90 ModulesMascaret/m_conflu_i.f90 Modules/m_loi_t.f90 ModulesDeriv/m_qrepar_i_d.f90 Modules/m_dpdz_i.f90 ModulesCasier/m_liaison_t.f90 ModulesMascaret/m_resol_i.f90 Lapack/dgbtrs.f Casier/lec_casier.f90 API/f90/version_mascaret.f90 Planim/compt_chenaux.f90 ModulesMascaret/m_calvar_i.f90 Pretrait/lec_pcsing.f90 Modules/m_debitance_s.f90 ModulesTracer/m_parametres_qualite_eau_t.f90 ModulesCasier/m_liaison_siphon_i.f90 Deriv/diff_z_cf12_bwd_sarap.f90 Mascaret/calcfl.f90 Pretrait/lec_apport.f90 Deriv/adbuffer.f Modules/m_lec_dever_i.f90 Pretrait/calc_connect.f90 ModulesMascaret/m_aintdc_i.f90 Planim/dicho.f90 Casier/liaison_siphon.f90 API/f90/calcul_mascaret.f90 Modules/m_lec_hydrau_i.f90 Casier/clpluie.f90 ModulesDeriv/m_interpolation_s_b.f90 Modules/m_section_t.f90 Modules/m_lec_loi_i.f90 Mascaret/caleta.f90 Stock/stock_tracer.f90 ModelesQualEau/lec_parphy.f90 Modules/m_repar_i.f90 API/f90/set_valeur_mascaret.f90 Modules/m_decoder_geom_i.f90 Pretrait/lec_zone_seche.f90 Mascaret/valida.f90 ModulesMascaret/m_solvro_i.f90 ModulesMascaret/m_bilan_i.f90 ModulesDeriv/m_psing_i_b.f90 Casier/liaison_orifice.f90 Casier/decoder_geom_casier.f90 Transdiffusion/tracer.f90 Pretrait/lec_loi_tracer.f90 ModulesDeriv/m_qnode_i_d.f90 ModulesCastor/m_constantes_calage_t.f90 Mascaret/carac.f90 Modules/m_calint_i.f90 ModulesTracer/m_lec_fic_loi_tracer_i.f90 ModulesDeriv/m_interpolation_s_d.f90 Modules/m_lec_geom_v2p0_i.f90 ModulesMascaret/m_seuil_i.f90 Y12M/y12mbf.f Mascaret/erodev.f90 ModulesTracer/m_calcs_biomass_i.f90 ModulesMascaret/m_vvpropi_i.f90 Modules/m_post_i.f90 ModulesMascaret/m_bissnd_i.f90 ModulesCastor/m_calage_n2qn1_i.f90 Pretrait/lec_abaque.f90 Modules/m_rezomat_t.f90 Mascaret/fludev.f90 ModulesMascaret/m_calcfl_i.f90 ModulesTracer/m_index_variable_tracer_c.f90 Pretrait/lec_source.f90 Y12M/y12mdf.f Modules/m_connect_t.f90 Modules/m_rhsbp_s.f90 ModulesTracer/m_calcs_eutro_i.f90 Modules/m_fichier_t.f90 ModelesQualEau/calcs_micropol.f90 ModulesMascaret/m_nulmat_i.f90 Pretrait/lec_barrage.f90 ModulesCastor/m_strick_i.f90 ModulesMascaret/m_fh_i.f90 Transdiffusion/hyp1fa.f90 Mascaret/decbar.f90 Modules/m_lec_ligne_i.f90 Modules/m_qcl_i.f90 Modules/m_rempli_i.f90 Mascaret/fluroe.f90 Modules/m_xindic_s.f90 API/f90/init_etat_mascaret.f90 Casier/liaison_chenal.f90 Pretrait/lec_geom.f90 Modules/m_calc_maillage_i.f90 Mascaret/fc1.f90 Mascaret/minmaj.f90 Qcl/qcl.f90 ModulesAPI/m_lec_ligne_interface_i.f90 Mascaret/csur.f90 ModulesDeriv/m_rhsbp_s_b.f90 ModulesMascaret/m_frotte_i.f90 ModulesMascaret/m_solex_i.f90 Modules/m_init_var_sortie_s.f90 Pretrait/lec_ligne_opt.f90 Modules/m_barrage_t.f90 Modules/m_psing_i.f90 Modules/m_sauve_maillage_i.f90 Pretrait/lec_reseau.f90 Lapack/dtbsv.f Mascaret/indice.f90 Modules/m_interpolation_s.f90 API/f90/calcul_mascaret_condition_limite.f90 ModulesMascaret/m_fluroe_i.f90 ModulesAPI/m_rezo_interface_i.f90 Mascaret/matri.f90 Modules/m_erreur_t.f90 ModelesQualEau/calcs_o2.f90 Mascaret/bissnd.f90 ModulesCastor/m_direct_i.f90 API/f90/import_xml.f90 Pretrait/decoder_geom.f90 API/f90/get_valeur_mascaret.f90 Modules/m_lec_pcsing_i.f90 Pretrait/lec_planim.f90 API/f90/init_abaque.f90 Modules/m_contq_i.f90 ModulesDeriv/m_cqinj_i_b.f90 Modules/m_sing3_i.f90 Planim/caldyg.f90 Modules/m_singularite_t.f90 Mascaret/carac3.f90 API/f90/lec_ligne_interface.f90 Modules/m_abs_abs_s.f90 Modules/m_gaussj_i.f90 Mascaret/debita.f90 Mascaret/solex.f90 Rezo/loilim.f90 ModulesTracer/m_lec_source_i.f90 Modules/m_dicho_i.f90 Mascaret/nulmat.f90 Sarap/sarap.f90 ModulesAPI/m_lec_loi_interface_i.f90 Mascaret/dichodm.f90 Rezo/chainage_rezo.f90 Lapack/ieeeck.f ModulesMascaret/m_frottd_i.f90 Casier/post_imp_casier.f90 Super/err_alloc.f90 Casier/kliaison.f90 Rezo/sing2.f90 ModulesMascaret/m_carac_i.f90 ModulesAPI/m_etat_casier_t.f90 ModulesTracer/m_parphy_i.f90 Modules/m_lec_abaque_i.f90 ModulesTracer/m_init_var_sortie_tracer_s.f90 Modules/m_constantes_calcul_c.f90 ModulesMascaret/m_promat_i.f90 Planim/papy.f90 Pretrait/lec_hydrau.f90 ModulesMascaret/m_dichodm_i.f90 ModulesDeriv/m_qrepar_i_b.f90 Modules/m_stock_i.f90 ModulesCasier/m_post_casier_i.f90 Castor/direct.f90 Castor/stock_calage.f90 API/f90/pretrait_interface.f90 Lapack/idamax.f ModulesAPI/m_modele_mascaret_t.f90 Transdiffusion/calcsa.f90 Mascaret/promat.f90 ModelesQualEau/lec_meteo.f90 ModulesMascaret/m_csur_i.f90 Deriv/diff_z_cf12_fwd_sarap.f90 Mascaret/alarg.f90 ModulesMascaret/m_csurm1_i.f90 Modules/m_lec_stockage_i.f90 Modules/m_lire_chaine_s.f90 Mascaret/csurmd.f90 API/cpp/apimascaret.c API/f90/export_xml.f90 ModulesAPI/m_mascaret_interface_i.f90
clean: neat
	-rm -f .mascaret.cppdefs $(OBJ) *.mod
neat:
	-rm -f $(TMPFILES)
proper: clean FOX_clean
	if test -e ${INSTALL_DIR}/lib/mascaret/mascaret.dll ; \
	then \
		(rm -f ${INSTALL_DIR}/lib/mascaret/mascaret.dll) ; \
	fi
	if test -e ${INSTALL_DIR}/lib/mascaret/mascaret.so ; \
	then \
		(rm -f ${INSTALL_DIR}/lib/mascaret/mascaret.so) ; \
	fi
	if test -e ${INSTALL_DIR}/lib/mascaret/mascaret.dylib ; \
	then \
		(rm -f ${INSTALL_DIR}/lib/mascaret/mascaret.dylib) ; \
	fi
	if test -e ${INSTALL_DIR}/bin/mascaret.exe ; \
	then \
		(rm -f ${INSTALL_DIR}/bin/mascaret.exe) ; \
	fi
	if test -e ${INSTALL_DIR}/bin/mascaret ; \
	then \
		(rm -f ${INSTALL_DIR}/bin/mascaret) ; \
	fi
	if test -e ${INSTALL_DIR}/bin/mascaretOSX ; \
	then \
		(rm -f ${INSTALL_DIR}/bin/mascaretOSX) ; \
	fi
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
mascaret.exe: FOX_LIB $(OBJ)
	$(LD) $(OBJ) -o ${INSTALL_DIR}/bin/mascaret.exe $(LDFLAGS)
mascaret.dll: FOX_LIB $(OBJ)
	$(LD) $(OBJ) -shared -Wl,-soname,mascaret.dll -o ${INSTALL_DIR}/lib/mascaret/mascaret.dll $(LDFLAGSLIB)
mascaret: FOX_LIB $(OBJ)
	$(LD) $(OBJ) -o ${INSTALL_DIR}/bin/mascaret $(LDFLAGS)
mascaret.so: FOX_LIB $(OBJ)
	$(LD) $(OBJ) -shared -Wl,-soname,mascaret.so -o ${INSTALL_DIR}/lib/mascaret/mascaret.so $(LDFLAGSLIB)
mascaretOSX: FOX_LIB $(OBJ)
	$(LD) $(OBJ) -o ${INSTALL_DIR}/bin/mascaretOSX $(LDFLAGS)
mascaret.dylib: FOX_LIB $(OBJ)
	$(LD) $(OBJ) -shared -Wl,-install_name,mascaret.dylib -o ${INSTALL_DIR}/lib/mascaret/mascaret.dylib $(LDFLAGSLIB)
libmascaret.a: FOX_LIB $(OBJ)
	-rm ${INSTALL_DIR}/lib/mascaret/libmascaret.a
	ar cq ${INSTALL_DIR}/lib/mascaret/libmascaret.a $(OBJ)
FOX_LIB:
	if test -e Fox/FoX-config ; \
	then \
		(cd Fox; $(MAKE)) ; \
	else \
		(cd Fox; ./configure; $(MAKE)) ; \
	fi
FOX_clean:
	(cd Fox; $(MAKE) clean)
	(cd Fox; rm -f FoX-config)

