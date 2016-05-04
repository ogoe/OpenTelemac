! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>stellt verschiedene Informationen zu physikalischen Gr&ouml;&szlig;en bereit</h2>
!! @author G&uuml;nther Lang
!! @version 3.3 vom 03/26/07, Quellcode: mod_b_phy.f90
!! <HR>
!! delivers various informations about physical quantities<BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-01-18 : G. Lang : Erstversion
!  01.02 : 2005-01-21 : G. Lang : Korrekturen an 009, 059, 060, 061
!  01.03 : 2005-01-24 : G. Lang : offset entfernt bei Grad C/h
!  01.04 : 2005-03-01 : G. Lang : SHAPE-Befehl in Parameter SHAPE durch explizite Dimensionen ersetzt
!  01.05 : 2005-03-24 : G. Lang : OMIELE, OMIGEN beruecksichtigt, sowie Funktionen integriert
!  01.06 : 2005-03-30 : G. Lang : neue Einheit mm/s**2
!  02.01 : 2005-07-21 : G. Lang : neue Funktionen sowie Anpassungen von OMIELE, OMIGEN
!  02.02 : 2005-08-10 : G. Lang : Indikator Flussgroessen, neue UNITS, Suchen der Einheit fuer Flussgroessen
!  02.03 : 2006-03-29 : G. Lang : Verschiedene neue Funktionen wg. CF-Konventionen
!  02.04 : 2005-04-10 : G. Lang : Plotfaktoren aus SI<-->DATA eliminiert
!  02.05 : 2006-07-25 : G. Lang : Konv.faktor zum Umrechnen in SI-Einheiten bei Salz teilweise modifiziert.
!  03.01 : 2006-12-21 : G. Lang : Umstellen der SI-Basiseinheiten auf Real-Zahlen
!  03.02 : 2007-02-09 : Schade  : FM -9030 falls Konfigurationsdatei nicht existiert
!  03.03 : 2007-02-09 : Schade  : FM -9030 verbessert
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! 1) Methoden zu physikalischen Gr&ouml;&szlig;en; <BR>
!! 2) Methoden zu physikalischen Einheiten, inklusive deren 
!!    Definition in SI-Einheiten sowie den entsprechenden 
!!    Umrechnungsfaktoren. <BR>
!! 3) Methoden zu physikalischen Klassen; <BR>
!! 4) Methoden zum Umrechnen von Daten. <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt <EM>keinen</EM> Datentyp zur Verf&uuml;gung. 
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden der Modulmethoden: USE b_phy
!!    <LI> Initialisieren und Setzen von Konstantwerten:
!!         <OL>
!!         <LI> CALL INIT_PHY ( )
!!         <LI> CALL SETUP_PHY_PRN_LUN ( <EM>parameter</EM> )
!!         <LI> CALL SETUP_PHY_TRC_LUN ( <EM>parameter</EM> )
!!         <LI> CALL SETUP_PHY_LANGUAGE ( <EM>parameter</EM> )
!!         <LI> CALL SETUP_PHY_THRESHOLD ( <EM>parameter</EM> )
!!         </OL>
!!    <LI> Korrektheit pr&uuml;fen:
!!         <OL>
!!         <LI> OK_PHY ( )
!!         </OL>
!!    <LI> Informationen &uuml;ber physikalische Gr&ouml;&szlig;en: 
!!         <OL>
!!         <LI> Funktionen GET_PHY_QUANT_*, 
!!         <LI> Funktionen IS_PHY_QUANT_*, und
!!         <LI> Funktionen HAS_PHY_QUANT_*.
!!         </OL>
!!    <LI> Informationen &uuml;ber physikalische Einheiten: 
!!         <OL>
!!         <LI> Funktionen GET_PHY_UNIT_*.
!!         </OL>
!!    <LI> Informationen &uuml;ber physikalische Klassen: 
!!         <OL>
!!         <LI> Funktionen GET_PHY_CLASS_*.
!!         </OL>
!!    <LI> Umrechnen von Datens&auml;tzen (Skalar, 1D-/2D-/3D-Felder): 
!!         <OL>
!!         <LI> GET_PHY_CONV_ORIG_TO_SI_DATA (Original nach SI),
!!         <LI> GET_PHY_CONV_ORIG_TO_PLOT_DATA (Original nach Plot) und
!!         <LI> GET_PHY_CONV_SI_TO_ORIG_DATA (SI nach Original).
!!         </OL>
!!    <LI> De-Initialisieren des Moduls:
!!         <OL>
!!         <LI> CALL CLEAR_PHY ( )
!!         </OL>
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,
!!          andere nicht. 
!!          Routinen die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,
!!          ob das Modul korrekt initialisert wurde (ok_initialised)  <BR>
!!                                                                    <BR>
!! Man verwende PRINT_PHY_ALL_ERRORS f&uuml;r eine &Uuml;bersicht zu
!! allen m&ouml;glichen Fehlermeldungen.
!!
!! <HR>
!
MODULE b_phy
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       DOUBLE,            &
       Single
  !
  ! [A.2] BASIS-Modul mit Fehler-Typ und -Routinen 
  !
  USE b_error, ONLY :       &
       ! Typdefinitionen
       t_error,             &
       ! Routinen
       init_error,          &
       clear_error,         &
       no_error,            &
       any_error,           &
       NEW_error,           &
       kill_error,          &
       PRINT_error,         &
       setup_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
  !
  ! [A.3] BASIS-Modul mit Datei-Typ und -Routinen 
  !
  USE b_file, ONLY :       &
       ! Typdefinitionen
       t_file,             &
       ! Routinen
       init_file,          &
       clear_file,         &
       setup_file_prn_lun, &
       setup_file_trc_lun, &
       NEW_file,           &
       kill_file,          &
       set_file_unit,      &
       set_file_name,      &
       set_file_status,    &
       set_file_access,    &
       set_file_form,      &
       set_file_recl,      &
       set_file_position,  &
       set_file_action,    &
       set_file_delim,     &
       set_file_type,      &
       get_file_unit,      &
       get_file_name,      &
       file_exists,        &
       OPEN_file,          &
       CLOSE_file
  !
  ! [A.4] BASIS-Modul mit Kommando-Typ und -Routinen 
  !
  USE b_cmds, ONLY :       &
       ! Routinen
       init_cmds,          &
       clear_cmds,         &
       setup_cmds_prn_lun, &
       setup_cmds_trc_lun, &
       getenvv_cmds,       &
       pathdelim_cmds
  !
  ! ---------------------------------------------------------------------
  ! [B] alles muss explizit deklariert werden und ist default privat
  ! ---------------------------------------------------------------------
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! max. Anzahl der Zeichen in dem Standardnamen
  INTEGER , PUBLIC  , PARAMETER :: c_len_standard_name=120 ! 
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  ! Hinweis: verschiedene Methoden arbeiten auf Skalar und 1D-Array.
  !          Ggf. weitere ergaenzen (z.B. 2D-Array) falls sinnvoll.
  !
  ! ---------------------------------------------------------------------
  ! INIT
  ! ---------------------------------------------------------------------
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_phy
     MODULE PROCEDURE init_phy_d ! 
  END INTERFACE
  !
  ! ---------------------------------------------------------------------
  ! CLEAR
  ! ---------------------------------------------------------------------
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_phy
     MODULE PROCEDURE clear_phy_d ! 
  END INTERFACE
  !
  ! ---------------------------------------------------------------------
  ! SETUP
  ! ---------------------------------------------------------------------
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_phy_prn_lun
     MODULE PROCEDURE setup_phy_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_phy_trc_lun
     MODULE PROCEDURE setup_phy_trc_lun_d ! 
  END INTERFACE
  !! Index f&uuml;r Spracheinstellung setzen <BR>
  !! 1 = Deutsch (Default) <BR>
  !! 2 = Englisch         
  INTERFACE setup_phy_language
     MODULE PROCEDURE setup_phy_language_d ! 
  END INTERFACE
  !! Setzen der Werte zum Kennzeichnen ung&uuml;ltiger Daten in Daten-
  !! Konversionsoperationen <BR>
  !! a) f&uuml;r INTEGER-Daten <BR>
  !! b) f&uuml;r REAL(Single)-Daten <BR>
  !! c) f&uuml;r REAL(Double)-Daten
  INTERFACE setup_phy_threshold
     MODULE PROCEDURE setup_phy_threshold_in_d
     MODULE PROCEDURE setup_phy_threshold_re_d
     MODULE PROCEDURE setup_phy_threshold_dp_d
  END INTERFACE
  !
  ! ---------------------------------------------------------------------
  ! GET
  ! ---------------------------------------------------------------------
  !! Index f&uuml;r Spracheinstellung ermitteln <BR>
  !! a) Deutsch (Default) <BR>
  !! b) Englisch         
  INTERFACE get_phy_language
     MODULE PROCEDURE get_phy_language_d ! 
  END INTERFACE
  ! --- QUANTITAETEN ----------------------------------------------------
  !! Index f&uuml;r Sprache einer physikalischen Gr&ouml;&szlig;e ermitteln <BR>
  !! a) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_language
     MODULE PROCEDURE get_phy_quant_language_0
     MODULE PROCEDURE get_phy_quant_language_1
  END INTERFACE
  !! Ermittle die Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_code
     MODULE PROCEDURE get_phy_quant_code_0
     MODULE PROCEDURE get_phy_quant_code_1
  END INTERFACE
  !! Ermittle die maximal zul&auml;ssige Code-Kennung einer physikalischen Gr&ouml;&szlig;e
  INTERFACE get_phy_quant_max_code
     MODULE PROCEDURE get_phy_quant_max_code_0
  END INTERFACE
  !! Ermittle die Id einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR> 
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_id
     MODULE PROCEDURE get_phy_quant_id_code_0
     MODULE PROCEDURE get_phy_quant_id_code_1
     MODULE PROCEDURE get_phy_quant_id_descr_0
     MODULE PROCEDURE get_phy_quant_id_descr_1
  END INTERFACE
  !! Ermittle die Beschreibung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR> 
  !! c) f&uuml;r die Id einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Ids physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_descr
     MODULE PROCEDURE get_phy_quant_descr_code_0
     MODULE PROCEDURE get_phy_quant_descr_code_1
     MODULE PROCEDURE get_phy_quant_descr_id_0
     MODULE PROCEDURE get_phy_quant_descr_id_1
  END INTERFACE
  !! Ermittle die Code-Kennung der Referenzgr&ouml;&szlig;e einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_ref_code
     MODULE PROCEDURE get_phy_quant_ref_code_code_0
     MODULE PROCEDURE get_phy_quant_ref_code_code_1
     MODULE PROCEDURE get_phy_quant_ref_code_descr_0
     MODULE PROCEDURE get_phy_quant_ref_code_descr_1
  END INTERFACE
  !! Ermittle die Id der Referenzgr&ouml;&szlig;e einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_ref_id
     MODULE PROCEDURE get_phy_quant_ref_id_code_0
     MODULE PROCEDURE get_phy_quant_ref_id_code_1
     MODULE PROCEDURE get_phy_quant_ref_id_descr_0
     MODULE PROCEDURE get_phy_quant_ref_id_descr_1
  END INTERFACE
  !! Ermittle die Beschreibung der Referenzgr&ouml;&szlig;e einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_ref_descr
     MODULE PROCEDURE get_phy_quant_ref_descr_code_0
     MODULE PROCEDURE get_phy_quant_ref_descr_code_1
     MODULE PROCEDURE get_phy_quant_ref_descr_descr_0
     MODULE PROCEDURE get_phy_quant_ref_descr_descr_1
  END INTERFACE
  !! Ermittle den Typ-Indikator einer physikalischen Gr&ouml;&szlig;e            <BR>
  !! [ Skalar == 1 ; Vektor == 2 ; Tensor == 3 ]                                 <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor)        <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_type_indicator
     MODULE PROCEDURE get_phy_quant_typei_code_0
     MODULE PROCEDURE get_phy_quant_typei_code_1
     MODULE PROCEDURE get_phy_quant_typei_descr_0
     MODULE PROCEDURE get_phy_quant_typei_descr_1
  END INTERFACE
  !! Ermittle den Zeit-Indikator einer physikalischen Gr&ouml;&szlig;e <BR>
  !! [ zeitunabh&auml;ngig == 0 ; synoptisch == 1 ; periodenbezogen == 2 ]       <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_time_indicator
     MODULE PROCEDURE get_phy_quant_timei_code_0
     MODULE PROCEDURE get_phy_quant_timei_code_1
     MODULE PROCEDURE get_phy_quant_timei_descr_0
     MODULE PROCEDURE get_phy_quant_timei_descr_1
  END INTERFACE
  !! Ermittle den Plot-Indikator einer physikalischen Gr&ouml;&szlig;e <BR>
  !! zur Bedeutung siehe Feld "c_plot_text"                            <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_plot_indicator
     MODULE PROCEDURE get_phy_quant_ploti_code_0
     MODULE PROCEDURE get_phy_quant_ploti_code_1
     MODULE PROCEDURE get_phy_quant_ploti_descr_0
     MODULE PROCEDURE get_phy_quant_ploti_descr_1
  END INTERFACE
  !! Ermittle den Plot-Text zu einer physikalischen Gr&ouml;&szlig;e <BR>
  !! siehe Feld "c_plot_text" <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_plot_text
     MODULE PROCEDURE get_phy_quant_plott_code_0
     MODULE PROCEDURE get_phy_quant_plott_code_1
     MODULE PROCEDURE get_phy_quant_plott_descr_0
     MODULE PROCEDURE get_phy_quant_plott_descr_1
  END INTERFACE
  !! Ermittle den Plot-Faktor einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_plot_factor
     MODULE PROCEDURE get_phy_quant_plotf_code_0
     MODULE PROCEDURE get_phy_quant_plotf_code_1
     MODULE PROCEDURE get_phy_quant_plotf_descr_0
     MODULE PROCEDURE get_phy_quant_plotf_descr_1
  END INTERFACE
  !! Ermittle die Anzahl der f&uuml;r eine physikalische Gr&ouml;&szlig;e
  !! zu ber&uuml;cksichtigenden Gitter-ElementSets                               <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor)        <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_omiele_nof
     MODULE PROCEDURE get_phy_quant_ele_nof_code_0
     MODULE PROCEDURE get_phy_quant_ele_nof_code_1
     MODULE PROCEDURE get_phy_quant_ele_nof_descr_0
     MODULE PROCEDURE get_phy_quant_ele_nof_descr_1
  END INTERFACE
  !! Ermittle die Anzahl der Aspekte, die bei der Erzeugung eines Daten-ElementSets 
  !! zu ber&uuml;cksichtigen sind <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor)        <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_omigen_nof
     MODULE PROCEDURE get_phy_quant_gen_nof_code_0
     MODULE PROCEDURE get_phy_quant_gen_nof_code_1
     MODULE PROCEDURE get_phy_quant_gen_nof_descr_0
     MODULE PROCEDURE get_phy_quant_gen_nof_descr_1
  END INTERFACE
  !! Ermittle f&uuml;r eine physikalische Gr&ouml;&szlig;e den Namen eines zu 
  !! ber&uuml;cksichtigenden Gitter-ElementSets anhand der lfd. Nummer          <BR>
  !! a) eine physikalische Gr&ouml;&szlig;e und eine lfd. Nummer                <BR>
  !! b) eine physikalische Gr&ouml;&szlig;e und viele lfd. Nummern              <BR>
  !! c) eine physikalische Beschreibung und eine lfd. Nummer                    <BR>
  !! d) eine physikalische Beschreibung und viele lfd. Nummern
  INTERFACE get_phy_quant_omiele
     MODULE PROCEDURE get_phy_quant_ele_code_0_0
     MODULE PROCEDURE get_phy_quant_ele_code_0_1
     MODULE PROCEDURE get_phy_quant_ele_descr_0_0
     MODULE PROCEDURE get_phy_quant_ele_descr_0_1
  END INTERFACE
  !! Ermittle f&uuml;r eine physikalische Gr&ouml;&szlig;e anhand der lfd. Nummer
  !! des zu erzeugenden <EM>ElementSet</EM>, ob dieses zwei- oder dreidimensional
  !! ist <BR>
  !! a) eine physikalische Gr&ouml;&szlig;e und eine lfd. Nummer                <BR>
  !! b) eine physikalische Gr&ouml;&szlig;e und viele lfd. Nummern              <BR>
  !! c) eine physikalische Beschreibung und eine lfd. Nummer                    <BR>
  !! d) eine physikalische Beschreibung und viele lfd. Nummern
  INTERFACE is_phy_quant_omiele_3d
     MODULE PROCEDURE is_phy_quant_ele_3d_code_0_0
     MODULE PROCEDURE is_phy_quant_ele_3d_code_0_1
     MODULE PROCEDURE is_phy_quant_ele_3d_descr_0_0
     MODULE PROCEDURE is_phy_quant_ele_3d_descr_0_1
  END INTERFACE
  !! Ermittle anhand der lfd. Nummer einen Aspekt, der bei der Erzeugung eines 
  !! Daten-ElementSets zu ber&uuml;cksichtigen sind                             <BR>
  !! a) eine physikalische Gr&ouml;&szlig;e und eine lfd. Nummer                <BR>
  !! b) eine physikalische Gr&ouml;&szlig;e und viele lfd. Nummern              <BR>
  !! c) eine physikalische Beschreibung und eine lfd. Nummer                    <BR>
  !! d) eine physikalische Beschreibung und viele lfd. Nummern
  INTERFACE get_phy_quant_omigen
     MODULE PROCEDURE get_phy_quant_gen_code_0_0
     MODULE PROCEDURE get_phy_quant_gen_code_0_1
     MODULE PROCEDURE get_phy_quant_gen_descr_0_0
     MODULE PROCEDURE get_phy_quant_gen_descr_0_1
  END INTERFACE
  ! --- SI-EINHEITEN ----------------------------------------------------
  !! Ermittle die Id der Einheit einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! e) f&uuml;r vorgegebene Code-Kennung und Basisdimensionen <EM>einer</EM> physikalischen Gr&ouml;&szlig;e
  INTERFACE get_phy_unit_id
     MODULE PROCEDURE get_phy_unit_id_code_0
     MODULE PROCEDURE get_phy_unit_id_code_1
     MODULE PROCEDURE get_phy_unit_id_descr_0
     MODULE PROCEDURE get_phy_unit_id_descr_1
     MODULE PROCEDURE get_phy_unit_id_si_base_0
  END INTERFACE 
  !! Ermittle die Beschreibung der Einheit einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r die Code-Kennungen mehrerer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  !! e) f&uuml;r vorgegebene Code-Kennung und Basisdimensionen <EM>einer</EM> physikalischen Gr&ouml;&szlig;e
  INTERFACE get_phy_unit_descr
     MODULE PROCEDURE get_phy_unit_descr_code_0
     MODULE PROCEDURE get_phy_unit_descr_code_1
     MODULE PROCEDURE get_phy_unit_descr_descr_0
     MODULE PROCEDURE get_phy_unit_descr_descr_1
     MODULE PROCEDURE get_phy_unit_descr_si_base_0
  END INTERFACE 
  !! Ermittle die Anzahl der SI-Basiseinheiten
  INTERFACE get_phy_unit_max_si_base
     MODULE PROCEDURE get_phy_unit_max_si_base_d
  END INTERFACE
  !! Ermittle die Namen der SI-Basiseinheiten
  INTERFACE get_phy_unit_si_base_name
     MODULE PROCEDURE get_phy_unit_si_base_name_d
  END INTERFACE
  !! Ermittle die SI-Basiseinheiten
  INTERFACE get_phy_unit_si_base_unit
     MODULE PROCEDURE get_phy_unit_si_base_unit_d
  END INTERFACE
  !! Ermittle Symbole der SI-Basiseinheiten
  INTERFACE get_phy_unit_si_base_symb
     MODULE PROCEDURE get_phy_unit_si_base_symb_d
  END INTERFACE
  !! Ermittle die SI-Basisdimension einer physikalischen Gr&ouml;&szlig;e 
  !! bei vorgegebenem Index der Basisdimension <BR>
  !! Index 1 = Laenge <BR>
  !! Index 2 = Masse  <BR>
  !! Index 3 = Zeit   <BR>
  !! Index 4 = elektr. Strom <BR>
  !! Index 5 = Temperatur <BR>
  !! Index 6 = Stoffmenge <BR>
  !! Index 7 = Lichtintensitaet <BR>
  !! Index 8 = Waehrung <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_unit_si_base_power
     MODULE PROCEDURE get_phy_unit_power_code_0
     MODULE PROCEDURE get_phy_unit_power_code_1
     MODULE PROCEDURE get_phy_unit_power_descr_0
     MODULE PROCEDURE get_phy_unit_power_descr_1
  END INTERFACE 
  !! Ermittle den Konversionsfaktor zur Umrechnung einer physikalischen 
  !! Gr&ouml;&szlig;e in SI-Einheiten <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! e) f&uuml;r vorgegebene Code-Kennung und Basisdimensionen <EM>einer</EM> physikalischen Gr&ouml;&szlig;e
  INTERFACE get_phy_unit_si_base_convf
     MODULE PROCEDURE get_phy_unit_convf_code_0
     MODULE PROCEDURE get_phy_unit_convf_code_1
     MODULE PROCEDURE get_phy_unit_convf_descr_0
     MODULE PROCEDURE get_phy_unit_convf_descr_1
     MODULE PROCEDURE get_phy_unit_convf_si_base_0
  END INTERFACE 
  !! Ermittle den Offset (in SI-Einheiten) zur Umrechnung einer physikalischen 
  !! Gr&ouml;&szlig;e in SI-Einheiten <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! e) f&uuml;r vorgegebene Code-Kennung und Basisdimensionen <EM>einer</EM> physikalischen Gr&ouml;&szlig;e
  INTERFACE get_phy_unit_si_base_offset
     MODULE PROCEDURE get_phy_unit_offset_code_0
     MODULE PROCEDURE get_phy_unit_offset_code_1
     MODULE PROCEDURE get_phy_unit_offset_descr_0
     MODULE PROCEDURE get_phy_unit_offset_descr_1
     MODULE PROCEDURE get_phy_unit_offset_si_base_0
  END INTERFACE 
  ! --- DATEN-KLASSEN ---------------------------------------------------
  !! Ermittle den Code der Klasse zu der eine physikalische Gr&ouml;&szlig;e geh&ouml;rt <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_class_code
     MODULE PROCEDURE get_phy_class_code_code_0
     MODULE PROCEDURE get_phy_class_code_code_1
     MODULE PROCEDURE get_phy_class_code_descr_0
     MODULE PROCEDURE get_phy_class_code_descr_1
  END INTERFACE 
  !! Ermittle die Id der Klasse zu der eine physikalische Gr&ouml;&szlig;e geh&ouml;rt <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_class_id
     MODULE PROCEDURE get_phy_class_id_code_0
     MODULE PROCEDURE get_phy_class_id_code_1
     MODULE PROCEDURE get_phy_class_id_descr_0
     MODULE PROCEDURE get_phy_class_id_descr_1
  END INTERFACE 
  !! Ermittle die Beschreibung der Klasse zu der eine physikalische Gr&ouml;&szlig;e geh&ouml;rt <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_class_descr
     MODULE PROCEDURE get_phy_class_descr_code_0
     MODULE PROCEDURE get_phy_class_descr_code_1
     MODULE PROCEDURE get_phy_class_descr_descr_0
     MODULE PROCEDURE get_phy_class_descr_descr_1
  END INTERFACE 
  ! --- STANDARD-BEZEICHNUNGEN ------------------------------------------
  !! Ermittle die Anzahl der zu einer phys. Gr&ouml;&szlig;e geh&ouml;renden Standardbezeichnungen <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar)                   <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor)                          <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar)                   <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE get_phy_quant_nof_standard_name
     MODULE PROCEDURE get_phy_quant_nof_stdn_code_0
     MODULE PROCEDURE get_phy_quant_nof_stdn_code_1
     MODULE PROCEDURE get_phy_quant_nof_stdn_descr_0
     MODULE PROCEDURE get_phy_quant_nof_stdn_descr_1
  END INTERFACE
  !! Ermittle die n-te zu einer phys. Gr&ouml;&szlig;e geh&ouml;rende Standardbezeichnung <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar)
  INTERFACE get_phy_quant_standard_name
     MODULE PROCEDURE get_phy_quant_stdn_code_0_0
     MODULE PROCEDURE get_phy_quant_stdn_descr_0_0
  END INTERFACE
  !! Ermittle die n-te zu einer phys. Gr&ouml;&szlig;e geh&ouml;rende Dimension <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar)
  INTERFACE get_phy_quant_standard_dim
     MODULE PROCEDURE get_phy_quant_stdd_code_0_0
     MODULE PROCEDURE get_phy_quant_stdd_descr_0_0
  END INTERFACE
  !
  !! Ermittle die zu einem Standard-Namen geh&ouml;rende phys. Gr&ouml;&szlig;e  <BR>
  !! a) f&uuml;r einen Standard-Namen (Skalar) <BR>
  !! b) f&uuml;r viele Standard-Namen (Vektor)
  INTERFACE get_phy_standard_name_quant
     MODULE PROCEDURE get_phy_stdn_quant_0
     MODULE PROCEDURE get_phy_stdn_quant_1
  END INTERFACE
  !! Ermittle die zu einem Standard-Namen geh&ouml;rende Dimension <BR>
  !! a) f&uuml;r einen Standard-Namen (Skalar) <BR>
  !! b) f&uuml;r viele Standard-Namen (Vektor)
  INTERFACE get_phy_standard_name_dim
     MODULE PROCEDURE get_phy_stdn_dim_0
     MODULE PROCEDURE get_phy_stdn_dim_1
  END INTERFACE
  !
  ! ---------------------------------------------------------------------
  ! PRINT
  ! ---------------------------------------------------------------------
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE PRINT_phy_static
     MODULE PROCEDURE PRINT_phy_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE PRINT_phy_all_errors
     MODULE PROCEDURE PRINT_phy_all_errors_d ! 
  END INTERFACE
  !
  ! ---------------------------------------------------------------------
  ! OK
  ! ---------------------------------------------------------------------
  !! Pr&uuml;fe, ob die Informationen zu den physikalischen Groessen
  !! o.k. sind
  INTERFACE ok_phy
     MODULE PROCEDURE ok_phy_d
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  ! ---------------------------------------------------------------------
  ! IS
  ! ---------------------------------------------------------------------
  !! Ermittle die G&uuml;ltigkeit einer physikalischen Gr&ouml;&szlig;e <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_valid
     MODULE PROCEDURE is_phy_quant_valid_code_0
     MODULE PROCEDURE is_phy_quant_valid_code_1
     MODULE PROCEDURE is_phy_quant_valid_descr_0
     MODULE PROCEDURE is_phy_quant_valid_descr_1
  END INTERFACE
  !! Ermittle ob eine physikalische Gr&ouml;&szlig;e ein <EM>Skalar</EM> ist <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_scalar
     MODULE PROCEDURE is_phy_quant_scalar_code_0
     MODULE PROCEDURE is_phy_quant_scalar_code_1
     MODULE PROCEDURE is_phy_quant_scalar_descr_0
     MODULE PROCEDURE is_phy_quant_scalar_descr_1
  END INTERFACE
  !! Ermittle ob eine physikalische Gr&ouml;&szlig;e ein <EM>Vektor</EM> ist <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_vector
     MODULE PROCEDURE is_phy_quant_vector_code_0
     MODULE PROCEDURE is_phy_quant_vector_code_1
     MODULE PROCEDURE is_phy_quant_vector_descr_0
     MODULE PROCEDURE is_phy_quant_vector_descr_1
  END INTERFACE
  !! Ermittle ob eine physikalische Gr&ouml;&szlig;e ein <EM>Tensor</EM> ist <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_tensor
     MODULE PROCEDURE is_phy_quant_tensor_code_0
     MODULE PROCEDURE is_phy_quant_tensor_code_1
     MODULE PROCEDURE is_phy_quant_tensor_descr_0
     MODULE PROCEDURE is_phy_quant_tensor_descr_1
  END INTERFACE
  !! Ermittle ob eine physikalische Gr&ouml;&szlig;e ein <EM>Anderes</EM> ist <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! c) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_other
     MODULE PROCEDURE is_phy_quant_other_code_0
     MODULE PROCEDURE is_phy_quant_other_code_1
     MODULE PROCEDURE is_phy_quant_other_descr_0
     MODULE PROCEDURE is_phy_quant_other_descr_1
  END INTERFACE
  !! Ermittle ob eine physikalische Gr&ouml;&szlig;e <EM>zeitunabh&auml;ngig</EM> ist <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_static
     MODULE PROCEDURE is_phy_quant_notim_code_0
     MODULE PROCEDURE is_phy_quant_notim_code_1
     MODULE PROCEDURE is_phy_quant_notim_descr_0
     MODULE PROCEDURE is_phy_quant_notim_descr_1
  END INTERFACE
  !! Ermittle ob eine physikalische Gr&ouml;&szlig;e <EM>synoptisch</EM> ist <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_synoptic
     MODULE PROCEDURE is_phy_quant_synop_code_0
     MODULE PROCEDURE is_phy_quant_synop_code_1
     MODULE PROCEDURE is_phy_quant_synop_descr_0
     MODULE PROCEDURE is_phy_quant_synop_descr_1
  END INTERFACE
  !! Ermittle ob eine physikalische Gr&ouml;&szlig;e <EM>periodenbezogen</EM> ist <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_period
     MODULE PROCEDURE is_phy_quant_perio_code_0
     MODULE PROCEDURE is_phy_quant_perio_code_1
     MODULE PROCEDURE is_phy_quant_perio_descr_0
     MODULE PROCEDURE is_phy_quant_perio_descr_1
  END INTERFACE
  !! Ermittle, ob eine physikalische Gr&ouml;szlig;e eine Flu&szlig;gr&ouml;&szlig;e ist
  !! oder nicht <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE is_phy_quant_flux
     MODULE PROCEDURE is_phy_quant_flux_code_0
     MODULE PROCEDURE is_phy_quant_flux_code_1
     MODULE PROCEDURE is_phy_quant_flux_descr_0
     MODULE PROCEDURE is_phy_quant_flux_descr_1
  END INTERFACE
  !
  ! ---------------------------------------------------------------------
  ! HAS
  ! ---------------------------------------------------------------------
  !! Ermittle ob eine physikalische Gr&ouml;&szlig;e eine Referenzgr&ouml;&szlig;e erfordert <BR>
  !! a) f&uuml;r die Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! b) f&uuml;r Code-Kennungen physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! c) f&uuml;r die Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! d) f&uuml;r Beschreibungen physikalischer Gr&ouml;&szlig;en (Vektor)
  INTERFACE has_phy_quant_ref_quant
     MODULE PROCEDURE has_phy_quant_ref_quant_code_0
     MODULE PROCEDURE has_phy_quant_ref_quant_code_1
     MODULE PROCEDURE has_phy_quant_ref_quant_descr_0
     MODULE PROCEDURE has_phy_quant_ref_quant_descr_1
  END INTERFACE
  !
  ! ---------------------------------------------------------------------
  ! UMRECHNUNGEN
  ! ---------------------------------------------------------------------
  !! wandle vorgegebene originale Daten in SI-Einheiten um <BR>
  !! ein eventueller Schwellenwert wird hierbei ber&uuml;cksichtigt <BR>
  !! A) die physikalische Gr&ouml;&szlig;e wird als Code-Kennung vorgegeben: <BR>
  !!    a) ganzzahliger Skalar <BR>
  !!    b) ganzzahliger 1D-Vektor <BR>
  !!    c) ganzzahliger 2D-Vektor <BR>
  !!    d) ganzzahliger 3D-Vektor <BR>
  !!    e) reellwertiger Skalar <BR>
  !!    f) reellwertiger 1D-Vektor <BR>
  !!    g) reellwertiger 2D-Vektor <BR>
  !!    h) reellwertiger 3D-Vektor <BR>
  !!    i) doppeltgenauer Skalar <BR>
  !!    j) doppeltgenauer 1D-Vektor <BR>
  !!    k) doppeltgenauer 2D-Vektor <BR>
  !!    l) doppeltgenauer 3D-Vektor <BR>
  !! B) die physikalische Gr&ouml;&szlig;e wird als Beschreibung vorgegeben: <BR>
  !!    a) ganzzahliger Skalar <BR>
  !!    b) ganzzahliger 1D-Vektor <BR>
  !!    c) ganzzahliger 2D-Vektor <BR>
  !!    d) ganzzahliger 3D-Vektor <BR>
  !!    e) reellwertiger Skalar <BR>
  !!    f) reellwertiger 1D-Vektor <BR>
  !!    g) reellwertiger 2D-Vektor <BR>
  !!    h) reellwertiger 3D-Vektor <BR>
  !!    i) doppeltgenauer Skalar <BR>
  !!    j) doppeltgenauer 1D-Vektor <BR>
  !!    k) doppeltgenauer 2D-Vektor <BR>
  !!    l) doppeltgenauer 3D-Vektor <BR>
  INTERFACE get_phy_conv_orig_to_si_data
     MODULE PROCEDURE convert_or_si_in_code_0
     MODULE PROCEDURE convert_or_si_in_code_1
     MODULE PROCEDURE convert_or_si_in_code_2
     MODULE PROCEDURE convert_or_si_in_code_3
     MODULE PROCEDURE convert_or_si_re_code_0
     MODULE PROCEDURE convert_or_si_re_code_1
     MODULE PROCEDURE convert_or_si_re_code_2
     MODULE PROCEDURE convert_or_si_re_code_3
     MODULE PROCEDURE convert_or_si_dp_code_0
     MODULE PROCEDURE convert_or_si_dp_code_1
     MODULE PROCEDURE convert_or_si_dp_code_2
     MODULE PROCEDURE convert_or_si_dp_code_3
     MODULE PROCEDURE convert_or_si_in_descr_0
     MODULE PROCEDURE convert_or_si_in_descr_1
     MODULE PROCEDURE convert_or_si_in_descr_2
     MODULE PROCEDURE convert_or_si_in_descr_3
     MODULE PROCEDURE convert_or_si_re_descr_0
     MODULE PROCEDURE convert_or_si_re_descr_1
     MODULE PROCEDURE convert_or_si_re_descr_2
     MODULE PROCEDURE convert_or_si_re_descr_3
     MODULE PROCEDURE convert_or_si_dp_descr_0
     MODULE PROCEDURE convert_or_si_dp_descr_1
     MODULE PROCEDURE convert_or_si_dp_descr_2
     MODULE PROCEDURE convert_or_si_dp_descr_3
  END INTERFACE
  !! wandle vorgegebene originale Daten zum Plotten um <BR>
  !! ein eventueller Schwellenwert wird hierbei ber&uuml;cksichtigt <BR>
  !! A) die physikalische Gr&ouml;&szlig;e wird als Code-Kennung vorgegeben: <BR>
  !!    a) ganzzahliger Skalar <BR>
  !!    b) ganzzahliger 1D-Vektor <BR>
  !!    c) ganzzahliger 2D-Vektor <BR>
  !!    d) ganzzahliger 3D-Vektor <BR>
  !!    e) reellwertiger Skalar <BR>
  !!    f) reellwertiger 1D-Vektor <BR>
  !!    g) reellwertiger 2D-Vektor <BR>
  !!    h) reellwertiger 3D-Vektor <BR>
  !!    i) doppeltgenauer Skalar <BR>
  !!    j) doppeltgenauer 1D-Vektor <BR>
  !!    k) doppeltgenauer 2D-Vektor <BR>
  !!    l) doppeltgenauer 3D-Vektor <BR>
  !! B) die physikalische Gr&ouml;&szlig;e wird als Beschreibung vorgegeben: <BR>
  !!    a) ganzzahliger Skalar <BR>
  !!    b) ganzzahliger 1D-Vektor <BR>
  !!    c) ganzzahliger 2D-Vektor <BR>
  !!    d) ganzzahliger 3D-Vektor <BR>
  !!    e) reellwertiger Skalar <BR>
  !!    f) reellwertiger 1D-Vektor <BR>
  !!    g) reellwertiger 2D-Vektor <BR>
  !!    h) reellwertiger 3D-Vektor <BR>
  !!    i) doppeltgenauer Skalar <BR>
  !!    j) doppeltgenauer 1D-Vektor <BR>
  !!    k) doppeltgenauer 2D-Vektor <BR>
  !!    l) doppeltgenauer 3D-Vektor <BR>
  INTERFACE get_phy_conv_orig_to_plot_data
     MODULE PROCEDURE convert_or_pl_in_code_0
     MODULE PROCEDURE convert_or_pl_in_code_1
     MODULE PROCEDURE convert_or_pl_in_code_2
     MODULE PROCEDURE convert_or_pl_in_code_3
     MODULE PROCEDURE convert_or_pl_re_code_0
     MODULE PROCEDURE convert_or_pl_re_code_1
     MODULE PROCEDURE convert_or_pl_re_code_2
     MODULE PROCEDURE convert_or_pl_re_code_3
     MODULE PROCEDURE convert_or_pl_dp_code_0
     MODULE PROCEDURE convert_or_pl_dp_code_1
     MODULE PROCEDURE convert_or_pl_dp_code_2
     MODULE PROCEDURE convert_or_pl_dp_code_3
     MODULE PROCEDURE convert_or_pl_in_descr_0
     MODULE PROCEDURE convert_or_pl_in_descr_1
     MODULE PROCEDURE convert_or_pl_in_descr_2
     MODULE PROCEDURE convert_or_pl_in_descr_3
     MODULE PROCEDURE convert_or_pl_re_descr_0
     MODULE PROCEDURE convert_or_pl_re_descr_1
     MODULE PROCEDURE convert_or_pl_re_descr_2
     MODULE PROCEDURE convert_or_pl_re_descr_3
     MODULE PROCEDURE convert_or_pl_dp_descr_0
     MODULE PROCEDURE convert_or_pl_dp_descr_1
     MODULE PROCEDURE convert_or_pl_dp_descr_2
     MODULE PROCEDURE convert_or_pl_dp_descr_3
  END INTERFACE
  !! wandle vorgegebene Daten in SI-Einheiten in originale Daten um <BR>
  !! ein eventueller Schwellenwert wird hierbei ber&uuml;cksichtigt <BR>
  !! A) die physikalische Gr&ouml;&szlig;e wird als Code-Kennung vorgegeben: <BR>
  !!    a) ganzzahliger Skalar <BR>
  !!    b) ganzzahliger 1D-Vektor <BR>
  !!    c) ganzzahliger 2D-Vektor <BR>
  !!    d) ganzzahliger 3D-Vektor <BR>
  !!    e) reellwertiger Skalar <BR>
  !!    f) reellwertiger 1D-Vektor <BR>
  !!    g) reellwertiger 2D-Vektor <BR>
  !!    h) reellwertiger 3D-Vektor <BR>
  !!    i) doppeltgenauer Skalar <BR>
  !!    j) doppeltgenauer 1D-Vektor <BR>
  !!    k) doppeltgenauer 2D-Vektor <BR>
  !!    l) doppeltgenauer 3D-Vektor <BR>
  !! B) die physikalische Gr&ouml;&szlig;e wird als Beschreibung vorgegeben: <BR>
  !!    a) ganzzahliger Skalar <BR>
  !!    b) ganzzahliger 1D-Vektor <BR>
  !!    c) ganzzahliger 2D-Vektor <BR>
  !!    d) ganzzahliger 3D-Vektor <BR>
  !!    e) reellwertiger Skalar <BR>
  !!    f) reellwertiger 1D-Vektor <BR>
  !!    g) reellwertiger 2D-Vektor <BR>
  !!    h) reellwertiger 3D-Vektor <BR>
  !!    i) doppeltgenauer Skalar <BR>
  !!    j) doppeltgenauer 1D-Vektor <BR>
  !!    k) doppeltgenauer 2D-Vektor <BR>
  !!    l) doppeltgenauer 3D-Vektor <BR>
  INTERFACE get_phy_conv_si_to_orig_data
     MODULE PROCEDURE convert_si_or_in_code_0
     MODULE PROCEDURE convert_si_or_in_code_1
     MODULE PROCEDURE convert_si_or_in_code_2
     MODULE PROCEDURE convert_si_or_in_code_3
     MODULE PROCEDURE convert_si_or_re_code_0
     MODULE PROCEDURE convert_si_or_re_code_1
     MODULE PROCEDURE convert_si_or_re_code_2
     MODULE PROCEDURE convert_si_or_re_code_3
     MODULE PROCEDURE convert_si_or_dp_code_0
     MODULE PROCEDURE convert_si_or_dp_code_1
     MODULE PROCEDURE convert_si_or_dp_code_2
     MODULE PROCEDURE convert_si_or_dp_code_3
     MODULE PROCEDURE convert_si_or_in_descr_0
     MODULE PROCEDURE convert_si_or_in_descr_1
     MODULE PROCEDURE convert_si_or_in_descr_2
     MODULE PROCEDURE convert_si_or_in_descr_3
     MODULE PROCEDURE convert_si_or_re_descr_0
     MODULE PROCEDURE convert_si_or_re_descr_1
     MODULE PROCEDURE convert_si_or_re_descr_2
     MODULE PROCEDURE convert_si_or_re_descr_3
     MODULE PROCEDURE convert_si_or_dp_descr_0
     MODULE PROCEDURE convert_si_or_dp_descr_1
     MODULE PROCEDURE convert_si_or_dp_descr_2
     MODULE PROCEDURE convert_si_or_dp_descr_3
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren
  !
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !
  ! Hinweis: Operatoren wurden fuer vier verschieden Parameter-
  !          Konstellationen formuliert. Ggf. nicht sinnvolle
  !          Konstellationen entfernen oder weitere sinnvolle
  !          hinzufuegen.
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_phy             ! 
  PUBLIC :: clear_phy            ! 
  PUBLIC :: setup_phy_prn_lun    ! 
  PUBLIC :: setup_phy_trc_lun    ! 
  PUBLIC :: PRINT_phy_static     ! 
  PUBLIC :: PRINT_phy_all_errors ! 
  PUBLIC :: ok_phy               ! 
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: setup_phy_language 
  PUBLIC :: setup_phy_threshold
  !
  PUBLIC :: get_phy_language 
  PUBLIC :: get_phy_quant_language 
  PUBLIC :: get_phy_quant_code
  PUBLIC :: get_phy_quant_max_code
  PUBLIC :: get_phy_quant_id
  PUBLIC :: get_phy_quant_descr
  PUBLIC :: get_phy_quant_ref_code
  PUBLIC :: get_phy_quant_ref_id
  PUBLIC :: get_phy_quant_ref_descr
  PUBLIC :: get_phy_quant_type_indicator
  PUBLIC :: get_phy_quant_time_indicator
  PUBLIC :: get_phy_quant_plot_indicator
  PUBLIC :: get_phy_quant_plot_text
  PUBLIC :: get_phy_quant_plot_factor
  PUBLIC :: get_phy_quant_omiele_nof
  PUBLIC :: get_phy_quant_omigen_nof
  PUBLIC :: get_phy_quant_omiele
  PUBLIC :: get_phy_quant_omigen
  PUBLIC :: is_phy_quant_omiele_3d 
  !
  PUBLIC :: get_phy_unit_id
  PUBLIC :: get_phy_unit_descr
  PUBLIC :: get_phy_unit_max_si_base
  PUBLIC :: get_phy_unit_si_base_name
  PUBLIC :: get_phy_unit_si_base_unit
  PUBLIC :: get_phy_unit_si_base_symb
  PUBLIC :: get_phy_unit_si_base_power
  PUBLIC :: get_phy_unit_si_base_convf
  PUBLIC :: get_phy_unit_si_base_offset
  !
  PUBLIC :: get_phy_class_code
  PUBLIC :: get_phy_class_id
  PUBLIC :: get_phy_class_descr
  !
  PUBLIC :: get_phy_conv_orig_to_si_data
  PUBLIC :: get_phy_conv_si_to_orig_data
  PUBLIC :: get_phy_conv_orig_to_plot_data
  !
  PUBLIC :: get_phy_quant_nof_standard_name
  PUBLIC :: get_phy_quant_standard_name
  PUBLIC :: get_phy_quant_standard_dim
  PUBLIC :: get_phy_standard_name_quant
  PUBLIC :: get_phy_standard_name_dim
  !
  PUBLIC :: is_phy_quant_valid
  PUBLIC :: is_phy_quant_scalar
  PUBLIC :: is_phy_quant_vector
  PUBLIC :: is_phy_quant_tensor
  PUBLIC :: is_phy_quant_other
  PUBLIC :: is_phy_quant_static
  PUBLIC :: is_phy_quant_synoptic
  PUBLIC :: is_phy_quant_period
  PUBLIC :: is_phy_quant_flux
  !
  PUBLIC :: has_phy_quant_ref_quant
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] Konstantwerte (Parameter)
  ! --- Allgemein
  !! Name des Moduls
  CHARACTER (LEN=05), PARAMETER :: c_modname      = 'b_phy' ! 
  !! "undefined"-String
  CHARACTER (LEN=09), PARAMETER :: c_undefined    = 'undefined' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .FALSE.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl einstellbarer Sprachen
  INTEGER           , PARAMETER :: c_max_language = 2                ! [Deutsch,Englisch]
  !! Default-Language
  INTEGER           , PARAMETER :: c_def_language = 1                ! [Deutsch]
  ! -- Informationen ueber SI-Basiseinheiten
  !! max. Anzahl der Zeichen im Namen einer SI-Gr&ouml;&szlig;e
  INTEGER           , PARAMETER :: c_len_si_base_name=19 ! 
  !! max. Anzahl der Zeichen in der Einheit einer SI-Gr&ouml;&szlig;e
  INTEGER           , PARAMETER :: c_len_si_base_unit=9  ! 
  !! max. Anzahl der Zeichen in dem Symbol einer SI-Gr&ouml;&szlig;e
  INTEGER           , PARAMETER :: c_len_si_base_symb=3  ! 
  !! max. Anzahl verschiedener SI-Basiseinheiten
  INTEGER           , PARAMETER :: c_max_si_base=8 ! 
  !! deutsche/englische Bezeichnungen der SI-Basiseinheiten
  CHARACTER (LEN=c_len_si_base_name) , PARAMETER :: c_si_base_name(c_max_si_base,c_max_language) = & ! 
       RESHAPE( (/ 'Laenge             ', 'Masse              ', 'Zeit               ', 'elektr. Strom      ',    & ! 
                   'Temperatur         ', 'Stoffmenge         ', 'Lichtintensitaet   ', 'Waehrung           ',    & !
                   'length             ', 'mass               ', 'time               ', 'electric current   ',    & ! 
                   'temperature        ', 'amount of substance', 'luminous intensity ', 'currency           ' /), & ! 
                SHAPE=(/c_max_si_base,c_max_language/) )
  !! deutsche/englische SI-Basiseinheiten
  CHARACTER (LEN=c_len_si_base_unit) , PARAMETER :: c_si_base_unit(c_max_si_base,c_max_language) = & ! 
       RESHAPE( (/ 'Meter    ', 'Kilogramm', 'Sekunde  ', 'Ampere   ',    & ! 
                   'Kelvin   ', 'Mol      ', 'Candela  ', 'Euro     ',    & !
                   'meter    ', 'kilogram ', 'second   ', 'ampere   ',    & ! 
                   'kelvin   ', 'mole     ', 'candela  ', 'euro     ' /), & ! 
                SHAPE=(/c_max_si_base,c_max_language/) )
  !! SI-Basissymbole
  CHARACTER (LEN=c_len_si_base_symb) , PARAMETER :: c_si_base_symb(c_max_si_base) = & ! 
       (/ 'm  ', 'kg ', 's  ', 'A  ', 'K  ', 'mol', 'cd ', 'E  ' /) 
  ! -- Informationen ueber physikalische Einheiten
  !! max. Anzahl der Zeichen in einer physikalischen Einheit
  INTEGER           , PARAMETER :: c_len_phy_unit_id=10     ! 
  !! max. Anzahl der Zeichen in der ausf&uuml;hrlichen Beschreibung einer physikalischen Einheit
  INTEGER           , PARAMETER :: c_len_phy_unit_descr=80  ! 
  !! max. Anzahl der Zeichen in einer Klasse
  INTEGER           , PARAMETER :: c_len_phy_class_id=7     ! 
  !! max. Anzahl der Zeichen in der ausf&uuml;hrlichen Beschreibung einer Klasse
  INTEGER           , PARAMETER :: c_len_phy_class_descr=80 ! 
  !! max. Anzahl der Zeichen der Kurzbezeichnung einer physikalischen Gr&ouml;&szlig;e
  INTEGER           , PARAMETER :: c_len_phy_id=8           ! 
  !! max. Anzahl der Zeichen in der ausf&uuml;hrlichen Beschreibung einer physikalischen Gr&ouml;&szlig;e
  INTEGER           , PARAMETER :: c_len_phy_descr=40      ! 
  !! max. Anzahl unterschiedlicher Typ-Bezeichner f&uuml;r physikalische Gr&ouml;&szlig;en
  INTEGER           , PARAMETER :: c_max_phy_quant_typ=3  ! 
  !! max. Anzahl unterschiedlicher Zeit-Bezeichner f&uuml;r physikalische Gr&ouml;&szlig;en
  INTEGER           , PARAMETER :: c_max_phy_quant_zta=3  ! 
  !! max. Anzahl unterschiedlicher Bezeichner zum Plotten von Zeitangaben
  INTEGER           , PARAMETER :: c_max_phy_quant_mzr=7  ! 
  !! max. Anzahl der Zeichen in Typ-Bezeichnern
  INTEGER           , PARAMETER :: c_len_phy_quant_typ=6  ! 
  !! max. Anzahl der Zeichen in Zeit-Bezeichnern
  INTEGER           , PARAMETER :: c_len_phy_quant_zta=20 ! 
  !! deutsche/englische Typ-Bezeichner   
  CHARACTER (LEN=c_len_phy_quant_typ) , PARAMETER :: c_phy_quant_typ(c_max_phy_quant_typ,c_max_language) = & ! 
       RESHAPE( (/ 'Skalar', 'Vektor', 'Tensor', 'scalar', 'vector', 'tensor' /), &
       SHAPE=(/c_max_phy_quant_typ,c_max_language/) )
  !! deutsche/englische Zeitabh&auml;ngigkeits-Bezeichner   
  CHARACTER (LEN=c_len_phy_quant_zta) , PARAMETER :: c_phy_quant_zta(c_max_phy_quant_typ,c_max_language) = & ! 
       RESHAPE( (/ 'zeitunabhaengig     ', 'synoptisch          ', 'periodenbezogen     ',     & !
                   'time-independent    ', 'synoptic            ', 'period-related      '  /), & !
                   SHAPE=(/c_max_phy_quant_typ,c_max_language/) )
  ! --- Konfigurationsdateien ---------------------------------------------------------
  !! maximale Anzahl der Konfigurationsdateien
  INTEGER                         , PARAMETER :: c_max_cfg_files=5  ! 
  !! maximale Anzahl der Zeichen in einem Namen der Konfiguraionsdateien
  INTEGER                         , PARAMETER :: c_len_cfg_files=19 ! 
  !! Namen der Konfigurationsdateien
  CHARACTER (LEN=c_len_cfg_files) , PARAMETER :: c_cfg_files(c_max_cfg_files)= & ! 
  !LEO renamed (/ 'phydef.cfg.de.dat  ', 'phydef.cfg.en.dat  ', 'phydef.cfg.rest.dat', 'phydef-cf.cfg.dat  ', 'phydef.cfg.si.dat  ' /) 
       (/ 'DSCFG2             ', 'DSCFG3             ','DSCFG4             ','DSCFG1             ','DSCFG5             ' /) 

  !
  ! --- Plot-Indikator-Texte ---------------------------------------------------------
  !! max. Anzahl der Plot-Indikator-Texte
  INTEGER                         , PARAMETER :: c_max_plot_text=7     ! 
  !! max. L&auml;nge der Plot-Indikator-Texte
  INTEGER                         , PARAMETER :: c_len_plot_text=26 ! 
  !! deutsche/englische Bezeichnungen der Plot-Indikator-Texte 
  CHARACTER (LEN=c_len_plot_text) , PARAMETER :: c_plot_text(c_max_plot_text,c_max_language)= & ! 
       RESHAPE( (/ 'Zeitpunkt:                ', 'Zeitraum:                 ', &
                   'Tideanfang:               ', 'Tidehochwasserzeit:       ', &
                   'Halbtidebeginn:           ', 'Kenterungszeit:           ', &
                   'Tideniedrigwasserzeit:    ',                               &
                   'time:                     ', 'period:                   ', &
                   'beginning of tide:        ', 'high water time:          ', &
                   'beginning of tidal phase: ', 'slack water time:         ', &
                   'low water time:           ' /), SHAPE=(/c_max_plot_text,c_max_language/) )
  !
  ! --- OpenMI-spezifische Texte ------------------------------------------------------
  !! max. Anzahl der Gitter-ElementSet bezogenen Texte
  INTEGER                         , PARAMETER :: c_max_omiele=10 ! 
  !! max. L&auml;nge der Gitter-ElementSet bezogenen Texte
  INTEGER                         , PARAMETER :: c_len_omiele=10  ! 
  !! Gitter-ElementSet bezogene Texte
  CHARACTER (LEN=c_len_omiele)    , PARAMETER :: c_omiele_text(c_max_omiele)=  & ! 
       (/ 'id        ', 'xyPt      ', 'xyLn      ', 'xyPl      ', 'xyPg      ', &  
          'xyzPt     ', 'xyzLn     ', 'xyzPl     ', 'xyzPg     ', 'xyzPh     ' /)
  !! max. Anzahl der Daten-ElementSet bezogenen Texte
  INTEGER                         , PARAMETER :: c_max_omigen=14 ! 
  !! max. L&auml;nge der Daten-ElementSet bezogenen Texte
  INTEGER                         , PARAMETER :: c_len_omigen=19 ! 
  !! Daten-ElementSet bezogene Texte
  CHARACTER (LEN=c_len_omigen)    , PARAMETER :: c_omigen_text(c_max_omigen)= (/ &     ! 
       'AllVertices        ', & ! [    1] 
       'AllLocations       ', & ! [    2]
       'Location           ', & ! [    4]
       'HorizontalGrid     ', & ! [    8]
       'AllBoundaries      ', & ! [   16]
       'AllClosedBoundaries', & ! [   32]
       'AllOpenBoundaries  ', & ! [   64] 
       'Boundary           ', & ! [  128]
       'ClosedBoundary     ', & ! [  256]
       'OpenBoundary       ', & ! [  512]
       'AllCrossProfiles   ', & ! [ 1024]
       'AllLongProfiles    ', & ! [ 2048]      
       'CrossProfile       ', & ! [ 4096]
       'LongProfile        ' /) ! [ 8192]
  !
  ! [D.2] lokale Typdefinitionen
  !
  !! Zusammenfassen aller Bezeichnungen der <EM>Einheiten</EM>: <BR>
  !! <EM>code</EM>   : Code-Kennung <BR>
  !! <EM>id</EM>     : physikalische Einheit (Deutsch/Englisch) <BR>
  !! <EM>descr</EM>  : ausf&uuml;hrliche Beschreibung der physikalischen Einheit (Deutsch/Englisch) <BR>
  !! <EM>base</EM>   : Exponenten der physikalischen Einheit in SI-Einheiten <BR>
  !! <EM>convf</EM>  : Umrechnungsfaktor in SI-Einheiten <BR>
  !! <EM>offset</EM> : Offset der physikalischen Gr&ouml;&szlig;e gegen Null (in SI-Einheiten)
  TYPE , PRIVATE :: t_phy_unit
     PRIVATE
     INTEGER                              :: code                  ! 
     CHARACTER (LEN=c_len_phy_unit_id)    :: id(c_max_language)    ! 
     CHARACTER (LEN=c_len_phy_unit_descr) :: descr(c_max_language) ! 
     REAL (KIND=Double)                   :: base(c_max_si_base)   ! 
     REAL (KIND=DOUBLE)                   :: convf                 ! 
     REAL (KIND=DOUBLE)                   :: offset                ! 
  END TYPE t_phy_unit
  !
  !! Zusammenfassen aller Bezeichnungen der <EM>Klassen</EM>: <BR>
  !! <EM>code</EM>   : Code-Kennung <BR>
  !! <EM>id</EM>     : Kurzbezeichnung der Klassen (Deutsch/Englisch) <BR>
  !! <EM>descr</EM>  : ausf&uuml;hrliche Beschreibung der Klassen (Deutsch/Englisch) <BR>
  TYPE , PRIVATE :: t_phy_class
     PRIVATE
     INTEGER                               :: code                  ! 
     CHARACTER (LEN=c_len_phy_class_id)    :: id(c_max_language)    ! 
     CHARACTER (LEN=c_len_phy_class_descr) :: descr(c_max_language) ! 
  END TYPE t_phy_class
  !
  !! Zusammenfassen aller <EM>physikalischen Gr&ouml;&szlig;en</EM>: <BR>
  !! <EM>code</EM>       : Code-Kennung <BR>
  !! <EM>id</EM>         : Kurzbezeichnung der Gr&ouml;&szlig;e (Deutsch/Englisch) <BR>
  !! <EM>descr</EM>      : ausf&uuml;hrliche Beschreibung der Gr&ouml;&szlig;e (Deutsch/Englisch) <BR>
  !! <EM>unit_code</EM>  : Code-Kennung der dazugeh&ouml;renden physikalischen Einheit <BR>
  !! <EM>class_code</EM> : Code-Kennung der dazugeh&ouml;renden Klasse <BR>
  !! <EM>ref_code</EM>   : Code-Kennung physikalischen Referenzgr&ouml;&szlig;e oder -1 <BR>
  !! <EM>typ</EM>        : Typ-Kennung (Skalar,Vektor,Tensor) = (-1,1,2,3) <BR>
  !! <EM>zta</EM>        : Typ-Kennung (ohne,synoptisch,Zeitraum) = (0,1,2) <BR>
  !! <EM>mzr</EM>        : Indikator f&uuml;r das Plotten von Zeitangaben <BR>
  !! <EM>mul</EM>        : Multiplikator f&uuml;r das Plotten der Daten <BR>
  !! <EM>ele</EM>        : summarische Code-Kennung f&uuml;r Gitter-ElementSets, die beim Erzeugen von 
  !!                       Daten-ElementSets ber&uuml;cksichtigt werden sollen <BR>
  !! <EM>gen</EM>        : summarische f&uuml;r Aspekte zur Lage der zu erzeugenden Daten-ElementSets <BR>
  !! <EM>flx</EM>        : Indikatorvariable zur Kennzeichnung von F;ussgr&ouml;&szlig;en 
  TYPE , PRIVATE :: t_phy_quant
     PRIVATE
     INTEGER                         :: code                  ! 
     CHARACTER (LEN=c_len_phy_id)    :: id(c_max_language)    ! 
     CHARACTER (LEN=c_len_phy_descr) :: descr(c_max_language) !   
     INTEGER                         :: unit_code             ! 
     INTEGER                         :: class_code            ! 
     INTEGER                         :: ref_code              ! 
     INTEGER                         :: typ                   ! 
     INTEGER                         :: zta                   ! 
     INTEGER                         :: mzr                   ! 
     REAL (KIND=DOUBLE)              :: mul                   ! 
     INTEGER                         :: ele                   ! 
     INTEGER                         :: gen                   ! 
     LOGICAL                         :: flx                   ! 
  END TYPE t_phy_quant
  !
  !! Zusammenfassen des Zusammenhangs mit den <EM>CF-Konventionen</EM>: <BR>
  !! <EM>code</EM>          : Code-Kennung <BR>
  !! <EM>idim</EM>          : Zeiger auf eine bestimmte Komponente einer vektoriellen Gr&ouml;&szlig;e <BR>
  !! <EM>ivar</EM>          : Zeiger auf eine bestimmte Datenvariation <BR>
  !! <EM>standard_name</EM> : Standard-Name (nach Namenskonvention)
  TYPE , PRIVATE :: t_phy_standard
     PRIVATE
     INTEGER                             :: code          ! 
     INTEGER                             :: idim          ! 
     INTEGER                             :: ivar          ! 
     CHARACTER (LEN=c_len_standard_name) :: standard_name ! 
  END TYPE t_phy_standard
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)! 
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                , SAVE :: initialised = .FALSE.   ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                , SAVE :: prn_op      = c_op      ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                , SAVE :: trc_op      = c_op      ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                , SAVE :: prn_lun     = c_lun     ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                , SAVE :: trc_lun     = c_lun     ! 
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                , SAVE :: n_init      = 0         ! 
  !! aktuelle Spracheinstellung ( 1 = Deutsch, 2 = Englisch )
  INTEGER                , SAVE :: language=c_def_language ! 
  !! Grenzwert f&uuml;r ung&uuml;ltige INTEGER-Daten
  INTEGER                , SAVE :: c_threshold_in=999999999      ! 
  !! Grenzwert f&uuml;r ung&uuml;ltige REAL(Single)-Daten
  REAL (KIND=Single)     , SAVE :: c_threshold_re=1.0E+31        ! 
  !! Grenzwert f&uuml;r ung&uuml;ltige REAL(Double)-Daten
  REAL (KIND=Double)     , SAVE :: c_threshold_dp=1.0E+31_Double ! 
  !
  !! Feld mit der Definition aller m&ouml;glichen physikalischen Einheiten
  TYPE (t_phy_unit)     , ALLOCATABLE, SAVE :: phy_units(:)      ! 
  !! Feld mit der Definition aller m&ouml;glichen Klassen
  TYPE (t_phy_class)    , ALLOCATABLE, SAVE :: phy_classes(:)    ! 
  !! Feld mit der Definition aller physikalischen Gr&ouml;&szlig;en
  TYPE (t_phy_quant)    , ALLOCATABLE, SAVE :: phy_quants(:)     ! 
  !! Feld mit den Informationen zum Zusammenhang der phys. Gr&ouml;&szlig;en mit externen Standardbezeichnungen
  TYPE (t_phy_standard) , ALLOCATABLE, SAVE :: phy_standards(:)   ! 
  !
  ! [D.4] Schnittstellen
  !
  !! Initialisieren einer physikalischen Einheit
  INTERFACE init_phy_unit
     MODULE PROCEDURE init_phy_unit_d
  END INTERFACE
  !! Initialisieren aller zul&auml;ssigen physikalischen Einheiten
  INTERFACE init_phy_all_units
     MODULE PROCEDURE init_phy_all_units_d
  END INTERFACE 
  !! De-Initialisieren aller zul&auml;ssigen physikalischen Einheiten
  INTERFACE clear_phy_all_units
     MODULE PROCEDURE clear_phy_all_units_d
  END INTERFACE 
  !
  !! Initialisieren einer Klasse
  INTERFACE init_phy_class
     MODULE PROCEDURE init_phy_class_d
  END INTERFACE
  !! Initialisieren aller zul&auml;ssigen Klassen
  INTERFACE init_phy_all_classes
     MODULE PROCEDURE init_phy_all_classes_d
  END INTERFACE 
  !! De-Initialisieren aller zul&auml;ssigen Klassen
  INTERFACE clear_phy_all_classes
     MODULE PROCEDURE clear_phy_all_classes_d
  END INTERFACE 
  !
  !! Initialisieren einer physikalischen Gr&ouml;&szlig;e
  INTERFACE init_phy_quant
     MODULE PROCEDURE init_phy_quant_d
  END INTERFACE
  !! Initialisieren aller physikalischen Gr&ouml;&szlig;en
  INTERFACE init_phy_all_quants
     MODULE PROCEDURE init_phy_all_quants_d
  END INTERFACE
  !! De-Initialisieren aller physikalischen Gr&ouml;&szlig;en
  INTERFACE clear_phy_all_quants
     MODULE PROCEDURE clear_phy_all_quants_d
  END INTERFACE 
  !! Initialisieren einer Informationsbeziehung
  INTERFACE init_phy_standard
     MODULE PROCEDURE init_phy_standard_d
  END INTERFACE
  !! Initialisieren aller Informationsbeziehungen zu Standardgr&ouml;&szlig;en
  INTERFACE init_phy_all_standards
     MODULE PROCEDURE init_phy_all_standards_d
  END INTERFACE 
  !! De-Initialisieren aller Informationsbeziehungen zu Standardgr&ouml;&szlig;en
  INTERFACE clear_phy_all_standards
     MODULE PROCEDURE clear_phy_all_standards_d
  END INTERFACE 
  !
  !! Zeiger auf physikalische Klasse (sprachunabh&auml;ngig) ermitteln <BR>
  !! a) bei Vorgabe der Id der physikalischen Klasse <BR>
  !! b) bei Vorgabe der Code-Kennung der physikalischen Klasse <BR>
  INTERFACE get_phy_classes_idx
     MODULE PROCEDURE get_phy_classes_id_idx_d
     MODULE PROCEDURE get_phy_classes_code_idx_d
  END INTERFACE
  !! Zeiger auf physikalische Einheit (sprachunabh&auml;ngig) ermitteln <BR>
  !! a) bei Vorgabe der Id der physikalischen Einheit <BR>
  !! b) bei Vorgabe der Code-Kennung der physikalischen Einheit <BR>
  !! c) bei Vorgabe der bekannten Basisdimensionen einer Gr&ouml;&szlig;e 
  INTERFACE get_phy_units_idx
     MODULE PROCEDURE get_phy_units_id_idx_d
     MODULE PROCEDURE get_phy_units_code_idx_d
     MODULE PROCEDURE get_phy_units_si_base_idx_d
  END INTERFACE
  !! Zeiger auf physikalische Gr&ouml;&szlig;e (sprachunabh&auml;ngig) ermitteln <BR>
  !! a) bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! b) bei Vorgabe der Id/Beschreibung der physikalischen Gr&ouml;&szlig;e
  INTERFACE get_phy_quants_idx
     MODULE PROCEDURE get_phy_quants_code_idx_d
     MODULE PROCEDURE get_phy_quants_descr_idx_d
  END INTERFACE
  !! ermittle die maximal zul&auml;ssige Codierung
  INTERFACE get_max_omi_code
     MODULE PROCEDURE get_max_omi_code_d
  END INTERFACE
  !! ermittle, welche Ziele bei vorgegebener Codierung vorhanden sind
  INTERFACE get_omi_coded
     MODULE PROCEDURE get_omi_coded_d
  END INTERFACE
  !
  !! Zeiger auf Standardbezeichnung ermitteln <BR>
  !! a) bei Vorgabe des Standardnamens <BR>
  !! b) bei Vorgabe des Codes der phys. Gr&ouml;&szlig;e sowie lfd. Nummer des Standardnamens
  INTERFACE get_phy_standards_idx
     MODULE PROCEDURE get_phy_standards_name_idx_d
     MODULE PROCEDURE get_phy_standards_name_cn_idx_d
  END INTERFACE
  !
  !! Ermittle den Namen einer Konfigurationsdatei
  INTERFACE get_phy_cfg_file
     MODULE PROCEDURE get_phy_cfg_file_d
  END INTERFACE
  !! Ermittle die Anzahl der phys. Gr&ouml;&szlig;en auf einer Konfigurationsdatei
  INTERFACE get_phy_cfg_file_nof_quants
     MODULE PROCEDURE get_phy_cfg_file_nof_quants_d
  END INTERFACE
  !! Lesen jeweils einer Zeile aus den verschiedenen Konfigurationsdateien
  INTERFACE read_phy_cfg_files_line
     MODULE PROCEDURE read_phy_cfg_files_line_d
  END INTERFACE 
  !! Lesen jeweils einer Zeile Informationsbeziehungen zu Standardnamen
  INTERFACE read_phy_cfg_standard_line
     MODULE PROCEDURE read_phy_cfg_standard_line_d
  END INTERFACE 
  !! Lesen jeweils einer Zeile Informationsbeziehungen zu SI-Einheiten
  INTERFACE read_phy_cfg_si_line
     MODULE PROCEDURE read_phy_cfg_si_line_d
  END INTERFACE 
  !
  !! Pr&uuml;fe, ob die Komponente "code" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_code
     MODULE PROCEDURE ok_phy_quant_code_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "id" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_id
     MODULE PROCEDURE ok_phy_quant_id_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "descr" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_descr
     MODULE PROCEDURE ok_phy_quant_descr_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "unit_code" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_unit_code
     MODULE PROCEDURE ok_phy_quant_unit_code_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "class_code" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_class_code
     MODULE PROCEDURE ok_phy_quant_class_code_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "ref_code" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_ref_code
     MODULE PROCEDURE ok_phy_quant_ref_code_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "typ" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_typ
     MODULE PROCEDURE ok_phy_quant_typ_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "zta" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_zta
     MODULE PROCEDURE ok_phy_quant_zta_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "mzr" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_mzr
     MODULE PROCEDURE ok_phy_quant_mzr_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "mul" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_mul
     MODULE PROCEDURE ok_phy_quant_mul_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "ele" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_ele
     MODULE PROCEDURE ok_phy_quant_ele_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "gen" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_gen
     MODULE PROCEDURE ok_phy_quant_gen_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "flx" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_quant_flx
     MODULE PROCEDURE ok_phy_quant_flx_0
  END INTERFACE
  !
  !! Pr&uuml;fe, ob die Komponente "code" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_standard_code
     MODULE PROCEDURE ok_phy_standard_code_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "idim" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_standard_idim
     MODULE PROCEDURE ok_phy_standard_idim_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "ivar" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_standard_ivar
     MODULE PROCEDURE ok_phy_standard_ivar_0
  END INTERFACE
  !! Pr&uuml;fe, ob die Komponente "name" o.k. ist <BR>
  !! a) f&uuml;r ein skalares Datenobjekt 
  INTERFACE ok_phy_standard_name
     MODULE PROCEDURE ok_phy_standard_name_0
  END INTERFACE
  !
  !! Drucken des Inhalts einer physikalischen Gr&ouml;&szlig;e "t_phy_quant" <BR>
  !! a) f&uuml;r ein skalares Datenonbjekt
  INTERFACE print_phy_quant
     MODULE PROCEDURE print_phy_quant_0
  END INTERFACE
  !! Drucken des Inhalts einer Informationsbeziehung StandardName "t_phy_standards" <BR>
  !! a) f&uuml;r ein skalares Datenonbjekt
  INTERFACE print_phy_standard
     MODULE PROCEDURE print_phy_standard_0
  END INTERFACE
  ! [D.5] Assignments
  !
  ! [D.6] Operatoren
  !
CONTAINS
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+                                                                     +
  !+           PPPPPP   UUU  UUU  BBBBBB   LLL    III   CCCCC            +
  !+           PPP  PP  UUU  UUU  BBB  BB  LLL    III  CCC  CC           +
  !+           PPP  PP  UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPPPPP   UUU  UUU  BBBBBB   LLL    III  CCC               +
  !+           PPP      UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPP      UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPP       UUUUUU   BBB  BB  LLL    III  CCC  CC           +
  !+           PPP        UUUU    BBBBBB   LLLLLL III   CCCCC            +
  !+                                                                     +
  !+                                                                     +
  !+   MM     MM  EEEEEEE TTTTTTT  HHH  HH   OOOOO   DDDDDD    SSSSS     +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS   SS    +
  !+   MMMM MMMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS         +
  !+   MMM M MMM  EEEEEE    TTT    HHHHHHH  OOO  OO  DDD  DD   SSSSSS    +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SSS  SSS   +
  !+   MMM   MMM  EEEEEEE   TTT    HHH  HH   OOOOO   DDDDDD    SSSSSS    +
  !+                                                                     +
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Globale Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_phy_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='init_phy_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_phy" version 3.3 of 03/26/07                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_cmds ( )
       IF ( no_error( ) ) CALL init_file ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .TRUE.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_phy_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] Initialisieren 
       IF ( no_error( ) ) CALL init_phy_all_units     ( )
       IF ( no_error( ) ) CALL init_phy_all_classes   ( )
       IF ( no_error( ) ) CALL init_phy_all_quants    ( )
       IF ( no_error( ) ) CALL init_phy_all_standards ( )
       ! [1.99] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .TRUE., .FALSE., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_phy_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_phy_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='clear_phy_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren 
       IF ( no_error( ) ) CALL clear_phy_all_standards ( ) 
       IF ( no_error( ) ) CALL clear_phy_all_quants    ( ) 
       IF ( no_error( ) ) CALL clear_phy_all_classes   ( ) 
       IF ( no_error( ) ) CALL clear_phy_all_units     ( ) 
       ! [1.2] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_phy_all_errors    ( ) 
       ! [1.3] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .FALSE., .TRUE., no_error( ) )
       ! [1.5] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.5.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_file ( )
       IF ( no_error( ) ) CALL clear_cmds ( )
       ! ... ggf. weitere De-Initialisierungen ergaenzen
       ! [1.5.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_phy_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_phy_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_phy_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .TRUE., .FALSE., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_file_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_cmds_prn_lun ( lun )
       ! ... ggf. weitere ergaenzen
    END IF
    !
  END SUBROUTINE setup_phy_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_phy_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_phy_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .TRUE., .FALSE., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_file_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_cmds_trc_lun ( lun )
       ! ... ggf. weitere ergaenzen
    END IF
    !
  END SUBROUTINE setup_phy_trc_lun_d
  !
  !! Setzen des Index f&uuml;r die Spracheinstellung f&uuml;r "b_phy" <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  SUBROUTINE setup_phy_language_d ( var )
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER , INTENT(IN) :: var ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_phy_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       language = MERGE ( var, c_def_language, ( 1 <= var .AND. var <= c_max_language ) )
    END IF
    !
  END SUBROUTINE setup_phy_language_d
  !
  !! Setzen des Grenzwertes zum Kennzeichnen ung&uuml;tiger Daten INTEGER <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE setup_phy_threshold_in_d ( var )
    !! Grenzwert f&uuml;r INTEGER-Daten
    INTEGER , INTENT(IN) :: var ! 
    !
    c_threshold_in = var
    !
  END SUBROUTINE setup_phy_threshold_in_d
  !
  !! Setzen des Grenzwertes zum Kennzeichnen ung&uuml;tiger Daten REAL(Single) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE setup_phy_threshold_re_d ( var )
    !! Grenzwert f&uuml;r REAL(Single)-Daten
    REAL (KIND=Single) , INTENT(IN) :: var ! 
    !
    c_threshold_re = var
    !
  END SUBROUTINE setup_phy_threshold_re_d
  !
  !! Setzen des Grenzwertes zum Kennzeichnen ung&uuml;tiger Daten REAL(Double) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE setup_phy_threshold_dp_d ( var )
    !! Grenzwert f&uuml;r REAL(Double)-Daten
    REAL (KIND=Double) , INTENT(IN) :: var ! 
    !
    c_threshold_dp = var
    !
  END SUBROUTINE setup_phy_threshold_dp_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Informationen zu den physikalischen 
  !! Gr&ouml;&szlig;en o.k. sind oder nicht <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION ok_phy_d ( ) &
       RESULT( ok )
    !! Ergebnis: Testergebnis
    LOGICAL :: ok        ! 
    !! Name der Subroutine
    CHARACTER (LEN=8), PARAMETER :: c_upname='ok_phy_d' 
    ! Hilfsvariablen
    CHARACTER (LEN=13) :: ch       ! 
    LOGICAL            :: l_ok(13) ! 
    INTEGER            :: i        ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       DO i=1,SIZE(phy_quants)
          IF ( phy_quants(i)%descr(2)(1:9) == c_undefined ) CYCLE
          l_ok( 1) = ok_phy_quant_code       ( phy_quants(i) )
          l_ok( 2) = ok_phy_quant_id         ( phy_quants(i) )
          l_ok( 3) = ok_phy_quant_descr      ( phy_quants(i) )
          l_ok( 4) = ok_phy_quant_unit_code  ( phy_quants(i) )
          l_ok( 5) = ok_phy_quant_class_code ( phy_quants(i) )
          l_ok( 6) = ok_phy_quant_ref_code   ( phy_quants(i) )
          l_ok( 7) = ok_phy_quant_typ        ( phy_quants(i) )
          l_ok( 8) = ok_phy_quant_zta        ( phy_quants(i) )
          l_ok( 9) = ok_phy_quant_mzr        ( phy_quants(i) )
          l_ok(10) = ok_phy_quant_mul        ( phy_quants(i) )
          l_ok(11) = ok_phy_quant_ele        ( phy_quants(i) )
          l_ok(12) = ok_phy_quant_gen        ( phy_quants(i) )
          l_ok(13) = ok_phy_quant_flx        ( phy_quants(i) )
          !
          IF ( .NOT. ALL( l_ok(:) ) ) THEN
             !
             CALL setup_error_act ( all_errors(:), 6200, c_upname, c_modname )
             CALL setup_error_act ( '<descr>', phy_quants(i)%descr(1) )
             WRITE(ch(1:10),'(I10)') phy_quants(i)%code
             CALL setup_error_act ( '<code>', ch(1:10) )
             WRITE(ch,'(12L1)') l_ok(1:12)
             CALL setup_error_act ( '<StatusCode>',      ch( 1: 1) )
             CALL setup_error_act ( '<StatusId>',        ch( 2: 2) )
             CALL setup_error_act ( '<StatusDescr>',     ch( 3: 3) )
             CALL setup_error_act ( '<StatusUnitCode>',  ch( 4: 4) )
             CALL setup_error_act ( '<StatusClassCode>', ch( 5: 5) )
             CALL setup_error_act ( '<StatusRefCode>',   ch( 6: 6) )
             CALL setup_error_act ( '<StatusTyp>',       ch( 7: 7) )
             CALL setup_error_act ( '<StatusZta>',       ch( 8: 8) )
             CALL setup_error_act ( '<StatusMzr>',       ch( 9: 9) )
             CALL setup_error_act ( '<StatusMul>',       ch(10:10) )
             CALL setup_error_act ( '<StatusEle>',       ch(11:11) )
             CALL setup_error_act ( '<StatusGen>',       ch(12:12) )
             CALL setup_error_act ( '<StatusFlx>',       ch(13:13) )
             !
             CALL print_phy_quant ( phy_quants(i) )
             !
          END IF
          !
       END DO
       DO i=1,SIZE(phy_standards)
          IF ( phy_standards(i)%standard_name(1:9) == c_undefined ) CYCLE
          l_ok( 1) = ok_phy_standard_code ( phy_standards(i) )
          l_ok( 2) = ok_phy_standard_idim ( phy_standards(i) )
          l_ok( 3) = ok_phy_standard_ivar ( phy_standards(i) )
          l_ok( 4) = ok_phy_standard_name ( phy_standards(i) )
          !
          IF ( .NOT. ALL( l_ok(1:4) ) ) THEN
             !
             CALL setup_error_act ( all_errors(:), 6201, c_upname, c_modname )
             WRITE(ch(1:4),'(4L1)') l_ok(1:4)
             CALL setup_error_act ( '<StatusCode>',         ch( 1: 1) )
             CALL setup_error_act ( '<StatusIdim>',         ch( 2: 2) )
             CALL setup_error_act ( '<StatusIvar>',         ch( 3: 3) )
             CALL setup_error_act ( '<StatusStandardName>', ch( 4: 4) )
             !
             CALL print_phy_standard ( phy_standards(i) )
             !
          END IF
          !
       END DO
       ok = no_error( )
    ELSE
       ok = .FALSE.
    END IF
    !
  END FUNCTION ok_phy_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE PRINT_phy_static_d ( )
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='print_phy_static_d' 
    !! Statusvariable
    INTEGER :: i, j, stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       ! --- allgemeine Daten ---------------------------------------------------
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, language, &
           c_max_language, c_max_si_base, c_threshold_in, c_threshold_re,   &
           c_threshold_dp
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component>', 'statische Variablen' )
       END IF
       IF ( no_error( ) ) CALL PRINT_phy_all_errors_d ( )
       ! --- Einheiten ----------------------------------------------------------
       IF ( ALLOCATED( phy_units ) .AND. no_error( ) ) THEN
          WRITE &
               ( UNIT   = prn_lun, &
                 FMT    =  8100,   &
                 IOSTAT = stat ) SIZE( phy_units ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component>', 'Anzahl der Elemente in "phy_units"' )
          END IF
          DO i=1,SIZE( phy_units )
             IF ( any_error( ) ) EXIT
             WRITE &
                  ( UNIT   = prn_lun, &
                    FMT    = 8101,    &
                    IOSTAT = stat )   &
                    i, phy_units(i)%code, &
                    phy_units(i)%id(1), TRIM(phy_units(i)%descr(1)), &
                    phy_units(i)%id(2), TRIM(phy_units(i)%descr(2)), &
                    phy_units(i)%convf, phy_units(i)%offset
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component>', 'Bezeichnungen "phy_units"' )
             END IF
             IF ( any_error( ) ) EXIT
             WRITE &
                  ( UNIT   = prn_lun, &
                    FMT    = 8102,    &
                    IOSTAT = stat )   &
                    ( c_si_base_symb(j), phy_units(i)%base(j), j=1,c_max_si_base )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component>', 'SI-Basiseinheiten "phy_units"' )
             END IF
          END DO
          WRITE &
               ( UNIT   = prn_lun, &
                 FMT    = 8103,    &
                 IOSTAT = stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component>', 'abschliessende Zeilen "phy_units"' )
          END IF
       END IF
       ! --- Klassen ------------------------------------------------------------
       IF ( ALLOCATED( phy_classes ) .AND. no_error( ) ) THEN
          WRITE &
               ( UNIT   = prn_lun, &
                 FMT    =  8200,   &
                 IOSTAT = stat ) SIZE( phy_classes ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component>', 'Anzahl der Elemente in "phy_classes"' )
          END IF
          DO i=1,SIZE( phy_classes )
             IF ( any_error( ) ) EXIT
             WRITE &
                  ( UNIT   = prn_lun, &
                    FMT    = 8201,    &
                    IOSTAT = stat )   &
                    i, phy_classes(i)%code, &
                    phy_classes(i)%id(1), TRIM(phy_classes(i)%descr(1)), &
                    phy_classes(i)%id(2), TRIM(phy_classes(i)%descr(2))
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component>', 'Bezeichnungen "phy_classes"' )
             END IF
          END DO
          WRITE &
               ( UNIT   = prn_lun, &
                 FMT    = 8203,    &
                 IOSTAT = stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component>', 'abschliessende Zeilen "phy_classes"' )
          END IF
       END IF
       ! --- physikalische Groessen ---------------------------------------------
       IF ( ALLOCATED( phy_quants ) .AND. no_error( ) ) THEN
          WRITE &
               ( UNIT   = prn_lun, &
                 FMT    =  8300,   &
                 IOSTAT = stat ) SIZE( phy_quants ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component>', 'Anzahl der Elemente in "phy_quants"' )
          END IF
          DO i=1,SIZE( phy_quants )
             IF ( any_error( ) ) EXIT
             WRITE &
                  ( UNIT   = prn_lun, &
                    FMT    = 8301,    &
                    IOSTAT = stat )   &
                    i, phy_quants(i)%code, &
                    phy_quants(i)%id(1), TRIM(phy_quants(i)%descr(1)), &
                    phy_quants(i)%id(2), TRIM(phy_quants(i)%descr(2)), &
                    phy_quants(i)%unit_code, phy_quants(i)%class_code, &
                    phy_quants(i)%ref_code, phy_quants(i)%typ, &
                    phy_quants(i)%zta, phy_quants(i)%mzr, phy_quants(i)%mul, &
                    phy_quants(i)%ele, phy_quants(i)%gen, phy_quants(i)%flx
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component>', 'Bezeichnungen "phy_quants"' )
             END IF
          END DO
          WRITE &
               ( UNIT   = prn_lun, &
                 FMT    = 8303,    &
                 IOSTAT = stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component>', 'abschliessende Zeilen "phy_quants"' )
          END IF
       END IF
       ! --- Informationsbeziehungen zu Standardbezeichnungen ----------------------------
       IF ( ALLOCATED( phy_standards ) .AND. no_error( ) ) THEN
          WRITE &
               ( UNIT   = prn_lun, &
                 FMT    =  8400,   &
                 IOSTAT = stat ) SIZE( phy_standards ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component>', 'Anzahl der Elemente in "phy_standards"' )
          END IF
          DO i=1,SIZE( phy_standards )
             IF ( any_error( ) ) EXIT
             WRITE &
                  ( UNIT   = prn_lun, &
                    FMT    = 8401,    &
                    IOSTAT = stat )   &
                    i, phy_standards(i)%code, phy_standards(i)%idim, phy_standards(i)%ivar, &
                    TRIM( phy_standards(i)%standard_name )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component>', 'Bezeichnungen "phy_standards"' )
             END IF
          END DO
          WRITE &
               ( UNIT   = prn_lun, &
                 FMT    = 8403,    &
                 IOSTAT = stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component>', 'abschliessende Zeilen "phy_standards"' )
          END IF
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
          '#------------------------------------------------------------',/ &
          '# aktuelle statische Daten des Moduls b_phy         ',/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
          '#  initialised    = ',L1,/ &
          '#       prn_op    = ',L1,/ &
          '#       trc_op    = ',L1,/ &
          '#      prn_lun    = ',I5,/ &
          '#      trc_lun    = ',I5,/ &
          '#       n_init    = ',I5,/ &
          '#     language    = ',I5,/ &
          '# max_language    = ',I5,/ &
          '# max_si_base     = ',I5,/ &
          '# c_threshold_in  = ',I10,/ &
          '# c_threshold_re  = ',G15.6,/ &
          '# c_threshold_dp  = ',G15.6,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '#------------------------------------------------------------') 
8100 FORMAT( &
          '#------------------------------------------------------------',/ &
          '# Informationen zu den definierten Einheiten ', / &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
          '# Anzahl = ',I5, / &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' )
8101 FORMAT( &
          '# ----------------------------- ',/ &
          '# lfd. Nr = ',I5,', Code = ',I5,/ &
          '# Id      = ',A,', Descr = ',A,/ &
          '# Id      = ',A,', Descr = ',A,/ &
          '# ConvF   = ',G15.7,', Offset  = ',G15.7 )                 
8102 FORMAT( &
          '# SI-Base: /',8(A,' = ',G12.5,'/') )
8103 FORMAT( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '#------------------------------------------------------------' )
8200 FORMAT( &
          '#------------------------------------------------------------',/ &
          '# Informationen zu den definierten Klassen ', / &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
          '# Anzahl = ',I5, / &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' )
8201 FORMAT( &
          '# ----------------------------- ',/ &
          '# lfd. Nr = ',I5,', Code = ',I5,/ &
          '# Id      = ',A,', Descr = ',A,/ &
          '# Id      = ',A,', Descr = ',A )
8203 FORMAT( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '#------------------------------------------------------------' )
8300 FORMAT( &
          '#------------------------------------------------------------',/ &
          '# Informationen zu den definierten physikalischen Groessen ', / &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
          '# Anzahl = ',I5, / &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' )
8301 FORMAT( &
          '# ----------------------------- ',/ &
          '# lfd. Nr = ',I5 ,', Code   = ',I5,/ &
          '# Id      = ',A  ,', Descr  = ',A,/ &
          '# Id      = ',A  ,', Descr  = ',A,/ &
          '# Unit    = ',I5 ,', Class  = ',I5,', RefQu = ',I5,/ &
          '# Typ     = ',I5 ,', ZAbh   = ',I5,', PloIn = ',I5,', PloFa = ',G15.6,/ &
          '# OmiEle  = ',I10,', OmiGen = ',I10,', Flux  = ',L2 )
8303 FORMAT( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '#------------------------------------------------------------' )
8400 FORMAT( &
          '#------------------------------------------------------------',/ &
          '# Informationen zu den definierten Standardbezeichnungen     ', / &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
          '# Anzahl = ',I5, / &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' )
8401 FORMAT( &
          '# ----------------------------- ',/ &
          '# lfd. Nr = ',I5 ,', Code   = ',I5,/ &
          '# Idim    = ',I5 ,', Ivar   = ',I5,/ &
          '# Name    = ',A )
8403 FORMAT( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '#------------------------------------------------------------' )
    !
  END SUBROUTINE PRINT_phy_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE PRINT_phy_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_phy_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL PRINT_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE PRINT_phy_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! Holen des Index f&uuml;r die Spracheinstellung f&uuml;r "b_phy" <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  FUNCTION get_phy_language_d ( ) &
       RESULT( res )
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER :: res ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_phy_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       res = language
    ELSE
       res = -1
    END IF
    !
  END FUNCTION get_phy_language_d
  !
  !! Ermitteln der Spracheinstellung f&uuml;r die Beschreibung einer 
  !! physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_language_0 ( var ) &
       RESULT ( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Sprache (1,2) = (Deutsch,Englisch) [ -1 == nicht definiert ]
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, i, l1 ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       l1 = LEN_TRIM(var)
       DO i=1,c_max_language
          IF ( res /= -1 ) EXIT
          IF ( var(1:l1) == phy_quants(idx)%descr(i)(1:l1) ) res = i 
       END DO
    END IF
    !
  END FUNCTION get_phy_quant_language_0
  !
  !! Ermitteln der Spracheinstellung f&uuml;r die Beschreibung mehrerer
  !! physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_language_1 ( var ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Sprache (1,2) = (Deutsch,Englisch) [ -1 == nicht definiert ]
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_language_0 ( var(i) ) 
    END DO
    !
  END FUNCTION get_phy_quant_language_1
  !
  !! Ermitteln der Code-Kennung f&uuml;r die Beschreibung einer 
  !! physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_code_0 ( var ) &
       RESULT ( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Code-Kennung, -1 == nicht definiert
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%code
    !
  END FUNCTION get_phy_quant_code_0
  !
  !! Ermitteln der Code-Kennungen f&uuml;r die Beschreibungen vieler 
  !! physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_code_1 ( var ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Code-Kennung, -1 == nicht definiert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_code_0 ( var(i) ) 
    END DO
    !
  END FUNCTION get_phy_quant_code_1
  !
  !! Ermitteln der maximal zul&auml;ssige Code-Kennung f&uuml;r 
  !! die Beschreibung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_max_code_0 ( ) &
       RESULT ( res )
    !! maximale Anzahl der Code-Kennungen
    INTEGER :: res ! 
    !
    IF ( ALLOCATED(phy_quants) ) THEN
       res = SIZE(phy_quants)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_phy_quant_max_code_0
  !
  !! Ermitteln der Id bei vorgegebener Code-Kennung einer 
  !! physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_id_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_id) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = 'undef' 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%id(language)
    !
  END FUNCTION get_phy_quant_id_code_0
  !
  !! Ermitteln der Id's bei vorgegebenen Code-Kennungen vieler 
  !! physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_id_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_id) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_id_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_id_code_1
  !
  !! Ermitteln der Id bei vorgegebener Beschreibung einer 
  !! physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_id_descr_0 ( var ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_id) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = 'undef' 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%id(language)
    !
  END FUNCTION get_phy_quant_id_descr_0
  !
  !! Ermitteln der Id's bei vorgegebenen Beschreibungen vieler 
  !! physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_id_descr_1 ( var ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_id) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_id_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_id_descr_1
  !
  !
  !! Ermitteln der Beschreibung bei vorgegebener Code-Kennung einer 
  !! physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_descr_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_descr) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = 'undefined' 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%descr(language)
    !
  END FUNCTION get_phy_quant_descr_code_0
  !
  !! Ermitteln der Beschreibungen bei vorgegebenen Code-Kennungen vieler 
  !! physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_descr_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Beschreibung oder "undefinded"
    CHARACTER (LEN=c_len_phy_descr) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_descr_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_descr_code_1
  !
  !! Ermitteln der Beschreibung bei vorgegebener Id einer 
  !! physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_descr_id_0 ( var ) &
       RESULT ( res )
    !! Id einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_descr) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = 'undefined' 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%descr(language)
    !
  END FUNCTION get_phy_quant_descr_id_0
  !
  !! Ermitteln der Beschreibungen bei vorgegebenen Id's vieler 
  !! physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_descr_id_1 ( var ) &
       RESULT ( res )
    !! Id's der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_descr) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_descr_id_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_descr_id_1
  !
  !! Ermitteln der Code-Kennung der Referenzgr&ouml;&szlig;e aus der
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_code_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Code-Kennung der Referenzgr&ouml;&szlig;e, -1 == undefiniert
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%ref_code
    !
  END FUNCTION get_phy_quant_ref_code_code_0
  !
  !! Ermitteln der Code-Kennungen der Referenzgr&ouml;&szlig;en aus den
  !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_code_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Code-Kennungen der Referenzgr&ouml;&szlig;en, -1 == undefiniert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ref_code_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ref_code_code_1
  !
  !! Ermitteln der Code-Kennung der Referenzgr&ouml;&szlig;e aus der
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_code_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Code-Kennung der Referenzgr&ouml;&szlig;e, -1 == undefiniert
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%ref_code
    !
  END FUNCTION get_phy_quant_ref_code_descr_0
  !
  !! Ermitteln der Code-Kennungen der Referenzgr&ouml;&szlig;en aus den
  !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_code_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Code-Kennungen der Referenzgr&ouml;&szlig;en, -1 == undefiniert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ref_code_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ref_code_descr_1
  !
  !! Ermitteln der Id der Referenzgr&ouml;&szlig;e aus der
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_id_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Id der Referenzgr&ouml;&szlig;e, "undef" == undefiniert
    CHARACTER(LEN=c_len_phy_id) :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    IF ( has_phy_quant_ref_quant( var ) ) THEN
       idx = get_phy_quant_ref_code( var )
       res = phy_quants(idx)%id(language)
    ELSE
       res = REPEAT( ' ', LEN(res) ) ; res = 'undef'
    END IF
    !
  END FUNCTION get_phy_quant_ref_id_code_0
  !
  !! Ermitteln der Id's der Referenzgr&ouml;&szlig;en aus den
  !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_id_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Id's der Referenzgr&ouml;&szlig;en, "undef" == undefiniert
    CHARACTER (LEN=c_len_phy_id) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ref_id_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ref_id_code_1
  !
  !! Ermitteln der Id der Referenzgr&ouml;&szlig;e aus der
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_id_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Id der Referenzgr&ouml;&szlig;e, "undef" == undefiniert
    CHARACTER (LEN=c_len_phy_id) :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    IF ( has_phy_quant_ref_quant( var ) ) THEN
       idx = get_phy_quant_ref_code( var )
       res = phy_quants(idx)%id(language)
    ELSE
       res = REPEAT( ' ', LEN(res) ) ; res = 'undef'
    END IF
    !
  END FUNCTION get_phy_quant_ref_id_descr_0
  !
  !! Ermitteln der Id's der Referenzgr&ouml;&szlig;en aus den
  !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_id_descr_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Id's der Referenzgr&ouml;&szlig;en, "undef" == undefiniert
    CHARACTER (LEN=c_len_phy_id) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ref_id_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ref_id_descr_1
  !
  !! Ermitteln der Beschreibung der Referenzgr&ouml;&szlig;e aus der
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_descr_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Beschreibung der Referenzgr&ouml;&szlig;e, "undefined" == undefiniert
    CHARACTER(LEN=c_len_phy_descr) :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    IF ( has_phy_quant_ref_quant( var ) ) THEN
       idx = get_phy_quant_ref_code( var )
       res = phy_quants(idx)%descr(language)
    ELSE
       res = REPEAT( ' ', LEN(res) ) ; res = 'undefined'
    END IF
    !
  END FUNCTION get_phy_quant_ref_descr_code_0
  !
  !! Ermitteln der Beschreibungen der Referenzgr&ouml;&szlig;en aus den
  !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_descr_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Beschreibungen der Referenzgr&ouml;&szlig;en, "undefined" == undefiniert
    CHARACTER (LEN=c_len_phy_descr) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ref_descr_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ref_descr_code_1
  !
  !! Ermitteln der Beschreibung der Referenzgr&ouml;&szlig;e aus der
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_descr_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Beschreibung der Referenzgr&ouml;&szlig;e, "undefined" == undefiniert
    CHARACTER (LEN=c_len_phy_descr) :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    IF ( has_phy_quant_ref_quant( var ) ) THEN
       idx = get_phy_quant_ref_code( var )
       res = phy_quants(idx)%descr(language)
    ELSE
       res = REPEAT( ' ', LEN(res) ) ; res = 'undefined'
    END IF
    !
  END FUNCTION get_phy_quant_ref_descr_descr_0
  !
  !! Ermitteln der Beschreibungen der Referenzgr&ouml;&szlig;en aus den
  !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ref_descr_descr_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Beschreibungen der Referenzgr&ouml;&szlig;en, "undefined" == undefiniert
    CHARACTER (LEN=c_len_phy_descr) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ref_descr_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ref_descr_descr_1
  !
  !! Ermitteln des Plot-Indikators einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ploti_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Plot-Indikator, -1 == undefiniert
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%mzr
    !
  END FUNCTION get_phy_quant_ploti_code_0
  !
  !! Ermitteln der Plot-Indikatoren vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ploti_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Plot-Indikator, -1 == undefiniert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ploti_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ploti_code_1
  !
  !! Ermitteln des Plot-Indikators einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ploti_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Plot-Indikator, -1 == undefiniert
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%mzr
    !
  END FUNCTION get_phy_quant_ploti_descr_0
  !
  !! Ermitteln der Plot-Indikatoren vieler physikalischer Gr&ouml;&szlig;en (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ploti_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Plot-Indikatoren, -1 == undefiniert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ploti_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ploti_descr_1
  !
  !! Ermitteln des Plot-Textes zu einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_plott_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Plot-Text, "undefined" == undefiniert
    CHARACTER (LEN=c_len_plot_text) :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = REPEAT( ' ', LEN(RES) ) ; res = c_undefined
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       IF ( phy_quants(idx)%mzr >=1 .AND. phy_quants(idx)%mzr <= c_max_plot_text ) THEN
          res = REPEAT( ' ', LEN(res) ) ; res = c_plot_text(phy_quants(idx)%mzr,language)
       ELSE
          res = REPEAT( ' ', LEN(res) ) ; res = 'NONE'
       END IF
    END IF
    !
  END FUNCTION get_phy_quant_plott_code_0
  !
  !! Ermitteln der Plot-Texte vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_plott_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Plot-Texte, "undefined" == undefiniert
    CHARACTER (LEN=c_len_plot_text) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_plott_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_plott_code_1
  !
  !! Ermitteln des Plot-Textes einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_plott_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Plot-Text, "undefined" == undefiniert
    CHARACTER (LEN=c_len_plot_text) :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = REPEAT( ' ', LEN(RES) ) ; res = c_undefined
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       IF ( phy_quants(idx)%mzr >= 1 .AND. phy_quants(idx)%mzr <= c_max_plot_text ) THEN
          res = REPEAT( ' ', LEN(res) ) ; res = c_plot_text(phy_quants(idx)%mzr,language)
       ELSE
          res = REPEAT( ' ', LEN(res) ) ; res = 'NONE'
       END IF
    END IF
    !
  END FUNCTION get_phy_quant_plott_descr_0
  !
  !! Ermitteln der Plot-Texte vieler physikalischer Gr&ouml;&szlig;en (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_plott_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Plot-Texte, -1 == undefiniert
    CHARACTER (LEN=c_len_plot_text) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_plott_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_plott_descr_1
  !
  !! Ermitteln des Zeit-Indikators einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_timei_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Zeit-Indikator, -1 == undefiniert
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%zta
    !
  END FUNCTION get_phy_quant_timei_code_0
  !
  !! Ermitteln der Zeit-Indikatoren vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_timei_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Zeit-Indikator, -1 == undefiniert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_timei_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_timei_code_1
  !
  !! Ermitteln des Zeit-Indikators einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_timei_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Zeit-Indikator, -1 == undefiniert
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%zta
    !
  END FUNCTION get_phy_quant_timei_descr_0
  !
  !! Ermitteln der Zeit-Indikatoren vieler physikalischer Gr&ouml;&szlig;en (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_timei_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Zeit-Indikatoren, -1 == undefiniert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_timei_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_timei_descr_1
  !
  !! Ermitteln des Typ-Indikators einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_typei_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Typ-Indikator, -1 == undefiniert
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%typ
    !
  END FUNCTION get_phy_quant_typei_code_0
  !
  !! Ermitteln der Typ-Indikatoren vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_typei_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Typ-Indikator, -1 == undefiniert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_typei_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_typei_code_1
  !
  !! Ermitteln des Typ-Indikators einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_typei_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Typ-Indikator, -1 == undefiniert
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%typ
    !
  END FUNCTION get_phy_quant_typei_descr_0
  !
  !! Ermitteln der Typ-Indikatoren vieler physikalischer Gr&ouml;&szlig;en (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_typei_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Typ-Indikatoren, -1 == undefiniert
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_typei_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_typei_descr_1
  !
  !! Ermitteln des Plot-Faktors einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_plotf_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Plot-Faktor, 0.0 == undefiniert
    REAL (KIND=DOUBLE) :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = 0.0_Double
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%mul
    !
  END FUNCTION get_phy_quant_plotf_code_0
  !
  !! Ermitteln der Plot-Faktoren vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_plotf_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Plot-Faktoren, 0.0 == undefiniert
    REAL (KIND=DOUBLE) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_plotf_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_plotf_code_1
  !
  !! Ermitteln des Plot-Faktoren einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_plotf_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Plot-Faktor, 0.9 == undefiniert
    REAL (KIND=DOUBLE) :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = 0.0_Double
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%mul
    !
  END FUNCTION get_phy_quant_plotf_descr_0
  !
  !! Ermitteln der Plot-Faktoren vieler physikalischer Gr&ouml;&szlig;en (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_plotf_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Plot-Faktoren, 0.0 == undefiniert
    REAL (KIND=DOUBLE) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_plotf_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_plotf_descr_1
  !
  !! Ermitteln der Anzahl der Gitter-ElementSets, die beim Verarbeiten einer 
  !! physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen sind (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ele_nof_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Anzahl der ElementSets
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = 0
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = COUNT( get_omi_coded( c_max_omiele, phy_quants(idx)%ele ) )
    !
  END FUNCTION get_phy_quant_ele_nof_code_0
  !
  !! Ermitteln der Anzahl der Gitter-ElementSets, die beim Verarbeiten mehrerer 
  !! physikalischer Gr&ouml;&szlig;en zu ber&uuml;cksichtigen sind (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ele_nof_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Anzahl der ElementSets
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ele_nof_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ele_nof_code_1
  !
  !! Ermitteln der Anzahl der Gitter-ElementSets, die beim Verarbeiten einer 
  !! physikalischen Gr&ouml;&szlig;e (Beschreibung) zu ber&uuml;cksichtigen sind (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ele_nof_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Anzahl der ElementSets
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = 0
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = get_phy_quant_ele_nof_code_0 ( phy_quants(idx)%code )
    !
  END FUNCTION get_phy_quant_ele_nof_descr_0
  !
  !! Ermitteln der Anzahl der Gitter-ElementSets, die beim Verarbeiten mehrerer 
  !! physikalischer Gr&ouml;&szlig;en (Beschreibungen) zu ber&uuml;cksichtigen sind (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ele_nof_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Anzahl der ElementSets
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_ele_nof_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ele_nof_descr_1
  !
  !! Ermitteln der Anzahl der Aspekte, die beim Erzeugen der Daten-ElementSets
  !! einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen sind (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_gen_nof_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Anzahl der Aspekte
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = 0
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = COUNT( get_omi_coded( c_max_omigen, phy_quants(idx)%gen ) )
    !
  END FUNCTION get_phy_quant_gen_nof_code_0
  !
  !! Ermitteln der Anzahl der Aspekte, die beim Erzeugen der Daten-ElementSets
  !! mehrerer physikalischer Gr&ouml;&szlig;en zu ber&uuml;cksichtigen sind (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_gen_nof_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Anzahl der Aspekte
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_gen_nof_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_gen_nof_code_1
  !
  !! Ermitteln der Anzahl der Aspekte, die beim Erzeugen der Daten-ElementSets
  !! einer physikalischen Gr&ouml;&szlig;e (Beschreibung) zu ber&uuml;cksichtigen sind (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_gen_nof_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Anzahl der Aspekte
    INTEGER :: res ! 
    ! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = 0
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = get_phy_quant_gen_nof_code_0 ( phy_quants(idx)%code )
    !
  END FUNCTION get_phy_quant_gen_nof_descr_0
  !
  !! Ermitteln der Anzahl der Aspekte, die beim Erzeugen der Daten-ElementSets
  !! mehrerer physikalischer Gr&ouml;&szlig;en zu ber&uuml;cksichtigen sind (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_gen_nof_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Anzahl der Aspekte
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_gen_nof_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_quant_gen_nof_descr_1
  !
  !! Ermittle den Namen des Gitter-ElementSets anhand der lfd. Nummer, das 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! ist (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ele_code_0_0 ( var, val ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! lfd. Nummer des Gitter-Elementsets
    INTEGER , INTENT(IN) :: val ! 
    !! Name des ElementSets
    CHARACTER (LEN=c_len_omiele) :: res ! 
    !! Hilfsvariablen
    LOGICAL :: l_ex(c_max_omiele) ! 
    INTEGER :: idx, i, n          ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undefined
    idx = get_phy_quants_idx ( var )
    IF ( val >= 1 .AND. val <= get_phy_quant_ele_nof_code_0 ( var ) .AND. idx > 0 ) THEN
       l_ex(:) = get_omi_coded ( c_max_omiele, phy_quants(idx)%ele )
       n       = 0
       DO i=1,SIZE(l_ex)
          IF ( res(1:LEN_TRIM(c_undefined)) /= c_undefined ) EXIT
          IF ( l_ex(i)  ) n = n + 1
          IF ( n == val ) THEN
             res = REPEAT( ' ', LEN(res) )
             res = c_omiele_text(i)
          END IF
       END DO
    END IF
    !
  END FUNCTION get_phy_quant_ele_code_0_0
  !
  !! Ermittle die Namen des Gitter-ElementSets anhand der lfd. Nummern, die 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! ist (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ele_code_0_1 ( var, val ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! lfd. Nummern der Gitter-Elementsets
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Namen der ElementSets
    CHARACTER (LEN=c_len_omiele) :: res(SIZE(val)) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_phy_quant_ele_code_0_0 ( var, val(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ele_code_0_1
  !
  !! Ermittle den Namen des Gitter-ElementSets anhand der lfd. Nummer, das 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! ist (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ele_descr_0_0 ( var, val ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! lfd. Nummer des Gitter-Elementsets
    INTEGER , INTENT(IN) :: val ! 
    !! Name des ElementSets
    CHARACTER (LEN=c_len_omiele) :: res ! 
    !! Hilfsvariable
    INTEGER                      :: idx ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undefined
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = get_phy_quant_ele_code_0_0 ( phy_quants(idx)%code, val )
    !
  END FUNCTION get_phy_quant_ele_descr_0_0
  !
  !! Ermittle die Namen der Gitter-ElementSets anhand der lfd. Nummern, die 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! sind (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_ele_descr_0_1 ( var, val ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! lfd. Nummern der Gitter-Elementsets
    INTEGER           , INTENT(IN) :: val(:) ! 
    !! Namen der ElementSets
    CHARACTER (LEN=c_len_omiele) :: res(SIZE(val)) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_phy_quant_ele_descr_0_0 ( var, val(i) )
    END DO
    !
  END FUNCTION get_phy_quant_ele_descr_0_1
  !
  !! Ermittle f&uuml;r ein Gitter-ElementSet anhand der lfd. Nummer, ob
  !! dieses ElementSet zwei- oder drei-dimensional ist (Skalar) <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_ele_3d_code_0_0 ( var, val ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! lfd. Nummer des Gitter-Elementsets
    INTEGER , INTENT(IN) :: val ! 
    !! Testergebnis : .true. == 3d, .false. == 2d
    LOGICAL :: res ! 
    !! Hilfsvariablen
    INTEGER :: idx                ! 
    !
    res = .false.
    idx = get_phy_quants_idx ( var )
    IF ( val >= 1 .AND. val <= get_phy_quant_ele_nof_code_0 ( var ) .AND. idx > 0 ) THEN
       res = ( INDEX( get_phy_quant_omiele( var, val ), 'xyz' ) > 0 )
    END IF
    !
  END FUNCTION is_phy_quant_ele_3d_code_0_0
  !
  !! Ermittle die Namen des Gitter-ElementSets anhand der lfd. Nummern, die 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! ist (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_ele_3d_code_0_1 ( var, val ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! lfd. Nummern der Gitter-Elementsets
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Testergebnis : .true. == 3d, .false. == 2d
    LOGICAL :: res(SIZE(val)) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = is_phy_quant_ele_3d_code_0_0 ( var, val(i) )
    END DO
    !
  END FUNCTION is_phy_quant_ele_3d_code_0_1
  !
  !! Ermittle den Namen des Gitter-ElementSets anhand der lfd. Nummer, das 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! ist (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_ele_3d_descr_0_0 ( var, val ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! lfd. Nummer des Gitter-Elementsets
    INTEGER , INTENT(IN) :: val ! 
    !! Testergebnis : .true. == 3d, .false. == 2d
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx ( var )
    IF ( idx > 0 ) res = is_phy_quant_ele_3d_code_0_0 ( phy_quants(idx)%code, val )
    !
  END FUNCTION is_phy_quant_ele_3d_descr_0_0
  !
  !! Ermittle die Namen der Gitter-ElementSets anhand der lfd. Nummern, die 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! sind (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_ele_3d_descr_0_1 ( var, val ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! lfd. Nummern der Gitter-Elementsets
    INTEGER           , INTENT(IN) :: val(:) ! 
    !! Testergebnis : .true. == 3d, .false. == 2d
    LOGICAL :: res(SIZE(val)) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = is_phy_quant_ele_3d_descr_0_0 ( var, val(i) )
    END DO
    !
  END FUNCTION is_phy_quant_ele_3d_descr_0_1
  !
  !! Ermittle den Aspektnamen des Daten-ElementSets anhand der lfd. Nummer, der
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! ist (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_gen_code_0_0 ( var, val ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! lfd. Nummer des Gitter-Elementsets
    INTEGER , INTENT(IN) :: val ! 
    !! Aspektname
    CHARACTER (LEN=c_len_omigen) :: res ! 
    !! Hilfsvariablen
    LOGICAL :: l_ex(c_max_omigen) ! 
    INTEGER :: idx, i, n          ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undefined
    idx = get_phy_quants_idx ( var )
    IF ( val >= 1 .AND. val <= get_phy_quant_gen_nof_code_0 ( var ) .AND. idx > 0 ) THEN
       l_ex(:) = get_omi_coded ( c_max_omigen, phy_quants(idx)%gen )
       n       = 0
       DO i=1,SIZE(l_ex)
          IF ( res(1:LEN_TRIM(c_undefined)) /= c_undefined ) EXIT
          IF ( l_ex(i)  ) n = n + 1
          IF ( n == val ) THEN
             res = REPEAT( ' ', LEN(res) )
             res = c_omigen_text(i)
          END IF
       END DO
    END IF
    !
  END FUNCTION get_phy_quant_gen_code_0_0
  !
  !! Ermittle die Aspektnamen des Daten-ElementSets anhand der lfd. Nummern, die 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! ist (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_gen_code_0_1 ( var, val ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! lfd. Nummern der Gitter-Elementsets
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Aspektnamen
    CHARACTER (LEN=c_len_omigen) :: res(SIZE(val)) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_phy_quant_gen_code_0_0 ( var, val(i) )
    END DO
    !
  END FUNCTION get_phy_quant_gen_code_0_1
  !
  !! Ermittle den Aspektnamen des Gitter-ElementSets anhand der lfd. Nummer, der 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! ist (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_gen_descr_0_0 ( var, val ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! lfd. Nummer des Gitter-Elementsets
    INTEGER , INTENT(IN) :: val ! 
    !! Aspektname
    CHARACTER (LEN=c_len_omigen) :: res ! 
    !! Hilfsvariable
    INTEGER                      :: idx ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undefined
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) res = get_phy_quant_gen_code_0_0 ( phy_quants(idx)%code, val )
    !
  END FUNCTION get_phy_quant_gen_descr_0_0
  !
  !! Ermittle die Aspektnamen der Gitter-ElementSets anhand der lfd. Nummern, die 
  !! beim Verarbeiten einer physikalischen Gr&ouml;&szlig;e zu ber&uuml;cksichtigen 
  !! sind (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_phy_quant_gen_descr_0_1 ( var, val ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! lfd. Nummern der Gitter-Elementsets
    INTEGER           , INTENT(IN) :: val(:) ! 
    !! Aspektnamen
    CHARACTER (LEN=c_len_omigen) :: res(SIZE(val)) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_phy_quant_gen_descr_0_0 ( var, val(i) )
    END DO
    !
  END FUNCTION get_phy_quant_gen_descr_0_1
  !
  !! Ermitteln der Id der physikalischen Einheit bei vorgegebener 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_id_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Id oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_id) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = c_undefined 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 ) THEN
          res = phy_units(jdx)%id(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_id_code_0
  !
  !! Ermitteln der Ids der physikalischen Einheiten bei vorgegebenen 
  !! Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_id_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Ids oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_id) :: res(SIZE(var)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_phy_unit_id_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_unit_id_code_1
  !
  !! Ermitteln der Id der physikalischen Einheit bei vorgegebener 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_id_descr_0 ( var ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Id oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_id) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = c_undefined 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 ) THEN
          res = phy_units(jdx)%id(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_id_descr_0
  !
  !! Ermitteln der Id's der physikalischen Einheiten bei vorgegebenen 
  !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_id_descr_1 ( var ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Id oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_id) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_unit_id_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_unit_id_descr_1
  !
  !! Ermitteln der Id der physikalischen Einheit bei vorgegebenen
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e sowie den
  !! SI-Basisdimensionen der Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_id_si_base_0 ( code, var ) &
       RESULT ( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: code   ! 
    !! bekannte Basisdimensionen einer physikalischen Gr&ouml;&szlig;e
    REAL (KIND=Double) , INTENT(IN) :: var(:) ! >GL>
    !! Id oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_id) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = c_undefined 
    idx = get_phy_quants_idx ( code )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 .AND. SIZE(var) == c_max_si_base ) THEN
          IF ( ALL( ABS( phy_units(jdx)%base(:) - var(:) ) <= EPSILON(var(:)) ) ) THEN ! >GL>
             res = phy_units(jdx)%id(language)
          END IF
       END IF
    END IF
    !
    IF ( res(1:LEN(c_undefined)) == c_undefined .AND. jdx > 0 ) THEN
       idx = get_phy_units_idx ( phy_units(jdx), var )
       IF ( idx > 0 ) THEN
          res = phy_units(idx)%id(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_id_si_base_0
  !
  !! Ermitteln der Beschreibung der physikalischen Einheit bei vorgegebener 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_descr_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_descr) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = c_undefined 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 ) THEN
          res = phy_units(jdx)%descr(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_descr_code_0
  !
  !! Ermitteln der Beschreibungen der physikalischen Einheiten bei vorgegebenen
  !! Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_descr_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_descr) :: res(SIZE(var)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_phy_unit_descr_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_unit_descr_code_1
  !
  !! Ermitteln der Beschreibung der physikalischen Einheit bei vorgegebener 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_descr_descr_0 ( var ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_descr) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = c_undefined 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 ) THEN
          res = phy_units(jdx)%descr(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_descr_descr_0
  !
  !! Ermitteln der Beschreibungen der physikalischen Einheiten bei vorgegebenen 
  !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_descr_descr_1 ( var ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Beschreibungen oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_descr) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_unit_descr_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_unit_descr_descr_1
  !
  !! Ermitteln der Beschreibung der physikalischen Einheit bei vorgegebener Code-Kennung
  !! sowie vorgegebenen Basisdimensionen <EM>einer</EM> physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_descr_si_base_0 ( code, var ) &
       RESULT ( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: code   ! 
    !! bekannte Basisdimensionen einer physikalischen Gr&ouml;&szlig;e
    REAL (KIND=Double) , INTENT(IN) :: var(:) ! >GL>
    !! Beschreibungen oder "undefined"
    CHARACTER (LEN=c_len_phy_unit_descr) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = c_undefined 
    idx = get_phy_quants_idx ( code )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 .AND. SIZE(var) == c_max_si_base ) THEN
          IF ( ALL( ABS( phy_units(jdx)%base(:) - var(:) ) <= EPSILON(var(:)) ) ) THEN ! >GL> 
             res = phy_units(jdx)%descr(language)
          END IF
       END IF
    END IF
    !
    IF ( res(1:LEN(c_undefined)) == c_undefined .AND. jdx > 0 ) THEN
       idx = get_phy_units_idx ( phy_units(jdx), var )
       IF ( idx > 0 ) THEN
          res = phy_units(idx)%descr(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_descr_si_base_0
  !
  !
  !! Ermittle die Anzahl der SI-Basiseinheiten <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_phy_unit_max_si_base_d ( ) &
       RESULT( res )
    !! Ergebnis: Anzahl der SI-Basiseinheiten
    INTEGER :: res ! 
    !
    res = c_max_si_base
    !
  END FUNCTION get_phy_unit_max_si_base_d
  !
  !! Ermittle den Namen der ind-ten SI-Basiseinheit <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_phy_unit_si_base_name_d ( ind ) &
       RESULT( res )
    !! Index der Basiseinheit
    INTEGER , INTENT(IN) :: ind ! 
    !! Ergebnis: Name der ind-ten SI-Basiseinheit oder "undefined"
    CHARACTER (LEN=c_len_si_base_name) :: res ! 
    !
    res = REPEAT( ' ', LEN(res) )
    !
    IF ( ind >= 1 .AND. ind <= c_max_si_base ) THEN
       res = c_si_base_name(ind,language)
    ELSE
       res = c_undefined
    END IF
    !
  END FUNCTION get_phy_unit_si_base_name_d
  !
  !! Ermittle die Einheit der ind-ten SI-Basiseinheit <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_phy_unit_si_base_unit_d ( ind ) &
       RESULT( res )
    !! Index der Basiseinheit
    INTEGER , INTENT(IN) :: ind ! 
    !! Ergebnis: Einheit der ind-ten SI-Basiseinheit oder "undefined"
    CHARACTER (LEN=c_len_si_base_unit) :: res ! 
    !
    res = REPEAT( ' ', LEN(res) )
    !
    IF ( ind >= 1 .AND. ind <= c_max_si_base ) THEN
       res = c_si_base_unit(ind,language)
    ELSE
       res = c_undefined
    END IF
    !
  END FUNCTION get_phy_unit_si_base_unit_d
  !
  !! Ermittle das Symbol der ind-ten SI-Basiseinheit <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_phy_unit_si_base_symb_d ( ind ) &
       RESULT( res )
    !! Index der Basiseinheit
    INTEGER , INTENT(IN) :: ind ! 
    !! Ergebnis: Symbol der ind-ten SI-Basiseinheit oder "---"
    CHARACTER (LEN=c_len_si_base_symb) :: res ! 
    !
    res = REPEAT( ' ', LEN(res) )
    !
    IF ( ind >= 1 .AND. ind <= c_max_si_base ) THEN
       res = c_si_base_symb(ind)
    ELSE
       res = REPEAT( ' ', LEN(res) )
    END IF
    !
  END FUNCTION get_phy_unit_si_base_symb_d
  !
  !! Ermitteln des Exponenten einer SI-Basisdimension der physikalischen Einheit 
  !! bei vorgegebener Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_power_code_0 ( var, ind ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Index der Basisdimension
    INTEGER , INTENT(IN) :: ind ! 
    !! Exponent der ind-ten SI-Basisdimension, oder undefiniert = 999.0
    REAL (KIND=Double) :: res ! >GL>
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = 999._Double ! >GL>
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 .AND. ind >= 1 .AND. ind <= c_max_si_base ) THEN
          res = phy_units(jdx)%base(ind)
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_power_code_0
  !
  !! Ermitteln der Exponenten einer SI-Basisdimension der physikalischen Einheiten 
  !! bei vorgegebenen Code-Kennungen der physikalischen Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_power_code_1 ( var, ind ) &
       RESULT ( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Index der Basisdimension
    INTEGER , INTENT(IN) :: ind ! 
    !! Exponenten der ind-ten SI-Basisdimension, oder undefiniert = 999.0
    REAL (KIND=Double) :: res(SIZE(var)) ! >GL> 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_unit_power_code_0 ( var(i), ind )
    END DO
    !
  END FUNCTION get_phy_unit_power_code_1
  !
  !! Ermitteln des Exponenten einer SI-Basisdimension der physikalischen Einheit 
  !! bei vorgegebener Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_power_descr_0 ( var, ind ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Index der Basisdimension
    INTEGER , INTENT(IN) :: ind ! 
    !! Exponent der ind-ten SI-Basisdimension, oder undefiniert = 999.0
    REAL (KIND=Double) :: res ! >GL>
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = 999._Double ! >GL>
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 .AND. ind >= 1 .AND. ind <= c_max_si_base ) THEN
          res = phy_units(jdx)%base(ind)
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_power_descr_0
  !
  !! Ermitteln der Exponenten einer SI-Basisdimension der physikalischen Einheiten 
  !! bei vorgegebenen Beschreibungen der physikalischen Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_power_descr_1 ( var, ind ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Index der Basisdimension
    INTEGER , INTENT(IN) :: ind ! 
    !! Exponenten der ind-ten SI-Basisdimension, oder undefiniert = 999.0 
    REAL (KIND=Double) :: res(SIZE(var)) ! >GL>
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_unit_power_descr_0 ( var(i), ind )
    END DO
    !
  END FUNCTION get_phy_unit_power_descr_1
  !
  !! Ermitteln des Faktors zum Umrechnen in SI-Einheiten bei vorgegebener 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_convf_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Umrechnungsfaktor nach SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = 0.0_Double
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 ) THEN
          res = phy_units(jdx)%convf
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_convf_code_0
  !
  !! Ermitteln der Faktoren zum Umrechnen in SI-Einheiten bei vorgegebenen 
  !! Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_convf_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:)         ! 
    !! Umrechnungsfaktoren nach SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE)   :: res(SIZE(var)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_phy_unit_convf_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_unit_convf_code_1
  !
  !! Ermitteln des Faktors zum Umrechnen in SI-Einheiten bei vorgegebener 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_convf_descr_0 ( var ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Umrechnungsfaktor nach SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = 0.0_Double
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 ) THEN
          res = phy_units(jdx)%convf
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_convf_descr_0
  !
  !! Ermitteln der Faktoren zum Umrechnen in SI-Einheiten bei vorgegebenen 
  !! Beschreibungen der physikalischen Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_convf_descr_1 ( var ) &
       RESULT ( res )
    !!Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Umrechnungsfaktoren nach SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_unit_convf_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_unit_convf_descr_1
  !
  !! Ermitteln des Faktors zum Umrechnen in SI-Einheiten bei vorgegebener
  !! Code-Kennung sowie vorgegebenen Basisdimensionen <EM>einer</EM> 
  !! physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_convf_si_base_0 ( code, var ) &
       RESULT ( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER            , INTENT(IN) :: code   ! 
    !! Basisdimensionen physikalischen Gr&ouml;&szlig;en
    REAL (KIND=Double) , INTENT(IN) :: var(:) ! >GL> 
    !! Umrechnungsfaktor nach SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE) :: res ! 
    ! Hilfsvariablen
    LOGICAL :: done     ! 
    INTEGER :: idx, jdx ! 
    !
    res  = 0.0_Double
    done = .false.
    idx  = get_phy_quants_idx ( code )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 .AND. SIZE(var) == c_max_si_base ) THEN
          IF ( ALL( phy_units(jdx)%base(:) - var(:) <= EPSILON(var(:)) ) ) THEN ! >GL>
             res  = phy_units(jdx)%convf
             done = .true. 
          END IF
       END IF
    END IF
    !
    IF ( .NOT. done .AND. jdx > 0 ) THEN
       idx = get_phy_units_idx ( phy_units(jdx), var )
       IF ( idx > 0 ) THEN
          res = phy_units(idx)%convf
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_convf_si_base_0
  !
  !
  !! Ermitteln des Offsets (in SI-Einheiten) zum Umrechnen in SI-Einheiten
  !! bei vorgegebener Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_offset_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Offset in SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = 0.0_Double
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 ) THEN
          res = phy_units(jdx)%offset
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_offset_code_0
  !
  !! Ermitteln des Offsets (in SI-Einheiten) zum Umrechnen in SI-Einheiten
  !! bei vorgegebenen Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_offset_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen mehrere physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:)         ! 
    !! Offsets in SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE)   :: res(SIZE(var)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_phy_unit_offset_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_unit_offset_code_1
  !
  !! Ermitteln des Offsets zum Umrechnen in SI-Einheiten bei vorgegebener 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_offset_descr_0 ( var ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Offset in SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = 0.0_Double
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 ) THEN
          res = phy_units(jdx)%offset
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_offset_descr_0
  !
  !! Ermitteln der Offsets zum Umrechnen in SI-Einheiten bei vorgegebenen 
  !! Beschreibungen der physikalischen Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_offset_descr_1 ( var ) &
       RESULT ( res )
    !!Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Offsets in SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_unit_offset_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_unit_offset_descr_1
  !
  !! Ermitteln der Offsets zum Umrechnen in SI-Einheiten bei vorgegebener Code-Kennung 
  !! und vorgegebenen Basisdimensionen der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_unit_offset_si_base_0 ( code, var ) &
       RESULT ( res )
    !! Code-Kennung der physikalischen Gr&ouml;szlig;e
    INTEGER            , INTENT(IN) :: code   ! 
    !! Basisdimensionen einer physikalischen Gr&ouml;&szlig;e
    REAL (KIND=Double) , INTENT(IN) :: var(:) ! >GL>
    !! Offsets in SI-Einheiten, oder undefiniert = 0.0
    REAL (KIND=DOUBLE)   :: res    ! 
    ! Hilfsvariablen
    LOGICAL :: done     ! 
    INTEGER :: idx, jdx ! 
    !
    res  = 0.0_Double
    done = .false.
    idx  = get_phy_quants_idx ( code )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_units_idx ( phy_quants(idx)%unit_code )
       IF ( jdx > 0 .AND. SIZE(var) == c_max_si_base ) THEN
          IF ( ALL( phy_units(jdx)%base(:) - var(:) <= EPSILON(var(:)) ) ) THEN ! >GL>
             res  = phy_units(jdx)%offset
             done = .true. 
          END IF
       END IF
    END IF
    !
    IF ( .NOT. done .AND. jdx > 0 ) THEN
       idx = get_phy_units_idx ( phy_units(jdx), var )
       IF ( idx > 0 ) THEN
          res = phy_units(idx)%offset
       END IF
    END IF
    !
  END FUNCTION get_phy_unit_offset_si_base_0
  !
  !! Ermitteln der Id der physikalischen Klasse bei vorgegebener 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_id_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_class_id) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = 'undef'
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_classes_idx ( phy_quants(idx)%class_code )
       IF ( jdx > 0 ) THEN
          res = phy_classes(jdx)%id(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_class_id_code_0
  !
  !! Ermitteln der Id's der physikalischen Klassen bei vorgegebenen 
  !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_id_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_class_id) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_class_id_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_class_id_code_1
  !
  !! Ermitteln der Id der physikalischen Klassen bei vorgegebener 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_id_descr_0 ( var ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_class_id) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = 'undef' 
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_classes_idx ( phy_quants(idx)%class_code )
       IF ( jdx > 0 ) THEN
          res = phy_classes(jdx)%id(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_class_id_descr_0
  !
  !! Ermitteln der Id's der physikalischen Klassen bei vorgegebenen 
  !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_id_descr_1 ( var ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_class_id) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_class_id_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_class_id_descr_1
  !
  !! Ermitteln den Code der physikalischen Klasse bei vorgegebener 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_code_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Code oder "-1"
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       res = phy_quants(idx)%class_code
    END IF
    !
  END FUNCTION get_phy_class_code_code_0
  !
  !! Ermitteln der Code's der physikalischen Klassen bei vorgegebenen 
  !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_code_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Code oder "-1"
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_class_code_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_class_code_code_1
  !
  !! Ermitteln des Code der physikalischen Klassen bei vorgegebener 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_code_descr_0 ( var ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Code oder -1
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = -1
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       res = phy_quants(idx)%class_code
    END IF
    !
  END FUNCTION get_phy_class_code_descr_0
  !
  !! Ermitteln der Code's der physikalischen Klassen bei vorgegebenen 
  !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_code_descr_1 ( var ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Code oder -1
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_class_code_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_class_code_descr_1
  !
  !! Ermitteln der Beschreibung der physikalischen Klasse bei vorgegebener 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_descr_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_class_descr) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = c_undefined
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_classes_idx ( phy_quants(idx)%class_code )
       IF ( jdx > 0 ) THEN
          res = phy_classes(jdx)%descr(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_class_descr_code_0
  !
  !! Ermitteln der Beschreibungen der physikalischen Klassen bei vorgegebenen 
  !! Code-Kennungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_descr_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_class_descr) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_class_descr_code_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_class_descr_code_1
  !
  !! Ermitteln der Beschreibung der physikalischen Klassen bei vorgegebener 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_descr_descr_0 ( var ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Beschreibung oder "undefined"
    CHARACTER (LEN=c_len_phy_class_descr) :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ; res = c_undefined
    idx = get_phy_quants_idx ( var )
    !
    IF ( idx > 0 ) THEN
       jdx = get_phy_classes_idx ( phy_quants(idx)%class_code )
       IF ( jdx > 0 ) THEN
          res = phy_classes(jdx)%descr(language)
       END IF
    END IF
    !
  END FUNCTION get_phy_class_descr_descr_0
  !
  !! Ermitteln der Beschreibungen der physikalischen Klassen bei vorgegebenen 
  !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_class_descr_descr_1 ( var ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Id oder "undef"
    CHARACTER (LEN=c_len_phy_class_descr) :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_class_descr_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION get_phy_class_descr_descr_1
  !
  !! Ermitteln der Anzahl der zu einer physikalischen Gr&ouml;&szlig;e
  !! geh&ouml;renden Standardbezeichnungen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_nof_stdn_code_0 ( var ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Ergebnis: Anzahl der zugeordneten Standardbezeichnungen
    INTEGER :: res ! 
    !
    res = 0
    IF ( ALLOCATED( phy_standards ) ) THEN
       res = COUNT( phy_standards(:)%code == var )
    END IF
    ! 
  END FUNCTION get_phy_quant_nof_stdn_code_0
  !
  !! Ermitteln der Anzahl der zu mehreren physikalischen Gr&ouml;&szlig;en
  !! geh&ouml;renden Standardbezeichnungen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_nof_stdn_code_1 ( var ) &
       RESULT ( res )
    !! Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Ergebnis: Anzahl der zugeordneten Standardbezeichnungen
    INTEGER :: res(SIZE(var)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_nof_stdn_code_0 ( var(i) )
    END DO
    ! 
  END FUNCTION get_phy_quant_nof_stdn_code_1
  !
  !! Ermitteln der Anzahl der zu einer physikalischen Gr&ouml;&szlig;e
  !! geh&ouml;renden Standardbezeichnungen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_nof_stdn_descr_0 ( var ) &
       RESULT ( res )
    !! Name einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebnis: Anzahl der zugeordneten Standardbezeichnungen
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = 0
    idx = get_phy_quants_idx ( var )
    IF ( idx > 0 ) THEN
       res = get_phy_quant_nof_stdn_code_0 ( phy_quants(idx)%code )
    END IF
    ! 
  END FUNCTION get_phy_quant_nof_stdn_descr_0
  !
  !! Ermitteln der Anzahl der zu mehreren physikalischen Gr&ouml;&szlig;en
  !! geh&ouml;renden Standardbezeichnungen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_nof_stdn_descr_1 ( var ) &
       RESULT ( res )
    !! Namen mehrerer physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Ergebnis: Anzahl der zugeordneten Standardbezeichnungen
    INTEGER :: res(SIZE(var)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_phy_quant_nof_stdn_descr_0 ( var(i) )
    END DO
    ! 
  END FUNCTION get_phy_quant_nof_stdn_descr_1
  !
  !! Ermittle die n-te Standardbezeichnung zu einer physikalischen Gr&ouml;&szlig;e (Code) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_stdn_code_0_0 ( var, idx ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER                , INTENT(IN) :: var ! 
    !! lfd. Nummer der zu "var" geh&ouml;renden Standardbezeichnung
    INTEGER                , INTENT(IN) :: idx ! 
    !! Ergebnis: Standardbezeichnung oder "undefined"
    CHARACTER (LEN=c_len_standard_name) :: res ! 
    !! Hilfsvariablen
    INTEGER :: jdx ! 
    !
    jdx = get_phy_standards_idx ( var, idx )
    IF ( jdx > 0 ) THEN
       res = phy_standards(jdx)%standard_name
    ELSE
       res = REPEAT( ' ', LEN(res) )
       res = c_undefined
    END IF
    ! 
  END FUNCTION get_phy_quant_stdn_code_0_0
  !
  !! Ermittle die n-te Standardbezeichnung zu einer physikalischen Gr&ouml;&szlig;e (Beschreibung) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_stdn_descr_0_0 ( var, idx ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*)      , INTENT(IN) :: var ! 
    !! lfd. Nummer der zu "var" geh&ouml;renden Standardbezeichnung
    INTEGER                , INTENT(IN) :: idx ! 
    !! Ergebnis: Standardbezeichnung oder "undefined"
    CHARACTER (LEN=c_len_standard_name) :: res ! 
    !! Hilfsvariablen
    INTEGER :: jdx ! 
    ! 
    jdx = get_phy_quants_idx ( var )
    IF ( jdx > 0 ) THEN
       res = get_phy_quant_stdn_code_0_0( phy_quants(jdx)%code, idx )
    ELSE
       res = REPEAT( ' ', LEN(res) )
       res = c_undefined
    END IF
    !
  END FUNCTION get_phy_quant_stdn_descr_0_0
  !
  !! Ermittle den n-ten Dimensionsindex zu einer physikalischen Gr&ouml;&szlig;e (Code) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_stdd_code_0_0 ( var, idx ) &
       RESULT ( res )
    !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! lfd. Nummer der zu "var" geh&ouml;renden physikalischen Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: idx ! 
    !! Ergebnis: Zeiger auf Dimension (-1 = nicht definiert, 0 = skalar, > 0 Vektordimension)
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER :: jdx ! 
    !
    jdx = get_phy_standards_idx ( var, idx )
    IF ( jdx > 0 ) THEN
       res = phy_standards(jdx)%idim
    ELSE
       res = -1
    END IF
    ! 
  END FUNCTION get_phy_quant_stdd_code_0_0
  !
  !! Ermittle den n-ten Dimensionsindex zu einer physikalischen Gr&ouml;&szlig;e (Beschreibung) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quant_stdd_descr_0_0 ( var, idx ) &
       RESULT ( res )
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! lfd. Nummer der zu "var" geh&ouml;renden physikalischen Gr&ouml;&szlig;en
    INTEGER           , INTENT(IN) :: idx ! 
    !! Ergebnis: Zeiger auf Dimension (-1 = nicht definiert, 0 = skalar, > 0 Vektordimension)
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER :: jdx ! 
    ! 
    jdx = get_phy_quants_idx ( var )
    IF ( jdx > 0 ) THEN
       res = get_phy_quant_stdd_code_0_0( phy_quants(jdx)%code, idx )
    ELSE
       res = -1
    END IF
    ! 
  END FUNCTION get_phy_quant_stdd_descr_0_0
  !
  !! Ermittle die Codekennung zu einem vorgegebenen Standardnamen (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_stdn_quant_0 ( var ) &
       RESULT( res )
    !! Standard-Name der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebnis: Code der korrespondierenden physikalischen Gr&ouml;&szlig;e
    !! -1 falls nicht vorhanden
    INTEGER                        :: res ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2 ! 
    !
    res = -1
    IF ( ALLOCATED(phy_standards) ) THEN
       l1 = LEN_TRIM(var)
       DO i=1,SIZE(phy_standards)
          IF ( res /= -1 ) EXIT
          l2 = LEN_TRIM(phy_standards(i)%standard_name)
          IF ( l1 == l2 ) THEN
             IF ( var(1:l1) == phy_standards(i)%standard_name(1:l2) ) res = phy_standards(i)%code
          END IF
       END DO
    END IF
    !
  END FUNCTION get_phy_stdn_quant_0
  !
  !! Ermittle die Codekennungen zu vorgegebenen Standardnamen (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_stdn_quant_1 ( var ) &
       RESULT( res )
    !! Standard-Namen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:)         ! 
    !! Ergebnis: Codes der korrespondierenden physikalischen Gr&ouml;&szlig;en
    !! -1 falls nicht vorhanden
    INTEGER                        :: res(SIZE(var)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_phy_stdn_quant_0( var(i) )
    END DO
    !
  END FUNCTION get_phy_stdn_quant_1
  !
  !! Ermittle die Dimension zu einem vorgegebenen Standardnamen (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_stdn_dim_0 ( var ) &
       RESULT( res )
    !! Standard-Name der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebnis: Dimension der korrespondierenden physikalischen Gr&ouml;&szlig;e
    !! -1 falls nicht vorhanden
    INTEGER                        :: res ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2 ! 
    !
    res = -1
    IF ( ALLOCATED(phy_standards) ) THEN
       l1 = LEN_TRIM(var)
       DO i=1,SIZE(phy_standards)
          IF ( res /= -1 ) EXIT
          l2 = LEN_TRIM(phy_standards(i)%standard_name)
          IF ( l1 == l2 ) THEN
             IF ( var(1:l1) == phy_standards(i)%standard_name(1:l2) ) res = phy_standards(i)%idim
          END IF
       END DO
    END IF
    !
  END FUNCTION get_phy_stdn_dim_0
  !
  !! Ermittle die Dimensionen zu vorgegebenen Standardnamen (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_stdn_dim_1 ( var ) &
       RESULT( res )
    !! Standard-Namen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:)         ! 
    !! Ergebnis: Dimensionen der korrespondierenden physikalischen Gr&ouml;&szlig;en
    !! -1 falls nicht vorhanden
    INTEGER                        :: res(SIZE(var)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_phy_stdn_dim_0( var(i) )
    END DO
    !
  END FUNCTION get_phy_stdn_dim_1
  !
  !! Konvertiere einen ganzzahligen Skalar (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_in_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       IF ( var /= c_threshold_in ) THEN
          res = INT( REAL(csi*cpl,Single)*REAL(var,Single) + off )
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_or_si_in_code_0
  !
  !! Konvertiere einen ganzzahligen 1D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_in_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_in )
          res(:) = INT( REAL(csi*cpl,Single)*REAL(var(:),Single) + off )
       ELSEWHERE
          res(:) = var(:)
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_or_si_in_code_1
  !
  !! Konvertiere einen ganzzahligen 2D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_in_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_in )
          res(:,:) = INT( REAL(csi*cpl,Single)*REAL(var(:,:),Single) + off )
       ELSEWHERE
          res(:,:) = var(:,:)
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_or_si_in_code_2
  !
  !! Konvertiere einen ganzzahligen 3D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_in_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:,:,:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_in )
          res(:,:,:) = INT( REAL(csi*cpl,Single)*REAL(var(:,:,:),Single) + off )
       ELSEWHERE
          res(:,:,:) = var(:,:,:)
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_or_si_in_code_3
  !
  !! Konvertiere einen reellwertigen Skalar (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_re_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       IF ( var /= c_threshold_re ) THEN
          res = REAL(csi*cpl,Single)*var + off
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_or_si_re_code_0
  !
  !! Konvertiere einen reellwertigen 1D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_re_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_re )
          res(:) = REAL(csi*cpl,Single)*var(:) + off
       ELSEWHERE
          res(:) = var(:)
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_or_si_re_code_1
  !
  !! Konvertiere einen reellwertigen 2D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_re_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_re )
          res(:,:) = REAL(csi*cpl,Single)*var(:,:) + off
       ELSEWHERE
          res(:,:) = var(:,:)
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_or_si_re_code_2
  !
  !! Konvertiere einen reellwertigen 3D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_re_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_re )
          res(:,:,:) = REAL(csi*cpl,Single)*var(:,:,:) + off
       ELSEWHERE
          res(:,:,:) = var(:,:,:)
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_or_si_re_code_3
  !
  !
  !! Konvertiere einen doppeltgenauen Skalar (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_dp_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=DOUBLE) :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       IF ( var /= c_threshold_dp ) THEN
          res = csi*cpl*var + off
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_or_si_dp_code_0
  !
  !! Konvertiere einen doppeltgenauen 1D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_dp_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_dp )
          res(:) = csi*cpl*var(:) + off
       ELSEWHERE
          res(:) = var(:)
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_or_si_dp_code_1
  !
  !! Konvertiere einen doppeltgenauen 2D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_dp_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_dp )
          res(:,:) = csi*cpl*var(:,:) + off
       ELSEWHERE
          res(:,:) = var(:,:)
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_or_si_dp_code_2
  !
  !! Konvertiere einen doppeltgenauen 3D-Vektor (Original -> SI)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_dp_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_dp )
          res(:,:,:) = csi*cpl*var(:,:,:) + off
       ELSEWHERE
          res(:,:,:) = var(:,:,:)
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_or_si_dp_code_3
  !
  !! Konvertiere einen ganzzahligen Skalar (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_in_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_or_si_in_code_0 ( code, var )
    !
  END FUNCTION convert_or_si_in_descr_0
  !
  !! Konvertiere einen ganzzahligen 1D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_in_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_or_si_in_code_1 ( code, var(:) )
    !
  END FUNCTION convert_or_si_in_descr_1
  !
  !! Konvertiere einen ganzzahligen 2D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_in_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_or_si_in_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_or_si_in_descr_2
  !
  !! Konvertiere einen ganzzahligen 3D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_in_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:,:,:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_or_si_in_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_or_si_in_descr_3
  !
  !! Konvertiere einen reellwertigen Skalar (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_re_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_or_si_re_code_0 ( code, var )
    !
  END FUNCTION convert_or_si_re_descr_0
  !
  !! Konvertiere einen reellwertigen 1D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_re_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_or_si_re_code_1 ( code, var(:) )
    !
  END FUNCTION convert_or_si_re_descr_1
  !
  !! Konvertiere einen reellwertigen 2D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_re_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_or_si_re_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_or_si_re_descr_2
  !
  !! Konvertiere einen reellwertigen 3D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_re_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_or_si_re_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_or_si_re_descr_3
  !
  !! Konvertiere einen doppeltgenauen Skalar (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_dp_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=DOUBLE) :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_or_si_dp_code_0 ( code, var )
    !
  END FUNCTION convert_or_si_dp_descr_0
  !
  !! Konvertiere einen doppeltgenauen 1D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_dp_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_or_si_dp_code_1 ( code, var(:) )
    !
  END FUNCTION convert_or_si_dp_descr_1
  !
  !! Konvertiere einen doppeltgenauen 2D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_dp_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_or_si_dp_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_or_si_dp_descr_2
  !
  !! Konvertiere einen doppeltgenauen 3D-Vektor (Original -> SI)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_si_dp_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_or_si_dp_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_or_si_dp_descr_3
  !
  !! Konvertiere einen ganzzahligen Skalar (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_in_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl  ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       IF ( var /= c_threshold_in ) THEN
          res = INT( REAL(cpl,Single)*REAL(var,Single) )
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_or_pl_in_code_0
  !
  !! Konvertiere einen ganzzahligen 1D-Vektor (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_in_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_in )
          res(:) = INT( REAL(cpl,Single)*REAL(var(:),Single) )
       ELSEWHERE
          res(:) = var(:)
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_or_pl_in_code_1
  !
  !! Konvertiere einen ganzzahligen 2D-Vektor (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_in_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_in )
          res(:,:) = INT( REAL(cpl,Single)*REAL(var(:,:),Single) )
       ELSEWHERE
          res(:,:) = var(:,:)
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_or_pl_in_code_2
  !
  !! Konvertiere einen ganzzahligen 3D-Vektor (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_in_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:,:,:) ! 
    !! nach SI umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_in )
          res(:,:,:) = INT( REAL(cpl,Single)*REAL(var(:,:,:),Single) )
       ELSEWHERE
          res(:,:,:) = var(:,:,:)
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_or_pl_in_code_3
  !
  !! Konvertiere einen reellwertigen Skalar (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_re_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       IF ( var /= c_threshold_re ) THEN
          res = REAL(cpl,Single)*var
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_or_pl_re_code_0
  !
  !! Konvertiere einen reellwertigen 1D-Vektor (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_re_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_re )
          res(:) = REAL(cpl,Single)*var(:) 
       ELSEWHERE
          res(:) = var(:)
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_or_pl_re_code_1
  !
  !! Konvertiere einen reellwertigen 2D-Vektor (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_re_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_re )
          res(:,:) = REAL(cpl,Single)*var(:,:)
       ELSEWHERE
          res(:,:) = var(:,:)
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_or_pl_re_code_2
  !
  !! Konvertiere einen reellwertigen 3D-Vektor (Original -> PLot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_re_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:,:) ! 
    !! nach SI umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_re )
          res(:,:,:) = REAL(cpl,Single)*var(:,:,:)
       ELSEWHERE
          res(:,:,:) = var(:,:,:)
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_or_pl_re_code_3
  !
  !! Konvertiere einen doppeltgenauen Skalar (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_dp_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=DOUBLE) :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl  ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       IF ( var /= c_threshold_dp ) THEN
          res = cpl*var
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_or_pl_dp_code_0
  !
  !! Konvertiere einen doppeltgenauen 1D-Vektor (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_dp_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_dp )
          res(:) = cpl*var(:) 
       ELSEWHERE
          res(:) = var(:)
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_or_pl_dp_code_1
  !
  !! Konvertiere einen doppeltgenauen 2D-Vektor (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_dp_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_dp )
          res(:,:) = cpl*var(:,:)
       ELSEWHERE
          res(:,:) = var(:,:)
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_or_pl_dp_code_2
  !
  !! Konvertiere einen doppeltgenauen 3D-Vektor (Original -> Plot)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_dp_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:,:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: cpl ! 
    !
    cpl = get_phy_quant_plot_factor   ( code )
    !
    IF ( cpl /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_dp )
          res(:,:,:) = cpl*var(:,:,:)
       ELSEWHERE
          res(:,:,:) = var(:,:,:)
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_or_pl_dp_code_3
  !
  !! Konvertiere einen ganzzahligen Skalar (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_in_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var ! 
    !! Nach Plot umgerechnete Daten
    INTEGER :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_or_pl_in_code_0 ( code, var )
    !
  END FUNCTION convert_or_pl_in_descr_0
  !
  !! Konvertiere einen ganzzahligen 1D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_in_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Nach Plot umgerechnete Daten
    INTEGER :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_or_pl_in_code_1 ( code, var(:) )
    !
  END FUNCTION convert_or_pl_in_descr_1
  !
  !! Konvertiere einen ganzzahligen 2D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_in_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !! Nach Plot umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_or_pl_in_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_or_pl_in_descr_2
  !
  !! Konvertiere einen ganzzahligen 3D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_in_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    INTEGER , INTENT(IN) :: var(:,:,:) ! 
    !! Nach Plot umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_or_pl_in_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_or_pl_in_descr_3
  !
  !! Konvertiere einen reellwertigen Skalar (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_re_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=Single) :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_or_pl_re_code_0 ( code, var )
    !
  END FUNCTION convert_or_pl_re_descr_0
  !
  !! Konvertiere einen reellwertigen 1D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_re_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_or_pl_re_code_1 ( code, var(:) )
    !
  END FUNCTION convert_or_pl_re_descr_1
  !
  !! Konvertiere einen reellwertigen 2D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_re_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_or_pl_re_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_or_pl_re_descr_2
  !
  !! Konvertiere einen reellwertigen 3D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_re_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:,:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_or_pl_re_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_or_pl_re_descr_3
  !
  !! Konvertiere einen doppeltgenauen Skalar (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_dp_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=DOUBLE) :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_or_pl_dp_code_0 ( code, var )
    !
  END FUNCTION convert_or_pl_dp_descr_0
  !
  !! Konvertiere einen doppeltgenauen 1D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_dp_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_or_pl_dp_code_1 ( code, var(:) )
    !
  END FUNCTION convert_or_pl_dp_descr_1
  !
  !! Konvertiere einen doppeltgenauen 2D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_dp_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_or_pl_dp_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_or_pl_dp_descr_2
  !
  !! Konvertiere einen doppeltgenauen 3D-Vektor (Original -> Plot)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_or_pl_dp_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Original-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:,:) ! 
    !! Nach Plot umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_or_pl_dp_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_or_pl_dp_descr_3
  !
  !
  !! Konvertiere einen ganzzahligen Skalar (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_in_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    INTEGER , INTENT(IN) :: var ! 
    !! nach Original umgerechnete Daten
    INTEGER :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       IF ( var /= c_threshold_in ) THEN
          res = INT( (REAL(var,Single)-off)/REAL(csi*cpl,Single) )
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_si_or_in_code_0
  !
  !! Konvertiere einen ganzzahligen 1D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_in_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    INTEGER , INTENT(IN) :: var(:) ! 
    !! nach Original umgerechnete Daten
    INTEGER :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_in )
          res(:) = INT( (REAL(var(:),Single)-off)/REAL(csi*cpl,Single) )
       ELSEWHERE
          res(:) = var
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_si_or_in_code_1
  !
  !! Konvertiere einen ganzzahligen 2D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_in_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !! nach Original umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_in )
          res(:,:) = INT( (REAL(var(:,:),Single)-off)/REAL(csi*cpl,Single) )
       ELSEWHERE
          res(:,:) = var
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_si_or_in_code_2
  !
  !! Konvertiere einen ganzzahligen 3D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_in_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    INTEGER , INTENT(IN) :: var(:,:,:) ! 
    !! nach Original umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_in )
          res(:,:,:) = INT( (REAL(var(:,:,:),Single)-off)/REAL(csi*cpl,Single) )
       ELSEWHERE
          res(:,:,:) = var
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_si_or_in_code_3
  !
  !! Konvertiere einen reellwertigen Skalar (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_re_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    REAL (KIND=Single) , INTENT(IN) :: var ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=Single) :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       IF ( var /= c_threshold_re ) THEN
          res = (var-off)/REAL(csi*cpl,Single)
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_si_or_re_code_0
  !
  !! Konvertiere einen reellwertigen 1D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_re_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Original-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_re )
          res(:) = (var(:)-off)/REAL(csi*cpl,Single)
       ELSEWHERE
          res(:) = var(:)
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_si_or_re_code_1
  !
  !! Konvertiere einen reellwertigen 2D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_re_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_re )
          res(:,:) = (var(:,:)-off)/REAL(csi*cpl,Single)
       ELSEWHERE
          res(:,:) = var(:,:)
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_si_or_re_code_2
  !
  !! Konvertiere einen reellwertigen 3D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_re_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:,:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_re )
          res(:,:,:) = (var(:,:,:)-off)/REAL(csi*cpl,Single)
       ELSEWHERE
          res(:,:,:) = var(:,:,:)
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_si_or_re_code_3
  !
  !
  !! Konvertiere einen doppeltgenauen Skalar (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_dp_code_0 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=DOUBLE) :: res  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       IF ( var /= c_threshold_dp ) THEN
          res = (var-off)/csi*cpl
       ELSE
          res = var
       END IF
    ELSE
       res = var
    END IF
    !
  END FUNCTION convert_si_or_dp_code_0
  !
  !! Konvertiere einen doppeltgenauen 1D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_dp_code_1 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:) /= c_threshold_dp )
          res(:) = (var(:)-off)/csi*cpl
       ELSEWHERE
          res(:) = var(:)
       END WHERE
    ELSE
       res(:) = var(:)
    END IF
    !
  END FUNCTION convert_si_or_dp_code_1
  !
  !! Konvertiere einen doppeltgenauen 2D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_dp_code_2 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:) /= c_threshold_dp )
          res(:,:) = (var(:,:)-off)/csi*cpl
       ELSEWHERE
          res(:,:) = var(:,:)
       END WHERE
    ELSE
       res(:,:) = var(:,:)
    END IF
    !
  END FUNCTION convert_si_or_dp_code_2
  !
  !! Konvertiere einen doppeltgenauen 3D-Vektor (SI -> Original)
  !! bei Vorgabe der Code-Kennung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_dp_code_3 ( code, var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! SI-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:,:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    REAL (KIND=DOUBLE) :: csi, cpl, off ! 
    !
    csi = get_phy_unit_si_base_convf  ( code )
    cpl = 1.0_Double ! get_phy_quant_plot_factor   ( code )
    off = get_phy_unit_si_base_offset ( code )
    !
    IF ( cpl /= 0.0_Double .AND. csi /= 0.0_Double ) THEN
       WHERE ( var(:,:,:) /= c_threshold_dp )
          res(:,:,:) = (var(:,:,:)-off)/csi*cpl
       ELSEWHERE
          res(:,:,:) = var(:,:,:)
       END WHERE
    ELSE
       res(:,:,:) = var(:,:,:)
    END IF
    !
  END FUNCTION convert_si_or_dp_code_3
  !
  !! Konvertiere einen ganzzahligen Skalar (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_in_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    INTEGER , INTENT(IN) :: var ! 
    !! nach Original umgerechnete Daten
    INTEGER :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_si_or_in_code_0 ( code, var )
    !
  END FUNCTION convert_si_or_in_descr_0
  !
  !! Konvertiere einen ganzzahligen 1D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_in_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    INTEGER , INTENT(IN) :: var(:) ! 
    !! nach Original umgerechnete Daten
    INTEGER :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_si_or_in_code_1 ( code, var(:) )
    !
  END FUNCTION convert_si_or_in_descr_1
  !
  !! Konvertiere einen ganzzahligen 2D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_in_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !! nach Original umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_si_or_in_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_si_or_in_descr_2
  !
  !! Konvertiere einen ganzzahligen 3D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_in_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    INTEGER , INTENT(IN) :: var(:,:,:) ! 
    !! nach Original umgerechnete Daten
    INTEGER :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_si_or_in_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_si_or_in_descr_3
  !
  !! Konvertiere einen reellwertigen Skalar (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_re_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    REAL (KIND=Single) , INTENT(IN) :: var ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=Single) :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_si_or_re_code_0 ( code, var )
    !
  END FUNCTION convert_si_or_re_descr_0
  !
  !! Konvertiere einen reellwertigen 1D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_re_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_si_or_re_code_1 ( code, var(:) )
    !
  END FUNCTION convert_si_or_re_descr_1
  !
  !! Konvertiere einen reellwertigen 2D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_re_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_si_or_re_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_si_or_re_descr_2
  !
  !! Konvertiere einen reellwertigen 3D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_re_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    REAL (KIND=Single) , INTENT(IN) :: var(:,:,:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=Single) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_si_or_re_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_si_or_re_descr_3
  !
  !! Konvertiere einen doppeltgenauen Skalar (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_dp_descr_0 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=DOUBLE) :: res  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code = get_phy_quant_code_0 ( descr )
    res  = convert_si_or_dp_code_0 ( code, var )
    !
  END FUNCTION convert_si_or_dp_descr_0
  !
  !! Konvertiere einen doppeltgenauen 1D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_dp_descr_1 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code   = get_phy_quant_code_0 ( descr )
    res(:) = convert_si_or_dp_code_1 ( code, var(:) )
    !
  END FUNCTION convert_si_or_dp_descr_1
  !
  !! Konvertiere einen doppeltgenauen 2D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_dp_descr_2 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code     = get_phy_quant_code_0 ( descr )
    res(:,:) = convert_si_or_dp_code_2 ( code, var(:,:) )
    !
  END FUNCTION convert_si_or_dp_descr_2
  !
  !! Konvertiere einen doppeltgenauen 3D-Vektor (SI -> Original)
  !! bei Vorgabe der Beschreibung der physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen 
  FUNCTION convert_si_or_dp_descr_3 ( descr, var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! SI-Daten
    REAL (KIND=DOUBLE) , INTENT(IN) :: var(:,:,:) ! 
    !! nach Original umgerechnete Daten
    REAL (KIND=DOUBLE) :: res(SIZE(var,1),SIZE(var,2),SIZE(var,3))  ! 
    ! Hilfsvariablen
    INTEGER :: code  ! 
    !
    code       = get_phy_quant_code_0 ( descr )
    res(:,:,:) = convert_si_or_dp_code_3 ( code, var(:,:,:) )
    !
  END FUNCTION convert_si_or_dp_descr_3
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-IS-Methoden <<< [ERR_NO = ????? bis ?????]
  ! ----------------------------------------------------------------------
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e g&uuml;ltig ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_valid_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( get_phy_quants_idx( var ) > 0 )
    !
  END FUNCTION is_phy_quant_valid_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en g&uuml;ltig sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_valid_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_valid_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_valid_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e g&uuml;ltig ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_valid_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( get_phy_quants_idx( var ) > 0 )
    !
  END FUNCTION is_phy_quant_valid_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en g&uuml;ltig sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_valid_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_valid_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_valid_descr_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e ein Skalar ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_scalar_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%typ == 1 )
    !
  END FUNCTION is_phy_quant_scalar_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en Skalare sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_scalar_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_scalar_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_scalar_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e ein Skalar ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_scalar_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%typ == 1 )
    !
  END FUNCTION is_phy_quant_scalar_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en Skalare sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_scalar_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_scalar_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_scalar_descr_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e ein Vektor ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_vector_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%typ == 2 )
    !
  END FUNCTION is_phy_quant_vector_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en Vektoren sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_vector_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_vector_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_vector_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e ein Vektor ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_vector_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%typ == 2 )
    !
  END FUNCTION is_phy_quant_vector_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en Vektoren sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_vector_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_vector_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_vector_descr_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e ein Tensor ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_tensor_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%typ == 3 )
    !
  END FUNCTION is_phy_quant_tensor_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en Tensoren sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_tensor_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_tensor_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_tensor_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e ein Tensor ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_tensor_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%typ == 3 )
    !
  END FUNCTION is_phy_quant_tensor_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en Tensoren sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_tensor_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_tensor_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_tensor_descr_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e ein Anderes ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_other_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%typ == -1 )
    !
  END FUNCTION is_phy_quant_other_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en Andere sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_other_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_other_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_other_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e ein Anderes ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_other_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%typ == -1 )
    !
  END FUNCTION is_phy_quant_other_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en Andere sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_other_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_other_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_other_descr_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e <EM>zeitunabh&auml;ngig</EM> ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_notim_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%zta == 0 )
    !
  END FUNCTION is_phy_quant_notim_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en <EM>zeitunabh&auml;ngig</EM> sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_notim_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_notim_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_notim_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e <EM>zeitunabh&auml;ngig</EM> ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_notim_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%zta == 0 )
    !
  END FUNCTION is_phy_quant_notim_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en <EM>zeitunabh&auml;ngig</EM> sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_notim_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_notim_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_notim_descr_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e <EM>synoptisch</EM> ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_synop_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%zta == 1 )
    !
  END FUNCTION is_phy_quant_synop_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en <EM>synoptisch</EM> sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_synop_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_synop_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_synop_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e <EM>synoptisch</EM> ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_synop_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%zta == 1 )
    !
  END FUNCTION is_phy_quant_synop_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en <EM>synoptisch</EM> sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_synop_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_synop_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_synop_descr_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e <EM>periodenbezogen</EM> ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_perio_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%zta == 2 )
    !
  END FUNCTION is_phy_quant_perio_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en <EM>periodenbezogen</EM> sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_perio_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_perio_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_perio_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e <EM>periodenbezogen</EM> ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_perio_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%zta == 2 )
    !
  END FUNCTION is_phy_quant_perio_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en <EM>periodenbezogen</EM> sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_perio_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_perio_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_perio_descr_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e <EM>Flu&szlig;gr&ouml;&szlig;e</EM> ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_flux_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%flx
    !
  END FUNCTION is_phy_quant_flux_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en <EM>Flu&szlig;gr&ouml;&szlig;e</EM> sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_flux_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_flux_code_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_flux_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e <EM>Flu&szlig;gr&ouml;&szlig;e</EM> ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_flux_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = phy_quants(idx)%flx
    !
  END FUNCTION is_phy_quant_flux_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en <EM>Flu&szlig;gr&ouml;&szlig;e</EM> sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION is_phy_quant_flux_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = is_phy_quant_flux_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION is_phy_quant_flux_descr_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-HAS-Methoden <<< [ERR_NO = ????? bis ?????]
  ! ----------------------------------------------------------------------
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e eine
  !! Referenzgr&ouml;&szlig;e erfordert oder nicht          <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION has_phy_quant_ref_quant_code_0 ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%ref_code > 0 )
    !
  END FUNCTION has_phy_quant_ref_quant_code_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en 
  !! Referenzgr&ouml;&szlig;en erfordern oder nicht         <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION has_phy_quant_ref_quant_code_1 ( var ) &
       RESULT( res )
    !! Code-Kennung vieler physikalischer Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = has_phy_quant_ref_quant_code_0 ( var(i) )
    END DO
    !
  END FUNCTION has_phy_quant_ref_quant_code_1
  !
  !! Ermittele, ob eine physikalische Gr&ouml;&szlig;e eine
  !! Referenzgr&ouml;&szlig;e erfordert oder nicht <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION has_phy_quant_ref_quant_descr_0 ( var ) &
       RESULT( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_phy_quants_idx( var )
    !
    IF ( idx > 0 ) res = ( phy_quants(idx)%ref_code > 0 )
    !
  END FUNCTION has_phy_quant_ref_quant_descr_0
  !
  !! Ermittele, ob viele physikalische Gr&ouml;&szlig;en  <BR>
  !! Referenzgr&ouml;&szlig;en erfordern oder nicht <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION has_phy_quant_ref_quant_descr_1 ( var ) &
       RESULT( res )
    !! Beschreibungen vieler physikalischer Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = has_phy_quant_ref_quant_descr_0 ( var(i) )
    END DO
    !
  END FUNCTION has_phy_quant_ref_quant_descr_1
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+                                                                     +
  !+     PPPPPP   RRRRRR   III  VVV  VVV     AA    TTTTTTT  EEEEEEE      +
  !+     PPP  PP  RRR  RR  III  VVV  VVV    AAAA     TTT    EEE          +
  !+     PPP  PP  RRR  RR  III  VVV  VVV   AAA AA    TTT    EEE          +
  !+     PPPPPP   RRRRRR   III  VVV  VVV  AAA  AAA   TTT    EEEEEE       +
  !+     PPP      RRRRR    III  VVV  VVV  AAA  AAA   TTT    EEE          +
  !+     PPP      RRR RR   III   VVV VV   AAAAAAAA   TTT    EEE          +
  !+     PPP      RRR  RR  III    VVVV    AAA  AAA   TTT    EEE          +
  !+     PPP      RRR   RR III     VV     AAA  AAA   TTT    EEEEEEE      +
  !+                                                                     +
  !+                                                                     +
  !+   MM     MM  EEEEEEE TTTTTTT  HHH  HH   OOOOO   DDDDDD    SSSSS     +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS   SS    +
  !+   MMMM MMMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS         +
  !+   MMM M MMM  EEEEEE    TTT    HHHHHHH  OOO  OO  DDD  DD   SSSSSS    +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SSS  SSS   +
  !+   MMM   MMM  EEEEEEE   TTT    HHH  HH   OOOOO   DDDDDD    SSSSSS    +
  !+                                                                     +
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Lokale Methoden:
  !
  ! ----------------------------------------------------------------------
  ! >>> ALLGEMEIN-Methoden <<< [ERR_NO =  0001 bis  0999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_initialised ( upname )         &
       RESULT( ok )
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    ! lokale Variablen
    !! Fehlernummer
    INTEGER            :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "b_phy" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_phy ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION not_initialised ( upname )          &
       RESULT( ok )
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .NOT. initialised
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_phy_all_errors ( )
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='init_phy_all_errors' !
    !! Z&auml;hlervariable
    INTEGER :: i, ic ! 
    !
    DO i=1,2
       !
       ic = 0
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 1 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist nicht initialisiert\n'//&
               '--> INIT_phy ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_phy ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "code"\n'//&
               'Wert muss groesser als Null sein\n'//&
               'aktueller Wert = <code>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "id(:)"\n'//&
               'Komponente darf nicht leer sein\n'//&
               'aktueller Wert id(1) = <id(1)>\n'//&
               'aktueller Wert id(2) = <id(2)>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "descr(:)"\n'//&
               'Komponente darf nicht leer sein\n'//&
               'aktueller Wert descr(1) = <descr(1)>\n'//&
               'aktueller Wert descr(2) = <descr(2)>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "unit_code"\n'//&
               'Wert muss in "phy_units(:)" vorkommen\n'//&
               'aktueller Wert = <unit_code>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "class_code"\n'//&
               'Wert muss in "phy_units(:)" vorkommen\n'//&
               'aktueller Wert = <class_code>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "ref_code"\n'//&
               'Wert muss in "phy_quants(:)" vorkommen\n'//&
               'aktueller Wert = <ref_code>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "typ"\n'//&
               'Wert muss 1, 2 oder 3 sein\n'//&
               'aktueller Wert = <typ>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "zta"\n'//&
               'Wert muss 0, 1 oder 2 sein\n'//&
               'aktueller Wert = <zta>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "mzr"\n'//&
               'Wert muss 0, 1, 2, 3, 4, 5 oder 6 sein\n'//&
               'aktueller Wert = <mzr>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "mul"\n'//&
               'Wert muss groesser als Null sein\n'//&
               'aktueller Wert = <mul>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "ele"\n'//&
               'aktueller Wert = <ele>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6120 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_quant"\n'//&
               'Typ-Komponente = "gen"\n'//&
               'aktueller Wert = <gen>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in mindestens einer Komponente von "t_phy_quant"\n'//&
               'Descr  = <descr>\n'//&
               'Code   = <code>\n'//&
               'Status[Code]      = <StatusCode>\n'//&
               'Status[Id]        = <StatusId>\n'//&
               'Status[Descr]     = <StatusDescr>\n'//&
               'Status[UnitCode]  = <StatusUnitCode>\n'//&
               'Status[ClassCode] = <StatusClassCode>\n'//&
               'Status[RefCode]   = <StatusRefCode>\n'//&
               'Status[Typ]       = <StatusTyp>\n'//&
               'Status[Zta]       = <StatusZta>\n'//&
               'Status[Mzr]       = <StatusMzr>\n'//&
               'Status[Mul]       = <StatusMul>\n'//&
               'Status[Ele]       = <StatusEle>\n'//&
               'Status[Gen]       = <StatusGen>\n'//&
               'Status[Flx]       = <StatusFlx>\n'//&
               '--> vorangehende Fehlermeldungen pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6201 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in mindestens einer Komponente von "t_phy_standards"\n'//&
               'Status[Code]         = <StatusCode>\n'//&
               'Status[Id]           = <StatusIdim>\n'//&
               'Status[Descr]        = <StatusIvar>\n'//&
               'Status[StandardName] = <StatusStandardName>\n'//&
               '--> vorangehende Fehlermeldungen pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6310 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_standard"\n'//&
               'Typ-Komponente = "code"\n'//&
               'Wert muss groesser als Null und als phys. Code definiert sein\n'//&
               'aktueller Wert = <code>\n'//&
               'Code definiert = <valid>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6320 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_standard"\n'//&
               'Typ-Komponente = "idim"\n'//&
               'vekt. Groesse  = <IsVector>\n'//&
               'Code o.k.      = <okCode>\n'//&
               'Idim o.k.      = <okIdim>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6330 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_standard"\n'//&
               'Typ-Komponente = "ivar"\n'//&
               'Code o.k.      = <okCode>\n'//&
               'Ivar o.k.      = <okIvar>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6340 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phy_standard"\n'//&
               'Typ-Komponente = "ivar"\n'//&
               'Code o.k.      = <okCode>\n'//&
               'Name o.k.      = <okName>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_phy"\n'//&
               'aktuelle Komponente = "<component>"\n'//&
               '--> Code in Modul "b_phy" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: interne GET-Methoden\n'//&
               'Fehler beim bestimmen eines IDX in Feld "phy_quants(:)"\n'//&
               'aktuelle Id/Code = <Id_or_Code>\n'//&
               '--> pruefen ob fuer Id/Code eine physikalische Groesse vorliegt' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -9000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: interne GET-Methoden\n'//&
               'Fehler beim bestimmen eines IDX in Feld "phy_units(:)"\n'//&
               'aktuelle Id/Code = <Id_or_Code>\n'//&
               '--> pruefen ob fuer Id/Code eine physikalische Einheit vorliegt' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -9010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: interne GET-Methoden\n'//&
               'Fehler beim Ermitteln der Anzahl der physikalischen Groessen\n'//&
               'Datei = <filename>\n'//&
               '--> Inhalt der genannten Datei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -9020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: interne GET-Methoden\n'//&
               'es liegen keine physikalischen Groessen in der Datei vor\n'//&
               'Datei = <filename>\n'//&
               '--> Inhalt der genannten Datei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -9030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: interne GET-Methoden\n'// &
               'Konfigurationsdatei ist weder in CFGDIR noch im Arbeitsverzeichnis vorhanden:\n'// &
               'CFGDIR = <pathname>\n'// &
               'Datei  = <filename>\n'// &
               '--> Umgebungsvariable CFGDIR ggf. mit passenden Pfad setzen:\n'// &
               '    (auf einem WIN-PC z.B. in System -> Erweitert -> Umgebungsvariablen)"\n'// &
               '--> alternativ die Datei ins Arbeitsverzeichnis kopieren')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -20000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: interne READ-Methoden\n'//&
               'es trat ein Fehler beim Lesen einer Zeile aus Datei auf\n'//&
               'Datei = <filename>\n'//&
               '--> Inhalt der genannten Datei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -20010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: interne READ-Methoden\n'//&
               'der Inhalt vergleichbarer Zeilen in Dateien ist nicht konsistent\n'//&
               'Datei(1) = <filename1>\n'//&
               'Datei(2) = <filename2>\n'//&
               'Datei(3) = <filename3>\n'//&
               'Zeile(1) = <line1>\n'//&
               'Zeile(2) = <line2>\n'//&
               'Zeile(3) = <line3>\n'//&
               '--> Inhalt der genannten Datei(en) pruefen' )
       END IF
       !
       ! Allokieren der Felder beim ersten Durchlauf (i==1)
       !
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL NEW_error( all_errors(:) )
       END IF
       !
    END DO
    !
  END SUBROUTINE init_phy_all_errors
  !
  ! --- EINHEITEN ---------------------------------------------------------
  !
  !! Initialisieren aller benutzten physikalischen Einheiten und Definieren
  !! des Zusammenhangs mit den SI-Einheiten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phy_all_units_d ( ) ! >GL>
    !
    TYPE (t_file) :: cfg_file ! 
    INTEGER       :: i, nof   ! 
    !
    ! Ermittle die Namen der Konfigurationsdateien und oeffne die Datei ---
    cfg_file = get_phy_cfg_file( c_cfg_files(5) ) ! "phydef.cfg.si.dat"
    ! Ermittle die Anzahl der SI-Definitionen auf Datei -------------------
    nof      = get_phy_cfg_file_nof_quants_d ( cfg_file )
    ! Allokieren/Initialisieren der Felder
    IF ( no_error( ) ) THEN
       ALLOCATE ( phy_units( nof ) )
       DO i=1,nof
          CALL init_phy_unit &
               ( phy_units(i), 0, c_undefined, c_undefined, c_undefined, c_undefined )
       END DO
    END IF
    ! Lesen aller Zeilen der SI-Konfigurationsdatei
    IF ( no_error( ) ) THEN
       CALL open_file ( cfg_file )
       DO i=1,nof
          IF ( any_error( ) ) EXIT
          CALL read_phy_cfg_si_line &
               ( cfg_file, phy_units(i) )
       END DO
       CALL close_file ( cfg_file )
    END IF
    !
  END SUBROUTINE init_phy_all_units_d
  !
  !! Initialisieren einer physikalischen Einheit <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phy_unit_d &
       ( this, code, id_de, descr_de, id_en, descr_en, m, kg, s, a, k, mol, cd, e, convf, offset )
    !! Objekt zur Ablage aller Informationen &uuml;ber physikalische Einheiten
    TYPE (t_phy_unit) , INTENT(INOUT) :: this     ! 
    !! Code-Kennung
    INTEGER           , INTENT(IN)    :: code     ! 
    !! physikalische Einheit (Deutsch)
    CHARACTER (LEN=*) , INTENT(IN)    :: id_de    ! 
    !! ausf&uuml;hrliche Beschreibung der physikalischen Einheit (Deutsch)
    CHARACTER (LEN=*) , INTENT(IN)    :: descr_de ! 
    !! physikalische Einheit (Englisch)
    CHARACTER (LEN=*) , INTENT(IN)    :: id_en    ! 
    !! ausf&uuml;hrliche Beschreibung der physikalischen Einheit (Englisch)
    CHARACTER (LEN=*) , INTENT(IN)    :: descr_en ! 
    !! Dimension der SI-Basiseinheit "m"
    REAL (KIND=Double), INTENT(IN) , OPTIONAL :: m   ! >GL> 
    !! Dimension der SI-Basiseinheit "kg"
    REAL (KIND=Double), INTENT(IN) , OPTIONAL :: kg  ! >GL>
    !! Dimension der SI-Basiseinheit "s"
    REAL (KIND=Double), INTENT(IN) , OPTIONAL :: s   ! >GL>
    !! Dimension der SI-Basiseinheit "A"
    REAL (KIND=Double), INTENT(IN) , OPTIONAL :: a   ! >GL>
    !! Dimension der SI-Basiseinheit "K"
    REAL (KIND=Double), INTENT(IN) , OPTIONAL :: k   ! >GL>
    !! Dimension der SI-Basiseinheit "Mol"
    REAL (KIND=Double), INTENT(IN) , OPTIONAL :: mol ! >GL>
    !! Dimension der SI-Basiseinheit "cd"
    REAL (KIND=Double), INTENT(IN) , OPTIONAL :: cd  ! >GL>
    !! Dimension der SI-Basiseinheit "E"
    REAL (KIND=Double), INTENT(IN) , OPTIONAL :: e   ! >GL>
    !! Konversionsfaktor zum Umrechnen in SI-Einheiten (Default = 1.0)
    REAL (KIND=DOUBLE), INTENT(IN) , OPTIONAL :: convf   ! 
    !! Verschiebung zum Nullpunkt (in SI-Einheiten) (Default = 0.0)
    REAL (KIND=DOUBLE), INTENT(IN) , OPTIONAL :: offset  ! 
    !
    !
    this%code     = code  
    this%id(:)    = REPEAT ( ' ', LEN(this%id)    ) ! 
    this%descr(:) = REPEAT ( ' ', LEN(this%descr) ) ! 
    this%base(:)  = 0.0_Double ! >GL>
    this%convf    = 1.0_Double
    this%offset   = 0.0_Double
    !
    IF ( LEN_TRIM(id_de   ) > 0 ) this%id(1)    = id_de(1:MIN(LEN_TRIM(id_de),LEN(this%id(1))))
    IF ( LEN_TRIM(id_en   ) > 0 ) this%id(2)    = id_en(1:MIN(LEN_TRIM(id_en),LEN(this%id(2))))
    IF ( LEN_TRIM(descr_de) > 0 ) this%descr(1) = descr_de(1:MIN(LEN_TRIM(descr_de),LEN(this%descr(1))))
    IF ( LEN_TRIM(descr_en) > 0 ) this%descr(2) = descr_en(1:MIN(LEN_TRIM(descr_en),LEN(this%descr(2))))
    !
    IF ( PRESENT( m      ) ) this%base(1) = m
    IF ( PRESENT( kg     ) ) this%base(2) = kg
    IF ( PRESENT( s      ) ) this%base(3) = s
    IF ( PRESENT( a      ) ) this%base(4) = a
    IF ( PRESENT( k      ) ) this%base(5) = k
    IF ( PRESENT( mol    ) ) this%base(6) = mol
    IF ( PRESENT( cd     ) ) this%base(7) = cd
    IF ( PRESENT( e      ) ) this%base(8) = e
    IF ( PRESENT( convf  ) ) this%convf   = convf 
    IF ( PRESENT( offset ) ) this%offset  = offset
    ! 
  END SUBROUTINE init_phy_unit_d
  !
  ! --- KLASSEN -----------------------------------------------------------
  !
  !! Initialisieren aller benutzten Klassen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phy_all_classes_d ( )
    !
    INTEGER :: i, j, ic ! 
    !
    DO i=1,2
       ic = 0
       !
       ! 001 bis 010 - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 01 : 'h_Flut '
          CALL init_phy_class ( phy_classes(ic), 01, &
               'h_Flut', 'flutbezogene Wasserstands-Groessen', &
               'h_Flut', 'flood oriented water level quantities' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 02 : 'h_Ebbe '
          CALL init_phy_class ( phy_classes(ic), 02, &
               'h_Ebbe', 'ebbebezogene Wasserstands-Groessen', &
               'h_Ebbe', 'ebb oriented water level quantities' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 03 : 'h_Tide '
          CALL init_phy_class ( phy_classes(ic), 03, &
               'h_Tide', 'tidebezogene Wasserstands-Groessen', &
               'h_Tide', 'tide oriented water level quantities' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 04 : 'v_Flut '
          CALL init_phy_class ( phy_classes(ic), 04, &
               'v_Flut', 'flutstrombezogene Groessen', &
               'v_Flut', 'flood current oriented quantities' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 05 : 'v_Ebbe '
          CALL init_phy_class ( phy_classes(ic), 05, &
               'v_Ebbe', 'ebbestrombezogene Groessen', &
               'v_Ebbe', 'ebb current oriented quantities' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 06 : 'v_Tide '
          CALL init_phy_class ( phy_classes(ic), 06, &
               'v_Tide', 'tidebezogene Stroemungs-Groessen', &
               'v_Tide', 'tide oriented current quantities' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 07 : 'Amplit '
          CALL init_phy_class ( phy_classes(ic), 07, &
               'Amplit', 'Partialtiden, amplitudenbezogen', &
               'Amplit', 'tidal constituents, amplitude related' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 08 : 'Phase  '
          CALL init_phy_class ( phy_classes(ic), 08, &
               'Phase', 'Partialtiden, phasenbezogen', &
               'Phase', 'tidal constituents, phase related' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 09 : 'AmpVst '
          CALL init_phy_class ( phy_classes(ic), 09, &
               'AmpVst', 'Partialtiden, Amplitudenverstaerkung', &
               'AmpVst', 'tidal constituents, amplitude amplification' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 10 : 'PhasDif'
          CALL init_phy_class ( phy_classes(ic), 10, &
               'PhasDif', 'Partialtiden, Phasendifferenz', &
               'PhasDif', 'tidal constituents, phase difference' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 11 : 'dAmplit'
          CALL init_phy_class ( phy_classes(ic), 11, &
               'dAmplit', 'Partialtiden, Differenz der Amplitude', &
               'dAmplit', 'tidal constituents, difference of amplitudes' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 12 : 'dPhase '
          CALL init_phy_class ( phy_classes(ic), 12, &
               'dPhase', 'Partialtiden, Differenz der Phasen', &
               'dPhase', 'tidal constituents, difference of phases' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 13 : 'dAmpVst'
          CALL init_phy_class ( phy_classes(ic), 13, &
               'dAmpVst', 'Partialtiden, Differenz der Amplitudenverstaerkung', &
               'dAmpVst', 'tidal constituents, difference of amplitude amplification' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 14 : 'dPhaDif'
          CALL init_phy_class ( phy_classes(ic), 14, &
               'dPhaDif', 'Partialtiden, Differenz der Phasendifferenz', &
               'dPhaDif', 'tidal constituents, difference of phase difference' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 15 : 'others'
          CALL init_phy_class ( phy_classes(ic), 15, &
               'others', 'nicht klassifizierte Groesse', &
               'others', 'unclassified quantity' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 16 : 'MetData'
          CALL init_phy_class ( phy_classes(ic), 16, &
               'MetData', 'meteorologische Groesse', &
               'MetData', 'meteorology related quantity' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 17 : 'Schweb '
          CALL init_phy_class ( phy_classes(ic), 17, &
               'Schweb', 'schwebstoffbezogene Groesse', &
               'Schweb', 'suspended sediment related quantity' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 18 : 'GeTrans'
          CALL init_phy_class ( phy_classes(ic), 18, &
               'GeTrans', 'geschiebetransportbezogene Groesse', &
               'GeTrans', 'bedload transport related quantity' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN ! code = 19 : 'GeKapaz'
          CALL init_phy_class ( phy_classes(ic), 19, &
               'GeKapaz', 'geschiebetransportkapazitaetbezogene Groesse', &
               'GeKapaz', 'bedload transport capacity related quantity' )
       END IF
       ! Allokieren/Initialisieren der Felder beim ersten Durchlauf (i==1)
       IF ( i == 1 ) THEN
          ALLOCATE ( phy_classes( ic ) )
          DO j=1,ic
             CALL init_phy_class &
                  ( phy_classes(j), 0, c_undefined, c_undefined, c_undefined, c_undefined )
          END DO
       END IF
    END DO
    !
  END SUBROUTINE init_phy_all_classes_d
  ! 
  !! Initialisieren einer Klasse <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phy_class_d &
       ( this, code, id_de, descr_de, id_en, descr_en )
    !! Objekt zur Ablage aller Informationen einer Klasse
    TYPE (t_phy_class) , INTENT(INOUT) :: this     ! 
    !! Code-Kennung
    INTEGER            , INTENT(IN)    :: code     ! 
    !! kurze Klassenbezeichnung (Deutsch)
    CHARACTER (LEN=*)  , INTENT(IN)    :: id_de    ! 
    !! ausf&uuml;hrliche Klassenbeschreibung (Deutsch)
    CHARACTER (LEN=*)  , INTENT(IN)    :: descr_de ! 
    !! kurze Klassenbezeichnung (Englisch)
    CHARACTER (LEN=*)  , INTENT(IN)    :: id_en    ! 
    !! ausf&uuml;hrliche Klassenbeschreibung (Englisch)
    CHARACTER (LEN=*)  , INTENT(IN)    :: descr_en ! 
    !
    !
    this%code     = code  
    this%id(:)    = REPEAT ( ' ', LEN(this%id)    ) ! 
    this%descr(:) = REPEAT ( ' ', LEN(this%descr) ) ! 
    !
    IF ( LEN_TRIM(id_de   ) > 0 ) this%id(1)    = id_de(1:MIN(LEN_TRIM(id_de),LEN(this%id(1))))
    IF ( LEN_TRIM(id_en   ) > 0 ) this%id(2)    = id_en(1:MIN(LEN_TRIM(id_en),LEN(this%id(2))))
    IF ( LEN_TRIM(descr_de) > 0 ) this%descr(1) = descr_de(1:MIN(LEN_TRIM(descr_de),LEN(this%descr(1))))
    IF ( LEN_TRIM(descr_en) > 0 ) this%descr(2) = descr_en(1:MIN(LEN_TRIM(descr_en),LEN(this%descr(2))))
    ! 
  END SUBROUTINE init_phy_class_d
  !
  ! --- QUANTITAETEN -------------------------------------------------------
  !
  !! Initialisieren aller benutzten physikalischen Gr&ouml;&szlig;en <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phy_all_quants_d ( )
    !
    TYPE (t_file) :: cfg_files(c_max_cfg_files) ! 
    INTEGER :: i,  nof                        ! 
    !
    ! Ermittle die Namen der Konfigurationsdateien und oeffne die Datei ---
    DO i=1,SIZE(c_cfg_files)
       IF ( any_error( ) ) EXIT
       cfg_files(i) = get_phy_cfg_file( c_cfg_files(i) )
    END DO
    ! Ermittle die Anzahl der physikalischen Groessen auf Datei 
    nof = get_phy_cfg_file_nof_quants_d ( cfg_files(1) )
    ! Allokieren/Initialisieren der Felder
    IF ( no_error( ) ) THEN
       ALLOCATE ( phy_quants( nof ) )
       DO i=1,nof
          CALL init_phy_quant &
               ( phy_quants(i), 0, c_undefined, c_undefined, c_undefined, c_undefined, &
               '-', 15, -1, -1, -1, -1, 0.0_Double, -1, -1, .false. )
       END DO
    END IF
    ! Lesen aller Zeilen der Konfigurationsdateien
    IF ( no_error( ) ) THEN
       CALL open_file ( cfg_files(1:3) )
       DO i=1,nof
          IF ( any_error( ) ) EXIT
          CALL read_phy_cfg_files_line &
               ( cfg_files(1:3), phy_quants(i) )
       END DO
       CALL close_file ( cfg_files(1:3) )
    END IF
    !
  END SUBROUTINE init_phy_all_quants_d
  ! 
  !! Initialisieren einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phy_quant_d &
       ( this, code, id_de, descr_de, id_en, descr_en, &
         unit_id, class_code, ref_code, typ, zta, mzr, mul, ele, gen, flx )
    !! Objekt zur Ablage aller Informationen einer Klasse
    TYPE (t_phy_quant) , INTENT(INOUT) :: this       ! 
    !! Code-Kennung
    INTEGER            , INTENT(IN)    :: code       ! 
    !! kurze Klassenbezeichnung (Deutsch)
    CHARACTER (LEN=*)  , INTENT(IN)    :: id_de      ! 
    !! ausf&uuml;hrliche Klassenbeschreibung (Deutsch)
    CHARACTER (LEN=*)  , INTENT(IN)    :: descr_de   ! 
    !! kurze Klassenbezeichnung (Englisch)
    CHARACTER (LEN=*)  , INTENT(IN)    :: id_en      ! 
    !! ausf&uuml;hrliche Klassenbeschreibung (Englisch)
    CHARACTER (LEN=*)  , INTENT(IN)    :: descr_en   ! 
    !! Bezeichnung der physikalischen Einheit
    CHARACTER (LEN=*)  , INTENT(IN)    :: unit_id    ! 
    !! Code-Kennung zur Klassenzugeh&ouml;rigkeit
    INTEGER            , INTENT(IN)    :: class_code ! 
    !! Code-Kennung zur physikalischen Referenzgr&ouml;&szlig;e
    INTEGER            , INTENT(IN)    :: ref_code   ! 
    !! Typ (1,2,3)
    INTEGER            , INTENT(IN)    :: typ        ! 
    !! Zeitabh&auml;ngigkeit (0,1,2)
    INTEGER            , INTENT(IN)    :: zta        ! 
    !! Plottindikator (0,1,2,...,6)
    INTEGER            , INTENT(IN)    :: mzr        ! 
    !! Multiplikator f&uuml;r das Plotten der Daten
    REAL (KIND=DOUBLE) , INTENT(IN)    :: mul        ! 
    !! Code OpenMI-Gitter-ElementSets
    INTEGER            , INTENT(IN)    :: ele        ! 
    !! Code Aspekte OpenMI-Daten-ElementSets
    INTEGER            , INTENT(IN)    :: gen        ! 
    !! Indikatorvariable, Flu&szlig;gr&ouml;&szlig;e
    LOGICAL            , INTENT(IN)    :: flx        ! 
    !
    !! Hilfsvariablen
    INTEGER :: idx ! 
    !
    this%code     = code  
    this%id(:)    = REPEAT ( ' ', LEN(this%id)    ) ! 
    this%descr(:) = REPEAT ( ' ', LEN(this%descr) ) ! 
    !
    IF ( LEN_TRIM(id_de   ) > 0 ) this%id(1)    = id_de(1:MIN(LEN_TRIM(id_de),LEN(this%id(1))))
    IF ( LEN_TRIM(id_en   ) > 0 ) this%id(2)    = id_en(1:MIN(LEN_TRIM(id_en),LEN(this%id(2))))
    IF ( LEN_TRIM(descr_de) > 0 ) this%descr(1) = descr_de(1:MIN(LEN_TRIM(descr_de),LEN(this%descr(1))))
    IF ( LEN_TRIM(descr_en) > 0 ) this%descr(2) = descr_en(1:MIN(LEN_TRIM(descr_en),LEN(this%descr(2))))
    ! 
    idx = get_phy_units_idx( unit_id )
    IF ( idx > 0 ) this%unit_code = phy_units(idx)%code
    !
    this%class_code = class_code
    this%ref_code   = ref_code
    this%typ        = typ
    this%zta        = zta
    this%mzr        = mzr
    this%mul        = mul
    this%ele        = ele
    this%gen        = gen
    this%flx        = flx
    !
  END SUBROUTINE init_phy_quant_d
  !
  ! --- STANDARDBEZIEHUNGEN -----------------------------------------------
  !
  !! Initialisieren aller benutzten Informationsbeziehungen zu Standardgr&ouml;&szlig;en <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phy_all_standards_d ( )
    !
    TYPE (t_file) :: cfg_file ! 
    INTEGER :: i, nof      ! 
    !
    ! Ermittle den Namen der Konfigurationsdatei und oeffne die Datei ---
    cfg_file = get_phy_cfg_file( c_cfg_files(4) )
    ! Ermittle die Anzahl der Informationsbeziehungen auf Datei ---------
    nof      = get_phy_cfg_file_nof_quants_d ( cfg_file )
    ! Allokieren/Initialisieren der Felder
    IF ( no_error( ) ) THEN
       ALLOCATE ( phy_standards( nof ) )
       DO i=1,nof
          CALL init_phy_standard ( phy_standards(i), -1, -1, -1, c_undefined )
       END DO
    END IF
    ! Lesen aller Zeilen der Konfigurationsdatei 4
    IF ( no_error( ) ) THEN
       CALL open_file ( cfg_file )
       DO i=1,nof
          IF ( any_error( ) ) EXIT
          CALL read_phy_cfg_standard_line ( cfg_file, phy_standards(i) )
       END DO
       CALL close_file ( cfg_file )
    END IF
    !
  END SUBROUTINE init_phy_all_standards_d
  ! 
  !! Initialisieren einer Informationsbeziehung zu Standardnamen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phy_standard_d ( this, code, idim, ivar, standard_name )
    !! Objekt zur Ablage aller Informationen einer Informationsbeziehung zu Standardbezeichnungen
    TYPE (t_phy_standard) , INTENT(INOUT) :: this          ! 
    !! Code-Kennung
    INTEGER               , INTENT(IN)    :: code          ! 
    !! Zeiger auf Dimension
    INTEGER               , INTENT(IN)    :: idim          ! 
    !! Zeiger auf Datenvariatione
    INTEGER               , INTENT(IN)    :: ivar          ! 
    !! Standard-Name
    CHARACTER (LEN=*)     , INTENT(IN)    :: standard_name ! 
    !
    this%code          = code  
    this%idim          = idim
    this%ivar          = ivar
    this%standard_name = REPEAT( ' ', LEN(standard_name) )
    this%standard_name = standard_name(1:MIN(LEN_TRIM(standard_name),LEN(this%standard_name)))
    !
  END SUBROUTINE init_phy_standard_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_phy_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_phy_all_errors
  !
  !! De-Allokieren/De-Initialisieren aller physikalischen Einheiten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE clear_phy_all_units_d ( )
    !
    IF ( ALLOCATED( phy_units )   ) DEALLOCATE( phy_units   )
    !
  END SUBROUTINE clear_phy_all_units_d
  !
  !! De-Allokieren/De-Initialisieren aller Klassen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE clear_phy_all_classes_d ( )
    !
    IF ( ALLOCATED( phy_classes ) ) DEALLOCATE( phy_classes )
    !
  END SUBROUTINE clear_phy_all_classes_d
  !
  !! De-Allokieren/De-Initialisieren aller physikalischen Gr&ouml;&szlig;en <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE clear_phy_all_quants_d ( )
    !
    IF ( ALLOCATED( phy_quants ) ) DEALLOCATE( phy_quants )
    !
  END SUBROUTINE clear_phy_all_quants_d
  !
  !! De-Allokieren/De-Initialisieren aller Informationsbeziehungen zu Standardgroessen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE clear_phy_all_standards_d ( )
    !
    IF ( ALLOCATED( phy_standards ) ) DEALLOCATE( phy_standards )
    !
  END SUBROUTINE clear_phy_all_standards_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "code" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_code_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_phy_quant_code_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%code > 0 ) 
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ch,'(I10)') this%code
       CALL setup_error_act ( '<code>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_code_0
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_id_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_phy_quant_id_0' ! 
    !
    ok = ( ALL( LEN_TRIM(this%id(:)) > 0 ) ) 
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<id(1)>', this%id(1) )
       CALL setup_error_act ( '<id(2)>', this%id(2) )
    END IF
    !
  END FUNCTION ok_phy_quant_id_0
  !
  !! Pr&uuml;fe, ob die Komponente "descr" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_descr_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='ok_phy_quant_descr_0' ! 
    !
    ok = ( ALL( LEN_TRIM(this%descr(:)) > 0 ) ) 
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       CALL setup_error_act ( '<descr(1)>', this%descr(1) )
       CALL setup_error_act ( '<descr(2)>', this%descr(2) )
    END IF
    !
  END FUNCTION ok_phy_quant_descr_0
  !
  !! Pr&uuml;fe, ob die Komponente "unit_code" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_unit_code_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='ok_phy_quant_unit_code_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ANY( phy_units(:)%code == this%unit_code )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       WRITE(ch,'(I10)') this%unit_code
       CALL setup_error_act ( '<unit_code>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_unit_code_0
  !
  !! Pr&uuml;fe, ob die Komponente "class_code" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_class_code_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='ok_phy_quant_class_code_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ANY( phy_classes(:)%code == this%class_code )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
       WRITE(ch,'(I10)') this%class_code
       CALL setup_error_act ( '<class_code>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_class_code_0
  !
  !! Pr&uuml;fe, ob die Komponente "ref_code" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_ref_code_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='ok_phy_quant_ref_code_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ANY( phy_quants(:)%code == this%ref_code )
    ok = MERGE( .TRUE., ok, ANY( (/ -1,0 /) == this%ref_code ) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
       WRITE(ch,'(I10)') this%ref_code
       CALL setup_error_act ( '<ref_code>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_ref_code_0
  !
  !! Pr&uuml;fe, ob die Komponente "typ" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_typ_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_phy_quant_typ_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ANY( (/-1,1,2,3/) == this%typ )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
       WRITE(ch,'(I10)') this%typ
       CALL setup_error_act ( '<typ>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_typ_0
  !
  !! Pr&uuml;fe, ob die Komponente "zta" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_zta_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_phy_quant_zta_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ANY( (/0,1,2/) == this%zta )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6080, c_upname, c_modname )
       WRITE(ch,'(I10)') this%zta
       CALL setup_error_act ( '<zta>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_zta_0
  !
  !! Pr&uuml;fe, ob die Komponente "mzr" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_mzr_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_phy_quant_mzr_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ANY( (/0,1,2,3,4,5,6,7/) == this%mzr )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6090, c_upname, c_modname )
       WRITE(ch,'(I10)') this%mzr
       CALL setup_error_act ( '<mzr>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_mzr_0
  !
  !! Pr&uuml;fe, ob die Komponente "mul" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_mul_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_phy_quant_mul_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=15) :: ch ! 
    !
    ok = ( this%mul /= 0.0_Double )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
       WRITE(ch,'(G15.6)') this%mul
       CALL setup_error_act ( '<mul>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_mul_0
  !
  !! Pr&uuml;fe, ob die Komponente "ele" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_ele_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_phy_quant_ele_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%ele >= 0 .AND. this%ele <= get_max_omi_code( c_max_omiele ) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6110, c_upname, c_modname )
       WRITE(ch,'(I10)') this%ele
       CALL setup_error_act ( '<ele>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_ele_0
  !
  !! Pr&uuml;fe, ob die Komponente "gen" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_gen_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_phy_quant_gen_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%gen >= 0 .AND. this%gen <= get_max_omi_code( c_max_omigen ) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6120, c_upname, c_modname )
       WRITE(ch,'(I10)') this%gen
       CALL setup_error_act ( '<gen>', ch )
    END IF
    !
  END FUNCTION ok_phy_quant_gen_0
  !
  !! Pr&uuml;fe, ob die Komponente "flx" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_quant_flx_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "physikalische Gr&ouml;&szlig;e"
    TYPE (t_phy_quant) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true.
    !
  END FUNCTION ok_phy_quant_flx_0
  !
  !! Pr&uuml;fe, ob die Komponente "code" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_standard_code_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "Standardbezeichnung"
    TYPE (t_phy_standard) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_phy_standard_code_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    IF ( this%code == 0 ) THEN
       ok = .true.
    ELSE
       ok = ( this%code > 0 .AND. is_phy_quant_valid ( this%code ) ) 
       !
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6310, c_upname, c_modname )
          WRITE(ch,'(I10)') this%code
          CALL setup_error_act ( '<code>', ch )
          WRITE(ch,'(L1)') is_phy_quant_valid ( this%code )
          CALL setup_error_act ( '<valid>', ch )
       END IF
    END IF
    !
  END FUNCTION ok_phy_standard_code_0
  !
  !! Pr&uuml;fe, ob die Komponente "idim" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_standard_idim_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "Standardbezeichnung"
    TYPE (t_phy_standard) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_phy_standard_idim_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=1) :: ch      ! 
    LOGICAL           :: l_ok(2) ! 
    !
    l_ok(1) = ok_phy_standard_code ( this )
    IF ( is_phy_quant_vector( this%code ) ) THEN
       l_ok(2) = ( 1 <= this%idim .AND. this%idim <= 3 )
    ELSE
       l_ok(2) = ( this%idim == 0 )
    END IF
    ok = ALL( l_ok )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6320, c_upname, c_modname )
       WRITE(ch,'(L1)') is_phy_quant_vector( this%code ) 
       CALL setup_error_act ( '<IsVector>', ch )
       WRITE(ch,'(L1)') l_ok(1)
       CALL setup_error_act ( '<okCode>', ch )
       WRITE(ch,'(L1)') l_ok(2)
       CALL setup_error_act ( '<okIdim>', ch )
    END IF
    !
  END FUNCTION ok_phy_standard_idim_0
  !
  !! Pr&uuml;fe, ob die Komponente "ivar" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_standard_ivar_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "Standardbezeichnung"
    TYPE (t_phy_standard) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_phy_standard_ivar_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=1) :: ch      ! 
    LOGICAL           :: l_ok(2) ! 
    !
    l_ok(1) = ok_phy_standard_code ( this )
    l_ok(2) = .true. ! hier fehlt noch was
    ok = ALL( l_ok )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6330, c_upname, c_modname )
       WRITE(ch,'(L1)') l_ok(1)
       CALL setup_error_act ( '<okCode>', ch )
       WRITE(ch,'(L1)') l_ok(2)
       CALL setup_error_act ( '<okIvar>', ch )
    END IF
    !
  END FUNCTION ok_phy_standard_ivar_0
  !
  !! Pr&uuml;fe, ob die Komponente "standard_name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phy_standard_name_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt "Standardbezeichnung"
    TYPE (t_phy_standard) , INTENT(IN) :: this ! 
    !! Rueckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_phy_standard_name_0' ! 
    ! Hilfsvariable
    CHARACTER (LEN=1) :: ch      ! 
    LOGICAL           :: l_ok(2) ! 
    !
    l_ok(1) = ok_phy_standard_code ( this )
    l_ok(2) = ( this%standard_name(1:9) /= c_undefined )
    ok = ALL( l_ok )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6340, c_upname, c_modname )
       WRITE(ch,'(L1)') l_ok(1)
       CALL setup_error_act ( '<okCode>', ch )
       WRITE(ch,'(L1)') l_ok(2)
       CALL setup_error_act ( '<okName>', ch )
    END IF
    !
  END FUNCTION ok_phy_standard_name_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucken des Inhalts eines Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phy_quant_0 ( this )
    !! Objekt mit Angaben zur physikalischen Gr&ouml;&szlig;e
    TYPE (t_phy_quant) , INTENT(IN) :: this
    !! Name der Programmeinheit
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_phy_quant_0' ! 
    ! Hilfsvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
            ( UNIT   = prn_lun, &
              FMT    = 8000,    &
              IOSTAT = stat )   &
              this%code, &
              this%id(1), TRIM(this%descr(1)), &
              this%id(2), TRIM(this%descr(2)), &
              this%unit_code, this%class_code, &
              this%ref_code, this%typ,         &
              this%zta, this%mzr, this%mul,    &
              this%ele, this%gen, this%flx
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component>', 'Bezeichnungen "phy_quants"' )
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
          '#------------------------------------------------------------',/ &
          '# Inhalt einer Komponente "t_phy_quant"',/                       &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '# Code  = ',I5,/ &
          '# Id[DE]      = ',A,', Descr[DE] = ',A,/ &
          '# Id[EN]      = ',A,', Descr[EN] = ',A,/ &
          '# Unit_Code   = ',I5,/ &
          '# Class_Code  = ',I5,/ &
          '# Ref_Code    = ',I5,/ &
          '# Typ         = ',I5,/ &
          '# ZeitAbh     = ',I5,/ &
          '# PlotIndex   = ',I5,/ &
          '# PlotFaktor  = ',G15.6,/ &
          '# OpenMI-Ele  = ',I10,/ &
          '# OpenMI-Gen  = ',I10,/ &
          '# Flux_Code   = ',L2,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '#------------------------------------------------------------' )
    !
  END SUBROUTINE print_phy_quant_0
  !
  !! Drucken des Inhalts eines Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phy_standard_0 ( this )
    !! Objekt mit Angaben zur physikalischen Gr&ouml;&szlig;e
    TYPE (t_phy_standard) , INTENT(IN) :: this
    !! Name der Programmeinheit
    CHARACTER (LEN=20)     , PARAMETER :: c_upname='print_phy_standard_0' ! 
    ! Hilfsvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
            ( UNIT   = prn_lun, &
              FMT    = 8000,    &
              IOSTAT = stat )   &
              this%code,        &
              this%idim,        &
              this%ivar,        &
              TRIM(this%standard_name)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component>', 'Bezeichnungen "phy_standards"' )
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
          '#------------------------------------------------------------',/ &
          '# Inhalt einer Komponente "t_phy_standard"',/                    &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '# Code          = ',I5,/ &
          '# Idim          = ',I5,/ &
          '# Ivar          = ',I5,/ &
          '# Standard_Name = ',A ,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '#------------------------------------------------------------' )
    !
  END SUBROUTINE print_phy_standard_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-GET-Methoden <<< [ERR_NO = -9000 bis -9999]
  ! ----------------------------------------------------------------------
  !
  !! Zeiger auf Eintrag in dem Feld "phy_classes" (sprachunabh&auml;ngig) 
  !! bei Vorgabe der Id ermitteln <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_classes_id_idx_d ( var ) &
       RESULT( res )
    !! Id der physikalischen Klasse
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_classes", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariablen
    INTEGER :: i, j, l ! 
    !
    res = -1
    !
    IF ( ALLOCATED(phy_classes) ) THEN
       l = LEN_TRIM(var)
       DO i=1,SIZE(phy_classes)
          IF ( res /= -1 ) EXIT
          DO j=1,c_max_language
             IF ( l == LEN_TRIM(phy_classes(i)%id(j)) ) THEN
                IF ( var(1:l) == phy_classes(i)%id(j)(1:l) ) res = i 
             END IF
          END DO
       END DO
    END IF
    !
  END FUNCTION get_phy_classes_id_idx_d
  !
  !! Zeiger auf Eintrag in dem Feld "phy_classes" (sprachunabh&auml;ngig) 
  !! bei Vorgabe der Code-Kennung ermitteln <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_classes_code_idx_d ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Klasses
    INTEGER , INTENT(IN) :: var ! 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_classes", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    res = -1
    !
    IF ( ALLOCATED(phy_classes) ) THEN
       DO i=1,SIZE(phy_classes)
          IF ( res /= -1 ) EXIT
          IF ( var == phy_classes(i)%code ) res = i 
       END DO
    END IF
    !
  END FUNCTION get_phy_classes_code_idx_d
  !
  !! Zeiger auf Eintrag in dem Feld "phy_units" (sprachunabh&auml;ngig) 
  !! bei Vorgabe der Id ermitteln <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_phy_units_id_idx_d ( var ) &
       RESULT( res )
    !! Id der physikalischen Einheit
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_units", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !
    !! Konstante
    CHARACTER (LEN=22) , PARAMETER :: c_upname='get_phy_units_id_idx_d' ! 
    !! Hilfsvariablen
    INTEGER :: i, j, l ! 
    !
    res = -1
    !
    IF ( ALLOCATED(phy_units) ) THEN
       l = LEN_TRIM(var)
       DO i=1,SIZE(phy_units)
          IF ( res /= -1 ) EXIT
          DO j=1,c_max_language
             IF ( l == LEN_TRIM(phy_units(i)%id(j)) ) THEN
                IF ( var(1:l) == phy_units(i)%id(j)(1:l) ) res = i 
             END IF
          END DO
       END DO
    END IF
    !
    IF ( res == -1 ) THEN
       CALL setup_error_act ( all_errors(:), -9000, c_upname, c_modname )
       CALL setup_error_act ( '<Id_or_Code>', var )
    END IF
    !
  END FUNCTION get_phy_units_id_idx_d
  !
  !! Zeiger auf Eintrag in dem Feld "phy_units" (sprachunabh&auml;ngig) 
  !! bei Vorgabe der Code-Kennung ermitteln <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_units_code_idx_d ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Einheit
    INTEGER , INTENT(IN) :: var ! 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_units", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    res = -1
    !
    IF ( ALLOCATED(phy_units) ) THEN
       DO i=1,SIZE(phy_units)
          IF ( res /= -1 ) EXIT
          IF ( var == phy_units(i)%code ) res = i 
       END DO
    END IF
    !
  END FUNCTION get_phy_units_code_idx_d
  !
  !! Zeiger auf Eintrag in dem Feld "phy_units" (sprachunabh&auml;ngig) 
  !! bei Vorgabe der bekannten Basisdimensionen einer Gr&ouml;&szlig;e ermitteln <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_units_si_base_idx_d ( unit, var ) &
       RESULT( res )
    !! physikalische Einheit (Vergleichsgr&ouml;&szlig;e f&uuml;r Offset und Konversionsfaktor)
    TYPE (t_phy_unit) , INTENT(IN) :: unit   ! 
    !! Basisdimensionen der physikalischen Gr&ouml;&szlig;e
    REAL (KIND=Double), INTENT(IN) :: var(:) ! >GL> 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_units", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariablen
    LOGICAL :: l_ok(2) ! 
    INTEGER :: i ! 
    !
    res     = -1
    l_ok(1) = ( INDEX( unit%id(language), 'E+' ) > 0 .OR. &
                INDEX( unit%id(language), 'E-' ) > 0 ) ! 
    !
    IF ( ALLOCATED(phy_units) ) THEN
       IF ( c_max_si_base == SIZE(var) ) THEN
          DO i=1,SIZE(phy_units)
             IF ( res /= -1 ) EXIT
             l_ok(2) = ( INDEX( phy_units(i)%id(language), 'E+' ) > 0 .OR. &
                         INDEX( phy_units(i)%id(language), 'E-' ) > 0 )
             IF ( ALL( var(:) - phy_units(i)%base(:) <= EPSILON(var(:)) ) .AND. & ! >GL> 
                  unit%convf  == phy_units(i)%convf    .AND. &
                  unit%offset == phy_units(i)%offset   .AND. &
                  ( ALL( l_ok(:) ) .OR. ALL( .NOT. l_ok(:) ) ) ) res = i 
          END DO
       END IF
    END IF
    !
  END FUNCTION get_phy_units_si_base_idx_d
  !
  !! Zeiger auf Eintrag in dem Feld "phy_quants" (sprachunabh&auml;ngig) 
  !! bei Vorgabe der Code-Kennung ermitteln <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quants_code_idx_d ( var ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_quants", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariable
    INTEGER :: l(1) ! 
    !
    res = -1
    !
    IF ( ALLOCATED(phy_quants) ) THEN
       l   = MINLOC( phy_quants(:)%code, phy_quants(:)%code == var )
       res = MERGE( l(1), -1, l(1) > 0 )
    END IF
    !
  END FUNCTION get_phy_quants_code_idx_d
  !
  !! Zeiger auf Eintrag in dem Feld "phy_quants" (sprachunabh&auml;ngig) 
  !! bei Vorgabe der Id/Beschreibung ermitteln <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_quants_descr_idx_d ( var ) &
       RESULT( res )
    !! Id/Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_quants", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariable
    INTEGER :: i, j, l1, l2 ! 
    !
    res = -1
    !
    IF ( ALLOCATED(phy_quants) ) THEN
       l1 = LEN_TRIM(var)
       ! Suchen, ob "var" in der Komponente "descr(:)" vorkommt
       DO j=1,c_max_language
          IF ( res /= -1 ) EXIT
          DO i=1,SIZE(phy_quants)
             IF ( res /= -1 ) EXIT
             l2 = LEN_TRIM(phy_quants(i)%descr(j))
             IF ( l1 == l2 ) THEN
                IF ( var(1:l1) == phy_quants(i)%descr(j)(1:l2) ) res = i
             END IF
          END DO
       END DO
       ! Suchen, ob "var" in der Komponente "id(:)" vorkommt
       DO j=1,c_max_language
          IF ( res /= -1 ) EXIT
          DO i=1,SIZE(phy_quants)
             IF ( res /= -1 ) EXIT
             l2 = LEN_TRIM(phy_quants(i)%id(j))
             IF ( l1 == l2 ) THEN
                IF ( var(1:l1) == phy_quants(i)%id(j)(1:l2) ) res = i
             END IF
          END DO
       END DO
    END IF
    !
  END FUNCTION get_phy_quants_descr_idx_d
  !
  !! Zeiger auf Eintrag in dem Feld "phy_standards" bei Vorgabe der Standardbezeichnung ermitteln <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_standards_name_idx_d ( var ) &
       RESULT( res )
    !! Standardbezeichnung einer Komponente
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_standards", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariable
    INTEGER :: i    ! 
    !
    res = -1
    !
    IF ( ALLOCATED(phy_standards) ) THEN
       DO i=1,SIZE(phy_standards)
          IF ( res /= -1 ) EXIT
          IF ( LEN_TRIM(phy_standards(i)%standard_name) == LEN_TRIM(var) ) THEN
             IF ( phy_standards(i)%standard_name(1:LEN_TRIM(var)) == var(1:LEN_TRIM(var)) ) res = i
          END IF
       END DO
    END IF
    !
  END FUNCTION get_phy_standards_name_idx_d
  !
  !! Zeiger auf Eintrag in dem Feld "phy_standards" 
  !! bei Vorgabe der Code-Kennung ermitteln <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_standards_name_cn_idx_d ( var, idx ) &
       RESULT( res )
    !! Code-Kennung der phys. Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: var ! 
    !! lfd. Nummer des zu der phys. Gr&ouml;&szlig;e geh&ouml;renden Standardnamens
    INTEGER , INTENT(IN) :: idx ! 
    !! Ergebniswert: Zeiger auf Eintrag in Feld "phy_standards", falls
    !! nicht definiert, wird -1 zur&uuml;ckgegeben
    INTEGER :: res     ! 
    !! Hilfsvariable
    INTEGER :: i, m, n ! 
    !
    res = -1
    n   = 0
    m   = get_phy_quant_nof_standard_name ( var )
    IF ( ALLOCATED( phy_standards ) ) THEN
       DO i=1,SIZE(phy_standards)
          IF ( res /= -1 ) EXIT
          IF ( phy_standards(i)%code == var ) n   = n + 1
          IF (                     n == idx ) res = i
       END DO
    END IF
    !
  END FUNCTION get_phy_standards_name_cn_idx_d
  !
  !! ermittle den maximal zul&auml;ssigen Code f&uuml;r OMIELE und OMIGEN in
  !! Abh&auml;ngigkeit von der Anzahl der zu codierenden Gr&ouml;&szlig;en <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_max_omi_code_d ( val ) &
       RESULT( res )
    !! Anzahl der zu codierenden Gr&ouml;&szlig;en
    INTEGER , INTENT(IN) :: val ! 
    !! Ergebnis: maximaler zul&auml;ssiger Code-Wert
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    res = 0
    DO i=1,val
       res = res + 2**(i-1)
    END DO
    !
  END FUNCTION get_max_omi_code_d
  !
  !! ermittle, welche lfd. Eintr&auml;ge mit einer Codierung bezeichnet werden <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungem
  FUNCTION get_omi_coded_d ( nof, code ) &
       RESULT( res )
    !! max. Anzahl der zu codierenden Ziele
    INTEGER , INTENT(IN) :: nof  ! 
    !! aktueller Code
    INTEGER , INTENT(IN) :: code ! 
    !! Ergebnis: Indikatorfeld f&uuml;r verf&uuml;gbare Ziele
    LOGICAL :: res(nof) ! 
    !! Hilfsvariable
    INTEGER :: i, n, l  ! 
    !
    res = .false.
    IF ( code >= 0 .AND. code <= get_max_omi_code( nof ) ) THEN
       l = code
       DO i=nof,1,-1
          n = 2**(i-1)
          IF ( l >= n ) THEN
             res(i) = .true.
             l      = l - n
          END IF
       END DO
    END IF
    !
  END FUNCTION get_omi_coded_d
  !
  ! -------------------------------------------------------------------------
  !
  !! Erzeugen eines File-Objekts f&uuml;r eine Konfigurationsdatei <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phy_cfg_file_d ( var ) &
       RESULT( res )
    !! Name der Konfigurationsdatei
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebnis: Dateiobjekt mit Konfigurationsdatei
    TYPE (t_file) :: res                  ! 
    !
    CHARACTER (LEN=18) , PARAMETER :: c_upname='get_phy_cfg_file_d' ! 
    ! Hilfsvariablen
    CHARACTER (LEN=80) :: cfg_path   ! 
    CHARACTER (LEN= 2) :: path_delim ! 
    INTEGER            :: i, l1, l2  ! 
    !
    CALL NEW_file ( res )
    !
    cfg_path   = REPEAT( ' ', LEN(cfg_path)   )
    path_delim = REPEAT( ' ', LEN(path_delim) )
    cfg_path   = getenvv_cmds( 'CFGDIR' ) 
    path_delim = pathdelim_cmds( )
    !
    ! lokale Datei (1) vor Konfigurationsdatei (2) ------------------------
    DO i=1,2
       CALL set_file_unit ( res, 0 ) ! automatische Suche
       SELECT CASE ( i )
       CASE ( 1 )
          CALL set_file_name ( res, var )
       CASE ( 2 )
          l1 = LEN_TRIM(cfg_path)
          l2 = LEN_TRIM(path_delim)
          CALL set_file_name ( res, cfg_path(1:l1)//path_delim(1:l2)//var )
       END SELECT
       CALL set_file_status   ( res, 'OLD' )
       CALL set_file_access   ( res, 'SEQUENTIAL' )
       CALL set_file_form     ( res, 'FORMATTED' )
       CALL set_file_recl     ( res, 1 )
       CALL set_file_position ( res, 'REWIND' )
       CALL set_file_action   ( res, 'READ' )
       CALL set_file_type     ( res, 'UNKNOWN' )
       IF ( file_exists( res ) ) EXIT
    END DO
    !
    IF ( .NOT. file_exists( res ) ) THEN
       CALL setup_error_act ( all_errors(:), -9030, c_upname, c_modname )
       CALL setup_error_act ( '<pathname>', TRIM(cfg_path) )
       CALL setup_error_act ( '<filename>', TRIM(var) )
    ENDIF
  END FUNCTION get_phy_cfg_file_d
  !
  !! Ermittle die Anzahl der phys. Gr&ouml;&szlig;en auf einer Konfigurationsdatei <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_phy_cfg_file_nof_quants_d ( var ) &
       RESULT( res )
    !! Dateiobjekt mit Konfigurationsdatei
    TYPE (t_file) , INTENT(INOUT) :: var ! 
    !! Ergebnis: Anzahl der physikalischen Gr&ouml;&szlig;en auf Datei
    INTEGER :: res ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=29) , PARAMETER :: c_upname='get_phy_cfg_file_nof_quants_d' ! 
    ! Hilfsgr&ouml;szlig;en
    CHARACTER (LEN=80)  :: karte !                                ! 
    INTEGER             :: stat  ! 
    !
    res  = 0
    stat = 0
    !
    IF ( no_error( ) ) CALL OPEN_file  ( var )
    IF ( no_error( ) ) THEN
       DO
          karte = REPEAT( ' ', LEN(karte) )
          READ(get_file_unit(var),'(A)',IOSTAT=stat) karte
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -9010, c_upname, c_modname, stat, karte )
             CALL setup_error_act ( '<filename>', TRIM(get_file_name(var)) )
          ELSE
             IF ( karte(1:1) /= 'C' .AND. karte(1:1) /= '#' .AND. karte(1:7) /= 'ENDDATA' ) res = res + 1
          END IF
          IF ( karte(1:7) == 'ENDDATA' .OR. any_error( ) ) EXIT
       END DO
    END IF
    !
    IF ( no_error( ) ) CALL CLOSE_file ( var )
    !
    IF ( res == 0 .AND. no_error() ) THEN
       CALL setup_error_act ( all_errors(:), -9020, c_upname, c_modname )
       CALL setup_error_act ( '<filename>', TRIM(get_file_name(var)) )
    END IF
    !
  END FUNCTION get_phy_cfg_file_nof_quants_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-READ-Methoden <<< [ERR_NO = -20000 bis -20999]
  ! ----------------------------------------------------------------------
  !
  !! Lesen einer Zeile aus den Konfigurationsdateien und Ablegen der
  !! Informationen in "phy_quants" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_phy_cfg_files_line_d &
       ( file, phy_quant )
    !
    !! Dateiobjekte
    TYPE (t_file)      , INTENT(IN)  :: file(:)   ! 
    !! Objekt zur Speicherung der Informationen
    TYPE (t_phy_quant) , INTENT(OUT) :: phy_quant !
    !! Name der Programmeinheit
    CHARACTER (LEN=25) , PARAMETER :: c_upname='read_phy_cfg_files_line_d' ! 
    !! lokale Variablen
    CHARACTER (LEN=80)  :: karte(SIZE(file))                    ! 
    CHARACTER (LEN=40)  :: descr_de, descr_en                   ! 
    CHARACTER (LEN=08)  :: cphysabk                             !  
    CHARACTER (LEN=10)  :: unit_id_de, unit_id_en               ! 
    INTEGER :: iphystyp, iphyszta, iphysklas, iphysref, iphymzr, omiele, omigen ! 
    REAL    :: rphymul !           
    LOGICAL :: flx     !  
    INTEGER :: i, n, m(SIZE(file)), stat(SIZE(file))         ! 
    ! ---------------------------------------------------------------------
    stat(:) = 0
    m(:)    = 0
    ! Lesen aus Datei "phydef.cfg.de.dat" ---------------------------------
    n = 0 
    DO
       IF ( n /= 0 ) EXIT
       karte(1) = REPEAT( ' ', LEN(karte(1)) )
       READ(get_file_unit(file(1)),'(A)',IOSTAT=stat(1)) karte(1)
       IF ( karte(1)(1:1) /= 'C' .AND. karte(1)(1:1) /= '#' ) THEN
          n = n + 1
          READ(karte(1),'(I5,5X,A40,5X,A10)') m(1), descr_de, unit_id_de 
       END IF
    END DO
    ! Lesen aus Datei "phydef.cfg.en.dat" --------------------------------
    n = 0
    DO
       IF ( n /= 0 ) EXIT
       karte(2) = REPEAT( ' ', LEN(karte(2)) )
       READ(get_file_unit(file(2)),'(A)',IOSTAT=stat(2)) karte(2)
       IF ( karte(2)(1:1) /= 'C' .AND. karte(2)(1:1) /= '#' ) THEN
          n = n + 1
          READ(karte(2),'(I5,5X,A40,5X,A10)') m(2), descr_en, unit_id_en 
       END IF
    END DO
    ! Lesen aus Datei "phydef.cfg.rest.dat" ------------------------------
    n = 0
    DO
       IF ( n /= 0 ) EXIT
       karte(3) = REPEAT( ' ', LEN(karte(3)) )
       READ(get_file_unit(file(3)),'(A)') karte(3)
       IF ( karte(3)(1:1) /= 'C' .AND. karte(3)(1:1) /= '#' ) THEN
          n = n + 1
          READ(karte(3),'(4I5,1X,A8,1X,I5,F15.0,I5,2I10,L2)',IOSTAT=stat(3)) m(3), &
               iphystyp, iphyszta, iphysklas, cphysabk, iphysref,    &
               rphymul, iphymzr, omiele, omigen, flx
       END IF
    END DO
    ! evtl.Fehlermeldungen erzeugen --------------------------------------
    DO i=1,SIZE(stat)
       IF ( stat(i) /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -20000, c_upname, c_modname, stat(i), karte(i) )
          CALL setup_error_act ( '<filename>', TRIM(get_file_name(file(i))) )
       END IF
    END DO
    IF ( ANY( m(:) /= m(1) ) ) THEN
       CALL setup_error_act ( all_errors(:), -20010, c_upname, c_modname )
       CALL setup_error_act ( '<filename1>', TRIM(get_file_name(file(1))) )
       CALL setup_error_act ( '<filename2>', TRIM(get_file_name(file(2))) )
       CALL setup_error_act ( '<filename3>', TRIM(get_file_name(file(3))) )
       CALL setup_error_act ( '<line1>', TRIM(karte(1)) )
       CALL setup_error_act ( '<line2>', TRIM(karte(2)) )
       CALL setup_error_act ( '<line3>', TRIM(karte(3)) )
    END IF
    ! Daten in Speicherfeld uebertragen
    IF ( no_error( ) ) THEN
       CALL init_phy_quant &
            ( phy_quant, m(1), cphysabk, descr_de, cphysabk, descr_en,      &
            unit_id_de, iphysklas, iphysref, iphystyp, iphyszta, iphymzr, &
            REAL( rphymul,DOUBLE ), omiele, omigen, flx )
    END IF
    !
  END SUBROUTINE read_phy_cfg_files_line_d
  !
  !! Lesen der Datei mit den Informationsbeziehungen zu den Standardnamen in "phy_standards" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_phy_cfg_standard_line_d ( file, phy_standard )
    !! Dateiobjekt
    TYPE (t_file)         , INTENT(IN)  :: file   ! 
    !! Objekt zur Speicherung der Informationen
    TYPE (t_phy_standard) , INTENT(OUT) :: phy_standard !
    !! Name der Programmeinheit
    CHARACTER (LEN=28) , PARAMETER :: c_upname='read_phy_cfg_standard_line_d' ! 
    !! lokale Variablen
    CHARACTER (LEN=140)                 :: karte             ! 
    CHARACTER (LEN=c_len_standard_name) :: standard_name     ! 
    INTEGER                             :: icode, idim, ivar ! 
    INTEGER                             :: n, m, stat     ! 
    ! ---------------------------------------------------------------------
    stat = 0
    m    = 0
    ! Lesen aus Datei "phydef-cf.cfg.dat" ---------------------------------
    n    = 0 
    DO
       IF ( n /= 0 ) EXIT
       karte = REPEAT( ' ', LEN(karte) )
       READ(get_file_unit(file),'(A)',IOSTAT=stat) karte
       IF ( karte(1:1) /= 'C' .AND. karte(1:1) /= '#' ) THEN
          n             = n + 1
          standard_name = REPEAT( ' ', LEN_TRIM(standard_name) )
          READ(karte,'(I5,I5,I5,1X,A)') icode, idim, ivar, standard_name
       END IF
    END DO
    ! evtl.Fehlermeldungen erzeugen --------------------------------------
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -20000, c_upname, c_modname, stat, karte )
       CALL setup_error_act ( '<filename>', TRIM(get_file_name(file)) )
    END IF
    ! Daten in Speicherfeld uebertragen
    IF ( no_error( ) ) THEN
       CALL init_phy_standard &
            ( phy_standard, icode, idim, ivar, standard_name )
    END IF
    !
  END SUBROUTINE read_phy_cfg_standard_line_d
  !
  !! Lesen der Datei mit den Informationsbeziehungen zu den SI-Einheiten in "phy_units" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_phy_cfg_si_line_d ( file, phy_unit )
    !! Dateiobjekt
    TYPE (t_file)         , INTENT(IN)  :: file   ! 
    !! Objekt zur Speicherung der Informationen
    TYPE (t_phy_unit)     , INTENT(OUT) :: phy_unit !
    !! Name der Programmeinheit
    CHARACTER (LEN=22) , PARAMETER :: c_upname='read_phy_cfg_si_line_d' ! 
    !! lokale Variablen
    CHARACTER (LEN=280)                  :: karte              ! 
    CHARACTER (LEN=c_len_phy_unit_id)    :: id_de, id_en       ! 
    CHARACTER (LEN=c_len_phy_unit_descr) :: descr_de, descr_en ! 
    INTEGER                              :: i, n, stat         ! 
    INTEGER                              :: code               ! 
    REAL (KIND=Double)                   :: m, kg, s, a, k, mol, cd, e, convf, offset ! 

    ! ---------------------------------------------------------------------
    stat = 0
    ! Lesen aus Datei "phydef.cfg.si.dat" ---------------------------------
    n    = 0 
    DO
       IF ( n /= 0 ) EXIT
       karte = REPEAT( ' ', LEN(karte) )
       READ(get_file_unit(file),'(A)',IOSTAT=stat) karte
       IF ( karte(1:1) /= 'C' .AND. karte(1:1) /= '#' ) THEN
          n             = n + 1
          id_de         = REPEAT( ' ', LEN_TRIM(id_de) )
          id_en         = REPEAT( ' ', LEN_TRIM(id_en) )
          descr_de      = REPEAT( ' ', LEN_TRIM(descr_de) )
          descr_en      = REPEAT( ' ', LEN_TRIM(descr_en) )
          READ(karte,'(I5,1X,I5,1X,A10,1X,A60,1X,A10,1X,A60,1X,2(G15.8,1X),8(F10.5,1X))',IOSTAT=stat) & ! 
               i, code, id_de(1:10), descr_de(1:60), id_en(1:10), descr_en(1:60),                     & ! 
               convf, offset, m, kg, s, a, k, mol, cd, e 
       END IF
    END DO
    ! evtl.Fehlermeldungen erzeugen --------------------------------------
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -20000, c_upname, c_modname, stat, karte )
       CALL setup_error_act ( '<filename>', TRIM(get_file_name(file)) )
    END IF
    ! Daten in Speicherfeld uebertragen
    IF ( no_error( ) ) THEN
       CALL init_phy_unit_d &
            ( phy_unit, code, id_de, descr_de, id_en, descr_en, m, kg, s, a, k, mol, cd, e, convf, offset )
    END IF
    !
  END SUBROUTINE read_phy_cfg_si_line_d
  !
END MODULE b_phy
! TailOfBaseModule --------------------------------------------------------
