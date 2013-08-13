! -------------------------------------------------------------------------
! HeadOfPackageUserInterface ----------------------------------------------
!
!! <H2>Lesen und Zugriff auf Daten, die in Datei(en) des Formats ipds
!!     (Initial Physical DataSets) stehen</h2>
!! @author Jens J&uuml;rges
!! @version 3.2 vom 04/13/07, Quellcode: mod_io_ipds_ui.f90
!! <HR>
!! Read and access of data stored in a/some file/s of type ipds
!! (Initial Physical DataSets)
!
!  Copyright-Hinweis
!
!  Copyright (C) 2002 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Packages
!  1.01 : 2002-08-08 : J. Juerges : Startversion
!  1.02 : 2002-09-16 : J. Juerges : get_ipds_rigid_layer_depth_type u. get_ipds_rigid_layer_rough_type ergaenzt
!  1.03 : 2002-09-17 : J. Juerges : get_ipds_val_xyz1 wird doppelt genau
!  1.04 : 2002-10-16 : J. Juerges : Interface get_ipds_val erweitert um Routinen mit mehreren phys. Groessen in einem Aufruf
!  1.05 : 2003-01-10 : J. Juerges : linklose Methodenbeschreibungen geloescht
!  1.06 : 2003-01-10 : J. Juerges : Entwicklungsgeschichte nicht fuer f90doc
!  1.07 : 2003-01-10 : J. Juerges : Lizenzangabe durch Platzhalter ersetzt
!  1.08 : 2003-01-10 : J. Juerges : keine Beschreibungen der Interfaces dort, wo sie PUBLIC gemacht werden
!  1.09 : 2003-01-31 : J. Juerges : Auslagerung der Testroutine
!  1.10 : 2003-02-05 : J. Juerges : Fit fuer Mehrprozessor-Betrieb (OpenMP)
!  1.11 : 2003-09-11 : J. Jueregs : Die Berechnung der zu den Regionen zugehoerigen Messpositionen
!                                   wird zentral erledigt und nicht mehr fuer jeden Punkt p
!                                   neu durchgefuehrt
!  1.12 : 2003-09-19 : J. Juerges : Die Berechnung der Messwerte an allen Positionen wird zentral
!                                   erledigt und nicht mehr fuer jeden Punkt p neu durchgefuehrt 
!  1.13 : 2004-01-29 : J. Juerges : Bei selbstdefinierten Datentypen INTENT(OUT) -> INTENT(INOUT) gewandelt
!  1.14 : 2004-07-08 : J. Juerges : s_ipds_physet_name_available ergaenzt
!  2.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  2.02 : 2005-08-22 : J. Juerges : Das neue Modul "m_ipds_phydef" beruecksichtigt + kleine Verbesserungen
!  2.03 : 2005-08-24 : J. Juerges : "Kleine Verbesserungen" waren ein Fehler
!  2.04 : 2005-09-01 : J. Juerges : Korrekte Initialisierung fuer prn_op und trc_op
!  2.05 : 2005-09-22 : G. Seiss   : Kleine Anpassungen fuer ALTIX in Karlsruhe
!  2.06 : 2005-12-19 : J. Juerges : Neue oeffentliche get-Routinen
!  3.01 : 2007-03-13 : G. Lang    : neue Schnittstellen get_ipds_dim, get_ipds_var und get_ipds_att
!  3.02 : 2007-04-13 : G. Lang, C. Maerker : neu PUBLIC is_ipds_point_in_region, PRIVATE is_point_in_region
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! <UL>
!!    <LI> Einlesen von Dateien des Typs "ipds.dat"
!!         (initial physical datasets)                              </LI>
!!    <LI> Speichern der Daten in geeigneten privaten Datenstrukturen
!!         (Daten mehrerer Dateien werden vorraetig gehalten)       </LI>
!!    <LI> Abfragemoeglichkeit bzgl. verschiedener physikalischer Daten
!!         an einem/vielen gewuenschten Ort/en
!!         (ggf. mittels Interpolation)                             </LI>
!! </UL>
!! <HR>
!!                                                                  <BR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt keinen &ouml;ffentlich zug&auml;nglichen     <BR>
!! Datentyp zur Verf&uuml;gung.                                     <BR>
!! <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Packages io_ipds_ui mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Package-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Packages io_ipds_ui mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: siehe Modul mod_m_ipds_errors.f90 oder verwende 
!! Methode PRINT_IPDS_ALL_ERRORS
!!                                                                    <BR>
!
MODULE io_ipds_ui
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] BASIS-Modul mit globalen Konstantwerten
  USE b_constants, ONLY : &
       ! Parameter
       single, &
       double
  !
  ! [A.1.2] BASIS-Modul mit Fehler-Typ und -Routinen
  USE b_error, ONLY :       &
       ! Typdefinitionen
       t_error,             &
       ! Routinen
       init_error,          &
       clear_error,         &
       no_error,            &
       any_error,           &
       setup_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun
  !
  ! [A.1.3] BASIS-Modul mit Datei-Typ und -Routinen
  USE b_file, ONLY :          &
       ! Typdefinitionen
       t_file,                &
       ! Routinen
       init_file,             &
       clear_file,            &
       setup_file_prn_lun,    &
       setup_file_trc_lun,    &
       new_file,              &
       kill_file,             &
       set_file_unit,         &
       set_file_name,         &
       set_file_type,         &
       set_file_status,       &
       auto_file_access_form, &
       open_file,             &
       close_file
  !
  ! [A.1.4] BASIS-Modul fuer Zeitpunktangaben
  USE b_datetime, ONLY :       &
       !   Typdefinitionen
       t_datetime,             &
       !   Routinen / Interfaces
       init_datetime,          &
       clear_datetime,         &
       setup_datetime_prn_lun, &
       setup_datetime_trc_lun
  ! 
  ! [A.1.5] BASIS-Modul fuer 2D-Koordinaten
  USE b_point_2d, ONLY :       &
       !   Typdefinitionen
       t_point_2d,             &
       !   Routinen / Interfaces
       init_point_2d,          &
       clear_point_2d,         &
       setup_point_2d_prn_lun, &
       setup_point_2d_trc_lun, &
       new_point_2d,           &
       kill_point_2d,          &
       set_point_2d_xy,        &
       inside_point_2d
  ! 
  ! [A.1.6] OpenMI-konformes Basis-Modul "b_omi_exch_r" [ "ragged array" Austauschgroesse ]
  !         Anmerkung: die folgenden Basis-Module werden von "exch" 
  !                    direkt bzw. indirekt mit-initialisiert
  !                    1.) mod_b_omi_exch.f90
  !                    2.) mod_b_omi_quant.f90
  !                        a) mod_b_dim.f90
  !                        b) mod_b_var.f90
  !                        c) mod_b_att.f90
  !                        d) mod_b_phy.f90
  !                        e) mod_b_omi_dim.f90
  !                        f) mod_b_omi_unit.f90
  !                    3.) mod_b_omi_ele.f90
  !                        a) mod_b_ind.f90
  !                        b) mod_b_xyz.f90
  !                           i) mod_b_omi_space.f90
  !                    4.) mod_b_omi_dope.f90
  !                        a) mod_b_omi_arg.f90
  USE b_omi_exch_r, ONLY :        &
       ! Typdefinition
       t_omi_exch_r,              &
       ! Routinen
       init_omi_exch_r,           &
       clear_omi_exch_r,          &
       setup_omi_exch_r_prn_lun,  &
       setup_omi_exch_r_trc_lun,  &
       setup_omi_exch_r_language, &
       new_omi_exch_r,            &
       set_omi_exch_r_odx,        &
       set_omi_exch_r_gdx,        &
       set_omi_exch_r_exch_ref,   &
       get_omi_exch_r_exch_ref,   &
       ok_omi_exch_r
  !
  ! [A.1.7] OpenMI-konformes Basis-Modul "span" [ Zeitspanne ]
  !         Anmerkung: die folgenden Basis-Module werden von "span" 
  !                    direkt bzw. indirekt mit-initialisiert
  !                    1.) mod_b_datetime.f90
  !                    2.) mod_b_omi_stamp.f90
  USE b_omi_span, ONLY :       &
       ! Typdefinition
       t_omi_span,             &
       ! Routinen
       init_omi_span,          &
       clear_omi_span,         &
       setup_omi_span_prn_lun, &
       setup_omi_span_trc_lun, &
       new_omi_span,           &
       get_omi_span_start,     &
       get_omi_span_end,       &
       set_omi_span_start,     &
       set_omi_span_end
  !
  ! [A.1.8] OpenMI-konformes Basis-Modul "stamp" [ Zeitangabe ]
  !
  USE b_omi_stamp, ONLY :      &
       ! Typdefinition
       t_omi_stamp,            &
       ! Routinen
       new_omi_stamp,          &
       get_omi_stamp_modjulianday
  !
  ! [A.1.9] OpenMI-konformes Basis-Modul "exch" [ Austauschgroessen ]
  USE b_omi_exch, ONLY : &
       ! Typdefinition
       t_omi_exch
  !
  ! [A.1.10] OpenMI-konformes Basis-Modul "quant" [ Quantities ]
  USE b_omi_quant, ONLY : &
       ! Daten
       c_undef_omi_quant_int
  ! [A.1.11] BASIS-Modul mit "Dimensionen" 
  USE b_dim, ONLY : &
       ! Datentyp
       t_dim
  ! [A.1.12] BASIS-Modul mit "Variablen" 
  USE b_var, ONLY : &
       ! Datentyp
       t_var
  ! [A.1.13] BASIS-Modul mit "Attributen" 
  USE b_att, ONLY : &
       ! Datentyp
       t_att
  !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  ! [A.2.1] Module mit Datentyp des Paketes "ipds"
  USE m_ipds_phydef, ONLY : &
       ! Daten
       c_phy_name_de
  USE m_ipds_data ! nutze im wesentlichen alle Groessen
  !
  ! [A.2.2] Initialisieren und De-Initialisieren der Fehlermeldungen
  USE m_ipds_errors, ONLY : &
       ! Routinen / Interfaces
       init_all_errors, &
       clear_all_errors
  !
  ! [A.2.3] Einlesen einer Datei des Typs ipds.dat
  USE m_ipds_readfile, ONLY : &
       ! Routinen / Interfaces
       read_ipds_0
  !
  ! [A.2.4] PACKAGE-Modul fuer Zugriff auf physikalische Daten an gegebenen Punkten
  !
  USE m_ipds_access, ONLY : &
       !   Routinen / Interfaces
       get_val_object
  !
  ! [A.2.5] PACKAGE-Modul fuer Datenbeschreibung des Typs "physet"
  USE m_ipds_physet, ONLY :  &
       !   Typdefinitionen
       t_physet,             &
       !   Routinen / Interfaces
       init_physet,          &
       clear_physet,         &
       setup_physet_prn_lun, &
       setup_physet_trc_lun, &
       get_physet_set,       &
       update_physet_var,    &
       kill_physet
  ! 
  ! [A.2.6] PACKAGE-Modul fuer Datenbeschreibung des Typs "region"
  USE m_ipds_region,   ONLY : &
       !   Daten
       c_len_region_name,     &
       !   Routinen / Interfaces
       init_region,           &
       clear_region,          &
       setup_region_prn_lun,  &
       setup_region_trc_lun,  &
       get_region_name,       &
       get_region_nof_border, &
       get_region_border,     &
       index_region_name
  ! 
  ! [A.2.7] PACKAGE-Modul fuer Datenbeschreibung des Typs "mespos"
  USE m_ipds_mespos,      ONLY :  &
       !   Daten
       c_len_mespos_name,         &
       !   Routinen / Interfaces
       init_mespos,               &
       clear_mespos,              &
       setup_mespos_prn_lun,      &
       setup_mespos_trc_lun,      &
       get_mespos_name,           &
       get_mespos_coor,           &
       index_mespos_name,         &
       get_mespos_nof_physet_set, &
       get_mespos_physet
  ! 
  ! [A.2.8] PACKAGE-Modul fuer Datenbeschreibung des Typs "regphyset"
  USE m_ipds_regphyset,        ONLY : &
       !   Daten
       c_len_regphyset_interpol_name, &
       !   Routinen / Interfaces
       init_regphyset,                &
       clear_regphyset,               &
       setup_regphyset_prn_lun,       &
       setup_regphyset_trc_lun,       &
       get_regphyset_region_name,     &
       get_regphyset_region_inside,   &
       get_regphyset_region_zmin,     &
       get_regphyset_region_zmax,     &
       get_regphyset_nof_mespos_name, &
       get_regphyset_mespos_name,     &
       get_regphyset_mespos_maxdist,  &
       get_regphyset_interpol_name
  ! 
  ! [A.2.9] PACKAGE-Modul fuer Datenbeschreibung des Typs "phyval"
  USE m_ipds_phyval, ONLY :     &
       !   Typdefinitionen
       t_phyval,                &
       !   Daten
       c_len_phyval_name,       &
       !   Routinen / Interfaces
       init_phyval,             &
       clear_phyval,            &
       setup_phyval_prn_lun,    &
       setup_phyval_trc_lun,    &
       new_phyval,              &
       ok_phyval,               &
       get_phyval_name,         &
       get_phyval_nof_type,     &
       get_phyval_type,         &
       get_phyval_nof_var_name, &
       get_phyval_var_name,     &
       get_phyval_val,          &
       kill_phyval
  !
  ! [A.2.10] Erstellen der OpenMI-konformen Komponenten
  USE m_ipds_omi, ONLY :    &
       create_omi,          &
       has_omi_discr_time,  &
       has_omi_time_span,   &
       nof_omi_discr_time,  &
       get_omi_discr_time,  &
       get_omi_time_span,   &
       get_omi_xyz_x_coord, &
       get_omi_xyz_y_coord, &
       get_omi_xyz_z_coord, &
       get_omi_phyval_name, &
       get_omi_variant_no,  &
       is_omi_magnitude
  !
  ! [A.2.11] Ableiten neuer Groessen aus bekannten Informationen
  USE m_ipds_derive, ONLY : &
       derive_dim, derive_var, derive_att
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
  !       Hinweis: ein io-Package definiert keinen oeffentlich 
  !                zugaenglichen Datentyp.
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  ! [C.3] Variablen [moeglichst nicht verwenden]
  ! [C.4] Schnittstellen
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  ! Hinweis: verschiedene Methoden arbeiten auf Skalar und 1D-Array.
  !          Ggf. weitere ergaenzen (z.B. 2D-Array) falls sinnvoll.
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Packages
  INTERFACE init_ipds
     MODULE PROCEDURE init_ipds_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Packages
  INTERFACE clear_ipds
     MODULE PROCEDURE clear_ipds_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_ipds_prn_lun
     MODULE PROCEDURE setup_ipds_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_ipds_trc_lun
     MODULE PROCEDURE setup_ipds_trc_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r Input (Sequential, Ascii)
  INTERFACE setup_ipds_asc_seq_lun
     MODULE PROCEDURE setup_ipds_asc_seq_lun_d ! 
  END INTERFACE
  !! Setzen des (Arbeits-) Objektes
  INTERFACE setup_ipds_work_object
     MODULE PROCEDURE setup_ipds_work_object_d
  END INTERFACE
  !! Setzen der Komponente "name" in Objekt <BR>
  !! a) Arbeitsobjekt belegen
  INTERFACE setup_ipds_name
     MODULE PROCEDURE setup_ipds_name_w0
  END INTERFACE setup_ipds_name
  !! Setzen der Komponente "file" in Objekt <BR>
  !! a) Arbeitsobjekt belegen
  INTERFACE setup_ipds_file
     MODULE PROCEDURE setup_ipds_file_w0
  END INTERFACE setup_ipds_file
  !! Setzen der Komponente "datetime" des Objektes "t_ipds" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen 
  INTERFACE setup_ipds_datetime
     MODULE PROCEDURE setup_ipds_datetime_w0 ! Arbeits-Objekt / Skalardaten
  END INTERFACE
  !
  !! Drucken aller in diesem Modul abgelegten statischen Daten <BR>
  !! die nicht zu den (Paket-) Objekten geh&ouml;ren
  INTERFACE print_ipds_static
     MODULE PROCEDURE print_ipds_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Packages
  INTERFACE print_ipds_all_errors
     MODULE PROCEDURE print_ipds_all_errors_d ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_ipds" (work-object)       <BR>
  !! a) Ausgabe des Inhalts des aktuellen (Arbeits-) Objektes
  INTERFACE print_ipds
     MODULE PROCEDURE print_ipds_w ! Version fuer Arbeits-Objekt
  END INTERFACE
  !
  !! Erzeugen von (Package-) Datenobjekten "t_ipds" (Skalar, 1D-Array) <BR>
  !! a) Erzeugen eines Objektes <BR>            
  !! b) Erzeugen vieler Objekte 
  INTERFACE new_ipds
     MODULE PROCEDURE new_ipds_0  ! Version fuer Skalar
     MODULE PROCEDURE new_ipds_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von (Package-) Datenobjekten "t_ipds" (Skalar, 1D-Array) <BR>
  !! a) Vernichten des aktuellen (Arbeits-) Objektes <BR>
  !! b) Vernichten eines Objektes mit Identifikationsnummer <BR>
  !! c) Vernichten mehrerer Objekte mit Identifikationsnummer
  INTERFACE kill_ipds
     MODULE PROCEDURE kill_ipds_w  ! Version fuer Arbeits-Objekt
     MODULE PROCEDURE kill_ipds_0  ! Version fuer ein Objekt mit ID
     MODULE PROCEDURE kill_ipds_1  ! Version fuer mehrere Objekte mit ID
  END INTERFACE
  !
  !! Pr&uuml;fen von (Package-) Datenobjekten "t_ipds" <BR>
  !! a) Pr&uuml;fen des aktuellen (Arbeits-) Objektes
  INTERFACE ok_ipds
     MODULE PROCEDURE ok_ipds_w ! Version fuer Arbeits-Objekt
  END INTERFACE
  !
  !! Anzahl der vorhandenen (Package-) Objekte "t_ipds"
  INTERFACE get_ipds_nofobjects
     MODULE PROCEDURE get_ipds_nofobjects_d
  END INTERFACE
  !! Identifikationsnummern aller vorhandenen (Package-) Objekte "t_ipds"
  INTERFACE get_ipds_all_id
     MODULE PROCEDURE get_ipds_all_id_d
  END INTERFACE
  !! Identifikationsnummer des aktuellen Arbeitsobjektes ermitteln
  INTERFACE get_ipds_work_object_id
     MODULE PROCEDURE get_ipds_work_object_id_d
  END INTERFACE
  !! Holen einer Kopie der Komponente "name" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt 
  INTERFACE get_ipds_name
     MODULE PROCEDURE get_ipds_name_w
  END INTERFACE
  !! Holen einer Kopie der Komponente "file" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_file
     MODULE PROCEDURE get_ipds_file_w
  END INTERFACE
  !! Hole Pointer auf skalare Komponente "datetime" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_datetime
     MODULE PROCEDURE get_ipds_datetime_w0
  END INTERFACE
  !! Holen der Text-Bezeichnung der Tiefenangaben-Art f&uuml;r die unerodierbare Schicht
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_rigid_layer_depth_type
     MODULE PROCEDURE get_ipds_rl_depth_type_w
  END INTERFACE
  !! Holen der Text-Bezeichnung des Typs der Bodenrauheit f&uuml;r die unerodierbare Schicht
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_rigid_layer_rough_type
     MODULE PROCEDURE get_ipds_rl_rough_type_w
  END INTERFACE
  !
  !! Anzahl Regionen fuer ein IPDS-Objekt veroeffentlichen
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_nof_regions
     MODULE PROCEDURE get_ipds_nof_regions_w
  END INTERFACE
  !! Die Namen einer oder aller Region/en eines IPDS-Objektes veroeffentlichen
  !! a) fuer eine Region   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle Regionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_region_name
     MODULE PROCEDURE get_ipds_region_name_wi
     MODULE PROCEDURE get_ipds_region_name_w1
  END INTERFACE
  !! Die Anzahl Grenzpunkte einer oder aller Region/en eines IPDS-Objektes veroeffentlichen
  !! a) fuer eine Region   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle Regionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_region_nof_border
     MODULE PROCEDURE get_ipds_region_nof_border_wi
     MODULE PROCEDURE get_ipds_region_nof_border_w1
  END INTERFACE
  !! Die Grenzpunkte einer Region eines IPDS-Objektes veroeffentlichen
  !! a) fuer eine Region / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_region_border
     MODULE PROCEDURE get_ipds_region_border_wi
  END INTERFACE
  !
  !! Anzahl Messpositionen (sampling_points) fuer ein IPDS-Objekt veroeffentlichen
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_nof_mespos
     MODULE PROCEDURE get_ipds_nof_mespos_w
  END INTERFACE
  !! Die Namen einer oder aller Messposition/en eines IPDS-Objektes veroeffentlichen
  !! a) fuer eine Messposition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle Messpositionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_mespos_name
     MODULE PROCEDURE get_ipds_mespos_name_wi
     MODULE PROCEDURE get_ipds_mespos_name_w1
  END INTERFACE
  !! Die xy-Koordinaten einer oder aller Messposition/en eines IPDS-Objektes veroeffentlichen
  !! a) fuer eine Messposition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle Messpositionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_mespos_coor
     MODULE PROCEDURE get_ipds_mespos_coor_wi
     MODULE PROCEDURE get_ipds_mespos_coor_w1
  END INTERFACE
  !! Die Anzahl phys. Datensaetze einer oder aller Messposition/en eines IPDS-Objektes veroeffentlichen
  !! a) fuer eine Messposition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle Messpositionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_mespos_nof_physet
     MODULE PROCEDURE get_ipds_mespos_nof_physet_wi
     MODULE PROCEDURE get_ipds_mespos_nof_physet_w1
  END INTERFACE
  !! Die Namen der phys. Datensaetze einer Messposition eines IPDS-Objektes veroeffentlichen
  !! a) fuer eine Messposition   / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_mespos_phyval_name
     MODULE PROCEDURE get_ipds_mespos_phyval_name_wi
  END INTERFACE
  !! Die Anzahl Namens-Variationen aller phys. Datensaetze einer Messposition eines IPDS-Objektes veroeffentlichen
  !! a) fuer alle Datensaetze / fuer eine Messposition / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_mespos_nof_var_name
     MODULE PROCEDURE get_ipds_mespos_nof_var_name_wi
  END INTERFACE
  !! Die Namens-Variationen eines phys. Datensatzes einer Messposition eines IPDS-Objektes veroeffentlichen
  !! a) fuer einen Datensatz / fuer eine Messposition / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_mespos_var_name
     MODULE PROCEDURE get_ipds_mespos_var_name_wi
  END INTERFACE
  !! Die Datenwert-Variationen eines phys. Datensatzes einer Messposition eines IPDS-Objektes veroeffentlichen
  !! a) fuer einen Datensatz / fuer eine Messposition / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_mespos_val
     MODULE PROCEDURE get_ipds_mespos_val_wi
  END INTERFACE
  !
  !! Anzahl regional abweichender phys. Werte-Definitionen (regphysets) fuer ein IPDS-Objekt
  !! veroeffentlichen
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_nof_regphysets
     MODULE PROCEDURE get_ipds_nof_regphysets_w
  END INTERFACE
  !! Namen der zugehoerigen Regionen einer oder aller regional abweichender phys. Werte-Definition/en
  !! (regphyset) fuer ein IPDS-Objekt veroeffentlichen
  !! a) fuer eine regionale  phys. Werte-Definition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle regionalen phys. Werte-Definitionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_regphyset_region_name
     MODULE PROCEDURE get_ipds_regphyset_regname_wi
     MODULE PROCEDURE get_ipds_regphyset_regname_w1
  END INTERFACE
  !! Innerhalb/Ausserhalb-Region-Kennungen einer oder aller regional abweichender phys. Werte-Definition/en
  !! (regphyset) fuer ein IPDS-Objekt veroeffentlichen
  !! a) fuer eine regionale  phys. Werte-Definition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle regionalen phys. Werte-Definitionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_regphyset_region_inout
     MODULE PROCEDURE get_ipds_regphyset_reginout_wi
     MODULE PROCEDURE get_ipds_regphyset_reginout_w1
  END INTERFACE
  !! Hoehenuntergrenzen einer oder aller regional abweichender phys. Werte-Definition/en
  !! (regphyset) fuer ein IPDS-Objekt veroeffentlichen
  !! a) fuer eine regionale  phys. Werte-Definition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle regionalen phys. Werte-Definitionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_regphyset_region_zmin
     MODULE PROCEDURE get_ipds_regphyset_regzmin_wi
     MODULE PROCEDURE get_ipds_regphyset_regzmin_w1
  END INTERFACE
  !! Hoehenobergrenzen einer oder aller regional abweichender phys. Werte-Definition/en
  !! (regphyset) fuer ein IPDS-Objekt veroeffentlichen
  !! a) fuer eine regionale  phys. Werte-Definition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle regionalen phys. Werte-Definitionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_regphyset_region_zmax
     MODULE PROCEDURE get_ipds_regphyset_regzmax_wi
     MODULE PROCEDURE get_ipds_regphyset_regzmax_w1
  END INTERFACE
  !! Anzahl zugehoeriger Messstationen einer oder aller regional abweichender phys. Werte-Definition/en
  !! (regphyset) fuer ein IPDS-Objekt veroeffentlichen
  !! a) fuer eine regionale  phys. Werte-Definition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle regionalen phys. Werte-Definitionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_regphyset_nof_mespos
     MODULE PROCEDURE get_ipds_regphyset_nofmespos_wi
     MODULE PROCEDURE get_ipds_regphyset_nofmespos_w1
  END INTERFACE
  !! Namen zugehoeriger Messstationen einer regional abweichenden phys. Werte-Definition
  !! (regphyset) fuer ein IPDS-Objekt veroeffentlichen
  !! a) fuer eine regionale  phys. Werte-Definition   / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_regphyset_mespos_name
     MODULE PROCEDURE get_ipds_regphyset_mesposnam_wi
  END INTERFACE
  !! Messstation-Interpolations-Abstaende einer oder aller regional abweichender phys. Werte-Definition/en
  !! (regphyset) fuer ein IPDS-Objekt veroeffentlichen
  !! a) fuer eine regionale  phys. Werte-Definition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle regionalen phys. Werte-Definitionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_regphyset_mespos_ipmax
     MODULE PROCEDURE get_ipds_regphyset_maxdist_wi
     MODULE PROCEDURE get_ipds_regphyset_maxdist_w1
  END INTERFACE
  !! Interpolationsmethoden-Namen einer oder aller regional abweichender phys. Werte-Definition/en
  !! (regphyset) fuer ein IPDS-Objekt veroeffentlichen
  !! a) fuer eine regionale  phys. Werte-Definition   / f&uuml;r das aktuelle Arbeitsobjekt
  !! b) fuer alle regionalen phys. Werte-Definitionen / f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_regphyset_ip_name
     MODULE PROCEDURE get_ipds_regphyset_ip_name_wi
     MODULE PROCEDURE get_ipds_regphyset_ip_name_w1
  END INTERFACE
  !
  !! Hole Zeiger auf Feld-Komponente "dim" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_dim
     MODULE PROCEDURE get_ipds_dim_w1
  END INTERFACE
  !! Hole Zeiger auf Feld-Komponente "att" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_att
     MODULE PROCEDURE get_ipds_att_w1
  END INTERFACE
  !! Hole Zeiger auf Feld-Komponente "var" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE get_ipds_var
     MODULE PROCEDURE get_ipds_var_w1
  END INTERFACE
  
  !! Pr&uuml;fe ob Pointer f&uuml;r "datetime" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  INTERFACE target_ipds_datetime
     MODULE PROCEDURE target_ipds_datetime_w0
  END INTERFACE
  !
  !! Lesen der Daten von Datei <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt <BR>
  !! b) f&uuml;r das aktuelle Arbeitsobjekt mit expliziter Angabe der Datei
  INTERFACE read_ipds
     MODULE PROCEDURE read_ipds_w
     MODULE PROCEDURE read_ipds_wf
  END INTERFACE
  !
  !! Hole die Anzahl der implementierten Datei-Varianten
  INTERFACE get_ipds_nof_variants
     MODULE PROCEDURE get_ipds_nof_variants_d
  END INTERFACE
  !! Hole den Datei-Type f&uuml;r das Paket
  INTERFACE get_ipds_variants_type
     MODULE PROCEDURE get_ipds_variants_type_d
     MODULE PROCEDURE get_ipds_variants_type_0
  END INTERFACE
  !! Hole FORTRAN-Form der implementierten Datei-Varianten
  INTERFACE get_ipds_variants_form
     MODULE PROCEDURE get_ipds_variants_form_d
     MODULE PROCEDURE get_ipds_variants_form_0
  END INTERFACE
  !! Hole FORTRAN-Access der implementierten Datei-Varianten
  INTERFACE get_ipds_variants_access
     MODULE PROCEDURE get_ipds_variants_access_d
     MODULE PROCEDURE get_ipds_variants_access_0
  END INTERFACE
  !! Hole FORTRAN-Delimiter der implementierten Datei-Varianten
  INTERFACE get_ipds_variants_delim
     MODULE PROCEDURE get_ipds_variants_delim_d
     MODULE PROCEDURE get_ipds_variants_delim_0
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  ! 
  !! Hole Datenwert/e einer/mehrerer physikalischer Groessen
  INTERFACE get_ipds_val
     MODULE PROCEDURE get_ipds_val_point0_phy0 ! INP: ein Punkt, eine phys.Groesse
     MODULE PROCEDURE get_ipds_val_xyz0_phy0   ! INP: eine xyz-Koord., eine phys.Groesse
     MODULE PROCEDURE get_ipds_val_point1_phy0 ! INP: viele Punkte, eine phys.Groesse
     MODULE PROCEDURE get_ipds_val_xyz1_phy0   ! INP: viele xyz-Koord., eine phys.Groesse
     MODULE PROCEDURE get_ipds_val_point1_phy1 ! INP: viele Punkte, viele phys.Groessen
     MODULE PROCEDURE get_ipds_val_xyz1_phy1   ! INP: viele xyz-Koord., viele phys.Groessen
  END INTERFACE
  !
  !! Pr&uuml;fen, ob Daten f&uuml;r eine bestimmte phys. Gr&ouml;&szlig;e
  !! vorhanden sind oder nicht <BR>
  !! a) f&uuml;r einen Namen <BR>
  !! b) f&uuml;r eine Liste von Namen
  INTERFACE is_ipds_physet_name_available
     MODULE PROCEDURE is_ipds_physet_name_available_0
     MODULE PROCEDURE is_ipds_physet_name_available_1
  END INTERFACE
  !! Pr&uuml;fen, ob ein Punkt innerhalb eines Polygons (Region) liegt oder nicht <BR>
  !! a) f&uuml;r einen Punkt und eine Region (Polygon) <BR>
  !! b) f&uuml;r mehrere Punkte und eine Region (Polygon)
  INTERFACE is_ipds_point_in_region
     MODULE PROCEDURE is_ipds_point_in_region_0
     MODULE PROCEDURE is_ipds_point_in_region_1
  END INTERFACE
  !! Pr&uuml;fen, ob ein Polygon (Region) definiert ist oder nicht <BR>
  !! a) f&uuml;r einen Namen und eine Region (Polygon) <BR>
  !! b) f&uuml;r mehrere Namen und eine Region (Polygon)
  INTERFACE is_ipds_region_defined
     MODULE PROCEDURE is_ipds_region_defined_0
     MODULE PROCEDURE is_ipds_region_defined_1
  END INTERFACE
  !
  !! Erzeuge alle Austauschgr&ouml;&szlig;en mit OpenMI-konformen Daten 
  !! f&uuml;r eine Liste von Arbeitsobjekten 
  INTERFACE get_ipds_exch_r
     MODULE PROCEDURE get_ipds_exch_r_1
  END INTERFACE
  !! Ermittle den Zeithorizont in OpenMI-konformer Weise f&uuml;r 
  !! f&uuml;r eine Liste von Arbeitsobjekten 
  INTERFACE get_ipds_time_horizon
     MODULE PROCEDURE get_ipds_time_horizon_1
  END INTERFACE
  !! Ermittle, ob diskrete Zeitangaben in OpenMI-konformer Weise f&uuml;r 
  !! ein Arbeitsobjekt und eine Austauschgr&ouml;&szlig;e vorhanden sind
  INTERFACE get_ipds_has_discr_time
     MODULE PROCEDURE get_ipds_has_discr_time_w
  END INTERFACE
  !! Ermittle, ob eine Zeitraumangabe in OpenMI-konformer Weise f&uuml;r 
  !! ein Arbeitsobjekt und eine Austauschgr&ouml;&szlig;e vorhanden sind
  INTERFACE get_ipds_has_time_span
     MODULE PROCEDURE get_ipds_has_time_span_w
  END INTERFACE
  !! Ermittle, wie viele Zeitangaben in OpenMI-konformer Weise f&uuml;r 
  !! ein Arbeitsobjekt und eine Austauschgr&ouml;&szlig;e vorhanden sind
  INTERFACE get_ipds_nof_discr_time
     MODULE PROCEDURE get_ipds_nof_discr_time_w
  END INTERFACE
  !! Ermittle eine bestimmte Zeitangabe in OpenMI-konformer Weise f&uuml;r 
  !! ein Arbeitsobjekt und eine Austauschgr&ouml;&szlig;e anhand der lfd. Nummer der Zeitangabe
  INTERFACE get_ipds_discr_time
     MODULE PROCEDURE get_ipds_discr_time_w
  END INTERFACE
  !! Ermittle die Angabe eines Zeitraums in OpenMI-konformer Weise f&uuml;r 
  !! ein Arbeitsobjekt und eine Austauschgr&ouml;&szlig;e anhand der lfd. Nummer der Zeitangabe
  INTERFACE get_ipds_time_span
     MODULE PROCEDURE get_ipds_time_span_w
  END INTERFACE
  !! Holen skalarer Daten aus vektoriellen oder skalaren Originaldaten
  INTERFACE get_ipds_scalar_var
     MODULE PROCEDURE get_ipds_scalar_var_w
  END INTERFACE
  !! Holen vektorieller Daten
  INTERFACE get_ipds_vector_var
     MODULE PROCEDURE get_ipds_vector_var_w
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! [C.6] Operatoren
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: c_len_phyval_name
  !
  PUBLIC :: init_ipds
  PUBLIC :: clear_ipds
  PUBLIC :: setup_ipds_prn_lun
  PUBLIC :: setup_ipds_trc_lun
  PUBLIC :: setup_ipds_asc_seq_lun
  PUBLIC :: print_ipds_static
  PUBLIC :: print_ipds_all_errors
  !
  PUBLIC :: get_ipds_nof_variants
  PUBLIC :: get_ipds_variants_type
  PUBLIC :: get_ipds_variants_form
  PUBLIC :: get_ipds_variants_access
  PUBLIC :: get_ipds_variants_delim
  !
  PUBLIC :: new_ipds
  PUBLIC :: kill_ipds
  PUBLIC :: print_ipds
  PUBLIC :: ok_ipds
  PUBLIC :: get_ipds_nofobjects
  PUBLIC :: get_ipds_all_id
  !
  PUBLIC :: setup_ipds_work_object
  PUBLIC :: setup_ipds_name
  PUBLIC :: setup_ipds_file
  PUBLIC :: setup_ipds_datetime
  !
  PUBLIC :: get_ipds_work_object_id
  PUBLIC :: get_ipds_name
  PUBLIC :: get_ipds_file
  PUBLIC :: get_ipds_datetime
  PUBLIC :: get_ipds_rigid_layer_depth_type
  PUBLIC :: get_ipds_rigid_layer_rough_type
  !
  PUBLIC :: get_ipds_nof_regions
  PUBLIC :: get_ipds_region_name
  PUBLIC :: get_ipds_region_nof_border
  PUBLIC :: get_ipds_region_border
  !
  PUBLIC :: get_ipds_nof_mespos
  PUBLIC :: get_ipds_mespos_name
  PUBLIC :: get_ipds_mespos_coor
  PUBLIC :: get_ipds_mespos_nof_physet
  PUBLIC :: get_ipds_mespos_phyval_name
  PUBLIC :: get_ipds_mespos_nof_var_name
  PUBLIC :: get_ipds_mespos_var_name
  PUBLIC :: get_ipds_mespos_val
  !
  PUBLIC :: get_ipds_nof_regphysets
  PUBLIC :: get_ipds_regphyset_region_name
  PUBLIC :: get_ipds_regphyset_region_inout
  PUBLIC :: get_ipds_regphyset_region_zmin
  PUBLIC :: get_ipds_regphyset_region_zmax
  PUBLIC :: get_ipds_regphyset_nof_mespos
  PUBLIC :: get_ipds_regphyset_mespos_name
  PUBLIC :: get_ipds_regphyset_mespos_ipmax
  PUBLIC :: get_ipds_regphyset_ip_name
  !
  PUBLIC :: get_ipds_dim 
  PUBLIC :: get_ipds_var
  PUBLIC :: get_ipds_att
  !
  PUBLIC :: target_ipds_datetime
  !
  PUBLIC :: read_ipds
  !
  PUBLIC :: get_ipds_val
  !
  PUBLIC :: is_ipds_physet_name_available
  PUBLIC :: is_ipds_point_in_region
  PUBLIC :: is_ipds_region_defined
  !
  PUBLIC :: get_ipds_exch_r               ! 
  PUBLIC :: get_ipds_time_horizon         ! 
  PUBLIC :: get_ipds_has_discr_time       ! 
  PUBLIC :: get_ipds_has_time_span        ! 
  PUBLIC :: get_ipds_nof_discr_time       ! 
  PUBLIC :: get_ipds_discr_time           ! 
  PUBLIC :: get_ipds_time_span            ! 
  PUBLIC :: get_ipds_scalar_var           ! 
  PUBLIC :: get_ipds_vector_var           ! 
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Konstantwerte (Parameter)
  !! Name des Moduls
  CHARACTER (LEN=10), PARAMETER :: c_modname = 'io_ipds_ui'
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  ! [D.4] Schnittstellen
  !
  !! Ermittle Regionenzugehoerigkeit
  INTERFACE get_p_in_region
     MODULE PROCEDURE get_p_in_region_w1
     MODULE PROCEDURE get_p_in_region_w2
  END INTERFACE
  !
  !! Ermittle die Anzahl der zu einem phys. Satz (Daten und Region)
  !! zugehoerigen Messpositionen
  INTERFACE get_nof_mespos_phyval
     MODULE PROCEDURE get_nof_mespos_phyval_w1
     MODULE PROCEDURE get_nof_mespos_phyval_w2
  END INTERFACE
  !
  !! Ermittle die Koordinaten und phys. Daten der zu einem phys. Satz (Daten und Region)
  !! zugehoerigen Messpositionen
  INTERFACE get_mespos_phyval
     MODULE PROCEDURE get_mespos_phyval_w2
     MODULE PROCEDURE get_mespos_phyval_w3
  END INTERFACE
  !
  !! Ermittle die Messwerte (REAL) aller Varianten einer phys. Groesse
  !! fuer alle Messpositionen
  INTERFACE get_mespos_val
     MODULE PROCEDURE get_mespos_val_3
  END INTERFACE
  !
  ! [D.5] Assignments
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
  ! Oeffentliche Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren der statischen Daten des Packages <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_ipds_d ( )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "io_ipds_ui" version 3.2 of 04/13/07'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_file       ( )
       IF ( no_error( ) ) CALL init_datetime   ( )
       IF ( no_error( ) ) CALL init_point_2d   ( )
       IF ( no_error( ) ) CALL init_physet     ( )
       IF ( no_error( ) ) CALL init_region     ( )
       IF ( no_error( ) ) CALL init_mespos     ( )
       IF ( no_error( ) ) CALL init_regphyset  ( )
       IF ( no_error( ) ) CALL init_phyval     ( )
       IF ( no_error( ) ) CALL init_omi_exch_r ( )
       IF ( no_error( ) ) CALL init_omi_span   ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_ipds_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun     = c_lun
       prn_op      = c_op
       trc_lun     = c_lun
       trc_op      = c_op
       asc_seq_lun = c_asc_seq_lun 
       ! [1.6] Initialisieren einiger statischer Variablen
       nofobjects = 0
       NULLIFY ( first_list_object ) 
       NULLIFY ( work_list_object ) 
       NULLIFY ( work_object  ) 
       ! [1.7] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_ipds_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_ipds_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_ipds_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun     = c_lun
       prn_op      = c_op
       trc_lun     = c_lun
       trc_op      = c_op
       asc_seq_lun = c_asc_seq_lun 
       ! [1.3] ggf. weitere De-Initialsierungsmethoden rufen
       IF ( no_error( ) ) CALL clear_ipds_all_objects ( )
       ! [1.4] Re-Initialisieren einiger statischer Variablen
       nofobjects = 0
       NULLIFY ( first_list_object ) 
       NULLIFY ( work_list_object ) 
       NULLIFY ( work_object  ) 
       ! [1.5] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.6] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.6.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_exch_r ( )
       IF ( no_error( ) ) CALL clear_omi_span   ( )
       IF ( no_error( ) ) CALL clear_file       ( )
       IF ( no_error( ) ) CALL clear_datetime   ( )
       IF ( no_error( ) ) CALL clear_point_2d   ( )
       IF ( no_error( ) ) CALL clear_physet     ( )
       IF ( no_error( ) ) CALL clear_region     ( )
       IF ( no_error( ) ) CALL clear_mespos     ( )
       IF ( no_error( ) ) CALL clear_regphyset  ( )
       IF ( no_error( ) ) CALL clear_phyval     ( )
       ! [1.6.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_ipds_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ipds_prn_lun_d ( lun )
    !
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_ipds_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       ! [1.1] "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun      ( lun )
       IF ( no_error( ) ) CALL setup_file_prn_lun       ( lun )
       IF ( no_error( ) ) CALL setup_datetime_prn_lun   ( lun )
       IF ( no_error( ) ) CALL setup_point_2d_prn_lun   ( lun )
       IF ( no_error( ) ) CALL setup_physet_prn_lun     ( lun )
       IF ( no_error( ) ) CALL setup_region_prn_lun     ( lun )
       IF ( no_error( ) ) CALL setup_mespos_prn_lun     ( lun )
       IF ( no_error( ) ) CALL setup_regphyset_prn_lun  ( lun )
       IF ( no_error( ) ) CALL setup_phyval_prn_lun     ( lun )
       IF ( no_error( ) ) CALL setup_omi_exch_r_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_span_prn_lun   ( lun )
       ! [1.2] Setzen der lokalen statischen Daten
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
    END IF
    !
  END SUBROUTINE setup_ipds_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ipds_trc_lun_d ( lun )
    !
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_ipds_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       ! [1.1] "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_file_trc_lun       ( lun )
       IF ( no_error( ) ) CALL setup_datetime_trc_lun   ( lun )
       IF ( no_error( ) ) CALL setup_point_2d_trc_lun   ( lun )
       IF ( no_error( ) ) CALL setup_physet_trc_lun     ( lun )
       IF ( no_error( ) ) CALL setup_region_trc_lun     ( lun )
       IF ( no_error( ) ) CALL setup_mespos_trc_lun     ( lun )
       IF ( no_error( ) ) CALL setup_regphyset_trc_lun  ( lun )
       IF ( no_error( ) ) CALL setup_phyval_trc_lun     ( lun )
       IF ( no_error( ) ) CALL setup_omi_exch_r_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_span_trc_lun   ( lun )
       ! [1.2] Setzen der lokalen statischen Daten
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
    END IF
    !
  END SUBROUTINE setup_ipds_trc_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r Input von Sequential, Ascii <BR>
  !! wird beim &Ouml;ffnen der Datei verwendet, falls "file%unit" negativ ist <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ipds_asc_seq_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r Input von Sequential, Ascii 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_ipds_asc_seq_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       asc_seq_lun = MERGE( lun, c_asc_seq_lun, lun > 0 )
    END IF
    !
  END SUBROUTINE setup_ipds_asc_seq_lun_d
  !
  !! Setzen des (Arbeits-) Objekts mit dem gearbeitet werden soll
  SUBROUTINE setup_ipds_work_object_d ( id )
    !! Identifikationsnummer des (Arbeits-) Objekts
    INTEGER , INTENT(IN) :: id ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_ipds_work_object_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       work_list_object => get_ipds_list_object ( id )
       work_object      => work_list_object%object
    END IF
    !
  END SUBROUTINE setup_ipds_work_object_d
  !
  !! Setzen der Komponente "name" des (Arbeits-) Objektes mit Skalardaten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ipds_name_w0 ( val )
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_ipds_name_w0' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_name_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_ipds_name_w0
  !
  !! Setzen der Komponente "file" des (Arbeits-) Objektes mit Skalardaten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ipds_file_w0 ( val )
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "file"
    TYPE (t_file) , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_ipds_file_w0' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_file_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_ipds_file_w0
  !
  !! Setzen der Komponente "datetime" des (Arbeits-) Objektes mit Skalardaten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ipds_datetime_w0 ( val )
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "datetime"
    CHARACTER (LEN=*) , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_ipds_datetime_w0' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_datetime_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_ipds_datetime_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren eines neuen (Package-) Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_ipds_0 ( id )
    !! Identifikationsnummer des Datenobjekts (Skalar) <BR>
    !! id = -1 : Allokieren/Initialisieren fehlgeschlagen
    INTEGER, INTENT(INOUT) :: id ! 
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='new_ipds_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       CALL new_ipds_object ( id )
    ELSE
       id = -1
    END IF
    !
  END SUBROUTINE new_ipds_0
  !
  !! Allokieren/Initialisieren mehrerer neuer (Package-) Datenobjekte <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_ipds_1 ( id )
    !! Identifikationsnummern des Datenobjekts (Vektor) <BR>
    !! id = -1 : Allokieren/Initialisieren fehlgeschlagen
    INTEGER, INTENT(INOUT) :: id(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='new_ipds_1' 
    !! Zaehler
    INTEGER :: i ! 
    !
    id = -1
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(id)
          IF ( any_error( ) ) EXIT
          CALL new_ipds_0 ( id(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_ipds_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren des aktuellen (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_ipds_w ( )
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='kill_ipds_w' 
    !
    IF ( ok_work_object ( c_upname ) ) THEN
       CALL kill_ipds_0 ( work_object%id )
    END IF
    !
  END SUBROUTINE kill_ipds_w
  !
  !! De-Allokieren/De-Initialisieren eines (Package-) Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_ipds_0 ( id )
    !! Identifikationsnummer des Datenobjekts (Skalar)
    INTEGER , INTENT(IN) :: id ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='kill_ipds_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       CALL kill_ipds_object ( id )
    END IF
    !
  END SUBROUTINE kill_ipds_0
  !
  !! De-Allokieren/De-Initialisieren mehrerer (Package-) Datenobjekte <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_ipds_1 ( id )
    !! Identifikationsnummern der Datenobjekte (Vektor)
    INTEGER , INTENT(IN) :: id(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='kill_ipds_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(id)
          IF ( any_error( ) ) EXIT
          CALL kill_ipds_0 ( id(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_ipds_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob das aktuelle Arbeits-Objekt ein g&uuml;ltiges Datenobjekt ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_w ( ) &
       RESULT( ok )
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=09), PARAMETER :: c_upname='ok_ipds_w' 
    !
    IF ( ok_work_object ( c_upname ) ) THEN
       ok = ok_ipds_object ( work_object )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION ok_ipds_w
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt des aktuellen Arbeits-Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_w ( )
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='print_ipds_w' 
    !
    IF ( ok_work_object ( c_upname ) .AND. ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_ipds_object ( work_object )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_ipds_w
  !
  !! Drucken aller statischen Daten des Packages (ohne Daten der Package-Objekte) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_static_d ( )
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_ipds_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_ipds_global ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_ipds_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler des Packages <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_ipds_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_ipds_errors ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_ipds_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! ermittle die Anzahl der (Package-) Objekte "t_ipds" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_nofobjects_d ( ) &
       RESULT( val ) 
    !! R&uuml;ckgabewert : Anzahl der vorhandenen Objekte "t_ipds"
    INTEGER :: val  ! 
    !
    val = nofobjects
    !
  END FUNCTION get_ipds_nofobjects_d
  !
  !! ermittle die Identifikationsnummern der aktuell vorhandenen (Package-) Objekte "t_ipds" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_all_id_d ( ) &
       RESULT( id )
    !! R&uuml;ckgabewert : Identifikationsnummern der vorhandenen Objekte "t_ipds" <BR>
    !! falls "not associated" : es sind keine Objekte vorhanden
    INTEGER , POINTER :: id(:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_ipds_all_id_d' 
    !! Zeiger auf aktuelles Datenobjekt "t_ipds_list" 
    TYPE (t_ipds_list) , POINTER :: this ! 
    !! Z&auml;hler
    INTEGER :: i    ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( nofobjects > 0 ) THEN
       ALLOCATE( id(nofobjects), STAT=stat )
       IF ( stat == 0 ) THEN
          id    = -1
          this => first_list_object
          i     = 0
          DO 
             i = i + 1
             IF ( i > nofobjects .OR. .NOT. ASSOCIATED( this ) ) EXIT
             id(i) = this%object%id
             this => this%next
          END DO
          NULLIFY ( this )
       ELSE
          CALL setup_error_act ( all_errors(:), 9001, c_upname, c_modname, stat )
       END IF
    ELSE
       NULLIFY ( id )
    END IF
    !
  END FUNCTION get_ipds_all_id_d
  !
  !! ermittle die Identifikationsnummer des aktuellen Arbeits-Objektes <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_work_object_id_d ( ) &
       RESULT( id )
    !! R&uuml;ckgabewert : Identifikationsnummer des aktuellen Arbeits-Objektes <BR>
    !! id = -1 : es existiert kein "work object"
    INTEGER :: id ! 
    !! Name der Function
    CHARACTER (LEN=25), PARAMETER :: c_upname='get_ipds_work_object_id_d' 
    !
    id = -1
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ASSOCIATED( work_object ) ) id = work_object%id
    END IF
    !
  END FUNCTION get_ipds_work_object_id_d
  !
  !! Holen der Komponente "name" des (Arbeits-) Objektes <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_name_w ( ) &
       RESULT ( val )
    !! Ergebniswert : Kopie der Komponente "name"
    CHARACTER (LEN=80) :: val ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='get_ipds_name_w' 
    !
    val = REPEAT( ' ', LEN( val ) )
    val = 'UNDEFINED'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val = get_name_object ( work_object ) 
       END IF
    END IF
    !
  END FUNCTION get_ipds_name_w
  !
  !! Holen der Komponente "file" des (Arbeits-) Objektes <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_file_w ( ) &
       RESULT ( val )
    !! Ergebniswert: Kopie der Komponente "file"
    TYPE (t_file) :: val ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='get_ipds_file_w' 
    !
    CALL new_file ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val = get_file_object ( work_object ) 
       END IF
    END IF
    !
  END FUNCTION get_ipds_file_w
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO = 21000 bis 21999]
  ! ----------------------------------------------------------------------
  !
  !! Hole Pointer auf Komponente "datetime" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_datetime_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "datetime"
    TYPE (t_datetime) , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='get_ipds_datetime_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_datetime_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_ipds_datetime_w0
  !
  !! Holen der Text-Bezeichnung der Tiefenangaben-Art f&uuml;r die unerodierbare Schicht
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_rl_depth_type_w ( ) &
       RESULT ( type )
    !! Ergebniswert : Bezeichnung der Tiefenangaben-Art
    CHARACTER (LEN=80) :: type
    !! Index auf die Bezeichnung der erforderlichen physikalischen Groesse
    !! im Array c_phy_name_de
    INTEGER           , PARAMETER :: c_idx_phy_name_de=9
    !! Alle Angaben zu einer physikalischen Groesse
    TYPE (t_phyval)      :: phyval
    !! Anzahl Type-Angaben zu einer physikalischen Groesse
    INTEGER              :: phyval_nof_type
    !! Type-Nummern zu einer physikalischen Groesse
    INTEGER, POINTER :: phyval_type(:)
    !! Physikalische Groesse (definiert durch c_idx_phy_name_de) gefunden?
    LOGICAL              :: found
    !! Zaehler Messstationen
    INTEGER              :: i_mespos
    !! Alle Angaben zu einem Satz physikalischer Groessen
    TYPE (t_physet)      :: physet
    !
    type = REPEAT( ' ', LEN( type ) )
    type = c_rl_depth_type( 0 )
    !
    ! Alle Stellen im Arbeitsobjekt durchsuchen, die Angaben zur phys. Groesse (c_idx_phy_name_de)
    ! enthalten koennen. Es sind dies im Einzelnen:
    ! 1.) Die Komponente physet mit den Angaben zu den Default-Werten
    ! 2.) Die Komponente mespos, die fuer jedes Element ebenfalls einen Satz phys. Groessen
    !     enthaelt
    !
    ! [1] Suche in den Default-Angaben
    !
    IF ( associated_physet_object( work_object ) ) THEN
       !
       found  = .FALSE.
       !
       phyval = get_physet_set( work_object%physet, TRIM( c_phy_name_de( c_idx_phy_name_de ) ) )
       !
       phyval_nof_type = get_phyval_nof_type( phyval )
       !
       IF ( phyval_nof_type > 0 ) THEN
          !
          IF( no_error() ) ALLOCATE( phyval_type( phyval_nof_type ) )
          IF( no_error() ) phyval_type = get_phyval_type( phyval )
          !
          IF( no_error() ) type = c_rl_depth_type( phyval_type( 1 ) )
          !
          IF( no_error() ) DEALLOCATE( phyval_type )
          !
          IF( no_error() ) CALL kill_phyval( phyval )
          !
          found = .TRUE.
          !
       END IF
       !
    END IF
    !
    ! [1] Suche in allen Messstationen
    !
    IF ( associated_mespos_object( work_object ) ) THEN
       !
       i_mespos = 0
       !
       DO
          !
          IF ( found ) EXIT
          !
          i_mespos = i_mespos + 1
          IF ( i_mespos > SIZE( work_object%mespos ) ) EXIT
          !
          physet = get_mespos_physet( work_object%mespos( i_mespos ) )
          !
          phyval = get_physet_set( physet, TRIM( c_phy_name_de( c_idx_phy_name_de ) ) )
          !
          phyval_nof_type = get_phyval_nof_type( phyval )
          !
          IF ( phyval_nof_type > 0 ) THEN
             !
             IF( no_error() ) ALLOCATE( phyval_type( phyval_nof_type ) )
             IF( no_error() ) phyval_type = get_phyval_type( phyval )
             !
             IF( no_error() ) type = c_rl_depth_type( phyval_type( 1 ) )
             !
             IF( no_error() ) DEALLOCATE( phyval_type )
             !
             IF( no_error() ) CALL kill_phyval( phyval )
             !
             found = .TRUE.
             !
          END IF
          !
          CALL kill_physet( physet )
          !
       END DO
       !
    END IF
    !
  END FUNCTION get_ipds_rl_depth_type_w
  !
  !! Holen der Text-Bezeichnung des Typs der Bodenrauheit f&uuml;r die unerodierbare Schicht
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_rl_rough_type_w ( ) &
       RESULT ( type )
    !
    !! Ergebniswert : Bezeichnung der Bodenrauheit
    CHARACTER (LEN=80) :: type
    !! Index auf die Bezeichnung der erforderlichen physikalischen Groesse
    !! im Array c_phy_name_de
    INTEGER           , PARAMETER :: c_idx_phy_name_de=10
    !! Alle Angaben zu einer physikalischen Groesse
    TYPE (t_phyval)      :: phyval
    !! Anzahl Type-Angaben zu einer physikalischen Groesse
    INTEGER              :: phyval_nof_type
    !! Type-Nummern zu einer physikalischen Groesse
    INTEGER, POINTER :: phyval_type(:)
    !! Physikalische Groesse (definiert durch c_idx_phy_name_de) gefunden?
    LOGICAL              :: found
    !! Zaehler Messstationen
    INTEGER              :: i_mespos
    !! Alle Angaben zu einem Satz physikalischer Groessen
    TYPE (t_physet)      :: physet
    !
    type = REPEAT( ' ', LEN( type ) )
    type = c_rl_rough_type( 0 )
    !
    ! Alle Stellen im Arbeitsobjekt durchsuchen, die Angaben zur phys. Groesse (c_idx_phy_name_de)
    ! enthalten koennen. Es sind dies im Einzelnen:
    ! 1.) Die Komponente physet mit den Angaben zu den Default-Werten
    ! 2.) Die Komponente mespos, die fuer jedes Element ebenfalls einen Satz phys. Groessen
    !     enthaelt
    !
    ! [1] Suche in den Default-Angaben
    !
    IF ( associated_physet_object( work_object ) ) THEN
       !
       found  = .FALSE.
       !
       phyval = get_physet_set( work_object%physet, TRIM( c_phy_name_de( c_idx_phy_name_de ) ) )
       !
       phyval_nof_type = get_phyval_nof_type( phyval )
       !
       IF ( phyval_nof_type > 0 ) THEN
          !
          IF( no_error() ) ALLOCATE( phyval_type( phyval_nof_type ) )
          IF( no_error() ) phyval_type = get_phyval_type( phyval )
          !
          IF( no_error() ) type = c_rl_rough_type( phyval_type( 1 ) )
          !
          IF( no_error() ) DEALLOCATE( phyval_type )
          !
          IF( no_error() ) CALL kill_phyval( phyval )
          !
          found = .TRUE.
          !
       END IF
       !
    END IF
    !
    ! [1] Suche in allen Messstationen
    !
    IF ( associated_mespos_object( work_object ) ) THEN
       !
       i_mespos = 0
       !
       DO
          !
          IF ( found ) EXIT
          !
          i_mespos = i_mespos + 1
          IF ( i_mespos > SIZE( work_object%mespos ) ) EXIT
          !
          physet = get_mespos_physet( work_object%mespos( i_mespos ) )
          !
          phyval = get_physet_set( physet, TRIM( c_phy_name_de( c_idx_phy_name_de ) ) )
          !
          phyval_nof_type = get_phyval_nof_type( phyval )
          !
          IF ( phyval_nof_type > 0 ) THEN
             !
             IF( no_error() ) ALLOCATE( phyval_type( phyval_nof_type ) )
             IF( no_error() ) phyval_type = get_phyval_type( phyval )
             !
             IF( no_error() ) type = c_rl_rough_type( phyval_type( 1 ) )
             !
             IF( no_error() ) DEALLOCATE( phyval_type )
             !
             IF( no_error() ) CALL kill_phyval( phyval )
             !
             found = .TRUE.
             !
          END IF
          !
          CALL kill_physet( physet )
          !
       END DO
       !
    END IF
    !
  END FUNCTION get_ipds_rl_rough_type_w
  !
  !! Anzahl Regionen fuer ein IPDS-Objekt veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_nof_regions_w ( ) &
       RESULT ( nof )
    !
    !! Ergebniswert : Anzahl Regionen im IPDS-Arbeitsobjekt
    INTEGER :: nof
    !
    nof = 0
    IF ( associated_region_object ( work_object ) ) nof = SIZE( work_object%region )
    !
  END FUNCTION get_ipds_nof_regions_w
  !
  !! Den Namen einer Region eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_region_name_wi ( iregion ) &
       RESULT ( val )
    !
    !! Nummer der Region
    INTEGER                           :: iregion
    !! Ergebniswert : Name der Region im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_region_name) :: val
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_region_object ( work_object ) ) val = get_region_name ( work_object%region(iregion) )
    !
  END FUNCTION get_ipds_region_name_wi
  !
  !! Die Namen aller Regionen eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_region_name_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Namen aller Regionen im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_region_name) :: val(SIZE(work_object%region))
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_region_object ( work_object ) ) val = get_region_name ( work_object%region )
    !
  END FUNCTION get_ipds_region_name_w1
  !
  !! Die Anzahl Grenzpunkte einer Region eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_region_nof_border_wi ( iregion ) &
       RESULT ( val )
    !
    !! Nummer der Region
    INTEGER :: iregion
    !! Ergebniswert : Anzahl Grenzpunkte der Region im IPDS-Arbeitsobjekt
    INTEGER :: val
    !
    val = 0
    !
    IF ( associated_region_object ( work_object ) ) val = get_region_nof_border ( work_object%region(iregion) )
    !
  END FUNCTION get_ipds_region_nof_border_wi
  !
  !! Die Anzahl Grenzpunkte aller Regionen eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_region_nof_border_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Anzahl Grenzpunkte aller Regionen im IPDS-Arbeitsobjekt
    INTEGER :: val(SIZE(work_object%region))
    !
    val = 0
    !
    IF ( associated_region_object ( work_object ) ) val = get_region_nof_border ( work_object%region )
    !
  END FUNCTION get_ipds_region_nof_border_w1
  !
  !! Die Grenzpunkte einer Region eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_region_border_wi ( iregion, nof_border ) &
       RESULT ( val )
    !
    !! Nummer der Region
    INTEGER :: iregion
    !! Anzahl Borderpunkte
    INTEGER :: nof_border
    !! Ergebniswert : Grenzpunkte der Region im IPDS-Arbeitsobjekt
    TYPE (t_point_2d) :: val(nof_border)
    !
    IF ( associated_region_object ( work_object ) ) val = get_region_border ( work_object%region(iregion) )
    !
  END FUNCTION get_ipds_region_border_wi
  !
  !! Anzahl Messpositionen (sampling_points) fuer ein IPDS-Objekt veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_nof_mespos_w ( ) &
       RESULT ( nof )
    !
    !! Ergebniswert : Anzahl Messpositionen im IPDS-Arbeitsobjekt
    INTEGER :: nof
    !
    nof = 0
    IF ( associated_mespos_object ( work_object ) ) nof = SIZE( work_object%mespos )
    !
  END FUNCTION get_ipds_nof_mespos_w
  !
  !! Den Namen einer Messposition eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_name_wi ( imespos ) &
       RESULT ( val )
    !
    !! Nummer der Messposition
    INTEGER                           :: imespos
    !! Ergebniswert : Name der Messposition im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_mespos_name) :: val
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_mespos_object ( work_object ) ) val = get_mespos_name ( work_object%mespos(imespos) )
    !
  END FUNCTION get_ipds_mespos_name_wi
  !
  !! Die Namen aller Messpositionen eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_name_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Namen aller Messpositionen im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_mespos_name) :: val(SIZE(work_object%mespos))
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_mespos_object ( work_object ) ) val = get_mespos_name ( work_object%mespos )
    !
  END FUNCTION get_ipds_mespos_name_w1
  !
  !! Die xy-Koordinaten einer Messposition eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_coor_wi ( imespos ) &
       RESULT ( val )
    !
    !! Nummer der Messposition
    INTEGER           :: imespos
    !! Ergebniswert : xy-Koordinaten der Messposition im IPDS-Arbeitsobjekt
    TYPE (t_point_2d) :: val
    !
    IF ( associated_mespos_object ( work_object ) ) val = get_mespos_coor ( work_object%mespos(imespos) )
    !
  END FUNCTION get_ipds_mespos_coor_wi
  !
  !! Die xy-Koordinaten aller Messpositionen eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_coor_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : xy-Koordinaten aller Messpositionen im IPDS-Arbeitsobjekt
    TYPE (t_point_2d) :: val(SIZE(work_object%mespos))
    !
    IF ( associated_mespos_object ( work_object ) ) val = get_mespos_coor ( work_object%mespos )
    !
  END FUNCTION get_ipds_mespos_coor_w1
  !
  !! Die Anzahl phys. Datensaetze einer Messposition eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_nof_physet_wi ( imespos ) &
       RESULT ( val )
    !
    !! Nummer der Messposition
    INTEGER :: imespos
    !! Ergebniswert : Anzahl phys. Datensaetze der Messposition im IPDS-Arbeitsobjekt
    INTEGER :: val
    !
    val = 0
    !
    IF ( associated_mespos_object ( work_object ) ) val = get_mespos_nof_physet_set ( work_object%mespos(imespos) )
    !
  END FUNCTION get_ipds_mespos_nof_physet_wi
  !
  !! Die Anzahl phys. Datensaetze aller Messpositionen eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_nof_physet_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Anzahl phys. Datensaetze aller Messpositionen im IPDS-Arbeitsobjekt
    INTEGER :: val(SIZE(work_object%mespos))
    !
    val = 0
    !
    IF ( associated_mespos_object ( work_object ) ) val = get_mespos_nof_physet_set ( work_object%mespos )
    !
  END FUNCTION get_ipds_mespos_nof_physet_w1
  !
  !! Die Namen der phys. Datensaetze einer Messposition eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_phyval_name_wi ( imespos, nof_physet ) &
       RESULT ( val )
    !
    !! Nummer der Messposition
    INTEGER :: imespos
    !! Anzahl Datensaetze
    INTEGER :: nof_physet
    !! Ergebniswert : Namen der phys. Datensaetze der Messposition im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_phyval_name) :: val(nof_physet)
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_mespos_object ( work_object ) ) &
         val = get_phyval_name ( get_physet_set ( get_mespos_physet ( work_object%mespos(imespos) ) ) )
    !
  END FUNCTION get_ipds_mespos_phyval_name_wi
  !
  !! Die Anzahl Namens-Variationen aller phys. Datensaetze einer Messposition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_nof_var_name_wi ( imespos, nof_physet ) &
       RESULT ( val )
    !
    !! Nummer der Messposition
    INTEGER :: imespos
    !! Anzahl Datensaetze
    INTEGER :: nof_physet
    !! Ergebniswert : Anzahl Namens-Variationen aller phys. Datensaetze einer Messposition im IPDS-Arbeitsobjekt
    INTEGER :: val(nof_physet)
    !
    val = 0
    !
    IF ( associated_mespos_object ( work_object ) ) &
         val = get_phyval_nof_var_name ( get_physet_set ( get_mespos_physet ( work_object%mespos(imespos) ) ) )
    !
  END FUNCTION get_ipds_mespos_nof_var_name_wi
  !
  !! Die Namens-Variationen eines phys. Datensatzes einer Messposition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_var_name_wi ( imespos, nof_physet, iphyset, nof_var_name ) &
       RESULT ( val )
    !
    !! Nummer der Messposition
    INTEGER :: imespos
    !! Anzahl Datensaetze
    INTEGER :: nof_physet
    !! Nummer des Datensatzes
    INTEGER :: iphyset
    !! Anzahl Namens-Variationen des Datensatzes
    INTEGER :: nof_var_name
    !! Ergebniswert : Namens-Variationen eines phys. Datensatzes einer Messposition im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_phyval_name) :: val(nof_var_name)
    !
    !! Liste der Datensaetze fuer eine Messposition
    TYPE (t_phyval) :: phyvals(nof_physet)
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_mespos_object ( work_object ) ) THEN
       !
       phyvals = get_physet_set ( get_mespos_physet ( work_object%mespos(imespos) ) )
       !
       val = get_phyval_var_name ( phyvals(iphyset) )
       !
       CALL kill_phyval ( phyvals )
       !
    END IF
    !
  END FUNCTION get_ipds_mespos_var_name_wi
  !
  !! Die Datenwert-Variationen eines phys. Datensatzes einer Messposition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_mespos_val_wi ( imespos, nof_physet, iphyset, nof_var_name ) &
       RESULT ( val )
    !
    !! Nummer der Messposition
    INTEGER :: imespos
    !! Anzahl Datensaetze
    INTEGER :: nof_physet
    !! Nummer des Datensatzes
    INTEGER :: iphyset
    !! Anzahl Namens-Variationen des Datensatzes
    INTEGER :: nof_var_name
    !! Ergebniswert : Datenwert-Variationen eines phys. Datensatzes einer Messposition im IPDS-Arbeitsobjekt
    REAL    :: val(nof_var_name)
    !
    !! Liste der Datensaetze fuer eine Messposition
    TYPE (t_phyval) :: phyvals(nof_physet)
    !
    val = HUGE(val)
    !
    IF ( associated_mespos_object ( work_object ) ) THEN
       !
       phyvals = get_physet_set ( get_mespos_physet ( work_object%mespos(imespos) ) )
       !
       val = get_phyval_val ( phyvals(iphyset) )
       !
       CALL kill_phyval ( phyvals )
       !
    END IF
    !
  END FUNCTION get_ipds_mespos_val_wi
  !
  !! Anzahl regional abweichender phys. Werte-Definitionen (regphysets) fuer ein
  !! IPDS-Objekt veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_nof_regphysets_w ( ) &
       RESULT ( nof )
    !
    !! Ergebniswert : Anzahl regional abweichender phys. Werte-Definitionen im IPDS-Arbeitsobjekt
    INTEGER :: nof
    !
    nof = 0
    IF ( associated_regphyset_object ( work_object ) ) nof = SIZE( work_object%regphyset )
    !
  END FUNCTION get_ipds_nof_regphysets_w
  !
  !! Den Namen der zugehoerigen Region einer regional abweichenden phys. Werte-Definition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_regname_wi ( iregphyset ) &
       RESULT ( val )
    !
    !! Nummer der regional abweichenden phys. Werte-Definition
    INTEGER                           :: iregphyset
    !! Ergebniswert : Name der zugehoerigen Region der regional abweichenden phys. Werte-Definition im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_region_name) :: val
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_region_name ( work_object%regphyset(iregphyset) )
    !
  END FUNCTION get_ipds_regphyset_regname_wi
  !
  !! Die Namen der zugehoerigen Regionen aller regional abweichenden phys. Werte-Definitionen
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_regname_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Namen der zugehoerigen Regionen aller regional abweichenden phys. Werte-Definitionen im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_region_name) :: val(SIZE(work_object%regphyset))
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_region_name ( work_object%regphyset )
    !
  END FUNCTION get_ipds_regphyset_regname_w1
  !
  !! Innerhalb/Ausserhalb-Region-Kennung einer regional abweichenden phys. Werte-Definition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_reginout_wi ( iregphyset ) &
       RESULT ( val )
    !
    !! Nummer der regional abweichenden phys. Werte-Definition
    INTEGER :: iregphyset
    !! Ergebniswert : Innerhalb/Ausserhalb-Region-Kennung der regional abweichenden phys. Werte-Definition im IPDS-Arbeitsobjekt
    LOGICAL :: val
    !
    val = .FALSE.
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_region_inside ( work_object%regphyset(iregphyset) )
    !
  END FUNCTION get_ipds_regphyset_reginout_wi
  !
  !! Innerhalb/Ausserhalb-Region-Kennungen aller regional abweichenden phys. Werte-Definitionen
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_reginout_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Innerhalb/Ausserhalb-Region-Kennung aller regional abweichenden phys. Werte-Definitionen im IPDS-Arbeitsobjekt
    LOGICAL :: val(SIZE(work_object%regphyset))
    !
    val = .FALSE.
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_region_inside ( work_object%regphyset )
    !
  END FUNCTION get_ipds_regphyset_reginout_w1
  !
  !! Hoehenuntergrenze einer regional abweichenden phys. Werte-Definition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_regzmin_wi ( iregphyset ) &
       RESULT ( val )
    !
    !! Nummer der regional abweichenden phys. Werte-Definition
    INTEGER :: iregphyset
    !! Ergebniswert : Hoehenuntergrenze der regional abweichenden phys. Werte-Definition im IPDS-Arbeitsobjekt
    REAL    :: val
    !
    val = HUGE(val)
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_region_zmin ( work_object%regphyset(iregphyset) )
    !
  END FUNCTION get_ipds_regphyset_regzmin_wi
  !
  !! Hoehenuntergrenzen aller regional abweichenden phys. Werte-Definitionen
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_regzmin_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Hoehenuntergrenzen aller regional abweichenden phys. Werte-Definitionen im IPDS-Arbeitsobjekt
    REAL :: val(SIZE(work_object%regphyset))
    !
    val = HUGE(val)
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_region_zmin ( work_object%regphyset )
    !
  END FUNCTION get_ipds_regphyset_regzmin_w1
  !
  !! Hoehenobergrenze einer regional abweichenden phys. Werte-Definition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_regzmax_wi ( iregphyset ) &
       RESULT ( val )
    !
    !! Nummer der regional abweichenden phys. Werte-Definition
    INTEGER :: iregphyset
    !! Ergebniswert : Hoehenuntergrenze der regional abweichenden phys. Werte-Definition im IPDS-Arbeitsobjekt
    REAL    :: val
    !
    val = HUGE(val)
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_region_zmax ( work_object%regphyset(iregphyset) )
    !
  END FUNCTION get_ipds_regphyset_regzmax_wi
  !
  !! Hoehenobergrenzen aller regional abweichenden phys. Werte-Definitionen
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_regzmax_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Hoehenuntergrenzen aller regional abweichenden phys. Werte-Definitionen im IPDS-Arbeitsobjekt
    REAL :: val(SIZE(work_object%regphyset))
    !
    val = HUGE(val)
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_region_zmax ( work_object%regphyset )
    !
  END FUNCTION get_ipds_regphyset_regzmax_w1
  !
  !! Die Anzahl zugehoeriger Messstationen einer regional abweichenden phys. Werte-Definition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_nofmespos_wi ( iregphyset ) &
       RESULT ( val )
    !
    !! Nummer der regional abweichenden phys. Werte-Definition
    INTEGER :: iregphyset
    !! Ergebniswert : Anzahl zugehoeriger Messstationen der regional abweichenden phys. Werte-Definition im IPDS-Arbeitsobjekt
    INTEGER :: val
    !
    val = 0
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_nof_mespos_name ( work_object%regphyset(iregphyset) )
    !
  END FUNCTION get_ipds_regphyset_nofmespos_wi
  !
  !! Die Anzahl zugehoeriger Messstationen aller regphysetal abweichenden phys. Werte-Definitionen
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_nofmespos_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Anzahl zugehoeriger Messstationen aller regional abweichenden phys. Werte-Definitionen im IPDS-Arbeitsobjekt
    INTEGER :: val(SIZE(work_object%regphyset))
    !
    val = 0
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_nof_mespos_name ( work_object%regphyset )
    !
  END FUNCTION get_ipds_regphyset_nofmespos_w1
  !
  !! Die Namen zugehoeriger Messstationen einer regional abweichenden phys. Werte-Definition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_mesposnam_wi ( iregphyset, nof_mespos ) &
       RESULT ( val )
    !
    !! Nummer der regional abweichenden phys. Werte-Definition
    INTEGER                           :: iregphyset
    !! Anzahl Messstationen
    INTEGER                           :: nof_mespos
    !! Ergebniswert : Namen zugehoeriger Messstationen der regional abweichenden phys. Werte-Definition im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_mespos_name) :: val(nof_mespos)
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_mespos_name ( work_object%regphyset(iregphyset) )
    !
  END FUNCTION get_ipds_regphyset_mesposnam_wi
  !
  !! Den Messstation-Interpolations-Abstand einer regional abweichenden phys. Werte-Definition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_maxdist_wi ( iregphyset ) &
       RESULT ( val )
    !
    !! Nummer der regional abweichenden phys. Werte-Definition
    INTEGER :: iregphyset
    !! Ergebniswert : Messstation-Interpolations-Abstand der regional abweichenden phys. Werte-Definition im IPDS-Arbeitsobjekt
    REAL    :: val
    !
    val = HUGE(val)
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_mespos_maxdist ( work_object%regphyset(iregphyset) )
    !
  END FUNCTION get_ipds_regphyset_maxdist_wi
  !
  !! Die Messstation-Interpolations-Abstaende aller regional abweichenden phys. Werte-Definitionen
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_maxdist_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Messstation-Interpolations-Abstaende aller regional abweichenden phys. Werte-Definitionen im IPDS-Arbeitsobjekt
    REAL :: val(SIZE(work_object%regphyset))
    !
    val = HUGE(val)
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_mespos_maxdist ( work_object%regphyset )
    !
  END FUNCTION get_ipds_regphyset_maxdist_w1
  !
  !! Den Interpolationsmethoden-Namen einer regional abweichenden phys. Werte-Definition
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_ip_name_wi ( iregphyset ) &
       RESULT ( val )
    !
    !! Nummer der regional abweichenden phys. Werte-Definition
    INTEGER                                       :: iregphyset
    !! Ergebniswert : Interpolationsmethoden-Name der regional abweichenden phys. Werte-Definition im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_regphyset_interpol_name) :: val
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_interpol_name ( work_object%regphyset(iregphyset) )
    !
  END FUNCTION get_ipds_regphyset_ip_name_wi
  !
  !! Die Interpolationsmethoden-Namen aller regional abweichenden phys. Werte-Definitionen
  !! eines IPDS-Objektes veroeffentlichen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_regphyset_ip_name_w1 ( ) &
       RESULT ( val )
    !
    !! Ergebniswerte : Interpolationsmethoden-Namen aller regional abweichenden phys. Werte-Definitionen im IPDS-Arbeitsobjekt
    CHARACTER (LEN=c_len_regphyset_interpol_name) :: val(SIZE(work_object%regphyset))
    !
    val = REPEAT ( ' ', LEN(val) )
    !
    IF ( associated_regphyset_object ( work_object ) ) val = get_regphyset_interpol_name ( work_object%regphyset )
    !
  END FUNCTION get_ipds_regphyset_ip_name_w1
  !
  !! Hole Pointer auf Komponente "dim" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_dim_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "dim"
    TYPE (t_dim) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='get_ipds_dim_w1' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_dim_object ( work_object ) 
          IF ( .NOT. ASSOCIATED(val) ) THEN
             CALL derive_dim ( work_object )
             val => get_dim_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_ipds_dim_w1
  !
  !! Hole Pointer auf Komponente "var" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_var_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "var"
    TYPE (t_var) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='get_ipds_var_w1' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_var_object ( work_object ) 
          IF ( .NOT. ASSOCIATED(val) ) THEN
             CALL derive_var ( work_object )
             val => get_var_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_ipds_var_w1
  !
  !! Hole Pointer auf Komponente "att" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_att_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "att"
    TYPE (t_att) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='get_ipds_att_w1' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_att_object ( work_object ) 
          IF ( .NOT. ASSOCIATED(val) ) THEN
             CALL derive_att ( work_object )
             val => get_att_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_ipds_att_w1
  !
  !! Hole Datenwerte einer physikalischer Groesse fuer einen Punkt mit Tiefenwert <BR>
  !! Subroutine erzeugt Fehlermeldung
  SUBROUTINE get_ipds_val_point0_phy0 &
       ( p, z, phyval_name, val, var_name )
    !
    ! Formalparameter
    !! Koordinaten der Datenposition
    TYPE (t_point_2d) , INTENT(IN) :: p
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN) :: z
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN) :: phyval_name
    !! Datenwerte aller Varianten der physikalischen Groesse, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert
    REAL              , POINTER    :: val(:)
    !! Bezeichnung aller Varianten der physikalischen Groesse, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert
    CHARACTER (LEN=c_len_phyval_name), POINTER :: var_name(:)
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_ipds_val_point0_phy0'
    !! Anzahl aller Variationen
    INTEGER                         :: nof_all_var
    !! Bezeichnung aller Varianten der physikalischen Groesse
    CHARACTER (LEN=c_len_phyval_name), POINTER :: all_var_name(:)
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten der physikalischen Groesse
    LOGICAL           , POINTER :: all_var_ex(:)
    !! Datenwerte aller Varianten der physikalischen Groesse
    REAL              , POINTER :: all_val(:)
    !! Anzahl Komponente "regphyset"
    INTEGER              :: nof_regphyset
    !! Punkt liegt innerhalb der unterschiedlichen Regionen?
    LOGICAL, POINTER :: p_in_region(:)
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , POINTER :: nof_mespos_phyval(:)
    !! Max. Anzahl Mespositionen
    INTEGER                         :: max_nof_mespos_phyval
    !! Max. Anzahl Mespositionen (nur fuer Dimensionierungen, also >= 1)
    INTEGER                         :: max_nof_mespos_phyval_dim
    !! Koordinaten aller zu einem phys. Satz zugehoerigen Messpositionen
    TYPE (t_point_2d) , POINTER :: mespos_coor(:,:)
    !! Phys. Daten aller zu einem phys. Satz zugehoerigen Messpositionen
    TYPE (t_phyval)   , POINTER :: mespos_phyval(:,:)
    !! Phys. Messwerte aller Varianten aller zu einem phys. Satz zugehoerigen Messpositionen
    REAL (KIND=Single), POINTER :: mespos_val(:,:,:)
    !
    !! Statusvariable
    INTEGER :: stat
    !
    CALL get_var_name_object( work_object, phyval_name, nof_all_var, all_var_name )
    !
    ALLOCATE ( &
         all_var_ex( nof_all_var ), &
         all_val(    nof_all_var ), &
         STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21030, c_upname, c_modname, stat )
    IF ( no_error() ) all_var_ex = .FALSE.
    IF ( no_error() ) all_val    = 0.00
    IF ( no_error() ) THEN
       !
       IF ( associated_physet_object( work_object ) ) CALL update_physet_var &
            ( work_object%physet, phyval_name, nof_all_var, all_var_name, all_var_ex, all_val )
       !
    END IF
    !
    IF ( associated_regphyset_object( work_object ) ) THEN
       !
       nof_regphyset = SIZE( work_object%regphyset )
       !
    ELSE
       !
       nof_regphyset = 1
       !
    ENDIF
    !
    ALLOCATE ( p_in_region( nof_regphyset ) )
    !
    CALL get_p_in_region ( &
         nof_regphyset, p, z, p_in_region )
    !
    ALLOCATE ( nof_mespos_phyval( nof_regphyset ) )
    !
    CALL get_nof_mespos_phyval ( &
         nof_regphyset, phyval_name, nof_mespos_phyval )
    !
    max_nof_mespos_phyval     = MAXVAL( nof_mespos_phyval )
    max_nof_mespos_phyval_dim = MAX(1,max_nof_mespos_phyval)
    !
    ALLOCATE ( &
         mespos_coor(   max_nof_mespos_phyval_dim, nof_regphyset ), &
         mespos_phyval( max_nof_mespos_phyval_dim, nof_regphyset ) )
    !
    CALL get_mespos_phyval ( &
         max_nof_mespos_phyval, max_nof_mespos_phyval_dim, &
         nof_regphyset, phyval_name, nof_mespos_phyval, &
         mespos_coor, mespos_phyval )
    !
    ALLOCATE ( mespos_val( nof_all_var, max_nof_mespos_phyval_dim, nof_regphyset ) )
    !
    CALL get_mespos_val ( &
         nof_all_var, max_nof_mespos_phyval_dim, nof_regphyset, &
         nof_mespos_phyval, mespos_phyval, all_var_name, &
         mespos_val )
    !
    DEALLOCATE ( mespos_phyval )
    !
    CALL get_val_object (                                &
         work_object, p, z, phyval_name,                 &
         nof_regphyset, p_in_region,                     &
         max_nof_mespos_phyval_dim,                      &
         nof_mespos_phyval,                              &
         mespos_coor, mespos_val,                        &
         nof_all_var, all_var_name, all_var_ex, all_val, &
         val, var_name )
    !
    DEALLOCATE ( mespos_val )
    DEALLOCATE ( mespos_coor )
    DEALLOCATE ( nof_mespos_phyval )
    !
    DEALLOCATE( p_in_region )
    !
    DEALLOCATE (       &
         all_var_name, &
         all_var_ex,   &
         all_val,      &
         STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21031, c_upname, c_modname, stat )
    !
  END SUBROUTINE get_ipds_val_point0_phy0
  !
  !! Hole Datenwerte einer physikalischer Groesse fuer eine xyz-Position <BR>
  !! Subroutine erzeugt Fehlermeldung
  SUBROUTINE get_ipds_val_xyz0_phy0 &
       ( x, y, z, phyval_name, val, var_name )
    !
    ! Formalparameter
    !! Koordinaten der Datenposition
    REAL (KIND=Double), INTENT(IN) :: x, y
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN) :: z
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN) :: phyval_name
    !! Datenwerte der Varianten der physikalischen Groesse <BR>
    !! Feld wird mit notwendigem Speicher allokiert
    REAL              , POINTER    :: val(:)
    !! Bezeichnung der Varianten der physikalischen Groesse <BR>
    !! Feld wird mit notwendigem Speicher allokiert
    CHARACTER (LEN=*) , POINTER    :: var_name(:)
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='get_ipds_val_xyz0_phy0' 
    !! Punktkoordinaten
    TYPE (t_point_2d)  :: p
    !
    CALL new_point_2d( p )
    !
    CALL set_point_2d_xy( p, x, y )
    !
    CALL get_ipds_val( p, z, phyval_name, val, var_name )
    !
    CALL kill_point_2d( p )
    !
  END SUBROUTINE get_ipds_val_xyz0_phy0
  !
  !! Hole Datenwerte einer physikalischer Groesse fuer Punkte mit Tiefenwerten <BR>
  !! Subroutine erzeugt Fehlermeldung
  SUBROUTINE get_ipds_val_point1_phy0 &
       ( p, z, phyval_name, val, var_name )
    !
    ! Formalparameter
    !! Koordinaten der Datenposition
    TYPE (t_point_2d) , INTENT(IN) :: p(:)
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN) :: z(:)
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN) :: phyval_name
    !! Datenwerte der Varianten der physikalischen Groesse <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert
    REAL              , POINTER    :: val(:,:)
    !! Bezeichnung der Varianten der physikalischen Groesse <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert
    CHARACTER (LEN=c_len_phyval_name), POINTER    :: var_name(:)
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_ipds_val_point1_phy0'
    !! Anzahl Punkte
    INTEGER :: n_point
    !! Punkt-Zaehler
    INTEGER :: i_point
    !! Zaehler fuer die Varianten eines Punktes
    INTEGER :: i_l_var
    !! Zaehler fuer alle Varianten
    INTEGER :: i_var
    !! Datenwerte der Varianten der physikalischen Groesse fuer einen Punkt
    REAL              , POINTER :: l_val(:)
    !! Bezeichnung der Varianten der physikalischen Groesse
    CHARACTER (LEN=c_len_phyval_name), POINTER :: l_var_name(:)
    !! Anzahl aller Variationen
    INTEGER                         :: nof_all_var
    !! Bezeichnung aller Varianten der physikalischen Groesse
    CHARACTER (LEN=c_len_phyval_name), POINTER     :: all_var_name(:)
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten der physikalischen Groesse
    LOGICAL           , POINTER :: all_var_ex(:)
    !! Datenwerte aller Varianten der physikalischen Groesse
    REAL              , POINTER :: all_val(:)
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten der physikalischen Groesse
    !! (Default-Angaben, nicht ortsabhaengig)
    LOGICAL           , POINTER :: def_all_var_ex(:)
    !! Datenwerte aller Varianten der physikalischen Groesse
    !! (Default-Angaben, nicht ortsabhaengig)
    REAL              , POINTER :: def_all_val(:)
    !! Anzahl Komponente "regphyset"
    INTEGER              :: nof_regphyset
    !! Punkt liegt innerhalb der unterschiedlichen Regionen?
    LOGICAL, POINTER :: p_in_region(:,:)
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , POINTER :: nof_mespos_phyval(:)
    !! Max. Anzahl Mespositionen
    INTEGER                         :: max_nof_mespos_phyval
    !! Max. Anzahl Mespositionen (nur fuer Dimensionierungen, also >= 1)
    INTEGER                         :: max_nof_mespos_phyval_dim
    !! Koordinaten aller zu einem phys. Satz zugehoerigen Messpositionen
    TYPE (t_point_2d) , POINTER :: mespos_coor(:,:)
    !! Phys. Daten aller zu einem phys. Satz zugehoerigen Messpositionen
    TYPE (t_phyval)   , POINTER :: mespos_phyval(:,:)
    !! Phys. Messwerte aller Varianten aller zu einem phys. Satz zugehoerigen Messpositionen
    REAL (KIND=Single), POINTER :: mespos_val(:,:,:)
    !! Statusvariable
    INTEGER            :: stat
    !! Bildschirmtextlaenge
    INTEGER            :: ie
    !! Suchstring im Fehlertext
    CHARACTER (LEN=10) :: cs
    !
    n_point = MIN( SIZE( p ), SIZE( z ) )
    !
    NULLIFY( l_val )
    NULLIFY( l_var_name )
    !
    CALL get_var_name_object( work_object, phyval_name, nof_all_var, all_var_name )
    !
    ALLOCATE ( &
         all_var_ex( nof_all_var ), &
         all_val(    nof_all_var ), &
         def_all_var_ex( nof_all_var ), &
         def_all_val(    nof_all_var ), &
         STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21030, c_upname, c_modname, stat )
    IF ( no_error() ) def_all_var_ex = .FALSE.
    IF ( no_error() ) def_all_val    = 0.00
    IF ( no_error() ) THEN
       !
       IF ( associated_physet_object( work_object ) ) CALL update_physet_var &
            ( work_object%physet, phyval_name, nof_all_var, all_var_name, def_all_var_ex, def_all_val )
       !
    END IF
    !
    IF ( no_error() ) THEN
       !
       ALLOCATE ( &
            val( n_point, nof_all_var ), &
            var_name(     nof_all_var ), &
            STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21030, c_upname, c_modname, stat )
       !
    END IF
    IF ( no_error() ) var_name = all_var_name
    !
    IF ( associated_regphyset_object( work_object ) ) THEN
       !
       nof_regphyset = SIZE( work_object%regphyset )
       !
    ELSE
       !
       nof_regphyset = 1
       !
    ENDIF
    !
    ALLOCATE ( p_in_region( nof_regphyset, n_point ) )
    !
    CALL get_p_in_region ( &
         nof_regphyset, n_point, p, z, p_in_region )
    !
    ALLOCATE ( nof_mespos_phyval( nof_regphyset ) )
    !
    CALL get_nof_mespos_phyval ( &
         nof_regphyset, phyval_name, nof_mespos_phyval )
    !
    max_nof_mespos_phyval     = MAXVAL( nof_mespos_phyval )
    max_nof_mespos_phyval_dim = MAX(1,max_nof_mespos_phyval)
    !
    ALLOCATE ( &
         mespos_coor(   max_nof_mespos_phyval_dim, nof_regphyset ), &
         mespos_phyval( max_nof_mespos_phyval_dim, nof_regphyset ) )
    !
    CALL get_mespos_phyval ( &
         max_nof_mespos_phyval, max_nof_mespos_phyval_dim, &
         nof_regphyset, phyval_name, nof_mespos_phyval, &
         mespos_coor, mespos_phyval )
    !
    ALLOCATE ( mespos_val( nof_all_var, max_nof_mespos_phyval_dim, nof_regphyset ) )
    !
    CALL get_mespos_val ( &
         nof_all_var, max_nof_mespos_phyval_dim, nof_regphyset, &
         nof_mespos_phyval, mespos_phyval, all_var_name, &
         mespos_val )
    !
    DEALLOCATE ( mespos_phyval )
    !
    IF ( n_point > 10000 ) THEN
       !
       ie = 33 + LEN_TRIM(phyval_name)
       WRITE(*,*)
       WRITE(*,'(3A)')             '     Daten holen fuer phys. Groesse "',TRIM(phyval_name),'"'
       WRITE(*,'(A)',ADVANCE='NO') '  0% '
       !
    END IF
    !
    DO i_point = 1, n_point
       !
       IF ( any_error() ) EXIT
       !
       all_var_ex = def_all_var_ex
       all_val    = def_all_val
       !
       CALL get_val_object           ( &
            work_object,               &
            p(i_point),                &
            z(i_point),                &
            phyval_name,               &
            nof_regphyset,             &
            p_in_region(:,i_point),    &
            max_nof_mespos_phyval_dim, &
            nof_mespos_phyval,         &
            mespos_coor,               &
            mespos_val,                &
            nof_all_var,               &
            all_var_name,              &
            all_var_ex,                &
            all_val,                   &
            l_val,                     &
            l_var_name )
       !
       IF ( any_error() ) EXIT
       !
       DO i_l_var = 1, SIZE( l_val )
          !
          i_var = 0
          !
          DO
             !
             i_var = i_var + 1
             IF ( i_var > nof_all_var ) EXIT
             !
             IF ( TRIM( l_var_name( i_l_var ) ) == TRIM( all_var_name( i_var ) ) ) EXIT
             !
          END DO
          !
          IF ( i_var > nof_all_var ) THEN
             !
             !$OMP critical
             !
             CALL setup_error_act( all_errors(:), 21100, c_upname, c_modname )
             !
             cs = '<phy_name>'
             CALL setup_error_act ( cs, TRIM( phyval_name ) )
             !
             cs = '<var_name>'
             CALL setup_error_act ( cs, TRIM( l_var_name( i_l_var ) ) )
             !
             !$OMP end critical
             !
          ELSE
             !
             val( i_point, i_var ) = l_val( i_l_var )
             !
          END IF
          !
       END DO
       !
       DEALLOCATE(      &
            l_val,      &
            l_var_name, &
            STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21011, c_upname, c_modname, stat )
       !
       IF ( n_point > 10000 ) THEN
          !
          IF (MOD (i_point,INT(REAL(n_point)/REAL(ie))) == 0) &
               WRITE(*,'(A)',ADVANCE='NO') '.'
          !
       END IF
       !
    END DO
    !
    IF ( n_point > 10000 ) WRITE(*,'(A)') ' 100%'
    !
    DEALLOCATE ( mespos_val )
    DEALLOCATE ( mespos_coor )
    DEALLOCATE ( nof_mespos_phyval )
    !
    DEALLOCATE ( p_in_region )
    !
    DEALLOCATE (         &
         all_var_name,   &
         all_var_ex,     &
         all_val,        &
         def_all_var_ex, &
         def_all_val,    &
         STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21031, c_upname, c_modname, stat )
    !
  END SUBROUTINE get_ipds_val_point1_phy0
  !
  !! Hole Datenwerte einer physikalischer Groesse fuer xyz-Positionen <BR>
  !! Subroutine erzeugt Fehlermeldung
  SUBROUTINE get_ipds_val_xyz1_phy0 &
       ( x, y, z, phyval_name, val, var_name )
    !
    ! Formalparameter
    !! Koordinaten der Datenposition
    REAL (KIND=Double), INTENT(IN) :: x(:), y(:)
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN) :: z(:)
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN) :: phyval_name
    !! Datenwerte der Varianten der physikalischen Groesse <BR>
    !! Feld wird mit notwendigem Speicher allokiert
    REAL              , POINTER    :: val(:,:)
    !! Bezeichnung der Varianten der physikalischen Groesse <BR>
    !! Feld wird mit notwendigem Speicher allokiert
    CHARACTER (LEN=c_len_phyval_name), POINTER    :: var_name(:)
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='get_ipds_val_xyz1_phy0'
    !! Anzahl Punkte
    INTEGER            :: n_point
    !! Punkt-Zaehler
    INTEGER            :: i_point
    !! Punktkoordinaten
    TYPE (t_point_2d), POINTER  :: p(:)
    !! Statusvariable
    INTEGER            :: stat
    !
    n_point = MIN( SIZE( x ), SIZE( y ), SIZE( z ) )
    !
    ALLOCATE( p( n_point ), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21020, c_upname, c_modname, stat )
    !
    CALL new_point_2d( p )
    !
    DO i_point = 1, n_point
       !
       CALL set_point_2d_xy( p( i_point ), x( i_point ), y( i_point ) )
       !
    ENDDO
    !
    CALL get_ipds_val_point1_phy0( p, z, phyval_name, val, var_name )
    !
    CALL kill_point_2d( p )
    !
    DEALLOCATE( p, STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21021, c_upname, c_modname, stat )
    !
  END SUBROUTINE get_ipds_val_xyz1_phy0
  !
  !! Hole Datenwerte vieler physikalischer Groessen fuer Punkte mit Tiefenwerten <BR>
  !! Subroutine erzeugt Fehlermeldung
  SUBROUTINE get_ipds_val_point1_phy1 &
       ( p, z, phyval_name, val, var_name, nof_var )
    !
    ! Formalparameter
    !! Koordinaten der Datenposition
    TYPE (t_point_2d) , INTENT(IN) :: p(:)
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN) :: z(:)
    !! Bezeichnungen der physikalischen Groessen
    CHARACTER (LEN=*) , INTENT(IN) :: phyval_name(:)
    !! Datenwerte aller Varianten der physikalischen Groessen, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert <BR>
    !! val(i,j,k) : i = 1 .. Anzahl Punkte <BR>
    !!              j = 1 .. Anzahl physikalischer Groessen <BR>
    !!              k = 1 .. Anzahl Varianten jeder physikalischer Groesse
    REAL              , POINTER    :: val(:,:,:)
    !! Bezeichnung aller Varianten der physikalischen Groessen, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert <BR>
    !! var_name(j,k) : j = 1 .. Anzahl physikalischer Groessen <BR>
    !!                 k = 1 .. Anzahl Varianten jeder physikalischer Groesse
    CHARACTER (LEN=c_len_phyval_name), POINTER    :: var_name(:,:)
    !! Anzahl der Varianten fuer jede physikalische Groesse, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert <BR>
    !! nof_var(j) : j = 1 .. Anzahl physikalischer Groessen
    INTEGER           , POINTER    :: nof_var(:)
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_ipds_val_point1_phy1'
    !! Anzahl Punkte
    INTEGER :: n_point
    !! Punkt-Zaehler
    INTEGER :: i_point
    !! Anzahl physikalischer Groessen
    INTEGER :: n_phy
    !! Zaehler fuer physikalische Groessen
    INTEGER :: i_phy
    !! max. Anzahl Varianten einer physikalischer Groesse
    INTEGER :: n_max_var
    !! Zaehler fuer die Varianten eines Punktes
    INTEGER :: i_l_var
    !! Zaehler fuer alle Varianten
    INTEGER :: i_var
    !! Anzahl Komponente "regphyset"
    INTEGER              :: nof_regphyset
    !! Punkt liegt innerhalb der unterschiedlichen Regionen?
    LOGICAL, POINTER :: p_in_region(:,:)
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , POINTER :: nof_mespos_phyval(:,:)
    !! Max. Anzahl Mespositionen
    INTEGER                         :: max_nof_mespos_phyval
    !! Max. Anzahl Mespositionen (nur fuer Dimensionierungen, also >= 1)
    INTEGER                         :: max_nof_mespos_phyval_dim
    !! Koordinaten aller zu einem phys. Satz zugehoerigen Messpositionen
    TYPE (t_point_2d) , POINTER :: mespos_coor(:,:,:)
    !! Phys. Daten aller zu einem phys. Satz zugehoerigen Messpositionen
    TYPE (t_phyval)   , POINTER :: mespos_phyval(:,:,:)
    !! Phys. Messwerte aller Varianten aller zu einem phys. Satz zugehoerigen Messpositionen
    REAL (KIND=Single), POINTER :: mespos_val(:,:,:)
    !! Anzahl aller Variationen
    INTEGER                         :: nof_all_var
    !! Bezeichnung aller Varianten der physikalischen Groesse
    CHARACTER (LEN=c_len_phyval_name), POINTER     :: all_var_name(:)
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten der physikalischen Groesse
    LOGICAL           , POINTER :: all_var_ex(:)
    !! Datenwerte aller Varianten der physikalischen Groesse
    REAL              , POINTER :: all_val(:)
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten der physikalischen Groesse
    !! (Default-Angaben, nicht ortsabhaengig)
    LOGICAL           , POINTER :: def_all_var_ex(:)
    !! Datenwerte aller Varianten der physikalischen Groesse
    !! (Default-Angaben, nicht ortsabhaengig)
    REAL              , POINTER :: def_all_val(:)
    !! Statusvariable
    INTEGER :: stat
    !! Bildschirmtextlaenge
    INTEGER :: ie
    !! Datenwerte der Varianten einer physikalischen Groesse fuer einen Punkt
    REAL              , POINTER :: l_val(:)
    !! Bezeichnung der Varianten einer physikalischen Groesse
    CHARACTER (LEN=c_len_phyval_name), POINTER :: l_var_name(:)
    !! Suchstring im Fehlertext
    CHARACTER (LEN=10) :: cs
    !
    n_point = MIN( SIZE( p ), SIZE( z ) )
    n_phy   = SIZE( phyval_name )
    !
    ALLOCATE             ( &
         nof_var( n_phy ), &
         STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21060, c_upname, c_modname, stat )
    !
    DO i_phy = 1, n_phy
       !
       CALL get_var_name_object( work_object, phyval_name(i_phy), nof_var(i_phy), all_var_name )
       !
       DEALLOCATE ( all_var_name, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21210, c_upname, c_modname, stat )
       !
    END DO
    !
    n_max_var = MAXVAL( nof_var )
    !
    ALLOCATE ( &
         val( n_point, n_phy, n_max_var ), &
         var_name(     n_phy, n_max_var ), &
         STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21040, c_upname, c_modname, stat )
    !
    val      = HUGE( val(1,1,1) )
    var_name = REPEAT( ' ', LEN( var_name(1,1) ) )
    !
    IF ( associated_regphyset_object( work_object ) ) THEN
       !
       nof_regphyset = SIZE( work_object%regphyset )
       !
    ELSE
       !
       nof_regphyset = 1
       !
    ENDIF
    !
    ALLOCATE ( p_in_region( nof_regphyset, n_point ) )
    !
    CALL get_p_in_region ( &
         nof_regphyset, n_point, p, z, p_in_region )
    !
    ALLOCATE ( nof_mespos_phyval( nof_regphyset, n_phy ) )
    !
    CALL get_nof_mespos_phyval ( &
         nof_regphyset, n_phy, phyval_name, nof_mespos_phyval )
    !
    max_nof_mespos_phyval     = MAXVAL( nof_mespos_phyval )
    max_nof_mespos_phyval_dim = MAX(1,max_nof_mespos_phyval)
    !
    ALLOCATE ( &
         mespos_coor(   max_nof_mespos_phyval_dim, nof_regphyset, n_phy ), &
         mespos_phyval( max_nof_mespos_phyval_dim, nof_regphyset, n_phy ) )
    !
    CALL get_mespos_phyval ( &
         max_nof_mespos_phyval, max_nof_mespos_phyval_dim, &
         nof_regphyset, n_phy, phyval_name, nof_mespos_phyval, &
         mespos_coor, mespos_phyval )
    !
    IF ( n_point > 10000 ) THEN
       !
       WRITE(*,*)
       WRITE(*,'(A)') '     Daten holen fuer die phys. Groessen:'
       !
    END IF
    !
    DO i_phy = 1, n_phy
       !
       IF ( n_point > 10000 ) THEN
          !
          WRITE(*,'(3A)') '     "',TRIM(phyval_name(i_phy)),'"'
          ie = 2+LEN_TRIM(phyval_name(i_phy))
          !
          WRITE(*,'(A)',ADVANCE='NO') '  0% '
          !
       END IF
       !
       CALL get_var_name_object( work_object, phyval_name(i_phy), nof_all_var, all_var_name )
       !
       ALLOCATE ( mespos_val( nof_all_var, max_nof_mespos_phyval_dim, nof_regphyset ) )
       !
       CALL get_mespos_val ( &
            nof_all_var, max_nof_mespos_phyval_dim, nof_regphyset, &
            nof_mespos_phyval(:,i_phy), mespos_phyval(:,:,i_phy), all_var_name, &
            mespos_val )
       !
       var_name( i_phy, 1:nof_var(i_phy) ) = all_var_name(:)
       !
       ALLOCATE ( &
            all_var_ex( nof_all_var ), &
            all_val(    nof_all_var ), &
            def_all_var_ex( nof_all_var ), &
            def_all_val(    nof_all_var ), &
            STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21030, c_upname, c_modname, stat )
       IF ( no_error() ) def_all_var_ex = .FALSE.
       IF ( no_error() ) def_all_val    = 0.00
       IF ( no_error() ) THEN
          !
          IF ( associated_physet_object( work_object ) ) &
               CALL update_physet_var ( &
               work_object%physet, phyval_name(i_phy), &
               nof_all_var, all_var_name, def_all_var_ex, def_all_val )
          !
       END IF
       !
       DO i_point = 1, n_point
          !
          IF ( no_error() ) THEN
             !
             all_var_ex = def_all_var_ex
             all_val    = def_all_val
             !
             CALL get_val_object(             &
                  work_object,                &
                  p(i_point),                 &
                  z(i_point),                 &
                  phyval_name(i_phy),         &
                  nof_regphyset,              &
                  p_in_region(:,i_point),     &
                  max_nof_mespos_phyval_dim,  &
                  nof_mespos_phyval(:,i_phy), &
                  mespos_coor(:,:,i_phy),     &
                  mespos_val,                 &
                  nof_all_var,                &
                  all_var_name,               &
                  all_var_ex,                 &
                  all_val,                    &
                  l_val,                      &
                  l_var_name )
             !
             IF ( no_error() ) THEN
                !
                DO i_l_var = 1, SIZE( l_val )
                   !
                   i_var = 0
                   !
                   DO
                      !
                      i_var = i_var + 1
                      IF ( i_var > nof_var( i_phy ) ) EXIT
                      !
                      IF ( TRIM( l_var_name( i_l_var ) ) == &
                           TRIM( var_name( i_phy, i_var ) ) ) EXIT
                      !
                   END DO
                   !
                   IF ( i_var > nof_var( i_phy ) ) THEN
                      !
                      CALL setup_error_act( all_errors(:), 21100, c_upname, c_modname )
                      !
                      cs = '<phy_name>'
                      CALL setup_error_act ( cs, TRIM( phyval_name( i_phy ) ) )
                      !
                      cs = '<var_name>'
                      CALL setup_error_act ( cs, TRIM( l_var_name( i_l_var ) ) )
                      !
                   ELSE
                      !
                      val( i_point, i_phy, i_var ) = l_val( i_l_var )
                      !
                   END IF
                   !
                END DO
                !
             END IF
             !
             DEALLOCATE(      &
                  l_val,      &
                  l_var_name, &
                  STAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21011, c_upname, c_modname, stat )
             !
          END IF
          !
          IF ( n_point > 10000 ) THEN
             !
             IF (MOD (i_point,INT(REAL(n_point)/REAL(ie))) == 0) WRITE(*,'(A)',ADVANCE='NO') '.'
             !
          END IF
          !
       END DO
       !
       DEALLOCATE ( mespos_val )
       !
       DEALLOCATE (         &
            all_var_name,   &
            all_var_ex,     &
            all_val,        &
            def_all_var_ex, &
            def_all_val,    &
            STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21031, c_upname, c_modname, stat )
       !
       IF ( n_point > 10000 ) WRITE(*,'(A)') ' 100%'
       !
    END DO
    !
    DEALLOCATE ( mespos_coor, mespos_phyval )
    DEALLOCATE ( nof_mespos_phyval )
    !
    DEALLOCATE ( p_in_region )
    !
  END SUBROUTINE get_ipds_val_point1_phy1
  !
  !! Hole Datenwerte vieler physikalischer Groessen fuer xyz-Positionen <BR>
  !! Subroutine erzeugt Fehlermeldung
  SUBROUTINE get_ipds_val_xyz1_phy1 &
       ( x, y, z, phyval_name, val, var_name, nof_var )
    !
    ! Formalparameter
    !! Koordinaten der Datenposition
    REAL (KIND=Double), INTENT(IN) :: x(:), y(:)
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN) :: z(:)
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN) :: phyval_name(:)
    !! Datenwerte aller Varianten der physikalischen Groessen, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert <BR>
    !! val(i,j,k) : i = 1 .. Anzahl Punkte <BR>
    !!              j = 1 .. Anzahl physikalischer Groessen <BR>
    !!              k = 1 .. Anzahl Varianten jeder physikalischer Groesse
    REAL              , POINTER    :: val(:,:,:)
    !! Bezeichnung aller Varianten der physikalischen Groessen, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert <BR>
    !! var_name(j,k) : j = 1 .. Anzahl physikalischer Groessen <BR>
    !!                 k = 1 .. Anzahl Varianten jeder physikalischer Groesse
    CHARACTER (LEN=c_len_phyval_name), POINTER    :: var_name(:,:)
    !! Anzahl der Varianten fuer jede physikalische Groesse, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert <BR>
    !! nof_var(j) : j = 1 .. Anzahl physikalischer Groessen
    INTEGER           , POINTER    :: nof_var(:)
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='get_ipds_val_xyz1_phy1'
    !! Anzahl Punkte
    INTEGER :: n_point
    !! Punkt-Zaehler
    INTEGER :: i_point
    !! Punktkoordinaten
    TYPE (t_point_2d), POINTER :: p(:)
    !! Statusvariable
    INTEGER :: stat
    !
    n_point = MIN( SIZE( x ), SIZE( y ), SIZE( z ) )
    !
    ALLOCATE( p( n_point ), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21020, c_upname, c_modname, stat )
    !
    CALL new_point_2d( p )
    !
    DO i_point = 1, n_point
       !
       CALL set_point_2d_xy( p( i_point ), x( i_point ), y( i_point ) )
       !
    ENDDO
    !
    CALL get_ipds_val( p, z, phyval_name, val, var_name, nof_var )
    !
    CALL kill_point_2d( p )
    !
    DEALLOCATE( p, STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21021, c_upname, c_modname, stat )
    !
  END SUBROUTINE get_ipds_val_xyz1_phy1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-TARGET-Methoden <<< [ERR_NO = 22000 bis 22999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "datetime" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_ipds_datetime_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "datetime"
    TYPE (t_datetime) , POINTER :: val !     
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='target_ipds_datetime_w0' 
    !
    ok = .false. 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_datetime_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_ipds_datetime_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-READ-Methoden <<< [ERR_NO = 23000 bis 23999]
  ! ----------------------------------------------------------------------
  !
  !! &Uuml;betragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_ipds_w ( )
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='read_ipds_w' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL read_ipds_0 ( work_object )
          IF ( .NOT. ok_ipds( ) ) &
               CALL setup_error_act ( all_errors(:), 23999, c_upname, c_modname )
       END IF
    END IF
    !
  END SUBROUTINE read_ipds_w
  !
  !! &Uuml;betragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! Es wird die in der Parameterliste stehende Datei verwendet <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_ipds_wf ( file )
    !! Bezeichnung der Datei aus der Daten gelesen werden sollen
    TYPE (t_file) , INTENT(IN) :: file ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='read_ipds_wf' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_file_object ( work_object, file )
          CALL read_ipds_w ( )
       END IF
    END IF
    !
  END SUBROUTINE read_ipds_wf
  !
  ! -------------------------------------------------------------------------
  ! PUBLIC-GET-Funktionen ueber implementierte Dateivarianten
  ! -------------------------------------------------------------------------
  !
  !! Holen der Anzahl der implementierten Datei-Varianten <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_ipds_nof_variants_d ( ) &
       RESULT( var )
    !! Ergebniswert: Anzahl der implementierten Datei-Varianten
    INTEGER :: var ! 
    !
    var = c_max_variants
    !
  END FUNCTION get_ipds_nof_variants_d
  !
  !! Holen des Datei-Types aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_ipds_variants_type_d ( ) &
       RESULT( var )
    !! Ergebniswert: Datei-Types aller implementierten Datei-Varianten
    CHARACTER (LEN=LEN(c_variants_type)) , POINTER :: var(:) ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_ipds_variants_type_d' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9002, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_type(:)
    END IF
    !
  END FUNCTION get_ipds_variants_type_d
  !
  !! Holen des Datei-Types einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_ipds_variants_type_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                 :: ivar ! 
    !! Ergebniswert: Datei-Type der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_type)) :: var ! 
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_type(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_ipds_variants_type_0
  !
  !! Holen der Fortran-Form aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_ipds_variants_form_d ( ) &
       RESULT( var )
    !! Ergebniswert: Fortran-Formen aller implementierten Datei-Varianten
    CHARACTER (LEN=LEN(c_variants_form)) , POINTER :: var(:) ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_ipds_variants_form_d' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9004, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_form(:)
    END IF
    !
  END FUNCTION get_ipds_variants_form_d
  !
  !! Holen der Fortran-Form einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_ipds_variants_form_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                 :: ivar ! 
    !! Ergebniswert: Fortran-Form der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_form)) :: var ! 
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_form(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_ipds_variants_form_0
  !
  !! Holen des Fortran-Access aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_ipds_variants_access_d ( ) &
       RESULT( var )
    !! Ergebniswert: Fortran-Access aller implementierten Datei-Varianten
    CHARACTER (LEN=LEN(c_variants_access)) , POINTER :: var(:) ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='get_ipds_variants_access_d' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9005, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_access(:)
    END IF
    !
  END FUNCTION get_ipds_variants_access_d
  !
  !! Holen des Fortran-Access einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_ipds_variants_access_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                   :: ivar ! 
    !! Ergebniswert: Fortran-Access der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_access)) :: var ! 
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_access(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_ipds_variants_access_0
  !
  !! Holen des Fortran-Delimiters aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_ipds_variants_delim_d ( ) &
       RESULT( var )
    !! Ergebniswert: Fortran-Delimiter aller implementierten Datei-Varianten
    CHARACTER (LEN=LEN(c_variants_delim)) , POINTER :: var(:) ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='get_ipds_variants_delim_d' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9006, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_delim(:)
    END IF
    !
  END FUNCTION get_ipds_variants_delim_d
  !
  !! Holen des Fortran-Delimiters einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_ipds_variants_delim_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                  :: ivar ! 
    !! Ergebniswert: Fortran-Delimiter der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_delim)) :: var ! 
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_delim(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_ipds_variants_delim_0
  !
  !! Erzeuge alle Austauschgr&ouml;&szlig;en mit OpenMI-konformen Daten 
  !! f&uuml;r eine Liste von Arbeitsobjekten <BR>
  !! Funktion erzeugt Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_ipds_exch_r_1 ( id, g_exch_r ) &
       RESULT( res )
    !! Liste mit Id's der Datenobjekte des Typs "t_ipds" f&uuml;r die
    !! alle OpenMI-konformen Austauschgr&ouml;&szlig;en erzeugt werden
    !! sollen
    INTEGER , INTENT(IN)          :: id(:)  ! 
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch_r) , INTENT(IN)  :: g_exch_r ! 
    !! Ergebnis: Liste mit allen OpenMI-konformen Austauschgr&ouml;&szlig;en
    TYPE (t_omi_exch_r) , POINTER :: res(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_ipds_exch_r_1' ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    TYPE (t_omi_exch) , POINTER   :: p_exch(:), g_exch(:) ! 
    !
    NULLIFY ( p_exch, g_exch )
    ALLOCATE( res(SIZE(id)) )
    CALL new_omi_exch_r ( res(:) )
    g_exch => get_omi_exch_r_exch_ref ( g_exch_r )
    ! bedarfsweises Erzeugen der OpenMI-konformen Informationen
    ! ... und Transfer in das "ragged array" "res(:)"
    DO i=1,SIZE(id)
       CALL setup_ipds_work_object_d ( id(i) )
       IF ( any_error( ) ) EXIT
       IF ( ok_work_object ( c_upname ) ) THEN
          p_exch => get_exch_object ( work_object )
          IF ( .NOT. ASSOCIATED( p_exch ) ) THEN
             CALL create_omi ( work_object, g_exch, id, i )
          END IF
          p_exch => get_exch_object ( work_object )
          IF ( ASSOCIATED( p_exch ) ) THEN
             CALL set_omi_exch_r_odx      ( res(i),  id(i) )
             CALL set_omi_exch_r_exch_ref ( res(i), p_exch )
          END IF
       END IF
    END DO
    ! [2.1] Aufsammeln der OpenMI-konformen Informationen in dem Feld "res(:)"
    NULLIFY ( p_exch )
    !
  END FUNCTION get_ipds_exch_r_1
  !
  !! Erzeuge die Informationen zum Zeithorizont in OpenMI-konformer Weise
  !! f&uuml;r eine Liste von Arbeitsobjekten <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_ipds_time_horizon_1 ( id ) &
       RESULT( res )
    !! Liste mit Id's der Datenobjekte des Typs "t_ipds" f&uuml;r die
    !! alle OpenMI-konformen Austauschgr&ouml;&szlig;en erzeugt werden
    !! sollen
    INTEGER , INTENT(IN)  :: id(:) ! 
    !! Ergebnis: Zeithorizont der Gitterobjekte
    TYPE (t_omi_span) , POINTER :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='get_ipds_time_horizon_1' ! 
    !! Hilfsvariablen
    TYPE (t_omi_span) , POINTER :: p_span ! 
    INTEGER :: i  ! 
    REAL (KIND=Double) :: l_start, l_end ! 
    LOGICAL :: ok ! 
    !
    ok = .false. 
    l_start = 1.0E+30_Double ; l_end = -1.0E+30_Double
    DO i=1,SIZE(id)
       CALL setup_ipds_work_object_d ( id(i) )
       IF ( any_error( ) ) EXIT
       IF ( ok_work_object ( c_upname ) ) THEN
          p_span => get_span_object ( work_object )
          IF ( ASSOCIATED( p_span ) ) THEN
             ok = .true.
             l_start = MIN( l_start, get_omi_stamp_modjulianday( get_omi_span_start( p_span ) ) )
             l_end   = MAX( l_end  , get_omi_stamp_modjulianday( get_omi_span_end  ( p_span ) ) )
          END IF
       END IF
    END DO
    !
    IF ( ok ) THEN
       ALLOCATE ( res )
       CALL new_omi_span ( res )
       CALL set_omi_span_start ( res, l_start )
       CALL set_omi_span_end   ( res, l_end   )
    ELSE
       NULLIFY ( res )
    END IF
    NULLIFY ( p_span )
    !
  END FUNCTION get_ipds_time_horizon_1
  !
  !! Ermittle, ob diskrete Zeitangaben f&uuml;r das aktuelle Arbeitsobjekt und 
  !! ein bestimmtes Austauschobjekt in OpenMI-konformer Weise vorhanden sind <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_ipds_has_discr_time_w ( exch_idx ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER , INTENT(IN)  :: exch_idx ! 
    !! Ergebnis: diskrete Zeitangaben sind vorhanden / nicht vorhanden
    LOGICAL :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='get_ipds_has_discr_time_w' ! 
    !
    res = .false.
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          res = has_omi_discr_time ( work_object, exch_idx )
       END IF
    END IF
    !
  END FUNCTION get_ipds_has_discr_time_w
  !
  !! Ermittle, ob eine Zeitraumangabe f&uuml;r das aktuelle Arbeitsobjekt und 
  !! ein bestimmtes Austauschobjekt in OpenMI-konformer Weise vorhanden sind <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_ipds_has_time_span_w ( exch_idx ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER , INTENT(IN)  :: exch_idx ! 
    !! Ergebnis: Angabe einer Zeitspanne ist vorhanden / nicht vorhanden
    LOGICAL :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_ipds_has_time_span_w' ! 
    !
    res = .false.
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          res = has_omi_time_span ( work_object, exch_idx )
       END IF
    END IF
    !
  END FUNCTION get_ipds_has_time_span_w
  !
  !! Ermittle, wie viele diskrete Zeitangaben f&uuml;r das aktuelle Arbeitsobjekt 
  !! und ein bestimmtes Austauschobjekt in OpenMI-konformer Weise vorhanden sind <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_ipds_nof_discr_time_w ( exch_idx ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER , INTENT(IN)  :: exch_idx ! 
    !! Ergebnis: Anzahl diskreter Zeitangaben 
    INTEGER :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='get_ipds_nof_discr_time_w' ! 
    !
    res = 0
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          IF ( get_ipds_has_discr_time_w ( exch_idx ) ) THEN
             res = nof_omi_discr_time ( work_object, exch_idx )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_ipds_nof_discr_time_w
  !
  !! Ermittle eine bestimmte diskrete Zeitangaben f&uuml;r das aktuelle Arbeitsobjekt und 
  !! ein bestimmtes Austauschobjekt in OpenMI-konformer Weise anhand der lfd. Nummer
  !! des Zeitpunktes <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_ipds_discr_time_w ( exch_idx, stamp_idx ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER , INTENT(IN)  :: exch_idx  ! 
    !! Zeiger auf die aktuelle Nummer der Zeitangabe
    INTEGER , INTENT(IN)  :: stamp_idx ! 
    !! Ergebnis: stamp_idx-te diskrete Zeitangabe
    TYPE (t_omi_stamp) :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='get_ipds_discr_time_w' ! 
    !
    CALL new_omi_stamp ( res )
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          IF ( get_ipds_has_discr_time_w ( exch_idx ) ) THEN
             res = get_omi_discr_time ( work_object, exch_idx, stamp_idx )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_ipds_discr_time_w
  !
  !! Ermittle die Angabe eines Zeitraums f&uuml;r das aktuelle Arbeitsobjekt und 
  !! ein bestimmtes Austauschobjekt in OpenMI-konformer Weise <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_ipds_time_span_w ( exch_idx ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER , INTENT(IN)  :: exch_idx  ! 
    !! Ergebnis: Angabe des Zeitraums
    TYPE (t_omi_span)     :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='get_ipds_time_span_w' ! 
    !
    CALL new_omi_span ( res )
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          IF ( get_ipds_has_time_span_w ( exch_idx ) ) THEN
             res = get_omi_time_span ( work_object, exch_idx )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_ipds_time_span_w
  !
  !! Ermittle skalare Daten aus vektoriellen oder skalaren Daten f&uuml;r das 
  !! aktuelle Arbeitsobjekt, ein bestimmtes Austauschobjekt und einen vorgegebenen
  !! Termin in OpenMI-konformer Weise <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_ipds_scalar_var_w ( exch_idx, time ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER            , INTENT(IN) :: exch_idx  ! 
    !! Zeitangabe als modifiziertes Julianisches Datum ( < 0.0 falls nicht vorhanden ) <BR>
    !! Anzahl der Tage, die seit dem <STRONG>17. November 1858 (0 Uhr) UTC</STRONG> vergangen sind
    REAL (KIND=Double) , INTENT(IN) :: time      ! 
    !! Ergebnis: Feld mit skalaren Ergebnisdaten
    REAL (KIND=Double) , POINTER :: res(:)       ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='get_ipds_scalar_var_w' ! 
    !! Hilfsvariablen
    INTEGER                                     :: i, var_no              ! 
    LOGICAL                                     :: alloc_p_z              ! 
    REAL (KIND=Double)                , POINTER :: p_x(:), p_y(:), p_z(:) ! 
    REAL                              , POINTER :: p_val(:,:)             ! 
    CHARACTER (LEN=c_len_phyval_name) , POINTER :: p_var_name(:)          ! 
    !
    NULLIFY( res )
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          p_x => get_omi_xyz_x_coord ( work_object, exch_idx )
          p_y => get_omi_xyz_y_coord ( work_object, exch_idx )
          p_z => get_omi_xyz_z_coord ( work_object, exch_idx )
          alloc_p_z = MERGE( .false., .true., ASSOCIATED(p_z) )
          IF ( alloc_p_z ) THEN
             ALLOCATE( p_z(SIZE(p_x)) )
             p_z(:) = 0.0_Double
          END IF
          CALL get_ipds_val ( p_x, p_y, p_z, &
               TRIM(get_omi_phyval_name(work_object,exch_idx)), p_val, p_var_name )
          var_no = MERGE( 1, get_omi_variant_no(work_object,exch_idx), &
               get_omi_variant_no(work_object,exch_idx) == c_undef_omi_quant_int )
          ALLOCATE ( res(SIZE(p_val,1)) )
          IF ( is_omi_magnitude(work_object,exch_idx) .AND. SIZE(p_val,2) == 2 ) THEN
             DO i=1,SIZE(res)
                res(i) = SQRT( p_val(i,1)*p_val(i,1) + p_val(i,2)*p_val(i,2) )
             END DO
          ELSE
             IF ( var_no >= 1 .AND. var_no <= SIZE(p_val,2) ) THEN
                DO i=1,SIZE(res)
                   res(i) = REAL( p_val(i,var_no), Double )
                END DO
             ELSE
                DO i=1,SIZE(res)
                   res(i) = c_missing_ipds_double
                END DO
             END IF
          END IF
       END IF
    END IF
    IF ( ASSOCIATED( p_z        ) .AND. alloc_p_z ) DEALLOCATE( p_z        )
    IF ( ASSOCIATED( p_val      )                 ) DEALLOCATE( p_val      )
    IF ( ASSOCIATED( p_var_name )                 ) DEALLOCATE( p_var_name )
    NULLIFY( p_x, p_y, p_z, p_val, p_var_name )
    !
  END FUNCTION get_ipds_scalar_var_w
  !
  !! Ermittle vektorielle Daten f&uuml;r das aktuelle Arbeitsobjekt, ein bestimmtes 
  !! Austauschobjekt und einen vorgegebenen Termin in OpenMI-konformer Weise <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_ipds_vector_var_w ( exch_idx, time ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER            , INTENT(IN) :: exch_idx  ! 
    !! Zeitangabe als modifiziertes Julianisches Datum ( < 0.0 falls nicht vorhanden ) <BR>
    !! Anzahl der Tage, die seit dem <STRONG>17. November 1858 (0 Uhr) UTC</STRONG> vergangen sind
    REAL (KIND=Double) , INTENT(IN) :: time      ! 
    !! Ergebnis: Feld mit vektoriellen Ergebnisdaten <BR>
    !!           res(:,1) : x-Komponente <BR>
    !!           res(:,2) : y-Komponente <BR>
    !!           res(:,3) : z-Komponente
    REAL (KIND=Double) , POINTER :: res(:,:)     ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='get_ipds_vector_var_w' ! 
    !! Hilfsvariablen
    INTEGER                                     :: i, j                   ! 
    LOGICAL                                     :: alloc_p_z              ! 
    REAL (KIND=Double)                , POINTER :: p_x(:), p_y(:), p_z(:) ! 
    REAL                              , POINTER :: p_val(:,:)             ! 
    CHARACTER (LEN=c_len_phyval_name) , POINTER :: p_var_name(:)          ! 
    !
    NULLIFY( res )
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          p_x => get_omi_xyz_x_coord ( work_object, exch_idx )
          p_y => get_omi_xyz_y_coord ( work_object, exch_idx )
          p_z => get_omi_xyz_z_coord ( work_object, exch_idx )
          alloc_p_z = MERGE( .false., .true., ASSOCIATED(p_z) )
          IF ( alloc_p_z ) THEN
             ALLOCATE( p_z(SIZE(p_x)) )
             p_z(:) = 0.0_Double
          END IF
          CALL get_ipds_val ( p_x, p_y, p_z, &
               TRIM(get_omi_phyval_name(work_object,exch_idx)), p_val, p_var_name )
          ALLOCATE ( res(SIZE(p_val,1),3) )
          DO j=1,MIN(2,SIZE(p_val,2))
             DO i=1,SIZE(p_val,1)
                res(i,j) = REAL( p_val(i,j), Double )
             END DO
          END DO
          DO i=1,SIZE(res,1)
             res(i,3) = 0.0_Double
          END DO
       END IF
    END IF
    IF ( ASSOCIATED( p_z        ) .AND. alloc_p_z ) DEALLOCATE( p_z        )
    IF ( ASSOCIATED( p_val      )                 ) DEALLOCATE( p_val      )
    IF ( ASSOCIATED( p_var_name )                 ) DEALLOCATE( p_var_name )
    NULLIFY( p_x, p_y, p_z, p_val, p_var_name )
    !
  END FUNCTION get_ipds_vector_var_w
  !
  !! Pr&uuml;fen, ob Daten f&uuml;r eine bestimmte physikalische
  !! Gr&ouml;&szlig;e in den Daten vorhanden sind oder nicht <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION is_ipds_physet_name_available_0 ( name ) &
       RESULT( ok )
    !
    USE m_ipds_physet, ONLY : index_physet_set_name ! 
    !! Name einer physikalischen Gr&ouml;&szlig;e, f&uuml;r die
    !! gepr&uuml;ft werden soll, ob Daten in dem aktuellen Arbeitsobjekt
    !! abgelegt sind oder nicht
    CHARACTER (LEN=*)  , INTENT(IN) :: name ! 
    !! Ergebnis: vorhanden / nicht vorhanden ( .true. / .false. )
    LOGICAL :: ok ! 
    !! Name der Subroutine
    CHARACTER (LEN=31) , PARAMETER :: c_upname='is_ipds_physet_name_available_0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = ( index_physet_set_name( work_object%physet, name ) > 0 )
       END IF
    END IF
    !
  END FUNCTION is_ipds_physet_name_available_0
  !
  !! Pr&uuml;fen, ob Daten f&uuml;r mehrere physikalische
  !! Gr&ouml;&szlig;e in den Daten vorhanden sind oder nicht <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION is_ipds_physet_name_available_1 ( name ) &
       RESULT( ok )
    !
    USE m_ipds_physet, ONLY : index_physet_set_name ! 
    !! Namesliste physikalischer Gr&ouml;&szlig;en, f&uuml;r die
    !! gepr&uuml;ft werden soll, ob Daten in dem aktuellen Arbeitsobjekt
    !! abgelegt sind oder nicht
    CHARACTER (LEN=*)  , INTENT(IN) :: name(:) ! 
    !! Ergebnis: vorhanden / nicht vorhanden ( .true. / .false. )
    LOGICAL :: ok(SIZE(name)) ! 
    !! Name der Subroutine
    CHARACTER (LEN=31) , PARAMETER :: c_upname='is_ipds_physet_name_available_1' ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          DO i=1,SIZE(name)
             ok(i) = ( index_physet_set_name( work_object%physet, name(i) ) > 0 )
          END DO
       END IF
    END IF
    !
  END FUNCTION is_ipds_physet_name_available_1
  !
  !! Pr&uuml;fen, ob ein Punkt innerhalb eines Polygons (Region) liegt oder nicht <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_ipds_point_in_region_0 ( point, region_name ) &
       RESULT( res )
    !! Koordinaten des Punktes
    TYPE (t_point_2d) , INTENT(IN) :: point ! 
    !! Name der Region
    CHARACTER (LEN=*) , INTENT(IN) :: region_name ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: res ! 
    ! Hilfsvariable
    INTEGER :: i, poly_number, l1, l2 ! 
    CHARACTER (LEN=c_len_region_name) :: l_region_name ! 
    !
    res         = .false. 
    poly_number = 0
    !
    DO i = 1, get_ipds_nof_regions( )
       l_region_name = get_ipds_region_name(i)
       l1 = LEN_TRIM(region_name)
       l2 = LEN_TRIM(l_region_name)
       IF ( l1 == l2 ) THEN
          IF ( region_name(1:l1) .EQ. l_region_name(1:l2) ) THEN
             poly_number = i
             res         = is_point_in_region ( point, poly_number )
          END IF
       END IF
    END DO
    !
  END FUNCTION is_ipds_point_in_region_0
  !
  !! Pr&uuml;fen, ob mehrere Punkte innerhalb eines Polygons (Region) liegt oder nicht <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_ipds_point_in_region_1 ( point, region_name ) &
       RESULT( res )
    !! Koordinaten der Punkte
    TYPE (t_point_2d) , INTENT(IN) :: point(:)    ! 
    !! Name der Region
    CHARACTER (LEN=*) , INTENT(IN) :: region_name ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: res(SIZE(point)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(point)
       res(i) = is_ipds_point_in_region_0 ( point(i), region_name )
    END DO
    !
  END FUNCTION is_ipds_point_in_region_1
  !
  !! Pr&uuml;fen, ob ein Polygon (Region) in ipds definiert ist oder nicht <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_ipds_region_defined_0 ( region_name ) &
       RESULT( res )
    !! Name der Region
    CHARACTER (LEN=*) , INTENT(IN) :: region_name ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: res ! 
    ! Hilfsvariable
    INTEGER :: i, l1, l2 ! 
    CHARACTER (LEN=c_len_region_name) :: l_region_name ! 
    !
    res         = .false. 
    !
    DO i = 1, get_ipds_nof_regions( )
       l_region_name = get_ipds_region_name(i)
       l1 = LEN_TRIM(region_name)
       l2 = LEN_TRIM(l_region_name)
       IF ( l1 == l2 ) THEN
          IF ( region_name(1:l1) .EQ. l_region_name(1:l2) ) THEN
			 res         = .TRUE.
          END IF
       END IF
    END DO
    !
  END FUNCTION is_ipds_region_defined_0
  !
  !! Pr&uuml;fen, ob mehrere Namen definiert sind oder nicht <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_ipds_region_defined_1 ( region_name ) &
       RESULT( res )
    !! Name der Region
    CHARACTER (LEN=*) , INTENT(IN) :: region_name(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: res(SIZE(region_name)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(region_name)
       res(i) = is_ipds_region_defined_0 ( region_name(i) )
    END DO
    !
  END FUNCTION is_ipds_region_defined_1
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
  ! Lokale Methoden (PRIVATE)
  !
  ! ----------------------------------------------------------------------
  ! >>> ALLGEMEIN-Methoden <<< [ERR_NO =  0001 bis  0999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_initialised ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Fehlernummer
    INTEGER            :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "io_ipds" nicht initialisiert'
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Package "io_ipds" ist nicht initialisiert'
       cerr(3) = '--> INIT_ipds ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION not_initialised ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .NOT. initialised
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Packages <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_ipds_all_errors ( )
    !
    CALL init_all_errors ( )
    !
  END SUBROUTINE init_ipds_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/Re-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_ipds_all_errors ( )
    !
    CALL clear_all_errors ( )
    !
  END SUBROUTINE clear_ipds_all_errors
  !
  !! De-Allokieren/Re-Initialisieren aller Objekte "t_ipds" des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_ipds_all_objects ( )
    !! Liste der aktuell vorhandenen Objekte
    INTEGER , POINTER :: id(:) ! 
    !
    id => get_ipds_all_id ( )
    !
    IF ( ASSOCIATED( id ) ) THEN
       CALL kill_ipds ( id )
       DEALLOCATE ( id )
    END IF
    !
  END SUBROUTINE clear_ipds_all_objects
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
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> modulspezifische PRIVATE-GET-Methoden <<< [ERR_NO = -1 bis -999]
  ! ----------------------------------------------------------------------
  !
  !! Ermittlung der Regionen-Zugehoerigkeit eines Punktes
  SUBROUTINE get_p_in_region_w1 ( nof_regphyset, p, z, p_in_region )
    !
    !! Anzahl Komponente "regphyset"
    INTEGER           , INTENT(IN)  :: nof_regphyset
    !! Koordinaten der Datenposition
    TYPE (t_point_2d) , INTENT(IN)  :: p
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN)  :: z
    !! Punkt liegt innerhalb der unterschiedlichen Regionen?
    LOGICAL           , INTENT(OUT) :: p_in_region(nof_regphyset)
    !
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_p_in_region_w1'
    !
    !! Zaehler fuer Komponente "regphyset"
    INTEGER :: i_regphyset
    !! Bezeichnung einer Region
    CHARACTER (LEN=c_len_region_name) :: region_name
    !! Kennung, ob der Punkt p innerhalb oder ausserhalb der Grenzlinie
    !! der Region liegen muss, um zur Region zu gehoeren
    LOGICAL :: region_inside
    !! Kleinster erlaubter Tiefenwert einer Region
    REAL    :: region_zmin
    !! Groesster erlaubter Tiefenwert einer Region
    REAL    :: region_zmax
    !! Regionsindexnummer
    INTEGER :: idx_region
    !! Anzahl Grenzlinienpunkte einer Region
    INTEGER :: nof_region_border
    !! Grenzlinie einer Region
    TYPE (t_point_2d) , POINTER :: region_border(:)
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( associated_regphyset_object( work_object ) ) THEN
       !
! Auf Anraten von RV, sgi entfernt
! EXIT innerhalb einer parallelisierten Schleife kann zu Problemen fuehren.
!rv,sgi       !$OMP parallel do private(region_name,region_inside,region_zmin,region_zmax, &
!rv,sgi       !$OMP idx_region, nof_region_border, region_border, stat )
       DO i_regphyset = 1, nof_regphyset
          !
          p_in_region(i_regphyset) = .FALSE.
          !
          region_name   = get_regphyset_region_name(   work_object%regphyset( i_regphyset ) )
          region_inside = get_regphyset_region_inside( work_object%regphyset( i_regphyset ) )
          region_zmin   = get_regphyset_region_zmin(   work_object%regphyset( i_regphyset ) )
          region_zmax   = get_regphyset_region_zmax(   work_object%regphyset( i_regphyset ) )
          !
          IF ( z < region_zmin .OR. z > region_zmax ) CYCLE
          !
          idx_region = index_region_name( work_object%region, region_name )
          IF ( any_error() ) EXIT
          !
          nof_region_border = get_region_nof_border( work_object%region( idx_region ) )
          !
          ALLOCATE ( region_border( nof_region_border ), STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21230, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          region_border = get_region_border( work_object%region( idx_region ) )
          IF ( any_error() ) EXIT
          !
          IF ( inside_point_2d( p, region_border ) ) THEN
             !
             IF ( region_inside ) p_in_region(i_regphyset) = .TRUE.
             !
          ELSE
             !
             IF ( .NOT. region_inside ) p_in_region(i_regphyset) = .TRUE.
             !
          END IF
          !
          DEALLOCATE ( region_border, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21231, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
       END DO
!rv,sgi       !$OMP end parallel do
       !
    END IF
    !
  END SUBROUTINE get_p_in_region_w1
  !
  !! Ermittlung der Regionen-Zugehoerigkeit aller Punkte
  SUBROUTINE get_p_in_region_w2 ( nof_regphyset, nof_points, p, z, p_in_region )
    !
    !! Anzahl Komponente "regphyset"
    INTEGER           , INTENT(IN)  :: nof_regphyset
    !! Anzahl Punkte
    INTEGER           , INTENT(IN)  :: nof_points
    !! Koordinaten der Datenposition
    ! p(nof_points)
    ! Achtung: hier musste die explizite Grosse des Feldes entfernt werden
    ! um mit dem ifort-Compiler auf ALTIX zu uebersetzen
    TYPE (t_point_2d) , INTENT(IN)  :: p( : )
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN)  :: z(nof_points)
    !! Punkt liegt innerhalb der unterschiedlichen Regionen?
    LOGICAL           , INTENT(OUT) :: p_in_region(nof_regphyset,nof_points)
    !
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_p_in_region_w2'
    !
    !! Punkt-Zaehler
    INTEGER :: i_point
    !! Zaehler fuer Komponente "regphyset"
    INTEGER :: i_regphyset
    !! Bezeichnung einer Region
    CHARACTER (LEN=c_len_region_name) :: region_name
    !! Kennung, ob der Punkt p innerhalb oder ausserhalb der Grenzlinie
    !! der Region liegen muss, um zur Region zu gehoeren
    LOGICAL :: region_inside
    !! Kleinster erlaubter Tiefenwert einer Region
    REAL    :: region_zmin
    !! Groesster erlaubter Tiefenwert einer Region
    REAL    :: region_zmax
    !! Regionsindexnummer
    INTEGER :: idx_region
    !! Anzahl Grenzlinienpunkte einer Region
    INTEGER :: nof_region_border
    !! Grenzlinie einer Region
    TYPE (t_point_2d) , POINTER :: region_border(:)
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( associated_regphyset_object( work_object ) ) THEN
       !
! Auf Anraten von RV, sgi entfernt
! EXIT innerhalb einer parallelisierten Schleife kann zu Problemen fuehren.
!rv,sgi        !$OMP parallel do private(region_name,region_inside,region_zmin,region_zmax, &
!rv,sgi        !$OMP idx_region, nof_region_border, region_border, i_point, stat )
       DO i_regphyset = 1, nof_regphyset
          !
          region_name   = get_regphyset_region_name(   work_object%regphyset( i_regphyset ) )
          region_inside = get_regphyset_region_inside( work_object%regphyset( i_regphyset ) )
          region_zmin   = get_regphyset_region_zmin(   work_object%regphyset( i_regphyset ) )
          region_zmax   = get_regphyset_region_zmax(   work_object%regphyset( i_regphyset ) )
          !
          idx_region = index_region_name( work_object%region, region_name )
          IF ( any_error() ) EXIT
          !
          nof_region_border = get_region_nof_border( work_object%region( idx_region ) )
          !
          ALLOCATE ( region_border( nof_region_border ), STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21230, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          region_border = get_region_border( work_object%region( idx_region ) )
          IF ( any_error() ) EXIT
          !
          DO i_point = 1, nof_points
             !
             p_in_region(i_regphyset,i_point) = .FALSE.
             !
             IF ( z(i_point) < region_zmin .OR. z(i_point) > region_zmax ) CYCLE
             !
             IF ( inside_point_2d( p(i_point), region_border ) ) THEN
                !
                IF ( region_inside ) p_in_region(i_regphyset,i_point) = .TRUE.
                !
             ELSE
                !
                IF ( .NOT. region_inside ) p_in_region(i_regphyset,i_point) = .TRUE.
                !
             END IF
             !
          END DO
          !
          DEALLOCATE ( region_border, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21231, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
       END DO
!rv,sgi        !$OMP end parallel do
       !
    END IF
    !
  END SUBROUTINE get_p_in_region_w2
  !
  !! Ermittle die Anzahl der zu einem phys. Satz (Daten und Region)
  !! zugehoerigen Messpositionen (fuer eine phys. Groesse)
  SUBROUTINE get_nof_mespos_phyval_w1 ( nof_regphyset, phyval_name, nof_mespos_phyval )
    !
    !! Anzahl Komponente "regphyset"
    INTEGER           , INTENT(IN)  :: nof_regphyset
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN)  :: phyval_name
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , INTENT(OUT) :: nof_mespos_phyval(nof_regphyset)
    !
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_nof_mespos_phyval_w1'
    !
    !! Zaehler fuer Komponente "regphyset"
    INTEGER                         :: i_regphyset
    !! Anzahl Messstationen einer Region
    INTEGER                         :: nof_mespos
    !! Zaehler fuer die Messstationen einer Region
    INTEGER                         :: i_mespos
    !! Namen aller Messstationen einer Region
    CHARACTER (LEN=c_len_mespos_name), POINTER :: mespos_name(:)
    !! Indexnummer fuer die Messstationen einer Region
    INTEGER,            POINTER :: idx_mespos(:)
    !! Anzahl physikalischer Sets fuer die Messstationen einer Region
    INTEGER,            POINTER :: mespos_nof_physet_set(:)
    !
    !! Statusvariable
    INTEGER :: stat
    !
    !! Suchstring im Fehlertext
    CHARACTER (LEN=10) :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=10) :: cr
    !
    nof_mespos_phyval = 0
    !
    IF ( associated_regphyset_object( work_object ) ) THEN
       !
       DO i_regphyset = 1, SIZE( work_object%regphyset )
          !
          ! Namen aller Messstationen ermitteln
          !
          nof_mespos = get_regphyset_nof_mespos_name( work_object%regphyset( i_regphyset ) )
          !
          ALLOCATE ( mespos_name( nof_mespos ), STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21240, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          mespos_name = get_regphyset_mespos_name( work_object%regphyset( i_regphyset ) )
          !
          ! Anzahl der physikalischen "Sets" fuer jede Messstation ermitteln
          !
          ALLOCATE (                                &
               idx_mespos( nof_mespos ),            &
               mespos_nof_physet_set( nof_mespos ), &
               STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21250, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          DO i_mespos = 1, nof_mespos
             !
             idx_mespos(            i_mespos ) = index_mespos_name( &
                  work_object%mespos, mespos_name( i_mespos ) )
             IF ( any_error() ) CYCLE
             !
             mespos_nof_physet_set( i_mespos ) = get_mespos_nof_physet_set( &
                  work_object%mespos( idx_mespos( i_mespos ) ), phyval_name )
             !
          END DO
          IF ( any_error() ) EXIT
          !
          IF ( ANY( mespos_nof_physet_set < 0 ) .OR. ANY( mespos_nof_physet_set > 1 ) ) THEN
             !
             !$OMP critical
             !
             CALL setup_error_act ( all_errors(:), 21400, c_upname, c_modname )
             !
             cs = '<phy_name>'
             CALL setup_error_act ( cs, TRIM( phyval_name ) )
             !
             cs = '<anz._lt0>'
             WRITE( cr, '(I10)') COUNT( mespos_nof_physet_set < 0 )
             CALL setup_error_act ( cs, cr )
             !
             cs = '<anz._gt1>'
             WRITE( cr, '(I10)') COUNT( mespos_nof_physet_set > 1 )
             CALL setup_error_act ( cs, cr )
             !
             !$OMP end critical
             !
          ENDIF
          !
          IF ( ANY( mespos_nof_physet_set > 0 ) ) THEN
             !
             ! Mindestens eine Messstation verfuegt ueber einen passenden physikalischen "Set"
             !
             ! Anzahl Messstationen mit einem passenden physikalischen "Set"
             !
             nof_mespos_phyval(i_regphyset) = SUM( mespos_nof_physet_set )
             !
          END IF
          !
          DEALLOCATE ( idx_mespos, mespos_nof_physet_set, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21251, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          DEALLOCATE ( mespos_name, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21241, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE get_nof_mespos_phyval_w1
  !
  !! Ermittle die Anzahl der zu einem phys. Satz (Daten und Region)
  !! zugehoerigen Messpositionen (fuer viele phys. Groessen)
  SUBROUTINE get_nof_mespos_phyval_w2 ( nof_regphyset, n_phy, phyval_name, nof_mespos_phyval )
    !
    !! Anzahl Komponente "regphyset"
    INTEGER           , INTENT(IN)  :: nof_regphyset
    !! Anzahl physikalischer Groessen
    INTEGER           , INTENT(IN)  :: n_phy
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN)  :: phyval_name(n_phy)
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , INTENT(OUT) :: nof_mespos_phyval(nof_regphyset,n_phy)
    !
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_nof_mespos_phyval_w2'
    !
    !! Zaehler fuer physikalische Groessen
    INTEGER :: i_phy
    !
    nof_mespos_phyval = 0
    !
    DO i_phy = 1, n_phy
       !
       CALL get_nof_mespos_phyval ( &
            nof_regphyset, phyval_name(i_phy), nof_mespos_phyval(:,i_phy) )
       !
    END DO
    !
  END SUBROUTINE get_nof_mespos_phyval_w2
  !
  !! Ermittle die Koordinaten und phys. Daten der zu einem phys. Satz (Daten und Region)
  !! zugehoerigen Messpositionen (fuer eine phys. Groesse)
  SUBROUTINE get_mespos_phyval_w2 ( &
       max_nof_mespos_phyval, max_nof_mespos_phyval_dim, &
       nof_regphyset, phyval_name, nof_mespos_phyval, &
       mespos_coor, mespos_phyval )
    !
    !! Max. Anzahl Mespositionen
    INTEGER           , INTENT(IN)  :: max_nof_mespos_phyval
    !! Max. Anzahl Mespositionen (nur fuer Dimensionierungen, also >= 1)
    INTEGER                         :: max_nof_mespos_phyval_dim
    !! Anzahl Komponente "regphyset"
    INTEGER           , INTENT(IN)  :: nof_regphyset
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN)  :: phyval_name
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , INTENT(IN)  :: nof_mespos_phyval(nof_regphyset)
    !! Koordinaten aller zu einem phys. Satz zugehoerigen Messpositionen
    ! mespos_coor(max_nof_mespos_phyval_dim,nof_regphyset)
    TYPE (t_point_2d) , INTENT(INOUT) :: mespos_coor( : , : )
    !! Phys. Daten aller zu einem phys. Satz zugehoerigen Messpositionen
    !    TYPE (t_phyval)   , INTENT(INOUT) :: mespos_phyval(max_nof_mespos_phyval_dim,nof_regphyset)
    TYPE (t_phyval)   , INTENT(INOUT) :: mespos_phyval( : , : )
    !
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='get_mespos_phyval_w2'
    !
    !! Zaehler fuer Komponente "regphyset"
    INTEGER                         :: i_regphyset
    !! Anzahl Messstationen einer Region
    INTEGER                         :: nof_mespos
    !! Zaehler fuer die Messstationen einer Region
    INTEGER                         :: i_mespos
    !! Namen aller Messstationen einer Region
    CHARACTER (LEN=c_len_mespos_name), POINTER :: mespos_name(:)
    !! Indexnummer fuer die Messstationen einer Region
    INTEGER,            POINTER :: idx_mespos(:)
    !! Anzahl physikalischer Sets fuer die Messstationen einer Region
    INTEGER,            POINTER :: mespos_nof_physet_set(:)
    !! Zaehler fuer Messstationen mit passendem physikalischen Set
    INTEGER                         :: i_mespos_phyval
    !
    !! Statusvariable
    INTEGER :: stat
    !
    !! Suchstring im Fehlertext
    CHARACTER (LEN=10) :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=10) :: cr
    !
    IF ( associated_regphyset_object( work_object ) ) THEN
       !
       DO i_regphyset = 1, SIZE( work_object%regphyset )
          !
          ! Initialisierung
          !
          CALL new_point_2d( mespos_coor  (:,i_regphyset) )
          CALL new_phyval  ( mespos_phyval(:,i_regphyset) )
          !
          ! Namen aller Messstationen ermitteln
          !
          nof_mespos = get_regphyset_nof_mespos_name( work_object%regphyset( i_regphyset ) )
          !
          ALLOCATE ( mespos_name( nof_mespos ), STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21240, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          mespos_name = get_regphyset_mespos_name( work_object%regphyset( i_regphyset ) )
          !
          ! Anzahl der physikalischen "Sets" fuer jede Messstation ermitteln
          !
          ALLOCATE (                                &
               idx_mespos( nof_mespos ),            &
               mespos_nof_physet_set( nof_mespos ), &
               STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21250, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          DO i_mespos = 1, nof_mespos
             !
             idx_mespos(            i_mespos ) = index_mespos_name( &
                  work_object%mespos, mespos_name( i_mespos ) )
             IF ( any_error() ) CYCLE
             !
             mespos_nof_physet_set( i_mespos ) = get_mespos_nof_physet_set( &
                  work_object%mespos( idx_mespos( i_mespos ) ), phyval_name )
             !
          END DO
          IF ( any_error() ) EXIT
          !
          IF ( ANY( mespos_nof_physet_set < 0 ) .OR. ANY( mespos_nof_physet_set > 1 ) ) THEN
             !
             !$OMP critical
             !
             CALL setup_error_act ( all_errors(:), 21400, c_upname, c_modname )
             !
             cs = '<phy_name>'
             CALL setup_error_act ( cs, TRIM( phyval_name ) )
             !
             cs = '<anz._lt0>'
             WRITE( cr, '(I10)') COUNT( mespos_nof_physet_set < 0 )
             CALL setup_error_act ( cs, cr )
             !
             cs = '<anz._gt1>'
             WRITE( cr, '(I10)') COUNT( mespos_nof_physet_set > 1 )
             CALL setup_error_act ( cs, cr )
             !
             !$OMP end critical
             !
          ENDIF
          !
          IF ( ANY( mespos_nof_physet_set > 0 ) ) THEN
             !
             ! Mindestens eine Messstation verfuegt ueber einen passenden physikalischen "Set"
             !
             ! Alle physikalischen "Set"s holen
             !
             i_mespos_phyval = 0
             !
             DO i_mespos = 1, nof_mespos
                !
                IF ( mespos_nof_physet_set( i_mespos ) == 1 ) THEN
                   !
                   i_mespos_phyval = i_mespos_phyval + 1
                   IF ( i_mespos_phyval > max_nof_mespos_phyval ) THEN
                      !
                      !$OMP critical
                      !
                      CALL setup_error_act( all_errors(:), 21500, c_upname, c_modname )
                      !
                      cs = '<phy_name>'
                      CALL setup_error_act ( cs, TRIM( phyval_name ) )
                      !
                      !$OMP end critical
                      !
                   END IF
                   IF ( any_error() ) EXIT
                   !
                   mespos_phyval( i_mespos_phyval, i_regphyset ) = get_physet_set( &
                           get_mespos_physet( work_object%mespos( idx_mespos( i_mespos ) ) ), &
                           phyval_name )
                   !
                   mespos_coor(   i_mespos_phyval, i_regphyset ) = get_mespos_coor( &
                           work_object%mespos( idx_mespos( i_mespos ) ) )
                   !
                END IF
                !
             END DO
             !
             IF ( any_error() ) EXIT
             !
             IF ( i_mespos_phyval /= nof_mespos_phyval(i_regphyset) ) THEN
                !
                !$OMP critical
                !
                CALL setup_error_act( all_errors(:), 21501, c_upname, c_modname )
                !
                cs = '<phy_name>'
                CALL setup_error_act ( cs, TRIM( phyval_name ) )
                !
                !$OMP end critical
                !
             ENDIF
             !
          END IF
          !
          DEALLOCATE ( idx_mespos, mespos_nof_physet_set, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21251, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          DEALLOCATE ( mespos_name, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21241, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE get_mespos_phyval_w2
  !
  !! Ermittle die Koordinaten und phys. Daten der zu einem phys. Satz (Daten und Region)
  !! zugehoerigen Messpositionen (fuer viele phys. Groessen)
  SUBROUTINE get_mespos_phyval_w3 ( &
       max_nof_mespos_phyval, max_nof_mespos_phyval_dim, &
       nof_regphyset, n_phy, phyval_name, nof_mespos_phyval, &
       mespos_coor, mespos_phyval )
    !
    !! Max. Anzahl Mespositionen
    INTEGER           , INTENT(IN)  :: max_nof_mespos_phyval
    !! Max. Anzahl Mespositionen (nur fuer Dimensionierungen, also >= 1)
    INTEGER                         :: max_nof_mespos_phyval_dim
    !! Anzahl Komponente "regphyset"
    INTEGER           , INTENT(IN)  :: nof_regphyset
    !! Anzahl physikalischer Groessen
    INTEGER           , INTENT(IN)  :: n_phy
    !! Bezeichnung der physikalischen Groessen
    CHARACTER (LEN=*) , INTENT(IN)  :: phyval_name(n_phy)
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , INTENT(IN)  :: nof_mespos_phyval(nof_regphyset,n_phy)
    !! Koordinaten aller zu einem phys. Satz zugehoerigen Messpositionen
    TYPE (t_point_2d) , INTENT(INOUT) :: mespos_coor(:,:,:)
    !! Phys. Daten aller zu einem phys. Satz zugehoerigen Messpositionen
    TYPE (t_phyval)   , INTENT(INOUT) :: mespos_phyval(:,:,:)
    !
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='get_mespos_phyval_w3'
    !
    !! Zaehler fuer physikalische Groessen
    INTEGER :: i_phy
    !
    DO i_phy = 1, n_phy
       !
       CALL get_mespos_phyval ( &
            max_nof_mespos_phyval, max_nof_mespos_phyval_dim, &
            nof_regphyset, phyval_name(i_phy), nof_mespos_phyval(:,i_phy), &
            mespos_coor(:,:,i_phy), mespos_phyval(:,:,i_phy) )
       !
    END DO
    !
  END SUBROUTINE get_mespos_phyval_w3
  !
  !! Ermittle die Messwerte (REAL) aller Varianten einer phys. Groesse
  !! fuer alle Messpositionen
  SUBROUTINE get_mespos_val_3 ( &
       nof_all_var, max_nof_mespos_phyval_dim, nof_regphyset, &
       nof_mespos_phyval, mespos_phyval, all_var_name, &
       mespos_val )
    !
    !! Anzahl aller Variationen einer phys. Groesse
    INTEGER           , INTENT(IN)  :: nof_all_var
    !! Max. Anzahl Mespositionen (nur fuer Dimensionierungen, also >= 1)
    INTEGER                         :: max_nof_mespos_phyval_dim
    !! Anzahl Komponente "regphyset"
    INTEGER           , INTENT(IN)  :: nof_regphyset
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , INTENT(IN)  :: nof_mespos_phyval(nof_regphyset)
    !! Phys. Daten aller zu einem phys. Satz zugehoerigen Messpositionen
    ! mespos_phyval(max_nof_mespos_phyval_dim,nof_regphyset)
    TYPE (t_phyval)   , INTENT(IN)  :: mespos_phyval(:,:)
    !! Bezeichnung _aller_ Varianten der physikalischen Groesse
    CHARACTER (LEN=c_len_phyval_name), INTENT(IN)  :: all_var_name(nof_all_var)
    !! Phys. Messwerte aller Varianten aller zu einem phys. Satz zugehoerigen Messpositionen
    REAL (KIND=Single), INTENT(OUT) :: mespos_val(nof_all_var,max_nof_mespos_phyval_dim,nof_regphyset)
    !
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_mespos_val_3'
    !
    !! Zaehler fuer Regionen
    INTEGER :: i_regphyset
    !! Zaehler fuer Positionen
    INTEGER :: i_mespos
    !! Anzahl Datenvariationen einer Messstation
    INTEGER :: nof_mespos_var
    !! Zaehler fuer Varianten
    INTEGER :: i_var, i_mespos_var
    !! Variante gefunden?
    LOGICAL :: found
    !
    !! Variantennamen fuer alle Messstationen
    CHARACTER (LEN=c_len_phyval_name), POINTER :: l_mespos_var_name(:)
    !! Daten aller Variante fuer alle Messstationen
    REAL,               POINTER :: l_mespos_val(:)
    !
    !! Statusvariable
    INTEGER :: stat
    !
    mespos_val = 0.00
    !
    DO i_regphyset = 1, nof_regphyset
       !
       DO i_mespos = 1, nof_mespos_phyval(i_regphyset)
          !
          IF ( ok_phyval( mespos_phyval( i_mespos, i_regphyset ) ) ) THEN
             !
             ! [1.1] Hilfsfelder bereitstellen
             !
             nof_mespos_var = get_phyval_nof_var_name( mespos_phyval( i_mespos, i_regphyset ) )
             !
             ALLOCATE ( &
                  l_mespos_var_name( nof_mespos_var ), &
                  l_mespos_val(      nof_mespos_var ), &
                  STAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 25010, c_upname, c_modname, stat )
             IF ( any_error() ) EXIT
             !
             ! [1.2] Komponenten des Datenobjekts holen
             !
             l_mespos_var_name = get_phyval_var_name( mespos_phyval( i_mespos, i_regphyset ) )
             l_mespos_val      = get_phyval_val(      mespos_phyval( i_mespos, i_regphyset ) )
             IF ( any_error() ) EXIT
             !
             ! [1.3] Die Daten der vorhandenen Varianten uebertragen
             !
             DO i_mespos_var = 1, nof_mespos_var
                !
                found = .FALSE.
                i_var = 0
                !
                DO
                   !
                   i_var = i_var + 1
                   IF ( i_var > nof_all_var ) EXIT
                   !
                   IF ( TRIM( all_var_name(i_var) ) == TRIM( l_mespos_var_name(i_mespos_var) ) ) found = .TRUE.
                   !
                   IF ( found ) EXIT
                   !
                END DO
                !
                IF ( found ) mespos_val( i_var, i_mespos, i_regphyset ) = l_mespos_val( i_mespos_var )
                !
             END DO
             !
             ! [1.4] Hilfsfelder entfernen
             !
             DEALLOCATE ( l_mespos_var_name, l_mespos_val, STAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 25011, c_upname, c_modname, stat )
             !
          END IF
          !
       END DO
       !
    END DO
    !
  END SUBROUTINE get_mespos_val_3
  !
  ! -----------------------------------------------------------------------------
  ! >>> modulspezifische PRIVATE-TARGET-Methoden <<< [ERR_NO = -22000 bis -22999]
  ! -----------------------------------------------------------------------------
  !
  ! -----------------------------------------------------------------------------
  ! >>> modulspezifische PRIVATE-READ-Methoden <<< [ERR_NO = -23000 bis -23999]
  ! -----------------------------------------------------------------------------
  !
  ! -------------------------------------------------------------------------
  ! PRIVATE-Methoden ueber implementierte Datei-Varianten
  ! -------------------------------------------------------------------------
  !
  ! -----------------------------------------------------------------------------
  ! PRIVATE IS-Methoden 
  ! -----------------------------------------------------------------------------
  !
  FUNCTION is_point_in_region ( point, poly_number ) &
       RESULT( res )
    !
    !! Koordinaten der Datenposition
    TYPE (t_point_2d) , INTENT(IN)  :: point      !
    !! Regionsnummer
    INTEGER           , INTENT(IN) :: poly_number ! 
    !! R&uuml;ckgabewert : Punkt liegt innerhalb der unterschiedlichen Regionen?
    LOGICAL  :: res ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='is_point_in_region' ! 
    !
    !! Bezeichnung einer Region
    CHARACTER (LEN=c_len_region_name) :: region_name ! 
    !! Kennung, ob der Punkt p innerhalb oder ausserhalb der Grenzlinie
    !! der Region liegen muss, um zur Region zu gehoeren
    LOGICAL :: region_inside ! 
    !! Index einer Region
    INTEGER :: idx_region ! 
    !! Anzahl Grenzlinienpunkte einer Region
    INTEGER :: nof_region_border ! 
    !! Grenzlinie einer Region
    TYPE (t_point_2d) , ALLOCATABLE :: region_border(:) ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ! ---------------------------------------------------------------------------
    ! [1] Initialisierungen 
    ! ---------------------------------------------------------------------------
    !
    res               = .false.
    region_name       = get_regphyset_region_name(   work_object%regphyset( poly_number ) )
    region_inside     = get_regphyset_region_inside( work_object%regphyset( poly_number ) )
    idx_region        = index_region_name( work_object%region, region_name )
    nof_region_border = get_region_nof_border( work_object%region( idx_region ) )
    !
    ALLOCATE ( region_border( nof_region_border ), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 21230, c_upname, c_modname, stat )
    ELSE
       region_border = get_region_border( work_object%region( idx_region ) )
       !
       IF ( inside_point_2d( point, region_border ) ) THEN
          IF ( region_inside ) res = .TRUE.
       ELSE
          IF ( .NOT. region_inside ) res = .TRUE.
       END IF
       DEALLOCATE ( region_border, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 21231, c_upname, c_modname, stat )
    END IF
    !
  END FUNCTION is_point_in_region
  !
END MODULE io_ipds_ui
! TailOfPackageUserInterface -----------------------------------------------
