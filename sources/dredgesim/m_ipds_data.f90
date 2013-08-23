! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Datenmodul des Paketes "ipds" und elementare Methoden auf Daten</H2>
!! @author Jens J&uuml;rges
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_data.f90
!! <HR>
!! global data for package "ipds" plus some elementary methods <BR>
!! <HR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2004 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Paket-Moduls
!
!  1.01 : 2002-08-08 : J. Juerges : Startversion
!  1.02 : 2002-09-16 : J. Juerges : Typ-Bezeichnungen fuer die unerodierbare Schicht ergaenzt
!  1.03 : 2002-10-15 : J. Juerges : Bugfix fuer ipds-Datei ohne Messpositionen
!  1.04 : 2003-01-10 : J. Juerges : Entwicklungsgeschichte nicht fuer f90doc
!  1.05 : 2003-01-10 : J. Juerges : Lizenzangabe durch Platzhalter ersetzt
!  1.06 : 2003-01-31 : J. Juerges : get_lowercase_char entfernt (braucht niemand aus dem Paket)
!  1.07 : 2003-02-10 : J. Juerges : Fehlerdeklaration fuer Mehrprozessor-Betrieb fit gemacht
!  1.08 : 2003-02-11 : J. Juerges : Routine get_physet_set wird neu ausgewertet
!  1.09 : 2004-04-28 : G. Lang    : Language bei datetime beruecksichtigen
!  1.10 : 2004-07-09 : G. Lang    : "suspendedload" hinzugefuegt
!  1.11 : 2004-07-12 : G. Lang    : "zo_roughnesslength" hinzugefeugt
!  1.12 : 2005-04-12 : J. Juerges : "Porenwasser" in "Porositaet" umbenannt
!  1.13 : 2005-04-21 : J. Juerges : get_var_name_object angepasst: Routine liefert mind. den Namen
!                                   der phys. Groesse als Variante. "Keine Varianten vorhanden"
!                                   kommt nun nicht mehr vor.
!  2.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  2.02 : 2005-08-22 : J. Juerges : Auslagerung der Definition phys. Groessen in Modul m_ipds_phydef
!  2.03 : 2006-01-04 : G. Lang    : in kill_ipds_object_0 wird work_object vor Zugriff mit associated geprueft
!  3.01 : 2007-03-13 : G. Lang    : neue Hauptversionsnummer 3
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! <OL>
!!   <LI> Definieren elementarer Types f&uuml;r das Paket "ipds";
!!   <LI> Speicherung globaler Daten f&uuml;r das Paket "ipds"
!!        um den Datenaustausch innerhalb des Paketes zu vereinfachen;
!!   <LI> Elementare Methoden auf Daten des Typs "t_ipds" des Paketes;
!!   <LI> Ein direkter Zugriff auf diese Daten/Methoden von anderen Paketen 
!!        aus ist nicht zul&auml;ssig.
!! </OL>
!! <HR>
!
MODULE m_ipds_data
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] BASIS-Modul mit globalen Konstantwerten
  USE b_constants, ONLY :   &
       ! Konstante
       Double
  !
  ! [A.1.2] BASIS-Modul mit Fehler-Typ und -Routinen
  USE b_error, ONLY :   &
       ! Typdefinitionen
       t_error,         &
       ! Routinen
       no_error,        &
       any_error,       &
       print_error,     &
       setup_error_act
  !
  ! [A.1.3] BASIS-Modul mit Datei-Typ und -Routinen
  USE b_file, ONLY :    &
       ! Typdefinitionen
       t_file,          &
       ! Routinen
       new_file,        &
       ok_file,         &
       print_file,      &
       set_file_name,   &
       set_file_unit,   &
       set_file_status, &
       set_file_access, &
       set_file_status, &
       set_file_form,   &
       set_file_action, &
       set_file_type,   &
       get_file_name,   &
       get_file_unit,   &
       get_file_form,   &
       get_file_delim,  &
       get_file_type,   &
       get_file_access
  !
  ! [A.1.4] BASIS-Modul fuer Zeitpunktangaben
  USE b_datetime, ONLY :        &
       !   Typdefinitionen
       t_datetime,              &
       !   Routinen / Interfaces
       new_datetime,            &
       kill_datetime,           &
       ok_datetime,             &
       print_datetime,          &
       string_to_datetime,      &
       datetime_to_string,      &
       setup_datetime_language, &
       get_datetime_language
  !
  ! [A.1.5] BASIS-Modul mit Dimensions-Methoden
  USE b_dim, ONLY : &
       !   Typdefinitionen
       t_dim,       &
       !   Routinen / Interfaces
       new_dim,     &
       kill_dim,    &
       ok_dim,      &
       print_dim,   &
       add_dim
  !
  ! [A.1.6] BASIS-Modul mit Attribut-Methoden
  USE b_att, ONLY : &
       !   Typdefinitionen
       t_att,       &
       !   Routinen / Interfaces
       new_att,     &
       kill_att,    &
       ok_att,      &
       print_att,   &
       add_att
  !
  ! [A.1.7] BASIS-Modul mit Variablen-Methoden
  USE b_var, ONLY : &
       !   Typdefinitionen
       t_var,       &
       !   Routinen / Interfaces
       new_var,     &
       kill_var,    &
       ok_var,      &
       print_var,   &
       add_var
  !
  ! [A.1.8] Basis-Modul mit Typ und Methoden Quantities (OpenMI)
  USE b_omi_quant, ONLY : &
       ! Typdefinition
       t_omi_quant,       &
       ! Routinen / Interfaces
       ok_omi_quant,      &
       print_omi_quant,   &
       copy_omi_quant,    &
       kill_omi_quant
  !
  ! [A.1.9] Basis-Modul mit Typ und Methoden Koordinatenobjekte (OpenMI)
  USE b_omi_xyz, ONLY :   &
       ! Typdefinition
       t_omi_xyz,         &
       ! Routinen / Interfaces
       ok_omi_xyz,        &
       print_omi_xyz,     &
       copy_omi_xyz,      &
       kill_omi_xyz
  !
  ! [A.1.10] Basis-Modul mit Typ und Methoden Verweislisten auf Koordinatenobjekte (OpenMI)
  USE b_omi_ind, ONLY : &
       ! Typdefinition
       t_omi_ind,       &
       ! Routinen / Interfaces
       ok_omi_ind,      &
       print_omi_ind,   &
       copy_omi_ind,    &
       kill_omi_ind
  !
  ! [A.1.11] Basis-Modul mit Typ und Methoden ElementSets (OpenMI)
  USE b_omi_ele, ONLY : &
       ! Typdefinition
       t_omi_ele,       &
       ! Routinen / Interfaces
       ok_omi_ele,      &
       print_omi_ele,   &
       copy_omi_ele,    &
       kill_omi_ele
  !
  ! [A.1.12] Basis-Modul mit Typ und Methoden DatenOperationen (OpenMI)
  USE b_omi_dope, ONLY : &
       ! Typdefinition
       t_omi_dope,       &
       ! Routinen / Interfaces
       ok_omi_dope,      &
       print_omi_dope,   &
       copy_omi_dope,    &
       kill_omi_dope
  !
  ! [A.1.13] Basis-Modul mit Typ und Methoden ExchangeItems (OpenMI)
  USE b_omi_exch, ONLY : &
       ! Typdefinition
       t_omi_exch,       &
       ! Routinen / Interfaces
       ok_omi_exch,      &
       print_omi_exch,   &
       copy_omi_exch,    &
       kill_omi_exch
  !
  ! [A.1.14] Basis-Modul mit Typ und Methoden Zeitspanne (OpenMI)
  USE b_omi_span, ONLY : &
       ! Typdefinition
       t_omi_span,       &
       ! Routinen / Interfaces
       ok_omi_span,      &
       print_omi_span,   &
       kill_omi_span
  !
  ! [A.1.15] Basis-Modul mit Typ und Methoden Zeitangabe (OpenMI)
  USE b_omi_stamp, ONLY : &
       ! Typdefinition
       t_omi_stamp,      &
       ! Routinen / Interfaces
       ok_omi_stamp,     &
       print_omi_stamp,  &
       kill_omi_stamp
  !
  ! [A.2.1] PACKAGE-Modul fuer einen Satz phys. Groessen
  USE m_ipds_physet, ONLY :     &
       !   Typdefinitionen
       t_physet,                &
       !   Routinen / Interfaces
       new_physet,              &
       kill_physet,             &
       assign_physet,           &
       get_physet_set,          &
       get_physet_nof_var_name, &
       get_physet_var_name,     &
       ok_physet,               &
       print_physet
  ! 
  ! [A.2.2] PACKAGE-Modul fuer Regionendaten
  USE m_ipds_region, ONLY :   &
       !   Typdefinitionen
       t_region,              &
       !   Routinen / Interfaces
       new_region,            &
       kill_region,           &
       assign_region,         &
       get_region_name,       &
       ok_region,             &
       print_region
  ! 
  ! [A.2.3] PACKAGE-Modul fuer Messstationsdaten
  USE m_ipds_mespos, ONLY :       &
       !   Typdefinitionen
       t_mespos,                  &
       !   Routinen / Interfaces
       new_mespos,                &
       kill_mespos,               &
       assign_mespos,             &
       index_mespos_name,         &
       get_mespos_name,           &
       get_mespos_physet,         &
       get_mespos_nof_var_name,   &
       get_mespos_var_name,       &
       ok_mespos,                 &
       print_mespos
  ! 
  ! [A.2.4] PACKAGE-Modul fuer phys. Groessen und Regionen
  USE m_ipds_regphyset, ONLY :        &
       !   Typdefinitionen
       t_regphyset,                   &
       !   Routinen / Interfaces
       new_regphyset,                 &
       kill_regphyset,                &
       assign_regphyset,              &
       get_regphyset_region_name,     &
       get_regphyset_nof_mespos_name, &
       get_regphyset_mespos_name,     &
       ok_regphyset,                  &
       print_regphyset
  ! 
  ! [A.2.5] PACKAGE-Modul fuer Daten einer phys. Groesse
  USE m_ipds_phyval, ONLY :   &
       !   Typdefinitionen
       t_phyval,              &
       !   Daten
       c_len_phyval_name,     &
       !   Routinen / Interfaces
       kill_phyval,           &
       get_phyval_nof_type,   &
       get_phyval_type
  ! 
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [B] oeffentlich zugaengliche Deklarationen
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Typdefinitionen
  !
  !! Typ zur Aufnahme aller (konstanten) Daten eines (Package-) Objekts                              <BR>
  !! "id"        : eindeutige Identifikationsnummer des Datensatzes                                  <BR>
  !! "name"      : beschreibender Name des Datensatzes                                               <BR>
  !! "file"      : Datei-Objekt                                                                      <BR>
  !! "datetime"  : Datum und Uhrzeit aus Block "date_and_time"                                       <BR>
  !! "physet"    : Globale Default-Werte aus Block "global_constant_values"                          <BR>
  !! "region"    : Alle Regionen (jeder "region"-Block)                                              <BR>
  !! "mespos"    : Alle Messstationen (jeder "sampling_point"-Block)                                 <BR>
  !! "regphyset" : Alle Regionen mit vom Default abweichenden Werten (jeder "regional_values"-Block) <BR>
  !! "dim"       : OpenMI-konforme Dimensionsbezeichner der Variablen als Objekt "t_dim"             <BR>
  !! "att"       : OpenMI-konforme globale und variablenbezogene Attribute als Objekt "t_att"        <BR>
  !! "var"       : OpenMI-konforme Beschreibung der Variablen als Objekt "t_var"                     <BR>
  !! "quant"     : OpenMI-konforme Definition der <EM>Quantities</EM>                                <BR>
  !! "xyz"       : OpenMI-konforme Definition der Koordinaten-Objekte                                <BR>
  !! "ind"       : OpenMI-konforme Definition der Zeiger auf Koordinaten-Objekte                     <BR>
  !! "ele"       : OpenMI-konforme Definition von Elementsets                                        <BR>
  !! "dope"      : OpenMI-konforme Definition von Datenoperationen                                   <BR>
  !! "exch"      : OpenMI-konforme Definition von <EM>ExchangeItems</EM>                             <BR>
  !! "span"      : OpenMI-konforme Zeitraum-Angabe                                                   <BR>
  !! "stamp"     : OpenMI-konforme Zeitangaben (Hilfsvariable)
  TYPE , PUBLIC :: t_ipds
     INTEGER                      :: id           ! [ a] 
     CHARACTER (LEN=80)           :: name         ! [ b]
     TYPE (t_file)                :: file         ! [ c]
     TYPE (t_datetime)  , POINTER :: datetime     ! [01]
     TYPE (t_physet)    , POINTER :: physet       ! [02]
     TYPE (t_region)    , POINTER :: region(:)    ! [03]
     TYPE (t_mespos)    , POINTER :: mespos(:)    ! [04]
     TYPE (t_regphyset) , POINTER :: regphyset(:) ! [05]
     TYPE (t_dim)       , POINTER :: dim(:)       ! [06]
     TYPE (t_att)       , POINTER :: att(:)       ! [07]
     TYPE (t_var)       , POINTER :: var(:)       ! [08]
     TYPE (t_omi_quant) , POINTER :: quant(:)     ! [09]
     TYPE (t_omi_xyz)   , POINTER :: xyz(:)       ! [10]
     TYPE (t_omi_ind)   , POINTER :: ind(:)       ! [11]
     TYPE (t_omi_ele)   , POINTER :: ele(:)       ! [12]
     TYPE (t_omi_dope)  , POINTER :: dope(:)      ! [13]
     TYPE (t_omi_exch)  , POINTER :: exch(:)      ! [14]
     TYPE (t_omi_span)  , POINTER :: span         ! [15]
     TYPE (t_omi_stamp) , POINTER :: stamp(:)     ! [16]
  END TYPE t_ipds
  !
  !! Verkettete Liste zum Verwalten mehrerer Objekte des Typs "t_ipds" <BR>
  !! Komponente "object" : ein Objekt "t_ipds" der verketteten Liste <BR>
  !! Komponente "prev"   : Zeiger auf das vorangehende Objekt der Liste <BR>
  !! Komponente "next"   : Zeiger auf das nachfolgende Objekt der Liste
  TYPE , PUBLIC :: t_ipds_list
     TYPE (t_ipds)      , POINTER :: object ! ein (Package-) Objekt mit (vielen) Daten
     TYPE (t_ipds_list) , POINTER :: prev   ! Zeiger auf vorangehendes Objekt
     TYPE (t_ipds_list) , POINTER :: next   ! Zeiger auf nachfolgendes Objekt
  END TYPE t_ipds_list
  !
  ! [B.2] Konstantwerte (Parameter)
  !
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PUBLIC , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r Sequential Ascii Input/Output
  INTEGER           , PUBLIC , PARAMETER :: c_asc_seq_lun  = 15               ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PUBLIC , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der anwenderspezifischen Datenkomponenten des Typs t_ipds
  INTEGER           , PUBLIC , PARAMETER :: c_nofcomp      = 16               ! 
  !
  !! Wert zum Kennzeichnen fehlender Gr&ouml;&szlig;en
  REAL (KIND=Double), PUBLIC,  PARAMETER :: c_missing_ipds_double=1.1E+30_Double ! 
  !
  !! Definition der implementierten ein-eindeutigen Datei-Varianten <BR>
  !! Anzahl der implementierten Varianten f&uuml;r i/o
  INTEGER           , PUBLIC , PARAMETER :: c_max_variants = 1
  !! --> die definierten Datei-Varianten muessen ein-eindeutig sein <---
  !! Bezeichnung der Datei-TYPE-Varianten 
  CHARACTER (LEN=04), PUBLIC , PARAMETER :: c_variants_type(c_max_variants) = &
       (/ 'ipds' /)
  !! Bezeichnung der implementieren Fortran ACCESS-Varianten
  CHARACTER (LEN=10), PUBLIC , PARAMETER :: c_variants_access(c_max_variants) = &
       (/ 'SEQUENTIAL' /)
  !! Bezeichnung der implementieren Fortran FORM-Varianten
  CHARACTER (LEN=11), PUBLIC , PARAMETER :: c_variants_form(c_max_variants) = &
       (/ 'FORMATTED  ' /)
  !! Bezeichnung der implementieren Fortran DELIM-Varianten
  CHARACTER (LEN=10), PUBLIC , PARAMETER :: c_variants_delim(c_max_variants) = &
       (/ 'NONE      ' /)
  !
  !! Anzahl der erschiedenen m&ouml;glichen Tiefenangaben zur unerodierbaren Schicht
  INTEGER           , PUBLIC , PARAMETER :: c_nof_rl_depth_type=2
  !! Typ-Nummern der verschiedenen m&ouml;glichen Tiefenangaben zur unerodierbaren Schicht
  INTEGER           , PUBLIC , PARAMETER :: c_rl_depth_nr(c_nof_rl_depth_type) = (/ 1, 2 /)
  !! Bezeichnung der verschiedenen m&ouml;glichen Tiefenangaben zur unerodierbaren Schicht
  CHARACTER (LEN= 8), PUBLIC , PARAMETER :: c_rl_depth_type(0:c_nof_rl_depth_type) = (/ &
       'nodef   ', &
       'relative', &
       'absolute' /)
  !! Anzahl der verschiedenen m&ouml;glichen Rauheitstypen der unerodierbaren Schicht
  INTEGER           , PUBLIC , PARAMETER :: c_nof_rl_rough_type=1
  !! Typ-Nummern der verschiedenen m&ouml;glichen Rauheitstypen der unerodierbaren Schicht
  INTEGER           , PUBLIC , PARAMETER :: c_rl_rough_nr(c_nof_rl_rough_type) = (/ 1 /)
  !! Bezeichnung der verschiedenen m&ouml;glichen Rauheitstypen der unerodierbaren Schicht
  CHARACTER (LEN= 9), PUBLIC , PARAMETER :: c_rl_rough_type(0:c_nof_rl_rough_type) = (/ &
       'nodef    ', &
       'nikuradse' /)
  !
  ! [B.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , PUBLIC , ALLOCATABLE, SAVE :: all_errors(:)! 
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                , PUBLIC , SAVE :: initialised = .false.  ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                , PUBLIC , SAVE :: prn_op      = c_op     ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                , PUBLIC , SAVE :: trc_op      = c_op     ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                , PUBLIC , SAVE :: prn_lun     = c_lun    ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                , PUBLIC , SAVE :: trc_lun     = c_lun    ! 
  !! logische Kanalnummer f&uuml;r Sequential Ascii Input/Output
  INTEGER                , PUBLIC , SAVE :: asc_seq_lun = c_asc_seq_lun ! 
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                , PUBLIC , SAVE :: n_init      = 0             ! 
  !
  !! aktuelle Anzahl verwalteter (Package-) Objekte
  INTEGER                      , PUBLIC , SAVE :: nofobjects = 0    ! 
  !! Zeiger auf das erste verwaltete (Package-) Objekt (in Liste)
  TYPE (t_ipds_list) , POINTER , PUBLIC , SAVE :: first_list_object ! 
  !! Zeiger auf das aktuell zu bearbeitende (Package-) Objekt (in Liste)
  TYPE (t_ipds_list) , POINTER , PUBLIC , SAVE :: work_list_object  ! 
  !! Zeiger auf das aktuell zu bearbeitende (Package-) Objekt
  TYPE (t_ipds)      , POINTER , PUBLIC , SAVE :: work_object       ! 
  !
  ! [B.4] Schnittstellen
  !
  !! Erzeugen eines neuen Gitterobjektes
  INTERFACE new_ipds_object
     MODULE PROCEDURE new_ipds_object_0
  END INTERFACE
  !! Entfernen eines vorhandenen Gitterobjektes
  INTERFACE kill_ipds_object
     MODULE PROCEDURE kill_ipds_object_0
  END INTERFACE
  !
  !! Pr&uuml;fen, ob ein Arbeistobjekt o.k. ist
  INTERFACE ok_work_object
     MODULE PROCEDURE ok_work_object_0
  END INTERFACE
  !
  !! Ermittle den Zeiger auf ein Objekt mit "id" <BR>
  !! a) ausf der Basis der Identifikationsnummer <BR>
  !! b) auf der Basis des Objektnamens
  INTERFACE get_ipds_list_object
     MODULE PROCEDURE get_ipds_list_object_0
     MODULE PROCEDURE get_ipds_list_object_n0
  END INTERFACE
  !! Holen einer Kopie f&uuml;r Komponente "name" aus Object "t_ipds"
  INTERFACE get_name_object
     MODULE PROCEDURE get_name_object_0
  END INTERFACE
  !! Holen einer Kopie f&uuml;r Komponente "file" aus Object "t_ipds"
  INTERFACE get_file_object
     MODULE PROCEDURE get_file_object_0
  END INTERFACE
  !! Holen eines Zeigers auf skalare Komponente "datetime" des Objekts "t_ipds"
  INTERFACE get_datetime_object
     MODULE PROCEDURE get_datetime_object_0
  END INTERFACE
  !! Holen eines Zeigers auf skalare Komponente "physet" des Objekts "t_ipds"
  INTERFACE get_physet_object
     MODULE PROCEDURE get_physet_object_0
  END INTERFACE
  !! Holen eines Zeigers auf vektorielle Komponente "region" des Objekts "t_ipds"
  INTERFACE get_region_object
     MODULE PROCEDURE get_region_object_1
  END INTERFACE
  !! Holen eines Zeigers auf vektorielle Komponente "mespos" des Objekts "t_ipds"
  INTERFACE get_mespos_object
     MODULE PROCEDURE get_mespos_object_1
  END INTERFACE
  !! Holen eines Zeigers auf vektorielle Komponente "regphyset" des Objekts "t_ipds"
  INTERFACE get_regphyset_object
     MODULE PROCEDURE get_regphyset_object_1
  END INTERFACE
  !! Holen eines Zeigers auf vektorielle Komponente "dim" des Objekts "t_ipds"
  INTERFACE get_dim_object
     MODULE PROCEDURE get_dim_object_1
  END INTERFACE
  !! Holen eines Zeigers auf vektorielle Komponente "att" des Objekts "t_ipds"
  INTERFACE get_att_object
     MODULE PROCEDURE get_att_object_1
  END INTERFACE
  !! Holen eines Zeigers akuf vektorielle Komponente "var" des Objekts "t_ipds"
  INTERFACE get_var_object
     MODULE PROCEDURE get_var_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "quant" aus Object "t_ipds"
  INTERFACE get_quant_object
     MODULE PROCEDURE get_quant_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "xyz" aus Object "t_ipds"
  INTERFACE get_xyz_object
     MODULE PROCEDURE get_xyz_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ind" aus Object "t_ipds"
  INTERFACE get_ind_object
     MODULE PROCEDURE get_ind_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ele" aus Object "t_ipds"
  INTERFACE get_ele_object
     MODULE PROCEDURE get_ele_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "dope" aus Object "t_ipds"
  INTERFACE get_dope_object
     MODULE PROCEDURE get_dope_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "exch" aus Object "t_ipds"
  INTERFACE get_exch_object
     MODULE PROCEDURE get_exch_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "span" aus Object "t_ipds"
  INTERFACE get_span_object
     MODULE PROCEDURE get_span_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "stamp" aus Object "t_ipds"
  INTERFACE get_stamp_object
     MODULE PROCEDURE get_stamp_object_1
  END INTERFACE
  !
  !! Holen der Varianten-Namen fuer eine physikalische Groesse
  !! (identische Varianten-Namen werden eliminiert)
  INTERFACE get_var_name_object
     MODULE PROCEDURE get_var_name_object_0
  END INTERFACE
  !
  !! Setzen der Komponente "file" des Objekts "t_ipds" auf Benutzerwerte
  INTERFACE setup_file_object
     MODULE PROCEDURE setup_file_object_0
  END INTERFACE
  !! Setzen der Komponente "name" des Objekts "t_ipds" auf Benutzerwerte
  INTERFACE setup_name_object
     MODULE PROCEDURE setup_name_object_0
  END INTERFACE
  !! Setzen der skalaren Komponente "datetime" des Objekts "t_ipds" auf Benutzer-Werte
  INTERFACE setup_datetime_object
     MODULE PROCEDURE setup_datetime_object_0
  END INTERFACE
  !! Setzen der skalaren Komponente "physet" des Objekts "t_ipds" auf Benutzer-Werte
  INTERFACE setup_physet_object
     MODULE PROCEDURE setup_physet_object_0
  END INTERFACE
  !! Setzen der vektoriellen Komponente "region" des Objekts "t_ipds" auf Benutzer-Werte
  INTERFACE setup_region_object
     MODULE PROCEDURE setup_region_object_1
  END INTERFACE
  !! Setzen der vektoriellen Komponente "mespos" des Objekts "t_ipds" auf Benutzer-Werte
  INTERFACE setup_mespos_object
     MODULE PROCEDURE setup_mespos_object_1
  END INTERFACE
  !! Setzen der vektoriellen Komponente "regphyset" des Objekts "t_ipds" auf Benutzer-Werte
  INTERFACE setup_regphyset_object
     MODULE PROCEDURE setup_regphyset_object_1
  END INTERFACE
  !! Dimensionsbezeichner zu der Komponente "dim" des Objektes "t_ipds" hinzuf&uuml;gen
  INTERFACE add_dim_object
     MODULE PROCEDURE add_dim_object_0
     MODULE PROCEDURE add_dim_object_1
  END INTERFACE
  !! Attribute zu der Komponente "att" des Objektes "t_ipds" hinzuf&uuml;gen
  INTERFACE add_att_object
     MODULE PROCEDURE add_att_object_0
     MODULE PROCEDURE add_att_object_1
  END INTERFACE
  !! Attribute zu der Komponente "var" des Objektes "t_ipds" hinzuf&uuml;gen
  INTERFACE add_var_object
     MODULE PROCEDURE add_var_object_0
     MODULE PROCEDURE add_var_object_1
  END INTERFACE
  !! Setzen der Komponente "quant" in einem Objekt "t_ipds"
  INTERFACE setup_quant_object
     MODULE PROCEDURE setup_quant_object_1
  END INTERFACE
  !! Setzen der Komponente "xyz" in einem Objekt "t_ipds"
  INTERFACE setup_xyz_object
     MODULE PROCEDURE setup_xyz_object_1
  END INTERFACE
  !! Setzen der Komponente "xyz" in einem Objekt "t_ipds" <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  INTERFACE setup_xyz_object_ref
     MODULE PROCEDURE setup_xyz_object_ref_1
  END INTERFACE
  !! Setzen der Komponente "ind" in einem Objekt "t_ipds"
  INTERFACE setup_ind_object
     MODULE PROCEDURE setup_ind_object_1
  END INTERFACE
  !! Setzen der Komponente "ind" in einem Objekt "t_ipds" <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  INTERFACE setup_ind_object_ref
     MODULE PROCEDURE setup_ind_object_ref_1
  END INTERFACE
  !! Setzen der Komponente "ele" in einem Objekt "t_ipds"
  INTERFACE setup_ele_object
     MODULE PROCEDURE setup_ele_object_1
  END INTERFACE
  !! Setzen der Komponente "dope" in einem Objekt "t_ipds"
  INTERFACE setup_dope_object
     MODULE PROCEDURE setup_dope_object_1
  END INTERFACE
  !! Setzen der Komponente "exch" in einem Objekt "t_ipds"
  INTERFACE setup_exch_object
     MODULE PROCEDURE setup_exch_object_1
  END INTERFACE
  !! Setzen der Komponente "span" in einem Objekt "t_ipds"
  INTERFACE setup_span_object
     MODULE PROCEDURE setup_span_object_0
  END INTERFACE
  !! Setzen der Komponente "stamp" in einem Objekt "t_ipds"
  INTERFACE setup_stamp_object
     MODULE PROCEDURE setup_stamp_object_1
  END INTERFACE
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "datetime" (Skalar) &uuml;bereinstimmen
  INTERFACE target_datetime_object
     MODULE PROCEDURE target_datetime_object_0
  END INTERFACE
  !! Pr&uuml;fe ob Pointer f&uuml;r "physet" (Skalar) &uuml;bereinstimmen 
  INTERFACE target_physet_object
     MODULE PROCEDURE target_physet_object_0
  END INTERFACE
  !! Pr&uuml;fe ob Pointer f&uuml;r "region" (Feld) &uuml;bereinstimmen
  INTERFACE target_region_object
     MODULE PROCEDURE target_region_object_1
  END INTERFACE
  !! Pr&uuml;fe ob Pointer f&uuml;r "mespos" (Feld) &uuml;bereinstimmen
  INTERFACE target_mespos_object
     MODULE PROCEDURE target_mespos_object_1
  END INTERFACE
  !! Pr&uuml;fe ob Pointer f&uuml;r "regphyset" (Feld) &uuml;bereinstimmen
  INTERFACE target_regphyset_object
     MODULE PROCEDURE target_regphyset_object_1
  END INTERFACE
  !! Pr&uuml;fe ob Pointer f&uuml;r "dim" (Feld) &uuml;bereinstimmen 
  INTERFACE target_dim_object
     MODULE PROCEDURE target_dim_object_1
  END INTERFACE
  !! Pr&uuml;fe ob Pointer f&uuml;r "att" (Feld) &uuml;bereinstimmen 
  INTERFACE target_att_object
     MODULE PROCEDURE target_att_object_1
  END INTERFACE
  !! Pr&uuml;fe ob Pointer f&uuml;r "var" (Feld) &uuml;bereinstimmen 
  INTERFACE target_var_object
     MODULE PROCEDURE target_var_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "quant" in "t_ipds" &uuml;bereinstimmt
  INTERFACE target_quant_object
     MODULE PROCEDURE target_quant_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "xyz" in "t_ipds" &uuml;bereinstimmt
  INTERFACE target_xyz_object
     MODULE PROCEDURE target_xyz_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ind" in "t_ipds" &uuml;bereinstimmt
  INTERFACE target_ind_object
     MODULE PROCEDURE target_ind_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ele" in "t_ipds" &uuml;bereinstimmt
  INTERFACE target_ele_object
     MODULE PROCEDURE target_ele_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "dope" in "t_ipds" &uuml;bereinstimmt
  INTERFACE target_dope_object
     MODULE PROCEDURE target_dope_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "exch" in "t_ipds" &uuml;bereinstimmt
  INTERFACE target_exch_object
     MODULE PROCEDURE target_exch_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "span" in "t_ipds" &uuml;bereinstimmt
  INTERFACE target_span_object
     MODULE PROCEDURE target_span_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "stamp" in "t_ipds" &uuml;bereinstimmt
  INTERFACE target_stamp_object
     MODULE PROCEDURE target_stamp_object_1
  END INTERFACE
  !
  !! Pr&uuml;fen Korrektheit eines Objektes "t_ipds"
  INTERFACE ok_ipds_object
     MODULE PROCEDURE ok_ipds_object_0
  END INTERFACE
  !
  !! Pr&uuml;fen Korrektheit einer Datei-Variante f&uuml;r Objekt "t_ipds"
  INTERFACE ok_ipds_variant_no
     MODULE PROCEDURE ok_ipds_variant_no_0
  END INTERFACE
  !! Hole Id-Nummer einer Datei-Variante f&uuml;r Objekt "t_ipds"
  INTERFACE get_ipds_variant_no
     MODULE PROCEDURE get_ipds_variant_no_0
  END INTERFACE
  !
  !! Drucken des Inhalts eines Objektes "t_ipds"
  INTERFACE print_ipds_object
     MODULE PROCEDURE print_ipds_object_0
  END INTERFACE
  !! Drucken der globalen statischen Daten des Paketes "ipds"
  INTERFACE print_ipds_global
     MODULE PROCEDURE print_ipds_global_0
  END INTERFACE
  !! Drucken aller Fehlermeldungen des Paketes "ipds"
  INTERFACE print_ipds_errors
     MODULE PROCEDURE print_ipds_errors_0
  END INTERFACE
  !
  !! Sind Daten in der Komponente "datetime" abgelegt?
  INTERFACE associated_datetime_object
     MODULE PROCEDURE associated_datetime_object_0
  END INTERFACE
  !! Sind Daten in der Komponente "physet" abgelegt?
  INTERFACE associated_physet_object
     MODULE PROCEDURE associated_physet_object_0
  END INTERFACE
  !! Sind Daten in der Komponente "region" abgelegt?
  INTERFACE associated_region_object
     MODULE PROCEDURE associated_region_object_0
  END INTERFACE
  !! Sind Daten in der Komponente "mespos" abgelegt?
  INTERFACE associated_mespos_object
     MODULE PROCEDURE associated_mespos_object_0
  END INTERFACE
  !! Sind Daten in der Komponente "regphyset" abgelegt?
  INTERFACE associated_regphyset_object
     MODULE PROCEDURE associated_regphyset_object_0
  END INTERFACE
  !
  !! Umsetzen eines Textes in Gro&szlig;buchstaben
  INTERFACE get_uppercase_char
     MODULE PROCEDURE get_uppercase_char_0
  END INTERFACE get_uppercase_char
  !
  ! [B.5] oeffentlich zugaengliche Methoden
  !
  PUBLIC :: new_ipds_object
  PUBLIC :: kill_ipds_object
  !
  PUBLIC :: ok_work_object
  !
  PUBLIC :: get_ipds_list_object ! 
  PUBLIC :: get_name_object      ! 
  PUBLIC :: get_file_object      ! 
  PUBLIC :: get_datetime_object  ! 
  PUBLIC :: get_physet_object    ! 
  PUBLIC :: get_region_object    ! 
  PUBLIC :: get_mespos_object    ! 
  PUBLIC :: get_regphyset_object ! 
  PUBLIC :: get_dim_object       !
  PUBLIC :: get_att_object       !
  PUBLIC :: get_var_object       !
  PUBLIC :: get_quant_object     !
  PUBLIC :: get_xyz_object       !
  PUBLIC :: get_ind_object       !
  PUBLIC :: get_ele_object       !
  PUBLIC :: get_dope_object      !
  PUBLIC :: get_exch_object      !
  PUBLIC :: get_span_object      !
  PUBLIC :: get_stamp_object     !
  !
  PUBLIC :: get_var_name_object  ! 
  !
  PUBLIC :: setup_name_object      ! 
  PUBLIC :: setup_file_object      ! 
  PUBLIC :: setup_datetime_object  ! 
  PUBLIC :: setup_physet_object    ! 
  PUBLIC :: setup_region_object    ! 
  PUBLIC :: setup_mespos_object    ! 
  PUBLIC :: setup_regphyset_object ! 
  PUBLIC :: add_dim_object         !
  PUBLIC :: add_att_object         !
  PUBLIC :: add_var_object         !
  PUBLIC :: setup_quant_object     ! 
  PUBLIC :: setup_xyz_object       ! 
  PUBLIC :: setup_xyz_object_ref   ! 
  PUBLIC :: setup_ind_object_ref   ! 
  PUBLIC :: setup_ind_object       ! 
  PUBLIC :: setup_ele_object       ! 
  PUBLIC :: setup_dope_object      !
  PUBLIC :: setup_exch_object      ! 
  PUBLIC :: setup_span_object      ! 
  PUBLIC :: setup_stamp_object     ! 
  ! 
  PUBLIC :: target_datetime_object
  PUBLIC :: target_physet_object
  PUBLIC :: target_region_object
  PUBLIC :: target_mespos_object
  PUBLIC :: target_regphyset_object
  PUBLIC :: target_dim_object              ! 
  PUBLIC :: target_att_object              ! 
  PUBLIC :: target_var_object              ! 
  PUBLIC :: target_quant_object            ! 
  PUBLIC :: target_xyz_object              ! 
  PUBLIC :: target_ind_object              ! 
  PUBLIC :: target_ele_object              ! 
  PUBLIC :: target_dope_object             ! 
  PUBLIC :: target_exch_object             ! 
  PUBLIC :: target_span_object             ! 
  PUBLIC :: target_stamp_object            ! 
  !
  PUBLIC :: ok_ipds_object
  !
  PUBLIC :: ok_ipds_variant_no
  PUBLIC :: get_ipds_variant_no
  !
  PUBLIC :: print_ipds_object
  PUBLIC :: print_ipds_global
  PUBLIC :: print_ipds_errors
  !
  PUBLIC :: associated_datetime_object
  PUBLIC :: associated_physet_object
  PUBLIC :: associated_region_object
  PUBLIC :: associated_mespos_object
  PUBLIC :: associated_regphyset_object
  !
  PUBLIC :: get_uppercase_char
  !
  ! ---------------------------------------------------------------------
  ! [C] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] lokale Typdefinitionen
  ! [C.2] Konstantwerte (Parameter)
  !! Name des Moduls
  CHARACTER (LEN=11), PRIVATE, PARAMETER :: c_modname= 'm_ipds_data'
  !
  ! [C.3] Schnittstellen
  !
  !! Allokieren von Memory f&uuml;r die Komponente "datetime"
  INTERFACE alloc_ipds_datetime 
     MODULE PROCEDURE alloc_ipds_datetime_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "physet"
  INTERFACE alloc_ipds_physet 
     MODULE PROCEDURE alloc_ipds_physet_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "region"
  INTERFACE alloc_ipds_region 
     MODULE PROCEDURE alloc_ipds_region_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "mespos"
  INTERFACE alloc_ipds_mespos
     MODULE PROCEDURE alloc_ipds_mespos_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "regphyset"
  INTERFACE alloc_ipds_regphyset
     MODULE PROCEDURE alloc_ipds_regphyset_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die vektorielle Komponente "dim"
  INTERFACE alloc_ipds_dim 
     MODULE PROCEDURE alloc_ipds_dim_1         !
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die vektorielle Komponente "att"
  INTERFACE alloc_ipds_att
     MODULE PROCEDURE alloc_ipds_att_1         !
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die vektorielle Komponente "var"
  INTERFACE alloc_ipds_var
     MODULE PROCEDURE alloc_ipds_var_1         !
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "quant"
  INTERFACE alloc_ipds_quant
     MODULE PROCEDURE alloc_ipds_quant_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "xyz"
  INTERFACE alloc_ipds_xyz
     MODULE PROCEDURE alloc_ipds_xyz_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ind"
  INTERFACE alloc_ipds_ind
     MODULE PROCEDURE alloc_ipds_ind_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ele"
  INTERFACE alloc_ipds_ele
     MODULE PROCEDURE alloc_ipds_ele_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "dope"
  INTERFACE alloc_ipds_dope
     MODULE PROCEDURE alloc_ipds_dope_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "exch"
  INTERFACE alloc_ipds_exch
     MODULE PROCEDURE alloc_ipds_exch_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "span"
  INTERFACE alloc_ipds_span
     MODULE PROCEDURE alloc_ipds_span_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "stamp"
  INTERFACE alloc_ipds_stamp
     MODULE PROCEDURE alloc_ipds_stamp_1
  END INTERFACE
  !
  !! De-Allokieren von Memory f&uuml;r die Komponente "datetime"
  INTERFACE dealloc_ipds_datetime 
     MODULE PROCEDURE dealloc_ipds_datetime_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "physet"
  INTERFACE dealloc_ipds_physet 
     MODULE PROCEDURE dealloc_ipds_physet_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "region"
  INTERFACE dealloc_ipds_region 
     MODULE PROCEDURE dealloc_ipds_region_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "mespos"
  INTERFACE dealloc_ipds_mespos
     MODULE PROCEDURE dealloc_ipds_mespos_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "regphyset"
  INTERFACE dealloc_ipds_regphyset
     MODULE PROCEDURE dealloc_ipds_regphyset_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die vektorielle Komponente "dim"
  INTERFACE dealloc_ipds_dim 
     MODULE PROCEDURE dealloc_ipds_dim_d ! 
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die vektorielle Komponente "att"
  INTERFACE dealloc_ipds_att 
     MODULE PROCEDURE dealloc_ipds_att_d ! 
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die vektorielle Komponente "var"
  INTERFACE dealloc_ipds_var 
     MODULE PROCEDURE dealloc_ipds_var_d ! 
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "quant"
  INTERFACE dealloc_ipds_quant
     MODULE PROCEDURE dealloc_ipds_quant_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "xyz"
  INTERFACE dealloc_ipds_xyz
     MODULE PROCEDURE dealloc_ipds_xyz_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ind"
  INTERFACE dealloc_ipds_ind
     MODULE PROCEDURE dealloc_ipds_ind_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ele"
  INTERFACE dealloc_ipds_ele
     MODULE PROCEDURE dealloc_ipds_ele_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "dope"
  INTERFACE dealloc_ipds_dope
     MODULE PROCEDURE dealloc_ipds_dope_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "exch"
  INTERFACE dealloc_ipds_exch
     MODULE PROCEDURE dealloc_ipds_exch_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "span"
  INTERFACE dealloc_ipds_span
     MODULE PROCEDURE dealloc_ipds_span_d
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "stamp"
  INTERFACE dealloc_ipds_stamp
     MODULE PROCEDURE dealloc_ipds_stamp_d
  END INTERFACE
  !
  !! De-Allokieren von Memory f&uuml;r alle Komponenten
  INTERFACE dealloc_ipds_object
     MODULE PROCEDURE dealloc_ipds_object_d ! 
  END INTERFACE
  !
  !! Ermitteln einer neuen Objekt-Id
  INTERFACE get_ipds_new_id
     MODULE PROCEDURE get_ipds_new_id_0
  END INTERFACE
  !! Gibt es &uuml;berhaupt Objekte
  INTERFACE any_objects
     MODULE PROCEDURE any_objects_0
  END INTERFACE
  !
  !! Pr&uuml;fe Komponente "id" in Object "t_ipds"
  INTERFACE ok_ipds_id
     MODULE PROCEDURE ok_ipds_id_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "name" in Object "t_ipds"
  INTERFACE ok_ipds_name
     MODULE PROCEDURE ok_ipds_name_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "file" in Object "t_ipds"
  INTERFACE ok_ipds_file
     MODULE PROCEDURE ok_ipds_file_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "datetime" in Object "t_ipds"
  INTERFACE ok_ipds_datetime
     MODULE PROCEDURE ok_ipds_datetime_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "physet" in Object "t_ipds"
  INTERFACE ok_ipds_physet
     MODULE PROCEDURE ok_ipds_physet_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "region" in Object "t_ipds"
  INTERFACE ok_ipds_region
     MODULE PROCEDURE ok_ipds_region_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "mespos" in Object "t_ipds"
  INTERFACE ok_ipds_mespos
     MODULE PROCEDURE ok_ipds_mespos_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "regphyset" in Object "t_ipds"
  INTERFACE ok_ipds_regphyset
     MODULE PROCEDURE ok_ipds_regphyset_0
  END INTERFACE
  !! Pr&uuml;fe Komponenten "regphyset" und "region" in Object "t_ipds"
  INTERFACE ok_ipds_name_in_region
     MODULE PROCEDURE ok_ipds_name_in_region_0
  END INTERFACE
  !! Pr&uuml;fe Komponenten "regphyset" und "mespos" in Object "t_ipds"
  INTERFACE ok_ipds_name_in_mespos
     MODULE PROCEDURE ok_ipds_name_in_mespos_0
  END INTERFACE
  !! Pr&uuml;fe die Types aller physikalischen Groessen eines Gebiets
  INTERFACE ok_ipds_phyval_type
     MODULE PROCEDURE ok_ipds_phyval_type_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "dim" in Object "t_ipds"
  INTERFACE ok_ipds_dim
     MODULE PROCEDURE ok_ipds_dim_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "att" in Object "t_ipds"
  INTERFACE ok_ipds_att
     MODULE PROCEDURE ok_ipds_att_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "var" in Object "t_ipds"
  INTERFACE ok_ipds_var
     MODULE PROCEDURE ok_ipds_var_0
  END INTERFACE
  !! Pr&uuml;fe f&uuml;r Komponente "var" ob alle Dimensionen in "dim" enthalten sind
  INTERFACE ok_ipds_var_dim
     MODULE PROCEDURE ok_ipds_var_dim_0
  END INTERFACE
  !! Pr&uuml;fe f&uuml;r Komponente "att" ob alle Variablen in "var" enthalten sind
  INTERFACE ok_ipds_att_var
     MODULE PROCEDURE ok_ipds_att_var_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "quant" in Object "t_ipds"
  INTERFACE ok_ipds_quant
     MODULE PROCEDURE ok_ipds_quant_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "xyz" in Object "t_ipds"
  INTERFACE ok_ipds_xyz
     MODULE PROCEDURE ok_ipds_xyz_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "ind" in Object "t_ipds"
  INTERFACE ok_ipds_ind
     MODULE PROCEDURE ok_ipds_ind_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "ele" in Object "t_ipds"
  INTERFACE ok_ipds_ele
     MODULE PROCEDURE ok_ipds_ele_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "dope" in Object "t_ipds"
  INTERFACE ok_ipds_dope
     MODULE PROCEDURE ok_ipds_dope_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "exch" in Object "t_ipds"
  INTERFACE ok_ipds_exch
     MODULE PROCEDURE ok_ipds_exch_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "span" in Object "t_ipds"
  INTERFACE ok_ipds_span
     MODULE PROCEDURE ok_ipds_span_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "stamp" in Object "t_ipds"
  INTERFACE ok_ipds_stamp
     MODULE PROCEDURE ok_ipds_stamp_0
  END INTERFACE
  !
  !! Drucken des Inhalts der Komponente "datetime" aus Objekt "t_ipds"
  INTERFACE print_ipds_datetime
     MODULE PROCEDURE print_ipds_datetime_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "physet" aus Objekt "t_ipds"
  INTERFACE print_ipds_physet
     MODULE PROCEDURE print_ipds_physet_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "region" aus Objekt "t_ipds"
  INTERFACE print_ipds_region
     MODULE PROCEDURE print_ipds_region_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "mespos" aus Objekt "t_ipds"
  INTERFACE print_ipds_mespos
     MODULE PROCEDURE print_ipds_mespos_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "regphyset" aus Objekt "t_ipds"
  INTERFACE print_ipds_regphyset
     MODULE PROCEDURE print_ipds_regphyset_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "dim" aus Objekt "t_ipds"
  INTERFACE print_ipds_dim
     MODULE PROCEDURE print_ipds_dim_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "att" aus Objekt "t_ipds"
  INTERFACE print_ipds_att
     MODULE PROCEDURE print_ipds_att_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "var" aus Objekt "t_ipds"
  INTERFACE print_ipds_var
     MODULE PROCEDURE print_ipds_var_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "quant" aus Objekt "t_ipds"
  INTERFACE print_ipds_quant
     MODULE PROCEDURE print_ipds_quant_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "xyz" aus Objekt "t_ipds"
  INTERFACE print_ipds_xyz
     MODULE PROCEDURE print_ipds_xyz_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "ind" aus Objekt "t_ipds"
  INTERFACE print_ipds_ind
     MODULE PROCEDURE print_ipds_ind_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "ele" aus Objekt "t_ipds"
  INTERFACE print_ipds_ele
     MODULE PROCEDURE print_ipds_ele_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "dope" aus Objekt "t_ipds"
  INTERFACE print_ipds_dope
     MODULE PROCEDURE print_ipds_dope_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "exch" aus Objekt "t_ipds"
  INTERFACE print_ipds_exch
     MODULE PROCEDURE print_ipds_exch_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "span" aus Objekt "t_ipds"
  INTERFACE print_ipds_span
     MODULE PROCEDURE print_ipds_span_0
  END INTERFACE
  !! Drucken des Inhalts der Komponente "stamp" aus Objekt "t_ipds"
  INTERFACE print_ipds_stamp
     MODULE PROCEDURE print_ipds_stamp_0
  END INTERFACE
  !
  !! Initialisieren der Komponenten eines Objekts vom Typ "t_ipds_list"
  INTERFACE init_ipds_list_object
     MODULE PROCEDURE init_ipds_list_object_0
  END INTERFACE
  !! Initialisieren der Komponenten eines Objekts vom Typ "t_ipds"
  INTERFACE init_ipds_object
     MODULE PROCEDURE init_ipds_object_0
  END INTERFACE
  !
  !! Merge Variantennamen
  INTERFACE merge_var_name
     MODULE PROCEDURE merge_var_name_d
  END INTERFACE
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
  !! Allokieren/Initialisieren eines neuen (Package-) Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_ipds_object_0 ( id )
    !! Identifikationsnummer des Datenobjekts (Skalar) <BR>
    !! id = -1 : Allokieren/Initialisieren fehlgeschlagen
    INTEGER, INTENT(INOUT) :: id
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='new_ipds_object_0'
    !! Statusvariable
    INTEGER :: stat
    !! neues Package-Datenobjekt des Typs "t_ipds_list"
    TYPE (t_ipds_list) , POINTER :: this
    !! vorangehendes Package-Datenobjekt des Typs "t_ipds_list"
    TYPE (t_ipds_list) , POINTER :: prev
    !
    ALLOCATE ( this, STAT=stat )
    !
    IF ( stat /= 0 ) &
         CALL setup_error_act ( all_errors(:), 4001, c_upname, c_modname, stat )
    !
    IF ( no_error( ) ) THEN
       CALL init_ipds_list_object ( this )
       IF ( no_error( ) ) THEN
          NULLIFY ( prev )
          nofobjects     = nofobjects + 1
          id             = get_ipds_new_id ( )
          this%object%id = id
          IF ( nofobjects == 1 ) THEN ! allererstes Objekt
             first_list_object => this
          ELSE                        ! Anhaengen an die verkettete Liste
             prev              => first_list_object
             DO
                IF ( .NOT. ASSOCIATED( prev%next ) ) EXIT
                prev => prev%next
             END DO
             this%prev => prev
             prev%next => this
          END IF
          NULLIFY ( prev )
       END IF
    END IF
    !
  END SUBROUTINE new_ipds_object_0
  !
  !! De-Allokieren/De-Initialisieren eines (Package-) Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_ipds_object_0 ( id )
    !! Identifikationsnummer des Datenobjekts (Skalar)
    INTEGER , INTENT(IN) :: id
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='kill_ipds_object_0'
    !! aktuelles (Package-) Datenobjekt vom Typ "t_ipds_list"
    TYPE (t_ipds_list) , POINTER :: this
    !! vorangehendes Objekt (previous)
    TYPE (t_ipds_list) , POINTER :: prev
    !! nachfolgendes Objekt (next)
    TYPE (t_ipds_list) , POINTER :: next
    !! Hilfsvariable
    LOGICAL :: l_next, l_prev ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ! das zu entfernende Objekt "this" ermitteln
    this => get_ipds_list_object ( id )
    !
    IF ( no_error( ) ) THEN
       ! Vorgaenger von "this" ermitteln
       IF ( ASSOCIATED( this%prev ) ) THEN
          prev   => this%prev
       ELSE
          NULLIFY ( prev )
       END IF
       ! Nachfolger von "this" ermitteln
       IF ( ASSOCIATED( this%next ) ) THEN
          next => this%next
       ELSE
          NULLIFY ( next )
       END IF
       l_prev = ASSOCIATED( prev )
       l_next = ASSOCIATED( next )
       IF ( l_prev .AND. l_next ) THEN
          ! Vorgaenger und Nachfolger zu "this" vorhanden
          prev%next => next
          next%prev => prev
       ELSE IF ( .NOT. l_prev .AND. .NOT. l_next ) THEN
          ! "this" ist das letzte Objekt
          NULLIFY ( first_list_object )
       ELSE IF ( .NOT. l_prev .AND.       l_next ) THEN
          ! "this" hat keinen Vorgaenger mehr, aber noch einen Nachfolger
          NULLIFY( next%prev )
          first_list_object => next
       ELSE IF (       l_prev .AND. .NOT. l_next ) THEN
          ! "this" hat keinen Nachfolger mehr, aber noch einen Vorgaenger
          NULLIFY( prev%next )
       ENDIF
       ! falls (Arbeits-) Objekt
       IF ( ASSOCIATED( work_object ) ) THEN
          IF ( work_object%id == this%object%id ) NULLIFY( work_object )
       END IF
       ! dynamische Komponenten des Objektes "this" vernichten
       IF ( no_error( ) ) CALL dealloc_ipds_object ( this%object )
       DEALLOCATE ( this%object, STAT=stat )
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 5002, c_upname, c_modname, stat )
       IF ( no_error( ) ) nofobjects = nofobjects - 1
       NULLIFY ( prev, next )
       DEALLOCATE ( this, STAT=stat )
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 5001, c_upname, c_modname, stat )
    END IF
    !
  END SUBROUTINE kill_ipds_object_0
  !
  !! f&uuml;r eine Identifikationsnummer wird der Zeiger auf ein (Package-) Objekt zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_list_object_0 ( id ) &
       RESULT( this )
    !! Identifikationsnummer des gew&uuml;nschten (Package-) Datenobjekts
    INTEGER , INTENT(IN) :: id
    !! R&uuml;ckgabewert : Zeiger auf vorhandenes (Package-) Objekt mit Identifikationsnummer "id"
    TYPE (t_ipds_list) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=22), PARAMETER :: c_upname='get_ipds_list_object_0'
    !! Hilfs-Zeiger auf (Package-) Objekte
    TYPE (t_ipds_list) , POINTER :: l_this
    !
    NULLIFY ( this )
    !
    IF ( any_objects ( c_upname ) ) THEN
       NULLIFY ( this, l_this )
       l_this => first_list_object
       DO
          IF ( l_this%object%id == id ) this => l_this
          IF (       ASSOCIATED( this        ) ) EXIT
          IF ( .NOT. ASSOCIATED( l_this%next ) ) EXIT
          l_this => l_this%next
       END DO
       IF ( .NOT. ASSOCIATED( this ) ) THEN
          WRITE(*,*) ' *** keine (Package-) Datenobjekt "t_ipds_list" fuer ID = ',id
          CALL setup_error_act ( all_errors(:), 4, c_upname, c_modname )
       END IF
    END IF
    !
  END FUNCTION get_ipds_list_object_0
  !
  !! f&uuml;r einen Namen wird der Zeiger auf ein (Package-) Objekt zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_ipds_list_object_n0 ( name ) &
       RESULT( this )
    !! Name des gew&uuml;nschten (Package-) Datenobjekts
    CHARACTER (LEN=*) , INTENT(IN) :: name ! 
    !! R&uuml;gabewert : Zeiger auf vorhandenes (Package-) Objekt mit Name "name"
    TYPE (t_ipds_list) , POINTER :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=23), PARAMETER :: c_upname='get_ipds_list_object_n0'
    !! Hilfs-Zeiger auf (Package-) Objekte
    TYPE (t_ipds_list) , POINTER :: l_this
    !
    NULLIFY ( this )
    !
    IF ( any_objects ( c_upname ) ) THEN
       NULLIFY ( this, l_this )
       l_this => first_list_object
       DO
          IF ( TRIM( l_this%object%name ) == TRIM( name ) ) this => l_this
          IF (       ASSOCIATED( this        ) ) EXIT
          IF ( .NOT. ASSOCIATED( l_this%next ) ) EXIT
          l_this => l_this%next
       END DO
       IF ( .NOT. ASSOCIATED( this ) ) THEN
          WRITE(*,*) ' *** keine (Package-) Datenobjekt "t_ipds_list" fuer name = '//TRIM(name)
          CALL setup_error_act ( all_errors(:), 4, c_upname, c_modname )
       END IF
    END IF
    !
  END FUNCTION get_ipds_list_object_n0
  !
  !! Pr&uuml;fe ob ein Arbeitsobjekt vorhanden ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_work_object_0 ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "ok_work_object" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok
    !
    ok = ASSOCIATED( work_object )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 5, upname, c_modname )
    !
  END FUNCTION ok_work_object_0
  !
  !! Ermittle f&uuml;r das Objekt "this" die Datei-Varianten-Nummer <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_variant_no_0 ( this ) &
       RESULT( ivar )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Nummer der aktuellen Variante <BR>
    !! -1 : nicht vorhanden 
    INTEGER :: ivar ! 
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='get_ipds_variant_no_0'
    !! logisches Feld
    LOGICAL :: l_ok(4) ! 
    !! Z&auml;hler
    INTEGER :: i       ! 
    !
    ivar = -1
    i    = 0
    !
    DO
       i = i + 1
       IF ( i > c_max_variants .OR. ivar /= -1 .OR. any_error ( ) ) EXIT
       l_ok(1) = ( TRIM( get_file_form  ( this%file ) ) == TRIM( c_variants_form(i)   ) ) 
       l_ok(2) = ( TRIM( get_file_access( this%file ) ) == TRIM( c_variants_access(i) ) ) 
       l_ok(3) = ( TRIM( get_file_delim ( this%file ) ) == TRIM( c_variants_delim(i)  ) ) 
       l_ok(4) = ( TRIM( get_uppercase_char(get_file_type(this%file))) == &
                   TRIM( get_uppercase_char(c_variants_type(i))) ) 
       ivar    = MERGE ( i, -1, ALL( l_ok(:) ) )
    END DO
    !
  END FUNCTION get_ipds_variant_no_0
  !
  !! Hole Kopie von "name" aus Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_name_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this
    !! Ergebniswert: Komponente "name" aus Objekt
    CHARACTER (LEN=80) :: val
    !
    val = this%name
    !
  END FUNCTION get_name_object_0
  !
  !! Hole Kopie von "file" aus Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this
    !! Ergebniswert: Komponente "file" aus Objekt
    TYPE (t_file) :: val
    !
    CALL new_file( val )
    !
    IF ( no_error() ) val = this%file
    !
  END FUNCTION get_file_object_0
  !
  !! Hole Pointer auf "datetime" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_datetime_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Komponente "datetime" aus Objekt
    TYPE (t_datetime) , POINTER :: val ! 
    !
    IF ( associated_datetime_object( this ) ) THEN
       val => this%datetime
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_datetime_object_0
  !
  !! Hole Pointer auf "physet" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_physet_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Komponente "physet" aus Objekt
    TYPE (t_physet) , POINTER :: val ! 
    !
    IF ( associated_physet_object( this ) ) THEN
       val => this%physet
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_physet_object_0
  !
  !! Hole Pointer auf "region" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_region_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Feld "region" aus Objekt
    TYPE (t_region) , POINTER :: val(:) ! 
    !
    IF ( associated_region_object( this ) ) THEN
       val => this%region
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_region_object_1
  !
  !! Hole Pointer auf "mespos" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_mespos_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Feld "mespos" aus Objekt
    TYPE (t_mespos) , POINTER :: val(:) ! 
    !
    IF ( associated_mespos_object( this ) ) THEN
       val => this%mespos
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_mespos_object_1
  !
  !! Hole Pointer auf "regphyset" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_regphyset_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Feld "regphyset" aus Objekt
    TYPE (t_regphyset) , POINTER :: val(:) ! 
    !
    IF ( associated_regphyset_object( this ) ) THEN
       val => this%regphyset
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_regphyset_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "dim" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_dim_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Feld "dim" aus Objekt
    TYPE (t_dim)  , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%dim ) ) THEN
       val => this%dim
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dim_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "att" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_att_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Feld "att" aus Objekt
    TYPE (t_att)  , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       val => this%att
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_att_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "var" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_var_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Feld "var" aus Objekt
    TYPE (t_var)  , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%var ) ) THEN
       val => this%var
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_var_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "quant" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_quant_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds)      , POINTER :: this ! 
    !! Ergebniswert: Feld "quant" aus Objekt
    TYPE (t_omi_quant) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       val => this%quant
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_quant_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "xyz" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_xyz_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds)    , POINTER :: this ! 
    !! Ergebniswert: Feld "xyz" aus Objekt
    TYPE (t_omi_xyz) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       val => this%xyz
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_xyz_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "ind" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_ind_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds)    , POINTER :: this ! 
    !! Ergebniswert: Feld "ind" aus Objekt
    TYPE (t_omi_ind) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       val => this%ind
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ind_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "ele" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_ele_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds)    , POINTER :: this ! 
    !! Ergebniswert: Feld "ele" aus Objekt
    TYPE (t_omi_ele) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       val => this%ele
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ele_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "dope" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_dope_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds)     , POINTER :: this ! 
    !! Ergebniswert: Feld "dope" aus Objekt
    TYPE (t_omi_dope) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       val => this%dope
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dope_object_1
  !
  !! Hole Pointer auf vektorielle Komponente "exch" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_exch_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds)     , POINTER :: this ! 
    !! Ergebniswert: Feld "exch" aus Objekt
    TYPE (t_omi_exch) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       val => this%exch
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_exch_object_1
  !
  !! Hole Pointer auf skalare Komponente "span" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_span_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds)     , POINTER :: this ! 
    !! Ergebniswert: Feld "span" aus Objekt
    TYPE (t_omi_span) , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       val => this%span
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_span_object_0
  !
  !! Hole Pointer auf vektorielle Komponente "stamp" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_stamp_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_ipds)      , POINTER :: this ! 
    !! Ergebniswert: Feld "stamp" aus Objekt
    TYPE (t_omi_stamp) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       val => this%stamp
    ELSE
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_stamp_object_1
  !
  !! Holen der Varianten-Namen fuer eine physikalische Groesse
  !! (identische Varianten-Namen werden eliminiert) <BR>
  !! Das Ergebnisfeld wird hier mit Speicher belegt.
  SUBROUTINE get_var_name_object_0 ( this, phyval_name, nof_var_name, var_name )
    !
    !! aktuelles Objekt
    TYPE (t_ipds),      POINTER    :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*),  INTENT(IN) :: phyval_name
    !! Anzahl Varianten_Namen
    INTEGER                        :: nof_var_name
    !! Varianten_Namen
    CHARACTER (LEN=c_len_phyval_name), POINTER    :: var_name(:)
    !
    !! Name der Routine
    CHARACTER (LEN=21), PARAMETER :: c_upname='get_var_name_object_0'
    !! Anzahl der Varianten innerhalb der physet-Komponente
    INTEGER :: nof_physet_var_name
    !! Anzahl der Varianten aller Messpositionen
    INTEGER, POINTER :: nof_mespos_var_name(:)
    !! max. erlaubte Anzahl unterschiedlicher Variantennamen
    INTEGER, PARAMETER :: m_d_var_name=1000
    !! akt. Anzahl unterschiedlicher Variantennamen
    INTEGER            :: i_d_var_name
    !! Hilfsfeld mit allen unterschiedlichen Variantennamen
    CHARACTER (LEN=c_len_phyval_name) :: d_var_name( m_d_var_name )
    !! Hilfsfeld mit Variantennamen
    CHARACTER (LEN=c_len_phyval_name), ALLOCATABLE :: l_var_name(:)
    !! Zaehler fuer mespos-Komponente
    INTEGER :: i_mespos
    !! Statusvariable
    INTEGER :: stat
    !
    nof_var_name = 0
    NULLIFY( var_name )
    !
    ! [1] Anzahl Varianten fuer die physet- und die mespos-Komponenten holen
    !
    nof_physet_var_name = 0
    IF ( associated_physet_object( this ) ) &
         nof_physet_var_name = get_physet_nof_var_name( this%physet, phyval_name )
    !
    IF ( associated_mespos_object( this ) ) THEN
       !
       ALLOCATE ( nof_mespos_var_name( SIZE( this%mespos ) ) )
       !
       nof_mespos_var_name = get_mespos_nof_var_name( this%mespos, phyval_name )
       !
    END IF
    !
    ! [2] Anzahl und Bezeichnung der unterschiedlichen Variantennamen bestimmen
    !
    DO
       !
       ! [2.1] physet-Komponente
       !
       i_d_var_name = 0
       !
       IF ( nof_physet_var_name > 0 ) THEN
          !
          ALLOCATE( l_var_name( nof_physet_var_name ), STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21200, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          l_var_name = get_physet_var_name( this%physet, phyval_name )
          IF ( any_error() ) EXIT
          !
          CALL merge_var_name( l_var_name, i_d_var_name, d_var_name )
          IF ( any_error() ) EXIT
          !
          DEALLOCATE( l_var_name, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21210, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
       ENDIF
       !
       ! [2.2] mespos-Komponenten
       !
       IF ( any_error() ) EXIT
       !
       IF ( associated_mespos_object( this ) ) THEN
          !
          DO i_mespos = 1, SIZE( this%mespos )
             !
             IF ( nof_mespos_var_name( i_mespos ) > 0 ) THEN
                !
                ALLOCATE( l_var_name( nof_mespos_var_name( i_mespos ) ), STAT=stat )
                IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21200, c_upname, c_modname, stat )
                IF ( any_error() ) EXIT
                !
                l_var_name = get_mespos_var_name( this%mespos( i_mespos ), phyval_name )
                IF ( any_error() ) EXIT
                !
                CALL merge_var_name( l_var_name, i_d_var_name, d_var_name )
                IF ( any_error() ) EXIT
                !
                DEALLOCATE( l_var_name, STAT=stat )
                IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21210, c_upname, c_modname, stat )
                IF ( any_error() ) EXIT
                !
             ENDIF
             !
          END DO
          !
       END IF
       !
       ! [2.3] Ende Teil 2
       !
       EXIT
       !
    END DO
    !
    ! [3] Ausgabe-Array zusammenstellen
    !
    DO
       !
       IF ( any_error() ) EXIT
       !
       nof_var_name = MAX( 1, i_d_var_name )
       !
       ALLOCATE( var_name( nof_var_name ), STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21220, c_upname, c_modname, stat )
       IF ( any_error() ) EXIT
       !
       IF ( i_d_var_name > 0 ) THEN
          var_name = d_var_name( 1: i_d_var_name )
       ELSE
          var_name = phyval_name
       END IF
       !
       EXIT
       !
    END DO
    !
    ! [4] Aufraeumen
    !
    IF ( associated_mespos_object( this ) ) DEALLOCATE ( nof_mespos_var_name )
    !
  END SUBROUTINE get_var_name_object_0
  !
  !! Setzen der Komponente "name" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE setup_name_object_0 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN) :: val ! 
    !
    this%name = REPEAT( ' ', LEN(this%name) )
    this%name = val
    !
  END SUBROUTINE setup_name_object_0
  !
  !! Setzen der Komponente "file" des (Arbeits-) Objektes <BR>
  !! falls "file%unit" nicht gesetzt (negativ), dann wird 
  !! diese Komponente auf "asc_seq_lun" gesetzt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_file_object_0 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "file"
    TYPE (t_file)  , INTENT(IN) :: val ! 
    !
    this%file = val
    !
    IF ( get_file_unit ( val ) < 0 ) THEN
       CALL set_file_unit ( this%file, asc_seq_lun )
    END IF
    !
  END SUBROUTINE setup_file_object_0
  !
  !! Setzen der Komponente "datetime" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_datetime_object_0 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "datetime"
    CHARACTER (LEN=*) , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_datetime_object_0'
    !! Memo-Variable
    INTEGER :: memo_language ! 
    !
    IF ( no_error( ) ) memo_language = get_datetime_language ( )
    IF ( no_error( ) ) CALL setup_datetime_language ( 1 ) ! Deutsch
    IF ( no_error( ) ) CALL dealloc_ipds_datetime ( this )
    IF ( no_error( ) ) CALL alloc_ipds_datetime   ( this )
    IF ( no_error( ) ) this%datetime = string_to_datetime ( val )
    IF ( no_error( ) ) CALL setup_datetime_language ( memo_language ) 
    !
  END SUBROUTINE setup_datetime_object_0
  !
  !! Setzen der Komponente "physet" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_physet_object_0 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "physet"
    TYPE (t_physet) , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_physet_object_0'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_physet ( this )
    IF ( no_error( ) ) CALL alloc_ipds_physet   ( this )
    IF ( no_error( ) ) CALL assign_physet( this%physet, val )
    !
  END SUBROUTINE setup_physet_object_0
  !
  !! Setzen der Feld-Komponente "region" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_region_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "region"
    TYPE (t_region) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_region_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_region ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_region   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL assign_region( this%region, val )
    !
  END SUBROUTINE setup_region_object_1
  !
  !! Setzen der Feld-Komponente "mespos" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_mespos_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "mespos"
    TYPE (t_mespos) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_mespos_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_mespos ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_mespos   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL assign_mespos( this%mespos, val )
    !
  END SUBROUTINE setup_mespos_object_1
  !
  !! Setzen der Feld-Komponente "regphyset" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_regphyset_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "regphyset"
    TYPE (t_regphyset) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_regphyset_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_regphyset ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_regphyset   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL assign_regphyset( this%regphyset, val )
    !
  END SUBROUTINE setup_regphyset_object_1
  !
  !! Hinzuf&uuml;gen einer Dimensionsangabe zu der Feld-Komponente "dim" 
  !! des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_dim_object_0 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER    :: this ! 
    !! hinzuzuf&uuml;gender Wert (Skalar) zur Komponente "dim"
    TYPE (t_dim)     , INTENT(IN) :: val  ! 
    !
    CALL add_dim ( this%dim, val )
    !
  END SUBROUTINE add_dim_object_0
  !
  !! Hinzuf&uuml;gen mehrerer Dimensionsangaben zu der Feld-Komponente
  !! "dim" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_dim_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dim"
    TYPE (t_dim) , INTENT(IN) :: val(:) ! 
    !
    CALL add_dim ( this%dim, val(:) )
    !
  END SUBROUTINE add_dim_object_1
  !
  !! Hinzuf&uuml;gen eines Attributs zu der Feld-Komponente "att" 
  !! des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_object_0 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER    :: this ! 
    !! hinzuzuf&uuml;gender Wert (Skalar) zur Komponente "att"
    TYPE (t_att)     , INTENT(IN) :: val  ! 
    !
    CALL add_att ( this%att, val )
    !
  END SUBROUTINE add_att_object_0
  !
  !! Hinzuf&uuml;gen mehrerer Attribute zu der Feld-Komponente
  !! "att" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "att"
    TYPE (t_att) , INTENT(IN) :: val(:) ! 
    !
    CALL add_att ( this%att, val(:) )
    !
  END SUBROUTINE add_att_object_1
  !
  !! Hinzuf&uuml;gen einer Variablen zu der Feld-Komponente "var" 
  !! des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_var_object_0 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER    :: this ! 
    !! hinzuzuf&uuml;gender Wert (Skalar) zur Komponente "var"
    TYPE (t_var)     , INTENT(IN) :: val  ! 
    !
    CALL add_var ( this%var, val )
    !
  END SUBROUTINE add_var_object_0
  !
  !! Hinzuf&uuml;gen mehrerer Variablen zu der Feld-Komponente
  !! "var" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_var_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "var"
    TYPE (t_var) , INTENT(IN) :: val(:) ! 
    !
    CALL add_var ( this%var, val(:) )
    !
  END SUBROUTINE add_var_object_1
  !
  !! Setzen der Feld-Komponente "quant" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE setup_quant_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)      , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "quant"
    TYPE (t_omi_quant) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_quant_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_quant ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_quant   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_quant ( this%quant, val )
    !
  END SUBROUTINE setup_quant_object_1
  !
  !! Setzen der Feld-Komponente "xyz" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE setup_xyz_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)      , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xyz"
    TYPE (t_omi_xyz)   , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18) , PARAMETER :: c_upname='setup_xyz_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_xyz ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_xyz   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_xyz ( this%xyz, val )
    !
  END SUBROUTINE setup_xyz_object_1
  !
  !! Setzen der Feld-Komponente "xyz" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE setup_xyz_object_ref_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)    , POINTER :: this   !
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xyz"
    TYPE (t_omi_xyz) , POINTER :: val(:) ! 
    !
    this%xyz => val
    !
  END SUBROUTINE setup_xyz_object_ref_1
  !
  !! Setzen der Feld-Komponente "ind" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE setup_ind_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)      , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ind"
    TYPE (t_omi_ind)   , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18) , PARAMETER :: c_upname='setup_ind_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_ind ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_ind   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_ind ( this%ind, val )
    !
  END SUBROUTINE setup_ind_object_1
  !
  !! Setzen der Feld-Komponente "ind" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE setup_ind_object_ref_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)    , POINTER :: this   !
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ind"
    TYPE (t_omi_ind) , POINTER :: val(:) ! 
    !
    this%ind => val
    !
  END SUBROUTINE setup_ind_object_ref_1
  !
  !! Setzen der Feld-Komponente "ele" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE setup_ele_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)      , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ele"
    TYPE (t_omi_ele)   , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18) , PARAMETER :: c_upname='setup_ele_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_ele ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_ele   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_ele ( this%ele, val )
    !
  END SUBROUTINE setup_ele_object_1
  !
  !! Setzen der Feld-Komponente "dope" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE setup_dope_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)       , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dope"
    TYPE (t_omi_dope)   , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='setup_dope_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_dope ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_dope   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_dope ( this%dope, val )
    !
  END SUBROUTINE setup_dope_object_1
  !
  !! Setzen der Feld-Komponente "exch" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE setup_exch_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)       , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "exch"
    TYPE (t_omi_exch)   , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='setup_exch_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_exch ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_exch   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_exch ( this%exch, val )
    !
  END SUBROUTINE setup_exch_object_1
  !
  !! Setzen der skalaren Komponente "span" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE setup_span_object_0 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)       , POINTER    :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "span"
    TYPE (t_omi_span)   , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='setup_span_object_0'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_span ( this )
    IF ( no_error( ) ) CALL alloc_ipds_span   ( this )
    IF ( no_error( ) ) this%span = val
    !
  END SUBROUTINE setup_span_object_0
  !
  !! Setzen der Feld-Komponente "stamp" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE setup_stamp_object_1 ( this, val )
    !! Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)        , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "stamp"
    TYPE (t_omi_stamp)   , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20) , PARAMETER :: c_upname='setup_stamp_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_ipds_stamp ( this            )
    IF ( no_error( ) ) CALL alloc_ipds_stamp   ( this, SIZE(val) )
    IF ( no_error( ) ) this%stamp(:) = val(:)
    !
  END SUBROUTINE setup_stamp_object_1
  !
  ! -----------------------------------------------------------------------------
  ! >>> paketspezifische TARGET-Methoden <<< [ERR_NO = -22000 bis -22999]
  ! -----------------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "datetime" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_datetime_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! aktueller Pointer
    TYPE (t_datetime) , POINTER :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok  ! 
    !
    IF ( associated_datetime_object( this ) ) THEN
       ok = ASSOCIATED( val, this%datetime )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_datetime_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "physet" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_physet_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! aktueller Pointer
    TYPE (t_physet) , POINTER :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL               :: ok  ! 
    !
    IF ( associated_physet_object( this ) ) THEN
       ok = ASSOCIATED( val, this%physet )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_physet_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "region" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_region_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! aktueller Pointer
    TYPE (t_region) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL               :: ok     ! 
    !
    IF ( associated_region_object( this ) ) THEN
       ok = ASSOCIATED( val, this%region )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_region_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "mespos" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_mespos_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! aktueller Pointer
    TYPE (t_mespos) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL               :: ok     ! 
    !
    IF ( associated_mespos_object( this ) ) THEN
       ok = ASSOCIATED( val, this%mespos )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_mespos_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "regphyset" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_regphyset_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! aktueller Pointer
    TYPE (t_regphyset) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL               :: ok     ! 
    !
    IF ( associated_regphyset_object( this ) ) THEN
       ok = ASSOCIATED( val, this%regphyset )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_regphyset_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dim" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_dim_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! aktueller Pointer
    TYPE (t_dim)  , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL               :: ok     ! 
    !
    IF ( ASSOCIATED( this%dim ) ) THEN
       ok = ASSOCIATED( val, this%dim )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_dim_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "att" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_att_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! aktueller Pointer
    TYPE (t_att)  , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL               :: ok     ! 
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       ok = ASSOCIATED( val, this%att )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_att_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "var" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_var_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! aktueller Pointer
    TYPE (t_var)  , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL               :: ok     ! 
    !
    IF ( ASSOCIATED( this%var ) ) THEN
       ok = ASSOCIATED( val, this%var )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_var_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "quant" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_quant_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds)        , POINTER :: this   ! 
    !! aktueller Pointer
    TYPE (t_omi_quant)   , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL                    :: ok     ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       ok = ASSOCIATED( val, this%quant )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_quant_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xyz" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_xyz_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds)      , POINTER :: this   ! 
    !! aktueller Pointer
    TYPE (t_omi_xyz)   , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL                    :: ok     ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       ok = ASSOCIATED( val, this%xyz )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_xyz_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ind" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_ind_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds)      , POINTER :: this   ! 
    !! aktueller Pointer
    TYPE (t_omi_ind)   , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL                    :: ok     ! 
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       ok = ASSOCIATED( val, this%ind )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_ind_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ele" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_ele_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds)      , POINTER :: this   ! 
    !! aktueller Pointer
    TYPE (t_omi_ele)   , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL                    :: ok     ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       ok = ASSOCIATED( val, this%ele )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_ele_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dope" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_dope_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds)       , POINTER :: this   ! 
    !! aktueller Pointer
    TYPE (t_omi_dope)   , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL                    :: ok     ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       ok = ASSOCIATED( val, this%dope )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_dope_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "exch" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_exch_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds)       , POINTER :: this   ! 
    !! aktueller Pointer
    TYPE (t_omi_exch)   , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL                    :: ok     ! 
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       ok = ASSOCIATED( val, this%exch )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_exch_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "span" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_span_object_0 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds)     , POINTER :: this   ! 
    !! aktueller Pointer
    TYPE (t_omi_span) , POINTER :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL                    :: ok     ! 
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       ok = ASSOCIATED( val, this%span )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_span_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "stamp" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_stamp_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_ipds)       , POINTER :: this   ! 
    !! aktueller Pointer
    TYPE (t_omi_stamp)  , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL                    :: ok     ! 
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       ok = ASSOCIATED( val, this%stamp )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION target_stamp_object_1
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Package-Objekt vorliegt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_object_0 ( this ) &
       RESULT( ok )
    !! Objekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_ipds_object_0'
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(3+c_nofcomp+5) ! 
    !
    l_ok = .false.
    !
    l_ok( 1) = ok_ipds_id             ( this )
    l_ok( 2) = ok_ipds_name           ( this )
    l_ok( 3) = ok_ipds_file           ( this )
    !
    l_ok( 4) = ok_ipds_datetime       ( this )
    l_ok( 5) = ok_ipds_physet         ( this )
    l_ok( 6) = ok_ipds_region         ( this )
    l_ok( 7) = ok_ipds_mespos         ( this )
    l_ok( 8) = ok_ipds_regphyset      ( this )
    l_ok( 9) = ok_ipds_dim            ( this )
    l_ok(10) = ok_ipds_att            ( this )
    l_ok(11) = ok_ipds_var            ( this )
    IF ( ALL( l_ok( 9:11:2) ) ) l_ok(12) = ok_ipds_var_dim ( this )
    IF ( ALL( l_ok(10:11:1) ) ) l_ok(13) = ok_ipds_att_var ( this )
    l_ok(14) = ok_ipds_quant          ( this )
    l_ok(15) = ok_ipds_xyz            ( this )
    l_ok(16) = ok_ipds_ind            ( this )
    l_ok(17) = ok_ipds_ele            ( this )
    l_ok(18) = ok_ipds_dope           ( this )
    l_ok(19) = ok_ipds_exch           ( this )
    l_ok(20) = ok_ipds_span           ( this )
    l_ok(21) = ok_ipds_stamp          ( this )
    !
    l_ok(22) = ok_ipds_name_in_region ( this )
    l_ok(23) = ok_ipds_name_in_mespos ( this )
    l_ok(24) = ok_ipds_phyval_type    ( this )
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_ipds_object_0
  !
  !! Pr&uuml;fe ob eine implementierte Datei-Variante vorliegt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_variant_no_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebniswert: Erforderliche Datei-Variante ist implementiert <BR>
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='ok_ipds_variant_no_0'
    !
    ok = ( get_ipds_variant_no_0 ( this ) > 0 )
    !
    IF ( .NOT. ok ) THEN
       !$OMP critical
       CALL setup_error_act ( all_errors(:), -6001, c_upname, c_modname )
       CALL setup_error_act ( '<FortranFileType>'  , get_file_type  ( this%file ) )
       CALL setup_error_act ( '<FortranFileForm>'  , get_file_form  ( this%file ) )
       CALL setup_error_act ( '<FortranFileAccess>', get_file_access( this%file ) )
       CALL setup_error_act ( '<FortranFileDelim>' , get_file_delim ( this%file ) )
       !$OMP end critical
    END IF
    !
  END FUNCTION ok_ipds_variant_no_0
  !
  !! Drucken aller statischen Daten des Packages (ohne Daten der Package-Objekte) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_global_0 ( )
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_ipds_global_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hlervariable
    INTEGER :: i    ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) &
         initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, nofobjects, &
         ASSOCIATED( first_list_object ), ASSOCIATED( work_object ), &
         asc_seq_lun
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
    !
    DO i=1,c_max_variants
       IF ( any_error( ) ) EXIT
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, &
            c_variants_access(i), c_variants_form(i), & 
            c_variants_delim(i),  c_variants_type(i)
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
    END DO
    IF ( no_error( ) ) CALL print_ipds_errors ( )
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Packages io_ipds_ui  ',/ &
    '# ohne der in den Package-Objekten abgelegten Daten          ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#       initialised = ',L1,/ &
    '#            prn_op = ',L1,/ &
    '#            trc_op = ',L1,/ &
    '#           prn_lun = ',I5,/ &
    '#           trc_lun = ',I5,/ &
    '#            n_init = ',I5,/ &
    '#        nofobjects = ',I5,' (Anzahl der Package-Objekte)',/ &
    '# first_list_object = ',L1,' (T = associated)',/ &
    '#       work_object = ',L1,' (T = associated)',/ &
    '#       asc_seq_lun = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '# Ueberblick ueber die implementierten Dateivarianten ',/ &
    '# -- Nr -- -- Access -- --  Form  -- -- Delim -- -- Type ----------')
8001 FORMAT( '#',I6,5X,A10,3X,A11,2X,A10,3X,A )
8002 FORMAT( &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_ipds_global_0
  !
  !! Drucken aller (m&ouml;glichen) Fehler des Packages <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_errors_0 ( )
    !
    IF ( no_error( ) ) CALL print_error( all_errors(:) )
    !
  END SUBROUTINE print_ipds_errors_0
  !
  !! Drucke den Inhalt eines Package-Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE print_ipds_object_0 ( this )
    !! Objekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_ipds_object_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT= 8000, IOSTAT=stat ) this%id, TRIM( this%name )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
    IF ( no_error( ) ) CALL print_file           ( this%file )
    IF ( no_error( ) ) CALL print_ipds_datetime  ( this )
    IF ( no_error( ) ) CALL print_ipds_physet    ( this )
    IF ( no_error( ) ) CALL print_ipds_region    ( this )
    IF ( no_error( ) ) CALL print_ipds_mespos    ( this )
    IF ( no_error( ) ) CALL print_ipds_regphyset ( this )
    IF ( no_error( ) ) CALL print_ipds_dim       ( this )
    IF ( no_error( ) ) CALL print_ipds_att       ( this )
    IF ( no_error( ) ) CALL print_ipds_var       ( this )
    IF ( no_error( ) ) CALL print_ipds_quant     ( this )
    IF ( no_error( ) ) CALL print_ipds_xyz       ( this )
    IF ( no_error( ) ) CALL print_ipds_ind       ( this )
    IF ( no_error( ) ) CALL print_ipds_ele       ( this )
    IF ( no_error( ) ) CALL print_ipds_dope      ( this )
    IF ( no_error( ) ) CALL print_ipds_exch      ( this )
    IF ( no_error( ) ) CALL print_ipds_span      ( this )
    IF ( no_error( ) ) CALL print_ipds_stamp     ( this )
    !
    IF ( no_error( ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
    END IF
    !
8000 FORMAT('# Beginn Objekt t_ipds ------------------------------',/&
            '# Objekt-Identifikationsnummer = ',I5,/&
            '# beschreibender Name          = ',A,/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT('# Ende   Objekt t_ipds ------------------------------')
    !
  END SUBROUTINE print_ipds_object_0
  !
  !! Sind Daten in der Komponente "datetime" abgelegt? <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION associated_datetime_object_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Ist die Komponente "datetime" des Datenobjekts mit Daten verbunden?
    LOGICAL :: ok
    !! Name der Subroutine
    CHARACTER (LEN=28), PARAMETER  :: c_upname='associated_datetime_object_0'
    !
    ok = .FALSE.
    IF( ASSOCIATED( this%datetime ) ) ok = .TRUE.
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
    !
  END FUNCTION associated_datetime_object_0
  !
  !! Sind Daten in der Komponente "physet" abgelegt? <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION associated_physet_object_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Ist die Komponente "physet" des Datenobjekts mit Daten verbunden?
    LOGICAL :: ok
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='associated_physet_object_0'
    !
    ok = .FALSE.
    IF( ASSOCIATED( this%physet ) ) ok = .TRUE.
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6520, c_upname, c_modname )
    !
  END FUNCTION associated_physet_object_0
  !
  !! Sind Daten in der Komponente "region" abgelegt? <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION associated_region_object_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Ist die Komponente "region" des Datenobjekts mit Daten verbunden?
    LOGICAL :: ok
    !
    ok = .FALSE.
    IF( ASSOCIATED( this%region ) ) ok = .TRUE.
    !
  END FUNCTION associated_region_object_0
  !
  !! Sind Daten in der Komponente "mespos" abgelegt? <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION associated_mespos_object_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Ist die Komponente "mespos" des Datenobjekts mit Daten verbunden?
    LOGICAL :: ok
    !
    ok = .FALSE.
    IF( ASSOCIATED( this%mespos ) ) ok = .TRUE.
    !
  END FUNCTION associated_mespos_object_0
  !
  !! Sind Daten in der Komponente "regphyset" abgelegt? <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION associated_regphyset_object_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Ist die Komponente "regphyset" des Datenobjekts mit Daten verbunden?
    LOGICAL :: ok
    !
    ok = .FALSE.
    IF( ASSOCIATED( this%regphyset ) ) ok = .TRUE.
    !
  END FUNCTION associated_regphyset_object_0
  !
  !! Umwandeln eines Textes in Gro&szlig;buchstaben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_uppercase_char_0 ( in ) &
       RESULT( out )
    !! Zeichenfolge (Original)
    CHARACTER (LEN=*) , INTENT(IN) :: in  ! 
    !! R&uuml;ckgabewert : Zeichenfolge (konvertiert in Gro&szlig;buchstaben)
    CHARACTER (LEN=LEN(in))        :: out ! 
    !! Z&auml;hler 
    INTEGER :: i, ic ! 
    !
    out = in
    DO i=1,LEN(out)
       ic = IACHAR(in(i:i))
       IF ( ic >= 97 .AND. ic <= 122 ) out(i:i) = ACHAR(ic-32)
    END DO
    !
  END FUNCTION get_uppercase_char_0
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
  !! De-Allokieren aller Komponenten eines Objektes "t_ipds" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_object_d ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !
    IF ( no_error( ) ) CALL dealloc_ipds_datetime  ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_physet    ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_region    ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_mespos    ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_regphyset ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_dim       ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_att       ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_var       ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_quant     ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_xyz       ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_ind       ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_ele       ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_dope      ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_exch      ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_span      ( this )
    IF ( no_error( ) ) CALL dealloc_ipds_stamp     ( this )
    !
  END SUBROUTINE dealloc_ipds_object_d
  !
  !! f&uuml;r ein neues (Package-) Objekt wird automatisch eine neue Identifikationsnummer erzeugt <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION get_ipds_new_id_0 ( ) &
       RESULT( id )
    !! R&uuml;ckgabewert : Identifikationsnummer des neuen (Package-) Objektes
    INTEGER :: id ! 
    !! Hilfsvariable (Package-) Objekt
    TYPE (t_ipds_list) , POINTER :: this ! 
    !
    id = -1
    !
    IF ( .NOT. ASSOCIATED( first_list_object ) ) THEN ! das allererste Mal
       id = 1 
    ELSE ! falls schon Objekte vorhanden sind
       id   =  0
       this => first_list_object
       DO 
          id = MAX (id, this%object%id) + 1 
          IF ( .NOT. ASSOCIATED( this%next ) ) EXIT
          this => this%next
       END DO
       NULLIFY ( this )
    END IF
    !
  END FUNCTION get_ipds_new_id_0
  !
  !! Setzen der Fehlerbedingung 3 = keine Objekte "t_ipds" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION any_objects_0 ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "any_objects" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok
    !
    ok = ( nofobjects > 0 )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 3, upname, c_modname )
    !
  END FUNCTION any_objects_0
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Package-Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_id_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_ipds_id_0'
    !! Hilfsfehlertext
    CHARACTER (LEN=10) :: ctmp
    !
    ok = ( this%id > 0 )
    !
    IF ( .NOT. ok ) THEN
       !$OMP critical
       CALL setup_error_act ( all_errors(:), 6001, c_upname, c_modname )
       WRITE( ctmp, '(I10)') this%id
       CALL setup_error_act ( '<id>', ctmp )
       !$OMP end critical
    END IF
    !
  END FUNCTION ok_ipds_id_0
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Package-Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_name_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_ipds_name_0'
    !
    ok = ( this%name(1:9) /= 'UNDEFINED' )
    !
    IF ( .NOT. ok ) THEN
       !$OMP critical
       CALL setup_error_act ( all_errors(:), 6002, c_upname, c_modname )
       CALL setup_error_act ( '<name>', TRIM( this%name ) )
       !$OMP end critical
    ENDIF
    !
  END FUNCTION ok_ipds_name_0
  !
  !! Pr&uuml;fe, ob die Komponente "file" eines Package-Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_file_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_ipds_file_0'
    !! logisches Testfeld
    LOGICAL :: l_ok(2) ! 
    !
    l_ok(1) = ok_file ( this%file )
    l_ok(2) = ok_ipds_variant_no ( this )
    ok      = ALL( l_ok )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6003, c_upname, c_modname )
    ENDIF
    !
  END FUNCTION ok_ipds_file_0
  !
  !! Pr&uuml;fe, ob die Komponente "datetime" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_datetime_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18), PARAMETER :: c_upname='ok_ipds_datetime_0'
    !
    IF ( associated_datetime_object( this ) ) THEN
       ok = ok_datetime(this%datetime)
       IF ( .NOT. ok ) THEN
          !$OMP critical
          CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
          CALL setup_error_act ( '<datetime>', datetime_to_string( this%datetime ) )
          !$OMP end critical
       END IF
    END IF
    !
  END FUNCTION ok_ipds_datetime_0
  !
  !! Pr&uuml;fe, ob die Komponente "physet" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_physet_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_ipds_physet_0'
    !
    IF ( associated_physet_object( this ) ) THEN
       ok = ( ok_physet( this%physet ) )
       IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    END IF
    !
  END FUNCTION ok_ipds_physet_0
  !
  !! Pr&uuml;fe, ob die Komponente "region" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_region_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_ipds_region_0'
    !
    ok = .true.
    !
    IF ( associated_region_object( this ) ) THEN
       ok = ALL( ok_region( this%region ) )
       IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    END IF
    !
  END FUNCTION ok_ipds_region_0
  !
  !! Pr&uuml;fe, ob die Komponente "mespos" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_mespos_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_ipds_mespos_0'
    !
    ok = .true.
    !
    IF ( associated_mespos_object( this ) ) THEN
       ok = ALL( ok_mespos( this%mespos ) )
       IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    END IF
    !
  END FUNCTION ok_ipds_mespos_0
  !
  !! Pr&uuml;fe, ob die Komponente "regphyset" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_regphyset_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19), PARAMETER :: c_upname='ok_ipds_regphyset_0'
    !
    ok = .true.
    !
    IF ( associated_regphyset_object( this ) ) THEN
       ok = ALL( ok_regphyset( this%regphyset ) )
       IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
    END IF
    !
  END FUNCTION ok_ipds_regphyset_0
  !
  !! Pr&uuml;fe, ob die Komponente "dim" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_dim_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dim ) ) THEN
       ok = ALL ( ok_dim( this%dim(:) ) )
    ELSE
       ok = .true. 
    END IF
    !
  END FUNCTION ok_ipds_dim_0
  !
  !! Pr&uuml;fe, ob die Komponente "att" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_att_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       ok = ALL ( ok_att( this%att(:) ) )
    ELSE
       ok = .true. 
    END IF
    !
  END FUNCTION ok_ipds_att_0
  !
  !! Pr&uuml;fe, ob die Komponente "var" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_var_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%var ) ) THEN
       ok = ALL ( ok_var( this%var(:) ) )
    ELSE
       ok = .true. 
    END IF
    !
  END FUNCTION ok_ipds_var_0
  !
  !! Pr&uuml;fe, ob f&uuml;r die Komponente "var" eines Datenobjektes alle
  !! Dimensionen in "dim" vorhanden sind <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_var_dim_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%var ) .AND. ASSOCIATED( this%dim ) ) THEN
       ok = ALL ( ok_var( this%var(:), this%dim(:) ) )
    ELSE
       ok = .true. 
    END IF
    !
  END FUNCTION ok_ipds_var_dim_0
  !
  !! Pr&uuml;fe, ob f&uuml;r die Komponente "att" eines Datenobjektes alle
  !! Variablen in "var" vorhanden sind <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_att_var_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%att ) .AND. ASSOCIATED( this%var ) ) THEN
       ok = ALL ( ok_att( this%att(:), this%var(:) ) )
    ELSE
       ok = .true. 
    END IF
    !
  END FUNCTION ok_ipds_att_var_0
  !
  !! Pr&uuml;fe, ob die Komponente "quant" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_quant_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       ok = ALL( ok_omi_quant ( this%quant(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_ipds_quant_0
  !
  !! Pr&uuml;fe, ob die Komponente "xyz" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_xyz_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       ok = ALL( ok_omi_xyz ( this%xyz(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_ipds_xyz_0
  !
  !! Pr&uuml;fe, ob die Komponente "ind" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_ind_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       ok = ALL( ok_omi_ind ( this%ind(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_ipds_ind_0
  !
  !! Pr&uuml;fe, ob die Komponente "ele" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_ele_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       ok = ALL( ok_omi_ele ( this%ele(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_ipds_ele_0
  !
  !! Pr&uuml;fe, ob die Komponente "dope" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_dope_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       ok = ALL( ok_omi_dope ( this%dope(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_ipds_dope_0
  !
  !! Pr&uuml;fe, ob die Komponente "exch" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_exch_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       ok = ALL( ok_omi_exch ( this%exch(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_ipds_exch_0
  !
  !! Pr&uuml;fe, ob die Komponente "span" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_span_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       ok = ok_omi_span ( this%span )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_ipds_span_0
  !
  !! Pr&uuml;fe, ob die Komponente "stamp" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_stamp_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       ok = ALL( ok_omi_stamp ( this%stamp(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_ipds_stamp_0
  !
  !! Pr&uuml;fe, ob die Komponente "regphyset" einen Regionennamen enthaelt, der auch
  !! in der Komponente "region" enthalten ist <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_name_in_region_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24), PARAMETER :: c_upname='ok_ipds_name_in_region_0'
    !! regphyset-Zaehler
    INTEGER                       :: i_regphyset
    !! region-Zaehler
    INTEGER                       :: i_region
    !
    ok = .false.
    !
    IF( associated_regphyset_object( this ) ) THEN
       !
       IF ( associated_region_object( this ) ) THEN
          !
          i_regphyset = 0
          !
          DO
             !
             i_regphyset = i_regphyset + 1
             IF ( i_regphyset > SIZE( this%regphyset ) ) EXIT
             !
             ok = .false.
             !
             i_region = 0
             !
             DO
                !
                i_region = i_region + 1
                IF ( i_region > SIZE( this%region ) ) EXIT
                !
                IF ( get_regphyset_region_name( this%regphyset( i_regphyset ) ) &
                     == get_region_name( this%region( i_region ) ) ) ok = .true.
                !
                IF ( ok ) EXIT
                !
             END DO
             !
             IF ( .NOT. ok ) THEN
                !
                !$OMP critical
                CALL setup_error_act ( all_errors(:), 6110, c_upname, c_modname )
                CALL setup_error_act ( '<region-name>', get_regphyset_region_name( this%regphyset( i_regphyset ) ) )
                !$OMP end critical
                !
             END IF
             !
          END DO
          !
       ELSE
          !
          CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
          !
       END IF
       !
    ELSE
       !
       ok = .true.
       !
    END IF
    !
  END FUNCTION ok_ipds_name_in_region_0
  !
  !! Pr&uuml;fe, ob die Komponente "regphyset" einen Messpositionsnamen enthaelt, der auch
  !! in der Komponente "mespos" enthalten ist <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION ok_ipds_name_in_mespos_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24),   PARAMETER :: c_upname='ok_ipds_name_in_mespos_0'
    !! regphyset-Zaehler
    INTEGER                         :: i_regphyset
    !! Anzahl Messpositionsangaben in Komponente "regphyset"
    INTEGER                         :: n_mespos_name
    !! mespos_name-Zaehler
    INTEGER                         :: i_mespos_name
    !! mespos-Zaehler
    INTEGER                         :: i_mespos
    !! Namen der Messpositionen fuer ein regphyset-Objekt
    CHARACTER (LEN=80), ALLOCATABLE :: mespos_name(:)
    !! Statusvariable
    INTEGER                         :: stat
    !
    ok = .false.
    !
    IF( associated_regphyset_object( this ) ) THEN
       !
       IF ( associated_mespos_object( this ) ) THEN
          !
          i_regphyset = 0
          !
          DO
             !
             i_regphyset = i_regphyset + 1
             IF ( i_regphyset > SIZE( this%regphyset ) ) EXIT
             !
             n_mespos_name = get_regphyset_nof_mespos_name( this%regphyset( i_regphyset ) )
             !
             ALLOCATE( mespos_name( n_mespos_name ), STAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 6220, c_upname, c_modname, stat )
             !
             mespos_name = get_regphyset_mespos_name( this%regphyset( i_regphyset ) )
             !
             i_mespos_name = 0
             !
             DO
                !
                i_mespos_name = i_mespos_name + 1
                IF ( i_mespos_name > n_mespos_name ) EXIT
                !
                ok = .false.
                !
                i_mespos = 0
                !
                DO
                   !
                   i_mespos = i_mespos + 1
                   IF ( i_mespos > SIZE( this%mespos ) ) EXIT
                   !
                   IF ( mespos_name( i_mespos_name ) &
                        == get_mespos_name( this%mespos( i_mespos ) ) ) ok = .true.
                   !
                   IF ( ok ) EXIT
                   !
                END DO
                !
                IF ( .NOT. ok ) THEN
                   !
                   !$OMP critical
                   CALL setup_error_act ( all_errors(:), 6210, c_upname, c_modname )
                   CALL setup_error_act ( '<mespos-name>', mespos_name( i_mespos_name ) )
                   !$OMP end critical
                   !
                END IF
                !
             END DO
             !
             DEALLOCATE( mespos_name, STAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 6230, c_upname, c_modname, stat )
             !
          END DO
          !
       ELSE
          !
          CALL setup_error_act ( all_errors(:), 6200, c_upname, c_modname )
          !
       END IF
       !
    ELSE
       !
       ok = .true.
       !
    END IF
    !
  END FUNCTION ok_ipds_name_in_mespos_0
  !
  !! Pr&uuml;fe die Types aller physikalischen Groessen <BR>
  !! Fuer alle Definitionen von physikalischen Werten muss folgendes geprueft werden: <BR>
  !! Alle physikalischen Groessen muessen jede fuer sich den gleichen Typ aufweisen. <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_phyval_type_0 ( this ) &
       RESULT( ok )
    !
    USE m_ipds_phydef , ONLY : &
         ! globale Daten
         c_phy_name_de
    !
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=21),   PARAMETER :: c_upname='ok_ipds_phyval_type_0'
    !! Typ jeder physikalischen Groesse
    INTEGER              :: mem_type( SIZE( c_phy_name_de ) )
    !! c_phy_name_de-Zaehler
    INTEGER              :: i_phy
    !! mespos-Zaehler
    INTEGER              :: i_mespos
    !! Ein Satz phys. Groessen an einer Messstation
    TYPE (t_physet)      :: physet
    !! Varianten-Daten einer phys. Groesse
    TYPE (t_phyval)      :: phyval
    !! Anzahl Varianten einer phys. Groesse an einer Messstation
    INTEGER              :: n_phyval_type
    !! Typen aller Varianten einer phys. Groesse an einer Messstation
    INTEGER, ALLOCATABLE :: phyval_type(:)
    !! Statusvariable
    INTEGER              :: stat
    !
    ok       = .true.
    !
    mem_type = -1
    !
    ! [1] Untersuchung der Defaut-Werte
    !
    IF( associated_physet_object( this ) ) THEN
       !
       i_phy = 0
       !
       DO
          !
          i_phy = i_phy + 1
          IF( i_phy > SIZE( c_phy_name_de ) ) EXIT
          !
          ! [1.1] Mittels des Namens der akt. phys. Groesse (c_phy_name_de(i_phy)) aus
          !       dem Satz physikalischer Groessen die Daten der akt. phys. Groesse holen.
          !       Hinweis: Die get-Funktion erzeugt eine Fehlermeldung, wenn die akt.
          !                phys. Groesse nicht im Satz enthalten ist. Diese Fehlermeldung
          !                wird abgefangen.
          !
          phyval = get_physet_set( this%physet, c_phy_name_de( i_phy ) )
          !
          n_phyval_type = get_phyval_nof_type( phyval )
          !
          IF( n_phyval_type > 0 ) THEN
             !
             ALLOCATE( phyval_type( n_phyval_type ), STAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( &
                  all_errors(:), 6240, c_upname, c_modname, stat )
             !
             IF( no_error() ) THEN
                !
                phyval_type = get_phyval_type( phyval )
                !
                IF( mem_type( i_phy ) == -1 ) mem_type( i_phy ) = phyval_type(1)
                !
                ok = ( ALL( phyval_type == mem_type( i_phy ) ) )
                !
             END IF
             !
             DEALLOCATE( phyval_type, STAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( &
                  all_errors(:), 6250, c_upname, c_modname, stat )
             !
          END IF
          !
          CALL kill_phyval( phyval )
          !
          IF( .NOT. ok ) EXIT
          !
       END DO
       !
    END IF
    !
    IF ( .NOT. ok ) THEN
       !
       !$OMP critical
       !
       CALL setup_error_act ( all_errors(:), 6301, c_upname, c_modname )
       !
       CALL setup_error_act ( '<phy_name>', TRIM( c_phy_name_de( i_phy ) ) )
       !
       !$OMP end critical
       !
    ENDIF
    !
    ! [2] Untersuchung der Messstationen
    !
    IF( associated_mespos_object( this ) ) THEN
       !
       i_mespos = 0
       !
       DO
          !
          i_mespos = i_mespos + 1
          IF( i_mespos > SIZE( this%mespos ) ) EXIT
          !
          physet = get_mespos_physet( this%mespos( i_mespos ) )
          !
          i_phy = 0
          !
          DO
             !
             i_phy = i_phy + 1
             IF( i_phy > SIZE( c_phy_name_de ) ) EXIT
             !
             ! [2.1] Mittels des Namens der akt. phys. Groesse (c_phy_name_de(i_phy)) aus
             !       dem Satz physikalischer Groessen die Daten der akt. phys. Groesse holen.
             !       Hinweis: Die get-Funktion erzeugt eine Fehlermeldung, wenn die akt.
             !                phys. Groesse nicht im Satz enthalten ist. Diese Fehlermeldung
             !                wird abgefangen.
             !
             IF( no_error() ) THEN
                !
                phyval = get_physet_set( physet, c_phy_name_de( i_phy ) )
                !
                n_phyval_type = get_phyval_nof_type( phyval )
                !
                IF( n_phyval_type > 0 ) THEN
                   !
                   ALLOCATE( phyval_type( n_phyval_type ), STAT=stat )
                   IF ( stat /= 0 ) CALL setup_error_act ( &
                        all_errors(:), 6240, c_upname, c_modname, stat )
                   !
                   IF( no_error() ) THEN
                      !
                      phyval_type = get_phyval_type( phyval )
                      !
                      IF( mem_type( i_phy ) == -1 ) mem_type( i_phy ) = phyval_type(1)
                      !
                      ok = ( ALL( phyval_type == mem_type( i_phy ) ) )
                      !
                   END IF
                   !
                   DEALLOCATE( phyval_type, STAT=stat )
                   IF ( stat /= 0 ) CALL setup_error_act ( &
                        all_errors(:), 6250, c_upname, c_modname, stat )
                   !
                END IF
                !
                CALL kill_phyval( phyval )
                !
             END IF
             !
             IF( .NOT. ok ) EXIT
             !
          END DO
          !
          CALL kill_physet( physet )
          !
          IF( .NOT. ok ) EXIT
          !
       END DO
       !
    END IF
    !
    IF ( .NOT. ok ) THEN
       !
       !$OMP critical
       !
       CALL setup_error_act ( all_errors(:), 6302, c_upname, c_modname )
       !
       CALL setup_error_act ( '<phy_name>', TRIM( c_phy_name_de( i_phy ) ) )
       !
       !$OMP end critical
       !
    ENDIF
    !
  END FUNCTION ok_ipds_phyval_type_0
  !
  !! Drucke den Inhalt der Komponente "datetime" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE print_ipds_datetime_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=21), PARAMETER :: c_upname='print_ipds_datetime_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
    IF ( associated_datetime_object( this ) ) THEN
       CALL print_datetime( this%datetime )
    ELSE
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    END IF
    !
    WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT ('# Inhalt der Komponente datetime- - - - - - - - - - - - - - - ')
8001 FORMAT ('# <..no.datetime.data.associated..>')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_datetime_0
  !
  !! Drucke den Inhalt der Komponente "physet" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE print_ipds_physet_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_ipds_physet_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
    IF ( associated_physet_object( this ) ) THEN
       CALL print_physet( this%physet )
    ELSE
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    END IF
    !
    WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT ('# Inhalt der Komponente physet  - - - - - - - - - - - - - - - ')
8001 FORMAT ('# <..no.physet.data.associated..>')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_physet_0
  !
  !! Drucke den Inhalt der Komponente "region" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE print_ipds_region_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_ipds_region_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
    IF ( associated_region_object( this ) ) THEN
       CALL print_region( this%region )
    ELSE
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    END IF
    !
    WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT ('# Inhalt der Komponente region  - - - - - - - - - - - - - - - ')
8001 FORMAT ('# <..no.region.data.associated..>')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_region_0
  !
  !! Drucke den Inhalt der Komponente "mespos" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE print_ipds_mespos_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_ipds_mespos_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
    IF ( associated_mespos_object( this ) ) THEN
       CALL print_mespos( this%mespos )
    ELSE
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    END IF
    !
    WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT ('# Inhalt der Komponente mespos  - - - - - - - - - - - - - - - ')
8001 FORMAT ('# <..no.mespos.data.associated..>')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_mespos_0
  !
  !! Drucke den Inhalt der Komponente "regphyset" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE print_ipds_regphyset_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_ipds_regphyset_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
    !
    IF ( associated_regphyset_object( this ) ) THEN
       CALL print_regphyset( this%regphyset )
    ELSE
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
    END IF
    !
    WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
    !
8000 FORMAT ('# Inhalt der Komponente regphyset - - - - - - - - - - - - - - ')
8001 FORMAT ('# <..no.regphyset.data.associated..>')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_regphyset_0
  !
  !! Drucke den Inhalt der Komponente "dim" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_dim_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_ipds_dim_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%dim ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'dim(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_dim ( this%dim(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'dim(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dim - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_dim_0
  !
  !! Drucke den Inhalt der Komponente "att" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_att_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_ipds_att_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'att(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_att ( this%att(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'att(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente att - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_att_0
  !
  !! Drucke den Inhalt der Komponente "var" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_var_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_ipds_var_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%var ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'var(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_var ( this%var(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'var(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente var - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_var_0
  !
  !! Drucke den Inhalt der Komponente "quant" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_quant_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_ipds_quant_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'quant(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_quant ( this%quant(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'quant(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente quant - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_quant_0
  !
  !! Drucke den Inhalt der Komponente "xyz" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_xyz_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_ipds_xyz_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'xyz(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_xyz ( this%xyz(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'xyz(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente xyz - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_xyz_0
  !
  !! Drucke den Inhalt der Komponente "ind" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_ind_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_ipds_ind_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'ind(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_ind ( this%ind(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'ind(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ind - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_ind_0
  !
  !! Drucke den Inhalt der Komponente "ele" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_ele_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_ipds_ele_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'ele(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_ele ( this%ele(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'ele(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ele - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_ele_0
  !
  !! Drucke den Inhalt der Komponente "dope" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_dope_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_ipds_dope_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'dope(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_dope ( this%dope(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'dope(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dope  - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_dope_0
  !
  !! Drucke den Inhalt der Komponente "exch" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_exch_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_ipds_exch_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'exch(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_exch ( this%exch(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'exch(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente exch  - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_exch_0
  !
  !! Drucke den Inhalt der Komponente "span" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_span_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_ipds_span_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'span' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_span ( this%span )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'span' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente span  - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_span_0
  !
  !! Drucke den Inhalt der Komponente "stamp" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_ipds_stamp_0 ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_ipds_stamp_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'stamp(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_stamp ( this%stamp(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<omi-component>', 'stamp(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente stamp - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_ipds_stamp_0
  !
  !! Initialisieren eines Package-Objekts vom Typ "t_ipds_list" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_ipds_list_object_0 ( this )
    !! Package-Objekt des Typs "t_ipds_list"
    TYPE (t_ipds_list) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='init_ipds_list_object_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    NULLIFY ( this%object )
    NULLIFY ( this%prev   )
    NULLIFY ( this%next   )
    ALLOCATE ( this%object, STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 4002, c_upname, c_modname, stat )
    IF ( no_error( ) ) CALL init_ipds_object ( this%object )
    !
  END SUBROUTINE init_ipds_list_object_0
  !
  !! Initialisieren eines Package-Objekts vom Typ "t_ipds" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_ipds_object_0 ( this )
    !! Package-Objekt des Typs "t_ipds_list"
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='init_ipds_object_0'
    !
    ! initialisieren der statischen Komponenten
    this%id   = -1
    this%name = REPEAT( ' ', LEN( this%name ) )
    this%name = 'UNDEFINED'
    !
    CALL new_file ( this%file )
    !
    ! initialisieren der restlichen Komponenten (immer als Pointer)
    NULLIFY ( this%datetime  )
    NULLIFY ( this%physet    )
    NULLIFY ( this%region    )
    NULLIFY ( this%mespos    )
    NULLIFY ( this%regphyset )
    NULLIFY ( this%dim       )
    NULLIFY ( this%att       )
    NULLIFY ( this%var       )
    NULLIFY ( this%quant     )
    NULLIFY ( this%xyz       )
    NULLIFY ( this%ind       )
    NULLIFY ( this%ele       )
    NULLIFY ( this%dope      )
    NULLIFY ( this%exch      )
    NULLIFY ( this%span      )
    NULLIFY ( this%stamp     )
    !
  END SUBROUTINE init_ipds_object_0
  !
  !! Allokieren der Komponente "datetime" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_datetime_0 ( this )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this   ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='alloc_ipds_datetime_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%datetime, STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
    !
    IF ( no_error() ) CALL new_datetime( this%datetime )
    !
  END SUBROUTINE alloc_ipds_datetime_0
  !
  !! Allokieren der Komponente "physet" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_physet_0 ( this )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this   ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='alloc_ipds_physet_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%physet, STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 3020, c_upname, c_modname, stat )
    !
    IF ( no_error() ) CALL new_physet( this%physet )
    !
  END SUBROUTINE alloc_ipds_physet_0
  !
  !! Allokieren der Komponente "region" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_region_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this   ! 
    !! Dimension f&uuml;r Komponente "region"
    INTEGER                , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='alloc_ipds_region'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%region(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 3030, c_upname, c_modname, stat )
    !
    IF ( no_error() ) CALL new_region( this%region )
    !
  END SUBROUTINE alloc_ipds_region_1
  !
  !! Allokieren der Komponente "mespos" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_mespos_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this   ! 
    !! Dimension f&uuml;r Komponente "mespos"
    INTEGER                , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='alloc_ipds_mespos'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%mespos(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 3040, c_upname, c_modname, stat )
    !
    IF ( no_error() ) CALL new_mespos( this%mespos )
    !
  END SUBROUTINE alloc_ipds_mespos_1
  !
  !! Allokieren der Komponente "regphyset" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_regphyset_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds) , POINTER :: this   ! 
    !! Dimensionphyset f&uuml;r Komponente "regphyset"
    INTEGER                , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='alloc_ipds_regphyset'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%regphyset(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 3050, c_upname, c_modname, stat )
    !
    IF ( no_error() ) CALL new_regphyset( this%regphyset )
    !
  END SUBROUTINE alloc_ipds_regphyset_1
  !
  !! Allokieren der Komponente "quant" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_quant_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Dimension f&uuml;r Komponente "quant"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_ipds_quant_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%quant(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'quant(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_quant_1
  !
  !! Allokieren der Komponente "dim" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_dim_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Dimension f&uuml;r Komponente "dim"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_ipds_dim_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dim(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'dim(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_dim_1
  !
  !! Allokieren der Komponente "att" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_att_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Attension f&uuml;r Komponente "att"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_ipds_att_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%att(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'att(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_att_1
  !
  !! Allokieren der Komponente "var" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_var_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Varension f&uuml;r Komponente "var"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_ipds_var_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%var(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'var(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_var_1
  !
  !! Allokieren der Komponente "xyz" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_xyz_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Dimension f&uuml;r Komponente "xyz"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_ipds_xyz_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%xyz(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'xyz(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_xyz_1
  !
  !! Allokieren der Komponente "ind" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_ind_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Dimension f&uuml;r Komponente "ind"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_ipds_ind_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ind(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'ind(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_ind_1
  !
  !! Allokieren der Komponente "ele" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_ele_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Dimension f&uuml;r Komponente "ele"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_ipds_ele_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ele(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'ele(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_ele_1
  !
  !! Allokieren der Komponente "dope" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_dope_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Dimension f&uuml;r Komponente "dope"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_ipds_dope_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dope(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'dope(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_dope_1
  !
  !! Allokieren der Komponente "exch" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_exch_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Dimension f&uuml;r Komponente "exch"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_ipds_exch_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%exch(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'exch(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_exch_1
  !
  !! Allokieren der Komponente "span" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_span_0 ( this )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)     , POINTER      :: this
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_ipds_span_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%span, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'span' )
    END IF
    !
  END SUBROUTINE alloc_ipds_span_0
  !
  !! Allokieren der Komponente "stamp" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_ipds_stamp_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_ipds"
    TYPE (t_ipds)   , POINTER      :: this
    !! Dimension f&uuml;r Komponente "exch"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_ipds_stamp_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%stamp(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<omi-component>', 'stamp(:)' )
    END IF
    !
  END SUBROUTINE alloc_ipds_stamp_1
  !
  !! De-Allokieren der Komponente "datetime" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_datetime_d ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='dealloc_ipds_datetime_d'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%datetime ) ) THEN
       CALL kill_datetime( this%datetime )
       DEALLOCATE( this%datetime, STAT=stat )
       NULLIFY( this%datetime )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
    END IF
    !
  END SUBROUTINE dealloc_ipds_datetime_d
  !
  !! De-Allokieren der Komponente "physet" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_physet_d &
       ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='dealloc_ipds_physet_d'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%physet ) ) THEN
       CALL kill_physet( this%physet )
       DEALLOCATE( this%physet, STAT=stat )
       NULLIFY( this%physet )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5020, c_upname, c_modname, stat )
    END IF
    !
  END SUBROUTINE dealloc_ipds_physet_d
  !
  !! De-Allokieren der Komponente "region" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_region_d &
       ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='dealloc_ipds_region_d'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%region ) ) THEN
       CALL kill_region( this%region )
       DEALLOCATE( this%region, STAT=stat )
       NULLIFY( this%region )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5030, c_upname, c_modname, stat )
    END IF
    !
  END SUBROUTINE dealloc_ipds_region_d
  !
  !! De-Allokieren der Komponente "mespos" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_mespos_d &
       ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='dealloc_ipds_mespos_d'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%mespos ) ) THEN
       CALL kill_mespos( this%mespos )
       DEALLOCATE( this%mespos, STAT=stat )
       NULLIFY( this%mespos )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5040, c_upname, c_modname, stat )
    END IF
    !
  END SUBROUTINE dealloc_ipds_mespos_d
  !
  !! De-Allokieren der Komponente "regphyset" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_regphyset_d &
       ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='dealloc_ipds_regphyset_d'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%regphyset ) ) THEN
       CALL kill_regphyset( this%regphyset )
       DEALLOCATE( this%regphyset, STAT=stat )
       NULLIFY( this%regphyset )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5050, c_upname, c_modname, stat )
    END IF
    !
  END SUBROUTINE dealloc_ipds_regphyset_d
  !
  !! De-Allokieren der Komponente "dim" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_dim_d ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_ipds_dim_d'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%dim ) ) THEN
       CALL kill_dim ( this%dim )
       DEALLOCATE( this%dim, STAT=stat )
       NULLIFY( this%dim )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'dim(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_dim_d
  !
  !! De-Allokieren der Komponente "att" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_att_d ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_ipds_att_d'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       CALL kill_att ( this%att )
       DEALLOCATE( this%att, STAT=stat )
       NULLIFY( this%att )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'att(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_att_d
  !
  !! De-Allokieren der Komponente "var" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_var_d ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_ipds_var_d'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%var ) ) THEN
       CALL kill_var ( this%var )
       DEALLOCATE( this%var, STAT=stat )
       NULLIFY( this%var )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'var(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_var_d
  !
  !! De-Allokieren der Komponente "quant" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_quant_d ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_ipds_quant_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       CALL kill_omi_quant ( this%quant )
       DEALLOCATE( this%quant, STAT=stat )
       NULLIFY( this%quant )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'quant(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_quant_d
  !
  !! De-Allokieren der Komponente "xyz" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_xyz_d ( this )
    !! Datenobjekt
    TYPE (t_ipds)     , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_ipds_xyz_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       CALL kill_omi_xyz ( this%xyz )
       DEALLOCATE( this%xyz, STAT=stat )
       NULLIFY( this%xyz )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'xyz(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_xyz_d
  !
  !! De-Allokieren der Komponente "ind" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_ind_d ( this )
    !! Datenobjekt
    TYPE (t_ipds)     , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_ipds_ind_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       CALL kill_omi_ind ( this%ind )
       DEALLOCATE( this%ind, STAT=stat )
       NULLIFY( this%ind )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'ind(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_ind_d
  !
  !! De-Allokieren der Komponente "ele" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_ele_d ( this )
    !! Datenobjekt
    TYPE (t_ipds)     , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_ipds_ele_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       CALL kill_omi_ele ( this%ele )
       DEALLOCATE( this%ele, STAT=stat )
       NULLIFY( this%ele )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'ele(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_ele_d
  !
  !! De-Allokieren der Komponente "dope" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_dope_d ( this )
    !! Datenobjekt
    TYPE (t_ipds)     , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_ipds_dope_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       CALL kill_omi_dope ( this%dope )
       DEALLOCATE( this%dope, STAT=stat )
       NULLIFY( this%dope )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'dope(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_dope_d
  !
  !! De-Allokieren der Komponente "exch" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_exch_d ( this )
    !! Datenobjekt
    TYPE (t_ipds)     , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_ipds_exch_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       CALL kill_omi_exch ( this%exch )
       DEALLOCATE( this%exch, STAT=stat )
       NULLIFY( this%exch )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'exch(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_exch_d
  !
  !! De-Allokieren der Komponente "span" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_span_d ( this )
    !! Datenobjekt
    TYPE (t_ipds)     , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_ipds_span_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       CALL kill_omi_span ( this%span )
       DEALLOCATE( this%span, STAT=stat )
       NULLIFY( this%span )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'span' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_span_d
  !
  !! De-Allokieren der Komponente "stamp" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_ipds_stamp_d ( this )
    !! Datenobjekt
    TYPE (t_ipds)     , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_ipds_stamp_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       CALL kill_omi_stamp ( this%stamp )
       DEALLOCATE( this%stamp, STAT=stat )
       NULLIFY( this%stamp )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<omi-component>', 'stamp(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ipds_stamp_d
  !
  !! Merge Variantennamen <BR>
  !! Nur unterschiedliche Variantennamen werden beruecksichtigt
  SUBROUTINE merge_var_name_d ( l_var_name, i_d_var_name, d_var_name )
    !
    !! (Zusaetzliche) Variantennamen
    CHARACTER (LEN=*), INTENT(IN)    :: l_var_name(:)
    !! (Gesamt-)Anzahl Variantennamen
    INTEGER,           INTENT(INOUT) :: i_d_var_name
    !! Liste aller Variantennamen
    CHARACTER (LEN=*), INTENT(INOUT) :: d_var_name(:)
    !
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='merge_var_name_d'
    !! Zaehler fuer Variantennamen
    INTEGER :: i_var_name
    !! Zaehler
    INTEGER :: i
    !! Variantenname neu?
    LOGICAL :: neu_var_name
    !! Suchstring im Fehlertext
    CHARACTER (LEN=11) :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=10) :: cr
    !
    i_var_name   = 0
    !
    DO
       !
       i_var_name = i_var_name + 1
       IF( i_var_name > SIZE( l_var_name ) ) EXIT
       !
       i = 0
       neu_var_name = .TRUE.
       !
       DO
          !
          i = i + 1
          IF( i > i_d_var_name ) EXIT
          !
          IF( TRIM( l_var_name( i_var_name ) ) == TRIM( d_var_name( i ) ) ) neu_var_name = .FALSE.
          !
          IF( .NOT. neu_var_name ) EXIT
          !
       END DO
       !
       IF( neu_var_name ) THEN
          !
          i_d_var_name = i_d_var_name + 1
          !
          IF( i_d_var_name > SIZE( d_var_name ) ) THEN
             !
             !$OMP critical
             !
             CALL setup_error_act ( all_errors(:), 21300, c_upname, c_modname )
             !
             cs = '<arraysize>'
             WRITE( cr, '(I10)') SIZE( d_var_name )
             CALL setup_error_act ( cs, cr )
             !
             !$OMP end critical
             !
          END IF
          !
          IF( no_error() ) d_var_name( i_d_var_name ) = l_var_name( i_var_name )
          !
       END IF
       !
    END DO
    !
  END SUBROUTINE merge_var_name_d
  !
END MODULE m_ipds_data
! TailOfPackageModule ------------------------------------------------------
