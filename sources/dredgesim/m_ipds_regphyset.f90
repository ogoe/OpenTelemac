! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Definition und Methoden fuer den Datentyp "t_regphyset"</h2>
!! @author Jens J&uuml;rges
!! @version 3.2 vom 26.03 07, Quellcode: mod_m_ipds_regphyset.f90
!! <HR>
!! definition and methods for data type "t_regphyset"                <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-08-09 : J. Juerges : Original
!  01.02 : 2003-01-10 : J. Juerges : Entwicklungsgeschichte nicht fuer f90doc
!  01.03 : 2003-01-10 : J. Juerges : Lizenzangabe durch Platzhalter ersetzt
!  01.04 : 2003-01-10 : J. Juerges : keine Beschreibungen der Interfaces dort, wo sie PUBLIC gemacht werden
!  01.05 : 2003-02-06 : J. Juerges : Lizenzangabe aus WRITE-Anweisung entfernt
!  01.06 : 2003-02-10 : J. Juerges : Fehlerdeklaration fuer Mehrprozessor-Betrieb fit gemacht
!  01.07 : 2003-09-22 : J. Juerges : Merkmal "mespos_maxdist" mit allen notwendigen Methoden
!  02.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-09-01 : J. Juerges : Korrekte Initialisierung fuer prn_op und trc_op
!  02.03 : 2007-03-02 : G. Lang    : ok_regphyset_1 - FM falls ein Region_Name mehrfach in regphyset(:) auftritt
!  03.01 : 2007-03-13 : G. Lang    : neue Hauptversionsnummer 3
!  03.02 : 2007-03-26 : G. Lang    : FM 6015 deaktiviert da nicht sinnvoll
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Definition des Datentyps "t_regphyset" fuer das Paket "ipds";
!!   <LI> Der Datentyp speichert die Daten eines "regional_values"-Blocks
!!        einer Datei des Typs "ipds";
!!   <LI> Elementare Methoden auf Daten des Typs "t_regphyset";
!!   <LI> Ein direkter Zugriff auf diese Daten/Methoden von anderen Paketen 
!!        aus ist nicht zul&auml;ssig.
!! </OL>
!! <HR>
!
MODULE m_ipds_regphyset
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] BASIS-Modul mit Fehler-Typ und -Routinen 
  !
  USE b_error, ONLY :       &
       ! Typdefinitionen
       t_error,             &
       ! Routinen
       init_error,          &
       clear_error,         &
       no_error,            &
       any_error,           &
       new_error,           &
       kill_error,          &
       print_error,         &
       setup_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
  !
  !  [A.2.1] PAKET-Modul mit Region
  !
  USE m_ipds_region, ONLY : &
       ! Daten
       c_len_region_name
  !
  !  [A.2.2] PAKET-Modul mit Sampling Point
  !
  USE m_ipds_mespos, ONLY : &
       ! Daten
       c_len_mespos_name
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
  ! [C.0] Konstantwerte zur Definition von Stringlaengen
  !
  !! max. L&auml;nge der Komponente "interpol_name" in "t_region"
  INTEGER , PUBLIC, PARAMETER :: c_len_regphyset_interpol_name=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Definition von regional abweichenden physikalischen Werten
  !! (Alle Infos eines "regional_values"-Blocks)                          <BR>
  !! region_name    : Bezeichnung der zugehoerigen Region                 <BR>
  !! region_inside  : Region liegt inner-/ausserhalb der Grenzkoordinaten <BR>
  !! region_zmin    : Hoehenuntergrenze                                   <BR>
  !! region_zmax    : Hoehenobergrenze                                    <BR>
  !! mespos_name    : Bezeichnung(en) der zugehoerigen Messstation(en)    <BR>
  !! mespos_maxdist : Max. Abstand zwischen einer Messstation und einem   <BR>
  !!                  Punkt p, wenn diese Messstation fuer die            <BR>
  !!                  Interpolation ihrer Daten auf Punkt p herangezogen  <BR>
  !!                  werden soll                                         <BR>
  !! interpol_name  : Bezeichnung der Interpolationsmethode
  TYPE , PUBLIC :: t_regphyset
     PRIVATE
     CHARACTER (LEN=c_len_region_name)             :: region_name    ! 
     LOGICAL                                       :: region_inside  ! 
     REAL                                          :: region_zmin    ! 
     REAL                                          :: region_zmax    ! 
     CHARACTER (LEN=c_len_mespos_name), POINTER    :: mespos_name(:) ! 
     REAL                                          :: mespos_maxdist ! 
     CHARACTER (LEN=c_len_regphyset_interpol_name) :: interpol_name  ! 
  END TYPE t_regphyset
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
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
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE init_regphyset
     MODULE PROCEDURE init_regphyset_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE clear_regphyset
     MODULE PROCEDURE clear_regphyset_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_regphyset_prn_lun
     MODULE PROCEDURE setup_regphyset_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_regphyset_trc_lun
     MODULE PROCEDURE setup_regphyset_trc_lun_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_regphyset" (Skalar, 1D-Array)
  INTERFACE new_regphyset
     MODULE PROCEDURE new_regphyset_0  ! Version fuer Skalar
     MODULE PROCEDURE new_regphyset_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_regphyset" (Skalar, 1D-Array)
  INTERFACE kill_regphyset
     MODULE PROCEDURE kill_regphyset_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_regphyset_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_regphyset" (Skalar, 1D-Array)
  INTERFACE ok_regphyset
     MODULE PROCEDURE ok_regphyset_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_regphyset_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Datenobjekten "t_regphyset" (Skalar, 1D-Array)
  INTERFACE print_regphyset
     MODULE PROCEDURE print_regphyset_0 ! Version fuer Skalar
     MODULE PROCEDURE print_regphyset_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten
  INTERFACE print_regphyset_static
     MODULE PROCEDURE print_regphyset_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls
  INTERFACE print_regphyset_all_errors
     MODULE PROCEDURE print_regphyset_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "region_name" in "t_regphyset" auf Benutzerwert
  INTERFACE set_regphyset_region_name
     MODULE PROCEDURE set_regphyset_region_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_regphyset_region_name_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "region_inside" in "t_regphyset" auf Benutzerwert
  INTERFACE set_regphyset_region_inside
     MODULE PROCEDURE set_regphyset_region_inside_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_regphyset_region_inside_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "region_zmin" in "t_regphyset" auf Benutzerwert
  INTERFACE set_regphyset_region_zmin
     MODULE PROCEDURE set_regphyset_region_zmin_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_regphyset_region_zmin_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "region_zmax" in "t_regphyset" auf Benutzerwert
  INTERFACE set_regphyset_region_zmax
     MODULE PROCEDURE set_regphyset_region_zmax_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_regphyset_region_zmax_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "mespos_name" in "t_regphyset" auf Benutzerwert
  INTERFACE set_regphyset_mespos_name
     MODULE PROCEDURE set_regphyset_mespos_name_0_1 ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_regphyset_mespos_name_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "mespos_maxdist" in "t_regphyset" auf Benutzerwert
  INTERFACE set_regphyset_mespos_maxdist
     MODULE PROCEDURE set_regphyset_mespos_xdist_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_regphyset_mespos_xdist_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "interpol_name" in "t_regphyset" auf Benutzerwert
  INTERFACE set_regphyset_interpol_name
     MODULE PROCEDURE set_regphyset_interpol_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_regphyset_interpol_name_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Hole Komponente "region_name" aus "t_regphyset"
  INTERFACE get_regphyset_region_name
     MODULE PROCEDURE get_regphyset_region_name_0_0 ! Skalar
     MODULE PROCEDURE get_regphyset_region_name_1_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "region_inside" aus "t_regphyset"
  INTERFACE get_regphyset_region_inside
     MODULE PROCEDURE get_regphyset_region_inside_0_0 ! Skalar
     MODULE PROCEDURE get_regphyset_region_inside_1_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "region_zmin" aus "t_regphyset"
  INTERFACE get_regphyset_region_zmin
     MODULE PROCEDURE get_regphyset_region_zmin_0_0 ! Skalar
     MODULE PROCEDURE get_regphyset_region_zmin_1_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "region_zmax" aus "t_regphyset"
  INTERFACE get_regphyset_region_zmax
     MODULE PROCEDURE get_regphyset_region_zmax_0_0 ! Skalar
     MODULE PROCEDURE get_regphyset_region_zmax_1_1 ! Vektor
  END INTERFACE
  !! Hole Anzahl "mespos_name" aus "t_regphyset"
  INTERFACE get_regphyset_nof_mespos_name
     MODULE PROCEDURE get_regphyset_nof_mespos_name_0 ! Skalar
     MODULE PROCEDURE get_regphyset_nof_mespos_name_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "mespos_name" aus "t_regphyset"
  INTERFACE get_regphyset_mespos_name
     MODULE PROCEDURE get_regphyset_mespos_name_0_1 ! Skalar
  END INTERFACE
  !! Hole Komponente "mespos_maxdist" aus "t_regphyset"
  INTERFACE get_regphyset_mespos_maxdist
     MODULE PROCEDURE get_regphyset_mespos_xdist_0_0 ! Skalar
     MODULE PROCEDURE get_regphyset_mespos_xdist_1_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "interpol_name" aus "t_regphyset"
  INTERFACE get_regphyset_interpol_name
     MODULE PROCEDURE get_regphyset_interpol_name_0_0 ! Skalar
     MODULE PROCEDURE get_regphyset_interpol_name_1_1 ! Vektor
  END INTERFACE
  !
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  INTERFACE assign_regphyset
     module procedure assign_regphyset_0_0
     module procedure assign_regphyset_1_0
     module procedure assign_regphyset_1_1
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  ! [C.5] Zuweisungen
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
  !! Pr&uuml;fung zweier Datenobjekte "t_regphyset" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_regphyset_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_regphyset_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_regphyset_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_regphyset_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_regphyset" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_regphyset_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_regphyset_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_regphyset_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_regphyset_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_regphyset
  PUBLIC :: clear_regphyset
  PUBLIC :: setup_regphyset_prn_lun
  PUBLIC :: setup_regphyset_trc_lun
  PUBLIC :: new_regphyset
  PUBLIC :: kill_regphyset
  PUBLIC :: ok_regphyset
  PUBLIC :: print_regphyset
  PUBLIC :: print_regphyset_static
  PUBLIC :: print_regphyset_all_errors
  PUBLIC :: set_regphyset_region_name
  PUBLIC :: set_regphyset_region_inside
  PUBLIC :: set_regphyset_region_zmin
  PUBLIC :: set_regphyset_region_zmax
  PUBLIC :: set_regphyset_mespos_name
  PUBLIC :: set_regphyset_mespos_maxdist
  PUBLIC :: set_regphyset_interpol_name
  PUBLIC :: get_regphyset_region_name
  PUBLIC :: get_regphyset_region_inside
  PUBLIC :: get_regphyset_region_zmin
  PUBLIC :: get_regphyset_region_zmax
  PUBLIC :: get_regphyset_nof_mespos_name
  PUBLIC :: get_regphyset_mespos_name
  PUBLIC :: get_regphyset_mespos_maxdist
  PUBLIC :: get_regphyset_interpol_name
  PUBLIC :: OPERATOR(==)
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: assign_regphyset
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=16), PARAMETER :: c_modname      = 'm_ipds_regphyset'
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1
  !! Anzahl der Datenkomponenten des Typs t_regphyset
  INTEGER           , PARAMETER :: c_nofcomp      = 7
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                     , SAVE :: initialised = .false.
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                     , SAVE :: prn_op      = c_op
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                     , SAVE :: trc_op      = c_op
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                     , SAVE :: prn_lun     = c_lun
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                     , SAVE :: trc_lun     = c_lun
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                     , SAVE :: n_init      = 0
  !
  ! [D.4] Schnittstellen
  !
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
  SUBROUTINE init_regphyset_d ( )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_ds > 0) THEN
       	  WRITE(*,*) ' '
          WRITE(*,*) ' "m_ipds_regphyset" version 3.2 of 26.03 07'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_regphyset_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       prn_op  = c_op
       trc_lun = c_lun
       trc_op  = c_op
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_regphyset_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_regphyset_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_regphyset_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       prn_op  = c_op
       trc_lun = c_lun
       trc_op  = c_op
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_regphyset_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_regphyset_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_regphyset_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_regphyset_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_regphyset_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_regphyset_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_regphyset_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_regphyset_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='new_regphyset_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%region_name    = REPEAT( ' ', LEN(this%region_name) )
       this%region_inside  = .TRUE.
       this%region_zmin    = HUGE( this%region_zmin )
       this%region_zmax    = HUGE( this%region_zmax )
       this%mespos_maxdist = HUGE( this%mespos_maxdist )
       this%interpol_name  = REPEAT( ' ', LEN(this%interpol_name) )
       NULLIFY ( this%mespos_name )
    END IF
    !
  END SUBROUTINE new_regphyset_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_regphyset_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='new_regphyset_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_regphyset_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_regphyset_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_regphyset_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='kill_regphyset_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL dealloc_regphyset_mespos_name( this )
       IF ( no_error( ) ) CALL new_regphyset_0 ( this )
    END IF
    !
  END SUBROUTINE kill_regphyset_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_regphyset_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='kill_regphyset_1' 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_regphyset_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_regphyset_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_regphyset_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp+1)
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_regphyset_region_name(    this )
       l_ok(2)  = ok_regphyset_region_inside(  this )
       l_ok(3)  = ok_regphyset_region_zmin(    this )
       l_ok(4)  = ok_regphyset_region_zmax(    this )
       l_ok(5)  = ok_regphyset_mespos_name(    this )
       l_ok(6)  = ok_regphyset_mespos_maxdist( this )
       l_ok(7)  = ok_regphyset_interpol_name(  this )
       l_ok(8)  = ok_regphyset_region_zmin_zmax( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_regphyset_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_regphyset_1' 
    !! Z&auml;hler      
    CHARACTER (LEN=10) :: l_char ! 
    INTEGER            :: i, j   ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_regphyset_0 ( this(i) )
       END DO
!>GL>       ! Pruefe nunmehr die Verschiedenheit der Komponente "region_name" --
!>GL>       DO i=1,SIZE(this)
!>GL>          DO j=i+1,SIZE(this)
!>GL>             IF ( this(i)%region_name == this(j)%region_name ) THEN
!>GL>                CALL setup_error_act ( all_errors, 6015, c_upname, c_modname )
!>GL>                WRITE(l_char,'(I10)') i ; CALL setup_error_act ( '<index1>', l_char )
!>GL>                WRITE(l_char,'(I10)') j ; CALL setup_error_act ( '<index2>', l_char )
!>GL>                CALL setup_error_act ( '<name1>', TRIM(this(i)%region_name) )
!>GL>                CALL setup_error_act ( '<name2>', TRIM(this(j)%region_name) )
!>GL>             END IF
!>GL>          END DO
!>GL>       END DO
    END IF
    !
  END FUNCTION ok_regphyset_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_regphyset_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_regphyset_region_name(    this )
       IF ( no_error( ) ) CALL print_regphyset_region_inside(  this )
       IF ( no_error( ) ) CALL print_regphyset_region_zmin(    this )
       IF ( no_error( ) ) CALL print_regphyset_region_zmax(    this )
       IF ( no_error( ) ) CALL print_regphyset_mespos_name(    this )
       IF ( no_error( ) ) CALL print_regphyset_mespos_maxdist( this )
       IF ( no_error( ) ) CALL print_regphyset_interpol_name(  this )
       !
       IF ( no_error( ) ) THEN
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8001,     & 
                 IOSTAT  = stat )
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
          !
       END IF
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT('# Beginn Objekt t_regphyset ----------------------------------')
8001 FORMAT('# Ende   Objekt t_regphyset ----------------------------------')
    !
  END SUBROUTINE print_regphyset_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_regphyset_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     & 
                 IOSTAT  = stat ) i
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          IF ( no_error( ) ) CALL print_regphyset_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_regphyset_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_static_d ( )
    !! Name der Function
    CHARACTER (LEN=24), PARAMETER :: c_upname='print_regphyset_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_regphyset_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls m_ipds_regphyset       ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '# initialised = ',L1,/ &
    '#      prn_op = ',L1,/ &
    '#      trc_op = ',L1,/ &
    '#     prn_lun = ',I5,/ &
    '#     trc_lun = ',I5,/ &
    '#      n_init = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_regphyset_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=28), PARAMETER :: c_upname='print_regphyset_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_regphyset_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "region_name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_region_name_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "region_name"
    CHARACTER (LEN=*)  , INTENT(IN)    :: val  ! 
    !
    this%region_name = REPEAT( ' ', LEN(this%region_name) )
    this%region_name = val(1:MIN(LEN(val),LEN(this%region_name)))
    !
  END SUBROUTINE set_regphyset_region_name_0_0
  !
  !! weise der Komponente "region_name" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_region_name_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "region_name"
    CHARACTER (LEN=*)     , INTENT(IN)    :: val     ! 
    !
    this%region_name = val(1:MIN(LEN(val),LEN(this(1)%region_name)))
    !
  END SUBROUTINE set_regphyset_region_name_1_0
  !
  !! weise der Komponente "region_inside" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_region_inside_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset), INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "region_inside"
    LOGICAL           , INTENT(IN)    :: val  ! 
    !
    this%region_inside = val
    !
  END SUBROUTINE set_regphyset_region_inside_0_0
  !
  !! weise der Komponente "region_inside" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_region_inside_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset), INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "region_inside"
    LOGICAL           , INTENT(IN)    :: val     ! 
    !
    this%region_inside = val
    !
  END SUBROUTINE set_regphyset_region_inside_1_0
  !
  !! weise der Komponente "region_zmin" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_region_zmin_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset), INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "region_zmin"
    REAL              , INTENT(IN)    :: val  ! 
    !
    this%region_zmin = val
    !
  END SUBROUTINE set_regphyset_region_zmin_0_0
  !
  !! weise der Komponente "region_zmin" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_region_zmin_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset), INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "region_zmin"
    REAL              , INTENT(IN)    :: val     ! 
    !
    this%region_zmin = val
    !
  END SUBROUTINE set_regphyset_region_zmin_1_0
  !
  !! weise der Komponente "region_zmax" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_region_zmax_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset), INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "region_zmax"
    REAL              , INTENT(IN)    :: val  ! 
    !
    this%region_zmax = val
    !
  END SUBROUTINE set_regphyset_region_zmax_0_0
  !
  !! weise der Komponente "region_zmax" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_region_zmax_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset), INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "region_zmax"
    REAL              , INTENT(IN)    :: val     ! 
    !
    this%region_zmax = val
    !
  END SUBROUTINE set_regphyset_region_zmax_1_0
  !
  !! weise der dynamischen Komponente "mespos_name" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_regphyset_mespos_name_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset), INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "mespos_name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=29), PARAMETER :: c_upname='set_regphyset_mespos_name_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_regphyset_mespos_name ( this            )
       IF ( no_error( ) ) CALL alloc_regphyset_mespos_name   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_regphyset_mespos_name    ( this            )
       IF ( no_error( ) ) this%mespos_name(:) = val(:)(1:MIN(LEN(val(1)),LEN(this%mespos_name(1))))
    END IF
    !
  END SUBROUTINE set_regphyset_mespos_name_0_1
  !
  !! weise der dynamischen Komponente "mespos_name" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_regphyset_mespos_name_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset), INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "mespos_name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:)  ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_regphyset_mespos_name_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_regphyset_mespos_name_1_1
  !
  !! weise der Komponente "mespos_maxdist" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_mespos_xdist_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset), INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "mespos_maxdist"
    REAL              , INTENT(IN)    :: val  ! 
    !
    this%mespos_maxdist = val
    !
  END SUBROUTINE set_regphyset_mespos_xdist_0_0
  !
  !! weise der Komponente "mespos_maxdist" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_mespos_xdist_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset), INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "mespos_maxdist"
    REAL              , INTENT(IN)    :: val     ! 
    !
    this%mespos_maxdist = val
    !
  END SUBROUTINE set_regphyset_mespos_xdist_1_0
  !
  !! weise der Komponente "interpol_name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_interpol_name_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset), INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "interpol_name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%interpol_name = REPEAT( ' ', LEN(this%interpol_name) )
    this%interpol_name = val
    !
  END SUBROUTINE set_regphyset_interpol_name_0_0
  !
  !! weise der Komponente "interpol_name" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_regphyset_interpol_name_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset), INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "interpol_name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_regphyset_interpol_name_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_regphyset_interpol_name_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "region_name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_region_name_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "region_name" (Skalar)
    CHARACTER (LEN=c_len_region_name) :: val  ! 
    !
    val = this%region_name
    !
  END FUNCTION get_regphyset_region_name_0_0
  !
  !! hole die Komponente "region_name" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_region_name_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "region_name"
    CHARACTER (LEN=c_len_region_name) :: val(SIZE(this))  ! 
    !
    val = this%region_name
    !
  END FUNCTION get_regphyset_region_name_1_1
  !
  !! hole die Komponente "region_inside" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_region_inside_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "region_inside" (Skalar)
    LOGICAL :: val  ! 
    !
    val = this%region_inside
    !
  END FUNCTION get_regphyset_region_inside_0_0
  !
  !! hole die Komponente "region_inside" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_region_inside_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "region_inside"
    LOGICAL :: val(SIZE(this))  ! 
    !
    val = this%region_inside
    !
  END FUNCTION get_regphyset_region_inside_1_1
  !
  !! hole die Komponente "region_zmin" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_region_zmin_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "region_zmin" (Skalar)
    REAL :: val  ! 
    !
    val = this%region_zmin
    !
  END FUNCTION get_regphyset_region_zmin_0_0
  !
  !! hole die Komponente "region_zmin" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_region_zmin_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "region_zmin"
    REAL :: val(SIZE(this))  ! 
    !
    val = this%region_zmin
    !
  END FUNCTION get_regphyset_region_zmin_1_1
  !
  !! hole die Komponente "region_zmax" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_region_zmax_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "region_zmax" (Skalar)
    REAL :: val  ! 
    !
    val = this%region_zmax
    !
  END FUNCTION get_regphyset_region_zmax_0_0
  !
  !! hole die Komponente "region_zmax" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_region_zmax_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "region_zmax"
    REAL :: val(SIZE(this))  ! 
    !
    val = this%region_zmax
    !
  END FUNCTION get_regphyset_region_zmax_1_1
  !
  !! hole die Anzahl der Messstationsnamen in "mespos_name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_nof_mespos_name_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this
    !! R&uuml;ckgabewert Anzahl "mespos_name"
    INTEGER                          :: val
    !
    IF( ASSOCIATED( this%mespos_name ) ) THEN
       val = SIZE(this%mespos_name)
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_regphyset_nof_mespos_name_0
  !
  !! hole die Anzahl der Messstationsnamen in "mespos_name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_nof_mespos_name_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this(:)
    !! R&uuml;ckgabewert Anzahl "mespos_name"
    INTEGER                          :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       val(i) = get_regphyset_nof_mespos_name_0( this(i) )
    END DO
    !
  END FUNCTION get_regphyset_nof_mespos_name_1
  !
  !! hole die dynamische Feld-Komponente "mespos_name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_mespos_name_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "mespos_name" (Vektor)
    CHARACTER (LEN=c_len_mespos_name) :: val(SIZE(this%mespos_name)) ! 
    !
    val = this%mespos_name
    !
  END FUNCTION get_regphyset_mespos_name_0_1
  !
  !! hole die Komponente "mespos_maxdist" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_mespos_xdist_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "mespos_maxdist" (Skalar)
    REAL :: val  ! 
    !
    val = this%mespos_maxdist
    !
  END FUNCTION get_regphyset_mespos_xdist_0_0
  !
  !! hole die Komponente "mespos_maxdist" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_mespos_xdist_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "mespos_maxdist"
    REAL :: val(SIZE(this))  ! 
    !
    val = this%mespos_maxdist
    !
  END FUNCTION get_regphyset_mespos_xdist_1_1
  !
  !! hole die Komponente "interpol_name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_interpol_name_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "interpol_name" (Skalar)
    CHARACTER (LEN=c_len_regphyset_interpol_name) :: val  ! 
    !
    val = this%interpol_name
    !
  END FUNCTION get_regphyset_interpol_name_0_0
  !
  !! hole die Komponente "interpol_name" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_regphyset_interpol_name_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_regphyset) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "interpol_name"
    CHARACTER (LEN=c_len_regphyset_interpol_name) :: val(SIZE(this))  ! 
    !
    val = this%interpol_name
    !
  END FUNCTION get_regphyset_interpol_name_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = eq_regphyset_region_name    ( this1, this2 )
    l_ok(2)  = eq_regphyset_region_inside  ( this1, this2 )
    l_ok(3)  = eq_regphyset_region_zmin    ( this1, this2 )
    l_ok(4)  = eq_regphyset_region_zmax    ( this1, this2 )
    l_ok(5)  = eq_regphyset_mespos_name    ( this1, this2 )
    l_ok(6)  = eq_regphyset_mespos_maxdist ( this1, this2 )
    l_ok(7)  = eq_regphyset_interpol_name  ( this1, this2 )
    ok       = ALL( l_ok )
    !
  END FUNCTION eq_regphyset_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_regphyset_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_regphyset_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_regphyset_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_regphyset_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_regphyset_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_regphyset_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_regphyset_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_regphyset_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_regphyset_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_regphyset_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_regphyset_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_regphyset_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_regphyset_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_regphyset) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_regphyset_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-ASSIGNMENT(=)-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_regphyset_0_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_regphyset), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_regphyset), INTENT(INOUT) :: target_this
    !
    target_this%region_name   = source_this%region_name
    target_this%region_inside = source_this%region_inside
    target_this%region_zmin   = source_this%region_zmin
    target_this%region_zmax   = source_this%region_zmax
    !
    IF ( ASSOCIATED ( source_this%mespos_name ) ) THEN
       CALL set_regphyset_mespos_name ( target_this, source_this%mespos_name )
    ELSE
       CALL dealloc_regphyset_mespos_name ( target_this )
    ENDIF
    !
    target_this%mespos_maxdist = source_this%mespos_maxdist
    target_this%interpol_name  = source_this%interpol_name
    !
  END SUBROUTINE assign_regphyset_0_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_regphyset_1_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_regphyset), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_regphyset), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(target_this)
       CALL assign_regphyset_0_0( target_this(i), source_this )
    END DO
    !
  END SUBROUTINE assign_regphyset_1_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen der Objektliste 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_regphyset_1_1 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_regphyset), INTENT(IN)    :: source_this(:)
    !! Zielobjekt
    TYPE (t_regphyset), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, MIN( SIZE(source_this), SIZE(target_this) )
       CALL assign_regphyset_0_0( target_this(i), source_this(i) )
    END DO
    !
  END SUBROUTINE assign_regphyset_1_1
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
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Fehlernummer
    INTEGER            :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "m_ipds_regphyset" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_regphyset ausfuehren'
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
    !! Testergebnis
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
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_regphyset_all_errors ( )
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
               '--> INIT_regphyset ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_regphyset ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "mespos_name"\n'//&
               '--> Code in Modul "m_ipds_regphyset" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "region_name"\n'//&
               'Die Komponente darf nicht leer sein\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
!>GL>       ic = ic + 1
!>GL>       IF ( i == 2 ) THEN
!>GL>          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6015 )
!>GL>          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
!>GL>               'Fehlerkategorie: OK-Methoden\n'//&
!>GL>               'Fehler in Komponente von "t_regphyset"\n'//&
!>GL>               'Typ-Komponente = "region_name" kommt mehrfach vor\n'//&
!>GL>               'Pos = <index1> , Name = <name1>\n'//&
!>GL>               'Pos = <index2> , Name = <name2>\n'//&
!>GL>               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
!>GL>       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "region_inside"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "region_zmin"\n'//&
               'Die Komponente ist nur initialisiert, aber noch nicht gesetzt\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "region_zmax"\n'//&
               'Die Komponente ist nur initialisiert, aber noch nicht gesetzt\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "mespos_name"\n'//&
               'Die Komponente ist ohne Daten oder enthaelt (zum Teil) leere Bezeichnungen\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6055 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "mespos_maxdist"\n'//&
               'Die Komponente ist nur initialisiert, aber noch nicht gesetzt\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "interpol_name"\n'//&
               'Die Komponente darf z.Z. nur den Wert "nearest_points_in_sectors" annehmen\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_regphyset"\n'//&
               'Typ-Komponenten = "region_zmin" und "region_zmax"\n'//&
               'Die Komponente "region_zmin" muss kleiner gleich "region_zmax" sein\n'//&
               '"region_zmin" (lower_validity_level) = <zmin>\n'//&
               '"region_zmax" (upper_validity_level) = <zmax>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "m_ipds_regphyset" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "m_ipds_regphyset" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "m_ipds_regphyset" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_regphyset"\n'//&
               'Typ-Komponente = "region_name"\n'//&
               '--> Code in Modul "m_ipds_regphyset" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_regphyset"\n'//&
               'Typ-Komponente = "region_inside"\n'//&
               '--> Code in Modul "m_ipds_regphyset" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_regphyset"\n'//&
               'Typ-Komponente = "region_zmin"\n'//&
               '--> Code in Modul "m_ipds_regphyset" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_regphyset"\n'//&
               'Typ-Komponente = "region_zmax"\n'//&
               '--> Code in Modul "m_ipds_regphyset" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_regphyset"\n'//&
               'Typ-Komponente = "mespos_name"\n'//&
               '--> Code in Modul "m_ipds_regphyset" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7055 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_regphyset"\n'//&
               'Typ-Komponente = "mespos_maxdist"\n'//&
               '--> Code in Modul "m_ipds_regphyset" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_regphyset"\n'//&
               'Typ-Komponente = "interpol_name"\n'//&
               '--> Code in Modul "m_ipds_regphyset" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "m_ipds_regphyset"\n'//&
               '--> Code in Modul "m_ipds_regphyset" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_regphyset"\n'//&
               'Typ-Komponente = "mespos_name"\n'//&
               '--> Code in Modul "m_ipds_regphyset" pruefen' )
       END IF
       !
       ! Allokieren der Felder beim ersten Durchlauf (i==1)
       !
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    END DO
    !
  END SUBROUTINE init_regphyset_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_regphyset_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_regphyset_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "mespos_name" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_regphyset_mespos_name ( this, idim )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "mespos_name"
    INTEGER            , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER :: c_upname='alloc_regphyset_mespos_name'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%mespos_name(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 8050, c_upname, c_modname, stat )
    !
  END SUBROUTINE alloc_regphyset_mespos_name
  !
  !! Initialisieren der Feld-Komponente "mespos_name" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_regphyset_mespos_name ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(INOUT) :: this   ! 
    !! Initialisierungswert mespos_name
    CHARACTER (LEN=c_len_mespos_name), PARAMETER :: c_var=REPEAT(' ',c_len_mespos_name)
    !
    this%mespos_name(:) = c_var
    !
  END SUBROUTINE init_regphyset_mespos_name
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "mespos_name" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_regphyset_mespos_name ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=29), PARAMETER :: c_upname='dealloc_regphyset_mespos_name'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%mespos_name ) ) THEN
       DEALLOCATE ( this%mespos_name, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5050, c_upname, c_modname, stat )
       NULLIFY ( this%mespos_name ) 
    END IF
    !
  END SUBROUTINE dealloc_regphyset_mespos_name
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "region_name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_region_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24), PARAMETER :: c_upname='ok_regphyset_region_name'
    !
    ok = ( LEN_TRIM (this%region_name) > 0 )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
  END FUNCTION ok_regphyset_region_name
  !
  !! Pr&uuml;fe, ob die Komponente "region_inside" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_region_inside ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=26), PARAMETER :: c_upname='ok_regphyset_region_inside'
    !
    ok = .true.
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
  END FUNCTION ok_regphyset_region_inside
  !
  !! Pr&uuml;fe, ob die Komponente "region_zmin" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_region_zmin ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24), PARAMETER :: c_upname='ok_regphyset_region_zmin'
    !
    ok = ( this%region_zmin < HUGE(this%region_zmin) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    !
  END FUNCTION ok_regphyset_region_zmin
  !
  !! Pr&uuml;fe, ob die Komponente "region_zmax" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_region_zmax ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24), PARAMETER :: c_upname='ok_regphyset_region_zmax'
    !
    ok = ( this%region_zmax < HUGE(this%region_zmax) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    !
  END FUNCTION ok_regphyset_region_zmax
  !
  !! Pr&uuml;fe, ob die Komponente "mespos_name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_mespos_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24), PARAMETER :: c_upname='ok_regphyset_mespos_name'
    !
    ok = ASSOCIATED(this%mespos_name)
    IF ( ok ) ok = ( ALL( LEN_TRIM( this%mespos_name ) > 0 ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
    !
  END FUNCTION ok_regphyset_mespos_name
  !
  !! Pr&uuml;fe, ob die Komponente "mespos_maxdist" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_mespos_maxdist ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=27), PARAMETER :: c_upname='ok_regphyset_mespos_maxdist'
    !
    ok = ( this%mespos_maxdist < HUGE(this%mespos_maxdist) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6055, c_upname, c_modname )
    !
  END FUNCTION ok_regphyset_mespos_maxdist
  !
  !! Pr&uuml;fe, ob die Komponente "interpol_name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_interpol_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=26), PARAMETER :: c_upname='ok_regphyset_interpol_name'
    !
    ok = ( this%interpol_name == 'nearest_points_in_sectors' )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
    !
  END FUNCTION ok_regphyset_interpol_name
  !
  !! Pr&uuml;fe, ob die Komponenten "region_zmin" und "region_zmax" o.k. sind <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_regphyset_region_zmin_zmax ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=29), PARAMETER :: c_upname='ok_regphyset_region_zmin_zmax'
    !! Hilfsfehlertext
    CHARACTER (LEN=15) :: ctmp
    !
    ok = ( this%region_zmin <= this%region_zmax )
    !
    IF ( .NOT. ok ) THEN
       !$OMP critical
       CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
       WRITE( ctmp, '(F15.6)') this%region_zmin
       CALL setup_error_act ( '<zmin>', ctmp )
       WRITE( ctmp, '(F15.6)') this%region_zmax
       CALL setup_error_act ( '<zmax>', ctmp )
       !$OMP end critical
    END IF
    !
  END FUNCTION ok_regphyset_region_zmin_zmax
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "region_name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_region_name ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_regphyset_region_name'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           TRIM(this%region_name)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente region_name - - - - - - - - - - - - - ',/ &
          '# region_name = ',A,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_regphyset_region_name
  !
  !! Drucke den Inhalt der Komponente "region_inside" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_region_inside ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=29), PARAMETER :: c_upname='print_regphyset_region_inside'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           this%region_inside
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente region_inside - - - - - - - - - - - - ',/ &
          '# region_inside = ',L1,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_regphyset_region_inside
  !
  !! Drucke den Inhalt der Komponente "region_zmin" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_region_zmin ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_regphyset_region_zmin'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           this%region_zmin
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente region_zmin - - - - - - - - - - - - - ',/&
          '# region_zmin = ',F12.4,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_regphyset_region_zmin
  !
  !! Drucke den Inhalt der Komponente "region_zmax" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_region_zmax ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_regphyset_region_zmax'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           this%region_zmax
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente region_zmax - - - - - - - - - - - - - ',/&
          '# region_zmax = ',F12.4,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_regphyset_region_zmax
  !
  !! Drucke den Inhalt der Komponente "mespos_name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_mespos_name ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_regphyset_mespos_name'
    !! Statusvariable
    INTEGER :: stat
    !! Zaehlvariable
    INTEGER :: i
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8000,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
    !
    IF ( ASSOCIATED (this%mespos_name) ) THEN
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this%mespos_name) ) EXIT
          !
          WRITE ( &
               UNIT    = prn_lun,  &
               FMT     = 8001,     & 
               IOSTAT  = stat )    &
               i, TRIM(this%mespos_name(i))
          !
          IF ( stat /= 0 ) EXIT
          !
       ENDDO
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
       !
    ELSE
       !
       WRITE ( &
            UNIT    = prn_lun,  &
            FMT     = 8001,     & 
            IOSTAT  = stat )    &
            -1, '<..no.mespos.name.associated..>'
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
       !
    ENDIF
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8002,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente mespos_name - - - - - - - - - - - - - ')
8001 FORMAT ( &
          '# = ',I9,'  mespos_name = ',A)
8002 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_regphyset_mespos_name
  !
  !! Drucke den Inhalt der Komponente "mespos_maxdist" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_mespos_maxdist ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=30), PARAMETER :: c_upname='print_regphyset_mespos_maxdist'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           this%mespos_maxdist
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7055, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente mespos_maxdist  - - - - - - - - - - - ',/&
          '# mespos_maxdist = ',F12.4,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_regphyset_mespos_maxdist
  !
  !! Drucke den Inhalt der Komponente "interpol_name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_regphyset_interpol_name ( this )
    !! Datenobjekt
    TYPE (t_regphyset) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=29), PARAMETER :: c_upname='print_regphyset_interpol_name'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           TRIM(this%interpol_name)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente interpol_name - - - - - - - - - - - - ',/&
          '# interpol_name = ',A,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_regphyset_interpol_name
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "region_name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_region_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%region_name == this2%region_name )
    !
  END FUNCTION eq_regphyset_region_name
  !
  !! pr&uuml;fe Komponente "region_inside" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_region_inside ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = (     this1%region_inside .AND.       this2%region_inside .OR. &
         .NOT. this1%region_inside .AND. .NOT. this2%region_inside )
    !
  END FUNCTION eq_regphyset_region_inside 
  !
  !! pr&uuml;fe Komponente "region_zmin" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_region_zmin ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%region_zmin == this2%region_zmin )
    !
  END FUNCTION eq_regphyset_region_zmin 
  !
  !! pr&uuml;fe Komponente "region_zmax" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_region_zmax ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%region_zmax == this2%region_zmax )
    !
  END FUNCTION eq_regphyset_region_zmax 
  !
  !! pr&uuml;fe Komponente "mespos_name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_mespos_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Zaehlvariable
    INTEGER :: i
    !
    ok = .true.
    !
    i  = 0
    DO
       i = i + 1
       IF ( i > MIN( SIZE( this1%mespos_name ), SIZE( this2%mespos_name ) ) ) EXIT
       ok = ( this1%mespos_name(i) == this2%mespos_name(i) )
       IF ( .NOT. ok ) EXIT
    ENDDO
    !
  END FUNCTION eq_regphyset_mespos_name 
  !
  !! pr&uuml;fe Komponente "mespos_maxdist" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_mespos_maxdist ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%mespos_maxdist == this2%mespos_maxdist )
    !
  END FUNCTION eq_regphyset_mespos_maxdist 
  !
  !! pr&uuml;fe Komponente "interpol_name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_regphyset_interpol_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_regphyset) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%interpol_name == this2%interpol_name )
    !
  END FUNCTION eq_regphyset_interpol_name 
  !
END MODULE m_ipds_regphyset
! TailOfPackageModule ------------------------------------------------------
