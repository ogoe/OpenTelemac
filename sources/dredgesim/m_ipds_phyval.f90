! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Definition und Methoden fuer den Datentyp "t_phyval"</h2>
!! @author Jens J&uuml;rges
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_phyval.f90
!! <HR>
!! definition and methods for data type "t_phyval"             <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-08-09 : J. Juerges  : Original
!  01.02 : 2003-01-10 : J. Juerges  : Entwicklungsgeschichte nicht fuer f90doc
!  01.03 : 2003-01-10 : J. Juerges  : Lizenzangabe durch Platzhalter ersetzt
!  01.04 : 2003-01-10 : J. Juerges  : keine Beschreibungen der Interfaces dort, wo sie PUBLIC gemacht werden
!  01.05 : 2003-02-06 : J. Juerges  : Lizenzangabe aus WRITE-Anweisung entfernt
!  01.06 : 2003-02-10 : J. Juerges  : Fehlerdeklaration fuer Mehrprozessor-Betrieb fit gemacht
!  01.07 : 2003-02-11 : J. Juerges  : Funktion is_phyval_in_use ergaenzt
!  01.08 : 2003-02-11 : J. Juerges  : Funktion index_phyval_name produziert keine Fehlermeldungen mehr
!  01.09 : 2004-01-29 : H. Weilbeer : Bei selbstdefinierten Datentypen INTENT(OUT) -> INTENT(INOUT) gewandelt
!  01.10 : 2004-02-03 : G. Seiss    : PC-Anpassung
!  01.11 : 2005-04-12 : J. Juerges  : In phyval_frac_name "Anteil" durch "Sedimentanteil" ersetzt
!  02.01 : 2005-08-10 : G. Lang     : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-08-22 : J. Juerges  : Indexberechnung funktioniert auch mit engl. Bezeichnungen
!  02.03 : 2005-08-25 : J. Juerges  : "is_phyval_frac" funktioniert auch mit engl. Bezeichnungen
!  02.04 : 2005-08-25 : J. Juerges  : "phyval_frac_name" in Modul m_ipds_phydef ausgegliedert
!  02.05 : 2005-08-25 : J. Juerges  : Normalisierungsberechnungen ergaenzt
!  02.06 : 2005-09-01 : J. Juerges  : Korrekte Initialisierung fuer prn_op und trc_op
!  03.01 : 2007-03-13 : G. Lang     : neue Hauptversionsnummer 3
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Definition des Datentyps "t_phyval" fuer das Paket "ipds";
!!   <LI> Der Datentyp speichert die Infos und Werte einer phys. Groesse;
!!   <LI> Elementare Methoden auf Daten des Typs "t_phyval";
!!   <LI> Ein direkter Zugriff auf diese Daten/Methoden von anderen Paketen 
!!        aus ist nicht zul&auml;ssig.
!! </OL>
!! <HR>
!
MODULE m_ipds_phyval
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit Fehler-Typ und -Routinen 
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
  ! [A.2] Datenmodul des Paketes IPDS
  !
  USE m_ipds_phydef, ONLY : &
       ! Variable
       c_nof_frac         , &
       c_idx_phyval_frac  , &
       c_phy_name_de      , &
       ! Funktionen
       index_phy_name
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
  !! max. L&auml;nge der Komponente "name" und "var_name" in "t_phyval"
  INTEGER , PUBLIC, PARAMETER :: c_len_phyval_name=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Aufnahme eines Wertespro Variante einer physikalischen Groesse
  !! name     : Bezeichnung der physikalischen Groesse
  !! var_name : Bezeichnungen der Varianten
  !! type     : Fuer jede Variante eine Typkennung (Varianten mit unterschiedlicher
  !!            Typkennung koennen nicht interpoliert werden)
  !! val      : Werte aller Varianten
  TYPE , PUBLIC :: t_phyval
     PRIVATE
     CHARACTER (LEN=c_len_phyval_name)           :: name        ! 
     CHARACTER (LEN=c_len_phyval_name) , POINTER :: var_name(:) ! 
     INTEGER                           , POINTER :: type(:)     ! 
     REAL                              , POINTER :: val(:)      ! 
  END TYPE t_phyval
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
  INTERFACE init_phyval
     MODULE PROCEDURE init_phyval_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE clear_phyval
     MODULE PROCEDURE clear_phyval_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_phyval_prn_lun
     MODULE PROCEDURE setup_phyval_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_phyval_trc_lun
     MODULE PROCEDURE setup_phyval_trc_lun_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_phyval" (Skalar, 1D-Array)
  INTERFACE new_phyval
     MODULE PROCEDURE new_phyval_0  ! Version fuer Skalar
     MODULE PROCEDURE new_phyval_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_phyval" (Skalar, 1D-Array)
  INTERFACE kill_phyval
     MODULE PROCEDURE kill_phyval_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_phyval_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_phyval" (Skalar, 1D-Array)
  INTERFACE ok_phyval
     MODULE PROCEDURE ok_phyval_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_phyval_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Datenobjekten "t_phyval" (Skalar, 1D-Array)
  INTERFACE print_phyval
     MODULE PROCEDURE print_phyval_0 ! Version fuer Skalar
     MODULE PROCEDURE print_phyval_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten
  INTERFACE print_phyval_static
     MODULE PROCEDURE print_phyval_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls
  INTERFACE print_phyval_all_errors
     MODULE PROCEDURE print_phyval_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "name" in "t_phyval" auf Benutzerwert
  INTERFACE set_phyval_name
     MODULE PROCEDURE set_phyval_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_phyval_name_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "var_name" in "t_phyval" auf Benutzerwert
  INTERFACE set_phyval_var_name
     MODULE PROCEDURE set_phyval_var_name_0_1 ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_phyval_var_name_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "type" in "t_phyval" auf Benutzerwert
  INTERFACE set_phyval_type
     MODULE PROCEDURE set_phyval_type_0_1 ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_phyval_type_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "val" in "t_phyval" auf Benutzerwert
  INTERFACE set_phyval_val
     MODULE PROCEDURE set_phyval_val_0_1 ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_phyval_val_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !
  !! Hole Komponente "name" aus "t_phyval"
  INTERFACE get_phyval_name
     MODULE PROCEDURE get_phyval_name_0_0 ! Skalar
     MODULE PROCEDURE get_phyval_name_1_1 ! Vektor
  END INTERFACE
  !! Hole Anzahl "var_name" aus "t_phyval"
  INTERFACE get_phyval_nof_var_name
     MODULE PROCEDURE get_phyval_nof_var_name_0 ! Skalar
     MODULE PROCEDURE get_phyval_nof_var_name_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "var_name" aus "t_phyval"
  INTERFACE get_phyval_var_name
     MODULE PROCEDURE get_phyval_var_name_0_1 ! Skalar
  END INTERFACE
  !! Hole Anzahl "type" aus "t_phyval"
  INTERFACE get_phyval_nof_type
     MODULE PROCEDURE get_phyval_nof_type_0 ! Skalar
     MODULE PROCEDURE get_phyval_nof_type_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "type" aus "t_phyval"
  INTERFACE get_phyval_type
     MODULE PROCEDURE get_phyval_type_0_1 ! Skalar
  END INTERFACE
  !! Hole Anzahl "val" aus "t_phyval"
  INTERFACE get_phyval_nof_val
     MODULE PROCEDURE get_phyval_nof_val_0 ! Skalar
     MODULE PROCEDURE get_phyval_nof_val_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "val" aus "t_phyval"
  INTERFACE get_phyval_val
     MODULE PROCEDURE get_phyval_val_0_1 ! Skalar
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Ist eine phys. Groesse eine Anteilsgroesse?
  INTERFACE is_phyval_frac
     MODULE PROCEDURE is_phyval_frac_0_0
     MODULE PROCEDURE is_phyval_frac_1_1
     MODULE PROCEDURE is_phyval_frac_n0_0
     MODULE PROCEDURE is_phyval_frac_n1_1
  END INTERFACE
  !
  !! Ermittlung der Anzahl phys. Anteilsgroessen und Varianten
  INTERFACE get_phyval_nof_frac
     MODULE PROCEDURE get_phyval_nof_frac_0_0
     MODULE PROCEDURE get_phyval_nof_frac_1_1
  END INTERFACE
  !! Ist ein Datenobjekt (oder eine Liste von Objekten) vom Typ t_phyval
  !! bereits in Gebrauch, oder sind die Komponenten nur initialisiert? <BR>
  !! Die Funktion liefert TRUE, wenn alle Komponenten vom Initialzustand
  !! verschieden sind, sonst FALSE.
  INTERFACE is_phyval_in_use
     MODULE PROCEDURE is_phyval_in_use_0
     MODULE PROCEDURE is_phyval_in_use_1
  END INTERFACE
  !! Ermittle aus einer Liste von Datenobjekten das Objekt mit dem gegebenen Namen
  INTERFACE index_phyval_name
     MODULE PROCEDURE index_phyval_name_1_0
  END INTERFACE
  !! Normalisieren der Anteilsgroessen
  INTERFACE normalize_phyval
     MODULE PROCEDURE normalize_phyval_0
     MODULE PROCEDURE normalize_phyval_l
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  INTERFACE assign_phyval
     MODULE PROCEDURE assign_phyval_0_0
     MODULE PROCEDURE assign_phyval_1_0
     MODULE PROCEDURE assign_phyval_1_1
  END INTERFACE
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
  !! Pr&uuml;fung zweier Datenobjekte "t_phyval" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_phyval_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_phyval_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_phyval_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_phyval_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_phyval" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_phyval_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_phyval_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_phyval_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_phyval_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_phyval
  PUBLIC :: clear_phyval
  PUBLIC :: setup_phyval_prn_lun
  PUBLIC :: setup_phyval_trc_lun
  PUBLIC :: new_phyval
  PUBLIC :: kill_phyval
  PUBLIC :: ok_phyval
  PUBLIC :: print_phyval
  PUBLIC :: print_phyval_static
  PUBLIC :: print_phyval_all_errors
  PUBLIC :: set_phyval_name
  PUBLIC :: set_phyval_var_name
  PUBLIC :: set_phyval_type
  PUBLIC :: set_phyval_val
  PUBLIC :: get_phyval_name
  PUBLIC :: get_phyval_nof_var_name
  PUBLIC :: get_phyval_var_name
  PUBLIC :: get_phyval_nof_type
  PUBLIC :: get_phyval_type
  PUBLIC :: get_phyval_nof_val
  PUBLIC :: get_phyval_val
  PUBLIC :: OPERATOR(==)
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: assign_phyval
  PUBLIC :: is_phyval_frac
  PUBLIC :: get_phyval_nof_frac
  PUBLIC :: is_phyval_in_use
  PUBLIC :: index_phyval_name
  PUBLIC :: normalize_phyval
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
  CHARACTER (LEN=13), PARAMETER :: c_modname           = 'm_ipds_phyval'
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op                = .false.
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun               = -1
  !! Anzahl der Datenkomponenten des Typs t_phyval
  INTEGER           , PARAMETER :: c_nofcomp           = 4
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
  SUBROUTINE init_phyval_d ( )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "m_ipds_phyval" version 3.1 of 13.03 07'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_phyval_all_errors ( ) 
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
  END SUBROUTINE init_phyval_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_phyval_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_phyval_all_errors ( ) 
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
  END SUBROUTINE clear_phyval_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_phyval_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_phyval_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_phyval_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_phyval_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_phyval_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_phyval_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_phyval_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_phyval_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%name  = REPEAT( ' ', LEN(this%name) )
       NULLIFY ( this%var_name )
       NULLIFY ( this%type )
       NULLIFY ( this%val )
    END IF
    !
  END SUBROUTINE new_phyval_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_phyval_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_phyval) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_phyval_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_phyval_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_phyval_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_phyval_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_phyval_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL dealloc_phyval_var_name( this )
       IF ( no_error( ) ) CALL dealloc_phyval_type( this )
       IF ( no_error( ) ) CALL dealloc_phyval_val( this )
       IF ( no_error( ) ) CALL new_phyval_0 ( this )
    END IF
    !
  END SUBROUTINE kill_phyval_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_phyval_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_phyval) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_phyval_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_phyval_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_phyval_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_phyval_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_phyval_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp+1) 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_phyval_name( this )
       l_ok(2)  = ok_phyval_var_name( this )
       l_ok(3)  = ok_phyval_type( this )
       l_ok(4)  = ok_phyval_val( this )
       l_ok(5)  = ok_phyval_nof( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_phyval_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_phyval_1 ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_phyval_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) ) EXIT
          ok(i) = ok_phyval_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_phyval_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phyval_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_phyval_0' 
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
       IF ( no_error( ) ) CALL print_phyval_name( this )
       IF ( no_error( ) ) CALL print_phyval_var_name( this )
       IF ( no_error( ) ) CALL print_phyval_type( this )
       IF ( no_error( ) ) CALL print_phyval_val( this )
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
8000 FORMAT('# Beginn Objekt t_phyval -------------------------------------')
8001 FORMAT('# Ende   Objekt t_phyval -------------------------------------')
    !
  END SUBROUTINE print_phyval_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phyval_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_phyval_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     & 
                 IOSTAT  = stat ) i
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          !
          IF ( no_error( ) ) CALL print_phyval_0 ( this(i) )
          !
       END DO
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_phyval_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phyval_static_d ( )
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='print_phyval_static_d' 
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
       IF ( no_error( ) ) CALL print_phyval_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls m_ipds_phyval         ',/ &
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
  END SUBROUTINE print_phyval_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phyval_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=25), PARAMETER :: c_upname='print_phyval_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_phyval_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_phyval_name_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*), INTENT(IN)    :: val  ! 
    !
    this%name = REPEAT( ' ', LEN(this%name) )
    this%name = val(1:MIN(LEN(val),LEN(this%name)))
    !
  END SUBROUTINE set_phyval_name_0_0
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_phyval_name_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_phyval)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*), INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_phyval_name_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_phyval_name_1_0
  !
  !! weise der dynamischen Komponente "var_name" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_phyval_var_name_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval)  , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "var_name"
    CHARACTER (LEN=*), INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='set_phyval_var_name_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_phyval_var_name ( this            )
       IF ( no_error( ) ) CALL alloc_phyval_var_name   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_phyval_var_name    ( this            )
       IF ( no_error( ) ) this%var_name(:) = val(:)(1:MIN(LEN(val(1)),LEN(this%var_name(1))))
    END IF
    !
  END SUBROUTINE set_phyval_var_name_0_1
  !
  !! weise der dynamischen Komponente "var_name" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_phyval_var_name_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_phyval)  , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "var_name"
    CHARACTER (LEN=*), INTENT(IN)    :: val(:)  ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_phyval_var_name_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_phyval_var_name_1_1
  !
  !! weise der dynamischen Komponente "type" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_phyval_type_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "type"
    INTEGER           , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='set_phyval_type_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_phyval_type ( this            )
       IF ( no_error( ) ) CALL alloc_phyval_type   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_phyval_type    ( this            )
       IF ( no_error( ) ) this%type(:) = val(:)
    END IF
    !
  END SUBROUTINE set_phyval_type_0_1
  !
  !! weise der dynamischen Komponente "type" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_phyval_type_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_phyval) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "type"
    INTEGER           , INTENT(IN)  :: val(:)  ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_phyval_type_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_phyval_type_1_1
  !
  !! weise der dynamischen Komponente "val" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_phyval_val_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "val"
    REAL           , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='set_phyval_val_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_phyval_val ( this            )
       IF ( no_error( ) ) CALL alloc_phyval_val   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_phyval_val    ( this            )
       IF ( no_error( ) ) this%val(:) = val(:)
    END IF
    !
  END SUBROUTINE set_phyval_val_0_1
  !
  !! weise der dynamischen Komponente "val" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_phyval_val_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_phyval) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "val"
    REAL           , INTENT(IN)  :: val(:)  ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_phyval_val_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_phyval_val_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_name_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval)     , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "name" (Skalar)
    CHARACTER (LEN=c_len_phyval_name) :: val  ! 
    !
    val = this%name
    !
  END FUNCTION get_phyval_name_0_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_name_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_phyval)     , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "name"
    CHARACTER (LEN=c_len_phyval_name) :: val(SIZE(this))  ! 
    !
    val = this%name
    !
  END FUNCTION get_phyval_name_1_1
  !
  !! hole die Anzahl der Variantennamen in "var_name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_nof_var_name_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN)  :: this
    !! R&uuml;ckgabewert Anzahl "var_name"
    INTEGER                       :: val
    !
    IF( ASSOCIATED( this%var_name ) ) THEN
       val = SIZE(this%var_name)
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_phyval_nof_var_name_0
  !
  !! hole die Anzahl der Variantennamen in "var_name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_nof_var_name_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN)  :: this(:)
    !! R&uuml;ckgabewert Anzahl "var_name"
    INTEGER                       :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       val(i) = get_phyval_nof_var_name_0( this(i) )
    END DO
    !
  END FUNCTION get_phyval_nof_var_name_1
  !
  !! hole die dynamische Feld-Komponente "var_name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_var_name_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval)     , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "var_name" (Vektor)
    CHARACTER (LEN=c_len_phyval_name) :: val(SIZE(this%var_name)) ! 
    !
    val = this%var_name
    !
  END FUNCTION get_phyval_var_name_0_1
  !
  !! hole die Anzahl der Typen in "type" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_nof_type_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN)  :: this
    !! R&uuml;ckgabewert Anzahl "type"
    INTEGER                       :: val
    !
    IF( ASSOCIATED( this%type ) ) THEN
       val = SIZE(this%type)
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_phyval_nof_type_0
  !
  !! hole die Anzahl der Typen in "type" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_nof_type_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN)  :: this(:)
    !! R&uuml;ckgabewert Anzahl "type"
    INTEGER                       :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       val(i) = get_phyval_nof_type_0( this(i) )
    END DO
    !
  END FUNCTION get_phyval_nof_type_1
  !
  !! hole die dynamische Feld-Komponente "type" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_type_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval), INTENT(IN) :: this     ! 
    !! R&uuml;ckgabewert "type" (Vektor)
    INTEGER                     :: val(SIZE(this%type)) ! 
    !
    val = this%type
    !
  END FUNCTION get_phyval_type_0_1
  !
  !! hole die Anzahl der Werte in "val" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_nof_val_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN)  :: this
    !! R&uuml;ckgabewert Anzahl "val"
    INTEGER                       :: val
    !
    IF( ASSOCIATED( this%val ) ) THEN
       val = SIZE(this%val)
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_phyval_nof_val_0
  !
  !! hole die Anzahl der Werte in "val" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_nof_val_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN)  :: this(:)
    !! R&uuml;ckgabewert Anzahl "val"
    INTEGER                       :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       val(i) = get_phyval_nof_val_0( this(i) )
    END DO
    !
  END FUNCTION get_phyval_nof_val_1
  !
  !! hole die dynamische Feld-Komponente "val" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_val_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval), INTENT(IN) :: this
    !! R&uuml;ckgabewert "val" (Vektor)
    REAL                        :: val(SIZE(this%val)) ! 
    !
    val = this%val
    !
  END FUNCTION get_phyval_val_0_1
  !
  !! Ermittlung der Anzahl phys. Anteilsgroessen und Varianten eines Datenobjekts
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_nof_frac_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_phyval), INTENT(IN) :: this
    !! R&uuml;ckgabewert "val" (Skalar)
    INTEGER                     :: val
    !
    val = 0
    IF( is_phyval_frac( this ) ) THEN
       IF( ASSOCIATED( this%val ) ) val = SIZE( this%val )
    END IF
    !
  END FUNCTION get_phyval_nof_frac_0_0
  !
  !! Ermittlung der Anzahl phys. Anteilsgroessen und Varianten mehrerer Datenobjekte
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_phyval_nof_frac_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekte
    TYPE (t_phyval), INTENT(IN) :: this(:)
    !! R&uuml;ckgabewerte "val"
    INTEGER                     :: val( SIZE( this ) )
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    DO i = 1, SIZE( this )
       val( i ) = get_phyval_nof_frac_0_0( this( i ) )
    ENDDO
    !
  END FUNCTION get_phyval_nof_frac_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_phyval_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = eq_phyval_name     ( this1, this2 )
    l_ok(2)  = eq_phyval_var_name ( this1, this2 )
    l_ok(3)  = eq_phyval_type     ( this1, this2 )
    l_ok(4)  = eq_phyval_val      ( this1, this2 )
    ok       = ALL( l_ok )
    !
  END FUNCTION eq_phyval_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_phyval_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_phyval_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_phyval_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_phyval_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_phyval_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_phyval_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_phyval_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_phyval_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_phyval_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_phyval_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_phyval_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_phyval_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_phyval_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_phyval_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_phyval_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_phyval_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_phyval) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_phyval_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-ASSIGNMENT(=)-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_phyval_0_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_phyval), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_phyval), INTENT(INOUT) :: target_this
    !
    target_this%name = source_this%name
    !
    IF ( ASSOCIATED ( source_this%var_name ) ) THEN
       CALL set_phyval_var_name ( target_this, source_this%var_name )
    ELSE
       CALL dealloc_phyval_var_name ( target_this )
    ENDIF
    !
    IF ( ASSOCIATED ( source_this%type ) ) THEN
       CALL set_phyval_type ( target_this, source_this%type )
    ELSE
       CALL dealloc_phyval_type ( target_this )
    ENDIF
    !
    IF ( ASSOCIATED ( source_this%val ) ) THEN
       CALL set_phyval_val ( target_this, source_this%val )
    ELSE
       CALL dealloc_phyval_val ( target_this )
    ENDIF
    !
  END SUBROUTINE assign_phyval_0_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_phyval_1_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_phyval), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_phyval), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(target_this)
       CALL assign_phyval_0_0 ( target_this(i), source_this )
    END DO
    !
  END SUBROUTINE assign_phyval_1_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen der Objektliste 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_phyval_1_1 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_phyval), INTENT(IN)    :: source_this(:)
    !! Zielobjekt
    TYPE (t_phyval), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, MIN( SIZE(source_this), SIZE(target_this) )
       CALL assign_phyval_0_0 ( target_this(i), source_this(i) )
    END DO
    !
  END SUBROUTINE assign_phyval_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INDEX-Methoden <<< [ERR_NO = 21000 bis 21999]
  ! ----------------------------------------------------------------------
  !
  !! Ermittle aus einer Liste von Datenobjekten das Objekt mit dem gegebenen Namen <BR>
  !! Da nicht die Namensbezeichnungen direkt verglichen werden, sondern lediglich die
  !! Indizes in den Feldern c_phy_name_[de|en], koennen auch deutsche mit englischen
  !! Bezeichnungen verglichen werden. <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION index_phyval_name_1_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_phyval)  , INTENT(IN)  :: this(:)
    !! Name des gesuchten Datenobjekts
    CHARACTER (LEN=*), INTENT(IN)  :: name
    !! R&uuml;ckgabewert Index
    INTEGER :: val
    !! Zaehler
    INTEGER :: i
    !! Lokale Indizes
    INTEGER :: idx_phyval, idx_name
    !
    val = 0
    DO i = 1, SIZE( this )
       !
       idx_phyval = index_phy_name( TRIM( get_phyval_name( this(i) ) ) )
       idx_name   = index_phy_name( TRIM( name ) )
       !
       IF( idx_phyval == idx_name .AND. idx_phyval > 0 ) val = i
       !
    END DO
    !
  END FUNCTION index_phyval_name_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-LOGICAL-Methoden <<< [ERR_NO = 22000 bis 22999]
  ! ----------------------------------------------------------------------
  !
  !! Ist eine phys. Groesse eine Anteilsgroesse (ein Datenobjekt)
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_phyval_frac_0_0 ( this ) &
       RESULT( is_frac )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval), INTENT(IN) :: this
    !! R&uuml;ckgabewert "is_frac" (Skalar)
    LOGICAL                     :: is_frac
    !! Zaehler
    INTEGER :: i
    !! Lokale Indizes
    INTEGER :: idx_phyval, idx_name
    !
    is_frac = .FALSE.
    !
    i = 0
    DO
       i = i + 1
       IF( i > c_nof_frac ) EXIT
       !
       idx_phyval = index_phy_name( TRIM( c_phy_name_de( c_idx_phyval_frac( i ) ) ) )
       idx_name   = index_phy_name( TRIM( get_phyval_name( this ) ) )
       !
       IF( idx_phyval == idx_name .AND. idx_phyval > 0 ) is_frac = .TRUE.
       IF( is_frac ) EXIT
    END DO
    !
  END FUNCTION is_phyval_frac_0_0
  !
  !! Ist eine phys. Groesse eine Anteilsgroesse (viele Datenobjekte)
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_phyval_frac_1_1 ( this ) &
       RESULT( is_frac )
    !! Datenobjekte
    TYPE (t_phyval), INTENT(IN) :: this(:)
    !! R&uuml;ckgabewerte "is_frac"
    LOGICAL                     :: is_frac( SIZE( this ) )
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE( this )
       is_frac( i ) = is_phyval_frac_0_0 ( this( i ) )
    END DO
    !
  END FUNCTION is_phyval_frac_1_1
  !
  !! Ist eine phys. Groesse eine Anteilsgroesse (ein Datenobjektname)
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_phyval_frac_n0_0 ( phyval_name ) &
       RESULT( is_frac )
    !! Name eines Datenobjekts (Skalar)
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert "is_frac" (Skalar)
    LOGICAL                       :: is_frac
    !! Zaehler
    INTEGER :: i
    !! Lokale Indizes
    INTEGER :: idx_phyval, idx_name
    !
    is_frac = .FALSE.
    !
    i = 0
    DO
       i = i + 1
       IF( i > c_nof_frac ) EXIT
       !
       idx_phyval = index_phy_name( TRIM( c_phy_name_de( c_idx_phyval_frac( i ) ) ) )
       idx_name   = index_phy_name( TRIM( phyval_name ) )
       !
       IF( idx_phyval == idx_name .AND. idx_phyval > 0 ) is_frac = .TRUE.
       IF( is_frac ) EXIT
    END DO
    !
  END FUNCTION is_phyval_frac_n0_0
  !
  !! Ist eine phys. Groesse eine Anteilsgroesse (viele Datenobjektnamen)
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_phyval_frac_n1_1 ( phyval_name ) &
       RESULT( is_frac )
    !! Namen der Datenobjekte (Vektor)
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name(:)
    !! R&uuml;ckgabewerte "is_frac"
    LOGICAL                       :: is_frac( SIZE( phyval_name ) )
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE( phyval_name )
       is_frac( i ) = is_phyval_frac_n0_0 ( phyval_name( i ) )
    END DO
    !
  END FUNCTION is_phyval_frac_n1_1
  !
  !! Ist ein Datenobjekt vom Typ t_phyval
  !! bereits in Gebrauch, oder sind die Komponenten nur initialisiert? <BR>
  !! Die Funktion liefert TRUE, wenn alle Komponenten vom Initialzustand
  !! verschieden sind, sonst FALSE. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen.
  FUNCTION is_phyval_in_use_0 ( this ) &
       RESULT( in_use )
    !! Datenobjekt (Skalar)
    TYPE (t_phyval), INTENT(IN) :: this
    !! R&uuml;ckgabewert "in_use"
    LOGICAL                     :: in_use
    !
    in_use = .FALSE.
    IF ( LEN_TRIM( this%name )         > 0 .AND. &
         get_phyval_nof_var_name(this) > 0 .AND. &
         get_phyval_nof_type(this)     > 0 .AND. &
         get_phyval_nof_val(this)      > 0 ) in_use = .TRUE.
    !
  END FUNCTION is_phyval_in_use_0
  !
  !! Ist eine Liste von Objekten vom Typ t_phyval
  !! bereits in Gebrauch, oder sind die Komponenten nur initialisiert? <BR>
  !! Die Funktion liefert TRUE, wenn alle Komponenten vom Initialzustand
  !! verschieden sind, sonst FALSE. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen.
  FUNCTION is_phyval_in_use_1 ( this ) &
       RESULT( in_use )
    !! Datenobjekte (Vektor)
    TYPE (t_phyval), INTENT(IN) :: this(:)
    !! R&uuml;ckgabewerte "in_use"
    LOGICAL                     :: in_use( SIZE(this) )
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       in_use(i) = is_phyval_in_use_0( this(i) )
    END DO
    !
  END FUNCTION is_phyval_in_use_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CALCU-Methoden <<< [ERR_NO = 24000 bis 24999]
  ! ----------------------------------------------------------------------
  !
  !! Normalisieren von Anteilsgroessen
  SUBROUTINE normalize_phyval_0 ( &
       this,                      &
       sum,                       &
       warn )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_phyval)   , INTENT(INOUT) :: this
    !! (Neue) Summe aller Varianten-Datenwerte
    REAL   , OPTIONAL , INTENT(IN)    :: sum
    !! Soll eine Warnung auf dem Bildschirm erscheinen, wenn die Normierung
    !! die Originaldaten veraendert?
    !! (Default=.FALSE.)
    LOGICAL, OPTIONAL , INTENT(IN)    :: warn
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER   :: c_upname='normalize_phyval_0'
    !! Anzahl Varianten
    INTEGER               :: nof_all_var
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten (hier immer .true.)
    LOGICAL , ALLOCATABLE :: all_var_ex(:)
    !! Datenwerte aller Varianten
    REAL    , ALLOCATABLE :: all_val(:)
    !! Zaehler
    INTEGER :: i_var
    !! Statusvariable
    INTEGER :: stat
    !
    ! [1] Eingangsdaten pruefen
    !
    IF ( .NOT. PRESENT( sum ) ) CALL setup_error_act( all_errors(:), 24100, c_upname, c_modname )
    !
    ! [2] Daten fuer die lokal-Routine bereitstellen
    !
    nof_all_var = 0
    IF ( no_error() ) nof_all_var = get_phyval_nof_frac ( this )
    !
    IF ( nof_all_var > 0 ) THEN
       !
       ALLOCATE ( &
            all_var_ex ( nof_all_var ) , &
            all_val    ( nof_all_var ) , &
            STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 24910, c_upname, c_modname, stat )
       !
       IF ( no_error() ) all_var_ex = .true.
       IF ( no_error() ) all_val    = get_phyval_val ( this )
       !
       IF ( no_error() .AND. prn_op ) THEN
          WRITE ( UNIT=prn_lun, FMT=8000 )
          DO i_var = 1, nof_all_var
             WRITE ( UNIT=prn_lun, FMT=8100 ) i_var, all_var_ex(i_var), all_val(i_var)
          END DO
          WRITE ( UNIT=prn_lun, FMT=8002 )
       END IF
       !
    END IF
    !
    ! [3] Daten lokal normieren
    !
    IF ( no_error() ) THEN
       !
       IF ( nof_all_var > 0 ) CALL normalize_phyval ( &
            nof_all_var,       &
            all_var_ex,        &
            all_val,           &
            sum,               &
            warn )
       !
    END IF
    !
    ! [4] Daten in das Datenobjekt eintragen
    !
    IF ( no_error() ) THEN
       !
       IF ( nof_all_var > 0 ) THEN
          !
          IF ( prn_op ) THEN
             WRITE ( UNIT=prn_lun, FMT=8001 )
             DO i_var = 1, nof_all_var
                WRITE ( UNIT=prn_lun, FMT=8100 ) i_var, all_var_ex(i_var), all_val(i_var)
             END DO
             WRITE ( UNIT=prn_lun, FMT=8002 )
          END IF
          !
          CALL set_phyval_val ( this, all_val )
          !
          DEALLOCATE ( all_var_ex, all_val, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 24920, c_upname, c_modname, stat )
          !
       END IF
       !
    END IF
    !
8000 FORMAT ( &
          '# Daten eines t_phyval-Objektes vor der Normierung  - - - - - ')
8001 FORMAT ( &
          '# Daten eines t_phyval-Objektes nach der Normierung - - - - - ')
8002 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
8100 FORMAT ( &
          '#=',I3,'  in Verwendung=',L1,'  Wert=',F10.7)
    !
  END SUBROUTINE normalize_phyval_0
  !
  !! Normalisieren von Anteilsgroessen
  SUBROUTINE normalize_phyval_l ( &
       nof_all_var,               &
       all_var_ex,                &
       all_val,                   &
       sum,                       &
       warn )
    !
    ! Formalparameter
    !! Anzahl Varianten
    INTEGER          , INTENT(IN)    :: nof_all_var
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten
    LOGICAL          , INTENT(IN)    :: all_var_ex(nof_all_var)
    !! Datenwerte aller Varianten
    REAL             , INTENT(INOUT) :: all_val(nof_all_var)
    !! (Neue) Summe aller Varianten-Datenwerte
    REAL, OPTIONAL   , INTENT(IN)    :: sum
    !! Soll eine Warnung auf dem Bildschirm erscheinen, wenn die Normierung
    !! die Originaldaten veraendert?
    !! (Default=.FALSE.)
    LOGICAL, OPTIONAL , INTENT(IN)    :: warn
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='normalize_phyval_l'
    !! Genauigkeitsangabe und Umrechnungsfaktor
    INTEGER           , PARAMETER :: c_accuracy=1000000
    !! Varianten-Zaehler
    INTEGER :: i_var
    !! (Alte) Summe der Varianten-Datenwerte
    REAL    :: valsum
    !! Normalisierungsfaktor
    REAL    :: fac
    !! Warnung ausgeben?
    LOGICAL :: l_warn
    !
    ! [1] Eingangsdaten pruefen
    !
    IF ( .NOT. PRESENT( sum ) ) CALL setup_error_act( all_errors(:), 24100, c_upname, c_modname )
    !
    l_warn = .FALSE.
    IF ( PRESENT( warn ) ) l_warn = warn
    !
    ! [2] Datensumme bilden
    !
    IF ( no_error() ) THEN
       !
       valsum = 0.0
       !
       DO i_var = 1, nof_all_var
          !
          IF( all_var_ex( i_var ) ) valsum = valsum + all_val( i_var )
          !
       END DO
       !
    END IF
    !
    ! [3] Daten normalisieren
    !
    IF ( no_error() ) THEN
       !
       IF ( valsum /= 0.00 ) THEN
          !
          fac = sum / valsum
          !
          IF ( l_warn ) THEN
             !
             IF ( NINT(REAL(c_accuracy)*fac) /= c_accuracy ) THEN
                !
                WRITE(*,*)
                WRITE(*,'(3x,A)')        '**** Normierung notwendig / normalisation required ****'
                WRITE(*,'(3x,A,F9.7,A)') '*        Normfaktor = ', fac ,' = norm factor         *'
                WRITE(*,'(3x,A)')        '*            Daten vor .. / data before ..            *'
                !
                i_var=1
                DO
                   IF ( i_var + 3  <= nof_all_var ) THEN
                      WRITE(*,'(3x,4(A,I2.2,A,F9.6),A)') &
                           '* ',i_var  ,'=',all_val(i_var)  , &
                           ' ' ,i_var+1,'=',all_val(i_var+1), &
                           ' ' ,i_var+2,'=',all_val(i_var+2), &
                           ' ' ,i_var+3,'=',all_val(i_var+3), &
                           ' *'
                      i_var=i_var+4
                   ELSE IF ( i_var + 2 == nof_all_var ) THEN
                      WRITE(*,'(3x,3(A,I2.2,A,F9.6),13x,A)') &
                           '* ',i_var  ,'=',all_val(i_var)  , &
                           ' ' ,i_var+1,'=',all_val(i_var+1), &
                           ' ' ,i_var+2,'=',all_val(i_var+2), &
                           ' *'
                      i_var=i_var+3
                   ELSE IF ( i_var + 1 == nof_all_var ) THEN
                      WRITE(*,'(3x,2(A,I2.2,A,F9.6),26x,A)') &
                           '* ',i_var  ,'=',all_val(i_var)  , &
                           ' ' ,i_var+1,'=',all_val(i_var+1), &
                           ' *'
                      i_var=i_var+2
                   ELSE IF ( i_var == nof_all_var ) THEN
                      WRITE(*,'(3x,A,I2.2,A,F9.6,39x,A)') &
                           '* ',i_var  ,'=',all_val(i_var)  , &
                           ' *'
                      i_var=i_var+1
                   END IF
                   IF ( i_var > nof_all_var ) EXIT
                END DO
                !
                WRITE(*,'(3x,A)') '*           Daten nach .. / data after ..             *'
                !
                i_var=1
                DO
                   IF ( i_var + 3  <= nof_all_var ) THEN
                      WRITE(*,'(3x,4(A,I2.2,A,F9.6),A)') &
                           '* ',i_var  ,'=',fac*all_val(i_var)  , &
                           ' ' ,i_var+1,'=',fac*all_val(i_var+1), &
                           ' ' ,i_var+2,'=',fac*all_val(i_var+2), &
                           ' ' ,i_var+3,'=',fac*all_val(i_var+3), &
                           ' *'
                      i_var=i_var+4
                   ELSE IF ( i_var + 2 == nof_all_var ) THEN
                      WRITE(*,'(3x,3(A,I2.2,A,F9.6),13x,A)') &
                           '* ',i_var  ,'=',fac*all_val(i_var)  , &
                           ' ' ,i_var+1,'=',fac*all_val(i_var+1), &
                           ' ' ,i_var+2,'=',fac*all_val(i_var+2), &
                           ' *'
                      i_var=i_var+3
                   ELSE IF ( i_var + 1 == nof_all_var ) THEN
                      WRITE(*,'(3x,2(A,I2.2,A,F9.6),26x,A)') &
                           '* ',i_var  ,'=',fac*all_val(i_var)  , &
                           ' ' ,i_var+1,'=',fac*all_val(i_var+1), &
                           ' *'
                      i_var=i_var+2
                   ELSE IF ( i_var == nof_all_var ) THEN
                      WRITE(*,'(3x,A,I2.2,A,F9.6,39x,A)') &
                           '* ',i_var  ,'=',fac*all_val(i_var)  , &
                           ' *'
                      i_var=i_var+1
                   END IF
                   IF ( i_var > nof_all_var ) EXIT
                END DO
                WRITE(*,'(3x,A)') '**** ****** .. Normierung / .. normalisation ***** ****'
                !
             END IF
             !
          END IF
          !
          DO i_var = 1, nof_all_var
             !
             IF( all_var_ex( i_var ) ) all_val( i_var ) = fac * all_val( i_var )
             !
          END DO
          !
       END IF
       !
    END IF
    !
  END SUBROUTINE normalize_phyval_l
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
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "m_ipds_phyval" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_phyval ausfuehren'
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
  SUBROUTINE init_phyval_all_errors ( )
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
               '--> INIT_phyval ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_phyval ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "var_name"\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "type"\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "val"\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "var_name"\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "type"\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "val"\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "name"\n'//&
               'Die Komponente darf nicht leer sein\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "var_name"\n'//&
               'Die Komponente enthaelt keine Daten\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6021 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "var_name"\n'//&
               'Mindestens eine Bezeichnung in der Komponente ist leer\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "type"\n'//&
               'Die Komponente enthaelt keine Daten\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "val"\n'//&
               'Die Komponente enthaelt keine Daten\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6041 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_phyval"\n'//&
               'Typ-Komponente = "val"\n'//&
               'Mind. ein Element der Komponente ist nur initialisiert, aber nicht gesetzt\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Array-Komponenten von "t_phyval"\n'//&
               'Die Komponente "var_name", "type" und "val" muessen gleich gross sein,\n'//&
               'sie sind es jedoch nicht:\n'//&
               'Anzahl ( var_name ) = <anz_nam>\n'//&
               'Anzahl ( type )     = <anz_typ>\n'//&
               'Anzahl ( val )      = <anz_val>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_phyval"\n'//&
               'Typ-Komponente = "name"\n'//&
               '--> Code in Modul "m_ipds_phyval" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_phyval"\n'//&
               'Typ-Komponente = "var_name"\n'//&
               '--> Code in Modul "m_ipds_phyval" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_phyval"\n'//&
               'Typ-Komponente = "type"\n'//&
               '--> Code in Modul "m_ipds_phyval" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_phyval"\n'//&
               'Typ-Komponente = "val"\n'//&
               '--> Code in Modul "m_ipds_phyval" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "m_ipds_phyval"\n'//&
               '--> Code in Modul "m_ipds_phyval" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 24100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NORMALIZE-Methoden\n'//&
               'Fuer die Normalisierung von Daten fehlt die Angabe der Datensumme.\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 24910 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NORMALIZE-Methoden\n'//&
               'Allocate-Fehler eines lokalen Feldes.\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 24920 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NORMALIZE-Methoden\n'//&
               'De-Allocate-Fehler eines lokalen Feldes.\n'//&
               '--> Code in Modul "m_ipds_phyval" pruefen' )
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
  END SUBROUTINE init_phyval_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_phyval_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_phyval_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "var_name" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_phyval_var_name ( this, idim )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "var_name"
    INTEGER         , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='alloc_phyval_var_name'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%var_name(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 4020, c_upname, c_modname, stat )
    !
  END SUBROUTINE alloc_phyval_var_name
  !
  !! Allokieren der dynamischen Feld-Komponente "type" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_phyval_type ( this, idim )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "type"
    INTEGER         , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='alloc_phyval_type'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%type(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 4030, c_upname, c_modname, stat )
    !
  END SUBROUTINE alloc_phyval_type
  !
  !! Allokieren der dynamischen Feld-Komponente "val" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_phyval_val ( this, idim )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "val"
    INTEGER         , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='alloc_phyval_val'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%val(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 4040, c_upname, c_modname, stat )
    !
  END SUBROUTINE alloc_phyval_val
  !
  !! Initialisieren der Feld-Komponente "var_name" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phyval_var_name ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this   ! 
    !! Initialisierungswert var_name
    CHARACTER (LEN=c_len_phyval_name), PARAMETER :: c_var=REPEAT(' ',c_len_phyval_name)
    !
    this%var_name(:) = c_var
    !
  END SUBROUTINE init_phyval_var_name
  !
  !! Initialisieren der Feld-Komponente "type" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phyval_type ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this   ! 
    !! Initialisierungswert type
    INTEGER , PARAMETER :: c_var=0 ! 
    !
    this%type(:) = c_var
    !
  END SUBROUTINE init_phyval_type
  !
  !! Initialisieren der Feld-Komponente "val" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_phyval_val ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this   ! 
    !! Initialisierungswert val
    REAL , PARAMETER :: c_var=HUGE(c_var) ! 
    !
    this%val(:) = c_var
    !
  END SUBROUTINE init_phyval_val
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "var_name" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_phyval_var_name ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='dealloc_phyval_var_name'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%var_name ) ) THEN
       DEALLOCATE ( this%var_name, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5020, c_upname, c_modname, stat )
       NULLIFY ( this%var_name ) 
    END IF
    !
  END SUBROUTINE dealloc_phyval_var_name
  !
  !! De-Allokieren der dynamischen Feld-Komponente "type" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_phyval_type ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='dealloc_phyval_type'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%type ) ) THEN
       DEALLOCATE ( this%type, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5030, c_upname, c_modname, stat )
       NULLIFY ( this%type ) 
    END IF
    !
  END SUBROUTINE dealloc_phyval_type
  !
  !! De-Allokieren der dynamischen Feld-Komponente "val" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_phyval_val ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='dealloc_phyval_val'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%val ) ) THEN
       DEALLOCATE ( this%val, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5040, c_upname, c_modname, stat )
       NULLIFY ( this%val ) 
    END IF
    !
  END SUBROUTINE dealloc_phyval_val
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phyval_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_phyval_name'
    !
    ok = ( LEN_TRIM( this%name ) > 0 )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
  END FUNCTION ok_phyval_name
  !
  !! Pr&uuml;fe, ob die Komponente "var_name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phyval_var_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18), PARAMETER :: c_upname='ok_phyval_var_name'
    !
    ok = ( ASSOCIATED( this%var_name ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
    IF ( ok ) THEN
       ok = ( ALL( LEN_TRIM( this%var_name ) > 0 ) )
       IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6021, c_upname, c_modname )
    ENDIF
    !
  END FUNCTION ok_phyval_var_name
  !
  !! Pr&uuml;fe, ob die Komponente "type" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phyval_type ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_phyval_type'
    !
    ok = ( ASSOCIATED( this%type ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    !
  END FUNCTION ok_phyval_type
  !
  !! Pr&uuml;fe, ob die Komponente "val" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phyval_val ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_phyval_val'
    !
    ok = ( ASSOCIATED( this%val ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    IF ( ok ) THEN
       ok = ( ALL( this%val < HUGE( this%val(1) ) ) )
       IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6041, c_upname, c_modname )
    ENDIF
    !
    !
  END FUNCTION ok_phyval_val
  !
  !! Pr&uuml;fe, ob die Anzahl der Arrayelemente identische ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_phyval_nof ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_phyval_nof'
    !! Suchstring im Fehlertext
    CHARACTER (LEN= 9) :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=10) :: cr
    !
    ok = ( ASSOCIATED( this%var_name ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
    ok = ( ASSOCIATED( this%type ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    !
    ok = ( ASSOCIATED( this%val ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    !
    IF ( ok ) THEN
       !
       ok = ( SIZE( this%var_name ) == SIZE( this%type ) .AND. SIZE( this%type ) == SIZE( this%val ) )
       !
       IF ( .NOT. ok ) THEN
          !
          !$OMP critical
          !
          CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
          !
          cs = '<anz_nam>'
          WRITE( cr, '(I10)') SIZE( this%var_name )
          CALL setup_error_act ( cs, cr )
          !
          cs = '<anz_typ>'
          WRITE( cr, '(I10)') SIZE( this%type )
          CALL setup_error_act ( cs, cr )
          !
          cs = '<anz_val>'
          WRITE( cr, '(I10)') SIZE( this%val )
          CALL setup_error_act ( cs, cr )
          !
          !$OMP end critical
          !
       END IF
       !
    ENDIF
    !
    !
  END FUNCTION ok_phyval_nof
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phyval_name ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_phyval_name'
    !! Statusvariable
    INTEGER :: stat
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           TRIM(this%name)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente name  - - - - - - - - - - - - - - - - ',/&
          '# name = ',A,/&
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_phyval_name
  !
  !! Drucke den Inhalt der Komponente "var_name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phyval_var_name ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=21), PARAMETER :: c_upname='print_phyval_var_name'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Zaehlvariable
    INTEGER :: i
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8000,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
    IF ( ASSOCIATED (this%var_name) ) THEN
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this%var_name) ) EXIT
          !
          WRITE ( &
               UNIT    = prn_lun,  &
               FMT     = 8001,     & 
               IOSTAT  = stat )    &
               i, TRIM(this%var_name(i))
          !
          IF ( stat /= 0 ) EXIT
          !
       ENDDO
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
       !
    ELSE
       !
       WRITE ( &
            UNIT    = prn_lun,  &
            FMT     = 8001,     & 
            IOSTAT  = stat )    &
            -1, '<..no.var.name.associated..>'
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
       !
    ENDIF
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8002,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente var_name  - - - - - - - - - - - - - - ')
8001 FORMAT ( &
          '# = ',I9,'  var_name = ',A)
8002 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_phyval_var_name
  !
  !! Drucke den Inhalt der Komponente "type" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phyval_type ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_phyval_type'
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
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
    IF ( ASSOCIATED (this%type) ) THEN
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this%type) ) EXIT
          !
          WRITE ( &
               UNIT    = prn_lun,  &
               FMT     = 8001,     & 
               IOSTAT  = stat )    &
               i, this%type(i)
          !
          IF ( stat /= 0 ) EXIT
          !
       ENDDO
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
       !
    ELSE
       !
       WRITE ( &
            UNIT    = prn_lun,  &
            FMT     = 8002,     & 
            IOSTAT  = stat )    &
            -1, '<..no.type.associated..>'
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
       !
    ENDIF
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8003,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente type  - - - - - - - - - - - - - - - - ')
8001 FORMAT ( &
          '# = ',I9,'  type = ',I9)
8002 FORMAT ( &
          '# = ',I9,'  type = ',A)
8003 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_phyval_type
  !
  !! Drucke den Inhalt der Komponente "val" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_phyval_val ( this )
    !! Datenobjekt
    TYPE (t_phyval) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_phyval_val'
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
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
    IF ( ASSOCIATED (this%val) ) THEN
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this%val) ) EXIT
          !
          WRITE ( &
               UNIT    = prn_lun,  &
               FMT     = 8001,     & 
               IOSTAT  = stat )    &
               i, this%val(i)
          !
          IF ( stat /= 0 ) EXIT
          !
       ENDDO
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
       !
    ELSE
       !
       WRITE ( &
            UNIT    = prn_lun,  &
            FMT     = 8002,     & 
            IOSTAT  = stat )    &
            -1, '<..no.val.associated..>'
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
       !
    ENDIF
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8003,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente val - - - - - - - - - - - - - - - - - ')
8001 FORMAT ( &
          '# = ',I9,'  type = ',F12.4)
8002 FORMAT ( &
          '# = ',I9,'  type = ',A)
8003 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_phyval_val
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_phyval_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%name == this2%name )
    !
  END FUNCTION eq_phyval_name
  !
  !! pr&uuml;fe Komponente "var_name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_phyval_var_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this2 ! 
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
       IF ( i > MIN( SIZE( this1%var_name ), SIZE( this2%var_name ) ) ) EXIT
       ok = ( this1%var_name(i) == this2%var_name(i) )
       IF ( .NOT. ok ) EXIT
    ENDDO
    !
  END FUNCTION eq_phyval_var_name 
  !
  !! pr&uuml;fe Komponente "type" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_phyval_type ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this2 ! 
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
       IF ( i > MIN( SIZE( this1%type ), SIZE( this2%type ) ) ) EXIT
       ok = ( this1%type(i) == this2%type(i) )
       IF ( .NOT. ok ) EXIT
    ENDDO
    !
  END FUNCTION eq_phyval_type 
  !
  !! pr&uuml;fe Komponente "val" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_phyval_val ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_phyval) , INTENT(IN) :: this2 ! 
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
       IF ( i > MIN( SIZE( this1%val ), SIZE( this2%val ) ) ) EXIT
       ok = ( this1%val(i) == this2%val(i) )
       IF ( .NOT. ok ) EXIT
    ENDDO
    !
  END FUNCTION eq_phyval_val 
  !
END MODULE m_ipds_phyval
! TailOfPackageModule ------------------------------------------------------
