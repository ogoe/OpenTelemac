! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Definition und Methoden fuer den Datentyp "t_mespos"</h2>
!! @author Jens J&uuml;rges
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_mespos.f90
!! <HR>
!! definition and methods for data type "t_mespos"             <BR>
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
!  01.07 : 2003-02-11 : J. Juerges : Routine index_physet_set_name wird neu ausgewertet
!  02.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-09-01 : J. Juerges : Korrekte Initialisierung fuer prn_op und trc_op
!  02.03 : 2007-01-05 : Schade     : Anpassungen an NT, z.B. Verzicht auf ==
!  02.04 : 2007-01-05 : Schade     : == fuer HP, SGI wiedereinfuehren
!  02.05 : 2007-03-02 : G. Lang    : ok_mespos_1 - FM falls ein Name mehrfach in mespos(:) auftritt
!  03.01 : 2007-03-13 : G. Lang    : Sampling Points muessen alle verschieden sein (ok-Methoden erweitert)
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Definition des Datentyps "t_mespos" fuer das Paket "ipds";
!!   <LI> Der Datentyp speichert die Daten eines "sampling_point"-Blocks
!!        einer Datei des Typs "ipds";
!!   <LI> Elementare Methoden auf Daten des Typs "t_mespos";
!!   <LI> Ein direkter Zugriff auf diese Daten/Methoden von anderen Paketen 
!!        aus ist nicht zul&auml;ssig.
!! </OL>
!! <HR>
!
MODULE m_ipds_mespos
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
  ! [A.2] BASIS-Modul fuer 2D-Koordinaten
  USE b_point_2d
  !
  ! [A.3] PACKAGE-Modul fuer Datenbeschreibung des Typs "physet"
  USE m_ipds_physet
  !
  USE m_ipds_phyval, ONLY : &
       ! Daten
       c_len_phyval_name
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
  !! max. L&auml;nge der Komponente "name" in "t_mespos"
  INTEGER , PUBLIC, PARAMETER :: c_len_mespos_name=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Definition einer Station mit den an ihr gemessenen physikalischen Werten <BR>
  !! name   : Bezeichnung     <BR>
  !! coor   : xy-Koordinaten  <BR>
  !! physet : physikalische Werte eines Satzes phys. Groessen
  TYPE , PUBLIC :: t_mespos
     PRIVATE
     CHARACTER (LEN=c_len_mespos_name) :: name ! 
     TYPE (t_point_2d)                 :: coor
     TYPE (t_physet)                   :: physet
  END TYPE t_mespos
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
  INTERFACE init_mespos
     MODULE PROCEDURE init_mespos_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE clear_mespos
     MODULE PROCEDURE clear_mespos_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_mespos_prn_lun
     MODULE PROCEDURE setup_mespos_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_mespos_trc_lun
     MODULE PROCEDURE setup_mespos_trc_lun_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_mespos" (Skalar, 1D-Array)
  INTERFACE new_mespos
     MODULE PROCEDURE new_mespos_0  ! Version fuer Skalar
     MODULE PROCEDURE new_mespos_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_mespos" (Skalar, 1D-Array)
  INTERFACE kill_mespos
     MODULE PROCEDURE kill_mespos_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_mespos_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_mespos" (Skalar, 1D-Array)
  INTERFACE ok_mespos
     MODULE PROCEDURE ok_mespos_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_mespos_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Datenobjekten "t_mespos" (Skalar, 1D-Array)
  INTERFACE print_mespos
     MODULE PROCEDURE print_mespos_0 ! Version fuer Skalar
     MODULE PROCEDURE print_mespos_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten
  INTERFACE print_mespos_static
     MODULE PROCEDURE print_mespos_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls
  INTERFACE print_mespos_all_errors
     MODULE PROCEDURE print_mespos_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "name" in "t_mespos" auf Benutzerwert
  INTERFACE set_mespos_name
     MODULE PROCEDURE set_mespos_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_mespos_name_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "coor" in "t_mespos" auf Benutzerwert
  INTERFACE set_mespos_coor
     MODULE PROCEDURE set_mespos_coor_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_mespos_coor_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "physet" in "t_mespos" auf Benutzerwert
  INTERFACE set_mespos_physet
     MODULE PROCEDURE set_mespos_physet_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_mespos_physet_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Hole Komponente "name" aus "t_mespos"
  INTERFACE get_mespos_name
     MODULE PROCEDURE get_mespos_name_0_0 ! Skalar
     MODULE PROCEDURE get_mespos_name_1_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "coor" aus "t_mespos"
  INTERFACE get_mespos_coor
     MODULE PROCEDURE get_mespos_coor_0_0 ! Skalar
     MODULE PROCEDURE get_mespos_coor_1_1 ! Vektor
     MODULE PROCEDURE get_mespos_coor_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "physet" aus "t_mespos"
  INTERFACE get_mespos_physet
     MODULE PROCEDURE get_mespos_physet_0_0 ! Skalar
     MODULE PROCEDURE get_mespos_physet_1_1 ! Vektor
     MODULE PROCEDURE get_mespos_physet_1_0 ! Vektor
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Hole Anzahl "physet"-"set"-Komponenten
  INTERFACE get_mespos_nof_physet_set
     MODULE PROCEDURE get_mespos_nof_physet_set_0 ! Skalar
     MODULE PROCEDURE get_mespos_nof_physet_set_1 ! Vektor
  END INTERFACE
  !! Hole Anzahl Varianten fuer ein "set"-Element der "physet"-Komponente
  INTERFACE get_mespos_nof_var_name
     MODULE PROCEDURE get_mespos_nof_var_name_0 ! Skalar
     MODULE PROCEDURE get_mespos_nof_var_name_1 ! Vektor
  END INTERFACE
  !! Hole Varianten-Namen fuer ein "set"-Element der "physet"-Komponente
  INTERFACE get_mespos_var_name
     MODULE PROCEDURE get_mespos_var_name_0_1 ! Skalar
  END INTERFACE
  !! Ermittle aus einer Liste von Datenobjekten das Objekt mit dem gegebenen Namen
  INTERFACE index_mespos_name
     MODULE PROCEDURE index_mespos_name_1_0
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  INTERFACE assign_mespos
     MODULE PROCEDURE assign_mespos_0_0
     MODULE PROCEDURE assign_mespos_1_0
     MODULE PROCEDURE assign_mespos_1_1
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
  !! Pr&uuml;fung zweier Datenobjekte "t_mespos" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_mespos_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_mespos_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_mespos_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_mespos_1_1  ! Vektor / Vektor
!xxx          MODULE PROCEDURE eq_physet_0_0  ! Skalar / Skalar
!xxx          MODULE PROCEDURE eq_physet_0_1  ! Skalar / Vektor 
!xxx          MODULE PROCEDURE eq_physet_1_0  ! Vektor / Skalar
!xxx          MODULE PROCEDURE eq_physet_1_1  ! Vektor / Vektor 
!>WIN-NT:     MODULE PROCEDURE eq_point_2d_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_point_2d_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE eq_point_2d_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_point_2d_1_1  ! Vektor / Vektor 
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_mespos" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_mespos_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_mespos_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_mespos_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_mespos_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_mespos
  PUBLIC :: clear_mespos
  PUBLIC :: setup_mespos_prn_lun
  PUBLIC :: setup_mespos_trc_lun
  PUBLIC :: new_mespos
  PUBLIC :: kill_mespos
  PUBLIC :: ok_mespos
  PUBLIC :: print_mespos
  PUBLIC :: print_mespos_static
  PUBLIC :: print_mespos_all_errors
  PUBLIC :: set_mespos_name
  PUBLIC :: set_mespos_coor
  PUBLIC :: set_mespos_physet
  PUBLIC :: get_mespos_name
  PUBLIC :: get_mespos_coor
  PUBLIC :: get_mespos_physet
  PUBLIC :: OPERATOR(==)
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: get_mespos_nof_physet_set
  PUBLIC :: get_mespos_nof_var_name
  PUBLIC :: get_mespos_var_name
  PUBLIC :: assign_mespos
  PUBLIC :: index_mespos_name
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
  CHARACTER (LEN=13), PARAMETER :: c_modname      = 'm_ipds_mespos'
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1
  !! Anzahl der Datenkomponenten des Typs t_mespos
  INTEGER           , PARAMETER :: c_nofcomp      = 3
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
  SUBROUTINE init_mespos_d ( )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !
    IF ( .NOT. initialised ) THEN
       !
       ! [1.1] Drucken des Copyright-Hinweises
       !
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "m_ipds_mespos" version 3.1 of 13.03 07'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_point_2d ( )
       IF ( no_error( ) ) CALL init_physet ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_mespos_all_errors ( ) 
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
  END SUBROUTINE init_mespos_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_mespos_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_mespos_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       prn_op  = c_op
       trc_lun = c_lun
       trc_op  = c_op
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_point_2d ( )
       IF ( no_error( ) ) CALL clear_physet ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_mespos_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_mespos_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_mespos_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_point_2d_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_physet_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_mespos_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_mespos_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_mespos_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_point_2d_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_physet_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_mespos_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_mespos_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_mespos) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_mespos_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%name   = REPEAT( ' ', LEN(this%name) )
       CALL new_point_2d ( this%coor )
       CALL new_physet ( this%physet )
    END IF
    !
  END SUBROUTINE new_mespos_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_mespos_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_mespos) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_mespos_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_mespos_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_mespos_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_mespos_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_mespos) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_mespos_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL kill_point_2d ( this%coor )
       IF ( no_error( ) ) CALL kill_physet ( this%physet )
       IF ( no_error( ) ) CALL new_mespos_0 ( this )
    END IF
    !
  END SUBROUTINE kill_mespos_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_mespos_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_mespos) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_mespos_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_mespos_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_mespos_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_mespos_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_mespos_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_mespos_name( this )
       l_ok(2)  = ok_mespos_coor( this )
       l_ok(3)  = ok_mespos_physet( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_mespos_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_mespos_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_mespos_1' 
    ! Hilfsvariablen
    CHARACTER (LEN=10) :: l_char ! 
    CHARACTER (LEN=30) :: g_char ! 
    INTEGER            :: i, j   ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_mespos_0 ( this(i) )
       END DO
       ! Pruefe nunmehr die Verschiedenheit der Komponente "name" ---------
       DO i=1,SIZE(this)
          DO j=i+1,SIZE(this)
             IF ( this(i)%name == this(j)%name ) THEN
                CALL setup_error_act ( all_errors, 6015, c_upname, c_modname )
                WRITE(l_char,'(I10)') i ; CALL setup_error_act ( '<index1>', l_char )
                WRITE(l_char,'(I10)') j ; CALL setup_error_act ( '<index2>', l_char )
                CALL setup_error_act ( '<name1>', TRIM(this(i)%name) )
                CALL setup_error_act ( '<name2>', TRIM(this(j)%name) )
             END IF
          END DO
       END DO
       ! Pruefe ausserdem die Verschiedenheit der Komponente "coor" ------
       DO i=1,SIZE(this)
          DO j=i+1,SIZE(this)
             IF ( this(i)%coor == this(j)%coor ) THEN
                CALL setup_error_act ( all_errors, 6016, c_upname, c_modname )
                WRITE(l_char,'(I10)') i ; CALL setup_error_act ( '<index1>', l_char )
                WRITE(l_char,'(I10)') j ; CALL setup_error_act ( '<index2>', l_char )
                CALL setup_error_act ( '<name1>', TRIM(this(i)%name) )
                CALL setup_error_act ( '<name2>', TRIM(this(j)%name) )
                WRITE(g_char,'(2G15.8)') get_point_2d_x(this(i)%coor),get_point_2d_y(this(i)%coor)
                CALL setup_error_act ( '<coor1>', g_char )
                WRITE(g_char,'(2G15.8)') get_point_2d_x(this(j)%coor),get_point_2d_y(this(j)%coor)
                CALL setup_error_act ( '<coor2>', g_char )
             END IF
          END DO
       END DO
    END IF
    !
  END FUNCTION ok_mespos_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_mespos_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_mespos_0' 
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
       IF ( no_error( ) ) CALL print_mespos_name( this )
       IF ( no_error( ) ) CALL print_mespos_coor( this )
       IF ( no_error( ) ) CALL print_mespos_physet( this )
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
8000 FORMAT('# Beginn Objekt t_mespos -------------------------------------')
8001 FORMAT('# Ende   Objekt t_mespos -------------------------------------')
    !
  END SUBROUTINE print_mespos_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_mespos_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_mespos_1' 
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
          IF ( no_error( ) ) CALL print_mespos_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_mespos_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_mespos_static_d ( )
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='print_mespos_static_d' 
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
       IF ( no_error( ) ) CALL print_mespos_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls m_ipds_mespos         ',/ &
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
  END SUBROUTINE print_mespos_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_mespos_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=25), PARAMETER :: c_upname='print_mespos_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_mespos_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_mespos_name_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_mespos)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%name = REPEAT( ' ', LEN(this%name) )
    this%name = val(1:MIN(LEN(val),LEN(this%name)))
    !
  END SUBROUTINE set_mespos_name_0_0
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_mespos_name_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_mespos)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_mespos_name_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_mespos_name_1_0
  !
  !! weise der Komponente "coor" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_mespos_coor_0_0 ( this,  val )
    !! Datenobjekt (Skalar)
    TYPE (t_mespos)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "coor"
    TYPE (t_point_2d), INTENT(IN)    :: val  ! 
    !
    this%coor = val
    !
  END SUBROUTINE set_mespos_coor_0_0
  !
  !! weise der Komponente "coor" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_mespos_coor_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_mespos)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "coor"
    TYPE (t_point_2d), INTENT(IN)    :: val     ! 
    !
    this%coor = val
    !
  END SUBROUTINE set_mespos_coor_1_0
  !
  !! weise der Komponente "physet" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_mespos_physet_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_mespos), INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "physet"
    TYPE (t_physet), INTENT(IN)    :: val  ! 
    !
    CALL assign_physet ( this%physet , val )
    !
  END SUBROUTINE set_mespos_physet_0_0
  !
  !! weise der Komponente "physet" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_mespos_physet_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_mespos), INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "physet"
    TYPE (t_physet), INTENT(IN)    :: val     ! 
    !
    CALL  assign_physet ( this%physet , val )
    !
  END SUBROUTINE set_mespos_physet_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_mespos_name_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_mespos)     , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "name" (Skalar)
    CHARACTER (LEN=c_len_mespos_name) :: val  ! 
    !
    val = this%name
    !
  END FUNCTION get_mespos_name_0_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_mespos_name_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_mespos)     , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "name"
    CHARACTER (LEN=c_len_mespos_name) :: val(SIZE(this))  ! 
    !
    val = this%name
    !
  END FUNCTION get_mespos_name_1_1
  !
  !! hole die Komponente "coor" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_mespos_coor_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "coor" (Skalar)
    TYPE (t_point_2d) :: val  ! 
    !
    CALL new_point_2d( val )
    IF( no_error() ) val = this%coor
    !
  END FUNCTION get_mespos_coor_0_0
  !
  !! hole die Komponente "coor" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_mespos_coor_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_mespos) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "coor"
    TYPE (t_point_2d) :: val(SIZE(this))  ! 
    !
    CALL new_point_2d( val )
    IF( no_error() ) val = this%coor
    !
  END FUNCTION get_mespos_coor_1_1
  !
  !! hole die Komponente "coor" aus einem vektoriellen Datenobjekt
  FUNCTION get_mespos_coor_1_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_mespos)  , INTENT(IN)  :: this(:)
    !! Name des gesuchten Datenobjekts
    CHARACTER (LEN=*), INTENT(IN)  :: name
    !! R&uuml;ckgabewert "coor"
    TYPE (t_point_2d) :: val
    !! Index
    INTEGER :: idx
    !
    CALL new_point_2d( val )
    IF( no_error() ) idx = index_mespos_name( this, name )
    IF( no_error() ) val = this(idx)%coor
    !
  END FUNCTION get_mespos_coor_1_0
  !
  !! hole die Komponente "physet" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_mespos_physet_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "physet" (Skalar)
    TYPE (t_physet) :: val  ! 
    !
    CALL new_physet( val )
    IF( no_error() ) CALL assign_physet ( val, this%physet )
    !
  END FUNCTION get_mespos_physet_0_0
  !
  !! hole die Komponente "physet" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_mespos_physet_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_mespos) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "physet"
    TYPE (t_physet) :: val(SIZE(this))  ! 
    !
    CALL new_physet( val )
    IF( no_error() ) CALL assign_physet ( val, this%physet )
    !
  END FUNCTION get_mespos_physet_1_1
  !
  !! hole die Komponente "physet" aus einem vektoriellen Datenobjekt
  FUNCTION get_mespos_physet_1_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_mespos)  , INTENT(IN)  :: this(:)
    !! Name des gesuchten Datenobjekts
    CHARACTER (LEN=*), INTENT(IN)  :: name
    !! R&uuml;ckgabewert "physet"
    TYPE (t_physet) :: val
    !! Index
    INTEGER :: idx
    !
    CALL new_physet( val )
    IF( no_error() ) idx = index_mespos_name( this, name )
    IF( no_error() ) CALL assign_physet ( val, this(idx)%physet )
    !
  END FUNCTION get_mespos_physet_1_0
  !
  !! Hole Anzahl "physet"-"set"-Komponenten eines Datenobjekts. <BR>
  !! Die "physet"-"set"-Komponente kann durch einen Namen eingeschraenkt werden
  FUNCTION get_mespos_nof_physet_set_0 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_mespos),   INTENT(IN)           :: this
    !! Optional: Name der "physet"-"set"-Komponente
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: phyval_name
    !! R&uuml;ckgabewert: Anzahl
    INTEGER :: val
    !! Index
    INTEGER :: idx
    !
    IF( PRESENT( phyval_name ) ) THEN
       val = 0
       idx = index_physet_set_name( this%physet, phyval_name )
       IF( idx > 0 ) val = 1
    ELSE
       val = get_physet_nof_set( this%physet )
    ENDIF
    !
  END FUNCTION get_mespos_nof_physet_set_0
  !
  !! Hole Anzahl "physet"-"set"-Komponenten mehrerer Datenobjekte. <BR>
  !! Die "physet"-"set"-Komponente kann durch einen Namen eingeschraenkt werden
  FUNCTION get_mespos_nof_physet_set_1 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekte (Vektor)
    TYPE (t_mespos),   INTENT(IN)           :: this(:)
    !! Optional: Name der "physet"-"set"-Komponente
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: phyval_name
    !! R&uuml;ckgabewert: Anzahl
    INTEGER :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       val(i) = get_mespos_nof_physet_set_0( this(i), phyval_name )
    END DO
    !
  END FUNCTION get_mespos_nof_physet_set_1
  !
  !! Hole Anzahl Varianten fuer ein "set"-Element der physet-Komponente eines Datenobjekts. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert.
  FUNCTION get_mespos_nof_var_name_0 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_mespos),   INTENT(IN) :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Anzahl
    INTEGER                       :: val
    !
    val = get_physet_nof_var_name( this%physet, phyval_name )
    !
  END FUNCTION get_mespos_nof_var_name_0
  !
  !! Hole Anzahl Varianten fuer ein "set"-Element der physet-Komponente mehrerer Datenobjekte. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert.
  FUNCTION get_mespos_nof_var_name_1 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_mespos),   INTENT(IN) :: this(:)
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Anzahl
    INTEGER                       :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE( this )
       val(i) = get_mespos_nof_var_name_0( this(i), phyval_name )
    ENDDO
    !
  END FUNCTION get_mespos_nof_var_name_1
  !
  !! Hole Varianten-Namen fuer ein "set"-Element der physet-Komponente eines Datenobjekts. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert. <BR>
  !! Das Ergebnisfeld wird hier mit Speicher belegt.
  FUNCTION get_mespos_var_name_0_1 ( this, phyval_name ) &
       RESULT( var_name ) 
    !! Datenobjekt (Skalar)
    TYPE (t_mespos),    INTENT(IN) :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*),  INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Varianten-Namen
    CHARACTER (LEN=c_len_phyval_name), POINTER    :: var_name(:)
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='get_mespos_var_name_0_1' 
    !! Anzahl Varianten-Namen
    INTEGER :: nof_var_name
    !! Statusvariable
    INTEGER :: stat
    !
    nof_var_name = get_mespos_nof_var_name( this, phyval_name )
    NULLIFY( var_name )
    IF ( no_error() .AND. nof_var_name > 0 ) THEN
       ALLOCATE( var_name( nof_var_name ), STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 9010, c_upname, c_modname, stat )
       IF ( no_error() ) var_name = get_physet_var_name( this%physet, phyval_name )
    ENDIF
    !
  END FUNCTION get_mespos_var_name_0_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_mespos_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = eq_mespos_name   ( this1, this2 )
    l_ok(2)  = eq_mespos_coor   ( this1, this2 )
    l_ok(3)  = eq_mespos_physet ( this1, this2 )
    ok       = ALL( l_ok )
    !
  END FUNCTION eq_mespos_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_mespos_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_mespos_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_mespos_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_mespos_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_mespos_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_mespos_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_mespos_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_mespos_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_mespos_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_mespos_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_mespos_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_mespos_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_mespos_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_mespos_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_mespos_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_mespos_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_mespos) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_mespos_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-ASSIGNMENT(=)-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_mespos_0_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_mespos), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_mespos), INTENT(INOUT) :: target_this
    !
    target_this%name   = source_this%name
    target_this%coor   = source_this%coor
    CALL assign_physet ( target_this%physet, source_this%physet )
    !
  END SUBROUTINE assign_mespos_0_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_mespos_1_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_mespos), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_mespos), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(target_this)
       CALL assign_mespos_0_0 ( target_this(i), source_this )
    END DO
    !
  END SUBROUTINE assign_mespos_1_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen der Objektliste 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_mespos_1_1 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_mespos), INTENT(IN)    :: source_this(:)
    !! Zielobjekt
    TYPE (t_mespos), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, MIN( SIZE(source_this), SIZE(target_this) )
       CALL assign_mespos_0_0 ( target_this(i), source_this(i) )
    END DO
    !
  END SUBROUTINE assign_mespos_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INDEX-Methoden <<< [ERR_NO = 21000 bis 21999]
  ! ----------------------------------------------------------------------
  !
  !! Ermittle aus einer Liste von Datenobjekten das Objekt mit dem gegebenen Namen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION index_mespos_name_1_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_mespos)  , INTENT(IN)  :: this(:)
    !! Name des gesuchten Datenobjekts
    CHARACTER (LEN=*), INTENT(IN)  :: name
    !! R&uuml;ckgabewert Index
    INTEGER :: val
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='index_mespos_name_1_0'
    !! Zaehler
    INTEGER :: i
    !
    val = 0
    DO i = 1, SIZE( this )
       IF( TRIM( get_mespos_name( this(i) ) ) == TRIM( name ) ) val = i
    END DO
    !
    IF ( val < 1 ) THEN
       !$OMP critical
       CALL setup_error_act ( all_errors(:), 21000, c_upname, c_modname )
       CALL setup_error_act ( '<mespos_name>',   TRIM( name ) )
       !$OMP end critical
    ENDIF
    !
  END FUNCTION index_mespos_name_1_0
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
       WRITE(*,*) ' *** Warnung *** Modul "m_ipds_mespos" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_mespos ausfuehren'
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
  SUBROUTINE init_mespos_all_errors ( )
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
               '--> INIT_mespos ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_mespos ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_mespos"\n'//&
               'Typ-Komponente = "name"\n'//&
               'Die Komponente darf nicht leer sein\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6015 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_mespos"\n'//&
               'Typ-Komponente = "name" kommt mehrfach vor\n'//&
               'Pos = <index1> , Name = <name1>\n'//&
               'Pos = <index2> , Name = <name2>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6016 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_mespos"\n'//&
               'Typ-Komponente = "name" kommt mehrfach vor\n'//&
               'Pos = <index1> , Name = <name1>\n'//&
               'Pos = <index2> , Name = <name2>\n'//&
               'Koordinaten 1  , Wert = <coor1>\n'//&
               'Koordinaten 2  , Wert = <coor2>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_mespos"\n'//&
               'Typ-Komponente = "coor"\n'//&
               'Die Komponente ist defekt im Sinne der point_2d-Definition\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_mespos"\n'//&
               'Typ-Komponente = "physet"\n'//&
               'Die Komponente ist defekt im Sinne der physet-Definition\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "m_ipds_mespos" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "m_ipds_mespos" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "m_ipds_mespos" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_mespos"\n'//&
               'Typ-Komponente = "name"\n'//&
               '--> Code in Modul "m_ipds_mespos" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_mespos"\n'//&
               'Typ-Komponente = "coor"\n'//&
               '--> Code in Modul "m_ipds_mespos" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_mespos"\n'//&
               'Typ-Komponente = "physet"\n'//&
               '--> Code in Modul "m_ipds_mespos" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "m_ipds_mespos"\n'//&
               '--> Code in Modul "m_ipds_mespos" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Rueckgabefeld "var_name"\n'//&
               '--> Code in Modul "m_ipds_mespos" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: INDEX-Methoden\n'//&
               'Aus einer Liste von Datenobjekten des Typs "t_mespos"\n'//&
               'konnte keines mit folgendem Namen entdeckt werden:\n'//&
               'Name des Objektes = <mespos_name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
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
  END SUBROUTINE init_mespos_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_mespos_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='clear_mespos_all_errors'
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_mespos_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_mespos_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_mespos) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_mespos_name'
    !
    ok = ( LEN_TRIM( this%name ) > 0 )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
  END FUNCTION ok_mespos_name
  !
  !! Pr&uuml;fe, ob die Komponente "coor" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_mespos_coor ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_mespos) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_mespos_coor'
    !
    ok = ( ok_point_2d ( this%coor ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
  END FUNCTION ok_mespos_coor
  !
  !! Pr&uuml;fe, ob die Komponente "physet" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_mespos_physet ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_mespos) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_mespos_physet'
    !
    ok = ( ok_physet ( this%physet ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    !
  END FUNCTION ok_mespos_physet
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_mespos_name ( this )
    !! Datenobjekt
    TYPE (t_mespos) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_mespos_name'
    !! Statusvariable
    INTEGER :: stat ! 
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
          '# name = ',A,/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_mespos_name
  !
  !! Drucke den Inhalt der Komponente "coor" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_mespos_coor ( this )
    !! Datenobjekt
    TYPE (t_mespos) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_mespos_coor'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
    CALL print_point_2d ( this%coor )
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8001,     & 
           IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente coor  - - - - - - - - - - - - - - - - ')
8001 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_mespos_coor
  !
  !! Drucke den Inhalt der Komponente "physet" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_mespos_physet ( this )
    !! Datenobjekt
    TYPE (t_mespos) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_mespos_physet'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
    CALL print_physet ( this%physet )
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8001,     & 
           IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente physet  - - - - - - - - - - - - - - - ')
8001 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_mespos_physet
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_mespos_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%name == this2%name )
    !
  END FUNCTION eq_mespos_name
  !
  !! pr&uuml;fe Komponente "coor" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_mespos_coor ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%coor == this2%coor )
    !
  END FUNCTION eq_mespos_coor 
  !
  !! pr&uuml;fe Komponente "physet" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_mespos_physet ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_mespos) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%physet == this2%physet )
!>WIN-NT:    ok = eq_physet_0_0 ( this1%physet, this2%physet )
    !
  END FUNCTION eq_mespos_physet 
  !
END MODULE m_ipds_mespos
! TailOfPackageModule ------------------------------------------------------
