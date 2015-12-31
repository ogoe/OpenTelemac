! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Definition und Methoden fuer den Datentyp "t_region"</h2>
!! @author Jens J&uuml;rges
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_region.f90
!! <HR>
!! definition and methods for data type "t_region"             <BR>
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
!  02.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-09-01 : J. Juerges : Korrekte Initialisierung fuer prn_op und trc_op
!  02.03 : 2007-03-02 : G. Lang    : ok_region_1 - FM falls ein Name mehrfach in region(:) auftritt
!  03.01 : 2007-03-13 : G. Lang    : neue Hauptversionsnummer 3
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Definition des Datentyps "t_region" fuer das Paket "ipds";
!!   <LI> Der Datentyp speichert die Daten eines "region"-Blocks
!!        einer Datei des Typs "ipds";
!!   <LI> Elementare Methoden auf Daten des Typs "t_region";
!!   <LI> Ein direkter Zugriff auf diese Daten/Methoden von anderen Paketen 
!!        aus ist nicht zul&auml;ssig.
!! </OL>
!! <HR>
!
MODULE m_ipds_region
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
       ok_point_2d,            &
       print_point_2d,         &
       !   Operatoren
       OPERATOR(==)
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
  !! max. L&auml;nge der Komponente "name" in "t_region"
  INTEGER , PUBLIC, PARAMETER :: c_len_region_name=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Definition einer Region mit ihren Grenzkoordinaten
  !! name   : Bezeichnung
  !! border : Grenzkoordinaten
  TYPE , PUBLIC :: t_region
     PRIVATE
     CHARACTER (LEN=c_len_region_name) :: name      ! 
     TYPE (t_point_2d)       , POINTER :: border(:) ! 
  END TYPE t_region
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
  INTERFACE init_region
     MODULE PROCEDURE init_region_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE clear_region
     MODULE PROCEDURE clear_region_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_region_prn_lun
     MODULE PROCEDURE setup_region_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_region_trc_lun
     MODULE PROCEDURE setup_region_trc_lun_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_region" (Skalar, 1D-Array)
  INTERFACE new_region
     MODULE PROCEDURE new_region_0  ! Version fuer Skalar
     MODULE PROCEDURE new_region_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_region" (Skalar, 1D-Array)
  INTERFACE kill_region
     MODULE PROCEDURE kill_region_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_region_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_region" (Skalar, 1D-Array)
  INTERFACE ok_region
     MODULE PROCEDURE ok_region_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_region_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Datenobjekten "t_region" (Skalar, 1D-Array)
  INTERFACE print_region
     MODULE PROCEDURE print_region_0 ! Version fuer Skalar
     MODULE PROCEDURE print_region_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten
  INTERFACE print_region_static
     MODULE PROCEDURE print_region_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls
  INTERFACE print_region_all_errors
     MODULE PROCEDURE print_region_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "name" in "t_region" auf Benutzerwert
  INTERFACE set_region_name
     MODULE PROCEDURE set_region_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_region_name_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "border" in "t_region" auf Benutzerwert
  INTERFACE set_region_border
     MODULE PROCEDURE set_region_border_0_1 ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_region_border_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Hole Komponente "name" aus "t_region"
  INTERFACE get_region_name
     MODULE PROCEDURE get_region_name_0_0 ! Skalar
     MODULE PROCEDURE get_region_name_1_1 ! Vektor
  END INTERFACE
  !! Holen der Anzahl Punkte in der Komponente border
  INTERFACE get_region_nof_border
     MODULE PROCEDURE get_region_nof_border_0 ! Skalar
     MODULE PROCEDURE get_region_nof_border_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "border" aus "t_region"
  INTERFACE get_region_border
     MODULE PROCEDURE get_region_border_0_1 ! Skalar
  END INTERFACE
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  INTERFACE assign_region
     module procedure assign_region_0_0
     module procedure assign_region_1_0
     module procedure assign_region_1_1
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Ermittle aus einer Liste von Datenobjekten das Objekt mit dem gegebenen Namen
  INTERFACE index_region_name
     MODULE PROCEDURE index_region_name_1_0
  END INTERFACE
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
  !! Pr&uuml;fung zweier Datenobjekte "t_region" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_region_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_region_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_region_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_region_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_region" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_region_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_region_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_region_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_region_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_region
  PUBLIC :: clear_region
  PUBLIC :: setup_region_prn_lun
  PUBLIC :: setup_region_trc_lun
  PUBLIC :: new_region
  PUBLIC :: kill_region
  PUBLIC :: ok_region
  PUBLIC :: print_region
  PUBLIC :: print_region_static
  PUBLIC :: print_region_all_errors
  PUBLIC :: set_region_name
  PUBLIC :: set_region_border
  PUBLIC :: get_region_name
  PUBLIC :: get_region_nof_border
  PUBLIC :: get_region_border
  PUBLIC :: OPERATOR(==)
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: assign_region
  PUBLIC :: index_region_name
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
  CHARACTER (LEN=13), PARAMETER :: c_modname      = 'm_ipds_region'
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1
  !! Anzahl der Datenkomponenten des Typs t_region
  INTEGER           , PARAMETER :: c_nofcomp      = 2
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
  SUBROUTINE init_region_d ( )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "m_ipds_region" version 3.1 of 13.03 07'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_point_2d ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_region_all_errors ( ) 
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
  END SUBROUTINE init_region_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_region_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_region_all_errors ( ) 
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
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_region_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_region_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_region_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_point_2d_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_region_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_region_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_region_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_point_2d_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_region_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_region_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_region) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_region_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%name  = REPEAT( ' ', LEN(this%name) )
       NULLIFY ( this%border )
    END IF
    !
  END SUBROUTINE new_region_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_region_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_region) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_region_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_region_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_region_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_region_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_region) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_region_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL dealloc_region_border( this )
       IF ( no_error( ) ) CALL new_region_0 ( this )
    END IF
    !
  END SUBROUTINE kill_region_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_region_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_region) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_region_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_region_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_region_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_region_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_region) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_region_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_region_name( this )
       l_ok(2)  = ok_region_border( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_region_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_region_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_region) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_region_1' 
    ! Hilfsvariablen
    CHARACTER (LEN=10) :: l_char ! 
    INTEGER            :: i, j   ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_region_0 ( this(i) )
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
    END IF
    !
  END FUNCTION ok_region_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_region_0  ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_region) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_region_0' 
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
       IF ( no_error( ) ) CALL print_region_name( this )
       IF ( no_error( ) ) CALL print_region_border( this )
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
8000 FORMAT('# Beginn Objekt t_region -------------------------------------')
8001 FORMAT('# Ende   Objekt t_region -------------------------------------')
    !
  END SUBROUTINE print_region_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_region_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_region) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_region_1' 
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
          IF ( no_error( ) ) CALL print_region_0 ( this(i) )
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
  END SUBROUTINE print_region_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_region_static_d ( )
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='print_region_static_d' 
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
       IF ( no_error( ) ) CALL print_region_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls m_ipds_region         ',/ &
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
  END SUBROUTINE print_region_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_region_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=25), PARAMETER :: c_upname='print_region_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_region_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_region_name_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_region)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*), INTENT(IN)    :: val  ! 
    !
    this%name = REPEAT( ' ', LEN(this%name) )
    this%name = val(1:MIN(LEN(val),LEN(this%name)))
    !
  END SUBROUTINE set_region_name_0_0
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_region_name_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_region)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*), INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_region_name_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_region_name_1_0
  !
  !! weise der dynamischen Komponente "border" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_region_border_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_region)  , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "border"
    TYPE (t_point_2d), INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='set_region_border_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_region_border ( this            )
       IF ( no_error( ) ) CALL alloc_region_border   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_region_border    ( this            )
       IF ( no_error( ) ) this%border = val
    END IF
    !
  END SUBROUTINE set_region_border_0_1
  !
  !! weise der dynamischen Komponente "border" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_region_border_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_region)  , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "border"
    TYPE (t_point_2d), INTENT(IN)    :: val(:)  ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_region_border_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_region_border_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_region_name_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_region)      , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert "name" (Skalar)
    CHARACTER (LEN=c_len_region_name) :: val  ! 
    !
    val = this%name
    !
  END FUNCTION get_region_name_0_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_region_name_1_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_region)      , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert "name"
    CHARACTER (LEN=c_len_region_name) :: val(SIZE(this))  ! 
    !
    val = this%name
    !
  END FUNCTION get_region_name_1_1
  !
  !! hole die Anzahl Punkte in der Komponente "border"
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_region_nof_border_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_region), INTENT(IN) :: this
    !! R&uuml;ckgabewert Anzahl "border"
    INTEGER                     :: val
    !
    IF( ASSOCIATED( this%border ) ) THEN
       val = SIZE(this%border)
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_region_nof_border_0
  !
  !! hole die Anzahl Punkte in der Komponente "border"
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_region_nof_border_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_region), INTENT(IN) :: this(:)
    !! R&uuml;ckgabewert Anzahl "border"
    INTEGER                     :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       val(i) = get_region_nof_border_0( this(i) )
    END DO
    !
  END FUNCTION get_region_nof_border_1
  !
  !! hole die dynamische Feld-Komponente "border" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_region_border_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_region), INTENT(IN) :: this     ! 
    !! R&uuml;ckgabewert "border" (Vektor)
    TYPE (t_point_2d)           :: val(SIZE(this%border)) ! 
    !
    CALL new_point_2d( val )
    IF( no_error() ) val = this%border
    !
  END FUNCTION get_region_border_0_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_region_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_region) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_region) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = eq_region_name   ( this1, this2 )
    l_ok(2)  = eq_region_border ( this1, this2 )
    ok       = ALL( l_ok )
    !
  END FUNCTION eq_region_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_region_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_region) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_region) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_region_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_region_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_region_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_region) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_region) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_region_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_region_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_region_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_region) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_region) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_region_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_region_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_region_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_region) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_region) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_region_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_region_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_region) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_region) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_region_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_region_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_region) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_region) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_region_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_region_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_region) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_region) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_region_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-ASSIGNMENT(=)-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_region_0_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_region), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_region), INTENT(INOUT) :: target_this
    !
    target_this%name = source_this%name
    IF ( ASSOCIATED ( source_this%border ) ) THEN
       CALL set_region_border ( target_this, source_this%border )
    ELSE
       CALL dealloc_region_border ( target_this )
    ENDIF
    !
  END SUBROUTINE assign_region_0_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_region_1_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_region), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_region), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(target_this)
       CALL assign_region_0_0 ( target_this(i), source_this )
    END DO
    !
  END SUBROUTINE assign_region_1_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen der Objektliste 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_region_1_1 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_region), INTENT(IN)    :: source_this(:)
    !! Zielobjekt
    TYPE (t_region), INTENT(INOUT) :: target_this(:)
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='assign_region_1_1'
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, MIN( SIZE(source_this), SIZE(target_this) )
       CALL assign_region_0_0 ( target_this(i), source_this(i) )
    END DO
    !
  END SUBROUTINE assign_region_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INDEX-Methoden <<< [ERR_NO = 21000 bis 21999]
  ! ----------------------------------------------------------------------
  !
  !! Ermittle aus einer Liste von Datenobjekten das Objekt mit dem gegebenen Namen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION index_region_name_1_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_region)  , INTENT(IN)  :: this(:)
    !! Name des gesuchten Datenobjekts
    CHARACTER (LEN=*), INTENT(IN)  :: name
    !! R&uuml;ckgabewert Index
    INTEGER :: val
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='index_region_name_1_0'
    !! Zaehler
    INTEGER :: i
    !
    val = 0
    DO i = 1, SIZE( this )
       IF( TRIM( get_region_name( this(i) ) ) == TRIM( name ) ) val = i
    END DO
    !
    IF ( val < 1 ) THEN
       !$OMP critical
       CALL setup_error_act ( all_errors(:), 21000, c_upname, c_modname )
       CALL setup_error_act ( '<region_name>',   TRIM( name ) )
       !$OMP end critical
    ENDIF
    !
  END FUNCTION index_region_name_1_0
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
  FUNCTION ok_initialised ( upname ) &
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
       WRITE(*,*) ' *** Warnung *** Modul "m_ipds_region" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_region ausfuehren'
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
  SUBROUTINE init_region_all_errors ( )
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
               '--> INIT_region ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_region ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_region"\n'//&
               'Typ-Komponente = "border"\n'//&
               '--> Code in Modul "m_ipds_region" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_region"\n'//&
               'Typ-Komponente = "border"\n'//&
               '--> Code in Modul "m_ipds_region" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_region"\n'//&
               'Typ-Komponente = "name"\n'//&
               'Die Komponente darf nicht leer sein\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6015 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_region"\n'//&
               'Typ-Komponente = "name" kommt mehrfach vor\n'//&
               'Pos = <index1> , Name = <name1>\n'//&
               'Pos = <index2> , Name = <name2>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_region"\n'//&
               'Typ-Komponente = "border"\n'//&
               'Mit der Komponente sind keine Daten assoziiert\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6021 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_region"\n'//&
               'Typ-Komponente = "border"\n'//&
               'Mind. ein Element der Komp. ist defekt im Sinne der Punktdef. von "point_2d"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6022 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_region"\n'//&
               'Name           = <region>\n'//&
               'Typ-Komponente = "border"\n'//&
               'Die Komponente muss mindestens drei Punkte enthalten\n'//&
               '--> Keyzeile "border_point" mind. drei mal verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "m_ipds_region" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "m_ipds_region" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "m_ipds_region" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_region"\n'//&
               'Typ-Komponente = "name"\n'//&
               '--> Code in Modul "m_ipds_region" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_region"\n'//&
               'Typ-Komponente = "border"\n'//&
               '--> Code in Modul "m_ipds_region" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "m_ipds_region"\n'//&
               '--> Code in Modul "m_ipds_region" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: INDEX-Methoden\n'//&
               'Aus einer Liste von Datenobjekten des Typs "t_region"\n'//&
               'konnte keines mit folgendem Namen entdeckt werden:\n'//&
               'Name des Objektes = <region_name>\n'//&
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
  END SUBROUTINE init_region_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_region_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_region_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "border" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_region_border ( this, idim )
    !! Datenobjekt
    TYPE (t_region) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "border"
    INTEGER         , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='alloc_region_border'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%border(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 4020, c_upname, c_modname, stat )
    !
  END SUBROUTINE alloc_region_border
  !
  !! Initialisieren der Feld-Komponente "border" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_region_border ( this )
    !! Datenobjekt
    TYPE (t_region) , INTENT(INOUT) :: this   ! 
    !! Initialisierungswert border
    TYPE (t_point_2d) :: c_var
    !
    CALL new_point_2d  ( c_var )
    this%border = c_var
    CALL kill_point_2d ( c_var )
    !
  END SUBROUTINE init_region_border
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "border" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_region_border ( this )
    !! Datenobjekt
    TYPE (t_region) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='dealloc_region_border'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%border ) ) THEN
       DEALLOCATE ( this%border, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5020, c_upname, c_modname, stat )
       NULLIFY ( this%border ) 
    END IF
    !
  END SUBROUTINE dealloc_region_border
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_region_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_region) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_region_name'
    !
    ok = ( LEN_TRIM( this%name ) > 0 )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
  END FUNCTION ok_region_name
  !
  !! Pr&uuml;fe, ob die Komponente "border" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_region_border ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_region) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_region_border'
    !
    ok = ( ASSOCIATED( this%border ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
    IF ( ok ) THEN
       ok = ( ALL( ok_point_2d( this%border ) ) )
       IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6021, c_upname, c_modname )
    ENDIF
    !
    IF ( ok ) THEN
       ok = ( SIZE( this%border ) > 2 )
       IF ( .NOT. ok ) THEN
          !
          !$OMP critical
          CALL setup_error_act ( all_errors(:), 6022, c_upname, c_modname )
          CALL setup_error_act ( '<region>', this%name )
          !$OMP end critical
          !
       END IF
    END IF
    !
  END FUNCTION ok_region_border
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_region_name ( this )
    !! Datenobjekt
    TYPE (t_region) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_region_name'
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
  END SUBROUTINE print_region_name
  !
  !! Drucke den Inhalt der Komponente "border" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_region_border ( this )
    !! Datenobjekt
    TYPE (t_region) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_region_border'
    !! Statusvariable
    INTEGER :: stat
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8000,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
    IF ( ASSOCIATED (this%border) ) THEN
       !
       CALL print_point_2d ( this%border )
       !
    ELSE
       !
       WRITE ( &
            UNIT    = prn_lun,  &
            FMT     = 8001,     & 
            IOSTAT  = stat )    &
            '<..no.border.associated..>'
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
          '# Inhalt der Komponente border  - - - - - - - - - - - - - - - ')
8001 FORMAT ( &
          '# ',A)
8002 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_region_border
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_region_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_region) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_region) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%name == this2%name )
    !
  END FUNCTION eq_region_name
  !
  !! pr&uuml;fe Komponente "border" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_region_border ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_region) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_region) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ALL( this1%border == this2%border )
    !
  END FUNCTION eq_region_border 
  !
END MODULE m_ipds_region
! TailOfPackageModule ------------------------------------------------------
