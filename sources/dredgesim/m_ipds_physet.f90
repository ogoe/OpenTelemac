! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Definition und Methoden fuer den Datentyp "t_physet"</h2>
!! @author Jens J&uuml;rges
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_physet.f90
!! <HR>
!! definition and methods for data type "t_physet"             <BR>
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
!  01.07 : 2003-02-11 : J. Juerges : index_phyval_name produziert keinen Fehler mehr
!  01.08 : 2003-11-13 : J. Juerges : update_physet_var liefert all_var_ex=T fuer Fraktionen,
!                                      auch, wenn die Fraktion nicht im physet enthalten ist.
!  01.09 : 2003-11-13 : J. Juerges : Testausgaben entfernt
!  02.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-08-25 : J. Juerges : Korrektur in ok_physet_fraction_sum
!  02.03 : 2005-09-01 : J. Juerges : Korrekte Initialisierung fuer prn_op und trc_op
!  03.01 : 2007-03-13 : G. Lang    : neue Hauptversionsnummer 3
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Definition des Datentyps "t_physet" fuer das Paket "ipds";
!!   <LI> Der Datentyp speichert die Infos und Werte einer Gruppe von
!!        phys. Groessen;
!!   <LI> Elementare Methoden auf Daten des Typs "t_physet";
!!   <LI> Ein direkter Zugriff auf diese Daten/Methoden von anderen Paketen 
!!        aus ist nicht zul&auml;ssig.
!! </OL>
!! <HR>
!
MODULE m_ipds_physet
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
  ! [A.1.2] BASIS-Modul mit Dimensionsbezeichnung 
  ! [A.2] PACKAGE-Modul fuer Datenbeschreibung des Typs "phyval"
  USE m_ipds_phyval, ONLY :     &
       ! Typdefinitionen
       t_phyval,                &
       ! Daten
       c_len_phyval_name,       &
       !   Routinen / Interfaces
       init_phyval,             &
       clear_phyval,            &
       setup_phyval_prn_lun,    &
       setup_phyval_trc_lun,    &
       new_phyval,              &
       assign_phyval,           &
       kill_phyval,             &
       ok_phyval,               &
       print_phyval,            &
       get_phyval_nof_frac,     &
       get_phyval_nof_var_name, &
       get_phyval_name,         &
       get_phyval_var_name,     &
       get_phyval_nof_val,      &
       get_phyval_val,          &
       index_phyval_name,       &
       is_phyval_frac,          &
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
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Definition eines Satzes physikalischer Groessen
  !! set : Ein Datensatz physikalischer Groessen mit Werten
  TYPE , PUBLIC :: t_physet
     PRIVATE
     TYPE (t_phyval), POINTER :: set(:)
  END TYPE t_physet
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
  INTERFACE init_physet
     MODULE PROCEDURE init_physet_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE clear_physet
     MODULE PROCEDURE clear_physet_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_physet_prn_lun
     MODULE PROCEDURE setup_physet_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_physet_trc_lun
     MODULE PROCEDURE setup_physet_trc_lun_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_physet" (Skalar, 1D-Array)
  INTERFACE new_physet
     MODULE PROCEDURE new_physet_0  ! Version fuer Skalar
     MODULE PROCEDURE new_physet_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_physet" (Skalar, 1D-Array)
  INTERFACE kill_physet
     MODULE PROCEDURE kill_physet_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_physet_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_physet" (Skalar, 1D-Array)
  INTERFACE ok_physet
     MODULE PROCEDURE ok_physet_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_physet_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Datenobjekten "t_physet" (Skalar, 1D-Array)
  INTERFACE print_physet
     MODULE PROCEDURE print_physet_0 ! Version fuer Skalar
     MODULE PROCEDURE print_physet_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten
  INTERFACE print_physet_static
     MODULE PROCEDURE print_physet_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls
  INTERFACE print_physet_all_errors
     MODULE PROCEDURE print_physet_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "set" in "t_physet" auf Benutzerwert
  INTERFACE set_physet_set
     MODULE PROCEDURE set_physet_set_0_1 ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_physet_set_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !
  !! Hole Anzahl "set"s
  INTERFACE get_physet_nof_set
     MODULE PROCEDURE get_physet_nof_set_0 ! Skalar
     MODULE PROCEDURE get_physet_nof_set_1 ! Vektor
  END INTERFACE
  !! Hole Komponente "set" aus "t_physet"
  INTERFACE get_physet_set
     MODULE PROCEDURE get_physet_set_0_1 ! Skalar
     MODULE PROCEDURE get_physet_set_0_0 ! Skalar
  END INTERFACE
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  INTERFACE assign_physet
     module procedure assign_physet_0_0
     module procedure assign_physet_1_0
     module procedure assign_physet_1_1
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Hole Anzahl Variantennamen fuer ein "set"-Element
  INTERFACE get_physet_nof_var_name
     MODULE PROCEDURE get_physet_nof_var_name_0 ! Skalar
     MODULE PROCEDURE get_physet_nof_var_name_1 ! Vektor
  END INTERFACE
  !! Hole Varianten-Namen fuer ein "set"-Element
  INTERFACE get_physet_var_name
     MODULE PROCEDURE get_physet_var_name_0_1 ! Skalar
  END INTERFACE
  !! Hole Anzahl Variantendatenwerte fuer ein "set"-Element
  INTERFACE get_physet_nof_val
     MODULE PROCEDURE get_physet_nof_val_0 ! Skalar
     MODULE PROCEDURE get_physet_nof_val_1 ! Vektor
  END INTERFACE
  !! Hole Variantendatenwerte fuer ein "set"-Element
  INTERFACE get_physet_val
     MODULE PROCEDURE get_physet_val_0_1 ! Skalar
  END INTERFACE
  !! Aktualisieren einer Varianten-Datenliste mit den Daten eines Objekts
  INTERFACE update_physet_var
     MODULE PROCEDURE update_physet_var_0
     MODULE PROCEDURE update_physet_var_1
  END INTERFACE
  !! Ermittle den set-Index mit dem gegebenen Namen
  INTERFACE index_physet_set_name
     MODULE PROCEDURE index_physet_set_name_0
     MODULE PROCEDURE index_physet_set_name_1
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
  !! Pr&uuml;fung zweier Datenobjekte "t_physet" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_physet_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_physet_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_physet_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_physet_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_physet" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_physet_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_physet_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_physet_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_physet_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_physet
  PUBLIC :: clear_physet
  PUBLIC :: setup_physet_prn_lun
  PUBLIC :: setup_physet_trc_lun
  PUBLIC :: new_physet
  PUBLIC :: kill_physet
  PUBLIC :: ok_physet
  PUBLIC :: print_physet
  PUBLIC :: print_physet_static
  PUBLIC :: print_physet_all_errors
  PUBLIC :: set_physet_set
  PUBLIC :: get_physet_nof_set
  PUBLIC :: get_physet_set
  PUBLIC :: OPERATOR(==)
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: get_physet_nof_var_name
  PUBLIC :: get_physet_var_name
  PUBLIC :: get_physet_nof_val
  PUBLIC :: get_physet_val
  PUBLIC :: assign_physet
  PUBLIC :: update_physet_var
  PUBLIC :: index_physet_set_name
  !
  !
!>WIN-NT:  PUBLIC :: eq_physet_0_0
!>WIN-NT:  PUBLIC :: eq_physet_0_1
!>WIN-NT:  PUBLIC :: eq_physet_1_0
!>WIN-NT:  PUBLIC :: eq_physet_1_1
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=13), PARAMETER :: c_modname      = 'm_ipds_physet'
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1
  !! Anzahl der Datenkomponenten des Typs t_physet
  INTEGER           , PARAMETER :: c_nofcomp      = 1
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
  SUBROUTINE init_physet_d ( )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "m_ipds_physet" version 3.1 of 13.03 07'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_phyval ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_physet_all_errors ( ) 
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
  END SUBROUTINE init_physet_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_physet_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_physet_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       prn_op  = c_op
       trc_lun = c_lun
       trc_op  = c_op
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_phyval ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_physet_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_physet_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_physet_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_phyval_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_physet_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_physet_trc_lun_d ( lun )
    !
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_physet_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_phyval_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_physet_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_physet_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_physet) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_physet_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       NULLIFY ( this%set )
    END IF
    !
  END SUBROUTINE new_physet_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_physet_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_physet) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_physet_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_physet_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_physet_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_physet_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_physet) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_physet_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       ! de-allokieren dynamisch allokierbarer Feld-Komponenten
       IF ( no_error( ) ) CALL dealloc_physet_set( this )
       IF ( no_error( ) ) CALL new_physet_0 ( this )
    END IF
    !
  END SUBROUTINE kill_physet_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_physet_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_physet) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_physet_1'
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_physet_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_physet_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_physet_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_physet_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp+1)
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_physet_set( this )
       l_ok(2)  = ok_physet_fraction_sum( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_physet_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_physet_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_physet_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) ) EXIT
          ok(i) = ok_physet_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_physet_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_physet_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_physet_0' 
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
       IF ( no_error( ) ) CALL print_physet_set( this )
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
8000 FORMAT('# Beginn Objekt t_physet -------------------------------------')
8001 FORMAT('# Ende   Objekt t_physet -------------------------------------')
    !
  END SUBROUTINE print_physet_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_physet_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_physet_1' 
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
          WRITE ( UNIT=prn_lun,FMT=8000, IOSTAT=stat ) i
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          IF ( no_error( ) ) CALL print_physet_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_physet_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_physet_static_d ( )
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='print_physet_static_d' 
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
       IF ( no_error( ) ) CALL print_physet_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls m_ipds_physet         ',/ &
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
  END SUBROUTINE print_physet_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_physet_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=25), PARAMETER :: c_upname='print_physet_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_physet_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der dynamischen Komponente "set" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_physet_set_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_physet) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "set"
    TYPE (t_phyval) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='set_physet_set_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_physet_set ( this            )
       IF ( no_error( ) ) CALL alloc_physet_set   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_physet_set    ( this            )
       IF ( no_error( ) ) CALL assign_phyval      ( this%set, val   )
    END IF
    !
  END SUBROUTINE set_physet_set_0_1
  !
  !! weise der dynamischen Komponente "set" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_physet_set_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_physet) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "set"
    TYPE (t_phyval) , INTENT(IN)    :: val(:)  ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_physet_set_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_physet_set_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! Hole Anzahl "set"s aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_physet_nof_set_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_physet), INTENT(IN) :: this
    !! R&uuml;ckgabewert Anzahl "set"s (Skalar)
    INTEGER                     :: val
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='get_physet_nof_set_0' 
    !
    IF ( ASSOCIATED ( this%set ) ) THEN
       val = SIZE(this%set)
    ELSE
       val = 0
    ENDIF
    !
  END FUNCTION get_physet_nof_set_0
  !
  !! Hole Anzahl "set"s aus einer Liste von Datenobjekten <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_physet_nof_set_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekte (Vektor)
    TYPE (t_physet), INTENT(IN) :: this(:)
    !! R&uuml;ckgabewert Anzahl "set"s (Vektor)
    INTEGER                     :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       val(i) = get_physet_nof_set_0( this(i) )
    END DO
    !
  END FUNCTION get_physet_nof_set_1
  !
  !! hole die dynamische Feld Komponente "set" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_physet_set_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_physet), INTENT(IN) :: this
    !! R&uuml;ckgabewert "set" (Vektor)
    TYPE (t_phyval)             :: val(SIZE(this%set))
    !
    CALL new_phyval( val )
    IF( no_error() ) CALL assign_phyval ( val, this%set )
    !
  END FUNCTION get_physet_set_0_1
  !
  !! hole ein Element der dynamischen Feld Komponente "set" aus einem skalaren Datenobjekt.
  !! Das Element wird ueber den Namen beschrieben.
  FUNCTION get_physet_set_0_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_physet),   INTENT(IN) :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN) :: name
    !! R&uuml;ckgabewert "set" (Vektor)
    TYPE (t_phyval)               :: val
    !! Index
    INTEGER :: idx
    !
    CALL new_phyval( val )
    IF( no_error() ) idx = index_physet_set_name ( this, name )
    IF( no_error() .AND. idx > 0 ) CALL assign_phyval ( val, this%set( idx ) )
    !
  END FUNCTION get_physet_set_0_0
  !
  !! Hole Anzahl Varianten fuer ein "set"-Element eines Datenobjekts. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert.
  FUNCTION get_physet_nof_var_name_0 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_physet),   INTENT(IN) :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Anzahl
    INTEGER                       :: val
    !! Index
    INTEGER :: idx
    !
    idx = index_phyval_name( this%set, phyval_name )
    val = 0
    IF( idx > 0 ) val = get_phyval_nof_var_name( this%set( idx ) )
    !
  END FUNCTION get_physet_nof_var_name_0
  !
  !! Hole Anzahl Varianten fuer ein "set"-Element mehrerer Datenobjekte. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert.
  FUNCTION get_physet_nof_var_name_1 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_physet),   INTENT(IN) :: this(:)
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Anzahl
    INTEGER                       :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE( this )
       val(i) = get_physet_nof_var_name_0( this(i), phyval_name )
    ENDDO
    !
  END FUNCTION get_physet_nof_var_name_1
  !
  !! Hole Varianten-Namen fuer ein "set"-Element eines Datenobjekts. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert. <BR>
  !! Das Ergebnisfeld wird hier mit Speicher belegt.
  FUNCTION get_physet_var_name_0_1 ( this, phyval_name ) &
       RESULT( var_name ) 
    !! Datenobjekt (Skalar)
    TYPE (t_physet),    INTENT(IN) :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*),  INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Varianten-Namen
    CHARACTER (LEN=c_len_phyval_name), POINTER    :: var_name(:)
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='get_physet_var_name_0_1' 
    !! Index
    INTEGER :: idx
    !! Anzahl Varianten-Namen
    INTEGER :: nof_var_name
    !! Statusvariable
    INTEGER :: stat
    !
    idx          = index_phyval_name( this%set, phyval_name )
    nof_var_name = get_physet_nof_var_name( this, phyval_name )
    NULLIFY( var_name )
    IF( no_error() .AND. idx > 0 .AND. nof_var_name > 0 ) THEN
       ALLOCATE( var_name( nof_var_name ), STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 9010, c_upname, c_modname, stat )
       IF ( no_error() ) var_name = get_phyval_var_name( this%set( idx ) )
    END IF
    !
  END FUNCTION get_physet_var_name_0_1
  !
  !! Hole Anzahl Variantendatenwerte fuer ein "set"-Element eines Datenobjekts. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert.
  FUNCTION get_physet_nof_val_0 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_physet),   INTENT(IN) :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Anzahl
    INTEGER                       :: val
    !! Index
    INTEGER :: idx
    !
    idx = index_phyval_name( this%set, phyval_name )
    val = 0
    IF( idx > 0 ) val = get_phyval_nof_val( this%set( idx ) )
    !
  END FUNCTION get_physet_nof_val_0
  !
  !! Hole Anzahl Variantendatenwerte fuer ein "set"-Element mehrerer Datenobjekte. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert.
  FUNCTION get_physet_nof_val_1 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_physet),   INTENT(IN) :: this(:)
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Anzahl
    INTEGER                       :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE( this )
       val(i) = get_physet_nof_val_0( this(i), phyval_name )
    ENDDO
    !
  END FUNCTION get_physet_nof_val_1
  !
  !! Hole Variantendatenwerte fuer ein "set"-Element eines Datenobjekts. <BR>
  !! Das "set"-Element wird ueber seinen Namen definiert. <BR>
  !! Das Ergebnisfeld wird hier mit Speicher belegt.
  FUNCTION get_physet_val_0_1 ( this, phyval_name ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_physet),   INTENT(IN) :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN) :: phyval_name
    !! R&uuml;ckgabewert: Variantendatenwerte
    REAL,              POINTER    :: val(:)
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_physet_val_0_1' 
    !! Index
    INTEGER :: idx
    !! Anzahl Varianten-Namen
    INTEGER :: nof_val
    !! Statusvariable
    INTEGER :: stat
    !
    idx     = index_phyval_name( this%set, phyval_name )
    nof_val = get_physet_nof_val( this, phyval_name )
    NULLIFY( val )
    IF( no_error() .AND. idx > 0 .AND. nof_val > 0 ) THEN
       ALLOCATE( val( nof_val ), STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 9020, c_upname, c_modname, stat )
       IF ( no_error() ) val = get_phyval_val( this%set( idx ) )
    END IF
    !
  END FUNCTION get_physet_val_0_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_physet_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = eq_physet_set ( this1, this2 )
    ok      = ALL( l_ok )
    !
  END FUNCTION eq_physet_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_physet_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_physet_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_physet_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_physet_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_physet_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_physet_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_physet_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_physet_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_physet_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_physet_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_physet_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_physet_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_physet_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_physet_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_physet_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_physet_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_physet) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_physet_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-ASSIGNMENT(=)-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Setzt alle Merkmale von Objekt 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_physet_0_0 ( target_this, source_this )
    !
    !! Quellobjekt
    TYPE (t_physet), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_physet), INTENT(INOUT) :: target_this
    !
    IF ( ASSOCIATED ( source_this%set ) ) THEN
       CALL set_physet_set ( target_this, source_this%set )
    ELSE
       CALL dealloc_physet_set ( target_this )
    ENDIF
    !
  END SUBROUTINE assign_physet_0_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen von Objekt 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_physet_1_0 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_physet), INTENT(IN)    :: source_this
    !! Zielobjekt
    TYPE (t_physet), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(target_this)
       CALL assign_physet_0_0 ( target_this(i), source_this )
    END DO
    !
  END SUBROUTINE assign_physet_1_0
  !
  !! Setzt alle Merkmale der Objektliste 1 gleich denen der Objektliste 2
  !! (Arrays werden kopiert)
  SUBROUTINE assign_physet_1_1 ( target_this, source_this )
    !! Quellobjekt
    TYPE (t_physet), INTENT(IN)    :: source_this(:)
    !! Zielobjekt
    TYPE (t_physet), INTENT(INOUT) :: target_this(:)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, MIN( SIZE(source_this), SIZE(target_this) )
       CALL assign_physet_0_0 ( target_this(i), source_this(i) )
    END DO
    !
  END SUBROUTINE assign_physet_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INDEX-Methoden <<< [ERR_NO = 21000 bis 21999]
  ! ----------------------------------------------------------------------
  !
  !! Ermittle fuer ein Datenobjekt den Index desjeniegen set-Objektes mit dem gegebenen Namen <BR>
  FUNCTION index_physet_set_name_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_physet)  , INTENT(IN)  :: this
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN)  :: name
    !! R&uuml;ckgabewert Index
    INTEGER :: val
    !
    val = index_phyval_name( this%set, name )
    !
  END FUNCTION index_physet_set_name_0
  !
  !! Ermittle fuer Datenobjekte den Index desjeniegen set-Objektes mit dem gegebenen Namen <BR>
  FUNCTION index_physet_set_name_1 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_physet)  , INTENT(IN)  :: this(:)
    !! Name des gesuchten set-Datenobjekts
    CHARACTER (LEN=*), INTENT(IN)  :: name
    !! R&uuml;ckgabewert Index
    INTEGER :: val(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       val(i) = index_physet_set_name_0( this(i), name )
    ENDDO
    !
  END FUNCTION index_physet_set_name_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-UPDATE-Methoden <<< [ERR_NO = 22000 bis 22999]
  ! ----------------------------------------------------------------------
  !
  !! Aktualisieren einer Varianten-Datenliste mit den Daten eines Objekts
  SUBROUTINE update_physet_var_0 ( &
       this,                       &
       phyval_name,                &
       nof_all_var,                &
       all_var_name,               &
       all_var_ex,                 &
       all_val )
    !
    !! Datenobjekt mit neuen Variantenwerten
    TYPE (t_physet),   INTENT(IN)    :: this
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*), INTENT(IN)    :: phyval_name
    !! Anzahl Varianten
    INTEGER          , INTENT(IN)    :: nof_all_var
    !! Bezeichnungen aller Varianten
    CHARACTER (LEN=*), INTENT(IN)    :: all_var_name(nof_all_var)
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten
    LOGICAL          , INTENT(INOUT) :: all_var_ex(nof_all_var)
    !! Datenwerte aller Varianten
    REAL             , INTENT(INOUT) :: all_val(nof_all_var)
    !
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='update_physet_var_0'
    !! Anzahl Variantenbezeichnungen im Datenobjekt fuer die akt. phys. Groesse
    INTEGER                         :: nof_var_name
    !! Variantenbezeichnungen im Datenobjekt fuer die akt. phys. Groesse
    CHARACTER (LEN=c_len_phyval_name), ALLOCATABLE :: var_name(:)
    !! Anzahl Variantendatenwerte im Datenobjekt fuer die akt. phys. Groesse
    INTEGER                         :: nof_val
    !! Variantendatenwerte im Datenobjekt fuer die akt. phys. Groesse
    REAL,               ALLOCATABLE :: val(:)
    !! Zaehler fuer die Varianten des Datenobjekts
    INTEGER :: i_var
    !! Zaehler fuer die Varianten der Parameterliste
    INTEGER :: i_all_var
    !! Statusvariable
    INTEGER :: stat
    !! Suchstring im Fehlertext
    CHARACTER (LEN=13) :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=10) :: cr
    !
    ! [1] Variantenbezeichnungen des Datenobjekts holen
    !
    nof_var_name = get_physet_nof_var_name( this, phyval_name )
    IF ( nof_var_name > 0 ) THEN
       ALLOCATE ( var_name( nof_var_name ), STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 9010, c_upname, c_modname, stat )
       IF ( no_error() ) var_name = get_physet_var_name( this, phyval_name )
    END IF
    ! [2] Variantendatenwerte des Datenobjekts holen
    nof_val = get_physet_nof_val( this, phyval_name )
    IF ( nof_val > 0 ) THEN
       ALLOCATE ( val( nof_val ), STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 9020, c_upname, c_modname, stat )
       IF ( no_error() ) val = get_physet_val( this, phyval_name )
    END IF
    !
    ! [3] Varianten-Datenliste aktualisieren
    !
    IF ( no_error() ) THEN
       IF ( nof_var_name > 0 .AND. nof_val > 0 ) THEN
          IF ( nof_var_name == nof_val ) THEN
             DO i_var = 1, nof_var_name
                i_all_var = 0
                DO
                   i_all_var = i_all_var + 1
                   IF ( i_all_var > nof_all_var ) EXIT
                   IF ( TRIM( var_name( i_var ) ) == TRIM( all_var_name( i_all_var ) ) ) EXIT
                END DO
                IF ( i_all_var > nof_all_var ) THEN
                   !$OMP critical
                   CALL setup_error_act( all_errors(:), 22020, c_upname, c_modname )
                   cs = '<phyval_name>'
                   CALL setup_error_act ( cs, TRIM( phyval_name ) )
                   cs = '<phyvar_name>'
                   CALL setup_error_act ( cs, TRIM( var_name( i_var ) ) )
                   !$OMP end critical
                ELSE
                   all_var_ex( i_all_var ) = .TRUE.
                   all_val(    i_all_var ) = val( i_var )
                END IF
             END DO
          ELSE
             !$OMP critical
             CALL setup_error_act( all_errors(:), 22010, c_upname, c_modname )
             cs = '<phyval_name>'
             CALL setup_error_act ( cs, TRIM( phyval_name ) )
             cs = '<anz_varname>'
             WRITE( cr, '(I10)' ) nof_var_name
             CALL setup_error_act ( cs, cr )
             cs = '<anz_vvalues>'
             WRITE( cr, '(I10)' ) nof_val
             CALL setup_error_act ( cs, cr )
             !$OMP end critical
          END IF
       END IF
    END IF
    !
    ! [4] Nicht vorhandene Fraktionen beruecksichtigen
    !
    IF ( no_error() ) THEN
       IF ( is_phyval_frac ( phyval_name ) ) THEN
          DO i_var = 1, nof_all_var
             IF ( .NOT. all_var_ex( i_var ) ) THEN
                all_var_ex( i_var ) = .TRUE.
                all_val(    i_var ) = 0.00
             END IF
          END DO
       END IF
    END IF
    !
    ! [5] Aufraeumen
    !
    IF ( nof_var_name > 0 ) THEN
       DEALLOCATE ( var_name, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 9011, c_upname, c_modname, stat )
    END IF
    !
    IF ( nof_val > 0 ) THEN
       DEALLOCATE ( val, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 9021, c_upname, c_modname, stat )
    END IF
    !
  END SUBROUTINE update_physet_var_0
  !
  !! Aktualisieren einer Varianten-Datenliste mit den Daten der gegebenen Objekte
  SUBROUTINE update_physet_var_1 ( &
       this,                       &
       phyval_name,                &
       nof_all_var,                &
       all_var_name,               &
       all_var_ex,                 &
       all_val )
    !
    ! Formalparameter
    !! Datenobjekte mit neuen Variantenwerten
    TYPE (t_physet),   INTENT(IN)    :: this(:)
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*), INTENT(IN)    :: phyval_name
    !! Anzahl Varianten
    INTEGER          , INTENT(IN)    :: nof_all_var
    !! Bezeichnungen aller Varianten
    CHARACTER (LEN=*), INTENT(IN)    :: all_var_name(nof_all_var)
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten
    LOGICAL          , INTENT(INOUT) :: all_var_ex(nof_all_var)
    !! Datenwerte aller Varianten
    REAL             , INTENT(INOUT) :: all_val(nof_all_var)
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       CALL update_physet_var_0 &
            ( this(i), phyval_name, nof_all_var, all_var_name, all_var_ex, all_val )
    END DO
    !
  END SUBROUTINE update_physet_var_1
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
       WRITE(*,*) ' *** Warnung *** Modul "m_ipds_physet" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_physet ausfuehren'
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
  SUBROUTINE init_physet_all_errors ( )
    !
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
               '--> INIT_physet ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_physet ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_physet"\n'//&
               'Typ-Komponente = "set"\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_physet"\n'//&
               'Typ-Komponente = "set"\n'//&
               'Die Komponente enthaelt keine Daten\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6011 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_physet"\n'//&
               'Typ-Komponente = "set"\n'//&
               'Mind. ein Element der Komponente ist defekt im Sinne der phyval-Definition\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_physet"\n'//&
               'Diejeniegen phys. Groessen, die Anteile beschreiben, sind mit Werten belegt,\n'//&
               'deren Summe nicht genau Eins betraegt.\n'//&
               'Bez. der phys. Groesse  = <phy>\n'//&
               'Anzahl Anteilsgroessen  = <anz>\n'//&
               'Anteilsgroesse  1       = <phy0001>\n'//&
               'Anteilsgroesse  2       = <phy0002>\n'//&
               'Anteilsgroesse  3       = <phy0003>\n'//&
               'Anteilsgroesse  4       = <phy0004>\n'//&
               'Anteilsgroesse  5       = <phy0005>\n'//&
               'Anteilsgroesse  6       = <phy0006>\n'//&
               'Anteilsgroesse  7       = <phy0007>\n'//&
               'Anteilsgroesse  8       = <phy0008>\n'//&
               'Anteilsgroesse  9       = <phy0009>\n'//&
               'Anteilsgroesse 10       = <phy0010>\n'//&
               'Anteilsgroesse 11       = <phy0011>\n'//&
               'Anteilsgroesse 12       = <phy0012>\n'//&
               'Anteilsgroesse 13       = <phy0013>\n'//&
               'Anteilsgroesse 14       = <phy0014>\n'//&
               'Anteilsgroesse 15       = <phy0015>\n'//&
               'Anteilsgroesse 16       = <phy0016>\n'//&
               'Anteilsgroesse 17       = <phy0017>\n'//&
               'Anteilsgroesse 18       = <phy0018>\n'//&
               'Anteilsgroesse 19       = <phy0019>\n'//&
               'Anteilsgroesse 20       = <phy0020>\n'//&
               'phys. Anteile  1 -  5   = <val0001> <val0002> <val0003> <val0004> <val0005>\n'//&
               'phys. Anteile  6 - 10   = <val0006> <val0007> <val0008> <val0009> <val0010>\n'//&
               'phys. Anteile 11 - 15   = <val0011> <val0012> <val0013> <val0014> <val0015>\n'//&
               'phys. Anteile 16 - 20   = <val0016> <val0017> <val0018> <val0019> <val0020>\n'//&
               'Summe der phys. Anteile = <sum>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "nof_frac"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6210 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "nof_frac"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "frac_val"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6310 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "frac_val"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_physet"\n'//&
               'Typ-Komponente = "set"\n'//&
               '--> Code in Modul "m_ipds_physet" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "m_ipds_physet"\n'//&
               '--> Code in Modul "m_ipds_physet" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_physet"\n'//&
               'Typ-Komponente = "set"\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Rueckgabefeld "var_name"\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9011 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Feld "var_name"\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Rueckgabefeld "val"\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9021 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Feld "val"\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 22010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: UPDATE-Methoden\n'//&
               'Fuer ein Datenobjekt sind die Anzahl der fuer eine physikalische Groesse\n'//&
               'ermittelten Varianten-Namen und -Datenwerte nicht identisch.\n'//&
               'phys. Groesse               = <phyval_name>\n'//&
               'Anzahl Varianten-Namen      = <anz_varname>\n'//&
               'Anzahl Varianten-Datenwerte = <anz_vvalues>\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 22020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: UPDATE-Methoden\n'//&
               'Fuer ein Datenobjekt ist fuer eine physikalische Groesse\n'//&
               'eine Varianten-Bezeichnung aufgetaucht, die nicht in der Variantenliste\n'//&
               'enthalten ist.\n'//&
               'phys. Groesse        = <phyval_name>\n'//&
               'Variantenbezeichnung = <phyvar_name>\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
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
  END SUBROUTINE init_physet_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_physet_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_physet_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "set" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_physet_set ( this, idim )
    !! Datenobjekt
    TYPE (t_physet) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "set"
    INTEGER         , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='alloc_physet_set'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%set(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 8010, c_upname, c_modname, stat )
    !
    IF ( no_error() ) CALL new_phyval( this%set )
    !
  END SUBROUTINE alloc_physet_set
  !
  !! Initialisieren der Feld-Komponente "set" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_physet_set ( this )
    !! Datenobjekt
    TYPE (t_physet) , INTENT(INOUT) :: this   ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='init_physet_set'
    !! Initialisierungswert set
    TYPE (t_phyval) :: c_var
    !
    CALL new_phyval ( c_var )
    CALL assign_phyval ( this%set, c_var )
    CALL kill_phyval ( c_var )
    !
  END SUBROUTINE init_physet_set
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "set" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_physet_set ( this )
    !! Datenobjekt
    TYPE (t_physet) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='dealloc_physet_set'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%set ) ) THEN
       CALL kill_phyval( this%set )
       DEALLOCATE ( this%set, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
       NULLIFY ( this%set ) 
    END IF
    !
  END SUBROUTINE dealloc_physet_set
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "set" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_physet_set ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_physet) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_physet_set'
    !
    ok = ( ASSOCIATED( this%set ) )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
    IF ( ok ) THEN
       ok = ( ALL( ok_phyval( this%set ) ) )
       IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6011, c_upname, c_modname )
    ENDIF
    !
    !
  END FUNCTION ok_physet_set
  !
  !! Pr&uuml;fe, ob physikalische Anteils-Groessen in der Summe Eins ergeben
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_physet_fraction_sum ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_physet) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22),   PARAMETER :: c_upname='ok_physet_fraction_sum'
    !! Anzahl der Anteilsgroessen, die im Fehlertext beruecksichtigt werden koennen
    INTEGER,              PARAMETER :: c_frac_err=20
    !! Umrechnungsfaktor fuer Anteile
    INTEGER,              PARAMETER :: c_sum_fak=1000000
    !! Anzahl der Anteilsgroessen und Varianten im Datenobjekt
    INTEGER,            ALLOCATABLE :: nof_frac(:)
    !! Anzahl phys. Anteilsgroessen
    INTEGER                         :: n_phy
    !! Zaehler fuer phys. Anteilsgroessen
    INTEGER                         :: i_phy
    !! Anzahl phys. Anteilsvarianten pro Anteilsgroesse
    INTEGER                         :: n_var
    !! Zaehler fuer Anteilsvarianten (gesamt)
    INTEGER                         :: i_frac
    !! Hilfsfeld fuer die Anteilsvariantennamen (Groesse:Variante)
    CHARACTER (LEN=c_len_phyval_name), ALLOCATABLE :: frac_var_name(:,:)
    !! Hilfsfeld fuer die Anteilswerte (Groesse:Variante)
    REAL,               ALLOCATABLE :: frac_val(:,:)
    !! Summe aller Anteile (unter Beruecksichtigung von c_sum_fak)
    INTEGER                         :: i_sum
    !! Suchstring im Fehlertext
    CHARACTER (LEN= 9)              :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=15)              :: cr
    !! Z&auml;hler      
    INTEGER                         :: i
    !! Statusvariable
    INTEGER                         :: stat 
    !
    ok = .TRUE.
    !
    IF( ASSOCIATED( this%set ) ) THEN
       !
       ! [1.1] Anzahl Anteilsgroessen feststellen
       !
       ALLOCATE( nof_frac( SIZE( this%set ) ), STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 6200, c_upname, c_modname, stat )
       !
       nof_frac = get_phyval_nof_frac( this%set )
       !
       n_phy    = COUNT( nof_frac > 0 )
       n_var    = MAXVAL( nof_frac )
       !
       IF( n_phy > 0 ) THEN
          ALLOCATE(                           &
               frac_var_name( n_phy, n_var ), &
               frac_val( n_phy, n_var ),      &
               STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 6300, c_upname, c_modname, stat )
          frac_var_name = REPEAT( ' ', LEN( frac_var_name(1,1) ) )
          frac_val      = 0.00
       ENDIF
       !
       ! [1.2] Anteilsvariantennamen und -werte holen
       !
       i_phy = 0
       !
       DO i = 1, SIZE( this%set )
          IF( nof_frac( i ) > 0 ) THEN
             i_phy = i_phy + 1
             frac_var_name( i_phy, 1:nof_frac( i ) ) = get_phyval_var_name ( this%set( i ) )
             frac_val(      i_phy, 1:nof_frac( i ) ) = get_phyval_val (      this%set( i ) )
          END IF
       END DO
       !
       ! [1.3] Summenbildung und Kontrolle
       !       Um die Genauigkeitspobleme mit REAL-Groessen zu umgehen, werden
       !       die Werte mit einem Faktor belegt und auf INTEGER umgerechnet
       !
       i_phy = 0
       !
       DO i = 1, SIZE( this%set )
          IF( nof_frac( i ) > 0 ) THEN
             i_phy = i_phy + 1
             i_sum = NINT( REAL( c_sum_fak ) * SUM( frac_val( i_phy, 1:nof_frac( i ) ) ) )
             IF ( i_sum /= c_sum_fak ) THEN
                ok = .FALSE.
                !$OMP critical
                !
                CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
                !
                cs = '<phy>'
                CALL setup_error_act ( TRIM( cs ), TRIM( get_phyval_name( this%set( i ) ) ) )
                !
                cs = '<anz>'
                WRITE( cr, '(I15)' ) SUM( nof_frac )
                CALL setup_error_act ( TRIM( cs ), TRIM( ADJUSTL( cr ) ) )
                !
                i_frac = 0
                DO
                   i_frac = i_frac + 1
                   IF( i_frac > nof_frac( i ) ) EXIT
                   IF( i_frac > c_frac_err ) EXIT
                   cs = '<val____>'
                   WRITE( cs(5:8), '(I4.4)'  ) i_frac
                   WRITE( cr,      '(F15.6)' ) frac_val(i_phy,i_frac)
                   CALL setup_error_act ( TRIM( cs ), TRIM( ADJUSTL( cr ) ) )
                   cs = '<phy____>'
                   WRITE( cs(5:8), '(I4.4)' ) i_frac
                   CALL setup_error_act ( TRIM( cs ), TRIM( frac_var_name( i_phy, i_frac ) ) )
                END DO
                !
                IF( i_frac < c_frac_err ) THEN
                   cr = REPEAT( ' ', LEN( cr ) )
                   DO
                      cs = '<val____>'
                      WRITE( cs(5:8), '(I4.4)' ) i_frac
                      CALL setup_error_act ( TRIM( cs ), cr(1:1) )
                      cs = '<phy____>'
                      WRITE( cs(5:8), '(I4.4)' ) i_frac
                      CALL setup_error_act ( TRIM( cs ), cr(1:1) )
                      i_frac = i_frac + 1
                      IF( i_frac > c_frac_err ) EXIT
                   END DO
                ENDIF
                !
                cs = '<sum>'
                WRITE( cr, '(F15.6)' ) REAL( i_sum ) / REAL( c_sum_fak )
                CALL setup_error_act ( TRIM( cs ), cr )
                !
                !$OMP end critical
             END IF
          END IF
       END DO
       !
       ! [1.4] Aufraeumen
       !
       IF( n_phy > 0 ) THEN
          DEALLOCATE( frac_var_name, frac_val, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 6310, c_upname, c_modname, stat )
       ENDIF
       DEALLOCATE( nof_frac, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 6210, c_upname, c_modname, stat )
       !
    END IF
    !
  END FUNCTION ok_physet_fraction_sum
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "set" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_physet_set ( this )
    !! Datenobjekt
    TYPE (t_physet) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_physet_set'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8000,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
    IF ( ASSOCIATED (this%set) ) THEN
       !
       CALL print_phyval ( this%set )
       !
    ELSE
       !
       WRITE ( &
            UNIT    = prn_lun,  &
            FMT     = 8001,     & 
            IOSTAT  = stat )    &
            '<..no.set.associated..>'
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
       !
    ENDIF
    !
    WRITE ( &
         UNIT    = prn_lun,  &
         FMT     = 8002,     & 
         IOSTAT  = stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT ( &
          '# Inhalt der Komponente set - - - - - - - - - - - - - - - - - ')
8001 FORMAT ( &
          '# ',A)
8002 FORMAT ( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_physet_set
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "set" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_physet_set ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_physet) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ALL( this1%set == this2%set )
    !
  END FUNCTION eq_physet_set
  !
END MODULE m_ipds_physet
! TailOfPackageModule ------------------------------------------------------
