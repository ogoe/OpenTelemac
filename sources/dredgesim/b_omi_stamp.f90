! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden analog zu OpenMI-Interface <EM>ITimeStamp</EM></h2>
!! @author G. Lang
!! @version 2.1 vom 07/21/05, Quellcode: mod_b_omi_stamp.f90
!! <HR>
!! type and methods equivalent to OpenMI-interface <EM>ITimeStamp</EM> <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-01-25 : G. Lang : Erstversion
!  01.02 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  01.03 : 2005-03-11 : G. Lang : Operatoren aus "b_datetime" und "b_time" durch Funktionen ersetzt
!  01.04 : 2004-05-09 : G. Lang : Erweiterungen fuer io_dataset-Einbindung
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>ITimeStamp</EM>. Dient
!! dazu, einen bestimmten Termin zu beschreiben. Die Zeitangaben werden
!! als <EM>modifiziertes Julianisches Datum</EM> behandelt. Dies entspricht
!! der Anzahl der Tage, die seit dem <STRONG>17. November 1858 (0 Uhr)</STRONG> 
!! verstrichen sind. Beispielsweise ist der 27. September 1995 der 49987-te Tag,
!! gem&auml;&szlig; modifiziert julianischer Rechnung. Als Zeitbezug hierf&uuml;r
!! wird UTC (<EM>Universal Time Coordinated</EM>) verwendet, die seit dem
!! Jahre 1926 als Ersatz f&uuml;r GMT (<EM>Greenwich Mean Time</EM>) eingef&uuml;hrt
!! wurde. UTC basiert auf der SI-Sekunde.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_stamp";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_stamp";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_stamp";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_stamp";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_stamp";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_stamp".
!! </OL>
!!                                                         
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_omi_stamp"
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> modjulianday : modifizierte Julianische Datumsangabe                                        
!!          [ Zeit in Tagen, die seit dem <EM>17. November 1858, 0 Uhr (UTC)</EM>
!!          vergangen sind ]
!! </OL>
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_omi_stamp mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_stamp mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   
!!          andere nicht. 
!!          Routinen, die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,
!!          ob das Modul korrekt initialisert wurde (ok_initialised).  <BR>
!!          F&uuml;r eine vollst&auml;ndige &Uuml;bersicht verwende man
!!          die Methode PRINT_OMI_STAMP_ALL_ERRORS.
!!                                                                    <BR>
!
MODULE b_omi_stamp
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Double
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
       new_error,           &
       kill_error,          &
       print_error,         &
       setup_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
  !
  ! [A.3] BASIS-Modul mit Typ und Methoden Datums-und-Zeitangaben
  !
  USE b_datetime, ONLY : &
       !   Typdefinitionen
       t_datetime,              &
       !   Routinen / Interfaces
       init_datetime,           &
       clear_datetime,          &
       setup_datetime_prn_lun,  &
       setup_datetime_trc_lun,  &
       new_datetime,            &
       kill_datetime,           &
       datetime_to_julian_time, &
       julian_time_to_datetime, &
       datetime_to_string,      &
       string_to_datetime,      &
       get_datetime_language,   &
       setup_datetime_language, &
       su_datetime,             &
       ad_time_to_datetime
  ! 
  ! [A.4] BASIS-Modul mit Typ und Methoden Zeitdifferenzen
  !
  USE b_time, ONLY : &
       !   Typdefinitionen
       t_time,             &
       !   Parameter 
       !   Variablen mit INTENT(IN)
       !   Variablen mit INTENT(INOUT)
       !   Variablen mit INTENT(OUT)
       !   Routinen / Interfaces
       init_time,             &
       clear_time,            &
       setup_time_prn_lun,    &
       setup_time_trc_lun,    &
       new_time,              &
       kill_time,             &
       time_to_real_days,     &
       real_days_to_time,     &   
       time_to_string,        &
       round_time_to_seconds, &
       get_time_nanos,        &
       set_time_nanos,        &
       su_time,               &
       ad_time
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
  !! <EM>modjulianday</EM> : modifizierte Julianische Datumsangabe <BR>
  !! Zeit in Tagen, die seit dem <EM>17. November 1858, 0 Uhr (UTC)</EM>
  !! vergangen sind
  TYPE , PUBLIC :: t_omi_stamp
     PRIVATE
     REAL (KIND=Double) :: modjulianday ! 
  END TYPE t_omi_stamp
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Undefined-Wert f&uuml;r REAL(Double)-Komponenten
  INTEGER            , PUBLIC , PARAMETER :: c_undef_omi_stamp_double=-1.0_Double ! 
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
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_omi_stamp
     MODULE PROCEDURE init_omi_stamp_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_stamp
     MODULE PROCEDURE clear_omi_stamp_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_stamp_prn_lun
     MODULE PROCEDURE setup_omi_stamp_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_stamp_trc_lun
     MODULE PROCEDURE setup_omi_stamp_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_stamp" und Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE new_omi_stamp
     MODULE PROCEDURE new_omi_stamp_0  ! 
     MODULE PROCEDURE new_omi_stamp_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_stamp" und Re-Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE kill_omi_stamp
     MODULE PROCEDURE kill_omi_stamp_0 ! 
     MODULE PROCEDURE kill_omi_stamp_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_stamp" auf G&uuml;ltigkeit: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE ok_omi_stamp
     MODULE PROCEDURE ok_omi_stamp_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_omi_stamp_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_stamp" <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_stamp" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_stamp
     MODULE PROCEDURE print_omi_stamp_0 ! 
     MODULE PROCEDURE print_omi_stamp_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_stamp_static
     MODULE PROCEDURE print_omi_stamp_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_stamp_all_errors
     MODULE PROCEDURE print_omi_stamp_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "modjulianday" in "t_omi_stamp" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)                <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)             <BR>
  !! c) f&uuml;r eine Datumsangabe "t_datetime" (Skalar) <BR>
  !! d) f&uuml;r viele Datumsangaben "t_datetime" (Vektor)
  INTERFACE set_omi_stamp_modjulianday
     MODULE PROCEDURE set_omi_stamp_modjulianday_0_0 ! 
     MODULE PROCEDURE set_omi_stamp_modjulianday_1_0 ! 
     MODULE PROCEDURE set_omi_stamp_datetime_0_0     ! 
     MODULE PROCEDURE set_omi_stamp_datetime_1_0     ! 
  END INTERFACE
  !
  !! Hole Komponente "modjulianday" aus "t_omi_stamp": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_stamp_modjulianday
     MODULE PROCEDURE get_omi_stamp_modjulianday_0_0 ! 
     MODULE PROCEDURE get_omi_stamp_modjulianday_1_0 ! 
  END INTERFACE
  !
  !! Ermittle die Indexposition des n&auml;chstgelegenen Termins <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)
  INTERFACE get_omi_stamp_closest_idx
     MODULE PROCEDURE get_omi_stamp_closest_idx_1_0
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Hole Komponente "modjulianday" als Datumsangabe "t_datetime"<BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_stamp_datetime
     MODULE PROCEDURE get_omi_stamp_datetime_0_0
     MODULE PROCEDURE get_omi_stamp_datetime_1_0
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_stamp" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_stamp
     MODULE PROCEDURE eq_omi_stamp_0_0  ! 
     MODULE PROCEDURE eq_omi_stamp_0_1  ! 
     MODULE PROCEDURE eq_omi_stamp_1_0  ! 
     MODULE PROCEDURE eq_omi_stamp_1_1  ! 
  END INTERFACE
  !! Vergleich ">" zweier Datenobjekte "t_omi_stamp" (Funktion) <BR>
  !! a) Skalar1 > Skalar2 <BR>
  !! b) Skalar1 > Vektor2 <BR>
  !! c) Vektor1 > Skalar2 <BR>
  !! d) Vektor1 > Vektor2
  INTERFACE gt_omi_stamp
     MODULE PROCEDURE gt_omi_stamp_0_0  !
     MODULE PROCEDURE gt_omi_stamp_0_1  !
     MODULE PROCEDURE gt_omi_stamp_1_0  !
     MODULE PROCEDURE gt_omi_stamp_1_1  !
  END INTERFACE
  !! Vergleich ">=" zweier Datenobjekte "t_omi_stamp" (Funktion) <BR>
  !! a) Skalar1 >= Skalar2 <BR>
  !! b) Skalar1 >= Vektor2 <BR>
  !! c) Vektor1 >= Skalar2 <BR>
  !! d) Vektor1 >= Vektor2
  INTERFACE ge_omi_stamp
     MODULE PROCEDURE ge_omi_stamp_0_0  ! 
     MODULE PROCEDURE ge_omi_stamp_0_1  ! 
     MODULE PROCEDURE ge_omi_stamp_1_0  ! 
     MODULE PROCEDURE ge_omi_stamp_1_1  ! 
  END INTERFACE
  !! Vergleich "<" zweier Datenobjekte "t_omi_stamp" (Funktion) <BR>
  !! a) Skalar1 < Skalar2 <BR>
  !! b) Skalar1 < Vektor2 <BR>
  !! c) Vektor1 < Skalar2 <BR>
  !! d) Vektor1 < Vektor2
  INTERFACE lt_omi_stamp
     MODULE PROCEDURE lt_omi_stamp_0_0  ! 
     MODULE PROCEDURE lt_omi_stamp_0_1  ! 
     MODULE PROCEDURE lt_omi_stamp_1_0  ! 
     MODULE PROCEDURE lt_omi_stamp_1_1  ! 
  END INTERFACE
  !! Vergleich "<=" zweier Datenobjekte "t_omi_stamp" (Funktion) <BR>
  !! a) Skalar1 <= Skalar2 <BR>
  !! b) Skalar1 <= Vektor2 <BR>
  !! c) Vektor1 <= Skalar2 <BR>
  !! d) Vektor1 <= Vektor2
  INTERFACE le_omi_stamp
     MODULE PROCEDURE le_omi_stamp_0_0  ! 
     MODULE PROCEDURE le_omi_stamp_0_1  ! 
     MODULE PROCEDURE le_omi_stamp_1_0  ! 
     MODULE PROCEDURE le_omi_stamp_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_stamp" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_stamp
     MODULE PROCEDURE ne_omi_stamp_0_0  ! 
     MODULE PROCEDURE ne_omi_stamp_0_1  ! 
     MODULE PROCEDURE ne_omi_stamp_1_0  ! 
     MODULE PROCEDURE ne_omi_stamp_1_1  ! 
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
  PUBLIC :: init_omi_stamp
  PUBLIC :: clear_omi_stamp
  PUBLIC :: setup_omi_stamp_prn_lun
  PUBLIC :: setup_omi_stamp_trc_lun
  PUBLIC :: new_omi_stamp
  PUBLIC :: kill_omi_stamp
  PUBLIC :: ok_omi_stamp
  PUBLIC :: print_omi_stamp
  PUBLIC :: print_omi_stamp_static
  PUBLIC :: print_omi_stamp_all_errors
  PUBLIC :: set_omi_stamp_modjulianday
  PUBLIC :: get_omi_stamp_modjulianday
  PUBLIC :: eq_omi_stamp
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: get_omi_stamp_datetime 
  PUBLIC :: get_omi_stamp_closest_idx
  PUBLIC :: gt_omi_stamp
  PUBLIC :: ge_omi_stamp
  PUBLIC :: lt_omi_stamp
  PUBLIC :: le_omi_stamp
  PUBLIC :: ne_omi_stamp
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
  CHARACTER (LEN=11), PARAMETER :: c_modname      = 'b_omi_stamp' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_stamp
  INTEGER           , PARAMETER :: c_nofcomp      = 1                ! ggf. modifizieren
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)! 
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                , SAVE :: initialised = .false.  ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                , SAVE :: prn_op      = c_op     ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                , SAVE :: trc_op      = c_op     ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                , SAVE :: prn_lun     = c_lun    ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                , SAVE :: trc_lun     = c_lun    ! 
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                , SAVE :: n_init      = 0        ! 
  !! Referenzdatum zum Berechnen des mod. Julianischen Datums
  TYPE (t_datetime)      , SAVE :: ref_datetime           ! 
  !
  ! [D.4] Schnittstellen
  !
  !! Runde eine Zeitdifferenz sinnvoll auf Sekundenbruchteile
  INTERFACE get_omi_stamp_round_time
     MODULE PROCEDURE get_omi_stamp_round_time_d
  END INTERFACE
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
  SUBROUTINE init_omi_stamp_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='init_omi_stamp_d' 
    !! Hilfsvariable
    INTEGER :: language ! 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_B > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_stamp" version 2.1 of 07/21/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_time ( )
       IF ( no_error( ) ) CALL init_datetime ( )
       ! ... ggf. weitere Initialisierungen ergaenzen
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_stamp_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern etc.
       prn_lun = c_lun
       trc_lun = c_lun
       IF ( no_error( ) ) THEN
          CALL new_datetime( ref_datetime )
          language = get_datetime_language ( )
          CALL setup_datetime_language ( 1        )
          ref_datetime = string_to_datetime( '17.11.1858-00:00:00.000000000 UTC ' )
          CALL setup_datetime_language ( language )
       END IF
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_stamp_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_stamp_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='clear_omi_stamp_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_stamp_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern etc.
       IF ( no_error( ) ) THEN
          CALL kill_datetime( ref_datetime )
       END IF
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_datetime ( )
       IF ( no_error( ) ) CALL clear_time ( )
       ! ... ggf. weitere De-Initialisierungen ergaenzen
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_stamp_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_stamp_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_omi_stamp_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_time_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_stamp_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_stamp_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_omi_stamp_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_time_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_stamp_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_stamp_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='new_omi_stamp_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%modjulianday = c_undef_omi_stamp_double
    END IF
    !
  END SUBROUTINE new_omi_stamp_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_stamp_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_stamp) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='new_omi_stamp_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_stamp_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_stamp_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_stamp_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='kill_omi_stamp_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_stamp_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_stamp_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_stamp_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_stamp) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='kill_omi_stamp_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_stamp_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_stamp_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_stamp_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_omi_stamp_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_omi_stamp_modjulianday( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_stamp_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_stamp_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_omi_stamp_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) ) EXIT
          ok(i) = ok_omi_stamp_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_stamp_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_stamp_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_omi_stamp_0' 
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
       IF ( no_error( ) ) CALL print_omi_stamp_modjulianday( this )
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
8000 FORMAT('# Beginn Objekt t_omi_stamp ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_stamp ------------------------------')
    !
  END SUBROUTINE print_omi_stamp_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_stamp_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_omi_stamp_1' 
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
          IF ( no_error( ) ) CALL print_omi_stamp_0 ( this(i) )
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
  END SUBROUTINE print_omi_stamp_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_stamp_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=24), PARAMETER :: c_upname='print_omi_stamp_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, c_undef_omi_stamp_double, &
           TRIM(datetime_to_string(ref_datetime))
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_omi_stamp_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_stamp         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#   initialised = ',L1,/ &
    '#        prn_op = ',L1,/ &
    '#        trc_op = ',L1,/ &
    '#       prn_lun = ',I5,/ &
    '#       trc_lun = ',I5,/ &
    '#        n_init = ',I5,/ &
    '# undef[double] = ',G15.6,/ &
    '#  ref_datetime = ',A,/,&
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_omi_stamp_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_stamp_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=28), PARAMETER :: c_upname='print_omi_stamp_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_stamp_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "modjulianday" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_stamp_modjulianday_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "modjulianday"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%modjulianday = val
    !
  END SUBROUTINE set_omi_stamp_modjulianday_0_0
  !
  !! weise der Komponente "modjulianday" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_stamp_modjulianday_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_stamp) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "modjulianday"
    REAL (KIND=Double)           , INTENT(IN)    :: val     ! 
    !
    this(:)%modjulianday = val
    !
  END SUBROUTINE set_omi_stamp_modjulianday_1_0
  !
  !! weise der Komponente "modjulianday" einen skalaren Wert zu 
  !! der als Typ "t_datetime" &uuml;bergeben wird               <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_stamp_datetime_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "modjulianday"
    TYPE (t_datetime)  , INTENT(IN)    :: val  ! 
    !! Hilfsvariable
    TYPE (t_time) :: l_time ! 
    !
    CALL new_time  ( l_time )
    !
    l_time            = su_datetime( val, ref_datetime )
    this%modjulianday = time_to_real_days( l_time )
    !
    CALL kill_time ( l_time )
    !
  END SUBROUTINE set_omi_stamp_datetime_0_0
  !
  !! weise den Komponenten "modjulianday" einen skalaren Wert zu 
  !! der als Typ "t_datetime" &uuml;bergeben wird               <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_stamp_datetime_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "modjulianday"
    TYPE (t_datetime)  , INTENT(IN)    :: val  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_stamp_datetime_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_stamp_datetime_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "modjulianday" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_stamp_modjulianday_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "modjulianday" (Skalar)
    REAL (KIND=Double)               :: val  ! 
    !
    val = this%modjulianday
    !
  END FUNCTION get_omi_stamp_modjulianday_0_0
  !
  !! hole die Komponente "modjulianday" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_stamp_modjulianday_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN)  :: this(:)         ! 
    !! R&uuml;ckgabewert "modjulianday"
    REAL (KIND=Double)               :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%modjulianday
    !
  END FUNCTION get_omi_stamp_modjulianday_1_0
  !
  !! Hole Komponente "modjulianday" als Datumsangabe "t_datetime" <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_omi_stamp_datetime_0_0 ( this ) &
       RESULT ( res )
    !! Datanobjekt (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this ! 
    !! Ergebnis: Datums- und Zeitangabe
    TYPE (t_datetime)               :: res  !
    !! Hilfsvariable
    TYPE (t_time) :: l_time !
    !
    CALL new_datetime( res )
    CALL new_time( l_time )
    !
    l_time = real_days_to_time ( this%modjulianday )
    l_time = get_omi_stamp_round_time ( l_time )
    res    = ad_time_to_datetime ( ref_datetime, l_time )
    !
    CALL kill_time( l_time )
    !
  END FUNCTION get_omi_stamp_datetime_0_0
  !
  !! Hole Komponenten "modjulianday" als Datumsangaben "t_datetime" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_stamp_datetime_1_0 ( this ) &
       RESULT ( res )
    !! Datanobjekt (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Datums- und Zeitangabe
    TYPE (t_datetime)               :: res(SIZE(this)) !
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       IF ( any_error( ) ) EXIT
       res(i) = get_omi_stamp_datetime_0_0 ( this(i) ) 
    END DO
    !
  END FUNCTION get_omi_stamp_datetime_1_0
  !
  !! Ermittle den Positionsindex f&uuml;r den n&auml;chstgelegenen Termin
  !! in stamp(:) zur aktuellen Zeit time_stamp <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_stamp_closest_idx_1_0 ( stamp, time_stamp ) &
       RESULT( res )
    !! Datenobjekt (Vektor) in dem gesucht werden soll
    TYPE (t_omi_stamp) , INTENT(IN) :: stamp(:)   ! 
    !! Termin, dessen n&auml;chstgelegene Zeitangabe gesucht werden soll
    TYPE (t_omi_stamp) , INTENT(IN) :: time_stamp !
    !! Ergebnis: Zeiger auf Position in stamp(:)
    INTEGER :: res ! 
    !! Hilfsvariablen
    REAL (KIND=Double) , ALLOCATABLE :: d(:)      ! 
    ! 
    ALLOCATE( d(SIZE(stamp)) )
    d(:) = ABS( stamp(:)%modjulianday - time_stamp%modjulianday )
    res  = MINVAL(MINLOC(d))
    DEALLOCATE( d )
    !
  END FUNCTION get_omi_stamp_closest_idx_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_stamp_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%modjulianday == this2%modjulianday )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_stamp_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_stamp_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_stamp_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_stamp_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_stamp_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_stamp_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_stamp_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_stamp_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_stamp_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_stamp_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_omi_stamp_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%modjulianday > this2%modjulianday )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION gt_omi_stamp_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_omi_stamp_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_omi_stamp_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION gt_omi_stamp_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_omi_stamp_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_omi_stamp_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION gt_omi_stamp_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_omi_stamp_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_omi_stamp_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION gt_omi_stamp_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_omi_stamp_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%modjulianday >= this2%modjulianday )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION ge_omi_stamp_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_omi_stamp_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_omi_stamp_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ge_omi_stamp_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_omi_stamp_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_omi_stamp_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ge_omi_stamp_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_omi_stamp_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_omi_stamp_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ge_omi_stamp_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_omi_stamp_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%modjulianday < this2%modjulianday )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION lt_omi_stamp_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_omi_stamp_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_omi_stamp_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION lt_omi_stamp_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_omi_stamp_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_omi_stamp_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION lt_omi_stamp_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_omi_stamp_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_omi_stamp_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION lt_omi_stamp_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_omi_stamp_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%modjulianday <= this2%modjulianday )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION le_omi_stamp_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_omi_stamp_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_omi_stamp_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION le_omi_stamp_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
 FUNCTION le_omi_stamp_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_omi_stamp_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION le_omi_stamp_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_omi_stamp_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_omi_stamp_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION le_omi_stamp_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_stamp_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_stamp_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_stamp_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_stamp_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_stamp_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_stamp_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_stamp_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_stamp_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_stamp_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_stamp_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_stamp) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l     = SIZE(ok)
    ok(:) = .NOT. eq_omi_stamp_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_stamp_1_1
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
  FUNCTION ok_initialised &
       ( upname )         &
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
       !
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_stamp" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_stamp ausfuehren'
       !
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
       !
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION not_initialised &
       ( upname )          &
       RESULT( ok )
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
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
  SUBROUTINE init_omi_stamp_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER  :: c_upname='init_omi_stamp_all_errors' !
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
               '--> INIT_omi_stamp ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_stamp ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_stamp"\n'//&
               'Typ-Komponente = "modjulianday" muss >= 0.0 sein\n'//&
               'aktuell = <aktuell> \n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_stamp" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_stamp" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_stamp" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_stamp"\n'//&
               'Typ-Komponente = "modjulianday"\n'//&
               '--> Code in Modul "b_omi_stamp" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_stamp"\n'//&
               '--> Code in Modul "b_omi_stamp" / Daten pruefen' )
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
  END SUBROUTINE init_omi_stamp_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-GET-Methoden
  ! ----------------------------------------------------------------------
  !
  !! runde eine Zeitdifferenz auf 1/1000 Sekundenbruchteile <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_stamp_round_time_d ( this ) &
       RESULT ( res )
    !! Zeitdifferenz
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! gerundete Zeitdifferenz
    TYPE (t_time)              :: res  ! 
    !! Rundungsparameter (Rundung auf 1/1000 Sekunden)
    INTEGER            , PARAMETER :: c_m=1000000            ! 
    !! Hilfsvariable
    TYPE (t_time )      :: d_time       !     
    INTEGER             :: nanos        ! 
    !
    res    = round_time_to_seconds ( this )
    d_time = su_time( this, res )
    nanos  = get_time_nanos( d_time )
    nanos  = NINT( REAL(nanos,Double)/REAL(c_m,Double) ) * c_m
    CALL set_time_nanos( d_time, nanos )
    res = ad_time( res, d_time )
    !
  END FUNCTION get_omi_stamp_round_time_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_stamp_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='clear_omi_stamp_all_errors' !
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_stamp_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "modjulianday" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_stamp_modjulianday &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_stamp) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='ok_omi_stamp_modjulianday' ! 
    !! Hilfsvariable
    CHARACTER (LEN=15) :: ch ! 
    !
    ok = ( this%modjulianday /= c_undef_omi_stamp_double .AND. this%modjulianday >= 0.0_Double )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ch,'(G15.6)') this%modjulianday ; CALL setup_error_act ( '<aktuell>', ch )
    END IF
    !
  END FUNCTION ok_omi_stamp_modjulianday
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "modjulianday" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_stamp_modjulianday &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_stamp) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=28) , PARAMETER :: c_upname='print_omi_stamp_modjulianday' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariablen
    TYPE (t_time)     :: l_time     ! 
    TYPE (t_datetime) :: l_datetime ! 
    !
    CALL new_time ( l_time ) ; CALL new_datetime( l_datetime )
    l_time     = real_days_to_time ( this%modjulianday )
    l_time     = get_omi_stamp_round_time ( l_time )
    l_datetime = ad_time_to_datetime ( ref_datetime, l_time )
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%modjulianday, &
           TRIM( time_to_string( l_time ) ), TRIM( datetime_to_string( l_datetime ) )
    !
    CALL kill_time ( l_time ) ; CALL kill_datetime( l_datetime )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
    !
8000 FORMAT &
          ('# Inhalt der Komponente modjulianday  - - - - - - - - - - - - ',/&
           '# aktuell    = ',G15.8,' Tage ',/ &
           '# time       = ',A,/ &
           '# l_datetime = ',A,/ &
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_stamp_modjulianday
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
END MODULE b_omi_stamp
! TailOfBaseModule --------------------------------------------------------
