! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Zeitangaben bestehend aus Datum, Uhrzeit und Zeitzone</h2>
!! @author Peter Schade, Ingrid Uliczka
!! @version 1.15 vom 03/26/07, Quellcode: mod_b_datetime.f90
!! <HR>
!! <dataype and methods describing with date, time and timezone> <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!  <CopyrightWildcard>
!!                                                                  <BR>
!! <HR>
! <H3>Entwicklungsgeschichte des Moduls</H3>
! 01.01 : 2002-06-18 : Peter Schade, Ingrid Uliczka : <BR>
! Das von Guntram Seiss erstellte Modul 
! $PROGHOME/fortran/lib/basismodule/*/mod_date_time_class.f90
! wurde in ein Basismodul uebertragen und es wurden Methoden hinzugefuegt.
! 01.07 : 2003-03-19 : Peter Schade : 
!                      Beschreibung oeffentlicher Methoden im Header entfernt,
!                      Copyright als Wildcard, 
!                      Entwicklungsgeschichte nicht in HTML-Beschreibung,
!                      Copyright-Hinweis in init_datetime_d gekuerzt,
!                      all_errors(:) als dynamisch allokierbares Feld
! 01.11 : 2003-03-19 : Peter Schade : 
!                      voreingestellte Zeitzone predefined_zone + Methoden
! 01.12 : 2003-03-19 : Peter Schade : Methodennamen auf max. 31 Zeichen gekuerzt
! 01.13 : 2006-03-29 : G. Lang      : neue Methode round_datetime, gt_datetime, lt_datetime
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Definiert den Basisdatentypen "t_datetime" (Datum + Uhrzeit + Zeitzone) <BR>
!! und stellt Methoden zum Rechnen mit diesen Angaben bereit. 
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_datetime <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> date : Kalender-Tag, -Monat und -Jahr
!!     <LI> hour : Stunde des Tages
!!     <LI> min  : Minute der Stunde
!!     <LI> sec  : Sekunde der Minute
!!     <LI> nanosec : Nanosekunde der Sekunde
!!     <LI> zone : Zeitzone als Anzahl der Stunden bezogen auf UTC
!! </OL>
!!                                                                  <BR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_datetime mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_datetime mit CLEAR-Methode.
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
!!          die Methode PRINT_DATETIME_ALL_ERRORS.
!!                                                                    <BR>
!
MODULE b_datetime
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       double,         &
       short
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
       print_error_act,     &
       setup_error_act,     &
       clear_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
  !
  ! [A.3] weitere BASIS-Module (ONLY benutzen!)
  !
  ! Datumsrechnung
  USE b_date
  !
  ! Behandlung von Zeitinkrementen
  USE b_time
  !
  !, ONLY : &
  !   Typdefinitionen    
  !   Parameter 
  !   Variablen mit INTENT(IN)
  !   Variablen mit INTENT(INOUT)
  !   Variablen mit INTENT(OUT)
  !   Routinen / Interfaces
  !   init_time
  !   clear_time
  !   setup_time_prn_lun
  !   setup_time_trc_lun
  !   Operatoren
  ! 
  ! ... fuer jedes weitere Basis-Modul wiederholen
  ! ... weitere Basis-Module mit USE in einzelnen Programmeinheiten 
  !     einbinden
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
  ! [C.0] Konstantwerte 
  !
  !! L&auml;nge des Strings als R&uuml;ckgabewert von "datetime_to_string"
  INTEGER , PUBLIC , PARAMETER :: c_len_datetime_to_string=34 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Zeitangabe bestehend aus Datum, Uhrzeit und Zeitzone <BR>
  !! date : Kalender-Tag, -Monat und Jahr  <BR>
  !! hour : Stunde des Tages  <BR>
  !! min  : Minute der Stunde  <BR>
  !! sec  : Sekunde der Minute  <BR>
  !! nanosec : Nanosekunde der Sekunde <BR>
  !! zone : Zeitzone als Anzahl der Stunden bezogen auf UTC
  TYPE , PUBLIC :: t_datetime
     PRIVATE
     TYPE (t_date)         :: date     ! 
     INTEGER (KIND=short)  :: hour     ! 
     INTEGER (KIND=short)  :: min      ! 
     INTEGER (KIND=short)  :: sec      ! 
     INTEGER               :: nanosec  ! 
     INTEGER (KIND=short)  :: zone     ! 
  END TYPE t_datetime
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
  !! Allokieren/Initialisieren der statischen Objekte des Moduls
  INTERFACE init_datetime
     MODULE PROCEDURE init_datetime_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Objekte des Moduls
  INTERFACE clear_datetime
     MODULE PROCEDURE clear_datetime_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_datetime_prn_lun
     MODULE PROCEDURE setup_datetime_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_datetime_trc_lun
     MODULE PROCEDURE setup_datetime_trc_lun_d ! 
  END INTERFACE
  !! Index f&uuml;r Spracheinstellung setzen
  INTERFACE setup_datetime_language
     MODULE PROCEDURE setup_datetime_language_d ! -
  END INTERFACE
  !! voreingestellte Zeitzone setzen, die dann bei new_datetime standardmaessig
  !! verwendet wird
  INTERFACE setup_datetime_predefined_zone
     MODULE PROCEDURE setup_datetime_predefined_zonei ! per Integer-Zahl
     MODULE PROCEDURE setup_datetime_predefined_zonec ! per Zeitzonen-String
  END INTERFACE
  !! Index f&uuml;r Art des Strings (z.Z. nur BAWstring) setzen
  INTERFACE setup_datetime_string_type
     MODULE PROCEDURE setup_datetime_string_type_d ! 
  END INTERFACE
  !! Erzeugen von Objekten "t_datetime" (Skalar, 1D-Array)
  INTERFACE new_datetime
     MODULE PROCEDURE new_datetime_0  ! Version fuer Skalar
     MODULE PROCEDURE new_datetime_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Objekten "t_datetime" (Skalar, 1D-Array)
  INTERFACE kill_datetime
     MODULE PROCEDURE kill_datetime_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_datetime_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Objekten "t_datetime" (Skalar, 1D-Array)
  INTERFACE ok_datetime
     MODULE PROCEDURE ok_datetime_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_datetime_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Objekten "t_datetime" (Skalar, 1D-Array)
  INTERFACE print_datetime
     MODULE PROCEDURE print_datetime_0 ! Version fuer Skalar
     MODULE PROCEDURE print_datetime_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten
  INTERFACE print_datetime_static
     MODULE PROCEDURE print_datetime_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls
  INTERFACE print_datetime_all_errors
     MODULE PROCEDURE print_datetime_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "date" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_date
     MODULE PROCEDURE set_datetime_date_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_date_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_date_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "hour" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_hour
     MODULE PROCEDURE set_datetime_hour_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_hour_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_hour_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "min" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_min
     MODULE PROCEDURE set_datetime_min_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_min_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_min_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "sec" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_sec
     MODULE PROCEDURE set_datetime_sec_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_sec_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_sec_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "nanosec" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_nanosec
     MODULE PROCEDURE set_datetime_nanosec_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_nanosec_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_nanosec_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "zone" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_zone
     MODULE PROCEDURE set_datetime_zone_i_0_0 ! Objekt (Skalar) / Integer (Skalar)
     MODULE PROCEDURE set_datetime_zone_i_1_0 ! Objekt (Vektor) / Integer (Skalar) 
     MODULE PROCEDURE set_datetime_zone_i_1_1 ! Objekt (Vektor) / Integer (Vektor) 
     MODULE PROCEDURE set_datetime_zone_c_0_0 ! Objekt (Skalar) / Character (Skalar)
     MODULE PROCEDURE set_datetime_zone_c_1_0 ! Objekt (Vektor) / Character (Skalar) 
     MODULE PROCEDURE set_datetime_zone_c_1_1 ! Objekt (Vektor) / Character (Vektor) 
  END INTERFACE
  !! Hole Komponente "date" aus "t_datetime"
  INTERFACE get_datetime_date
     MODULE PROCEDURE get_datetime_date_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_date_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "hour" aus "t_datetime"
  INTERFACE get_datetime_hour
     MODULE PROCEDURE get_datetime_hour_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_hour_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "min" aus "t_datetime"
  INTERFACE get_datetime_min
     MODULE PROCEDURE get_datetime_min_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_min_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "sec" aus "t_datetime"
  INTERFACE get_datetime_sec
     MODULE PROCEDURE get_datetime_sec_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_sec_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "nanosec" aus "t_datetime"
  INTERFACE get_datetime_nanosec
     MODULE PROCEDURE get_datetime_nanosec_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_nanosec_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "zone" aus "t_datetime"
  INTERFACE get_datetime_zone
     MODULE PROCEDURE get_datetime_zone_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_zone_1_0 ! Vektor
  END INTERFACE
  !! Hole die aktuelle Spracheinstellung
  INTERFACE get_datetime_language
     MODULE PROCEDURE get_datetime_language_d
  END INTERFACE
  !! Hole die aktuelle voreingestellte Zeitzone
  INTERFACE get_datetime_predefined_zone
     MODULE PROCEDURE get_datetime_predefined_zone_i
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Setze Subkomponente "date%day" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_day
     MODULE PROCEDURE set_datetime_day_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_day_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_day_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Subkomponente "date%month" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_month
     MODULE PROCEDURE set_datetime_month_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_month_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_month_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Subkomponente "date%year" in "t_datetime" auf Benutzerwert
  INTERFACE set_datetime_year
     MODULE PROCEDURE set_datetime_year_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_year_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_year_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze alle Komponenten und Subkomponenten mit einem Aufruf auf Benutzerwerte
  INTERFACE set_datetime
     MODULE PROCEDURE set_datetime_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_datetime_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_datetime_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Hole Subkomponente "date%day" aus "t_datetime"
  INTERFACE get_datetime_day
     MODULE PROCEDURE get_datetime_day_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_day_1_0 ! Vektor
  END INTERFACE
  !! Hole Subkomponente "date%month" aus "t_datetime"
  INTERFACE get_datetime_month
     MODULE PROCEDURE get_datetime_month_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_month_1_0 ! Vektor
  END INTERFACE
  !! Hole Subkomponente "date%year" aus "t_datetime"
  INTERFACE get_datetime_year
     MODULE PROCEDURE get_datetime_year_0_0 ! Skalar
     MODULE PROCEDURE get_datetime_year_1_0 ! Vektor
  END INTERFACE
  !! Zeitzone in einen String wandeln <BR>
  !! abh&auml;ngig von Sprache [Deutsch;Englisch] und Art des Strings (z.Z. BAWstring)
  INTERFACE datetime_zone_to_string
     MODULE PROCEDURE datetime_zone_to_string_0 ! Skalar
     MODULE PROCEDURE datetime_zone_to_string_1 ! Vektor
  END INTERFACE
  !! Objekt "t_datetime" teilweise (ohne Nanosekunden und Zeitzone) <BR>
  !! in einen String mit 19 Zeichen Laenge wandeln; <BR>
  !! abh&auml;ngig von Sprache [Deutsch;Englisch] und Art des Strings; <BR>
  !! z.Z. nur '??.??.yyyy-hh.mm.ss'
  INTERFACE datetime_to_short_string
     MODULE PROCEDURE datetime_to_short_string_0 ! Skalar
     MODULE PROCEDURE datetime_to_short_string_1 ! Vektor
  END INTERFACE
  !! Objekt "t_datetime" teilweise (ohne Zeitzone) in einen String mit 29 Zeichen Laenge wandeln; <BR>
  !! abh&auml;ngig von Sprache [Deutsch;Englisch] und Art des Strings; <BR>
  !! z.Z. nur '??.??.yyyy-hh.mm.ss.nnnnnnnnn' 
  INTERFACE datetime_to_medium_string
     MODULE PROCEDURE datetime_to_medium_string_0 ! Skalar
     MODULE PROCEDURE datetime_to_medium_string_1 ! Vektor
  END INTERFACE
  !! Objekt "t_datetime" in einen String mit 34 Zeichen Laenge wandeln; <BR>
  !! abh&auml;ngig von Sprache [Deutsch;Englisch] und Art des Strings; <BR>
  !! z.Z. nur BAWstring '??.??.yyyy-hh.mm.ss.nnnnnnnnn zone' 
  INTERFACE datetime_to_string
     MODULE PROCEDURE datetime_to_string_0 ! Skalar
     MODULE PROCEDURE datetime_to_string_1 ! Vektor
  END INTERFACE
  !! String in Objekt "t_datetime" wandeln;
  !! abh&auml;ngig von Sprache [Deutsch;Englisch] und Art des Strings; <BR>
  INTERFACE string_to_datetime
     MODULE PROCEDURE string_to_datetime_0 ! Skalar/Skalar
     MODULE PROCEDURE string_to_datetime_1 ! Vektor/Vektor
  END INTERFACE
  !! Zeitinkrement berechnen fuer die Zeit seit Beginn des julianischen Kalenders <BR>
  !! bis zu dem Zeitpunkt, der als Objekt "t_datetime" uebergeben wurde
  INTERFACE datetime_to_julian_time
     MODULE PROCEDURE datetime_to_julian_time_0 ! Skalar
     MODULE PROCEDURE datetime_to_julian_time_1 ! Vektor
  END INTERFACE
  !! Berechne ein Objekt des Typs t_datetime aus dem julianischen Zeitinkrement
  INTERFACE julian_time_to_datetime
     MODULE PROCEDURE julian_time_to_datetime_0 ! Skalar
     MODULE PROCEDURE julian_time_to_datetime_1 ! Vektor
  END INTERFACE
  !! Ermittle das Datum (mit Uhrzeit) mit Hilfe der Systemuhr <BR>
  !! a) f&uuml;r eine Datumsangabe
  INTERFACE get_datetime_from_system
     MODULE PROCEDURE get_datetime_from_system_0
  END INTERFACE
  !! Ermittle den Mittelwert zwischen zwei Objekten vom Datentyp t_datetime <BR>
  INTERFACE mean_datetime
     MODULE PROCEDURE mean_datetime_0
     MODULE PROCEDURE mean_datetime_1
  END INTERFACE
  !! &Uuml;berf&uuml;hre ein Objekt des Typs t_datetime 
  !! in eine neue Zeitzone; <BR>  
  INTERFACE get_datetime_in_changed_zone
     MODULE PROCEDURE get_datetime_in_changed_zone_i0 ! Argument INTEGER, Skalar
     MODULE PROCEDURE get_datetime_in_changed_zone_i1 ! Argument INTEGER, Vektor
     MODULE PROCEDURE get_datetime_in_changed_zone_c0 ! Argument CHARACTER, Skalar
     MODULE PROCEDURE get_datetime_in_changed_zone_c1 ! Argument CHARACTER, Vektor
  END INTERFACE
  !! Runde die Nano-Sekundenanteile auf eine gewisse Anzahl von Nachkommastellen <BR>
  !! a) f&uuml;r einen Argumentwert (Skalar) <BR>
  !! b) f&uuml;r viele Argumentwerte (Vektor) 
  INTERFACE round_datetime
     MODULE PROCEDURE round_datetime_0 ! 
     MODULE PROCEDURE round_datetime_1 ! 
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! [C.6] Operatoren
  !
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Objekte "t_datetime" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_datetime_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_datetime_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_datetime_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE eq_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE eq_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_time_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE eq_date_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_date_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE eq_date_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_date_1_1  ! Vektor / Vektor 
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Addiere ein Zeitinkrement oder eine Anzahl Sekunden (INTEGER) zu einem Objekt t_datetime 
  INTERFACE ad_time_to_datetime
     MODULE PROCEDURE add_time_to_datetime_dt_0_0 ! Argumente in der Reihenfolge t_datetime .. t_time ; Skalar/Skalar
     MODULE PROCEDURE add_time_to_datetime_dt_0_1 ! Argumente in der Reihenfolge t_datetime .. t_time ; Skalar/Vektor
     MODULE PROCEDURE add_time_to_datetime_dt_1_0 ! Argumente in der Reihenfolge t_datetime .. t_time ; Vektor/Skalar
     MODULE PROCEDURE add_time_to_datetime_dt_1_1 ! Argumente in der Reihenfolge t_datetime .. t_time ; Vektor/Vektor
  END INTERFACE
  !! Addiere ein Zeitinkrement oder eine Anzahl Sekunden (INTEGER) zu einem Objekt t_datetime 
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE add_time_to_datetime_dt_0_0 ! Argumente in der Reihenfolge t_datetime .. t_time ; Skalar/Skalar
     MODULE PROCEDURE add_time_to_datetime_dt_0_1 ! Argumente in der Reihenfolge t_datetime .. t_time ; Skalar/Vektor
     MODULE PROCEDURE add_time_to_datetime_dt_1_0 ! Argumente in der Reihenfolge t_datetime .. t_time ; Vektor/Skalar
     MODULE PROCEDURE add_time_to_datetime_dt_1_1 ! Argumente in der Reihenfolge t_datetime .. t_time ; Vektor/Vektor
     MODULE PROCEDURE add_time_to_datetime_td_0_0 ! Argumente in der Reihenfolge t_time .. t_datetime ; Skalar/Skalar
     MODULE PROCEDURE add_time_to_datetime_td_0_1 ! Argumente in der Reihenfolge t_time .. t_datetime ; Skalar/Vektor
     MODULE PROCEDURE add_time_to_datetime_td_1_0 ! Argumente in der Reihenfolge t_time .. t_datetime ; Vektor/Skalar
     MODULE PROCEDURE add_time_to_datetime_td_1_1 ! Argumente in der Reihenfolge t_time .. t_datetime ; Vektor/Vektor
     MODULE PROCEDURE add_seconds_to_datetime_oi_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE add_seconds_to_datetime_oi_1_0 ! Objekt (Vektor) / Daten (Skalar)
     MODULE PROCEDURE add_seconds_to_datetime_oi_0_1 ! Objekt (Skalar) / Daten (Vektor) 
     MODULE PROCEDURE add_seconds_to_datetime_oi_1_1 ! Objekt (Vektor) / Daten (Vektor) 
     MODULE PROCEDURE add_seconds_to_datetime_io_0_0 ! Daten (Skalar) / Objekt (Skalar)
     MODULE PROCEDURE add_seconds_to_datetime_io_1_0 ! Daten (Vektor) / Objekt (Skalar)
     MODULE PROCEDURE add_seconds_to_datetime_io_0_1 ! Daten (Skalar) / Objekt (Vektor) 
     MODULE PROCEDURE add_seconds_to_datetime_io_1_1 ! Daten (Vektor) / Objekt (Vektor) 
!>WIN-NT:     MODULE PROCEDURE ad_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ad_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ad_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ad_time_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE get_add_days_to_date_0_0_oi ! Skalar/Skalar
!>WIN-NT:     MODULE PROCEDURE get_add_days_to_date_1_0_oi ! Vektor/Skalar
!>WIN-NT:     MODULE PROCEDURE get_add_days_to_date_1_1_oi ! Vektor/Vektor
!>WIN-NT:     MODULE PROCEDURE get_add_days_to_date_0_0_io ! Skalar/Skalar
!>WIN-NT:     MODULE PROCEDURE get_add_days_to_date_1_0_io ! Vektor/Skalar
!>WIN-NT:     MODULE PROCEDURE get_add_days_to_date_1_1_io ! Vektor/Vektor 
  END INTERFACE
  !
  !! Subtrahiere eine Zeitdifferenz oder einen Zeitpunkt von einem Objekt des Typs t_datetime
  INTERFACE su_time_from_datetime
     MODULE PROCEDURE sub_time_from_datetime_0_0 ! Skalar/Skalar
     MODULE PROCEDURE sub_time_from_datetime_0_1 ! Skalar/Vektor
     MODULE PROCEDURE sub_time_from_datetime_1_0 ! Vektor/Skalar
     MODULE PROCEDURE sub_time_from_datetime_1_1 ! Vektor/Vektor
  END INTERFACE
  INTERFACE su_datetime
     MODULE PROCEDURE sub_datetime_0_0           ! Skalar/Skalar; Rueckgabewert "time"
     MODULE PROCEDURE sub_datetime_0_1           ! Skalar/Vektor; Rueckgabewert "time"
     MODULE PROCEDURE sub_datetime_1_0           ! Vektor/Skalar; Rueckgabewert "time"
     MODULE PROCEDURE sub_datetime_1_1           ! Vektor/Vektor; Rueckgabewert "time"
  END INTERFACE
  !! Subtrahiere eine Zeitdifferenz oder einen Zeitpunkt von einem Objekt des Typs t_datetime
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE sub_time_from_datetime_0_0 ! Skalar/Skalar
     MODULE PROCEDURE sub_time_from_datetime_0_1 ! Skalar/Vektor
     MODULE PROCEDURE sub_time_from_datetime_1_0 ! Vektor/Skalar
     MODULE PROCEDURE sub_time_from_datetime_1_1 ! Vektor/Vektor
     MODULE PROCEDURE sub_seconds_from_datetime_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE sub_seconds_from_datetime_1_0 ! Objekt (Vektor) / Daten (Skalar)
     MODULE PROCEDURE sub_seconds_from_datetime_0_1 ! Objekt (Skalar) / Daten (Vektor) 
     MODULE PROCEDURE sub_seconds_from_datetime_1_1 ! Objekt (Vektor) / Daten (Vektor) 
     MODULE PROCEDURE sub_datetime_0_0 ! Skalar/Skalar; Rueckgabewert "time"
     MODULE PROCEDURE sub_datetime_0_1 ! Skalar/Vektor; Rueckgabewert "time"
     MODULE PROCEDURE sub_datetime_1_0 ! Vektor/Skalar; Rueckgabewert "time"
     MODULE PROCEDURE sub_datetime_1_1 ! Vektor/Vektor; Rueckgabewert "time"
!>WIN-NT:     MODULE PROCEDURE su_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE su_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE su_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE su_time_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE negate_time_0  ! Negation Skalar
!>WIN-NT:     MODULE PROCEDURE negate_time_1  ! Negation Vektor
!>WIN-NT:     MODULE PROCEDURE get_sub_days_from_date_0_0_oi ! Skalar/Skalar
!>WIN-NT:     MODULE PROCEDURE get_sub_days_from_date_1_0_oi ! Vektor/Skalar
!>WIN-NT:     MODULE PROCEDURE get_sub_days_from_date_1_1_oi ! Vektor/Vektor
!>WIN-NT:     MODULE PROCEDURE get_sub_days_from_date_0_0_oo ! Skalar/Skalar
!>WIN-NT:     MODULE PROCEDURE get_sub_days_from_date_0_1_oo ! Skalar/Vektor
!>WIN-NT:     MODULE PROCEDURE get_sub_days_from_date_1_0_oo ! Vektor/Skalar
!>WIN-NT:     MODULE PROCEDURE get_sub_days_from_date_1_1_oo ! Vektor/Vektor 
  END INTERFACE
  !
  !! Vergleich ">" zweier Objekte "t_datetime"
  INTERFACE gt_datetime
     MODULE PROCEDURE gt_datetime_0_0  ! Skalar / Skalar
     MODULE PROCEDURE gt_datetime_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE gt_datetime_1_0  ! Vektor / Skalar
     MODULE PROCEDURE gt_datetime_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich ">" zweier Objekte "t_datetime"
  INTERFACE OPERATOR(>)
     MODULE PROCEDURE gt_datetime_0_0  ! Skalar / Skalar
     MODULE PROCEDURE gt_datetime_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE gt_datetime_1_0  ! Vektor / Skalar
     MODULE PROCEDURE gt_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE gt_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE gt_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE gt_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE gt_time_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE gt_date_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE gt_date_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE gt_date_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE gt_date_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich ">=" zweier Objekte "t_datetime"
  INTERFACE OPERATOR(>=)
     MODULE PROCEDURE ge_datetime_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ge_datetime_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ge_datetime_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ge_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE ge_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ge_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_time_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE ge_date_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_date_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ge_date_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_date_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich "<" zweier Objekte "t_datetime"
  INTERFACE lt_datetime
     MODULE PROCEDURE lt_datetime_0_0  ! Skalar / Skalar
     MODULE PROCEDURE lt_datetime_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE lt_datetime_1_0  ! Vektor / Skalar
     MODULE PROCEDURE lt_datetime_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "<" zweier Objekte "t_datetime"
  INTERFACE OPERATOR(<) 
     MODULE PROCEDURE lt_datetime_0_0  ! Skalar / Skalar
     MODULE PROCEDURE lt_datetime_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE lt_datetime_1_0  ! Vektor / Skalar
     MODULE PROCEDURE lt_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE lt_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE lt_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE lt_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE lt_time_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE lt_date_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE lt_date_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE lt_date_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE lt_date_1_1  ! Vektor / Vektor 
  END INTERFACE
  !
  !! Vergleich "<=" zweier Objekte "t_datetime"
  INTERFACE OPERATOR(<=)
     MODULE PROCEDURE le_datetime_0_0  ! Skalar / Skalar
     MODULE PROCEDURE le_datetime_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE le_datetime_1_0  ! Vektor / Skalar
     MODULE PROCEDURE le_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE le_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE le_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE le_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE le_time_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE le_date_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE le_date_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE le_date_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE le_date_1_1  ! Vektor / Vektor 
  END INTERFACE
  !
  !! Pr&uuml;fung zweier Objekte "t_datetime" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_datetime_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_datetime_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_datetime_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE ne_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ne_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_time_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE ne_date_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_date_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ne_date_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_date_1_1  ! Vektor / Vektor 
  END INTERFACE
  !
!>WIN-NT:  INTERFACE OPERATOR(/)
!>WIN-NT:     MODULE PROCEDURE di_time_0_I0  ! Skalar / Integer
!>WIN-NT:     MODULE PROCEDURE di_time_1_I0  ! Vektor / Integer
!>WIN-NT:  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_datetime                 ! Initialisieren (Modul)
  PUBLIC :: clear_datetime                ! De-Initialisieren (Modul)
  PUBLIC :: setup_datetime_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_datetime_trc_lun        ! Setzen trc_lun 
  PUBLIC :: setup_datetime_language       ! Setzen der Sprache
  PUBLIC :: setup_datetime_predefined_zone! Setzen der voreingestellten Zeitzone
  PUBLIC :: setup_datetime_string_type    ! Setzen der Art des Strings (z.Z. BAWstring)
  PUBLIC :: new_datetime                  ! Erzeugen 
  PUBLIC :: kill_datetime                 ! Vernichten
  PUBLIC :: ok_datetime                   ! Pruefen
  PUBLIC :: print_datetime                ! Drucken
  PUBLIC :: print_datetime_static         ! Drucken aller statischen Daten
  PUBLIC :: print_datetime_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_datetime_date             ! Setzen der Komponente date
  PUBLIC :: set_datetime_hour             ! Setzen der Komponente hour
  PUBLIC :: set_datetime_min              ! Setzen der Komponente min
  PUBLIC :: set_datetime_sec              ! Setzen der Komponente sec
  PUBLIC :: set_datetime_nanosec          ! Setzen der Komponente nanosec
  PUBLIC :: set_datetime_zone             ! Setzen der Komponente zone
  PUBLIC :: get_datetime_date             ! Holen der Komponente date
  PUBLIC :: get_datetime_hour             ! Holen der Komponente hour
  PUBLIC :: get_datetime_min              ! Holen der Komponente min
  PUBLIC :: get_datetime_sec              ! Holen der Komponente sec
  PUBLIC :: get_datetime_nanosec          ! Holen der Komponente nanosec
  PUBLIC :: get_datetime_zone             ! Holen der Komponente zone
  PUBLIC :: get_datetime_language         ! Holen der aktuellen Spracheinstellung
  PUBLIC :: get_datetime_predefined_zone  ! Holen der voreingestellten Zeitzone
  !
  PUBLIC :: OPERATOR(==)                  ! Operator "=="
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: set_datetime_day              ! Setzen der Subkomponente date%day
  PUBLIC :: set_datetime_month            ! Setzen der Subkomponente date%month
  PUBLIC :: set_datetime_year             ! Setzen der Subkomponente date%year
  PUBLIC :: set_datetime                  ! Setzen aller Komponenten und Subkomponenten
  !
  PUBLIC :: get_datetime_day              ! Holen der Subkomponente date%day
  PUBLIC :: get_datetime_month            ! Holen der Subkomponente date%month
  PUBLIC :: get_datetime_year             ! Holen der Subkomponente date%year
  !
  PUBLIC :: datetime_zone_to_string       ! Zeitzone in String wandeln
  PUBLIC :: datetime_to_short_string      ! Objekt "t_datetime" teilweise in String (LEN=19) wandeln
  PUBLIC :: datetime_to_medium_string     ! Objekt "t_datetime" teilweise in String (LEN=29) wandeln
  PUBLIC :: datetime_to_string            ! Objekt "t_datetime" vollstaendig in String (LEN=34) wandeln
  PUBLIC :: string_to_datetime            ! String in Datum, Uhrzeit und Zeitzone wandeln
  PUBLIC :: datetime_to_julian_time       ! datetime in julianisches Zeitinkrement wandeln
  PUBLIC :: julian_time_to_datetime       ! julianisches Zeitinkrement in datetime wandeln
  PUBLIC :: get_datetime_in_changed_zone  ! Datum, Uhrzeit und Zeitzone mit Hilfe der Systemuhr ermitteln
  PUBLIC :: get_datetime_from_system      ! Datum, Uhrzeit und Zeitzone mit Hilfe der Systemuhr ermitteln
  PUBLIC :: mean_datetime                 ! Mittelwert zwischen zwei Zeitpunkten ermitteln
  PUBLIC :: round_datetime                ! Nachkommsatellen runden
  !
  PUBLIC :: ad_time_to_datetime
  PUBLIC :: su_time_from_datetime
  PUBLIC :: su_datetime
  PUBLIC :: gt_datetime                   ! 
  PUBLIC :: lt_datetime                   ! 
  PUBLIC :: OPERATOR(+)                   ! Operator "+"
  PUBLIC :: OPERATOR(-)                   ! Operator "-"
  PUBLIC :: OPERATOR(>)                   ! Operator ">"
  PUBLIC :: OPERATOR(>=)                  ! Operator ">="
  PUBLIC :: OPERATOR(<)                   ! Operator "<"
  PUBLIC :: OPERATOR(<=)                  ! Operator "<="
  PUBLIC :: OPERATOR(/=)                  ! Operator "/="
  !
!>WIN-NT:  PUBLIC:: eq_datetime_0_0, ne_datetime_0_0, lt_datetime_0_0, le_datetime_0_0
!>WIN-NT:  PUBLIC:: gt_datetime_0_0, ge_datetime_0_0
!>WIN-NT:  PUBLIC:: eq_datetime_0_1, ne_datetime_0_1, lt_datetime_0_1, le_datetime_0_1
!>WIN-NT:  PUBLIC:: gt_datetime_0_1, ge_datetime_0_1
!>WIN-NT:  PUBLIC:: eq_datetime_1_0, ne_datetime_1_0, lt_datetime_1_0, le_datetime_1_0
!>WIN-NT:  PUBLIC:: gt_datetime_1_0, ge_datetime_1_0
!>WIN-NT:  PUBLIC:: eq_datetime_1_1, ne_datetime_1_1, lt_datetime_1_1, le_datetime_1_1
!>WIN-NT:  PUBLIC:: gt_datetime_1_1, ge_datetime_1_1
!>WIN-NT:  PUBLIC:: sub_time_from_datetime_0_0, sub_time_from_datetime_0_1
!>WIN-NT:  PUBLIC:: sub_time_from_datetime_1_0, sub_time_from_datetime_1_1
!>WIN-NT:  PUBLIC:: sub_seconds_from_datetime_0_0, sub_seconds_from_datetime_1_0
!>WIN-NT:  PUBLIC:: sub_seconds_from_datetime_0_1, sub_seconds_from_datetime_1_1
!>WIN-NT:  PUBLIC:: sub_datetime_0_0, sub_datetime_0_1, sub_datetime_1_0, sub_datetime_1_1
!>WIN-NT:  PUBLIC:: add_time_to_datetime_dt_0_0, add_time_to_datetime_dt_0_1
!>WIN-NT:  PUBLIC:: add_time_to_datetime_dt_1_0, add_time_to_datetime_dt_1_1
!>WIN-NT:  PUBLIC:: add_time_to_datetime_td_0_0, add_time_to_datetime_td_0_1
!>WIN-NT:  PUBLIC:: add_time_to_datetime_td_1_0, add_time_to_datetime_td_1_1
!>WIN-NT:  PUBLIC:: add_seconds_to_datetime_oi_0_0, add_seconds_to_datetime_oi_1_0
!>WIN-NT:  PUBLIC:: add_seconds_to_datetime_oi_0_1, add_seconds_to_datetime_oi_1_1
!>WIN-NT:  PUBLIC:: add_seconds_to_datetime_io_0_0, add_seconds_to_datetime_io_1_0
!>WIN-NT:  PUBLIC:: add_seconds_to_datetime_io_0_1, add_seconds_to_datetime_io_1_1
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'b_datetime' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false. ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1 ! 
  !! Anzahl der Datenkomponenten des Typs t_datetime
  INTEGER           , PARAMETER :: c_nofcomp      = 6 !         
  !! Anzahl einstellbarer Sprachen
  INTEGER           , PARAMETER :: c_max_language = 2                ! [Deutsch,Englisch]
  !! Default-Language
  INTEGER           , PARAMETER :: c_def_language = 1                ! [Deutsch]
  !! Default der voreingestellten Zeitzone
  INTEGER           , PARAMETER :: c_def_predefined_zone = 1         ! [MEZ]
  !! Anzahl einstellbarer Arten des Strings
  INTEGER           , PARAMETER :: c_max_string_type = 1             ! [BAWstring]
  !! Default der Art des Strings
  INTEGER           , PARAMETER :: c_def_string_type = 1             ! [BAWstring]
  !! "spaeteste" Zeitzone
  INTEGER           , PARAMETER :: c_latest_zone  = -11 ! 
  !! "frueheste" Zeitzone
  INTEGER           , PARAMETER :: c_earliest_zone = 12 ! 
  !! englische Namen der Zeitzonen von West nach Ost; <BR>
  !! s. auch datetime_zone_to_string_0
  CHARACTER (LEN=4) , PARAMETER :: c_zone(c_latest_zone:c_earliest_zone)= & ! 
       (/ "-11H", "-10H", "-11H", "PST ", "MST ", "CST ", & ! 
       "EST ", "AST ", "-3H ", "-2H ", "-1H ", "UTC ", &    ! 
       "CET ", "CEST", "+3H ", "+4H ", "+5H ", "+6H ", &    ! 
       "+7H ", "+8H ", "+9H ", "+10H", "+11H", "+12H"/)     ! 
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
  !! aktuelle Sprache
  INTEGER                , SAVE :: language    = c_def_language  ! [Deutsch]
  !! aktuelle voreingestellte Zeitzone, die beim date_time-Konstrukor verwendet wird
  INTEGER                , SAVE :: predefined_zone = c_def_predefined_zone ! [MEZ]
  !! aktuelle Art des Strings
  INTEGER                , SAVE :: string_type = c_def_string_type ! [BAWstring]
  !! Referenzdatum zum Berechnen des mod. Julianischen Datums
  TYPE (t_datetime)      , SAVE :: ref_datetime           ! 
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
  SUBROUTINE init_datetime_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='init_datetime_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_datetime" version 1.15 of 03/26/07                 '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis_module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       CALL init_date ( )
       CALL init_time ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       ! IF ( no_error( ) ) CALL init_datetime ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_datetime_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] Initialisieren der Sprache            
       IF ( no_error( ) ) CALL setup_datetime_language ( 1 )
       IF ( no_error( ) ) CALL new_datetime( ref_datetime )
       IF ( no_error( ) ) ref_datetime = string_to_datetime( '17.11.1858-00:00:00.000000000 UTC ' )
       ! [1.7] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! [2.0] Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_datetime_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_datetime_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='clear_datetime_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_datetime_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_time ( )
       IF ( no_error( ) ) CALL clear_date ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! [2.0] Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_datetime_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_datetime_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_datetime_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       ! ... ggf. weitere ergaenzen !!
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_date_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_time_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_datetime_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_datetime_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_datetime_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       ! ... ggf. weitere ergaenzen !!
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_date_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_time_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_datetime_trc_lun_d
  !
  !! Setzen des Index f&uuml;r die Spracheinstellung fuer b_datetime _und_ b_date<BR>
  !! 1 = Deutsch <BR>
  !! 2 = Englisch <BR>
  !! Anzahl und Reihenfolge der unterstuetzten Sprachen muessen in b_datetime und b_date uebereinstimmen<BR>
  SUBROUTINE setup_datetime_language_d ( act_language )
    !! Index f&uuml;r Spracheinstellung
    INTEGER , INTENT(IN) :: act_language ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_datetime_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       language = MERGE ( act_language, c_def_language, ( 1 <= act_language .AND. act_language <= c_max_language ) )
       CALL setup_date_language ( act_language )
    END IF
    !
  END SUBROUTINE setup_datetime_language_d
  !
  !! Setzen der voreingestellten Zeitzone, die beim Aufrufen von
  !! new_datetime verwendet wird
  SUBROUTINE setup_datetime_predefined_zonei ( act_predefined_zone )
    !! aktuelle voreingestellte Zeitzone
    INTEGER , INTENT(IN) :: act_predefined_zone ! 
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='setup_datetime_predefined_zonei' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       predefined_zone = act_predefined_zone
    END IF
    !
  END SUBROUTINE setup_datetime_predefined_zonei
  !
  !! Setzen der voreingestellten Zeitzone, die beim Aufrufen von
  !! new_datetime verwendet wird <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_datetime_predefined_zonec ( val )
    !! String mit voreingestellter Zeitzone 
    CHARACTER (LEN=4), INTENT(IN)  :: val  ! 
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_datetime_predefined_zonec' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    SELECT CASE (val)
       !
       CASE ('-11H')
          predefined_zone = -11
       CASE ('-10H')
          predefined_zone = -10
       CASE ('PST ', '-9H')
          predefined_zone = -9 
       CASE ('MST ', '-8H')
          predefined_zone = -8
       CASE ('CST ', '-7H')
          predefined_zone = -7 
       CASE ('EST ', '-6H')
          predefined_zone = -6
       CASE ('AST ', '-5H')
          predefined_zone = -5
       CASE ('-4H ')
          predefined_zone = -4
       CASE ('-3H ')
          predefined_zone = -3
       CASE ('-2H ')
          predefined_zone = -2
       CASE ('-1H ')
          predefined_zone = -1
       CASE ('UTC ', 'GMT ', '+0H ', '0H  ')
          predefined_zone = 0
       CASE ('CET ', 'MEZ ', '+1H ')
          predefined_zone = 1
       CASE ('CEST', 'MESZ', '+2H ')
          predefined_zone = 2
       CASE ('+3H ')
          predefined_zone = 3
       CASE ('+4H ')
          predefined_zone = 4
       CASE ('+5H ')
          predefined_zone = 5
       CASE ('+6H ')
          predefined_zone = 6
       CASE ('+7H ')
          predefined_zone = 7
       CASE ('+8H ')
          predefined_zone = 8
       CASE ('+9H ')
          predefined_zone = 9
       CASE ('+10H')
          predefined_zone = 10
       CASE ('+11H')
          predefined_zone = 11
       CASE ('+12H')
          predefined_zone = 12
       CASE DEFAULT
          !
          !
          IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein           
             !
             CALL setup_error_act (all_errors(:), -1070, c_upname, c_modname, stat )
             !
             CALL setup_error_act &
                  ('<..zone..>', val)
          ENDIF
          !
    END SELECT
    !
  END SUBROUTINE setup_datetime_predefined_zonec
  !
  !! Setzen des Index f&uuml;r Art des Strings <BR>
  !! 1 = BAWstring  <BR>
  !!   - language = 1 (deutsch)
  !!         'tt.mm.jjjj-hh:mm:ss.nnnnnnnnn zone'  (LEN=34)
  !!         'tt.mm.jjjj-hh:mm:ss.nnnnnnnnn'  (LEN=29), medium
  !!         'tt.mm.jjjj-hh:mm:ss'  (LEN=19), short
  !!   - language = 2 (englisch)
  !!         'mm/dd/yyyy-hh:mm:ss.nnnnnnnnn zone'  (LEN=34)
  !!         'mm/dd/yyyy-hh:mm:ss.nnnnnnnnn'  (LEN=29), medium
  !!         'mm/dd/yyyy-hh:mm:ss'  (LEN=19), short
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_datetime_string_type_d ( act_string_type )
    !! Index f&uuml;r Art des Strings
    INTEGER , INTENT(IN) :: act_string_type ! 
    ! Lokale Parameter und Variablen
    CHARACTER (LEN=28), PARAMETER :: c_upname='setup_datetime_string_type_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       string_type = MERGE ( act_string_type, 1, ( 1 <= act_string_type .AND. act_string_type <= c_max_string_type ) )
    END IF
    !
  END SUBROUTINE setup_datetime_string_type_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Objekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_datetime_0 ( this )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_datetime_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       CALL new_date &
            (this%date)
       !
       this%hour = 0
       this%min  = 0
       this%sec  = 0
       this%nanosec  = 0
       this%zone = predefined_zone  ! Default-Zeitzone, die per 
                                    ! setup_datetime_predefined_zone gesetzt werden kann
       !
    END IF
    !
  END SUBROUTINE new_datetime_0
  !
  !! Initialisieren eines neuen Objekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_datetime_1 ( this )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_datetime_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_datetime_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_datetime_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Objekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_datetime_0 ( this )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_datetime_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_datetime_0 ( this )
    END IF
    !
  END SUBROUTINE kill_datetime_0
  !
  !! De-Allokieren/De-Initialisieren eines Objekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_datetime_1 ( this )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_datetime_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_datetime_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_datetime_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Objekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_datetime_0 ( this ) &
       RESULT( ok )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_datetime_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_date( this%date )
       l_ok(2)  = ok_datetime_hour( this )
       l_ok(3)  = ok_datetime_min( this )
       l_ok(4)  = ok_datetime_sec( this )
       l_ok(5)  = ok_datetime_nanosec( this )
       l_ok(6)  = ok_datetime_zone( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_datetime_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Objekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_datetime_1 ( this ) &
       RESULT( ok )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_datetime_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) ) EXIT
          ok(i) = ok_datetime_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_datetime_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Objektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_0 ( this )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_datetime_0' 
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
       IF ( no_error( ) ) CALL print_datetime_date( this )
       IF ( no_error( ) ) CALL print_datetime_hour( this )
       IF ( no_error( ) ) CALL print_datetime_min( this )
       IF ( no_error( ) ) CALL print_datetime_sec( this )
       IF ( no_error( ) ) CALL print_datetime_nanosec( this )
       IF ( no_error( ) ) CALL print_datetime_zone( this )
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
8000 FORMAT('# Beginn Objekt t_datetime ------------------------------')
8001 FORMAT('# Ende   Objekt t_datetime ------------------------------')
    !
  END SUBROUTINE print_datetime_0
  !
  !! Drucke den Inhalt eines Objektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_1 ( this )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_datetime_1' 
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
          IF ( no_error( ) ) CALL print_datetime_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
8000 FORMAT ('# Objekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_datetime_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_static_d ( )
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_datetime_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, &
           TRIM(datetime_to_string(ref_datetime))
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_datetime_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_datetime         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#  initialised = ',L1,/ &
    '#       prn_op = ',L1,/ &
    '#       trc_op = ',L1,/ &
    '#      prn_lun = ',I5,/ &
    '#      trc_lun = ',I5,/ &
    '#       n_init = ',I5,/ &
    '# ref_datetime = ',A,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_datetime_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_datetime_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_datetime_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "date" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_date_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "date"
    TYPE (t_date)       , INTENT(IN)  :: val  ! 
    !
    this%date = val
    !
  END SUBROUTINE set_datetime_date_0_0
  !
  !! weise der Komponente "date" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_date_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "date"
    TYPE (t_date)       , INTENT(IN)  :: val     ! 
    !
    this%date = val
    !
  END SUBROUTINE set_datetime_date_1_0
  !
  !! weise der Komponente "date" einen Vektor zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_date_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "date"
    TYPE (t_date)       , INTENT(IN)  :: val(:)  ! 
    !
    this(:)%date = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_datetime_date_1_1
  !
  !! weise der Subkomponente "date%day" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_day_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Subkomponente "date%day"
    INTEGER       , INTENT(IN)  :: val  ! 
    !
    CALL set_date_day(this%date, val)
    !
  END SUBROUTINE set_datetime_day_0_0
  !
  !! weise der Subkomponente "date%day" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_day_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Subkomponente "date%day"
    INTEGER       , INTENT(IN)  :: val     ! 
    !
    CALL set_date_day (this(:)%date, val)
    !
  END SUBROUTINE set_datetime_day_1_0
  !
  !! weise der Subkomponente "date%day" einen Vektor zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_day_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "date"
    INTEGER       , INTENT(IN)  :: val(:)  ! 
    !
    CALL set_date_day (this(:)%date, val(:))
    !
  END SUBROUTINE set_datetime_day_1_1
  !
  !! weise der Subkomponente "date%month" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_month_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Subkomponente "date%month"
    INTEGER       , INTENT(IN)  :: val  ! 
    !
    CALL set_date_month(this%date, val)
    !
  END SUBROUTINE set_datetime_month_0_0
  !
  !! weise der Subkomponente "date%month" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_month_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Subkomponente "date%month"
    INTEGER       , INTENT(IN)  :: val     ! 
    !
    CALL set_date_month (this(:)%date, val)
    !
  END SUBROUTINE set_datetime_month_1_0
  !
  !! weise der Subkomponente "date%month" einen Vektor zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_month_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "date"
    INTEGER       , INTENT(IN)  :: val(:)  ! 
    !
    CALL set_date_month (this(:)%date, val(:))
    !
  END SUBROUTINE set_datetime_month_1_1
  !
  !
  !! weise der Subkomponente "date%year" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_year_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Subkomponente "date%year"
    INTEGER       , INTENT(IN)  :: val  ! 
    !
    CALL set_date_year(this%date, val)
    !
  END SUBROUTINE set_datetime_year_0_0
  !
  !! weise der Subkomponente "date%year" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_year_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Subkomponente "date%year"
    INTEGER       , INTENT(IN)  :: val     ! 
    !
    CALL set_date_year (this(:)%date, val)
    !
  END SUBROUTINE set_datetime_year_1_0
  !
  !! weise der Subkomponente "date%year" einen Vektor zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_year_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "date"
    INTEGER       , INTENT(IN)  :: val(:)  ! 
    !
    CALL set_date_year (this(:)%date, val(:))
    !
  END SUBROUTINE set_datetime_year_1_1
  !
  !! weise der Komponente "hour" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_hour_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "hour"
    INTEGER     , INTENT(IN)  :: val  ! 
    !
    this%hour = val
    !
  END SUBROUTINE set_datetime_hour_0_0
  !
  !! weise der Komponente "hour" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_hour_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "hour"
    INTEGER           , INTENT(IN)  :: val     ! 
    !
    this%hour = val
    !
  END SUBROUTINE set_datetime_hour_1_0
  !
  !! weise der Komponente "hour" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_hour_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "hour"
    INTEGER           , INTENT(IN)  :: val(:)  ! 
    !
    this(:)%hour = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_datetime_hour_1_1
  !
  !! weise der Komponente "min" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_min_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "min"
    INTEGER           , INTENT(IN)  :: val  ! 
    !
    this%min = val
    !
  END SUBROUTINE set_datetime_min_0_0
  !
  !! weise der Komponente "min" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_min_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "min"
    INTEGER           , INTENT(IN)  :: val     ! 
    !
    this%min = val
    !
  END SUBROUTINE set_datetime_min_1_0
  !
  !! weise der Komponente "min" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_min_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "min"
    INTEGER           , INTENT(IN)  :: val(:)  ! 
    !
    this(:)%min = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_datetime_min_1_1
  !
  !! weise der Komponente "sec" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_sec_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "sec"
    INTEGER            , INTENT(IN)  :: val  ! 
    !
    this%sec = val
    !
  END SUBROUTINE set_datetime_sec_0_0
  !
  !! weise der Komponente "sec" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_sec_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "sec"
    INTEGER            , INTENT(IN)  :: val     ! 
    !
    this%sec = val
    !
  END SUBROUTINE set_datetime_sec_1_0
  !
  !! weise der Komponente "sec" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_sec_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "sec"
    INTEGER            , INTENT(IN)  :: val(:)  ! 
    !
    this(:)%sec = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_datetime_sec_1_1
  !
  !! weise der Komponente "nanosec" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_nanosec_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "nanosec"
    INTEGER            , INTENT(IN)  :: val  ! 
    !
    this%nanosec = val
    !
  END SUBROUTINE set_datetime_nanosec_0_0
  !
  !! weise der Komponente "nanosec" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_nanosec_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "nanosec"
    INTEGER            , INTENT(IN)  :: val     ! 
    !
    this%nanosec = val
    !
  END SUBROUTINE set_datetime_nanosec_1_0
  !
  !! weise der Komponente "nanosec" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_nanosec_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "nanosec"
    INTEGER            , INTENT(IN)  :: val(:)  ! 
    !
    this(:)%nanosec = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_datetime_nanosec_1_1
  !
  !! weise der Komponente "zone" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_zone_i_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "zone"
    INTEGER            , INTENT(IN)  :: val  ! 
    !
    this%zone = val
    !
  END SUBROUTINE set_datetime_zone_i_0_0
  !
  !! weise der Komponente "zone" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_zone_i_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "zone"
    INTEGER            , INTENT(IN)  :: val     ! 
    !
    this%zone = val
    !
  END SUBROUTINE set_datetime_zone_i_1_0
  !
  !! weise der Komponente "zone" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_zone_i_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "zone"
    INTEGER            , INTENT(IN)  :: val(:)  ! 
    !
    this(:)%zone = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_datetime_zone_i_1_1
  !! weise der Komponente "zone" durch einen skalaren String einen Wert zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_datetime_zone_c_0_0 ( this, val )
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this ! 
    !! String mit Angabe zur Zeitzone (Skalar)
    CHARACTER (LEN=4), INTENT(IN)  :: val  ! 
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_datetime_zone_c_0_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    SELECT CASE (val)
       !
       CASE ('-11H')
          this%zone = -11
       CASE ('-10H')
          this%zone = -10
       CASE ('PST ', '-9H')
          this%zone = -9 
       CASE ('MST ', '-8H')
          this%zone = -8
       CASE ('CST ', '-7H')
          this%zone = -7 
       CASE ('EST ', '-6H')
          this%zone = -6
       CASE ('AST ', '-5H')
          this%zone = -5
       CASE ('-4H ')
          this%zone = -4
       CASE ('-3H ')
          this%zone = -3
       CASE ('-2H ')
          this%zone = -2
       CASE ('-1H ')
          this%zone = -1
       CASE ('UTC ', 'GMT ', '+0H ', '0H  ')
          this%zone = 0
       CASE ('CET ', 'MEZ ', '+1H ')
          this%zone = 1
       CASE ('CEST', 'MESZ', '+2H ')
          this%zone = 2
       CASE ('+3H ')
          this%zone = 3
       CASE ('+4H ')
          this%zone = 4
       CASE ('+5H ')
          this%zone = 5
       CASE ('+6H ')
          this%zone = 6
       CASE ('+7H ')
          this%zone = 7
       CASE ('+8H ')
          this%zone = 8
       CASE ('+9H ')
          this%zone = 9
       CASE ('+10H')
          this%zone = 10
       CASE ('+11H')
          this%zone = 11
       CASE ('+12H')
          this%zone = 12
       CASE DEFAULT
          !
          IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein           
             !
             CALL setup_error_act (all_errors(:), -1040, c_upname, c_modname, stat )
             !
             CALL setup_error_act &
                  ('<..zone..>', val)
          ENDIF
          !
    END SELECT
    !
  END SUBROUTINE set_datetime_zone_c_0_0
  !
  !! weise der Komponente "zone" durch einen skalaren String Werte zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_datetime_zone_c_1_0 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! String mit Angabe zur Zeitzone (Skalar)
    CHARACTER (LEN=4), INTENT(IN)  :: val
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1, SIZE(this)
       CALL set_datetime_zone_c_0_0 &
            (this(i), val)
    ENDDO
    !
  END SUBROUTINE set_datetime_zone_c_1_0
  !
  !! weise der Komponente "zone" (Vektor) durch einen String (Vektor) Werte zu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_datetime_zone_c_1_1 ( this, val )
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! String mit Angabe zur Zeitzone (Skalar)
    CHARACTER (LEN=4), INTENT(IN)  :: val(:)
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1, MIN(SIZE(this),SIZE(val))
       CALL set_datetime_zone_c_0_0 &
            (this(i), val(i))
    ENDDO
    !
  END SUBROUTINE set_datetime_zone_c_1_1
  !
  !! weise allen Komponenten und Subkomponenten mit einem Aufruf einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_0_0 ( this, day, month, year, hour, min, sec, nanosec, zone)
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this 
    !! Werte f&uuml;r Subkomponenten "day", "month", "year"
    INTEGER            , INTENT(IN)  :: day, month, year
    !! Werte f&uuml;r Komponenten "hour", "min", "sec", "nanosec"
    INTEGER            , INTENT(IN)  :: hour, min, sec, nanosec
    !! Wert f&uuml;r Komponente "zone"
    INTEGER            , INTENT(IN)  :: zone
    !
    CALL set_date_day   ( this%date, day )
    CALL set_date_month ( this%date, month )
    CALL set_date_year  ( this%date, year )
    CALL set_datetime_hour  ( this, hour )
    CALL set_datetime_min   ( this, min )
    CALL set_datetime_sec   ( this, sec )
    CALL set_datetime_nanosec ( this, nanosec )
    CALL set_datetime_zone  ( this, zone )
    !
  END SUBROUTINE set_datetime_0_0
  !
  !! weise allen Komponenten und Subkomponenten mit einem Aufruf einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_1_0 (this, day, month, year, hour, min, sec, nanosec, zone)
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Subkomponenten "day", "month", "year"
    INTEGER            , INTENT(IN)  :: day, month, year
    !! Werte f&uuml;r Komponenten "hour", "min", "sec", "nanosec"
    INTEGER            , INTENT(IN)  :: hour, min, sec, nanosec
    !! Wert f&uuml;r Komponente "zone"
    INTEGER            , INTENT(IN)  :: zone
    !
    CALL set_date_day ( this%date, day )
    CALL set_date_month ( this%date, month )
    CALL set_date_year ( this%date, year )
    CALL set_datetime_hour ( this, hour )
    CALL set_datetime_min ( this, min )
    CALL set_datetime_sec ( this, sec )
    CALL set_datetime_nanosec ( this, nanosec )
    CALL set_datetime_zone ( this, zone )
    !
  END SUBROUTINE set_datetime_1_0
  !
  !! weise allen Komponenten und Subkomponenten mit einem Aufruf Felder zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_datetime_1_1 ( this, day, month, year, hour, min, sec, nanosec, zone)
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Subkomponenten "day", "month", "year"
    INTEGER            , INTENT(IN)  :: day(:), month(:), year(:)
    !! Werte f&uuml;r Komponenten "hour", "min", "sec", "nanosec"
    INTEGER            , INTENT(IN)  :: hour(:), min(:), sec(:), nanosec(:)
    !! Wert f&uuml;r Komponente "zone"
    INTEGER            , INTENT(IN)  :: zone(:)
    !
    CALL set_date_day   ( this(:)%date, day(:) )
    CALL set_date_month ( this(:)%date, month(:) )
    CALL set_date_year  ( this(:)%date, year(:) )
    CALL set_datetime_hour_1_1 ( this(:), hour(:) )
    CALL set_datetime_min_1_1  ( this(:), min(:) )
    CALL set_datetime_sec_1_1  ( this(:), sec(:) )
    CALL set_datetime_nanosec_1_1   ( this(:), nanosec(:) )
    CALL set_datetime_zone_i_1_1 ( this(:), zone(:) )
    !  
  END SUBROUTINE set_datetime_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "date" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_date_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "date" (Skalar)
    TYPE (t_date) :: val  ! 
    !
    val = this%date
    !
  END FUNCTION get_datetime_date_0_0
  !
  !! hole die Komponente "date" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_date_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "date"
    TYPE (t_date) :: val(SIZE(this))  ! 
    !
    val = this%date
    !
  END FUNCTION get_datetime_date_1_0
  !
  !! hole die Komponente "day" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_day_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "day" (Skalar)
    INTEGER  :: val  ! 
    !
    val = get_date_day (this%date)
    !
  END FUNCTION get_datetime_day_0_0
  !
  !! hole die Komponente "day" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_day_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "day"
    INTEGER  :: val(SIZE(this))  ! 
    !
    val = get_date_day (this%date)
    !
  END FUNCTION get_datetime_day_1_0
  !
  !! hole die Komponente "month" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_month_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "month" (Skalar)
    INTEGER  :: val  ! 
    !
    val = get_date_month (this%date)
    !
  END FUNCTION get_datetime_month_0_0
  !
  !! hole die Komponente "month" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_month_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "month"
    INTEGER  :: val(SIZE(this))  ! 
    !
    val = get_date_month (this%date)
    !
  END FUNCTION get_datetime_month_1_0
  !
  !! hole die Komponente "year" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_year_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "year" (Skalar)
    INTEGER  :: val  ! 
    !
    val = get_date_year (this%date)
    !
  END FUNCTION get_datetime_year_0_0
  !
  !! hole die Komponente "year" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_year_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "year"
    INTEGER  :: val(SIZE(this))  ! 
    !
    val = get_date_year (this%date)
    !
  END FUNCTION get_datetime_year_1_0
  !
  !! hole die Komponente "hour" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_hour_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "hour" (Skalar)
    INTEGER  :: val  ! 
    !
    val = this%hour
    !
  END FUNCTION get_datetime_hour_0_0
  !
  !! hole die Komponente "hour" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_hour_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "hour"
    INTEGER  :: val(SIZE(this))  ! 
    !
    val = this%hour
    !
  END FUNCTION get_datetime_hour_1_0
  !
  !! hole die Komponente "min" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_min_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "min" (Skalar)
    INTEGER  :: val  ! 
    !
    val = this%min
    !
  END FUNCTION get_datetime_min_0_0
  !
  !! hole die Komponente "min" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_min_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "min"
    INTEGER  :: val(SIZE(this))  ! 
    !
    val = this%min
    !
  END FUNCTION get_datetime_min_1_0
  !
  !! hole die Komponente "sec" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_sec_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "sec" (Skalar)
    INTEGER  :: val  ! 
    !
    val = this%sec
    !
  END FUNCTION get_datetime_sec_0_0
  !
  !! hole die Komponente "sec" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_sec_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "sec"
    INTEGER  :: val(SIZE(this))  ! 
    !
    val = this%sec
    !
  END FUNCTION get_datetime_sec_1_0
  !
  !! hole die Komponente "nanosec" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_nanosec_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "nanosec" (Skalar)
    INTEGER  :: val  ! 
    !
    val = this%nanosec
    !
  END FUNCTION get_datetime_nanosec_0_0
  !
  !! hole die Komponente "nanosec" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_nanosec_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "nanosec"
    INTEGER  :: val(SIZE(this))  ! 
    !
    val = this%nanosec
    !
  END FUNCTION get_datetime_nanosec_1_0
  !
  !! hole die Komponente "zone" aus einem skalaren Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_zone_0_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "zone" (Skalar)
    INTEGER  :: val  ! 
    !
    val = this%zone
    !
  END FUNCTION get_datetime_zone_0_0
  !
  !! Hole die aktuelle Spracheinstellung <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_language_d ( ) &
       RESULT( val )
    !! R&uuml;ckgabewert : Spracheinstellung
    INTEGER :: val ! 
    !
    val = language
    !
  END FUNCTION get_datetime_language_d
  !
  !! Hole die aktuelle voreingestellte Zeitzone als Integer, die beim Aufrufen von
  !! new_datetime verwendet wird. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_predefined_zone_i ( ) &
       RESULT( val )
    !! R&uuml;ckgabewert : voreingestellte Zeitzone
    INTEGER :: val ! 
    !
    val = predefined_zone
    !
  END FUNCTION get_datetime_predefined_zone_i
  !
  !! hole die Komponente "zone" aus einem vektoriellen Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_zone_1_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "zone"
    INTEGER  :: val(SIZE(this))  ! 
    !
    val = this%zone
    !
  END FUNCTION get_datetime_zone_1_0
  !
  !! Ermittle einen String mit der Zeitzonenangabe (Skalar) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_zone_to_string_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this     ! 
    !! Zeitzonen-String <BR> 
    !! language=1 (Deutsch)  : 'MEZ' und 'MESZ' <BR> 
    !! language=2 (Englisch) : 'CET' und 'CEST'
    CHARACTER (LEN=4) :: val ! 
    !
    IF (this%zone == 1) THEN
       !
       SELECT CASE ( language )
       CASE ( 1 )
          val(1:4) = 'MEZ '
       CASE ( 2 )
          val(1:4) = 'CET '       
       END SELECT
       !
    ELSEIF (this%zone == 2) THEN
       !
       SELECT CASE ( language )
       CASE ( 1 )
          val(1:4) = 'MESZ'
       CASE ( 2 )
          val(1:4) = 'CEST'       
       END SELECT
       !
       ! andernfalls Zeitzonen-String unabhaengig von der Sprache [Deutsch;Englisch]
    ELSE
       !
       val(1:4) = c_zone(get_datetime_zone(this))
       !
    ENDIF
    !
  END FUNCTION datetime_zone_to_string_0 
  !
  !! Ermittle Strings mit Zeitzonenangaben (Vektor) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_zone_to_string_1 ( this ) &
       RESULT( val ) 
    !! Objekt (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this(:) 
    !! Zeitzonen-String <BR> 
    !! language=1 (Deutsch)  : 'MEZ' und 'MESZ'
    !! language=2 (Englisch) : 'CET' und 'CEST'
    CHARACTER (LEN=4) :: val(SIZE(this))
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = datetime_zone_to_string_0 ( this(i) )
    END DO
    !
  END FUNCTION datetime_zone_to_string_1
  !
  !! Objekt "t_datetime" teilweise in einen String mit 19 Zeichen Laenge wandeln (Skalar), <BR>
  !! z.Z. nur BAWstring '??.??.yyyy-hh.mm.ss'; 
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "string_type" und "language"; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_to_short_string_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this     ! 
    !! Datums-String <BR> 
    !! fuer string_type=1 gilt: <BR>
    !!  - language=1    :      '24.12.2000-11:11:11' <BR>
    !!  - language=2    :      '12/24/2000-11:11:11'
    !                           1234567890123456789
    CHARACTER (LEN=19) :: val ! 
    !! Fehlervariable
    INTEGER :: stat ! ... damit kein Abbruch bei fehlerhaftem Schreiben erfolgt
    !
    SELECT CASE (string_type)
       !
       ! BAWstring liegt vor
    CASE (1)
       !
       val( 1:10) = date_to_string (this%date)
       val(11:11) = '-'
       !
       WRITE(val(12:19),'(2(I2.2,A1),I2.2)',IOSTAT=stat) &
            this%hour, ":", this%min, ":", this%sec
       !
!   Hinweis an Programmierer:
!   an dieser Stelle das Lesen von Stringarten, die von BAWstring verschieden
!   sind, einhaengen
!   CASE (n) 
!      <Code ergaenzen>
!
       !
    END SELECT
  END FUNCTION datetime_to_short_string_0 
  !
  !! Objekt "t_datetime" teilweise in einen String mit 19 Zeichen Laenge wandeln (Vektor), <BR>
  !! z.Z. nur BAWstring '??.??.yyyy-hh.mm.ss'; 
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "string_type" und "language"; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_to_short_string_1 ( this ) &
       RESULT( val ) 
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this(:)
    !! Datums-String <BR> 
    !! fuer string_type=1 gilt: <BR>
    !!  - language=1    :      '24.12.2000-11:11:11' <BR>
    !!  - language=2    :      '12/24/2000-11:11:11' <BR> 
    !                           1234567890123456789
    CHARACTER (LEN=19) :: val(SIZE(this))
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = datetime_to_short_string_0 ( this(i) )
    END DO
    !
  END FUNCTION datetime_to_short_string_1 
  !
  !! Objekt "t_datetime" teilweise in einen String mit 29 Zeichen Laenge wandeln, <BR>
  !! z.Z. nur BAWstring '??.??.yyyy-hh.mm.ss.nnnnnnnnn'; 
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "string_type" und "language"; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_to_medium_string_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this     ! 
    !! Datums-String <BR> 
    !! fuer string_type=1 gilt: <BR>
    !!  - language=1    :      '24.12.2000-11:11:11:000000000' <BR>
    !!  - language=2    :      '12/24/2000-11:11:11:000000000'
    !                           12345678901234567890123456789
    CHARACTER (LEN=29) :: val ! 
    !! Fehlervariable
    INTEGER :: stat ! ... damit kein Abbruch bei fehlerhaftem Schreiben erfolgt
    !
    SELECT CASE (string_type)
       !
       ! BAWstring liegt vor
    CASE (1)
       !
       val( 1:19) = datetime_to_short_string_0 (this)
       !
       WRITE(val(20:29),'(A1,I9.9)',IOSTAT=stat) '.', this%nanosec
       !
!   Hinweis an Programmierer:
!   an dieser Stelle das Lesen von Stringarten, die von BAWstring verschieden
!   sind, einhaengen
!   CASE (n) 
!      <Code ergaenzen>
!
       !
    END SELECT
  END FUNCTION datetime_to_medium_string_0 
  !
  !! Objekt "t_datetime" teilweise in einen String mit 29 Zeichen Laenge wandeln (Vektor), <BR>
  !! z.Z. nur BAWstring '??.??.yyyy-hh.mm.ss.nnnnnnnnn'; 
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "string_type" und "language"; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_to_medium_string_1 ( this ) &
       RESULT( val ) 
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this(:)
    !! Datums-String <BR> 
    !! fuer string_type=1 gilt: <BR>
    !!  - language=1    :      '24.12.2000-11:11:11:000000000' <BR>
    !!  - language=2    :      '12/24/2000-11:11:11:000000000'
    !                           12345678901234567890123456789
    CHARACTER (LEN=29) :: val(SIZE(this))
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = datetime_to_medium_string_0 ( this(i) )
    END DO
    !
  END FUNCTION datetime_to_medium_string_1 
  !
  !! Objekt "t_datetime" vollstaendig in einen String mit 34 Zeichen Laenge wandeln, <BR>
  !! z.Z. nur BAWstring '??.??.yyyy-hh.mm.ss.nnnnnnnnn zone'; 
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "string_type" und "language"; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_to_string_0 ( this ) &
       RESULT( val ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this     ! 
    !! Datums-String <BR> 
    !! fuer string_type=1 gilt: <BR>
    !!  - language=1    :      '24.12.2000-11:11:11:000000000 MEZ ' <BR>
    !!  - language=2    :      '12/24/2000-11:11:11:000000000 CET '
    !                           1234567890123456789012345678901234
    CHARACTER (LEN=c_len_datetime_to_string) :: val ! 
    !! Fehlervariable
    INTEGER :: stat ! ... damit kein Abbruch bei fehlerhaftem Schreiben erfolgt
    !
    SELECT CASE (string_type)
       !
       ! BAWstring liegt vor
    CASE (1)
       !
       val( 1:29) = datetime_to_medium_string_0 (this)
       !
       WRITE(val(30:34),'(A1,A4)',IOSTAT=stat) ' ', datetime_zone_to_string(this)
       !
!   Hinweis an Programmierer:
!   an dieser Stelle das Lesen von Stringarten, die von BAWstring verschieden
!   sind, einhaengen
!   CASE (n) 
!      <Code ergaenzen>
!
       !
    END SELECT
  END FUNCTION datetime_to_string_0 
  !
  !! Objekt "t_datetime" in einen String mit 34 Zeichen Laenge wandeln (Vektor)<BR>
  !! z.Z. nur BAWstring '??.??.yyyy-hh.mm.ss.nnnnnnnnn zone' 
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "string_type" und "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_to_string_1 ( this ) &
       RESULT( val ) 
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this(:)
    !! Datums-String <BR> 1234567890123456789012345678901234
    !! language=1 :       '24.12.2000-11:11:11:000000000-MEZ '
    !! language=2 :       '12/24/2000-11:11:11:000000000-CET '
    CHARACTER (LEN=c_len_datetime_to_string) :: val(SIZE(this))
    !! lokaler Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this)
       val(i) = datetime_to_string_0 ( this(i) )
    END DO
    !
  END FUNCTION datetime_to_string_1
  !
  !! &Uuml;berf&uuml;hre einen Datums-String (z.Zt. nur BAW-Format m&ouml;glich) <BR>
  !! in ein Objekt vom Typ t_datetime (Skalar) <BR>
  FUNCTION string_to_datetime_0 ( val ) &
       RESULT( this ) 
    !! Datums-String <BR> 
    !! fuer string_type=1 gilt: <BR>
    !!                          1234567890123456789012345678901234<BR>
    !!  - language=1    :      '24.12.2000-11:11:11:000000000 MEZ ' <BR>
    !!  - language=2    :      '12/24/2000-11:11:11:000000000 CET '
    CHARACTER (LEN=*), INTENT(IN)    :: val  ! 
    !! Objekt (Skalar)
    TYPE (t_datetime)         :: this ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='string_to_datetime_0'
    !! Hilfstext f&uuml;r Fehlerbehandlung
    CHARACTER (LEN=29) :: c_tmp
    !! ausgelesener String f&uuml;r Zeitzone
    CHARACTER (LEN=4) :: zone
    !! Fehlervariable
    INTEGER :: stat ! ... damit kein Abbruch bei fehlerhaftem Lesen erfolgt
    !! ausgelesene INTEGER-Zahl
    INTEGER :: ival1, ival2, ival3
    INTEGER :: i_len
    !
    CALL new_datetime (this)
    !
    i_len = LEN_TRIM ( val )
    !
    SELECT CASE (string_type)
       !
       ! BAWstring liegt vor
    CASE (1)
       !
       ! Datum mit Tag, Monat und Jahr einlesen
       !
       IF (i_len == 19 .OR. i_len == 29 .OR. i_len == 33 .OR. i_len == 34) THEN

          READ(val(1:10),'(2(I2,1X),I4)',IOSTAT=stat) ival1, ival2, ival3
          !
          IF ( stat /= 0 ) THEN
             !
             CALL setup_error_act (all_errors(:), -1010, c_upname, c_modname, stat )
             !
             CALL setup_error_act &
                  ('<..Datum..>', val(1:10) )
             !
          END IF
          !
          IF ( language == 2 ) THEN    ! englische Schreibweise
             !
             CALL set_date ( this%date, ival2, ival1, ival3 )
             ! CALL set_datetime_day   ( this, ival2 )
             ! CALL set_datetime_month ( this, ival1 )
             ! CALL set_datetime_year  ( this, ival3 )
             !
          ELSE     ! wenn keine Sprache gesetzt ist, ist deutsche Schreibweise als Default vorgesehen
             !
             CALL set_date ( this%date, ival1, ival2, ival3 )
             ! CALL set_datetime_day   ( this, ival1 )
             ! CALL set_datetime_month ( this, ival2 )
             ! CALL set_datetime_year  ( this, ival3 )
             !
          END IF
          !
          ! Uhrzeit mit Stunden, Minuten und Sekunden einlesen
          !
          READ(val(12:19),'(2(I2,1X),I2)',IOSTAT=stat) ival1, ival2, ival3
          !
          IF ( stat /= 0 ) THEN
             !
             CALL setup_error_act (all_errors(:), -1020, c_upname, c_modname, stat )
             !
             CALL setup_error_act &
                  ('<..Uhrzeit..>', val(12:19) )
             !
          END IF
          !
          CALL set_datetime_hour ( this, ival1 )
          CALL set_datetime_min  ( this, ival2 )
          CALL set_datetime_sec  ( this, ival3 )
          !
          ! ggf. nanosec wird gelesen
          !
          IF (i_len == 29 .OR. i_len == 33 .OR. i_len == 34) THEN
             !
             READ(val(21:29),'(I9)',IOSTAT=stat) ival1
             !
             IF ( stat /= 0 ) THEN
                !
                CALL setup_error_act (all_errors(:), -1030, c_upname, c_modname, stat )
                !
                CALL setup_error_act &
                     ('<..nanos..>', val(12:29) )
                !
             END IF
             !
             CALL set_datetime_nanosec ( this, ival1 ) 
             !
          ENDIF
          !
          ! ggf. Zeitzone einlesen
          !
          IF (i_len == 33) THEN
             ! zone wird gelesen
             READ(val(31:33),'(A3)') zone
             ! unsinnige Angabe fuer Zone wird im nachfolgenden UP abgefangen
             CALL set_datetime_zone_c_0_0 ( this, ADJUSTL(zone) )
             !
          ELSEIF (i_len == 34) THEN
             ! zone wird gelesen
             READ(val(31:34),'(A4)') zone
             ! unsinnige Angabe fuer Zone wird im nachfolgenden UP abgefangen
             CALL set_datetime_zone_c_0_0 ( this, ADJUSTL(zone) )
             !
          END IF
          !
       ELSE 
          CALL setup_error_act ( all_errors(:), -1060, c_upname, c_modname )
          WRITE(c_tmp,*) i_len
          CALL setup_error_act ( '<i_len>',  c_tmp )           
          CALL setup_error_act ( '<String>',  TRIM(val) )           
          
       END IF
!   Hinweis an Programmierer:
!   an dieser Stelle das Lesen von Stringarten, die von BAWstring verschieden
!   sind, einhaengen
!   CASE (n) 
!      <Code ergaenzen>
!
       !
    CASE DEFAULT
       !
       CALL setup_error_act ( all_errors(:), -1050, c_upname, c_modname )
       WRITE(c_tmp,*) string_type
       CALL setup_error_act ( '<string_type>',  c_tmp ) 
       !
    END SELECT
    !
  END FUNCTION string_to_datetime_0
  !
  !! &Uuml;berf&uuml;hre einen Vektor an Datums-Strings (z.Zt. nur BAW-Format m&ouml;glich) 
  !! in ein Objekt vom Typ t_datetime (Vektor) <BR>
  FUNCTION string_to_datetime_1 ( val ) &
       RESULT( this ) 
    !! Datums-String <BR> 
    CHARACTER (LEN=*), INTENT(IN)    :: val(:)  ! 
    !! Objekt (Skalar)
    TYPE (t_datetime)         :: this(SIZE(val)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = string_to_datetime_0 ( val(i) )
    END DO
    !
  END FUNCTION string_to_datetime_1
  !
  !! Berechne ein Zeitinkrement (Skalar) fuer die Zeit seit Beginn des julianischen Kalenders <BR>
  !! bis zum Zeitpunkt, der als Objekt vom Typ t_datetime uebergeben wurde; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_to_julian_time_0 ( this ) &
       RESULT( time ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this     ! 
    !! julianisches Zeitinkrement <BR> 
    TYPE (t_time) :: time     ! 
    !
!!iu leichter zu lesen   CALL set_time_days (time, date_to_julian_day (get_datetime_date(this)))
    ! time%sign wird bei den ueblichen Anwendungen auf 1 gesetzt
    CALL set_time_sign (time, date_to_julian_day (this%date))
    CALL set_time_days (time, date_to_julian_day (this%date))
    CALL set_time_hours (time, get_datetime_hour_0_0 (this))
    CALL set_time_minutes (time, get_datetime_min_0_0 (this))
    CALL set_time_seconds (time, get_datetime_sec_0_0 (this))
    CALL set_time_nanos (time, get_datetime_nanosec_0_0 (this))
    !
  END FUNCTION datetime_to_julian_time_0
  !
  !! Berechne ein Zeitinkrement (Vektor) fuer die Zeit seit Beginn des julian. Kalenders <BR>
  !! bis zum Zeitpunkt, der als Objekt vom Typ t_datetime uebergeben wurde; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION datetime_to_julian_time_1 ( this ) &
       RESULT( time ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this(:)     ! 
    !! julianisches Zeitinkrement <BR> 
    TYPE (t_time) :: time(SIZE(this))     ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       time(i) = datetime_to_julian_time_0 (this(i))
    END DO
    !
  END FUNCTION datetime_to_julian_time_1
  !
  !! Berechne ein Objekt (Skalar) des Typs t_datetime aus dem julianischen Zeitinkrement; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen; <BR>
  !! Zeitzone wird auf fehlerhaften Wert gesetzt.
  FUNCTION julian_time_to_datetime_0 ( time ) &
       RESULT( this ) 
    !! Zeitzonen-String <BR> 
    TYPE (t_time), INTENT(IN) :: time 
    !! Rueckgabewert: Objekt (Skalar)
    TYPE (t_datetime) :: this     ! 
    !
    this%date = julian_day_to_date (get_time_days(time))
    this%hour = get_time_hours (time)
    this%min = get_time_minutes (time)
    this%sec = get_time_seconds (time)
    this%nanosec = get_time_nanos (time)
    this%zone = 99 
    !
  END FUNCTION julian_time_to_datetime_0
  !
  !! Berechne ein Objekt (Vektor) des Typs t_datetime aus dem julianischen Zeitinkrement; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION julian_time_to_datetime_1 ( time ) &
       RESULT( this ) 
    !! Zeitzonen-String <BR> 
    TYPE (t_time), INTENT(IN) :: time(:)
    !! Rueckgabewert: Objekt (Vektor)
    TYPE (t_datetime) :: this(SIZE(time))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(time)
       this(i) = julian_time_to_datetime_0 (time(i))
    END DO
    !
  END FUNCTION julian_time_to_datetime_1
  !
  !
  !! &Uuml;berf&uuml;hre einen Zeitpunkt des Typs t_datetime (Skalar) 
  !! in eine neue Zeitzone; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_datetime_in_changed_zone_i0 ( this1, zone_new ) &
       RESULT( this2 ) 
    !! Objekt alter Zeitpunkt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1
    !! Zeitdifferenz der neuen Zeitzone bezogen auf UTC in h
    INTEGER , INTENT(IN) :: zone_new
    !! R&uuml;ckgabewert Zeitpunkt in neuer Zeitzone
    TYPE (t_datetime)  :: this2
    !! Zeitdifferenz als Inkrement
    TYPE (t_time) :: zone_diff
    !
    CALL new_time(zone_diff)
    CALL set_time_sign (zone_diff, zone_new -  this1%zone)
    CALL set_time_hours (zone_diff, ABS(zone_new -  this1%zone))
    !
    this2 = add_time_to_datetime_dt_0_0 (this1, zone_diff)
    !
    CALL set_datetime_zone (this2, zone_new)
    !
  END FUNCTION get_datetime_in_changed_zone_i0
  !
  !! &Uuml;berf&uuml;hre einen Zeitpunkt des Typs t_datetime (Vektor) 
  !! in eine neue Zeitzone; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_datetime_in_changed_zone_i1 ( this1, zone_new ) &
       RESULT( this2 ) 
    !! Objekt alter Zeitpunkt (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:)
    !! Zeitdifferenz der neuen Zeitzone bezogen auf UTC in h
    INTEGER , INTENT(IN) :: zone_new
    !! R&uuml;ckgabewert Zeitpunkt in neuer Zeitzone
    TYPE (t_datetime)  :: this2(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this1)
       this2(i) = get_datetime_in_changed_zone_i0 (this1(i), zone_new)
    END DO
    !
  END FUNCTION get_datetime_in_changed_zone_i1
  !
  !! &Uuml;berf&uuml;hre einen Zeitpunkt des Typs t_datetime (Skalar) 
  !! in eine neue Zeitzone (Eingabe als String); <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION get_datetime_in_changed_zone_c0 ( this1, c_zone_new ) &
       RESULT( this2 ) 
    !! Objekt alter Zeitpunkt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this1
    !! Angabe der neuen Zeitzone bezogen auf UTC als String
    CHARACTER(LEN=4) , INTENT(IN) :: c_zone_new
    !! R&uuml;ckgabewert Zeitpunkt in neuer Zeitzone
    TYPE (t_datetime)  :: this2
    !! alte und neue Zeitzone als Integer-Wert
    INTEGER :: zone_old, zone_new
    ! c_zone_new in INTEGER zone_new umwandeln
    zone_old = this1%zone 
    CALL set_datetime_zone_c_0_0 (this1, c_zone_new)
    !
    IF(no_error()) THEN
       zone_new = this1%zone
       this1%zone = zone_old
       this2 = get_datetime_in_changed_zone_i0 (this1, zone_new)
    ENDIF
    !
  END FUNCTION get_datetime_in_changed_zone_c0
  !
  !! &Uuml;berf&uuml;hre einen Zeitpunkt des Typs t_datetime (Vektor) 
  !! in eine neue Zeitzone (Eingabe als String); <BR>
  !! Function erzeugt Fehlermeldungen 
  FUNCTION get_datetime_in_changed_zone_c1 ( this1, c_zone_new ) &
       RESULT( this2 ) 
    !! Objekt alter Zeitpunkt (Skalar)
    TYPE (t_datetime) , INTENT(INOUT) :: this1(:)
    !! Angabe der neuen Zeitzone bezogen auf UTC als String
    CHARACTER(LEN=4) , INTENT(IN) :: c_zone_new
    !! R&uuml;ckgabewert Zeitpunkt in neuer Zeitzone
    TYPE (t_datetime)  :: this2(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this1)
       this2(i) = get_datetime_in_changed_zone_c0 (this1(i), c_zone_new)
    END DO
    !
  END FUNCTION get_datetime_in_changed_zone_c1
  !
  !! Ermitteln des Datums (mit Uhrzeit) mit Hilfe der Systemuhr (Skalar)<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_datetime_from_system_0 ( ) &
       RESULT( this )
    !! R&uuml;ckgabewert : aktuelles Datum
    TYPE (t_datetime) :: this ! 
    !! lokales Hilfsfeld
    INTEGER :: dt(8)      ! 
    !
    CALL new_datetime ( this )
    !
    CALL date_and_time ( VALUES=dt(:) )
    !
    IF ( dt(1) /= -HUGE(0) ) CALL set_datetime_year  ( this, dt(1) )
    IF ( dt(2) /= -HUGE(0) ) CALL set_datetime_month ( this, dt(2) )
    IF ( dt(3) /= -HUGE(0) ) CALL set_datetime_day   ( this, dt(3) )
    IF ( dt(5) /= -HUGE(0) ) CALL set_datetime_hour  ( this, dt(5) )
    IF ( dt(6) /= -HUGE(0) ) CALL set_datetime_min   ( this, dt(6) )
    IF ( dt(7) /= -HUGE(0) ) CALL set_datetime_sec   ( this, dt(7) )
    IF ( dt(8) /= -HUGE(0) ) CALL set_datetime_nanosec ( this, 1000*dt(8) )
    ! dt(4) Zeitunterschied zur UTC-Zeit in Minuten
    IF ( dt(4) /= -HUGE(0) ) CALL set_datetime_zone ( this, dt(4)/60 )
    !
  END FUNCTION get_datetime_from_system_0
  !
  !! Berechne den Mittelwert zwischen zwei Zeitpunkten
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mean_datetime_0 ( this1, this2 ) &
       RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1 !
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2 !
    ! Rueckgabewert
    !! Mittelwert (Skalar)
    TYPE (t_datetime) :: this ! 
    ! Lokale Parameter und Variablen
    !! Differenz zwischen beiden Objekten
    TYPE (t_time) :: time_diff
    !
    time_diff = this2 - this1
    ! Mittelwert berechnen      
    this = this1 + (time_diff / 2)
    !
  END FUNCTION mean_datetime_0
  !
  !! Berechne den Mittelwert zwischen zwei Zeitpunkten
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mean_datetime_1 ( this1, this2 ) &
       RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) !
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) !
    !! Mittelwert (Vektor)
    TYPE (t_datetime) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    ! Lokale Parameter und Variablen
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mean_datetime_0 ( this1(i), this2(i) )
    END DO
    !
  END FUNCTION mean_datetime_1
  !
  !! Runde Nanosekundenanteil auf eine vorgegebene Anzahl von Stellen (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_datetime_0 ( this, nks ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_datetime), INTENT(IN) :: this ! 
    !! Anzahl der Nachkommastellen
    INTEGER          , INTENT(IN) :: nks  ! 
    !! Ergebnis: gerundete Zeitangabe
    TYPE (t_datetime) :: res ! 
    !! Hilfsvariablen
    TYPE (t_time)     :: l_time, s_time, d_time ! 
    INTEGER           :: mks, m, nanos          ! 
    !
    mks = MAX( 0, nks ) 
    mks = MIN( 6, mks ) 
    m   = 10**(9-mks)
    !
    l_time = su_datetime( this, ref_datetime ) ! rel. Zeit seit "ref_datum"
    s_time = round_time_to_seconds ( l_time )
    d_time = su_time( l_time, s_time )
    nanos  = get_time_nanos( d_time )
    nanos  = NINT( REAL(nanos,Double)/REAL(m,Double) ) * m
    CALL set_time_nanos( d_time, nanos )
    l_time = ad_time( s_time, d_time )
    res    = ad_time_to_datetime ( ref_datetime, l_time )
    !
  END FUNCTION round_datetime_0
  !
  !! Runde Nanosekundenanteil auf eine vorgegebene Anzahl von Stellen (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_datetime_1 ( this, nks ) &
       RESULT(res)
    !! Datenobjekt (Vektor)
    TYPE (t_datetime), INTENT(IN) :: this(:) ! 
    !! Anzahl der Nachkommastellen
    INTEGER          , INTENT(IN) :: nks     ! 
    !! Ergebnis: gerundete Zeitangabe (Vektor)
    TYPE (t_datetime) :: res(SIZE(this))     ! 
    !! Hilfsvariablen
    INTEGER           :: i                   ! 
    !
    DO i=1,SIZE(this)
       res(i) = round_datetime( this(i), nks )
    END DO
    !
  END FUNCTION round_datetime_1
  ! 
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Objekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_datetime_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp)  
    !
    IF ( this1%zone  /= this2%zone ) THEN
       this_tmp = get_datetime_in_changed_zone_i0 (this2, get_datetime_zone(this1))
    ELSE
       this_tmp = this2
    ENDIF
    !
    l_ok(1)  = ( this1%date == this_tmp%date )
    l_ok(2)  = ( this1%hour  == this_tmp%hour )
    l_ok(3)  = ( this1%min  == this_tmp%min )
    l_ok(4)  = ( this1%sec  == this_tmp%sec )
    l_ok(5)  = ( this1%nanosec  == this_tmp%nanosec )
    l_ok(6)  = ( this1%zone  == this_tmp%zone )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_datetime_0_0
  !
  !! pr&uuml;fe zwei Objekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_datetime_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_datetime_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_datetime_1_0
  !
  !! pr&uuml;fe zwei Objekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_datetime_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_datetime_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_datetime_0_1
  !
  !! pr&uuml;fe zwei Objekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_datetime_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_datetime_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_datetime_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! ein Zeitinkrement des Typs t_time (Skalar); <BR>
  !! Reihenfolge der Formalparameter t_datetime, t_time; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_time_to_datetime_dt_0_0 ( this1, time_to_add ) &
       RESULT( this2 ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1     ! Zeitpunkt alt 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_add   ! Zeitinkrement 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2                 ! Zeitpunkt neu
    !! julianischer Tag inkl. Uhrzeit
    TYPE (t_time) :: jul_time
    !
    ! Addition zweier Objekte vom Typ t_time
    jul_time = datetime_to_julian_time(this1) + time_to_add
    ! Umrechnen des neuen julianischen Tages in ein echtes Datum
    this2 = julian_time_to_datetime(jul_time)
    ! Zone bleibt unveraendert
    this2%zone = this1%zone
    !
  END FUNCTION add_time_to_datetime_dt_0_0
  !
  !! Addiere zu einem Zeitpunkt des Typs datetime (Vektor) 
  !! ein Zeitinkrement des Typs t_time (Skalar); <BR>
  !! Reihenfolge der Formalparameter t_datetime, t_time; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_time_to_datetime_dt_1_0 ( this1, time_to_add ) &
       RESULT( this2 ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1(:)     ! Zeitpunkt alt 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_add      ! Zeitinkrement 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(SIZE(this1))       ! Zeitpunkt neu
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this1)
       this2(i) = add_time_to_datetime_dt_0_0 (this1(i), time_to_add)
    END DO
    !
  END FUNCTION add_time_to_datetime_dt_1_0
  !
  !! Addiere zu einem Zeitpunkt des Typs datetime (Skalar) 
  !! ein Zeitinkrement des Typs t_time (Vektor); <BR>
  !! Reihenfolge der Formalparameter t_datetime, t_time; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_time_to_datetime_dt_0_1 ( this1, time_to_add ) &
       RESULT( this2 ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1           ! Zeitpunkt alt 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_add(:)      ! Zeitinkrement 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(SIZE(time_to_add))    ! Zeitpunkt neu
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(time_to_add)
       this2(i) = add_time_to_datetime_dt_0_0 (this1, time_to_add(i))
    END DO
    !
  END FUNCTION add_time_to_datetime_dt_0_1
  !
  !! Addiere zu einem Zeitpunkt des Typs datetime (Vektor)
  !! ein Zeitinkrement des Typs t_time (Vektor); <BR>
  !! Reihenfolge der Formalparameter t_datetime, t_time; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_time_to_datetime_dt_1_1 ( this1, time_to_add ) &
       RESULT( this2 ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1(:)        ! Zeitpunkt alt 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_add(:)      ! Zeitinkrement 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(MIN(SIZE(this1),SIZE(time_to_add)))    ! Zeitpunkt neu
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this2)
       this2(i) = add_time_to_datetime_dt_0_0 (this1(i), time_to_add(i))
    END DO
    !
  END FUNCTION add_time_to_datetime_dt_1_1
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! ein Zeitinkrement des Typs t_time (Skalar), <BR>
  !! Reihenfolge der Formalparameter t_time, t_datetime; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_time_to_datetime_td_0_0 ( time_to_add, this1 ) &
       RESULT( this2 ) 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_add   ! Zeitinkrement 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1     ! Zeitpunkt alt 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2                 ! Zeitpunkt neu
    !
    this2 = add_time_to_datetime_dt_0_0 ( this1, time_to_add )
    !
  END FUNCTION add_time_to_datetime_td_0_0
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Vektor) 
  !! ein Zeitinkrement des Typs t_time (Skalar), <BR>
  !! Reihenfolge der Formalparameter t_time, t_datetime; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_time_to_datetime_td_0_1 ( time_to_add, this1 ) &
       RESULT( this2 ) 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_add   ! Zeitinkrement 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1(:)  ! Zeitpunkt alt 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(SIZE(this1))    ! Zeitpunkt neu
    !
    this2 = add_time_to_datetime_dt_1_0 ( this1, time_to_add )
    !
  END FUNCTION add_time_to_datetime_td_0_1
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! ein Zeitinkrement des Typs t_time (Vektor), <BR>
  !! Reihenfolge der Formalparameter t_time, t_datetime; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_time_to_datetime_td_1_0 ( time_to_add, this1 ) &
       RESULT( this2 ) 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_add(:)   ! Zeitinkrement 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1        ! Zeitpunkt alt 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(SIZE(time_to_add)) ! Zeitpunkt neu
    !
    this2 = add_time_to_datetime_dt_0_1 ( this1, time_to_add )
    !
  END FUNCTION add_time_to_datetime_td_1_0
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Vektor) 
  !! ein Zeitinkrement des Typs t_time (Vektorr), <BR>
  !! Reihenfolge der Formalparameter t_time, t_datetime; <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_time_to_datetime_td_1_1 ( time_to_add, this1 ) &
       RESULT( this2 ) 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_add(:)                    ! Zeitinkrement 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1(:)                      ! Zeitpunkt alt 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(MIN(SIZE(this1),SIZE(time_to_add))) ! Zeitpunkt neu
    !
    this2 = add_time_to_datetime_dt_1_1 ( this1(:), time_to_add(:) )
    !
  END FUNCTION add_time_to_datetime_td_1_1
  !
  ! ----------------------------------------------------------------------
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! eine Anzahl Sekunden (Skalar); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_seconds_to_datetime_oi_0_0 ( this1, i_sec ) &
       RESULT( this2 ) 
    !! Objekt (Skalar) alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2
    !! julianischer Tag inkl. Uhrzeit
    TYPE (t_time) :: jul_time_1, jul_time_2
    !! Anzahl Sekunden als REAL
    REAL (KIND=double) :: r_sec
    ! 
    jul_time_1 = datetime_to_julian_time(this1)
    r_sec      = i_sec
    jul_time_2 = real_seconds_to_time(r_sec)
    this2      = julian_time_to_datetime (jul_time_1 + jul_time_2)
    this2%zone = this1%zone
    !
  END FUNCTION add_seconds_to_datetime_oi_0_0
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Vektor) 
  !! eine Anzahl Sekunden (Skalar); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  !! Anmerkung: Vektorfunktionen von b_time nutzen? bislang nein, da Zeitrechnung wenig rechenintensiv
  FUNCTION add_seconds_to_datetime_oi_1_0 ( this1, i_sec ) &
       RESULT( this2 ) 
    !! Objekt (Vektor) alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1(:)
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this1)
       this2(i) = add_seconds_to_datetime_oi_0_0 (this1(i), i_sec)
    END DO
    ! 
  END FUNCTION add_seconds_to_datetime_oi_1_0
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! eine Anzahl Sekunden (Vektor); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_seconds_to_datetime_oi_0_1 ( this1, i_sec ) &
       RESULT( this2 ) 
    !! Objekt (Skalar) alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec(:)
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(SIZE(i_sec))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(i_sec)
       this2(i) = add_seconds_to_datetime_oi_0_0 (this1, i_sec(i))
    END DO
    ! 
  END FUNCTION add_seconds_to_datetime_oi_0_1
  !
  !! Addiere zu einem Zeitpunkt des Typs t_datetime (Vektor) 
  !! eine Anzahl Sekunden (Vektor); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_seconds_to_datetime_oi_1_1 ( this1, i_sec ) &
       RESULT( this2 ) 
    !! Objekt (Vektor) alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1(:)
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec(:)
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(MIN(SIZE(this1), SIZE(i_sec)))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this2)
       this2(i) = add_seconds_to_datetime_oi_0_0 (this1(i), i_sec(i))
    END DO
    ! 
  END FUNCTION add_seconds_to_datetime_oi_1_1
  !
  !! Addiere zu einer Anzahl Sekunden (Skalar)
  !! einen Zeitpunkt des Typs t_datetime (Skalar); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_seconds_to_datetime_io_0_0 ( i_sec, this1 ) &
       RESULT( this2 ) 
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec
    !! Objekt alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2
    !
    this2 = add_seconds_to_datetime_oi_0_0 &
         ( this1, i_sec ) 
    ! 
    this2%zone = this1%zone
    !
  END FUNCTION add_seconds_to_datetime_io_0_0
  !
  !! Addiere zu einer Anzahl Sekunden (Vektor)
  !! einen Zeitpunkt des Typs t_datetime (Skalar); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_seconds_to_datetime_io_1_0 ( i_sec, this1 ) &
       RESULT( this2 ) 
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec(:)
    !! Objekt alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(SIZE(i_sec))
    !
    this2 = add_seconds_to_datetime_oi_0_1 &
         ( this1, i_sec ) 
    ! 
  END FUNCTION add_seconds_to_datetime_io_1_0
  !
  !! Addiere zu einer Anzahl Sekunden (Skalar)
  !! einen Zeitpunkt des Typs t_datetime (Vektor); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_seconds_to_datetime_io_0_1 ( i_sec, this1 ) &
       RESULT( this2 ) 
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec
    !! Objekt alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1(:)
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(SIZE(this1))
    !
    this2 = add_seconds_to_datetime_oi_1_0 &
         ( this1, i_sec ) 
    ! 
  END FUNCTION add_seconds_to_datetime_io_0_1
  !
  !! Addiere zu einer Anzahl Sekunden (Vektor)
  !! einen Zeitpunkt des Typs t_datetime (Vektor); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION add_seconds_to_datetime_io_1_1 ( i_sec, this1 ) &
       RESULT( this2 ) 
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec(:)
    !! Objekt alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1(:)
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(MIN(SIZE(i_sec),SIZE(this1)))
    !
    this2 = add_seconds_to_datetime_oi_1_1 &
         ( this1, i_sec ) 
    ! 
  END FUNCTION add_seconds_to_datetime_io_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  !! Subtrahiere von einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! ein Zeitinkrement des Typs t_time (Skalar); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION sub_time_from_datetime_0_0 ( this1, time_to_sub ) &
       RESULT( this2 ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1     ! Zeitpunkt alt 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_sub   ! Zeitinkrement 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2                 ! Zeitpunkt neu
    !
    this2 = add_time_to_datetime_dt_0_0 &
         ( this1, -time_to_sub )
    !
  END FUNCTION sub_time_from_datetime_0_0
  !
  !! Subtrahiere von einem Zeitpunkt des Typs t_datetime (Vektor) 
  !! ein Zeitinkrement des Typs t_time (Skalar); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION sub_time_from_datetime_1_0 ( this1, time_to_sub ) &
       RESULT( this2 ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1(:)  ! Zeitpunkt alt 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_sub   ! Zeitinkrement 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(SIZE(this1))    ! Zeitpunkt neu
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this1)
       this2(i) = add_time_to_datetime_dt_0_0 (this1(i), -time_to_sub)
    END DO
    !
  END FUNCTION sub_time_from_datetime_1_0
  !
  !! Subtrahiere von einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! ein Zeitinkrement des Typs t_time (Vektor); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION sub_time_from_datetime_0_1 ( this1, time_to_sub ) &
       RESULT( this2 ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1         ! Zeitpunkt alt 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_sub(:)    ! Zeitinkrement 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(SIZE(time_to_sub))  ! Zeitpunkt neu
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this2)
       this2(i) = add_time_to_datetime_dt_0_0 (this1, -time_to_sub(i))
    END DO
    !
  END FUNCTION sub_time_from_datetime_0_1
  !
  !! Subtrahiere von einem Zeitpunkt des Typs t_datetime (Vektor) 
  !! ein Zeitinkrement des Typs t_time (Vektor); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION sub_time_from_datetime_1_1 ( this1, time_to_sub ) &
       RESULT( this2 ) 
    !! Objekt (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1(:)                        ! Zeitpunkt alt 
    !! Zeitinkrement <BR> 
    TYPE (t_time) , INTENT(IN) :: time_to_sub(:)                      ! Zeitinkrement 
    !! R&uuml;ckgabewert
    TYPE (t_datetime)  :: this2(MIN(SIZE(this1),SIZE(time_to_sub)))    ! Zeitpunkt neu
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this2)
       this2(i) = add_time_to_datetime_dt_0_0 (this1(i), -time_to_sub(i))
    END DO
    !
  END FUNCTION sub_time_from_datetime_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  !! Subtrahiere von einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! eine Anzahl Sekunden (Skalar); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION sub_seconds_from_datetime_0_0 ( this1, i_sec ) &
       RESULT( this2 ) 
    !! Objekt (Skalar) alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2
    ! 
    this2 = add_seconds_to_datetime_oi_0_0 &
       ( this1, -i_sec ) 
    !
  END FUNCTION sub_seconds_from_datetime_0_0
  !
  !! Subtrahiere von einem Zeitpunkt des Typs t_datetime (Vektor) 
  !! eine Anzahl Sekunden (Skalar); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION sub_seconds_from_datetime_1_0 ( this1, i_sec ) &
       RESULT( this2 ) 
    !! Objekt (Vektor) alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1(:)
    !! Anzahl Sekunden <BR> 
    INTEGER, INTENT(IN) :: i_sec
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(SIZE(this1))
    !
    this2 = add_seconds_to_datetime_oi_1_0 &
       ( this1, -i_sec ) 
    !
  END FUNCTION sub_seconds_from_datetime_1_0
  !
  !! Subtrahiere von einem Zeitpunkt des Typs t_datetime (Skalar) 
  !! eine Anzahl Sekunden (Vektor); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION sub_seconds_from_datetime_0_1 ( this1, i_sec ) &
       RESULT( this2 ) 
    !! Objekt (Skalar) alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1
    !! Anzahl Sekunden (Vektor)
    INTEGER, INTENT(IN) :: i_sec(:)
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(SIZE(i_sec))
    !
    this2 = add_seconds_to_datetime_oi_0_1 &
       ( this1, -i_sec ) 
    !
  END FUNCTION sub_seconds_from_datetime_0_1
  !
  !! Subtrahiere von einem Zeitpunkt des Typs t_datetime (Vektor) 
  !! eine Anzahl Sekunden (Vektor); <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION sub_seconds_from_datetime_1_1 ( this1, i_sec ) &
       RESULT( this2 ) 
    !! Objekt alter Zeitpunkt
    TYPE (t_datetime) , INTENT(IN) :: this1(:)
    !! Anzahl Sekunden (Vektor)
    INTEGER, INTENT(IN) :: i_sec(:)
    !! R&uuml;ckgabewert neuer Zeitpunkt
    TYPE (t_datetime)  :: this2(MIN(SIZE(this1), SIZE(i_sec)))
    !
    this2 = add_seconds_to_datetime_oi_1_1 &
       ( this1, -i_sec ) 
    !
  END FUNCTION sub_seconds_from_datetime_1_1
  !
  !! Subtrahiere zwei Objekte ( Skalar / Skalar ) <BR>
  !! R&uuml;ckgabewert ist vom Datentyp t_time
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION sub_datetime_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2 ! 
    !! Subtraktionsergebnis (Skalar)
    TYPE (t_time) :: this ! 
    !! Objekte 1 und 2 als Datentyp t_time
    TYPE (t_time) :: jul_time1, jul_time2
    !! Zeitpunkt 2 bezogen auf Zeitzone 1
    TYPE (t_datetime) :: datetime_tmp
    !
    ! ggf. &Uuml;berf&uuml;hren von Zeitpunkt 2 in Zeitzone 1
    !
    IF ( this1%zone /= this2%zone ) THEN
       datetime_tmp = get_datetime_in_changed_zone ( this2, get_datetime_zone(this1) )
    ELSE
       datetime_tmp = this2
    END IF
    !
    ! Umwandeln von Zeitpunkt this1 in julianischen Tag + Uhrzeit
    !
    jul_time1 = datetime_to_julian_time ( this1 )
    !
    ! Umwandeln von Zeitpunkt this2 (mit neuer Zeitzone) in julianischen Tag + Uhrzeit
    ! 
    jul_time2 = datetime_to_julian_time ( datetime_tmp )
    !
    ! Subtraktion beider Objekte t_time 
    !
    this = jul_time1 - jul_time2
    !
  END FUNCTION sub_datetime_0_0
  !
  !! Subtrahiere zwei Objekte ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION sub_datetime_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2    ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = sub_datetime_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION sub_datetime_1_0
  !
  !! Subtrahiere zwei Objekte ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION sub_datetime_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = sub_datetime_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION sub_datetime_0_1
  !
  !! Subtrahiere zwei Objekte ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION sub_datetime_1_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_time) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = sub_datetime_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION sub_datetime_1_1
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
  !! pr&uuml;fe zwei Objekte auf ">" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_datetime_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp
    !
    IF ( this1%zone  /= this2%zone ) THEN
       this_tmp = get_datetime_in_changed_zone_i0 (this2, get_datetime_zone(this1))
    ELSE
       this_tmp = this2
    ENDIF
    !
    ok = datetime_to_julian_time(this1) > &
         datetime_to_julian_time(this_tmp)
    !
  END FUNCTION gt_datetime_0_0
  !
  !! pr&uuml;fe zwei Objekte auf ">" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_datetime_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! lokale Kopie von this1; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this1)
       IF ( this1(i)%zone  /= this2%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this1(i), get_datetime_zone(this2))
       ELSE
          this_tmp(i) = this1(i)
       ENDIF
       !
    ENDDO
    ok = datetime_to_julian_time(this1) > &
         datetime_to_julian_time(this2)
    !
  END FUNCTION gt_datetime_1_0
  !
  !! pr&uuml;fe zwei Objekte auf ">" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_datetime_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2))
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(SIZE(this2))
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this2)
       IF ( this1%zone  /= this2(i)%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this2(i), get_datetime_zone(this1))
       ELSE
          this_tmp(i) = this2(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) > &
         datetime_to_julian_time(this2)
    !
  END FUNCTION gt_datetime_0_1
  !
  !! pr&uuml;fe zwei Objekte auf ">" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_datetime_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(MIN(SIZE(this1),SIZE(this2))) 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       IF ( this1(i)%zone  /= this2(i)%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this2(i), get_datetime_zone(this1(i)))
       ELSE
          this_tmp(i) = this2(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) > &
         datetime_to_julian_time(this2)
    !
  END FUNCTION gt_datetime_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Objekte auf ">=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_datetime_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp
    !
    IF ( this1%zone  /= this2%zone ) THEN
       this_tmp = get_datetime_in_changed_zone_i0 (this2, get_datetime_zone(this1))
    ELSE
       this_tmp = this2
    ENDIF
    !
    ok = datetime_to_julian_time(this1) >= &
         datetime_to_julian_time(this_tmp)
    !
  END FUNCTION ge_datetime_0_0
  !
  !! pr&uuml;fe zwei Objekte auf ">=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_datetime_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) 
    !! lokale Kopie von this1; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this1)
       IF ( this1(i)%zone  /= this2%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this1(i), get_datetime_zone(this2))
       ELSE
          this_tmp(i) = this1(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) >= &
         datetime_to_julian_time(this2)
    !
  END FUNCTION ge_datetime_1_0
  !
  !! pr&uuml;fe zwei Objekte auf ">=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_datetime_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(SIZE(this2))
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this2)
       IF ( this1%zone  /= this2(i)%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this2(i), get_datetime_zone(this1))
       ELSE
          this_tmp(i) = this2(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) >= &
         datetime_to_julian_time(this2)
    !
  END FUNCTION ge_datetime_0_1
  !
  !! pr&uuml;fe zwei Objekte auf ">=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_datetime_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    ! Lokale Parameter und Variablen
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(MIN(SIZE(this1),SIZE(this2))) 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       IF ( this1(i)%zone  /= this2(i)%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this2(i), get_datetime_zone(this1(i)))
       ELSE
          this_tmp(i) = this2(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) >= &
         datetime_to_julian_time(this2)
    !
  END FUNCTION ge_datetime_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Objekte auf "<" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_datetime_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp
    !
    IF ( this1%zone  /= this2%zone ) THEN
       this_tmp = get_datetime_in_changed_zone_i0 (this2, get_datetime_zone(this1))
    ELSE
       this_tmp = this2
    ENDIF
    !
    ok = datetime_to_julian_time(this1) < &
         datetime_to_julian_time(this_tmp)
    !
  END FUNCTION lt_datetime_0_0
  !
  !! pr&uuml;fe zwei Objekte auf "<" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_datetime_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! lokale Kopie von this1; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this1)
       IF ( this1(i)%zone  /= this2%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this1(i), get_datetime_zone(this2))
       ELSE
          this_tmp(i) = this1(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) < &
         datetime_to_julian_time(this2)
    !
  END FUNCTION lt_datetime_1_0
  !
  !! pr&uuml;fe zwei Objekte auf "<" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_datetime_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(SIZE(this2))
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this2)
       IF ( this1%zone  /= this2(i)%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this2(i), get_datetime_zone(this1))
       ELSE
          this_tmp(i) = this2(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) < &
         datetime_to_julian_time(this2)
    !
  END FUNCTION lt_datetime_0_1
  !
  !! pr&uuml;fe zwei Objekte auf "<" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_datetime_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(MIN(SIZE(this1),SIZE(this2))) 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       IF ( this1(i)%zone  /= this2(i)%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this2(i), get_datetime_zone(this1(i)))
       ELSE
          this_tmp(i) = this2(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) < &
         datetime_to_julian_time(this2)
    !
  END FUNCTION lt_datetime_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Objekte auf "<=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_datetime_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp
    !
    IF ( this1%zone  /= this2%zone ) THEN
       this_tmp = get_datetime_in_changed_zone_i0 (this2, get_datetime_zone(this1))
    ELSE
       this_tmp = this2
    ENDIF
    !
    ok = datetime_to_julian_time(this1) <= &
         datetime_to_julian_time(this_tmp)
    !
  END FUNCTION le_datetime_0_0
  !
  !! pr&uuml;fe zwei Objekte auf "<=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_datetime_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! lokale Kopie von this1; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this1)
       IF ( this1(i)%zone  /= this2%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this1(i), get_datetime_zone(this2))
       ELSE
          this_tmp(i) = this1(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) <= &
         datetime_to_julian_time(this2)
    !
  END FUNCTION le_datetime_1_0
  !
  !! pr&uuml;fe zwei Objekte auf "<=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_datetime_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(SIZE(this2))
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,SIZE(this2)
       IF ( this1%zone  /= this2(i)%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this2(i), get_datetime_zone(this1))
       ELSE
          this_tmp(i) = this2(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) <= &
         datetime_to_julian_time(this2)
    !
  END FUNCTION le_datetime_0_1
  !
  !! pr&uuml;fe zwei Objekte auf "<=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_datetime_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! lokale Kopie von this2; ggf. mit veraenderter Zeitzone
    TYPE (t_datetime) :: this_tmp(MIN(SIZE(this1),SIZE(this2))) 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       IF ( this1(i)%zone  /= this2(i)%zone ) THEN
          this_tmp(i) = get_datetime_in_changed_zone_i0 (this2(i), get_datetime_zone(this1(i)))
       ELSE
          this_tmp(i) = this2(i)
       ENDIF
    ENDDO
    !
    ok = datetime_to_julian_time(this1) <= &
         datetime_to_julian_time(this2)
    !
  END FUNCTION le_datetime_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Objekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_datetime_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_datetime_0_0
  !
  !! pr&uuml;fe zwei Objekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_datetime_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_datetime_1_0
  !
  !! pr&uuml;fe zwei Objekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_datetime_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_datetime) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_datetime_0_1
  !
  !! pr&uuml;fe zwei Objekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_datetime_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_datetime_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-TEST-Methoden <<< [ERR_NO = 20000 bis 20999]
  ! ----------------------------------------------------------------------
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
       WRITE(*,*) ' *** Warnung *** Modul "b_datetime" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_datetime ausfuehren'
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
  SUBROUTINE init_datetime_all_errors ( )
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
               '--> INIT_datetime ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_datetime ausfuehren' )
       END IF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 003
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_datetime"\n'//&
               'Typ-Komponente = "date"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 004
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_datetime"\n'//&
               'Typ-Komponente = "hour"\n'//&
               'Wert von "hour" = <hour>\n'//&
               'Datumsangabe = <datetime>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 005
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_datetime"\n'//&
               'Typ-Komponente = "min"\n'//&
               'Wert von "min" = <min>\n'//&
               'Datumsangabe = <datetime>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 006
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_datetime"\n'//&
               'Typ-Komponente = "sec"\n'//&
               'Wert von "sec" = <sec>\n'//&
               'Datumsangabe = <datetime>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 007
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_datetime"\n'//&
               'Typ-Komponente = "nanosec"\n'//&
               'Wert von "nanosec" = <nanosec>\n'//&
               'Datumsangabe = <datetime>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 008
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_datetime"\n'//&
               'Typ-Komponente = "zone"\n'//&
               'Wert von "zone" = <zone>\n'//&
               'Datumsangabe = <datetime>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 009
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_datetime" pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 010
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_datetime" pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 011
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Objektes (1D-Array)\n'//&
               '--> Code in Modul "b_datetime" pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 012
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_datetime"\n'//&
               'Typ-Komponente = "date"\n'//&
               '--> Code in Modul "b_datetime" / Daten pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 013
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_datetime"\n'//&
               'Typ-Komponente = "hour"\n'//&
               '--> Code in Modul "b_datetime" / Daten pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 014
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_datetime"\n'//&
               'Typ-Komponente = "min"\n'//&
               '--> Code in Modul "b_datetime" / Daten pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 015
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_datetime"\n'//&
               'Typ-Komponente = "sec"\n'//&
               '--> Code in Modul "b_datetime" / Daten pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 016
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_datetime"\n'//&
               'Typ-Komponente = "nanosec"\n'//&
               '--> Code in Modul "b_datetime" / Daten pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 017
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_datetime"\n'//&
               'Typ-Komponente = "zone"\n'//&
               '--> Code in Modul "b_datetime" / Daten pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 018
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_datetime"\n'//&
               '--> Code in Modul "b_datetime" / Daten pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          !
          ! Index 019
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic),-1010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Umwandlung String/Datentyp\n'//&
               'Fehler beim Einlesen des Datums \n'//&
               'Datum = <..Datum..> '//&
               'gewuenschtes Format fuer Zeichen 1 - 10: tt.mm.jjjj   bzw.   mm/dd/yyyy\n'//&
               '--> Angabe des Zeitpunktes pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 020
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic),-1020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Umwandlung String/Datentyp\n'//&
               'Fehler beim Einlesen der Uhrzeit \n'//&
               'Uhrzeit = <..Uhrzeit..> '//&
               'gewuenschtes Format : hh:mm:ss  \n'//&
               '--> Angabe des Zeitpunktes pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 021
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic),-1030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Umwandlung String/Datentyp\n'//&
               'Fehler beim Einlesen der Uhrzeit \n'//&
               'Uhrzeit mit Nanosekunden = <..nanos..> '//&
               'gewuenschtes Format : hh:mm:ss:nnnnnnnnn  \n'//&
               '--> Angabe des Zeitpunktes pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 022
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic),-1040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Umwandlung String/Datentyp\n'//&
               'Fehler beim Einlesen der Zeitzone \n'//&
               'zone = <..zone..> \n'//&
               'erlaubte Zeitzonen-Strings siehe Methode set_datetime_zone_c_0_0\n'//&
               '--> Angabe der Zeitzone pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 023
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic),-1050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Methoden fuer Strings und t_datetime\n'//&
               'string_type enthaelt keinen gueltigen Wert\n'//&
               'string_type = <string_type>\n'//&
               '--> gesetzte Variable string_type pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 024
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic),-1060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Methoden fuer Strings und t_datetime\n'//&
               'Stringlaenge (Laenge bis zum letzten Zeichen ungleich Leerzeichen) hat unerlaubten Wert\n'//&
               'erlaubte Stringlaengen = [19,29,33,34] \n'//&
               'aktuelle Stringlaenge = <i_len> \n'//&
               'String = <String> \n'//&
               '--> Datumsstring pruefen' )
       ENDIF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 025
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic),-1070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Setzen von Voreinstellungen\n'//&
               'Fehler beim Setzen der voreingestellten Zeitzone \n'//&
               'zone = <..zone..> \n'//&
               'erlaubte Zeitzonen-Strings siehe Methode set_datetime_predefined_zone_c \n'//&
               '--> Angabe der Zeitzone pruefen' )
       ENDIF
       !
       ! Allokieren der Felder beim ersten Durchlauf (i==1)
       !
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    ENDDO
    !
    !
  END SUBROUTINE init_datetime_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_datetime_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors ) 
    !
  END SUBROUTINE clear_datetime_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "hour" eines Objektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_datetime_hour ( this ) &
       RESULT( ok )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_datetime_hour' 
    !! temporaerer String
    CHARACTER (LEN=7) :: c_tmp
    !
    ok = ( 0 <= this%hour .AND. this%hour <= 23 )
    !
    IF ( .NOT. ok ) THEN
       !
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       WRITE(c_tmp,*) this%hour
       CALL setup_error_act ( '<hour>',  c_tmp ) 
       CALL setup_error_act ( '<datetime>',  datetime_to_string(this) ) 
       !
    ENDIF
    !
  END FUNCTION ok_datetime_hour
  !
  !! Pr&uuml;fe, ob die Komponente "min" eines Objektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_datetime_min ( this ) &
       RESULT( ok )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_datetime_min' ! 
    !! temporaerer String
    CHARACTER (LEN=7) :: c_tmp
    !
    ok = ( 0 <= this%min .AND. this%min <= 59 ) 
    !
    IF ( .NOT. ok ) THEN
       !
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(c_tmp,*) this%min
       CALL setup_error_act ( '<min>',  c_tmp ) 
       CALL setup_error_act ( '<datetime>',  datetime_to_string(this) ) 
       !
    ENDIF
    !
  END FUNCTION ok_datetime_min
  !
  !! Pr&uuml;fe, ob die Komponente "sec" eines Objektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_datetime_sec ( this ) &
       RESULT( ok )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_datetime_sec' ! 
    !! temporaerer String
    CHARACTER (LEN=7) :: c_tmp
    !
    ok = ( 0 <= this%sec .AND. this%sec <= 59 ) 
    !
    IF ( .NOT. ok ) THEN
       !
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       WRITE(c_tmp,*) this%sec
       CALL setup_error_act ( '<sec>',  c_tmp ) 
       CALL setup_error_act ( '<datetime>',  datetime_to_string(this) ) 
       !
    ENDIF
    !
  END FUNCTION ok_datetime_sec
  !
  !! Pr&uuml;fe, ob die Komponente "nanosec" eines Objektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_datetime_nanosec ( this ) &
       RESULT( ok )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_datetime_nanosec' ! 
    !! temporaerer String
    CHARACTER (LEN=7) :: c_tmp
    !
    ok = ( 0 <= this%nanosec .AND. this%nanosec <= 999999999 ) 
    !
    IF ( .NOT. ok ) THEN
       !
       CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
       WRITE(c_tmp,*) this%nanosec
       CALL setup_error_act ( '<nanosec>',  c_tmp ) 
       CALL setup_error_act ( '<datetime>',  datetime_to_string(this) ) 
       !
    ENDIF
    !
  END FUNCTION ok_datetime_nanosec
  !
  !! Pr&uuml;fe, ob die Komponente "zone" eines Objektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_datetime_zone ( this ) &
       RESULT( ok )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_datetime_zone' ! 
    !! temporaerer String
    CHARACTER (LEN=7) :: c_tmp
    !
    ok = ( -11 <= this%zone .AND. this%zone <= 12 )
    !
    IF ( .NOT. ok ) THEN
       !
       CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
       WRITE(c_tmp,*) this%zone
       CALL setup_error_act ( '<zone>',  c_tmp ) 
       CALL setup_error_act ( '<datetime>',  datetime_to_string(this) ) 
       !
    ENDIF
    !
  END FUNCTION ok_datetime_zone
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "date" eines Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_date ( this )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_datetime_date' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) 
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
       WRITE(*,*) ' Status /= 0'
    ENDIF
    !
    CALL print_date (this%date)
    !
8000 FORMAT &
          ('# Inhalt der Komponente date  - - - - - - - - - - - - - - - - ',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_datetime_date
  !
  !! Drucke den Inhalt der Komponente "hour" eines Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_hour ( this )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_datetime_hour' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%hour
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente hour  - - - - - - - - - - - - - - - - ',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
           '# hour = ', I7)
    !
  END SUBROUTINE print_datetime_hour
  !
  !! Drucke den Inhalt der Komponente "min" eines Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_min ( this )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_datetime_min' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%min
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente min - - - - - - - - - - - - - - - - - ',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
           '# min = ', I7)
    !
  END SUBROUTINE print_datetime_min
  !
  !! Drucke den Inhalt der Komponente "sec" eines Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_sec ( this )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_datetime_sec' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%sec
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente sec - - - - - - - - - - - - - - - - - ',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
           '# sec = ', I7)
    !
  END SUBROUTINE print_datetime_sec
  !
  !! Drucke den Inhalt der Komponente "nanosec" eines Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_nanosec ( this )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='print_datetime_nanosec' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%nanosec
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente nanosec - - - - - - - - - - - - - - - ',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
           '# nanosec = ', I8)
    !
  END SUBROUTINE print_datetime_nanosec
  !
  !! Drucke den Inhalt der Komponente "zone" eines Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_datetime_zone ( this )
    !! Objekt
    TYPE (t_datetime) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_datetime_zone' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%zone, c_zone(this%zone)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente zone  - - - - - - - - - - - - - - - - ',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
           '# zone = ', I2,' [',A,']')
    !
  END SUBROUTINE print_datetime_zone
  !
END MODULE b_datetime
! TailOfBaseModule --------------------------------------------------------
