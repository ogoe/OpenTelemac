! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Behandlung von Zeitinkrementen</h2>
!! @author Guntram Sei&szlig;, Susanne Spohr
!! @version 1.19 vom 03/23/07, Quellcode: mod_b_time.f90
!! <HR>
!! A module for handling time increments. <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-05-02 : Susanne Spohr, Guntram Sei&szlig; : abgeleitet von time_inc_class
!  01.02 : 2002/05/16 : S. Spohr   : Fehler in setup_time_trc_lun_d behoben
!  01.03 : 2002/05/30 : G. Seiss   : Uebergabeparameter in Settern -> INTEGER
!  01.04 : 2002/06/05 : G. Lang    : + ok_initialised modifiziert
!  01.05 : 2002/06/12 : S. Spohr   : Vereinheitlichung von Methoden-Namen
!  01.06 : 2002/06/12 : G. Lang    : + INIT/CLEAR und SETUP_*_PRN_LUN, SETUP_*_TRC_LUN modifiziert
!  01.07 : 2002/06/19 : S. Spohr   : + string_to_time-Methoden,
!                                    + time_to_string-Methoden verkuerzter Laenge
!  01.08 : 2002/06/19 : S. Spohr   : kleine Kommentarkorrektur fuer html-Doc
!  01.09 : 2002/08/12 : G. Lang    : + Testausgaben aus dem Code entfernt
!  01.10 : 2003/01/14 : S. Spohr   : Anpassungen HTMLDOC und Fehlerbehandlung
!  01.11 : 2003/01/16 : S. Spohr   : Begruessungsmeldung gekuerzt
!  01.12 : 2003/01/16 : S. Spohr   : Testroutine ausgelagert
!  01.13 : 2003/01/16 : S. Spohr   : alte Testausgaben entfernt
!  01.14 : 2003/10/24 : S. Spohr   : + is_integer_multiple,
!                                    + abs_time,
!                                    + get_nearest_integer_multiple
!  01.15 : 2004/01/19 : H. Weilbeer: bei selbstdefinierten Datentypen INTENT(OUT) -> INTENT(INOUT) gewandelt
!  01.16 : 2004/01/28 : G. Seiss   : Anpassung Digital Fortran Compiler
!  01.17 : 2005/03/11 : G. Lang    : neue (Operator-) Funktionen ad_time, su_time
!  01.18 : 2005/03/29 : G. Lang    : neue (Operator-) Funktionen eq_time, ne_time
!  01.19 : 2007/03/23 : G. Lang    : Funktionsaufrufe aus CARRY durch Unterprogrammaufrufe ersetzt
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Dieses Modul definiert ein Objekt Zeitinkrement TYPE (t_time) und           <BR>
!! stellt Funktionen und Operatoren f&uuml;r dieses Objekt zur Verf&uuml;gung. <BR>
!! Es handelt sich bei diesem Modul um einen speziellen Anwendungsfall des     <BR>
!! &Uuml;bertragsrechners, den das Basis-Modul "b_carry" enth&auml;lt.         <BR>
!! Funktionen und Operatoren des Typs "t_time" rufen im Prinzip lediglich      <BR>
!! die entsprechenden Funktionen und Operatoren des Typs "t_carry" auf.        <BR>
!! <HR>
!!                                                                  <BR>
!! <A href="b_time.html#links"<B><FONT COLOR="red" SIZE=14> [Zu den Links]</FONT></B></A>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_time <BR>
!! zur Verf&uuml;gung.<BR>
!! Dieser besteht aus den folgenden Komponenten: <BR>
!! <OL>
!!     <LI> sign     : das Vorzeichen ( +1 / -1 )
!!     <LI> days     : die vollen Tage
!!     <LI> hours    : die Stunden
!!     <LI> minutes  : die Minuten
!!     <LI> seconds  : die Sekunden
!!     <LI> nanos    : die Nanosekunden
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
!!    <LI> Initialisieren des Moduls b_time mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_time mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   <BR>
!!          andere nicht. 
!!          Routinen die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,  <BR>
!!          ob das Modul korrekt initialisert wurde (ok_initialised)  <BR>
!!                                                                    <BR>
!! <HR>
!! Allgemein             [  0000 bis  0999 ]                        <BR>
!! 00000 = kein Fehler                                              <BR> 
!! 00001 = Modul ist nicht initialisiert.                           <BR>
!! 00002 = Modul ist schon initialisiert.                           <BR>
!! <HR>
!! INIT-Methoden         [  1000 bis  1999 ]                        <BR>
!! <HR>
!! CLEAR-Methoden        [  2000 bis  2999 ]                        <BR>
!! <HR>
!! SETUP-Methoden        [  3000 bis  3999 ]                        <BR>
!! <HR>
!! NEW-Methoden          [  4000 bis  4999 ]                        <BR>
!! <HR>
!! KILL-Methoden         [  5000 bis  5999 ]                        <BR>
!! <HR>
!! OK-Methoden           [  6000 bis  6999 ]                        <BR>
!! 06010 = Fehler in Komponente sign     des Typs t_time           <BR>
!! 06020 = Fehler in Komponente days     des Typs t_time           <BR>
!! 06030 = Fehler in Komponente hours    des Typs t_time           <BR>
!! 06040 = Fehler in Komponente minutes  des Typs t_time           <BR>
!! 06050 = Fehler in Komponente seconds  des Typs t_time           <BR>
!! 06060 = Fehler in Komponente nanos    des Typs t_time           <BR>
!! <HR>
!! PRINT-Methoden        [  7000 bis  7999 ]                        <BR>
!! 07001 = Drucken der Kopfzeilen                                   <BR>
!! 07002 = Drucken der Fu&szlig;zeilen                              <BR>
!! 07003 = Drucken des Index des Datenobjektes (1D-Array)           <BR>
!! 07010 = Drucken des Datenobjektes vom Typ t_time                 <BR>
!! 07500 = Drucken der statischen Daten (ohne Fehlermeldungen)      <BR>
!! <HR>
!! SET-Methoden          [  8000 bis  8999 ]                        <BR>
!! <HR>
!! GET-Methoden          [  9000 bis  9999 ]                        <BR>
!! <HR>
!! OPERATOR(==)-Methoden [ 10000 bis 10999 ]                        <BR>
!! <HR>
!! OPERATOR(+)-Methoden  [ 11000 bis 11999 ]                        <BR>
!! <HR>
!! OPERATOR(-)-Methoden  [ 12000 bis 12999 ]                        <BR>
!! <HR>
!! OPERATOR(*)-Methoden  [ 13000 bis 13999 ]                        <BR>
!! <HR>
!! OPERATOR(/)-Methoden  [ 14000 bis 14999 ]                        <BR>
!! <HR>
!! OPERATOR(>)-Methoden  [ 15000 bis 15999 ]                        <BR>
!! <HR>
!! OPERATOR(>=)-Methoden [ 16000 bis 16999 ]                        <BR>
!! <HR>
!! OPERATOR(<)-Methoden  [ 17000 bis 17999 ]                        <BR>
!! <HR>
!! OPERATOR(<=)-Methoden [ 18000 bis 18999 ]                        <BR>
!! <HR>
!! OPERATOR(/=)-Methoden [ 19000 bis 19999 ]                        <BR>
!! <HR>
!! MODUL-SPEZIFISCHE-Methoden   [      < 0 ]                        <BR>
!! <BR>
!! STRING_TO_TIME-Methoden :                                        <BR>
!! . -10 = Zeitinkrement-String enthaelt nur Leerzeichen            <BR>
!! . -20 = Zeitinkrement-String hat ungueltiges 1. Zeichen          <BR>
!! . -30 = Zeitinkrement-String (ohne VZ) laenger als 25 Zeichen    <BR>
!! . -40 = Zeitinkrement-String hat ungueltiges Format              <BR>
!! . -50 = Fehler beim Lesen des Zeitinkrements                     <BR>
!! <BR>
!! <A NAME="links"> </A>
!! <HR>
MODULE b_time
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       short,        &
       long,         &
       double
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
  ! [A.3] BASIS-Modul mit Uebertragsrechner und -Routinen
  !
  USE b_carry, ONLY :       &
       ! Typdefinitionen
       t_carry,             &
       ! Routinen / Interfaces
       init_carry,          &
       clear_carry,         &
       new_carry,           &
       kill_carry,          &
       setup_carry_prn_lun, &
       setup_carry_trc_lun, &
       set_carry,           &
       set_carry_values,    &
       get_carry_values,    &
       get_carry_sign,      &
       rnd_carry,           &
       carry_to_real,       &
       double_to_carry,     & 
       ! Operatoren
       OPERATOR(==),        &
       add_carry,           &
       sub_carry,           &
       mul_carry,           &
       div_carry,           &
       OPERATOR(>),         &
       OPERATOR(>=),        &
       OPERATOR(<),         &
       OPERATOR(<=),        &
       OPERATOR(/=),        &
       OPERATOR(.DIV.)
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
  !! <kurze Beschreibung des Typs angeben>  
  !! sign     : das Vorzeichen  ( +1, -1 )
  !! days     : die vollen Tage
  !! hours    : die Stunden
  !! minutes  : die Minuten
  !! seconds  : die Sekunden
  !! nanos    : die Nanosekunden
  TYPE , PUBLIC :: t_time
     PRIVATE
     INTEGER (kind=short)  :: sign
     INTEGER               :: days
     INTEGER (kind=short)  :: hours
     INTEGER (kind=short)  :: minutes
     INTEGER (kind=short)  :: seconds
     INTEGER               :: nanos
  END TYPE t_time
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
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls.
  INTERFACE init_time
     MODULE PROCEDURE init_time_d ! 
  END INTERFACE
  !
  !! De-Allokieren/Re-Initialisieren der statischen Datenobjekte des Moduls.
  INTERFACE clear_time
     MODULE PROCEDURE clear_time_d ! 
  END INTERFACE
  !
  !! Logische Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden auf
  !! Benutzerwert setzen. <BR>
  !! Keine Ausgabe, wenn <EM>PRN_LUN</EM> = -1 .<BR>
  !! Ausgabe nur, wenn <EM>PRN_LUN</EM> = >0 .<BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_time_prn_lun
     MODULE PROCEDURE setup_time_prn_lun_d ! 
  END INTERFACE
  !
  !! Logische Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden auf
  !! Benutzerwert setzen. <BR>
  !! Keine Ausgabe, wenn <EM>TRC_LUN</EM> = -1 .<BR>
  !! Ausgabe nur, wenn <EM>TRC_LUN</EM> = >0 .<BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_time_trc_lun
     MODULE PROCEDURE setup_time_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen eines neuen Objektes vom Typ "t_time" (Skalar, 1D-Array). <BR>
  !! NULLIFY f&uuml;r dynamische Komponenten-Felder. <BR>
  !! Initialisieren mit Default-Werten.
  INTERFACE new_time
     MODULE PROCEDURE new_time_0  ! Version fuer Skalar
     MODULE PROCEDURE new_time_1  ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Vernichten von Datenobjekten "t_time" (Skalar, 1D-Array).<BR>
  !! Ggf. De-Allokieren von Memory.<BR>
  !! Teilweise Re-Initialisieren mit Default-Werten.
  INTERFACE kill_time
     MODULE PROCEDURE kill_time_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_time_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Pr&uuml;fen von Datenobjekten "t_time" auf G&uuml;ltigkeit (Skalar, 1D-Array).<BR>
  !! Aufrufe: <BR>
  !! a) valid = ok_time(this)<BR>
  !! b) valid(:) = ok_time(this(:))
  INTERFACE ok_time
     MODULE PROCEDURE ok_time_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_time_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Drucken aller Komponenten von Datenobjekten "t_time"  auf <EM>PRN_LUN</EM>
  !! (Skalar, 1D-Array). <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_time
     MODULE PROCEDURE print_time_0 ! Version fuer Skalar
     MODULE PROCEDURE print_time_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Drucken aller in diesem Modul abgelegten statischen Daten auf <EM>PRN_LUN</EM>. <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_time_static
     MODULE PROCEDURE print_time_static_d ! 
  END INTERFACE
  !
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls auf <EM>PRN_LUN</EM>. <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_time_all_errors
     MODULE PROCEDURE print_time_all_errors_d ! 
  END INTERFACE
  !
  !! Setter, der alle Komponenten von "t_time" gleichzeitig auf Benutzerwerte setzt. <BR>
  !! Aufrufe: <BR>
  !! a) Objekt (Skalar) / Daten (Skalar)<BR>
  !! b) Objekt (Vektor) / Daten (Skalar) 
  INTERFACE set_time
     MODULE PROCEDURE set_time_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_time_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Setze Komponente "sign" in "t_time" auf Benutzerwert
  INTERFACE set_time_sign
     MODULE PROCEDURE set_time_sign_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_time_sign_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Setze Komponente "days" in "t_time" auf Benutzerwert
  INTERFACE set_time_days
     MODULE PROCEDURE set_time_days_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_time_days_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Setze Komponente "hours" in "t_time" auf Benutzerwert
  INTERFACE set_time_hours
     MODULE PROCEDURE set_time_hours_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_time_hours_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Setze Komponente "minutes" in "t_time" auf Benutzerwert
  INTERFACE set_time_minutes
     MODULE PROCEDURE set_time_minutes_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_time_minutes_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Setze Komponente "seconds" in "t_time" auf Benutzerwert
  INTERFACE set_time_seconds
     MODULE PROCEDURE set_time_seconds_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_time_seconds_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Setze Komponente "nanos" in "t_time" auf Benutzerwert
  INTERFACE set_time_nanos
     MODULE PROCEDURE set_time_nanos_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_time_nanos_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  !! Hole Komponente "sign" aus "t_time"
  INTERFACE get_time_sign
     MODULE PROCEDURE get_time_sign_0_0 ! Skalar
     MODULE PROCEDURE get_time_sign_1_0 ! Vektor
  END INTERFACE
  !
  !! Hole Komponente "days" aus "t_time"
  INTERFACE get_time_days
     MODULE PROCEDURE get_time_days_0_0 ! Skalar
     MODULE PROCEDURE get_time_days_1_0 ! Vektor
  END INTERFACE
  !
  !! Hole Komponente "hours" aus "t_time"
  INTERFACE get_time_hours
     MODULE PROCEDURE get_time_hours_0_0 ! Skalar
     MODULE PROCEDURE get_time_hours_1_0 ! Vektor
  END INTERFACE
  !
  !! Hole Komponente "minutes" aus "t_time"
  INTERFACE get_time_minutes
     MODULE PROCEDURE get_time_minutes_0_0 ! Skalar
     MODULE PROCEDURE get_time_minutes_1_0 ! Vektor
  END INTERFACE
  !
  !! Hole Komponente "seconds" aus "t_time"
  INTERFACE get_time_seconds
     MODULE PROCEDURE get_time_seconds_0_0 ! Skalar
     MODULE PROCEDURE get_time_seconds_1_0 ! Vektor
  END INTERFACE
  !
  !! Hole Komponente "nanos" aus "t_time"
  INTERFACE get_time_nanos
     MODULE PROCEDURE get_time_nanos_0_0 ! Skalar
     MODULE PROCEDURE get_time_nanos_1_0 ! Vektor
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  ! ----------------------------------------------------------------
  ! --> MODULSPEZIFISCHE-Interfaces
  ! ----------------------------------------------------------------
  !
  !! Runde den Wert eines Time-Objektes auf Tage.
  INTERFACE round_time_to_days
     MODULE PROCEDURE round_time_to_days_0 ! 
     MODULE PROCEDURE round_time_to_days_1 ! 
  END INTERFACE
  !
  !! Runde den Wert eines Time-Objektes auf Stunden.
  INTERFACE round_time_to_hours
     MODULE PROCEDURE round_time_to_hours_0 ! 
     MODULE PROCEDURE round_time_to_hours_1 ! 
  END INTERFACE
  !
  !! Runde den Wert eines Time-Objektes auf Minuten.
  INTERFACE round_time_to_minutes
     MODULE PROCEDURE round_time_to_minutes_0 ! 
     MODULE PROCEDURE round_time_to_minutes_1 ! 
  END INTERFACE
  !
  !! Runde den Wert eines Time-Objektes auf Sekunden.
  INTERFACE round_time_to_seconds
     MODULE PROCEDURE round_time_to_seconds_0 ! 
     MODULE PROCEDURE round_time_to_seconds_1 ! 
  END INTERFACE
  !
  !! Konvertiere den Wert eines Time-Objektes in eine
  !! Double-Real-Zahl in Tagen.
  INTERFACE time_to_real_days
     MODULE PROCEDURE time_to_real_days_0 ! 
     MODULE PROCEDURE time_to_real_days_1 ! 
  END INTERFACE
  !
  !! Konvertiere den Wert eines Time-Objektes in eine
  !! Double-Real-Zahl in Stunden.
  INTERFACE time_to_real_hours
     MODULE PROCEDURE time_to_real_hours_0 ! 
     MODULE PROCEDURE time_to_real_hours_1 ! 
  END INTERFACE
  !
  !! Konvertiere den Wert eines Time-Objektes in eine
  !! Double-Real-Zahl in Minuten.
  INTERFACE time_to_real_minutes
     MODULE PROCEDURE time_to_real_minutes_0 ! 
     MODULE PROCEDURE time_to_real_minutes_1 ! 
  END INTERFACE
  !
  !! Konvertiere den Wert eines Time-Objektes in eine
  !! Double-Real-Zahl in Sekunden.
  INTERFACE time_to_real_seconds
     MODULE PROCEDURE time_to_real_seconds_0 ! 
     MODULE PROCEDURE time_to_real_seconds_1 ! 
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert einer Double-Real-Zahl, welcher in Tagen
  !! vorliegt, in ein Time-Objekt.
  INTERFACE real_days_to_time
     MODULE PROCEDURE real_days_to_time_0 ! 
     MODULE PROCEDURE real_days_to_time_1 ! 
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert einer Double-Real-Zahl, welcher in Stunden
  !! vorliegt, in ein Time-Objekt.
  INTERFACE real_hours_to_time
     MODULE PROCEDURE real_hours_to_time_0 ! 
     MODULE PROCEDURE real_hours_to_time_1 ! 
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert einer Double-Real-Zahl, welcher in Minuten
  !! vorliegt, in ein Time-Objekt.
  INTERFACE real_minutes_to_time
     MODULE PROCEDURE real_minutes_to_time_0 ! 
     MODULE PROCEDURE real_minutes_to_time_1 ! 
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert einer Double-Real-Zahl, welcher in Sekunden
  !! vorliegt, in ein Time-Objekt.
  INTERFACE real_seconds_to_time
     MODULE PROCEDURE real_seconds_to_time_0 ! 
     MODULE PROCEDURE real_seconds_to_time_1 ! 
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert eines CHARACTER-String in ein Time-Objekt.<BR>
  !! Standard-Eingabe-Formate sind :
  !! <UL>
  !!    <LI> +dddddd-hh:mm:ss.nnnnnnnnn
  !!    <LI> +dddddd-hh:mm:ss
  !!    <LI> +hh:mm:ss.nnnnnnnnn
  !!    <LI> +hh:mm:ss
  !! </UL>
  !! Allerdings kann dabei :
  !! <UL>
  !!    <LI> die Tagesangabe aus 1 bis 6 Zeichen bestehen.<BR>
  !!         Auch moeglich ist bspw : +d-hh:mm:ss</LI>
  !!    <LI> die Angabe der Nanosekunden aus 1 bis 9 Zeichen bestehen.<BR>
  !!         Auch moeglich ist bspw : +hh:mm:ss.nn</LI>
  !! </UL>
  INTERFACE string_to_time
     MODULE PROCEDURE string_to_time_0 !
     MODULE PROCEDURE string_to_time_1 !
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert eines Time-Objektes in einen CHARACTER-String.<BR>
  !! Erzeugt einen vollst&auml;ndigen Zeitinkrement-String.<BR>
  !! Ausgabeformat (LEN=26) : +dddddd-hh:mm:ss.nnnnnnnnn
  INTERFACE time_to_string
     MODULE PROCEDURE time_to_string_26_0 !
     MODULE PROCEDURE time_to_string_26_1 !
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert eines Time-Objektes in einen CHARACTER-String.<BR>
  !! Erzeugt einen Zeitinkrement-String ohne Nanosekunden.<BR>
  !! Ausgabeformat (LEN=16) : +dddddd-hh:mm:ss
  INTERFACE time_to_string_16
     MODULE PROCEDURE time_to_string_16_0 !
     MODULE PROCEDURE time_to_string_16_1 !
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert eines Time-Objektes in einen CHARACTER-String.<BR>
  !! Erzeugt einen Zeitinkrement-String ohne Tagesangabe.<BR>
  !! Ausgabeformat (LEN=19) : +hh:mm:ss.nnnnnnnnn
  INTERFACE time_to_string_19
     MODULE PROCEDURE time_to_string_19_0 !
     MODULE PROCEDURE time_to_string_19_1 !
  END INTERFACE
  !
  !! &Uuml;bertrage den Wert eines Time-Objektes in einen CHARACTER-String.<BR>
  !! Erzeugt einen Zeitinkrement-String ohne Tagesangabe und ohne Nanosekunden.<BR>
  !! Ausgabeformat (LEN= 9) : +hh:mm:ss
  INTERFACE time_to_string_9
     MODULE PROCEDURE time_to_string_9_0 !
     MODULE PROCEDURE time_to_string_9_1 !
  END INTERFACE
  !
  !! Pr&uuml;ft, ob ein Time-Objekt ein ganzzahliges Vielfaches eines anderen
  !! Time-Objektes ist.
  INTERFACE is_integer_multiple
     MODULE PROCEDURE is_integer_multiple_0_0 !
  END INTERFACE
  !
  !! Berechnet den Absolutwert eines Objektes "t_time"
  INTERFACE abs_time
     MODULE PROCEDURE abs_time_0_0 !
  END INTERFACE
  !
  !! Berechnet das zu Time-Objekt 1 betragsmaessig naechstgelegene ganzzahlige Vielfache
  !! von Time-Objekt 2, mit Vorzeichen von Time-Objekt 2. <BR>
  INTERFACE get_nearest_integer_multiple
     MODULE PROCEDURE get_nearest_int_multiple_0_0 ! 
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
  !! Pr&uuml;fung zweier Datenobjekte "t_time" auf Gleichheit
  INTERFACE eq_time
     MODULE PROCEDURE eq_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Pr&uuml;fung zweier Datenobjekte "t_time" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Addition zweier Datenobjekte "t_time"
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE ad_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ad_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ad_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ad_time_1_1  ! Vektor / Vektor
  END INTERFACE
  INTERFACE ad_time
     MODULE PROCEDURE ad_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ad_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ad_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ad_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Subtraktion zweier Datenobjekte "t_time"  oder
  !! Negation von Datenobjekt "t_time"
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE su_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE su_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE su_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE su_time_1_1  ! Vektor / Vektor
     MODULE PROCEDURE negate_time_0  ! Negation Skalar
     MODULE PROCEDURE negate_time_1  ! Negation Vektor
  END INTERFACE
  INTERFACE su_time
     MODULE PROCEDURE su_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE su_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE su_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE su_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Multiplikation von Datenobjekten "t_time" mit einem INTEGER-Wert
  INTERFACE OPERATOR(*)
     MODULE PROCEDURE mu_time_I0_0  ! Integer / Skalar
     MODULE PROCEDURE mu_time_0_I0  ! Skalar  / Integer
     MODULE PROCEDURE mu_time_I0_1  ! Integer / Vektor
     MODULE PROCEDURE mu_time_1_I0  ! Vektor  / Integer
  END INTERFACE
  !
  !! Division Datenobjekte "t_time" durch Integer-Faktor
  INTERFACE OPERATOR(/)
     MODULE PROCEDURE di_time_0_I0  ! Skalar / Integer
     MODULE PROCEDURE di_time_1_I0  ! Vektor / Integer
     ! vielleicht spaeter noch realisieren :
     ! MODULE PROCEDURE di_time_0_0  ! Skalar / Skalar - Res : REAL
     ! MODULE PROCEDURE di_time_1_0  ! Vektor / Skalar - REs : REAL
     ! MODULE PROCEDURE di_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Division Datenobjekt "t_time" durch Datenobjekt "t_time".
  !! Ergebnis ist ein INTEGER (KIND=long)-Wert. <BR>
  !! Derzeit nur als Skalar-Version realisiert!
  INTERFACE OPERATOR(.DIV.)
     MODULE PROCEDURE idiv_time_0_0 ! Skalar .DIV. Skalar
     MODULE PROCEDURE idiv_time_1_0 ! Vektor .DIV. Skalar
  END INTERFACE
  !
  !! Vergleich ">" zweier Datenobjekte "t_time"
  INTERFACE OPERATOR(>)
     MODULE PROCEDURE gt_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE gt_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE gt_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE gt_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich ">=" zweier Datenobjekte "t_time"
  INTERFACE OPERATOR(>=)
     MODULE PROCEDURE ge_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ge_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ge_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ge_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich "<" zweier Datenobjekte "t_time"
  INTERFACE OPERATOR(<) 
     MODULE PROCEDURE lt_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE lt_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE lt_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE lt_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich "<=" zweier Datenobjekte "t_time"
  INTERFACE OPERATOR(<=)
     MODULE PROCEDURE le_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE le_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE le_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE le_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_time" auf Ungleichheit
  INTERFACE ne_time
     MODULE PROCEDURE ne_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Pr&uuml;fung zweier Datenobjekte "t_time" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_time_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_time_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_time_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_time_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_time                 ! Initialisieren (Modul)
  PUBLIC :: clear_time                ! De-Initialisieren (Modul)
  PUBLIC :: setup_time_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_time_trc_lun        ! Setzen trc_lun 
  PUBLIC :: new_time                  ! Erzeugen 
  PUBLIC :: kill_time                 ! Vernichten
  PUBLIC :: ok_time                   ! Pruefen
  PUBLIC :: print_time                ! Drucken
  PUBLIC :: print_time_static         ! Drucken aller statischen Daten
  PUBLIC :: print_time_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_time         ! Setzen aller Komponenten gleichzeitig
  PUBLIC :: set_time_sign    ! Setzen der Komponente sign
  PUBLIC :: set_time_days    ! Setzen der Komponente days
  PUBLIC :: set_time_hours   ! Setzen der Komponente hours
  PUBLIC :: set_time_minutes ! Setzen der Komponente minutes
  PUBLIC :: set_time_seconds ! Setzen der Komponente seconds
  PUBLIC :: set_time_nanos   ! Setzen der Komponente nanos
  PUBLIC :: get_time_sign    ! Holen der Komponente sign
  PUBLIC :: get_time_days    ! Holen der Komponente days
  PUBLIC :: get_time_hours   ! Holen der Komponente hours
  PUBLIC :: get_time_minutes ! Holen der Komponente minutes
  PUBLIC :: get_time_seconds ! Holen der Komponente seconds
  PUBLIC :: get_time_nanos   ! Holen der Komponente nanos
  PUBLIC :: OPERATOR(==)                      ! Operator "=="
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  PUBLIC :: round_time_to_days     ! Runde Time-Objekt auf Tage
  PUBLIC :: round_time_to_hours    ! Runde Time-Objekt auf Stunden
  PUBLIC :: round_time_to_minutes  ! Runde Time-Objekt auf Minuten
  PUBLIC :: round_time_to_seconds  ! Runde Time-Objekt auf Sekunden
  PUBLIC :: time_to_real_days      ! Ermittle Real-Wert des Time-Objektes in Tagen
  PUBLIC :: time_to_real_hours     ! Ermittle Real-Wert des Time-Objektes in Stunden
  PUBLIC :: time_to_real_minutes   ! Ermittle Real-Wert des Time-Objektes in Minuten
  PUBLIC :: time_to_real_seconds   ! Ermittle Real-Wert des Time-Objektes in Sekunden
  PUBLIC :: real_days_to_time      ! Uebertrage Real-Wert in Tagen nach Time-Objekt
  PUBLIC :: real_hours_to_time     ! Uebertrage Real-Wert in Stunden nach Time-Objekt
  PUBLIC :: real_minutes_to_time   ! Uebertrage Real-Wert in Minuten nach Time-Objekt
  PUBLIC :: real_seconds_to_time   ! Uebertrage Real-Wert in Sekunden nach Time-Objekt
  PUBLIC :: string_to_time         ! Uebertrage den Wert eines Strings in ein Time-Objekt
  PUBLIC :: time_to_string         ! Erzeuge Zeitinkrement-String : +dddddd-hh:mm:ss.nnnnnnnnn
  PUBLIC :: time_to_string_16      ! Erzeuge Zeitinkrement-String : +dddddd-hh:mm:ss
  PUBLIC :: time_to_string_19      ! Erzeuge Zeitinkrement-String : +hh:mm:ss.nnnnnnnnn
  PUBLIC :: time_to_string_9       ! Erzeuge Zeitinkrement-String : +hh:mm:ss
  PUBLIC :: is_integer_multiple    ! Prueft ob Time-Objekt ganzzahliges Vielfaches eines anderen
                                   ! Time-Objektes ist
  PUBLIC :: abs_time               ! Berechnet den Absolutwert eines Objektes "t_time"
  PUBLIC :: get_nearest_integer_multiple ! Berechnet das zu Time-Objekt 1 betragsmaessig
                                         ! naechstgelegene ganzzahlige Vielfache
                                         ! von Time-Objekt 2, mit Vorzeichen von Time-Objekt 2
  !
  PUBLIC :: ad_time
  PUBLIC :: su_time
  PUBLIC :: ne_time
  PUBLIC :: eq_time
  PUBLIC :: OPERATOR(+)                       ! Operator "+"
  PUBLIC :: OPERATOR(-)                       ! Operator "-"
  PUBLIC :: OPERATOR(*)                       ! Operator "*"
  PUBLIC :: OPERATOR(/)                       ! Operator "/"
  PUBLIC :: OPERATOR(.DIV.)                   ! Operator ".DIV."
  PUBLIC :: OPERATOR(>)                       ! Operator ">"
  PUBLIC :: OPERATOR(>=)                      ! Operator ">="
  PUBLIC :: OPERATOR(<)                       ! Operator "<"
  PUBLIC :: OPERATOR(<=)                      ! Operator "<="
  PUBLIC :: OPERATOR(/=)                      ! Operator "/="
  !
!>WIN-NT:  PUBLIC ::  eq_time_0_0, gt_time_0_0, ge_time_0_0, lt_time_0_0, &
!>WIN-NT:             le_time_0_0, ne_time_0_0
!>WIN-NT:  PUBLIC ::  eq_time_0_1, gt_time_0_1, ge_time_0_1, lt_time_0_1, &
!>WIN-NT:             le_time_0_1, ne_time_0_1
!>WIN-NT:  PUBLIC ::  eq_time_1_0, gt_time_1_0, ge_time_1_0, lt_time_1_0, &
!>WIN-NT:             le_time_1_0, ne_time_1_0
!>WIN-NT:  PUBLIC ::  eq_time_1_1, gt_time_1_1, ge_time_1_1, lt_time_1_1, &
!>WIN-NT:             le_time_1_1, ne_time_1_1
!>WIN-NT:  PUBLIC ::  ad_time_0_0, su_time_0_0
!>WIN-NT:  PUBLIC ::  ad_time_0_1, su_time_0_1
!>WIN-NT:  PUBLIC ::  ad_time_1_0, su_time_1_0
!>WIN-NT:  PUBLIC ::  ad_time_1_1, su_time_1_1
!>WIN-NT:  PUBLIC ::  negate_time_0
!>WIN-NT:  PUBLIC ::  negate_time_1
!>WIN-NT:  PUBLIC ::  mu_time_i0_0, mu_time_0_i0
!>WIN-NT:  PUBLIC ::  mu_time_i0_1, mu_time_1_i0
!>WIN-NT:  PUBLIC ::  di_time_0_i0
!>WIN-NT:  PUBLIC ::  di_time_1_i0
!>WIN-NT:  PUBLIC ::  idiv_time_0_0
!>WIN-NT:  PUBLIC ::  idiv_time_1_0
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
  CHARACTER (LEN=06), PARAMETER :: c_modname      = 'b_time' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_time
  INTEGER           , PARAMETER :: c_nofcomp      = 6                !
  !! Anzahl der Komponenten, in die der Datentyp zum Rechnen 
  !! zerlegt wird.
  INTEGER           , PARAMETER :: nkomp_tinc = 6
  !! Maximale Anzahl an Tagen
  INTEGER           , PARAMETER :: maxdays = huge(1)
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)     ! 
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
  !! Grenzwerte fuer die Komponenten des Datentyps t_time !
  INTEGER , TARGET, SAVE        :: time_limits(nkomp_tinc) = &
               (/ maxdays, 24 , 60, 60, 10000, 100000 /)
  !! Drei Hilfsvariablen vom Typ t_carry, welche bei Initialisierung des Moduls
  !! angelegt werden. Sie nehmen die Werte von t_time-Objekten auf, um auf ihnen
  !! die vom Modul b_carry bereitgestellten Vergleichs- und Rechenoperationen
  !! durchzufuehren.
  !! Dieses Bereitstellen der t_carry-Variablen fuehrt dazu, dass die Operationen
  !! garantiert fehlerfrei ablaufen.
  TYPE (t_carry)  , SAVE        :: carry1, carry2, rcarry
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
  SUBROUTINE init_time_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='init_time_d' 
    !! Initialisierungsfeld fuer die values-Komponente der beiden Hilfsvariablen
    !! vom Typ t_carry
    INTEGER (kind=long), DIMENSION(nkomp_tinc)  :: init_values !
    !
    init_values = 0 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_time" version 1.19 of 03/23/07                     '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_carry ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_time_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] die drei modulinternen Hilfsvariablen vom Typ t_carry anlegen
       IF ( no_error( ) ) CALL new_carry ( carry1 )
       IF ( no_error( ) ) CALL new_carry ( carry2 )
       IF ( no_error( ) ) CALL new_carry ( rcarry )
       IF ( no_error( ) ) CALL set_carry ( carry1, init_values, time_limits )
       IF ( no_error( ) ) CALL set_carry ( carry2, init_values, time_limits )
       IF ( no_error( ) ) CALL set_carry ( rcarry, init_values, time_limits )
       ! [1.7] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_time_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_time_d ( )
    !
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='clear_time_d' ! 
    !
    IF ( initialised .AND. n_init == 1) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_time_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Vernichten der drei modulinternen Hilfsvariablen vom Typ t_carry
       IF ( no_error( ) ) CALL kill_carry ( carry1 )
       IF ( no_error( ) ) CALL kill_carry ( carry2 )
       IF ( no_error( ) ) CALL kill_carry ( rcarry )
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.5] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.5.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_carry ( )
       ! [1.5.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_time_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_time_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_time_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_carry_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_time_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_time_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_time_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_carry_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_time_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_time_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='new_time_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       this%sign     = 1
       this%days     = 0
       this%hours    = 0
       this%minutes  = 0
       this%seconds  = 0
       this%nanos    = 0
       !
    END IF
    !
  END SUBROUTINE new_time_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_time_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='new_time_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL new_time_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_time_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_time_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='kill_time_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_time_0 ( this )
    END IF
    !
  END SUBROUTINE kill_time_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_time_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='kill_time_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL kill_time_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_time_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_time_0 ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=09), PARAMETER :: c_upname='ok_time_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       l_ok(1)  = ok_time_sign( this )
       l_ok(2)  = ok_time_days( this )
       l_ok(3)  = ok_time_hours( this )
       l_ok(4)  = ok_time_minutes( this )
       l_ok(5)  = ok_time_seconds( this )
       l_ok(6)  = ok_time_nanos( this )
       !
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_time_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_time_1 ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=09), PARAMETER :: c_upname='ok_time_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_time_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_time_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_time_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='print_time_0' 
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
       IF ( no_error( ) ) THEN
          !
          WRITE( UNIT    = prn_lun,  &
                 FMT     = *,        &
                 IOSTAT  = stat ) "THIS=", time_to_string ( this )  ! schreibe Inhalt
          !
          IF ( stat /= 0 ) &
                 CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
          !
       END IF
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
8000 FORMAT('# Beginn Objekt t_time ------------------------------')
8001 FORMAT('# Ende   Objekt t_time ------------------------------')
    !
  END SUBROUTINE print_time_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_time_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_time_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     & 
                 IOSTAT  = stat ) i
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          !
          IF ( no_error( ) ) CALL print_time_0 ( this(i) )
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
  END SUBROUTINE print_time_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_time_static_d ( )
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_time_static_d' 
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
       IF ( no_error( ) ) CALL print_time_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_time         ',/ &
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
  END SUBROUTINE print_time_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_time_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_time_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_time_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise allen Komponenten des Objektes vom Typ "t_time" (Skalar) <BR>
  !! einen skalaren Benutzerwert zu. <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_0_0 ( this, ivz, dd, hh, min, sec, nan )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this   ! 
    !! Wert f&uuml;r Komponente "sign"
    INTEGER              , INTENT(IN)  :: ivz ! 
    !! Wert f&uuml;r Komponente "days"
    INTEGER              , INTENT(IN)  :: dd  ! 
    !! Wert f&uuml;r Komponente "hours"
    INTEGER              , INTENT(IN)  :: hh  ! 
    !! Wert f&uuml;r Komponente "minutes"
    INTEGER              , INTENT(IN)  :: min ! 
    !! Wert f&uuml;r Komponente "seconds"
    INTEGER              , INTENT(IN)  :: sec ! 
    !! Wert f&uuml;r Komponente "nanos"
    INTEGER              , INTENT(IN)  :: nan ! 
    !
    CALL set_time_sign_0_0 ( this, ivz )
    CALL set_time_days_0_0 ( this, dd )
    CALL set_time_hours_0_0 ( this, hh )
    CALL set_time_minutes_0_0 ( this, min )
    CALL set_time_seconds_0_0 ( this, sec )
    CALL set_time_nanos_0_0 ( this, nan )
    !
  END SUBROUTINE set_time_0_0
  !
  !! weise allen Komponenten der Objekte vom Typ "t_time" (Vektor) <BR>
  !! einen skalaren Benutzerwert zu. <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_1_0 ( this, ivz, dd, hh, min, sec, nan )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:)   ! 
    !! Wert f&uuml;r Komponente "sign"
    INTEGER              , INTENT(IN)  :: ivz ! 
    !! Wert f&uuml;r Komponente "days"
    INTEGER              , INTENT(IN)  :: dd  ! 
    !! Wert f&uuml;r Komponente "hours"
    INTEGER              , INTENT(IN)  :: hh  ! 
    !! Wert f&uuml;r Komponente "minutes"
    INTEGER              , INTENT(IN)  :: min ! 
    !! Wert f&uuml;r Komponente "seconds"
    INTEGER              , INTENT(IN)  :: sec ! 
    !! Wert f&uuml;r Komponente "nanos"
    INTEGER              , INTENT(IN)  :: nan ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
      CALL set_time_0_0 ( this(i), ivz, dd, hh, min, sec, nan )
    ENDDO
    !
  END SUBROUTINE set_time_1_0
  !
  !! weise der Komponente "sign" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_sign_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "sign"
    INTEGER                 , INTENT(IN)  :: val  ! 
    !
    IF (val >= 0) THEN
       this%sign =  1
    ELSE
       this%sign = -1
    END IF
    !
  END SUBROUTINE set_time_sign_0_0
  !
  !! weise der Komponente "sign" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_sign_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "sign"
    INTEGER                       , INTENT(IN)  :: val     ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       CALL set_time_sign ( this(i), val )
    END DO
    !
  END SUBROUTINE set_time_sign_1_0
  !
  !! weise der Komponente "days" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_days_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "days"
    INTEGER       , INTENT(IN)  :: val  ! 
    !
    this%days = val
    !
  END SUBROUTINE set_time_days_0_0
  !
  !! weise der Komponente "days" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_days_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "days"
    INTEGER       , INTENT(IN)  :: val     ! 
    !
    this%days = val
    !
  END SUBROUTINE set_time_days_1_0
  !
  !! weise der Komponente "hours" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_hours_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "hours"
    INTEGER                       , INTENT(IN)  :: val  ! 
    !
    this%hours = val
    !
  END SUBROUTINE set_time_hours_0_0
  !
  !! weise der Komponente "hours" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_hours_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "hours"
    INTEGER                       , INTENT(IN)  :: val     ! 
    !
    this%hours = val
    !
  END SUBROUTINE set_time_hours_1_0
  !
  !! weise der Komponente "minutes" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_minutes_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "minutes"
    INTEGER                       , INTENT(IN)  :: val  ! 
    !
    this%minutes = val
    !
  END SUBROUTINE set_time_minutes_0_0
  !
  !! weise der Komponente "minutes" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_minutes_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "minutes"
    INTEGER                       , INTENT(IN)  :: val     ! 
    !
    this%minutes = val
    !
  END SUBROUTINE set_time_minutes_1_0
  !
  !! weise der Komponente "seconds" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_seconds_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "seconds"
    INTEGER                       , INTENT(IN)  :: val  ! 
    !
    this%seconds = val
    !
  END SUBROUTINE set_time_seconds_0_0
  !
  !! weise der Komponente "seconds" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_seconds_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "seconds"
    INTEGER                       , INTENT(IN)  :: val     ! 
    !
    this%seconds = val
    !
  END SUBROUTINE set_time_seconds_1_0
  !
  !! weise der Komponente "nanos" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_nanos_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "nanos"
    INTEGER       , INTENT(IN)  :: val  ! 
    !
    this%nanos = val
    !
  END SUBROUTINE set_time_nanos_0_0
  !
  !! weise der Komponente "nanos" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_time_nanos_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "nanos"
    INTEGER       , INTENT(IN)  :: val     ! 
    !
    this%nanos = val
    !
  END SUBROUTINE set_time_nanos_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "sign" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_sign_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "sign" (Skalar)
    INTEGER              :: val  ! 
    !! Hilfsgroesse
    INTEGER :: ihlp ! 
    !
    ihlp = this%sign
    !
    if(ihlp == 0) ihlp=1
    !
    val = ihlp /abs(ihlp)
    !
  END FUNCTION get_time_sign_0_0
  !
  !! hole die Komponente "sign" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_sign_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "sign"
    INTEGER              :: val(SIZE(this))  ! 
    !! Zaehler
    INTEGER :: i
    !
    DO i=1,SIZE(this)
       val(i) = get_time_sign_0_0( this(i) )
    END DO
    !
  END FUNCTION get_time_sign_1_0
  !
  !! hole die Komponente "days" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_days_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "days" (Skalar)
    INTEGER       :: val  ! 
    !
    val = this%days
    !
  END FUNCTION get_time_days_0_0
  !
  !! hole die Komponente "days" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_days_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "days"
    INTEGER       :: val(SIZE(this))  ! 
    !
    val = this%days
    !
  END FUNCTION get_time_days_1_0
  !
  !! hole die Komponente "hours" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_hours_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "hours" (Skalar)
    INTEGER              :: val  ! 
    !
    val = this%hours
    !
  END FUNCTION get_time_hours_0_0
  !
  !! hole die Komponente "hours" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_hours_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "hours"
    INTEGER              :: val(SIZE(this))  ! 
    !
    val = this%hours
    !
  END FUNCTION get_time_hours_1_0
  !
  !! hole die Komponente "minutes" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_minutes_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "minutes" (Skalar)
    INTEGER              :: val  ! 
    !
    val = this%minutes
    !
  END FUNCTION get_time_minutes_0_0
  !
  !! hole die Komponente "minutes" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_minutes_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "minutes"
    INTEGER              :: val(SIZE(this))  ! 
    !
    val = this%minutes
    !
  END FUNCTION get_time_minutes_1_0
  !
  !! hole die Komponente "seconds" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_seconds_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "seconds" (Skalar)
    INTEGER              :: val  ! 
    !
    val = this%seconds
    !
  END FUNCTION get_time_seconds_0_0
  !
  !! hole die Komponente "seconds" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_seconds_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "seconds"
    INTEGER              :: val(SIZE(this))  ! 
    !
    val = this%seconds
    !
  END FUNCTION get_time_seconds_1_0
  !
  !! hole die Komponente "nanos" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_nanos_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "nanos" (Skalar)
    INTEGER       :: val  ! 
    !
    val = this%nanos
    !
  END FUNCTION get_time_nanos_0_0
  !
  !! hole die Komponente "nanos" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_nanos_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "nanos"
    INTEGER       :: val(SIZE(this))  ! 
    !
    val = this%nanos
    !
  END FUNCTION get_time_nanos_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_time_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    ok = ( carry1 == carry2 )
    !
  END FUNCTION eq_time_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_time_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_time_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_time_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_time_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_time_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_time_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_time_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_time_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_time_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  !! Addiere zwei Datenobjekte ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ad_time_0_0 ( this1, this2 ) &
       RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Ergebnis: Additionsergebnis (Skalar)
    TYPE (t_time) :: this ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    CALL add_carry( carry1, carry2, rcarry ) ! >GL> new add
    !
    this = carry_to_time ( rcarry )
    !
  END FUNCTION ad_time_0_0
  !
  !! Addiere zwei Datenobjekte ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ad_time_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = ad_time_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ad_time_1_0
  !
  !! Addiere zwei Datenobjekte ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ad_time_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = ad_time_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ad_time_0_1
  !
  !! Addiere zwei Datenobjekte ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ad_time_1_1 ( this1, this2 ) &
       RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_time) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = ad_time_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ad_time_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  !! Subtrahiere zwei Datenobjekte ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION su_time_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Subtraktionsergebnis (Skalar)
    TYPE (t_time) :: this ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    CALL sub_carry( carry1, carry2, rcarry ) ! >GL> new sub
    !
    this     = carry_to_time ( rcarry )
    !
  END FUNCTION su_time_0_0
  !
  !! Subtrahiere zwei Datenobjekte ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION su_time_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_time_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION su_time_1_0
  !
  !! Subtrahiere zwei Datenobjekte ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION su_time_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_time_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION su_time_0_1
  !
  !! Subtrahiere zwei Datenobjekte ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION su_time_1_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_time) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_time_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION su_time_1_1
  !
  !! Negiere Datenobjekt ( Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION negate_time_0 ( this1 ) &
         RESULT( this )
    !! Objekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Negation (Skalar)
    TYPE (t_time) :: this ! 
    !
    this      = this1
    this%sign = -this1%sign
    !
  END FUNCTION negate_time_0
  !
  !! Negiere Datenobjekte ( Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION negate_time_1 ( this1 ) &
         RESULT( this )
    !! Objekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Negation (Skalar)
    TYPE (t_time) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       this(i) = negate_time_0 ( this1(i) )
    END DO
    !
  END FUNCTION negate_time_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! ----------------------------------------------------------------------
  !
  !! Multipliziere Integer-Wert mit Datenobjekt ( Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_time_I0_0 ( imul, this1 ) &
       RESULT( this )
    !! Integer-Wert
    INTEGER       , INTENT(IN)  :: imul ! 
    !! Objekt (Skalar)
    TYPE (t_time) , INTENT(IN)  :: this1 ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_time) :: this ! 
    !! Hilfsvariable
    INTEGER (kind=long)  :: il_mul ! 
    !
    il_mul = imul
    !
    CALL time_to_carry ( this1, carry1 )
    !
    CALL mul_carry( il_mul, carry1, rcarry ) ! >GL> new mul
    !
    this = carry_to_time ( rcarry )
    !
  END FUNCTION mu_time_I0_0
  !
  !! Multipliziere Datenobjekt ( Skalar ) mit Integer-Wert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_time_0_I0 ( this1, imul ) &
         RESULT( this )
    !! Objekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Integer-Wert
    INTEGER       , INTENT(IN) :: imul ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_time) :: this ! 
    !
    this = mu_time_I0_0( imul, this1 )
    !
  END FUNCTION mu_time_0_I0
  !
  !! Multipliziere Integer-Wert mit Datenobjekten (Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_time_I0_1 ( imul, this1 ) &
         RESULT( this )
    !! Integer-Wert
    INTEGER       , INTENT(IN) :: imul     ! 
    !! Objekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       this(i) = mu_time_I0_0( imul, this1(i) )
    END DO
    !
  END FUNCTION mu_time_I0_1
  !
  !! Multipliziere Datenobjekte (Vektor) mit Integer-Wert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_time_1_I0 ( this1, imul ) &
         RESULT( this )
    !! Objekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Integer-Wert
    INTEGER       , INTENT(IN) :: imul ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       this(i) = mu_time_I0_0( imul, this1(i) )
    END DO
    !
  END FUNCTION mu_time_1_I0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  !! Division eines Datenobjekts (Skalar) durch eine
  !! INTEGER-Zahl.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION di_time_0_I0 ( this1, idiv ) &
         RESULT( this )
    !! Objekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Integer-Wert
    INTEGER       , INTENT(IN) :: idiv ! 
    !! Divisionsergebnis (Skalar)
    TYPE (t_time) :: this ! 
    !! Hilfsvariable
    INTEGER (kind=long)  :: il_div
    !
    il_div = idiv
    !
    CALL time_to_carry ( this1, carry1 )
    CALL div_carry( carry1, il_div, rcarry ) ! >GL> new div
    !
    this     = carry_to_time ( rcarry )
    !
  END FUNCTION di_time_0_I0
  !
  !! Dividiere Datenobjekte (Vektor) durch eine Integer-Zahl <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION di_time_1_I0 ( this1, idiv ) &
         RESULT( this )
    !! Objekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Integer-Wert
    INTEGER       , INTENT(IN) :: idiv     ! 
    !! Divisionsergebnis (Vektor)
    TYPE (t_time) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       this(i) = di_time_0_I0( this1(i), idiv )
    END DO
    !
  END FUNCTION di_time_1_I0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(.DIV.)-Methoden <<< [ERR_NO <0]
  ! ----------------------------------------------------------------------
  !
  !! Dividiere zwei Datenobjekte ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION idiv_time_0_0 ( this1, this2 ) &
         RESULT( il_res )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Divisionsergebnis ( INTEGER(kind=long) )
    INTEGER (kind=long)        :: il_res ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    il_res   = carry1 .DIV. carry2
    !
  END FUNCTION idiv_time_0_0
  !
  !! Dividiere zwei Datenobjekte ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION idiv_time_1_0 ( this1, this2 ) &
         RESULT( il_res )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Divisionsergebnis (INTEGER(kind=long)-Vektor)
    INTEGER (kind=long)        :: il_res(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(il_res)
       il_res(i) = idiv_time_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION idiv_time_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_time_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    ok = ( carry1 > carry2 )
    !
  END FUNCTION gt_time_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_time_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_time_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION gt_time_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_time_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_time_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION gt_time_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_time_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_time_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION gt_time_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_time_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    ok = ( carry1 >= carry2 )
    !
  END FUNCTION ge_time_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_time_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_time_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ge_time_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_time_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_time_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ge_time_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_time_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_time_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ge_time_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_time_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    ok = ( carry1 < carry2 )
    !
  END FUNCTION lt_time_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_time_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_time_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION lt_time_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_time_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_time_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION lt_time_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_time_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_time_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION lt_time_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_time_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    ok = ( carry1 <= carry2 )
    !
  END FUNCTION le_time_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_time_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_time_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION le_time_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_time_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_time_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION le_time_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_time_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_time_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION le_time_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_time_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    CALL time_to_carry ( this1, carry1 )
    CALL time_to_carry ( this2, carry2 )
    !
    ok = ( carry1 /= carry2 )
    !
  END FUNCTION ne_time_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_time_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_time_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_time_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_time_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_time_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_time) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_time_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-MODULSPEZIFISCHE-Methoden <<< [ERR_NO <0]
  ! ----------------------------------------------------------------------
  !
  !! Rundet den Wert eines Time-Objektes (Skalar) auf Tage.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_days_0 ( this1 ) &
       RESULT( this )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! gerundetes Datenobjekt (Skalar)
    TYPE (t_time)  :: this !
    !! Hilfsvariable
    INTEGER        :: i_KompNr=1
    !
    this   = round_time_to_komponent ( this1, i_KompNr )
    !
  END FUNCTION round_time_to_days_0
  !
  !! Rundet Time-Objekte (Vektor) auf Tage.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_days_1 ( this1 ) &
       RESULT( this )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! gerundete Datenobjekte (Vektor)
    TYPE (t_time)  :: this(SIZE(this1)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(this1)
       this(i) = round_time_to_days_0 ( this1(i) )
    END DO
    !
  END FUNCTION round_time_to_days_1
  !
  !! Rundet den Wert eines Time-Objektes (Skalar) auf Stunden.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_hours_0 ( this1 ) &
         RESULT( this )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! gerundetes Datenobjekt (Skalar)
    TYPE (t_time)  :: this ! 
    !! Hilfsvariable
    INTEGER        :: i_KompNr=2
    !
    this   = round_time_to_komponent ( this1, i_KompNr )
    !
  END FUNCTION round_time_to_hours_0
  !
  !! Rundet Time-Objekte (Vektor) auf Stunden.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_hours_1 ( this1 ) &
         RESULT( this )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! gerundete Datenobjekte (Vektor)
    TYPE (t_time)  :: this(SIZE(this1)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(this1)
       this(i) = round_time_to_hours_0 ( this1(i) )
    END DO
    !
  END FUNCTION round_time_to_hours_1
  !
  !! Rundet den Wert eines Time-Objektes (Skalar) auf Minuten.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_minutes_0 ( this1 ) &
         RESULT( this )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! gerundetes Datenobjekt (Skalar)
    TYPE (t_time)  :: this ! 
    !! Hilfsvariable
    INTEGER        :: i_KompNr=3
    !
    this   = round_time_to_komponent ( this1, i_KompNr )
    !
  END FUNCTION round_time_to_minutes_0
  !
  !! Rundet Time-Objekte (Vektor) auf Minuten.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_minutes_1 ( this1 ) &
         RESULT( this )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! gerundete Datenobjekte (Vektor)
    TYPE (t_time)  :: this(SIZE(this1)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(this1)
       this(i) = round_time_to_minutes_0 ( this1(i) )
    END DO
    !
  END FUNCTION round_time_to_minutes_1
  !
  !! Rundet den Wert eines Time-Objektes (Skalar) auf Sekunden.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_seconds_0 ( this1 ) &
         RESULT( this )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! gerundetes Datenobjekt (Skalar)
    TYPE (t_time)  :: this ! 
    !! Hilfsvariable
    INTEGER        :: i_KompNr=4
    !
    this   = round_time_to_komponent ( this1, i_KompNr )
    !
  END FUNCTION round_time_to_seconds_0
  !
  !! Rundet Time-Objekte (Vektor) auf Sekunden.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_seconds_1 ( this1 ) &
         RESULT( this )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this1(:) ! 
    !! gerundete Datenobjekte (Vektor)
    TYPE (t_time)  :: this(SIZE(this1)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(this1)
       this(i) = round_time_to_seconds_0( this1(i) )
    END DO
    !
  END FUNCTION round_time_to_seconds_1
  !
  !! Ermittelt den Wert eines Time-Objektes (Skalar) in Tagen
  !! und schreibt ihn in eine (Double-)Real-Zahl. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real_days_0 ( this ) &
       RESULT( rwert )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Double-Real-Wert (Skalar)
    REAL (kind=double)  :: rwert !
    !! Hilfsvariable
    INTEGER        :: i_KompNr=1
    !
    rwert = time_to_real ( this, i_KompNr )
    !
  END FUNCTION time_to_real_days_0
  !
  !! Ermittelt die Werte von Time-Objekten (Vektor) in Tagen
  !! und schreibt sie in ein Feld von (Double-)Real-Zahlen. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real_days_1 ( this ) &
       RESULT( rwert )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Double-Real-Feld (Vektor)
    REAL (kind=double)   :: rwert(SIZE(this)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(this)
       rwert(i) = time_to_real_days_0 ( this(i) )
    END DO
    !
  END FUNCTION time_to_real_days_1
  !
  !! Ermittelt den Wert eines Time-Objektes (Skalar) in Stunden
  !! und schreibt ihn in eine (Double-)Real-Zahl. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real_hours_0 ( this ) &
       RESULT( rwert )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Double-Real-Wert (Skalar)
    REAL (kind=double)  :: rwert !
    !! Hilfsvariable
    INTEGER        :: i_KompNr=2
    !
    rwert = time_to_real ( this, i_KompNr )
    !
  END FUNCTION time_to_real_hours_0
  !
  !! Ermittelt die Werte von Time-Objekten (Vektor) in Stunden
  !! und schreibt sie in ein Feld von (Double-)Real-Zahlen. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real_hours_1 ( this ) &
         RESULT( rwert )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Double-Real-Feld (Vektor)
    REAL (kind=double)   :: rwert(SIZE(this)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(this)
       rwert(i) = time_to_real_hours_0 ( this(i) )
    END DO
    !
  END FUNCTION time_to_real_hours_1
  !
  !! Ermittelt den Wert eines Time-Objektes (Skalar) in Minuten
  !! und schreibt ihn in eine (Double-)Real-Zahl. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real_minutes_0 ( this ) &
         RESULT( rwert )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Double-Real-Wert (Skalar)
    REAL (kind=double)  :: rwert !
    !! Hilfsvariable
    INTEGER        :: i_KompNr=3
    !
    rwert = time_to_real ( this, i_KompNr )
    !
  END FUNCTION time_to_real_minutes_0
  !
  !! Ermittelt die Werte von Time-Objekten (Vektor) in Minuten
  !! und schreibt sie in ein Feld von (Double-)Real-Zahlen. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real_minutes_1 ( this ) &
         RESULT( rwert )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Double-Real-Feld (Vektor)
    REAL (kind=double)   :: rwert(SIZE(this)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(this)
       rwert(i) = time_to_real_minutes_0 ( this(i) )
    END DO
    !
  END FUNCTION time_to_real_minutes_1
  !
  !! Ermittelt den Wert eines Time-Objektes (Skalar) in Sekunden
  !! und schreibt ihn in eine (Double-)Real-Zahl. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real_seconds_0 ( this ) &
         RESULT( rwert )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Double-Real-Wert (Skalar)
    REAL (kind=double)  :: rwert !
    !! Hilfsvariable
    INTEGER        :: i_KompNr=4
    !
    rwert = time_to_real ( this, i_KompNr )
    !
  END FUNCTION time_to_real_seconds_0
  !
  !! Ermittelt die Werte von Time-Objekten (Vektor) in Sekunden
  !! und schreibt sie in ein Feld von (Double-)Real-Zahlen. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real_seconds_1 ( this ) &
         RESULT( rwert )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Double-Real-Feld (Vektor)
    REAL (kind=double)   :: rwert(SIZE(this)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(this)
       rwert(i) = time_to_real_seconds_0 ( this(i) )
    END DO
    !
  END FUNCTION time_to_real_seconds_1
  !
  !! Uebertraegt den Wert einer (Double-)Real-Zahl (Skalar), welcher in Tagen
  !! vorliegt, in ein Time-Objekt (Skalar). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_days_to_time_0 ( rwert ) &
       RESULT( this )
    !! Double-Real-Wert (Skalar)
    REAL (kind=double) , INTENT(IN)  :: rwert !
    !! Datenobjekt (Skalar)
    TYPE (t_time)   :: this ! 
    !! Hilfsvariable
    INTEGER        :: i_KompNr=1
    !
    this = real_to_time ( rwert, i_KompNr )
    !
  END FUNCTION real_days_to_time_0
  !
  !! Uebertraegt die Werte von (Double-)Real-Zahlen (Vektor), welche in Tagen
  !! vorliegen, in Time-Objekte (Vektor). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_days_to_time_1 ( rwert ) &
       RESULT( this )
    !! Double-Real-Feld (Vektor)
    REAL (kind=double) , INTENT(IN) :: rwert(:) ! 
    !! Datenobjekte (Vektor)
    TYPE (t_time)   :: this(SIZE(rwert)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(rwert)
       this(i) = real_days_to_time_0 ( rwert(i) )
    END DO
    !
  END FUNCTION real_days_to_time_1
  !
  !! Uebertraegt den Wert einer (Double-)Real-Zahl (Skalar), welcher in Stunden
  !! vorliegt, in ein Time-Objekt (Skalar). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_hours_to_time_0 ( rwert ) &
       RESULT( this )
    !! Double-Real-Wert (Skalar)
    REAL (kind=double) , INTENT(IN)  :: rwert !
    !! Datenobjekt (Skalar)
    TYPE (t_time)   :: this ! 
    !! Hilfsvariable
    INTEGER        :: i_KompNr=2
    !
    this = real_to_time ( rwert, i_KompNr )
    !
  END FUNCTION real_hours_to_time_0
  !
  !! Uebertraegt die Werte von (Double-)Real-Zahlen (Vektor), welche in Stunden
  !! vorliegen, in Time-Objekte (Vektor). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_hours_to_time_1 ( rwert ) &
         RESULT( this )
    !! Double-Real-Feld (Vektor)
    REAL (kind=double) , INTENT(IN) :: rwert(:) ! 
    !! Datenobjekte (Vektor)
    TYPE (t_time)   :: this(SIZE(rwert)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(rwert)
       this(i) = real_hours_to_time_0 ( rwert(i) )
    END DO
    !
  END FUNCTION real_hours_to_time_1
  !
  !! Uebertraegt den Wert einer (Double-)Real-Zahl (Skalar), welcher in Minuten
  !! vorliegt, in ein Time-Objekt (Skalar). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_minutes_to_time_0 ( rwert ) &
         RESULT( this )
    !! Double-Real-Wert (Skalar)
    REAL (kind=double) , INTENT(IN)  :: rwert !
    !! Datenobjekt (Skalar)
    TYPE (t_time)   :: this ! 
    !! Hilfsvariable
    INTEGER        :: i_KompNr=3
    !
    this = real_to_time ( rwert, i_KompNr )
    !
  END FUNCTION real_minutes_to_time_0
  !
  !! Uebertraegt die Werte von (Double-)Real-Zahlen (Vektor), welche in Minuten
  !! vorliegen, in Time-Objekte (Vektor). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_minutes_to_time_1 ( rwert ) &
         RESULT( this )
    !! Double-Real-Feld (Vektor)
    REAL (kind=double) , INTENT(IN) :: rwert(:) ! 
    !! Datenobjekte (Vektor)
    TYPE (t_time)   :: this(SIZE(rwert)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(rwert)
       this(i) = real_minutes_to_time_0 ( rwert(i) )
    END DO
    !
  END FUNCTION real_minutes_to_time_1
  !
  !! Uebertraegt den Wert einer (Double-)Real-Zahl (Skalar), welcher in Sekunden
  !! vorliegt, in ein Time-Objekt (Skalar). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_seconds_to_time_0 ( rwert ) &
         RESULT( this )
    !! Double-Real-Wert (Skalar)
    REAL (kind=double) , INTENT(IN)  :: rwert !
    !! Datenobjekt (Skalar)
    TYPE (t_time)   :: this ! 
    !! Hilfsvariable
    INTEGER        :: i_KompNr=4
    !
    this = real_to_time ( rwert, i_KompNr )
    !
  END FUNCTION real_seconds_to_time_0
  !
  !! Uebertraegt die Werte von (Double-)Real-Zahlen (Vektor), welche in Sekunden
  !! vorliegen, in Time-Objekte (Vektor). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_seconds_to_time_1 ( rwert ) &
         RESULT( this )
    !! Double-Real-Feld (Vektor)
    REAL (kind=double) , INTENT(IN) :: rwert(:) ! 
    !! Datenobjekte (Vektor)
    TYPE (t_time)   :: this(SIZE(rwert)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i = 1, SIZE(rwert)
       this(i) = real_seconds_to_time_0 ( rwert(i) )
    END DO
    !
  END FUNCTION real_seconds_to_time_1
  !
  !! Gibt den Zeitinkrement-Wert eines CHARACTER-Strings
  !! in einem Datenobjekt (Skalar) zur&uuml;ck. <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION string_to_time_0 ( instr )              &
       RESULT( this )
    !! Zeitinkrement-String
    CHARACTER (LEN=*) , INTENT(IN) :: instr
    !! Datenobjekt (Skalar)
    TYPE (t_time)     :: this ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='string_to_time_0'
    !! 1. Zeichen des Inputstrings
    CHARACTER (LEN=1)  :: first_sign
    !! Inputstring ohne Vorzeichen
    CHARACTER (LEN=25) :: instr_ovz
    !! Zeitinkrement-String ohne Vorzeichen
    CHARACTER (LEN=25) :: increment
    !! Formatfehler in Eingabestring T/F
    LOGICAL            :: l_formfehler
    !! Position erstes Auftreten eines Bindestriches im Input-String
    INTEGER            :: hyphen_pos
    !! Position letztes Auftreten eines Punktes im Input-String
    INTEGER            :: point_pos
    !
    INTEGER :: hms_start, hms_end
    INTEGER :: nano_len
    CHARACTER (LEN=5) :: c_instr_len
    INTEGER :: i_len, i_start
    !! IO-Statusvariable
    INTEGER :: ios
    INTEGER :: ivz, dd, hh, mm, sec, nan
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Initialisierungen
       !
       increment    = '000000-00:00:00.000000000'
       l_formfehler = .false.
       !
       IF ( LEN_TRIM(instr) .EQ. 0 ) THEN
          !       
          ! Fehler -10 : String enthaelt nur Leerzeichen !
          !
          CALL setup_error_act ( all_errors(:), -10, c_upname, c_modname )
          !
       END IF
       !
       !
       ! Untersuchung des 1. Zeichens : Vorzeichen ivz ermitteln
       !
       IF ( no_error( ) ) THEN
          !
          first_sign = instr(1:1)
          !
          SELECT CASE ( first_sign )
          !
          CASE('0','1','2','3','4','5','6','7','8','9')
             !
             i_start = 1
             ivz     = 1
             !
          CASE DEFAULT
             !
             i_start = 2
             !
             IF ( first_sign == '-') THEN
                ivz = -1
             ELSE IF ( first_sign == '+') THEN
                ivz = 1
             ELSE 
                !
                ! Fehler -20 : Ungueltiges 1. Zeichen !
                !
                CALL setup_error_act ( all_errors(:), -20, c_upname, c_modname )
                CALL setup_error_act ( '<string>', TRIM(instr) )
                !
             END IF
             ! 
          END SELECT
          !
       END IF ! no_error( )
       !        
       !
       IF ( no_error( ) ) THEN
          !
          i_len = MIN( LEN_TRIM(instr), i_start+24 )
          !
          instr_ovz  = instr(i_start:i_len) 
          !
          hyphen_pos = INDEX(instr_ovz,'-')              ! Erste  '-'-Position
          point_pos  = INDEX(instr_ovz,'.',BACK=.TRUE.)  ! Letzte '.'-Position
          !
          !
          IF ( LEN_TRIM(instr(i_start:)) .GT. 25 ) THEN
             !       
             ! Fehler -30 : Zeitinkrement-String (ohne VZ) laenger als 25 Zeichen !
             !
             WRITE(c_instr_len,'(I5)') LEN_TRIM(instr(i_start:))
             !
             CALL setup_error_act ( all_errors(:), -30, c_upname, c_modname )
             CALL setup_error_act ( '<string>', TRIM(instr) )
             CALL setup_error_act ( '<instr_len>', c_instr_len )
             !
          END IF
          !
       END IF ! no_error( )
       !        
       IF ( no_error( ) ) THEN
          !
          ! Tage ermitteln
          !
          IF ( hyphen_pos .GT. 1 ) THEN
             !
             IF ( hyphen_pos .LE. 7 ) THEN
                increment(8-hyphen_pos:6) = instr_ovz(1:hyphen_pos-1)
             ELSE
                ! 
                ! Formatfehler : Tagesangabe mehr als 6 Zeichen !
                !
                l_formfehler = .true.
                !
             END IF
             !
          END IF
          !
          ! Nanosekunden ermitteln
          !
          IF ( point_pos .GT. 0 .AND. point_pos .LT. LEN_TRIM(instr_ovz) ) THEN
             !
             nano_len = LEN_TRIM(instr_ovz) - point_pos
             !
             IF ( nano_len .LE. 9 ) THEN
                increment(17:16+nano_len) = instr_ovz(point_pos+1:point_pos+nano_len)
             ELSE
                ! 
                ! Formatfehler : Nanosekunden mehr als 9 Zeichen !
                !
                l_formfehler = .true.
                !
             END IF 
             !
          END IF
          !
          ! Stunden, Minuten Sekunden ermitteln
          !
          hms_start  = hyphen_pos + 1
          IF ( point_pos .EQ. 0 ) THEN
             !
             hms_end = LEN_TRIM(instr_ovz)
             !
          ELSE
             !
             hms_end = point_pos - 1
             !
          END IF
          !
          IF ( (hms_end-hms_start) .EQ. 7 ) THEN
             ! 
             increment(8:9)   = instr_ovz(hms_start:hms_start+1)
             increment(11:12) = instr_ovz(hms_start+3:hms_start+4)
             increment(14:15) = instr_ovz(hms_start+6:hms_start+7)
          ELSE
             ! 
             ! Formatfehler : Mittelteil besteht nicht aus 8 Zeichen 00:00:00
             !
             l_formfehler = .true.
             !
          END IF
          !
          IF ( l_formfehler ) THEN
             !
             ! Fehler -40 : Zeitstring hat ungueltiges Format
             !
             CALL setup_error_act ( all_errors(:), -40, c_upname, c_modname )
             CALL setup_error_act ( '<string>', TRIM(instr_ovz) )
             !
          END IF
          !
       END IF ! no_error( )
       !
       IF ( no_error( ) ) THEN
          !
          ! Komponenten des Zeitinkrementstrings lesen
          !
          READ ( increment, '(i6,1x,3(I2,1x),I9)', IOSTAT=ios ) &
              dd, hh, mm, sec, nan
          !
          IF ( ios .NE. 0 ) THEN
             !
             ! Fehler -50 : Fehler beim Lesen des Zeitinkrements
             !
             CALL setup_error_act ( all_errors(:), -50, c_upname, c_modname, ios )
             CALL setup_error_act ( '<string>', increment )
             !
          END IF
          !
       END IF ! no_error( )
       !        
       !        
       ! t_time-Objekt anlegen und Komponenten setzen
       !
       IF ( no_error( ) ) CALL new_time ( this )
       IF ( no_error( ) ) CALL set_time ( this, ivz, dd, hh, mm, sec, nan )
       !
    END IF  ! ok_initialised
    !
  END FUNCTION string_to_time_0
  !
  !! Gibt die in einem Feld von Strings vorliegenden Zeitinkrement-Werte
  !! in einem Feld von Datenobjekten (Vektor) zur&uuml;ck. <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION string_to_time_1 ( instr )              &
       RESULT( this )
    !! Stringfeld
    CHARACTER (LEN=*) , INTENT(IN) :: instr(:)
    !! Datenobjekt (Vektor)
    TYPE (t_time)      :: this(SIZE(instr)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i=1,SIZE(instr)
       this(i) = string_to_time_0( instr(i) )
    END DO
    !
  END FUNCTION string_to_time_1
  !
  !! Gibt den Wert eines Datenobjektes (Skalar) als CHARACTER-String
  !! zur&uuml;ck. <BR>
  !! Format: +dddddd-hh:mm:ss.nnnnnnnnn<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_string_26_0 ( this )              &
       RESULT( outstr )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! String
    CHARACTER (LEN=26) :: outstr
    !! Vorzeichen
    CHARACTER (LEN=1)  :: csign
    !
    csign = '+'
    IF (this%sign < 0) csign = '-'
    !
    WRITE ( outstr, '(A1,I6.6,A1,3(I2.2,A1),I9.9)' ) &
       csign, this%days, "-", this%hours, ":", this%minutes, ":", &
       this%seconds,".",this%nanos
    !
  END FUNCTION time_to_string_26_0
  !
  !! Gibt die Werte von Datenobjekten (Vektor) als Strings
  !! in einem CHARACTER-Feld zur&uuml;ck. <BR>
  !! Format: +dddddd-hh:mm:ss.nnnnnnnnn<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_string_26_1 ( this )              &
       RESULT( outstr )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Stringfeld
    CHARACTER (LEN=26) :: outstr(SIZE(this))
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i=1,SIZE(this)
       outstr(i) = time_to_string_26_0( this(i) )
    END DO
    !
  END FUNCTION time_to_string_26_1
  !! Gibt den Wert eines Datenobjektes (Skalar) als CHARACTER-String
  !! zur&uuml;ck. <BR>
  !! Format: +dddddd-hh:mm:ss<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_string_16_0 ( this )              &
       RESULT( outstr )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! String
    CHARACTER (LEN=16) :: outstr
    !! Vorzeichen
    CHARACTER (LEN=1)  :: csign
    !
    csign = '+'
    IF (this%sign < 0) csign = '-'
    !
    WRITE ( outstr, '(A1,I6.6,3(A1,I2.2))' ) &
       csign, this%days, "-", this%hours, ":", this%minutes, ":", &
       this%seconds
    !
  END FUNCTION time_to_string_16_0
  !
  !! Gibt die Werte von Datenobjekten (Vektor) als Strings
  !! in einem CHARACTER-Feld zur&uuml;ck. <BR>
  !! Format: +dddddd-hh:mm:ss<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_string_16_1 ( this )              &
       RESULT( outstr )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Stringfeld
    CHARACTER (LEN=16) :: outstr(SIZE(this))
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i=1,SIZE(this)
       outstr(i) = time_to_string_16_0( this(i) )
    END DO
    !
  END FUNCTION time_to_string_16_1
  !! Gibt den Wert eines Datenobjektes (Skalar) als CHARACTER-String
  !! zur&uuml;ck. <BR>
  !! Format: +hh:mm:ss.nnnnnnnnn<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_string_19_0 ( this )              &
       RESULT( outstr )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! String
    CHARACTER (LEN=19) :: outstr
    !! Vorzeichen
    CHARACTER (LEN=1)  :: csign
    !
    csign = '+'
    IF (this%sign < 0) csign = '-'
    !
    WRITE ( outstr, '(A1,3(I2.2,A1),I9.9)' ) &
       csign, this%hours, ":", this%minutes, ":", &
       this%seconds,".",this%nanos
    !
  END FUNCTION time_to_string_19_0
  !
  !! Gibt die Werte von Datenobjekten (Vektor) als Strings
  !! in einem CHARACTER-Feld zur&uuml;ck. <BR>
  !! Format: +hh:mm:ss.nnnnnnnnn<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_string_19_1 ( this )              &
       RESULT( outstr )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Stringfeld
    CHARACTER (LEN=19) :: outstr(SIZE(this))
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i=1,SIZE(this)
       outstr(i) = time_to_string_19_0( this(i) )
    END DO
    !
  END FUNCTION time_to_string_19_1
  !! Gibt den Wert eines Datenobjektes (Skalar) als CHARACTER-String
  !! zur&uuml;ck. <BR>
  !! Format: +hh:mm:ss<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_string_9_0 ( this )              &
       RESULT( outstr )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! String
    CHARACTER (LEN=9) :: outstr
    !! Vorzeichen
    CHARACTER (LEN=1)  :: csign
    !
    csign = '+'
    IF (this%sign < 0) csign = '-'
    !
    WRITE ( outstr, '(3(A1,I2.2))' ) &
       csign, this%hours, ":", this%minutes, ":", &
       this%seconds
    !
  END FUNCTION time_to_string_9_0
  !
  !! Gibt die Werte von Datenobjekten (Vektor) als Strings
  !! in einem CHARACTER-Feld zur&uuml;ck. <BR>
  !! Format: +hh:mm:ss<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_string_9_1 ( this )              &
       RESULT( outstr )
    !! Datenobjekt (Vektor)
    TYPE (t_time) , INTENT(IN) :: this(:) ! 
    !! Stringfeld
    CHARACTER (LEN=9) :: outstr(SIZE(this))
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i=1,SIZE(this)
       outstr(i) = time_to_string_9_0( this(i) )
    END DO
    !
  END FUNCTION time_to_string_9_1
  !
  !! Pr&uuml;ft, ob ein Time-Objekt 1 ein ganzzahliges Vielfaches von
  !! Time-Objekt 2 ist. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_integer_multiple_0_0 ( this1, this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !
    IF ( (this1 .DIV. this2)*this2 == this1 ) THEN
       ok = .true.
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION is_integer_multiple_0_0
  !
  !! Berechnet den Absolutwert eines Objektes "t_time"<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION abs_time_0_0 ( this1 ) &
       RESULT( this )
    !! Objekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Absolutwert (Skalar)
    TYPE (t_time) :: this ! 
    !
    IF ( get_time_sign(this1) >= 0 ) THEN
       this = this1
    ELSE
       this = -this1
    END IF
    !
  END FUNCTION abs_time_0_0
  !
  !! Berechnet das zum  Time-Objekt 1 betragsmaessig naechstgelegene ganzzahlige Vielfache
  !! von Time-Objekt 2, mit Vorzeichen von Time-Objekt 2. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nearest_int_multiple_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_time) , INTENT(IN) :: this2 ! 
    !! Berechnungsergebnis (Skalar)
    TYPE (t_time)  :: this ! 
    ! Lokale Parameter und Variablen
    !! Hilfsobjekt vom Typ "t_time"
    TYPE (t_time) :: abs_t1, abs_t2
    !
    IF ( this1 == this2 ) THEN
       this = this1
    ELSE
       !
       abs_t1 = abs_time ( this1 )
       abs_t2 = abs_time ( this2 )
       !
       IF ( abs_t1 < abs_t2 ) THEN
          this = get_time_sign(this2) * abs_t2
       ELSE IF ( abs_t1-(abs_t1 .DIV. abs_t2)*abs_t2 <= ((abs_t1 .DIV. abs_t2)+1)*abs_t2-abs_t1 ) THEN
          this = get_time_sign(this2) * (abs_t1 .DIV. abs_t2)*abs_t2
       ELSE
          this = get_time_sign(this2) * ((abs_t1 .DIV. abs_t2)+1)*abs_t2
       END IF
       !
    END IF
    !
  END FUNCTION get_nearest_int_multiple_0_0
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
       !
       WRITE(*,*) ' *** Warnung *** Modul "b_time" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_time ausfuehren'
       !
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
       !
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
  SUBROUTINE init_time_all_errors ( )
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
               '--> INIT_time ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_time ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_time"\n'//&
               'Typ-Komponente = "sign"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_time"\n'//&
               'Typ-Komponente = "days"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_time"\n'//&
               'Typ-Komponente = "hours"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_time"\n'//&
               'Typ-Komponente = "minutes"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_time"\n'//&
               'Typ-Komponente = "seconds"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_time"\n'//&
               'Typ-Komponente = "nanos"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_time" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_time" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_time" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_time"\n'//&
               '--> Code in Modul "b_time" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_time"\n'//&
               '--> Code in Modul "b_time" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -10 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
               'Zeitinkrement-String besteht lediglich aus Leerzeichen !\n'//&
               '\n'//&
               '--> Aktualparameter in UP-Aufruf pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -20 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
               'Erstes Zeichen des Zeitinkrement-Strings ist ungueltig !\n'//&
               'Inkrement-String : <string>\n'//&
               'erlaubt fuer 1. Zeichen : +,-,0,1,2,3,4,5,6,7,8,9\n'//&
               '--> Aktualparameter in UP-Aufruf pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
               'Zeitinkrement-String ist zu lang !\n'//&
               'Inkrement-String : <string>\n'//&
               'aktuelle Laenge  = <instr_len> Zeichen\n'//&
               'maximale Laenge  =    25 Zeichen + ggf. Vorzeichen\n'//&
               '\n'//&
               '--> Aktualparameter in UP-Aufruf pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -40 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
               'Zeitinkrement-String hat ungueltiges Format !\n'//&
               'Inkrement-String : <string>\n'//&
               '\n'//&
               '--> Aktualparameter in UP-Aufruf pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -50 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
               'Fehler beim Lesen des Zeitinkrements !\n'//&
               'Inkrement-String : <string>\n'//&
               '\n'//&
               '--> Aktualparameter/Programm pruefen' )
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
  END SUBROUTINE init_time_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_time_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_time_all_errors
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
  ! --> nicht benoetigte ALLOC-Routinen bitte unbedingt loeschen <--------
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte INIT-Routinen bitte unbedingt loeschen <---------
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte DEALLOC-Routinen bitte unbedingt loeschen <------
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "sign" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_time_sign ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_time_sign' ! 
    !
    ok = ( this%sign == 1 .OR. this%sign == -1 )
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
  END FUNCTION ok_time_sign
  !
  !! Pr&uuml;fe, ob die Komponente "days" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_time_days ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_time_days' ! 
    !
    ok = ( this%days >= 0 )
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
  END FUNCTION ok_time_days
  !
  !! Pr&uuml;fe, ob die Komponente "hours" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_time_hours ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_time_hours' ! 
    !
    ok = ( this%hours >= 0 )
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    !
  END FUNCTION ok_time_hours
  !
  !! Pr&uuml;fe, ob die Komponente "minutes" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_time_minutes ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_time_minutes' ! 
    !
    ok = ( this%minutes >= 0 )
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    !
  END FUNCTION ok_time_minutes
  !
  !! Pr&uuml;fe, ob die Komponente "seconds" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_time_seconds ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_time_seconds' ! 
    !
    ok = ( this%seconds >= 0 )
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
    !
  END FUNCTION ok_time_seconds
  !
  !! Pr&uuml;fe, ob die Komponente "nanos" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_time_nanos ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_time) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_time_nanos' ! 
    !
    ok = ( this%nanos >= 0 )
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
    !
  END FUNCTION ok_time_nanos
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
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
  ! >>> PRIVATE-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE modul-spezifische Methoden <<< [ERR_NO < 0 ]
  ! ----------------------------------------------------------------------
  !
  !! Rundet den Wert eines Time-Objektes (Skalar) hinsichtlich der
  !! durch i_bezug bestimmten Komponente.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_time_to_komponent ( this1, i_bezug ) &
       RESULT( this )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN) :: this1 ! 
    !! Bezugskomponente
    INTEGER       , INTENT(INOUT) :: i_bezug
    !! gerundetes Datenobjekt (Skalar)
    TYPE (t_time)  :: this !
    !
    CALL time_to_carry ( this1, carry1 )
    !
    CALL rnd_carry ( carry1, i_bezug, rcarry ) ! >GL> new rnd
    !
    this   = carry_to_time ( rcarry )
    !
  END FUNCTION round_time_to_komponent
  !
  !! Ermittelt den Wert eines Time-Objektes hinsichtlich der durch i_bezug 
  !! bestimmten Zeiteinheit und schreibt diesen in eine (Double-)Real-Zahl.<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION time_to_real ( this, i_bezug ) &
         RESULT( rwert )
    !! Datenobjekt (Skalar)
    TYPE (t_time) , INTENT(IN)    :: this ! 
    !! Bezugskomponente
    INTEGER       , INTENT(INOUT) :: i_bezug
    !! Double-Realwert
    REAL (kind=double)  :: rwert !
    !
    CALL time_to_carry ( this, carry1 )
    !
    rwert = carry_to_real ( carry1, i_bezug )
    !
  END FUNCTION time_to_real
  !
  !! Uebertraegt den Wert einer (Double-)Real-Zahl, welcher in der durch
  !! i_bezug bestimmten Zeiteinheit vorliegt, in ein Time-Objekt. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_to_time ( rwert, i_bezug ) &
       RESULT( this )
    !! Double-Realwert
    REAL (kind=double), INTENT(IN)  :: rwert !
    !! Bezugskomponente
    INTEGER       , INTENT(INOUT)   :: i_bezug
    !! Datenobjekt (Skalar)
    TYPE (t_time)   :: this ! 
    !
    CALL double_to_carry ( rwert, i_bezug, time_limits, carry1 ) ! >GL> new double
    !
    this   = carry_to_time ( carry1 )
    !
  END FUNCTION real_to_time
  !
  !! Uebertraegt den Wert einer t_time-Variablen in eine Variable
  !! vom Typ t_carry.
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE time_to_carry ( this , carry )
    !! Objekt (Skalar)
    TYPE (t_time)  , INTENT(IN) :: this ! 
    !! Umwandlungsergebnis
    TYPE (t_carry) , INTENT(INOUT) :: carry
    !! Hilfsfeld
    INTEGER (KIND=long), DIMENSION(nkomp_tinc)  :: lfeld
    !
    lfeld(1) = this%days    * this%sign
    lfeld(2) = this%hours   * this%sign
    lfeld(3) = this%minutes * this%sign
    lfeld(4) = this%seconds * this%sign
    lfeld(5) = 0
    lfeld(6) = this%nanos   * this%sign
    !
    CALL set_carry_values ( carry, lfeld )
    !
  END SUBROUTINE time_to_carry
  !
  !! Uebertraegt den Wert einer t_carry-Variablen in eine Variable
  !! vom Typ t_time.
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION carry_to_time ( carry ) &
         RESULT( this )
    !! Wert im Uebertragsrechner
    TYPE (t_carry) , INTENT(IN) :: carry
    !! Umwandlungsergebnis
    TYPE (t_time) :: this
    !! Hilfsfeld
    INTEGER (KIND=long), DIMENSION(nkomp_tinc) :: lfeld
    !
    lfeld        = get_carry_values ( carry )
    !
    this%sign    = get_carry_sign ( carry )
    !
    this%days    = ABS( lfeld(1) )
    this%hours   = ABS( lfeld(2) )
    this%minutes = ABS( lfeld(3) )
    this%seconds = ABS( lfeld(4) )
    this%nanos   = ABS( lfeld(6) ) + ABS( lfeld(5) ) * time_limits(6)
    !
  END FUNCTION carry_to_time
  !
END MODULE b_time
! TailOfBaseModule --------------------------------------------------------
