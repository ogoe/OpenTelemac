! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Datumsrechnung</h2>
!! @author <A HREF="mailto:seiss@hamburg.baw.de">Guntram Sei&szlig;</A>
!! @version 1.9 vom 02/23/05, Quellcode: mod_b_date.f90
!! <HR>
!! defines type "t"date" and provides a few methods for date calculations <BR>
!  Copyright-Hinweis
!
!  Copyright (C) 2002 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-04-30 : Andreas Malcherek, G&uuml;nther Lang : Version aus "mod_date_class.f90"
!  01.02 : 2002-05-28 : G&uuml;nther Lang : Korrektur in MERGE fuer trc_op
!  01.03 : 2002-06-05 : G&uuml;nther Lang : get_date_as_xxl_string (internal), ok_initialised modifiziert
!  01.04 : 2002-06-12 : Peter Schade : Funktionsnamen veraendert und c_def_language = 1 (Deutsch)
!  01.05 : 2002-06-12 : G&uuml;nther Lang : INIT/CLEAR sowie SETUP_date_PRN_LUN, SETUP_date_TRC_LUN angepasst
!  01.06 : 2003-03-04 : G&uuml;nther Lang : Anpassungen TV12 vom Dez. 2002
!  01.07 : 2003-03-04 : G&uuml;nther Lang : Copyright-Hinweis in INIT_DATE
!
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Definiert den &ouml;ffentlichen Typ "t_date" und stellt verschiedene <BR>
!! Methoden zur Datumsrechnung bereit.                              <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_date <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> day   : Tag                                       
!!     <LI> month : Monat
!!     <LI> year  : Jahr
!! </OL>
!!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_date mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_date mit CLEAR-Methode.
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
!! 06010 = Fehler in Komponente day des Typs t_date                 <BR>
!! 06020 = Fehler in Komponente month des Typs t_date               <BR>
!! 06030 = Fehler in Komponente year des Typs t_date                <BR>
!! 06040 = Tag kommt wegen Kalenderumstellung (Julianisch->Gregorianisch) nicht vor  <BR>
!! <HR>
!! PRINT-Methoden        [  7000 bis  7999 ]                        <BR>
!! 07001 = Drucken der Kopfzeilen                                   <BR>
!! 07002 = Drucken der Fu&szlig;zeilen                              <BR>
!! 07003 = Drucken des Index des Datenobjektes (1D-Array)           <BR>
!! 07010 = Drucken der Komponente day des Typs t_date               <BR>
!! 07020 = Drucken der Komponente month des Typs t_date             <BR>
!! 07030 = Drucken der Komponente year des Typs t_date              <BR>
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
!! modul-spezifische Methoden   [      < 0 ]                        <BR>
!
MODULE b_date
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
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
       set_error_cerr,      &
       clear_error_act,     &
       print_error_act
  !
  ! [A.3] weitere BASIS-Module (ONLY benutzen!)
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
  !! Datumsangabe  <BR>
  !! day   : Tag   <BR> 
  !! month : Monat <BR>
  !! year  : Jahr 
  TYPE , PUBLIC :: t_date
     PRIVATE
     INTEGER :: day   ! 
     INTEGER :: month ! 
     INTEGER :: year  ! 
  END TYPE t_date
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
  !! ggf. Allokieren der statischen Daten des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_date
     MODULE PROCEDURE init_date_d ! 
  END INTERFACE
  !! ggf. De-Allokieren der statischen Daten des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_date
     MODULE PROCEDURE clear_date_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_date_prn_lun
     MODULE PROCEDURE setup_date_prn_lun_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_date_trc_lun
     MODULE PROCEDURE setup_date_trc_lun_d ! 
  END INTERFACE
  !! Setzen der Sprache f&uuml;r einige GET-Methoden (z.B. Wochentag als String): <BR>
  !! 1 = Deutsch (Default); <BR>
  !! 2 = Englisch. 
  INTERFACE setup_date_language
     MODULE PROCEDURE setup_date_language_d ! 
  END INTERFACE
  !! Neues Objekt vom Typ "t_date" erzeugen; <BR>
  !! Initialisieren mit Default-Werten.
  INTERFACE new_date
     MODULE PROCEDURE new_date_0  ! Version fuer Skalar
     MODULE PROCEDURE new_date_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_date" vernichten; <BR>
  !! Re-Initialisieren mit Default-Werten.
  INTERFACE kill_date
     MODULE PROCEDURE kill_date_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_date_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_date" auf G&uuml;ltigkeit pr&uuml;fen.
  INTERFACE ok_date
     MODULE PROCEDURE ok_date_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_date_1 ! Version fuer 1D-Array
  END INTERFACE
  !! alle Komponenten des Typs "t_date" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_date
     MODULE PROCEDURE print_date_0 ! Version fuer Skalar
     MODULE PROCEDURE print_date_1 ! Version fuer 1D-Array
  END INTERFACE
  !! alle statischen Daten des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_date_static
     MODULE PROCEDURE print_date_static_d ! 
  END INTERFACE
  !! Alle Fehlermeldungen des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_date_all_errors
     MODULE PROCEDURE print_date_all_errors_d ! 
  END INTERFACE
  !
  ! ----------------------------------------------------------------
  ! --> nicht benoetigte SET-Interfaces bitte unbedingt loeschen <--
  ! ----------------------------------------------------------------
  !
  !! Setze Komponente "day" in "t_date" auf Benutzerwert <BR>
  !! a) Wert in einem skalaren Objekt setzen <BR>
  !! b) Wert in einem 1D-Vektor Objekt setzen <BR>
  !! c) Werte in einem 1D-Vektor Objekt setzen 
  INTERFACE set_date_day
     MODULE PROCEDURE set_date_day_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_date_day_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_date_day_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "month" in "t_date" auf Benutzerwert <BR>
  !! a) Wert in einem skalaren Objekt setzen <BR>
  !! b) Wert in einem 1D-Vektor Objekt setzen <BR>
  !! c) Werte in einem 1D-Vektor Objekt setzen 
  INTERFACE set_date_month
     MODULE PROCEDURE set_date_month_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_date_month_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_date_month_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "year" in "t_date" auf Benutzerwert <BR>
  !! a) Wert in einem skalaren Objekt setzen <BR>
  !! b) Wert in einem 1D-Vektor Objekt setzen <BR>
  !! c) Werte in einem 1D-Vektor Objekt setzen 
  INTERFACE set_date_year
     MODULE PROCEDURE set_date_year_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_date_year_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_date_year_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Hole Komponente "day" aus "t_date" <BR>
  !! a) Wert aus einem skalaren Objekt holen <BR>
  !! b) Werte (1D-Vektor) aus einem 1D-Vektor Objekt holen
  INTERFACE get_date_day
     MODULE PROCEDURE get_date_day_0_0 ! Skalar
     MODULE PROCEDURE get_date_day_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "month" aus "t_date" <BR>
  !! a) Wert aus einem skalaren Objekt holen <BR>
  !! b) Werte (1D-Vektor) aus einem 1D-Vektor Objekt holen
  INTERFACE get_date_month
     MODULE PROCEDURE get_date_month_0_0 ! Skalar
     MODULE PROCEDURE get_date_month_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "year" aus "t_date" <BR>
  !! a) Wert aus einem skalaren Objekt holen <BR>
  !! b) Werte (1D-Vektor) aus einem 1D-Vektor Objekt holen
  INTERFACE get_date_year
     MODULE PROCEDURE get_date_year_0_0 ! Skalar
     MODULE PROCEDURE get_date_year_1_0 ! Vektor
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Setzen aller Komponenten eines Objekts vom Typ "t_date" <BR>
  !! a) Werte in einem skalaren Objekt setzen <BR>
  !! b) Werte in einem 1D-Vektor Objekt setzen <BR>
  !! c) Werte in einem 1D-Vektor Objekt setzen 
  INTERFACE set_date
     MODULE PROCEDURE set_date_0_0
     MODULE PROCEDURE set_date_1_0
     MODULE PROCEDURE set_date_1_1
  END INTERFACE
  !! Anzahl der Tage im Monat einer Datumsangabe ermitteln <BR>
  !! a) Anzahl f&uuml;r ein skalares Objekt ermitteln <BR>
  !! b) Anzahl f&uuml;r einen 1D-Vektor Objekt ermitteln 
  INTERFACE get_date_length_of_month
     MODULE PROCEDURE get_date_length_of_month_0 ! Skalar
     MODULE PROCEDURE get_date_length_of_month_1 ! Vektor
  END INTERFACE
  !! den julianischen Tag einer Datumsangabe ermitteln <BR>
  !! a) Tag f&uuml;r ein skalares Objekt ermitteln <BR>
  !! b) Tage f&uuml;r einen 1D-Vektor Objekt ermitteln 
  INTERFACE date_to_julian_day
     MODULE PROCEDURE date_to_julian_day_0 ! Skalar
     MODULE PROCEDURE date_to_julian_day_1 ! Vektor
  END INTERFACE
  !! Datumsangabe aus dem julianischen Tag ermitteln <BR>
  !! a) Datum f&uuml;r ein skalares Objekt ermitteln <BR>
  !! b) Datumsangaben f&uuml;r ein 1D-Vektor Objekt ermitteln 
  INTERFACE julian_day_to_date
     MODULE PROCEDURE julian_day_to_date_0 ! Skalar
     MODULE PROCEDURE julian_day_to_date_1 ! Vektor
  END INTERFACE
  !! Datumsangabe in einen String wandeln <BR>
  !! language = 1 : R&uuml;ckgabeformat TT.MM.JJJJ <BR>
  !! language = 2 : R&uuml;ckgabeformat MM/DD/YYYY <BR>
  !! a) String aus einem skalaren Objekt ermitteln <BR>
  !! b) String-Vektor f&uuml;r einen 1D-Vektor ermitteln 
  INTERFACE date_to_string
     MODULE PROCEDURE date_to_string_0 ! Skalar
     MODULE PROCEDURE date_to_string_1 ! Vektor
  END INTERFACE
  !! Wochentag (Kurzform) in einen String wandeln <BR>
  !! language = 1 : z.B. "So", ...; <BR>
  !! language = 2 : z.B. "Su", ...; <BR>
  !! a) String aus einem skalaren Objekt ermitteln <BR>
  !! b) String-Vektor f&uuml;r einen 1D-Vektor ermitteln 
  INTERFACE date_day_to_short_string
     MODULE PROCEDURE date_day_to_short_string_0 ! Skalar
     MODULE PROCEDURE date_day_to_short_string_1 ! Vektor
  END INTERFACE
  !! Wochentag (Langform) in einen String wandeln <BR>
  !! language = 1 : z.B. "Sonntag", ...; <BR>
  !! language = 2 : z.B. "Sunday", ...; <BR>
  !! a) String aus einem skalaren Objekt ermitteln <BR>
  !! b) String-Vektor f&uuml;r einen 1D-Vektor ermitteln 
  INTERFACE date_day_to_long_string
     MODULE PROCEDURE date_day_to_long_string_0 ! Skalar
     MODULE PROCEDURE date_day_to_long_string_1 ! Vektor
  END INTERFACE
  !! Monatsname (Kurzform) in einen String wandeln <BR>
  !! language = 1 : z.B. "Mrz", ...; <BR>
  !! language = 2 : z.B. "Mar", ...; <BR>
  !! a) String aus einem skalaren Objekt ermitteln <BR>
  !! b) String-Vektor f&uuml;r einen 1D-Vektor ermitteln 
  INTERFACE date_month_to_short_string
     MODULE PROCEDURE date_month_to_short_string_0 ! Skalar
     MODULE PROCEDURE date_month_to_short_string_1 ! Vektor
  END INTERFACE
  !! Monatsname (Langform) in einen String wandeln <BR>
  !! language = 1 : z.B. "Maerz", ...; <BR>
  !! language = 2 : z.B. "March", ...; <BR>
  !! a) String aus einem skalaren Objekt ermitteln <BR>
  !! b) String-Vektor f&uuml;r einen 1D-Vektor ermitteln 
  INTERFACE date_month_to_long_string
     MODULE PROCEDURE date_month_to_long_string_0 ! Skalar
     MODULE PROCEDURE date_month_to_long_string_1 ! Vektor
  END INTERFACE
  !! Feststellen ob eine Datumsangabe in einem Schaltjahr liegt <BR>
  !! a) f&uuml;r eine Datumsangabe <BR>
  !! b) f&uuml;r einen 1D-Vektor mit Datumsangaben
  INTERFACE is_leap_year
     MODULE PROCEDURE is_leap_year_0 ! Skalar
     MODULE PROCEDURE is_leap_year_1 ! Vektor
  END INTERFACE
  !! Ermitteln des Datums mit Hilfe der Systemuhr <BR>
  !! a) f&uuml;r eine Datumsangabe
  INTERFACE get_date_from_system
     MODULE PROCEDURE get_date_from_system_0
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
  !! Pr&uuml;fen zweier Datenobjekte "t_date" auf Gleichheit <BR>
  !! zwei Datenobjekte sind dann gleich, wenn sie in allen Komponenten
  !! &uuml;bereinstimmen: <BR>
  !! a) date    == date    <BR>
  !! b) date    == date(:) <BR>
  !! c) date(:) == date    <BR>
  !! d) date(:) == date(:) 
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_date_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_date_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_date_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_date_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Vergleich "&#062;" zweier Datenobjekte "t_date" <BR>
  !! a) date    &#062; date    <BR>
  !! b) date    &#062; date(:) <BR>
  !! c) date(:) &#062; date    <BR>
  !! d) date(:) &#062; date(:) 
  INTERFACE OPERATOR(>)
     MODULE PROCEDURE gt_date_0_0  ! Skalar / Skalar
     MODULE PROCEDURE gt_date_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE gt_date_1_0  ! Vektor / Skalar
     MODULE PROCEDURE gt_date_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#062;=" zweier Datenobjekte "t_date" <BR>
  !! a) date    &#062;= date    <BR>
  !! b) date    &#062;= date(:) <BR>
  !! c) date(:) &#062;= date    <BR>
  !! d) date(:) &#062;= date(:) 
  INTERFACE OPERATOR(>=)
     MODULE PROCEDURE ge_date_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ge_date_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ge_date_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ge_date_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#060;" zweier Datenobjekte "t_date" <BR>
  !! a) date    &#060; date    <BR>
  !! b) date    &#060; date(:) <BR>
  !! c) date(:) &#060; date    <BR>
  !! d) date(:) &#060; date(:) 
  INTERFACE OPERATOR(<) 
     MODULE PROCEDURE lt_date_0_0  ! Skalar / Skalar
     MODULE PROCEDURE lt_date_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE lt_date_1_0  ! Vektor / Skalar
     MODULE PROCEDURE lt_date_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#060;=" zweier Datenobjekte "t_date" <BR>
  !! a) date    &#060;= date    <BR>
  !! b) date    &#060;= date(:) <BR>
  !! c) date(:) &#060;= date    <BR>
  !! d) date(:) &#060;= date(:) 
  INTERFACE OPERATOR(<=)
     MODULE PROCEDURE le_date_0_0  ! Skalar / Skalar
     MODULE PROCEDURE le_date_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE le_date_1_0  ! Vektor / Skalar
     MODULE PROCEDURE le_date_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_date" auf Ungleichheit <BR>
  !! a) date    /= date    <BR>
  !! b) date    /= date(:) <BR>
  !! c) date(:) /= date    <BR>
  !! d) date(:) /= date(:) 
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_date_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_date_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_date_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_date_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Addition einer Anzahl von Tages (Integer) zu einem Datenobjekt "t_date" <BR>
  !! a) date          + julian_day    <BR>
  !! b) date(:)       + julian_day    <BR>
  !! c) date(:)       + julian_day(:) <BR>
  !! d) julian_day    + date          <BR>
  !! e) julian_day(:) + date          <BR>
  !! f) julian_day(:) + date(:)
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE get_add_days_to_date_0_0_oi ! Skalar/Skalar
     MODULE PROCEDURE get_add_days_to_date_1_0_oi ! Vektor/Skalar
     MODULE PROCEDURE get_add_days_to_date_1_1_oi ! Vektor/Vektor
     MODULE PROCEDURE get_add_days_to_date_0_0_io ! Skalar/Skalar
     MODULE PROCEDURE get_add_days_to_date_1_0_io ! Vektor/Skalar
     MODULE PROCEDURE get_add_days_to_date_1_1_io ! Vektor/Vektor
  END INTERFACE
  !! Subtraktion einer Integer-Zahl von einem Datenobjekt "t_date" <BR>
  !! a) date          - julian_day    <BR>
  !! b) date(:)       - julian_day    <BR>
  !! c) date(:)       - julian_day(:) <BR>
  !! Subtraktion zweier Datumsangeben (ergibt Differenz in Tagen) <BR>
  !! a) date1    - date2    <BR>
  !! b) date1    - date2(:) <BR>
  !! c) date1(:) - date2    <BR>
  !! d) date1(:) - date2(:)
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE get_sub_days_from_date_0_0_oi ! Skalar/Skalar
     MODULE PROCEDURE get_sub_days_from_date_1_0_oi ! Vektor/Skalar
     MODULE PROCEDURE get_sub_days_from_date_1_1_oi ! Vektor/Vektor
     MODULE PROCEDURE get_sub_days_from_date_0_0_oo ! Skalar/Skalar
     MODULE PROCEDURE get_sub_days_from_date_0_1_oo ! Skalar/Vektor
     MODULE PROCEDURE get_sub_days_from_date_1_0_oo ! Vektor/Skalar
     MODULE PROCEDURE get_sub_days_from_date_1_1_oo ! Vektor/Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_date                 ! Initialisieren (Modul)
  PUBLIC :: clear_date                ! De-Initialisieren (Modul)
  PUBLIC :: setup_date_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_date_trc_lun        ! Setzen trc_lun 
  PUBLIC :: setup_date_language       ! Setzen der Sprache
  PUBLIC :: new_date                  ! Erzeugen 
  PUBLIC :: kill_date                 ! Vernichten
  PUBLIC :: ok_date                   ! Pruefen
  PUBLIC :: print_date                ! Drucken
  PUBLIC :: print_date_static         ! Drucken aller statischen Daten
  PUBLIC :: print_date_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_date_day ! Setzen der Komponente day
  PUBLIC :: set_date_month ! Setzen der Komponente month
  PUBLIC :: set_date_year ! Setzen der Komponente year
  PUBLIC :: get_date_day ! Holen der Komponente day
  PUBLIC :: get_date_month ! Holen der Komponente month
  PUBLIC :: get_date_year ! Holen der Komponente year
  PUBLIC :: OPERATOR(==)              ! Operator "=="
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: OPERATOR(>)               ! Operator ">"
  PUBLIC :: OPERATOR(>=)              ! Operator ">="
  PUBLIC :: OPERATOR(<)               ! Operator "<"
  PUBLIC :: OPERATOR(<=)              ! Operator "<="
  PUBLIC :: OPERATOR(/=)              ! Operator "/="
  !                                   
  PUBLIC :: OPERATOR(+)               ! Operator "+"
  PUBLIC :: OPERATOR(-)               ! Operator "-"
  !                                   
  PUBLIC :: set_date                  ! 
  PUBLIC :: get_date_length_of_month  ! 
  PUBLIC :: date_to_julian_day        ! 
  PUBLIC :: julian_day_to_date        ! 
  PUBLIC :: date_to_string            ! 
  PUBLIC :: date_day_to_short_string  ! 
  PUBLIC :: date_day_to_long_string   ! 
  PUBLIC :: date_month_to_short_string! 
  PUBLIC :: date_month_to_long_string ! 
  PUBLIC :: is_leap_year              !
  PUBLIC :: get_date_from_system      ! 
  !
  ! Die mit !>WIN-NT gekennzeichneten Kommentare bitte nicht entfernen.
  ! Ohne sie geht nichts auf Windows-NT unter DR-Fortran.
!>WIN-NT:  PUBLIC :: eq_date_0_0, gt_date_0_0, ge_date_0_0, lt_date_0_0, &
!>WIN-NT:            le_date_0_0, ne_date_0_0
!>WIN-NT:  PUBLIC :: eq_date_0_1, gt_date_0_1, ge_date_0_1, lt_date_0_1, &
!>WIN-NT:            le_date_0_1, ne_date_0_1
!>WIN-NT:  PUBLIC :: eq_date_1_0, gt_date_1_0, ge_date_1_0, lt_date_1_0, &
!>WIN-NT:            le_date_1_0, ne_date_1_0
!>WIN-NT:  PUBLIC :: eq_date_1_1, gt_date_1_1, ge_date_1_1, lt_date_1_1, &
!>WIN-NT:            le_date_1_1, ne_date_1_1
!>WIN-NT:  PUBLIC :: get_add_days_to_date_0_0_oi, get_add_days_to_date_0_0_io
!>WIN-NT:  PUBLIC :: get_add_days_to_date_1_0_oi, get_add_days_to_date_1_0_io
!>WIN-NT:  PUBLIC :: get_add_days_to_date_1_1_oi, get_add_days_to_date_1_1_io
!>WIN-NT:  PUBLIC :: get_sub_days_from_date_0_0_oi, get_sub_days_from_date_0_0_oo
!>WIN-NT:  PUBLIC :: get_sub_days_from_date_1_0_oi, get_sub_days_from_date_1_0_oo
!>WIN-NT:  PUBLIC :: get_sub_days_from_date_0_1_oo
!>WIN-NT:  PUBLIC :: get_sub_days_from_date_1_1_oi, get_sub_days_from_date_1_1_oo
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=6) , PARAMETER :: c_modname      = 'b_date' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_date
  INTEGER           , PARAMETER :: c_nofcomp      = 3                ! ggf. modifizieren
  !! Anzahl der Fehlermeldungen
  INTEGER           , PARAMETER :: c_nofallerrors = 13               ! ggf. modifizieren
  !! Anzahl der verschiedenen Spracheinstellungen
  INTEGER           , PARAMETER :: c_max_language = 2                ! [Deutsch,Englisch]
  !! Default-Language
  INTEGER           , PARAMETER :: c_def_language = 1                ! [Deutsch]
  !! Anzahl der Wochentage
  INTEGER           , PARAMETER :: c_days_per_week= 7                ! 
  !! Anzahl der Monate im Jahr
  INTEGER           , PARAMETER :: c_months_per_year=12              ! 
  !! Kurzbezeichnungen der Wochentage
  CHARACTER (LEN=2) , PARAMETER :: c_days_short(c_days_per_week,c_max_language)= & ! 
       RESHAPE( (/ "So", "Mo", "Di", "Mi", "Do", "Fr", "Sa",    & ! 
                   "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa" /), &
                   SHAPE=(/c_days_per_week,c_max_language/) )     ! 
  !! lange Bezeichnungen der Wochentage
  CHARACTER (LEN=10) , PARAMETER :: c_days_long(c_days_per_week,c_max_language)= & ! 
       RESHAPE( (/ "Sonntag   ", "Montag    ", "Dienstag  ", "Mittwoch  ", "Donnerstag", "Freitag   ", "Samstag   ",    & !  
                   "Sunday    ", "Monday    ", "Tuesday   ", "Wednesday ", "Thursday  ", "Friday    ", "Saturday  " /), &  ! 
                   SHAPE=(/c_days_per_week,c_max_language/) )     ! 
  !! Kurzbezeichnungen der Monate
  CHARACTER (LEN=3) , PARAMETER :: c_months_short(c_months_per_year,c_max_language)= & ! 
       RESHAPE( (/ "Jan", "Feb", "Mrz", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez", & ! 
                   "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" /), & !
                   SHAPE=(/c_months_per_year,c_max_language/) )     ! 
  !! lange Bezeichnungen der Monate
  CHARACTER (LEN=9) , PARAMETER :: c_months_long(c_months_per_year,c_max_language)= & ! 
       RESHAPE( (/ "Januar   ", "Februar  ", "Maerz    ", "April    ", "Mai      ", "Juni     ", & !
                   "Juli     ", "August   ", "September", "Oktober  ", "November ", "Dezember ", & ! 
                   "January  ", "February ", "March    ", "April    ", "May      ", "June     ", & ! 
                   "July     ", "August   ", "September", "October  ", "November ", "December " /), & !
                   SHAPE=(/c_months_per_year,c_max_language/) )     ! 
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , SAVE :: all_errors(c_nofallerrors)! 
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
  !! aktuelle Sprache
  INTEGER                , SAVE :: language    = c_def_language ! [Englisch]
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                , SAVE :: n_init      = 0        ! 
  ! [D.4] Schnittstellen
  !
  !! Umwandeln einer Datumsangabe in einen String mit gr&ouml;sserer L&auml;nge
  INTERFACE get_date_as_xxl_string
     MODULE PROCEDURE get_date_as_xxl_string_0
     MODULE PROCEDURE get_date_as_xxl_string_1
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
  SUBROUTINE init_date_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_date_d' 
    !
    IF ( .NOT. initialised ) THEN
       !
       ! [1.1] Drucken des Copyright-Hinweises
       !
       IF (DEBUG_b  > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_date" version 1.9 of 02/23/05'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
          WRITE(*,*)
       END IF
       !
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       !
       ! [1.2.1] Error-Modul zuerst initialisieren
       !
       CALL init_error ( )
       !
       ! [1.2.2] ggf. weitere Module initialisieren
       !
       ! ... derzeit nicht erforderlich 
       !
       ! [1.3] vorlaeufiges Setzen von "initialised"
       !
       initialised = .true.
       !
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       !
       IF ( no_error( ) ) CALL init_date_all_errors ( ) 
       !
       ! [1.5] Initialisieren der logischen Kanalnummern
       !
       prn_lun = c_lun 
       trc_lun = c_lun 
       !
       ! [1.6] ggf. weitere Initialsierungsmethoden rufen
       !
       IF ( no_error( ) ) CALL setup_date_language ( c_def_language )
       !
       ! [1.7] endgueltiges Setzen des Initialisierungs-Indikators
       !
       initialised = MERGE( .true., .false., no_error( ) )
       !
    END IF
    !
    ! 2.0 Initialisierungszaehler heraufsetzen
    !
    n_init = n_init + 1
    !
  END SUBROUTINE init_date_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_date_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='clear_date_d' ! 
    !
    IF ( initialised .AND. n_init == 1) THEN
       !
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       !
       IF ( no_error( ) ) CALL clear_date_all_errors ( ) 
       !
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       !
       prn_lun = c_lun 
       trc_lun = c_lun 
       !
       ! [1.3] ggf. weitere De-Initialsierungsmethoden rufen
       !
       IF ( no_error( ) ) CALL setup_date_language ( c_def_language )
       !
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       !
       initialised = MERGE( .false., .true., no_error( ) )
       !
       ! [1.5] alle mit USE eingebundenen Basis-Module de-initialisieren
       !
       ! [1.5.1] ggf. weitere Module de-initialisieren
       !
       ! ... derzeit nicht erforderlich
       !
       ! [1.5.2] Error-Modul zuletzt de-initialisieren
       !
       IF ( no_error( ) ) CALL clear_error ( )
       !
    END IF
    !
    ! 2.0 Initialisierungszaehler heruntersetzen
    !
    n_init = n_init - 1
    !
  END SUBROUTINE clear_date_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_date_prn_lun_d &
       ( lun )
    !
    ! Formalparameter
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='setup_date_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       !
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       !
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       ! ... weitere Basis-Module derzeit nicht erforderlich
       !
    END IF
    !
  END SUBROUTINE setup_date_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_date_trc_lun_d &
       ( lun )
    !
    ! Formalparameter
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='setup_date_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       !
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       !
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       ! ... weitere Basis-Module derzeit nicht erforderlich
       !
    END IF
    !
  END SUBROUTINE setup_date_trc_lun_d
  !
  !! Setzen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch <BR>
  !! 2 = Englisch <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_date_language_d &
       ( act_language )
    !
    ! Formalparameter
    !! Index f&uuml;r Spracheinstellung
    INTEGER , INTENT(IN) :: act_language ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='setup_date_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       language = MERGE ( act_language, c_def_language, ( 1 <= act_language .AND. act_language <= c_max_language ) )
       !
    END IF
    !
  END SUBROUTINE setup_date_language_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_date_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(INOUT) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='new_date_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       this%day   = 0
       this%month = 0
       this%year  = 0
       !
    END IF
    !
  END SUBROUTINE new_date_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_date_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='new_date_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          !
          CALL new_date_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE new_date_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_date_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(INOUT) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='kill_date_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       CALL new_date_0 ( this )
       !
    END IF
    !
  END SUBROUTINE kill_date_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_date_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='kill_date_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          !
          CALL kill_date_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE kill_date_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_date_0 &
       ( this )              &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ok_date_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp+1) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       l_ok(1) = ok_date_day( this )
       l_ok(2) = ok_date_month( this )
       l_ok(3) = ok_date_year( this )
       l_ok(4) = ok_date_julian_gregorian ( this )
       !
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_date_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_date_1 &
       ( this )              &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ok_date_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this) ) EXIT
          !
          ok(i) = ok_date_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END FUNCTION ok_date_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_date_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_date_0' 
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
       IF ( no_error( ) ) CALL print_date_day   ( this )
       IF ( no_error( ) ) CALL print_date_month ( this )
       IF ( no_error( ) ) CALL print_date_year  ( this )
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
8000 FORMAT('# Beginn Objekt t_date ------------------------------')
8001 FORMAT('# Ende   Objekt t_date ------------------------------')
    !
  END SUBROUTINE print_date_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_date_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_date_1' 
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
          IF ( no_error( ) ) CALL print_date_0 ( this(i) )
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
  END SUBROUTINE print_date_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_date_static_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_date_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, language
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_date_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_date         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '# initialised = ',L1,/ &
    '#      prn_op = ',L1,/ &
    '#      trc_op = ',L1,/ &
    '#     prn_lun = ',I5,/ &
    '#     trc_lun = ',I5,/ &
    '#      n_init = ',I5,/ &
    '#    language = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_date_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_date_all_errors_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_date_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       CALL print_error( all_errors(:) )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
  END SUBROUTINE print_date_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "day" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_day_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "day"
    INTEGER           , INTENT(IN)  :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_day_0_0' 
    !
    this%day = val
    !
  END SUBROUTINE set_date_day_0_0
  !
  !! weise der Komponente "day" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_day_1_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "day"
    INTEGER           , INTENT(IN)  :: val     ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_day_1_0' 
    !
    this(:)%day = val
    !
  END SUBROUTINE set_date_day_1_0
  !
  !! weise der Komponente "day" einen Vektor zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_day_1_1 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !! Komponente "day" (Vektor)
    INTEGER       , INTENT(IN)  :: val(:)  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_day_1_1' 
    !
    this(:)%day = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_date_day_1_1
  !
  ! --- Version(en) fuer skalare Komponente "month" [ggf. entfernen]
  !
  !! weise der Komponente "month" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_month_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "month"
    INTEGER           , INTENT(IN)  :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_month_0_0' 
    !
    this%month = val
    !
  END SUBROUTINE set_date_month_0_0
  !
  !! weise der Komponente "month" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_month_1_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "month"
    INTEGER           , INTENT(IN)  :: val     ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_month_1_0' 
    !
    this(:)%month = val
    !
  END SUBROUTINE set_date_month_1_0
  !
  !! weise der Komponente "month" einen Vektor zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_month_1_1 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !! Komponente "month" (Vektor)
    INTEGER       , INTENT(IN)  :: val(:)  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_month_1_1' 
    !
    this(:)%month = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_date_month_1_1
  !
  ! --- Version(en) fuer skalare Komponente "year" [ggf. entfernen]
  !
  !! weise der Komponente "year" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_year_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "year"
    INTEGER           , INTENT(IN)  :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_year_0_0' 
    !
    this%year = val
    !
  END SUBROUTINE set_date_year_0_0
  !
  !! weise der Komponente "year" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_year_1_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "year"
    INTEGER           , INTENT(IN)  :: val     ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_year_1_0' 
    !
    this(:)%year = val
    !
  END SUBROUTINE set_date_year_1_0
  !
  !! weise der Komponente "year" einen Vektor zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_year_1_1 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !! Komponente "year" (Vektor)
    INTEGER       , INTENT(IN)  :: val(:)  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_year_1_1' 
    !
    this(:)%year = val(:MIN(SIZE(val),SIZE(this)))
    !
  END SUBROUTINE set_date_year_1_1
  !
  !! weise allen Komponenten "day","month","year" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_0_0 &
       ( this,  &
         day,   &
         month, &
         year )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "day"
    INTEGER       , INTENT(IN)  :: day  ! 
    !! Wert f&uuml;r Komponente "month"
    INTEGER       , INTENT(IN)  :: month! 
    !! Wert f&uuml;r Komponente "year"
    INTEGER       , INTENT(IN)  :: year ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_0_0' 
    !
    CALL set_date_day_0_0   ( this, day )
    CALL set_date_month_0_0 ( this, month )
    CALL set_date_year_0_0  ( this, year )
    !
  END SUBROUTINE set_date_0_0
  !
  !! weise allen Komponenten "day","month","year" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_1_0 &
       ( this,  &
         day,   &
         month, &
         year )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "day"
    INTEGER       , INTENT(IN)  :: day  ! 
    !! Wert f&uuml;r Komponente "month"
    INTEGER       , INTENT(IN)  :: month! 
    !! Wert f&uuml;r Komponente "year"
    INTEGER       , INTENT(IN)  :: year ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_1_0' 
    !
    CALL set_date_day_1_0   ( this(:), day )
    CALL set_date_month_1_0 ( this(:), month )
    CALL set_date_year_1_0  ( this(:), year )
    !
  END SUBROUTINE set_date_1_0
  !
  !! weise allen Komponenten "day","month","year" einen Vektor zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_date_1_1 &
       ( this,  &
         day,   &
         month, &
         year )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "day"
    INTEGER       , INTENT(IN)  :: day(:)  ! 
    !! Wert f&uuml;r Komponente "month"
    INTEGER       , INTENT(IN)  :: month(:) ! 
    !! Wert f&uuml;r Komponente "year"
    INTEGER       , INTENT(IN)  :: year(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_date_1_1' 
    !
    CALL set_date_day_1_1   ( this(:), day(:) )
    CALL set_date_month_1_1 ( this(:), month(:) )
    CALL set_date_year_1_1  ( this(:), year(:) )
    !
  END SUBROUTINE set_date_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "day" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_day_0_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "day" (Skalar)
    INTEGER :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_date_day_0_0' 
    !
    val = this%day
    !
  END FUNCTION get_date_day_0_0
  !
  !! hole die Komponente "day" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_day_1_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "day"
    INTEGER :: val(SIZE(this))  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_date_day_1_0' 
    !
    val(:) = this(:)%day
    !
  END FUNCTION get_date_day_1_0
  !
  ! --- Version(en) fuer skalare Komponente "month" [ggf. entfernen]
  !
  !! hole die Komponente "month" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_month_0_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "month" (Skalar)
    INTEGER :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_date_month_0_0' 
    !
    val = this%month
    !
  END FUNCTION get_date_month_0_0
  !
  !! hole die Komponente "month" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_month_1_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "month"
    INTEGER :: val(SIZE(this))  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_date_month_1_0' 
    !
    val(:) = this(:)%month
    !
  END FUNCTION get_date_month_1_0
  !
  ! --- Version(en) fuer skalare Komponente "year" [ggf. entfernen]
  !
  !! hole die Komponente "year" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_year_0_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "year" (Skalar)
    INTEGER :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_date_year_0_0' 
    !
    val = this%year
    !
  END FUNCTION get_date_year_0_0
  !
  !! hole die Komponente "year" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_year_1_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "year"
    INTEGER :: val(SIZE(this))  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_date_year_1_0' 
    !
    val(:) = this(:)%year
    !
  END FUNCTION get_date_year_1_0
  !
  !! Ermittle die Tage im Monat (Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_length_of_month_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this     ! 
    !! Tage im Monat (Skalar)
    INTEGER :: val ! 
    !
    SELECT CASE ( this%month )
    CASE (1, 3, 5, 7, 8, 10, 12) 
       val = 31
    CASE (2)
       val = get_nof_days_in_february ( this )
    CASE (4, 6, 9, 11)
       val = 30
    END SELECT
    !
  END FUNCTION get_date_length_of_month_0
  !
  !! Ermittle die Tage im Monat (Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_length_of_month_1 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:)     ! 
    !! Tage im Monat (Vektor)
    INTEGER :: val(SIZE(this)) ! 
    !! lokaler Z&auml;her
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = get_date_length_of_month_0 ( this(i) )
    END DO
    !
  END FUNCTION get_date_length_of_month_1
  !
  !! Ermittle den julianischen Tag f&uuml;r ein beliebiges Datum (Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION date_to_julian_day_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this     ! 
    !! julianischer Tag (Skalar)
    INTEGER :: val ! 
    ! lokale Variablen etv.
    INTEGER, PARAMETER :: IGREG=15+31*(10+12*1582) ! 
    INTEGER :: ID, IM, IY, JY, JM, JA              ! 
    !
    iy = get_date_year  ( this )
    im = get_date_month ( this )
    id = get_date_day   ( this )
    !       
    IF (iy .LT. 0) iy = iy + 1
    IF (im .GT. 2) THEN
       jy = iy
       jm = im + 1
    ELSE
       jy = iy - 1
       jm = im + 13
    END IF
    val = INT(DBLE(365.25)*jy) + INT(DBLE(30.6001)*jm) + id + 1720995
    IF (id+31*(im+12*iy) .GE. igreg) THEN
       ja = INT(DBLE(0.01)*jy)
       val = val + 2 - ja + INT(DBLE(0.25)*ja)
    END IF
    !
  END FUNCTION date_to_julian_day_0
  !
  !! Ermittle den julianischen Tag f&uuml;r ein beliebiges Datum (Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION date_to_julian_day_1 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:) ! 
    !! julianischer Tag (Vektor)
    INTEGER :: val(SIZE(this)) ! 
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = date_to_julian_day ( this(i) )
    END DO
    !
  END FUNCTION date_to_julian_day_1
  !
  !! Ermittle das Datum f&uuml;r einen julianischen Tag (Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION julian_day_to_date_0 &
       ( julian ) &
       RESULT( this ) 
    !
    ! Formalparameter
    !! julianischer Tag
    INTEGER , INTENT(IN) :: julian ! 
    !! Ergebniswert: Datenobjekt (Skalar)
    TYPE (t_date)        :: this    ! 
    !
    INTEGER, PARAMETER :: igreg=2299161 ! 
    INTEGER :: jalpha, ja, jb, jc, jd, je, id, im, iy ! 
    !
    IF (julian .GE. igreg) THEN
       jalpha = INT((DBLE((julian - 1867216)) - DBLE(0.25)) / DBLE(36524.25))
       ja = julian + 1 + jalpha - INT(DBLE(0.25) * jalpha)
    ELSE
       ja = julian
    END IF
    !
    jb = ja + 1524
    jc = INT(DBLE(6680.) + (DBLE((jb - 2439870)) - DBLE(122.1)) / DBLE(365.25))
    jd = 365*jc + INT(0.25*jc)
    jd = 365*jc + INT(DBLE(0.25)*jc)
    je = INT(DBLE((jb - jd)) / DBLE(30.6001))
    id = jb - jd - INT(DBLE(30.6001)*je)
    im = je - 1
    IF (im .GT. 12) im = im - 12
    iy = jc - 4715
    IF (im .GT. 2) iy = iy - 1
    IF (iy .LE. 0) iy = iy - 1
    !
    CALL set_date( this, id, im, iy)
    !
  END FUNCTION julian_day_to_date_0
  !
  !! Ermittle das Datum f&uuml;r einen julianischen Tag (Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION julian_day_to_date_1 &
       ( julian ) &
       RESULT( this ) 
    !
    ! Formalparameter
    !! julianischer Tag
    INTEGER , INTENT(IN) :: julian(:)          ! 
    !! Ergebniswert: Datenobjekt (Skalar)
    TYPE (t_date)        :: this(SIZE(julian)) ! 
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(julian)
       this(i) = julian_day_to_date_0 ( julian(i) )
    END DO
    !
  END FUNCTION julian_day_to_date_1
  !
  !! Ermittle einen Datums-String (Skalar) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_to_string_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this     ! 
    !! Datums-String <BR>
    !! language=1 : TT.MM.JJJJ <BR>
    !! language=2 : DD/MM/YYYY
    CHARACTER (LEN=10) :: val ! 
    !! Fehlervariable
    INTEGER :: stat ! ... damit kein Abbruch bei fehlerhaftem Schreiben erfolgt
    !
    SELECT CASE ( language )
    CASE ( 1 )
       WRITE(val,'(2(I2.2,A1),I4.4)',IOSTAT=stat) &
            this%day, ".", this%month, ".", this%year
    CASE ( 2 )
       WRITE(val,'(2(I2.2,A1),I4.4)',IOSTAT=stat) &
            this%month, "/", this%day, "/", this%year
    CASE DEFAULT
       val = 'undefined '
    END SELECT
    !
  END FUNCTION date_to_string_0
  !
  !! Ermittle einen Datums-String (Vektor) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_to_string_1 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:)     ! 
    !! Datums-String <BR>
    !! language=1 : TT.MM.JJJJ <BR>
    !! language=2 : DD/MM/YYYY
    CHARACTER (LEN=10) :: val(SIZE(this)) ! 
    !! lokaler Z&auml;her
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = date_to_string_0 ( this(i) )
    END DO
    !
  END FUNCTION date_to_string_1
  !
  !! Gib den Wochentag als Datums-String (Kurzform) zur&uuml;ck (Skalar) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_day_to_short_string_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this     ! 
    !! Wochentag als String <BR>
    !! language=1 : "So", ... <BR>
    !! language=2 : "Su", ...
    CHARACTER (LEN=LEN(c_days_short)) :: val ! 
    ! lokale Variablen, etc.
    !! Referenzdatum mit bekanntem Wochentag
    TYPE(t_date) , PARAMETER :: reference_date = t_date(10,11,1996)
    !! Hilfsvariable
    INTEGER :: jdays ! 
    !
    jdays = this - reference_date
    jdays = MOD (jdays, 7)
    !
    IF (jdays < 0) jdays = jdays + 7
    SELECT CASE ( language )
    CASE ( 1,2 )
       val = c_days_short ( jdays, language )
    CASE DEFAULT
       val = REPEAT( '?', LEN(val) )
    END SELECT
    !
  END FUNCTION date_day_to_short_string_0
  !
  !! Gib den Wochentag als Datums-String (Kurzform) zur&uuml;ck (Vektor) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_day_to_short_string_1 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:)     ! 
    !! Wochentag als String <BR>
    !! language=1 : "So", ... <BR>
    !! language=2 : "Su", ...
    CHARACTER (LEN=LEN(c_days_short)) :: val(SIZE(this)) ! 
    !! lokaler Z&auml;her
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = date_day_to_short_string_0 ( this(i) )
    END DO
    !
  END FUNCTION date_day_to_short_string_1
  !
  !! Gib den Wochentag als Datums-String (Langform) zur&uuml;ck (Skalar) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_day_to_long_string_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this     ! 
    !! Wochentag als String <BR>
    !! language=1 : "Sonntag", ... <BR>
    !! language=2 : "Sunday", ...
    CHARACTER (LEN=LEN(c_days_long)) :: val ! 
    ! lokale Variablen, etc.
    !! Referenzdatum mit bekanntem Wochentag
    TYPE(t_date) , PARAMETER :: reference_date = t_date(10,11,1996)
    !! Hilfsvariable
    INTEGER :: jdays !
    !
    jdays = this - reference_date
    jdays = MOD (jdays, 7)
    !
    IF (jdays < 0) jdays = jdays + 7
    SELECT CASE ( language )
    CASE ( 1,2 )
       val = c_days_long ( jdays, language )
    CASE DEFAULT
       val = REPEAT( '?', LEN(val) )
    END SELECT
    !
  END FUNCTION date_day_to_long_string_0
  !
  !! Gib den Wochentag als Datums-String (Langform) zur&uuml;ck (Vektor) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_day_to_long_string_1 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:)     ! 
    !! Wochentag als String <BR>
    !! language=1 : "Sonntag", ... <BR>
    !! language=2 : "Sunday", ...
    CHARACTER (LEN=LEN(c_days_long)) :: val(SIZE(this)) ! 
    !! lokaler Z&auml;her
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = date_day_to_long_string_0 ( this(i) )
    END DO
    !
  END FUNCTION date_day_to_long_string_1
  !
  !! Gib den Monat als String (Kurzform) zur&uuml;ck (Skalar) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_month_to_short_string_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this     ! 
    !! Monat als String <BR>
    !! language=1 : "Mrz", ... <BR>
    !! language=2 : "Mar", ...
    CHARACTER (LEN=LEN(c_months_short)) :: val ! 
    !
    SELECT CASE ( language )
    CASE ( 1,2 )
       val = c_months_short ( this%month, language )
    CASE DEFAULT
       val = REPEAT( '?', LEN(val) )
    END SELECT
    !
  END FUNCTION date_month_to_short_string_0
  !
  !! Gib den Monat als String (Kurzform) zur&uuml;ck (Vektor) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_month_to_short_string_1 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:)     ! 
    !! Monat als String <BR>
    !! language=1 : "Mrz", ... <BR>
    !! language=2 : "Mar", ...
    CHARACTER (LEN=LEN(c_months_short)) :: val(SIZE(this)) ! 
    !! lokaler Z&auml;her
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = date_month_to_short_string_0 ( this(i) )
    END DO
    !
  END FUNCTION date_month_to_short_string_1
  !
  !! Gib den Monat als String (Langform) zur&uuml;ck (Skalar) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_month_to_long_string_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this     ! 
    !! Monat als String <BR>
    !! language=1 : "Maerz", ... <BR>
    !! language=2 : "March", ...
    CHARACTER (LEN=LEN(c_months_long)) :: val ! 
    !
    SELECT CASE ( language )
    CASE ( 1,2 )
       val = c_months_long ( this%month, language )
    CASE DEFAULT
       val = REPEAT( '?', LEN(val) )
    END SELECT
    !
  END FUNCTION date_month_to_long_string_0
  !
  !! Gib den Monat als String (Langform) zur&uuml;ck (Vektor) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION date_month_to_long_string_1 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:)     ! 
    !! Monat als String <BR>
    !! language=1 : "Maerz", ... <BR>
    !! language=2 : "March", ...
    CHARACTER (LEN=LEN(c_months_long)) :: val(SIZE(this)) ! 
    !! lokaler Z&auml;her
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = date_month_to_long_string_0 ( this(i) )
    END DO
    !
  END FUNCTION date_month_to_long_string_1
  !
  !! Addiert eine Anzahl von Tagen zu einem Datum hinzu (Skalar/Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_add_days_to_date_0_0_oi &
       ( this1, idays ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Datenobjekt zu dem eine Anzahl von Tagen addiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Anzahl von Tagen, die zu "this1" hinzugez&auml;hlt werden sollen
    INTEGER       , INTENT(IN) :: idays ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2 !
    !
    this2 = julian_day_to_date ( date_to_julian_day ( this1 ) + idays )
    !
  END FUNCTION get_add_days_to_date_0_0_oi
  !
  !! Addiert eine Anzahl von Tagen zu einem Datum hinzu (Vektor/Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_add_days_to_date_1_0_oi &
       ( this1, idays ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Datenobjekt zu dem eine Anzahl von Tagen addiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Anzahl von Tagen, die zu "this1" hinzugez&auml;hlt werden sollen
    INTEGER       , INTENT(IN) :: idays ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2(SIZE(this1)) !
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this1)
       this2(i) = get_add_days_to_date_0_0_oi ( this1(i), idays )
    END DO
    !
  END FUNCTION get_add_days_to_date_1_0_oi
  !
  !! Addiert eine Anzahl von Tagen zu einem Datum hinzu (Vektor/Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_add_days_to_date_1_1_oi &
       ( this1, idays ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Datenobjekt zu dem eine Anzahl von Tagen addiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Anzahl von Tagen, die zu "this1" hinzugez&auml;hlt werden sollen
    INTEGER       , INTENT(IN) :: idays(:) ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2(MIN(SIZE(this1),SIZE(idays))) !
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this2)
       this2(i) = get_add_days_to_date_0_0_oi ( this1(i), idays(i) )
    END DO
    !
  END FUNCTION get_add_days_to_date_1_1_oi
  !
  !! Addiert eine Anzahl von Tagen zu einem Datum hinzu (Skalar/Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_add_days_to_date_0_0_io &
       ( idays, this1 ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Anzahl von Tagen, die zu "this1" hinzugez&auml;hlt werden sollen
    INTEGER       , INTENT(IN) :: idays ! 
    !! Datenobjekt zu dem eine Anzahl von Tagen addiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2 !
    !
    this2 = get_add_days_to_date_0_0_oi ( this1, idays )
    !
  END FUNCTION get_add_days_to_date_0_0_io
  !
  !! Addiert eine Anzahl von Tagen zu einem Datum hinzu (Vektor/Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_add_days_to_date_1_0_io &
       ( idays, this1 ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Anzahl von Tagen, die zu "this1" hinzugez&auml;hlt werden sollen
    INTEGER       , INTENT(IN) :: idays ! 
    !! Datenobjekt zu dem eine Anzahl von Tagen addiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2(SIZE(this1)) !
    !
    this2 = get_add_days_to_date_1_0_oi ( this1(:), idays )
    !
  END FUNCTION get_add_days_to_date_1_0_io
  !
  !! Addiert eine Anzahl von Tagen zu einem Datum hinzu (Vektor/Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_add_days_to_date_1_1_io &
       ( idays, this1 ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Anzahl von Tagen, die zu "this1" hinzugez&auml;hlt werden sollen
    INTEGER       , INTENT(IN) :: idays(:) ! 
    !! Datenobjekt zu dem eine Anzahl von Tagen addiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2(MIN(SIZE(this1),SIZE(idays))) !
    !
    this2 = get_add_days_to_date_1_1_oi ( this1(:), idays(:) )
    !
  END FUNCTION get_add_days_to_date_1_1_io
  !
  !! Subtrahiert eine Anzahl von Tagen von einem Datum (Skalar/Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_sub_days_from_date_0_0_oi &
       ( this1, idays ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Datenobjekt von dem eine Anzahl von Tagen subtrahiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Anzahl von Tagen, die von "this1" subtrahiert werden sollen
    INTEGER       , INTENT(IN) :: idays ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2 !
    !
    this2 = get_add_days_to_date_0_0_oi ( this1, -idays )
    !
  END FUNCTION get_sub_days_from_date_0_0_oi
  !
  !! Subtrahiert eine Anzahl von Tagen von einem Datum (Vektor/Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_sub_days_from_date_1_0_oi &
       ( this1, idays ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Datenobjekt von dem eine Anzahl von Tagen subtrahiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Anzahl von Tagen, die von "this1" subtrahiert werden sollen
    INTEGER       , INTENT(IN) :: idays ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2(SIZE(this1)) !
    !
    this2(:) = get_add_days_to_date_1_0_oi ( this1(:), -idays )
    !
  END FUNCTION get_sub_days_from_date_1_0_oi
  !
  !! Subtrahiert eine Anzahl von Tagen von einem Datum (Vektor/Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_sub_days_from_date_1_1_oi &
       ( this1, idays ) &
       RESULT( this2 )
    !
    ! Formalparameter
    !! Datenobjekt von dem eine Anzahl von Tagen subtrahiert werden soll
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Anzahl von Tagen, die von "this1" subtrahiert werden sollen
    INTEGER       , INTENT(IN) :: idays(:) ! 
    !! Ergebniswert: neues Datum
    TYPE (t_date ) :: this2(MIN(SIZE(this1),SIZE(idays))) !
    !
    this2(:) = get_add_days_to_date_1_1_oi ( this1(:), -idays(:) )
    !
  END FUNCTION get_sub_days_from_date_1_1_oi
  !
  !! Subtrahiert "this2" von "this1" und liefert die Anzahl von <BR>
  !! dazwischen liegenden Tagen (Skalar/Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_sub_days_from_date_0_0_oo &
       ( this1, this2 ) &
       RESULT( idays )
    !
    ! Formalparameter
    !! Datenobjekt 1
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Datenobjekt 2
    TYPE (t_date) , INTENT(IN) :: this2 ! 
    !! Ergebniswert: Differenz der Tage zwischen "this1" und "this2"
    INTEGER :: idays !
    !
    idays = date_to_julian_day_0( this1 ) - &
            date_to_julian_day_0( this2 )
    !
  END FUNCTION get_sub_days_from_date_0_0_oo
  !
  !! Subtrahiert "this2" von "this1" und liefert die Anzahl von <BR>
  !! dazwischen liegenden Tagen (Skalar/Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_sub_days_from_date_0_1_oo &
       ( this1, this2 ) &
       RESULT( idays )
    !
    ! Formalparameter
    !! Datenobjekt 1
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Datenobjekt 2
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !! Ergebniswert: Differenz der Tage zwischen "this1" und "this2"
    INTEGER :: idays(SIZE(this2)) ! 
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(idays)
       idays(i) = get_sub_days_from_date_0_0_oo ( this1, this2(i) )
    END DO
    !
  END FUNCTION get_sub_days_from_date_0_1_oo
  !
  !! Subtrahiert "this2" von "this1" und liefert die Anzahl von <BR>
  !! dazwischen liegenden Tagen (Vektor/Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_sub_days_from_date_1_0_oo &
       ( this1, this2 ) &
       RESULT( idays )
    !
    ! Formalparameter
    !! Datenobjekt 1
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Datenobjekt 2
    TYPE (t_date) , INTENT(IN) :: this2 ! 
    !! Ergebniswert: Differenz der Tage zwischen "this1" und "this2"
    INTEGER :: idays(SIZE(this1)) ! 
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(idays)
       idays(i) = get_sub_days_from_date_0_0_oo ( this1(i), this2 )
    END DO
    !
  END FUNCTION get_sub_days_from_date_1_0_oo
  !
  !! Subtrahiert "this2" von "this1" und liefert die Anzahl von <BR>
  !! dazwischen liegenden Tagen (Vektor/Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_sub_days_from_date_1_1_oo &
       ( this1, this2 ) &
       RESULT( idays )
    !
    ! Formalparameter
    !! Datenobjekt 1
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Datenobjekt 2
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !! Ergebniswert: Differenz der Tage zwischen "this1" und "this2"
    INTEGER :: idays(MIN(SIZE(this1),SIZE(this2))) ! 
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(idays)
       idays(i) = get_sub_days_from_date_0_0_oo ( this1(i), this2(i) )
    END DO
    !
  END FUNCTION get_sub_days_from_date_1_1_oo
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_date_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_date_0_0' 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%day   == this2%day )
    l_ok(2) = ( this1%month == this2%month )
    l_ok(3) = ( this1%year  == this2%year )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_date_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_date_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_date_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = eq_date_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_date_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_date_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_date_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = eq_date_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_date_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_date_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_date_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = eq_date_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_date_1_1
  !
  !! Ermitteln des Datums mit Hilfe der Systemuhr (Skalar)<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_date_from_system_0 &
       ( )                        &
       RESULT( this )
    !
    !! R&uuml;ckgabewert : aktuelles Datum
    TYPE (t_date) :: this ! 
    !! lokales Hilfsfeld
    INTEGER :: dt(8)      ! 
    !
    CALL new_date ( this )
    !
    CALL date_and_time ( VALUES=dt(:) )
    !
    IF ( dt(1) /= -HUGE(0) ) CALL set_date_year  ( this, dt(1) )
    IF ( dt(2) /= -HUGE(0) ) CALL set_date_month ( this, dt(2) )
    IF ( dt(3) /= -HUGE(0) ) CALL set_date_day   ( this, dt(3) )
    !
  END FUNCTION get_date_from_system_0
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
  FUNCTION gt_date_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_date_0_0' 
    !
    ok = ( date_to_julian_day ( this1 ) > date_to_julian_day ( this2 ) )
    !
  END FUNCTION gt_date_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_date_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_date_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = gt_date_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION gt_date_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_date_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_date_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = gt_date_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION gt_date_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_date_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_date_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = gt_date_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION gt_date_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_date_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_date_0_0' 
    !
    ok = ( date_to_julian_day ( this1 ) >= date_to_julian_day ( this2 ) )
    !
  END FUNCTION ge_date_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_date_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_date_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = ge_date_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ge_date_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_date_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_date_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = ge_date_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ge_date_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_date_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_date_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = ge_date_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ge_date_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_date_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_date_0_0' 
    !
    ok = ( date_to_julian_day ( this1 ) < date_to_julian_day ( this2 ) )
    !
  END FUNCTION lt_date_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_date_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_date_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = lt_date_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION lt_date_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_date_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_date_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = lt_date_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION lt_date_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_date_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_date_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = lt_date_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION lt_date_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_date_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_date_0_0' 
    !
    ok = ( date_to_julian_day ( this1 ) <= date_to_julian_day ( this2 ) )
    !
  END FUNCTION le_date_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_date_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_date_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = le_date_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION le_date_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
 FUNCTION le_date_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_date_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = le_date_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION le_date_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_date_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_date_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = le_date_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION le_date_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_date_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ne_date_0_0' 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_date_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_date_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ne_date_1_0' 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_date_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_date_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_date) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ne_date_0_1' 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_date_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_date_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_date) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ne_date_1_1' 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_date_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-ASSIGNMENT(=)-Methoden <<< 
  ! ----------------------------------------------------------------------
  !
  !! Zuweisen eines julianischen Tages auf ein Objekt "t_date" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE as_date_0_0_oi &
       ( this, julian )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(INOUT)  :: this ! 
    !! julianischer Tag (Skalar)
    INTEGER       , INTENT(IN) :: julian  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='as_date_0_0_oi' 
    !
    this = julian_day_to_date_0 ( julian )
    !
  END SUBROUTINE as_date_0_0_oi
  !
  !! Zuweisen eines julianischen Tages auf ein vektorielles Objekt "t_date" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE as_date_1_0_oi &
       ( this, julian )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT)  :: this(:) ! 
    !! julianischer Tag (Skalar)
    INTEGER       , INTENT(IN) :: julian  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='as_date_1_0_oi' 
    !
    this(:) = julian_day_to_date_0 ( julian )
    !
  END SUBROUTINE as_date_1_0_oi
  !
  !! Zuweisen mehrerer julianischer Tage auf ein vektorielles Objekt "t_date" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE as_date_1_1_oi &
       ( this, julian )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(INOUT)  :: this(:) ! 
    !! julianischer Tag (Skalar)
    INTEGER       , INTENT(IN) :: julian(:)  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='as_date_1_1_oi' 
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this),SIZE(julian))
       this(i) = julian_day_to_date_0 ( julian(i) )
    END DO
    !
  END SUBROUTINE as_date_1_1_oi
  !
  !! Umrechnen eines Objekts "t_date" in einen julianischen Tag <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE as_date_0_0_io &
       ( julian, this )
    !
    ! Formalparameter
    !! julianischer Tag (Skalar)
    INTEGER       , INTENT(OUT) :: julian  ! 
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN)  :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='as_date_0_0_io' 
    !
    julian = date_to_julian_day_0 ( this )
    !
  END SUBROUTINE as_date_0_0_io
  !
  !! Umrechnen eines Objekts "t_date" in einen julianischen Tag <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE as_date_1_0_io &
       ( julian, this )
    !
    ! Formalparameter
    !! julianischer Tag (Feld)
    INTEGER       , INTENT(OUT) :: julian(:)! 
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN)  :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='as_date_1_0_io' 
    !
    julian(:) = date_to_julian_day_0 ( this )
    !
  END SUBROUTINE as_date_1_0_io
  !
  !! Umrechnen mehrerer Objekte "t_date" in julianische Tage <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE as_date_1_1_io &
       ( julian, this )
    !
    ! Formalparameter
    !! julianischer Tag (Feld)
    INTEGER       , INTENT(OUT) :: julian(:)! 
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN)  :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='as_date_1_1_io' 
    !! lokaler Za&uml;hler
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this),SIZE(julian))
       julian(i) = date_to_julian_day_0 ( this(i) )
    END DO
    !
  END SUBROUTINE as_date_1_1_io
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-LOGICAL-Methoden <<< 
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fen ob das Jahr in der Datumsangabe ein Schaltjahr ist (Skalar)
  FUNCTION is_leap_year_0 &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Objekt mit aktueller Datumsangabe
    TYPE (t_date) , INTENT(IN) :: this ! 
    !
    !! Rueckgabewert: Testergebnis 
    LOGICAL :: ok ! 
    !
    ok = ( get_nof_days_in_february ( this ) == 29 )
    !
  END FUNCTION is_leap_year_0
  !
  !! Pr&uuml;fen ob das Jahr ein Schaltjahr ist (Vektor)
  FUNCTION is_leap_year_1 &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Objekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! lokaler Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       ok(i) = is_leap_year_0 ( this(i) )
    END DO
    !
  END FUNCTION is_leap_year_1
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
    !
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
       WRITE(*,*) ' *** Warnung *** Modul "b_date" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_date ausfuehren'
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
    !
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    ! Rueckgabewert
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
  SUBROUTINE init_date_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='init_date_all_errors' !
    !
    CALL new_error( all_errors(:) )
    !
    ! Index 001
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 1), 1 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 1), &
         'Fehlerkategorie: ALLGEMEIN\n'//&
         'Modul ist nicht initialisiert\n'//&
         '--> INIT_date ausfuehren' )
    ! Index 002
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 2), 2 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 2), &
         'Fehlerkategorie: ALLGEMEIN\n'//&
         'Modul ist schon initialisiert\n'//&
         '--> CLEAR_date ausfuehren' )
    ! Index 003
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 3), 6010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 3), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_date"\n'//&
         'Typ-Komponente = "day"\n'//&
         'Datumsangabe = <AktuellesDatum>\n'//&
         '--> Daten pruefen' )
    ! Index 004
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 4), 6020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 4), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_date"\n'//&
         'Typ-Komponente = "month"\n'//&
         'Datumsangabe = <AktuellesDatum>\n'//&
         '--> Daten pruefen' )
    ! Index 005
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 5), 6030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 5), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_date"\n'//&
         'Typ-Komponente = "year"\n'//&
         'Datumsangabe = <AktuellesDatum>\n'//&
         '--> Daten pruefen' )
    ! Index 006
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 6), 6040 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 6), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_date"\n'//&
         'Nicht erlaubtes Datum wegen Kalenderumstellung (Julianisch->Gregorianisch)\n'//&
         'die Periode 5.10.1582 bis 14.10.1582 existiert nicht\n'//&
         'Datumsangabe = <AktuellesDatum>\n'//&
         '--> Daten pruefen' )
    ! Index 007
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 7), 7001 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 7), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken der Kopfzeilen\n'//&
         '--> Code in Modul "b_date" pruefen' )
    ! Index 008
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 8), 7002 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 8), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken der Fusszeilen\n'//&
         '--> Code in Modul "b_date" pruefen' )
    ! Index 009
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 9), 7003 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 9), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken des Index des Datenobjektes (1D-Array)\n'//&
         '--> Code in Modul "b_date" pruefen' )
    ! Index 010
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(10), 7010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(10), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_date"\n'//&
         'Typ-Komponente = "day"\n'//&
         '--> Code in Modul "b_date" / Daten pruefen' )
    ! Index 011
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(11), 7020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(11), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_date"\n'//&
         'Typ-Komponente = "month"\n'//&
         '--> Code in Modul "b_date" / Daten pruefen' )
    ! Index 012
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(12), 7030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(12), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_date"\n'//&
         'Typ-Komponente = "year"\n'//&
         '--> Code in Modul "b_date" / Daten pruefen' )
    ! Index 013
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(13), 7500 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(13), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken statischer Daten aus "b_date"\n'//&
         '--> Code in Modul "b_date" / Daten pruefen' )
    !
  END SUBROUTINE init_date_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_date_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='clear_date_all_errors' !
    !
    CALL kill_error( all_errors(:) )
    !
  END SUBROUTINE clear_date_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "day" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_date_day &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_date) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_date_day' ! 
    !
    ok = ( 1 <= this%day .AND. this%day <= get_date_length_of_month( this ) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<AktuellesDatum>', get_date_as_xxl_string( this ) ) 
    END IF
    !
  END FUNCTION ok_date_day
  !
  !! Pr&uuml;fe, ob die Komponente "month" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_date_month &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_date) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_date_month' ! 
    !
    ok = ( 1 <= this%month .AND. this%month <= 12 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<AktuellesDatum>', get_date_as_xxl_string( this ) ) 
    END IF
    !
  END FUNCTION ok_date_month
  !
  !! Pr&uuml;fe, ob die Komponente "year" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_date_year &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_date) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_date_year' ! 
    !
    ok = ( 1 <= this%year .AND. this%year <= 9999 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       CALL setup_error_act ( '<AktuellesDatum>', get_date_as_xxl_string( this ) ) 
    END IF
    !
  END FUNCTION ok_date_year
  !
  !! Pr&uuml;fe, ob Datum  nicht im Bereich 5.10.1582 - 14.10.1582 liegt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_date_julian_gregorian &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_date) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_date_julian_gregorian' ! 
    !
    ok = .NOT. ( this%year == 1582 .AND. this%month == 10 .AND. &
                 this%day  >=    5 .AND. this%day    < 15 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       CALL setup_error_act ( '<AktuellesDatum>', get_date_as_xxl_string( this ) ) 
    END IF
    !
  END FUNCTION ok_date_julian_gregorian
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "day" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_date_day &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_date) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_date_day' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%day
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente day  - - - - - - - - - - ',/&
           '# day   = ',I4,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_date_day
  !
  !! Drucke den Inhalt der Komponente "month" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_date_month &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_date) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_date_month' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%month
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente month  - - - - - - - - - - ',/&
           '# month = ',I4,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_date_month
  !
  !! Drucke den Inhalt der Komponente "year" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_date_year &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_date) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_date_year' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%year
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente year  - - - - - - - - - - ',/&
           '# year  = ',I4,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_date_year
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
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-COMPUTE-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Berechnet die Anzahl der Tage im Februar des Jahres.
  FUNCTION get_nof_days_in_february &
       ( this )        &
       RESULT( val )
    !
    ! Formalparameter
    !! Objekt des Typs "t_date" 
    TYPE (t_date) , INTENT(IN) :: this ! 
    !! Hilfsdatum
    TYPE (t_date) :: thlp 
    !! Ergebniswert: Anzahl der Tage im Februar
    INTEGER :: val ! 
    !
    thlp = t_date(1, 3, this%year) - 1
    val  = thlp%day
    !
  END FUNCTION get_nof_days_in_february 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-STRING-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Ermittle einen Datums-XXL-String (Skalar) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_date_as_xxl_string_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_date) , INTENT(IN) :: this     ! 
    !! Datums-String <BR>
    !! language=1 : TT.MM.JJJJ <BR>
    !! language=2 : DD/MM/YYYY
    CHARACTER (LEN=29) :: val ! 
    !! Fehlervariable
    INTEGER :: stat ! ... damit kein Abbruch bei fehlerhaftem Schreiben erfolgt
    !
    SELECT CASE ( language )
    CASE ( 1 )
       WRITE(val,'(2(I9,A1),I9)',IOSTAT=stat) &
            this%day, ".", this%month, ".", this%year
    CASE ( 2 )
       WRITE(val,'(2(I9,A1),I9)',IOSTAT=stat) &
            this%month, "/", this%day, "/", this%year
    CASE DEFAULT
       val = REPEAT( ' ', LEN( val ) )
       val = 'undefined '
    END SELECT
    !
  END FUNCTION get_date_as_xxl_string_0
  !
  !! Ermittle einen Datums-XXL-String (Vektor) <BR>
  !! das R&uuml;ckgabeformat ist abh&auml;ngig von "language" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_date_as_xxl_string_1 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_date) , INTENT(IN) :: this(:)     ! 
    !! Datums-String <BR>
    !! language=1 : TT.MM.JJJJ <BR>
    !! language=2 : DD/MM/YYYY
    CHARACTER (LEN=29) :: val(SIZE(this)) ! 
    !! lokaler Z&auml;her
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = get_date_as_xxl_string_0 ( this(i) )
    END DO
    !
  END FUNCTION get_date_as_xxl_string_1
  !
END MODULE b_date
! TailOfBaseModule --------------------------------------------------------
