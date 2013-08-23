! HeadOfBaseModule --------------------------------------------------------
!! <H2>Datentyp und Methoden f&uuml;r Fehlerbehandlung</h2>
!! @author G&uuml;nther Lang
!! @version 1.15 vom 02/23/05, Quellcode: mod_b_error.f90
!! <HR>
!! Basic type and methods to deal with errors in Fortran90 programs. <BR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2001 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2001-10-08 : G&uuml;nther Lang : Original-Version
!  01.02 : 2002-04-16 : G&uuml;nther Lang : bei erneutem SETUP wird aktueller Fehler gedruckt
!  01.03 : 2002-05-28 : G&uuml;nther Lang : Korrektur in MERGE fuer trc_op
!  01.04 : 2002-06-05 : G&uuml;nther Lang : ok_initialised modifiziert
!  01.05 : 2002-06-12 : G&uuml;nther Lang : Zaehler n_init integriert
!  01.06 : 2002-06-12 : G&uuml;nther Lang : kleine Korrektur beim Initialisieren von prn_lun, trc_lun
!  01.07 : 2002-09-24 : G&uuml;nther Lang : print_error_to_screen (Kurznachricht)
!  01.08 : 2002-10-16 : Jens J&uuml;rges : "err_warn" eingefuehrt, um
!                       die Funktionalitaet der Routine print_error_to_screen
!                       ein- und ausschalten zu koennen.
!  01.09 : 2003-02-10 : Jens J&uuml;rges : Fehlerbehandlung wird fit fuer 
!                       den Mehrprozessor-Betrieb (OpenMP Direktiven verwendet)
!  01.10 : 2003-03-04 : G&uuml;nther Lang : Anpassungen TV12 vom Dez. 2002
!  01.11 : 2003-03-04 : G&uuml;nther Lang : Copyright-Hinweis in INIT_ERROR
!  01.12 : 2003-07-09 : Andreas Malcherek : t_error in set_error_ierror auf intent inout gesetzt
!  01.13 : 2004-01-19 : Holger Weilbeer   : Bei selbstdefinierten Datentypen INTENT(OUT) -> INTENT(INOUT) gewandelt
!  01.14 : 2005-01-17 : G&uuml;nther Lang : Neue Funktionen: get_error_act_merr, get_error_act_ierr, get_error_act_cerr
!  01.15 : 2005-02-23 : G&uuml;nther Lang : Konstante zum Definieren der (max.) L&auml;nge einer Fehlerzeile,
!                                           ausserdem wurden einige c_upname gestutzt oder entfernt
!  
! 2013 : Rebekka Kopmann : Einfueren einer debug Variable zum Vermeiden der 
!                          laenglichen Fehelermeldungen
!
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Datentyp f&uuml;r das Speichern von Fehlermeldungen.             <BR>
!! Methoden zum Arbeiten mit Objekten des vorgenannten Datentyps.   <BR>
!! Daten und Methoden zum Speichern der aktuellen Fehlersituation.  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_error <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> merr : Gr&ouml;&szlig;e des Feldes CERR (0 = nicht allokiert) 
!!     <LI> ierr : Fehlernummer (0 = keine Fehler)      
!!     <LI> cerr : Feld zur Aufnahme der Fehlermeldung
!! </OL>
!! Datentyp und Methoden m&uuml;ssen in <EM>Base-Modules</EM> und <EM>Work-Packages</EM>
!! zum Arbeiten mit Fehlermeldungen in Fortran-Programmen benutzt 
!! werden. 
!!
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_error mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_error mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,
!!          andere nicht. 
!!          Routinen die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,
!!          ob das Modul korrekt initialisert wurde (ok_initialised).
!!                                                                    <BR>
!! <HR>
!! Allgemein             [  0000 bis  0999 ]                        <BR>
!! 00000 = kein Fehler                                              <BR> 
!! 00001 = Modul ist noch nicht initialisiert                       <BR>
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
!! 05030 = De-Allokieren von Komponente 3 des Typs t_error          <BR>
!! <HR>
!! OK-Methoden           [  6000 bis  6999 ]                        <BR>
!! 06010 = Fehler in Komponente 1 des Typs t_error                  <BR>
!! 06020 = Fehler in Komponente 2 des Typs t_error                  <BR>
!! 06030 = Fehler in Komponente 3 des Typs t_error                  <BR>
!! <HR>
!! PRINT-Methoden        [  7000 bis  7999 ]                        <BR>
!! 07001 = Drucken der Kopfzeilen                                   <BR>
!! 07002 = Drucken der Fu&szlig;zeilen                              <BR>
!! 07003 = Drucken des Index des Datenobjektes (1D-Array)           <BR>
!! 07010 = Drucken der Komponente 1 des Typs t_error                <BR>
!! 07020 = Drucken der Komponente 2 des Typs t_error                <BR>
!! 07030 = Drucken der Komponente 3 des Typs t_error                <BR>
!! 07500 = Drucken der statischen Daten (ohne Fehlermeldungen)      <BR>
!! <HR>
!! SET-Methoden          [  8000 bis  8999 ]                        <BR>
!! 08030 = Allokieren von Komponente 3 des Typs t_error             <BR>
!! <HR>
!! GET-Methoden          [  9000 bis  9999 ]                        <BR>
!! <HR>
!! OPERATOR(==)-Methoden [ 10000 bis 10999 ]                        <BR>
!! <HR>
!! OPERATOR(>)-Methoden  [ 15000 bis 15999 ]                        <BR>
!! <HR>
!! OPERATOR(>=)-Methoden [ 16000 bis 16999 ]                        <BR>
!! <HR>
!! OPERATOR(<)-Methoden  [ 17000 bis 17999 ]                        <BR>
!! <HR>
!! OPERATOR(<=)-Methoden [ 18000 bis 18999 ]                        <BR>
!! <HR>
!! modul-spezifische Methoden   [      < 0 ]                        <BR>
!! <HR>
!! NO-Methoden           [  -999 bis    -1 ]                        <BR>
!! <HR>
!! ANY-Methoden          [ -1999 bis -1000 ]                        <BR>
!! <HR>
!! SETUP-Methoden        [ -2999 bis -2000 ]                        <BR>
!! 2000 = Fehlernummer nicht in "all_errors(:)" enthalten           <BR>
!! <HR>
!! PRINT-Methoden        [ -3999 bis -3000 ]                        <BR>
!! <HR>
!! CLEAR-Methoden        [ -4999 bis -4000 ]                        <BR>
!! <HR>
MODULE b_error
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  ! [A.2] BASIS-Modul mit Fehler-Typ und -Routinen
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
  ! [C.0] oeffentlich zugaengliche Maximalwerte
  !! max. L&auml;nge einer Zeile mit Fehlertext
  INTEGER , PUBLIC, PARAMETER :: c_len_error_cerr=80 ! 
!RK for switch on/off of debug information
! default = 0 : no information
!LEO mybe it is better to rename it to debug_b
  INTEGER, PUBLIC, PARAMETER :: debug_b = 1
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Datentyp zur Aufnahme von Fehlerindikator und -text <BR>
  !! L&auml;nge "merr" des Feldes "cerr" <BR>
  !! Fehlerkennung "ierr" <BR>
  !! Fehlermeldung "cerr"
  TYPE , PUBLIC :: t_error
     PRIVATE
     INTEGER                                    :: merr    ! Groesse von CERR
     INTEGER                                    :: ierr    ! Fehlernummer
     CHARACTER (LEN=c_len_error_cerr) , POINTER :: cerr(:) ! Fehlermeldung
  END TYPE t_error
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
  INTERFACE init_error
     MODULE PROCEDURE init_error_d ! 
  END INTERFACE
  !! ggf. De-Allokieren der statischen Daten des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_error
     MODULE PROCEDURE clear_error_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! Hinweis: PRN_LUN in rufender Programmeinheit &ouml;ffnen. 
  INTERFACE setup_error_prn_lun
     MODULE PROCEDURE setup_error_prn_lun_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_error_trc_lun
     MODULE PROCEDURE setup_error_trc_lun_d ! 
  END INTERFACE
  !! Setzen der logischen Variablen <EM>ERR_WARN</EM> auf .TRUE.; <BR>
  !! print_error_to_screen wird ausgefuehrt.
  INTERFACE setup_error_err_warn_on
     MODULE PROCEDURE setup_error_err_warn_on_d
  END INTERFACE
  !! Setzen der logischen Variablen <EM>ERR_WARN</EM> auf .FALSE.; <BR>
  !! print_error_to_screen wird nicht ausgefuehrt; <BR>
  !! dies ist auch der Default-Zustand.
  INTERFACE setup_error_err_warn_off
     MODULE PROCEDURE setup_error_err_warn_off_d
  END INTERFACE
  !! Neues Objekt vom Typ "t_error" erzeugen; <BR>
  !! Initialisieren mit Default-Werten.
  INTERFACE new_error
     MODULE PROCEDURE new_error_0  ! Version fuer Skalar
     MODULE PROCEDURE new_error_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_error" vernichten; <BR>
  !! ggf. De-Allokieren von Memory; <BR>
  !! teilweise Re-Initialisieren mit Default-Werten.
  INTERFACE kill_error
     MODULE PROCEDURE kill_error_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_error_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_error" auf G&uuml;ltigkeit pr&uuml;fen.
  INTERFACE ok_error
     MODULE PROCEDURE ok_error_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_error_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Alle Komponenten des Typs "t_error" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_error
     MODULE PROCEDURE print_error_0 ! Version fuer Skalar
     MODULE PROCEDURE print_error_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Alle statischen Daten des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_error_static
     MODULE PROCEDURE print_error_static_d ! 
  END INTERFACE
  !! Alle Fehlermeldungen des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_error_all_errors
     MODULE PROCEDURE print_error_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "ierr" auf Benutzer-Werte.
  INTERFACE set_error_ierr
     MODULE PROCEDURE set_error_ierr_0_0 ! Objekt (Skalar) / Daten (Skalar)
  END INTERFACE
  !! Allokieren von Memory zur Aufnahme der Fehlertexte. <BR>
  !! Setze Komponente "cerr" auf Benutzer-Werte: <BR>
  !! a) einen String (Skalar) zuweisen; <BR>
  !! b) ein Feld von Strings (Vektor) zuweisen. <BR>
  INTERFACE set_error_cerr
     MODULE PROCEDURE set_error_cerr_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_error_cerr_0_1 ! Objekt (Skalar) / Daten (Array)
  END INTERFACE
  !! Hole Komponente "merr" aus "t_error"
  INTERFACE get_error_merr
     MODULE PROCEDURE get_error_merr_0 ! Skalar
  END INTERFACE
  !! Hole Komponente "ierr" aus "t_error"
  INTERFACE get_error_ierr
     MODULE PROCEDURE get_error_ierr_0 ! Skalar
  END INTERFACE
  !! Hole Komponente "cerr" aus "t_error": <BR>
  !! a) alle Zeilen auslesen; <BR>
  !! b) eine (bestimmte) Zeilen auslesen.
  INTERFACE get_error_cerr
     MODULE PROCEDURE get_error_cerr_0   ! Skalar - alle Zeilen
     MODULE PROCEDURE get_error_cerr_0_i ! Skalar - eine Zeile
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! pr&uuml;fe, ob keine aktuelle Fehlerbedingung gesetzt ist.
  INTERFACE no_error
     MODULE PROCEDURE no_error_d
  END INTERFACE
  !! pr&uuml;fe, ob eine aktuelle Fehlerbedingung gesetzt ist.
  INTERFACE any_error
     MODULE PROCEDURE any_error_d
  END INTERFACE
  !! pr&uuml;fe, ob eine spezifische Fehlerbedingung (mit
  !! einer bestimmten Fehlernummer) gesetzt ist.
  INTERFACE specific_error
     MODULE PROCEDURE specific_error_d
  END INTERFACE
  !! &Uuml;bertragen der aktuellen Fehlersituation in statische Moduldaten; <BR>
  !! Ersetzen eines Platzhalters durch Benutzerwerte: <BR>
  !! a) transferiere die in einem Skalar des Typs "t_error" enthaltenen
  !!    Daten in das Fehlermodul; <BR>
  !! b) transferiere einen bestimmten Fehler (mit Nummer) aus einem
  !!    Feld von Fehlermeldungen (Feld vom Typ "t_error") in das
  !!    Fehlermodul; <BR>
  !! c) transferieren ein Feld von Strings (Typ CHARACTER) sowie eine
  !!    Fehlernummer (Typ INTEGER) in dasFehlermodul; <BR>
  !! d) ersetze in der modulintern abgelegten Fehlermeldung einen
  !!    Platzhalter-String durch den aktuellen Fehlertext.
  INTERFACE setup_error_act
     MODULE PROCEDURE setup_error_act_0
     MODULE PROCEDURE setup_error_act_1
     MODULE PROCEDURE setup_error_act_c
     MODULE PROCEDURE setup_error_act_r
  END INTERFACE
  !! Drucke den Inhalt der aktuellen Fehlermeldung auf <EM>PRN_LUN</EM>
  INTERFACE print_error_act
     MODULE PROCEDURE print_error_act_d
  END INTERFACE
  !! Re-Initialisieren der statischen Moduldaten zur Aufnahme 
  !! einer Fehlersituation
  INTERFACE clear_error_act
     MODULE PROCEDURE clear_error_act_d
  END INTERFACE
  !
  !! Anzahl der in der aktuellen Fehlermeldung enthaltenen Textzeilen ermitteln
  INTERFACE get_error_act_merr
     MODULE PROCEDURE get_error_act_merr_d
  END INTERFACE
  !! Nummer der aktuellen Fehlermeldung ermitteln
  INTERFACE get_error_act_ierr
     MODULE PROCEDURE get_error_act_ierr_d
  END INTERFACE
  !! eine Zeile in der aktuellen Fehlermeldung ermitteln
  INTERFACE get_error_act_cerr
     MODULE PROCEDURE get_error_act_cerr_i
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! [C.6] Operatoren
  !
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !
  ! Hinweis: Operatoren wurden fuer vier verschiedene Parameter-
  !          Konstellationen formuliert. Ggf. nicht sinnvolle
  !          Konstellationen entfernen oder weitere sinnvolle
  !          hinzufuegen.
  !
  !! Vergleich zweier Datenobjekte "t_error"; <BR>
  !! zwei Objekte sind dann gleich, wenn sie in allen Komponenten
  !! &uuml;bereinstimmen
  INTERFACE OPERATOR (==)
     MODULE PROCEDURE eq_error_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_error_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_error_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_error_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Vergleich "(&#062;" zweier Objekte vom Typ "t_error"; <BR>
  !! es werden die Fehlernummern miteinander verglichen.
  INTERFACE OPERATOR (>)
     MODULE PROCEDURE gt_error_0_0  ! Skalar / Skalar
     MODULE PROCEDURE gt_error_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE gt_error_1_0  ! Vektor / Skalar
     MODULE PROCEDURE gt_error_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#062;=" zweier Objekte vom Typ "t_error"; <BR>
  !! es werden die Fehlernummern miteinander verglichen.
  INTERFACE OPERATOR (>=)
     MODULE PROCEDURE ge_error_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ge_error_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ge_error_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ge_error_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#060;" zweier Objekte vom Typ "t_error"; <BR>
  !! es werden die Fehlernummern miteinander verglichen.
  INTERFACE OPERATOR (<) 
     MODULE PROCEDURE lt_error_0_0  ! Skalar / Skalar
     MODULE PROCEDURE lt_error_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE lt_error_1_0  ! Vektor / Skalar
     MODULE PROCEDURE lt_error_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#060;=" zweier Objekte vom Typ "t_error"; <BR>
  !! es werden die Fehlernummern miteinander verglichen.
  INTERFACE OPERATOR (<=)
     MODULE PROCEDURE le_error_0_0  ! Skalar / Skalar
     MODULE PROCEDURE le_error_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE le_error_1_0  ! Vektor / Skalar
     MODULE PROCEDURE le_error_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_error                 ! Initialisieren (Modul)
  PUBLIC :: clear_error                ! De-Initialisieren (Modul)
  PUBLIC :: setup_error_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_error_trc_lun        ! Setzen trc_lun 
  PUBLIC :: setup_error_err_warn_on    ! Setzen err_warn auf .TRUE.
  PUBLIC :: setup_error_err_warn_off   ! Setzen err_warn auf .FALSE.
  PUBLIC :: new_error                  ! Erzeugen 
  PUBLIC :: kill_error                 ! Vernichten
  PUBLIC :: ok_error                   ! Pruefen
  PUBLIC :: print_error                ! Drucken
  PUBLIC :: print_error_static         ! Drucken aller statischen Daten
  PUBLIC :: print_error_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_error_ierr             ! Setzen der Komponente ierr
  PUBLIC :: set_error_cerr             ! Setzen der Komponente cerr
  PUBLIC :: get_error_merr             ! Holen der Komponente merr
  PUBLIC :: get_error_ierr             ! Holen der Komponente ierr
  PUBLIC :: get_error_cerr             ! Holen der Komponente cerr
  PUBLIC :: OPERATOR(==)               ! Operator "=="
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: OPERATOR(>)                ! Operator ">"
  PUBLIC :: OPERATOR(>=)               ! Operator ">="
  PUBLIC :: OPERATOR(<)                ! Operator "<"
  PUBLIC :: OPERATOR(<=)               ! Operator "<="
  !
  PUBLIC :: no_error                   ! Pruefe ob kein Fehler vorliegt
  PUBLIC :: any_error                  ! Pruefe ob ein Fehler vorliegt
  PUBLIC :: specific_error             ! Pruefe ob ein spezifischer Fehler vorliegt
  PUBLIC :: setup_error_act            ! Setzen aller Fehlerdaten
  PUBLIC :: print_error_act            ! Drucke die aktuellen Fehlerdaten
  PUBLIC :: clear_error_act            ! Re-Initialisieren der aktuellen Fehlerdaten
  PUBLIC :: get_error_act_merr         ! Anzahl der aktuellen Fehlerzeilen
  PUBLIC :: get_error_act_ierr         ! Nummer der aktuellen Fehlermeldung
  PUBLIC :: get_error_act_cerr         ! eine Zeile der aktuellen Fehlermeldung
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'b_error' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Routine print_error_to_screen nutzen? (Default)
  LOGICAL           , PARAMETER :: c_err_warn     = .false.          ! 
  !! Anzahl der Datenkomponenten des Typs t_error
  INTEGER           , PARAMETER :: c_nofcomp      = 3                ! ggf. modifizieren
  !! Anzahl der verschiedenen Fehlermeldungen
  INTEGER           , PARAMETER :: c_nofallerrors = 15               ! ggf. modifizieren
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error)     , SAVE :: all_errors(c_nofallerrors)! 
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL            , SAVE :: initialised = .false.  ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL            , SAVE :: prn_op      = c_op     ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL            , SAVE :: trc_op      = c_op     ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER            , SAVE :: prn_lun     = c_lun    ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER            , SAVE :: trc_lun     = c_lun    ! 
  !! Routine print_error_to_screen nutzen?
  LOGICAL            , SAVE :: err_warn    = c_err_warn ! 
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe     
  INTEGER            , SAVE :: n_init      = 0        ! 
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  !! Variable zum Speichern der aktuellen Fehlersituation
  TYPE (t_error)     , SAVE :: act_err               ! 
  !! Name der Routine aus der heraus die Fehlersituation gesetzt wurde
  CHARACTER (LEN=80) , SAVE :: act_upname            ! 
  !! Name des Moduls aus dem heraus die Fehlersituation gesetzt wurde
  CHARACTER (LEN=80) , SAVE :: act_modname           ! 
  !! Inhalt einer zuletzt gelesenen Eingabezeile
  CHARACTER (LEN=132), SAVE :: act_card              ! 
  !! zur Fehlermeldung geh&ouml;rende Fortran Statusvariable
  INTEGER            , SAVE :: act_stat              ! 
  !
  ! [D.4] Schnittstellen
  !
  !! Setzen der aktuellen Fehlermeldung und -nummer
  INTERFACE setup_error_act_err
     MODULE PROCEDURE setup_error_act_err_0
     MODULE PROCEDURE setup_error_act_err_1
  END INTERFACE
  !! Setzen der zur Fehlermeldung geh&ouml;renden Fortran Statusvariable
  INTERFACE setup_error_act_stat
     MODULE PROCEDURE setup_error_act_stat_d
  END INTERFACE
  !! Setzen des zur Fehlermeldung geh&ouml;renden Modul-Namens
  INTERFACE setup_error_act_modname
     MODULE PROCEDURE setup_error_act_modname_d
  END INTERFACE
  !! Setzen des zur Fehlermeldung geh&ouml;renden Subroutine-Namens
  INTERFACE setup_error_act_upname
     MODULE PROCEDURE setup_error_act_upname_d
  END INTERFACE
  !! Setzen der zur Fehlermeldung geh&ouml;renden letzten gelesenen Datenzeile
  INTERFACE setup_error_act_card
     MODULE PROCEDURE setup_error_act_card_d
  END INTERFACE
  !! Kurzer Hinweis auf Bildschirm dass eine Fehlersituation eingetreten ist
  INTERFACE print_error_to_screen
     MODULE PROCEDURE print_error_to_screen_0
  END INTERFACE
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
  !! Allokieren/Initialisieren der statischen Daten des Moduls
  SUBROUTINE init_error_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='init_error_d' 
    !

    IF ( .NOT. initialised ) THEN
       !
       ! [1.1] Drucken des Copyright-Hinweises
       !
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_error" version 1.15 of 02/23/05'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
          WRITE(*,*)
       END IF
       !
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       !
       ! ... in diesem Basis-Modul nicht erforderlich
       !
       ! [1.3] Initialisieren der Fehlervariablen zur Speicherung
       !
       initialised  = .true.
       !
       CALL new_error ( act_err )
       !
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       !
       IF ( no_error( ) ) CALL init_error_all_errors ( ) 
       !
       ! [1.5] Initialisieren der logischen Kanalnummern
       !
       prn_lun = c_lun
       trc_lun = c_lun
       !
       ! [1.6] ggf. weitere Initialsierungsmethoden rufen
       !
       IF ( no_error( ) ) CALL setup_error_act_upname  ( 'undefiniert' )
       IF ( no_error( ) ) CALL setup_error_act_modname ( 'undefiniert' )
       IF ( no_error( ) ) CALL setup_error_act_card    ( 'undefiniert' )
       IF ( no_error( ) ) CALL setup_error_act_stat    ( 0             )
       !
       ! [1.7] Setzen des Initialisierungs-Indikators (falls erfolgreich)
       !
       initialised = MERGE( .true., .false., no_error( ) )
       !
    END IF
    !
    ! 2.0 Initialisierungszaehler heraufsetzen
    !
    n_init = n_init + 1
    !
  END SUBROUTINE init_error_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls
  SUBROUTINE clear_error_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='clear_error_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       !
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       !
       IF ( no_error( ) ) CALL clear_error_all_errors ( ) 
       !
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       !
       prn_lun = c_lun
       trc_lun = c_lun
       !
       ! [1.3] ggf. weitere De-Initialsierungsmethoden rufen
       !
       IF ( no_error( ) ) CALL clear_error_act ( )
       !
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       !
       initialised = MERGE( .false., .true., no_error( ) )
       !
       ! [1.5] ggf. weitere Module de-initialisieren
       !
       ! ... in diesem Basis-Modul nicht erforderlich
       !
    END IF
    !
    ! 2.0 Initialisierungszaehler heruntersetzen
    !
    n_init = n_init - 1
    !
  END SUBROUTINE clear_error_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden
  SUBROUTINE setup_error_prn_lun_d &
       ( lun )
    !
    ! Formalparameter
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_error_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       !
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       !
       ! ... in diesem Basis-Modul nicht erforderlich
       ! 
    END IF
    !
  END SUBROUTINE setup_error_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden
  SUBROUTINE setup_error_trc_lun_d &
       ( lun )
    !
    ! Formalparameter
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_error_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       !
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       !
       ! ... in diesem Basis-Modul nicht erforderlich
       ! 
    END IF
    !
  END SUBROUTINE setup_error_trc_lun_d
  !
  !! Setzen der logischen Variablen <EM>ERR_WARN</EM> auf .TRUE.
  SUBROUTINE setup_error_err_warn_on_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_error_err_warn_on_d'
    !
    IF ( ok_initialised ( c_upname ) ) err_warn = .TRUE.
    !
  END SUBROUTINE setup_error_err_warn_on_d
  !
  !! Setzen der logischen Variablen <EM>ERR_WARN</EM> auf .FALSE.
  SUBROUTINE setup_error_err_warn_off_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_error_err_warn_off_d'
    !
    IF ( ok_initialised ( c_upname ) ) err_warn = .FALSE.
    !
  END SUBROUTINE setup_error_err_warn_off_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar)
  SUBROUTINE new_error_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(INOUT) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='new_error_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       this%merr = 0
       this%ierr = 0
       NULLIFY ( this%cerr )
       !
    END IF
    !
  END SUBROUTINE new_error_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor)
  SUBROUTINE new_error_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_error) , INTENT(INOUT) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='new_error_1' ! 
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
          CALL new_error_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE new_error_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar)
  SUBROUTINE kill_error_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(INOUT) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='kill_error_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       IF ( no_error( ) ) CALL dealloc_error_cerr( this )
       IF ( no_error( ) ) CALL new_error( this )
       !
    END IF
    !
  END SUBROUTINE kill_error_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor)
  SUBROUTINE kill_error_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_error) , INTENT(INOUT) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='kill_error_1' ! 
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
          CALL kill_error_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE kill_error_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar)
  FUNCTION ok_error_0 &
       ( this )              &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=10), PARAMETER :: c_upname='ok_error_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       l_ok(1) = ok_error_merr( this )
       l_ok(2) = ok_error_ierr( this )
       l_ok(3) = ok_error_cerr( this )
       !
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_error_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor)
  FUNCTION ok_error_1 &
       ( this )              &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_error) , INTENT(IN) :: this(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=10), PARAMETER :: c_upname='ok_error_1' 
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
          ok(i) = ok_error_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END FUNCTION ok_error_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar)
  SUBROUTINE print_error_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='print_error_0' 
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
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_error_merr( this )
       IF ( no_error( ) ) CALL print_error_ierr( this )
       IF ( no_error( ) ) CALL print_error_cerr( this )
       !
       IF ( no_error( ) ) THEN
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8001,     & 
                 IOSTAT  = stat )
          !
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
          !
       END IF
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT('# Beginn Objekt t_error ------------------------------')
8001 FORMAT('# Ende   Objekt t_error ------------------------------')
    !
  END SUBROUTINE print_error_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor)
  SUBROUTINE print_error_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_error) , INTENT(IN) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='print_error_1' 
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
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          !
          IF ( no_error( ) ) CALL print_error_0 ( this(i) )
          !
       END DO
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.9,' ----------------------------')
    !
  END SUBROUTINE print_error_1
  !
  !! Drucken aller statischen Daten eines Moduls
  SUBROUTINE print_error_static_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='print_error_static_d' 
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
           TRIM(act_upname), TRIM(act_modname), TRIM(act_card),   &
           act_stat
       !
       IF ( stat /= 0   ) &
            CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_error ( act_err )
       !
       IF ( no_error( ) ) THEN
          !
          WRITE  &
               ( UNIT    = prn_lun, &
                 FMT     = 8001,    & 
                 IOSTAT  = stat )
          !
          IF ( stat /= 0   ) &
               CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
          !
       END IF
       !
       IF ( no_error( ) ) CALL print_error_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_error         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '# initialised = ',L1,/ &
    '#      prn_op = ',L1,/ &
    '#      trc_op = ',L1,/ &
    '#     prn_lun = ',I5,/ &
    '#     trc_lun = ',I5,/ &
    '#      n_init = ',I5,/ &
    '#  act_upname = ',A,/  &
    '# act_modname = ',A,/  &
    '#    act_card = ',A,/  &
    '#    act_stat = ',I5 )
8001 FORMAT( &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_error_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls
  SUBROUTINE print_error_all_errors_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=24), PARAMETER :: c_upname='print_error_all_errors_d' 
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
  END SUBROUTINE print_error_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "ierr" einen skalaren Wert zu (Skalar)
  SUBROUTINE set_error_ierr_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "ierr"
    INTEGER           , INTENT(IN)  :: val  ! 
    !
    this%ierr = val
    !
  END SUBROUTINE set_error_ierr_0_0
  !
  !! weise "cerr" den Wert aus einem String zu (Skalar)
  !! die Zeichen "\n" dienen als Trennzeichen zwischen
  !! einzelnen Zeilen der Fehlermeldung
  SUBROUTINE set_error_cerr_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)    
    TYPE (t_error)    , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "cerr"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='set_error_cerr_0_0' 
    !! Trennzeichen
    CHARACTER (LEN=2) , PARAMETER :: c_t='\n' ! 
    !! Hilfsfeld
    CHARACTER (LEN=LEN(this%cerr)), ALLOCATABLE :: cerr(:) ! 
    !! Anzahl der Fehlerzeilen
    INTEGER :: merr           ! 
    !! Indices
    INTEGER :: i, ia, ie, idx ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       merr = 0
       idx  = 0
       ia   = 1
       ie   = LEN( val )
       !
       DO
          !
          merr = merr + 1
          idx  = INDEX( val(ia:ie), c_t )
          !
          IF ( idx == 0 ) EXIT
          !
          ia = MIN( ia+idx+1,ie )
          !
       END DO
       !
       ALLOCATE ( cerr(merr) )
       !
       ia = 1
       !
       DO i=1,merr
          !
          idx = INDEX( val(ia:ie), c_t        )
          !
          IF ( idx == 0 ) THEN
             cerr(i) = TRIM( val(ia:ie) )
          ELSE
             cerr(i) = TRIM( val(ia:ia+idx-2) )
          END IF
          !
          ia = MIN( ia+idx+1,ie )
          !
       END DO
       !
       CALL set_error_cerr_0_1 ( this, cerr )
       !
       DEALLOCATE ( cerr )
       !
    END IF
    !
  END SUBROUTINE set_error_cerr_0_0
  !
  !! weise "cerr" den Wert aus einem Feld "val" zu (Skalar)
  SUBROUTINE set_error_cerr_0_1 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)    
    TYPE (t_error)    , INTENT(INOUT) :: this   ! 
    !! Wert f&uuml;r Komponente "cerr"
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='set_error_cerr_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       IF ( no_error( ) ) CALL dealloc_error_cerr ( this )
       IF ( no_error( ) ) this%merr = SIZE( val )
       IF ( no_error( ) ) CALL alloc_error_cerr   ( this )
       IF ( no_error( ) ) CALL init_error_cerr    ( this )
       IF ( no_error( ) ) this%cerr(:) = val(:)
       !
    END IF
    !
  END SUBROUTINE set_error_cerr_0_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "merr" aus einem skalaren Datenobjekt
  FUNCTION get_error_merr_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "merr" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%merr
    !
  END FUNCTION get_error_merr_0
  !
  !! hole die Komponente "ierr" aus einem skalaren Datenobjekt
  FUNCTION get_error_ierr_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "ierr" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%ierr
    !
  END FUNCTION get_error_ierr_0
  !
  !! hole die Komponente "cerr" aus einem skalaren Datenobjekt
  FUNCTION get_error_cerr_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(IN)  :: this  ! 
    !! R&uuml;ckgabewert "cerr"
    CHARACTER (LEN=c_len_error_cerr) :: val(this%merr)  ! 
    !
    val = this%cerr(:)
    !
  END FUNCTION get_error_cerr_0
  !
  !! hole die i-te Zeile der Komponente "cerr" aus einem skalaren Datenobjekt
  FUNCTION get_error_cerr_0_i &
       ( this , &
         i    ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this ! 
    !! Zeilennummer der Fehlermeldung
    INTEGER        , INTENT(IN) :: i    ! 
    !! R&uuml;ckgabewert "cerr(i)" 
    CHARACTER (LEN=c_len_error_cerr) :: val  ! 
    !
    IF ( i >= 1 .AND. i <= this%merr ) THEN
       val = this%cerr(i)
    ELSE
       val = REPEAT( ' ', LEN(val) )
       val = 'undefined'
    END IF
    !
  END FUNCTION get_error_cerr_0_i
  !
  !! Anzahl der in der aktuellen Fehlermeldung abgelegten Fehler ermitteln <BR>
  !! falls kein Fehler vorhanden ist, so wird Null zur&uuml;ckgegeben
  FUNCTION get_error_act_merr_d ( ) &
       RESULT( res )
    !! Ergebniswert: Anzahl der Zeilen in der aktuellen Fehlermeldung
    INTEGER :: res ! 
    !
    IF ( any_error( ) ) THEN
       res = get_error_merr( act_err )
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_error_act_merr_d
  !
  !! Nummer des in der aktuellen Fehlermeldung abgelegten Fehlers ermitteln <BR>
  !! falls kein Fehler vorhanden ist, so wird Null zur&uuml;ckgegeben
  FUNCTION get_error_act_ierr_d ( ) &
       RESULT( res )
    !! Ergebniswert: Anzahl der Zeilen in der aktuellen Fehlermeldung
    INTEGER :: res ! 
    !
    IF ( any_error( ) ) THEN
       res = get_error_ierr( act_err )
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_error_act_ierr_d
  !
  !! eine Zeile des in der aktuellen Fehlermeldung abgelegten Textes ermitteln <BR>
  !! falls kein Fehler vorhanden ist oder eine falsche Zeile ausgegeben werden
  !! soll, so wird "undefined" zur&uuml;ckgegeben
  FUNCTION get_error_act_cerr_i ( var ) &
       RESULT( res )
    !! Nummer der gesuchten Zeile des Fehlertextes
    INTEGER , INTENT(IN) :: var ! 
    !! Ergebniswert: Zeile der Fehlermeldung
    CHARACTER (LEN=c_len_error_cerr) :: res ! 
    !
    IF ( any_error( ) ) THEN
       res = get_error_cerr( act_err, var )
    ELSE
       res = REPEAT( ' ', LEN(res) ) ; res = 'undefined' ! 
    END IF
    !
  END FUNCTION get_error_act_cerr_i
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar )
  FUNCTION eq_error_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = eq_error_merr ( this1, this2 )
    l_ok(2) = eq_error_ierr ( this1, this2 )
    l_ok(3) = eq_error_cerr ( this1, this2 )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_error_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar )
  FUNCTION eq_error_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_error_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_error_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor )
  FUNCTION eq_error_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_error_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_error_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor )
  FUNCTION eq_error_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_error_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_error_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Skalar )
  FUNCTION gt_error_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = gt_error_ierr ( this1, this2 )
    !
  END FUNCTION gt_error_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Skalar )
  FUNCTION gt_error_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_error_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION gt_error_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Vektor )
  FUNCTION gt_error_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_error_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION gt_error_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Vektor )
  FUNCTION gt_error_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_error_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION gt_error_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Skalar )
  FUNCTION ge_error_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ge_error_ierr ( this1, this2 )
    !
  END FUNCTION ge_error_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Skalar )
  FUNCTION ge_error_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_error_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ge_error_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Vektor )
  FUNCTION ge_error_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_error_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ge_error_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Vektor )
  FUNCTION ge_error_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_error_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ge_error_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Skalar )
  FUNCTION lt_error_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = lt_error_ierr ( this1, this2 )
    !
  END FUNCTION lt_error_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Skalar )
  FUNCTION lt_error_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_error_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION lt_error_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Vektor )
  FUNCTION lt_error_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_error_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION lt_error_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Vektor )
  FUNCTION lt_error_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_error_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION lt_error_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Skalar )
  FUNCTION le_error_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    ok = le_error_ierr ( this1, this2 )
    !
  END FUNCTION le_error_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Skalar )
  FUNCTION le_error_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_error_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION le_error_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Vektor )
  FUNCTION le_error_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_error_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION le_error_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Vektor )
  FUNCTION le_error_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_error) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_error_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION le_error_1_1
  !
  !! Pr&uuml;fe ob keine Fehlerbedingung vorliegt
  FUNCTION no_error_d &
       ( )          &
       RESULT( ok )
    !
    ! Formalparameter
    ! Rueckgabewert
    !! Testergebnis auf nicht vorliegende Fehlerbedingung
    LOGICAL :: ok ! 
    !
    ok = MERGE ( .true., .false., act_err%ierr == 0 )
    !
  END FUNCTION no_error_d
  !
  !! Pr&uuml;fe ob eine Fehlerbedingung vorliegt
  FUNCTION any_error_d &
       ( )          &
       RESULT( ok )
    !
    ! Formalparameter
    ! Rueckgabewert
    !! Testergebnis auf vorliegende Fehlerbedingung
    LOGICAL :: ok ! 
    !
    ok = .NOT. no_error( )
    !
  END FUNCTION any_error_d
  !
  !! Pr&uuml;fe ob eine spezifische Fehlerbedingung vorliegt
  FUNCTION specific_error_d &
       ( ierr )             &
       RESULT( ok )
    !
    ! Formalparameter
    !! spezifische Fehlernummer
    INTEGER , INTENT(IN) :: ierr ! 
    ! Rueckgabewert
    !! Testergebnis auf vorliegende Fehlerbedingung
    LOGICAL :: ok ! 
    !
    ok = ( get_error_ierr ( act_err ) == ierr )
    !
  END FUNCTION specific_error_d
  !
  !! Transferiere Fehlersituation (Skalar) in statische Moduldaten
  SUBROUTINE setup_error_act_0 &
       ( this,                 &
         upname,               &
         modname,              &
         stat,                 &
         card )
    !
    ! Formalparameter
    !! Objekt mit Fehlerdaten (Skalar)
    TYPE (t_error)              , INTENT(IN) :: this    ! 
    !! Subroutine-Name der Programmeinheit in welcher der Fehler auftrat
    CHARACTER (LEN=*)           , INTENT(IN) :: upname  ! 
    !! Modul-Name in welchem der Fehler auftrat
    CHARACTER (LEN=*)           , INTENT(IN) :: modname ! 
    !! (optional) Fehlerstatus
    INTEGER           , OPTIONAL, INTENT(IN) :: stat    ! 
    !! (Optional) Inhalt der zuletzt gelesenen Datenzeile
    CHARACTER (LEN=*) , OPTIONAL, INTENT(IN) :: card    ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='setup_error_act_0'
    !
    !$OMP critical (setup_err_act)
    !
    CALL print_error_act ( c_upname ) ! evtl. Drucken einer vorhandenen FM
    CALL clear_error_act ( )          ! Loeschen des alten Fehlers
    CALL setup_error_act_err_0     ( this    )
    CALL setup_error_act_upname_d  ( upname  )
    CALL setup_error_act_modname_d ( modname )
    !
    IF ( PRESENT( stat ) ) CALL setup_error_act_stat_d ( stat )
    IF ( PRESENT( card ) ) CALL setup_error_act_card_d ( card )
    !
    CALL print_error_to_screen ( upname, modname, this%ierr ) ! Kurznachricht auf Std-IO
    !
    !$OMP end critical (setup_err_act)
    !
  END SUBROUTINE setup_error_act_0
  !
  !! Transferiere Fehlersituation (Vekor) in statische Moduldaten
  SUBROUTINE setup_error_act_1 &
       ( this,                 &
         ierr,                 &
         upname,               &
         modname,              &
         stat,                 &
         card )
    !
    ! Formalparameter
    !! Objekt mit vielen Fehlerdaten (Vektor)
    TYPE (t_error)              , INTENT(IN) :: this(:) ! 
    !! Nummer des Fehlers der gesetzt werden soll
    INTEGER                     , INTENT(IN) :: ierr    ! 
    !! Subroutine-Name der Programmeinheit in welcher der Fehler auftrat
    CHARACTER (LEN=*)           , INTENT(IN) :: upname  ! 
    !! Modul-Name in welchem der Fehler auftrat
    CHARACTER (LEN=*)           , INTENT(IN) :: modname ! 
    !! (optional) Fehlerstatus
    INTEGER           , OPTIONAL, INTENT(IN) :: stat    ! 
    !! (Optional) Inhalt der zuletzt gelesenen Datenzeile
    CHARACTER (LEN=*) , OPTIONAL, INTENT(IN) :: card    ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='setup_error_act_1'
    !
    !$OMP critical (setup_err_act)
    !
    CALL print_error_act ( c_upname ) ! evtl. Drucken einer vorhandenen FM
    CALL clear_error_act ( )          ! Loeschen des alten Fehlers
    CALL setup_error_act_err_1     ( this(:), ierr )
    CALL setup_error_act_upname_d  ( upname        )
    CALL setup_error_act_modname_d ( modname       )
    !
    IF ( PRESENT( stat ) ) CALL setup_error_act_stat_d ( stat )
    IF ( PRESENT( card ) ) CALL setup_error_act_card_d ( card )
    !
    CALL print_error_to_screen ( upname, modname, ierr ) ! Kurznachricht auf Std-IO
    !
    !$OMP end critical (setup_err_act)
    !
  END SUBROUTINE setup_error_act_1
  !
  !! Transferiere Fehlersituation (CERR und IERR) in statische Moduldaten
  SUBROUTINE setup_error_act_c &
       ( ierr,                 &
         cerr,                 &
         upname,               &
         modname,              &
         stat,                 &
         card )
    !
    ! Formalparameter
    !! Fehlernummer
    INTEGER                     , INTENT(IN) :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=*)           , INTENT(IN) :: cerr(:) ! 
    !! Subroutine-Name der Programmeinheit in welcher der Fehler auftrat
    CHARACTER (LEN=*)           , INTENT(IN) :: upname  ! 
    !! Modul-Name in welchem der Fehler auftrat
    CHARACTER (LEN=*) , OPTIONAL, INTENT(IN) :: modname ! 
    !! (optional) Fehlerstatus
    INTEGER           , OPTIONAL, INTENT(IN) :: stat    ! 
    !! (Optional) Inhalt der zuletzt gelesenen Datenzeile
    CHARACTER (LEN=*) , OPTIONAL, INTENT(IN) :: card    ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='setup_error_act_c'
    !! Hilfsvariable
    TYPE (t_error)                 :: this ! 
    !
    !$OMP critical (setup_err_act)
    !
    CALL print_error_act ( c_upname ) ! evtl. Drucken einer vorhandenen FM
    CALL clear_error_act ( )          ! Loeschen des alten Fehlers
    !
    CALL new_error ( this )
    CALL set_error_ierr ( this, ierr    )
    CALL set_error_cerr ( this, cerr(:) )
    !
    IF ( no_error( ) ) THEN
       !
       CALL setup_error_act_err_0     ( this    )
       CALL setup_error_act_upname_d  ( upname  )
       !
       IF ( PRESENT( modname ) ) CALL setup_error_act_modname_d ( modname )
       IF ( PRESENT( stat    ) ) CALL setup_error_act_stat_d    ( stat    )
       IF ( PRESENT( card    ) ) CALL setup_error_act_card_d    ( card    )
       !
       IF ( PRESENT(modname) ) THEN
          CALL print_error_to_screen ( upname, modname, ierr )     ! Kurznachricht auf Std-IO
       ELSE
          CALL print_error_to_screen ( upname, 'undefined', ierr ) ! Kurznachricht auf Std-IO
       END IF
       !
       CALL kill_error ( this )
       !
    END IF
    !
    !$OMP end critical (setup_err_act)
    !
  END SUBROUTINE setup_error_act_c
  !
  !! Ersetzen eines Platzhalter-Strings in der aktuellen Fehlermeldung
  SUBROUTINE setup_error_act_r &
       ( p,                    &
         r )
    !
    ! Formalparameter
    !! Platzhalter-String
    CHARACTER (LEN=*) , INTENT(IN) :: p ! 
    !! String mit Ersatz f&uuml;r Platzhalter
    CHARACTER (LEN=*) , INTENT(IN) :: r ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='setup_error_act_r'
    !! lokaler String
    CHARACTER (LEN=c_len_error_cerr) :: l_cerr ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Positionsindices
    INTEGER :: nf, nl, nn, mm, nr ! 
    !
    !$OMP critical (setup_err_act)
    !
    IF ( act_err%merr > 0 ) THEN
       !
       i  = 0
       nr = 0
       !
       DO 
          !
          i = i + 1
          !
          IF ( i > act_err%merr ) EXIT
          !
          nf =    INDEX ( act_err%cerr(i), p )
          nl =    nf + LEN( p ) - 1
          nn = LEN_TRIM ( act_err%cerr(i)    )
          !
          IF ( nf > 0 ) THEN
             !
             l_cerr = REPEAT( ' ', LEN( l_cerr ) )
             mm     = 1
             nr     = nr + 1
             !
             IF ( nf > 1 ) THEN
                l_cerr = act_err%cerr(i)(1:nf-1)
                mm     = nf
             END IF
             !
             l_cerr(mm:) = r
             mm          = mm + LEN(r)
             !
             IF ( nl < nn ) THEN
                !
                l_cerr(mm:) = act_err%cerr(i)(nl+1:nn)
                mm          = mm + (nl-nn+2)
                !
             END IF
             !
             act_err%cerr(i) = l_cerr
             !
             i = 0 ! Ruecksetzen bis alle Platzhalter ersetzt sind
             !
          END IF
          !
       END DO
       !
       IF ( nr == 0 ) THEN
          !
          WRITE(*,*) ' *** '//TRIM( c_upname )
          WRITE(*,*) ' *** Platzhalter = '//TRIM( p )
          WRITE(*,*) ' *** Platzhalter wurde nicht gefunden '
          !
       END IF
       !
    ELSE
       !
       WRITE(*,*) ' *** '//TRIM( c_upname )
       WRITE(*,*) ' *** noch kein aktueller Fehler definiert '
       !
    END IF
    !
    !$OMP end critical (setup_err_act)
    !
  END SUBROUTINE setup_error_act_r
  !
  !! Drucken der in statischen Moduldaten abgelegten aktuellen Fehlersituation
  SUBROUTINE print_error_act_d &
       ( upname )
    !
    ! Formalparameter
    !! Name des "print_error_act" rufenden Unterprogramms
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_error_act_d'
    !! Statsuvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( any_error( ) ) THEN
       !
       IF ( prn_op ) THEN
          !
          WRITE                      &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     & 
                 IOSTAT  = stat )    &
                 TRIM(upname), TRIM(act_upname), TRIM(act_modname), &
                 act_stat, TRIM(act_card), get_error_ierr( act_err )
          !
          IF ( stat == 0 ) THEN
             !
             i = 0
             !
             DO
                !
                i = i + 1
                !
                IF ( i > get_error_merr( act_err ) .OR. stat /= 0 ) EXIT
                !
                WRITE                      &
                     ( UNIT    = prn_lun,  &
                       FMT     = 8001,     & 
                       IOSTAT  = stat ) i, TRIM( get_error_cerr( act_err, i ) )
                !
             END DO
             !
             IF ( stat == 0 ) THEN
                !
                WRITE                      &
                     ( UNIT    = prn_lun,  &
                       FMT     = 8002,     & 
                       IOSTAT  = stat )
                !
             END IF
             !
          END IF
          !
          IF ( stat /= 0 ) THEN
             WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - Fehler bei Druckausgabe '
             WRITE(*,*) ' >>> Fortran-Statusvariable = ',stat
          END IF
          !
       ELSE
          !
          WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
          !
       END IF
       !
    END IF
    !
8000 FORMAT(&
          '# ---------------------------------------------------------------',/&
          '# *** PRINT_ERROR_ACT - aktuelle Fehlermeldung',/&
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/&
          '#      wurde gerufen von : ',A,/&
          '#      ... initiiert von : ',A,/&
          '#      ...      in Modul : ',A,/&
          '# Fortran-Statusvariable : ',I10.9,/&
          '#           letzte Zeile : ',A,/&
          '#           Fehlernummer : ',I10.9)
8001 FORMAT(&
          '#        Zeile ',I2.2,' , Text : ',A)
8002 FORMAT(&
          '# ---------------------------------------------------------------')
    !
  END SUBROUTINE print_error_act_d
  !
  !! Re-Initialisieren der statischen Moduldaten zur Aufnahme einer Fehlersituation.
  SUBROUTINE clear_error_act_d &
       ( )
    !
    ! Formalparameter
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='clear_error_act_d'
    !
    act_err%ierr = 0 ! 
    !
    CALL kill_error ( act_err )
    !
    CALL setup_error_act_upname  ( 'undefiniert' )
    CALL setup_error_act_modname ( 'undefiniert' )
    CALL setup_error_act_card    ( 'undefiniert' )
    CALL setup_error_act_stat    ( 0             )
    !
  END SUBROUTINE clear_error_act_d
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
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert
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
    CHARACTER (LEN=c_len_error_cerr) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       !
       WRITE(*,*) ' *** Warnung *** Modul "b_error" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_error ausfuehren'
       !
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
       !
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert
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
    IF ( .NOT. ok ) &
         CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls
  SUBROUTINE init_error_all_errors &
       ( )
    !
    CALL new_error( all_errors(:) )
    !
    ! Obacht auf "c_nofallerrors" geben
    !
    ! Index 001
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 1), 1 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 1), &
         'Fehlerkategorie: ALLGEMEIN\n'//&
         'Modul ist nicht initialisiert\n'//&
         '--> INIT_error ausfuehren')
    ! Index 002
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 2), 2 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 2), &
         'Fehlerkategorie: ALLGEMEIN\n'//&
         'Modul ist schon initialisiert\n'//&
         '--> CLEAR_error ausfuehren')
    ! Index 003
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 3), 5030 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 3), &
         'Fehlerkategorie: KILL-Methoden\n'//&
         'De-Allocate-Fehler fuer Komponente von "t_error"\n'//&
         'Typ-Komponente = "cerr"\n'//&
         '--> Code in Modul "b_error" pruefen')
    ! Index 004
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 4), 6010 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 4), &
         'Fehlerkategorie: OK-Methoden\n'//       &
         'Fehler in Komponente von "t_error"\n'// &
         'Typ-Komponente = "merr"\n'//            &
         '--> Daten pruefen, ggf. PRINT-Methode verwenden')
    ! Index 005
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 5), 6020 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 5), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_error"\n'//&
         'Typ-Komponente = "ierr"\n'//&
         '--> Daten pruefen, ggf. PRINT-Methode verwenden')
    ! Index 006
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 6), 6030 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 6), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_error"\n'//&
         'Typ-Komponente = "cerr"\n'//&
         '--> Daten pruefen, ggf. PRINT-Methode verwenden')
    ! Index 007
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 7), 7001 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 7), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken der Kopfzeilen\n'//&
         '--> Code in Modul "b_error" pruefen')
    ! Index 008
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 8), 7002 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 8), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken des Fusszeilen\n'//&
         '--> Code in Modul "b_error" pruefen')
    ! Index 009
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors( 9), 7003 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors( 9), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken des Index des Datenobjektes (1D-Array)\n'//&
         '--> Code in Modul "b_error" pruefen')
    ! Index 010
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors(10), 7010 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors(10), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_error"\n'//&
         'Typ-Komponente = "merr"\n'//&
         '--> Code in Modul "b_error" / Daten pruefen')
    ! Index 011
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors(11), 7020 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors(11), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_error"\n'//&
         'Typ-Komponente = "ierr"\n'//&
         '--> Code in Modul "b_error" / Daten pruefen')
    ! Index 012
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors(12), 7030 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors(12), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_error"\n'//&
         'Typ-Komponente = "cerr\n"'//&
         '--> Code in Modul "b_error" / Daten pruefen')
    ! Index 013
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors(13), 7500 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors(13), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken statischer Daten aus "b_error"\n'//&
         '--> Code in Modul "b_error" / Daten pruefen')
    ! Index 014
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors(14), 8030 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors(14), &
         'Fehlerkategorie: SET-Methoden\n'//&
         'Allocate-Fehler fuer Komponente von "t_error"\n'//&
         'Typ-Komponente = "cerr"\n'//&
         '--> Code in Modul "b_error" pruefen')
    ! Index 015
    IF ( no_error ( ) ) CALL set_error_ierr ( all_errors(15), -2000 )
    IF ( no_error ( ) ) CALL set_error_cerr ( all_errors(15), &
         'Fehlerkategorie: (Modul-) SETUP-Methoden\n'//&
         'Gewuenschte Fehlernummer "ierr" nicht in "all_errors(:)" enthalten\n'//&
         '--> Code in Modul "b_error" pruefen')
    !
  END SUBROUTINE init_error_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls
  SUBROUTINE clear_error_all_errors &
       ( )
    !
    CALL kill_error( all_errors(:) )
    !
  END SUBROUTINE clear_error_all_errors
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
  !! De-Allokieren der Komponente "cerr"
  SUBROUTINE dealloc_error_cerr &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(INOUT) :: this ! 
    !
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_error_merr' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%cerr ) ) THEN
       !
       DEALLOCATE ( this%cerr, STAT=stat )
       !
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 5030, c_upname, c_modname, stat )
       !
       this%merr = 0
       NULLIFY ( this%cerr )
       !
    END IF
    !
  END SUBROUTINE dealloc_error_cerr
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "merr" eines Datenobjektes o.k. ist
  FUNCTION ok_error_merr &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Function
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_error_merr' ! 
    !
    ok = ( this%merr > 0 )
    !
    IF ( .NOT. ok ) &
         CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
  END FUNCTION ok_error_merr
  !
  !! Pr&uuml;fe, ob die Komponente "ierr" eines Datenobjektes o.k. ist
  FUNCTION ok_error_ierr &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Function
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_error_ierr' ! 
    !
    ok = ( this%ierr > -99999 .AND. this%ierr < +99999 )
    !
    IF ( .NOT. ok ) &
         CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
  END FUNCTION ok_error_ierr
  !
  !! Pr&uuml;fe, ob die Komponente "cerr" eines Datenobjektes o.k. ist
  FUNCTION ok_error_cerr &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Function
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_error_cerr' ! 
    !
    ok = ASSOCIATED( this%cerr )
    !
    IF ( .NOT. ok ) &
         CALL setup_error_act( all_errors(:), 6030, c_upname, c_modname )
    !
  END FUNCTION ok_error_cerr
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "merr" eines Datenobjektes
  SUBROUTINE print_error_merr &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_error_merr' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%merr
    !
    IF ( stat /= 0 ) &
         CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente merr  - - - - - - - - - - ',/&
           '# wert = ',I10.9,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_error_merr
  !
  !! Drucke den Inhalt der Komponente "ierr" eines Datenobjektes
  SUBROUTINE print_error_ierr &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_error_ierr' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%ierr
    !
    IF ( stat /= 0 ) &
         CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente ierr  - - - - - - - - - - ',/&
           '# wert = ',I10.9,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_error_ierr
  !
  !! Drucke den Inhalt der Komponente "cerr" eines Datenobjektes
  SUBROUTINE print_error_cerr &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_error_cerr' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i    ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) 
    !
    IF ( stat /= 0 ) &
         CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
    DO i=1,this%merr
       !
       WRITE &
            ( UNIT    = prn_lun,  &
              FMT     = 8001,     &
              IOSTAT  = stat ) i, TRIM(this%cerr(i))
       !
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
       !
    END DO
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8002,     & 
           IOSTAT  = stat ) 
    !
    IF ( stat /= 0 ) &
         CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente cerr  - - - - - - - - - - ')
8001 FORMAT &
          ('# Zeile = ',I2.2,' , wert = ',A)
8002 FORMAT &
           ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_error_cerr
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der Komponente "cerr"
  SUBROUTINE alloc_error_cerr &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(INOUT) :: this   ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_error_cerr' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    NULLIFY ( this%cerr )
    !
    IF ( this%merr > 0 ) THEN
       !
       ALLOCATE( this%cerr(this%merr), STAT=stat )
       !
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 8030, c_upname, c_modname, stat )
       !
    END IF
    !
  END SUBROUTINE alloc_error_cerr
  !
  !! Initialisieren der Komponente "cerr" mit Default-Werten
  SUBROUTINE init_error_cerr &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_error) , INTENT(INOUT) :: this   ! 
    !
    IF ( ASSOCIATED(this%cerr) ) this%cerr = REPEAT( ' ', LEN( this%cerr ) )
    !
  END SUBROUTINE init_error_cerr
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "merr" zweier Datenobjekte auf Gleichheit
  FUNCTION eq_error_merr &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%merr == this2%merr )
    !
  END FUNCTION eq_error_merr
  !
  !! pr&uuml;fe Komponente "ierr" zweier Datenobjekte auf Gleichheit
  FUNCTION eq_error_ierr &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%ierr == this2%ierr )
    !
  END FUNCTION eq_error_ierr 
  !
  !! pr&uuml;fe Komponente "cerr" zweier Datenobjekte auf Gleichheit
  FUNCTION eq_error_cerr &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ok = MERGE( .true., .false., this1%merr == this2%merr )
    i  = 0
    !
    DO
       !
       i = i + 1
       !
       IF ( i > MIN(this1%merr,this2%merr) .OR. .NOT. ok ) EXIT
       !
       ok = ( this1%cerr(i) == this2%cerr(i) )
       !
    END DO
    !
  END FUNCTION eq_error_cerr 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "ierr" zweier Datenobjekte auf ">"
  FUNCTION gt_error_ierr &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%ierr > this2%ierr )
    !
  END FUNCTION gt_error_ierr 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "ierr" zweier Datenobjekte auf ">="
  FUNCTION ge_error_ierr &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%ierr >= this2%ierr )
    !
  END FUNCTION ge_error_ierr 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "ierr" zweier Datenobjekte auf "<"
  FUNCTION lt_error_ierr &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%ierr < this2%ierr )
    !
  END FUNCTION lt_error_ierr 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "ierr" zweier Datenobjekte auf "<="
  FUNCTION le_error_ierr &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_error) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%ierr <= this2%ierr )
    !
  END FUNCTION le_error_ierr 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-modulspezifische-SETUP-Methoden <<< [ERR_NO = -2999 bis -2000]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der zur Fehlermeldung geh&ouml;renden Statusvariablen
  SUBROUTINE setup_error_act_stat_d &
       ( stat )
    !
    ! Formalparameter
    !! Statusvariable
    INTEGER , INTENT(IN) :: stat ! 
    !
    act_stat = stat
    !
  END SUBROUTINE setup_error_act_stat_d
  !
  !! Setzen des Modul-Namens aus dem heraus die Fehlerbedingung gesetzt wurde
  SUBROUTINE setup_error_act_modname_d &
       ( modname )
    !
    ! Formalparameter
    !! Modulname
    CHARACTER (LEN=*) , INTENT(IN) :: modname ! 
    !
    act_modname = REPEAT( ' ', LEN(act_modname) )
    !LEO BUG removed: act_modname = MERGE ( modname, 'undefiniert', LEN_TRIM(modname) > 0 )
    !Leo replaced:
    IF (LEN_TRIM(modname) .GT. 0) THEN
      act_upname = TRIM(modname)
    ELSE
      act_upname = 'undefiniert'
    END IF
    !
  END SUBROUTINE setup_error_act_modname_d
  !
  !! Setzen des Subrotine-Namens aus dem heraus die Fehlerbedingung gesetzt wurde
  SUBROUTINE setup_error_act_upname_d &
       ( upname )
    !
    ! Formalparameter
    !! Subroutine-Name
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !
    act_upname = REPEAT( ' ', LEN(act_upname) )
    !LEO there is somewhere a bug since len(act_upname is not equal to
    !LEO undefiniert, per default MERGE objects should have the same size!
    !LEO original code :
    !act_upname = MERGE ( upname, 'undefiniert', LEN_TRIM(upname) > 0 )
    !LEO workaround:
    IF (LEN_TRIM(upname) .GT. 0) THEN
      act_upname = TRIM(upname)
    ELSE
      act_upname = 'undefiniert'
    END IF
    !
  END SUBROUTINE setup_error_act_upname_d
  !
  !! Setzen des Inhalts der letzten Datenzeile als die Fehlerbedingung eintrat
  SUBROUTINE setup_error_act_card_d &
       ( card )
    !
    ! Formalparameter
    !! Inhalt der zuletzt gelesenen Datenzeile
    CHARACTER (LEN=*) , INTENT(IN) :: card ! 
    !
    act_card = REPEAT( ' ', LEN(act_card) )
    !LEO removed act_card = MERGE ( card, 'undefiniert', LEN_TRIM(card) > 0 )
    !LEO replaced
    IF (LEN_TRIM(card) .GT. 0) THEN
      act_upname = TRIM(card)
    ELSE
      act_upname = 'undefiniert'
    END IF
    !
  END SUBROUTINE setup_error_act_card_d
  !
  !! Setzen des Inhalts von act_err (Skalar)
  SUBROUTINE setup_error_act_err_0 &
       ( this )
    !
    ! Formalparameter
    !! Objekt mit aktueller Fehlersituation
    TYPE (t_error) , INTENT(IN) :: this ! 
    !
    act_err%ierr = 0
    !
    CALL kill_error ( act_err )
    CALL new_error  ( act_err )
    !
    CALL set_error_cerr ( act_err, this%cerr )
    CALL set_error_ierr ( act_err, this%ierr )
    !
  END SUBROUTINE setup_error_act_err_0
  !
  !! Setzen des Inhalts von act_err (Vektor)
  SUBROUTINE setup_error_act_err_1 &
       ( this, &
         ierr )
    !
    ! Formalparameter
    !! Objekt mit vielen Fehlersituation
    TYPE (t_error) , INTENT(IN) :: this(:) ! 
    !! Nummer des Fehlers der gesetzt werden soll
    INTEGER        , INTENT(IN) :: ierr    ! 
    !
    !! Name der Subroutine
    CHARACTER (LEN=21) , PARAMETER :: c_upname='setup_error_act_err_1' ! 
    !! Z&auml;hler
    INTEGER :: i, ii
    !
    i  = 0
    ii = 0
    !
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. ii > 0 ) EXIT
       ii = MERGE( i, 0, this(i)%ierr == ierr )
    END DO
    !
    IF ( ii > 0 ) THEN
       CALL setup_error_act_err_0 ( this(ii) )
    ELSE
       WRITE(*,*) ' *** keine Fehlermeldung fuer IERR = ',ierr
       CALL setup_error_act ( all_errors(:), -2000, c_upname, c_modname )
    END IF
    !
  END SUBROUTINE setup_error_act_err_1
  !
  SUBROUTINE print_error_to_screen_0 &
       ( upname, modname, ierr )
    !! Name der Programmeinheit in welcher der Fehler auftrat
    CHARACTER (LEN=*) , INTENT(IN) :: upname  ! 
    !! Name des Moduls in welcher diese Programmeinheit existiert
    CHARACTER (LEN=*) , INTENT(IN) :: modname ! 
    !! Fehlerstatus
    INTEGER           , INTENT(IN) :: ierr    ! 
    !
    IF ( err_warn ) THEN
       !
       WRITE(*,*) ' *** notice, an error message was generated *** '
       WRITE(*,*) ' ... error flag (ierr)   : ',ierr
       WRITE(*,*) ' ... function/subroutine : '//TRIM(upname)
       WRITE(*,*) ' ... module              : '//TRIM(modname)
       !
    END IF
    !
  END SUBROUTINE print_error_to_screen_0
  !
END MODULE b_error
! TailOfBaseModule --------------------------------------------------------
