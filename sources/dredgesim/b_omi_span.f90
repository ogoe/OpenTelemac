! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden analog zu OpenMI-Interface <EM>ITimeSpan</EM></h2>
!! @author G. Lang
!! @version 2.1 vom 07/21/05, Quellcode: mod_b_omi_span.f90
!! <HR>
!! type and methods equivalent to OpenMI-interface <EM>ITimeSpan</EM> <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-01-31 : G. Lang : Erstversion
!  01.02 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>ITimeSpan</EM>. Dient
!! dazu, einen Zeitraum (Anfang und Ende) zu beschreiben. 
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_span";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_span";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_span";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_span";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_span";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_span".
!! </OL>
!!                                                         
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_omi_span 
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> start : Beginn des Zeitraums als mod. julianische Datumsangabe
!!     <LI> end : Ende des Zeitraums als mod. julianische Datumsangabe
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
!!    <LI> Initialisieren des Moduls b_omi_span mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_span mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_SPAN_ALL_ERRORS.
!!                                                                    <BR>
!
MODULE b_omi_span
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Single, &
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
       setup_datetime_trc_lun
  !
  ! [A.4] BASIS-Modul mit Typ und Methoden mod. Julianische Zeitangabe
  !
  USE b_omi_stamp, ONLY :          &
       !   Typdefinitionen
       t_omi_stamp,                &
       !   Routinen / Interfaces
       init_omi_stamp,             &
       clear_omi_stamp,            &
       setup_omi_stamp_prn_lun,    &
       setup_omi_stamp_trc_lun,    &
       new_omi_stamp,              &
       kill_omi_stamp,             &
       ok_omi_stamp,               &
       print_omi_stamp,            &
       set_omi_stamp_modjulianday, &
       get_omi_stamp_datetime,     &
       get_omi_stamp_modjulianday, &
       eq_omi_stamp,               &
       gt_omi_stamp,               &
       ge_omi_stamp,               &
       le_omi_stamp,               &
       lt_omi_stamp
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
  !! start : Beginn des Zeitraums als mod. julianische Datumsangabe <BR>
  !! end   : Ende des Zeitraums als mod. julianische Datumsangabe
  TYPE , PUBLIC :: t_omi_span
     PRIVATE
     TYPE (t_omi_stamp) :: start
     TYPE (t_omi_stamp) :: end
  END TYPE t_omi_span
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_omi_span
     MODULE PROCEDURE init_omi_span_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_span
     MODULE PROCEDURE clear_omi_span_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_span_prn_lun
     MODULE PROCEDURE setup_omi_span_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_span_trc_lun
     MODULE PROCEDURE setup_omi_span_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_span" (Skalar, 1D-Array) und
  !! Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE new_omi_span
     MODULE PROCEDURE new_omi_span_0  ! 
     MODULE PROCEDURE new_omi_span_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_span" (Skalar, 1D-Array) und
  !! teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE kill_omi_span
     MODULE PROCEDURE kill_omi_span_0 ! 
     MODULE PROCEDURE kill_omi_span_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_span" auf G&uuml;ltigkeit: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE ok_omi_span
     MODULE PROCEDURE ok_omi_span_0 ! 
     MODULE PROCEDURE ok_omi_span_1 ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_span": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_span" auf <EM>PRN_LUN</EM> ausgeben.
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_span
     MODULE PROCEDURE print_omi_span_0 ! 
     MODULE PROCEDURE print_omi_span_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_span_static
     MODULE PROCEDURE print_omi_span_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_span_all_errors
     MODULE PROCEDURE print_omi_span_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "start" in "t_omi_span" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! c) f&uuml;r ein Datenobjekt (Skalar) und eine Zeitangabe "t_datetime" <BR>
  !! d) f&uuml;r viele Datenobjekte (Vektor) und eine Zeitangabe "t_datetime" <BR>
  !! e) f&uuml;r ein Datenobjekt (Skalar) und eine Zeitangabe REAL(Double) <BR>
  !! f) f&uuml;r viele Datenobjekte (Vektor) und eine Zeitangabe REAL(Double)
  INTERFACE set_omi_span_start
     MODULE PROCEDURE set_omi_span_start_0_0 ! 
     MODULE PROCEDURE set_omi_span_start_1_0 ! 
     MODULE PROCEDURE set_omi_span_start_datetime_0_0 ! 
     MODULE PROCEDURE set_omi_span_start_datetime_1_0 ! 
     MODULE PROCEDURE set_omi_span_start_double_0_0 ! 
     MODULE PROCEDURE set_omi_span_start_double_1_0 ! 
  END INTERFACE
  !! Setze Komponente "end" in "t_omi_span" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! c) f&uuml;r ein Datenobjekt (Skalar) und eine Zeitangabe "t_datetime" <BR>
  !! d) f&uuml;r viele Datenobjekte (Vektor) und eine Zeitangabe "t_datetime" <BR>
  !! e) f&uuml;r ein Datenobjekt (Skalar) und eine Zeitangabe REAL(Double) <BR>
  !! f) f&uuml;r viele Datenobjekte (Vektor) und eine Zeitangabe REAL(Double)
  INTERFACE set_omi_span_end
     MODULE PROCEDURE set_omi_span_end_0_0 ! 
     MODULE PROCEDURE set_omi_span_end_1_0 ! 
     MODULE PROCEDURE set_omi_span_end_datetime_0_0 ! 
     MODULE PROCEDURE set_omi_span_end_datetime_1_0 ! 
     MODULE PROCEDURE set_omi_span_end_double_0_0 ! 
     MODULE PROCEDURE set_omi_span_end_double_1_0 ! 
  END INTERFACE
  !
  !! Hole Komponente "start" aus "t_omi_span": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_span_start
     MODULE PROCEDURE get_omi_span_start_0_0 ! 
     MODULE PROCEDURE get_omi_span_start_1_0 ! 
  END INTERFACE
  !! Hole Komponente "end" aus "t_omi_span": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_span_end
     MODULE PROCEDURE get_omi_span_end_0_0 ! 
     MODULE PROCEDURE get_omi_span_end_1_0 ! 
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Hole Komponente "start" aus "t_omi_span" als Datumsangabe "t_datetime": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_span_start_datetime
     MODULE PROCEDURE get_omi_span_start_datetime_0_0 ! 
     MODULE PROCEDURE get_omi_span_start_datetime_1_0 ! 
  END INTERFACE
  !! Hole Komponente "end" aus "t_omi_span" als Datumsangabe "t_datetime": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_span_end_datetime
     MODULE PROCEDURE get_omi_span_end_datetime_0_0 ! 
     MODULE PROCEDURE get_omi_span_end_datetime_1_0 ! 
  END INTERFACE
  !
  !! Hole Komponente "start" aus "t_omi_span" als mod. julianisches Datum REAL(Double): <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_span_start_double
     MODULE PROCEDURE get_omi_span_start_double_0_0 ! 
     MODULE PROCEDURE get_omi_span_start_double_1_0 ! 
  END INTERFACE
  !! Hole Komponente "end" aus "t_omi_span" als mod. julianisches Datum REAL(Double): <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_span_end_double
     MODULE PROCEDURE get_omi_span_end_double_0_0 ! 
     MODULE PROCEDURE get_omi_span_end_double_1_0 ! 
  END INTERFACE
  !
  !! Pr&uuml;fe, ob eine Zeitangabe innerhalb eines Zeitraums liegt <BR>
  !! a) f&uuml;r ein Datenobjekt und einen Vergleichstermin <BR>
  !! b) f&uuml;r viele Datenobjekte und einen Vergleichstermin <BR>
  !! c) f&uuml;r ein Datenobjekt und viele Vergleichstermine <BR>
  !! d) f&uuml;r viele Datenobjekte und viele Vergleichstermine <BR>
  !! e) f&uuml;r ein Datenobjekt und einen Vergleichstermin (t_datetime) <BR>
  !! f) f&uuml;r viele Datenobjekte und einen Vergleichstermin (t_datetime) <BR>
  !! g) f&uuml;r ein Datenobjekt und viele Vergleichstermine (t_datetime) <BR>
  !! h) f&uuml;r viele Datenobjekte und viele Vergleichstermine (t_datetime) <BR>
  !! i) f&uuml;r ein Datenobjekt und einen Vergleichstermin (Double) <BR>
  !! j) f&uuml;r viele Datenobjekte und einen Vergleichstermin (Double) <BR>
  !! k) f&uuml;r ein Datenobjekt und viele Vergleichstermine (Double) <BR>
  !! l) f&uuml;r viele Datenobjekte und viele Vergleichstermine (Double)
  INTERFACE is_inside_omi_span
     MODULE PROCEDURE is_inside_omi_span_0_0
     MODULE PROCEDURE is_inside_omi_span_1_0
     MODULE PROCEDURE is_inside_omi_span_0_1
     MODULE PROCEDURE is_inside_omi_span_1_1
     MODULE PROCEDURE is_inside_omi_span_datetime_0_0
     MODULE PROCEDURE is_inside_omi_span_datetime_1_0
     MODULE PROCEDURE is_inside_omi_span_datetime_0_1
     MODULE PROCEDURE is_inside_omi_span_datetime_1_1
     MODULE PROCEDURE is_inside_omi_span_double_0_0
     MODULE PROCEDURE is_inside_omi_span_double_1_0
     MODULE PROCEDURE is_inside_omi_span_double_0_1
     MODULE PROCEDURE is_inside_omi_span_double_1_1
  END INTERFACE
  !
  !! Pr&uuml;fe, ob eine Zeitangabe au&szlig;erhalb eines Zeitraums liegt <BR>
  !! a) f&uuml;r ein Datenobjekt und einen Vergleichstermin <BR>
  !! b) f&uuml;r viele Datenobjekte und einen Vergleichstermin <BR>
  !! c) f&uuml;r ein Datenobjekt und viele Vergleichstermine <BR>
  !! d) f&uuml;r viele Datenobjekte und viele Vergleichstermine <BR>
  !! e) f&uuml;r ein Datenobjekt und einen Vergleichstermin (t_datetime) <BR>
  !! f) f&uuml;r viele Datenobjekte und einen Vergleichstermin (t_datetime) <BR>
  !! g) f&uuml;r ein Datenobjekt und viele Vergleichstermine (t_datetime) <BR>
  !! h) f&uuml;r viele Datenobjekte und viele Vergleichstermine (t_datetime) <BR>
  !! i) f&uuml;r ein Datenobjekt und einen Vergleichstermin (Double) <BR>
  !! j) f&uuml;r viele Datenobjekte und einen Vergleichstermin (Double) <BR>
  !! k) f&uuml;r ein Datenobjekt und viele Vergleichstermine (Double) <BR>
  !! l) f&uuml;r viele Datenobjekte und viele Vergleichstermine (Double)
  INTERFACE is_outside_omi_span
     MODULE PROCEDURE is_outside_omi_span_0_0
     MODULE PROCEDURE is_outside_omi_span_1_0
     MODULE PROCEDURE is_outside_omi_span_0_1
     MODULE PROCEDURE is_outside_omi_span_1_1
     MODULE PROCEDURE is_outside_omi_span_dateti_0_0
     MODULE PROCEDURE is_outside_omi_span_dateti_1_0
     MODULE PROCEDURE is_outside_omi_span_dateti_0_1
     MODULE PROCEDURE is_outside_omi_span_dateti_1_1
     MODULE PROCEDURE is_outside_omi_span_double_0_0
     MODULE PROCEDURE is_outside_omi_span_double_1_0
     MODULE PROCEDURE is_outside_omi_span_double_0_1
     MODULE PROCEDURE is_outside_omi_span_double_1_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_span" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_span
     MODULE PROCEDURE eq_omi_span_0_0  ! 
     MODULE PROCEDURE eq_omi_span_0_1  ! 
     MODULE PROCEDURE eq_omi_span_1_0  ! 
     MODULE PROCEDURE eq_omi_span_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_span" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_span
     MODULE PROCEDURE ne_omi_span_0_0  ! 
     MODULE PROCEDURE ne_omi_span_0_1  ! 
     MODULE PROCEDURE ne_omi_span_1_0  ! 
     MODULE PROCEDURE ne_omi_span_1_1  ! 
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
  PUBLIC :: init_omi_span
  PUBLIC :: clear_omi_span
  PUBLIC :: setup_omi_span_prn_lun
  PUBLIC :: setup_omi_span_trc_lun
  PUBLIC :: new_omi_span
  PUBLIC :: kill_omi_span
  PUBLIC :: ok_omi_span
  PUBLIC :: print_omi_span
  PUBLIC :: print_omi_span_static
  PUBLIC :: print_omi_span_all_errors
  PUBLIC :: set_omi_span_start
  PUBLIC :: set_omi_span_end
  PUBLIC :: get_omi_span_start
  PUBLIC :: get_omi_span_end
  PUBLIC :: eq_omi_span
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: get_omi_span_start_datetime
  PUBLIC :: get_omi_span_start_double
  PUBLIC :: get_omi_span_end_datetime
  PUBLIC :: get_omi_span_end_double
  PUBLIC :: is_inside_omi_span
  PUBLIC :: is_outside_omi_span
  PUBLIC :: ne_omi_span
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
  CHARACTER (LEN=10), PARAMETER :: c_modname      = 'b_omi_span' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_span
  INTEGER           , PARAMETER :: c_nofcomp      =  2               ! ggf. modifizieren
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
  SUBROUTINE init_omi_span_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_omi_span_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_span" version 2.1 of 07/21/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_datetime  ( )
       IF ( no_error( ) ) CALL init_omi_stamp ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_span_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_span_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_span_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='clear_omi_span_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_span_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_stamp ( )
       IF ( no_error( ) ) CALL clear_datetime  ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
  END SUBROUTINE clear_omi_span_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_span_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_span_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_stamp_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_span_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_span_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_span_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_stamp_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_span_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_span_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_omi_span_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_stamp( this%start )
       IF ( no_error( ) ) CALL new_omi_stamp( this%end   )
    END IF
    !
  END SUBROUTINE new_omi_span_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_span_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_omi_span_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_span_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_span_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_span_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_omi_span_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL kill_omi_stamp( this%start )
       IF ( no_error( ) ) CALL kill_omi_stamp( this%end   )
    END IF
    !
  END SUBROUTINE kill_omi_span_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_span_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_omi_span_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_span_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_span_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_span_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_omi_span_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp+1) ! 
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ch     ! 
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_omi_span_start ( this )
       l_ok(2)  = ok_omi_span_end   ( this )
       l_ok(3)  = ge_omi_stamp( this%end, this%start )
       IF ( .NOT. l_ok(3) ) THEN
          CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
          WRITE(ch,'(G15.6)') get_omi_stamp_modjulianday( this%start )
          CALL setup_error_act ( '<start>', ch )
          WRITE(ch,'(G15.6)') get_omi_stamp_modjulianday( this%end )
          CALL setup_error_act ( '<end>', ch )
       END IF
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_span_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_span_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_omi_span_1' 
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
          ok(i) = ok_omi_span_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_span_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_span_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_omi_span_0' 
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
       IF ( no_error( ) ) CALL print_omi_span_start( this )
       IF ( no_error( ) ) CALL print_omi_span_end( this )
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
8000 FORMAT('# Beginn Objekt t_omi_span ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_span ------------------------------')
    !
  END SUBROUTINE print_omi_span_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_span_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_omi_span_1' 
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
          IF ( no_error( ) ) CALL print_omi_span_0 ( this(i) )
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
  END SUBROUTINE print_omi_span_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_span_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_omi_span_static_d' 
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
       IF ( no_error( ) ) CALL print_omi_span_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_span         ',/ &
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
  END SUBROUTINE print_omi_span_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_span_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_omi_span_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_span_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "start" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_start_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "start"
    TYPE (t_omi_stamp) , INTENT(IN)    :: val  ! 
    !
    this%start = val
    !
  END SUBROUTINE set_omi_span_start_0_0
  !
  !! weise der Komponente "start" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_start_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "start"
    TYPE (t_omi_stamp)           , INTENT(IN)  :: val     ! 
    !
    this(:)%start = val
    !
  END SUBROUTINE set_omi_span_start_1_0
  !
  !! weise der Komponente "start" einen skalaren Wert zu (Typ "t_datetime") <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_start_datetime_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "start"
    TYPE (t_datetime) , INTENT(IN)    :: val  ! 
    !
    CALL set_omi_stamp_modjulianday( this%start, val )
    !
  END SUBROUTINE set_omi_span_start_datetime_0_0
  !
  !! weise der Komponente "start" einen skalaren Wert zu (Typ "t_datetime") <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_start_datetime_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "start"
    TYPE (t_datetime) , INTENT(IN)    :: val     ! 
    !
    CALL set_omi_stamp_modjulianday( this(:)%start, val )
    !
  END SUBROUTINE set_omi_span_start_datetime_1_0
  !
  !! weise der Komponente "start" einen skalaren Wert zu (Typ REAL(Double)) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_start_double_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "start"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    CALL set_omi_stamp_modjulianday( this%start, val )
    !
  END SUBROUTINE set_omi_span_start_double_0_0
  !
  !! weise der Komponente "start" einen skalaren Wert zu (Typ REAL(Double)) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_start_double_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "start"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    CALL set_omi_stamp_modjulianday( this(:)%start, val )
    !
  END SUBROUTINE set_omi_span_start_double_1_0
  !
  !! weise der Komponente "end" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_end_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "end"
    TYPE (t_omi_stamp) , INTENT(IN)    :: val  ! 
    !
    this%end = val
    !
  END SUBROUTINE set_omi_span_end_0_0
  !
  !! weise der Komponente "end" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_end_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "end"
    TYPE (t_omi_stamp)           , INTENT(IN)  :: val     ! 
    !
    this(:)%end = val
    !
  END SUBROUTINE set_omi_span_end_1_0
  !
  !! weise der Komponente "end" einen skalaren Wert zu (Typ "t_datetime") <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_end_datetime_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "end"
    TYPE (t_datetime) , INTENT(IN)    :: val  ! 
    !
    CALL set_omi_stamp_modjulianday( this%end, val )
    !
  END SUBROUTINE set_omi_span_end_datetime_0_0
  !
  !! weise der Komponente "end" einen skalaren Wert zu (Typ "t_datetime") <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_end_datetime_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "end"
    TYPE (t_datetime) , INTENT(IN)    :: val     ! 
    !
    CALL set_omi_stamp_modjulianday( this(:)%end, val )
    !
  END SUBROUTINE set_omi_span_end_datetime_1_0
  !
  !! weise der Komponente "end" einen skalaren Wert zu (Typ REAL(Double)) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_end_double_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "end"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    CALL set_omi_stamp_modjulianday( this%end, val )
    !
  END SUBROUTINE set_omi_span_end_double_0_0
  !
  !! weise der Komponente "end" einen skalaren Wert zu (Typ REAL(Double)) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_span_end_double_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "end"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    CALL set_omi_stamp_modjulianday( this(:)%end, val )
    !
  END SUBROUTINE set_omi_span_end_double_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "start" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_start_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "start" (Skalar)
    TYPE (t_omi_stamp) :: val  ! 
    !
    val = this%start
    !
  END FUNCTION get_omi_span_start_0_0
  !
  !! hole die Komponente "start" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_start_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "start"
    TYPE (t_omi_stamp) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%start
    !
  END FUNCTION get_omi_span_start_1_0
  !
  !! hole die Komponente "start" als Datumsangabe aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_start_datetime_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "start" als Datumsangabe
    TYPE (t_datetime) :: val  ! 
    !
    val = get_omi_stamp_datetime( this%start )
    !
  END FUNCTION get_omi_span_start_datetime_0_0
  !
  !! hole die Komponente "start" als datumsangaben aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_start_datetime_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "start" als Datumsangaben
    TYPE (t_datetime) :: val(SIZE(this))  ! 
    !
    val(:) = get_omi_stamp_datetime( this(:)%start )
    !
  END FUNCTION get_omi_span_start_datetime_1_0
  !
  !! hole die Komponente "start" als REAL(Double) aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_start_double_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "start" als REAL(Double)
    REAL (KIND=Double) :: val  ! 
    !
    val = get_omi_stamp_modjulianday( this%start )
    !
  END FUNCTION get_omi_span_start_double_0_0
  !
  !! hole die Komponente "start" als REAL(Double) aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_start_double_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "start" als REAL(Double)
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = get_omi_stamp_modjulianday( this(:)%start )
    !
  END FUNCTION get_omi_span_start_double_1_0
  !
  !! hole die Komponente "end" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_end_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "end" (Skalar)
    TYPE (t_omi_stamp) :: val  ! 
    !
    val = this%end
    !
  END FUNCTION get_omi_span_end_0_0
  !
  !! hole die Komponente "end" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_end_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "end"
    TYPE (t_omi_stamp) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%end
    !
  END FUNCTION get_omi_span_end_1_0
  !
  !! hole die Komponente "end" als Datumsangabe aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_end_datetime_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "end" als Datumsangabe
    TYPE (t_datetime) :: val  ! 
    !
    val = get_omi_stamp_datetime( this%end )
    !
  END FUNCTION get_omi_span_end_datetime_0_0
  !
  !! hole die Komponente "end" als datumsangaben aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_end_datetime_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "end" als Datumsangaben
    TYPE (t_datetime) :: val(SIZE(this))  ! 
    !
    val(:) = get_omi_stamp_datetime( this(:)%end )
    !
  END FUNCTION get_omi_span_end_datetime_1_0
  !
  !! hole die Komponente "end" als REAL(Double) aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_end_double_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_span) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "end" als REAL(Double)
    REAL (KIND=Double) :: val  ! 
    !
    val = get_omi_stamp_modjulianday( this%end )
    !
  END FUNCTION get_omi_span_end_double_0_0
  !
  !! hole die Komponente "end" als REAL(Double) aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_span_end_double_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_span) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "end" als REAL(Double)
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = get_omi_stamp_modjulianday( this(:)%end )
    !
  END FUNCTION get_omi_span_end_double_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-IS-Methoden
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob der Termin "val" innerhalb des Zeitraums "this" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this ! 
    !! Termin 
    TYPE (t_omi_stamp) , INTENT(IN) :: val  ! 
    !! R&uuml;ckgabewert [T|F]
    LOGICAL :: res ! 
    !
    res = ( ge_omi_stamp( val, this%start ) .AND. le_omi_stamp( val, this%end ) )
    !
  END FUNCTION is_inside_omi_span_0_0
  !
  !! Pr&uuml;fe, ob der Termin "val" innerhalb der Zeitr&auml;e "this(:)" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termin 
    TYPE (t_omi_stamp) , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(this))                 ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_0_0 ( this(i), val )
    END DO
    !
  END FUNCTION is_inside_omi_span_1_0
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" innerhalb des Zeitraums "this" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this   ! 
    !! Termine 
    TYPE (t_omi_stamp) , INTENT(IN) :: val(:) ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(val))                 ! 
    !! Hilfsvariable
    INTEGER :: i                              ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION is_inside_omi_span_0_1
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" innerhalb der Zeitr&auml;e "this(:)" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termine 
    TYPE (t_omi_stamp) , INTENT(IN) :: val(:)  ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(MIN(SIZE(val),SIZE(this)))  ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_0_0 ( this(i), val(i) )
    END DO
    !
  END FUNCTION is_inside_omi_span_1_1
  !
  !! Pr&uuml;fe, ob der Termin "val" (t_datetime) innerhalb des Zeitraums "this" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_datetime_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this    ! 
    !! Termin (t_datetime) 
    TYPE (t_datetime)  , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewert [T|F]
    LOGICAL :: res ! 
    !! Hilfstermin
    TYPE (t_omi_stamp) :: l_stamp ! 
    !
    CALL new_omi_stamp ( l_stamp )
    CALL set_omi_stamp_modjulianday ( l_stamp, val )
    res = is_inside_omi_span_0_0 ( this, l_stamp )
    CALL kill_omi_stamp ( l_stamp )
    !
  END FUNCTION is_inside_omi_span_datetime_0_0
  !
  !! Pr&uuml;fe, ob der Termin "val" (t_datetime) innerhalb der Zeitr&auml;e "this(:)" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_datetime_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termin (t_datetime)
    TYPE (t_datetime)  , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(this))                 ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_datetime_0_0 ( this(i), val )
    END DO
    !
  END FUNCTION is_inside_omi_span_datetime_1_0
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" (t_datetime) innerhalb des Zeitraums "this" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_datetime_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this   ! 
    !! Termine (t_datetime)
    TYPE (t_datetime)  , INTENT(IN) :: val(:) ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(val))                 ! 
    !! Hilfsvariable
    INTEGER :: i                              ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_datetime_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION is_inside_omi_span_datetime_0_1
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" (t_datetime) innerhalb der Zeitr&auml;e "this(:)" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_datetime_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termine (t_datetime)
    TYPE (t_datetime) , INTENT(IN) :: val(:)  ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(MIN(SIZE(val),SIZE(this)))  ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_datetime_0_0 ( this(i), val(i) )
    END DO
    !
  END FUNCTION is_inside_omi_span_datetime_1_1
  !
  !! Pr&uuml;fe, ob der Termin "val" (REAL (KIND=Double)) innerhalb des Zeitraums "this" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_double_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this    ! 
    !! Termin (REAL (KIND=Double)) 
    REAL (KIND=Double) , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewert [T|F]
    LOGICAL :: res ! 
    !! Hilfstermin
    TYPE (t_omi_stamp) :: l_stamp ! 
    !
    CALL new_omi_stamp ( l_stamp )
    CALL set_omi_stamp_modjulianday ( l_stamp, val )
    res = is_inside_omi_span_0_0 ( this, l_stamp )
    CALL kill_omi_stamp ( l_stamp )
    !
  END FUNCTION is_inside_omi_span_double_0_0
  !
  !! Pr&uuml;fe, ob der Termin "val" (REAL (KIND=Double)) innerhalb der Zeitr&auml;e "this(:)" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_double_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termin (REAL (KIND=Double))
    REAL (KIND=Double) , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(this))                 ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_double_0_0 ( this(i), val )
    END DO
    !
  END FUNCTION is_inside_omi_span_double_1_0
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" (REAL (KIND=Double)) innerhalb des Zeitraums "this" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_double_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this   ! 
    !! Termine (REAL (KIND=Double))
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(val))                 ! 
    !! Hilfsvariable
    INTEGER :: i                              ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_double_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION is_inside_omi_span_double_0_1
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" (REAL (KIND=Double)) innerhalb der Zeitr&auml;e "this(:)" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_inside_omi_span_double_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termine (REAL (KIND=Double))
    REAL (KIND=Double) , INTENT(IN) :: val(:)  ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(MIN(SIZE(val),SIZE(this)))  ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_inside_omi_span_double_0_0 ( this(i), val(i) )
    END DO
    !
  END FUNCTION is_inside_omi_span_double_1_1
  !
  !! Pr&uuml;fe, ob der Termin "val" au&szlig;erhalb des Zeitraums "this" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this ! 
    !! Termin 
    TYPE (t_omi_stamp) , INTENT(IN) :: val  ! 
    !! R&uuml;ckgabewert [T|F]
    LOGICAL :: res ! 
    !
    res = ( lt_omi_stamp( val, this%start ) .OR. gt_omi_stamp( val, this%end ) )
    !
  END FUNCTION is_outside_omi_span_0_0
  !
  !! Pr&uuml;fe, ob der Termin "val" au&szlig;erhalb der Zeitr&auml;e "this(:)" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termin 
    TYPE (t_omi_stamp) , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(this))                 ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_0_0 ( this(i), val )
    END DO
    !
  END FUNCTION is_outside_omi_span_1_0
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" au&szlig;erhalb des Zeitraums "this" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this   ! 
    !! Termine 
    TYPE (t_omi_stamp) , INTENT(IN) :: val(:) ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(val))                 ! 
    !! Hilfsvariable
    INTEGER :: i                              ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION is_outside_omi_span_0_1
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" au&szlig;erhalb der Zeitr&auml;e "this(:)" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termine 
    TYPE (t_omi_stamp) , INTENT(IN) :: val(:)  ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(MIN(SIZE(val),SIZE(this)))  ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_0_0 ( this(i), val(i) )
    END DO
    !
  END FUNCTION is_outside_omi_span_1_1
  !
  !! Pr&uuml;fe, ob der Termin "val" (t_datetime) au&szlig;erhalb des Zeitraums "this" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_dateti_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this    ! 
    !! Termin (t_datetime) 
    TYPE (t_datetime)  , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewert [T|F]
    LOGICAL :: res ! 
    !! Hilfstermin
    TYPE (t_omi_stamp) :: l_stamp ! 
    !
    CALL new_omi_stamp ( l_stamp )
    CALL set_omi_stamp_modjulianday ( l_stamp, val )
    res = is_outside_omi_span_0_0 ( this, l_stamp )
    CALL kill_omi_stamp ( l_stamp )
    !
  END FUNCTION is_outside_omi_span_dateti_0_0
  !
  !! Pr&uuml;fe, ob der Termin "val" (t_datetime) au&szlig;erhalb der Zeitr&auml;e "this(:)" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_dateti_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termin (t_datetime)
    TYPE (t_datetime)  , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(this))                 ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_dateti_0_0 ( this(i), val )
    END DO
    !
  END FUNCTION is_outside_omi_span_dateti_1_0
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" (t_datetime) au&szlig;erhalb des Zeitraums "this" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_dateti_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this   ! 
    !! Termine (t_datetime)
    TYPE (t_datetime)  , INTENT(IN) :: val(:) ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(val))                 ! 
    !! Hilfsvariable
    INTEGER :: i                              ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_dateti_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION is_outside_omi_span_dateti_0_1
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" (t_datetime) au&szlig;erhalb der Zeitr&auml;e "this(:)" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_dateti_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termine (t_datetime)
    TYPE (t_datetime) , INTENT(IN) :: val(:)  ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(MIN(SIZE(val),SIZE(this)))  ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_dateti_0_0 ( this(i), val(i) )
    END DO
    !
  END FUNCTION is_outside_omi_span_dateti_1_1
  !
  !! Pr&uuml;fe, ob der Termin "val" (REAL (KIND=Double)) au&szlig;erhalb des Zeitraums "this" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_double_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this    ! 
    !! Termin (REAL (KIND=Double)) 
    REAL (KIND=Double) , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewert [T|F]
    LOGICAL :: res ! 
    !! Hilfstermin
    TYPE (t_omi_stamp) :: l_stamp ! 
    !
    CALL new_omi_stamp ( l_stamp )
    CALL set_omi_stamp_modjulianday ( l_stamp, val )
    res = is_outside_omi_span_0_0 ( this, l_stamp )
    CALL kill_omi_stamp ( l_stamp )
    !
  END FUNCTION is_outside_omi_span_double_0_0
  !
  !! Pr&uuml;fe, ob der Termin "val" (REAL (KIND=Double)) au&szlig;erhalb der Zeitr&auml;e "this(:)" liegt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_double_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termin (REAL (KIND=Double))
    REAL (KIND=Double) , INTENT(IN) :: val     ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(this))                 ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_double_0_0 ( this(i), val )
    END DO
    !
  END FUNCTION is_outside_omi_span_double_1_0
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" (REAL (KIND=Double)) au&szlig;erhalb des Zeitraums "this" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_double_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt mit Zeitraum
    TYPE (t_omi_span)  , INTENT(IN) :: this   ! 
    !! Termine (REAL (KIND=Double))
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(SIZE(val))                 ! 
    !! Hilfsvariable
    INTEGER :: i                              ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_double_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION is_outside_omi_span_double_0_1
  !
  !! Pr&uuml;fe, ob die Termine "val(:)" (REAL (KIND=Double)) au&szlig;erhalb der Zeitr&auml;e "this(:)" liegen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_outside_omi_span_double_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte mit Zeitr&auml;en
    TYPE (t_omi_span)  , INTENT(IN) :: this(:) ! 
    !! Termine (REAL (KIND=Double))
    REAL (KIND=Double) , INTENT(IN) :: val(:)  ! 
    !! R&uuml;ckgabewerte [T|F]
    LOGICAL :: res(MIN(SIZE(val),SIZE(this)))  ! 
    !! Hilfsvariable
    INTEGER :: i                               ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_outside_omi_span_double_0_0 ( this(i), val(i) )
    END DO
    !
  END FUNCTION is_outside_omi_span_double_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_span_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = eq_omi_stamp( this1%start, this2%start )
    l_ok(2)  = eq_omi_stamp( this1%end, this2%end   )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_span_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_span_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_span_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_span_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_span_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_span_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_span_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_span_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_span_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_span_1_1
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
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_span_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_span_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_span_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_span_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_span_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_span_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_span_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_span) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_span_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_span_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_span_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_span) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l     = SIZE(ok)
    ok(:) = .NOT. eq_omi_span_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_span_1_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_span" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_span ausfuehren'
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
  SUBROUTINE init_omi_span_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER  :: c_upname='init_omi_span_all_errors' !
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
               '--> INIT_omi_span ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_span ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_span"\n'//&
               'Typ-Komponente = "start"\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_span"\n'//&
               'Typ-Komponente = "end"\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_span"\n'//&
               '"start" liegt hinter "end"\n'//&
               'start = <start>\n'//&
               'end   = <end>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_span" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_span" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_span" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_span"\n'//&
               'Typ-Komponente = "start"\n'//&
               '--> Code in Modul "b_omi_span" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_span"\n'//&
               'Typ-Komponente = "end"\n'//&
               '--> Code in Modul "b_omi_span" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_span"\n'//&
               '--> Code in Modul "b_omi_span" / Daten pruefen' )
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
  END SUBROUTINE init_omi_span_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_span_all_errors &
       ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_span_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "start" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_span_start &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_span) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_span_start' ! 
    !! Hilfsvariable
    CHARACTER (LEN=15) :: ch ! 
    !
    ok = ok_omi_stamp( this%start ) 
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ch,'(G15.6)') get_omi_stamp_modjulianday( this%start )
       CALL setup_error_act( '<aktuell>', ch )
    END IF
    !
  END FUNCTION ok_omi_span_start
  !
  !! Pr&uuml;fe, ob die Komponente "end" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_span_end &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_span) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_omi_span_end' ! 
    !! Hilfsvariable
    CHARACTER (LEN=15) :: ch ! 
    !
    ok = ok_omi_stamp( this%end ) 
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       WRITE(ch,'(G15.6)') get_omi_stamp_modjulianday( this%end )
       CALL setup_error_act( '<aktuell>', ch )
    END IF
    !
  END FUNCTION ok_omi_span_end
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "start" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_span_start &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_span) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='print_omi_span_start' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) 
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
    CALL print_omi_stamp ( this%start )
    !
8000 FORMAT &
          ('# Inhalt der Komponente start - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_span_start
  !
  !! Drucke den Inhalt der Komponente "end" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_span_end &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_span) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_omi_span_end' ! 
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
    CALL print_omi_stamp ( this%end )
    !
8000 FORMAT &
          ('# Inhalt der Komponente end - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_span_end
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
END MODULE b_omi_span
! TailOfBaseModule --------------------------------------------------------
