! HeadOfBaseModule --------------------------------------------------------
!! <H2>Arbeiten mit Punkten in einem zweidimensionalen Koordinatensystem</h2>
!! @author Peter Schade
!! @version 1.18 vom 02.03 07, Quellcode: mod_b_point_2d.f90
!! <HR>
!! Basic type and methods to deal with points in a two dimensional coordinate system <BR>
!! <HR>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!  <CopyrightWildcard>
!!                                                                  <BR>
!! <HR>
! <H3>Entwicklungsgeschichte des Moduls</H3>
! 01.01 : 2002-01-15 : P. Schade   : Original Version 
! 01.02 : 2002-01-18 : P. Schade   : + .rotate. , + Testen von (*), (/), REAL(KIND=Double) in _allen_ Methodenaufrufe
! 01.03 : 2002-02-04 : P. Schade   : + set_point_2d_xy; Loeschen von >, >=, <, <=;  Genauigkeit der Variablen scale mit DP angeben
! 01.04 : 2002-04-23 : P. Schade   : Fehlernummerierung an das Template angepasst
! 01.05 : 2002-04-24 : P. Schade   : nochmalige Anpassung der Fehlernummerierung an das Template
! 01.06 : 2002-05-13 : P. Schade   : ???
! 01.07 : 2002-05-13 : P. Schade   : Kommentarzeile Aenderung von Rechts zu Hoch
! 01.08 : 2002-05-17 : P. Schade   : Korrektur: trc_op  = MERGE( .true., .false., trc_lun > 0 )
! 01.09 : 2002-06-05 : G. Lang     : ok_initialised modifiziert
! 01.10 : 2002-06-12 : G. Lang     : INIT/CLEAR und SETUP_*_PRN_LUN, SETUP_*_TRC_LUN modifiziert
! 01.11 : 2002-08-27 : J. Juerges  : Routine inside_point_2d eingefuegt.
! 01.12 : 2003-02-13 : P. Schade   : test_point_2d in HP main_test_point_2d.f90 verlegt
! 01.13 : 2002-03-19 : P. Schade   : Beschreibung oeffentlicher Methoden im Header entfernt, 
!                                    Copyright als Wildcard, 
!                                    Entwicklungsgeschichte nicht in HTML-Beschreibung 
! 01.14 : 2002-03-19 : P. Schade   : all_errors(:) als dynamisch allokierbares Feld
! 01.15 : 2002-04-10 : G. Lang     : Copyright-Hinweis modifiziert
! 01.17 : 2004-01-19 : H. Weilbeer : bei selbstdefinierten Datentypen INTENT(OUT) -> INTENT(INOUT) gewandelt
! 01.17 : 2004-02-02 : G. Seiss    : Anpassungen fuer Windows NT
! 01.18 : 2005-08-10 : G. Lang     : Korrektur "/=", verschiedene Methoden get_point_2d_diff_* zum Ermitteln
!                                    lageidentischer/-verschiedener Punkte
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Datentyp und Methoden zum Arbeiten mit Punkten in einem          <BR>
!! zweidimensionalen kartesischen Koordinatensystem.               <BR>
!! <HR>
!!                                                                  <BR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul enh&auml;lt den selbst definierten Datentyp t_point_2d, <BR>
!! der die Lage von Punkten in einem zweidimensionalen kartesischen <BR>
!! Koordinatensystem beschreibt.<BR>
!! Komponenten:
!! <OL>
!!     <LI> x : X- oder Rechts-Koordinate
!!     <LI> y : Y- oder Hoch-Koordinate
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
!!    <LI> Initialisieren des Moduls b_point_2d mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_point_2d mit CLEAR-Methode.
!! </OL>
!!                                                                  <BR>
!! <HR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   
!!          andere nicht. 
!!          Routinen, die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,
!!          ob das Modul korrekt initialisert wurde (ok_initialised).  <BR>
!!          F&uuml;r eine vollst&auml;ndige &Uuml;bersicht verwende man
!!          die Methode PRINT_POINT_2D_ALL_ERRORS.
!
MODULE b_point_2d
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
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       setup_error_act,     &
       set_error_ierr,      &
       set_error_cerr
  !
  ! [A.3] weitere BASIS-Module (ONLY benutzen!)
  !
  ! USE b_<BaseModuleName>, ONLY : &
  !   Typdefinitionen
  !   Parameter 
  !   Variablen mit INTENT(IN)
  !   Variablen mit INTENT(INOUT)
  !   Variablen mit INTENT(OUT)
  !   Routinen / Interfaces
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
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! selbst definierter Datentyp, der die Lage von Punkten in einem <BR>
  !! zweidimensionalen kartesischen Koordinatensystem beschreibt.<BR>
  TYPE , PUBLIC :: t_point_2d
     PRIVATE
     !! X- oder Rechts-Koordinate
     REAL (KIND=Double) :: x 
     !! Y- oder Hoch-Koordinate
     REAL (KIND=Double) :: y 
  END TYPE t_point_2d
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
  INTERFACE init_point_2d
     MODULE PROCEDURE init_point_2d_d ! welche Bedeutung hat die Endung _d? 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE clear_point_2d
     MODULE PROCEDURE clear_point_2d_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_point_2d_prn_lun
     MODULE PROCEDURE setup_point_2d_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_point_2d_trc_lun
     MODULE PROCEDURE setup_point_2d_trc_lun_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_point_2d" (Skalar, 1D-Array)
  INTERFACE new_point_2d
     MODULE PROCEDURE new_point_2d_0  ! Version fuer Skalar
     MODULE PROCEDURE new_point_2d_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_point_2d" (Skalar, 1D-Array)
  INTERFACE kill_point_2d
     MODULE PROCEDURE kill_point_2d_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_point_2d_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_point_2d" (Skalar, 1D-Array)
  INTERFACE ok_point_2d
     MODULE PROCEDURE ok_point_2d_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_point_2d_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Datenobjekten "t_point_2d" (Skalar, 1D-Array)
  INTERFACE print_point_2d
     MODULE PROCEDURE print_point_2d_0 ! Version fuer Skalar
     MODULE PROCEDURE print_point_2d_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten
  INTERFACE print_point_2d_static
     MODULE PROCEDURE print_point_2d_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls
  INTERFACE print_point_2d_all_errors
     MODULE PROCEDURE print_point_2d_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "x" in "t_point_2d" auf Benutzerwert
  INTERFACE set_point_2d_x
     MODULE PROCEDURE set_point_2d_x_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_point_2d_x_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "y" in "t_point_2d" auf Benutzerwert
  INTERFACE set_point_2d_y
     MODULE PROCEDURE set_point_2d_y_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_point_2d_y_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Hole Komponente "x" aus "t_point_2d"
  INTERFACE get_point_2d_x
     MODULE PROCEDURE get_point_2d_x_0_0 ! Skalar
     MODULE PROCEDURE get_point_2d_x_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "y" aus "t_point_2d"
  INTERFACE get_point_2d_y
     MODULE PROCEDURE get_point_2d_y_0_0 ! Skalar
     MODULE PROCEDURE get_point_2d_y_1_0 ! Vektor
  END INTERFACE
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Setze Komponenten "x" und "y" in "t_point_2d" auf Benutzerwerte
  INTERFACE set_point_2d_xy
     MODULE PROCEDURE set_point_2d_xy_0_0 ! Skalar
     MODULE PROCEDURE set_point_2d_xy_1_0 ! Vektor
  END INTERFACE
  !! Addition von Datenobjekt und zwei Real-Variablen, <BR>
  !! die erste zur x-Komponente, die zweite zur y-Komponente
  INTERFACE add_point_2d_xy
     MODULE PROCEDURE ad_point_2d_xy_0 ! Skalar
     MODULE PROCEDURE ad_point_2d_xy_1 ! Vektor
  END INTERFACE
  !! Addition von Datenobjekt und zwei Real-Variablen,  <BR>
  !! die erste zur x-Komponente, die zweite zur y-Komponente
  INTERFACE sub_point_2d_xy
     MODULE PROCEDURE su_point_2d_xy_0 ! Skalar
     MODULE PROCEDURE su_point_2d_xy_1 ! Vektor
  END INTERFACE
  !! Liegt ein 2D-Punkt innerhalb eines durch 2D-Punkte
  !! aufgespannten Polygons?
  INTERFACE inside_point_2d
     MODULE PROCEDURE inside_point_2d_0 ! Skalar
     MODULE PROCEDURE inside_point_2d_1 ! Vektor
  END INTERFACE
  !
  !! Ermittle eine Indikatorliste f&uuml;r einen Vektor von Koordinaten
  !! zur Indizierung voneinander verschiedener Datenpunkte
  INTERFACE get_point_2d_diff_ind
     MODULE PROCEDURE get_point_2d_diff_ind_1
  END INTERFACE
  !! Ermittle die Anzahl unterschiedlicher Punkte in einer Liste von Koordinaten
  INTERFACE get_point_2d_diff_count
     MODULE PROCEDURE get_point_2d_diff_count_1
  END INTERFACE
  !! Ermittle die Listenindices f&uuml;r einen Vektor von Koordinaten, die
  !! von eventuell mehrdeutigen Punkten auf die eindeutigen verweisen
  INTERFACE get_point_2d_diff_idx
     MODULE PROCEDURE get_point_2d_diff_idx_1
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
  !! Pruefung zweier Datenobjekte "t_point_2d" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_point_2d_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_point_2d_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_point_2d_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_point_2d_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Addition zweier Datenobjekte "t_point_2d"
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE ad_point_2d_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ad_point_2d_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ad_point_2d_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ad_point_2d_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Subtraktion zweier Datenobjekte "t_point_2d"
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE su_point_2d_0_0  ! Skalar / Skalar
     MODULE PROCEDURE su_point_2d_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE su_point_2d_1_0  ! Vektor / Skalar
     MODULE PROCEDURE su_point_2d_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Multiplikation eines Datenobjektes "t_point_2d" mit einer intrinsischen Variablen
  INTERFACE OPERATOR(*)
     MODULE PROCEDURE mu_point_2d_0_i  ! Skalar / Integer
     MODULE PROCEDURE mu_point_2d_i_0  ! Integer / Skalar 
     MODULE PROCEDURE mu_point_2d_0_r  ! Skalar / Real
     MODULE PROCEDURE mu_point_2d_r_0  ! Real / Skalar
     MODULE PROCEDURE mu_point_2d_1_i  ! Vektor / Integer
     MODULE PROCEDURE mu_point_2d_i_1  ! Integer / Vektor 
     MODULE PROCEDURE mu_point_2d_1_r  ! Vektor / Real
     MODULE PROCEDURE mu_point_2d_r_1  ! Real / Vektor
  END INTERFACE
  !! Division zweier Datenobjekte "t_point_2d"
  INTERFACE OPERATOR(/)
     MODULE PROCEDURE di_point_2d_0_i  ! Skalar / Integer
     MODULE PROCEDURE di_point_2d_0_r  ! Skalar / Real
     MODULE PROCEDURE di_point_2d_1_i  ! Vektor / Integer
     MODULE PROCEDURE di_point_2d_1_r  ! Vektor / Real
  END INTERFACE
  !! Pr&uuml;fung zweier Datenobjekte "t_point_2d" auf Ungleichheit 
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_point_2d_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_point_2d_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_point_2d_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_point_2d_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Winkel zwischen zwei Objekten des Typs "t_point_2d" <BR>
  !! in mathematisch positiver Drehrichtung
  INTERFACE OPERATOR(.angle.)
     MODULE PROCEDURE angle_point_2d_0_0  ! Skalar / Skalar
     MODULE PROCEDURE angle_point_2d_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE angle_point_2d_1_0  ! Vektor / Skalar
     MODULE PROCEDURE angle_point_2d_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Abstand zwischen zwei Objekten des Typs "t_point_2d"
  INTERFACE OPERATOR(.dist.)
     MODULE PROCEDURE dist_point_2d_0_0  ! Skalar / Skalar
     MODULE PROCEDURE dist_point_2d_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE dist_point_2d_1_0  ! Vektor / Skalar
     MODULE PROCEDURE dist_point_2d_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Abstandsquadrat zwischen zwei Objekten des Typs "t_point_2d"
  INTERFACE OPERATOR(.sqdist.)
     MODULE PROCEDURE sq_dist_point_2d_0_0  ! Skalar / Skalar
     MODULE PROCEDURE sq_dist_point_2d_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE sq_dist_point_2d_1_0  ! Vektor / Skalar
     MODULE PROCEDURE sq_dist_point_2d_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Rotation eines Objektes um den Koordinatenursprung in mathematisch  <BR>
  !! positiver Drehrichtung
  INTERFACE OPERATOR(.rotate.)
     MODULE PROCEDURE rot_point_2d_0  ! Skalar
     MODULE PROCEDURE rot_point_2d_1  ! Vektor 
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_point_2d                 ! Initialisieren (Modul)
  PUBLIC :: clear_point_2d                ! De-Initialisieren (Modul)
  PUBLIC :: setup_point_2d_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_point_2d_trc_lun        ! Setzen trc_lun 
  PUBLIC :: new_point_2d                  ! Erzeugen 
  PUBLIC :: kill_point_2d                 ! Vernichten
  PUBLIC :: ok_point_2d                   ! Pruefen
  PUBLIC :: print_point_2d                ! Drucken
  PUBLIC :: print_point_2d_static         ! Drucken aller statischen Daten
  PUBLIC :: print_point_2d_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_point_2d_x                ! Setzen der Komponente x
  PUBLIC :: set_point_2d_y                ! Setzen der Komponente y
  PUBLIC :: get_point_2d_x                ! Holen der Komponente x
  PUBLIC :: get_point_2d_y                ! Holen der Komponente y
  PUBLIC :: OPERATOR(==)                  ! Operator "=="
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: set_point_2d_xy               ! Setzen der Komponenten x und y
  PUBLIC :: OPERATOR(+)                   ! Operator "+"
  PUBLIC :: add_point_2d_xy               ! Addition von Objekt und Realvariablen
  PUBLIC :: OPERATOR(-)                   ! Operator "-"
  PUBLIC :: sub_point_2d_xy               ! Subtraktion zweier Realzahlen vom Objekt
  PUBLIC :: OPERATOR(*)                   ! Operator "*"
  PUBLIC :: OPERATOR(/)                   ! Operator "/"
  PUBLIC :: OPERATOR(/=)                  ! Operator "/=" 
  PUBLIC :: OPERATOR(.angle.)             ! Operator zum Berechnen eines Winkels 
                                          ! zwischen zwei Objekten
  PUBLIC :: OPERATOR(.dist.)              ! Operator zum Berechnen eines Abstandes 
                                          ! zwischen zwei Objekten
  PUBLIC :: OPERATOR(.sqdist.)            ! Operator zum Berechnen eines Abstandsquadrates
                                          ! zwischen zwei Objekten
  PUBLIC :: OPERATOR(.rotate.)            ! Operator zum Berechnen der Rotation eines 
                                          ! Objektes um den Koordinatenursprung
  PUBLIC :: inside_point_2d               ! Liegt ein 2D-Punkt innerhalb eines Polygons?
  PUBLIC :: get_point_2d_diff_ind         ! 
  PUBLIC :: get_point_2d_diff_count       ! 
  PUBLIC :: get_point_2d_diff_idx         ! 
  !Windows-spezifische Zeilen
!>WIN-NT:  PUBLIC :: eq_point_2d_0_0
!>WIN-NT:  PUBLIC :: eq_point_2d_0_1
!>WIN-NT:  PUBLIC :: eq_point_2d_1_0
!>WIN-NT:  PUBLIC :: eq_point_2d_1_1
!>WIN-NT:  PUBLIC :: ne_point_2d_0_0
!>WIN-NT:  PUBLIC :: ne_point_2d_0_1
!>WIN-NT:  PUBLIC :: ne_point_2d_1_0
!>WIN-NT:  PUBLIC :: ne_point_2d_1_1
!>WIN-NT:  PUBLIC :: ad_point_2d_0_0 
!>WIN-NT:  PUBLIC :: ad_point_2d_0_1  
!>WIN-NT:  PUBLIC :: ad_point_2d_1_0
!>WIN-NT:  PUBLIC :: ad_point_2d_1_1 
!>WIN-NT:  PUBLIC :: su_point_2d_0_0 
!>WIN-NT:  PUBLIC :: su_point_2d_0_1 
!>WIN-NT:  PUBLIC :: su_point_2d_1_0
!>WIN-NT:  PUBLIC :: su_point_2d_1_1  
!>WIN-NT:  PUBLIC :: mu_point_2d_0_i 
!>WIN-NT:  PUBLIC :: mu_point_2d_i_0  
!>WIN-NT:  PUBLIC :: mu_point_2d_0_r
!>WIN-NT:  PUBLIC :: mu_point_2d_r_0 
!>WIN-NT:  PUBLIC :: mu_point_2d_1_i 
!>WIN-NT:  PUBLIC :: mu_point_2d_i_1  
!>WIN-NT:  PUBLIC :: mu_point_2d_1_r 
!>WIN-NT:  PUBLIC :: mu_point_2d_r_1 
!>WIN-NT:  PUBLIC :: di_point_2d_0_i
!>WIN-NT:  PUBLIC :: di_point_2d_0_r
!>WIN-NT:  PUBLIC :: di_point_2d_1_i 
!>WIN-NT:  PUBLIC :: di_point_2d_1_r 
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
  CHARACTER (LEN=10), PARAMETER :: c_modname      = 'b_point_2d' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_point_2d
  INTEGER           , PARAMETER :: c_nofcomp      = 2                ! 
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
  !! Allokieren/Initialisieren der statischen Daten des Moduls
  SUBROUTINE init_point_2d_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_point_2d" version 1.18 of 02.03 07                 '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*) ' '
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       ! ... derzeit nicht erforderlich 
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_point_2d_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] ggf. weitere Initialsierungsmethoden rufen
       ! [1.7] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_point_2d_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls
  SUBROUTINE clear_point_2d_d ( )
    !
    IF ( initialised .AND. n_init == 1) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_point_2d_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] ggf. weitere De-Initialsierungsmethoden rufen
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.5] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.5.1] ggf. weitere Module de-initialisieren
       ! ... derzeit nicht erforderlich
       ! [1.5.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_point_2d_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden
  SUBROUTINE setup_point_2d_prn_lun_d ( lun )
    !
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_point_2d_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       ! ... weitere Basis-Module derzeit nicht erforderlich
    END IF
    !
  END SUBROUTINE setup_point_2d_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden
  SUBROUTINE setup_point_2d_trc_lun_d ( lun )
    !
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_point_2d_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       ! ... weitere Basis-Module derzeit nicht erforderlich
    END IF
    !
  END SUBROUTINE setup_point_2d_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar)
  SUBROUTINE new_point_2d_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_point_2d_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%x = HUGE(this%x)
       this%y = HUGE(this%y)
    END IF
    !
  END SUBROUTINE new_point_2d_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor)
  SUBROUTINE new_point_2d_1 ( this )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_point_2d_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_point_2d_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_point_2d_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar)
  SUBROUTINE kill_point_2d_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_point_2d_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_point_2d_0 ( this )
    END IF
    !
  END SUBROUTINE kill_point_2d_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor)
  SUBROUTINE kill_point_2d_1 ( this )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_point_2d_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_point_2d_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_point_2d_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar)
  FUNCTION ok_point_2d_0 ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_point_2d_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_point_2d_x( this )
       l_ok(2) = ok_point_2d_y( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_point_2d_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor)
  FUNCTION ok_point_2d_1 ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_point_2d_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) ) EXIT
          ok(i) = ok_point_2d_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_point_2d_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar)
  SUBROUTINE print_point_2d_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_point_2d_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT   = prn_lun, &
               FMT    = 8000,    &
               IOSTAT = stat )
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_point_2d_x( this )
       IF ( no_error( ) ) CALL print_point_2d_y( this )
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
8000 FORMAT('# Beginn Objekt t_point_2d ------------------------------')
8001 FORMAT('# Ende   Objekt t_point_2d ------------------------------')
    !
  END SUBROUTINE print_point_2d_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor)
  SUBROUTINE print_point_2d_1 ( this )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_point_2d_1' 
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
          WRITE ( UNIT = prn_lun, FMT = 8000, IOSTAT = stat ) i
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          IF ( no_error( ) ) CALL print_point_2d_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_point_2d_1
  !
  !! Drucken aller statischen Daten eines Moduls
  SUBROUTINE print_point_2d_static_d ( )
    !
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_point_2d_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_point_2d_all_errors_d ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_point_2d         ',/ &
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
  END SUBROUTINE print_point_2d_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls
  SUBROUTINE print_point_2d_all_errors_d ( )
    !
    !! Name der Function
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_point_2d_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_point_2d_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "x" einen skalaren Wert zu (Skalar)
  SUBROUTINE set_point_2d_x_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "x"
    REAL (KIND=Double)           , INTENT(IN)  :: val  ! 
    !
    this%x = val
    !
  END SUBROUTINE set_point_2d_x_0_0
  !
  !! weise der Komponente "x" einen skalaren Wert zu (Vektor)
  SUBROUTINE set_point_2d_x_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "x"
    REAL (KIND=Double)           , INTENT(IN)  :: val     ! 
    !
    this%x = val
    !
  END SUBROUTINE set_point_2d_x_1_0
  !
  !! weise der Komponente "y" einen skalaren Wert zu (Skalar)
  SUBROUTINE set_point_2d_y_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "y"
    REAL (KIND=Double)           , INTENT(IN)  :: val  ! 
    !
    this%y = val
    !
  END SUBROUTINE set_point_2d_y_0_0
  !
  !! weise der Komponente "y" einen skalaren Wert zu (Vektor)
  SUBROUTINE set_point_2d_y_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "y"
    REAL (KIND=Double)           , INTENT(IN)  :: val     ! 
    !
    this%y = val
    !
  END SUBROUTINE set_point_2d_y_1_0
  !
  !! weise den Komponente "x" und "y" einen skalaren Wert zu (Skalar)
  SUBROUTINE set_point_2d_xy_0_0 ( this, val_x, val_y )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "x"
    REAL (KIND=Double)           , INTENT(IN)  :: val_x  ! 
    !! Wert f&uuml;r Komponente "y"
    REAL (KIND=Double)           , INTENT(IN)  :: val_y  ! 
    !
    CALL set_point_2d_x_0_0 (this, val_x)
    CALL set_point_2d_y_0_0 (this, val_y)
    !
  END SUBROUTINE set_point_2d_xy_0_0
  !
  !! weise den Komponente "x" und "y" einen skalaren Wert zu (Vektor)
  SUBROUTINE set_point_2d_xy_1_0 ( this, val_x, val_y )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "x"
    REAL (KIND=Double)           , INTENT(IN)  :: val_x  ! 
    !! Wert f&uuml;r Komponente "y"
    REAL (KIND=Double)           , INTENT(IN)  :: val_y  ! 
    !
    CALL set_point_2d_x_1_0 (this, val_x)
    CALL set_point_2d_y_1_0 (this, val_y)
    !
  END SUBROUTINE set_point_2d_xy_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "x" aus einem skalaren Datenobjekt
  FUNCTION get_point_2d_x_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "x" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%x
    !
  END FUNCTION get_point_2d_x_0_0
  !
  !! hole die Komponente "x" aus einem vektoriellen Datenobjekt
  FUNCTION get_point_2d_x_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "x"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val = this%x
    !
  END FUNCTION get_point_2d_x_1_0
  !
  !! hole die Komponente "y" aus einem skalaren Datenobjekt
  FUNCTION get_point_2d_y_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "y" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%y
    !
  END FUNCTION get_point_2d_y_0_0
  !
  !! hole die Komponente "y" aus einem vektoriellen Datenobjekt
  FUNCTION get_point_2d_y_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "y"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val = this%y
    !
  END FUNCTION get_point_2d_y_1_0
  !
  !! Ermittle eine Indikatorliste f&uuml;r einen Vektor von Koordinaten
  !! zur Indizierung voneinander verschiedener Datenpunkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_point_2d_diff_ind_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this(:) ! 
    !! Testergebnis: Punkte, die eine zweites, drittes, ... Mal auftreten,
    !!               werden mit .false. gekennzeichnet, alle anderen mit 
    !!               .true.
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariablen
    INTEGER :: i, j ! 
    !
    res(:) = .true.
    DO i=1,SIZE(res)
       IF ( .NOT. res(i) ) CYCLE
       DO j=i+1,SIZE(res)
          IF ( .NOT. res(j)       ) CYCLE
          res(j) = ( this(j) /= this(i) )
       END DO
    END DO
    !
  END FUNCTION get_point_2d_diff_ind_1
  !
  !! Ermittle die Anzahl unterschiedlicher Punkte in einer Liste von Koordinaten
  FUNCTION get_point_2d_diff_count_1 ( this ) &
       RESULT(res)
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der unterschiedlichen Punkte in "this(:)"
    INTEGER                        :: res     ! 
    !
    res = COUNT( get_point_2d_diff_ind ( this ) )
    !
  END FUNCTION get_point_2d_diff_count_1
  !
  !! Ermittle die Listenindices f&uuml;r einen Vektor von Koordinaten, die
  !! von eventuell mehrdeutigen Punkten auf die eindeutigen verweisen
  FUNCTION get_point_2d_diff_idx_1 ( this ) &
       RESULT(res)
    !! Datenobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Indexliste mit Verweisen auf die eindeutigen Punkte aus "this(:)"
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariablen
    INTEGER               :: i, j, n  ! 
    !    
    res(:) = 0
    n      = 0
    DO i=1,SIZE(res)
       IF ( res(i) /= 0 ) CYCLE
       n      = n + 1
       res(i) = n
       DO j=i+1,SIZE(res)
          IF ( this(j) == this(i) ) res(j) = res(i)
       END DO
    END DO
    !
  END FUNCTION get_point_2d_diff_idx_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar )
  FUNCTION eq_point_2d_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%x == this2%x ) 
    l_ok(2) = ( this1%y == this2%y ) 
    ok      = ALL( l_ok )
    !
  END FUNCTION eq_point_2d_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar )
  FUNCTION eq_point_2d_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_point_2d_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_point_2d_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor )
  FUNCTION eq_point_2d_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_point_2d_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_point_2d_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor )
  FUNCTION eq_point_2d_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_point_2d_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_point_2d_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar )
  FUNCTION ne_point_2d_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%x /= this2%x ) 
    l_ok(2) = ( this1%y /= this2%y ) 
    ok      = ( l_ok(1) .OR. l_ok(2) )
    !
  END FUNCTION ne_point_2d_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar )
  FUNCTION ne_point_2d_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_point_2d_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ne_point_2d_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )
  FUNCTION ne_point_2d_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_point_2d_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ne_point_2d_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor )
  FUNCTION ne_point_2d_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_point_2d_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ne_point_2d_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(+)-Methoden und
  !     add_point_2d_xy-Methoden <<<            [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  !! Addiere zwei Datenobjekte ( Skalar / Skalar )
  FUNCTION ad_point_2d_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this2 ! 
    !! Additionsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = this1%x + this2%x
    this%y = this1%y + this2%y
    !
  END FUNCTION ad_point_2d_0_0
  !
  !! Addiere zwei Datenobjekte ( Vektor / Skalar )
  FUNCTION ad_point_2d_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this2    ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = ad_point_2d_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ad_point_2d_1_0
  !
  !! Addiere zwei Datenobjekte ( Skalar / Vektor )
  FUNCTION ad_point_2d_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = ad_point_2d_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ad_point_2d_0_1
  !
  !! Addiere zwei Datenobjekte ( Vektor / Vektor )
  FUNCTION ad_point_2d_1_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = ad_point_2d_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ad_point_2d_1_1
  !
  ! ----------------------------------------------------------------------
  !
  !! Addiere auf ein Objekt (Skalar) zwei Real-Variablen, <BR>
  !! die erste auf Komponente x, die zweite auf Komponente y
  FUNCTION ad_point_2d_xy_0 ( this1, x1, y1 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1
    !! Summand zur Komponenten x
    REAL (KIND = Double)  , INTENT(IN) :: x1 ! 
    !! Summand zur Komponenten y
    REAL (KIND = Double)  , INTENT(IN) :: y1 ! 
    !! Additionsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = this1%x + x1
    this%y = this1%y + y1
    !
  END FUNCTION ad_point_2d_xy_0
  !
  !! Addiere auf ein Objekt (Vektor) zwei Real-Variablen, <BR>
  !! die erste auf Komponente x, die zweite auf Komponente y
  FUNCTION ad_point_2d_xy_1 ( this1, x1, y1 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Summand zur Komponenten x
    REAL (KIND = Double)  , INTENT(IN) :: x1 ! 
    !! Summand zur Komponenten y
    REAL (KIND = Double)  , INTENT(IN) :: y1 ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = ad_point_2d_xy_0( this1(i), x1, y1)
    END DO
    !
  END FUNCTION ad_point_2d_xy_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-Methoden und
  !     sub_point_2d_xy-Methoden <<<            [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  !! Subtrahiere zwei Datenobjekte ( Skalar / Skalar )
  FUNCTION su_point_2d_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this2 ! 
    !! Subtraktionsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = this1%x - this2%x
    this%y = this1%y - this2%y
    !
  END FUNCTION su_point_2d_0_0
  !
  !! Subtrahiere zwei Datenobjekte ( Vektor / Skalar )
  FUNCTION su_point_2d_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this2    ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_point_2d_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION su_point_2d_1_0
  !
  !! Subtrahiere zwei Datenobjekte ( Skalar / Vektor )
  FUNCTION su_point_2d_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_point_2d_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION su_point_2d_0_1
  !
  !! Subtrahiere zwei Datenobjekte ( Vektor / Vektor )
  FUNCTION su_point_2d_1_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_point_2d_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION su_point_2d_1_1
  !
  ! ----------------------------------------------------------------------
  !
  !! Subtrahiere von einem Objekt (Skalar) zwei Real-Variablen, <BR>
  !! die erste von Komponente x, die zweite von Komponente y
  FUNCTION su_point_2d_xy_0 ( this1, x1, y1 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1
    !! Subtrahent zur Komponenten x
    REAL (KIND = Double)  , INTENT(IN) :: x1 ! 
    !! Subtrahent zur Komponenten y
    REAL (KIND = Double)  , INTENT(IN) :: y1 ! 
    !! Subtraktionsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = this1%x - x1
    this%y = this1%y - y1
    !
  END FUNCTION su_point_2d_xy_0
  !
  !! Subtrahiere von einem Objekt (Vektor) zwei Real-Variablen, <BR>
  !! die erste von Komponente x, die zweite von Komponente y
  FUNCTION su_point_2d_xy_1 ( this1, x1, y1 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Subtrahent zur Komponenten x
    REAL (KIND = Double)  , INTENT(IN) :: x1 ! 
    !! Subtrahent zur Komponenten y
    REAL (KIND = Double)  , INTENT(IN) :: y1 ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_point_2d_xy_0( this1(i), x1, y1)
    END DO
    !
  END FUNCTION su_point_2d_xy_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! umwandeln in Multiplikation Datenobjekt mit Integer/Real
  ! ----------------------------------------------------------------------
  !
  !! Multipliziere ein Datenobjekt ( Skalar ) mit einer Integer-Variablen
  FUNCTION mu_point_2d_0_i ( this1, int ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Integer-Variable
    INTEGER , INTENT(IN) :: int ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = this1%x * int
    this%y = this1%y * int
    !
  END FUNCTION mu_point_2d_0_i
  !
  !! Multipliziere eine Integer-Variable mit einem Datenobjekt ( Skalar )
  FUNCTION mu_point_2d_i_0 ( int, this1 ) &
       RESULT( this )
    !! Integer-Variable
    INTEGER , INTENT(IN) :: int 
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = int * this1%x 
    this%y = int * this1%y
    !
  END FUNCTION mu_point_2d_i_0
  !
  !! Multipliziere ein Datenobjekt ( Skalar ) mit einer Real-Variablen
  FUNCTION mu_point_2d_0_r ( this1, re ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Real-Variable
    REAL (KIND=Double), INTENT(IN) :: re ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = this1%x * re
    this%y = this1%y * re
    !
  END FUNCTION mu_point_2d_0_r
  !
  !! Multipliziere eine Real-Variable mit einem Datenobjekt ( Skalar )
  FUNCTION mu_point_2d_r_0 ( re, this1 ) &
       RESULT( this )
    !! Real-Variable
    REAL (KIND=Double), INTENT(IN) :: re 
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = re * this1%x 
    this%y = re * this1%y
    !
  END FUNCTION mu_point_2d_r_0
  !
  !! Multipliziere ein Datenobjekt ( Vektor ) mit einer Integer-Variablen
  FUNCTION mu_point_2d_1_i ( this1, int ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Integer-Variable 
    INTEGER , INTENT(IN) :: int
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_point_2d_0_i( this1(i), int )
    END DO
    !
  END FUNCTION mu_point_2d_1_i
  !
  !! Multipliziere eine Integer-Variable mit einem Datenobjekt ( Vektor ) 
  FUNCTION mu_point_2d_i_1 ( int, this1 ) &
         RESULT( this )
    !! Integer-Variable
    INTEGER , INTENT(IN) :: int   ! 
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_point_2d_i_0( int, this1(i) )
    END DO
    !
  END FUNCTION mu_point_2d_i_1
  !
  !! Multipliziere ein Datenobjekt ( Vektor ) mit einer Real-Variablen
  FUNCTION mu_point_2d_1_r ( this1, re ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Real-Variable 
    REAL (KIND=Double), INTENT(IN) :: re
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_point_2d_0_r( this1(i), re )
    END DO
    !
  END FUNCTION mu_point_2d_1_r
  !
  !! Multipliziere eine Real-Variable mit einem Datenobjekt ( Vektor ) 
  FUNCTION mu_point_2d_r_1 ( re, this1 ) &
         RESULT( this )
    !! Real-Variable
    REAL (KIND=Double), INTENT(IN) :: re   ! 
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_point_2d_r_0( re, this1(i) )
    END DO
    !
  END FUNCTION mu_point_2d_r_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  !! Dividiere ein Datenobjekt ( Skalar ) durch einer Integer-Variable, <BR>
  !! es wird nicht auf int==0 geprueft
  FUNCTION di_point_2d_0_i ( this1, int ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Integer-Variable
    INTEGER , INTENT(IN) :: int ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = this1%x / int
    this%y = this1%y / int
    !
  END FUNCTION di_point_2d_0_i
  !
  !! Dividiere ein Datenobjekt ( Skalar ) durch einer Real-Variable
  !! es wird nicht auf re==0. geprueft
  FUNCTION di_point_2d_0_r ( this1, re ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Real-Variable
    REAL (KIND=Double), INTENT(IN) :: re ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_point_2d) :: this ! 
    !
    this%x = this1%x / re
    this%y = this1%y / re
    !
  END FUNCTION di_point_2d_0_r
  !
  !! Dividiere ein Datenobjekt ( Vektor ) durch eine Integer-Variable
  FUNCTION di_point_2d_1_i ( this1, int ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Integer-Variable
    INTEGER , INTENT(IN) :: int    ! 
    !! Divisionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER:: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = di_point_2d_0_i( this1(i), int )
    END DO
    !
  END FUNCTION di_point_2d_1_i
  !
  !! Dividiere ein Datenobjekt ( Vektor ) durch eine Real-Variable
  FUNCTION di_point_2d_1_r ( this1, re ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Real-Variable
    REAL (KIND=Double), INTENT(IN) :: re
    !! Divisionsergebnis (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER:: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = di_point_2d_0_r( this1(i), re )
    END DO
    !
  END FUNCTION di_point_2d_1_r
  !
  !! Winkel zwischen einem Punkt this2 und seinem Bezugspunkt this1 in rad <BR>
  !! (Skalar/Skalar)
  FUNCTION angle_point_2d_0_0 ( this1, this2 ) &
         RESULT( angle )
    !! Objekt 1 als Bezugsobjekt
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 
    TYPE (t_point_2d) , INTENT(IN) :: this2 ! 
    !! Winkel zwischen Objekt 2 und Objekt 1 (Bezugspunkt)
    REAL (KIND=Double):: angle
    !
    IF( this1%x == this2%x .AND. this1%y == this2%y ) THEN
       angle = 0.
    ELSE
       angle = ATAN2( this2%y-this1%y , this2%x-this1%x )
    ENDIF
    !
  END FUNCTION angle_point_2d_0_0
  !
  !! Winkel (rad) zwischen einer Punktemenge this2 und deren Bezugspunkt this1 in <BR>
  !! (Skalar/Vektor)
  FUNCTION angle_point_2d_0_1 ( this1, this2 ) &
         RESULT( angle )
    !! Objekt 1 als Bezugspunkt
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) !
    !! Winkel (Vektor) zwischen Objekt 2 und Objekt 1 (Bezugspunkt)
    REAL (KIND=Double):: angle(SIZE(this2))
    INTEGER :: i ! 
    !
    DO i=1,SIZE(angle)
       angle(i) = angle_point_2d_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION angle_point_2d_0_1
  !
  !! Winkel (rad) zwischen einem Punkt this2 und einer Menge an Bezugspunkten this1 <BR>
  !! (Vektor/Skalar)
  FUNCTION angle_point_2d_1_0 ( this1, this2 ) &
         RESULT( angle )
    !! Objekt 1 als Bezugspunkte (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 
    TYPE (t_point_2d) , INTENT(IN) :: this2 !
    !! Winkel (Vektor) zwischen Objekt 2 und Objekt 1  (Bezugspunkte)
    REAL (KIND=Double):: angle(SIZE(this1))
    INTEGER :: i ! 
    !
    DO i=1,SIZE(angle)
       angle(i) = angle_point_2d_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION angle_point_2d_1_0
  !
  !! Winkel (rad) zwischen Punkten einer Punktmenge this2 und den Bezugs- <BR>
  !! punkten einer Punktmenge this1 (Vektor/Vektor)
  FUNCTION angle_point_2d_1_1 ( this1, this2 ) &
       RESULT( angle )
    !! Objekt 1 als Bezugsobjekt (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) !
    !! Winkel (Vektor) zwischen Objekt 2 und Objekt 1 (Bezugspunkte)
    REAL (KIND=Double):: angle(MIN(SIZE(this1),SIZE(this2)))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(angle)
       angle(i) = angle_point_2d_0_0( this1(i), this2(i))
    END DO
    !
  END FUNCTION angle_point_2d_1_1
  !
  !! Abstand zwischen einem Punkt this1 und einem Punkt this2 <BR>
  !! (Skalar/Skalar, R&uuml;ckgabewert hat den Kind-Parameter Double)
  FUNCTION dist_point_2d_0_0 ( this1, this2 ) &
       RESULT( distance )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 
    TYPE (t_point_2d) , INTENT(IN) :: this2 ! 
    !! Abstand zwischen Objekt 1 und Objekt 2
    REAL (KIND=Double) :: distance
    !
    distance = SQRT( (this1%x-this2%x)**2 + (this1%y-this2%y)**2 )
    !
  END FUNCTION dist_point_2d_0_0 
  !
  !! Abstand zwischen einem Punkt this1 und einer Punktemenge this2 <BR>
  !! (Skalar/Vektor, R&uuml;ckgabewert hat den Kind-Parameter Double)
  FUNCTION dist_point_2d_0_1 ( this1, this2 ) &
       RESULT( distance )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) !
    !! Abstand zwischen Objekt 1 und Objekt 2
    REAL (KIND=Double) :: distance(SIZE(this2))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(distance)
       distance(i) = dist_point_2d_0_0( this1, this2(i))
    END DO
    !
  END FUNCTION dist_point_2d_0_1 
  !
  !! Abstand zwischen einer Punktemenge this1 und einem Punkt this2 <BR>
  !! (Skalar/Vektor, R&uuml;ckgabewert hat den Kind-Parameter Double)
  FUNCTION dist_point_2d_1_0 ( this1, this2 ) &
       RESULT( distance )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2 !
    !! Abstand zwischen Objekt 1 und Objekt 2
    REAL (KIND=Double) :: distance(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(distance)
       distance(i) = dist_point_2d_0_0( this1(i), this2)
    END DO
    !
  END FUNCTION dist_point_2d_1_0 
  !
  !! Abstand zwischen den Punkten zweier Punktemengen this1 und this2 <BR>
  !! (Vektor/Vektor, R&uuml;ckgabewert hat den Kind-Parameter Double)
  FUNCTION dist_point_2d_1_1 ( this1, this2 ) &
       RESULT( distance )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) !
    !! Abstand zwischen Objekt 1 und Objekt 2
    REAL (KIND=Double) :: distance(MIN(SIZE(this1),SIZE(this2)))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(distance)
       distance(i) = dist_point_2d_0_0( this1(i), this2(i))
    END DO
    !
  END FUNCTION dist_point_2d_1_1 
  !
  !! Abstandsquadrat zwischen einem Punkt this1 und einem Punkt this2 <BR>
  !! (Skalar/Skalar, R&uuml;ckgabewert hat den Kind-Parameter Double)
  FUNCTION sq_dist_point_2d_0_0 ( this1, this2 ) &
       RESULT( square_dist )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 
    TYPE (t_point_2d) , INTENT(IN) :: this2 ! 
    !! Abstandsquadrat zwischen Objekt 1 und Objekt 2
    REAL (KIND=Double) :: square_dist
    !
    square_dist = (this2%x-this1%x)**2 + (this2%y-this1%y)**2
    !
  END FUNCTION sq_dist_point_2d_0_0 
  !
  !! Abstandsquadrat zwischen einem Punkt this1 und einer Punktemenge this2 <BR>
  !! (Skalar/Vektor, R&uuml;ckgabewert hat den Kind-Parameter Double)
  FUNCTION sq_dist_point_2d_0_1 ( this1, this2 ) &
       RESULT( square_dist )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) !
    !! Abstandsquadrat zwischen Objekt 1 und Objekt 2
    REAL (KIND=Double) :: square_dist(SIZE(this2))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(square_dist)
       square_dist(i)  = sq_dist_point_2d_0_0 ( this1, this2(i) )
    ENDDO
    !
  END FUNCTION sq_dist_point_2d_0_1 
  !
  !! Abstandquadrat zwischen einer Punktemenge this1 und einem Punkt this2 <BR>
  !! (Skalar/Vektor, R&uuml;ckgabewert hat den Kind-Parameter Double)
  FUNCTION sq_dist_point_2d_1_0 ( this1, this2 ) &
       RESULT( square_dist )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2 !
    !! Abstandquadrat zwischen Objekt 1 und Objekt 2
    REAL (KIND=Double) :: square_dist(SIZE(this1))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(square_dist)
       square_dist(i)  = sq_dist_point_2d_0_0 ( this1(i), this2 )
    ENDDO
    !
  END FUNCTION sq_dist_point_2d_1_0 
  !
  !! Abstandquadrat zwischen den Punkten zweier Punktemengen this1 und this2 <BR>
  !! (Vektor/Vektor, R&uuml;ckgabewert hat den Kind-Parameter Double)
  FUNCTION sq_dist_point_2d_1_1 ( this1, this2 ) &
       RESULT( square_dist )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this2(:) !
    !! Abstandquadrat zwischen Objekt 1 und Objekt 2
    REAL (KIND=Double) :: square_dist(MIN(SIZE(this1),SIZE(this2)))
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(square_dist)
       square_dist(i) = sq_dist_point_2d_0_0( this1(i), this2(i))
    END DO
    !
  END FUNCTION sq_dist_point_2d_1_1 
  !
  !! Rotation eines skalaren Objektes um den Koordinatenursprung in mathematisch  <BR>
  !! positiver Drehrichtung um einen Winkel angegeben in rad 
  FUNCTION rot_point_2d_0 ( this1, rot_angle ) &
         RESULT( this )
    !! Objekt 1 
    TYPE (t_point_2d) , INTENT(IN) :: this1 ! 
    !! Drehwinkel in rad
    REAL (KIND=Double) , INTENT(IN) :: rot_angle
    !! Objekt nach der Drehung
    TYPE (t_point_2d) :: this ! 
    ! 
    this%x = cos(rot_angle) * this1%x - sin(rot_angle) * this1%y
    this%y = sin(rot_angle) * this1%x + cos(rot_angle) * this1%y
    !
  END FUNCTION rot_point_2d_0
  !
  !! Rotation eines vektoriellen Objektes um den Koordinatenursprung 
  !! in mathematisch positiver Drehrichtung um einen Winkel angegeben in rad
  FUNCTION rot_point_2d_1 ( this1, rot_angle ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this1(:) ! 
    !! Drehwinkel in rad
    REAL (KIND=Double) , INTENT(IN) :: rot_angle
    !! Objekt nach der Drehung (Vektor)
    TYPE (t_point_2d) :: this(SIZE(this1))
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = rot_point_2d_0( this1(i), rot_angle)
    END DO
    !
  END FUNCTION rot_point_2d_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INSIDE-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  !! Liegt ein 2D-Punkt innerhalb eines durch 2D-Punkte
  !! aufgespannten Polygons?
  FUNCTION inside_point_2d_0 ( this, polygon ) &
       RESULT( inside )
    !! Datenobjekt (Skalar)
    TYPE (t_point_2d) , INTENT(IN) :: this
    !! Polygon
    TYPE (t_point_2d) , INTENT(IN) :: polygon(:)
    !! Datenobjekt innerhalb Polygon?
    LOGICAL :: inside
    !! Gehoert der Rand zum Polygongebiet dazu? Hier Ja
    LOGICAL           , PARAMETER :: lrinc=.TRUE.
    !! Koordinaten des 2D-Punktes
    REAL (KIND=Double) :: xp, yp
    !! Anzahl Polygonpunkte
    INTEGER            :: n_polygon
    !! Koordinaten der Polygonpunkte
    REAL (KIND=Double) :: xy_polygon(2,SIZE(polygon))
    !
    xp = get_point_2d_x( this )
    yp = get_point_2d_y( this )
    !
    n_polygon = SIZE(polygon)
    !
    xy_polygon(1,1:n_polygon) = get_point_2d_x( polygon )
    xy_polygon(2,1:n_polygon) = get_point_2d_y( polygon )
    !
    CALL inside_point_2d_d &
         ( xp, yp, n_polygon, xy_polygon, lrinc, inside )
    !
  END FUNCTION inside_point_2d_0
  !
  !! Liegt eine Liste von 2D-Punkten jeder fuer sich innerhalb
  !! eines durch 2D-Punkte aufgespannten Polygons?
  FUNCTION inside_point_2d_1 ( this, polygon ) &
       RESULT( inside )
    !! Datenobjekte (Vektor)
    TYPE (t_point_2d) , INTENT(IN) :: this(:)
    !! Polygon
    TYPE (t_point_2d) , INTENT(IN) :: polygon(:)
    !! Datenobjekte innerhalb Polygon?
    LOGICAL :: inside(SIZE(this))
    !! Zaehler
    INTEGER :: i
    !
    DO i = 1, SIZE( this )
       inside( i ) = inside_point_2d_0( this( i ), polygon )
    END DO
    !
  END FUNCTION inside_point_2d_1
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
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "b_point_2d" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_point_2d ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert
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
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls
  SUBROUTINE init_point_2d_all_errors ( )
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
               '--> INIT_point_2d ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_point_2d ausfuehren' )
       END IF
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 003
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_point_2d"\n'//&
               'Typ-Komponente = "x" ist zu gross\n' //&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 004
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_point_2d"\n'//&
               'Typ-Komponente = "x" ist zu klein\n' //&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 005
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_point_2d"\n'//&
               'Typ-Komponente = "y" ist zu gross\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 006
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_point_2d"\n'//&
               'Typ-Komponente = "y" ist zu klein\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 007
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_point_2d" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 008
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_point_2d" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 009
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_point_2d" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 010
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_point_2d"\n'//&
               'Typ-Komponente = "x"\n'//&
               '--> Code in Modul "b_point_2d" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 011
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_point_2d"\n'//&
               'Typ-Komponente = "y"\n'//&
               '--> Code in Modul "b_point_2d" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 012
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_point_2d"\n'//&
               '--> Code in Modul "b_point_2d" / Daten pruefen' )
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
  END SUBROUTINE init_point_2d_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls
  SUBROUTINE clear_point_2d_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_point_2d_all_errors
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
  ! --> nicht benoetigte INIT-Routinen bitte unbedingt loeschen <---------
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren der Feld-Komponente "x" mit Default-Werten
  SUBROUTINE init_point_2d_x ( this )
    !! Datenobjekt
    TYPE (t_point_2d) , INTENT(INOUT) :: this   ! 
    !! Initialisierungswert x
    REAL (KIND=Double) , PARAMETER :: c_var = HUGE(this%x) ! 
    !
    this%x = c_var
    !
  END SUBROUTINE init_point_2d_x
  !
  !! Initialisieren der Feld-Komponente "y" mit Default-Werten
  SUBROUTINE init_point_2d_y ( this )
    !! Datenobjekt
    TYPE (t_point_2d) , INTENT(INOUT) :: this   ! 
    !! Initialisierungswert y
    REAL (KIND=Double) , PARAMETER :: c_var = HUGE(this%y)
    !
    this%y = c_var
    !
  END SUBROUTINE init_point_2d_y
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "x" eines Datenobjektes o.k. ist
  FUNCTION ok_point_2d_x ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_point_2d) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_point_2d_x' ! 
    REAL (KIND=Double) :: max_x = 10000000. ! 
    !
    ok = .true. 
    IF(this%x > max_x) THEN
       ok = .false.
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    ELSE IF(this%x < -1. * max_x) THEN
       ok = .false.
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    ENDIF
    !
  END FUNCTION ok_point_2d_x
  !
  !! Pr&uuml;fe, ob die Komponente "y" eines Datenobjektes o.k. ist
  FUNCTION ok_point_2d_y ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_point_2d) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_point_2d_y' ! 
    REAL (KIND=Double) :: max_y = 10000000. ! 
    !
    ok = .true.
    IF(this%y > max_y) THEN
       ok = .false.
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    ELSE IF(this%y < -1. * max_y) THEN
       ok = .false.
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    ENDIF
    !
  END FUNCTION ok_point_2d_y
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "x" eines Datenobjektes
  SUBROUTINE print_point_2d_x ( this )
    !! Datenobjekt
    TYPE (t_point_2d) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_point_2d_x' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%x ! schreibe Inhalt von this%x
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente x = ', F15.5)
    !
  END SUBROUTINE print_point_2d_x
  !
  !! Drucke den Inhalt der Komponente "y" eines Datenobjektes
  SUBROUTINE print_point_2d_y ( this )
    !! Datenobjekt
    TYPE (t_point_2d) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_point_2d_y' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%y ! schreibe Inhalt von this%y
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente y = ', F15.5)
    !
  END SUBROUTINE print_point_2d_y
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INSIDE-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  SUBROUTINE inside_point_2d_d &
       ( YP, XP, NYX, YX, LRINC, LFIT )
    !                                       R
    !
    !     R = RUECKGABEPARAMETER, DIE NICHT ALS KONSTANTE ODER AUSDRUECKE
    !     ----------------------  UEBERGEBEN WERDEN DUERFEN
    !
    !     DIESE ROUTINE PRUEFT, OB DER PUNKT  Y P / X P  INNERHALB DES BELIEBI-
    !     GEN  N - ECKES  Y X  VON  N Y X  PUNKTEN LIEGT. ANFANGSINDEX IST = 1.
    !
    !     ZU DEN KOORDINATEN IN FELD  Y X  GILT ------>
    !     YX(1,I) = RECHTSWERT IN EINEM BELIEBIGEN RECHTW. KOORDINATENSYSTEM
    !     YX(2,I) = HOCHWERT
    !
    !     DIE  N  ECKPUNKTE MUESSEN IN FELD  Y X  -  UMFAHRUNGSSINN BELIEBIG -
    !     IN DER STRENGEN FOLGE DER PERIPHERIE ABGELEGT SEIN.
    !     DIE SEITEN DES N-ECKES DUERFEN SICH NICHT SCHNEIDEN. ANFANGS- UND
    !     ENDPUNKT KOENNEN, MUESSEN ABER NICHT IDENTISCH SEIN. DIE GLEICHSET-
    !     ZUNG DES 1. UND LETZTEN PUNKTES ERFOLGT IN DIESER ROUTINE AUTOMATISCH,
    !     JEDOCH UNSCHAEDLICH FUER DEN FALL, DASS DIE GLEICHHEIT VON 1. UND
    !     LETZTEM PUNKT SCHON GEGEBEN WAERE.
    !
    !
    !     DAS PRUEFERGEBNIS WIRD WIE FOLGT ZURUECKGEGEBEN:
    !     ================================================
    !
    !     N P = NUMMER DES ECKPUNKTES, WENN  Y P / X P  IN IHM LIEGT, FALLS
    !           NICHT AUF ECKPUNKT, WIRD  N P  MIT NULL ZURUECKGEGEBEN.
    !
    !     N S = NUMMER DER SEITE ALS NUMMER DES AM ANFANG DER SEITE LIEGENDEN
    !           ECKPUNKTES, WENN DER PUNKT  Y P / X P  AUF EINER SEITE LIEGT.
    !           LIEGT DER PUNKT NICHT AUF EINER SEITE, WIRD  N S  MIT NULL ZU-
    !           RUECKGEGEBEN.
    !
    !     LIEGT DER PUNKT  Y P / X P  INNERHALB DES N-ECKES, SO WIRD
    !
    !          --------->    N P > 0  UND  N S > 0    <----------
    !
    !     ZURUECKGEGEBEN. DEMNACH IST BEI NP = 0 UND NS = 0 PUNKTLAGE AUSSER-
    !     HALB.
    !
    !     VERFAHREN:
    !     ==========
    !
    !     DER PUNKT  Y P / X P  LIEGT INNERHALB DES  N - ECKES, WENN DIE WAAG-
    !     RECHTE GERADE ODER DIE SENKRECHTE GERADE DURCH IHN LINKS ODER RECHTS
    !     ODER UNTER- ODER OBERHALB DES PUNKTES  Y P / X P  MIT DEN SEITEN DES
    !     N - ECKES ZU JE EINER UNGERADEN ANZAHL VON SCHNITTPUNKTEN FUEHRT.
    !
    !     ES WIRD HIER DER FALL MIT SCHNITTPUNKTANZAHL RECHTS VON YP/XP UNTER-
    !     SUCHT. BEZUG IST ALSO DIE WAAGRECHTE GERADE DURCH PUNKT  YP/XP.
    !
    !
    INTEGER            :: NYX,NA,NP,NS,I,J,K,N
    LOGICAL            :: TRIN,TQXIJ,TQXI,TQYJ,TQXJ,LFIT, LRINC
    REAL (KIND=Double) :: YX(2,NYX), XP, YP
    REAL (KIND=Double) :: DXP, DYP, DYX1J,DYX2J,DYX1I,DYX2I,DYX1NA,DYX2NA,DYS
    !
    !     PROGRAMMBEGINN
    !     ==============
    !
    DXP=XP
    DYP=YP
    !
    TRIN =.FALSE.
    NA=1
    NP=0
    NS=0
    IF(NYX.LT.1)GOTO 10
    NP=NA
    DYX1NA=YX(1,NA)
    DYX2NA=YX(2,NA)
    IF(DYX1NA.EQ.DYP.AND.DYX2NA.EQ.DXP)GOTO 10
    !
    !     GLEICHE PUNKTE AM ENDE NICHT GELTEN LASSEN
    !     ------------------------------------------
    !
    DO 15 I=NYX,NA+1,-1
       
       DYX1NA=YX(1,NA)
       DYX2NA=YX(2,NA)
       DYX1I=YX(1,I)
       DYX2I=YX(2,I)
       
       IF (DYX1I.NE.DYX1NA.OR.DYX2I.NE.DYX2NA) GOTO 20
15  CONTINUE
    !
    NP=0
    GOTO 10
    !
20  N=I
    I=NA
    !               <--- INDEX LAUFENDER PUNKT, DER NUR BEI NICHT
    !                    IDENTISCHEN PUNKTEN WEITERLAEUFT
    !
    !     DURCHLAUF N-ECK
    !     ---------------
    !
    DO 25 K=NA,N
       J=K+1
       IF(K.EQ.N)J=NA
       !
       !
       !     MIT VORGAENGER IDENTISCHE ECKPUNKTE UEBERGEHEN
       !     ----------------------------------------------
       !
       DYX1I=YX(1,I)
       DYX2I=YX(2,I)
       DYX1J=YX(1,J)
       DYX2J=YX(2,J)
       IF(DYX1J.EQ.DYX1I.AND.DYX2J.EQ.DYX2I)GOTO 25
       !
       !
       !     ALLE  N-ECK-SEITEN  MIT ECKPUNKTKOORDINATEN < YP
       !     ODER < XP HABEN KEINEN EINFLUSS AUF DAS ERGEBNIS
       !     ------------------------------------------------
       !
       DYX1I=YX(1,I)
       DYX2I=YX(2,I)
       DYX1J=YX(1,J)
       DYX2J=YX(2,J)
       IF(DYX1I.LT.DYP.AND.DYX1J.LT.DYP.OR.&
            DYX2I.LT.DXP.AND.DYX2J.LT.DXP)GOTO 35
       !
       !
       !     IDENTITAET MIT PUNKT  YP/XP  FESTSTELLEN
       !     ----------------------------------------
       !
       NP=J
       NS=0
       DYX1J=YX(1,J)
       DYX2J=YX(2,J)
       TQYJ=DYX1J.EQ.DYP
       TQXJ=DYX2J.EQ.DXP
       IF(TQYJ.AND.TQXJ)GOTO 10
       !
       !
       !     ALLE SEITEN MIT ECK-PUNKTKOORDINATEN = UND > XP ODER <
       !     HABEN  AB  HIER  KEINEN EINFLUSS MEHR AUF DAS ERGEBNIS
       !     ------------------------------------------------------
       !
       DYX1I=YX(1,I)
       DYX2I=YX(2,I)
       DYX1J=YX(1,J)
       DYX2J=YX(2,J)
       IF(DYX2I.GE.DXP.AND.DYX2J.GT.DXP.OR.&
            DYX2I.GT.DXP.AND.DYX2J.GE.DXP)GOTO 35
       NP=0
       NS=K
       DYX2J=YX(2,I)
       TQXI=DYX2I.EQ.DXP
       TQXIJ=TQXI.AND.TQXJ
       !
       !
       !     ECKPUNKT-SEITE  I - J  LIEGT IN WAAGRECHTER GERADE  XP
       !     ------------------------------------------------------
       !
       IF(.NOT.(TQXI.OR.TQXJ))GOTO 45
       DYX1I=YX(1,I)
       DYX1J=YX(1,J)
       IF(TQXIJ.AND.(DYX1I.LT.DYP.OR.DYX1J.LT.DYP))GOTO 10
       IF(TQXIJ)GOTO 35
       !
       !
       !     UNGERADENANZAHL VON ECKPUNKTEN AUF GERADE  XP  FESTSTELLEN
       !     ----------------------------------------------------------
       !
       DYX1I=YX(1,I)
       DYX1J=YX(1,J)
       IF(TQXI.AND.DYX1I.GT.DYP.OR.TQXJ.AND.DYX1J.GT.DYP)GOTO 40
       GOTO 35
       !
       !
       !     ECKPUNKTSEITE  I - J  KREUZT DIE GERADE  XP
       !     -------------------------------------------
       !
45     CONTINUE
       DYX1I=YX(1,I)
       IF(DYX1I.EQ.DYP.AND.TQYJ)GOTO 10
       !                                        <--- ECKPUNKTSEITE LIEGT IN YP
       !
       DYX1I=YX(1,I)
       DYX2I=YX(2,I)
       DYX1J=YX(1,J)
       DYX2J=YX(2,J)
       DYS=(DYX1J*(DXP-DYX2I)+DYX1I*(DYX2J-DXP))/(DYX2J-DYX2I)
       IF(DYS.EQ.DYP)GOTO 10
       IF(DYS.LT.DYP)GOTO 35
       !
40     TRIN=.NOT.TRIN
       !
35     I=J
       !               <------- LAUFINDEX I FORTSCHALTEN, BLEIBT
       !                        BEI IDENTISCHEN PUNKTEN STEHEN
25  CONTINUE
    !
    IF(TRIN)THEN
       NP=1
       NS=1
    ELSE
       NP=0
       NS=0
    END IF
    !
10  LFIT=.FALSE.
    
    IF(LRINC) THEN
       IF(NP.GT.0.OR.NS.GT.0) LFIT=.TRUE.
    ELSE
       IF(NP.GT.0.AND.NS.GT.0) LFIT=.TRUE.
    ENDIF
    
  END SUBROUTINE inside_point_2d_d
  !
END MODULE b_point_2d
! TailOfBaseModule --------------------------------------------------------
