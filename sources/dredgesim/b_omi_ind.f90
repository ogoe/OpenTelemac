! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden (teilweise) analog zu der OpenMI-Schnittstelle <EM>IElementSet</EM></h2>
!! @author G. Lang
!! @version 2.3 vom 03/01/07, Quellcode: mod_b_omi_ind.f90
!! <HR>
!! type and methods (in parts) equivalent to OpenMI-interface <EM>IElementSet</EM> <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-02-15 : G. Lang : Erstversion
!  01.02 : 2005-02-17 : G. Lang : + has_omi_ind_stru_id
!  01.03 : 2005-02-24 : G. Lang : + Komponente point_list(:) und Funktionen
!  01.04 : 2005-03-02 : G. Lang : Kopierfunktion "copy_omi_ind"
!  01.05 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  01.06 : 2005-05-30 : G. Lang : c_len_omi_ind_stru_id von 40 auf 80 vergroessert
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!  02.02 : 2005-07-29 : G. Lang : create_omi_ind_lay_pt_d und create_omi_ind_pt_d ergaenzt
!  02.03 : 2007-03-01 : G. Lang : c_len_omi_ind_id von 40 auf 80 vergroessert
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden (teilweise) analog OpenMI-Interface <EM>IElementSet</EM>.
!! Implementiert einen Typ zur Aufnahme von Verweisen auf Datenpunkte, die
!! in einer Struktur des Typs "t_omi_xyz" abgelegt sind. Dies erlaubt die
!! Definition verschiedener Sub-Strukturen basierend auf denselben Koordinaten
!! (z.B. gesamtes Gitter, offene und geschlossene R&auml;nder). Typ und
!! Methoden erleichtern den Austausch der zwischen verschiedenen 
!! OpenMI-konformen Komponenten.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_ind";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_ind";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_ind";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_ind";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_ind";
!!    <LI> Extrahieren von Daten aus einem Datenvektor f&uuml;r das Objekt "t_omi_ind"
!!         bzw. eine Teilstruktur davon;
!!    <LI> Interpolieren von Daten f&uuml;r ein Koordinatenobjekt und Extrahieren 
!!         von Daten aus einem Datenvektor f&uuml;r das Objekt "t_omi_ind"
!!         bzw. eine Teilstruktur davon;
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_ind".
!! </OL>
!!
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_omi_ind"
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> id            : kurzer Identifikationsbezeichner
!!     <LI> description   : ausf&uuml;hrliche verbale Beschreibung
!!     <LI> stru_id(:)    : (optional) beschreibende Namen der verschiedenen Strukturen
!!     <LI> stru_start(:) : Startindizes der verschiedenen Strukturen in der Komponente "stru_list(:)"
!!     <LI> stru_len(:)   : L&auml;nge der verschiedenen Strukturen
!!     <LI> stru_list(:)  : Liste der Datenpunkte aller Strukturen
!!     <LI> point_list(:) : Liste der unterschiedlichen Datenpunkte 
!! </OL>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                      
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden:
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_omi_ind mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> Extrahieren und ggf. interpolieren von Daten (GET_OMI_IND_DATA);
!!    <LI> De-Initialisieren des Moduls b_omi_ind mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_IND_ALL_ERRORS.
!!                                                                    <BR>
!! <HR>
!
MODULE b_omi_ind
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Single,            &
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
  ! [A.3] weitere BASIS-Module (ONLY benutzen!)
  !
  USE b_omi_xyz, ONLY : &
       !   Typdefinitionen
       t_omi_xyz,       &
       !   Parameter 
       !   Variablen mit INTENT(IN)
       !   Variablen mit INTENT(INOUT)
       !   Variablen mit INTENT(OUT)
       !   Routinen / Interfaces
       init_omi_xyz,                 &
       clear_omi_xyz,                &
       setup_omi_xyz_prn_lun,        &
       setup_omi_xyz_trc_lun,        &
       get_omi_xyz_point_count,      &
       get_omi_xyz_data,             &
       get_omi_xyz_x_ref,            &
       get_omi_xyz_y_ref,            &
       get_omi_xyz_z_ref,            &
       get_omi_xyz_west,             &
       get_omi_xyz_east,             &
       get_omi_xyz_north,            &
       get_omi_xyz_south,            &
       get_omi_xyz_top,              &
       get_omi_xyz_bottom,           &
       get_omi_xyz_column_point_idx, &
       get_omi_xyz_layer_point_idx,  &
       get_omi_xyz_layer_name,       &
       get_omi_xyz_layer_2d_mask
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
  ! [C.0] Konstantwerte f&uuml;r die Typ-Definition (Parameter)
  !! max. Anzahl der Zeichen in der Komponente <EM>id</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_ind_id=80          ! 
  !! max. Anzahl der Zeichen in der Komponente <EM>description</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_ind_description=80 ! 
  !! max. Anzahl der Zeichen in der Komponente <EM>stru_id</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_ind_stru_id=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! id            : kurzer Identifikationsbezeichner <BR>
  !! description   : ausf&uuml;hrliche verbale Beschreibung <BR>
  !! stru_id(:)    : (optional) beschreibende Namen der verschiedenen Strukturen <BR>
  !! stru_start(:) : Startindizes der verschiedenen Strukturen in der Komponente "stru_list(:)" <BR>
  !! stru_len(:)   : L&auml;nge der verschiedenen Strukturen <BR>
  !! stru_list(:)  : Liste der Datenpunkte aller Strukturen <BR>
  !! point_list(:) : Liste der unterschiedlichen Datenpunkte 
  TYPE , PUBLIC :: t_omi_ind
     PRIVATE
     CHARACTER (LEN=c_len_omi_ind_id)                :: id            ! 
     CHARACTER (LEN=c_len_omi_ind_description)       :: description   ! 
     CHARACTER (LEN=c_len_omi_ind_stru_id) , POINTER :: stru_id(:)    ! 
     INTEGER                               , POINTER :: stru_start(:) ! 
     INTEGER                               , POINTER :: stru_len(:)   ! 
     INTEGER                               , POINTER :: stru_list(:)  ! 
     INTEGER                               , POINTER :: point_list(:) ! 
  END TYPE t_omi_ind
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Undefined-Wert f&uuml;r INTEGER-Komponenten
  INTEGER            , PUBLIC , PARAMETER :: c_undef_omi_ind_int=-999         ! 
  !! Undefined-Wert f&uuml;r DOUBLE-Komponenten
  REAL (KIND=Double) , PUBLIC , PARAMETER :: c_undef_omi_ind_double=1.0E+31_Double ! 
  !! Undefined-Wert f&uuml;r CHARACTER-Komponenten
  CHARACTER (LEN=9)  , PUBLIC , PARAMETER :: c_undef_omi_ind_char='undefined' ! 
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_omi_ind
     MODULE PROCEDURE init_omi_ind_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_ind
     MODULE PROCEDURE clear_omi_ind_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_ind_prn_lun
     MODULE PROCEDURE setup_omi_ind_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_ind_trc_lun
     MODULE PROCEDURE setup_omi_ind_trc_lun_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_omi_ind"; NULLIFY f&uuml;r dynamische 
  !! Komponenten-Felder und Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE new_omi_ind
     MODULE PROCEDURE new_omi_ind_0  ! 
     MODULE PROCEDURE new_omi_ind_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_ind"; ggf. De-Allokieren von 
  !! Memory und teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE kill_omi_ind
     MODULE PROCEDURE kill_omi_ind_0 ! 
     MODULE PROCEDURE kill_omi_ind_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_ind" auf G&uuml;ltigkeit: <BR>
  !! a) ein Datenobjekt (Skalar)    <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! c) f&uuml;r ein Datenobjekt und ein Koordinatenobjekt "t_omi_xyz" <BR>
  !! d) f&uuml;r mehrere Datenobjekte und ein Koordinatenobjekt "t_omi_xyz"
  INTERFACE ok_omi_ind
     MODULE PROCEDURE ok_omi_ind_0 ! 
     MODULE PROCEDURE ok_omi_ind_1 ! 
     MODULE PROCEDURE ok_omi_ind_xyz_0 ! 
     MODULE PROCEDURE ok_omi_ind_xyz_1 ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_ind": <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_ind" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_ind
     MODULE PROCEDURE print_omi_ind_0 ! 
     MODULE PROCEDURE print_omi_ind_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_ind_static
     MODULE PROCEDURE print_omi_ind_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_ind_all_errors
     MODULE PROCEDURE print_omi_ind_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "id" in "t_omi_ind" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ind_id
     MODULE PROCEDURE set_omi_ind_id_0_0 ! 
     MODULE PROCEDURE set_omi_ind_id_1_0 ! 
  END INTERFACE
  !! Setze Komponente "description" in "t_omi_ind" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ind_description
     MODULE PROCEDURE set_omi_ind_description_0_0 ! 
     MODULE PROCEDURE set_omi_ind_description_1_0 ! 
  END INTERFACE
  !! Setze Komponente "stru_id" in "t_omi_ind" auf Benutzerwert; <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ind_stru_id
     MODULE PROCEDURE set_omi_ind_stru_id_0_1 ! 
     MODULE PROCEDURE set_omi_ind_stru_id_1_1 ! 
  END INTERFACE
  !! Setze Komponente "stru_id" in "t_omi_ind" auf Benutzerwert: <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ind_stru_start
     MODULE PROCEDURE set_omi_ind_stru_start_0_1 ! 
     MODULE PROCEDURE set_omi_ind_stru_start_1_1 ! 
  END INTERFACE
  !! Setze Komponente "stru_len" in "t_omi_ind" auf Benutzerwert: <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ind_stru_len
     MODULE PROCEDURE set_omi_ind_stru_len_0_1 ! 
     MODULE PROCEDURE set_omi_ind_stru_len_1_1 ! 
  END INTERFACE
  !! Setze Komponente "stru_list" in "t_omi_ind" auf Benutzerwert: <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ind_stru_list
     MODULE PROCEDURE set_omi_ind_stru_list_0_1 ! 
     MODULE PROCEDURE set_omi_ind_stru_list_1_1 ! 
  END INTERFACE
  !! Setze Komponente "point_list" in "t_omi_ind" auf Benutzerwert: <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ind_point_list ! interne Funktion
     MODULE PROCEDURE set_omi_ind_point_list_0_1 ! 
     MODULE PROCEDURE set_omi_ind_point_list_1_1 ! 
  END INTERFACE
  !
  !! Hole Komponente "id" aus "t_omi_ind": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ind_id
     MODULE PROCEDURE get_omi_ind_id_0_0 ! 
     MODULE PROCEDURE get_omi_ind_id_1_0 ! 
  END INTERFACE
  !! Hole Komponente "description" aus "t_omi_ind": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ind_description
     MODULE PROCEDURE get_omi_ind_description_0_0 ! 
     MODULE PROCEDURE get_omi_ind_description_1_0 ! 
  END INTERFACE
  !! Hole Komponente "stru_id" aus "t_omi_ind": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ind_stru_id_ref
     MODULE PROCEDURE get_omi_ind_stru_id_ref_0_1 ! 
  END INTERFACE
  !! Hole Komponente "stru_start" aus "t_omi_ind": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ind_stru_start_ref
     MODULE PROCEDURE get_omi_ind_stru_start_ref_0_1 ! 
  END INTERFACE
  !! Hole Komponente "stru_len" aus "t_omi_ind": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ind_stru_len_ref
     MODULE PROCEDURE get_omi_ind_stru_len_ref_0_1 ! 
  END INTERFACE
  !! Hole Komponente "stru_list" aus "t_omi_ind": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ind_stru_list_ref
     MODULE PROCEDURE get_omi_ind_stru_list_ref_0_1 ! 
  END INTERFACE
  !! Hole Komponente "point_list" aus "t_omi_ind": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ind_point_list_ref
     MODULE PROCEDURE get_omi_ind_point_list_ref_0_1 ! 
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_ind": <BR>
  !! a) f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! b) f&uuml;r viele Werte <EM>id(:)</EM> (Vektor)
  INTERFACE get_omi_ind_idx
     MODULE PROCEDURE get_omi_ind_idx_1_0
     MODULE PROCEDURE get_omi_ind_idx_1_1
  END INTERFACE
  !! ermittle die Anzahl der Strukturen in dem aktuellen Datenobjekt: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_ind_stru_count
     MODULE PROCEDURE get_omi_ind_stru_count_0
     MODULE PROCEDURE get_omi_ind_stru_count_1
  END INTERFACE
  !! ermittle die Anzahl der Koordinatenpunkte in dem aktuellen Datenobjekt: <BR>
  !! a) f&uuml;r ein Objekt, alle Positionen             <BR>
  !! b) f&uuml;r viele Objekte, jeweils alle Positionen <BR>
  !! c) f&uuml;r ein Objekt, Positionen einer Struktur  <BR>
  !! d) f&uuml;r ein Objekt, Positionen vieler Strukturen
  INTERFACE get_omi_ind_point_count
     MODULE PROCEDURE get_omi_ind_point_count_0
     MODULE PROCEDURE get_omi_ind_point_count_1
     MODULE PROCEDURE get_omi_ind_point_count_0_0
     MODULE PROCEDURE get_omi_ind_point_count_0_1
  END INTERFACE
  !! suche nach einem bestimmten Wert der Struktur <EM>stru_id</EM> in einem 
  !! Datenobjekt  des Typs "t_omi_ind": <BR>
  !! a) f&uuml;r ein Objekt und eine Strukturkennung <BR>
  !! b) f&uuml;r ein Objekte und viele Strukturkennungen
  INTERFACE get_omi_ind_stru_id_idx
     MODULE PROCEDURE get_omi_ind_stru_id_idx_0_0
     MODULE PROCEDURE get_omi_ind_stru_id_idx_0_1
  END INTERFACE
  !! gib den Namen f&uuml;r eine bestimmte Struktur an Position "idx" zur&uuml;ck <BR>
  !! a) f&uuml;r ein Datenobjekt und einen Index <BR>
  !! b) f&uuml;r ein Datenobjekt und mehrere Indices <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ind_stru_id
     MODULE PROCEDURE get_omi_ind_stru_id_0_0
     MODULE PROCEDURE get_omi_ind_stru_id_0_1
  END INTERFACE
  !! gib die Anzahl der Eintr&auml;ge f&uuml;r Strukturen zur&uuml;ck <BR>
  !! a) Anzahl aller Eintr&auml;ge f&uuml;r ein Objekt <BR>
  !! b) Anzahl aller Eintr&auml;ge f&uuml;r viele Objekte <BR>
  !! c) Anzahl der Eintr&auml;ge einer Struktur f&uuml;r ein Objekt <BR>
  !! d) Anzahl der Eintr&auml;ge mehrerer Strukturen f&uuml;r ein Objekt
  INTERFACE get_omi_ind_stru_len_count
     MODULE PROCEDURE get_omi_ind_stru_len_count_0
     MODULE PROCEDURE get_omi_ind_stru_len_count_1
     MODULE PROCEDURE get_omi_ind_stru_len_count_0_0
     MODULE PROCEDURE get_omi_ind_stru_len_count_0_1
  END INTERFACE
  !! ermittle die maximale Strukturl&auml;nge in einem Datenobjekt <BR>
  !! a) f&uuml;r ein Datenobjekt
  INTERFACE get_omi_ind_stru_len_max
     MODULE PROCEDURE get_omi_ind_stru_len_max_0
  END INTERFACE
  !! ermittle die minimale Strukturl&auml;nge in einem Datenobjekt <BR>
  !! a) f&uuml;r ein Datenobjekt
  INTERFACE get_omi_ind_stru_len_min
     MODULE PROCEDURE get_omi_ind_stru_len_min_0
  END INTERFACE
  !
  !! extrahiere die zu dem Datenobjekt (oder eines Teils davon) geh&ouml;renden
  !! Daten, bezogen auf Objekte vom Typ XYPoint bzw. XYZPoint: <BR>
  !! a) alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" bei 
  !!    Vorgabe der auf das korrespondierende Koordinatenobjekt "t_omi_xyz" 
  !!    interpolierten Daten <BR> 
  !! b) alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" bei 
  !!    Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !!    interpolierten Daten <BR> 
  !! c) alle Daten (INTEGER) f&uuml;r ein Datenobjekt "t_omi_ind" bei 
  !!    Vorgabe der auf das korrespondierende Koordinatenobjekt "t_omi_xyz" 
  !!    interpolierten Daten <BR> 
  !! d) alle Daten (INTEGER) f&uuml;r ein Datenobjekt "t_omi_ind" bei 
  !!    Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !!    interpolierten Daten <BR> 
  INTERFACE get_omi_ind_pt_data
     MODULE PROCEDURE get_omi_ind_data_pt_d_1
     MODULE PROCEDURE get_omi_ind_data_xyz_pt_d_1
     MODULE PROCEDURE get_omi_ind_data_pt_i_1
     MODULE PROCEDURE get_omi_ind_data_xyz_pt_i_1
  END INTERFACE
  !
  !! ermittle die westliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar)
  INTERFACE get_omi_ind_west
     MODULE PROCEDURE get_omi_ind_west_0
  END INTERFACE
  !! ermittle die &ouml;stliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar)
  INTERFACE get_omi_ind_east
     MODULE PROCEDURE get_omi_ind_east_0
  END INTERFACE
  !! ermittle die n&ouml;rdliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar)
  INTERFACE get_omi_ind_north
     MODULE PROCEDURE get_omi_ind_north_0
  END INTERFACE
  !! ermittle die s&uuml;dliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar)
  INTERFACE get_omi_ind_south
     MODULE PROCEDURE get_omi_ind_south_0
  END INTERFACE
  !! ermittle die obere Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar)
  INTERFACE get_omi_ind_top
     MODULE PROCEDURE get_omi_ind_top_0
  END INTERFACE
  !! ermittle die unterste Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar)
  INTERFACE get_omi_ind_bottom
     MODULE PROCEDURE get_omi_ind_bottom_0
  END INTERFACE
  !
  !! extrahiere die zu dem Datenobjekt (oder eines Teils davon) geh&ouml;renden
  !! Daten, bezogen auf Objekte vom Typ XYLine bzw. XYZLine: <BR>
  !! a) alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" bei 
  !!    Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !!    interpolierten Daten <BR> 
  INTERFACE get_omi_ind_li_data
     MODULE PROCEDURE get_omi_ind_li_data_xyz_d_1
  END INTERFACE
  !! extrahiere die zu dem Datenobjekt (oder eines Teils davon) geh&ouml;renden
  !! Daten, bezogen auf Objekte vom Typ XYPolyLine bzw. XYZPolyLine: <BR>
  !! a) alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" bei 
  !!    Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !!    interpolierten Daten <BR> 
  INTERFACE get_omi_ind_pl_data
     MODULE PROCEDURE get_omi_ind_pl_data_xyz_d_1
  END INTERFACE
  !
  !! Pr&uuml;fe, ob ein Objekt die Komponente "stru_id" besitzt: <BR>
  !! a) f&uuml;r ein Objekt <BR>
  !! b) f&uuml;r ein Objekte 
  INTERFACE has_omi_ind_stru_id
     MODULE PROCEDURE has_omi_ind_stru_id_0
     MODULE PROCEDURE has_omi_ind_stru_id_1
  END INTERFACE
  !
  !! Kopiere den Inhalt einer Variablen des Typs "t_omi_ind" in
  !! ein anderes Objekt vom gleichen Typ <BR>
  !! a) ein Quell-Objekt wird in ein Ziel-Objekt kopiert <BR>
  !! a) mehrere Quell-Objekte werden auf mehrere Ziel-Objekte kopiert
  INTERFACE copy_omi_ind
     MODULE PROCEDURE copy_omi_ind_0_0
     MODULE PROCEDURE copy_omi_ind_1_1
  END INTERFACE
  !
  !! Erzeuge eine neues (3D-) Index-Listen-Objekt aus den Informationen in einem
  !! Koordinatenobjekt sowie einem schon vorhandenen (2D-) Indexlisten-Objekt
  INTERFACE create_omi_ind_pt
     MODULE PROCEDURE create_omi_ind_pt_d
  END INTERFACE
  !! Erzeuge eine neues (3D-) Index-Listen-Objekt aus den Informationen in einem
  !! Koordinatenobjekt sowie einem schon vorhandenen (2D-) Indexlisten-Objekt,
  !! wobei nur die Punkte in einer bestimmten Schicht Verwendung finden <BR>
  !! das Objekt wird nur dann erzeugt, insofern &uuml;berhaupt Positionen in der
  !! jeweiligen Schicht vorhanden sind
  INTERFACE create_omi_ind_lay_pt
     MODULE PROCEDURE create_omi_ind_lay_pt_d
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_ind" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_ind
     MODULE PROCEDURE eq_omi_ind_0_0  ! 
     MODULE PROCEDURE eq_omi_ind_0_1  ! 
     MODULE PROCEDURE eq_omi_ind_1_0  ! 
     MODULE PROCEDURE eq_omi_ind_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_ind" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_ind
     MODULE PROCEDURE ne_omi_ind_0_0  ! 
     MODULE PROCEDURE ne_omi_ind_0_1  ! 
     MODULE PROCEDURE ne_omi_ind_1_0  ! 
     MODULE PROCEDURE ne_omi_ind_1_1  ! 
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
  PUBLIC :: init_omi_ind
  PUBLIC :: clear_omi_ind
  PUBLIC :: setup_omi_ind_prn_lun
  PUBLIC :: setup_omi_ind_trc_lun
  PUBLIC :: new_omi_ind
  PUBLIC :: kill_omi_ind
  PUBLIC :: ok_omi_ind
  PUBLIC :: print_omi_ind
  PUBLIC :: print_omi_ind_static
  PUBLIC :: print_omi_ind_all_errors
  PUBLIC :: set_omi_ind_id
  PUBLIC :: set_omi_ind_description
  PUBLIC :: set_omi_ind_stru_id
  PUBLIC :: set_omi_ind_stru_start
  PUBLIC :: set_omi_ind_stru_len
  PUBLIC :: set_omi_ind_stru_list
  ! PUBLIC :: set_omi_ind_stru_list ! privat, nicht oeffentlich setzen
  PUBLIC :: get_omi_ind_id
  PUBLIC :: get_omi_ind_description
  PUBLIC :: get_omi_ind_stru_id_ref
  PUBLIC :: get_omi_ind_stru_start_ref
  PUBLIC :: get_omi_ind_stru_len_ref
  PUBLIC :: get_omi_ind_stru_list_ref
  PUBLIC :: get_omi_ind_point_list_ref
  PUBLIC :: eq_omi_ind
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: get_omi_ind_idx
  PUBLIC :: get_omi_ind_stru_count
  PUBLIC :: get_omi_ind_point_count
  PUBLIC :: get_omi_ind_stru_id_idx
  PUBLIC :: get_omi_ind_stru_id
  PUBLIC :: get_omi_ind_stru_len_count
  PUBLIC :: get_omi_ind_stru_len_max
  PUBLIC :: get_omi_ind_stru_len_min
  PUBLIC :: get_omi_ind_pt_data
  PUBLIC :: get_omi_ind_li_data
  PUBLIC :: get_omi_ind_pl_data
  PUBLIC :: get_omi_ind_west
  PUBLIC :: get_omi_ind_east
  PUBLIC :: get_omi_ind_north
  PUBLIC :: get_omi_ind_south
  PUBLIC :: get_omi_ind_top
  PUBLIC :: get_omi_ind_bottom
  PUBLIC :: has_omi_ind_stru_id 
  PUBLIC :: copy_omi_ind
  PUBLIC :: create_omi_ind_pt 
  PUBLIC :: create_omi_ind_lay_pt 
  PUBLIC :: ne_omi_ind
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
  CHARACTER (LEN=09), PARAMETER :: c_modname      = 'b_omi_ind' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_ind
  INTEGER           , PARAMETER :: c_nofcomp      =  7               ! ggf. modifizieren
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
  SUBROUTINE init_omi_ind_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='init_omi_ind_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_ind" version 2.3 of 03/01/07                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_omi_xyz ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_ind_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_ind_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_ind_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='clear_omi_ind_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_ind_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_xyz ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_ind_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_ind_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_ind_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_xyz_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_ind_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_ind_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_ind_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_xyz_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_ind_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_ind_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_ind_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%id          = REPEAT( ' ', LEN(this%id) )
       this%id          = c_undef_omi_ind_char 
       this%description = REPEAT( ' ', LEN(this%description) )
       this%description = c_undef_omi_ind_char
       NULLIFY ( this%stru_id    )
       NULLIFY ( this%stru_start )
       NULLIFY ( this%stru_len   )
       NULLIFY ( this%stru_list  )
       NULLIFY ( this%point_list )
    END IF
    !
  END SUBROUTINE new_omi_ind_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_ind_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_ind_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_ind_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_ind_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_ind_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_ind_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL dealloc_omi_ind_stru_id    ( this )
       IF ( no_error( ) ) CALL dealloc_omi_ind_stru_start ( this )
       IF ( no_error( ) ) CALL dealloc_omi_ind_stru_len   ( this )
       IF ( no_error( ) ) CALL dealloc_omi_ind_stru_list  ( this )
       IF ( no_error( ) ) CALL dealloc_omi_ind_point_list ( this )
       IF ( no_error( ) ) CALL new_omi_ind_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_ind_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_ind_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_ind_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_ind_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_ind_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_ind_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_omi_ind_id          ( this )
       l_ok(2) = ok_omi_ind_description ( this )
       l_ok(3) = ok_omi_ind_stru_id     ( this )
       l_ok(4) = ok_omi_ind_stru_start  ( this )
       l_ok(5) = ok_omi_ind_stru_len    ( this )
       l_ok(6) = ok_omi_ind_stru_list   ( this )
       l_ok(7) = ok_omi_ind_point_list  ( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_ind_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_ind_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) ) EXIT
          ok(i) = ok_omi_ind_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_ind_1
  !
  !! Pr&uuml;fe ob ein Datenobjekt g&uuml;ltig ist, auch im Vergleich 
  !! zu einem Koordinatenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_xyz_0 ( this, xyz ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Koordinatenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz  ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_omi_ind_xyz_0' 
    !! Hilfsvariablen
    LOGICAL            :: l_ok(2)    ! 
    CHARACTER (LEN=10) :: ch         ! 
    INTEGER            :: np, mn, mx ! 
    !
    ok = .false. ; l_ok(:) = .false. ; np = -1 ; mn = -1 ; mx = -1
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_omi_ind_0 ( this )
       IF ( l_ok(1) ) THEN
          np = get_omi_xyz_point_count ( xyz )
          mn = MINVAL( this%stru_list(:) )
          mx = MAXVAL( this%stru_list(:) )
          l_ok(2) = ( mn >= 1 .AND. mn <= np .AND. mx >= 1 .AND. mx <= np )
       END IF
       ok = ALL( l_ok(:) )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6500, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
          WRITE(ch,'(I10)') np ; CALL setup_error_act ( '<n-xyz>', ch )
          WRITE(ch,'(I10)') mn ; CALL setup_error_act ( '<minimum>', ch )
          WRITE(ch,'(I10)') mx ; CALL setup_error_act ( '<maximum>', ch )
       END IF
    END IF
    !
  END FUNCTION ok_omi_ind_xyz_0
  !
  !! Pr&uuml;fe ob mehrere Datenobjekte g&uuml;tig sind, auch im Vergleich 
  !! mit einem Koordinatenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_xyz_1 ( this, xyz ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this(:) ! 
    !! Koordinatenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz     ! 
    !! Testergebnisse (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_omi_ind_xyz_1' 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       ok(i) = ok_omi_ind_xyz_0 ( this(i), xyz )
    END DO
    !
  END FUNCTION ok_omi_ind_xyz_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_ind_0' 
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
       IF ( no_error( ) ) CALL print_omi_ind_id          ( this )
       IF ( no_error( ) ) CALL print_omi_ind_description ( this )
       IF ( no_error( ) ) CALL print_omi_ind_stru_id     ( this )
       IF ( no_error( ) ) CALL print_omi_ind_stru_start  ( this )
       IF ( no_error( ) ) CALL print_omi_ind_stru_len    ( this )
       IF ( no_error( ) ) CALL print_omi_ind_stru_list   ( this )
       IF ( no_error( ) ) CALL print_omi_ind_point_list  ( this )
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
8000 FORMAT('# Beginn Objekt t_omi_ind ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_ind ------------------------------')
    !
  END SUBROUTINE print_omi_ind_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_ind_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       i = 0
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
          IF ( no_error( ) ) CALL print_omi_ind_0 ( this(i) )
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
  END SUBROUTINE print_omi_ind_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_omi_ind_static_d' 
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
           c_undef_omi_ind_char, c_undef_omi_ind_int, c_undef_omi_ind_double
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_omi_ind_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_ind         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#   initialised = ',L1,/ &
    '#        prn_op = ',L1,/ &
    '#        trc_op = ',L1,/ &
    '#       prn_lun = ',I5,/ &
    '#       trc_lun = ',I5,/ &
    '#        n_init = ',I5,/ &
    '#   undef[char] = ',A,/ &
    '#    undef[int] = ',I10,/ &
    '# undef[double] = ',G15.6,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_omi_ind_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=26), PARAMETER :: c_upname='print_omi_ind_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_ind_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_id_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%id = REPEAT( ' ', LEN(this%id) )
    this%id = val(1:MIN(LEN(this%id),LEN_TRIM(val)))
    !
  END SUBROUTINE set_omi_ind_id_0_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_id_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_ind_id_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_ind_id_1_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_description_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%description = REPEAT( ' ', LEN(this%description) )
    this%description = val(1:MIN(LEN(this%description),LEN_TRIM(val)))
    !
  END SUBROUTINE set_omi_ind_description_0_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_description_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_ind_description_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_ind_description_1_0
  !
  !! weise der dynamischen Komponente "stru_id" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_stru_id_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)  , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "stru_id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='set_omi_ind_stru_id_0_1' 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_ind_stru_id ( this            )
       IF ( no_error( ) ) CALL alloc_omi_ind_stru_id   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_ind_stru_id    ( this            )
       IF ( no_error( ) ) THEN
          DO i=1,SIZE(val)
             this%stru_id(i) = val(i)(1:MIN(LEN(this%stru_id),LEN_TRIM(val(i))))
          END DO
       END IF
    END IF
    !
  END SUBROUTINE set_omi_ind_stru_id_0_1
  !
  !! weise der dynamischen Komponente "stru_id" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_stru_id_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)  , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "stru_id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='set_omi_ind_stru_id_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_ind_stru_id_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_ind_stru_id_1_1
  !
  !! weise der dynamischen Komponente "stru_start" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_stru_start_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "stru_start"
    INTEGER          , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='set_omi_ind_stru_start_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_ind_stru_start ( this            )
       IF ( no_error( ) ) CALL alloc_omi_ind_stru_start   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_ind_stru_start    ( this            )
       IF ( no_error( ) ) this%stru_start(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_ind_stru_start_0_1
  !
  !! weise der dynamischen Komponente "stru_start" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_stru_start_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "stru_start"
    INTEGER          , INTENT(IN)    :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='set_omi_ind_stru_start_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_ind_stru_start_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_ind_stru_start_1_1
  !
  !! weise der dynamischen Komponente "stru_len" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_stru_len_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "stru_len"
    INTEGER          , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='set_omi_ind_stru_len_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_ind_stru_len ( this            )
       IF ( no_error( ) ) CALL alloc_omi_ind_stru_len   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_ind_stru_len    ( this            )
       IF ( no_error( ) ) this%stru_len(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_ind_stru_len_0_1
  !
  !! weise der dynamischen Komponente "stru_len" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_stru_len_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "stru_len"
    INTEGER          , INTENT(IN)    :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='set_omi_ind_stru_len_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_ind_stru_len_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_ind_stru_len_1_1
  !
  !! weise der dynamischen Komponente "stru_list" ein Feld zu (Skalar)    <BR>
  !! des weiteren wird die Komponente "point_list(:)" automatisch erzeugt <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_stru_list_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "stru_list"
    INTEGER          , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='set_omi_ind_stru_list_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_ind_stru_list ( this            )
       IF ( no_error( ) ) CALL alloc_omi_ind_stru_list   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_ind_stru_list    ( this            )
       IF ( no_error( ) ) this%stru_list(:) = val(:)
       IF ( no_error( ) ) CALL create_point_list_0       ( this            )
    END IF
    !
  END SUBROUTINE set_omi_ind_stru_list_0_1
  !
  !! weise der dynamischen Komponente "stru_list" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_stru_list_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "stru_list"
    INTEGER          , INTENT(IN)    :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='set_omi_ind_stru_list_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_ind_stru_list_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_ind_stru_list_1_1
  !
  !! weise der dynamischen Komponente "point_list" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_point_list_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "point_list"
    INTEGER          , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='set_omi_ind_point_list_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_ind_point_list ( this            )
       IF ( no_error( ) ) CALL alloc_omi_ind_point_list   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_ind_point_list    ( this            )
       IF ( no_error( ) ) this%point_list(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_ind_point_list_0_1
  !
  !! weise der dynamischen Komponente "point_list" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ind_point_list_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "point_list"
    INTEGER          , INTENT(IN)    :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='set_omi_ind_point_list_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_ind_point_list_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_ind_point_list_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_id_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)   , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    CHARACTER (LEN=c_len_omi_ind_id) :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_omi_ind_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_id_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)   , INTENT(IN)  :: this(:)         ! 
    !! R&uuml;ckgabewert "id"
    CHARACTER (LEN=c_len_omi_ind_id) :: val(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = get_omi_ind_id_0_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ind_id_1_0
  !
  !! hole die Komponente "description" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_description_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)            , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "description" (Skalar)
    CHARACTER (LEN=c_len_omi_ind_description) :: val  ! 
    !
    val = this%description
    !
  END FUNCTION get_omi_ind_description_0_0
  !
  !! hole die Komponente "description" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_description_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)            , INTENT(IN)  :: this(:)         ! 
    !! R&uuml;ckgabewert "description"
    CHARACTER (LEN=c_len_omi_ind_description) :: val(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = get_omi_ind_description_0_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ind_description_1_0
  !
  !! hole die dynamische Feld-Komponente "stru_id" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_stru_id_ref_0_1 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)                      , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "stru_id" (Vektor)
    CHARACTER (LEN=c_len_omi_ind_stru_id) , POINTER    :: val(:) ! 
    !
    val => this%stru_id
    !
  END FUNCTION get_omi_ind_stru_id_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "stru_start" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_stru_start_ref_0_1 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "stru_start" (Vektor)
    INTEGER             , POINTER :: val(:) ! 
    !
    val => this%stru_start
    !
  END FUNCTION get_omi_ind_stru_start_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "stru_len" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_stru_len_ref_0_1 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "stru_len" (Vektor)
    INTEGER          , POINTER    :: val(:) ! 
    !
    val => this%stru_len
    !
  END FUNCTION get_omi_ind_stru_len_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "stru_list" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_stru_list_ref_0_1 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "stru_list" (Vektor)
    INTEGER          , POINTER    :: val(:) ! 
    !
    val => this%stru_list
    !
  END FUNCTION get_omi_ind_stru_list_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "point_list" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ind_point_list_ref_0_1 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "point_list" (Vektor)
    INTEGER          , POINTER    :: val(:) ! 
    !
    val => this%point_list
    !
  END FUNCTION get_omi_ind_point_list_ref_0_1
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_ind" f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)  , INTENT(IN) :: this(:) ! 
    !! zu suchender Wert der Komponente <EM>id</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), deren aktueller Wert
    !! der Komponente <EM>id</EM> identisch mit <EM>val</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird -1 zur&uuml;ckgegeben
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: i, l1, l2 ! 
    !
    res = -1
    l1  = LEN_TRIM(val)
    DO i=1,SIZE(this)
       IF ( res /= -1 ) EXIT
       l2 = LEN_TRIM(this(i)%id)
       IF ( l1 == l2 ) THEN
          IF ( val(1:l1) == this(i)%id(1:l2) ) res = i
       END IF
    END DO
    !
  END FUNCTION get_omi_ind_idx_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_ind" f&uuml;r viele Werte <EM>id(:)</EM> (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)  , INTENT(IN) :: this(:) ! 
    !! zu suchende Werte der Komponente <EM>id</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), deren aktueller 
    !! Wert der Komponente <EM>id</EM> identisch mit <EM>id(:)</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird -1 zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_omi_ind_idx_1_0 ( this(:), val(i) )
    END DO
    !
  END FUNCTION get_omi_ind_idx_1_1
  !
  !! ermittle die Anzahl der Strukturen in dem aktuellen Datenobjekt (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)  , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der Strukturen in dem aktuellen Datenobjekt
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%stru_start ) ) THEN
       res = SIZE(this%stru_start)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_ind_stru_count_0
  !
  !! ermittle die Anzahl der Strukturen in den aktuellen Datenobjekten (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)  , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der Strukturen in den aktuellen Datenobjekten
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       res(i) =  get_omi_ind_stru_count_0 ( this(i) ) 
    END DO
    !
  END FUNCTION get_omi_ind_stru_count_1
  !
  !! ermittle die Anzahl der Koordinatenpunkte in dem aktuellen Datenobjekt (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_point_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)  , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der Koordinatenpunkte in dem  Datenobjekt
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%point_list ) ) THEN
       res = SIZE(this%point_list)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_ind_point_count_0
  !
  !! ermittle die Anzahl der Koordinatenpunkte in den aktuellen Datenobjekten (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_point_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)  , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der Koordinatenpunkte in allen Datenobjekten
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       res(i) =  get_omi_ind_point_count_0 ( this(i) ) 
    END DO
    !
  END FUNCTION get_omi_ind_point_count_1
  !
  !! ermittle die Anzahl der Koordinatenpunkte in dem aktuellen Datenobjekt 
  !! f&uuml;r eine Teilstruktur mit Positionsnummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_point_count_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)  , INTENT(IN) :: this ! 
    !! Nummer der Teilstruktur
    INTEGER           , INTENT(IN) :: val  ! 
    !! Ergebnis: Anzahl der Koordinatenpunkte in der Teilstruktur
    INTEGER :: res ! 
    !
    res = 0
    IF ( ASSOCIATED( this%stru_len ) ) THEN
       IF ( val >= 1 .AND. val <= SIZE(this%stru_len) ) THEN
          res = this%stru_len(val)
       END IF
    END IF
    !
  END FUNCTION get_omi_ind_point_count_0_0
  !
  !! ermittle die Anzahl der Koordinatenpunkte in dem aktuellen Datenobjekt 
  !! f&uuml;r mehrere Teilstrukturen mit Positionsnummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_point_count_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)  , INTENT(IN) :: this   ! 
    !! Nummern der Teilstrukturen
    INTEGER           , INTENT(IN) :: val(:) ! 
    !! Ergebnis: Anzahl der Koordinatenpunkte in den Teilstrukturen
    INTEGER :: res(SIZE(val)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ind_point_count_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_ind_point_count_0_1
  !
  !! suche nach einem bestimmten Wert der Struktur <EM>stru_id</EM> in einem 
  !! Datenobjekt  des Typs "t_omi_ind" (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_id_idx_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind)  , INTENT(IN) :: this ! 
    !! Name der "stru_id", nach der gesucht werden gesucht werden soll
    CHARACTER (LEN=*) , INTENT(IN) :: val  ! 
    !! Ergebnis: Zeiger auf die Struktur mit dem Namen "stru_id" <BR>
    !! falls keine Struktur mit dem gesuchten Namen vorhanden ist,
    !! wird res=-1 zur&uuml;ckgegeben
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: i, l1, l2 ! 
    !
    res = -1
    IF ( ASSOCIATED(this%stru_id) ) THEN
       l1  = LEN_TRIM(val)
       DO i=1,SIZE(this%stru_id)
          IF ( res /= -1 ) EXIT
          l2 = LEN_TRIM(this%stru_id(i))
          IF ( l1 == l2 ) THEN
             IF ( val(1:l1) == this%stru_id(i)(1:l2) ) res = i
          END IF
       END DO
    END IF
    !
  END FUNCTION get_omi_ind_stru_id_idx_0_0
  !
  !! suche nach bestimmten Werten der Struktur <EM>stru_id</EM> in
  !! einem Datenobjekt des Typs "t_omi_ind" (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_id_idx_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind)  , INTENT(IN) :: this   ! 
    !! Namen der "stru_id", nach der gesucht werden gesucht werden soll
    CHARACTER (LEN=*) , INTENT(IN) :: val(:) ! 
    !! Ergebnis: Zeiger auf die Strukturen mit den Namen "stru_id" <BR>
    !! falls keine Struktur mit dem gesuchten Namen vorhanden ist,
    !! wird res=-1 zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_omi_ind_stru_id_idx_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_ind_stru_id_idx_0_1
  !
  !! gib den Namen f&uuml;r eine bestimmte Struktur an Position "idx" zur&uuml;ck <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_id_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Index-Position f&uuml;r die Struktur, nach der gesucht werden soll
    INTEGER          , INTENT(IN) :: val  ! 
    !! Name der Struktur oder "undefined", falls nicht vorhanden
    CHARACTER (LEN=c_len_omi_ind_stru_id) :: res ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_omi_ind_char
    IF ( ASSOCIATED( this%stru_id ) ) THEN
       IF ( val >= 1 .AND. val <= SIZE(this%stru_id) ) THEN
          res = this%stru_id(val)
       END IF
    END IF
    !
  END FUNCTION get_omi_ind_stru_id_0_0
  !
  !! gib den Namen f&uuml;r der Strukturen f&uuml;r verschiedene Positionn "idx" zur&uuml;ck <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_id_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this   ! 
    !! Index-Positionen f&uuml;r die Strukturen nach denen gesucht werden soll
    INTEGER          , INTENT(IN) :: val(:) ! 
    !! Name der Struktur oder "undefined", falls nicht vorhanden
    CHARACTER (LEN=c_len_omi_ind_stru_id) :: res(SIZE(val)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ind_stru_id_0_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_ind_stru_id_0_1
  !
  !! gib die Anzahl aller Eintr&auml;ge (Verweise auf Punkte) f&uuml;lr ein
  !! Datenobjekt zur&uuml;ck <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_len_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Anzahl der Eintr&auml;ge
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%stru_len ) ) THEN
       res = SUM( this%stru_len )
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_ind_stru_len_count_0
  !
  !! gib die Anzahl aller Eintr&auml;ge (Verweise auf Punkte) f&uuml;lr mehrere
  !! Datenobjekte zur&uuml;ck <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_len_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this(:) ! 
    !! Anzahl der Eintr&auml;ge
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ind_stru_len_count_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ind_stru_len_count_1
  !
  !! gib die Anzahl der Eintr&auml;ge (Verweise auf Punkte) f&uuml;lr eine
  !! bestimmte Struktur (Index) in einem Datenobjekt zur&uuml;ck <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_len_count_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Indexposition der Struktur
    INTEGER          , INTENT(IN) :: val  ! 
    !! Anzahl der Eintr&auml;ge
    INTEGER :: res ! 
    !
    res = 0
    IF ( ASSOCIATED( this%stru_len ) ) THEN
       IF ( val >= 1 .AND. val <= SIZE(this%stru_len) ) THEN
          res = this%stru_len(val)
       END IF
    END IF
    !
  END FUNCTION get_omi_ind_stru_len_count_0_0
  !
  !! gib die Anzahl der Eintr&auml;ge (Verweise auf Punkte) f&uuml;lr mehrere
  !! Strukturen (Indices) in einem Datenobjekt zur&uuml;ck <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_len_count_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this   ! 
    !! Indexpositionen der Strukturen
    INTEGER          , INTENT(IN) :: val(:) ! 
    !! Anzahl der Eintr&auml;ge
    INTEGER :: res(SIZE(val)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) =  get_omi_ind_stru_len_count_0_0 ( this, val(i) )
    END  DO
    !
  END FUNCTION get_omi_ind_stru_len_count_0_1
  !  
  !! ermittle die maximale L&auml;nge einer Struktur in dem Objekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_len_max_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! maximale Strukturl&auml;nge
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%stru_len ) ) THEN
       res = MAXVAL( this%stru_len(:) )
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_ind_stru_len_max_0
  !  
  !! ermittle die minimale L&auml;nge einer Struktur in dem Objekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_stru_len_min_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! maximale Strukturl&auml;nge
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%stru_len ) ) THEN
       res = MINVAL( this%stru_len(:) )
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_ind_stru_len_min_0
  !
  !! extrahiere alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" 
  !! bei Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !! interpolierten Daten; die Daten werden f&uuml;r Punkt-Objekte XYPoint bzw.
  !! XYZPoint extrahiert: <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_data_pt_d_1 ( this, val ) &
       RESULT(res)
    !! Datenobjekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_ind)   , INTENT(IN) :: this   ! 
    !! auf das korrespondierende Koordinaten-Objekt interpolierte Daten (Double)
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! extrahierte Ergebnisdaten [ Daten f&uuml;r die au&szlig;erhalb
    !! des Zeigerbereichs von "this%list(this%stru_start)" liegen werden auf
    !! undefiniert gesetzt ]
    REAL (KIND=Double) :: res(SIZE(this%stru_start)) ! 
    !! Hilfsvariablen
    INTEGER :: np ! 
    !
    np = SIZE(val)
    WHERE ( this%stru_list(this%stru_start) <= np )
       res(:) = val(this%stru_list(this%stru_start))
    ELSEWHERE
       res(:) = c_undef_omi_ind_double
    END WHERE
    !
  END FUNCTION get_omi_ind_data_pt_d_1
  !
  !! extrahiere alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" 
  !! bei Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !! interpolierten Daten; 
  !! interpolierten Daten; die Daten werden f&uuml;r Punkt-Objekte XYPoint bzw.
  !! XYZPoint extrahiert: <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_data_xyz_pt_d_1 ( this, xyz, val, missing ) &
       RESULT(res)
    !! Datenobjekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_ind)   , INTENT(IN) :: this   ! 
    !! Koordinaten-Objekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_xyz)   , INTENT(IN) :: xyz    ! 
    !! noch nicht interpolierte Daten (Double)
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! Wert zur Kennzeichnung fehlender/ung&uuml;ltiger Daten
    REAL (KIND=Double) , INTENT(IN) :: missing ! 
    !! extrahierte Ergebnisdaten [ Daten f&uuml;r die au&szlig;erhalb
    !! des Zeigerbereichs von "this%point_list" liegen werden auf
    !! undefiniert gesetzt ]
    REAL (KIND=Double) :: res(SIZE(this%stru_start)) ! 
    !! Hilfsvariablen
    INTEGER                          :: np     ! 
    REAL (KIND=Double) , ALLOCATABLE :: arr(:) ! 
    !
    np = get_omi_xyz_point_count( xyz )
    IF ( np > 0 ) THEN
       ALLOCATE( arr(np) )
       arr(:) = get_omi_xyz_data ( xyz, val(:), missing )
       res(:) = get_omi_ind_data_pt_d_1 ( this, arr(:) )
       DEALLOCATE( arr )
    ELSE
       res(:) = c_undef_omi_ind_double
    END IF
    !
  END FUNCTION get_omi_ind_data_xyz_pt_d_1
  !
  !! extrahiere alle Daten (INTEGER) f&uuml;r ein Datenobjekt "t_omi_ind" 
  !! bei Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !! interpolierten Daten; die Daten werden f&uuml;r Punkt-Objekte XYPoint bzw.
  !! XYZPoint extrahiert: <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_data_pt_i_1 ( this, val ) &
       RESULT(res)
    !! Datenobjekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_ind) , INTENT(IN) :: this   ! 
    !! auf das korrespondierende Koordinaten-Objekt interpolierte Daten (Integer)
    INTEGER          , INTENT(IN) :: val(:) ! 
    !! extrahierte Ergebnisdaten [ Daten f&uuml;r die au&szlig;erhalb
    !! des Zeigerbereichs von "this%list(this%stru_start)" liegen werden auf
    !! undefiniert gesetzt
    INTEGER :: res(SIZE(this%stru_start))      ! 
    !! Hilfsvariablen
    INTEGER :: np ! 
    !
    np = SIZE(val)
    WHERE ( this%stru_list(this%stru_start) <= np )
       res(:) = val(this%stru_list(this%stru_start))
    ELSEWHERE
       res(:) = c_undef_omi_ind_int
    END WHERE
    !
  END FUNCTION get_omi_ind_data_pt_i_1
  !
  !! extrahiere alle Daten (Integer) f&uuml;r ein Datenobjekt "t_omi_ind" 
  !! bei Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !! interpolierten Daten; die Daten werden f&uuml;r Punkt-Objekte XYPoint bzw.
  !! XYZPoint extrahiert: <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_data_xyz_pt_i_1 ( this, xyz, val, missing ) &
       RESULT(res)
    !! Datenobjekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_ind) , INTENT(IN) :: this    ! 
    !! Koordinaten-Objekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz     ! 
    !! noch nicht interpolierte Daten (Integer)
    INTEGER          , INTENT(IN) :: val(:)  ! 
    !! Wert zur Kennzeichnung fehlender/ung&uuml;ltiger Daten
    INTEGER          , INTENT(IN) :: missing ! 
    !! extrahierte Ergebnisdaten [ Daten f&uuml;r die au&szlig;erhalb
    !! des Zeigerbereichs von "this%list(this%stru_start)" liegen werden auf
    !! undefiniert gesetzt ]
    INTEGER :: res(SIZE(this%stru_start)) ! 
    !! Hilfsvariablen
    INTEGER               :: np     ! 
    INTEGER , ALLOCATABLE :: arr(:) ! 
    !
    np = get_omi_xyz_point_count( xyz )
    IF ( np > 0 ) THEN
       ALLOCATE( arr(np) )
       arr(:) = get_omi_xyz_data ( xyz, val(:), missing )
       res(:) = get_omi_ind_data_pt_i_1 ( this, arr(:) )
       DEALLOCATE( arr )
    ELSE
       res(:) = c_undef_omi_ind_int
    END IF
    !
  END FUNCTION get_omi_ind_data_xyz_pt_i_1
  !! extrahiere alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" 
  !! bei Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !! interpolierten Daten; die Daten werden auf Linien-Objekte XYLine bzw.
  !! XYZLine interpoliert und extrahiert: <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_li_data_xyz_d_1 ( this, xyz, val, missing ) &
       RESULT(res)
    !! Datenobjekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_ind)   , INTENT(IN) :: this              ! 
    !! Koordinaten-Objekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_xyz)   , INTENT(IN) :: xyz               ! 
    !! noch nicht interpolierte Daten (Double)
    REAL (KIND=Double) , INTENT(IN) :: val(:)            ! 
    !! Wert zur Kennzeichnung fehlender/ung&uuml;ltiger Daten
    REAL (KIND=Double) , INTENT(IN) :: missing           ! 
    !! extrahierte Ergebnisdaten [ f&uuml;r alle Elemente ]
    REAL (KIND=Double) :: res(SIZE(this%stru_start))     ! 
    !! Hilfsvariablen
    INTEGER                          :: i, j, jj, np, nn ! 
    REAL (KIND=Double) , ALLOCATABLE :: arr(:)           ! 
    !
    np = get_omi_xyz_point_count( xyz )
    IF ( np > 0 ) THEN
       ALLOCATE( arr(np) )
       arr(:) = get_omi_xyz_data ( xyz, val(:), missing )
       DO i=1,SIZE(this%stru_start)
          res(i) = 0.0_Double
          nn     = 0
          DO j=1,this%stru_len(i)
             jj = this%stru_list(this%stru_start(i)+j-1)
             IF ( arr(jj) /= missing ) THEN
                res(i) = res(i) + arr(jj)
                nn     = nn     + 1
             END IF
          END DO
          IF ( nn > 0 ) THEN
             res(i) = res(i)/REAL(nn,KIND=Double)
          ELSE
             res(i) = missing
          END IF
       END DO
       DEALLOCATE( arr )
    ELSE
       res(:) = c_undef_omi_ind_double
    END IF
    !
  END FUNCTION get_omi_ind_li_data_xyz_d_1
  !
  !! extrahiere alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" 
  !! bei Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !! interpolierten Daten; die Daten werden auf PolyLinien-Objekte XYPolyLine 
  !! bzw. XYZPolyLine interpoliert und extrahiert: <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_pl_data_xyz_d_1 ( this, xyz, val, missing ) &
       RESULT(res)
    !! Datenobjekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_ind)   , INTENT(IN) :: this   ! 
    !! Koordinaten-Objekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_xyz)   , INTENT(IN) :: xyz    ! 
    !! noch nicht interpolierte Daten (Double)
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! Wert zur Kennzeichnung fehlender/ung&uuml;ltiger Daten
    REAL (KIND=Double) , INTENT(IN) :: missing ! 
    !! extrahierte Ergebnisdaten [ f&uuml;r alle Elemente ]
    REAL (KIND=Double) :: res(SIZE(this%stru_start)) ! 
    !! Hilfsvariablen
    INTEGER                          :: i, j, j1, j2, np       ! 
    REAL (KIND=Double)               :: dd, ww                 ! 
    REAL (KIND=Double) , POINTER     :: p_x(:), p_y(:), p_z(:) ! 
    REAL (KIND=Double) , ALLOCATABLE :: arr(:)                 ! 
    !
    np = get_omi_xyz_point_count( xyz )
    IF ( np > 0 ) THEN
       ALLOCATE( arr(np) )
       arr(:) = get_omi_xyz_data ( xyz, val(:), missing )
       p_x    => get_omi_xyz_x_ref ( xyz )
       p_y    => get_omi_xyz_y_ref ( xyz )
       p_z    => get_omi_xyz_z_ref ( xyz )
       IF ( ASSOCIATED( p_x ) .AND. ASSOCIATED( p_y ) ) THEN
          DO i=1,SIZE(this%stru_start)
             res(i) = 0.0_Double
             ww     = 0.0_Double
             DO j=1,this%stru_len(i)-1
                j1 = this%stru_list(this%stru_start(i)+j-1)
                j2 = this%stru_list(this%stru_start(i)+j  )
                dd = ( p_x(j1)-p_x(j2) )*( p_x(j1)-p_x(j2) )
                dd = ( p_y(j1)-p_y(j2) )*( p_y(j1)-p_y(j2) ) + dd
                IF ( ASSOCIATED( p_z ) ) dd = ( p_z(j1)-p_z(j2) )*( p_z(j1)-p_z(j2) ) + dd
                dd = SQRT( dd )
                IF ( arr(j1) /= missing ) THEN
                   res(i) = res(i) + arr(j1)*dd
                   ww     = ww     + dd
                END IF
                IF ( arr(j2) /= missing ) THEN
                   res(i) = res(i) + arr(j2)*dd
                   ww     = ww     + dd
                END IF
             END DO
             IF ( ww > 0.0_Double ) THEN
                res(i) = res(i)/ww
             ELSE
                res(i) = missing
             END IF
          END DO
       END IF
       NULLIFY   ( p_x, p_y, p_z )
       DEALLOCATE( arr           )
    ELSE
       res(:) = c_undef_omi_ind_double
    END IF
    !
  END FUNCTION get_omi_ind_pl_data_xyz_d_1
  !
  !! Ermittle die Koordinate f&uuml;r den westlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_west_0 ( this, xyz ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! zugeordnetes Koordinaten-Objekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz  ! 
    !! Ergebnis: Koordinate f&uuml;r westlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%point_list) ) THEN
       res = get_omi_xyz_west ( xyz, this%point_list )
    ELSE
       res = get_omi_xyz_west ( xyz )
    END IF
    !
  END FUNCTION get_omi_ind_west_0
  !
  !! Ermittle die Koordinate f&uuml;r den &ouml;stlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_east_0 ( this, xyz ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! zugeordnetes Koordinaten-Objekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz  ! 
    !! Ergebnis: Koordinate f&uuml;r &ouml;stlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%point_list) ) THEN
       res = get_omi_xyz_east ( xyz, this%point_list )
    ELSE
       res = get_omi_xyz_east ( xyz )
    END IF
    !
  END FUNCTION get_omi_ind_east_0
  !
  !! Ermittle die Koordinate f&uuml;r den n&ouml;rdlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_north_0 ( this, xyz ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! zugeordnetes Koordinaten-Objekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz  ! 
    !! Ergebnis: Koordinate f&uuml;r n&ouml;rdlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%point_list) ) THEN
       res = get_omi_xyz_north ( xyz, this%point_list )
    ELSE
       res = get_omi_xyz_north ( xyz )
    END IF
    !
  END FUNCTION get_omi_ind_north_0
  !
  !! Ermittle die Koordinate f&uuml;r den s&uuml;dlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_south_0 ( this, xyz ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! zugeordnetes Koordinaten-Objekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz  ! 
    !! Ergebnis: Koordinate f&uuml;r s&uuml;dlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%point_list) ) THEN
       res = get_omi_xyz_south ( xyz, this%point_list )
    ELSE
       res = get_omi_xyz_south ( xyz )
    END IF
    !
  END FUNCTION get_omi_ind_south_0
  !
  !! Ermittle die Koordinate f&uuml;r den oberen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_top_0 ( this, xyz ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! zugeordnetes Koordinaten-Objekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz  ! 
    !! Ergebnis: Koordinate f&uuml;r oberen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%point_list) ) THEN
       res = get_omi_xyz_top ( xyz, this%point_list )
    ELSE
       res = get_omi_xyz_top ( xyz )
    END IF
    !
  END FUNCTION get_omi_ind_top_0
  !
  !! Ermittle die Koordinate f&uuml;r den unteren Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ind_bottom_0 ( this, xyz ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! zugeordnetes Koordinaten-Objekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz  ! 
    !! Ergebnis: Koordinate f&uuml;r unteren Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%point_list) ) THEN
       res = get_omi_xyz_bottom ( xyz, this%point_list )
    ELSE
       res = get_omi_xyz_bottom ( xyz )
    END IF
    !
  END FUNCTION get_omi_ind_bottom_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-HAS-Methoden
  ! ----------------------------------------------------------------------
  !
  !! Ermittle, ob eine Objekt eine (allokierte) Komponente "stru_id" aufweist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_ind_stru_id_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Test-Ergebnis
    LOGICAL :: res 
    !
    res = ASSOCIATED( this%stru_id )
    !
  END FUNCTION has_omi_ind_stru_id_0
  !
  !! Ermittle, ob viele Objekte eine (allokierte) Komponente "stru_id" aufweisen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_ind_stru_id_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this(:) ! 
    !! Test-Ergebnisse
    LOGICAL :: res(SIZE(this))
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_ind_stru_id_0 ( this(i) ) 
    END DO
    !
  END FUNCTION has_omi_ind_stru_id_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = ( this1%id          == this2%id          )
    l_ok(2)  = ( this1%description == this2%description )
    l_ok(3)  = eq_omi_ind_stru_id    ( this1, this2 )
    l_ok(4)  = eq_omi_ind_stru_start ( this1, this2 )
    l_ok(5)  = eq_omi_ind_stru_len   ( this1, this2 )
    l_ok(6)  = eq_omi_ind_stru_list  ( this1, this2 )
    l_ok(7)  = eq_omi_ind_point_list ( this1, this2 )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_ind_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_ind_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_ind_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_ind_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_ind_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_ind_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_ind_1_1
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
  FUNCTION ne_omi_ind_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ne_omi_ind_0_0' 
    !
    ok = .NOT. eq_omi_ind_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_ind_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_ind_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_ind_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_ind_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_ind_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_ind_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_ind_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_ind_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_ind) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l       = SIZE( ok )
    ok(1:l) = .NOT. eq_omi_ind_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_ind_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-Copy-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! kopiere den Inhalt einer Komponente in eine andere Komponente <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_ind_0_0 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_ind) , INTENT(OUT) :: this1 ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN)  :: this2 ! 
    !
    CALL new_omi_ind_0               ( this1 )
    CALL set_omi_ind_id_0_0          ( this1, this2%id )
    CALL set_omi_ind_description_0_0 ( this1, this2%description )
    IF ( ASSOCIATED( this2%stru_id    ) ) CALL set_omi_ind_stru_id    ( this1, this2%stru_id    )
    IF ( ASSOCIATED( this2%stru_start ) ) CALL set_omi_ind_stru_start ( this1, this2%stru_start )
    IF ( ASSOCIATED( this2%stru_len   ) ) CALL set_omi_ind_stru_len   ( this1, this2%stru_len   )
    IF ( ASSOCIATED( this2%stru_list  ) ) CALL set_omi_ind_stru_list  ( this1, this2%stru_list  ) 
    IF ( ASSOCIATED( this2%point_list ) ) CALL set_omi_ind_point_list ( this1, this2%point_list ) 
    !
  END SUBROUTINE copy_omi_ind_0_0
  !
  !! kopiere den Inhalt mehrere Komponente auf andere Komponenten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_ind_1_1 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_ind) , INTENT(OUT) :: this1(:) ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN)  :: this2(:) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       CALL copy_omi_ind_0_0 ( this1(i), this2(i) )
    END DO
    !
  END SUBROUTINE copy_omi_ind_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-Create-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge eine neues Index-Listen-Objekt aus einem vorhandenen Koordinaten-
  !! objekt und einem Index-Listen-Objekt <BR>
  !! das zu erzeugende Index-Listen-Objekt soll ausschliesslich Punkte enthalten <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE create_omi_ind_pt_d ( this, xyz, ind, id )
    !! zu erzeugendes Index-Listen-Objekt
    TYPE (t_omi_ind) , INTENT(OUT) :: this ! 
    !! vorhandenes Koordinaten-Objekt [3D]
    TYPE (t_omi_xyz) , INTENT(IN)  :: xyz  ! 
    !! vorhandenes Index-Listen-Objekt [2D]
    TYPE (t_omi_ind) , INTENT(IN)  :: ind  ! 
    !! Id-Bezeichnung f&uuml;r das zu erzeugende Index-Listen-Objekt [3D]
    CHARACTER (LEN=*) , INTENT(IN) :: id   ! 
    !! Hilfsvariablen
    INTEGER               :: i                              ! 
    INTEGER , ALLOCATABLE :: l_stru_start(:), l_stru_len(:) ! 
    INTEGER , POINTER     :: p_stru_list(:)                 ! 
    !
    CALL new_omi_ind ( this )
    NULLIFY( p_stru_list )
    IF ( ASSOCIATED( ind%stru_list ) ) THEN
       p_stru_list => get_omi_xyz_column_point_idx ( xyz, ind%stru_list )
       IF ( ASSOCIATED( p_stru_list ) ) THEN
          ALLOCATE( l_stru_start(SIZE(p_stru_list)), l_stru_len(SIZE(p_stru_list)) )
          DO i=1,SIZE(l_stru_start)
             l_stru_start(i) = i
             l_stru_len(i)   = 1
          END DO
          CALL set_omi_ind_id          ( this, id )
          CALL set_omi_ind_description ( this, TRIM(ind%description)//', 3D' )
          CALL set_omi_ind_stru_start  ( this, l_stru_start )
          CALL set_omi_ind_stru_len    ( this, l_stru_len )
          CALL set_omi_ind_stru_list   ( this, p_stru_list )
       END IF
    END IF
    IF ( ALLOCATED( l_stru_start ) ) DEALLOCATE( l_stru_start )
    IF ( ALLOCATED( l_stru_len   ) ) DEALLOCATE( l_stru_len   )
    IF ( ASSOCIATED( p_stru_list ) ) DEALLOCATE( p_stru_list  )
    !
  END SUBROUTINE create_omi_ind_pt_d
  !
  !! Erzeuge eine neues (3D-) Index-Listen-Objekt aus den Informationen in einem
  !! Koordinatenobjekt sowie einem schon vorhandenen (2D-) Indexlisten-Objekt,
  !! wobei nur die Punkte in einer bestimmten Schicht Verwendung finden <BR>
  !! das Objekt wird nur dann erzeugt, insofern &uuml;berhaupt Positionen in der
  !! jeweiligen Schicht vorhanden sind <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE create_omi_ind_lay_pt_d ( this, xyz, ind, id, n, ex )
    !! zu erzeugendes Index-Listen-Objekt
    TYPE (t_omi_ind) , INTENT(OUT) :: this ! 
    !! vorhandenes Koordinaten-Objekt [3D]
    TYPE (t_omi_xyz) , INTENT(IN)  :: xyz  ! 
    !! vorhandenes Index-Listen-Objekt [2D]
    TYPE (t_omi_ind) , INTENT(IN)  :: ind  ! 
    !! Id-Bezeichnung f&uuml;r das zu erzeugende Index-Listen-Objekt [3D]
    CHARACTER (LEN=*) , INTENT(IN) :: id   ! 
    !! Nummer der aktuell zu erzeugenden Schicht
    INTEGER           , INTENT(IN) :: n    ! 
    !! Indikatorvariable, ob das Objekt erzeugt wurde
    LOGICAL          , INTENT(OUT) :: ex   ! 
    !! Hilfsvariablen
    LOGICAL , ALLOCATABLE :: l_mask(:)                      ! 
    INTEGER               :: i, nn, np                      ! 
    INTEGER , ALLOCATABLE :: l_stru_start(:), l_stru_len(:) ! 
    INTEGER , POINTER     :: p_stru_list(:)                 ! 
    CHARACTER (LEN=c_len_omi_ind_stru_id) , ALLOCATABLE :: l_stru_id(:) ! 
    !
    CALL new_omi_ind ( this )
    NULLIFY( p_stru_list )
    IF ( ASSOCIATED( ind%stru_list ) ) THEN
       p_stru_list => get_omi_xyz_layer_point_idx ( xyz, n, ind%stru_list )
       IF ( ASSOCIATED(p_stru_list) ) THEN
          ex = .true.
          ALLOCATE( l_stru_start(SIZE(p_stru_list)), l_stru_len(SIZE(p_stru_list)) )
          DO i=1,SIZE(l_stru_start)
             l_stru_start(i) = i
             l_stru_len(i)   = 1
          END DO
          CALL set_omi_ind_id          ( this, id )
          CALL set_omi_ind_description ( this, TRIM(ind%description)//', '//TRIM(get_omi_xyz_layer_name(xyz,n)) )
          CALL set_omi_ind_stru_start  ( this, l_stru_start )
          CALL set_omi_ind_stru_len    ( this, l_stru_len )
          CALL set_omi_ind_stru_list   ( this, p_stru_list )
          ! optional wird auch eine Selektion der stru_ids transferiert
          IF ( ASSOCIATED(ind%stru_id) ) THEN
             IF ( SIZE(ind%stru_id) == SIZE(ind%stru_list) ) THEN
                ALLOCATE ( l_mask(SIZE(ind%stru_list)) )
                l_mask = get_omi_xyz_layer_2d_mask ( xyz, n, ind%stru_list )
                IF ( COUNT( l_mask ) == SIZE(p_stru_list) ) THEN
                   ALLOCATE ( l_stru_id(SIZE(p_stru_list)) )
                   nn = 0
                   DO i=1,SIZE(l_mask)
                      IF ( .NOT. l_mask(i) ) CYCLE
                      nn            = nn + 1
                      l_stru_id(nn) = ind%stru_id(i)
                   END DO
                   CALL set_omi_ind_stru_id ( this, l_stru_id )
                END IF
             END IF
          END IF
       ELSE
          ex = .false.
       END IF
    END IF
    IF ( ALLOCATED( l_mask       ) ) DEALLOCATE( l_mask       )
    IF ( ALLOCATED( l_stru_start ) ) DEALLOCATE( l_stru_start )
    IF ( ALLOCATED( l_stru_len   ) ) DEALLOCATE( l_stru_len   )
    IF ( ALLOCATED( l_stru_id    ) ) DEALLOCATE( l_stru_id    )
    IF ( ASSOCIATED( p_stru_list ) ) DEALLOCATE( p_stru_list  )
    !
  END SUBROUTINE create_omi_ind_lay_pt_d
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_ind" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_ind ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
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
  SUBROUTINE init_omi_ind_all_errors &
       ( )
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
               '--> INIT_omi_ind ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_ind ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_id"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_start"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_len"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_list"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "point_list"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "id"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "description"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_id"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'assoziiert  : id=<a-stru_id>, start=<a-stru_start>\n'//&
               'undefiniert : id=<u-stru_id>, start=<u-stru_start>\n'//&
               'Groesse     : id=<s-stru_id>, start=<s-stru_start>\n'//&
               'leer        : id=<l-stru_id>\n'//&
               'mehrfach    : id=<m-stru_id>\n'//&
               '--> SET_OMI_IND_[STRU_START|STRU_ID] konsistent verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_start"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'assoziiert  : <a-stru_start>\n'//&
               'undefiniert : <u-stru_start>\n'//&
               'Groesse     : <s-stru_start>\n'//&
               '<=0         : <l-stru_start>\n'//&
               'mehrfach    : <m-stru_start>\n'//&
               '--> SET_OMI_IND_STRU_START korrekt verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_len"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'assoziiert  : len=<a-stru_len>, start=<a-stru_start>\n'//&
               'undefiniert : len=<u-stru_len>, start=<u-stru_start>\n'//&
               'Groesse     : len=<s-stru_len>, start=<s-stru_start>\n'//&
               '<=0         : len=<l-stru_len>, start=<l-stru_start>\n'//&
               '-->  SET_OMI_IND_STRU_LEN korrekt verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_list"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'assoziiert  : len=<a-stru_len>, start=<a-stru_start>, list=<a-stru_list>\n'//&
               'undefiniert : len=<u-stru_len>, start=<u-stru_start>, list=<u-stru_list>\n'//&
               'Groesse     : len=<s-stru_len>, start=<s-stru_start>, list=<s-stru_list>\n'//&
               '<=0         : len=<l-stru_len>, start=<l-stru_start>, list=<l-stru_list>\n'//&
               'GroesseLen  : len=<s-ss> [ aus LEN fuer LIST abgeleitete Groesse ]\n'//&
               'ausserhalb  : nof=<i-in> [ aus LEN und START abgeleitete Adressen ]\n'//&
               '--> SET_OMI_IND_STRU_LIST korrekt verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "point_list"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'assoziiert  : list=<a-stru_list>, point=<a-point_list>\n'//&
               'undefiniert : list=<u-stru_list>, point=<u-point_list>\n'//&
               'Minimum     : list=<n-stru_list>, point=<n-point_list>\n'//&
               'Maximum     : list=<x-stru_list>, point=<x-point_list>\n'//&
               '<=0         : list=<l-stru_list>, point=<l-point_list>\n'//&
               '--> SET_OMI_IND_POINT_LIST korrekt verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ind"\n'//&
               'Inhalt der Komponente "stru_list" inkompatibel mit Objekt "t_omi_xyz"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'Punkte in "t_omi_xyz"     = <n-xyz>\n'//&
               'Minimum in "stru_list(:)" = <minimum>\n'//&
               'Maximum in "stru_list(:)" = <maximum>\n'//&
               '--> Datenobjekt korrekt definieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ind"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_omi_ind" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ind"\n'//&
               'Typ-Komponente = "description"\n'//&
               '--> Code in Modul "b_omi_ind" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_id(:)"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_ind" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_start"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_ind" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_len"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_ind" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_list"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_ind" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ind"\n'//&
               'Typ-Komponente = "point_list"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_ind" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_ind"\n'//&
               '--> Code in Modul "b_omi_ind" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_id"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'Dimension      = <idim>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_start"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'Dimension      = <idim>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_len"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'Dimension      = <idim>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "stru_list"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'Dimension      = <idim>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_ind"\n'//&
               'Typ-Komponente = "point_list"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'Dimension      = <idim>\n'//&
               '--> Code in Modul "b_omi_ind" pruefen' )
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
  END SUBROUTINE init_omi_ind_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_ind_all_errors &
       ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_ind_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "stru_id" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_ind_stru_id &
       ( this, &
         idim )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Dimension der dynamischen Feld-Komponente "stru_id"
    INTEGER          , INTENT(IN)    :: idim ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='alloc_omi_ind_stru_id' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%stru_id(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8030, c_upname, c_modname, stat )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_ind_stru_id
  !
  !! Allokieren der dynamischen Feld-Komponente "stru_start" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_ind_stru_start &
       ( this, &
         idim )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Dimension der dynamischen Feld-Komponente "stru_start"
    INTEGER          , INTENT(IN)    :: idim ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER  :: c_upname='alloc_omi_ind_stru_start' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%stru_start(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8040, c_upname, c_modname, stat )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_ind_stru_start
  !
  !! Allokieren der dynamischen Feld-Komponente "stru_len" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_ind_stru_len &
       ( this, &
         idim )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Dimension der dynamischen Feld-Komponente "stru_len"
    INTEGER          , INTENT(IN)    :: idim !  
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='alloc_omi_ind_stru_len' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%stru_len(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8050, c_upname, c_modname, stat )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_ind_stru_len
  !
  !! Allokieren der dynamischen Feld-Komponente "stru_list" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_ind_stru_list &
       ( this, &
         idim )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Dimension der dynamischen Feld-Komponente "stru_list"
    INTEGER          , INTENT(IN)    :: idim ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER  :: c_upname='alloc_omi_ind_stru_list' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%stru_list(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8060, c_upname, c_modname, stat )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_ind_stru_list
  !
  !! Allokieren der dynamischen Feld-Komponente "point_list" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_ind_point_list &
       ( this, &
         idim )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Dimension der dynamischen Feld-Komponente "point_list"
    INTEGER          , INTENT(IN)    :: idim ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER  :: c_upname='alloc_omi_ind_point_list' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%point_list(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8070, c_upname, c_modname, stat )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_ind_point_list
  !
  !! Initialisieren der Feld-Komponente "stru_id" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_ind_stru_id &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !
    this%stru_id(:) = REPEAT( ' ', LEN(this%stru_id) )
    this%stru_id(:) = c_undef_omi_ind_char
    !
  END SUBROUTINE init_omi_ind_stru_id
  !
  !! Initialisieren der Feld-Komponente "stru_start" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_ind_stru_start &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !
    this%stru_start(:) = c_undef_omi_ind_int
    !
  END SUBROUTINE init_omi_ind_stru_start
  !
  !! Initialisieren der Feld-Komponente "stru_len" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_ind_stru_len &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !
    this%stru_len(:) = c_undef_omi_ind_int
    !
  END SUBROUTINE init_omi_ind_stru_len
  !
  !! Initialisieren der Feld-Komponente "stru_list" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_ind_stru_list &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !
    this%stru_list(:) = c_undef_omi_ind_int
    !
  END SUBROUTINE init_omi_ind_stru_list
  !
  !! Initialisieren der Feld-Komponente "point_list" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_ind_point_list &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this   ! 
    !
    this%point_list(:) = c_undef_omi_ind_int
    !
  END SUBROUTINE init_omi_ind_point_list
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "stru_id" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_ind_stru_id &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER  :: c_upname='dealloc_omi_ind_stru_id' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%stru_id ) ) THEN
       DEALLOCATE ( this%stru_id, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5030, c_upname, c_modname, stat )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
       NULLIFY ( this%stru_id ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_ind_stru_id
  !
  !! De-Allokieren der dynamischen Feld-Komponente "stru_start" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_ind_stru_start &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='dealloc_omi_ind_stru_start' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%stru_start ) ) THEN
       DEALLOCATE ( this%stru_start, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5040, c_upname, c_modname, stat )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
       NULLIFY ( this%stru_start ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_ind_stru_start
  !
  !! De-Allokieren der dynamischen Feld-Komponente "stru_len" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_ind_stru_len &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER  :: c_upname='dealloc_omi_ind_stru_len' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%stru_len ) ) THEN
       DEALLOCATE ( this%stru_len, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5050, c_upname, c_modname, stat )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
       NULLIFY ( this%stru_len ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_ind_stru_len
  !
  !! De-Allokieren der dynamischen Feld-Komponente "stru_list" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_ind_stru_list &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER  :: c_upname='dealloc_omi_ind_stru_list' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%stru_list ) ) THEN
       DEALLOCATE ( this%stru_list, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
       NULLIFY ( this%stru_list ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_ind_stru_list
  !
  !! De-Allokieren der dynamischen Feld-Komponente "point_list" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_ind_point_list &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='dealloc_omi_ind_point_list' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%point_list ) ) THEN
       DEALLOCATE ( this%point_list, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5070, c_upname, c_modname, stat )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
       NULLIFY ( this%point_list ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_ind_point_list
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_omi_ind_id' ! 
    !
    ok = ( LEN_TRIM(this%id) > 0 .AND. &
           this%id(1:LEN(c_undef_omi_ind_char)) /= c_undef_omi_ind_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       CALL setup_error_act ( '<aktuell>', TRIM(this%id) )
    END IF
    !
  END FUNCTION ok_omi_ind_id
  !
  !! Pr&uuml;fe, ob die Komponente "description" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_description &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_ind_description' ! 
    !
    ok = ( LEN_TRIM(this%description) > 0 .AND. &
           this%description(1:LEN(c_undef_omi_ind_char)) /= c_undef_omi_ind_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       CALL setup_error_act ( '<aktuell>', TRIM(this%description) )
    END IF
    !
  END FUNCTION ok_omi_ind_description
  !
  !! Pr&uuml;fe, ob die Komponente "stru_id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_stru_id &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_omi_ind_stru_id' ! 
    INTEGER            , PARAMETER :: c_max=2 ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                           ! 
    LOGICAL            :: as(c_max)                    ! 
    INTEGER            :: un(c_max), sz(c_max), ll, mm ! 
    INTEGER            :: i ! 
    !
    as(1) = ASSOCIATED(this%stru_start)
    as(2) = ASSOCIATED(this%stru_id   )
    un(:) = -1
    sz(:) = -1
    ll    = -1
    mm    = -1
    IF ( as(1) ) THEN
       un(1) = COUNT( this%stru_start(:) == c_undef_omi_ind_int )
       sz(1) = SIZE ( this%stru_start(:) )
    END IF
    IF ( as(2) ) THEN
       un(2) = COUNT( this%stru_id(:)(1:LEN(c_undef_omi_ind_char)) == c_undef_omi_ind_char )
       sz(2) = SIZE ( this%stru_id(:) )
       ll    = COUNT( LEN_TRIM(this%stru_id(:)) <= 0 )
       DO i=1,sz(2)
          mm = COUNT( this%stru_id(:) == this%stru_id(i) )
          IF ( mm/=1 ) EXIT
       END DO
    END IF
    !
    IF ( .NOT. as(2) ) THEN
       ok = .true.
    ELSE
       ok = ( ALL(as) .AND. ALL(un==0) .AND. ALL(sz==sz(1)) .AND. ll==0 .AND. mm==1 )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
          WRITE(ch(1:1),'(L1)') as(1) ; CALL setup_error_act ( '<a-stru_start>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') as(2) ; CALL setup_error_act ( '<a-stru_id>', ch(1:1) )
          WRITE(ch,'(I10)'    ) un(1) ; CALL setup_error_act ( '<u-stru_start>', ch )
          WRITE(ch,'(I10)'    ) un(2) ; CALL setup_error_act ( '<u-stru_id>', ch )
          WRITE(ch,'(I10)'    ) sz(1) ; CALL setup_error_act ( '<s-stru_start>', ch )
          WRITE(ch,'(I10)'    ) sz(2) ; CALL setup_error_act ( '<s-stru_id>', ch )
          WRITE(ch,'(I10)'    ) ll    ; CALL setup_error_act ( '<l-stru_id>', ch )
          WRITE(ch,'(I10)'    ) mm    ; CALL setup_error_act ( '<m-stru_id>', ch )
       END IF
    END IF
    !
  END FUNCTION ok_omi_ind_stru_id
  !
  !! Pr&uuml;fe, ob die Komponente "stru_start" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_stru_start &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN)  :: this ! 
    !! Testergebnis
    LOGICAL                        :: ok   ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='ok_omi_ind_stru_start' ! 
    INTEGER            , PARAMETER :: c_max=1 ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                                         ! 
    LOGICAL            :: as(c_max)                                  ! 
    INTEGER            :: un(c_max), sz(c_max), ll(c_max), mm(c_max) ! 
    INTEGER            :: i ! 
    INTEGER , ALLOCATABLE :: l_stru_list(:) ! 
    !
    as(1) = ASSOCIATED(this%stru_start)
    un(:) = -1
    sz(:) = -1
    ll(:) = -1
    mm(:) = -1
    IF ( as(1) ) THEN
       ALLOCATE (l_stru_list(SIZE(this%stru_list)))
       l_stru_list(:) = 0
       un(1)          = COUNT( this%stru_start(:) == c_undef_omi_ind_int )
       sz(1)          = SIZE ( this%stru_start(:)   )
       ll(1)          = COUNT( this%stru_start(:) <= 0 )
       DO i=1,sz(1)
          l_stru_list(this%stru_start(i)) = l_stru_list(this%stru_start(i)) + 1
       END DO
       mm(1) = MAXVAL(l_stru_list)
       DEALLOCATE(l_stru_list)
    END IF
    ok = ( ALL(as) .AND. ALL(un==0) .AND. ALL(sz==sz(1)) .AND. ALL(ll==0) .AND. ALL(mm==1) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') as(1) ; CALL setup_error_act ( '<a-stru_start>', ch(1:1) )
       WRITE(ch,'(I10)'    ) un(1) ; CALL setup_error_act ( '<u-stru_start>', ch )
       WRITE(ch,'(I10)'    ) sz(1) ; CALL setup_error_act ( '<s-stru_start>', ch )
       WRITE(ch,'(I10)'    ) ll(1) ; CALL setup_error_act ( '<l-stru_start>', ch )
       WRITE(ch,'(I10)'    ) mm(1) ; CALL setup_error_act ( '<m-stru_start>', ch )
    END IF
    !
  END FUNCTION ok_omi_ind_stru_start
  !
  !! Pr&uuml;fe, ob die Komponente "stru_len" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_stru_len &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_omi_ind_stru_len' ! 
    INTEGER            , PARAMETER :: c_max=2 ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                              ! 
    LOGICAL            :: as(c_max)                       ! 
    INTEGER            :: un(c_max), sz(c_max), ll(c_max) ! 
    !
    as(1) = ASSOCIATED(this%stru_start)
    as(2) = ASSOCIATED(this%stru_len)
    un(:) = -1
    sz(:) = -1
    ll(:) = -1
    IF ( as(1) ) THEN
       un(1) = COUNT( this%stru_start(:) == c_undef_omi_ind_int )
       sz(1) = SIZE ( this%stru_start(:) )
       ll(1) = COUNT( this%stru_start(:) <= 0 )
    END IF
    IF ( as(2) ) THEN
       un(2) = COUNT( this%stru_len(:) == c_undef_omi_ind_int )
       sz(2) = SIZE ( this%stru_len(:) )
       ll(2) = COUNT( this%stru_len(:) <= 0 )
    END IF
    ok = ( ALL(as) .AND. ALL(un==0) .AND. ALL(sz==sz(1)) .AND. ALL(ll==0) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') as(1) ; CALL setup_error_act ( '<a-stru_start>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') as(2) ; CALL setup_error_act ( '<a-stru_len>', ch(1:1) )
       WRITE(ch,'(I10)'    ) un(1) ; CALL setup_error_act ( '<u-stru_start>', ch )
       WRITE(ch,'(I10)'    ) un(2) ; CALL setup_error_act ( '<u-stru_len>', ch )
       WRITE(ch,'(I10)'    ) sz(1) ; CALL setup_error_act ( '<s-stru_start>', ch )
       WRITE(ch,'(I10)'    ) sz(2) ; CALL setup_error_act ( '<s-stru_len>', ch )
       WRITE(ch,'(I10)'    ) ll(1) ; CALL setup_error_act ( '<l-stru_start>', ch )
       WRITE(ch,'(I10)'    ) ll(2) ; CALL setup_error_act ( '<l-stru_len>', ch )
    END IF
    !
  END FUNCTION ok_omi_ind_stru_len
  !
  !! Pr&uuml;fe, ob die Komponente "stru_list" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_stru_list &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='ok_omi_ind_stru_list' ! 
    INTEGER            , PARAMETER :: c_max=3 ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                                  ! 
    LOGICAL            :: as(c_max)                               ! 
    INTEGER            :: un(c_max), sz(c_max), ll(c_max), ss, in ! 
    !
    as(1) = ASSOCIATED(this%stru_start)
    as(2) = ASSOCIATED(this%stru_len)
    as(3) = ASSOCIATED(this%stru_list)
    un(:) = -1
    sz(:) = -1
    ll(:) = -1
    ss    = -1
    IF ( as(1) ) THEN
       un(1) = COUNT( this%stru_start(:) == c_undef_omi_ind_int )
       sz(1) = SIZE ( this%stru_start(:) )
       ll(1) = COUNT( this%stru_start(:) <= 0 )
    END IF
    IF ( as(2) ) THEN
       un(2) = COUNT( this%stru_len(:) == c_undef_omi_ind_int )
       sz(2) = SIZE ( this%stru_len(:) )
       ll(2) = COUNT( this%stru_len(:) <= 0 )
       ss    = SUM  ( this%stru_len(:) )
    END IF
    IF ( as(3) ) THEN
       un(3) = COUNT( this%stru_list(:) == c_undef_omi_ind_int )
       sz(3) = SIZE ( this%stru_list(:) )
       ll(3) = COUNT( this%stru_list(:) <= 0 )
       IF ( ALL(as(1:2)) ) THEN
          in = COUNT( this%stru_start(:) > sz(3) ) + COUNT( this%stru_start(:) + this%stru_len(:) - 1 > sz(3) )
       END IF
    END IF
    ok = ( ALL(as) .AND. ALL(un==0) .AND. ss==sz(3) .AND. ALL(ll==0) .AND. in==0 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') as(1) ; CALL setup_error_act ( '<a-stru_start>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') as(2) ; CALL setup_error_act ( '<a-stru_len>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') as(3) ; CALL setup_error_act ( '<a-stru_list>', ch(1:1) )
       WRITE(ch,'(I10)'    ) un(1) ; CALL setup_error_act ( '<u-stru_start>', ch )
       WRITE(ch,'(I10)'    ) un(2) ; CALL setup_error_act ( '<u-stru_len>', ch )
       WRITE(ch,'(I10)'    ) un(3) ; CALL setup_error_act ( '<u-stru_list>', ch )
       WRITE(ch,'(I10)'    ) sz(1) ; CALL setup_error_act ( '<s-stru_start>', ch )
       WRITE(ch,'(I10)'    ) sz(2) ; CALL setup_error_act ( '<s-stru_len>', ch )
       WRITE(ch,'(I10)'    ) sz(3) ; CALL setup_error_act ( '<s-stru_list>', ch )
       WRITE(ch,'(I10)'    ) ll(1) ; CALL setup_error_act ( '<l-stru_start>', ch )
       WRITE(ch,'(I10)'    ) ll(2) ; CALL setup_error_act ( '<l-stru_len>', ch )
       WRITE(ch,'(I10)'    ) ll(3) ; CALL setup_error_act ( '<l-stru_list>', ch )
       WRITE(ch,'(I10)'    ) ss    ; CALL setup_error_act ( '<s-ss>', ch )
       WRITE(ch,'(I10)'    ) in    ; CALL setup_error_act ( '<i-in>', ch )
    END IF
    !
  END FUNCTION ok_omi_ind_stru_list
  !
  !! Pr&uuml;fe, ob die Komponente "point_list" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ind_point_list &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='ok_omi_ind_point_list' ! 
    INTEGER            , PARAMETER :: c_max=2 ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                               ! 
    LOGICAL            :: as(c_max)                        ! 
    INTEGER            :: un(c_max), mn(c_max), mx(c_max), ll(c_max)  ! 
    !
    as(1) = ASSOCIATED(this%stru_list)
    as(2) = ASSOCIATED(this%point_list)
    un(:) = -1
    mn(:) = -1
    mx(:) = -1
    ll(:) = -1
    IF ( as(1) ) THEN
       un(1) = COUNT( this%stru_list(:) == c_undef_omi_ind_int )
       mn(1) = MINVAL( this%stru_list(:) )
       mx(1) = MAXVAL( this%stru_list(:) )
       ll(1) = COUNT( this%stru_list(:) <= 0 )
    END IF
    IF ( as(2) ) THEN
       un(2) = COUNT( this%point_list(:) == c_undef_omi_ind_int )
       mn(2) = MINVAL( this%point_list(:) )
       mx(2) = MAXVAL( this%point_list(:) )
       ll(2) = COUNT( this%point_list(:) <= 0 )
    END IF
    ok = ( ALL(as) .AND. ALL(un==0) .AND. ALL(ll==0) .AND. ALL(mn==mn(1)) .AND. ALL(mx==mx(1)) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') as(1) ; CALL setup_error_act ( '<a-stru_list>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') as(2) ; CALL setup_error_act ( '<a-point_list>', ch(1:1) )
       WRITE(ch,'(I10)'    ) un(1) ; CALL setup_error_act ( '<u-stru_list>', ch )
       WRITE(ch,'(I10)'    ) un(2) ; CALL setup_error_act ( '<u-point_list>', ch )
       WRITE(ch,'(I10)'    ) mn(1) ; CALL setup_error_act ( '<n-stru_list>', ch )
       WRITE(ch,'(I10)'    ) mn(2) ; CALL setup_error_act ( '<n-point_list>', ch )
       WRITE(ch,'(I10)'    ) mx(1) ; CALL setup_error_act ( '<x-stru_list>', ch )
       WRITE(ch,'(I10)'    ) mx(2) ; CALL setup_error_act ( '<x-point_list>', ch )
       WRITE(ch,'(I10)'    ) ll(1) ; CALL setup_error_act ( '<l-stru_list>', ch )
       WRITE(ch,'(I10)'    ) ll(2) ; CALL setup_error_act ( '<l-point_list>', ch )
    END IF
    !
  END FUNCTION ok_omi_ind_point_list
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_id &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_omi_ind_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%id)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente id  - - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ind_id
  !
  !! Drucke den Inhalt der Komponente "description" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_description &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='print_omi_ind_description' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%description)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente description - - - - - - - - - - - - - ',/&
           '# aktuell = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ind_description
  !
  !! Drucke den Inhalt der Komponente "stru_id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_stru_id &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='print_omi_ind_stru_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    INTEGER :: i    ! 
    !
    IF ( ASSOCIATED( this%stru_id ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%stru_id)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, TRIM(this%stru_id(i))
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'stru_id-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente stru_id(:)  - - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', stru_id = ',A)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ind_stru_id
  !
  !! Drucke den Inhalt der Komponente "stru_start" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_stru_start &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='print_omi_ind_stru_start' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    INTEGER :: i    ! 
    !
    IF ( ASSOCIATED( this%stru_start ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%stru_start)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%stru_start(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'stru_start-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente stru_start(:) - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', stru_start = ',I10)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ind_stru_start
  !
  !! Drucke den Inhalt der Komponente "stru_len" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_stru_len &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='print_omi_ind_stru_len' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    INTEGER :: i    ! 
    !
    IF ( ASSOCIATED( this%stru_len ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%stru_len)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%stru_len(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'stru_len-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente stru_len(:) - - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', stru_len = ',I10)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ind_stru_len
  !
  !! Drucke den Inhalt der Komponente "stru_list" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_stru_list &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='print_omi_ind_stru_list' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    INTEGER :: i    ! 
    !
    IF ( ASSOCIATED( this%stru_list ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%stru_list)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%stru_list(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'stru_list-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente stru_list(:)  - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', stru_list = ',I10)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ind_stru_list
  !
  !! Drucke den Inhalt der Komponente "point_list" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ind_point_list &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_ind) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='print_omi_ind_point_list' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    INTEGER :: i    ! 
    !
    IF ( ASSOCIATED( this%point_list ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7070, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%point_list)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%point_list(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7070, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'point_list-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7070, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente point_list(:) - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', point_list = ',I10)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ind_point_list
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "stru_id" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_stru_id &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    INTEGER , PARAMETER :: c_max=2   ! 
    LOGICAL             :: as(c_max) ! 
    INTEGER             :: sz(c_max) ! 
    !
    as(1) = ASSOCIATED( this1%stru_id )
    as(2) = ASSOCIATED( this2%stru_id )
    sz(:) = -1
    IF ( as(1) ) sz(1) = SIZE(this1%stru_id)
    IF ( as(2) ) sz(2) = SIZE(this2%stru_id)
    !
    IF ( ALL(.NOT.as) ) THEN
       ok = .true. 
    ELSE
       IF ( ALL(as) .AND. ALL(sz==sz(1)) ) THEN
          ok = ALL( this1%stru_id(:) == this2%stru_id(:) )
       ELSE
          ok = .false.
       END IF
    END IF
    !
  END FUNCTION eq_omi_ind_stru_id 
  !
  !! pr&uuml;fe Komponente "stru_start" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_stru_start &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    INTEGER , PARAMETER :: c_max=2   ! 
    LOGICAL             :: as(c_max) ! 
    INTEGER             :: sz(c_max) ! 
    !
    as(1) = ASSOCIATED( this1%stru_start )
    as(2) = ASSOCIATED( this2%stru_start )
    sz(:) = -1
    IF ( as(1) ) sz(1) = SIZE(this1%stru_start)
    IF ( as(2) ) sz(2) = SIZE(this2%stru_start)
    !
    IF ( ALL(.NOT.as) ) THEN
       ok = .true. 
    ELSE
       IF ( ALL(as) .AND. ALL(sz==sz(1)) ) THEN
          ok = ALL( this1%stru_start(:) == this2%stru_start(:) )
       ELSE
          ok = .false.
       END IF
    END IF
    !
  END FUNCTION eq_omi_ind_stru_start 
  !
  !! pr&uuml;fe Komponente "stru_len" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_stru_len &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    INTEGER , PARAMETER :: c_max=2   ! 
    LOGICAL             :: as(c_max) ! 
    INTEGER             :: sz(c_max) ! 
    !
    as(1) = ASSOCIATED( this1%stru_len )
    as(2) = ASSOCIATED( this2%stru_len )
    sz(:) = -1
    IF ( as(1) ) sz(1) = SIZE(this1%stru_len)
    IF ( as(2) ) sz(2) = SIZE(this2%stru_len)
    !
    IF ( ALL(.NOT.as) ) THEN
       ok = .true. 
    ELSE
       IF ( ALL(as) .AND. ALL(sz==sz(1)) ) THEN
          ok = ALL( this1%stru_len(:) == this2%stru_len(:) )
       ELSE
          ok = .false.
       END IF
    END IF
    !
  END FUNCTION eq_omi_ind_stru_len 
  !
  !! pr&uuml;fe Komponente "stru_list" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_stru_list &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    INTEGER , PARAMETER :: c_max=2   ! 
    LOGICAL             :: as(c_max) ! 
    INTEGER             :: sz(c_max) ! 
    !
    as(1) = ASSOCIATED( this1%stru_list )
    as(2) = ASSOCIATED( this2%stru_list )
    sz(:) = -1
    IF ( as(1) ) sz(1) = SIZE(this1%stru_list)
    IF ( as(2) ) sz(2) = SIZE(this2%stru_list)
    !
    IF ( ALL(.NOT.as) ) THEN
       ok = .true. 
    ELSE
       IF ( ALL(as) .AND. ALL(sz==sz(1)) ) THEN
          ok = ALL( this1%stru_list(:) == this2%stru_list(:) )
       ELSE
          ok = .false.
       END IF
    END IF
    !
  END FUNCTION eq_omi_ind_stru_list 
  !
  !! pr&uuml;fe Komponente "point_list" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ind_point_list &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    INTEGER , PARAMETER :: c_max=2   ! 
    LOGICAL             :: as(c_max) ! 
    INTEGER             :: sz(c_max) ! 
    !
    as(1) = ASSOCIATED( this1%point_list )
    as(2) = ASSOCIATED( this2%point_list )
    sz(:) = -1
    IF ( as(1) ) sz(1) = SIZE(this1%point_list)
    IF ( as(2) ) sz(2) = SIZE(this2%point_list)
    !
    IF ( ALL(.NOT.as) ) THEN
       ok = .true. 
    ELSE
       IF ( ALL(as) .AND. ALL(sz==sz(1)) ) THEN
          ok = ALL( this1%point_list(:) == this2%point_list(:) )
       ELSE
          ok = .false.
       END IF
    END IF
    !
  END FUNCTION eq_omi_ind_point_list 
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
  ! >>> CREATE-Methoden <<< 
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die Komponente "point_list(:)" aus der Komponente "stru_list(:)" <BR>
  !! Subroutien erzeugt Fehlermeldungen
  SUBROUTINE create_point_list_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ind) , INTENT(INOUT) :: this ! 
    !! Hilfsvariable
    INTEGER :: i, mn, mx, nn ! 
    INTEGER , ALLOCATABLE :: val(:) ! 
    LOGICAL , ALLOCATABLE :: hlp(:) ! 
    ! 
    IF ( ASSOCIATED(this%stru_list) ) THEN
       mn = MINVAL(this%stru_list)
       mx = MAXVAL(this%stru_list)
       IF ( mn > 0 .AND. mx >= mn ) THEN
          ALLOCATE( hlp(mx) )
          hlp(:) = .false.
          DO i=1,SIZE(this%stru_list)
             hlp(this%stru_list(i)) = .true.
          END DO
          nn = COUNT( hlp )
          ALLOCATE( val(nn) )
          nn = 0
          DO i=1,SIZE(hlp)
             IF ( hlp(i) ) THEN
                nn = nn + 1
                val(nn) = i
             END IF
          END DO
          DEALLOCATE( hlp )
          CALL set_omi_ind_point_list_0_1 ( this, val )
          DEALLOCATE( val )
       END IF
    END IF
    !
  END SUBROUTINE create_point_list_0
  !
END MODULE b_omi_ind
! TailOfBaseModule --------------------------------------------------------
