! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden (teilweise) analog zu der OpenMI-Schnittstelle <EM>IElementSet</EM></h2>
!! @author G. Lang
!! @version 2.4 vom 08/10/05, Quellcode: mod_b_omi_ele.f90
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
!  01.01 : 2005-02-18 : G. Lang : Erstversion
!  01.02 : 2005-02-23 : G. Lang : einige Set-Funktionen jetzt mit Endung "_ref"
!  01.03 : 2005-02-25 : G. Lang : neue Funktionen *_point_*
!  01.04 : 2005-02-25 : G. Lang : Korrektur in ok_omi_ele_xyz
!  01.05 : 2005-03-01 : G. Lang : SHAPE-Befehl in Parameter SHAPE durch explizite Dimensionen ersetzt
!  01.06 : 2005-03-02 : G. Lang : Kopierfunktion "copy_omi_ele"
!  01.07 : 2005-03-03 : G. Lang : neues Interface "set_omi_ele_auto"
!  01.08 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  01.09 : 2005-03-15 : G. Lang : AllBoundaries ergaenzt
!  01.10 : 2005-03-16 : G. Lang : GET-Funktionen zum Holen der Koordinaten, Automatisierung erweitert
!  01.11 : 2005-05-09 : G. Lang : Erweiterungen fuer io_dataset-Einbindung
!  01.12 : 2005-05-30 : G. Lang : Auto-Erweiterung "AllLocations" ergaenzt
!  01.13 : 2005-07-01 : G. Lang : Auto-Erweiterung "*Profiles" ergaenzt
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!  02.02 : 2005-07-25 : G. Lang : get_description_auto uebernimmt Description aus Indexlistenobjekt
!  02.03 : 2005-07-29 : G. Lang : FUNCTION get_omi_ref_ele_id_0 erweitert
!  02.04 : 2005-08-10 : G. Lang : Korrektur in get_description_auto
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>IElementSet</EM>. 
!! Impelementiert einen Typ zur Aufnahme von Informationen und Daten 
!! sogenannter <EM>ElementSets</EM>, welche den r&auml;lichen Bezug
!! verschiedener Datens&auml;tze beschreiben. Typ und Methoden 
!! erleichtern den Austausch der zwischen verschiedenen OpenMI-konformen 
!! Komponenten.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_ele";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_ele";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_ele";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_ele";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_ele";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_ele".
!! </OL>
!!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_omi_ele"
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:
!! <OL>
!!     <LI> id          : kurzer Identifikationsbezeichner
!!     <LI> description : ausf&uuml;hrliche verbale Beschreibung des Gitters
!!     <LI> elementtype : einheitlicher Typ aller Elemente eines <EM>ElementSet</EM>
!!     <LI> xyz         : Zeiger auf die Koordinaten (x,y,z) der Datenpunkte
!!     <LI> ind_xyz     : Zeiger auf Indexlisten mit Verweisen auf Datenpunkte
!!     <LI> ind_fac     : Zeiger auf Indexlisten mit Verweisen auf Oberfl&auml;chen und Polyeder
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
!!    <LI> Initialisieren des Moduls b_omi_ele mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_ele mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_ELE_ALL_ERRORS.
!!                                                                    <BR>
!
MODULE b_omi_ele
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  USE b_constants, ONLY : &
       ! Parameter
       Single,            &
       Double
  ! [A.2] BASIS-Modul mit Fehler-Typ und -Routinen 
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
  ! [A.3] weitere BASIS-Module (ONLY benutzen!)
  USE b_omi_xyz, ONLY :              &
       !   Typdefinitionen
       t_omi_xyz,                    &
       ! Konstante
       c_len_omi_xyz_id,             &
       !   Routinen / Interfaces
       init_omi_xyz,                 &
       clear_omi_xyz,                &
       setup_omi_xyz_prn_lun,        &
       setup_omi_xyz_trc_lun,        &
       is_omi_xyz_two_dimensional,   &
       is_omi_xyz_three_dimensional, &
       ok_omi_xyz,                   &
       get_omi_xyz_x_ref,            &
       get_omi_xyz_y_ref,            &
       get_omi_xyz_z_ref,            &
       get_omi_xyz_version,          &
       print_omi_xyz,                &
       eq_omi_xyz,                   &
       ne_omi_xyz
  ! [A.4] weitere BASIS-Module (ONLY benutzen!)
  USE b_omi_ind, ONLY :            &
       !   Typdefinitionen
       t_omi_ind,                  &
       !   Konstantwerte
       c_len_omi_ind_id,           &
       c_len_omi_ind_stru_id,      &
       !   Routinen / Interfaces
       init_omi_ind,               &
       clear_omi_ind,              &
       setup_omi_ind_prn_lun,      &
       setup_omi_ind_trc_lun,      &
       ok_omi_ind,                 &
       get_omi_ind_stru_count,     &
       has_omi_ind_stru_id,        &
       get_omi_ind_id,             &
       get_omi_ind_description,    &
       get_omi_ind_stru_id,        &
       get_omi_ind_stru_id_idx,    &
       get_omi_ind_stru_count,     &
       get_omi_ind_stru_start_ref, &
       get_omi_ind_stru_len_ref,   &
       get_omi_ind_stru_list_ref,  &
       get_omi_ind_point_list_ref, &
       get_omi_ind_stru_len_count, &
       get_omi_ind_stru_len_max,   &
       get_omi_ind_stru_len_min,   &
       get_omi_ind_point_count,    &
       get_omi_ind_pt_data,        &
       get_omi_ind_li_data,        &
       get_omi_ind_pl_data,        &
       get_omi_ind_west,           &
       get_omi_ind_east,           &
       get_omi_ind_north,          &
       get_omi_ind_south,          &
       get_omi_ind_top,            &
       get_omi_ind_bottom,         &
       print_omi_ind,              &
       eq_omi_ind,                 &
       ne_omi_ind
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
  !
  !! max. Anzahl der Zeichen in der Komponente <EM>id</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_ele_id=80          ! 
  !! max. Anzahl der Zeichen in der Komponente <EM>description</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_ele_description=80 ! 
  !! max. Anzahl der Zeichen in der Bezeichnung des ElementSetTyps
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_ele_typ=13         ! 
  !! max. Anzahl der Zeichen in der Kurz-Bezeichnung des ElementSetTyps
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_ele_short=5        ! 
  !! max. Anzahl der Zeichen in der Beschreibung des ElementSetTyps
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_ele_descr=44       ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! id          : kurzer Identifikationsbezeichner                                        <BR>
  !! description : ausf&uuml;hrliche verbale Beschreibung des Gitters                      <BR>
  !! elementtype : einheitlicher Typ aller Elemente eines <EM>ElementSet</EM>              <BR>
  !! xyz         : Zeiger auf die Koordinaten (x,y,z) der Datenpunkte                      <BR>
  !! ind_xyz     : Zeiger auf Indexlisten mit Verweisen auf Datenpunkte                    <BR>
  !! ind_fac     : Zeiger auf Indexlisten mit Verweisen auf Oberfl&auml;chen und Polyeder
  TYPE , PUBLIC :: t_omi_ele
     PRIVATE
     CHARACTER (LEN=c_len_omi_ele_id)          :: id          ! 
     CHARACTER (LEN=c_len_omi_ele_description) :: description ! 
     INTEGER                                   :: elementtype ! 
     TYPE (t_omi_xyz) , POINTER                :: xyz         ! 
     TYPE (t_omi_ind) , POINTER                :: ind_xyz     ! 
     TYPE (t_omi_ind) , POINTER                :: ind_fac     ! 
  END TYPE t_omi_ele
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Undefined-Wert f&uuml;r INTEGER-Komponenten
  INTEGER            , PUBLIC , PARAMETER :: c_undef_omi_ele_int=-999              ! 
  !! Undefined-Wert f&uuml;r DOUBLE-Komponenten
  REAL (KIND=Double) , PUBLIC , PARAMETER :: c_undef_omi_ele_double=1.0E+31_Double ! 
  !! Undefined-Wert f&uuml;r CHARACTER-Komponenten
  CHARACTER (LEN=9)  , PUBLIC , PARAMETER :: c_undef_omi_ele_char='undefined'      ! 
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_omi_ele
     MODULE PROCEDURE init_omi_ele_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_ele
     MODULE PROCEDURE clear_omi_ele_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_ele_prn_lun
     MODULE PROCEDURE setup_omi_ele_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_ele_trc_lun
     MODULE PROCEDURE setup_omi_ele_trc_lun_d ! 
  END INTERFACE
  !! Index f&uuml;r Spracheinstellung setzen <BR>
  !! 1 = Deutsch (Default) <BR>
  !! 2 = Englisch         
  INTERFACE setup_omi_ele_language
     MODULE PROCEDURE setup_omi_ele_language_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_omi_ele"; NULLIFY f&uuml;r dynamische 
  !! Komponenten-Felder und Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE new_omi_ele
     MODULE PROCEDURE new_omi_ele_0  ! 
     MODULE PROCEDURE new_omi_ele_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_ele"; ggf. De-Allokieren von 
  !! Memory und teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE kill_omi_ele
     MODULE PROCEDURE kill_omi_ele_0 ! 
     MODULE PROCEDURE kill_omi_ele_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_ele" auf G&uuml;ltigkeit: <BR>
  !! a) ein Datenobjekt (Skalar)    <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE ok_omi_ele
     MODULE PROCEDURE ok_omi_ele_0 ! 
     MODULE PROCEDURE ok_omi_ele_1 ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_ele": <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_ind" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_ele
     MODULE PROCEDURE print_omi_ele_0 ! 
     MODULE PROCEDURE print_omi_ele_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_ele_static
     MODULE PROCEDURE print_omi_ele_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_ele_all_errors
     MODULE PROCEDURE print_omi_ele_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "id" in "t_omi_ele" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar)                            <BR>
  !! b) viele Datenobjekte (Vektor)                         <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ele_id
     MODULE PROCEDURE set_omi_ele_id_0_0 ! 
     MODULE PROCEDURE set_omi_ele_id_1_0 ! 
  END INTERFACE
  !! Setze Komponente "description" in "t_omi_ele" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar)                                     <BR>
  !! b) viele Datenobjekte (Vektor)                                  <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ele_description
     MODULE PROCEDURE set_omi_ele_description_0_0 ! 
     MODULE PROCEDURE set_omi_ele_description_1_0 ! 
  END INTERFACE
  !! Setze Komponente "elementtype" in "t_omi_ele" auf Benutzerwert; <BR>
  !! a) ein Datenobjekt (Skalar)                                     <BR>
  !! b) viele Datenobjekte (Vektor)                                  <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_ele_elementtype
     MODULE PROCEDURE set_omi_ele_elementtype_0_0 ! 
     MODULE PROCEDURE set_omi_ele_elementtype_1_0 ! 
  END INTERFACE
  !! Setze Komponente "xyz" in "t_omi_ele" auf Benutzerwert; <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten:  <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) und einen Skalar   <BR>
  !! Es wird ein Zeiger auf die externen Daten eingerichtet.
  INTERFACE set_omi_ele_xyz_ref
     MODULE PROCEDURE set_omi_ele_xyz_ref_0_0 ! 
  END INTERFACE
  !! Setze Komponente "ind_xyz" in "t_omi_ele" auf Benutzerwert; <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten:      <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) und einen Skalar       <BR>
  !! Es wird ein Zeiger auf die externen Daten eingerichtet.
  INTERFACE set_omi_ele_ind_xyz_ref
     MODULE PROCEDURE set_omi_ele_ind_xyz_ref_0_0 !
  END INTERFACE
  !! Setze Komponente "ind_fac" in "t_omi_ele" auf Benutzerwert; <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten:      <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) und einen Skalar       <BR>
  !! Es wird ein Zeiger auf die externen Daten eingerichtet.
  INTERFACE set_omi_ele_ind_fac_ref
     MODULE PROCEDURE set_omi_ele_ind_fac_ref_0_0 ! 
  END INTERFACE
  !
  !! Index f&uuml;r Spracheinstellung ermitteln <BR>
  !! a) Deutsch (Default) <BR>
  !! b) Englisch         
  INTERFACE get_omi_ele_language
     MODULE PROCEDURE get_omi_ele_language_d ! 
  END INTERFACE
  !! Hole Komponente "id" aus "t_omi_ele": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_id
     MODULE PROCEDURE get_omi_ele_id_0_0 ! 
     MODULE PROCEDURE get_omi_ele_id_1_0 ! 
  END INTERFACE
  !! Hole Komponente "description" aus "t_omi_ele": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_description
     MODULE PROCEDURE get_omi_ele_description_0_0 ! 
     MODULE PROCEDURE get_omi_ele_description_1_0 ! 
  END INTERFACE
  !! Hole Komponente "elementtype" aus "t_omi_ele": <BR>
  !! a) f&uuml;r ein Objekt (Skalar)                <BR>
  !! b) f&uuml;r viele Objekte (Vektor)             <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_elementtype
     MODULE PROCEDURE get_omi_ele_elementtype_0_0 ! 
     MODULE PROCEDURE get_omi_ele_elementtype_1_0 ! 
  END INTERFACE
  !! Hole Komponente "version" aus "t_omi_ele": <BR>
  !! a) f&uuml;r ein Objekt (Skalar)            <BR>
  !! b) f&uuml;r viele Objekte (Vektor)         <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_version
     MODULE PROCEDURE get_omi_ele_version_0_0 ! 
     MODULE PROCEDURE get_omi_ele_version_1_0 ! 
  END INTERFACE
  !! Hole Komponente "xyz" aus "t_omi_ele": <BR>
  !! a) f&uuml;r ein Objekt (Skalar)        <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_xyz_ref
     MODULE PROCEDURE get_omi_ele_xyz_ref_0_0 ! 
  END INTERFACE
  !! Hole Komponente "ind_xyz" aus "t_omi_ele": <BR>
  !! a) f&uuml;r ein Objekt (Skalar)            <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_ind_xyz_ref
     MODULE PROCEDURE get_omi_ele_ind_xyz_ref_0_0 ! Skalar
  END INTERFACE
  !! Hole Komponente "ind_fac" aus "t_omi_ele": <BR>
  !! a) f&uuml;r ein Objekt (Skalar)            <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_ind_fac_ref
     MODULE PROCEDURE get_omi_ele_ind_fac_ref_0_0 ! Skalar
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! setze alle Komponenten der Variablen in einem Aufruf <BR>
  !! a) f&uuml;r ein Objekt
  INTERFACE set_omi_ele_auto
     MODULE PROCEDURE set_omi_ele_auto_0
  END INTERFACE
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_ele": <BR>
  !! a) f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! b) f&uuml;r viele Werte <EM>id(:)</EM> (Vektor)
  INTERFACE get_omi_ele_idx
     MODULE PROCEDURE get_omi_ele_idx_1_0
     MODULE PROCEDURE get_omi_ele_idx_1_1
  END INTERFACE
  !! ermittle die Anzahl der Elemente (im Sinne von OpenMI) in dem aktuellen ElementSet: <BR>
  !! Elemente im Sinne von OpenMI k&ouml;nnen z.B. Punkte, Linien oder Polygone sein: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_ele_element_count
     MODULE PROCEDURE get_omi_ele_element_count_0
     MODULE PROCEDURE get_omi_ele_element_count_1
  END INTERFACE
  !! ermittle die Anzahl der Fl&auml;chen (im Sinne von OpenMI) aus 
  !! denen ein r&auml;liches Element aufgebaut ist, bei Vorgabe eines
  !! Positionsindex f&uuml;r das gesuchte Element: <BR>
  !! a) f&uuml;r ein Objekt und einen Element-Positionsindex <BR>
  !! b) f&uuml;r ein Objekt und mehrere Element-Positionsindizes
  INTERFACE get_omi_ele_face_count
     MODULE PROCEDURE get_omi_ele_face_count_0_0
     MODULE PROCEDURE get_omi_ele_face_count_0_1
  END INTERFACE
  !! ermittle die Anzahl der Punkte (im Sinne von OpenMI) aus 
  !! denen ein r&auml;liches Element aufgebaut ist, bei Vorgabe eines
  !! Positionsindex f&uuml;r das gesuchte Element: <BR>
  !! a) f&uuml;r ein Objekt und einen Element-Positionsindex <BR>
  !! b) f&uuml;r ein Objekt und mehrere Element-Positionsindizes <BR>
  !! c) f&uuml;r ein Objekt (alle Punkte) <BR>
  !! d) f&uuml;r mehrere Objekte (alle Punkte)
  INTERFACE get_omi_ele_vertex_count
     MODULE PROCEDURE get_omi_ele_vertex_count_0_0
     MODULE PROCEDURE get_omi_ele_vertex_count_0_1
     MODULE PROCEDURE get_omi_ele_vertex_count_0
     MODULE PROCEDURE get_omi_ele_vertex_count_1
  END INTERFACE
  !! ermittle die Anzahl der Koordinatenpunkte in dem aktuellen Datenobjekt: <BR>
  !! a) f&uuml;r ein Objekt, alle Koordinatenpunkte <BR>
  !! b) f&uuml;r viele Objekte, jeweils alle Koordinatenpunkte
  INTERFACE get_omi_ele_point_count
     MODULE PROCEDURE get_omi_ele_point_count_0
     MODULE PROCEDURE get_omi_ele_point_count_1
  END INTERFACE
  !! ermittle die Anzahl der Punkte (im Sinne von OpenMI) aus 
  !! denen ein r&auml;liches Element aufgebaut ist, bei Vorgabe eines
  !! Positionsindex f&uuml;r das gesuchte Element sowie der Nummer
  !! der zu dem Elenet geh&ouml;renden Fl&aumlche: <BR>
  !! a) f&uuml;r ein Objekt, einen Element-Positionsindex und eine Fl&auml;chenposition <BR>
  !! b) f&uuml;r ein Objekt, einen Element-Positionsindex und mehrere Fl&auml;chenpositionen
  INTERFACE get_omi_ele_face_vertex_count
     MODULE PROCEDURE get_omi_ele_face_vertex_count00
     MODULE PROCEDURE get_omi_ele_face_vertex_count01
  END INTERFACE
  !! ermittle den Speicher-Index f&uuml;r ein Element (im Sinne von OpenMI) in
  !! Abh&auml;ngigkeit von dessen Name in einem ElementSet: <BR>
  !! a) f&uuml;r ein Objekt und eine Position <BR>
  !! b) f&uuml;r ein Objekt und mehrere Positionen
  INTERFACE get_omi_ele_element_idx
     MODULE PROCEDURE get_omi_ele_element_idx_0_0
     MODULE PROCEDURE get_omi_ele_element_idx_0_1
  END INTERFACE
  !! ermittle den Namen f&uuml;r ein Element (im Sinne von OpenMI) in
  !! Abh&auml;ngigkeit von dessen Position in einem ElementSet: <BR>
  !! a) f&uuml;r ein Objekt und eine Position <BR>
  !! b) f&uuml;r ein Objekt und mehrere Positionen
  INTERFACE get_omi_ele_element_id
     MODULE PROCEDURE get_omi_ele_element_id_0_0
     MODULE PROCEDURE get_omi_ele_element_id_0_1
  END INTERFACE
  !! ermittle den Positionsindex eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex) und Punkt (Position in dem Element) <BR>
  !! a) f&uuml;r ein Objekt, eine ElementId (Positionsindex) und eine Punktposition <BR>
  !! b) f&uuml;r ein Objekt, eine ElementId (Positionsindex) und mehrere Punktpositionen
  INTERFACE get_omi_ele_vertex_idx
     MODULE PROCEDURE get_omi_ele_vertex_idx_0_0
     MODULE PROCEDURE get_omi_ele_vertex_idx_0_1
  END INTERFACE
  !! ermittle den Positionsindex eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex), Fl&auml;che (Position in dem Element) und Punkt 
  !! (Position in der Fl&auml;che): <BR>
  !! a) f&uuml;r ein Objekt, eine ElementId (Positionsindex), eine Fl&auml;che und eine Punktposition <BR>
  !! b) f&uuml;r ein Objekt, eine ElementId (Positionsindex), eine Fl&auml;che und mehrere Punktpositionen
  INTERFACE get_omi_ele_face_vertex_idx
     MODULE PROCEDURE get_omi_ele_face_vertex_idx_0_0
     MODULE PROCEDURE get_omi_ele_face_vertex_idx_0_1
  END INTERFACE
  !! ermittle die globale Nummer eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex) und Punkt (Position in dem Element) bezogen
  !! auf das zu dem ElementSet geh&ouml;rende Koordinaten-Objekt  <BR>
  !! a) f&uuml;r ein Objekt, eine ElementId (Positionsindex) und eine Punktposition <BR>
  !! b) f&uuml;r ein Objekt, eine ElementId (Positionsindex) und mehrere Punktpositionen
  INTERFACE get_omi_ele_vertex_no
     MODULE PROCEDURE get_omi_ele_vertex_no_0_0
     MODULE PROCEDURE get_omi_ele_vertex_no_0_1
  END INTERFACE
  !! ermittle die globale Nummer eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex), Fl&auml;che (Position in dem Element) und Punkt 
  !! (Position in der Fl&auml;che) bezogen auf das zu dem ElementSet geh&ouml;rende 
  !! Koordinaten-Objekt: <BR>
  !! a) f&uuml;r ein Objekt, eine ElementId (Positionsindex), eine Fl&auml;che und eine Punktposition <BR>
  !! b) f&uuml;r ein Objekt, eine ElementId (Positionsindex), eine Fl&auml;che und mehrere Punktpositionen
  INTERFACE get_omi_ele_face_vertex_no
     MODULE PROCEDURE get_omi_ele_face_vertex_no_0_0
     MODULE PROCEDURE get_omi_ele_face_vertex_no_0_1
  END INTERFACE
  !! ermittle die globale Nummer eines bestimmten Koordinaten-Punktes in Abh&auml;ngigkeit
  !! von der Punkt-Nummer (get_omi_ele_point_count) bezogen auf das
  !! zu dem ElementSet geh&ouml;rende Koordinaten-Objekt  <BR>
  !! a) f&uuml;r ein Objekt und eine Punkt-Nummer <BR>
  !! b) f&uuml;r ein Objekt und mehrere Punkt-Nummern
  INTERFACE get_omi_ele_point_no
     MODULE PROCEDURE get_omi_ele_point_no_0_0
     MODULE PROCEDURE get_omi_ele_point_no_0_1
  END INTERFACE
  !
  !! ermittle die x-Koordinate eines bestimmten Punktes aus dem 
  !! Positionsindex des Punktes: <BR>
  !! a) f&uuml;r ein Objekt und eine Position <BR>
  !! b) f&uuml;r ein Objekt und viele Positionen 
  INTERFACE get_omi_ele_vertex_idx_x
     MODULE PROCEDURE get_omi_ele_vertex_idx_x_0_0
     MODULE PROCEDURE get_omi_ele_vertex_idx_x_0_1
  END INTERFACE
  !! ermittle die y-Koordinate eines bestimmten Punktes aus dem 
  !! Positionsindex des Punktes: <BR>
  !! a) f&uuml;r ein Objekt und eine Position <BR>
  !! b) f&uuml;r ein Objekt und viele Positionen 
  INTERFACE get_omi_ele_vertex_idx_y
     MODULE PROCEDURE get_omi_ele_vertex_idx_y_0_0
     MODULE PROCEDURE get_omi_ele_vertex_idx_y_0_1
  END INTERFACE
  !! ermittle die z-Koordinate eines bestimmten Punktes aus dem 
  !! Positionsindex des Punktes: <BR>
  !! a) f&uuml;r ein Objekt und eine Position <BR>
  !! b) f&uuml;r ein Objekt und viele Positionen 
  INTERFACE get_omi_ele_vertex_idx_z
     MODULE PROCEDURE get_omi_ele_vertex_idx_z_0_0
     MODULE PROCEDURE get_omi_ele_vertex_idx_z_0_1
  END INTERFACE
  !
  !! ermittle die x-Koordinate eines bestimmten Koordinaten-Punktes aus 
  !! der Punkt-Position (get_omi_ele_point_count) des Punktes: <BR>
  !! a) f&uuml;r ein Objekt und eine Position <BR>
  !! b) f&uuml;r ein Objekt und viele Positionen 
  INTERFACE get_omi_ele_point_idx_x
     MODULE PROCEDURE get_omi_ele_point_idx_x_0_0
     MODULE PROCEDURE get_omi_ele_point_idx_x_0_1
  END INTERFACE
  !! ermittle die y-Koordinate eines bestimmten Koordinaten-Punktes aus
  !! der Punkt-Position (get_omi_ele_point_count) des Punktes: <BR>
  !! a) f&uuml;r ein Objekt und eine Position <BR>
  !! b) f&uuml;r ein Objekt und viele Positionen 
  INTERFACE get_omi_ele_point_idx_y
     MODULE PROCEDURE get_omi_ele_point_idx_y_0_0
     MODULE PROCEDURE get_omi_ele_point_idx_y_0_1
  END INTERFACE
  !! ermittle die z-Koordinate eines bestimmten Koordinaten-Punktes aus 
  !! der Punkt-Position (get_omi_ele_point_count) des Punktes: <BR>
  !! a) f&uuml;r ein Objekt und eine Position <BR>
  !! b) f&uuml;r ein Objekt und viele Positionen 
  INTERFACE get_omi_ele_point_idx_z
     MODULE PROCEDURE get_omi_ele_point_idx_z_0_0
     MODULE PROCEDURE get_omi_ele_point_idx_z_0_1
  END INTERFACE
  !
  !! Hole die x-Koordinaten aus "t_omi_ele" <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_x_coord
     MODULE PROCEDURE get_omi_ele_x_coord_0
  END INTERFACE
  !! Hole die y-Koordinaten aus "t_omi_ele" <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_y_coord
     MODULE PROCEDURE get_omi_ele_y_coord_0
  END INTERFACE
  !! Hole die z-Koordinaten aus "t_omi_ele" <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_ele_z_coord
     MODULE PROCEDURE get_omi_ele_z_coord_0
  END INTERFACE
  !
  !! Hole die Id f&uuml;r eine Referebzgr&ouml;szlig;e aus der
  !! ElementSet Id eines vorgegebenen ElementSets
  INTERFACE get_omi_ref_ele_id
     MODULE PROCEDURE get_omi_ref_ele_id_0
  END INTERFACE
  !
  !! extrahiere die zu dem Datenobjekt (oder eines Teils davon) geh&ouml;renden
  !! Daten, die zuvor f&uuml;r das passende Koordinaten-Objekt interpoliert
  !! wurden: <BR>
  !! a) alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ele" bei 
  !!    Vorgabe der noch nicht interpolierten Daten <BR> 
  !! b) alle Daten (INTEGER) f&uuml;r ein Datenobjekt "t_omi_ind" bei 
  !!    Vorgabe der noch nicht interpolierten Daten
  INTERFACE get_omi_ele_data
     MODULE PROCEDURE get_omi_ele_data_d_1
!     MODULE PROCEDURE get_omi_ele_data_i_1
  END INTERFACE
  !
  !! ermittle die westliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_ele_west
     MODULE PROCEDURE get_omi_ele_west_0
     MODULE PROCEDURE get_omi_ele_west_1
  END INTERFACE
  !! ermittle die &ouml;stliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_ele_east
     MODULE PROCEDURE get_omi_ele_east_0
     MODULE PROCEDURE get_omi_ele_east_1
  END INTERFACE
  !! ermittle die n&ouml;rdliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_ele_north
     MODULE PROCEDURE get_omi_ele_north_0
     MODULE PROCEDURE get_omi_ele_north_1
  END INTERFACE
  !! ermittle die s&uuml;dliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_ele_south
     MODULE PROCEDURE get_omi_ele_south_0
     MODULE PROCEDURE get_omi_ele_south_1
  END INTERFACE
  !! ermittle die obere Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_ele_top
     MODULE PROCEDURE get_omi_ele_top_0
     MODULE PROCEDURE get_omi_ele_top_1
  END INTERFACE
  !! ermittle die unterste Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_ele_bottom
     MODULE PROCEDURE get_omi_ele_bottom_0
     MODULE PROCEDURE get_omi_ele_bottom_1
  END INTERFACE
  !
  !! Kopiere den Inhalt einer Variablen des Typs "t_omi_ele" in
  !! ein anderes Objekt vom gleichen Typ <BR>
  !! a) ein Quell-Objekt wird in ein Ziel-Objekt kopiert <BR>
  !! a) mehrere Quell-Objekte werden auf mehrere Ziel-Objekte kopiert
  INTERFACE copy_omi_ele
     MODULE PROCEDURE copy_omi_ele_0_0
     MODULE PROCEDURE copy_omi_ele_1_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_ele" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_ele
     MODULE PROCEDURE eq_omi_ele_0_0  ! 
     MODULE PROCEDURE eq_omi_ele_0_1  ! 
     MODULE PROCEDURE eq_omi_ele_1_0  ! 
     MODULE PROCEDURE eq_omi_ele_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fung zweier Datenobjekte "t_omi_ele" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_ele
     MODULE PROCEDURE ne_omi_ele_0_0  ! 
     MODULE PROCEDURE ne_omi_ele_0_1  ! 
     MODULE PROCEDURE ne_omi_ele_1_0  ! 
     MODULE PROCEDURE ne_omi_ele_1_1  ! 
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
  PUBLIC :: init_omi_ele
  PUBLIC :: clear_omi_ele
  PUBLIC :: setup_omi_ele_prn_lun
  PUBLIC :: setup_omi_ele_trc_lun
  PUBLIC :: new_omi_ele
  PUBLIC :: kill_omi_ele
  PUBLIC :: ok_omi_ele
  PUBLIC :: print_omi_ele
  PUBLIC :: print_omi_ele_static
  PUBLIC :: print_omi_ele_all_errors
  PUBLIC :: set_omi_ele_id
  PUBLIC :: set_omi_ele_description
  PUBLIC :: set_omi_ele_elementtype
  PUBLIC :: set_omi_ele_xyz_ref
  PUBLIC :: set_omi_ele_ind_xyz_ref
  PUBLIC :: set_omi_ele_ind_fac_ref
  PUBLIC :: get_omi_ele_id
  PUBLIC :: get_omi_ele_description
  PUBLIC :: get_omi_ele_elementtype
  PUBLIC :: get_omi_ele_version
  PUBLIC :: get_omi_ele_xyz_ref
  PUBLIC :: get_omi_ele_ind_xyz_ref
  PUBLIC :: get_omi_ele_ind_fac_ref
  PUBLIC :: eq_omi_ele
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: setup_omi_ele_language
  PUBLIC :: set_omi_ele_auto
  PUBLIC :: get_omi_ele_language
  PUBLIC :: get_omi_ele_idx 
  PUBLIC :: get_omi_ele_element_count
  PUBLIC :: get_omi_ele_element_idx
  PUBLIC :: get_omi_ele_element_id
  PUBLIC :: get_omi_ele_vertex_count
  PUBLIC :: get_omi_ele_vertex_idx
  PUBLIC :: get_omi_ele_vertex_no
  PUBLIC :: get_omi_ele_face_count
  PUBLIC :: get_omi_ele_face_vertex_count
  PUBLIC :: get_omi_ele_face_vertex_idx
  PUBLIC :: get_omi_ele_face_vertex_no
  PUBLIC :: get_omi_ele_point_count
  PUBLIC :: get_omi_ele_point_no
  PUBLIC :: get_omi_ele_vertex_idx_x
  PUBLIC :: get_omi_ele_vertex_idx_y
  PUBLIC :: get_omi_ele_vertex_idx_z
  PUBLIC :: get_omi_ele_point_idx_x
  PUBLIC :: get_omi_ele_point_idx_y
  PUBLIC :: get_omi_ele_point_idx_z
  PUBLIC :: get_omi_ele_x_coord
  PUBLIC :: get_omi_ele_y_coord
  PUBLIC :: get_omi_ele_z_coord
  PUBLIC :: get_omi_ref_ele_id
  PUBLIC :: get_omi_ele_data
  PUBLIC :: get_omi_ele_west
  PUBLIC :: get_omi_ele_east
  PUBLIC :: get_omi_ele_north
  PUBLIC :: get_omi_ele_south
  PUBLIC :: get_omi_ele_top
  PUBLIC :: get_omi_ele_bottom
  PUBLIC :: copy_omi_ele
  PUBLIC :: ne_omi_ele
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
  CHARACTER (LEN=09), PARAMETER :: c_modname      = 'b_omi_ele' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_ele
  INTEGER           , PARAMETER :: c_nofcomp      =  6               ! ggf. modifizieren
  !! Anzahl einstellbarer Sprachen
  INTEGER           , PARAMETER :: c_max_language = 2                ! [Deutsch,Englisch]
  !! Default-Language
  INTEGER           , PARAMETER :: c_def_language = 1                ! [Deutsch]
  !! max. Anzahl unterschiedlicher Elementtypen
  INTEGER           , PARAMETER :: c_max_omi_ele_typ=10              ! 
  !! Feld mit den Bezeichnungen der ElementSetTypen
  CHARACTER (LEN=c_len_omi_ele_typ) , PARAMETER :: c_omi_ele_typ(c_max_omi_ele_typ) = & !  
       (/ 'IDBased      ', 'XYPoint      ', 'XYLine       ', 'XYPolyline   ', 'XYPolygon    ', &  
          'XYZPoint     ', 'XYZLine      ', 'XYZPolyline  ', 'XYZPolygon   ', 'XYZPolyhedron' /)
  !! Feld mit den Kurz-Bezeichnungen der ElementSetTypen
  CHARACTER (LEN=c_len_omi_ele_short) , PARAMETER :: c_omi_ele_short(c_max_omi_ele_typ) = & !  
       (/ 'id   ', 'xyPt ', 'xyLn ', 'xyPl ', 'xyPg ', &  
          'xyzPt', 'xyzLn', 'xyzPl', 'xyzPg', 'xyzPh' /)
  !! Feld mit den sprachspezifischen Beschreibungen der ElementSetTypen
  CHARACTER (LEN=c_len_omi_ele_descr) , PARAMETER :: c_omi_ele_descr(c_max_omi_ele_typ,c_max_language) = & !  
       RESHAPE( (/ 'ID-basiert, keine Koordinatenangaben        ', &  
                   'geo-referenzierter Punkt in der xy-Ebene    ', &  
                   'geo-referenzierte Linie in der xy-Ebene     ', &  
                   'geo-referenzierter Linienzug in der xy-Ebene', &  
                   'geo-referenzierte Polygone in der xy-Ebene  ', &  
                   'geo-referenzierter Punkt im Raum            ', &  
                   'geo-referenzierte Linie im Raum             ', &  
                   'geo-referenzierter Linienzug im Raum        ', &  
                   'geo-referenzierte Polygone im Raum          ', &  
                   'geo-referenzierte Volumenelemente im Raum   ', &  
                   'id-based, no coordinates                    ', &  
                   'geo referenced point in xy-plane            ', &  
                   'geo referenced line in xy-plane             ', &  
                   'geo referenced poly-line in xy-plane        ', &  
                   'geo referenced polygons in xy-plane         ', &  
                   'geo-referenced point in space               ', &  
                   'geo-referenced line in space                ', &  
                   'geo-referenced polyline in space            ', &  
                   'geo-referenced polygons in space            ', &  
                   'geo-referenced polyhedrons in space         ' /), &
                   SHAPE=(/c_max_omi_ele_typ,c_max_language/) )
  !! Feld mit dem Code der ElementSetTypen
  INTEGER , PARAMETER :: c_omi_ele_code(c_max_omi_ele_typ) = (/0,1,2,3,4,5,6,7,8,9/) ! 
  !
  !! max. Anzahl Id's bei autoine set_omi_ele_automatischer Generierung
  INTEGER          , PARAMETER :: c_max_omi_ele_auto=15 ! 
  !! max. Anzahl der Zeichen in c_omi_ele_auto
  INTEGER          , PARAMETER :: c_len_omi_ele_auto=19 ! 
  !! max. Anzahl unterschiedlicher Dimensionen (2D, 3D)
  INTEGER          , PARAMETER :: c_max_omi_ele_dim=2   ! 
  !! max. Anzahl unterschiedlicher Strrukturen
  INTEGER          , PARAMETER :: c_max_omi_ele_stru=3  ! 
  !! Feld mit zul&auml;ssigen Id's der Index-Objekte f&uuml;r automatisches Erzeugen 
  CHARACTER (LEN=c_len_omi_ele_auto) , PARAMETER :: c_omi_ele_auto(c_max_omi_ele_auto) = (/ & ! 
       'AllVertices        ', & !  1
       'HorizontalGrid     ', & !  2 
       'AllBoundaries      ', & !  3
       'AllClosedBoundaries', & !  4
       'AllOpenBoundaries  ', & !  5 
       'AllLocations       ', & !  6 
       'AllProfiles        ', & !  7 
       'AllCrossProfiles   ', & !  8
       'AllLongProfiles    ', & !  9
       'CrossProfile?????  ', & ! 10
       'LongProfile?????   ', & ! 11
       'Boundary?????      ', & ! 12
       'ClosedBoundary?????', & ! 13
       'OpenBoundary?????  ', & ! 14 
       'Location?????      ' /) ! 15
  !! Feld mit Elementtypen bei automatischer Generierung
  INTEGER          , PARAMETER :: c_omi_ele_auto_code(c_max_omi_ele_auto,c_max_omi_ele_dim,c_max_omi_ele_stru) = & ! 
       RESHAPE( (/ 1,-1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    & ! 2D[1]
                   5,-1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,    & ! 3D[1]
                  -1,-1, 2, 2, 2,-1, 2, 2, 2, 2, 2, 2, 2, 2,-1,    & ! 2D[2]
                  -1,-1, 6, 6, 6,-1, 6, 6, 6, 6, 6, 6, 6, 6,-1,    & ! 3D[2]
                  -1, 4, 3, 3, 3,-1, 3, 3, 3, 3, 3, 3, 3, 3,-1,    & ! 2D[3]
                  -1, 8, 7, 7, 7,-1, 7, 7, 7, 7, 7, 7, 7, 7,-1 /), & ! 3D[3]
                   SHAPE=(/c_max_omi_ele_auto,c_max_omi_ele_dim,c_max_omi_ele_stru/) ) !
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
  !! aktuelle Spracheinstellung ( 1 = Deutsch, 2 = Englisch )
  INTEGER                , SAVE :: language=c_def_language ! 
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
  SUBROUTINE init_omi_ele_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='init_omi_ele_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_ele" version 2.4 of 08/10/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_omi_xyz ( )
       IF ( no_error( ) ) CALL init_omi_ind ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_ele_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_ele_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_ele_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='clear_omi_ele_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_ele_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_ind ( )
       IF ( no_error( ) ) CALL clear_omi_xyz ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_ele_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_ele_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_ele_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_xyz_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_ind_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_ele_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_ele_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_ele_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_xyz_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_ind_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_ele_trc_lun_d
  !
  !! Setzen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  SUBROUTINE setup_omi_ele_language_d ( var )
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER , INTENT(IN) :: var ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_ele_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       language = MERGE ( var, c_def_language, ( 1 <= var .AND. var <= c_max_language ) )
    END IF
    !
  END SUBROUTINE setup_omi_ele_language_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_ele_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_ele_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%id          = REPEAT( ' ', LEN(this%id) )
       this%id          = c_undef_omi_ele_char
       this%description = REPEAT( ' ', LEN(this%description) )
       this%description = c_undef_omi_ele_char
       this%elementtype = c_undef_omi_ele_int
       !
       NULLIFY ( this%xyz     )
       NULLIFY ( this%ind_xyz )
       NULLIFY ( this%ind_fac )
    END IF
    !
  END SUBROUTINE new_omi_ele_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_ele_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_ele_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_ele_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_ele_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_ele_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_ele_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       ! kein deallokieren der dynamischen Komponennten erforderlich
       ! ... da es sich bei ihnen um Zeiger auf externe Daten handelt
       IF ( no_error( ) ) CALL new_omi_ele_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_ele_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_ele_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_ele_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_ele_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_ele_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_ele_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_ele_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_omi_ele_id          ( this )
       l_ok(2) = ok_omi_ele_description ( this )
       l_ok(3) = ok_omi_ele_elementtype ( this )
       l_ok(4) = ok_omi_ele_xyz         ( this )
       l_ok(5) = ok_omi_ele_ind_xyz     ( this )
       l_ok(6) = ok_omi_ele_ind_fac     ( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_ele_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_ele_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_ele_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) ) EXIT
          ok(i) = ok_omi_ele_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_ele_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_ele_0' 
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
       IF ( no_error( ) ) CALL print_omi_ele_id          ( this )
       IF ( no_error( ) ) CALL print_omi_ele_description ( this )
       IF ( no_error( ) ) CALL print_omi_ele_elementtype ( this )
       IF ( no_error( ) ) CALL print_omi_ele_xyz         ( this )
       IF ( no_error( ) ) CALL print_omi_ele_ind_xyz     ( this )
       IF ( no_error( ) ) CALL print_omi_ele_ind_fac     ( this )
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
8000 FORMAT('# Beginn Objekt t_omi_ele ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_ele ------------------------------')
    !
  END SUBROUTINE print_omi_ele_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_ele_1' 
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
          IF ( no_error( ) ) CALL print_omi_ele_0 ( this(i) )
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
  END SUBROUTINE print_omi_ele_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_omi_ele_static_d' 
    !! Statusvariable
    INTEGER :: i, stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, language,   &
           c_undef_omi_ele_char, c_undef_omi_ele_int, c_undef_omi_ele_double, &
           ( c_omi_ele_code(i),c_omi_ele_typ(i),c_omi_ele_short(i),           &
             TRIM(c_omi_ele_descr(i,language)),i=1,c_max_omi_ele_typ )
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_omi_ele_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_ele         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#   initialised = ',L1,/ &
    '#        prn_op = ',L1,/ &
    '#        trc_op = ',L1,/ &
    '#       prn_lun = ',I5,/ &
    '#       trc_lun = ',I5,/ &
    '#        n_init = ',I5,/ &
    '#      language = ',I5,/ &
    '#   undef[char] = ',A,/ &
    '#    undef[int] = ',I10,/ &
    '# undef[double] = ',G15.6,/ &
    '# -code ----------typ short ---------------------------------------descr',/ &
    10('# ',I5,1X,A,1X,A,1X,A,/), &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_omi_ele_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=26), PARAMETER :: c_upname='print_omi_ele_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_ele_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! setze alle Komponenten eines Objekts, teilweise automatisch <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_ele_auto_0 ( this, xyz, ind_xyz, name, ind_fac )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(OUT)        :: this    ! 
    !! Wert f&uuml;r Komponente "xyz"
    TYPE (t_omi_xyz) , POINTER            :: xyz     ! 
    !! Wert f&uuml;r Komponente "ind_xyz"
    TYPE (t_omi_ind) , POINTER            :: ind_xyz ! 
    !! Wert mit Namenserg&auml;nzung
    CHARACTER (LEN=*) , INTENT(IN)        :: name    ! 
    !! (optional) Wert f&uuml;r Komponente "ind_fac"
    TYPE (t_omi_ind)  , OPTIONAL, POINTER :: ind_fac ! 
    !
    CALL new_omi_ele_0               ( this )
    CALL set_omi_ele_id_0_0          ( this, get_id_auto( xyz, ind_xyz, name ) )
    CALL set_omi_ele_description_0_0 ( this, get_description_auto( xyz, ind_xyz, name ) )
    CALL set_omi_ele_elementtype_0_0 ( this, get_elementtype_auto( xyz, ind_xyz ) ) 
    CALL set_omi_ele_xyz_ref_0_0     ( this, xyz )
    CALL set_omi_ele_ind_xyz_ref_0_0 ( this, ind_xyz )
    IF ( PRESENT(ind_fac) ) CALL set_omi_ele_ind_fac_ref_0_0 ( this, ind_fac )
    !
  END SUBROUTINE set_omi_ele_auto_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ele_id_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%id = REPEAT( ' ', LEN(this%id) )
    this%id = val(1:MIN(LEN(this%id),LEN_TRIM(val)))
    !
  END SUBROUTINE set_omi_ele_id_0_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ele_id_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_ele_id_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_ele_id_1_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ele_description_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%description = REPEAT( ' ', LEN(this%description) )
    this%description = val(1:MIN(LEN(this%description),LEN_TRIM(val)))
    !
  END SUBROUTINE set_omi_ele_description_0_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ele_description_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_ele_description_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_ele_description_1_0
  !
  !! weise der Komponente "elementtype" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ele_elementtype_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "elementtype"
    INTEGER          , INTENT(IN)    :: val  ! 
    !
    this%elementtype = val
    !
  END SUBROUTINE set_omi_ele_elementtype_0_0
  !
  !! weise der Komponente "elementtype" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_ele_elementtype_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "elementtype"
    INTEGER          , INTENT(IN)    :: val     ! 
    !
    this(:)%elementtype = val
    !
  END SUBROUTINE set_omi_ele_elementtype_1_0
  !
  !! weise der dynamischen Komponente "xyz" einen Skalar zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_ele_xyz_ref_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(INOUT) :: this ! 
    !! Werte f&uuml;r Komponente "xyz"
    TYPE (t_omi_xyz) , POINTER       :: val  ! 
    !
    this%xyz => val
    !
  END SUBROUTINE set_omi_ele_xyz_ref_0_0
  !
  !! weise der dynamischen Komponente "ind_xyz" einen Skalar zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_ele_ind_xyz_ref_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(INOUT) :: this ! 
    !! Werte f&uuml;r Komponente "ind_xyz"
    TYPE (t_omi_ind) , POINTER       :: val  ! 
    !
    this%ind_xyz => val
    !
  END SUBROUTINE set_omi_ele_ind_xyz_ref_0_0
  !
  !! weise der dynamischen Komponente "ind_fac" einen Skalar zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_ele_ind_fac_ref_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(INOUT) :: this ! 
    !! Werte f&uuml;r Komponente "ind_fac"
    TYPE (t_omi_ind) , POINTER       :: val  ! 
    !
    this%ind_xyz => val
    !
  END SUBROUTINE set_omi_ele_ind_fac_ref_0_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! Holen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  FUNCTION get_omi_ele_language_d ( ) &
       RESULT( res )
    !! R&uuml;ckgabewert: 
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER :: res ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='get_omi_ele_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       res = language
    ELSE
       res = c_undef_omi_ele_int
    END IF
    !
  END FUNCTION get_omi_ele_language_d
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_id_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele)   , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    CHARACTER (LEN=c_len_omi_ele_id) :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_omi_ele_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_id_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele)   , INTENT(IN)  :: this(:)         ! 
    !! R&uuml;ckgabewert "id"
    CHARACTER (LEN=c_len_omi_ele_id) :: val(SIZE(this)) ! 
    !
    val(:) = this%id
    !
  END FUNCTION get_omi_ele_id_1_0
  !
  !! hole die Komponente "description" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_description_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele)            , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "description" (Skalar)
    CHARACTER (LEN=c_len_omi_ele_description) :: val  ! 
    !
    val = this%description
    !
  END FUNCTION get_omi_ele_description_0_0
  !
  !! hole die Komponente "description" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_description_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele)            , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "description"
    CHARACTER (LEN=c_len_omi_ele_description) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%description
    !
  END FUNCTION get_omi_ele_description_1_0
  !
  !! hole die Komponente "elementtype" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_elementtype_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "elementtype" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%elementtype
    !
  END FUNCTION get_omi_ele_elementtype_0_0
  !
  !! hole die Komponente "elementtype" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_elementtype_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "elementtype"
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%elementtype
    !
  END FUNCTION get_omi_ele_elementtype_1_0
  !
  !! hole die Komponente "version" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_version_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "version" (Skalar)
    INTEGER :: val  ! 
    !
    val = get_omi_xyz_version ( this%xyz )
    !
  END FUNCTION get_omi_ele_version_0_0
  !
  !! hole die Komponente "version" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_version_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "version"
    INTEGER :: val(SIZE(this))  ! 
    !! Hilfsvariable
    INTEGER :: i                ! 
    !
    DO i=1,SIZE(val)
       val(i) = get_omi_ele_version_0_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ele_version_1_0
  !
  !! hole die dynamische Feld-Komponente "xyz" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_xyz_ref_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "xyz" 
    TYPE (t_omi_xyz) , POINTER     :: val  ! 
    !
    val => this%xyz
    !
  END FUNCTION get_omi_ele_xyz_ref_0_0
  !
  !! hole die dynamische Feld-Komponente "ind_xyz" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_ind_xyz_ref_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "ind_xyz"
    TYPE (t_omi_ind) , POINTER     :: val  ! 
    !
    val => this%ind_xyz
    !
  END FUNCTION get_omi_ele_ind_xyz_ref_0_0
  !
  !! hole die dynamische Feld-Komponente "ind_fac" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_ele_ind_fac_ref_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert "ind_fac" (Vektor)
    TYPE (t_omi_ind) , POINTER    :: val  ! 
    !
    val => this%ind_fac
    !
  END FUNCTION get_omi_ele_ind_fac_ref_0_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_ele" f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele)  , INTENT(IN) :: this(:) ! 
    !! zu suchender Wert der Komponente <EM>id</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), deren aktueller Wert
    !! der Komponente <EM>id</EM> identisch mit <EM>val</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: i, l1, l2 ! 
    !
    res = c_undef_omi_ele_int
    l1  = LEN_TRIM(val)
    DO i=1,SIZE(this)
       IF ( res /= c_undef_omi_ele_int ) EXIT
       l2 = LEN_TRIM(this(i)%id)
       IF ( l1 == l2 ) THEN
          IF ( val(1:l1) == this(i)%id(1:l2) ) res = i
       END IF
    END DO
    !
  END FUNCTION get_omi_ele_idx_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_ele" f&uuml;r viele Werte <EM>id(:)</EM> (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele)  , INTENT(IN) :: this(:) ! 
    !! zu suchende Werte der Komponente <EM>id</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), deren aktueller 
    !! Wert der Komponente <EM>id</EM> identisch mit <EM>id(:)</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_omi_ele_idx_1_0 ( this(:), val(i) )
    END DO
    !
  END FUNCTION get_omi_ele_idx_1_1
  !
  !! Ermittle die Anzahl der Elemente (im Sinne von OpenMI) in einem Objekt (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_element_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der Elemente (im Sinne von OpenMI)
    INTEGER                       :: res  ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this%ind_xyz )
    as(2) = ASSOCIATED( this%ind_fac )
    !
    IF      ( ALL(as) ) THEN 
       res = get_omi_ind_stru_count( this%ind_fac )
    ELSE IF ( as(1) .AND. .NOT. as(2) ) THEN
       res = get_omi_ind_stru_count( this%ind_xyz )
    ELSE
       res = 0
    END IF
    ! 
  END FUNCTION get_omi_ele_element_count_0
  !
  !! Ermittle die Anzahl der Elemente (im Sinne von OpenMI) in vielen Objekten (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_element_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:)        ! 
    !! Ergebnis: Anzahl der Elemente (im Sinne von OpenMI)
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_element_count_0 ( this(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_element_count_1
  !
  !! ermittle die Anzahl der Fl&auml;chen (im Sinne von OpenMI) aus 
  !! denen ein r&auml;liches Element aufgebaut ist, bei Vorgabe eines
  !! Positionsindex f&uuml;r das gesuchte Element <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_face_count_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Positionsindex des Elements
    INTEGER          , INTENT(IN) :: val  ! 
    !! Ergebnis: Anzahl der Fl&auml;chen des Elements
    INTEGER                       :: res  ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this%ind_xyz )
    as(2) = ASSOCIATED( this%ind_fac )
    !
    IF ( ALL(as) ) THEN 
       res = get_omi_ind_stru_len_count( this%ind_fac, val )
    ELSE
       res = 0
    END IF
    ! 
  END FUNCTION get_omi_ele_face_count_0_0
  !
  !! ermittle die Anzahl der Fl&auml;chen (im Sinne von OpenMI) aus 
  !! denen verschiedene r&auml;liche Elemente aufgebaut sind, bei 
  !! Vorgabe der Positionsindices f&uuml;r die gesuchten Elemente <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_face_count_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindices der Elemente
    INTEGER          , INTENT(IN) :: val(:) ! 
    !! Ergebnis: Anzahl der Fl&auml;chen der Elemente
    INTEGER :: res(SIZE(val))               ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_face_count_0_0( this, val(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_face_count_0_1
  !
  !! ermittle die Anzahl der Punkte (im Sinne von OpenMI) aus 
  !! denen ein r&auml;liches Element aufgebaut ist, bei Vorgabe eines
  !! Positionsindex f&uuml;r das gesuchte Element <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_count_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Positionsindex des Elements
    INTEGER          , INTENT(IN) :: val  ! 
    !! Ergebnis: Anzahl der Punkte des Elements
    INTEGER                       :: res  ! 
    !! Hilfsvariable
    LOGICAL :: as(2)             ! 
    INTEGER :: nn, ia, ie, i, ii ! 
    INTEGER , POINTER :: f_start(:), f_list(:) ! 
    !
    as(1) = ASSOCIATED( this%ind_xyz )
    as(2) = ASSOCIATED( this%ind_fac )
    !
    IF ( ALL(as) ) THEN 
       res = 0
       nn  = get_omi_ind_stru_len_count( this%ind_fac, val )
       IF ( nn > 0 ) THEN
          f_start => get_omi_ind_stru_start_ref ( this%ind_fac )
          f_list  => get_omi_ind_stru_list_ref  ( this%ind_fac )
          ia = f_start(val)
          ie = ia + nn - 1
          DO i=ia,ie
             ii  = f_list(i)
             res = res + get_omi_ind_stru_len_count( this%ind_xyz, ii )
          END DO
          NULLIFY( f_start, f_list )
       END IF
    ELSE IF ( as(1) .AND. .NOT. as(2) ) THEN
       res = get_omi_ind_stru_len_count( this%ind_xyz, val )
    ELSE
       res = 0
    END IF
    ! 
  END FUNCTION get_omi_ele_vertex_count_0_0
  !
  !! ermittle die Anzahl der Punkte (im Sinne von OpenMI) aus 
  !! denen verschiedene r&auml;liche Elemente aufgebaut sind, bei 
  !! Vorgabe der Positionsindices f&uuml;r die gesuchten Elemente <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_count_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindices der Elemente
    INTEGER          , INTENT(IN) :: val(:) ! 
    !! Ergebnis: Anzahl der Punkte der Elemente
    INTEGER :: res(SIZE(val))               ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_vertex_count_0_0( this, val(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_vertex_count_0_1
  !
  !! ermittle die Anzahl aller Punkte (im Sinne von OpenMI) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl aller Punkte
    INTEGER                       :: res  ! 
    !
    IF ( ASSOCIATED( this%ind_xyz ) ) THEN
       res = get_omi_ind_stru_len_count ( this%ind_xyz )
    ELSE
       res = 0
    END IF
    ! 
  END FUNCTION get_omi_ele_vertex_count_0
  !
  !! ermittle die Anzahl aller Punkte (im Sinne von OpenMI) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:)         ! 
    !! Ergebnis: Anzahl aller Punkte
    INTEGER                       :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_vertex_count_0 ( this(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_vertex_count_1
  !
  !! ermittle die Anzahl der Punkte (im Sinne von OpenMI) aus 
  !! denen die Fl&auml;che eines r&auml;lichen Elementes aufgebaut 
  !! ist, bei Vorgabe eines Positionsindex f&uuml;r das gesuchte 
  !! Element sowie der Nummer der Fl&auml;che im Element <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_face_vertex_count00 ( this, e_idx, n_f ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this  ! 
    !! Positionsindex des Elements
    INTEGER          , INTENT(IN) :: e_idx ! 
    !! Nummer der Fl&auml;che in dem Element (n_f-te Position)
    INTEGER          , INTENT(IN) :: n_f   ! 
    !! Ergebnis: Anzahl der Punkte der Fl&auml;che des Elements
    INTEGER                       :: res  ! 
    !! Hilfsvariable
    LOGICAL :: as(2)             ! 
    INTEGER :: ne, nf ! 
    INTEGER , POINTER :: f_start(:), f_list(:), v_len(:) ! 
    !
    as(1) = ASSOCIATED( this%ind_xyz )
    as(2) = ASSOCIATED( this%ind_fac )
    res   = 0
    !
    IF ( ALL(as) ) THEN 
       ne = get_omi_ind_stru_count ( this%ind_fac )
       IF ( e_idx >= 1 .AND. e_idx <= ne ) THEN
          nf = get_omi_ind_stru_len_count ( this%ind_fac, e_idx )
          IF ( n_f >= 1 .AND. n_f <= nf ) THEN
             f_start => get_omi_ind_stru_start_ref ( this%ind_fac )
             f_list  => get_omi_ind_stru_list_ref  ( this%ind_fac )
             v_len   => get_omi_ind_stru_len_ref   ( this%ind_xyz )
             res = v_len(f_start(e_idx)+n_f-1)
             NULLIFY ( f_start, f_list, v_len )
          END IF
       END IF
    END IF
    ! 
  END FUNCTION get_omi_ele_face_vertex_count00
  !
  !! ermittle die Anzahl der Punkte (im Sinne von OpenMI) aus 
  !! denen die Fl&auml;che eines r&auml;lichen Elementes aufgebaut 
  !! ist, bei Vorgabe eines Positionsindex f&uuml;r das gesuchte 
  !! Element sowie den Nummern der Fl&auml;chen im Element <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_face_vertex_count01 ( this, e_idx, n_f ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindex des Elements
    INTEGER          , INTENT(IN) :: e_idx  ! 
    !! Nummern der Fl&auml;chen in dem Element (n_f(:)-te Position)
    INTEGER          , INTENT(IN) :: n_f(:) ! 
    !! Ergebnis: Anzahl der Punkte der Fl&auml;che des Elements
    INTEGER :: res(SIZE(n_f))               ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_face_vertex_count00 ( this, e_idx, n_f(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_face_vertex_count01
  !
  !! Ermittle die Position eines Elements anhand einer "ElementID" in einem ElementSet <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_element_idx_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele)  , INTENT(IN) :: this ! 
    !! "ElementID" des Elements
    CHARACTER (LEN=*) , INTENT(IN) :: val  !  
    !! Index der Position mit "ElementID" <BR>
    !! falls undefiniert, wird undefiniert zur&uuml;ckgegeben
    INTEGER :: res   ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    INTEGER :: idx   ! 
    !
    as(1) = ASSOCIATED( this%ind_xyz )
    as(2) = ASSOCIATED( this%ind_fac )
    !
    IF      ( ALL(as)                 ) THEN 
       IF ( has_omi_ind_stru_id ( this%ind_fac ) ) THEN
          res = get_omi_ind_stru_id_idx ( this%ind_fac, val )
       ELSE
          idx = INDEX( val, '#' )
          IF ( idx > 1 ) THEN
             READ(val(1:idx-1),*) res
          ELSE
             res = c_undef_omi_ele_int
          END IF
       END IF
    ELSE IF ( as(1) .AND. .NOT. as(2) ) THEN
       IF ( has_omi_ind_stru_id ( this%ind_xyz ) ) THEN
          res = get_omi_ind_stru_id_idx ( this%ind_xyz, val )
       ELSE
          idx = INDEX( val, '#' )
          IF ( idx > 1 ) THEN
             READ(val(1:idx-1),*) res
          ELSE
             res = c_undef_omi_ele_int
          END IF
       END IF
    ELSE
       res = c_undef_omi_ele_int
    END IF
    ! 
  END FUNCTION get_omi_ele_element_idx_0_0
  !
  !! Ermittle die Positionen verschiedener Elemente anhand von "ElementIDs" in einem ElementSet <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_element_idx_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele)  , INTENT(IN) :: this   ! 
    !! "ElementID"s der Elemente
    CHARACTER (LEN=*) , INTENT(IN) :: val(:) !  
    !! Ergebnis: Indices der Positionen mit ElementID "val" <BR>
    !! falls undefiniert, wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_element_idx_0_0 ( this, val(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_element_idx_0_1
  !
  !! ermittle den Positionsindex eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex) und Punkt (Position in dem Element) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_idx_0_0 ( this, e_idx, n_v ) &
       RESULT( res ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindex des Elements (ElementID)
    INTEGER          , INTENT(IN) :: e_idx  ! 
    !! Nummer des Punktes in dem Element (n_v-te Position)
    INTEGER          , INTENT(IN) :: n_v    ! 
    !! Ergebnis: Positionsindex des Punktes im ElementSet
    INTEGER :: res   ! 
    !! Hilfsvariablen
    LOGICAL :: as(2) ! 
    INTEGER :: i, j, ne, ns, nn, ia, ie, ja, je ! 
    INTEGER , POINTER :: v_start(:), v_len(:)            ! 
    INTEGER , POINTER :: f_start(:), f_len(:), f_list(:) ! 
    !
    as(1) = ASSOCIATED( this%ind_xyz )
    as(2) = ASSOCIATED( this%ind_fac )
    res   = c_undef_omi_ele_int
    !
    IF ( ALL(as) ) THEN
       ne = get_omi_ind_stru_count( this%ind_fac )
       IF ( e_idx >=1 .AND. e_idx <= ne ) THEN
          f_start => get_omi_ind_stru_start_ref ( this%ind_fac )
          f_list  => get_omi_ind_stru_list_ref  ( this%ind_fac )
          f_len   => get_omi_ind_stru_len_ref   ( this%ind_fac )
          v_start => get_omi_ind_stru_start_ref ( this%ind_xyz )
          v_len   => get_omi_ind_stru_len_ref   ( this%ind_xyz )
          ia = f_start(e_idx)
          ie = ia + f_len(e_idx) - 1
          nn = 0
          DO i=ia,ie
             IF ( res /= c_undef_omi_ele_int ) EXIT
             ja = v_start(f_list(i))
             je = ja + v_len(f_list(i)) - 1
             DO j=ja,je
                IF ( res /= c_undef_omi_ele_int ) EXIT
                IF ( n_v > nn .AND. n_v <= nn+v_len(f_list(i)) ) THEN
                   res = v_start(f_list(i)) + n_v - nn
                END IF
             END DO
          END DO
          NULLIFY ( f_start, f_list, f_len, v_start, v_len )
       END IF
    ELSE IF ( as(1) .AND. .NOT. as(2) ) THEN
       ns = get_omi_ind_stru_count( this%ind_xyz )
       IF ( e_idx >= 1 .AND. e_idx <= ns ) THEN
          v_start => get_omi_ind_stru_start_ref ( this%ind_xyz )
          v_len   => get_omi_ind_stru_len_ref   ( this%ind_xyz )
          res = v_start(e_idx)
          IF ( n_v>=1 .AND. n_v<=v_len(e_idx) ) THEN
             res = v_start(e_idx) + n_v - 1
          END IF
          NULLIFY ( v_start, v_len )
       END IF
    END IF
    !
  END FUNCTION get_omi_ele_vertex_idx_0_0
  !
  !! ermittle den Positionsindex eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex) mehreren Punkten (Position in dem Element) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_idx_0_1 ( this, e_idx, n_v ) &
       RESULT( res ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindex des Elements (ElementID)
    INTEGER          , INTENT(IN) :: e_idx  ! 
    !! Nummer der Punkte in dem Element (n_v(:)-te Positionen)
    INTEGER          , INTENT(IN) :: n_v(:) ! 
    !! Ergebnis: Positionsindex des Punktes im ElementSet
    INTEGER :: res(SIZE(n_v)) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_vertex_idx_0_0 ( this, e_idx, n_v(i) ) 
    END DO
    !
  END FUNCTION get_omi_ele_vertex_idx_0_1
  !
  !! ermittle den Positionsindex eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex), Fl&auml;che (Position im Element) und Punkt 
  !! (Position in der Fl&auml;che): f&uuml;r ein Objekt, eine ElementId 
  !! (Positionsindex), eine Fl&aumlchenposition und eine Punktposition <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_face_vertex_idx_0_0 ( this, e_idx, n_f, n_v ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindex des Elements (ElementID)
    INTEGER          , INTENT(IN) :: e_idx  ! 
    !! Position der Fl&auml;che innerhalb des Elements (n_f-te Fl&auml;che)
    INTEGER          , INTENT(IN) :: n_f    ! 
    !! Position des Punktes innerhalb der Fl&auml;che (n_v-ter Punkt)
    INTEGER          , INTENT(IN) :: n_v    ! 
    !! Ergebnis: Positionsindex des Punktes im ElementSet
    INTEGER :: res   ! 
    !! Hilfsvariable
    LOGICAL :: as(2)          ! 
    INTEGER :: ne, nf, ns, nv ! 
    INTEGER , POINTER :: f_start(:), f_list(:) ! 
    INTEGER , POINTER :: v_start(:)            ! 
    ! 
    as(1) = ASSOCIATED( this%ind_xyz )
    as(2) = ASSOCIATED( this%ind_fac )
    res   = c_undef_omi_ele_int
    !
    IF ( ALL(as) ) THEN
       ne = get_omi_ind_stru_count ( this%ind_fac )
       IF ( e_idx >= 1 .AND. e_idx <= ne ) THEN
          nf = get_omi_ind_stru_len_count ( this%ind_fac, e_idx )
          IF ( n_f >= 1 .AND. n_f <= nf ) THEN
             f_start => get_omi_ind_stru_start_ref ( this%ind_fac )
             f_list  => get_omi_ind_stru_list_ref  ( this%ind_fac )
             ns = f_list(f_start(e_idx)+n_f-1)
             nv = get_omi_ind_stru_len_count( this%ind_xyz, ns )
             IF ( n_v >= 1 .AND. n_v <= nv ) THEN
                v_start => get_omi_ind_stru_start_ref ( this%ind_xyz )
                res = v_start(ns)+n_v-1 
                NULLIFY( v_start )
             END IF
             NULLIFY ( f_start, f_list )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_omi_ele_face_vertex_idx_0_0
  !
  !! ermittle den Positionsindex eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex), Fl&auml;che (Position im Element) und Punkt 
  !! (Position in der Fl&auml;che): f&uuml;r ein Objekt, eine ElementId 
  !! (Positionsindex), eine Fl&aumlchenposition und mehrere Punktpositionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_face_vertex_idx_0_1 ( this, e_idx, n_f, n_v ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindex des Elements (ElementID)
    INTEGER          , INTENT(IN) :: e_idx  ! 
    !! Position der Fl&auml;che innerhalb des Elements (n_f-te Fl&auml;che)
    INTEGER          , INTENT(IN) :: n_f    ! 
    !! Positionen der Punkte innerhalb der Fl&auml;che (n_v(:)-te Punkte)
    INTEGER          , INTENT(IN) :: n_v(:) ! 
    !! Ergebnis: Positionsindex des Punktes im< ElementSet
    INTEGER :: res(SIZE(n_v)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_face_vertex_idx_0_0 ( this, e_idx, n_f, n_v(i) )
    END DO
    !
  END FUNCTION get_omi_ele_face_vertex_idx_0_1
  !
  !! Ermittle den Namen eines Elements bei vorgegebener Indexposition in einem ElementSet <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_element_id_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele)  , INTENT(IN) :: this ! 
    !! Indexposition des Elements
    INTEGER           , INTENT(IN) :: val  !  
    !! Name des Elements
    CHARACTER (LEN=c_len_omi_ind_stru_id) :: res   ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this%ind_xyz )
    as(2) = ASSOCIATED( this%ind_fac )
    !
    IF      ( ALL(as)                 ) THEN 
       IF ( has_omi_ind_stru_id ( this%ind_fac ) ) THEN
          res = get_omi_ind_stru_id ( this%ind_fac, val )
       ELSE
          res = REPEAT( ' ', LEN(res) )
          WRITE(res,'(I8.8,A1,A5)') val, '#', c_omi_ele_short(this%elementtype+1)
       END IF
    ELSE IF ( as(1) .AND. .NOT. as(2) ) THEN
       IF ( has_omi_ind_stru_id ( this%ind_xyz ) ) THEN
          res = get_omi_ind_stru_id ( this%ind_xyz, val )
       ELSE
          res = REPEAT( ' ', LEN(res) )
          WRITE(res,'(I8.8,A1,A5)') val, '#', c_omi_ele_short(this%elementtype+1)
       END IF
    ELSE
       res   = REPEAT( ' ', LEN(res) )
       res   = c_undef_omi_ele_char
    END IF
    ! 
  END FUNCTION get_omi_ele_element_id_0_0
  !
  !! Ermittle den Namen eines Elements bei vorgegebener Indexposition in einem ElementSet <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_element_id_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele)  , INTENT(IN) :: this   ! 
    !! Indexpositionen der Elemente
    INTEGER           , INTENT(IN) :: val(:) !  
    !! Namen der Elemente
    CHARACTER (LEN=c_len_omi_ind_stru_id) :: res(SIZE(val)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_element_id_0_0 ( this, val(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_element_id_0_1
  !
  !! ermittle die globale Nummer eines bestimmten Punktes eines <EM>ElementSet</EM>
  !! bezogen auf das zugeh&ouml;rige Koordinaten-Objekt; die Nummer wird in 
  !! Abh&auml;ngigkeit der Positionsindices f&uuml;r Element und Punkt ermittelt; <BR>
  !! f&uuml;r ein Objekt, eine ElementId (Positionsindex) und einen Punkt (Nummer im Element)
  FUNCTION get_omi_ele_vertex_no_0_0 ( this, e_idx, n_v ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this  ! 
    !! Positionsindex Element (ElementID)
    INTEGER          , INTENT(IN) :: e_idx ! 
    !! Nummer des Punktes in dem Element (n_v-te Position)
    INTEGER          , INTENT(IN) :: n_v ! 
    !! Ergebnis: (globale) Nummer des Punktes im Koordinaten-Objekt des ElementSet
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: idx       ! 
    INTEGER , POINTER :: v_list(:) ! 
    !
    res = c_undef_omi_ele_int
    idx = get_omi_ele_vertex_idx ( this, e_idx, n_v )
    IF ( idx /= c_undef_omi_ele_int .AND. ASSOCIATED( this%ind_xyz ) ) THEN
       v_list => get_omi_ind_stru_list_ref ( this%ind_xyz )
       res    = v_list(idx)
       NULLIFY ( v_list )
    END IF
    !
  END FUNCTION get_omi_ele_vertex_no_0_0
  !
  !! ermittle die globalen Nummern mehrerer Punkte eines <EM>ElementSet</EM> bezogen
  !! auf das zugeh&ouml;rige Koordinaten-Objekt; die Nummern werden in Abh&auml;ngigkeit 
  !! der Positionsindices f&uuml;r Element und Punkt ermittelt; <BR>
  !! f&uuml;r ein Objekt, eine ElementId (Positionsindex) und mehrere Punkte (Positionsindices)
  FUNCTION get_omi_ele_vertex_no_0_1 ( this, e_idx, n_v ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindex Element (ElementID)
    INTEGER          , INTENT(IN) :: e_idx  ! 
    !! Nummern der Punkte in dem Element (n_v(:)-te Position)
    INTEGER          , INTENT(IN) :: n_v(:) ! 
    !! Ergebnis: (globale) Nummern der Punkte im Koordinaten-Objekt des ElementSet
    INTEGER :: res(SIZE(n_v))               ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_vertex_no_0_0 ( this, e_idx, n_v(i) )
    END DO
    !
  END FUNCTION get_omi_ele_vertex_no_0_1
  !
  !! ermittle die globale Nummer eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von Element (Positionsindex), Fl&auml;che (Position im Element) und Punkt 
  !! (Position in der Fl&auml;che): f&uuml;r ein Objekt, eine ElementId 
  !! (Positionsindex), eine Fl&aumlchenposition und eine Punktposition 
  !! bezogen auf das zu dem ElementSet geh&ouml;rende Koordinaten-Objekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_face_vertex_no_0_0 ( this, e_idx, n_f, n_v ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindex des Elements (ElementID)
    INTEGER          , INTENT(IN) :: e_idx  ! 
    !! Position der Fl&auml;che innerhalb des Elements (n_f-te Fl&auml;che)
    INTEGER          , INTENT(IN) :: n_f    ! 
    !! Position des Punktes innerhalb der Fl&auml;che (n_v-ter Punkt)
    INTEGER          , INTENT(IN) :: n_v    ! 
    !! Ergebnis: (globale) Nummer des Punktes im Koordinaten-Objekt des ElementSet
    INTEGER :: res   ! 
    !! Hilfsvariable
    INTEGER :: idx       ! 
    INTEGER , POINTER :: v_list(:) ! 
    !
    res = c_undef_omi_ele_int
    idx = get_omi_ele_face_vertex_idx ( this, e_idx, n_f, n_v )
    IF ( idx /= c_undef_omi_ele_int .AND. ASSOCIATED( this%ind_xyz ) ) THEN
       v_list => get_omi_ind_stru_list_ref ( this%ind_xyz )
       res    = v_list(idx)
       NULLIFY ( v_list )
    END IF
    !
  END FUNCTION get_omi_ele_face_vertex_no_0_0
  !
  !! ermittle die globalen Nummern verschiedener Punkte in Abh&auml;ngigkeit
  !! von Element (Positionsindex), Fl&auml;che (Position im Element) und Punkten 
  !! (Positionen in der Fl&auml;che): f&uuml;r ein Objekt, eine ElementId 
  !! (Positionsindex), eine Fl&aumlchenposition und eine Punktposition 
  !! bezogen auf das zu dem ElementSet geh&ouml;rende Koordinaten-Objekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_face_vertex_no_0_1 ( this, e_idx, n_f, n_v ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Positionsindex des Elements (ElementID)
    INTEGER          , INTENT(IN) :: e_idx  ! 
    !! Position der Fl&auml;che innerhalb des Elements (n_f-te Fl&auml;che)
    INTEGER          , INTENT(IN) :: n_f    ! 
    !! Positionen der Punkte innerhalb der Fl&auml;che (n_v(:)-te Punkte)
    INTEGER          , INTENT(IN) :: n_v(:) ! 
    !! Ergebnis: (globale) Nummer des Punktes im Koordinaten-Objekt des ElementSet
    INTEGER :: res(SIZE(n_v)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_face_vertex_no_0_0 ( this, e_idx, n_f, n_v(i) )
    END DO
    !
  END FUNCTION get_omi_ele_face_vertex_no_0_1
  !
  !! ermittle die x-Koordinate eines Punktes aus dem Positionsindex des Punktes <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_idx_x_0_0 ( this, v_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this  ! 
    !! Positionsindex des Punktes
    INTEGER          , INTENT(IN) :: v_idx ! 
    !! Ergebnis: x-Koordinate des Punktes oder "undefiniert"
    REAL (KIND=Double) :: res    ! 
    !! Hilfsvariable
    REAL (KIND=Double) , POINTER :: v_x(:)    ! 
    INTEGER            , POINTER :: v_list(:) ! 
    !
    res = c_undef_omi_ele_double
    IF ( ASSOCIATED(this%xyz) .AND. ASSOCIATED(this%ind_xyz) ) THEN
       v_x    => get_omi_xyz_x_ref         ( this%xyz     )
       v_list => get_omi_ind_stru_list_ref ( this%ind_xyz )
       IF ( v_idx >= 1 .AND. v_idx <= SIZE(v_list) ) res = v_x(v_list(v_idx))
       NULLIFY( v_x, v_list )
    END IF
    ! 
  END FUNCTION get_omi_ele_vertex_idx_x_0_0
  !
  !! ermittle die x-Koordinaten mehrerer Punkte aus den Positionsindices der Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_idx_x_0_1 ( this, v_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this     ! 
    !! Positionsindices der Punkte
    INTEGER          , INTENT(IN) :: v_idx(:) ! 
    !! Ergebnis: x-Koordinaten der Punkte oder "undefiniert"
    REAL (KIND=Double) :: res(SIZE(v_idx))    ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_vertex_idx_x_0_0 ( this, v_idx(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_vertex_idx_x_0_1
  !
  !! ermittle die y-Koordinate eines Punktes aus dem Positionsindex des Punktes <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_idx_y_0_0 ( this, v_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this  ! 
    !! Positionsindex des Punktes
    INTEGER          , INTENT(IN) :: v_idx ! 
    !! Ergebnis: y-Koordinate des Punktes oder "undefiniert"
    REAL (KIND=Double) :: res    ! 
    !! Hilfsvariable
    REAL (KIND=Double) , POINTER :: v_y(:)    ! 
    INTEGER            , POINTER :: v_list(:) ! 
    !
    res = c_undef_omi_ele_double
    IF ( ASSOCIATED(this%xyz) .AND. ASSOCIATED(this%ind_xyz) ) THEN
       v_y    => get_omi_xyz_y_ref         ( this%xyz     )
       v_list => get_omi_ind_stru_list_ref ( this%ind_xyz )
       IF ( v_idx >= 1 .AND. v_idx <= SIZE(v_list) ) res = v_y(v_list(v_idx))
       NULLIFY( v_y, v_list )
    END IF
    ! 
  END FUNCTION get_omi_ele_vertex_idx_y_0_0
  !
  !! ermittle die y-Koordinaten mehrerer Punkte aus den Positionsindices der Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_idx_y_0_1 ( this, v_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this     ! 
    !! Positionsindices der Punkte
    INTEGER          , INTENT(IN) :: v_idx(:) ! 
    !! Ergebnis: y-Koordinaten der Punkte oder "undefiniert"
    REAL (KIND=Double) :: res(SIZE(v_idx))    ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_vertex_idx_y_0_0 ( this, v_idx(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_vertex_idx_y_0_1
  !
  !! ermittle die z-Koordinate eines Punktes aus dem Positionsindex des Punktes <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_idx_z_0_0 ( this, v_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this  ! 
    !! Positionsindex des Punktes
    INTEGER          , INTENT(IN) :: v_idx ! 
    !! Ergebnis: z-Koordinate des Punktes oder "undefiniert"
    REAL (KIND=Double) :: res    ! 
    !! Hilfsvariable
    REAL (KIND=Double) , POINTER :: v_z(:)    ! 
    INTEGER            , POINTER :: v_list(:) ! 
    !
    res = c_undef_omi_ele_double
    IF ( ASSOCIATED(this%xyz) .AND. ASSOCIATED(this%ind_xyz) ) THEN
       IF ( is_omi_xyz_three_dimensional ( this%xyz ) ) THEN
          v_z    => get_omi_xyz_z_ref         ( this%xyz     )
          v_list => get_omi_ind_stru_list_ref ( this%ind_xyz )
          IF ( v_idx >= 1 .AND. v_idx <= SIZE(v_list) ) res = v_z(v_list(v_idx))
          NULLIFY( v_z, v_list )
       END IF
    END IF
    ! 
  END FUNCTION get_omi_ele_vertex_idx_z_0_0
  !
  !! ermittle die z-Koordinaten mehrerer Punkte aus den Positionsindices der Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_vertex_idx_z_0_1 ( this, v_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this     ! 
    !! Positionsindices der Punkte
    INTEGER          , INTENT(IN) :: v_idx(:) ! 
    !! Ergebnis: z-Koordinaten der Punkte oder "undefiniert"
    REAL (KIND=Double) :: res(SIZE(v_idx))    ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_vertex_idx_z_0_0 ( this, v_idx(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_vertex_idx_z_0_1
  !
  !! ermittle die Anzahl aller Koordinaten-Punkte (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl aller Koordinaten-Punkte
    INTEGER                       :: res  ! 
    !
    IF ( ASSOCIATED( this%ind_xyz ) ) THEN
       res = get_omi_ind_point_count ( this%ind_xyz )
    ELSE
       res = 0
    END IF
    ! 
  END FUNCTION get_omi_ele_point_count_0
  !
  !! ermittle die Anzahl aller Koordinaten-Punkte (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:)         ! 
    !! Ergebnis: Anzahl aller Koordinaten-Punkte
    INTEGER                       :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_point_count_0 ( this(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_point_count_1
  !
  !! ermittle die globale Nummer eines bestimmten Punktes in Abh&auml;ngigkeit
  !! von der Punkt-Nummer (get_omi_ele_point_count) bezogen auf das zu dem 
  !! ElementSet geh&ouml;rende Koordinaten-Objekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_no_0_0 ( this, p_idx ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Punkt-Nummer
    INTEGER          , INTENT(IN) :: p_idx    ! 
    !! Ergebnis: (globale) Nummer des Koordinaten-Punktes im Koordinaten-Objekt des ElementSet
    INTEGER :: res   ! 
    !! Hilfsvariable
    INTEGER , POINTER :: v_point(:) ! 
    !
    res = c_undef_omi_ele_int
    IF ( ASSOCIATED( this%ind_xyz ) ) THEN
       v_point => get_omi_ind_point_list_ref ( this%ind_xyz )
       res     = v_point(p_idx)
       NULLIFY ( v_point )
    END IF
    !
  END FUNCTION get_omi_ele_point_no_0_0
  !
  !! ermittle die globale Nummer mehrerer bestimmter Koordinaten-Punkte in Abh&auml;ngigkeit
  !! von den Punkt-Nummern (get_omi_ele_point_count) bezogen auf das zu dem 
  !! ElementSet geh&ouml;rende Koordinaten-Objekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_no_0_1 ( this, p_idx ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   ! 
    !! Punkte-Nummern
    INTEGER          , INTENT(IN) :: p_idx(:) ! 
    !! Ergebnis: (globale) Nummern der Koordinaten-Punkte im Koordinaten-Objekt des ElementSet
    INTEGER :: res(SIZE(p_idx))               ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_point_no_0_0 ( this, p_idx(i) )
    END DO
    !
  END FUNCTION get_omi_ele_point_no_0_1
  !
  !! ermittle die x-Koordinate eines Koordinaten-Punktes aus der Punkt-Position des Punktes <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_idx_x_0_0 ( this, p_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this  ! 
    !! Punkt-Position des Punktes
    INTEGER          , INTENT(IN) :: p_idx ! 
    !! Ergebnis: x-Koordinate des Koordinaten-Punktes oder "undefiniert"
    REAL (KIND=Double) :: res    ! 
    !! Hilfsvariable
    REAL (KIND=Double) , POINTER :: v_x(:)     ! 
    INTEGER            , POINTER :: v_point(:) ! 
    !
    res = c_undef_omi_ele_double
    IF ( ASSOCIATED(this%xyz) .AND. ASSOCIATED(this%ind_xyz) ) THEN
       v_x     => get_omi_xyz_x_ref          ( this%xyz     )
       v_point => get_omi_ind_point_list_ref ( this%ind_xyz )
       IF ( p_idx >= 1 .AND. p_idx <= SIZE(v_point) ) res = v_x(v_point(p_idx))
       NULLIFY( v_x, v_point )
    END IF
    ! 
  END FUNCTION get_omi_ele_point_idx_x_0_0
  !
  !! ermittle die x-Koordinaten mehrerer Koordinaten-Punkte aus den Punkt-Positionen der Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_idx_x_0_1 ( this, p_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this     ! 
    !! Punkt-Positionen der Punkte
    INTEGER          , INTENT(IN) :: p_idx(:) ! 
    !! Ergebnis: x-Koordinaten der Koordinaten-Punkte oder "undefiniert"
    REAL (KIND=Double) :: res(SIZE(p_idx))    ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_point_idx_x_0_0 ( this, p_idx(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_point_idx_x_0_1
  !
  !! ermittle die y-Koordinate eines Koordinaten-Punktes aus der Punkt-Position des Punktes <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_idx_y_0_0 ( this, p_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this  ! 
    !! Punkt-Position des Punktes
    INTEGER          , INTENT(IN) :: p_idx ! 
    !! Ergebnis: y-Koordinate des Koordinaten-Punktes oder "undefiniert"
    REAL (KIND=Double) :: res    ! 
    !! Hilfsvariable
    REAL (KIND=Double) , POINTER :: v_y(:)     ! 
    INTEGER            , POINTER :: v_point(:) ! 
    !
    res = c_undef_omi_ele_double
    IF ( ASSOCIATED(this%xyz) .AND. ASSOCIATED(this%ind_xyz) ) THEN
       v_y     => get_omi_xyz_y_ref          ( this%xyz     )
       v_point => get_omi_ind_point_list_ref ( this%ind_xyz )
       IF ( p_idx >= 1 .AND. p_idx <= SIZE(v_point) ) res = v_y(v_point(p_idx))
       NULLIFY( v_y, v_point )
    END IF
    ! 
  END FUNCTION get_omi_ele_point_idx_y_0_0
  !
  !! ermittle die y-Koordinaten mehrerer Koordinaten-Punkte aus den Punkt-Positionen der Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_idx_y_0_1 ( this, p_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this     ! 
    !! Punkt-Positionen der Punkte
    INTEGER          , INTENT(IN) :: p_idx(:) ! 
    !! Ergebnis: y-Koordinaten der Koordinaten-Punkte oder "undefiniert"
    REAL (KIND=Double) :: res(SIZE(p_idx))    ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_point_idx_y_0_0 ( this, p_idx(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_point_idx_y_0_1
  !
  !! ermittle die z-Koordinate eines Koordinaten-Punktes aus der Punkt-Position des Punktes <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_idx_z_0_0 ( this, p_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this  ! 
    !! Punkt-Position des Punktes
    INTEGER          , INTENT(IN) :: p_idx ! 
    !! Ergebnis: z-Koordinate des Koordinaten-Punktes oder "undefiniert"
    REAL (KIND=Double) :: res    ! 
    !! Hilfsvariable
    REAL (KIND=Double) , POINTER :: v_z(:)     ! 
    INTEGER            , POINTER :: v_point(:) ! 
    !
    res = c_undef_omi_ele_double
    IF ( ASSOCIATED(this%xyz) .AND. ASSOCIATED(this%ind_xyz) ) THEN
       IF ( is_omi_xyz_three_dimensional ( this%xyz ) ) THEN
          v_z     => get_omi_xyz_z_ref          ( this%xyz     )
          v_point => get_omi_ind_point_list_ref ( this%ind_xyz )
          IF ( p_idx >= 1 .AND. p_idx <= SIZE(v_point) ) res = v_z(v_point(p_idx))
          NULLIFY( v_z, v_point )
       END IF
    END IF
    ! 
  END FUNCTION get_omi_ele_point_idx_z_0_0
  !
  !! ermittle die z-Koordinaten mehrerer Koordinaten-Punkte aus den Punkt-Positionen der Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_point_idx_z_0_1 ( this, p_idx ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this     ! 
    !! Punkt-Position der Punkte
    INTEGER          , INTENT(IN) :: p_idx(:) ! 
    !! Ergebnis: z-Koordinaten der Koordinaten-Punkte oder "undefiniert"
    REAL (KIND=Double) :: res(SIZE(p_idx))    ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_point_idx_z_0_0 ( this, p_idx(i) )
    END DO
    ! 
  END FUNCTION get_omi_ele_point_idx_z_0_1
  !
  !! ermittle die x-Koordinaten des ElementSet <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_x_coord_0 ( this ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   !
    !! Ergebnis: x-Koordinaten
    REAL (KIND=Double) , POINTER :: res(:)  ! 
    !! Hilfsvariablen
    INTEGER            , POINTER :: p_p(:)  ! 
    REAL (KIND=Double) , POINTER :: p_x(:)  ! 
    !
    NULLIFY ( res )
    IF ( get_omi_ind_point_count( this%ind_xyz ) > 0 ) THEN
       p_x => get_omi_xyz_x_ref( this%xyz )
       p_p => get_omi_ind_point_list_ref ( this%ind_xyz )
       IF ( ASSOCIATED( p_x ) .AND. ASSOCIATED( p_p ) ) THEN
          ALLOCATE ( res(get_omi_ind_point_count( this%ind_xyz )) )
          res(:) = c_undef_omi_ele_double
          res(:) = p_x(p_p(:))
       END IF
    END IF
    NULLIFY ( p_p, p_x )
    !
  END FUNCTION get_omi_ele_x_coord_0
  !
  !! ermittle die y-Koordinaten des ElementSet <BR>
  !! es wrid eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_y_coord_0 ( this ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   !
    !! Ergebnis: y-Koordinaten
    REAL (KIND=Double) , POINTER :: res(:)  ! 
    !! Hilfsvariablen
    INTEGER            , POINTER :: p_p(:)  ! 
    REAL (KIND=Double) , POINTER :: p_y(:)  ! 
    !
    NULLIFY ( res )
    IF ( get_omi_ind_point_count( this%ind_xyz ) > 0 ) THEN
       p_y => get_omi_xyz_y_ref( this%xyz )
       p_p => get_omi_ind_point_list_ref ( this%ind_xyz )
       IF ( ASSOCIATED( p_y ) .AND. ASSOCIATED( p_p ) ) THEN
          ALLOCATE ( res(get_omi_ind_point_count( this%ind_xyz )) )
          res(:) = c_undef_omi_ele_double
          res(:) = p_y(p_p(:))
       END IF
    END IF
    NULLIFY ( p_p, p_y )
    !
  END FUNCTION get_omi_ele_y_coord_0
  !
  !! ermittle die z-Koordinaten des ElementSet <BR>
  !! es wrid eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_z_coord_0 ( this ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this   !
    !! Ergebnis: z-Koordinaten
    REAL (KIND=Double) , POINTER :: res(:)  ! 
    !! Hilfsvariablen
    INTEGER            , POINTER :: p_p(:)  ! 
    REAL (KIND=Double) , POINTER :: p_z(:)  ! 
    !
    NULLIFY ( res )
    IF ( get_omi_ind_point_count( this%ind_xyz ) > 0 ) THEN
       p_z => get_omi_xyz_z_ref( this%xyz )
       p_p => get_omi_ind_point_list_ref ( this%ind_xyz )
       IF ( ASSOCIATED( p_z ) .AND. ASSOCIATED( p_p ) ) THEN
          ALLOCATE ( res(get_omi_ind_point_count( this%ind_xyz )) )
          res(:) = c_undef_omi_ele_double
          res(:) = p_z(p_p(:))
       END IF
    END IF
    NULLIFY ( p_p, p_z )
    !
  END FUNCTION get_omi_ele_z_coord_0
  !
  FUNCTION get_omi_ref_ele_id_0 ( this, val ) &
       RESULT(res)
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele)    , INTENT(IN) :: this ! 
    !! f&uuml;hrender Namensteil der Referenz ElementSet Id,
    !! z.B. "AllVertices/xyPt" oder "AllLocations/xyPt"
    CHARACTER (LEN=*)   , INTENT(IN) :: val  ! 
    !! Ergebnis: Id f&uuml;r ElementSet der Referenzgr&ouml;&szlig;e
    CHARACTER (LEN=c_len_omi_ele_id) :: res  ! 
    !! Hilfsvariable
    CHARACTER (LEN=6) :: ch   ! 
    INTEGER :: ia, ie, ja, je ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_omi_ele_char
    ia  = INDEX( this%id, '[' )
    ie  = INDEX( this%id, ']' )
    res = TRIM(val)//this%id(ia:ie)
    ja  = INDEX( res, '(' )+1
    je  = INDEX( res, ')' )-1
    WRITE(ch,'(A2,I1,A1,I1,A1)') '(I', je-ja+1, '.', je-ja+1, ')'
    WRITE(res(ja:je),ch) 2
    ! 
  END FUNCTION get_omi_ref_ele_id_0
  !
  !! extrahiere alle Daten (REAL(Double)) f&uuml;r ein Datenobjekt "t_omi_ind" 
  !! bei Vorgabe des Koordinaten-Objekts "t_omi_xyz" sowie der noch nicht 
  !! interpolierten Daten; die Daten werden interpoliert und extrahiert: <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_data_d_1 ( this, val, missing, nres ) &
       RESULT(res)
    !! Datenobjekt (Skalar) [ muss o.k. sein ]
    TYPE (t_omi_ele)   , INTENT(IN)  :: this    ! 
    !! noch nicht interpolierte Daten (Double)
    REAL (KIND=Double) , INTENT(IN)  :: val(:)  ! 
    !! Wert zur Kennzeichnung fehlender/ung&uuml;ltiger Daten
    REAL (KIND=Double) , INTENT(IN)  :: missing ! 
    !! L&auml;nge des zu erzeugenden Ergebnisvektors
    INTEGER            , INTENT(IN)  :: nres    ! 
    !! interpoliertes Ergebnis f&uuml;r die Elemente des <EM>ElementSet</EM>
    REAL (KIND=Double)               :: res(nres)  ! 
    !
    res(:) = c_undef_omi_ele_double
    IF ( get_omi_ele_element_count( this ) > 0 ) THEN
       SELECT CASE ( get_omi_ele_elementtype(this) )
       CASE ( 1, 5 ) ! XY|XYZPoint
          res(:) = get_omi_ind_pt_data ( this%ind_xyz, this%xyz, val, missing )
       CASE ( 2, 6 ) ! XY|XYZLine
          res(:) = get_omi_ind_li_data ( this%ind_xyz, this%xyz, val, missing )
       CASE ( 3, 7 ) ! XY|XYZPolyLine
          res(:) = get_omi_ind_pl_data ( this%ind_xyz, this%xyz, val, missing )
       CASE ( 4, 8 ) ! XY|XYZPolyGon [ derzeit dieselbe Methode wie XY|XYZLine ]
          res(:) = get_omi_ind_li_data ( this%ind_xyz, this%xyz, val, missing )
       END SELECT
    END IF
    !
  END FUNCTION get_omi_ele_data_d_1
  !
  !! Ermittle die Koordinate f&uuml;r den westlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_west_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Koordinate f&uuml;r westlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    res = get_omi_ind_west ( this%ind_xyz, this%xyz )
    !
  END FUNCTION get_omi_ele_west_0
  !
  !! Ermittle die Koordinaten f&uuml;r die westlichen R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_west_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r westliche R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_west_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ele_west_1
  !
  !! Ermittle die Koordinate f&uuml;r den &ouml;stlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_east_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Koordinate f&uuml;r &ouml;stlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    res = get_omi_ind_east ( this%ind_xyz, this%xyz )
    !
  END FUNCTION get_omi_ele_east_0
  !
  !! Ermittle die Koordinaten f&uuml;r die &ouml;stliche R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_east_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r &ouml;stliche R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_east_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ele_east_1
  !
  !! Ermittle die Koordinate f&uuml;r den n&ouml;rdlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_north_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Koordinate f&uuml;r n&ouml;rdlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    res = get_omi_ind_north ( this%ind_xyz, this%xyz )
    !
  END FUNCTION get_omi_ele_north_0
  !
  !! Ermittle die Koordinaten f&uuml;r die n&ouml;rdliche R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_north_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r n&ouml;rdliche R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_north_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ele_north_1
  !
  !! Ermittle die Koordinate f&uuml;r den s&uuml;dlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_south_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Koordinate f&uuml;r s&uuml;dlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    res = get_omi_ind_south ( this%ind_xyz, this%xyz )
    !
  END FUNCTION get_omi_ele_south_0
  !
  !! Ermittle die Koordinaten f&uuml;r die s&uuml;dliche R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_south_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r s&uuml;dliche R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_south_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ele_south_1
  !
  !! Ermittle die Koordinate f&uuml;r den oberen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_top_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Koordinate f&uuml;r oberen Rand
    REAL (KIND=Double) :: res ! 
    !
    res = get_omi_ind_top ( this%ind_xyz, this%xyz )
    !
  END FUNCTION get_omi_ele_top_0
  !
  !! Ermittle die Koordinaten f&uuml;r die obere R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_top_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r obere R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_top_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ele_top_1
  !
  !! Ermittle die Koordinate f&uuml;r den unteren Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_bottom_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Ergebnis: Koordinate f&uuml;r unteren Rand
    REAL (KIND=Double) :: res ! 
    !
    res = get_omi_ind_bottom ( this%ind_xyz, this%xyz )
    !
  END FUNCTION get_omi_ele_bottom_0
  !
  !! Ermittle die Koordinaten f&uuml;r die untere R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_ele_bottom_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r untere R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_ele_bottom_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_ele_bottom_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ele_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = ( this1%id          == this2%id          )
    l_ok(2)  = ( this1%description == this2%description )
    l_ok(3)  = ( this1%elementtype == this2%elementtype )
    l_ok(4)  = eq_omi_ele_xyz ( this1, this2 )
    l_ok(5)  = eq_omi_ele_ind_xyz ( this1, this2 )
    l_ok(6)  = eq_omi_ele_ind_fac ( this1, this2 )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_ele_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ele_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_ele_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_ele_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ele_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_ele_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_ele_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ele_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_ele_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_ele_1_1
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
  FUNCTION ne_omi_ele_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_ele_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_ele_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_ele_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_ele_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_ele_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_ele_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_ele_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_ele_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_ele_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_ele) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariablen
    INTEGER :: l ! 
    !
    l       = SIZE(ok)
    ok(1:l) = .NOT. eq_omi_ele_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_ele_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-Copy-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! kopiere den Inhalt einer Komponente in eine andere Komponente <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_ele_0_0 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_ele) , INTENT(OUT) :: this1 ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN)  :: this2 ! 
    !
    CALL new_omi_ele_0               ( this1 )
    CALL set_omi_ele_id_0_0          ( this1, this2%id          )
    CALL set_omi_ele_description_0_0 ( this1, this2%description )
    CALL set_omi_ele_elementtype_0_0 ( this1, this2%elementtype )
    IF ( ASSOCIATED( this2%xyz     ) ) CALL set_omi_ele_xyz_ref_0_0     ( this1, this2%xyz )
    IF ( ASSOCIATED( this2%ind_xyz ) ) CALL set_omi_ele_ind_xyz_ref_0_0 ( this1, this2%ind_xyz )
    IF ( ASSOCIATED( this2%ind_fac ) ) CALL set_omi_ele_ind_fac_ref_0_0 ( this1, this2%ind_fac )
    !
  END SUBROUTINE copy_omi_ele_0_0
  !
  !! kopiere den Inhalt mehrere Komponente auf andere Komponenten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_ele_1_1 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_ele) , INTENT(OUT) :: this1(:) ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN)  :: this2(:) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       CALL copy_omi_ele_0_0 ( this1(i), this2(i) )
    END DO
    !
  END SUBROUTINE copy_omi_ele_1_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_ele" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_ele ausfuehren'
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
  SUBROUTINE init_omi_ele_all_errors ( )
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
               '--> INIT_omi_ele ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_ele ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ele"\n'//&
               'Typ-Komponente = "id"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> korrekte Daten mit SET_OMI_ELE_ID zuweisen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ele"\n'//&
               'Typ-Komponente = "description"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> korrekte Daten mit SET_OMI_ELE_DESCRIPTION zuweisen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ele"\n'//&
               'Typ-Komponente = "elementtype"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> korrekte Daten mit SET_OMI_ELE_ELEMENTTYPE zuweisen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ele"\n'//&
               'Typ-Komponente = "xyz"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'ElementSetTyp  = <ele-typ>\n'//&
               'assoziiert     = <associated>\n'//&
               'Inhalt o.k.    = <ok-contents>\n'//&
               'Dimension o.k. = <ok-dimension>\n'//&
               '--> korrekte Daten mit SET_OMI_ELE_XYZ_REF zuweisen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ele"\n'//&
               'Typ-Komponente = "ind_xyz"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'assoziiert     = <associated>\n'//&
               'Inhalt o.k.    = <ok-contents>\n'//&
               'Konsistenz     = <ok-consistent>\n'//&
               '--> korrekte Daten mit SET_OMI_ELE_IND_XYZ_REF zuweisen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_ele"\n'//&
               'Typ-Komponente = "ind_fac"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'assoziiert     : xyz=<as-xyz>, fac=<as-fac>\n'//&
               'Inhalt o.k.    : xyz=<ok-xyz>, fac=<ok-fac>\n'//&
               'erforderlich   : xyz=<req-xyz>, fac=<req-fac>\n'//&
               'MinimalWerte   : xyz=<mn-xyz>, fac=<mn-fac>\n'//&
               'MaximalWerte   : xyz=<mx-xyz>, fac=<mx-fac>\n'//&
               '--> korrekte Daten mit SET_OMI_ELE_IND_XYZ_REF zuweisen, oder\n'//&
               '--> keine Daten mit SET_OMI_ELE_IND_XYZ_REF zuweisen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_ele" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_ele" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_ele" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ele"\n'//&
               'Typ-Komponente = "id"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ele" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ele"\n'//&
               'Typ-Komponente = "description"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ele" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ele"\n'//&
               'Typ-Komponente = "elementtype"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ele" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ele"\n'//&
               'Typ-Komponente = "xyz"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ele" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ele"\n'//&
               'Typ-Komponente = "ind_xyz"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ele" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_ele"\n'//&
               'Typ-Komponente = "ind_fac"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_ele" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_ele"\n'//&
               '--> Code in Modul "b_omi_ele" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -9000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: interne GET-Methoden\n'//&
               'Elementtyp kann nicht automatisch ermittelt werden\n'//&
               'Id Indexlistenobjekt = <id>\n'//&
               'Beschreibung         = <descr>\n'//&
               'na, nd, ns           = <na>, <nd>, <ns>\n'//&
               '--> Code in Modul "b_omi_ele" / Daten pruefen' )
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
  END SUBROUTINE init_omi_ele_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_ele_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_ele_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ele_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_omi_ele_id' ! 
    !
    ok = ( LEN_TRIM(this%id) > 0 .AND. &
           this%id(1:LEN(c_undef_omi_ele_char)) /= c_undef_omi_ele_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       CALL setup_error_act ( '<aktuell>', TRIM(this%id) )
    END IF
    !
  END FUNCTION ok_omi_ele_id
  !
  !! Pr&uuml;fe, ob die Komponente "description" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ele_description ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_ele_description' ! 
    !
    ok = ( LEN_TRIM(this%description) > 0 .AND. &
           this%description(1:LEN(c_undef_omi_ele_char)) /= c_undef_omi_ele_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       CALL setup_error_act ( '<aktuell>', TRIM(this%description) )
    END IF
    !
  END FUNCTION ok_omi_ele_description
  !
  !! Pr&uuml;fe, ob die Komponente "elementtype" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ele_elementtype ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_ele_elementtype' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ANY( this%elementtype == c_omi_ele_code(:) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') this%elementtype ; CALL setup_error_act ( '<aktuell>', ch )
    END IF
    !
  END FUNCTION ok_omi_ele_elementtype
  ! 
  !! Pr&uuml;fe, ob die Komponente "xyz" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ele_xyz ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL                        :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_omi_ele_xyz' ! 
    !! Hilfsvariable
    CHARACTER (LEN=1) :: ch      ! 
    LOGICAL           :: l_ok(3) ! 
    !
    IF ( this%elementtype /= 0 ) THEN ! geo-referenzierten Daten
       l_ok(:) = .false.
       l_ok(1) = MERGE( .true., .false., ASSOCIATED( this%xyz ) )
       IF ( l_ok(1) ) l_ok(2) = ok_omi_xyz( this%xyz )
       IF ( l_ok(2) ) THEN
          IF ( INDEX(c_omi_ele_typ(this%elementtype+1),'XYZ') > 0 ) THEN ! 3d data
             l_ok(3) = is_omi_xyz_three_dimensional ( this%xyz )
          ELSE
             l_ok(3) = is_omi_xyz_two_dimensional   ( this%xyz )
          END IF
       END IF
    ELSE
       l_ok(:) = .true.
    END IF
    !
    ok = ALL( l_ok(:) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       CALL setup_error_act ( '<ele-typ>', TRIM(c_omi_ele_typ(this%elementtype+1)) )
       WRITE(ch,'(L1)') l_ok(1) ; CALL setup_error_act ( '<associated>', ch )
       WRITE(ch,'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-contents>', ch )
       WRITE(ch,'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-dimension>', ch )
    END IF
    !
  END FUNCTION ok_omi_ele_xyz
  !
  !! Pr&uuml;fe, ob die Komponente "ind_xyz" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ele_ind_xyz ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_omi_ele_ind_xyz' ! 
    !! Hilfsvariable
    CHARACTER (LEN=1) :: ch      ! 
    LOGICAL           :: l_ok(3) ! 
    !
    IF ( this%elementtype /= 0 ) THEN ! geo-referenzierten Daten
       l_ok(:) = .false.
       l_ok(1) = MERGE( .true., .false., ASSOCIATED( this%ind_xyz ) )
       IF ( l_ok(1) ) THEN
          IF ( ASSOCIATED( this%xyz ) ) THEN 
             l_ok(2) = ok_omi_ind ( this%ind_xyz, this%xyz )
             l_ok(3) = l_ok(2)
          ELSE
             l_ok(2) = ok_omi_ind( this%ind_xyz )
          END IF
       END IF
    ELSE
       l_ok(:) = .true.
    END IF
    !
    ok = ALL( l_ok(:) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(L1)') l_ok(1) ; CALL setup_error_act ( '<associated>', ch )
       WRITE(ch,'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-contents>', ch )
       WRITE(ch,'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-consistent>', ch )
    END IF
    !
  END FUNCTION ok_omi_ele_ind_xyz
  !
  !! Pr&uuml;fe, ob die Komponente "ind_fac" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_ele_ind_fac ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_omi_ele_ind_fac' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch                     ! 
    LOGICAL            :: as(2), l_ok(2), req(2) ! 
    INTEGER            :: mn(2), mx(2)           ! 
    INTEGER  , POINTER :: p_stru_list(:)         ! 
    !
    IF ( this%elementtype == 9 ) THEN
       req(:) = .true.
       mn(:)  = -1
       mx(:)  = -1
       as(1)  = ASSOCIATED( this%ind_xyz )
       as(2)  = ASSOCIATED( this%ind_fac )
       IF ( as(1) ) THEN
          mn(1)   = 1 
          mx(1)   = get_omi_ind_stru_count( this%ind_xyz )
          l_ok(1) = ok_omi_ind ( this%ind_xyz )
       END IF
       IF ( as(2) ) THEN
          p_stru_list => get_omi_ind_stru_list_ref ( this%ind_fac )
          IF ( ASSOCIATED( p_stru_list ) ) THEN
             mn(2)   = MINVAL( p_stru_list )
             mx(2)   = MAXVAL( p_stru_list )
             l_ok(2) = ok_omi_ind ( this%ind_fac )
          END IF
          NULLIFY ( p_stru_list )
       END IF
       IF ( this%elementtype == 9 ) THEN ! Polyhedron
          ok = ( ALL(as) .AND. ALL(mn>0) .AND. ALL(mx>0) .AND. ALL(mx>=mn) .AND. &
                 ALL(mn==mn(1)) .AND. ALL(mx==mx(1)) .AND. ALL(l_ok)             )
       ELSE
          req(2) = .false. 
          ok     = ( .NOT. as(2) )
       END IF
    ELSE
       ok = .true.
    END IF
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') as(1)   ; CALL setup_error_act ( '<as-xyz>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') as(2)   ; CALL setup_error_act ( '<as-fac>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-xyz>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-fac>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') req(1)  ; CALL setup_error_act ( '<req-xyz>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') req(2)  ; CALL setup_error_act ( '<req-fac>', ch(1:1) )
       WRITE(ch,'(I10)') mn(1)       ; CALL setup_error_act ( '<mn-xyz>', ch )
       WRITE(ch,'(I10)') mn(2)       ; CALL setup_error_act ( '<mn-fac>', ch )
       WRITE(ch,'(I10)') mx(1)       ; CALL setup_error_act ( '<mx-xyz>', ch )
       WRITE(ch,'(I10)') mx(2)       ; CALL setup_error_act ( '<mx-fac>', ch )
    END IF
    !
  END FUNCTION ok_omi_ele_ind_fac
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_id ( this )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_omi_ele_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%id)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente id  - - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ele_id
  !
  !! Drucke den Inhalt der Komponente "description" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_description ( this )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='print_omi_ele_description' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%description)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente description - - - - - - - - - - - - - ',/&
           '# aktuell = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ele_description
  !
  !! Drucke den Inhalt der Komponente "elementtype" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_elementtype ( this )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='print_omi_ele_elementtype' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_omi_ele_elementtype( this ) ) THEN
       !
       WRITE &
            ( UNIT    = prn_lun,  &
              FMT     = 8000,     & 
              IOSTAT  = stat ) this%elementtype, TRIM(c_omi_ele_typ(this%elementtype+1))
       !
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
       !
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente elementtype - - - - - - - - - - - - - ',/&
           '# aktuell = ',I10,' [',A,']',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_ele_elementtype
  !
  !! Drucke den Inhalt der Komponente "xyz" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_xyz ( this )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_omi_ele_xyz' ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       CALL print_omi_xyz ( this%xyz )
       IF ( any_error( ) ) THEN
          CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
    END IF
    !
  END SUBROUTINE print_omi_ele_xyz
  !
  !! Drucke den Inhalt der Komponente "ind_xyz" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_ind_xyz ( this )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='print_omi_ele_ind_xyz' ! 
    !
    IF ( ASSOCIATED( this%ind_xyz ) ) THEN
       CALL print_omi_ind ( this%ind_xyz )
       IF ( any_error( ) ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
    END IF
    !
  END SUBROUTINE print_omi_ele_ind_xyz
  !
  !! Drucke den Inhalt der Komponente "ind_fac" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_ele_ind_fac ( this )
    !! Datenobjekt
    TYPE (t_omi_ele) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='print_omi_ele_ind_fac' ! 
    !
    IF ( ASSOCIATED( this%ind_fac ) ) THEN
       CALL print_omi_ind ( this%ind_fac )
       IF ( any_error( ) ) THEN
          CALL setup_error_act ( all_errors(:), 7070, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
    END IF
    !
  END SUBROUTINE print_omi_ele_ind_fac
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "xyz" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ele_xyz ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok    ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this1%xyz )
    as(2) = ASSOCIATED( this2%xyz )
    !
    IF      ( ALL( as(:) )       ) THEN
       ok = eq_omi_xyz( this1%xyz, this2%xyz )
    ELSE IF ( ALL( .NOT. as(:) ) ) THEN
       ok = .true.
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION eq_omi_ele_xyz 
  !
  !! pr&uuml;fe Komponente "ind_xyz" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ele_ind_xyz ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this1%ind_xyz )
    as(2) = ASSOCIATED( this2%ind_xyz )
    !
    IF      ( ALL( as(:) )       ) THEN
       ok = eq_omi_ind( this1%ind_xyz, this2%ind_xyz )
    ELSE IF ( ALL( .NOT. as(:) ) ) THEN
       ok = .true.
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION eq_omi_ele_ind_xyz 
  !
  !! pr&uuml;fe Komponente "ind_fac" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_ele_ind_fac ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_ele) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this1%ind_fac )
    as(2) = ASSOCIATED( this2%ind_fac )
    !
    IF      ( ALL( as(:) )       ) THEN
       ok = eq_omi_ind( this1%ind_fac, this2%ind_fac )
    ELSE IF ( ALL( .NOT. as(:) ) ) THEN
       ok = .true.
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION eq_omi_ele_ind_fac 
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
  ! >>> PRIVATE-GET-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! automatisches Ermitteln des "elementtype" <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_elementtype_auto ( xyz, ind_xyz ) &
       RESULT( res )
    !! Koordinaten-Objekt
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz     ! 
    !! Verweislisten-Objekt
    TYPE (t_omi_ind) , INTENT(IN) :: ind_xyz ! 
    !! Ergebnis "elementtype" oder "undefiniert", falls nicht ermittelbar
    INTEGER :: res ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=20) , PARAMETER :: c_upname='get_elementtype_auto' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=c_len_omi_xyz_id) :: id ! 
    CHARACTER (LEN=5)                :: ch ! 
    INTEGER :: i, na, nd, nl, ns, ia ! 
    !
    res = c_undef_omi_ele_int
    nd  = c_undef_omi_ele_int
    na  = c_undef_omi_ele_int
    ns  = c_undef_omi_ele_int
    nd  = MERGE( 1, nd, is_omi_xyz_two_dimensional  ( xyz ) )
    nd  = MERGE( 2, nd, is_omi_xyz_three_dimensional( xyz ) )
    id  = get_omi_ind_id ( ind_xyz )
    IF      ( get_omi_ind_stru_len_min( ind_xyz ) == 1 .AND. get_omi_ind_stru_len_max( ind_xyz ) == 1 ) THEN
       ns = 1
    ELSE IF ( get_omi_ind_stru_len_min( ind_xyz ) == 2 .AND. get_omi_ind_stru_len_max( ind_xyz ) == 2 ) THEN
       ns = 2
    ELSE IF ( get_omi_ind_stru_len_min( ind_xyz ) >= 2 .AND. get_omi_ind_stru_len_max( ind_xyz ) >= 3 ) THEN
       ns = 3
    END IF
    nl  = MERGE( INDEX(id,'/')-1, LEN_TRIM(id), INDEX(id,'/') > 0 )
    DO i=1,SIZE(c_omi_ele_auto)
       IF ( na /= c_undef_omi_ele_int ) EXIT
       IF ( nl == LEN_TRIM(c_omi_ele_auto(i)) ) THEN
          IF ( id(1:nl) == c_omi_ele_auto(i)(1:nl) ) THEN
             na = i
          END IF
       END IF
    END DO
    DO i=1,SIZE(c_omi_ele_auto)
       IF ( na /= c_undef_omi_ele_int ) EXIT
       IF ( nl == LEN_TRIM(c_omi_ele_auto(i)) ) THEN
          ia = INDEX( c_omi_ele_auto(i), '?' ) 
          IF ( ia > 0 ) THEN
             IF ( id(1:ia-1) == c_omi_ele_auto(i)(1:ia-1) ) THEN
                na = i
             END IF
          END IF
       END IF
    END DO
    IF ( nd /= c_undef_omi_ele_int .AND. na /= c_undef_omi_ele_int .AND. ns /= c_undef_omi_ele_int ) THEN
       res = c_omi_ele_auto_code(na,nd,ns)
    ELSE
       CALL setup_error_act ( all_errors(:), -9000, c_upname, c_modname )
       CALL setup_error_act ( '<id>', id )
       CALL setup_error_act ( '<descr>', TRIM(get_omi_ind_description(ind_xyz)) )
       WRITE(ch,'(I5)') na ; CALL setup_error_act ( '<na>', ch )
       WRITE(ch,'(I5)') nd ; CALL setup_error_act ( '<nd>', ch )
       WRITE(ch,'(I5)') ns ; CALL setup_error_act ( '<ns>', ch )
    END IF
    !
  END FUNCTION get_elementtype_auto
  !
  !! automatisches Ermitteln der "description" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_description_auto ( xyz, ind_xyz, name ) &
       RESULT( res )
    !! Koordinaten-Objekt
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz     ! 
    !! Verweislisten-Objekt
    TYPE (t_omi_ind) , INTENT(IN) :: ind_xyz ! 
    !! erg&auml;nzender Name
    CHARACTER (LEN=*) , INTENT(IN) :: name   ! 
    !! Ergebnis "description" oder "undefiniert", falls nicht ermittelbar
    CHARACTER (LEN=c_len_omi_ele_description) :: res ! 
    !! Hilfsvariable
    INTEGER :: n, l ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_omi_ele_char
    n   = get_elementtype_auto ( xyz, ind_xyz )
    IF ( n /= c_undef_omi_ele_int ) THEN
       res = get_omi_ind_description( ind_xyz )
       l   = LEN_TRIM(res) + 2
       IF ( LEN(res)-l+1 >= LEN_TRIM(name)+2 .AND. LEN_TRIM(name) > 0 ) THEN
          res(l:) = '['//TRIM(name)//']'
       END IF
    END IF
    !
  END FUNCTION get_description_auto
  !
  !! automatisches Ermitteln der "id" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_id_auto ( xyz, ind_xyz, name ) &
       RESULT( res )
    !! Koordinaten-Objekt
    TYPE (t_omi_xyz) , INTENT(IN) :: xyz     ! 
    !! Verweislisten-Objekt
    TYPE (t_omi_ind) , INTENT(IN) :: ind_xyz ! 
    !! erg&auml;nzender Name
    CHARACTER (LEN=*) , INTENT(IN) :: name   ! 
    !! Ergebnis "id" oder "undefiniert", falls nicht ermittelbar
    CHARACTER (LEN=c_len_omi_ele_id) :: res ! 
    !! Hilfsvariable
    LOGICAL :: do_short ! 
    INTEGER :: n, l     ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_omi_ele_char
    n   = get_elementtype_auto ( xyz, ind_xyz )
    IF ( n /= c_undef_omi_ele_int ) THEN
       res      = get_omi_ind_id ( ind_xyz )
       do_short = .true.
       DO l=1,SIZE(c_omi_ele_short)
          IF ( .NOT. do_short ) EXIT
          do_short = ( INDEX( res, '/'//TRIM(c_omi_ele_short(l)) ) <= 0 )
       END DO
       IF ( do_short) THEN
          l   = LEN_TRIM(res) + 1
          res(l:) = '/'//c_omi_ele_short(n+1)
       END IF
       l   = LEN_TRIM(res) + 1
       IF ( LEN_TRIM(name) > 0 ) res(l:) = '['//TRIM(name)//']'
    END IF
    !
  END FUNCTION get_id_auto
  !
END MODULE b_omi_ele
! TailOfBaseModule --------------------------------------------------------
