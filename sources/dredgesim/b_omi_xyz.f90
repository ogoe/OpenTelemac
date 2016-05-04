! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden (teilweise) analog zu der OpenMI-Schnittstelle <EM>IElementSet</EM></h2>
!! @author G. Lang
!! @version 2.2 vom 07/29/05, Quellcode: mod_b_omi_xyz.f90
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
!  01.01 : 2005-02-12 : G. Lang : Erstversion
!  01.02 : 2005-02-22 : G. Lang : FM korrigiert
!  01.03 : 2005-02-25 : G. Lang : verbesserte Abfragen in einigen internen EQ-Funktionen
!  01.04 : 2005-03-02 : G. Lang : Kopierfunktion "copy_omi_xyz"
!  01.05 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  01.06 : 2004-05-09 : G. Lang : Erweiterungen fuer io_dataset-Einbindung
!  01.07 : 2004-05-09 : G. Lang : kleine Anpassungen in Zushg. mit "version"
!  01.08 : 2005-05-10 : G. Lang : Ergaenzungen wegen Komponente "bini"
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!  02.02 : 2005-07-29 : G. Lang : verschiedene neue Funktionen und Modifikationen, insbesondere fuer Layer
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden (teilweise) analog OpenMI-Interface <EM>IElementSet</EM>.
!! Implementiert einen Typ zur Aufnahme der Koordinaten der Datenpunkte sowie
!! weitere Hilfsgr&ouml;&szlig;en. Es werden verschiedene Methoden im Umfeld 
!! der Verwendung der Koordinaten der Datenpunkte bereitgestellt. Typ und
!! Methoden erleichtern den Austausch der zischen verschiedenen OpenMI-konformen
!! Komponenten.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_xyz";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_xyz";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_xyz";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_xyz";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_xyz";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_xyz";
!!    <LI> Erzeugen der dynamischen Komponenten aus verschiedenen elementaren Gr&ouml;&szlig;en;
!!    <LI> Aktualisieren der dynamischen Komponenten;
!!    <LI> Interpolieren von Daten;
!!    <LI> Abfragen extremer Koordinatenwerte.
!! </OL>
!!                                                         
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_omi_xyz 
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> id          : kurzer Identifikationsbezeichner
!!     <LI> description : ausf&uuml;hrliche verbale Beschreibung
!!     <LI> refsystem   : Koordinatensystem, in welchem die Datenpunkte
!!                        definiert wurden
!!     <LI> ztype       : Typ f&uuml;r Bestimmung der z-Koordinaten: <BR>
!!                        0 = keine Vertikalstruktur <BR>
!!                        1 = Schichten in konstanter Tiefe <BR>
!!                        2 = Schichten in konstanter Sigma-Tiefe
!!     <LI> x(:)        : Feld mit x-Koordinaten der Datenpunkte
!!     <LI> y(:)        : Feld mit y-Koordinaten der Datenpunkte
!!     <LI> z(:)        : Feld mit z-Koordinaten der Datenpunkte
!!     <LI> valid(:)    : Feld zur Kennzeichnung ung&uuml;tiger Datenpunkte; 
!!     <LI> l3d(:,:)    : Feld mit Zuordnung der Datenpunkte (x,y,z) zur 
!!                        Speicherposition in einem ein-dimensionalen Datenvektor;
!!     <LI> l2d(:)      : Feld mit Zuordnung der Datenpunkte (x,y) zur 
!!                        Speicherposition in einem ein-dimensionalen Datenvektor
!!     <LI> layers(:)   : (optional) Schichtgrenzen, die bei verschiedenen Prozessen 
!!                        ben&ouml;tigt werden
!!     <LI> modif(:)    : (optional) zeigt an, ob Koordinaten bei einem Update-Schritt
!!                        modifiziert wurden oder nicht <BR>
!!     <LI> hlimit      : Grenzwert f&uuml;r geometrische Bedeckung von Punkten <BR>
!!     <LI> ctype       : Lage der z-Koordinaten bez&uuml;glich der Schichtgrenzen <BR>
!!                        0 = keine Angabe <BR>
!!                        1 = in der Mitte zwischen den Schichtgrenzen <BR>
!!                        2 = auf den Schichtgrenzen <BR>
!!     <LI> zmax(:)     : Feld mit tiefster zul&auml;ssiger z-Koordinate bei z-Schichten 
!!                        (kann nicht von Au&szlig;en ver&auml;ndert werden) <BR>
!!     <LI> bini(:)     : Feld mit anf&auml;nglicher Lage des Bodens
!!                        (kann nicht von Au&szlig;en ver&auml;ndert werden) <BR>
!!     <LI> version     : Versionsbezeichnung eines <EM>KoordinatenSets</EM>, falls 
!!                        dieses zeitabh&auml;ngig ist, &auml;ndert sich die 
!!                        Versionsbezeichnung im Laufe der Zeit <BR>
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
!!    <LI> Initialisieren des Moduls b_omi_xyz mit INIT-Methode;
!!    <LI> Verwenden verschiedener SETUP-Methoden: es m&uuml;ssen folgende
!!         Komponenten definiert werden:
!!         <OL>
!!            <LI> (req) <EM>id</EM> (SET_OMI_XYZ_ID), 
!!            <LI> (req) <EM>description</EM> (SET_OMI_XYZ_DESCRIPTION), 
!!            <LI> (req) <EM>refsystem</EM> (SET_OMI_XYZ_REFSYSTEM), 
!!            <LI> (req) <EM>ztype</EM> (SET_OMI_XYZ_ZTYPE), 
!!            <LI> (req) <EM>ctype</EM> (SET_OMI_XYZ_CTYPE), 
!!            <LI> (req) <EM>hlimit</EM> (SET_OMI_XYZ_HLIMIT), und 
!!            <LI> (opt) <EM>layers</EM> (SET_OMI_XYZ_LAYERS).
!!         </OL>
!!    <LI> Erzeugen der dynamischen Komponenten:
!!         <OL>
!!            <LI> CREATE_OMI_XYZ_NO_LAYERS, falls keine Schichtinformationen
!!                 vorhanden sind, oder
!!            <LI> CREATE_OMI_XYZ_WITH_LAYERS, falls Schichtinformationen 
!!                 vorhanden sind.
!!         </OL>
!!    <LI> Aktualisieren der dynamischen Komponenten mit neuen Wasserspiegel-
!!         und Sohllagen (UPDATE_OMI_XYZ).
!!    <LI> Datenvektor auf Datenpunkte umrechnen (GET_OMI_XYZ_DATA);
!!    <LI> Verwenden weiterer Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_xyz mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_XYZ_ALL_ERRORS.
!!                                                                    <BR>
!! <HR>
!
MODULE b_omi_xyz
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
  USE b_omi_space, ONLY : &
       !   Typdefinitionen
       t_omi_space,             &
       !   Parameter 
       !   Variablen mit INTENT(IN)
       !   Variablen mit INTENT(INOUT)
       !   Variablen mit INTENT(OUT)
       !   Routinen / Interfaces
       init_omi_space,          &
       clear_omi_space,         &
       setup_omi_space_prn_lun, &
       setup_omi_space_trc_lun, &
       new_omi_space,           &
       kill_omi_space,          &
       ok_omi_space,            &
       get_omi_space_id,        &
       print_omi_space,         &
       has_omi_space_altitude,  &
       has_omi_space_depth,     &
       !   Operatoren
       eq_omi_space
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
  ! [C.0] Konstantwerte f&uuml;r die Typ-Definition (Parameter)
  !! max. Anzahl der Zeichen in der Komponente <EM>id</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_xyz_id=40          ! 
  !! max. Anzahl der Zeichen in der Komponente <EM>description</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_xyz_description=80 ! 
  !! max. Anzahl der Zeichen in dem zusammengesetzten Namen einer Schicht
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_xyz_layer_name=35  ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! id          : kurzer Identifikationsbezeichner <BR>
  !! description : ausf&uuml;hrliche verbale Beschreibung <BR>
  !! refsystem   : Koordinatensystem, in welchem die Koordinaten definiert wurden <BR>
  !! ztype       : Typ f&uuml;r Bestimmung der z-Koordinaten: <BR>
  !!               0 = keine Vertikalstruktur <BR>
  !!               1 = Schichten in konstanter Tiefe <BR>
  !!               2 = Schichten in konstanter Sigma-Tiefe <BR>
  !! x(:)        : Feld mit x-Koordinaten der Datenpunkte <BR>
  !! y(:)        : Feld mit y-Koordinaten der Datenpunkte <BR>
  !! z(:)        : Feld mit z-Koordinaten der Datenpunkte <BR>
  !! valid(:)    : Feld zur Kennzeichnung ung&uuml;tiger Datenpunkte; <BR>
  !! l3d(:,:)    : Feld mit Zuordnung der Datenpunkte (x,y,z) zur Speicherposition 
  !!               in einem ein-dimensionalen Datenvektor; <BR>
  !! l2d(:)      : Feld mit Zuordnung der Datenpunkte (x,y) zur Speicherposition 
  !!               in einem ein-dimensionalen Datenvektor; <BR>
  !! layers(:)   : (optional) Schichtgrenzen, die bei verschiedenen Prozessen ben&ouml;tigt 
  !!               werden <BR>
  !! modif(:)    : (optional) zeigt an, ob Koordinaten bei einem Update-Schritt
  !!               modifiziert wurden oder nicht <BR>
  !! hlimit      : Grenzwert f&uuml;r geometrische Bedeckung von Punkten <BR>
  !! ctype       : Lage der z-Koordinaten bez&uuml;glich der Schichtgrenzen <BR>
  !!               0 = keine Angabe <BR>
  !!               1 = in der Mitte zwischen den Schichtgrenzen <BR>
  !! zmax(:)     : Feld mit tiefster zul&auml;ssiger z-Koordinate bei z-Schichten <BR>
  !!               (kann nicht von Au&szlig;en ver&auml;ndert werden) <BR>
  !! bini(:)     : Feld mit anf&auml;nglicher Lage des Bodens <BR>
  !!               (kann nicht von Au&szlig;en ver&auml;ndert werden) <BR>
  !! version     : Versionsbezeichnung eines <EM>KoordinatenSets</EM>, falls 
  !!               dieses zeitabh&auml;ngig ist, &auml;ndert sich die 
  !!               Versionsbezeichnung im Laufe der Zeit <BR>
  TYPE , PUBLIC :: t_omi_xyz
     PRIVATE
     CHARACTER (LEN=c_len_omi_xyz_id)          :: id          ! 
     CHARACTER (LEN=c_len_omi_xyz_description) :: description ! 
     TYPE (t_omi_space)                        :: refsystem   ! 
     INTEGER                                   :: ztype       ! 
     REAL (KIND=Double)              , POINTER :: x(:)        ! 
     REAL (KIND=Double)              , POINTER :: y(:)        ! 
     REAL (KIND=Double)              , POINTER :: z(:)        ! 
     LOGICAL                         , POINTER :: valid(:)    ! 
     INTEGER                         , POINTER :: l3d(:,:)    ! 
     INTEGER                         , POINTER :: l2d(:)      ! 
     REAL (KIND=Double)              , POINTER :: layers(:)   ! 
     LOGICAL                         , POINTER :: modif(:)    ! 
     REAL (KIND=Double)                        :: hlimit      ! 
     INTEGER                                   :: ctype       ! 
     REAL (KIND=Double)              , POINTER :: zmax(:)     ! 
     REAL (KIND=Double)              , POINTER :: bini(:)     ! 
     INTEGER                                   :: version     ! 
  END TYPE t_omi_xyz
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Undefined-Wert f&uuml;r INTEGER-Komponenten
  INTEGER            , PUBLIC , PARAMETER :: c_undef_omi_xyz_int=-999              ! 
  !! Undefined-Wert f&uuml;r DOUBLE-Komponenten
  REAL (KIND=Double) , PUBLIC , PARAMETER :: c_undef_omi_xyz_double=1.0E+31_Double ! 
  !! Undefined-Wert f&uuml;r CHARACTER-Komponenten
  CHARACTER (LEN=9)  , PUBLIC , PARAMETER :: c_undef_omi_xyz_char='undefined'      ! 
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
  INTERFACE init_omi_xyz
     MODULE PROCEDURE init_omi_xyz_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_xyz
     MODULE PROCEDURE clear_omi_xyz_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_xyz_prn_lun
     MODULE PROCEDURE setup_omi_xyz_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_xyz_trc_lun
     MODULE PROCEDURE setup_omi_xyz_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_xyz"; NULLIFY f&uuml;r dynamische 
  !! Komponenten-Felder und Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE new_omi_xyz
     MODULE PROCEDURE new_omi_xyz_0  ! 
     MODULE PROCEDURE new_omi_xyz_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_xyz"; ggf. De-Allokieren von 
  !! Memory und teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE kill_omi_xyz
     MODULE PROCEDURE kill_omi_xyz_0 ! 
     MODULE PROCEDURE kill_omi_xyz_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_xyz" auf G&uuml;ltigkeit: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE ok_omi_xyz
     MODULE PROCEDURE ok_omi_xyz_0 !
     MODULE PROCEDURE ok_omi_xyz_1 !
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_xyz": <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_xyz" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_xyz
     MODULE PROCEDURE print_omi_xyz_0 ! 
     MODULE PROCEDURE print_omi_xyz_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_xyz_static
     MODULE PROCEDURE print_omi_xyz_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_xyz_all_errors
     MODULE PROCEDURE print_omi_xyz_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "id" in "t_omi_xyz" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_id
     MODULE PROCEDURE set_omi_xyz_id_0_0 ! 
     MODULE PROCEDURE set_omi_xyz_id_1_0 ! 
  END INTERFACE
  !! Setze Komponente "description" in "t_omi_xyz" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_description
     MODULE PROCEDURE set_omi_xyz_description_0_0 ! 
     MODULE PROCEDURE set_omi_xyz_description_1_0 ! 
  END INTERFACE
  !! Setze Komponente "refsystem" in "t_omi_xyz" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_refsystem
     MODULE PROCEDURE set_omi_xyz_refsystem_0_0 !
     MODULE PROCEDURE set_omi_xyz_refsystem_1_0 !
  END INTERFACE
  !! Setze Komponente "ztype" in "t_omi_xyz" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_ztype
     MODULE PROCEDURE set_omi_xyz_ztype_0_0 ! 
     MODULE PROCEDURE set_omi_xyz_ztype_1_0 ! 
  END INTERFACE
  !! Setze Komponente "x(:)" in "t_omi_xyz" auf Benutzerwert; 
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_x
     MODULE PROCEDURE set_omi_xyz_x_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_x_1_1 ! 
  END INTERFACE
  !! Setze Komponente "y(:)" in "t_omi_xyz" auf Benutzerwert; 
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_y
     MODULE PROCEDURE set_omi_xyz_y_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_y_1_1 ! 
  END INTERFACE
  !! Setze Komponente "z(:)" in "t_omi_xyz" auf Benutzerwert;
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_z
     MODULE PROCEDURE set_omi_xyz_z_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_z_1_1 ! 
  END INTERFACE
  !! Setze Komponente "valid(:)" in "t_omi_xyz" auf Benutzerwert; 
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_valid
     MODULE PROCEDURE set_omi_xyz_valid_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_valid_1_1 ! 
  END INTERFACE
  !! Setze Komponente "l3d(:,:)" in "t_omi_xyz" auf Benutzerwert;
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_l3d
     MODULE PROCEDURE set_omi_xyz_l3d_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_l3d_1_1 ! 
  END INTERFACE
  !! Setze Komponente "l2d(:)" in "t_omi_xyz" auf Benutzerwert;
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_l2d
     MODULE PROCEDURE set_omi_xyz_l2d_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_l2d_1_1 ! 
  END INTERFACE
  !! Setze Komponente "layers(:)" in "t_omi_xyz" auf Benutzerwert;
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_layers
     MODULE PROCEDURE set_omi_xyz_layers_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_layers_1_1 ! 
  END INTERFACE
  !! Setze Komponente "modif(:)" in "t_omi_xyz" auf Benutzerwert;
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_modif
     MODULE PROCEDURE set_omi_xyz_modif_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_modif_1_1 ! 
  END INTERFACE
  !! Setze Komponente "hlimit" in "t_omi_xyz" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_hlimit
     MODULE PROCEDURE set_omi_xyz_hlimit_0_0 ! 
     MODULE PROCEDURE set_omi_xyz_hlimit_1_0 ! 
  END INTERFACE
  !! Setze Komponente "ctype" in "t_omi_xyz" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_ctype
     MODULE PROCEDURE set_omi_xyz_ctype_0_0 ! 
     MODULE PROCEDURE set_omi_xyz_ctype_1_0 ! 
  END INTERFACE
  !! Setze Komponente "zmax(:)" in "t_omi_xyz" auf Benutzerwert;
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_zmax
     MODULE PROCEDURE set_omi_xyz_zmax_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_zmax_1_1 ! 
  END INTERFACE
  !! Setze Komponente "bini(:)" in "t_omi_xyz" auf Benutzerwert;
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten: <BR>
  !! a) ein Objekt (Skalar) mit Daten-Vektor <BR>
  !! b) viele Objekte (Vektor) mit Daten-Vektor <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_bini
     MODULE PROCEDURE set_omi_xyz_bini_0_1 ! 
     MODULE PROCEDURE set_omi_xyz_bini_1_1 ! 
  END INTERFACE
  !! Setze Komponente "version" in "t_omi_xyz" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_xyz_version
     MODULE PROCEDURE set_omi_xyz_version_0_0 ! 
     MODULE PROCEDURE set_omi_xyz_version_1_0 ! 
  END INTERFACE
  !
  !! Hole Komponente "id" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_id
     MODULE PROCEDURE get_omi_xyz_id_0_0 ! 
     MODULE PROCEDURE get_omi_xyz_id_1_0 ! 
  END INTERFACE
  !! Hole Komponente "description" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_description
     MODULE PROCEDURE get_omi_xyz_description_0_0 ! 
     MODULE PROCEDURE get_omi_xyz_description_1_0 ! 
  END INTERFACE
  !! Hole Komponente "refsystem" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_refsystem
     MODULE PROCEDURE get_omi_xyz_refsystem_0_0 ! 
     MODULE PROCEDURE get_omi_xyz_refsystem_1_0 ! 
  END INTERFACE
  !! Hole Komponente "ztype" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_ztype
     MODULE PROCEDURE get_omi_xyz_ztype_0_0 ! 
     MODULE PROCEDURE get_omi_xyz_ztype_1_0 ! 
  END INTERFACE
  !! Hole Komponente "x(:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_x_ref
     MODULE PROCEDURE get_omi_xyz_x_ref_0_1 ! 
  END INTERFACE
  !! Hole Komponente "y(:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_y_ref
     MODULE PROCEDURE get_omi_xyz_y_ref_0_1 !
  END INTERFACE
  !! Hole Komponente "z(:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_z_ref
     MODULE PROCEDURE get_omi_xyz_z_ref_0_1 !
  END INTERFACE
  !! Hole Komponente "bini(:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_bini_ref
     MODULE PROCEDURE get_omi_xyz_bini_ref_0_1 !
  END INTERFACE
  !! Hole Komponente "valid(:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_valid_ref
     MODULE PROCEDURE get_omi_xyz_valid_ref_0_1 ! 
  END INTERFACE
  !! Hole Komponente "l3d(:,:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_l3d_ref
     MODULE PROCEDURE get_omi_xyz_l3d_ref_0_1 ! 
  END INTERFACE
  !! Hole Komponente "l2d(:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_l2d_ref
     MODULE PROCEDURE get_omi_xyz_l2d_ref_0_1 !
  END INTERFACE
  !! Hole Komponente "layers(:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_layers_ref
     MODULE PROCEDURE get_omi_xyz_layers_ref_0_1 !
  END INTERFACE
  !! Hole Komponente "modif(:)" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_modif_ref
     MODULE PROCEDURE get_omi_xyz_modif_ref_0_1 !
  END INTERFACE
  !! Hole Komponente "hlimit" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_hlimit
     MODULE PROCEDURE get_omi_xyz_hlimit_0_0 ! 
     MODULE PROCEDURE get_omi_xyz_hlimit_1_0 ! 
  END INTERFACE
  !! Hole Komponente "ctype" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_ctype
     MODULE PROCEDURE get_omi_xyz_ctype_0_0 ! 
     MODULE PROCEDURE get_omi_xyz_ctype_1_0 ! 
  END INTERFACE
  !! Hole die auf die Positionen (x,y[,z]) interpolierten Daten: <BR>
  !! a) f&uuml;r ein Objekt (Skalar): <BR>
  !!    - REAL (KIND=Double) :: array(:) <BR> 
  !!    - INTEGER            :: array(:)
  INTERFACE get_omi_xyz_data
     MODULE PROCEDURE get_omi_xyz_data_d_1
     MODULE PROCEDURE get_omi_xyz_data_i_1
  END INTERFACE
  !! Hole Komponente "version" aus "t_omi_xyz": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_xyz_version
     MODULE PROCEDURE get_omi_xyz_version_0_0 ! 
     MODULE PROCEDURE get_omi_xyz_version_1_0 ! 
  END INTERFACE
  
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_xyz": <BR>
  !! a) f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! b) f&uuml;r viele Werte <EM>id(:)</EM> (Vektor)
  INTERFACE get_omi_xyz_idx
     MODULE PROCEDURE get_omi_xyz_idx_1_0
     MODULE PROCEDURE get_omi_xyz_idx_1_1
  END INTERFACE
  !! ermittle die westliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_west
     MODULE PROCEDURE get_omi_xyz_west_0
     MODULE PROCEDURE get_omi_xyz_west_1
  END INTERFACE
  !! ermittle die &ouml;stliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_east
     MODULE PROCEDURE get_omi_xyz_east_0
     MODULE PROCEDURE get_omi_xyz_east_1
  END INTERFACE
  !! ermittle die n&ouml;rdliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_north
     MODULE PROCEDURE get_omi_xyz_north_0
     MODULE PROCEDURE get_omi_xyz_north_1
  END INTERFACE
  !! ermittle die s&uuml;dliche Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_south
     MODULE PROCEDURE get_omi_xyz_south_0
     MODULE PROCEDURE get_omi_xyz_south_1
  END INTERFACE
  !! ermittle die obere Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_top
     MODULE PROCEDURE get_omi_xyz_top_0
     MODULE PROCEDURE get_omi_xyz_top_1
  END INTERFACE
  !! ermittle die unterste Rand-Koordinate <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_bottom
     MODULE PROCEDURE get_omi_xyz_bottom_0
     MODULE PROCEDURE get_omi_xyz_bottom_1
  END INTERFACE
  !! ermittle die Anzahl der Positionen <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_point_count
     MODULE PROCEDURE get_omi_xyz_point_count_0
     MODULE PROCEDURE get_omi_xyz_point_count_1
  END INTERFACE
  !! ermittle die Anzahl der zwei-dimensionalen Positionen <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_2d_point_count
     MODULE PROCEDURE get_omi_xyz_2d_point_count_0
     MODULE PROCEDURE get_omi_xyz_2d_point_count_1
  END INTERFACE
  !! ermittle die Anzahl der Schichten/Schichtgrenzen
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_xyz_layer_count
     MODULE PROCEDURE get_omi_xyz_layer_count_0
     MODULE PROCEDURE get_omi_xyz_layer_count_1
  END INTERFACE
  !! ermittle die Textbezeichnung einer bestimmten Schicht
  !! a) f&uuml;r ein Objekt (Skalar)
  INTERFACE get_omi_xyz_layer_name
     MODULE PROCEDURE get_omi_xyz_layer_name_0
  END INTERFACE
  !! ermittle die Anzahl der in einer bestimmten Schicht liegenden Positionen
  !! a) f&uuml;r ein Objekt (Skalar) und alle zwei-dimensionalen Positionen <BR>
  !! b) f&uuml;r ein Objekt (Skalar) und ausgew&auml;hlte zwei-dimensionale Positionen
  INTERFACE get_omi_xyz_layer_point_count
     MODULE PROCEDURE get_omi_xyz_layer_point_count_0
     MODULE PROCEDURE get_omi_xyz_layer_point_count_1
  END INTERFACE
  !! ermittle das Feld mit den Index-Positionen aller in einer bestimmten
  !! Schicht liegenden Punkte
  !! a) f&uuml;r ein Objekt (Skalar) und alle zwei-dimensionalen Positionen <BR>
  !! b) f&uuml;r ein Objekt (Skalar) und ausgew&auml;hlte zwei-dimensionale Positionen
  INTERFACE get_omi_xyz_layer_point_idx
     MODULE PROCEDURE get_omi_xyz_layer_point_idx_0
     MODULE PROCEDURE get_omi_xyz_layer_point_idx_1
  END INTERFACE
  !! ermittle eine Maske mit true/false, ob diese Punkte in einer bestimmten Schicht 
  !! vorhanden sind oder nicht <BR>
  !! b) f&uuml;r ein Objekt (Skalar) und ausgew&auml;hlte zwei-dimensionale Positionen
  INTERFACE get_omi_xyz_layer_2d_mask
     MODULE PROCEDURE get_omi_xyz_layer_2d_mask_1
  END INTERFACE
  !! ermittle die Anzahl der Positionen in S&auml;len eines Datenobjekts <BR>
  !! a) f&uuml;r eine S&auml;le <BR>
  !! b) f&uuml;r mehrere S&auml;len
  INTERFACE get_omi_xyz_column_point_count
     MODULE PROCEDURE get_omi_xyz_col_point_count_0_0
     MODULE PROCEDURE get_omi_xyz_col_point_count_0_1
  END INTERFACE
  !! ermittle das Feld mit den Index-Positionen aller in einer bestimmten
  !! S&auml;le liegenden Punkte eines Datenobjekts <BR>
  !! a) f&uuml;r eine S&auml;le <BR>
  !! b) f&uuml;r mehrere S&auml;len 
  INTERFACE get_omi_xyz_column_point_idx
     MODULE PROCEDURE get_omi_xyz_col_point_idx_0_0
     MODULE PROCEDURE get_omi_xyz_col_point_idx_0_1
  END INTERFACE
  !
  !! Erzeugen der dynamischen Komponenten "x(:)", "y(:)"[, "z(:)"], "l2d(:)", 
  !! "l3d(:,:)" und "valid(:)" eines Datenobjektes f&uuml;r vorgegebene
  !! Orte (x,y). Ist insbesondere f&uuml;r Strukturen n&uuml;tzlich, 
  !! die entweder keine Tiefeninformation oder keine Schichtinformationen
  !! ben&ouml;tigen: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! <EM>Anmerkung:</EM> die Komponenten "id", "description", 
  !! "refsystem", "ztype" und "hlimit" m&uuml;ssen hierf&uuml;r 
  !! vorab definiert worden sein.
  INTERFACE create_omi_xyz_no_layers
     MODULE PROCEDURE create_omi_xyz_no_layers_0
  END INTERFACE
  !! Erzeugen der dynamischen Komponenten "x(:)", "y(:)", "z(:)", "l2d(:)", 
  !! "l3d(:,:)", "valid(:)" und "modif(:)" eines Datenobjektes f&uuml;r 
  !! vorgegebene Orte (x,y). Die vertikalen Koordinaten werden in Abh&auml;ngigkeit
  !! vom Wert der Komponente "ctype" entweder in die Mitte zwischen zwei 
  !! Schichtgrenzen oder auf die Schichtgrenzen gelegt. Ist insbesondere f&uuml;r 
  !! Daten erforderlich, die Tiefeninformation ben&ouml;tigen: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! <EM>Anmerkung:</EM> die Komponenten "id", "description", 
  !! "refsystem", "ztype", "layers(:)" und "hlimit" m&uuml;ssen hierf&uuml;r 
  !! vorab definiert worden sein.
  INTERFACE create_omi_xyz_with_layers
     MODULE PROCEDURE create_omi_xyz_with_layers_0
  END INTERFACE
  !
  !! Aktualisieren schon vorhandener Koordinten
  INTERFACE update_omi_xyz
     MODULE PROCEDURE update_omi_xyz_0
  END INTERFACE
  !
  !! Pr&uuml;fe, ob die Gitterinformationen zwei-dimensional (x,y) sind: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE is_omi_xyz_two_dimensional
     MODULE PROCEDURE is_omi_xyz_two_dimensional_0
     MODULE PROCEDURE is_omi_xyz_two_dimensional_1
  END INTERFACE
  !! Pr&uuml;fe, ob die Gitterinformationen drei-dimensional (x,y,z) sind: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE is_omi_xyz_three_dimensional
     MODULE PROCEDURE is_omi_xyz_three_dimensional_0
     MODULE PROCEDURE is_omi_xyz_three_dimensional_1
  END INTERFACE
  !! Pr&uuml;fe, ob die Koordinaten (x,y[,z]) modifiziert wurden: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE is_omi_xyz_modified
     MODULE PROCEDURE is_omi_xyz_modified_0
     MODULE PROCEDURE is_omi_xyz_modified_1
  END INTERFACE
  !
  !! Pr&uuml;fe, ob f&uuml;r das Gitter Schichten bekannt sind: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE has_omi_xyz_layers
     MODULE PROCEDURE has_omi_xyz_layers_0
     MODULE PROCEDURE has_omi_xyz_layers_1
  END INTERFACE
  !! Pr&uuml;fe, ob das Gitter z-Schichten aufweist: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE has_omi_xyz_z_layers
     MODULE PROCEDURE has_omi_xyz_z_layers_0
     MODULE PROCEDURE has_omi_xyz_z_layers_1
  END INTERFACE
  !! Pr&uuml;fe, ob das Gitter sigma-Schichten aufweist: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE has_omi_xyz_sigma_layers
     MODULE PROCEDURE has_omi_xyz_sigma_layers_0
     MODULE PROCEDURE has_omi_xyz_sigma_layers_1
  END INTERFACE
  !
  !! Kopiere den Inhalt einer Variablen des Typs "t_omi_xyz" in
  !! ein anderes Objekt vom gleichen Typ <BR>
  !! a) ein Quell-Objekt wird in ein Ziel-Objekt kopiert <BR>
  !! a) mehrere Quell-Objekte werden auf mehrere Ziel-Objekte kopiert
  INTERFACE copy_omi_xyz
     MODULE PROCEDURE copy_omi_xyz_0_0
     MODULE PROCEDURE copy_omi_xyz_1_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_xyz" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_xyz
     MODULE PROCEDURE eq_omi_xyz_0_0  ! 
     MODULE PROCEDURE eq_omi_xyz_0_1  ! 
     MODULE PROCEDURE eq_omi_xyz_1_0  ! 
     MODULE PROCEDURE eq_omi_xyz_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_xyz" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_xyz
     MODULE PROCEDURE ne_omi_xyz_0_0  ! 
     MODULE PROCEDURE ne_omi_xyz_0_1  ! 
     MODULE PROCEDURE ne_omi_xyz_1_0  ! 
     MODULE PROCEDURE ne_omi_xyz_1_1  ! 
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
  PUBLIC :: init_omi_xyz
  PUBLIC :: clear_omi_xyz
  PUBLIC :: setup_omi_xyz_prn_lun
  PUBLIC :: setup_omi_xyz_trc_lun
  PUBLIC :: new_omi_xyz
  PUBLIC :: kill_omi_xyz
  PUBLIC :: ok_omi_xyz
  PUBLIC :: print_omi_xyz
  PUBLIC :: print_omi_xyz_static
  PUBLIC :: print_omi_xyz_all_errors
  PUBLIC :: set_omi_xyz_id
  PUBLIC :: set_omi_xyz_description
  PUBLIC :: set_omi_xyz_refsystem
  PUBLIC :: set_omi_xyz_ztype
  PUBLIC :: set_omi_xyz_hlimit
  PUBLIC :: set_omi_xyz_ctype
  !  PUBLIC :: set_omi_xyz_x     ! sicherheitshalber nicht
  !  PUBLIC :: set_omi_xyz_y     ! sicherheitshalber nicht
  !  PUBLIC :: set_omi_xyz_z     ! sicherheitshalber nicht
  !  PUBLIC :: set_omi_xyz_valid ! sicherheitshalber nicht
  !  PUBLIC :: set_omi_xyz_l3d   ! sicherheitshalber nicht
  !  PUBLIC :: set_omi_xyz_l2d   ! sicherheitshalber nicht
  PUBLIC :: set_omi_xyz_layers
  !  PUBLIC :: set_omi_xyz_modif ! sicherheitshalber nicht
  !  PUBLIC :: set_omi_xyz_zmax  ! sicherheitshalber nicht
  !  PUBLIC :: set_omi_xyz_bini  ! sicherheitshalber nicht
  PUBLIC :: set_omi_xyz_version
  PUBLIC :: get_omi_xyz_id
  PUBLIC :: get_omi_xyz_description
  PUBLIC :: get_omi_xyz_refsystem
  PUBLIC :: get_omi_xyz_ztype
  PUBLIC :: get_omi_xyz_hlimit
  PUBLIC :: get_omi_xyz_ctype
  PUBLIC :: get_omi_xyz_x_ref
  PUBLIC :: get_omi_xyz_y_ref
  PUBLIC :: get_omi_xyz_z_ref
  PUBLIC :: get_omi_xyz_bini_ref
  PUBLIC :: get_omi_xyz_valid_ref
  PUBLIC :: get_omi_xyz_l3d_ref
  PUBLIC :: get_omi_xyz_l2d_ref
  PUBLIC :: get_omi_xyz_layers_ref
  PUBLIC :: get_omi_xyz_modif_ref
  PUBLIC :: get_omi_xyz_version
  PUBLIC :: eq_omi_xyz
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: get_omi_xyz_idx
  PUBLIC :: get_omi_xyz_west
  PUBLIC :: get_omi_xyz_east
  PUBLIC :: get_omi_xyz_north
  PUBLIC :: get_omi_xyz_south
  PUBLIC :: get_omi_xyz_top
  PUBLIC :: get_omi_xyz_bottom
  PUBLIC :: get_omi_xyz_point_count 
  PUBLIC :: get_omi_xyz_2d_point_count 
  PUBLIC :: get_omi_xyz_layer_count 
  PUBLIC :: get_omi_xyz_layer_name
  PUBLIC :: get_omi_xyz_layer_point_count
  PUBLIC :: get_omi_xyz_layer_point_idx
  PUBLIC :: get_omi_xyz_layer_2d_mask
  PUBLIC :: get_omi_xyz_column_point_count
  PUBLIC :: get_omi_xyz_column_point_idx
  PUBLIC :: get_omi_xyz_data
  PUBLIC :: create_omi_xyz_no_layers
  PUBLIC :: create_omi_xyz_with_layers
  PUBLIC :: update_omi_xyz
  PUBLIC :: is_omi_xyz_two_dimensional
  PUBLIC :: is_omi_xyz_three_dimensional
  PUBLIC :: is_omi_xyz_modified
  PUBLIC :: has_omi_xyz_layers
  PUBLIC :: has_omi_xyz_z_layers
  PUBLIC :: has_omi_xyz_sigma_layers
  PUBLIC :: copy_omi_xyz
  PUBLIC :: ne_omi_xyz
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
  CHARACTER (LEN=09), PARAMETER :: c_modname      = 'b_omi_xyz' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_xyz
  INTEGER           , PARAMETER :: c_nofcomp      = 17               ! ggf. modifizieren
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
  !! Ermittle die Anzahl der Datenpunkte <BR>
  !! a) aus der Schichtstruktur und dem Boden <BR>
  !! b) aus den Informationen in der Komponente "l2d(:)"
  INTERFACE get_omi_xyz_nof_point
     MODULE PROCEDURE get_omi_xyz_nof_point_b
     MODULE PROCEDURE get_omi_xyz_nof_point_l
  END INTERFACE
  !! Ermittle eine Indikatormaske, welche Punkte innerhalb einer bestimmten Schicht liegen
  INTERFACE get_omi_xyz_layer_point_mask
     MODULE PROCEDURE get_omi_xyz_layer_point_mask_0
  END INTERFACE
  !! Ermittle eine Indikatormaske, welche Punkte innerhalb bestimmter S&auml;len liegen
  INTERFACE get_omi_xyz_column_point_mask
     MODULE PROCEDURE get_omi_xyz_column_point_mask_1
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
  SUBROUTINE init_omi_xyz_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_B
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='init_omi_xyz_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_xyz" version 2.2 of 07/29/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_omi_space ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_xyz_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_xyz_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_xyz_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='clear_omi_xyz_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_xyz_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_space ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_xyz_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_xyz_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_xyz_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_space_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_xyz_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_xyz_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_xyz_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_space_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_xyz_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_xyz_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_xyz_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       this%id          = REPEAT( ' ', LEN(this%id) )
       this%id          = c_undef_omi_xyz_char
       !
       this%description = REPEAT( ' ', LEN(this%description) )
       this%description = c_undef_omi_xyz_char
       !
       CALL new_omi_space( this%refsystem )
       !
       this%ztype       = c_undef_omi_xyz_int
       this%hlimit      = c_undef_omi_xyz_double
       this%ctype       = c_undef_omi_xyz_int
       this%version     = 0
       !
       NULLIFY ( this%x      )
       NULLIFY ( this%y      )
       NULLIFY ( this%z      )
       NULLIFY ( this%valid  )
       NULLIFY ( this%l3d    )
       NULLIFY ( this%l2d    )
       NULLIFY ( this%layers )
       NULLIFY ( this%modif  )
       NULLIFY ( this%zmax   )
       NULLIFY ( this%bini   )
       !
    END IF
    !
  END SUBROUTINE new_omi_xyz_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_xyz_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_xyz_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_xyz_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_xyz_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_xyz_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_xyz_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL kill_omi_space                 ( this%refsystem )
       IF ( no_error( ) ) CALL dealloc_omi_xyz_dyn_components ( this )
       IF ( no_error( ) ) CALL new_omi_xyz_0                  ( this )
    END IF
    !
  END SUBROUTINE kill_omi_xyz_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_xyz_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_xyz_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_xyz_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_xyz_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_xyz_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_omi_xyz_id         ( this )
       l_ok(2)  = ok_omi_xyz_description( this )
       l_ok(3)  = ok_omi_xyz_refsystem  ( this )
       l_ok(4)  = ok_omi_xyz_ztype      ( this )
       l_ok(5)  = ok_omi_xyz_x          ( this )
       l_ok(6)  = ok_omi_xyz_y          ( this )
       l_ok(7)  = ok_omi_xyz_z          ( this )
       l_ok(8)  = ok_omi_xyz_valid      ( this )
       l_ok(9)  = ok_omi_xyz_l3d        ( this )
       l_ok(10) = ok_omi_xyz_l2d        ( this )
       l_ok(11) = ok_omi_xyz_layers     ( this )
       l_ok(12) = ok_omi_xyz_modif      ( this )
       l_ok(13) = ok_omi_xyz_hlimit     ( this )
       l_ok(14) = ok_omi_xyz_ctype      ( this )
       l_ok(15) = ok_omi_xyz_zmax       ( this )
       l_ok(16) = ok_omi_xyz_bini       ( this )
       l_ok(17) = ok_omi_xyz_version    ( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_xyz_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_xyz_1' 
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
          ok(i) = ok_omi_xyz_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_xyz_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_xyz_0' 
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
       IF ( no_error( ) ) CALL print_omi_xyz_id         ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_description( this )
       IF ( no_error( ) ) CALL print_omi_xyz_refsystem  ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_ztype      ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_hlimit     ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_ctype      ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_version    ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_xyz        ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_valid      ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_l3d        ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_l2d        ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_layers     ( this )
       IF ( no_error( ) ) CALL print_omi_xyz_modif      ( this )
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
8000 FORMAT('# Beginn Objekt t_omi_xyz ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_xyz ------------------------------')
    !
  END SUBROUTINE print_omi_xyz_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_xyz_1' 
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
          IF ( no_error( ) ) CALL print_omi_xyz_0 ( this(i) )
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
  END SUBROUTINE print_omi_xyz_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_omi_xyz_static_d' 
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
           c_undef_omi_xyz_char, c_undef_omi_xyz_int, c_undef_omi_xyz_double
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_omi_xyz_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_xyz         ',/ &
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
  END SUBROUTINE print_omi_xyz_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=26), PARAMETER :: c_upname='print_omi_xyz_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_xyz_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_id_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id": kurzer Identifikationsbezeichner
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%id = REPEAT( ' ', LEN(this%id) )
    this%id = val(1:MIN(LEN(this%id),LEN_TRIM(val)))
    !
  END SUBROUTINE set_omi_xyz_id_0_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_id_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id": kurzer Identifikationsbezeichner
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_xyz_id_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_xyz_id_1_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_description_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "description": ausf&uuml;hrliche Beschreibung
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%description = REPEAT( ' ', LEN(this%description) )
    this%description = val(1:MIN(LEN(this%description),LEN_TRIM(val)))
    !
  END SUBROUTINE set_omi_xyz_description_0_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_description_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "description": ausf&uuml;hrliche Beschreibung
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_xyz_description_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_xyz_description_1_0
  !
  !! weise der Komponente "refsystem" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_refsystem_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "refsystem": Koordinatensystem der Datenpunkte
    TYPE (t_omi_space) , INTENT(IN)  :: val  ! 
    !
    this%refsystem = val
    !
  END SUBROUTINE set_omi_xyz_refsystem_0_0
  !
  !! weise der Komponente "refsystem" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_refsystem_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "refsystem": Koordinatensystem der Datenpunkte
    TYPE (t_omi_space) , INTENT(IN)  :: val     ! 
    !
    this(:)%refsystem = val
    !
  END SUBROUTINE set_omi_xyz_refsystem_1_0
  !
  !! weise der Komponente "ztype" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_ztype_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "ztype": Typ der z-Koordinaten [0,1,2] = [keine,z,sigma]
    INTEGER            , INTENT(IN)  :: val  ! 
    !
    this%ztype = val
    !
  END SUBROUTINE set_omi_xyz_ztype_0_0
  !
  !! weise der Komponente "ztype" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_ztype_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "ztype": Typ der z-Koordinaten [0,1,2] = [keine,z,sigma]
    INTEGER            , INTENT(IN)  :: val     ! 
    !
    this(:)%ztype = val
    !
  END SUBROUTINE set_omi_xyz_ztype_1_0
  !
  !! weise der dynamischen Komponente "x(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_x_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "x(:)": x-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='set_omi_xyz_x_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_x ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_x   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_x    ( this            )
       IF ( no_error( ) ) this%x(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_x_0_1
  !
  !! weise der dynamischen Komponente "x(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_x_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "x(:)": x-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='set_omi_xyz_x_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_x_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_x_1_1
  !
  !! weise der dynamischen Komponente "y(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_y_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "y(:)": y-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='set_omi_xyz_y_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_y ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_y   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_y    ( this            )
       IF ( no_error( ) ) this%y(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_y_0_1
  !
  !! weise der dynamischen Komponente "y(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_y_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "y(:)": y-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='set_omi_xyz_y_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_y_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_y_1_1
  !
  !! weise der (optionalen) dynamischen Komponente "z(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_z_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "z(:)": z-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='set_omi_xyz_z_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_z ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_z   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_z    ( this            )
       IF ( no_error( ) ) this%z(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_z_0_1
  !
  !! weise der (optionalen) dynamischen Komponente "z(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_z_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "z(:)": z-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='set_omi_xyz_z_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_z_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_z_1_1
  !
  !! weise der dynamischen Komponente "valid(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_valid_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "valid(:)": Kennzeichnung g&uuml;ltiger/ung&uuml;ltiger Datenpunkte <BR>
    !! valid(i) ist der Indikator f&uuml;r der i-ten Datenpunkt (x,y[,z])
    LOGICAL          , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='set_omi_xyz_valid_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_valid ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_valid   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_valid    ( this            )
       IF ( no_error( ) ) this%valid(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_valid_0_1
  !
  !! weise der dynamischen Komponente "valid(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_valid_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "valid(:)": Kennzeichnung g&uuml;ltiger/ung&uuml;ltiger Datenpunkte <BR>
    !! valid(i) ist der Indikator f&uuml;r der i-ten Datenpunkt (x,y[,z])
    LOGICAL :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='set_omi_xyz_valid_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_valid_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_valid_1_1
  !
  !! weise der dynamischen Komponente "l3d(:,:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_l3d_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this     ! 
    !! Werte f&uuml;r Komponente "l3d(:,:)": Zuordnung der Koordinaten zu Speicherpositionen
    !! der Daten in einem ein-dimensionalen Vektor <BR>
    !! l3d(i,:) sind die Speicherpositionen der Daten f&uuml;r den i-ten Datenpunkt (x,y[,z]); 
    !! ist die zweite Dimension gr&ouml;&szlig;er als 1, so muss f&uuml;r den Datenpunkt
    !! aus mehrere Speicherpositionen gleichgewichtet interpoliert werden
    INTEGER             , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='set_omi_xyz_l3d_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_l3d ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_l3d   ( this, SIZE(val,1), SIZE(val,2) )
       IF ( no_error( ) ) CALL init_omi_xyz_l3d    ( this            )
       IF ( no_error( ) ) this%l3d(:,:) = val(:,:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_l3d_0_1
  !
  !! weise der dynamischen Komponente "l3d(:,:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_l3d_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "l3d(:,:)": Zuordnung der Koordinaten zu Speicherpositionen
    !! der Daten in einem ein-dimensionalen Vektor <BR>
    !! l3d(i,:) sind die Speicherpositionen der Daten f&uuml;r den i-ten Datenpunkt (x,y[,z]); 
    !! ist die zweite Dimension gr&ouml;&szlig;er als 1, so muss f&uuml;r den Datenpunkt
    !! aus mehrere Speicherpositionen gleichgewichtet interpoliert werden
    INTEGER :: val(:,:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='set_omi_xyz_l3d_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_l3d_0_1 ( this(i), val(:,:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_l3d_1_1
  !
  !! weise der dynamischen Komponente "l2d(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_l2d_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "l2d(:)": Zuordnung der Koordinaten zu Speicherpositionen
    !! der Daten in einem ein-dimensionalen Vektor <BR>
    !! l2d(i) ist die Speicherpositionen der Daten f&uuml;r den i-ten Datenpunkt (x,y[,z])
    INTEGER          , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='set_omi_xyz_l2d_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_l2d ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_l2d   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_l2d    ( this            )
       IF ( no_error( ) ) this%l2d(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_l2d_0_1
  !
  !! weise der dynamischen Komponente "l2d(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_l2d_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "l2d(:)": Zuordnung der Koordinaten zu Speicherpositionen
    !! der Daten in einem ein-dimensionalen Vektor <BR>
    !! l2d(i) ist die Speicherpositionen der Daten f&uuml;r den i-ten Datenpunkt (x,y[,z])
    INTEGER            , INTENT(IN)  :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='set_omi_xyz_l2d_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_l2d_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_l2d_1_1
  !
  !! weise der (optionalen) dynamischen Komponente "layers(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_layers_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "layers(:)": Schichtgrenzen, von unten nach oben
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='set_omi_xyz_layers_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_layers ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_layers   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_layers    ( this            )
       IF ( no_error( ) ) this%layers(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_layers_0_1
  !
  !! weise der (optionalen) dynamischen Komponente "layers(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_layers_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz)   , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "layers(:)": Schichtgrenzen, von unten nach oben
    REAL (KIND=Double) , INTENT(IN)    :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='set_omi_xyz_layers_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_layers_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_layers_1_1
  !
  !! weise der (optionalen) dynamischen Komponente "modif(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_modif_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "modif(:)": Modifikationsindikatoren
    LOGICAL            , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='set_omi_xyz_modif_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_modif ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_modif  ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_modif    ( this            )
       IF ( no_error( ) ) this%modif(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_modif_0_1
  !
  !! weise der (optionalen) dynamischen Komponente "modif(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_modif_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz)   , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "modif(:)": Modifikationsindikatoren
    LOGICAL            , INTENT(IN)    :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='set_omi_xyz_modif_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_modif_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_modif_1_1
  !
  !! weise der Komponente "hlimit" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_hlimit_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "hlimit": Grenzwert f&uuml;r geometrische Bedeckung
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%hlimit = val
    !
  END SUBROUTINE set_omi_xyz_hlimit_0_0
  !
  !! weise der Komponente "hlimit" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_hlimit_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "hlimit": Grenzwert f&uuml;r geometrische Bedeckung
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this(:)%hlimit = val
    !
  END SUBROUTINE set_omi_xyz_hlimit_1_0
  !
  !! weise der Komponente "ctype" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_ctype_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "ctype": Lage der Koordinaten (0,1,2 = ohne Angabe,
    !! mittig, auf Schichtgrenzen)
    INTEGER            , INTENT(IN)    :: val  ! 
    !
    this%ctype = val
    !
  END SUBROUTINE set_omi_xyz_ctype_0_0
  !
  !! weise der Komponente "ctype" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_ctype_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "ctype": Lage der Koordinaten (0,1,2 = ohne Angabe,
    !! mittig, auf Schichtgrenzen)
    INTEGER          , INTENT(IN)    :: val  ! 
    !
    this(:)%ctype = val
    !
  END SUBROUTINE set_omi_xyz_ctype_1_0
  !
  !! weise der Komponente "version" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_version_0_0 &
       ( this, &
       val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "version": [0,1,2,3,...]
    INTEGER            , INTENT(IN)    :: val  ! 
    !
    this%version = val
    !
  END SUBROUTINE set_omi_xyz_version_0_0
  !
  !! weise der Komponente "version" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_xyz_version_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "version": [0,1,2,3,...]
    INTEGER          , INTENT(IN)    :: val  ! 
    !
    this(:)%version = val
    !
  END SUBROUTINE set_omi_xyz_version_1_0
  !
  !! weise der (optionalen) dynamischen Komponente "zmax(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_zmax_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "zmax(:)": maximale z-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='set_omi_xyz_zmax_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_zmax ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_zmax   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_zmax    ( this            )
       IF ( no_error( ) ) this%zmax(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_zmax_0_1
  !
  !! weise der (optionalen) dynamischen Komponente "zmax(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_zmax_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "zmax(:)": maximale z-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='set_omi_xyz_zmax_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_zmax_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_zmax_1_1
  !
  !! weise der (optionalen) dynamischen Komponente "bini(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_bini_0_1 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "bini(:)": maximale z-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='set_omi_xyz_bini_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_xyz_bini ( this            )
       IF ( no_error( ) ) CALL alloc_omi_xyz_bini   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_xyz_bini    ( this            )
       IF ( no_error( ) ) this%bini(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_xyz_bini_0_1
  !
  !! weise der (optionalen) dynamischen Komponente "bini(:)" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_xyz_bini_1_1 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "bini(:)": maximale z-Koordinaten der Datenpunkte
    REAL (KIND=Double) , INTENT(IN)  :: val(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='set_omi_xyz_bini_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    i = 0
    DO
       i = i + 1
       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
       CALL set_omi_xyz_bini_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_omi_xyz_bini_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_id_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id": kurzer Identifikationsbezeichner
    CHARACTER (LEN=c_len_omi_xyz_id) :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_omi_xyz_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_id_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz)   , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewerte "id(:)": kurzer Identifikationsbezeichner
    CHARACTER (LEN=c_len_omi_xyz_id) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%id
    !
  END FUNCTION get_omi_xyz_id_1_0
  !
  !! hole die Komponente "description" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_description_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)            , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "description": ausf&uuml;hrliche Beschreibung
    CHARACTER (LEN=c_len_omi_xyz_description) :: val  ! 
    !
    val = this%description
    !
  END FUNCTION get_omi_xyz_description_0_0
  !
  !! hole die Komponente "description" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_description_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz)            , INTENT(IN)  :: this(:)         ! 
    !! R&uuml;ckgabewerte "description": ausf&uuml;hrliche Beschreibung
    CHARACTER (LEN=c_len_omi_xyz_description) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%description
    !
  END FUNCTION get_omi_xyz_description_1_0
  !
  !! hole die Komponente "refsystem" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_refsystem_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "refsystem": Koordinatensystem der Datenpunkte
    TYPE (t_omi_space)             :: val  ! 
    !
    val = this%refsystem
    !
  END FUNCTION get_omi_xyz_refsystem_0_0
  !
  !! hole die Komponente "refsystem" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_refsystem_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewerte "refsystem": Koordinatensystem der Datenpunkte
    TYPE (t_omi_space)             :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%refsystem
    !
  END FUNCTION get_omi_xyz_refsystem_1_0
  !
  !! hole die Komponente "ztype" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_ztype_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "ztype": Typ der z-Koordinaten [0,1,2] = [keine,z,sigma]
    INTEGER :: val  ! 
    !
    val = this%ztype
    !
  END FUNCTION get_omi_xyz_ztype_0_0
  !
  !! hole die Komponente "ztype" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_ztype_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewerte "ztype": Typ der z-Koordinaten [0,1,2] = [keine,z,sigma]
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%ztype
    !
  END FUNCTION get_omi_xyz_ztype_1_0
  !
  !! hole die dynamische Feld-Komponente "x(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_x_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "x" : x-Koordinaten der Datenpunkte
    REAL (KIND=Double) , POINTER    :: val(:) ! 
    !
    val => this%x
    !
  END FUNCTION get_omi_xyz_x_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "y(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_y_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "y(:)" (Vektor): y-Koordinaten der Datenpunkte
    REAL (KIND=Double) , POINTER    :: val(:) ! 
    !
    val => this%y
    !
  END FUNCTION get_omi_xyz_y_ref_0_1
  !
  !! hole die (optionale) dynamische Feld-Komponente "z(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_z_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "z(:)": z-Koordinaten der Datenpunkte
    REAL (KIND=Double) , POINTER    :: val(:) ! 
    !
    val => this%z
    !
  END FUNCTION get_omi_xyz_z_ref_0_1
  !
  !! hole die (optionale) dynamische Feld-Komponente "bini(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_bini_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "bini(:)": bini-Koordinaten der Datenpunkte
    REAL (KIND=Double) , POINTER    :: val(:) ! 
    !
    val => this%bini
    !
  END FUNCTION get_omi_xyz_bini_ref_0_1
  !
  !! hole die (optionale) dynamische Feld-Komponente "valid(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_valid_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "valid(:)": Kennzeichnung g&uuml;ltiger/ung&uuml;ltiger Datenpunkte <BR>
    !! valid(i) ist der Indikator f&uuml;r der i-ten Datenpunkt (x,y[,z])
    LOGICAL          , POINTER    :: val(:) ! 
    !
    val => this%valid
    !
  END FUNCTION get_omi_xyz_valid_ref_0_1
  !
  !! hole die (optionale) dynamische Feld-Komponente "l3d(:,:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_l3d_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this     ! 
    !! R&uuml;ckgabewert "l3d(:,:)": Zuordnung der Koordinaten zu Speicherpositionen
    !! der Daten in einem ein-dimensionalen Vektor <BR>
    !! l3d(i,:) sind die Speicherpositionen der Daten f&uuml;r den i-ten Datenpunkt (x,y[,z]); 
    !! ist die zweite Dimension gr&ouml;&szlig;er als 1, so muss f&uuml;r den Datenpunkt
    !! aus mehrere Speicherpositionen gleichgewichtet interpoliert werden
    INTEGER          , POINTER    :: val(:,:) ! 
    !
    val => this%l3d
    !
  END FUNCTION get_omi_xyz_l3d_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "l2d(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_l2d_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "l2d(:)": Zuordnung der Koordinaten zu Speicherpositionen
    !! der Daten in einem ein-dimensionalen Vektor <BR>
    !! l2d(i) ist die Speicherpositionen der Daten f&uuml;r den i-ten Datenpunkt (x,y[,z])
    INTEGER          , POINTER    :: val(:) ! 
    !
    val => this%l2d
    !
  END FUNCTION get_omi_xyz_l2d_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "layers(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_layers_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "layers(:)": Schichtgrenzen, von unten nach oben
    REAL (KIND=Double) , POINTER    :: val(:) ! 
    !
    val => this%layers
    !
  END FUNCTION get_omi_xyz_layers_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "modif(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_modif_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "modif(:)": Modifikationsindikatoren
    LOGICAL , POINTER    :: val(:) ! 
    !
    val => this%modif
    !
  END FUNCTION get_omi_xyz_modif_ref_0_1
  !
  !! hole die Komponente "hlimit" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_hlimit_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "hlimit": Grenzwert f&uuml;r geometrische Bedeckung
    REAL (KIND=Double) :: val  ! 
    !
    val = this%hlimit
    !
  END FUNCTION get_omi_xyz_hlimit_0_0
  !
  !! hole die Komponente "hlimit" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_hlimit_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "hlimit": Grenzwert f&uuml;r geometrische Bedeckung
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%hlimit
    !
  END FUNCTION get_omi_xyz_hlimit_1_0
  !
  !! hole die Komponente "ctype" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_ctype_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "ctype": Lage der Koordinaten (0,1,2 = ohne Angabe,
    !! mittig, auf Schichtgrenzen)
    INTEGER :: val  ! 
    !
    val = this%ctype
    !
  END FUNCTION get_omi_xyz_ctype_0_0
  !
  !! hole die Komponente "ctype" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_ctype_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "ctype": Lage der Koordinaten (0,1,2 = ohne Angabe,
    !! mittig, auf Schichtgrenzen)
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%ctype
    !
  END FUNCTION get_omi_xyz_ctype_1_0
  !
  !! hole die Komponente "version" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_version_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "version": [0,1,2,3,...]
    INTEGER :: val  ! 
    !
    val = this%version
    !
  END FUNCTION get_omi_xyz_version_0_0
  !
  !! hole die Komponente "version" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_xyz_version_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "version": [0,1,2,3,...]
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%version
    !
  END FUNCTION get_omi_xyz_version_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_xyz" f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz)  , INTENT(IN) :: this(:) ! 
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
  END FUNCTION get_omi_xyz_idx_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_xyz" f&uuml;r viele Werte <EM>id(:)</EM> (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz)  , INTENT(IN) :: this(:) ! 
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
       res(i) = get_omi_xyz_idx_1_0 ( this(:), val(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_idx_1_1
  !
  !! Ermittle die Koordinate f&uuml;r den westlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_west_0 ( this, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)  , INTENT(IN) :: this ! 
    !! (optional) Liste der Koordinatenpunkte, die zum Ermitteln des Ergebnisses verwendet werden sollen
    INTEGER , OPTIONAL, INTENT(IN) :: list(:) ! 
    !! Ergebnis: Koordinate f&uuml;r westlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%x ) ) THEN
       IF ( PRESENT(list) ) THEN
          res = MINVAL( this%x(list), this%x(list) /= c_undef_omi_xyz_double )
       ELSE
          res = MINVAL( this%x, this%x /= c_undef_omi_xyz_double )
       END IF
    ELSE
       res = c_undef_omi_xyz_double
    END IF
    !
  END FUNCTION get_omi_xyz_west_0
  !
  !! Ermittle die Koordinaten f&uuml;r die westlichen R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_west_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r westliche R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_west_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_west_1
  !
  !! Ermittle die Koordinate f&uuml;r den &ouml;stlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_east_0 ( this, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! (optional) Liste der Koordinatenpunkte, die zum Ermitteln des Ergebnisses verwendet werden sollen
    INTEGER , OPTIONAL, INTENT(IN) :: list(:) ! 
    !! Ergebnis: Koordinate f&uuml;r &ouml;stlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%x ) ) THEN
       IF ( PRESENT(list) ) THEN
          res = MAXVAL( this%x(list), this%x(list) /= c_undef_omi_xyz_double )
       ELSE
          res = MAXVAL( this%x, this%x /= c_undef_omi_xyz_double )
       END IF
    ELSE
       res = c_undef_omi_xyz_double
    END IF
    !
  END FUNCTION get_omi_xyz_east_0
  !
  !! Ermittle die Koordinaten f&uuml;r die &ouml;stlichen R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_east_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r &ouml;stliche R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_east_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_east_1
  !
  !! Ermittle die Koordinate f&uuml;r den n&ouml;rdlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_north_0 ( this, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)  , INTENT(IN) :: this ! 
    !! (optional) Liste der Koordinatenpunkte, die zum Ermitteln des Ergebnisses verwendet werden sollen
    INTEGER , OPTIONAL, INTENT(IN) :: list(:) ! 
    !! Ergebnis: Koordinate f&uuml;r n&ouml;rdlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%y ) ) THEN
       IF ( PRESENT(list) ) THEN
          res = MAXVAL( this%y(list), this%y(list) /= c_undef_omi_xyz_double )
       ELSE
          res = MAXVAL( this%y, this%y /= c_undef_omi_xyz_double )
       END IF
    ELSE
       res = c_undef_omi_xyz_double
    END IF
    !
  END FUNCTION get_omi_xyz_north_0
  !
  !! Ermittle die Koordinaten f&uuml;r die n&ouml;rdlichen R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_north_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r n&ouml;rdliche R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_north_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_north_1
  !
  !! Ermittle die Koordinate f&uuml;r den s&uuml;dlichen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_south_0 ( this, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)  , INTENT(IN) :: this ! 
    !! (optional) Liste der Koordinatenpunkte, die zum Ermitteln des Ergebnisses verwendet werden sollen
    INTEGER , OPTIONAL, INTENT(IN) :: list(:) ! 
    !! Ergebnis: Koordinate f&uuml;r s&uuml;dlichen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%y ) ) THEN
       IF ( PRESENT(list) ) THEN
          res = MINVAL( this%y(list), this%y(list) /= c_undef_omi_xyz_double )
       ELSE
          res = MINVAL( this%y, this%y /= c_undef_omi_xyz_double )
       END IF
    ELSE
       res = c_undef_omi_xyz_double
    END IF
    !
  END FUNCTION get_omi_xyz_south_0
  !
  !! Ermittle die Koordinaten f&uuml;r die s&uuml;dlichen R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_south_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r s&uuml;dliche R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_south_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_south_1
  !
  !! Ermittle die Koordinate f&uuml;r den oberen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_top_0 ( this, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)  , INTENT(IN) :: this ! 
    !! (optional) Liste der Koordinatenpunkte, die zum Ermitteln des Ergebnisses verwendet werden sollen
    INTEGER , OPTIONAL, INTENT(IN) :: list(:) ! 
    !! Ergebnis: Koordinate f&uuml;r oberen Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%z ) ) THEN
       IF ( has_omi_space_altitude( this%refsystem ) ) THEN
          IF ( PRESENT(list) ) THEN
             res = MAXVAL( this%z(list), this%z(list) /= c_undef_omi_xyz_double )
          ELSE
             res = MAXVAL( this%z, this%z /= c_undef_omi_xyz_double )
          END IF
       ELSE IF ( has_omi_space_depth( this%refsystem ) ) THEN
          IF ( PRESENT(list) ) THEN
             res = MINVAL( this%z(list), this%z(list) /= c_undef_omi_xyz_double )
          ELSE
             res = MINVAL( this%z, this%z /= c_undef_omi_xyz_double )
          END IF
       ELSE
          res = c_undef_omi_xyz_double
       END IF
    ELSE
       res = c_undef_omi_xyz_double
    END IF
    !
  END FUNCTION get_omi_xyz_top_0
  !
  !! Ermittle die Koordinaten f&uuml;r die oberen R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_top_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r obere R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_top_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_top_1
  !
  !! Ermittle die Koordinate f&uuml;r den oberen Rand <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_bottom_0 ( this, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)  , INTENT(IN) :: this ! 
    !! (optional) Liste der Koordinatenpunkte, die zum Ermitteln des Ergebnisses verwendet werden sollen
    INTEGER , OPTIONAL, INTENT(IN) :: list(:) ! 
    !! Ergebnis: Koordinate f&uuml;r unteren Rand
    REAL (KIND=Double) :: res ! 
    !
    IF ( ASSOCIATED( this%z ) ) THEN
       IF ( has_omi_space_altitude( this%refsystem ) ) THEN
          IF ( PRESENT(list) ) THEN
             res = MINVAL( this%z(list), this%z(list) /= c_undef_omi_xyz_double )
          ELSE
             res = MINVAL( this%z, this%z /= c_undef_omi_xyz_double )
          END IF
       ELSE IF ( has_omi_space_depth( this%refsystem ) ) THEN
          IF ( PRESENT(list) ) THEN
             res = MAXVAL( this%z(list), this%z(list) /= c_undef_omi_xyz_double )
          ELSE
             res = MAXVAL( this%z, this%z /= c_undef_omi_xyz_double )
          END IF
       ELSE
          res = c_undef_omi_xyz_double
       END IF
    ELSE
       res = c_undef_omi_xyz_double
    END IF
    !
  END FUNCTION get_omi_xyz_bottom_0
  !
  !! Ermittle die Koordinaten f&uuml;r die oberen R&auml;nder <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_bottom_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Koordinaten f&uuml;r unteren R&auml;nder
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_bottom_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_bottom_1
  !
  !! Ermittle die Anzahl der Positionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_point_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der Positionen (x,y) oder (x,y,z)
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%x ) ) THEN
       res = SIZE( this%x )
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_xyz_point_count_0
  !
  !! Ermittle die Anzahl der Positionen f&uuml;r viele Objekte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_point_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der Positionen (x,y) oder (x,y,z)
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_point_count_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_point_count_1
  !
  !! Ermittle die Anzahl der zwei-dimensionalen Positionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_2d_point_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der zwei-dimensionalen Positionen <BR>
    !! die Anzahl wird auf der Basis der unterschiedlichen Eintr&auml;ge in
    !! der Komponente "l2d(:)" ermittelt
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: i, memo ! 
    !
    IF ( ASSOCIATED( this%l2d ) ) THEN
       memo = this%l2d(1)
       res  = 1
       DO i=2,SIZE(this%l2d)
          IF ( this%l2d(i) == memo ) CYCLE
          res  = res + 1
          memo = this%l2d(i)
       END DO
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_xyz_2d_point_count_0
  !
  !! Ermittle die Anzahl der zwei-dimensionalen Positionen f&uuml;r viele Objekte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_2d_point_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der zwei-dimensionalen Positionen <BR>
    !! die Anzahl wird auf der Basis der unterschiedlichen Eintr&auml;ge in
    !! der Komponente "l2d(:)" ermittelt
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_2d_point_count_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_2d_point_count_1
  !
  !! Ermittle die Anzahl der Schichten/Schichtgrenzen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der Schichten/Schichtgrenzen
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%layers ) ) THEN
       IF      ( this%ctype == 1 ) THEN
          res = SIZE( this%layers ) - 1
       ELSE IF ( this%ctype == 2 ) THEN
          res = SIZE( this%layers )
       END IF
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_xyz_layer_count_0
  !
  !! Ermittle die Anzahl der Schichten (nicht Schichtgrenzen) f&uuml;r viele Objekte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der Schichten (nicht Schichtgrenzen)
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_xyz_layer_count_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_xyz_layer_count_1
  !
  !! Ermittle den Namen einer bestimmten Schicht <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_name_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Nummer der aktuellen Schicht, f&uuml;r die ein Name ermittelt werden soll
    INTEGER          , INTENT(IN) :: val  ! 
    !! Ergebnis: Bezeichnung des Namens
    CHARACTER (LEN=c_len_omi_xyz_layer_name) :: res ! 
    !! Hilfsvariable
    INTEGER :: n ! 
    !
    n = get_omi_xyz_layer_count( this )
    IF ( n > 0 .AND. val >= 1 .AND. val <= n ) THEN
       WRITE(res(1:35),'(A8,I3.3,A4,F9.2,A1,F9.2,A1)') &
            '?-Layer ',val,' : [',this%layers(val),',',this%layers(val+1),']'
       IF ( has_omi_xyz_z_layers( this )     ) WRITE(res(1:1),'(A1)') 'z'
       IF ( has_omi_xyz_sigma_layers( this ) ) WRITE(res(1:1),'(A1)') 's'
    ELSE
       res = REPEAT( ' ', LEN(res) )
       res = c_undef_omi_xyz_char
    END IF
    !
  END FUNCTION get_omi_xyz_layer_name_0
  !
  !! Ermittle die Anzahl der in einer bestimmten Schicht liegenden Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_point_count_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Nummer der aktuellen Schicht, f&uuml;r die ein Name ermittelt werden soll
    INTEGER          , INTENT(IN) :: val  ! 
    !! Ergebnis: Anzahl der Datenpunkte in der val-ten Schicht
    INTEGER :: res                        ! 
    !! Hilfsvariable
    LOGICAL , POINTER :: mask(:)      ! 
    !
    mask => get_omi_xyz_layer_point_mask ( this, val )
    IF ( ASSOCIATED( mask ) ) THEN
       res = COUNT( mask )
       DEALLOCATE( mask ) ; NULLIFY( mask )
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_xyz_layer_point_count_0
  !
  !! Ermittle die Anzahl der in einer bestimmten Schicht liegenden Punkte,
  !! wobei die Auswahl durch eine zus&auml;tzliche Liste von zwei-dimensionalen
  !! Positionen weiter eingeschr&auml;nkt wird  <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_point_count_1 ( this, val, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this    ! 
    !! Nummer der aktuellen Schicht, f&uuml;r die ein Name ermittelt werden soll
    INTEGER          , INTENT(IN) :: val     ! 
    !! Liste mit den Nummern der zwei-dimensionalen Positionen, f&uuml;r die
    !! die Anzahl ermittelt werden soll
    INTEGER          , INTENT(IN) :: list(:) ! 
    !! Ergebnis: Anzahl der Datenpunkte in der val-ten Schicht
    INTEGER :: res                        ! 
    !! Hilfsvariable
    LOGICAL , POINTER :: mask_layer(:), mask_column(:) ! 
    !
    mask_layer  => get_omi_xyz_layer_point_mask  ( this, val  )
    mask_column => get_omi_xyz_column_point_mask ( this, list )
    IF ( ASSOCIATED(mask_layer) .AND. ASSOCIATED(mask_column) ) THEN
       res = COUNT( mask_layer .AND. mask_column )
    ELSE
       res = 0
    END IF
    IF ( ASSOCIATED( mask_layer  ) ) DEALLOCATE( mask_layer  )
    IF ( ASSOCIATED( mask_column ) ) DEALLOCATE( mask_column )
    NULLIFY( mask_layer, mask_column )
    !
  END FUNCTION get_omi_xyz_layer_point_count_1
  !
  !! Ermittle das Feld mit den Index-Positionen aller in einer bestimmten Schicht liegenden Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_point_idx_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Nummer der aktuellen Schicht, f&uuml;r die ein Name ermittelt werden soll
    INTEGER          , INTENT(IN) :: val  ! 
    !! Ergebnis: Liste der Index-Positionen der in der val-ten Schicht liegenden Punkte
    INTEGER , POINTER :: res(:)           ! 
    !! Hilfsvariable
    LOGICAL , POINTER :: mask(:) ! 
    INTEGER           :: i, m, n ! 
    !
    NULLIFY( res )
    mask => get_omi_xyz_layer_point_mask ( this, val )
    IF ( ASSOCIATED( mask ) ) THEN
       n = COUNT(mask)
       IF ( n > 0 ) THEN
          ALLOCATE( res(n) )
          m = 0
          DO i=1,SIZE(mask)
             IF ( .NOT. mask(i) ) CYCLE
             m      = m + 1
             res(m) = i 
          END DO
       END IF
       DEALLOCATE( mask ) ; NULLIFY( mask )
    END IF
    !
  END FUNCTION get_omi_xyz_layer_point_idx_0
  !
  !! Ermittle das Feld mit den Index-Positionen aller in einer bestimmten Schicht liegenden Punkte,
  !! wobei die Auswahl durch eine zus&auml;tzliche Liste von zwei-dimensionalen
  !! Positionen weiter eingeschr&auml;nkt wird  <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_point_idx_1 ( this, val, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Nummer der aktuellen Schicht, f&uuml;r die ein Name ermittelt werden soll
    INTEGER          , INTENT(IN) :: val  ! 
    !! Liste mit den Nummern der zwei-dimensionalen Positionen, f&uuml;r die
    !! die Anzahl ermittelt werden soll
    INTEGER          , INTENT(IN) :: list(:) ! 
    !! Ergebnis: Liste der Index-Positionen der in der val-ten Schicht liegenden Punkte
    INTEGER , POINTER :: res(:)           ! 
    !! Hilfsvariable
    LOGICAL , POINTER :: mask_layer(:), mask_column(:) ! 
    INTEGER           :: i, m, n ! 
    !
    NULLIFY( res )
    mask_layer  => get_omi_xyz_layer_point_mask  ( this, val  )
    mask_column => get_omi_xyz_column_point_mask ( this, list )
    IF ( ASSOCIATED( mask_layer ) .AND. ASSOCIATED( mask_column ) ) THEN
       n = COUNT( mask_layer .AND. mask_column )
       IF ( n > 0 ) THEN
          ALLOCATE( res(n) )
          m = 0
          DO i=1,SIZE(mask_layer)
             IF ( .NOT. ( mask_layer(i) .AND. mask_column(i) ) ) CYCLE
             m      = m + 1
             res(m) = i 
          END DO
       END IF
    END IF
    IF ( ASSOCIATED( mask_layer  ) ) DEALLOCATE( mask_layer  )
    IF ( ASSOCIATED( mask_column ) ) DEALLOCATE( mask_column )
    NULLIFY( mask_column, mask_layer )
    !
  END FUNCTION get_omi_xyz_layer_point_idx_1
  !
  !! ermittle eine Maske mit true/false f&uuml;r alle 2D-Punkte, ob diese Punkte
  !! in einer bestimmten Schicht aktiv sind oder nicht <BR>
  !! f&uuml;r ein Objekt (Skalar) und ausgew&auml;hlte zwei-dimensionale Positionen
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_2d_mask_1 ( this, val, list ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this    ! 
    !! Nummer der aktuellen Schicht, f&uuml;r die ein Name ermittelt werden soll
    INTEGER          , INTENT(IN) :: val     ! 
    !! Liste mit den Nummern der zwei-dimensionalen Positionen, f&uuml;r die
    !! die Anzahl ermittelt werden soll
    INTEGER          , INTENT(IN) :: list(:) ! 
    !! Ergebnis: f&uuml;r jede in der Liste list(:) aufgef&uuml;hrte zwei-dimensionale
    !! Position wird true/false zur&uuml;ckgegeben, je nachdem ob diese Position
    !! in der Schicht n vorhanden ist oder nicht
    LOGICAL                       :: res(SIZE(list))       ! 
    !! Hilfsvariable
    LOGICAL , ALLOCATABLE :: l_res(:)                      ! 
    LOGICAL , POINTER     :: mask_layer(:), mask_column(:) ! 
    INTEGER               :: i                             ! 
    !
    mask_layer  => get_omi_xyz_layer_point_mask  ( this, val  )
    mask_column => get_omi_xyz_column_point_mask ( this, list )
    IF ( ASSOCIATED(this%l2d) .AND. ASSOCIATED(mask_layer) .AND. ASSOCIATED(mask_column) ) THEN
       ALLOCATE( l_res(get_omi_xyz_2d_point_count(this)) )
       l_res(:) = .false.
       DO i=1,SIZE(mask_layer)
          IF ( mask_layer(i) .AND. mask_column(i) ) l_res(this%l2d(i)) = .true.
       END DO
       DO i=1,SIZE(list)
          IF ( list(i) < 1 .OR. list(i) > SIZE(l_res) ) CYCLE
          res(i) = l_res(list(i))
       END DO
    END IF
    IF ( ALLOCATED ( l_res       ) ) DEALLOCATE( l_res       )
    IF ( ASSOCIATED( mask_layer  ) ) DEALLOCATE( mask_layer  )
    IF ( ASSOCIATED( mask_column ) ) DEALLOCATE( mask_column )
    NULLIFY( mask_column, mask_layer )
    !
  END FUNCTION get_omi_xyz_layer_2d_mask_1
  !
  !! Ermittle die Anzahl der in einer S&auml;le eines Datenobjekts liegenden Positionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_col_point_count_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz), INTENT(IN) :: this ! 
    !! Nummer der aktuellen S&auml;le, f&uuml;r die die Anzahl der Positionen ermittelt werden soll
    INTEGER , INTENT(IN)         :: val  ! 
    !! Ergebnis: Anzahl der Positionen in S&auml;le "val"
    INTEGER :: res  !
    !
    res = 0
    IF ( ASSOCIATED(this%l2d) ) res = COUNT( this%l2d == val )
    !
  END FUNCTION get_omi_xyz_col_point_count_0_0
  !
  !! Ermittle die Anzahl der in mehreren S&auml;len eines Datenobjekts liegenden Positionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_col_point_count_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz), INTENT(IN) :: this   ! 
    !! Nummern der aktuellen S&auml;len, f&uuml;r die die Anzahl der Positionen ermittelt werden sollen
    INTEGER , INTENT(IN)         :: val(:) ! 
    !! Ergebnis: Anzahl der Positionen in den S&auml;len "val(:)"
    INTEGER               :: res(SIZE(val)) ! 
    !! Hilfsvariable
    INTEGER               :: i              ! 
    INTEGER , ALLOCATABLE :: l_nc(:)        ! 
    !
    res(:) = 0
    IF ( ASSOCIATED(this%l2d) ) THEN
       ALLOCATE( l_nc( get_omi_xyz_2d_point_count( this ) ) )
       l_nc = get_omi_xyz_nof_point ( this, SIZE(l_nc) )
       DO i=1,SIZE(val)
          IF ( val(i) < 1 .OR. val(i) > SIZE(l_nc) ) CYCLE
          res(i) = l_nc(val(i))
       END DO
       DEALLOCATE( l_nc )
    END IF
    !
  END FUNCTION get_omi_xyz_col_point_count_0_1
  !
  !! Ermittle die Index-Positionen der in einer S&auml;le eines Datenobjekts liegenden Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_col_point_idx_0_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz), INTENT(IN) :: this ! 
    !! Nummer der aktuellen S&auml;le, f&uuml;r die die Index-Positionen ermittelt werden sollen
    INTEGER , INTENT(IN)         :: val  ! 
    !! Ergebnis: Liste der Index-Positionen der Punkte in S&auml;le "val"
    INTEGER, POINTER :: res(:)   ! 
    !! Hilfsvariablen   
    INTEGER          :: i, n, nn ! 
    !
    NULLIFY( res )
    n = get_omi_xyz_col_point_count_0_0 ( this, val )
    IF ( n > 0 ) THEN
       ALLOCATE( res(n) )
       res(:) = c_undef_omi_xyz_int
       nn     = 0
       DO i=1,SIZE(this%l2d)
          IF ( nn == SIZE(res)    ) EXIT
          IF ( this%l2d(i) /= val ) CYCLE
          nn      = nn + 1
          res(nn) = i
       END DO
    END IF
    !
  END FUNCTION get_omi_xyz_col_point_idx_0_0
  !
  !! Ermittle die Index-Positionen der in mehreren S&auml;len eines Datenobjekts liegenden Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_col_point_idx_0_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz), INTENT(IN) :: this   ! 
    !! Nummern der aktuellen S&auml;len, f&uuml;r die die Index-Positionen ermittelt werden sollen
    INTEGER , INTENT(IN)         :: val(:) ! 
    !! Ergebnis: Liste der Index-Positionen der Punkte in allen S&auml;len "val(:)"
    INTEGER, POINTER :: res(:)   ! 
    !! Hilfsvariablen
    INTEGER               :: i, j, mm, n2d             ! 
    INTEGER , ALLOCATABLE :: l_nc(:), l_np(:), l_ss(:) ! 
    !
    NULLIFY( res )
    IF ( ASSOCIATED(this%l2d) ) THEN
       n2d = get_omi_xyz_2d_point_count( this )
       ALLOCATE( l_nc(n2d), l_ss(n2d), l_np(SIZE(val)) )
       l_nc(:) = get_omi_xyz_nof_point ( this, SIZE(l_nc) )
       l_np(:) = get_omi_xyz_col_point_count_0_1( this, val )
       ALLOCATE( res(SUM(l_np)) )
       res(:) = c_undef_omi_xyz_int
       mm     = 0
       DO i=1,SIZE(l_ss)
          l_ss(i) = mm
          mm      = mm + l_nc(i)
       END DO
       mm     = 0
       DO j=1,SIZE(l_np)
          DO i=1,l_np(j)
             mm      = mm + 1
             res(mm) = l_ss(val(j)) + i 
          END DO
       END DO
       DEALLOCATE( l_nc, l_np, l_ss )
    END IF
    !
  END FUNCTION get_omi_xyz_col_point_idx_0_1
  !
  !! Interpoliere die in einem ein-dimensionalen Feld &uuml;bergebenen 
  !! Daten (Real(Double)) auf die Positionen der Datenpunkte (x,y[,z]) <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_omi_xyz_data_d_1 ( this, val, missing ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this    ! 
    !! Feld mit Original-Daten
    REAL (KIND=Double) , INTENT(IN) :: val(:)  ! 
    !! Wert zur Kennzeichnung fehlender/ung&uuml;ltiger Daten
    REAL (KIND=Double) , INTENT(IN) :: missing ! 
    !! Ergebnis: interpolierte Werte
    REAL (KIND=Double) :: res(SIZE(this%x))    ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='get_omi_xyz_data_d_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                  ! 
    INTEGER            :: i, j, l, nv, nl, nn ! 
    !
    nv = SIZE(val)
    IF ( ASSOCIATED(this%l3d) ) THEN
       nl = MAXVAL(this%l3d)
    ELSE
       nl = 0
    END IF
    !
    IF ( nl == nv ) THEN
       DO i=1,SIZE(this%l3d,1)
          nn     = 0
          res(i) = 0.0_Double
          IF ( this%valid(i) ) THEN
             DO j=1,SIZE(this%l3d,2)
                l = this%l3d(i,j)
                IF ( l > 0 ) THEN
                   IF ( val(l) /= missing ) THEN
                      res(i) = res(i) + val(l)
                      nn     = nn     + 1
                   END IF
                END IF
             END DO
          END IF
          IF ( nn > 0 ) THEN
             res(i) = res(i)/nn
          ELSE
             res(i) = missing
          END IF
       END DO
    ELSE
       res(:) = c_undef_omi_xyz_double
       CALL setup_error_act ( all_errors(:), 9000, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') nv ; CALL setup_error_act ( '<act>', ch )
       WRITE(ch,'(I10)') nl ; CALL setup_error_act ( '<req>', ch )
    END IF
    !
  END FUNCTION get_omi_xyz_data_d_1
  !
  !! Interpoliere die in einem ein-dimensionalen Feld &uuml;bergebenen 
  !! Daten (Ineteger) auf die Positionen der Datenpunkte (x,y[,z]) <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_omi_xyz_data_i_1 ( this, val, missing ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this    ! 
    !! Feld mit Original-Daten
    INTEGER          , INTENT(IN) :: val(:)  ! 
    !! Wert zur Kennzeichnung fehlender/ung&uuml;ltiger Daten
    INTEGER          , INTENT(IN) :: missing ! 
    !! Ergebnis: interpolierte Werte
    INTEGER :: res(SIZE(this%x))    ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='get_omi_xyz_data_i_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                  ! 
    INTEGER            :: i, j, l, nv, nl, nn ! 
    !
    nv = SIZE(val)
    IF ( ASSOCIATED(this%l3d) ) THEN
       nl = MAXVAL(this%l3d)
    ELSE
       nl = 0
    END IF
    !
    IF ( nl == nv ) THEN
       DO i=1,SIZE(this%l3d,1)
          nn     = 0
          res(i) = 0
          IF ( this%valid(i) ) THEN
             DO j=1,SIZE(this%l3d,2)
                l = this%l3d(i,j)
                IF ( l > 0 ) THEN
                   IF ( val(l) /= missing ) THEN
                      res(i) = res(i) + val(l)
                      nn     = nn     + 1
                   END IF
                END IF
             END DO
          END IF
          IF ( nn > 0 ) THEN
             res(i) = NINT(REAL(res(i),Double)/REAL(nn,Double))
          ELSE
             res(i) = missing
          END IF
       END DO
    ELSE
       res(:) = c_undef_omi_xyz_int
       CALL setup_error_act ( all_errors(:), 9000, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') nv ; CALL setup_error_act ( '<act>', ch )
       WRITE(ch,'(I10)') nl ; CALL setup_error_act ( '<req>', ch )
    END IF
    !
  END FUNCTION get_omi_xyz_data_i_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-IS/HAS-Methoden <<< 
  ! ----------------------------------------------------------------------
  !
  !! Ermittle, ob das Gitter zwei-dimensional (x,y) ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_xyz_two_dimensional_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( ASSOCIATED( this%x ) .AND. ASSOCIATED( this%y ) .AND. .NOT.ASSOCIATED( this%z ) )
    !
  END FUNCTION is_omi_xyz_two_dimensional_0
  !
  !! Ermittle, ob das mehrere Gitter zwei-dimensional (x,y) sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_xyz_two_dimensional_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_omi_xyz_two_dimensional_0 ( this(i) )
    END DO
    !
  END FUNCTION is_omi_xyz_two_dimensional_1
  !
  !! Ermittle, ob das Gitter drei-dimensional (x,y,z) ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_xyz_three_dimensional_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( ASSOCIATED( this%x ) .AND. ASSOCIATED( this%y ) .AND. ASSOCIATED( this%z ) )
    !
  END FUNCTION is_omi_xyz_three_dimensional_0
  !
  !! Ermittle, ob das mehrere Gitter drei-dimensional (x,y) sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_xyz_three_dimensional_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_omi_xyz_three_dimensional_0 ( this(i) )
    END DO
    !
  END FUNCTION is_omi_xyz_three_dimensional_1
  !
  !! Pr&uuml;fe, ob die Koordinaten der Punkte (x,y[,z]) modifiziert wurden (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_xyz_modified_0 ( this ) & 
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    IF ( ASSOCIATED( this%modif ) ) THEN
       res = ANY( this%modif(:) )
    ELSE
       res = .false.
    END IF
    !
  END FUNCTION is_omi_xyz_modified_0 
  !
  !! Pr&uuml;fe, ob die Koordinaten der Punkte (x,y[,z]) modifiziert wurden (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_xyz_modified_1 ( this ) & 
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_omi_xyz_modified_0 ( this(i) )
    END DO
    !
  END FUNCTION is_omi_xyz_modified_1
  !
  !! Ermittle, ob ein Gitter Schichten aufweist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_xyz_layers_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( this%ztype > 0 .AND. ASSOCIATED( this%layers ) ) 
    !
  END FUNCTION has_omi_xyz_layers_0 
  !
  !! Ermittle, ob viele Gitter Schichten aufweisen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_xyz_layers_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_xyz_layers_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_xyz_layers_1 
  !
  !! Ermittle, ob ein Gitter z-Schichten aufweist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_xyz_z_layers_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( this%ztype == 1 .AND. ASSOCIATED( this%layers ) ) 
    !
  END FUNCTION has_omi_xyz_z_layers_0 
  !
  !! Ermittle, ob viele Gitter z-Schichten aufweisen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_xyz_z_layers_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_xyz_z_layers_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_xyz_z_layers_1 
  !
  !! Ermittle, ob ein Gitter sigma-Schichten aufweist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_xyz_sigma_layers_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( this%ztype == 2 .AND. ASSOCIATED( this%layers ) ) 
    !
  END FUNCTION has_omi_xyz_sigma_layers_0 
  !
  !! Ermittle, ob viele Gitter sigma-Schichten aufweisen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_xyz_sigma_layers_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_xyz_sigma_layers_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_xyz_sigma_layers_1 
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-UPDATE-Methoden <<< [ERR_NO = ????? bis ?????]
  ! ----------------------------------------------------------------------
  !
  !! Aktualisieren der z-Koordinate <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE update_omi_xyz_0 ( this, surface, bottom )
    !! Datenobjekt (Skalar) [ Datenobjekt wird von dieser Subroutine nicht gepr&uuml;ft ]
    TYPE (t_omi_xyz)              , INTENT(INOUT) :: this       ! 
    !! Lage der Oberfl&auml;che f&uuml;r alle Orte (x,y)
    REAL (KIND=Double) , OPTIONAL , INTENT(IN)    :: surface(:) ! 
    !! Lage des (tiefsten) Bodens f&uuml;r alle Orte (x,y)
    REAL (KIND=Double) , OPTIONAL , INTENT(IN)    :: bottom(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='update_omi_xyz_0' ! 
    !! Hilfsvariable
    LOGICAL                          :: ok                      !  
    REAL (KIND=Double) , ALLOCATABLE :: z_old(:)                ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( ok_omi_xyz ( this ) ) THEN
          IF ( PRESENT( surface ) ) THEN
             ok = ok_omi_xyz_surface ( this, surface(:) )
          END IF
          IF ( PRESENT( bottom  ) ) THEN
             ok = ok_omi_xyz_bottom       ( this, bottom(:)  )
             ok = ok_omi_xyz_bottom_zmax  ( this, bottom(:)  )
          END IF
          IF ( PRESENT( bottom  ) .AND. PRESENT( surface ) ) THEN
             ok = ok_omi_xyz_surface_bottom ( this, surface(:), bottom(:)  )
          END IF
          IF ( no_error( ) ) THEN
             ! ... Initialisierungen 
             IF ( ASSOCIATED( this%z) ) THEN
                ALLOCATE( z_old(SIZE(this%z)) )
                z_old(:) = this%z(:)
             END IF
             ! Aktualisieren der Tiefen [ falls vorhanden ]
             IF ( ASSOCIATED( this%z ) ) THEN
                IF      (       PRESENT(surface) .AND.       PRESENT(bottom) ) THEN
                   CALL update_omi_xyz_z ( this, surface, bottom )
                ELSE IF (       PRESENT(surface) .AND. .NOT. PRESENT(bottom) ) THEN
                   IF ( ASSOCIATED(this%bini) ) THEN
                      CALL update_omi_xyz_z ( this, surface=surface, bottom=this%bini )
                   ELSE
                      CALL update_omi_xyz_z ( this, surface=surface )
                   END IF
                ELSE IF ( .NOT. PRESENT(surface) .AND.       PRESENT(bottom) ) THEN
                   CALL update_omi_xyz_z ( this, bottom=bottom )
                ELSE 
                   CALL update_omi_xyz_z ( this )
                END IF
             END IF
             ! Aktualisieren der Komponente "modif(:)" [ falls vorhanden ]
             IF ( ALLOCATED( z_old ) ) THEN
                CALL update_omi_xyz_modif ( this, z_old(:) )
             ELSE
                CALL update_omi_xyz_modif ( this )
             END IF
             ! Aktualisieren der Komponente "valid(:)"
             IF      (       PRESENT(surface) .AND.       PRESENT(bottom) ) THEN
                CALL update_omi_xyz_valid ( this, surface, bottom )
             ELSE IF (       PRESENT(surface) .AND. .NOT. PRESENT(bottom) ) THEN
                IF ( ASSOCIATED(this%bini) ) THEN
                   CALL update_omi_xyz_valid ( this, surface=surface, bottom=this%bini )
                ELSE
                   CALL update_omi_xyz_valid ( this, surface=surface )
                END IF
             ELSE IF ( .NOT. PRESENT(surface) .AND.       PRESENT(bottom) ) THEN
                CALL update_omi_xyz_valid ( this, bottom=bottom )
             ELSE 
                CALL update_omi_xyz_valid ( this )
             END IF
             ! Deallokieren
             IF ( ALLOCATED( z_old ) ) DEALLOCATE( z_old )
          END IF
          IF ( is_omi_xyz_modified( this ) ) this%version = this%version + 1
       END IF
    END IF
    !
  END SUBROUTINE update_omi_xyz_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CREATE-Methoden <<< [ERR_NO = ????? bis ?????]
  ! ----------------------------------------------------------------------
  !
  !! Erzeugen der dynamischen Komponenten "x(:)", "y(:)"[, "z(:)"], "l2d(:)", 
  !! "l3d(:,:)" und "valid(:)" eines Datenobjektes f&uuml;r vorgegebene
  !! Orte (x,y). Ist insbesondere f&uuml;r Strukturen n&uuml;tzlich, 
  !! die entweder keine Tiefeninformation oder keine Schichtinformationen
  !! ben&ouml;tigen: <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_xyz_no_layers_0 ( this, x2d, y2d, z2d, bottom )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)              , INTENT(INOUT) :: this      ! 
    !! x-Koordinaten der Orte (x,y) in der Ebene (2D)
    REAL (KIND=Double)            , INTENT(IN)    :: x2d(:)    ! 
    !! y-Koordinaten der Orte (x,y) in der Ebene (2D)
    REAL (KIND=Double)            , INTENT(IN)    :: y2d(:)    ! 
    !! z-Koordinaten der Orte (x,y) [ es wird ein 3D-Objekt mit Tiefeninformation erzeugt  ]
    REAL (KIND=Double) , OPTIONAL , INTENT(IN)    :: z2d(:)    ! 
    !! z-Koordinaten der Orte (x,y) [ es wird ein 2D-Objekt ohne Tiefeninformation, allerdixsxsngs
    !! wird die Information &uuml;ber die Sohllage in der Komponente "bini(:)" gespeichert ]
    REAL (KIND=Double) , OPTIONAL , INTENT(IN)    :: bottom(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=26) , PARAMETER :: c_upname='create_omi_xyz_no_layers_0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                      ! 
    INTEGER            :: i, nn                   ! 
    LOGICAL            :: l_ok(7)                 ! 
    LOGICAL , ALLOCATABLE :: l_valid(:)           ! 
    INTEGER , ALLOCATABLE :: l_l2d(:), l_l3d(:,:) ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_omi_xyz_id          ( this )
       l_ok(2) = ok_omi_xyz_description ( this )
       l_ok(3) = ok_omi_xyz_refsystem   ( this )
       l_ok(4) = ok_omi_xyz_ztype       ( this )
       l_ok(5) = ( this%ztype == 0 )
       l_ok(6) = ok_omi_xyz_ctype       ( this )
       l_ok(7) = ( this%ctype == 0 )
       IF ( .NOT. ALL( l_ok(:) ) ) THEN
          CALL setup_error_act ( all_errors(:), 8500, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
          WRITE(ch(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-id>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-description>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-refsystem>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<ok-ztype>', ch(1:1) )
          WRITE(ch,'(I10)') this%ztype ; CALL setup_error_act ( '<ztype>', ch )
          WRITE(ch(1:1),'(L1)') l_ok(6) ; CALL setup_error_act ( '<ok-ctype>', ch(1:1) )
          WRITE(ch,'(I10)') this%ctype ; CALL setup_error_act ( '<ctype>', ch )
       ELSE
          CALL dealloc_omi_xyz_dyn_components ( this ) 
          IF ( no_error( ) ) THEN
             nn = SIZE(x2d)
             ALLOCATE ( l_valid(nn), l_l2d(nn), l_l3d(nn,1) )
             DO i=1,nn
                l_valid(i) = .true. 
                l_l2d(i)   = i
                l_l3d(i,1) = i
             END DO
             IF ( no_error( ) ) CALL set_omi_xyz_x_0_1       ( this, x2d(:)     )
             IF ( no_error( ) ) CALL set_omi_xyz_y_0_1       ( this, y2d(:)     )
             IF ( PRESENT( z2d ) ) THEN
                IF ( no_error( ) ) CALL set_omi_xyz_z_0_1    ( this, z2d(:)     )
                IF ( no_error( ) ) CALL create_omi_xyz_bini  ( this, z2d(:)     )
             ELSE IF ( .NOT. PRESENT( z2d ) .AND. PRESENT( bottom ) ) THEN
                IF ( no_error( ) ) CALL create_omi_xyz_bini  ( this, bottom(:)  )
             END IF
             IF ( no_error( ) ) CALL set_omi_xyz_l2d_0_1     ( this, l_l2d(:)   )
             IF ( no_error( ) ) CALL set_omi_xyz_l3d_0_1     ( this, l_l3d(:,:) )
             IF ( no_error( ) ) CALL set_omi_xyz_valid_0_1   ( this, l_valid(:) )
             DEALLOCATE ( l_valid, l_l2d, l_l3d )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE create_omi_xyz_no_layers_0
  !
  !! Erzeugen der dynamischen Komponenten "x(:)", "y(:)", "z(:)", "l2d(:)", 
  !! "l3d(:,:)", "valid(:)" und "modif(:)" eines Datenobjektes f&uuml;r 
  !! vorgegebene Orte (x,y). Die vertikalen Koordinaten werden in Abh&auml;ngigkeit
  !! vom Wert der Komponente "ctype" entweder in die Mitte zwischen zwei 
  !! Schichtgrenzen oder auf die Schichtgrenzen gelegt. Ist insbesondere f&uuml;r 
  !! Daten erforderlich, die Tiefeninformation ben&ouml;tigen: <BR>
  !! erforderlich, die Tiefeninformation ben&ouml;tigen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_xyz_with_layers_0 ( this, x2d, y2d, surface, bottom )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(INOUT) :: this       ! 
    !! x-Koordinaten der Orte (x,y) in der Ebene (2D)
    REAL (KIND=Double) , INTENT(IN)    :: x2d(:)     ! 
    !! y-Koordinaten der Orte (x,y) in der Ebene (2D)
    REAL (KIND=Double) , INTENT(IN)    :: y2d(:)     ! 
    !! Lage der Oberfl&auml;che f&uuml;r alle Orte (x,y) <BR>
    !! ... kann bei z-Schichten mit "bottom(:)" identisch sein
    REAL (KIND=Double) , INTENT(IN)    :: surface(:) ! 
    !! Lage des (tiefsten) Bodens f&uuml;r alle Orte (x,y)
    REAL (KIND=Double) , INTENT(IN)    :: bottom(:)  ! 
    !! Name der Subroutine
    CHARACTER (LEN=28) , PARAMETER :: c_upname='create_omi_xyz_with_layers_0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch               ! 
    INTEGER            :: snc ! 
    LOGICAL            :: l_ok(12)         ! 
    LOGICAL            , ALLOCATABLE :: l_valid(:), l_modif(:)         ! 
    INTEGER            , ALLOCATABLE :: l_l2d(:), l_l3d(:,:), l_nc(:)  ! 
    REAL (KIND=Double) , ALLOCATABLE :: l_x(:), l_y(:), l_z(:)         ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok( 1) = ok_omi_xyz_id          ( this )
       l_ok( 2) = ok_omi_xyz_description ( this )
       l_ok( 3) = ok_omi_xyz_refsystem   ( this )
       l_ok( 4) = ok_omi_xyz_ztype       ( this )
       l_ok( 5) = ok_omi_xyz_ctype       ( this )
       l_ok( 6) = ok_omi_xyz_layers      ( this )
       l_ok( 7) = ok_omi_xyz_hlimit      ( this )
       l_ok( 8) = ( ANY( this%ztype == (/1,2/) ) )
       l_ok( 9) = ( SIZE(surface) == SIZE(bottom) )
       l_ok(10) = ok_omi_xyz_surface( this, surface(:) )
       l_ok(11) = ok_omi_xyz_bottom ( this, bottom(:) )
       l_ok(12) = ok_omi_xyz_surface_bottom ( this, surface(:), bottom(:) )
       IF ( .NOT. ALL( l_ok(:) ) ) THEN
          CALL setup_error_act ( all_errors(:), 8510, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
          WRITE(ch(1:1),'(L1)') l_ok( 1) ; CALL setup_error_act ( '<ok-id>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok( 2) ; CALL setup_error_act ( '<ok-description>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok( 3) ; CALL setup_error_act ( '<ok-refsystem>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok( 4) ; CALL setup_error_act ( '<ok-ztype>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok( 5) ; CALL setup_error_act ( '<ok-ctype>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok( 6) ; CALL setup_error_act ( '<ok-layers>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok( 7) ; CALL setup_error_act ( '<ok-hlimit>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok( 9) ; CALL setup_error_act ( '<ok-bot-sur>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok(10) ; CALL setup_error_act ( '<ok-surface>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok(11) ; CALL setup_error_act ( '<ok-bottom>', ch(1:1) )
          WRITE(ch(1:1),'(L1)') l_ok(12) ; CALL setup_error_act ( '<ok-surface-bottom>', ch(1:1) )
          WRITE(ch,'(I10)') this%ztype  ; CALL setup_error_act ( '<ztype>', ch )
          WRITE(ch,'(I10)') this%ctype  ; CALL setup_error_act ( '<ctype>', ch )
       ELSE
          ALLOCATE( l_nc(SIZE(bottom)) )
          l_nc(:) = get_omi_xyz_nof_point ( this, bottom(:) )
          snc     = SUM(l_nc(:))
          ALLOCATE( l_valid(snc), l_modif(snc), l_l2d(snc), l_l3d(snc,this%ctype), l_x(snc), &
               l_y(snc), l_z(snc) ) 
          ! ... Normal-Koordinaten berechnen
          l_x(:)     =  get_omi_xyz_normal_x_coordinate ( this, l_nc(:), snc, x2d(:) )
          l_y(:)     =  get_omi_xyz_normal_y_coordinate ( this, l_nc(:), snc, y2d(:) )
          l_z(:)     =  get_omi_xyz_normal_z_coordinate ( this, l_nc(:), snc, bottom(:), surface(:) )
          l_l2d(:)   =  get_omi_xyz_normal_l2d          ( this, l_nc(:), snc )
          l_l3d(:,:) =  get_omi_xyz_normal_l3d          ( this, l_nc(:), snc, this%ctype )
          l_valid(:) = .true.
          l_modif(:) = .false.
          ! ... Daten in aktuelle Komponente eintragen
          IF ( no_error( ) ) CALL set_omi_xyz_x_0_1       ( this, l_x(:)     )
          IF ( no_error( ) ) CALL set_omi_xyz_y_0_1       ( this, l_y(:)     )
          IF ( no_error( ) ) CALL set_omi_xyz_z_0_1       ( this, l_z(:)     )
          IF ( no_error( ) ) CALL set_omi_xyz_l2d_0_1     ( this, l_l2d(:)   )
          IF ( no_error( ) ) CALL set_omi_xyz_l3d_0_1     ( this, l_l3d(:,:) )
          IF ( no_error( ) ) CALL set_omi_xyz_valid_0_1   ( this, l_valid(:) )
          IF ( no_error( ) ) CALL set_omi_xyz_modif_0_1   ( this, l_modif(:) )
          IF ( no_error( ) ) CALL create_omi_xyz_zmax     ( this             )
          IF ( no_error( ) ) CALL create_omi_xyz_bini     ( this, bottom(:)  )
          DEALLOCATE( l_valid, l_modif, l_l2d, l_l3d, l_x, l_y, l_z ) 
          DEALLOCATE( l_nc )
       END IF
    END IF
    !
  END SUBROUTINE create_omi_xyz_with_layers_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = ( this1%id          == this2%id )
    l_ok(2)  = ( this1%description == this2%description )
    l_ok(3)  = eq_omi_space( this1%refsystem, this2%refsystem )
    l_ok(4)  = ( this1%ztype       == this2%ztype )
    !
    l_ok(5)  = eq_omi_xyz_x      ( this1, this2 )
    l_ok(6)  = eq_omi_xyz_y      ( this1, this2 )
    l_ok(7)  = eq_omi_xyz_z      ( this1, this2 )
    l_ok(8)  = eq_omi_xyz_valid  ( this1, this2 )
    l_ok(9)  = eq_omi_xyz_l3d    ( this1, this2 )
    l_ok(10) = eq_omi_xyz_l2d    ( this1, this2 )
    l_ok(11) = eq_omi_xyz_layers ( this1, this2 )
    l_ok(12) = eq_omi_xyz_modif  ( this1, this2 )
    l_ok(15) = eq_omi_xyz_zmax   ( this1, this2 )
    l_ok(16) = eq_omi_xyz_bini   ( this1, this2 )
    !
    l_ok(13) = ( this1%hlimit      == this2%hlimit )
    l_ok(14) = ( this1%ctype       == this2%ctype  )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_xyz_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_xyz_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_xyz_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_0_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_xyz_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_xyz_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_1_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_xyz_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_xyz_1_1
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
  FUNCTION ne_omi_xyz_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_xyz_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_xyz_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_xyz_1_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_xyz_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_xyz_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_xyz_0_1 ( this1, this2 ) &
      RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_xyz_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_xyz_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_xyz_1_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l     = SIZE(ok) 
    ok(:) = .NOT. eq_omi_xyz_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_xyz_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-Copy-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! kopiere den Inhalt einer Komponente in eine andere Komponente <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_xyz_0_0 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_xyz) , INTENT(OUT) :: this1 ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN)  :: this2 ! 
    !
    CALL new_omi_xyz_0               ( this1 )
    CALL set_omi_xyz_id_0_0          ( this1, this2%id )
    CALL set_omi_xyz_description_0_0 ( this1, this2%description )
    CALL set_omi_xyz_refsystem_0_0   ( this1, this2%refsystem )
    CALL set_omi_xyz_ztype_0_0       ( this1, this2%ztype )
    CALL set_omi_xyz_ctype_0_0       ( this1, this2%ctype )
    CALL set_omi_xyz_hlimit_0_0      ( this1, this2%hlimit )
    IF ( ASSOCIATED( this2%x      ) ) CALL set_omi_xyz_x      ( this1, this2%x )
    IF ( ASSOCIATED( this2%y      ) ) CALL set_omi_xyz_y      ( this1, this2%y )
    IF ( ASSOCIATED( this2%z      ) ) CALL set_omi_xyz_z      ( this1, this2%z )
    IF ( ASSOCIATED( this2%valid  ) ) CALL set_omi_xyz_valid  ( this1, this2%valid ) 
    IF ( ASSOCIATED( this2%l3d    ) ) CALL set_omi_xyz_l3d    ( this1, this2%l3d ) 
    IF ( ASSOCIATED( this2%l2d    ) ) CALL set_omi_xyz_l2d    ( this1, this2%l2d )
    IF ( ASSOCIATED( this2%layers ) ) CALL set_omi_xyz_layers ( this1, this2%layers )
    IF ( ASSOCIATED( this2%modif  ) ) CALL set_omi_xyz_modif  ( this1, this2%modif )
    IF ( ASSOCIATED( this2%zmax   ) ) CALL set_omi_xyz_zmax   ( this1, this2%zmax )
    IF ( ASSOCIATED( this2%bini   ) ) CALL set_omi_xyz_bini   ( this1, this2%bini )
    !
  END SUBROUTINE copy_omi_xyz_0_0
  !
  !! kopiere den Inhalt mehrere Komponente auf andere Komponenten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_xyz_1_1 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_xyz) , INTENT(OUT) :: this1(:) ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN)  :: this2(:) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       CALL copy_omi_xyz_0_0 ( this1(i), this2(i) )
    END DO
    !
  END SUBROUTINE copy_omi_xyz_1_1
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
  FUNCTION ok_initialised ( upname ) &
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_xyz" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_xyz ausfuehren'
       !
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
       !
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION not_initialised ( upname ) &
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
  SUBROUTINE init_omi_xyz_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER  :: c_upname='init_omi_xyz_all_errors' !
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
               '--> INIT_omi_xyz ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_xyz ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "x(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "y(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "z(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "valid(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "l3d(:,:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "l2d(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "layers(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5120 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "modif(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5150 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "zmax(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5170 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "bini(:)"\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "id"\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "description"\n'//&
               'aktuell        = <aktuell>\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "refsystem"\n'//&
               'aktuell        = <aktuell>\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "ztype" muss [0,1,2] sein\n'//&
               'aktuell        = <aktuell>\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'undefined      : <undef>\n'//&
               ' ... beide Felder muessen (nicht) assoziiert sein\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "x(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'associated     : <status>\n'//&
               'size           : <size>\n'//&
               'nof undefinded : <nof>\n'//&
               '--> Daten pruefen / SET_OMI_XYZ_X verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "y(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'associated     : x = <x-status>, y = <y-status> \n'//&
               'size           : x = <x-size>, y = <y-size>\n'//&
               'nof undefined  : x = <x-nof>, y = <y-nof>\n'//&
               '--> Daten pruefen / SET_OMI_XYZ_Y verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "z(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'associated     : x = <x-status>, y = <y-status>, z = <z-status> \n'//&
               'size           : x = <x-size>, y = <y-size>, z = <z-size>\n'//&
               'nof undefined  : x = <x-nof>, y = <y-nof>, z = <z-nof>\n'//&
               '--> Daten pruefen / SET_OMI_XYZ_Z verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "valid(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'associated     : x = <x-status>, valid = <valid-status> \n'//&
               'size           : x = <x-size>, valid = <valid-size>\n'//&
               '--> Daten pruefen / SET_OMI_XYZ_VALID verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "l3d(:,:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'associated     : x = <x-status>, l3d = <l3d-status> \n'//&
               'size           : x = <x-size>, l3d = <l3d-size>\n'//&
               'nof undefined  : x = <x-nof>, l3d = <l3d-nof>\n'//&
               '--> Daten pruefen / SET_OMI_XYZ_L3D verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "l2d(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'associated     : x = <x-status>, l2d = <l2d-status> \n'//&
               'size           : x = <x-size>, l2d = <l2d-size>\n'//&
               'nof undefined  : x = <x-nof>, l2d = <l2d-nof>\n'//&
               '--> Daten pruefen / SET_OMI_XYZ_L2D verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "layers(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'nof layers     = <nof-layers>, Status = <status>\n'//&
               'sorting        = <sorting>\n'//&
               '--> Daten pruefen / SET_OMI_XYZ_LAYERS verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6120 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "modif(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'associated     : x = <x-status>, modif = <modif-status> \n'//&
               'size           : x = <x-size>, modif = <modif-size>\n'//&
               'nof undefined  : x = <x-nof>, modif = <modif-nof>\n'//&
               '--> Daten pruefen / SET_OMI_XYZ_MODIF verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6130 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "hlimit" muss >= 0.0 sein\n'//&
               'aktuell        = <aktuell>\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'undefined      : <undef>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6140 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "ctype" muss [0,1,2] sein\n'//&
               'aktuell        = <aktuell>\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'undefined      : <undef>\n'//&
               'Ztype          : <ztype>, konsistent = <consistent>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6150 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "zmax(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'associated     : l2d = <l2d-status>, zmax = <zmax-status>, z = <z-status> \n'//&
               'size           : l2d = <l2d-size>, zmax = <zmax-size>, z = <z-size>\n'//&
               'nof undefined  : l2d = <l2d-nof>, zmax = <zmax-nof>, z = <z-nof>\n'//&
               'nof invalid z  = <nval> [ Tiefen unterhalb zmax ]\n'//&
               '--> Programm pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6170 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "bini(:)" muss korrekt allokiert sein\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Programm pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6160 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "version" muss [0,1,2,3,...] sein\n'//&
               'aktuell        = <aktuell>\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "description"\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "refsystem"\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "ztype"\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "x(:)", "y(:)" [ und "z(:)"]\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "valid(:)"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "l3d(:,:)"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "l2d(:)"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "layers(:)"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7120 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "modif(:)"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7130 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "hlimit"\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7140 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "ctype"\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7160 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_xyz"\n'//&
               'Typ-Komponente = "version"\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_xyz"\n'//&
               '--> Code in Modul "b_omi_xyz" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "x(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "y(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "z(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "valid(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "l3d(:,:)"\n'//&
               'Dimension      = <idim1>, <idim2>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "l2d(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "layers(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8120 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "modif(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8150 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "zmax(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8170 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_xyz"\n'//&
               'Typ-Komponente = "bini(:)"\n'//&
               'Dimension      = <idim1>\n'//&
               '--> Code in Modul "b_omi_xyz" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CREATE-Methoden\n'//&
               'einzelne Komponenten von "t_omi_xyz" sind nicht o.k.\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Id          : ok = <ok-id>\n'//&
               'Description : ok = <ok-description>\n'//&
               'Ztype       : ok = <ok-ztype>\n'//&
               'Refsys      : ok = <ok-refsystem>\n'//&
               'Ztype       : ok = <ok-ztype>, Ztype = <ztype> muss 0 sein\n'//&
               'Ctype       : ok = <ok-ctype>, Ctype = <ctype> muss 0 sein\n'//&
               '--> Komponenten vorab mit SET-Funktionen korrekt definieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CREATE-Methoden\n'//&
               'einzelne Komponenten von "t_omi_xyz" sind nicht o.k.\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Id          : ok = <ok-id>\n'//&
               'Description : ok = <ok-description>\n'//&
               'Ztype       : ok = <ok-ztype>\n'//&
               'Refsys      : ok = <ok-refsystem>\n'//&
               'Ztype       : ok = <ok-ztype>\n'//&
               'Ctype       : ok = <ok-ctype>\n'//&
               'Layers      : ok = <ok-layers>\n'//&
               'Hlimit      : ok = <ok-hlimit>\n'//&
               'Surface     : ok = <ok-surface>\n'//&
               'Bottom      : ok = <ok-bottom>\n'//&
               'Surf/Bottom : ok = <ok-surface-bottom>\n'//&
               'Ztype       = <ztype>, muss 1 oder 2 sein\n'//&
               'Ctype       = <ctype>, muss 1 oder 2 sein\n'//&
               'bot/sur     : identical size = <ok-bot-sur>\n'//&
               '--> Komponenten vorab mit SET-Funktionen korrekt definieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8520 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CREATE-Methoden\n'//&
               'Daten fuer Normal-X-Koordinaten nicht o.k.\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'nc/x2d      : identical size = <ok-nc-x2d>\n'//&
               'Layers      : ok = <ok-layers>\n'//&
               'Size        : Nc = <nc>, X2d = <x2d> \n'//&
               '--> Komponenten korrekt definieren / Daten uebergeben' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8530 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CREATE-Methoden\n'//&
               'Daten fuer Normal-Y-Koordinaten nicht o.k.\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'nc/y2d      : identical size = <ok-nc-y2d>\n'//&
               'Layers      : ok = <ok-layers>\n'//&
               'Size        : Nc = <nc>, Y2d = <y2d> \n'//&
               '--> Komponenten korrekt definieren / Daten uebergeben' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8540 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CREATE-Methoden\n'//&
               'einzelne Komponenten von "t_omi_xyz" sind nicht o.k.\n'//&
               'Daten fuer Normal-Z-Koordinaten nicht o.k.\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Ztype       : ok = <ok-ztype>\n'//&
               'Ctype       : ok = <ok-ctype>\n'//&
               'Layers      : ok = <ok-layers>\n'//&
               'Ztype       = <ztype>, muss 1 oder 2 sein\n'//&
               'Ctype       = <ctype>, muss 1 oder 2 sein\n'//&
               'bot/sur     : identical size = <ok-bot-sur>\n'//&
               'Size        : Surface = <surface>, Bottom = <bottom> \n'//&
               '--> Komponenten korrekt definieren / Daten uebergeben' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8550 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CREATE-Methoden\n'//&
               'einzelne Komponenten von "t_omi_xyz" sind nicht o.k.\n'//&
               'Daten fuer Normal-L2D-Zeiger nicht o.k.\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Layers      : ok = <ok-layers>\n'//&
               '--> Komponenten korrekt definieren / Daten uebergeben' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8560 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CREATE-Methoden\n'//&
               'einzelne Komponenten von "t_omi_xyz" sind nicht o.k.\n'//&
               'Daten fuer Normal-L3D-Zeiger nicht o.k.\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Layers      : ok = <ok-layers>\n'//&
               '--> Komponenten korrekt definieren / Daten uebergeben' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Laenge des uebergebenen Datenvektors ist nicht o.k.\n'//&
               'Objekt-Id    = <obj-id>\n'//&
               'aktuell      = <act>\n'//&
               'erforderlich = <req> [ aus Komponente "l3d(:,:)" ]\n'//&
               '--> korrekte Daten uebergeben' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -6000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: private OK-Methoden\n'//&
               'Bodenkoordinate(n) liegt nicht im Schichtbereich\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Anzahl      : bot = <bot-count>, top = <top-count>\n'// &
               'Schicht     : bot = <bot-layer>, top = <top-layer>\n'// &
               'Boden       : bot = <bot-bottom>, top = <top-bottom>\n'// &
               '... ggf. sind auch Feldgroessen ungleich\n'// &
               'Laenge      : l2d = <l2d>, bottom = <bottom>\n'// &
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: private OK-Methoden\n'//&
               'Oberflaechenkoordinate(n) liegt nicht im Schichtbereich\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Anzahl      : bot = <bot-count>, top = <top-count>\n'// &
               'Schicht     : bot = <bot-layer>, top = <top-layer>\n'// &
               'Oberflaeche : bot = <bot-surface>, top = <top-surface>\n'// &
               '... ggf. sind auch Feldgroessen ungleich\n'// &
               'Laenge      : l2d = <l2d>, surface = <surface>\n'// &
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: private OK-Methoden\n'//&
               'Oberflaeche liegt unterhalb des Bodens\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Anzahl      : <count>\n'// &
               'Lage        : bot = <bot>, sur = <sur>\n'// &
               '... ggf. sind auch Feldgroessen ungleich\n'// &
               'Laenge      : l2d = <l2d>, surface = <surface>, bottom = <bottom>\n'// &
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: private OK-Methoden\n'//&
               'Bodentiefen liegen tiefer als Komponente "zmax(:)"\n'//&
               'Objekt-Id   = <obj-id>\n'//&
               'Anzahl      = <count>\n'// &
               'Lage        : bot = <x-bot>, zmax = <x-zmax>\n'// &
               '... ggf. sind auch Feldgroessen ungleich\n'// &
               'Laenge      : zmax = <zmax>, bottom = <bottom>\n'// &
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: private UPDATE-Methoden\n'//&
               'Fehler in Komponente von "t_omi_xyz"\n'//&
               'Methode fuer aktuelles Koordinatensystem nicht realisiert\n'// &
               'aktuell = <id>\n'//&
               '--> Programmcode erweitern' )
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
  END SUBROUTINE init_omi_xyz_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_xyz_all_errors &
       ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_xyz_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "x(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_x ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "x(:)": Anzahl der Datenpunkte
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER  :: c_upname='alloc_omi_xyz_x' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%x(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8050, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_x
  !
  !! Allokieren der dynamischen Feld-Komponente "y(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_y ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "y(:)": Anzahl der Datenpunkte
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER  :: c_upname='alloc_omi_xyz_y' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%y(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8060, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_y
  !
  !! Allokieren der dynamischen Feld-Komponente "z(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_z ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "z(:)": Anzahl der Datenpunkte
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER  :: c_upname='alloc_omi_xyz_z' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%z(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8070, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_z
  !
  !! Allokieren der dynamischen Feld-Komponente "valid(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_valid ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "valid(:)"
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_omi_xyz_valid' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%valid(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8080, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_valid
  !
  !! Allokieren der dynamischen Feld-Komponente "l3d(:,:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_l3d ( this, idim1, idim2 )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! erste Dimension der dynamischen Feld-Komponente "l3d(:,:)": Anzahl der Datenpunkte
    INTEGER          , INTENT(IN)    :: idim1  ! 
    !! zweite Dimension der dynamischen Feld-Komponente "l3d(:,:)": max. Anzahl der Interpolationspunkte
    INTEGER          , INTENT(IN)    :: idim2  ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_omi_xyz_l3d' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%l3d(idim1,idim2), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8090, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', ch )
       WRITE(ch,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_l3d
  !
  !! Allokieren der dynamischen Feld-Komponente "l2d(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_l2d ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "l2d(:)": Anzahl der Datenpunkte
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_omi_xyz_l2d' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%l2d(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8100, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_l2d
  !
  !! Allokieren der dynamischen Feld-Komponente "layers(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_layers ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "layers(:)": Anzahl der Schichtgrenzen
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_omi_xyz_layers' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%layers(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8110, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_layers
  !
  !! Allokieren der dynamischen Feld-Komponente "modif(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_modif ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "modif(:)": Anzahl der Datenpunkte
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_omi_xyz_modif' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%modif(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8120, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_modif
  !
  !! Allokieren der dynamischen Feld-Komponente "zmax(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_zmax ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "zmax(:)": Anzahl der Datenpunkte
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_omi_xyz_zmax' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%zmax(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8150, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_zmax
  !
  !! Allokieren der dynamischen Feld-Komponente "bini(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_xyz_bini ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "bini(:)": Anzahl der Datenpunkte
    INTEGER          , INTENT(IN)    :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_omi_xyz_bini' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ch ! 
    !
    ALLOCATE ( this%bini(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8170, c_upname, c_modname, stat )
       WRITE(ch,'(I10)') idim ; CALL setup_error_act ( '<idim1>', ch )
    END IF
    !
  END SUBROUTINE alloc_omi_xyz_bini
  !
  !! Initialisieren der Feld-Komponente "x(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_x ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%x(:) = c_undef_omi_xyz_double
    !
  END SUBROUTINE init_omi_xyz_x
  !
  !! Initialisieren der Feld-Komponente "y(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_y ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%y(:) = c_undef_omi_xyz_double
    !
  END SUBROUTINE init_omi_xyz_y
  !
  !! Initialisieren der Feld-Komponente "z(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_z ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%z(:) = c_undef_omi_xyz_double
    !
  END SUBROUTINE init_omi_xyz_z
  !
  !! Initialisieren der Feld-Komponente "valid(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_valid ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%valid(:) = .true. 
    !
  END SUBROUTINE init_omi_xyz_valid
  !
  !! Initialisieren der Feld-Komponente "l3d(:,:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_l3d ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%l3d(:,:) = c_undef_omi_xyz_int
    !
  END SUBROUTINE init_omi_xyz_l3d
  !
  !! Initialisieren der Feld-Komponente "l2d(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_l2d ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%l2d(:) = c_undef_omi_xyz_int
    !
  END SUBROUTINE init_omi_xyz_l2d
  !
  !! Initialisieren der Feld-Komponente "layers(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_layers ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%layers(:) = c_undef_omi_xyz_double
    !
  END SUBROUTINE init_omi_xyz_layers
  !
  !! Initialisieren der Feld-Komponente "modif(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_modif ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%modif(:) = .false. 
    !
  END SUBROUTINE init_omi_xyz_modif
  !
  !! Initialisieren der Feld-Komponente "zmax(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_zmax ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%zmax(:) = c_undef_omi_xyz_double
    !
  END SUBROUTINE init_omi_xyz_zmax
  !
  !! Initialisieren der Feld-Komponente "bini(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_xyz_bini ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this   ! 
    !
    this%bini(:) = c_undef_omi_xyz_double
    !
  END SUBROUTINE init_omi_xyz_bini
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "x(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_x ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='dealloc_omi_xyz_x' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%x ) ) THEN
       DEALLOCATE ( this%x, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5050, c_upname, c_modname, stat )
       NULLIFY ( this%x ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_x
  !
  !! De-Allokieren der dynamischen Feld-Komponente "y(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_y ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='dealloc_omi_xyz_y' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%y ) ) THEN
       DEALLOCATE ( this%y, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
       NULLIFY ( this%y ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_y
  !
  !! De-Allokieren der dynamischen Feld-Komponente "z(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_z ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='dealloc_omi_xyz_z' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%z ) ) THEN
       DEALLOCATE ( this%z, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5070, c_upname, c_modname, stat )
       NULLIFY ( this%z ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_z
  !
  !! De-Allokieren der dynamischen Feld-Komponente "valid(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_valid ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_omi_xyz_valid' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%valid ) ) THEN
       DEALLOCATE ( this%valid, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5080, c_upname, c_modname, stat )
       NULLIFY ( this%valid ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_valid
  !
  !! De-Allokieren der dynamischen Feld-Komponente "l3d(:,:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_l3d ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_omi_xyz_l3d' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%l3d ) ) THEN
       DEALLOCATE ( this%l3d, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5090, c_upname, c_modname, stat )
       NULLIFY ( this%l3d ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_l3d
  !
  !! De-Allokieren der dynamischen Feld-Komponente "l2d(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_l2d ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_omi_xyz_l2d' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%l2d ) ) THEN
       DEALLOCATE ( this%l2d, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5100, c_upname, c_modname, stat )
       NULLIFY ( this%l2d ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_l2d
  !
  !! De-Allokieren der dynamischen Feld-Komponente "layers(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_layers ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_omi_xyz_layers' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%layers ) ) THEN
       DEALLOCATE ( this%layers, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5110, c_upname, c_modname, stat )
       NULLIFY ( this%layers ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_layers
  !
  !! De-Allokieren der dynamischen Feld-Komponente "modif(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_modif ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_omi_xyz_modif' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%modif ) ) THEN
       DEALLOCATE ( this%modif, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5120, c_upname, c_modname, stat )
       NULLIFY ( this%modif ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_modif
  !
  !! De-Allokieren der dynamischen Feld-Komponente "zmax(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_zmax ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_omi_xyz_zmax' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%zmax ) ) THEN
       DEALLOCATE ( this%zmax, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5150, c_upname, c_modname, stat )
       NULLIFY ( this%zmax ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_zmax
  !
  !! De-Allokieren der dynamischen Feld-Komponente "bini(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_bini ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_omi_xyz_bini' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%bini ) ) THEN
       DEALLOCATE ( this%bini, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5170, c_upname, c_modname, stat )
       NULLIFY ( this%bini ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_xyz_bini
  !
  !! Deallokieren aller dynamischer Komponenten des Typs "t_omi_xyz" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_xyz_dyn_components ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this ! 
    !
    IF ( no_error( ) ) CALL dealloc_omi_xyz_x      ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_y      ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_z      ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_valid  ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_l3d    ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_l2d    ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_layers ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_modif  ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_zmax   ( this )
    IF ( no_error( ) ) CALL dealloc_omi_xyz_bini   ( this )
    !
  END SUBROUTINE dealloc_omi_xyz_dyn_components
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_omi_xyz_id' ! 
    !
    ok = ( LEN_TRIM(this%id) > 0 .AND. &
           this%id(1:LEN(c_undef_omi_xyz_char)) /= c_undef_omi_xyz_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<aktuell>', TRIM(this%id) )
    END IF
    !
  END FUNCTION ok_omi_xyz_id
  !
  !! Pr&uuml;fe, ob die Komponente "description" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_description ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_xyz_description' ! 
    !
    ok = ( LEN_TRIM(this%description) > 0 .AND. &
           this%description(1:LEN(c_undef_omi_xyz_char)) /= c_undef_omi_xyz_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       CALL setup_error_act ( '<aktuell>', TRIM(this%description) )
    END IF
    !
  END FUNCTION ok_omi_xyz_description
  !
  !! Pr&uuml;fe, ob die Komponente "refsystem" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_refsystem ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='ok_omi_xyz_refsystem' ! 
    !
    ok = ok_omi_space ( this%refsystem )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       CALL setup_error_act ( '<aktuell>', get_omi_space_id ( this%refsystem ) )
    END IF
    !
  END FUNCTION ok_omi_xyz_refsystem
  !
  !! Pr&uuml;fe, ob die Komponente "ztype" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_ztype ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_omi_xyz_ztype' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch      ! 
    LOGICAL            :: l_ok(2) ! 
    !
    l_ok(1) = ( this%ztype /= c_undef_omi_xyz_int )
    l_ok(2) = ( ANY( (/0,1,2/) == this%ztype ) )
    ok      = ALL( l_ok(:) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') this%ztype  ; CALL setup_error_act ( '<aktuell>', ch )
       WRITE(ch(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<undef>', ch(1:1) )
    END IF
    !
  END FUNCTION ok_omi_xyz_ztype
  !
  !! Pr&uuml;fe, ob die Komponente "x(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_x ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_omi_xyz_x' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    LOGICAL            :: as ! 
    INTEGER            :: in, un ! 
    !
    as = ASSOCIATED( this%x ) ; in = -1 ; un = -1
    IF ( as ) THEN
       in = SIZE( this%x )
       un = COUNT( this%x == c_undef_omi_xyz_double )
    END IF

    ok = ( as .AND. in > 0 .AND. un == 0 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(L1)' ) as ; CALL setup_error_act ( '<status>', ch )
       WRITE(ch,'(I10)') in ; CALL setup_error_act ( '<size>', ch )
       WRITE(ch,'(I10)') un ; CALL setup_error_act ( '<nof>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_x
  !
  !! Pr&uuml;fe, ob die Komponente "y(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_y ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_omi_xyz_y' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    LOGICAL            :: as(2) ! 
    INTEGER            :: in(2), un(2) ! 
    !
    as(1) = ASSOCIATED( this%x ) ; in(1) = -1 ; un(1) = -1
    as(2) = ASSOCIATED( this%y ) ; in(2) = -1 ; un(2) = -1
    IF ( as(1) ) THEN
       in(1) = SIZE( this%x )
       un(1) = COUNT( this%x == c_undef_omi_xyz_double )
    END IF
    IF ( as(2) ) THEN
       in(2) = SIZE( this%y )
       un(2) = COUNT( this%y == c_undef_omi_xyz_double )
    END IF
    !
    ok = ( ALL( as(:) ) .AND. ALL( in(:) > 0 ) .AND. ALL( in(:) == in(1) ) .AND. ALL( un(:) == 0 ) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(L1)' ) as(1) ; CALL setup_error_act ( '<x-status>', ch )
       WRITE(ch,'(I10)') in(1) ; CALL setup_error_act ( '<x-size>', ch )
       WRITE(ch,'(I10)') un(1) ; CALL setup_error_act ( '<x-nof>', ch )
       WRITE(ch,'(L1)' ) as(2) ; CALL setup_error_act ( '<y-status>', ch )
       WRITE(ch,'(I10)') in(2) ; CALL setup_error_act ( '<y-size>', ch )
       WRITE(ch,'(I10)') un(2) ; CALL setup_error_act ( '<y-nof>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_y
  !
  !! Pr&uuml;fe, ob die Komponente "z(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_z ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_omi_xyz_z' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    LOGICAL            :: as(3) ! 
    INTEGER            :: in(3) ! 
    INTEGER            :: un(3) ! 
    !
    as(1) = ASSOCIATED( this%x ) ; in(1) = -1 ; un(1) = -1
    as(2) = ASSOCIATED( this%y ) ; in(2) = -1 ; un(2) = -1
    as(3) = ASSOCIATED( this%z ) ; in(3) = -1 ; un(3) = -1
    IF ( as(1) ) THEN
       in(1) = SIZE( this%x )
       un(1) = COUNT( this%x == c_undef_omi_xyz_double )
    END IF
    IF ( as(2) ) THEN
       in(2) = SIZE( this%y )
       un(2) = COUNT( this%y == c_undef_omi_xyz_double )
    END IF
    IF ( as(3) ) THEN
       in(3) = SIZE( this%z )
       un(3) = COUNT( this%z == c_undef_omi_xyz_double )
    END IF
    !
    IF ( .NOT. as(3) ) THEN
       ok = .true.
    ELSE
       ok = ( ALL( as(:) ) .AND. ALL( in(:) > 0 ) .AND. ALL( in(:) == in(1) ) .AND. ALL( un(:) == 0 ) )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
          WRITE(ch,'(L1)' ) as(1) ; CALL setup_error_act ( '<x-status>', ch )
          WRITE(ch,'(I10)') in(1) ; CALL setup_error_act ( '<x-size>', ch )
          WRITE(ch,'(I10)') un(1) ; CALL setup_error_act ( '<x-nof>', ch )
          WRITE(ch,'(L1)' ) as(2) ; CALL setup_error_act ( '<y-status>', ch )
          WRITE(ch,'(I10)') in(2) ; CALL setup_error_act ( '<y-size>', ch )
          WRITE(ch,'(I10)') un(2) ; CALL setup_error_act ( '<y-nof>', ch )
          WRITE(ch,'(L1)' ) as(3) ; CALL setup_error_act ( '<z-status>', ch )
          WRITE(ch,'(I10)') in(3) ; CALL setup_error_act ( '<z-size>', ch )
          WRITE(ch,'(I10)') un(3) ; CALL setup_error_act ( '<z-nof>', ch )
       END IF
    END IF
    !
  END FUNCTION ok_omi_xyz_z
  !
  !! Pr&uuml;fe, ob die Komponente "valid(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_valid ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_omi_xyz_valid' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    LOGICAL            :: as(2) ! 
    INTEGER            :: in(2) ! 
    !
    as(1) = ASSOCIATED( this%x     ) ; in(1) = -1
    as(2) = ASSOCIATED( this%valid ) ; in(2) = -1
    IF ( as(1) ) THEN
       in(1) = SIZE( this%x     )
    END IF
    IF ( as(2) ) THEN
       in(2) = SIZE( this%valid )
    END IF
    !
    ok = ( ALL( as(:) ) .AND. ALL( in(:) > 0 ) .AND. ALL( in(:) == in(1) ) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6080, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(L1)' ) as(1) ; CALL setup_error_act ( '<x-status>', ch )
       WRITE(ch,'(I10)') in(1) ; CALL setup_error_act ( '<x-size>', ch )
       WRITE(ch,'(L1)' ) as(2) ; CALL setup_error_act ( '<valid-status>', ch )
       WRITE(ch,'(I10)') in(2) ; CALL setup_error_act ( '<valid-size>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_valid
  !
  !! Pr&uuml;fe, ob die Komponente "l3d(:,:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_l3d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_omi_xyz_l3d' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    LOGICAL            :: as(2) ! 
    INTEGER            :: in(2) ! 
    INTEGER            :: un(2) ! 
    !
    as(1) = ASSOCIATED( this%x   ) ; in(1) = -1 ; un(1) = -1
    as(2) = ASSOCIATED( this%l3d ) ; in(2) = -1 ; un(2) = -1
    IF ( as(1) ) THEN
       in(1) = SIZE( this%x      )
       un(1) = COUNT( this%x == c_undef_omi_xyz_double )
    END IF
    IF ( as(2) ) THEN
       in(2) = SIZE( this%l3d, 1 )
       un(2) = COUNT( this%l3d == c_undef_omi_xyz_int )
    END IF
    !
    ok = ( ALL( as(:) ) .AND. ALL( in(:) > 0 ) .AND. ALL( in(:) == in(1) ) .AND. ALL( un(:) == 0 ) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6090, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(L1)' ) as(1) ; CALL setup_error_act ( '<x-status>', ch )
       WRITE(ch,'(I10)') in(1) ; CALL setup_error_act ( '<x-size>', ch )
       WRITE(ch,'(I10)') un(1) ; CALL setup_error_act ( '<x-nof>', ch )
       WRITE(ch,'(L1)' ) as(2) ; CALL setup_error_act ( '<l3d-status>', ch )
       WRITE(ch,'(I10)') in(2) ; CALL setup_error_act ( '<l3d-size>', ch )
       WRITE(ch,'(I10)') un(2) ; CALL setup_error_act ( '<l3d-nof>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_l3d
  !
  !! Pr&uuml;fe, ob die Komponente "l2d(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_l2d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_omi_xyz_l2d' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    LOGICAL            :: as(2) ! 
    INTEGER            :: in(2) ! 
    INTEGER            :: un(2) ! 
    !
    as(1) = ASSOCIATED( this%x   ) ; in(1) = -1 ; un(1) = -1
    as(2) = ASSOCIATED( this%l2d ) ; in(2) = -1 ; un(2) = -1
    IF ( as(1) ) THEN
       in(1) = SIZE( this%x )
       un(1) = COUNT( this%x == c_undef_omi_xyz_double )
    END IF
    IF ( as(2) ) THEN
       in(2) = SIZE( this%l2d )
       un(2) = COUNT( this%l2d == c_undef_omi_xyz_int )
    END IF
    ok = ( ALL( as(:) ) .AND. ALL( in(:) > 0 ) .AND. ALL( in(:) == in(1) ) .AND. ALL( un(:) == 0 ) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(L1)' ) as(1) ; CALL setup_error_act ( '<x-status>', ch )
       WRITE(ch,'(I10)') in(1) ; CALL setup_error_act ( '<x-size>', ch )
       WRITE(ch,'(I10)') un(1) ; CALL setup_error_act ( '<x-nof>', ch )
       WRITE(ch,'(L1)' ) as(2) ; CALL setup_error_act ( '<l2d-status>', ch )
       WRITE(ch,'(I10)') in(2) ; CALL setup_error_act ( '<l2d-size>', ch )
       WRITE(ch,'(I10)') un(2) ; CALL setup_error_act ( '<l2d-nof>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_l2d
  !
  !! Pr&uuml;fe, ob die Komponente "layers(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_layers ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_xyz_layers' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch          ! 
    LOGICAL            :: as, l_ok(2) ! 
    INTEGER            :: nk          ! 
    !
    as = ASSOCIATED( this%layers )
    !
    IF ( .NOT. as .AND. this%ztype == 0 ) THEN
       ok = .true.
    ELSE
       IF ( as ) THEN
          nk      = SIZE( this%layers )
          l_ok(1) = ( nk > 1 )
          IF      ( has_omi_space_altitude( this%refsystem ) ) THEN
             l_ok(2) = ( ALL( this%layers(1:nk-1) - this%layers(2:nk) < 0.0_Double ) )
          ELSE IF ( has_omi_space_depth( this%refsystem )    ) THEN
             l_ok(2) = ( ALL( this%layers(1:nk-1) - this%layers(2:nk) > 0.0_Double ) )
          ELSE
             CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
             CALL setup_error_act ( '<id>', TRIM(get_omi_space_id( this%refsystem )) )
             ok = .false. 
          END IF
       ELSE
          nk = 0 ; l_ok(:) = .false.
       END IF
       IF ( no_error( ) ) THEN
          ok = ALL( l_ok(:) )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6110, c_upname, c_modname )
             CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
             WRITE(ch,'(I10)') nk ; CALL setup_error_act ( '<nof-layers>', ch )
             WRITE(ch(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<status>', ch(1:1) )
             WRITE(ch(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<sorting>', ch(1:1) )
          END IF
       END IF
    END IF
    !
  END FUNCTION ok_omi_xyz_layers
  !
  !! Pr&uuml;fe, ob die Komponente "modif(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_modif ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_omi_xyz_modif' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    LOGICAL            :: as(2) ! 
    INTEGER            :: in(2) ! 
    INTEGER            :: un(2) ! 
    !
    as(1) = ASSOCIATED( this%x     ) ; in(1) = -1 ; un(1) = -1
    as(2) = ASSOCIATED( this%modif ) ; in(2) = -1 ; un(2) = -1
    IF ( as(1) ) THEN
       in(1) = SIZE( this%x )
       un(1) = COUNT( this%x == c_undef_omi_xyz_double )
    END IF
    IF ( as(2) ) THEN
       in(2) = SIZE( this%modif )
       un(2) = 0
    END IF
    !
    IF ( .NOT. as(2) ) THEN
       ok = .true.
    ELSE
       ok = ( ALL( as(:) ) .AND. ALL( in(:) > 0 ) .AND. ALL( in(:) == in(1) ) .AND. ALL( un(:) == 0 ) )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6120, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
          WRITE(ch,'(L1)' ) as(1) ; CALL setup_error_act ( '<x-status>', ch )
          WRITE(ch,'(I10)') in(1) ; CALL setup_error_act ( '<x-size>', ch )
          WRITE(ch,'(I10)') un(1) ; CALL setup_error_act ( '<x-nof>', ch )
          WRITE(ch,'(L1)' ) as(2) ; CALL setup_error_act ( '<modif-status>', ch )
          WRITE(ch,'(I10)') in(2) ; CALL setup_error_act ( '<modif-size>', ch )
          WRITE(ch,'(I10)') un(2) ; CALL setup_error_act ( '<modif-nof>', ch )
       END IF
    END IF
    !
  END FUNCTION ok_omi_xyz_modif
  !
  !! Pr&uuml;fe, ob die Komponente "hlimit" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_hlimit ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_xyz_hlimit' ! 
    !! Hilfsvariable
    CHARACTER (LEN=15) :: ch      ! 
    INTEGER            :: un      ! 
    !
    ok = ( this%hlimit /= c_undef_omi_xyz_double .AND. this%hlimit >= 0.0_Double )
    un = MERGE( 1, 0, this%hlimit == c_undef_omi_xyz_double )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6130, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(G15.6)') this%hlimit  ; CALL setup_error_act ( '<aktuell>', ch )
       WRITE(ch(1:1),'(I1)') un  ; CALL setup_error_act ( '<undef>', ch(1:1) )
    END IF
    !
  END FUNCTION ok_omi_xyz_hlimit
  !
  !! Pr&uuml;fe, ob die Komponente "ctype" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_ctype ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_omi_xyz_ctype' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch      ! 
    INTEGER            :: un      ! 
    LOGICAL            :: l_ok(2) ! 
    !
    l_ok(1) = ( this%ctype /= c_undef_omi_xyz_int .AND. ANY( this%ctype == (/0,1,2/) ) )
    l_ok(2) = MERGE( this%ctype == 0, this%ctype > 0, this%ztype == 0 ) 
    un      = MERGE( 1, 0, this%ctype == c_undef_omi_xyz_int )
    ok      = ALL( l_ok(:) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6140, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') this%ctype  ; CALL setup_error_act ( '<aktuell>', ch )
       WRITE(ch,'(I10)') this%ztype  ; CALL setup_error_act ( '<ztype>', ch )
       WRITE(ch(1:1),'(I1)') un  ; CALL setup_error_act ( '<undef>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<consistent>', ch(1:1) )
    END IF
    !
  END FUNCTION ok_omi_xyz_ctype
  !
  !! Pr&uuml;fe, ob die Komponente "version" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_version ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_omi_xyz_version' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch      ! 
    !
    ok = this%version >= 0
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6160, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch,'(I10)') this%ctype  ; CALL setup_error_act ( '<aktuell>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_version
  !
  !! Pr&uuml;fe, ob die Komponente "zmax(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_zmax ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_omi_xyz_zmax' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    LOGICAL            :: as(3)       ! 
    INTEGER            :: in(3)       ! 
    INTEGER            :: un(3), nval ! 
    !
    SELECT CASE ( this%ztype )
    CASE ( 1 )
       IF ( SIZE(this%layers) > 2 ) THEN
          as(1) = ASSOCIATED( this%l2d  ) ; in(1) = -1 ; un(1) = -1
          as(2) = ASSOCIATED( this%zmax ) ; in(2) = -1 ; un(2) = -1
          as(3) = ASSOCIATED( this%z    ) ; in(3) = -1 ; un(3) = -1
          IF ( as(1) ) THEN
             in(1) = MAXVAL( this%l2d )
             un(1) = COUNT( this%l2d == c_undef_omi_xyz_int )
          END IF
          IF ( as(2) ) THEN
             in(2) = SIZE( this%zmax )
             un(2) = COUNT( this%zmax == c_undef_omi_xyz_double )
          END IF
          IF ( as(3) ) THEN
             in(3) = SIZE( this%z )
             un(3) = COUNT( this%z == c_undef_omi_xyz_double )
             IF      ( has_omi_space_depth( this%refsystem )    ) THEN
                nval = COUNT( this%z(:) - this%zmax(this%l2d(:)) > 0.0_Double )
             ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                nval = COUNT( this%z(:) - this%zmax(this%l2d(:)) < 0.0_Double )
             END IF
          END IF
          !
          ok = ( ALL(as) .AND. ALL(in>0) .AND. ALL(in(1:2)==in(1)) .AND. ALL(un==0) .AND. nval==0 )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6150, c_upname, c_modname )
             CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
             WRITE(ch,'(L1)' ) as(1) ; CALL setup_error_act ( '<l2d-status>', ch )
             WRITE(ch,'(I10)') in(1) ; CALL setup_error_act ( '<l2d-size>', ch )
             WRITE(ch,'(I10)') un(1) ; CALL setup_error_act ( '<l2d-nof>', ch )
             WRITE(ch,'(L1)' ) as(2) ; CALL setup_error_act ( '<zmax-status>', ch )
             WRITE(ch,'(I10)') in(2) ; CALL setup_error_act ( '<zmax-size>', ch )
             WRITE(ch,'(I10)') un(2) ; CALL setup_error_act ( '<zmax-nof>', ch )
             WRITE(ch,'(L1)' ) as(3) ; CALL setup_error_act ( '<z-status>', ch )
             WRITE(ch,'(I10)') in(3) ; CALL setup_error_act ( '<z-size>', ch )
             WRITE(ch,'(I10)') un(3) ; CALL setup_error_act ( '<z-nof>', ch )
             WRITE(ch,'(I10)') nval  ; CALL setup_error_act ( '<nval>', ch )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
    END SELECT
    !
  END FUNCTION ok_omi_xyz_zmax
  !
  !! Pr&uuml;fe, ob die Komponente "bini(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_bini ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_omi_xyz_bini' ! 
    !
    ok = .true.
    !
  END FUNCTION ok_omi_xyz_bini
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_id ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_omi_xyz_id' ! 
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
  END SUBROUTINE print_omi_xyz_id
  !
  !! Drucke den Inhalt der Komponente "description" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_description ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25), PARAMETER :: c_upname='print_omi_xyz_description' ! 
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
  END SUBROUTINE print_omi_xyz_description
  !
  !! Drucke den Inhalt der Komponente "refsystem" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_refsystem ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='print_omi_xyz_refsystem' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) 
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
    CALL print_omi_space ( this%refsystem )
    !
8000 FORMAT &
          ('# Inhalt der Komponente refsystem - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_refsystem
  !
  !! Drucke den Inhalt der Komponente "ztype" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_ztype ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_omi_xyz_ztype' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%ztype
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente ztype - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',I10,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_ztype
  !
  !! Drucke den Inhalt der Komponente "x(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_xyz ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_omi_xyz_xyz' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%x ) .AND. ASSOCIATED( this%y ) ) THEN
       IF      ( ASSOCIATED( this%z ) .AND. .NOT. ASSOCIATED( this%zmax) ) THEN
          WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
          ELSE
             DO i=1,MIN(SIZE(this%x),SIZE(this%y),SIZE(this%z))
                IF ( stat /= 0 ) EXIT
                WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%x(i),this%y(i),this%z(i)
             END DO
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
                CALL setup_error_act ( '<action>', 'x-y-z-Zeile (8001)' )
             END IF
          END IF
       ELSE IF ( ASSOCIATED( this%z ) .AND.       ASSOCIATED( this%zmax) ) THEN
          WRITE (UNIT=prn_lun,FMT=8030,IOSTAT=stat) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Header-Zeile (8030)' )
          ELSE
             DO i=1,MIN(SIZE(this%x),SIZE(this%y),SIZE(this%z),SIZE(this%l2d))
                IF ( stat /= 0 ) EXIT
                WRITE(UNIT=prn_lun,FMT=8031,IOSTAT=stat) i, this%x(i),this%y(i),this%z(i),this%zmax(this%l2d(i))
             END DO
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
                CALL setup_error_act ( '<action>', 'x-y-z-Zeile (8031)' )
             END IF
          END IF
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8010, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Header-Zeile (8010)' )
          ELSE
             DO i=1,MIN(SIZE(this%x),SIZE(this%y))
                IF ( stat /= 0 ) EXIT
                WRITE(UNIT=prn_lun,FMT=8011,IOSTAT=stat) i, this%x(i),this%y(i)
             END DO
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
                CALL setup_error_act ( '<action>', 'x-y-Zeile (8011)' )
             END IF
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
          ('# Inhalt der Komponenten x(:), y(:) und z(:)  - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', x = ',F15.3,', y = ',F15.3,', z = ',F15.3)
8010 FORMAT &
          ('# Inhalt der Komponenten x(:) und y(:) - - - -- - - - - - - - ')
8011 FORMAT &
          ('# i = ',I10,', x = ',F15.3,', y = ',F15.3)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
8030 FORMAT &
          ('# Inhalt der Komponenten x(:), y(:), z(:) und zmax(:) - - - - ')
8031 FORMAT &
          ('# i = ',I10,', x = ',F15.3,', y = ',F15.3,', z = ',F15.3,', zmax = ',F15.3)
    !
  END SUBROUTINE print_omi_xyz_xyz
  !
  !! Drucke den Inhalt der Komponente "valid(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_valid ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_omi_xyz_valid' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    INTEGER :: i !  
    !
    IF ( ASSOCIATED( this%valid ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7080, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%valid)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%valid(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7080, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'valid-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7080, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente valid(:)  - - - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', valid = ',L1)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_valid
  !
  !! Drucke den Inhalt der Komponente "l3d(:,:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_l3d ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_omi_xyz_l3d' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    INTEGER :: i !  
    !
    IF ( ASSOCIATED( this%l3d ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7090, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%valid)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%l3d(i,:)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7090, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'valid-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7090, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente l3d(:,:)  - - - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', l3d = ',6I10)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_l3d
  !
  !! Drucke den Inhalt der Komponente "l2d(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_l2d ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_omi_xyz_l2d' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    INTEGER :: i !  
    !
    IF ( ASSOCIATED( this%l2d ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7100, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%valid)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%l2d(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7100, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'valid-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7100, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente l2d(:)- - - - - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', l2d = ',I10)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_l2d
  !
  !! Drucke den Inhalt der Komponente "layers(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_layers ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=20), PARAMETER :: c_upname='print_omi_xyz_layers' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    INTEGER :: i !  
    !
    IF ( ASSOCIATED( this%layers ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7110, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%layers)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%layers(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7110, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'valid-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7110, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente layers(:) - - - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', layer = ',G15.6)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_layers
  !
  !! Drucke den Inhalt der Komponente "modif(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_modif ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_omi_xyz_modif' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    INTEGER :: i !  
    !
    IF ( ASSOCIATED( this%modif ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7120, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          DO i=1,SIZE(this%modif)
             IF ( stat /= 0 ) EXIT
             WRITE(UNIT=prn_lun,FMT=8001,IOSTAT=stat) i, this%modif(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7120, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'valid-Zeile (8001)' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7120, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente modif(:)  - - - - - - - - - - - - - - ')
8001 FORMAT &
          ('# i = ',I10,', modif = ',L1)
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_modif
  !
  !! Drucke den Inhalt der Komponente "hlimit" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_hlimit ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_omi_xyz_hlimit' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%hlimit
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7130, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente hlimit  - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',G15.6,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_hlimit
  !
  !! Drucke den Inhalt der Komponente "ctype" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_ctype ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_omi_xyz_ctype' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%ctype
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7140, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente ctype - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',I10,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_ctype
  !
  !! Drucke den Inhalt der Komponente "version" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_xyz_version ( this )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='print_omi_xyz_version' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%version
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7160, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente version - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',I10,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_xyz_version
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! Ermittle die Anzahl der Datenpunkte aus der Schichtstruktur und dem Boden <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_omi_xyz_nof_point_b ( this, bottom ) &
       RESULT( res )
    !! Datenobjekt
    TYPE (t_omi_xyz)   , INTENT(IN) :: this      ! 
    !! Lage des Bodens f&uuml;r alle Orte (x,y)
    REAL (KIND=Double) , INTENT(IN) :: bottom(:) ! 
    !! Anzahl der vertikalen Datenpunkte f&uuml;r alle Schichten 
    INTEGER :: res(SIZE(bottom)) ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='get_omi_xyz_nof_point_b' ! 
    !! Hilfsvariable 
    INTEGER :: i ! 
    !
    IF ( has_omi_xyz_sigma_layers_0( this ) ) THEN
       DO i=1,SIZE(res)
          res(i) = SIZE( this%layers ) - 1
       END DO
    ELSE IF ( has_omi_xyz_z_layers_0 ( this ) ) THEN
       IF      ( has_omi_space_altitude ( this%refsystem ) ) THEN
          DO i=1,SIZE(res)
             res(i) = MAX(1, COUNT( this%layers(2:) - bottom(i) >= this%hlimit ) )
          END DO
       ELSE IF ( has_omi_space_depth    ( this%refsystem ) ) THEN
          DO i=1,SIZE(res)
             res(i) = MAX(1, COUNT( bottom(i) - this%layers(2:) >= this%hlimit ) )
          END DO
       ELSE
          CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
          CALL setup_error_act ( '<id>', TRIM(get_omi_space_id( this%refsystem )) )
       END IF
    END IF
    !
    IF      ( this%ctype == 1 ) THEN
       CONTINUE
    ELSE IF ( this%ctype == 2 ) THEN
       DO i=1,SIZE(res)
          res(i) = res(i) + 1
       END DO
    END IF
    !
  END FUNCTION get_omi_xyz_nof_point_b
  !
  !! Ermittle die Anzahl der Datenpunkte aus der Komponente "l2d(:)" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_nof_point_l ( this, nres ) &
       RESULT( res )
    !! Datenobjekt
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Anzahl der 2D-Punkte
    INTEGER          , INTENT(IN) :: nres ! 
    !! Anzahl der vertikalen Datenpunkte f&uuml;r alle Schichten 
    INTEGER :: res(nres) ! 
    !! Hilfsvariable 
    INTEGER :: i !  
    !
    res(:) = 0
    !
    DO i=1,SIZE(this%l2d)
       res(this%l2d(i)) = res(this%l2d(i)) + 1
    END DO
    !
  END FUNCTION get_omi_xyz_nof_point_l
  !
  !! Ermittle die Normal-X-Koordinate f&uuml;r alle Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_normal_x_coordinate ( this, nc, nx, x2d ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this   !
    !! Anzahl der Schichten an den verschiedenen Positionen (x,y)
    INTEGER            , INTENT(IN) :: nc(:)  ! 
    !! Gr&ouml;&szlig;e des Feldes "res"
    INTEGER            , INTENT(IN) :: nx     ! 
    !! x-Koordinaten der Datenpunkte (x,y)
    REAL (KIND=Double) , INTENT(IN) :: x2d(:) ! 
    !! Ergebnis: x-Koordinate
    REAL (KIND=Double) :: res(nx) ! 
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='get_omi_xyz_normal_x_coordinate' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                  ! 
    INTEGER            :: i, j, n3, nnc, nx2d ! 
    LOGICAL            :: l_ok(2)             ! 
    !
    nnc     = SIZE(nc)
    nx2d    = SIZE(x2d)
    l_ok(1) = ( nnc == nx2d )
    l_ok(2) = ok_omi_xyz_layers ( this )
    !
    IF ( ALL( l_ok ) ) THEN
       !
       n3 = 0
       DO i=1,nnc
          DO j=1,nc(i)
             n3      = n3+1
             res(n3) = x2d(i)
          END DO
       END DO
       !
    ELSE
       !
       CALL setup_error_act ( all_errors(:), 8520, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-nc-x2d>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-layers>', ch(1:1) )
       WRITE(ch,'(I10)') nnc         ; CALL setup_error_act ( '<nc>', ch )
       WRITE(ch,'(I10)') nx2d        ; CALL setup_error_act ( '<nx2d>', ch )
       !
    END IF
    !
  END FUNCTION get_omi_xyz_normal_x_coordinate 
  !
  !! Ermittle die Normal-Y-Koordinate f&uuml;r alle Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_normal_y_coordinate ( this, nc, ny, y2d ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this   !
    !! Anzahl der Schichten an den verschiedenen Positionen (x,y)
    INTEGER            , INTENT(IN) :: nc(:)  ! 
    !! Gr&ouml;&szlig;e des Feldes "res"
    INTEGER            , INTENT(IN) :: ny     ! 
    !! y-Koordinaten der Datenpunkte (x,y)
    REAL (KIND=Double) , INTENT(IN) :: y2d(:) ! 
    !! Ergebnis: y-Koordinate
    REAL (KIND=Double) :: res(ny) ! 
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='get_omi_xyz_normal_y_coordinate' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                  ! 
    INTEGER            :: i, j, n3, nnc, ny2d ! 
    LOGICAL            :: l_ok(2)             ! 
    !
    nnc     = SIZE(nc)
    ny2d    = SIZE(y2d)
    l_ok(1) = ( nnc == ny2d )
    l_ok(2) = ok_omi_xyz_layers ( this )
    !
    IF ( ALL( l_ok ) ) THEN
       !
       n3 = 0
       DO i=1,nnc
          DO j=1,nc(i)
             n3      = n3+1
             res(n3) = y2d(i)
          END DO
       END DO
       !
    ELSE
       !
       CALL setup_error_act ( all_errors(:), 8530, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-nc-y2d>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-layers>', ch(1:1) )
       WRITE(ch,'(I10)') nnc         ; CALL setup_error_act ( '<nc>', ch )
       WRITE(ch,'(I10)') ny2d        ; CALL setup_error_act ( '<ny2d>', ch )
       !
    END IF
    !
  END FUNCTION get_omi_xyz_normal_y_coordinate 
  !
  !! Ermittle die Normal-Koordinate f&uuml;r alle Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_normal_z_coordinate ( this, nc, nz, bottom, surface ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)              , INTENT(IN) :: this       !
    !! Anzahl der Schichten an den verschiedenen Positionen (x,y)
    INTEGER                       , INTENT(IN) :: nc(:)      ! 
    !! Gr&ouml;&szlig;e des Feldes "res"
    INTEGER                       , INTENT(IN) :: nz         ! 
    !! z-Koordinaten des Bodens (nur f&uuml;r sigma-Schichten)
    REAL (KIND=Double) , OPTIONAL , INTENT(IN) :: bottom(:)  ! 
    !! z-Koordinaten der Oberfl&auml;che (nur f&uuml;r sigma-Schichten)
    REAL (KIND=Double) , OPTIONAL , INTENT(IN) :: surface(:) ! 
    !! Ergebnis: z-Koordinate
    REAL (KIND=Double) :: res(nz) ! 
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='get_omi_xyz_normal_z_coordinate' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    INTEGER            :: i, j, k, n3, nk, ns, nb ! 
    LOGICAL            :: ps, pb, l_ok(4) ! 
    !
    ps      = PRESENT(surface) ; ns = -1
    pb      = PRESENT(bottom)  ; nb = -1
    l_ok(1) = ok_omi_xyz_ztype  ( this )
    l_ok(2) = ok_omi_xyz_ctype  ( this )
    l_ok(3) = ok_omi_xyz_layers ( this )
    SELECT CASE ( this%ztype )
    CASE ( 1 )
       l_ok(4) = .true.
    CASE ( 2 )
       IF ( PRESENT(surface) .AND. PRESENT(bottom) ) THEN
          ns      = SIZE(surface)
          nb      = SIZE(bottom )
          l_ok(4) = ( ns == nb )
       ELSE
          l_ok(4) = .false.
       END IF
    CASE DEFAULT
       l_ok(4) = .false.
    END SELECT
    !
    IF ( ALL( l_ok(:) ) ) THEN
       !
       n3 = 0
       nk = SIZE(this%layers)
       !
       SELECT CASE ( this%ctype )
       CASE ( 1 ) ! schichtmittige Anordnung
          SELECT CASE ( this%ztype) 
          CASE ( 1 )  ! z-Schichten
             DO i=1,SIZE(nc)
                DO j=1,nc(i)
                   k       = nk-j
                   n3      = n3+1
                   res(n3) = 0.5_Double*(this%layers(k)+this%layers(k+1))
                END DO
             END DO
          CASE ( 2 )  ! sigma-Schichten
             DO i=1,SIZE(nc)
                DO j=1,nc(i)
                   k       = nk-j
                   n3      = n3+1
                   res(n3) = surface(i) + 0.5_Double*(this%layers(k)+this%layers(k+1))*(bottom(i)-surface(i))
                END DO
             END DO
          CASE DEFAULT
             res(:) = c_undef_omi_xyz_double
          END SELECT
       CASE ( 2 ) ! schichtgrenzenorientierte Positionierung
          SELECT CASE ( this%ztype) 
          CASE ( 1 )  ! z-Schichten
             DO i=1,SIZE(nc)
                DO j=1,nc(i)
                   k       = nk-j
                   n3      = n3+1
                   res(n3) = this%layers(k+1)
                END DO
             END DO
          CASE ( 2 )  ! sigma-Schichten
             DO i=1,SIZE(nc)
                DO j=1,nc(i)
                   k       = nk-j
                   n3      = n3+1
                   res(n3) = surface(i) + this%layers(k+1)*(bottom(i)-surface(i))
                END DO
             END DO
          CASE DEFAULT
             res(:) = c_undef_omi_xyz_double
          END SELECT
       CASE DEFAULT
          res(:) = c_undef_omi_xyz_double
       END SELECT
       !
    ELSE
       !
       CALL setup_error_act ( all_errors(:), 8540, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-ztype>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-ctype>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-layers>', ch(1:1) )
       WRITE(ch(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<ok-bot-sur>', ch(1:1) )
       WRITE(ch,'(I10)') this%ztype  ; CALL setup_error_act ( '<ztype>', ch )
       WRITE(ch,'(I10)') this%ctype  ; CALL setup_error_act ( '<ctype>', ch )
       WRITE(ch,'(I10)') ns          ; CALL setup_error_act ( '<surface>', ch )
       WRITE(ch,'(I10)') nb          ; CALL setup_error_act ( '<bottom>', ch )
       !
    END IF
    !
  END FUNCTION get_omi_xyz_normal_z_coordinate 
  !
  !! Ermittle die Zeiger auf die 2D-Positionen f&uuml;r alle Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_normal_l2d ( this, nc, nz ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)              , INTENT(IN) :: this    !
    !! Anzahl der Schichten an den verschiedenen Positionen (x,y)
    INTEGER                       , INTENT(IN) :: nc(:)   ! 
    !! Gr&ouml;&szlig;e des Feldes "res"
    INTEGER                       , INTENT(IN) :: nz      ! 
    !! Ergebnis: Zeiger auf Positionen (x,y)
    INTEGER                                    :: res(nz) ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='get_omi_xyz_normal_l2d' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch       ! 
    INTEGER            :: i, j, n3 ! 
    LOGICAL            :: l_ok     ! 
    !
    l_ok = ok_omi_xyz_layers ( this )
    !
    IF ( l_ok ) THEN
       !
       n3 = 0
       DO i=1,SIZE(nc)
          DO j=1,nc(i)
             n3      = n3+1
             res(n3) = i
          END DO
       END DO
       !
    ELSE
       !
       CALL setup_error_act ( all_errors(:), 8550, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') l_ok ; CALL setup_error_act ( '<ok-layers>', ch(1:1) )
       !
    END IF
    !
  END FUNCTION get_omi_xyz_normal_l2d 
  !
  !! Ermittle die Zeiger auf die 3D-Positionen f&uuml;r alle Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_normal_l3d ( this, nc, nz, np ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)              , INTENT(IN) :: this    !
    !! Anzahl der Schichten an den verschiedenen Positionen (x,y)
    INTEGER                       , INTENT(IN) :: nc(:)   ! 
    !! Gr&ouml;&szlig;e des Feldes "res", erste Dimension
    INTEGER                       , INTENT(IN) :: nz      ! 
    !! Gr&ouml;&szlig;e des Feldes "res", zweite Dimension
    INTEGER                       , INTENT(IN) :: np      ! 
    !! Ergebnis: Zeiger auf Positionen (x,y)
    INTEGER                                    :: res(nz,np) ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='get_omi_xyz_normal_l3d' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                   ! 
    INTEGER            :: i, j, k, n3, nk, n3d ! 
    LOGICAL            :: l_ok                 ! 
    !
    l_ok = ok_omi_xyz_layers ( this )
    !
    IF ( l_ok ) THEN
       !
       n3  = 0
       n3d = 0
       nk  = SIZE(this%layers)
       SELECT CASE ( np ) 
       CASE ( 1 ) ! mittig
          DO i=1,SIZE(nc)
             DO j=1,nc(i)
                n3        = n3+1
                res(n3,1) = n3
             END DO
          END DO
       CASE ( 2 ) ! schichtig
          DO i=1,SIZE(nc)
             DO j=1,nc(i)
                k  = nk-j
                n3 = n3 + 1
                IF ( j == 1          ) THEN ! oben
                   n3d       = n3d+1
                   res(n3,1) = n3d
                   res(n3,2) = 0
                ELSE IF ( j == nc(i) ) THEN ! unten
                   res(n3,1) = n3d
                   res(n3,2) = 0
                ELSE                        ! sonst
                   res(n3,1) = n3d
                   n3d       = n3d+1
                   res(n3,2) = n3d
                END IF
             END DO
          END DO
       CASE DEFAULT
          res(:,:) = c_undef_omi_xyz_int
       END SELECT
       !
    ELSE
       !
       CALL setup_error_act ( all_errors(:), 8560, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:1),'(L1)') l_ok ; CALL setup_error_act ( '<ok-layers>', ch(1:1) )
       !
    END IF
    !
  END FUNCTION get_omi_xyz_normal_l3d 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "x(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_x ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%x ) ; a2 = ASSOCIATED( this2%x )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%x ) ; l2 = SIZE( this2%x )
       IF ( l1 == l2 ) ok = ALL( this1%x(:) == this2%x(:) )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_xyz_x
  !
  !! pr&uuml;fe Komponente "y(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_y ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%y ) ; a2 = ASSOCIATED( this2%y )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%y ) ; l2 = SIZE( this2%y )
       IF ( l1 == l2 ) ok = ALL( this1%y(:) == this2%y(:) )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION eq_omi_xyz_y
  !
  !! pr&uuml;fe Komponente "z(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_z ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%z ) ; a2 = ASSOCIATED( this2%z )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%z ) ; l2 = SIZE( this2%z )
       IF ( l1 == l2 ) ok = ALL( this1%z(:) == this2%z(:) )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION eq_omi_xyz_z
  !
  !! pr&uuml;fe Komponente "valid(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_valid ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%valid ) ; a2 = ASSOCIATED( this2%valid )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%valid ) ; l2 = SIZE( this2%valid )
       IF ( l1 == l2 ) ok = &
            ALL( (     this1%valid(:).AND.     this2%valid(:)) .OR. &
                 (.NOT.this1%valid(:).AND..NOT.this2%valid(:)) )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION eq_omi_xyz_valid 
  !
  !! pr&uuml;fe Komponente "l3d(:,:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_l3d ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1(2), l2(2) ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%l3d ) ; a2 = ASSOCIATED( this2%l3d )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SHAPE( this1%l3d ) ; l2 = SHAPE( this2%l3d )
       IF ( ALL( l1(:) == l2(:) ) ) ok = ALL( this1%l3d(:,:) .EQ. this2%l3d(:,:) )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_xyz_l3d
  !
  !! pr&uuml;fe Komponente "l2d(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_l2d ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%l2d ) ; a2 = ASSOCIATED( this2%l2d )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%l2d ) ; l2 = SIZE( this2%l2d )
       IF ( l1 == l2 ) ok = ALL( this1%l2d(:) == this2%l2d(:) )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_xyz_l2d
  !
  !! pr&uuml;fe Komponente "layers(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_layers ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%layers ) ; a2 = ASSOCIATED( this2%layers )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%layers ) ; l2 = SIZE( this2%layers )
       IF ( l1 == l2 ) ok = ALL( this1%layers(:) == this2%layers(:) )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_xyz_layers
  !
  !! pr&uuml;fe Komponente "modif(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_modif ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%modif ) ; a2 = ASSOCIATED( this2%modif )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%modif ) ; l2 = SIZE( this2%modif )
       IF ( l1 == l2 ) ok = &
            ALL( (     this1%modif(:).AND.     this2%modif(:)) .OR. &
                 (.NOT.this1%modif(:).AND..NOT.this2%modif(:)) )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_xyz_modif
  !
  !! pr&uuml;fe Komponente "zmax(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_zmax ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%zmax ) ; a2 = ASSOCIATED( this2%zmax )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%zmax ) ; l2 = SIZE( this2%zmax )
       IF ( l1 == l2 ) ok = ALL( this1%zmax(:) == this2%zmax(:) )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION eq_omi_xyz_zmax
  !
  !! pr&uuml;fe Komponente "bini(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_xyz_bini ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariablen
    LOGICAL :: a1, a2 ! 
    INTEGER :: l1, l2 ! 
    !
    ok = .false. 
    a1 = ASSOCIATED( this1%bini ) ; a2 = ASSOCIATED( this2%bini )
    !
    IF ( .NOT. a1 .AND. .NOT. a2 ) THEN
       ok = .true.
    ELSE IF ( a1 .AND. a2 ) THEN
       l1 = SIZE( this1%bini ) ; l2 = SIZE( this2%bini )
       IF ( l1 == l2 ) ok = ALL( this1%bini(:) == this2%bini(:) )
    ELSE
       ok = .false. 
    END IF
    !
  END FUNCTION eq_omi_xyz_bini
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
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO = -6000 bis -6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob bottom(:) und this%layers(:) zusammenpassen (nur f&uuml;r z-Schichten) <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_bottom ( this, bottom ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this      ! 
    !! Feld mit Lage des Bodens 
    REAL (KIND=Double) , INTENT(IN) :: bottom(:) ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_xyz_bottom' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=15) :: ch               ! 
    INTEGER            :: n, m, nk, nl, nb ! 
    REAL (KIND=Double) :: bu, bo           ! 
    !
    IF ( this%ztype == 1 ) THEN
       nk = SIZE(this%layers)
       IF      ( has_omi_space_depth( this%refsystem )    ) THEN
          n   = COUNT( this%layers(1)  - bottom(:) < -0.000001_Double )
          m   = COUNT( this%layers(nk) - bottom(:) >  0.000001_Double )
          bu  = MAXVAL( bottom(:) ) ; bo = MINVAL( bottom(:) )
       ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
          n   = COUNT( bottom(:) - this%layers(1)  < -0.000001_Double )
          m   = COUNT( bottom(:) - this%layers(nk) >  0.000001_Double )
          bu  = MINVAL( bottom(:) ) ; bo = MAXVAL( bottom(:) )
       ELSE
          n   = -1; m = -1
          bu  = c_undef_omi_xyz_double ; bo  = c_undef_omi_xyz_double
       END IF

       res = ( n == 0 .AND. m == 0 )
    ELSE
       res = .true.
    END IF
    !
    IF ( ASSOCIATED(this%l2d) ) THEN
       nb  = SIZE(bottom)
       nl  = MAXVAL(this%l2d)
    ELSE
       nb  = SIZE(bottom)
       nl  = nb
    END IF
    res = MERGE( res, .false., nl == nb )
    !
    IF ( .NOT. res ) THEN
       CALL setup_error_act ( all_errors(:), -6000, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:10),'(I10)') n               ; CALL setup_error_act ( '<bot-count>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') m               ; CALL setup_error_act ( '<top-count>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') nl              ; CALL setup_error_act ( '<l2d>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') nb              ; CALL setup_error_act ( '<bottom>', ch(1:10) )
       WRITE(ch,'(G15.6)'    ) bu              ; CALL setup_error_act ( '<bot-bottom>', ch )
       WRITE(ch,'(G15.6)'    ) bo              ; CALL setup_error_act ( '<top-bottom>', ch )
       WRITE(ch,'(G15.6)'    ) this%layers(1)  ; CALL setup_error_act ( '<bot-layer>', ch )
       WRITE(ch,'(G15.6)'    ) this%layers(nk) ; CALL setup_error_act ( '<top-layer>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_bottom 
  !
  !! Pr&uuml;fe, ob surface(:) und this%layers(:) zusammenpassen (nur f&uuml;r z-Schichten) <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_surface ( this, surface ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this      ! 
    !! Feld mit Lage des Bodens 
    REAL (KIND=Double) , INTENT(IN) :: surface(:) ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_omi_xyz_surface' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=15) :: ch               ! 
    INTEGER            :: n, m, nk, nl, ns ! 
    REAL (KIND=Double) :: bu, bo           ! 
    !
    IF ( this%ztype == 1 ) THEN
       nk = SIZE(this%layers)
       IF      ( has_omi_space_depth( this%refsystem )    ) THEN
          n   = COUNT( this%layers(1)  - surface(:) < 0.0_Double )
          m   = COUNT( this%layers(nk) - surface(:) > 0.0_Double )
          bu  = MAXVAL( surface(:) ) ; bo = MINVAL( surface(:) )
       ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
          n   = COUNT( surface(:) - this%layers(1)  < 0.0_Double )
          m   = COUNT( surface(:) - this%layers(nk) > 0.0_Double )
          bu  = MINVAL( surface(:) ) ; bo = MAXVAL( surface(:) )
       ELSE
          n   = -1; m = -1
          bu  = c_undef_omi_xyz_double ; bo  = c_undef_omi_xyz_double
       END IF
       res = ( n == 0 .AND. m == 0 )
    ELSE
       res = .true.
    END IF
    IF ( ASSOCIATED( this%l2d ) ) THEN
       ns  = SIZE(surface)
       nl  = MAXVAL(this%l2d)
    ELSE
       ns  = SIZE(surface)
       nl = ns
    END IF
    res = MERGE( res, .false., nl == ns )
    !
    IF ( .NOT. res ) THEN
       nk = SIZE(this%layers)
       CALL setup_error_act ( all_errors(:), -6010, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:10),'(I10)') n               ; CALL setup_error_act ( '<bot-count>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') m               ; CALL setup_error_act ( '<top-count>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') nl              ; CALL setup_error_act ( '<l2d>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') ns              ; CALL setup_error_act ( '<surface>', ch(1:10) )
       WRITE(ch,'(G15.6)'    ) bu              ; CALL setup_error_act ( '<bot-surface>', ch )
       WRITE(ch,'(G15.6)'    ) bo              ; CALL setup_error_act ( '<top-surface>', ch )
       WRITE(ch,'(G15.6)'    ) this%layers(1)  ; CALL setup_error_act ( '<bot-layer>', ch )
       WRITE(ch,'(G15.6)'    ) this%layers(nk) ; CALL setup_error_act ( '<top-layer>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_surface 
  !
  !! Pr&uuml;fe, ob surface(:) und bottom(:) zusammenpassen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_surface_bottom ( this, surface, bottom ) &
       RESULT( res ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this       ! 
    !! Feld mit Lage der Oberfl&auml;che
    REAL (KIND=Double) , INTENT(IN) :: surface(:) ! 
    !! Feld mit Lage des Bodens 
    REAL (KIND=Double) , INTENT(IN) :: bottom(:)  ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='ok_omi_xyz_surface_bottom' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=15) :: ch            ! 
    INTEGER            :: n, nl, ns, nb ! 
    REAL (KIND=Double) :: bu, su        ! 
    !
    IF      ( has_omi_space_depth( this%refsystem )    ) THEN
       n   = COUNT( bottom(:) - surface(:) < 0.0_Double )
       bu  = MAXVAL( bottom(:) ) ; su = MAXVAL( surface(:) )
    ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
       n   = COUNT( bottom(:) - surface(:) > 0.0_Double )
       bu  = MINVAL( bottom(:) ) ; su = MINVAL( surface(:) )
    ELSE
       n   = -1
       bu  = c_undef_omi_xyz_double ; su = c_undef_omi_xyz_double
    END IF
    res = ( n == 0 )
    !
    IF ( ASSOCIATED(this%l2d) ) THEN
       ns  = SIZE(surface)
       nb  = SIZE(bottom)
       nl  = MAXVAL(this%l2d)
    ELSE
       ns  = SIZE(surface)
       nb  = SIZE(bottom)
       nl  = nb
    END IF
    res = MERGE( res, .false., nl == nb .AND. nl == ns )
    !
    IF ( .NOT. res ) THEN
       CALL setup_error_act ( all_errors(:), -6020, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:10),'(I10)') n               ; CALL setup_error_act ( '<count>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') nl              ; CALL setup_error_act ( '<l2d>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') ns              ; CALL setup_error_act ( '<surface>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') nb              ; CALL setup_error_act ( '<bottom>', ch(1:10) )
       WRITE(ch,'(G15.6)'    ) bu              ; CALL setup_error_act ( '<bot>', ch )
       WRITE(ch,'(G15.6)'    ) su              ; CALL setup_error_act ( '<sur>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_surface_bottom
  !
  !
  !! Pr&uuml;fe, ob bottom(:) und this%zmax(:) zusammenpassen (nur f&uuml;r z-Schichten) <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_xyz_bottom_zmax ( this, bottom ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)   , INTENT(IN) :: this      ! 
    !! Feld mit Lage des Bodens 
    REAL (KIND=Double) , INTENT(IN) :: bottom(:) ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_xyz_bottom_zmax' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=15) :: ch        ! 
    INTEGER            :: n, nb, nz ! 
    REAL (KIND=Double) :: bb, bx    ! 
    !
    IF ( this%ztype == 1 ) THEN
       IF ( ASSOCIATED( this%zmax) ) THEN
          nz = SIZE(this%zmax)
          nb = SIZE(bottom)
          IF      ( has_omi_space_depth( this%refsystem )    ) THEN
             n   = COUNT( this%zmax(:) - bottom(:) < 0.0_Double )
             bb  = MAXVAL( bottom(:) ) ; bx = MAXVAL( this%zmax(:) )
          ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
             n   = COUNT( this%zmax(:) - bottom(:) > 0.0_Double )
             bb  = MINVAL( bottom(:) ) ; bx = MINVAL( this%zmax(:) )
          ELSE
             n   = -1
             bb  = c_undef_omi_xyz_double
             bx  = c_undef_omi_xyz_double
          END IF
          res = ( n == 0 .AND. nz == nb )
       ELSE
          res = .true.
       END IF
    ELSE
       res = .true.
    END IF
    !
    IF ( .NOT. res ) THEN
       CALL setup_error_act ( all_errors(:), -6030, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       WRITE(ch(1:10),'(I10)') n  ; CALL setup_error_act ( '<count>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') nz ; CALL setup_error_act ( '<zmax>', ch(1:10) )
       WRITE(ch(1:10),'(I10)') nb ; CALL setup_error_act ( '<bottom>', ch(1:10) )
       WRITE(ch,'(G15.6)'    ) bb ; CALL setup_error_act ( '<x-bot>', ch )
       WRITE(ch,'(G15.6)'    ) bx ; CALL setup_error_act ( '<x-zmax>', ch )
    END IF
    !
  END FUNCTION ok_omi_xyz_bottom_zmax
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CREATE-Methoden
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge "zmax(:)" aus dem aktuellen Objekt <BR>
  !! Subroutine erzeugt keine Fehleremeldungen
  SUBROUTINE create_omi_xyz_zmax ( this ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this    ! 
    INTEGER               :: i, k, nl, nk, id   ! 
    INTEGER , ALLOCATABLE :: nc(:)              ! 
    REAL (KIND=Double) , ALLOCATABLE :: zmax(:) ! 
    !
    IF ( this%ztype == 1 ) THEN
       nk = SIZE(this%layers)
       id = MERGE( 1, 0, this%ctype == 2 )
       IF ( nk > 2 ) THEN
          nl = MAXVAL(this%l2d)
          ALLOCATE(nc(nl),zmax(nl))
          nc(:)   = get_omi_xyz_nof_point( this, nl )
          zmax(:) = c_undef_omi_xyz_double
          DO i=1,nl
             k       = nk - nc(i) + id
             zmax(i) = this%layers(k)
          END DO
          CALL set_omi_xyz_zmax_0_1 ( this, zmax )
          DEALLOCATE(nc,zmax)
       END IF
    END IF
    !
  END SUBROUTINE create_omi_xyz_zmax
  !
  !! Erzeuge "bini(:)" aus dem aktuellen Objekt <BR>
  !! Subroutine erzeugt keine Fehleremeldungen
  SUBROUTINE create_omi_xyz_bini ( this, val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(INOUT) :: this    ! 
    !! Wert
    REAL (KIND=Double) , INTENT(IN)  :: val(:)  ! 
    !
    CALL set_omi_xyz_bini ( this, val )
    !
  END SUBROUTINE create_omi_xyz_bini
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-UPDATE-Methoden
  ! ----------------------------------------------------------------------
  !
  !! Aktualisieren der Komponente "z(:)" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE update_omi_xyz_z ( this, surface, bottom )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)             , INTENT(INOUT) :: this       ! 
    !! Oberfl&auml;che
    REAL (KIND=Double) , OPTIONAL, INTENT(IN)    :: surface(:) ! 
    !! Boden
    REAL (KIND=Double) , OPTIONAL, INTENT(IN)    :: bottom(:)  ! 
    !! Hilfsvariable
    INTEGER               :: i, j, k, nk, n3, ns, nb, nz, nl, snc, id ! 
    INTEGER , ALLOCATABLE :: nc(:)                      ! 
    !
    IF ( ASSOCIATED( this%z ) ) THEN
       nz = SIZE( this%z )
       nl = MAXVAL( this%l2d )
       id = MERGE( 1, 0, this%ctype == 2 )
       SELECT CASE ( this%ztype )
          ! ---------------------------------------------------------------------------
       CASE ( 0 ) ! no layer
          ! ---------------------------------------------------------------------------
          IF ( PRESENT( bottom ) ) THEN
             nb = SIZE( bottom )
             IF ( nb == nl .AND. nb == nz ) THEN
                DO i=1,nz
                   this%z(i) = bottom(i)
                END DO
             END IF
          END IF
          ! ---------------------------------------------------------------------------
       CASE ( 1,2 ) ! z-layer / sigma-layer
          ! ---------------------------------------------------------------------------
          ALLOCATE( nc(nl) )
          nc(:) = get_omi_xyz_nof_point( this, nl )
          snc   = SUM(nc(:))
          ! ... berechne zunaechst die Normalkoordinaten
          IF      (       PRESENT(surface) .AND. .NOT. PRESENT(bottom) ) THEN
             ns = SIZE(surface)
             IF ( ns == nl  ) this%z(:) = get_omi_xyz_normal_z_coordinate &
                     ( this, nc(:), snc, surface=surface(:) )
          ELSE IF ( .NOT. PRESENT(surface) .AND.       PRESENT(bottom) ) THEN
             nb = SIZE(bottom)
             IF ( nb == nl  ) this%z(:) = get_omi_xyz_normal_z_coordinate &
                  ( this, nc(:), snc, bottom=bottom(:) )
          ELSE IF (       PRESENT(surface) .AND.       PRESENT(bottom) ) THEN
             ns = SIZE(surface)
             nb = SIZE(bottom)
             IF ( ns == nl .AND. nb == nl  ) this%z(:) = get_omi_xyz_normal_z_coordinate &
                  ( this, nc(:), snc, bottom=bottom(:), surface=surface(:) )
          END IF
          ! ... Korrekturen fuer z-Layer am Boden
          IF ( this%ztype == 1 ) THEN
             IF ( PRESENT(bottom) ) THEN
                nk = SIZE(this%layers)
                n3 = 0
                IF      ( has_omi_space_depth   ( this%refsystem ) ) THEN
                   DO i=1,nl
                      DO j=1,nc(i)-id
                         k  = nk-j
                         n3 = n3+1
                         IF ( bottom(i) > this%layers(k+1) .AND. bottom(i) < this%layers(k) ) THEN
                            IF      ( this%ctype == 1 ) THEN ! mittig 
                               this%z(n3)   = this%z(n3) + 0.5_Double*(bottom(i)-this%layers(k))
                            ELSE IF ( this%ctype == 2 ) THEN ! schichtig
                               this%z(n3+1) = bottom(i) ! unteren Punkt hochziehen
                            END IF
                         END IF
                      END DO
                      n3 = n3+id
                   END DO
                ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                   DO i=1,nl
                      DO j=1,nc(i)-id
                         k  = nk-j
                         n3 = n3+1
                         IF ( bottom(i) < this%layers(k+1) .AND. bottom(i) > this%layers(k) ) THEN
                            IF      ( this%ctype == 1 ) THEN ! mittig 
                               this%z(n3) = this%z(n3) - 0.5_Double*(bottom(i)-this%layers(k))
                            ELSE IF ( this%ctype == 2 ) THEN ! schichtig
                               this%z(n3+1) = bottom(i) ! unteren Punkt hochziehen
                            END IF
                         END IF
                      END DO
                      n3 = n3+id
                   END DO
                END IF
             END IF
             IF ( PRESENT(surface) ) THEN
                nk = SIZE(this%layers)
                n3 = 0
                IF      ( has_omi_space_depth   ( this%refsystem ) ) THEN
                   DO i=1,nl
                      DO j=1,nc(i)-id
                         k  = nk-j
                         n3 = n3+1
                         IF ( surface(i) > this%layers(k+1) .AND. surface(i) < this%layers(k) ) THEN
                            IF      ( this%ctype == 1 ) THEN ! mittig 
                               this%z(n3) = this%z(n3) + 0.5_Double*(surface(i)-this%layers(k+1))
                            ELSE IF ( this%ctype == 2 ) THEN ! schichtig
                               this%z(n3) = surface(i) ! oberen Punkt absenken
                            END IF
                         END IF
                      END DO
                      n3 = n3+id
                   END DO
                ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                   DO i=1,nl
                      DO j=1,nc(i)-id
                         k  = nk-j
                         n3 = n3+1
                         IF ( surface(i) < this%layers(k+1) .AND. surface(i) > this%layers(k) ) THEN
                            IF      ( this%ctype == 1 ) THEN ! mittig 
                               this%z(n3) = this%z(n3) - 0.5_Double*(surface(i)-this%layers(k+1))
                            ELSE IF ( this%ctype == 2 ) THEN ! schichtig
                               this%z(n3) = surface(i) ! oberen Punkt absenken
                            END IF
                         END IF
                      END DO
                      n3 = n3+id
                   END DO
                END IF
             END IF
          END IF
          DEALLOCATE( nc )
       END SELECT
    END IF
    !
  END SUBROUTINE update_omi_xyz_z
  !
  !! Aktualisieren der Komponente "modif(:)" durch Vergleich der neuen mit
  !! den alten Tiefen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE update_omi_xyz_modif ( this, z )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)              , INTENT(INOUT) :: this ! 
    !! (optional) Feld mit neuen Tiefen
    REAL (KIND=Double) , OPTIONAL , INTENT(IN)    :: z(:) ! 
    !
    IF ( ASSOCIATED( this%modif ) ) THEN
       IF ( ASSOCIATED( this%z ) .AND. PRESENT( z ) ) THEN
          IF ( SIZE( this%z ) == SIZE( z ) ) THEN
             this%modif(:) = .NOT. ( this%z(:) == z(:) )
          END IF
       ELSE
          this%modif(:) = .false.
       END IF
    END IF
    !
  END SUBROUTINE update_omi_xyz_modif 
  !
  !! Aktualisieren der Komponente "valid(:)" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE update_omi_xyz_valid ( this, surface, bottom )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz)             , INTENT(INOUT) :: this        ! 
    !! Oberfl&auml;che
    REAL (KIND=Double) , OPTIONAL, INTENT(IN)    :: surface(:)  ! 
    !! Boden
    REAL (KIND=Double) , OPTIONAL, INTENT(IN)    :: bottom(:)   ! 
    !! Hilfsvariable
    INTEGER               :: i, j, nl, ns, nb, nz, n3, n3s, n3b ! 
    INTEGER , ALLOCATABLE :: nc(:) ! 
    REAL (KIND=Double)    :: l_hlimit ! 
    !
    nl            = MAXVAL(this%l2d(:))
    this%valid(:) = .true.
    IF ( ASSOCIATED(this%z) ) THEN
       ! falls "z(:)" allokiert wurde
       nz = SIZE(this%z)
       SELECT CASE ( this%ztype )
          ! ---------------------------------------------------------------------------
       CASE ( 0 ) ! no layer
          ! ---------------------------------------------------------------------------
          IF ( PRESENT(surface) ) THEN
             ns = SIZE(surface)
             IF ( ns == nl .AND. ns == nz ) THEN
                IF      ( has_omi_space_depth   ( this%refsystem ) ) THEN
                   DO i=1,nz
                      this%valid(i) = MERGE( .true., .false., this%z(i)-surface(i) >= this%hlimit )
                   END DO
                ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                   DO i=1,nz
                      this%valid(i) = MERGE( .true., .false., surface(i)-this%z(i)  >= this%hlimit )
                   END DO
                END IF
             END IF
          END IF
          ! ---------------------------------------------------------------------------
       CASE ( 1 ) ! z-layer
          ! ---------------------------------------------------------------------------
          ALLOCATE( nc(nl) )
          nc(:) = get_omi_xyz_nof_point( this, nl )
          l_hlimit = MERGE( 0.0_Double, 0.5_Double*this%hlimit, this%ctype==2 )
          IF ( PRESENT(surface) ) THEN
             ns = SIZE(surface)
             IF ( ns == nl ) THEN
                n3 = 0
                IF      ( has_omi_space_depth   ( this%refsystem ) ) THEN
                   DO i=1,nl
                      DO j=1,nc(i)
                         n3 = n3+1
                         IF ( this%z(n3) - surface(i) < l_hlimit ) this%valid(n3) = .false.
                      END DO
                   END DO
                ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                   DO i=1,nl
                      DO j=1,nc(i)
                         n3 = n3+1
                         IF ( surface(i) - this%z(n3) < l_hlimit ) this%valid(n3) = .false.
                      END DO
                   END DO
                END IF
             END IF
          END IF
          IF ( PRESENT(bottom) ) THEN
             nb = SIZE(bottom)
             IF ( nb == nl ) THEN
                n3 = 0
                IF      ( has_omi_space_depth   ( this%refsystem ) ) THEN
                   DO i=1,nl
                      DO j=1,nc(i)
                         n3 = n3+1
                         IF ( bottom(i) - this%z(n3) < l_hlimit ) this%valid(n3) = .false.
                      END DO
                   END DO
                ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                   DO i=1,nl
                      DO j=1,nc(i)
                         n3 = n3+1
                         IF ( this%z(n3) - bottom(i) < l_hlimit ) this%valid(n3) = .false.
                      END DO
                   END DO
                END IF
             END IF
          END IF
          IF ( this%ctype == 2 ) THEN
             n3 = 0
             DO i=1,nl
                n3s = -1
                n3b = -1
                DO j=1,nc(i)
                   n3 = n3+1
                   IF ( this%valid(n3) .AND. n3s == -1 ) n3s = n3
                   IF ( this%valid(n3)                 ) n3b = n3
                END DO
                IF ( n3s > 0 .AND. n3b > 0 ) THEN
                   IF      ( has_omi_space_depth   ( this%refsystem ) ) THEN
                      IF ( this%z(n3b) - this%z(n3s) < this%hlimit ) this%valid(n3s:n3b) = .false.
                   ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                      IF ( this%z(n3s) - this%z(n3b) < this%hlimit ) this%valid(n3s:n3b) = .false.
                   END IF
                END IF
             END DO
          END IF
          DEALLOCATE( nc )
          ! ---------------------------------------------------------------------------
       CASE ( 2 ) ! sigma layer
          ! ---------------------------------------------------------------------------
          IF ( PRESENT(surface) .AND. PRESENT(bottom) ) THEN
             ns = SIZE(surface)
             nb = SIZE(bottom)
             IF ( ns == nb .AND. ns == nl ) THEN
                ALLOCATE( nc(nl) )
                nc(:) = get_omi_xyz_nof_point( this, nl )
                n3 = 0
                IF      ( has_omi_space_depth   ( this%refsystem ) ) THEN
                   DO i=1,SIZE(nc)
                      IF ( bottom(i)-surface(i) < this%hlimit ) this%valid(n3+1:n3+nc(i)) = .false.
                      n3 = n3 + nc(i)
                   END DO
                ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                   DO i=1,SIZE(nc)
                      IF ( surface(i)-bottom(i) < this%hlimit ) this%valid(n3+1:n3+nc(i)) = .false.
                      n3 = n3 + nc(i)
                   END DO
                END IF
                DEALLOCATE( nc )
             END IF
          END IF
       END SELECT
    ELSE
       ! ggf. Ableiten von "valid(:)" aus "surface" und "bottom"
       IF ( PRESENT(surface) .AND. PRESENT(bottom) ) THEN
          ns = SIZE(surface)
          nb = SIZE(bottom)
          IF ( ns == nb .AND. ns == nl ) THEN
             IF      ( has_omi_space_depth   ( this%refsystem ) ) THEN
                DO i=1,ns
                   this%valid(i) = MERGE( .true., .false., bottom(i)-surface(i) >= this%hlimit )
                END DO
             ELSE IF ( has_omi_space_altitude( this%refsystem ) ) THEN
                DO i=1,ns
                   this%valid(i) = MERGE( .true., .false., surface(i)-bottom(i) >= this%hlimit )
                END DO
             END IF
          END IF
       END IF
    END IF
    !
  END SUBROUTINE update_omi_xyz_valid
  !
  !! Ermittle eine Maske f&uuml;r die in einer bestimmten Schicht liegenden Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_layer_point_mask_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this ! 
    !! Nummer der aktuellen Schicht, f&uuml;r die ein Name ermittelt werden soll
    INTEGER          , INTENT(IN) :: val  ! 
    !! Ergebnis: Indikatormaske 
    LOGICAL , POINTER :: res(:) ! 
    !! Hilfsvariable
    INTEGER :: i, j, ia, ja, memo, n ! 
    !
    n = get_omi_xyz_layer_count( this )
    IF ( n > 0 .AND. val >= 1 .AND. val <= n .AND. this%ctype > 0 .AND. ASSOCIATED(this%l2d) ) THEN
       ALLOCATE( res(SIZE(this%l2d)) )
       DO i=1,SIZE(this%l2d)
          res(i) = .false.
       END DO
       ia   = 1
       memo = this%l2d(1)
       DO j=1,get_omi_xyz_2d_point_count( this )
          IF ( ia+n-val <= SIZE(this%l2d) ) THEN
             IF ( this%l2d(ia+n-val) == memo ) res(ia+n-val) = .true.
          END IF
          DO i=ia+1,SIZE(this%l2d)
             IF ( this%l2d(i) == memo ) CYCLE
             ja   = i
             memo = this%l2d(i)
             EXIT
          END DO
          ia = ja
       END DO
    ELSE
       NULLIFY( res )
    END IF
    !
  END FUNCTION get_omi_xyz_layer_point_mask_0
  !
  !! Ermittle eine Maske f&uuml;r die in bestimmten S&auml;len liegenden Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_column_point_mask_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_xyz) , INTENT(IN) :: this   ! 
    !! Liste mit den Nummern der S&auml;len die ermittelt werden sollen
    INTEGER          , INTENT(IN) :: val(:) ! 
    !! Ergebnis: Indikatormaske 
    LOGICAL , POINTER :: res(:) ! 
    !! Hilfsvariablen
    INTEGER               :: i, j, n2d, mm    ! 
    INTEGER , ALLOCATABLE :: l_nc(:), l_ss(:) ! 
    !
    IF ( ASSOCIATED(this%l2d) ) THEN
       ALLOCATE ( res(SIZE(this%l2d)) )
       res(:) = .false.
       n2d    = get_omi_xyz_2d_point_count( this )
       ALLOCATE( l_nc(n2d), l_ss(n2d) )
       l_nc(:) = get_omi_xyz_nof_point ( this, SIZE(l_nc) )
       mm      = 0
       DO i=1,SIZE(l_ss)
          l_ss(i) = mm
          mm      = mm + l_nc(i)
       END DO
       DO j=1,SIZE(val)
          IF ( val(j) < 1 .OR. val(j) > n2d ) CYCLE
          DO i=1,l_nc(val(j))
             res(l_ss(val(j))+i) = .true.
          END DO
       END DO
    ELSE
       NULLIFY( res )
    END IF
    !
    IF ( ALLOCATED( l_nc ) ) DEALLOCATE( l_nc )
    IF ( ALLOCATED( l_ss ) ) DEALLOCATE( l_ss )
    !
  END FUNCTION get_omi_xyz_column_point_mask_1
  !
END MODULE b_omi_xyz
! TailOfBaseModule --------------------------------------------------------
