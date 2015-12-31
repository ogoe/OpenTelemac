! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden analog zu OpenMI-Interface <EM>ISpatialReference</EM></h2>
!! @author G. Lang
!! @version 2.1 vom 07/21/05, Quellcode: mod_b_omi_space.f90
!! <HR>
!! type and methods equivalent to OpenMI-Interface <EM>ISpatialReference</EM> <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-02-02 : G. Lang : Erstversion
!  01.02 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt!
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>ISpatialReference</EM>. 
!! Dient dazu, das geographische Bezugssystem n&auml;her zu bezeichnen.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_space";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_space";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_space";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_space";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_space";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_space".
!! </OL>
!!                                                         
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_omi_space"
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> id : kurze Identifikationsbezeichnung des geographischen Bezugssystems                                       
!! </OL>
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                 
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden:
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_omi_space mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_space mit CLEAR-Methode.
!! </OL>
!! <HR>
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
!!          die Methode PRINT_OMI_SPACE_ALL_ERRORS.
!!                                                                    <BR>
!
MODULE b_omi_space
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
  USE b_att, ONLY : &
       !   Typdefinitionen
       t_att,             &
       !   Parameter 
       c_att_name,         &
       c_hor_coord_system, &
       c_ver_coord_system, &  
       !   Variablen mit INTENT(IN)
       !   Variablen mit INTENT(INOUT)
       !   Variablen mit INTENT(OUT)
       !   Routinen / Interfaces
       init_att,           &
       clear_att,          &
       setup_att_prn_lun,  &
       setup_att_trc_lun,  &
       get_att_ch,         &
       get_att_idx,        &
       get_att_nof_values, &
       is_att_ch
  !   Operatoren
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
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_space_id=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! id : kurze Identifikationsbezeichnung des geographischen Bezugssystems                                       
  TYPE , PUBLIC :: t_omi_space
     PRIVATE
     CHARACTER (LEN=c_len_omi_space_id) :: id ! 
  END TYPE t_omi_space
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Undefined-Wert f&uuml;r CHARACTER-Komponenten
  CHARACTER (LEN=9)  , PUBLIC , PARAMETER :: c_undef_omi_space_char='undefined'      ! 
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
  INTERFACE init_omi_space
     MODULE PROCEDURE init_omi_space_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_space
     MODULE PROCEDURE clear_omi_space_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_space_prn_lun
     MODULE PROCEDURE setup_omi_space_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_space_trc_lun
     MODULE PROCEDURE setup_omi_space_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_space" 
  !! und Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE new_omi_space
     MODULE PROCEDURE new_omi_space_0  ! 
     MODULE PROCEDURE new_omi_space_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_space" 
  !! und teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE kill_omi_space
     MODULE PROCEDURE kill_omi_space_0 ! 
     MODULE PROCEDURE kill_omi_space_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_space" auf G&uuml;ltigkeit: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE ok_omi_space
     MODULE PROCEDURE ok_omi_space_0 ! 
     MODULE PROCEDURE ok_omi_space_1 ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_space": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_space" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_space
     MODULE PROCEDURE print_omi_space_0 ! 
     MODULE PROCEDURE print_omi_space_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_space_static
     MODULE PROCEDURE print_omi_space_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_space_all_errors
     MODULE PROCEDURE print_omi_space_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "id" in "t_omi_space" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! c) bei vorgegebener Attributliste f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! d) bei vorgegebener Attributliste f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_space_id
     MODULE PROCEDURE set_omi_space_id_0_0 ! 
     MODULE PROCEDURE set_omi_space_id_1_0 ! 
     MODULE PROCEDURE set_omi_space_id_att_0_0 ! 
     MODULE PROCEDURE set_omi_space_id_att_1_0 ! 
  END INTERFACE
  !
  !! Hole Komponente "id" aus "t_omi_space": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_space_id
     MODULE PROCEDURE get_omi_space_id_0_0 ! 
     MODULE PROCEDURE get_omi_space_id_1_0 ! 
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_space": <BR>
  !! a) f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! b) f&uuml;r viele Werte <EM>id(:)</EM> (Vektor)
  INTERFACE get_omi_space_idx
     MODULE PROCEDURE get_omi_space_idx_1_0
     MODULE PROCEDURE get_omi_space_idx_1_1
  END INTERFACE
  !
  !! wird als Vertikalkoordinate die <EM>H&ouml;he</EM> benutzt <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE has_omi_space_altitude
     MODULE PROCEDURE has_omi_space_altitude_0
     MODULE PROCEDURE has_omi_space_altitude_1
  END INTERFACE
  !! wird als Vertikalkoordinate die <EM>Tiefe</EM> benutzt <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE has_omi_space_depth
     MODULE PROCEDURE has_omi_space_depth_0
     MODULE PROCEDURE has_omi_space_depth_1
  END INTERFACE
  !! wird als Vertikalkoordinate die <EM>Geoidh&ouml;he</EM> benutzt <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE has_omi_space_geoidal_height
     MODULE PROCEDURE has_omi_space_geoidal_height_0
     MODULE PROCEDURE has_omi_space_geoidal_height_1
  END INTERFACE
  !
  !! werden <EM>projizierte kartesische Koordinaten</EM> benutzt <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE has_omi_space_cartesian_proj
     MODULE PROCEDURE has_omi_space_cartesian_proj_0
     MODULE PROCEDURE has_omi_space_cartesian_proj_1
  END INTERFACE
  !! werden <EM>geozentrische kartesische Koordinaten</EM> benutzt <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE has_omi_space_cartesian_geoc
     MODULE PROCEDURE has_omi_space_cartesian_geoc_0
     MODULE PROCEDURE has_omi_space_cartesian_geoc_1
  END INTERFACE
  !! werden <EM>geozentrische sph&auml;rische Koordinaten</EM> benutzt <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE has_omi_space_spherical_geoc
     MODULE PROCEDURE has_omi_space_spherical_geoc_0
     MODULE PROCEDURE has_omi_space_spherical_geoc_1
  END INTERFACE
  !! werden <EM>geographische Ellipsoid-Koordinaten</EM> benutzt <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE has_omi_space_ellipsoidal_geog
     MODULE PROCEDURE has_omi_space_ellipsoidal_geog0
     MODULE PROCEDURE has_omi_space_ellipsoidal_geog1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_space" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_space
     MODULE PROCEDURE eq_omi_space_0_0  ! 
     MODULE PROCEDURE eq_omi_space_0_1  ! 
     MODULE PROCEDURE eq_omi_space_1_0  ! 
     MODULE PROCEDURE eq_omi_space_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_space" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_space
     MODULE PROCEDURE ne_omi_space_0_0  !
     MODULE PROCEDURE ne_omi_space_0_1  ! 
     MODULE PROCEDURE ne_omi_space_1_0  ! 
     MODULE PROCEDURE ne_omi_space_1_1  ! 
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
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_omi_space
  PUBLIC :: clear_omi_space
  PUBLIC :: setup_omi_space_prn_lun
  PUBLIC :: setup_omi_space_trc_lun
  PUBLIC :: new_omi_space
  PUBLIC :: kill_omi_space
  PUBLIC :: ok_omi_space
  PUBLIC :: print_omi_space
  PUBLIC :: print_omi_space_static
  PUBLIC :: print_omi_space_all_errors
  PUBLIC :: set_omi_space_id
  PUBLIC :: get_omi_space_id
  PUBLIC :: eq_omi_space
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: get_omi_space_idx 
  PUBLIC :: has_omi_space_altitude
  PUBLIC :: has_omi_space_depth
  PUBLIC :: has_omi_space_geoidal_height
  PUBLIC :: has_omi_space_cartesian_proj
  PUBLIC :: has_omi_space_cartesian_geoc
  PUBLIC :: has_omi_space_spherical_geoc
  PUBLIC :: has_omi_space_ellipsoidal_geog
  PUBLIC :: ne_omi_space
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
  CHARACTER (LEN=11), PARAMETER :: c_modname         = 'b_omi_space' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op              = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun             = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_space
  INTEGER           , PARAMETER :: c_nofcomp         = 1                ! ggf. modifizieren
  !! Anzahl der verschiedenen Koordinaten-System-Definitionen
  INTEGER           , PARAMETER :: c_max_omi_space_id=7 ! 
  !! Feld mit den g&uuml;ltigen Koordinaten-System-Definitionen
  CHARACTER (LEN=55), PARAMETER :: c_omi_space_id(c_max_omi_space_id) = (/ & ! 
       'Cartesian_Geocentric[X,Y,Z]                            ' , &
       'Spherical_Geocentric[Latitude,Longitude,Radius]        ' , &
       'Ellipsoidal_Geographic[Latitude,Longitude,Height]      ' , &
       'Cartesian_Projected[Easting,Northing]Vertical[Depth]   ' , &
       'Cartesian_Projected[Easting,Northing]Vertical[Altitude]' , &
       'Cartesian_Projected[Westing,Southing]Vertical[Depth]   ' , &
       'Cartesian_Projected[Westing,Southing]Vertical[Altitude]' /) ! 
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
  !! Zeiger auf Eintrag in c_omi_space_id(:) bei vorgegebener Id finden
  INTERFACE get_c_omi_space_id_idx
     MODULE PROCEDURE get_c_omi_space_id_idx_d
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
  SUBROUTINE init_omi_space_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='init_omi_space_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_space" version 2.1 of 07/21/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_att ( )
       ! ... ggf. weitere Initialisierungen ergaenzen
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_space_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_space_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_space_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='clear_omi_space_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_space_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_att ( )
       ! ... ggf. weitere De-Initialisierungen ergaenzen
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_space_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_space_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_omi_space_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_att_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_space_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_space_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_omi_space_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_att_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_space_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_space_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='new_omi_space_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%id = REPEAT( ' ', LEN(this%id) )
       this%id = c_undef_omi_space_char
    END IF
    !
  END SUBROUTINE new_omi_space_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_space_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_space) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='new_omi_space_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_space_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_space_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_space_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='kill_omi_space_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_space_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_space_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_space_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_space) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='kill_omi_space_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_space_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_space_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_space_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_omi_space_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_omi_space_id( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_space_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_space_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_omi_space_1' 
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
          ok(i) = ok_omi_space_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_space_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_space_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_omi_space_0' 
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
       IF ( no_error( ) ) CALL print_omi_space_id( this )
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
8000 FORMAT('# Beginn Objekt t_omi_space ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_space ------------------------------')
    !
  END SUBROUTINE print_omi_space_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_space_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_omi_space_1' 
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
          IF ( no_error( ) ) CALL print_omi_space_0 ( this(i) )
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
  END SUBROUTINE print_omi_space_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_space_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=24), PARAMETER :: c_upname='print_omi_space_static_d' 
    !! Statusvariable
    INTEGER :: stat !
    !! Hilfsbariable
    INTEGER :: i    ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, &
           c_undef_omi_space_char, ( TRIM(c_omi_space_id(i)), i=1,c_max_omi_space_id )
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_omi_space_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_space         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#     initialised = ',L1,/ &
    '#          prn_op = ',L1,/ &
    '#          trc_op = ',L1,/ &
    '#         prn_lun = ',I5,/ &
    '#         trc_lun = ',I5,/ &
    '#          n_init = ',I5,/ &
    '#     undef[char] = ',A,/ &
    '# omi_space_id[1] = ',A,/ &
    '# omi_space_id[2] = ',A,/ &
    '# omi_space_id[3] = ',A,/ &
    '# omi_space_id[4] = ',A,/ &
    '# omi_space_id[5] = ',A,/ &
    '# omi_space_id[6] = ',A,/ &
    '# omi_space_id[7] = ',A,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_omi_space_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_space_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=28), PARAMETER :: c_upname='print_omi_space_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_space_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_space_id_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*)  , INTENT(IN)    :: val  ! 
    !
    this%id = REPEAT( ' ', LEN(this%id) )
    this%id = val(1:MIN(LEN(this%id),LEN_TRIM(val)))
    !
  END SUBROUTINE set_omi_space_id_0_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_space_id_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_space) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*)  , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_space_id_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_space_id_1_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu, der aus
  !! den in der Attributliste (t_att) vorhandenen Angaben ermittelt wird <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_space_id_att_0_0 &
       ( this, &
         att )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(INOUT) :: this    ! 
    !! Liste mit Attributen (t_att) 
    TYPE (t_att)       , INTENT(IN)    :: att(:)  ! 
    !! Feldgr&ouml;&szlig;e
    INTEGER           , PARAMETER      :: c_max=2 ! 
    !! Id f&uuml;r Attribute in dem Feld "c_att_name(:)" <BR>
    !! (08) = <EM>horizontal_coordinate_system_definition</EM>: [geographic|planar|local]  <BR>
    !! (19) = <EM>vertical_coordinate_system_definition</EM>: [altitude|depth]             
    INTEGER           , PARAMETER      :: c_idx(c_max)= (/08,19/) ! 
    !! allokierbare Felder
    CHARACTER (LEN=80) , ALLOCATABLE :: ch1(:), ch2(:) ! 
    !! Hilfsfeld
    INTEGER :: idx(c_max) ! 
    !! Hilfsvariablen
    INTEGER :: i, nof     ! 
    !
    CALL new_omi_space ( this )
    !
    idx(:) = -1
    DO i=1,c_max
       idx(i) = get_att_idx ( att(:), c_att_name(c_idx(i)) ) 
    END DO
    IF ( idx(1) > 0 ) THEN
       nof = get_att_nof_values ( att(idx(1)) )
       IF ( nof == 1 ) THEN
          ALLOCATE ( ch1(nof) )
          ch1 = get_att_ch( att(idx(1)) )
          IF ( ANY( c_hor_coord_system(1:3)(1:10) == ch1(1)(1:10) ) ) THEN
             CALL set_omi_space_id ( this, c_omi_space_id(3) ) ! geographic
          ELSE
             IF ( idx(2) > 0 ) THEN
                nof = get_att_nof_values ( att(idx(2)) )
                IF ( nof == 1 ) THEN
                   ALLOCATE ( ch2(nof) )
                   ch2 = get_att_ch( att(idx(2)) )
                   IF ( ANY( c_ver_coord_system(1:3)(1:8) == ch2(1)(1:8) ) ) THEN
                      CALL set_omi_space_id ( this, c_omi_space_id(5) ) ! cartesian., altitude
                   ELSE
                      CALL set_omi_space_id ( this, c_omi_space_id(4) ) ! cartesian, depth
                   END IF
                END IF
             END IF
          END IF
          IF ( ALLOCATED(ch1) ) DEALLOCATE ( ch1 )
          IF ( ALLOCATED(ch2) ) DEALLOCATE ( ch2 )
       END IF
    ELSE
       CALL set_omi_space_id ( this, c_omi_space_id(4) ) ! cartesian, depth
    END IF
    !
  END SUBROUTINE set_omi_space_id_att_0_0
  !
  !! weise den Komponenten "id" einen skalaren Wert zu, der aus
  !! den in der Attributliste (t_att) vorhandenen Angaben ermittelt wird <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_space_id_att_1_0 &
       ( this, &
         att )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(INOUT) :: this(:) ! 
    !! Liste mit Attributen (t_att) 
    TYPE (t_att)       , INTENT(IN)    :: att(:)  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_space_id_att_0_0 ( this(i), att(:) )
    END DO
    !
  END SUBROUTINE set_omi_space_id_att_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_space_id_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space)   , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    CHARACTER (LEN=c_len_omi_space_id) :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_omi_space_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_space_id_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_space)   , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "id"
    CHARACTER (LEN=c_len_omi_space_id) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%id
    !
  END FUNCTION get_omi_space_id_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_space" f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_space_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! zu suchender Wert der Komponente <EM>id</EM>
    CHARACTER (LEN=*)  , INTENT(IN) :: val     ! 
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
  END FUNCTION get_omi_space_idx_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_space" f&uuml;r viele Werte <EM>id(:)</EM> (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_space_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
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
       res(i) = get_omi_space_idx_1_0 ( this(:), val(i) )
    END DO
    !
  END FUNCTION get_omi_space_idx_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-HAS-Methoden
  ! ----------------------------------------------------------------------
  !
  !! besitzt ein Objekt die H&ouml;he als Vertikalkoordinate <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_altitude_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res ! 
    !! Anzahl der CS mit "Altitude"
    INTEGER , PARAMETER :: c_max=2 ! 
    !! Indices der CS mit "Altitude"
    INTEGER , PARAMETER :: c_idx(c_max) = (/ 5, 7 /) ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    idx = get_c_omi_space_id_idx ( this%id )
    res = ANY( c_idx(:) == idx )
    !
  END FUNCTION has_omi_space_altitude_0
  !
  !! besitzen viele Objekte die H&ouml;he als Vertikalkoordinate <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_altitude_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_space_altitude_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_space_altitude_1
  !
  !! besitzt ein Objekt die Tiefe als Vertikalkoordinate <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_depth_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res ! 
    !! Anzahl der CS mit "Altitude"
    INTEGER , PARAMETER :: c_max=2 ! 
    !! Indices der CS mit "Altitude"
    INTEGER , PARAMETER :: c_idx(c_max) = (/ 4, 6 /) ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    idx = get_c_omi_space_id_idx ( this%id )
    res = ANY( c_idx(:) == idx )
    !
  END FUNCTION has_omi_space_depth_0
  !
  !! besitzen viele Objekte die Tiefe als Vertikalkoordinate <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_depth_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_space_depth_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_space_depth_1
  !
  !! besitzt ein Objekt die Geoidh&ouml;he als Vertikalkoordinate <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_geoidal_height_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res ! 
    !! Anzahl der CS mit "Altitude"
    INTEGER , PARAMETER :: c_max=1 ! 
    !! Indices der CS mit "Altitude"
    INTEGER , PARAMETER :: c_idx(c_max) = (/ 3 /) ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    idx = get_c_omi_space_id_idx ( this%id )
    res = ANY( c_idx(:) == idx )
    !
  END FUNCTION has_omi_space_geoidal_height_0
  !
  !! besitzen viele Objekte die Geoidh&ouml;he als Vertikalkoordinate <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_geoidal_height_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_space_geoidal_height_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_space_geoidal_height_1
  !
  !! besitzt ein Objekt die projizierte kartesische Koordinaten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_cartesian_proj_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res ! 
    !! Anzahl der CS mit "Altitude"
    INTEGER , PARAMETER :: c_max=4 ! 
    !! Indices der CS mit "Altitude"
    INTEGER , PARAMETER :: c_idx(c_max) = (/ 4, 5, 6, 7 /) ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    idx = get_c_omi_space_id_idx ( this%id )
    res = ANY( c_idx(:) == idx )
    !
  END FUNCTION has_omi_space_cartesian_proj_0
  !
  !! besitzen viele Objekte projizierte kartesische Koordinaten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_cartesian_proj_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_space_cartesian_proj_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_space_cartesian_proj_1
  !
  !! besitzt ein Objekt die geozentrsiche kartesische Koordinaten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_cartesian_geoc_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res ! 
    !! Anzahl der CS mit "Altitude"
    INTEGER , PARAMETER :: c_max=1 ! 
    !! Indices der CS mit "Altitude"
    INTEGER , PARAMETER :: c_idx(c_max) = (/ 1 /) ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    idx = get_c_omi_space_id_idx ( this%id )
    res = ANY( c_idx(:) == idx )
    !
  END FUNCTION has_omi_space_cartesian_geoc_0
  !
  !! besitzen viele Objekte geozentrsiche kartesische Koordinaten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_cartesian_geoc_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_space_cartesian_geoc_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_space_cartesian_geoc_1
  !
  !! besitzt ein Objekt die geozentrische sph&auml;rische Koordinaten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_spherical_geoc_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res ! 
    !! Anzahl der CS mit "Altitude"
    INTEGER , PARAMETER :: c_max=1 ! 
    !! Indices der CS mit "Altitude"
    INTEGER , PARAMETER :: c_idx(c_max) = (/ 2 /) ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    idx = get_c_omi_space_id_idx ( this%id )
    res = ANY( c_idx(:) == idx )
    !
  END FUNCTION has_omi_space_spherical_geoc_0
  !
  !! besitzen viele Objekte geozentrische sph&auml;rische Koordinaten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_spherical_geoc_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_space_spherical_geoc_0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_space_spherical_geoc_1
  !
  !! besitzt ein Objekt die geographische Elliposid-Koordinaten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_ellipsoidal_geog0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res ! 
    !! Anzahl der CS mit "Altitude"
    INTEGER , PARAMETER :: c_max=1 ! 
    !! Indices der CS mit "Altitude"
    INTEGER , PARAMETER :: c_idx(c_max) = (/ 3 /) ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    idx = get_c_omi_space_id_idx ( this%id )
    res = ANY( c_idx(:) == idx )
    !
  END FUNCTION has_omi_space_ellipsoidal_geog0
  !
  !! besitzen viele Objekte geographische Ellipsoid-Koordinaten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_omi_space_ellipsoidal_geog1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : wahr/falsch
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = has_omi_space_ellipsoidal_geog0 ( this(i) )
    END DO
    !
  END FUNCTION has_omi_space_ellipsoidal_geog1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_space_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%id == this2%id )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_space_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_space_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_space_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_space_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_space_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_space_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_space_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_space_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_space_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_space_1_1
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
  FUNCTION ne_omi_space_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_space_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_space_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_space_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_space_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_space_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_space_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_space) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_space_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_space_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_space_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_space) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l     = SIZE(ok)
    ok(:) = .NOT. eq_omi_space_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_space_1_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_space" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_space ausfuehren'
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
  SUBROUTINE init_omi_space_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER  :: c_upname='init_omi_space_all_errors' !
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
               '--> INIT_omi_space ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_space ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_space"\n'//&
               'Typ-Komponente = "id"\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_space" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_space" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_space" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_space"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_omi_space" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_space"\n'//&
               '--> Code in Modul "b_omi_space" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_space"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_omi_space" pruefen' )
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
  END SUBROUTINE init_omi_space_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_space_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='clear_omi_space_all_errors' !
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_space_all_errors
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
  FUNCTION ok_omi_space_id &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_omi_space_id' ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    idx = get_c_omi_space_id_idx ( this%id )
    ok  = ( idx > 0 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<aktuell>', TRIM(this%id) )
    END IF
    !
  END FUNCTION ok_omi_space_id
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_space_id &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_space) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_omi_space_id' ! 
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
  END SUBROUTINE print_omi_space_id
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
  ! >>> PRIVATE-GET-Methoden
  ! ----------------------------------------------------------------------
  !
  !! Hole den Zeiger auf einen Eintrag in "c_omi_space_id" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_c_omi_space_id_idx_d ( var ) & 
       RESULT( idx )
    !! String mit <EM>Id</EM> nach der in "c_omi_space_id(:)" zu suchen ist
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebnis: Zeiger auf Eintrag in dem Feld "c_omi_space_id(:)" der mit
    !! "var" &uuml;bereinstimmt; falls kein Eintrag gefunden wird, wird -1
    !! zur&uuml;ckgegeben
    INTEGER :: idx  ! 
    !! Hilfsvariable
    INTEGER :: i, l ! 
    !
    idx = -1
    l   = LEN_TRIM(var)
    DO i=1,c_max_omi_space_id
       IF ( idx /= -1 ) EXIT
       IF ( l == LEN_TRIM(c_omi_space_id(i)) ) THEN
          IF ( var(1:l) == c_omi_space_id(i)(1:l) ) idx = i
       END IF
    END DO
    !
  END FUNCTION get_c_omi_space_id_idx_d
  !
END MODULE b_omi_space
! TailOfBaseModule --------------------------------------------------------
