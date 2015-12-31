! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden analog OpenMI-Interface <EM>IUnit</EM></h2>
!! @author G&uuml;nther Lang
!! @version 3.1 vom 12/21/06, Quellcode: mod_b_omi_unit.f90
!! <HR>
!! type and methods equivalent to OpenMI interface "IUnit" <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-01-24 : G. Lang : Erstversion
!  01.02 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!  02.02 : 2005-08-10 : G. Lang : Einheit bei Flussgroessen anpassen
!  03.01 : 2006-12-21 : G. Lang : Umstellen der SI-Basiseinheiten auf Real-Zahlen
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>IUnit</EM>. Dient
!! dazu, die Einheiten einer physikalischen Gr&ouml;&szlig;e zu 
!! bezeichnen, in der die Gr&ouml;&szlig;e ausgedr&uuml;ckt wird.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_unit";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_unit";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_unit";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_unit";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_unit";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_unit";
!!    <LI> Nach einem bestimmten <EM>id</EM> in einer Liste von Variablen des Typs "t_omi_unit" suchen.
!! </OL>
!! <HR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_omi_unit 
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:
!! <OL>
!!     <LI> id          : kurzer Identifikationsbezeichner, z.B. "mm/h"
!!     <LI> description : <kurze Beschreibung2>
!!     <LI> convfactosi : <kurze Beschreibung3>
!!     <LI> offsettosi  : <kurze Beschreibung4>
!! </OL>
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: 
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_omi_unit mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_unit mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_UNIT_ALL_ERRORS.
!!                                                                    <BR>
!! <HR>
!
MODULE b_omi_unit
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
  ! [A.3] BASIS-Modul mit grundlegenden Informationen zu physikalischen Gr&ouml;&szlig;en
  !
  USE b_phy, ONLY :                 &
       !   Routinen / Interfaces
       init_phy,                    &
       clear_phy,                   &
       setup_phy_prn_lun,           &
       setup_phy_trc_lun,           &
       setup_phy_language,          &
       ok_phy,                      &
       is_phy_quant_valid,          &
       get_phy_quant_code,          &
       get_phy_unit_id,             &
       get_phy_unit_descr,          &
       get_phy_unit_si_base_convf,  &
       get_phy_unit_si_base_offset, &
       get_phy_unit_max_si_base
  !
  ! [A.4] BASIS-Modul mit Typ und Methoden Variablenbezeichner
  !
  USE b_var, ONLY :       &
       ! Datentyp
       t_var,             &
       ! Routinen / Interfaces
       init_var,          &
       clear_var,         &
       setup_var_prn_lun, &
       setup_var_trc_lun, &
       get_var_id
  !
  ! [A.5] BASIS-Modul mit Typ und Methoden Attributbezeichner
  !
  USE b_att, ONLY :       &
       ! Datentyp
       t_att,             &
       ! Konstante
       c_att_name,        &
       ! Routinen / Interfaces
       init_att,          &
       clear_att,         &
       setup_att_prn_lun, &
       setup_att_trc_lun, &
       is_att_in,         &
       is_att_ch,         &
       get_att_in,        &
       get_att_ch,        &
       get_att_idx,       &
       get_att_nof_values
  !
  ! [A.6] BASIS-Modul mit Typ und Methoden Dimensionsbezeichner
  !
  USE b_omi_dim, ONLY :               &
       ! Datentyp
       t_omi_dim,                     &
       ! Routinen / Interfaces
       init_omi_dim,                  &
       clear_omi_dim,                 &
       get_omi_dim_length,            &
       get_omi_dim_mass,              &
       get_omi_dim_time,              &
       get_omi_dim_electriccurrent,   &
       get_omi_dim_temperature,       &
       get_omi_dim_amountofsubstance, &
       get_omi_dim_luminousintensity, &
       get_omi_dim_currency
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
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_unit_id=40          ! 
  !! max. Anzahl der Zeichen in der Komponente <EM>description</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_unit_description=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! id          : kurzer Identifikationsbezeichner, z.B. "mm/h"       <BR>
  !! description : verbale Beschreibung, z.B. "Millimeter pro Stunde"  <BR>
  !! convfactosi : Umrechnungsfaktor der Einheit in SI-Basiseinheiten  <BR>
  !! offsettosi  : Offset gegen Null (in SI-Einheiten)                 
  TYPE , PUBLIC :: t_omi_unit
     PRIVATE
     CHARACTER (LEN=c_len_omi_unit_id)          :: id          ! 
     CHARACTER (LEN=c_len_omi_unit_description) :: description ! 
     REAL (KIND=Double)                         :: convfactosi ! 
     REAL (KIND=Double)                         :: offsettosi  ! 
  END TYPE t_omi_unit
  !
  ! [C.2] Konstantwerte (Parameter)
  !
  !! Undefined-Wert f&uuml;r REAL(Double)-Komponenten
  REAL (KIND=Double) , PUBLIC , PARAMETER :: c_undef_omi_unit_double=1.0E+31_Double ! 
  !! Undefined-Wert f&uuml;r CHARACTER-Komponenten
  CHARACTER (LEN=9)  , PUBLIC , PARAMETER :: c_undef_omi_unit_char='undefined'      ! 
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
  INTERFACE init_omi_unit
     MODULE PROCEDURE init_omi_unit_d ! 
  END INTERFACE
  !
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_unit
     MODULE PROCEDURE clear_omi_unit_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_unit_prn_lun
     MODULE PROCEDURE setup_omi_unit_prn_lun_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_unit_trc_lun
     MODULE PROCEDURE setup_omi_unit_trc_lun_d ! 
  END INTERFACE
  !! Index f&uuml;r Spracheinstellung setzen <BR>
  !! 1 = Deutsch (Default) <BR>
  !! 2 = Englisch         
  INTERFACE setup_omi_unit_language
     MODULE PROCEDURE setup_omi_unit_language_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_unit" (Skalar, 1D-Array) und
  !! Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE new_omi_unit
     MODULE PROCEDURE new_omi_unit_0  ! 
     MODULE PROCEDURE new_omi_unit_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_unit" (Skalar, 1D-Array) und
  !! Re-Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE kill_omi_unit
     MODULE PROCEDURE kill_omi_unit_0 ! 
     MODULE PROCEDURE kill_omi_unit_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_unit" auf G&uuml;ltigkeit: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE ok_omi_unit
     MODULE PROCEDURE ok_omi_unit_0 ! 
     MODULE PROCEDURE ok_omi_unit_1 ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_unit"; alle Komponenten des Typs 
  !! "t_omi_unit" auf <EM>PRN_LUN</EM> ausgeben:                  <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)                         <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)                      <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_unit
     MODULE PROCEDURE print_omi_unit_0 ! 
     MODULE PROCEDURE print_omi_unit_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_unit_static
     MODULE PROCEDURE print_omi_unit_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_unit_all_errors
     MODULE PROCEDURE print_omi_unit_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "id" in "t_omi_unit" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_unit_id
     MODULE PROCEDURE set_omi_unit_id_0_0 !
     MODULE PROCEDURE set_omi_unit_id_1_0 ! 
  END INTERFACE
  !! Setze Komponente "description" in "t_omi_unit" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_unit_description
     MODULE PROCEDURE set_omi_unit_description_0_0 ! 
     MODULE PROCEDURE set_omi_unit_description_1_0 ! 
  END INTERFACE
  !! Setze Komponente "convfactosi" in "t_omi_unit" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_unit_convfactosi
     MODULE PROCEDURE set_omi_unit_convfactosi_0_0 ! 
     MODULE PROCEDURE set_omi_unit_convfactosi_1_0 ! 
  END INTERFACE
  !! Setze Komponente "offsettosi" in "t_omi_unit" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_unit_offsettosi
     MODULE PROCEDURE set_omi_unit_offsettosi_0_0 ! 
     MODULE PROCEDURE set_omi_unit_offsettosi_1_0 ! 
  END INTERFACE
  !
  !! Index f&uuml;r Spracheinstellung ermitteln <BR>
  !! a) Deutsch (Default) <BR>
  !! b) Englisch         
  INTERFACE get_omi_unit_language
     MODULE PROCEDURE get_omi_unit_language_d ! 
  END INTERFACE
  !! Hole Komponente "id" aus "t_omi_unit":   <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)     <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_unit_id
     MODULE PROCEDURE get_omi_unit_id_0_0 ! 
     MODULE PROCEDURE get_omi_unit_id_1_0 ! 
  END INTERFACE
  !! Hole Komponente "description" aus "t_omi_unit": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)            <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_unit_description
     MODULE PROCEDURE get_omi_unit_description_0_0 ! 
     MODULE PROCEDURE get_omi_unit_description_1_0 ! 
  END INTERFACE
  !! Hole Komponente "convfactosi" aus "t_omi_unit": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)            <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_unit_convfactosi
     MODULE PROCEDURE get_omi_unit_convfactosi_0_0 ! 
     MODULE PROCEDURE get_omi_unit_convfactosi_1_0 ! 
  END INTERFACE
  !! Hole Komponente "offsettosi" aus "t_omi_unit": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)          <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_unit_offsettosi
     MODULE PROCEDURE get_omi_unit_offsettosi_0_0 ! 
     MODULE PROCEDURE get_omi_unit_offsettosi_1_0 ! 
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Setze alle Komponenten mit Hilfe verschiedener Hilfsinformationen: <BR>
  !! a) bei vorgegebener Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! b) bei vorgegebenen Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en <BR>
  !! c) bei vorgegebener Beschreibung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! d) bei vorgegebenen Beschreibungen mehrerer physikalischer Gr&ouml;&szlig;en <BR>
  !! e) bei vorgegebener Variablenbezeichnung (t_var), Attributliste (t_att(:)) und 
  !!    Dimensionsbeschreibung (t_dim) <BR>
  !! e) bei vorgegebenen Variablenbezeichnungen (t_var(:)), Attributliste (t_att(:) und
  !!    Dimensionsbeschreibungen (t_dim(:)) 
  INTERFACE set_omi_unit
     MODULE PROCEDURE set_omi_unit_code_0_0
     MODULE PROCEDURE set_omi_unit_code_1_1
     MODULE PROCEDURE set_omi_unit_descr_0_0
     MODULE PROCEDURE set_omi_unit_descr_1_1
     MODULE PROCEDURE set_omi_unit_var_att_dim_0
     MODULE PROCEDURE set_omi_unit_var_att_dim_1
  END INTERFACE
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_unit": <BR>
  !! a) f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! b) f&uuml;r viele Werte <EM>id(:)</EM> (Vektor)
  INTERFACE get_omi_unit_idx
     MODULE PROCEDURE get_omi_unit_idx_1_0
     MODULE PROCEDURE get_omi_unit_idx_1_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_unit" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_unit
     MODULE PROCEDURE eq_omi_unit_0_0  ! 
     MODULE PROCEDURE eq_omi_unit_0_1  ! 
     MODULE PROCEDURE eq_omi_unit_1_0  ! 
     MODULE PROCEDURE eq_omi_unit_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_unit" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE ne_omi_unit
     MODULE PROCEDURE ne_omi_unit_0_0  ! 
     MODULE PROCEDURE ne_omi_unit_0_1  ! 
     MODULE PROCEDURE ne_omi_unit_1_0  ! 
     MODULE PROCEDURE ne_omi_unit_1_1  ! 
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
  PUBLIC :: init_omi_unit
  PUBLIC :: clear_omi_unit
  PUBLIC :: setup_omi_unit_prn_lun
  PUBLIC :: setup_omi_unit_trc_lun
  PUBLIC :: new_omi_unit
  PUBLIC :: kill_omi_unit
  PUBLIC :: ok_omi_unit
  PUBLIC :: print_omi_unit
  PUBLIC :: print_omi_unit_static
  PUBLIC :: print_omi_unit_all_errors
  PUBLIC :: set_omi_unit_id
  PUBLIC :: set_omi_unit_description
  PUBLIC :: set_omi_unit_convfactosi
  PUBLIC :: set_omi_unit_offsettosi
  PUBLIC :: get_omi_unit_id
  PUBLIC :: get_omi_unit_description
  PUBLIC :: get_omi_unit_convfactosi
  PUBLIC :: get_omi_unit_offsettosi
  PUBLIC :: eq_omi_unit
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: setup_omi_unit_language
  PUBLIC :: set_omi_unit  
  PUBLIC :: get_omi_unit_language
  PUBLIC :: get_omi_unit_idx 
  PUBLIC :: ne_omi_unit
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
  CHARACTER (LEN=10), PARAMETER :: c_modname      = 'b_omi_unit' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_unit
  INTEGER           , PARAMETER :: c_nofcomp      =  4               ! ggf. modifizieren
  !! Anzahl einstellbarer Sprachen
  INTEGER           , PARAMETER :: c_max_language = 2                ! [Deutsch,Englisch]
  !! Default-Language
  INTEGER           , PARAMETER :: c_def_language = 1                ! [Deutsch]
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
  SUBROUTINE init_omi_unit_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='init_omi_unit_d' 
    !! Hilfsvariable
    LOGICAL :: ok ! 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_unit" version 3.1 of 12/21/06                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_var     ( )
       IF ( no_error( ) ) CALL init_att     ( )
       IF ( no_error( ) ) CALL init_phy     ( )
       IF ( no_error( ) ) CALL init_omi_dim ( )
       IF ( no_error( ) ) ok = ok_phy   ( )
       ! ... ggf. weitere Initialisierungen ergaenzen
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_unit_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_unit_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_unit_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='clear_omi_unit_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_unit_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_dim ( )
       IF ( no_error( ) ) CALL clear_phy     ( )
       IF ( no_error( ) ) CALL clear_att     ( )
       IF ( no_error( ) ) CALL clear_var     ( )
       ! ... ggf. weitere De-Initialisierungen ergaenzen
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_unit_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_unit_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_unit_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_var_prn_lun   ( lun )
       IF ( no_error( ) ) CALL setup_att_prn_lun   ( lun )
       IF ( no_error( ) ) CALL setup_phy_prn_lun   ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_unit_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_unit_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_unit_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_var_trc_lun   ( lun )
       IF ( no_error( ) ) CALL setup_att_trc_lun   ( lun )
       IF ( no_error( ) ) CALL setup_phy_trc_lun   ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_unit_trc_lun_d
  !
  !! Setzen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  SUBROUTINE setup_omi_unit_language_d ( var )
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER , INTENT(IN) :: var ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_omi_unit_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       language = MERGE ( var, c_def_language, ( 1 <= var .AND. var <= c_max_language ) )
       IF ( no_error( ) ) CALL setup_phy_language ( language ) 
    END IF
    !
  END SUBROUTINE setup_omi_unit_language_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_unit_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_omi_unit_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%id          = REPEAT( ' ', LEN(this%id         ) )
       this%id          = c_undef_omi_unit_char   ! 
       this%description = REPEAT( ' ', LEN(this%description) )
       this%description = c_undef_omi_unit_char   ! 
       this%convfactosi = c_undef_omi_unit_double ! 
       this%offsettosi  = c_undef_omi_unit_double !
    END IF
    !
  END SUBROUTINE new_omi_unit_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_unit_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_omi_unit_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_unit_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_unit_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_unit_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_omi_unit_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_unit_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_unit_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_unit_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_omi_unit_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_unit_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_unit_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_unit_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_omi_unit_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_omi_unit_id          ( this )
       l_ok(2)  = ok_omi_unit_description ( this )
       l_ok(3)  = ok_omi_unit_convfactosi ( this )
       l_ok(4)  = ok_omi_unit_offsettosi  ( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_unit_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_unit_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_omi_unit_1' 
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
          ok(i) = ok_omi_unit_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_unit_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_unit_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_omi_unit_0' 
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
       IF ( no_error( ) ) CALL print_omi_unit_id          ( this )
       IF ( no_error( ) ) CALL print_omi_unit_description ( this )
       IF ( no_error( ) ) CALL print_omi_unit_convfactosi ( this )
       IF ( no_error( ) ) CALL print_omi_unit_offsettosi  ( this )
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
8000 FORMAT('# Beginn Objekt t_omi_unit ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_unit ------------------------------')
    !
  END SUBROUTINE print_omi_unit_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_unit_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_omi_unit_1' 
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
          IF ( no_error( ) ) CALL print_omi_unit_0 ( this(i) )
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
  END SUBROUTINE print_omi_unit_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_unit_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_omi_unit_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, language, &
           TRIM(c_undef_omi_unit_char), c_undef_omi_unit_double
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_omi_unit_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_unit         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#   initialised = ',L1,/ &
    '#        prn_op = ',L1,/ &
    '#        trc_op = ',L1,/ &
    '#       prn_lun = ',I5,/ &
    '#       trc_lun = ',I5,/ &
    '#        n_init = ',I5,/ &
    '#      language = ',I5,/ &
    '#   undef[char] = ',A,/ &
    '# undef[double] = ',G15.6,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_omi_unit_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_unit_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_omi_unit_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_unit_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_id_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%id = REPEAT( ' ', LEN(this%id) )
    this%id = val
    !
  END SUBROUTINE set_omi_unit_id_0_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_id_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !
    this(:)%id = REPEAT( ' ', LEN(this%id) )
    this(:)%id = val
    !
  END SUBROUTINE set_omi_unit_id_1_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_description_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%description = REPEAT( ' ', LEN(this%description) )
    this%description = val
    !
  END SUBROUTINE set_omi_unit_description_0_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_description_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !
    this(:)%description = REPEAT( ' ', LEN(this%description) )
    this(:)%description = val
    !
  END SUBROUTINE set_omi_unit_description_1_0
  !
  !! weise der Komponente "convfactosi" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_convfactosi_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "convfactosi"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%convfactosi = val
    !
  END SUBROUTINE set_omi_unit_convfactosi_0_0
  !
  !! weise der Komponente "convfactosi" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_convfactosi_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "convfactosi"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%convfactosi = val
    !
  END SUBROUTINE set_omi_unit_convfactosi_1_0
  !
  !! weise der Komponente "offsettosi" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_offsettosi_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "offsettosi"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%offsettosi = val
    !
  END SUBROUTINE set_omi_unit_offsettosi_0_0
  !
  !! weise der Komponente "offsettosi" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_offsettosi_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "offsettosi"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%offsettosi = val
    !
  END SUBROUTINE set_omi_unit_offsettosi_1_0
  !
  !! Setze alle Komponenten eines Datenobjektes auf der Basis der 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_unit_code_0_0 ( this, code ) 
    !! Datenobjekt (Skalar) 
    TYPE (t_omi_unit) , INTENT(INOUT) :: this ! 
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER           , INTENT(IN)    :: code ! 
    !
    CALL new_omi_unit_0 ( this )
    !
    IF ( is_phy_quant_valid( code ) ) THEN
       this%id          = REPEAT( ' ', LEN(this%id) )
       this%id          = get_phy_unit_id    ( code )
       this%description = REPEAT( ' ', LEN(this%description) )
       this%description = get_phy_unit_descr ( code )
       this%convfactosi = get_phy_unit_si_base_convf ( code )
       this%offsettosi  = get_phy_unit_si_base_offset ( code )
    END IF
    !
  END SUBROUTINE set_omi_unit_code_0_0
  !
  !! Setze alle Komponenten mehrerer Datenobjekte auf der Basis der 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_unit_code_1_1 ( this, code ) 
    !! Datenobjekte (Vektor) 
    TYPE (t_omi_unit) , INTENT(INOUT) :: this(:) ! 
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER           , INTENT(IN)    :: code(:) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    CALL new_omi_unit_1 ( this(:) )
    !
    DO i=1,MIN(SIZE(this),SIZE(code))
       IF ( any_error( ) ) EXIT
       CALL set_omi_unit_code_0_0 ( this(i), code(i) )
    END DO
    !
  END SUBROUTINE set_omi_unit_code_1_1
  !
  !! Setze alle Komponenten eines Datenobjektes auf der Basis der 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_unit_descr_0_0 ( this, descr ) 
    !! Datenobjekt (Skalar) 
    TYPE (t_omi_unit) , INTENT(INOUT) :: this ! 
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN)    :: descr ! 
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER :: code ! 
    !
    code = get_phy_quant_code  ( descr      )
    CALL set_omi_unit_code_0_0 ( this, code )
    !
  END SUBROUTINE set_omi_unit_descr_0_0
  !
  !! Setze alle Komponenten mehrerer Datenobjekte auf der Basis der 
  !! Beschreibungen verschiedener physikalischer Gr&ouml;&szlig;en <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_unit_descr_1_1 ( this, descr ) 
    !! Datenobjekte (Vektor) 
    TYPE (t_omi_unit) , INTENT(INOUT) :: this(:) ! 
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN)    :: descr(:) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    CALL new_omi_unit_1 ( this(:) )
    !
    DO i=1,MIN(SIZE(this),SIZE(descr))
       IF ( any_error( ) ) EXIT
       CALL set_omi_unit_descr_0_0 ( this(i), descr(i) )
    END DO
    !
  END SUBROUTINE set_omi_unit_descr_1_1
  !
  !! Setze alle Komponenten eines Datenobjektes auf der Basis einer
  !! Variablenbeschreibung sowie einer Attributliste <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_unit_var_att_dim_0 ( this, var, att, dim ) 
    !! Datenobjekt (Skalar) 
    TYPE (t_omi_unit) , INTENT(INOUT) :: this     ! 
    !! Variablenbezeichner
    TYPE (t_var)      , INTENT(IN)    :: var      ! 
    !! Attributliste
    TYPE (t_att)      , INTENT(IN)    :: att(:)   ! 
    !! Dimensionsbezeichner (OpenMI-konform)
    TYPE (t_omi_dim)  , INTENT(IN)    :: dim      ! 
    !! verschiedene Hifsgr&ouml;&szlig;en
    INTEGER , PARAMETER   :: c_max=2                          !  
    INTEGER , PARAMETER   :: c_idx(c_max)= (/41,38/)          ! 
    REAL (KIND=Double) , ALLOCATABLE :: si_base(:)            ! 
    INTEGER               :: i, var_id, idx(c_max), nof, code !  
    CHARACTER (LEN=80) , ALLOCATABLE :: ch(:) ! 
    INTEGER            , ALLOCATABLE :: in(:) ! 
    !
    CALL new_omi_unit_0 ( this )
    ALLOCATE ( si_base(get_phy_unit_max_si_base()) )
    ! [1] lokales Feld "si_base(:)" mit Exponenten der Basisdimensionen fuellen
    DO i=1,SIZE(si_base)
       SELECT CASE ( i )
       CASE ( 1 ) ! 'm  '
          si_base(1) = get_omi_dim_length            ( dim )
       CASE ( 2 ) ! 'kg '
          si_base(2) = get_omi_dim_mass              ( dim )
       CASE ( 3 ) ! 's  '
          si_base(3) = get_omi_dim_time              ( dim )
       CASE ( 4 ) ! 'A  '
          si_base(4) = get_omi_dim_electriccurrent   ( dim )
       CASE ( 5 ) ! 'K  '
          si_base(5) = get_omi_dim_temperature       ( dim )
       CASE ( 6 ) ! 'mol'
          si_base(6) = get_omi_dim_amountofsubstance ( dim )
       CASE ( 7 ) ! 'cd '
          si_base(7) = get_omi_dim_luminousintensity ( dim )
       CASE ( 8 ) ! 'E  '
          si_base(8) = get_omi_dim_currency          ( dim )
       END SELECT
    END DO
    ! [2] Code-Bezeichnung der physikalischen Groesse bestimmen
    var_id = get_var_id  ( var )
    code   = -1
    DO i=1,c_max
       idx(i) = get_att_idx ( att(:), c_att_name(c_idx(i)), var_id ) 
       IF ( idx(i) > 0 ) THEN
          IF ( is_att_ch ( att(idx(i)) ) ) THEN
             nof = get_att_nof_values ( att(idx(i)) )
             IF ( nof == 1 ) THEN
                ALLOCATE ( ch(nof) )
                ch   = get_att_ch( att(idx(i)) )
                code = get_phy_quant_code ( ch(1) )
                DEALLOCATE ( ch )
             END IF
          ELSE IF ( is_att_in ( att(idx(i)) ) ) THEN
             nof = get_att_nof_values ( att(idx(i)) )
             IF ( nof == 1 ) THEN
                ALLOCATE ( in(nof) )
                in   = get_att_in( att(idx(i)) )
                code = in(1)
                DEALLOCATE ( in )
             END IF
          END IF
       END IF
       IF ( idx(1) > 0 ) EXIT ! preference to "name_id"
    END DO
    ! [3] Komponenten von "this" mit aktuellen Werten belegen
    this%id          = REPEAT( ' ', LEN(this%id) )
    this%id          = get_phy_unit_id             ( code, si_base(:) )
    this%description = REPEAT( ' ', LEN(this%description) )
    this%description = get_phy_unit_descr          ( code, si_base(:) )
    this%convfactosi = get_phy_unit_si_base_convf  ( code, si_base(:) )
    this%offsettosi  = get_phy_unit_si_base_offset ( code, si_base(:) )
    !
    DEALLOCATE ( si_base )
    !
  END SUBROUTINE set_omi_unit_var_att_dim_0
  !
  !! Setze alle Komponenten eines Datenobjektes auf der Basis vieler
  !! Variablenbeschreibungen sowie einer Attributliste <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_unit_var_att_dim_1 ( this, var, att, dim ) 
    !! Datenobjekte (Vektor) 
    TYPE (t_omi_unit) , INTENT(INOUT) :: this(:)  ! 
    !! Veriablenbezeichner
    TYPE (t_var)      , INTENT(IN)    :: var(:)   ! 
    !! Attributliste  
    TYPE (t_att)      , INTENT(IN)    :: att(:)   ! 
    !! Dimensionsbezeichner (OpenMI-konform)
    TYPE (t_omi_dim)  , INTENT(IN)    :: dim(:)   ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    CALL new_omi_unit_1 ( this(:) )
    !
    DO i=1,MIN(SIZE(this),SIZE(dim),SIZE(var))
       IF ( any_error( ) ) EXIT
       CALL set_omi_unit_var_att_dim_0 ( this(i), var(i), att(:), dim(i) )
    END DO
    !
  END SUBROUTINE set_omi_unit_var_att_dim_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! Holen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  FUNCTION get_omi_unit_language_d ( ) &
       RESULT( res )
    !! R&uuml;ckgabewert: 
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER :: res ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='get_omi_unit_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       res = language
    ELSE
       res = -1
    END IF
    !
  END FUNCTION get_omi_unit_language_d
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_id_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit)   , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    CHARACTER (LEN=c_len_omi_unit_id) :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_omi_unit_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_id_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit)   , INTENT(IN)  :: this(:)          ! 
    !! R&uuml;ckgabewert "id"
    CHARACTER (LEN=c_len_omi_unit_id) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%id
    !
  END FUNCTION get_omi_unit_id_1_0
  !
  !! hole die Komponente "description" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_description_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit)            , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "description" (Skalar)
    CHARACTER (LEN=c_len_omi_unit_description) :: val  ! 
    !
    val = this%description
    !
  END FUNCTION get_omi_unit_description_0_0
  !
  !! hole die Komponente "description" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_description_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit)            , INTENT(IN)  :: this(:)         ! 
    !! R&uuml;ckgabewert "description"
    CHARACTER (LEN=c_len_omi_unit_description) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%description
    !
  END FUNCTION get_omi_unit_description_1_0
  !
  !! hole die Komponente "convfactosi" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_convfactosi_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "convfactosi" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%convfactosi
    !
  END FUNCTION get_omi_unit_convfactosi_0_0
  !
  !! hole die Komponente "convfactosi" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_convfactosi_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "convfactosi"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%convfactosi
    !
  END FUNCTION get_omi_unit_convfactosi_1_0
  !
  !! hole die Komponente "offsettosi" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_offsettosi_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_unit) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "offsettosi" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%offsettosi
    !
  END FUNCTION get_omi_unit_offsettosi_0_0
  !
  !! hole die Komponente "offsettosi" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_offsettosi_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "offsettosi"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%offsettosi
    !
  END FUNCTION get_omi_unit_offsettosi_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_unit" f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this(:) ! 
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
  END FUNCTION get_omi_unit_idx_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_unit" f&uuml;r viele Werte <EM>id(:)</EM> (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_unit_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this(:) ! 
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
       res(i) = get_omi_unit_idx_1_0 ( this(:), val(i) )
    END DO
    !
  END FUNCTION get_omi_unit_idx_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_unit_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = ( this1%id          == this2%id          )
    l_ok(2)  = ( this1%description == this2%description )
    l_ok(3)  = ( this1%convfactosi == this2%convfactosi )
    l_ok(4)  = ( this1%offsettosi  == this2%offsettosi  )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_unit_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_unit_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_unit_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_unit_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_unit_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_unit_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_unit_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_unit_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_unit_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_unit_1_1
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
  FUNCTION ne_omi_unit_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_unit_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_unit_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_unit_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this2    ! 
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_unit_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_unit_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_unit_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_unit) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_unit_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_unit_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_unit_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_unit) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l     = SIZE(ok)
    ok(:) = .NOT. eq_omi_unit_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_unit_1_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_unit" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_unit ausfuehren'
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
  SUBROUTINE init_omi_unit_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER  :: c_upname='init_omi_unit_all_errors' !
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
               '--> INIT_omi_unit ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_unit ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_unit"\n'//&
               'Typ-Komponente = "id" darf nicht leer/undefined sein\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_unit"\n'//&
               'Typ-Komponente = "description" darf nicht leer/undefined sein\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_unit"\n'//&
               'Typ-Komponente = "convfactosi" darf nicht 0.0/undefined sein\n'//&
               'aktuell        = <aktuell>\n'//&
               'undefiniert    = <undef>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_unit"\n'//&
               'Typ-Komponente = "offsettosi" darf nicht undefined sein\n'//&
               'aktuell        = <aktuell>\n'//&
               'undefiniert    = <undef>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_unit" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_unit" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_unit" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_unit"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_omi_unit" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_unit"\n'//&
               'Typ-Komponente = "description"\n'//&
               '--> Code in Modul "b_omi_unit" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_unit"\n'//&
               'Typ-Komponente = "convfactosi"\n'//&
               '--> Code in Modul "b_omi_unit" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_unit"\n'//&
               'Typ-Komponente = "offsettosi"\n'//&
               '--> Code in Modul "b_omi_unit" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_unit"\n'//&
               '--> Code in Modul "b_omi_unit" / Daten pruefen' )
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
  END SUBROUTINE init_omi_unit_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_unit_all_errors &
       ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_unit_all_errors
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
  FUNCTION ok_omi_unit_id &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_omi_unit_id' ! 
    !
    ok = ( LEN_TRIM(this%id) > 0 .AND. &
           this%id(1:LEN(c_undef_omi_unit_char)) /= c_undef_omi_unit_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<aktuell>', this%id )
    END IF
    !
  END FUNCTION ok_omi_unit_id
  !
  !! Pr&uuml;fe, ob die Komponente "description" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_unit_description &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='ok_omi_unit_description' ! 
    !
    ok = ( LEN_TRIM(this%description) > 0 .AND. &
           this%description(1:LEN(c_undef_omi_unit_char)) /= c_undef_omi_unit_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<aktuell>', this%description )
    END IF
    !
  END FUNCTION ok_omi_unit_description
  !
  !! Pr&uuml;fe, ob die Komponente "convfactosi" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_unit_convfactosi &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='ok_omi_unit_convfactosi' ! 
    !! Hilfsvariable
    CHARACTER (LEN=15) :: ch ! 
    !
    ok = ( this%convfactosi /= 0.0_Double .AND. this%convfactosi /= c_undef_omi_unit_double )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(ch,'(G15.6)') this%convfactosi        ; CALL setup_error_act ( '<aktuell>', ch )
       WRITE(ch,'(G15.6)') c_undef_omi_unit_double ; CALL setup_error_act ( '<undef>', ch )
    END IF
    !
  END FUNCTION ok_omi_unit_convfactosi
  !
  !! Pr&uuml;fe, ob die Komponente "offsettosi" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_unit_offsettosi &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_unit_offsettosi' ! 
    !! Hilfsvariable
    CHARACTER (LEN=15) :: ch ! 
    !
    ok = ( this%offsettosi /= c_undef_omi_unit_double )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       WRITE(ch,'(G15.6)') this%offsettosi         ; CALL setup_error_act ( '<aktuell>', ch )
       WRITE(ch,'(G15.6)') c_undef_omi_unit_double ; CALL setup_error_act ( '<undef>', ch )
    END IF
    !
  END FUNCTION ok_omi_unit_offsettosi
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_unit_id &
       ( this )
    !
    !! Datenobjekt
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_omi_unit_id' ! 
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
  END SUBROUTINE print_omi_unit_id
  !
  !! Drucke den Inhalt der Komponente "description" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_unit_description &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=26) , PARAMETER :: c_upname='print_omi_unit_description' ! 
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
  END SUBROUTINE print_omi_unit_description
  !
  !! Drucke den Inhalt der Komponente "convfactosi" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_unit_convfactosi &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=26) , PARAMETER :: c_upname='print_omi_unit_convfactosi' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%convfactosi
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente convfactosi - - - - - - - - - - - - - ',/&
           '# aktuell = ',G15.6,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_unit_convfactosi
  !
  !! Drucke den Inhalt der Komponente "offsettosi" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_unit_offsettosi &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_unit) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='print_omi_unit_offsettosi' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%offsettosi
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente offsettosi  - - - - - - - - - - - - - ',/&
           '# aktuell = ',G15.6,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_unit_offsettosi
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
END MODULE b_omi_unit
! TailOfBaseModule --------------------------------------------------------
