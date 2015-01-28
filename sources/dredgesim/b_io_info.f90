! <DataType10>        : Typdefinition der 10ten Komponente von "t_io_info"
! 
! <ComponentName10>   : Name der 10ten Komponente von "t_io_info"
!
! --> Nicht benoetigte Teile aus Modul loeschen.
! --> bis hierher loeschen ------------------------------------------------
! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden zum Verwalten von Informationen ueber Ein- und Ausgabedateien</h2>
!! @author G. Lang
!! @version 1.2 vom 03/22/07, Quellcode: mod_b_io_info.f90
!! <HR>
!! type and methods to administrate in- and output data files <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2007 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2007-03-19 : G. Lang : Erstversion
!  01.02 : 2007-03-22 : G. Lang : neue Komponente "tsec(:)"; "get_time_position" ueberarbeitet
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Sammlung von eng zusammengh&ouml;renden Daten f&uuml;r IO-Dateien <BR>
!! sowie den geeigneten Methoden in einem Basismodul.                <BR>
!! Derzeit ist nur ein Teil der Daten sowie ein Teil der Methoden    <BR>
!! integriert, die insbesondere f&uuml;r das Lesen von Daten         <BR>
!! ben&ouml;tigt werden.                                             <BR>
!!                                                                   <BR>
!! F&uuml;r das Schreiben m&uuml;ssen noch geeignete Daten und       <BR>
!! Methoden erg&auml;nzt werden.                                     <BR>
!!                                                                   <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_io_info <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> file      : Dateibezeichner
!!     <LI> key       : Schl&uuml;sselwort
!!     <LI> id        : ein-eindeutige Kennnummer des korrespondierenden Datensatzbezeichners
!!     <LI> pac       : Bezeichnung des Lesepakets
!!     <LI> code(:)   : (optional) Codekennnung der erforderlichen Groessen
!!     <LI> dim(:)    : (optional) Dimensionen in Datei
!!     <LI> var(:)    : (optional) Variablen in Datei
!!     <LI> att(:)    : (optional) Attribute in Datei
!!     <LI> time(:)   : (optional) Zeitangaben in Datei
!!     <LI> lasttime  : Indikator fuer den zuletzt angesprochenen Zeitpunkt zur Optimierung des Lesens
!!     <LI> tsec(:)   : (optional) Zeitangaben bezogen auf time(1) in Sekunden
!!     <LI> <ComponentName10> : <kurze Beschreibung10>
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
!!    <LI> Initialisieren des Moduls "b_io_info" mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls "b_io_info" mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   
!!          andere nicht. Routinen die Fehlermeldungen generieren 
!!          m&uuml;ssen pr&uuml;fen, ob das Modul korrekt initialisert 
!!          wurde (ok_initialised)  <BR>
!!          Mit print_io_info_all_errors kann eine Liste aller 
!!          von dem Modul erzeugter Fehlermeldungen generiert werden.
!
MODULE b_io_info
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit "globalen Konstantwerten" ----------------------
  USE b_constants, ONLY : &
       ! Parameter
       Single, Double
  ! [A.2] BASIS-Modul mit "Fehler-Typ und -Methoden" ---------------------
  USE b_error, ONLY :       &
       ! Typdefinitionen
       t_error,             &
       ! Routinen
       init_error, clear_error, &
       no_error, any_error,     &
       new_error, kill_error, print_error, &
       setup_error_prn_lun, setup_error_trc_lun, setup_error_act, &
       set_error_ierr, set_error_cerr
  ! [A.3] BASIS-Modul mit "Datei-Typ und -Methoden" ----------------------
  USE b_file, ONLY :       &
       ! Typdefinitionen
       t_file,             &
       ! Routinen
       init_file, clear_file,                                  &
       new_file, kill_file, print_file, ok_file, eq_file,      &
       setup_file_prn_lun, setup_file_trc_lun,                 &
       file_is_none, eq_file_path_and_name,                    & 
       set_file_name, set_file_recl, set_file_unit,            &
       get_file_name, get_file_path
  ! [A.4] BASIS-Modul mit "Definition der physikalischen Groessen" ------
  USE b_phy, ONLY :        &
       ! Routinen / Methoden
       init_phy, clear_phy, &
       setup_phy_prn_lun, setup_phy_trc_lun, &
       is_phy_quant_valid, get_phy_quant_descr
  ! [A.5] BASIS-Modul mit "Dimensionen" ---------------------------------
  USE b_dim, ONLY : &
       ! Datentyp
       t_dim,       &
       ! Konstante
       c_dim_name,  &
       ! Routinen / Methoden
       init_dim, clear_dim, &
       setup_dim_prn_lun, setup_dim_trc_lun, &
       ok_dim, print_dim, valid_unlimited_dim_name, &
       OPERATOR(==)
  ! [A.6] BASIS-Modul mit "Variablen" -----------------------------------
  USE b_var, ONLY :    &
       ! Datentyp
       t_var,          &
       ! Konstante
       c_len_var_name, &
       ! Routinen / Methoden
       init_var, clear_var, &
       setup_var_prn_lun, setup_var_trc_lun,       &
       ok_var, print_var, is_unlimited_var,        &
       get_var_name, get_var_start, get_var_shape, &
       OPERATOR(==)
  ! [A.7] BASIS-Modul mit "Attributen" ----------------------------------
  USE b_att, ONLY : &
       ! Datentyp
       t_att, &
       ! Konstante
       c_att_name, &
       ! Routinen / Methoden
       init_att, clear_att, &
       setup_att_prn_lun, setup_att_trc_lun, &
       ok_att, print_att, &
       has_att_name_id, has_att_minimum_water_depth, has_att_dynamic_bathymetry, &
       is_datetime_in_att_period, get_att_ch_as_datetime, &
       get_att_idx, get_att_nof_values, is_att_ch, get_att_interfaces_count, &
       get_att_var_name_id, get_att_minimum_water_depth, &
       OPERATOR(==)
  ! [A.8] BASIS-Module mit "Datums- und Zeitreichnung" -----------------
  USE b_datetime, ONLY : &
       ! Datentyp
       t_datetime, &
       ! Routinen / Methoden
       init_datetime, clear_datetime, &
       setup_datetime_prn_lun, setup_datetime_trc_lun, &
       new_datetime, kill_datetime, ok_datetime, print_datetime, &
       datetime_to_string, su_datetime, mean_datetime, &
       OPERATOR(==)
  ! [A.9] BASIS-Modul mit "Zeitdifferenzrechnung" ---------------------
  USE b_time, ONLY : &
       ! Datentyp
       t_time, &
       ! Routinen / Methoden
       time_to_real_seconds
  !
  ! [A.4] weitere BASIS-Module (ONLY benutzen!)
  !
  ! USE b_<BaseModuleName>, ONLY : &
  !   Typdefinitionen
  !   Parameter 
  !   Variablen mit INTENT(IN)
  !   Variablen mit INTENT(INOUT)
  !   Variablen mit INTENT(OUT)
  !   Routinen / Interfaces
  !   init_<BaseModuleName>,          &
  !   clear_<BaseModuleName>,         &
  !   setup_<BaseModuleName>_prn_lun, &
  !   setup_<BaseModuleName>_trc_lun
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
  ! [C.0] Konstantwerte (z. B. fuer die Typdeklaration) -----------------
  !
  !! maximale L&auml;nge der Komponente <EM>key</EM> 
  INTEGER , PUBLIC , PARAMETER :: c_max_len_key=80 ! 
  !! maximale L&auml;nge der Komponente <EM>package</EM>
  INTEGER , PUBLIC , PARAMETER :: c_max_len_pac=10 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren -----
  !
  !! file      : Dateibezeichner <BR>
  !! key       : Schl&uuml;sselwort <BR>
  !! id        : ein-eindeutige Kennnummer des korrespondierenden Datensatzbezeichners <BR>
  !! pac       : Bezeichnung des Lesepakets <BR>
  !! code      : (optional) Codekennnung der erforderlichen Groessen <BR>
  !! dim(:)    : (optional) Dimensionen in Datei <BR>
  !! var(:)    : (optional) Variablen in Datei <BR>
  !! att(:)    : (optional) Attribute in Datei <BR>
  !! time(:)   : (optional) Zeitangaben in Datei <BR>
  !! lasttime  : Indikator fuer den zuletzt angesprochenen Zeitpunkt zur Optimierung des Lesens
  !! tsec(:)   : Zeitangaben bezogen auf time(1) in Sekunden
  !! <ComponentName10> : <kurze Beschreibung10>
  TYPE , PUBLIC :: t_io_info
     PRIVATE
     TYPE (t_file)                 :: file      ! Dateibezeichner
     CHARACTER (LEN=c_max_len_key) :: key       ! Schluesselwort
     INTEGER                       :: id        ! Kennnummer des Datensatzbezeichners
     CHARACTER (LEN=c_max_len_pac) :: pac       ! Bezeichnung des Lesepakets
     INTEGER  , POINTER            :: code(:)   ! (optional) Code-Kennung der erforderlichen phys. Groessen
     TYPE (t_dim) , POINTER        :: dim(:)    ! (optional) Dimensionen in Datei
     TYPE (t_var) , POINTER        :: var(:)    ! (optional) Variablen in Datei
     TYPE (t_att) , POINTER        :: att(:)    ! (optional) Attribute in Datei
     TYPE (t_datetime) , POINTER   :: time(:)   ! (optional) Zeitangaben in Datei
     INTEGER                       :: lasttime  ! Indikator fuer den zuletzt angesprochenen Termin in time(:)
     REAL (KIND=Double) , POINTER  :: tsec(:)   ! (optional) Zeitangaben in Sekunden bezogen auf time(1)
!!$     <DataType10> <,Attributes10> :: <ComponentName10>
  END TYPE t_io_info
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
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_io_info
     MODULE PROCEDURE init_io_info_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_io_info
     MODULE PROCEDURE clear_io_info_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_io_info_prn_lun
     MODULE PROCEDURE setup_io_info_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_io_info_trc_lun
     MODULE PROCEDURE setup_io_info_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_io_info" (Skalar, 1D-Array); <BR>
  !! NULLIFY f&uuml;r dynamische Komponenten-Felder;            <BR>
  !! Initialisieren mit Default-Werten.
  INTERFACE new_io_info
     MODULE PROCEDURE new_io_info_0  ! Version fuer Skalar
     MODULE PROCEDURE new_io_info_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_io_info" (Skalar, 1D-Array); <BR>
  !! ggf. De-Allokieren von Memory;                               <BR>
  !! teilweise Re-Initialisieren mit Default-Werten.
  INTERFACE kill_io_info
     MODULE PROCEDURE kill_io_info_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_io_info_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Pr&uuml;fen von Datenobjekten "t_io_info" auf G&uuml;ltigkeit (Skalar, 1D-Array)
  INTERFACE ok_io_info
     MODULE PROCEDURE ok_io_info_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_io_info_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Drucken von Datenobjekten "t_io_info" (Skalar, 1D-Array); <BR>
  !! Alle Komponenten des Typs "t_io_info" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_io_info
     MODULE PROCEDURE print_io_info_0 ! Version fuer Skalar
     MODULE PROCEDURE print_io_info_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_io_info_static
     MODULE PROCEDURE print_io_info_static_d ! 
  END INTERFACE
  !
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_io_info_all_errors
     MODULE PROCEDURE print_io_info_all_errors_d ! 
  END INTERFACE
  !
  ! ----------------------------------------------------------------
  ! --> nicht benoetigte SET-Interfaces bitte unbedingt loeschen <--
  ! ----------------------------------------------------------------
  !
  !! Setze Komponente "file" in "t_io_info" auf Benutzerwert <BR>
  !! a) skalares Datenobjekt und skalare Daten               <BR>
  !! b) vektorielles Datenobjekt und skalare Daten           <BR>
  !! Hinweis: die externen Daten werden auf die Komponente kopiert
  INTERFACE set_io_info_file
     MODULE PROCEDURE set_io_info_file_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_io_info_file_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "key" in "t_io_info" auf Benutzerwert <BR>
  !! a) skalares Datenobjekt und skalare Daten              <BR>
  !! b) vektorielles Datenobjekt und skalare Daten          <BR>
  !! Hinweis: die externen Daten werden auf die Komponente kopiert
  INTERFACE set_io_info_key
     MODULE PROCEDURE set_io_info_key_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_io_info_key_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "id" in "t_io_info" auf Benutzerwert <BR>
  !! a) skalares Datenobjekt und skalare Daten             <BR>
  !! b) vektorielles Datenobjekt und skalare Daten         <BR>
  !! Hinweis: die externen Daten werden auf die Komponente kopiert
  INTERFACE set_io_info_id
     MODULE PROCEDURE set_io_info_id_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_io_info_id_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "pac" in "t_io_info" auf Benutzerwert <BR>
  !! a) skalares Datenobjekt und skalare Daten              <BR>
  !! b) vektorielles Datenobjekt und skalare Daten          <BR>
  !! Hinweis: die externen Daten werden auf die Komponente kopiert
  INTERFACE set_io_info_pac
     MODULE PROCEDURE set_io_info_pac_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_io_info_pac_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "code" in "t_io_info" auf Benutzerwert; <BR>
  !! a) skalares Datenobjekt und vektorielle Daten            <BR>
  !! b) vektorielles Datenobjekt und vektorielle Daten        <BR>
  !! Hinweis: die externen Daten werden auf die Komponente kopiert
  INTERFACE set_io_info_code
     MODULE PROCEDURE set_io_info_code_0_1 ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_io_info_code_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "dim(:)" in "t_io_info" auf Benutzerwert; <BR>
  !! a) skalares Datenobjekt und vektorielle Daten              <BR>
  !! Hinweis: es wird ein Zeiger auf die extern allokierten daten eingerichtet 
  INTERFACE set_io_info_dim_ref
     MODULE PROCEDURE set_io_info_dim_ref_0_1 ! Objekt (Skalar) / Daten (Vektor)
  END INTERFACE
  !! Setze Komponente "var(:)" in "t_io_info" auf Benutzerwert; <BR>
  !! a) skalares Datenobjekt und vektorielle Daten              <BR>
  !! Hinweis: es wird ein Zeiger auf die extern allokierten daten eingerichtet 
  INTERFACE set_io_info_var_ref
     MODULE PROCEDURE set_io_info_var_ref_0_1 ! Objekt (Skalar) / Daten (Vektor)
  END INTERFACE
  !! Setze Komponente "att(:)" in "t_io_info" auf Benutzerwert; <BR>
  !! a) skalares Datenobjekt und vektorielle Daten              <BR>
  !! Hinweis: es wird ein Zeiger auf die extern allokierten daten eingerichtet <BR>
  !!          und die Komponente "time(:)" wird zusaetzlich automatisch gesetzt
  INTERFACE set_io_info_att_ref
     MODULE PROCEDURE set_io_info_att_ref_0_1 ! Objekt (Skalar) / Daten (Vektor)
  END INTERFACE
  !! Setze Komponente "time" in "t_io_info" auf Benutzerwert; <BR>
  !! a) skalares Datenobjekt und vektorielle Daten            <BR>
  !! Hinweis: die Daten werden auf die interne Komponente kopiert
  INTERFACE set_io_info_time
     MODULE PROCEDURE set_io_info_time_0_1 ! Objekt (Skalar) / Daten (Vektor)
  END INTERFACE
  !! Setze Komponente "lasttime" in "t_io_info" auf Benutzerwert
  INTERFACE set_io_info_lasttime
     MODULE PROCEDURE set_io_info_lasttime_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_io_info_lasttime_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "tsec" in "t_io_info" auf Benutzerwert; <BR>
  !! a) skalares Datenobjekt und vektorielle Daten (REAL) <BR>
  !! b) skalares Datenobjekt und vektorielle Daten (Datum und Uhrzeit) 
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten.
  INTERFACE set_io_info_tsec
     MODULE PROCEDURE set_io_info_tsec_0_1   ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_io_info_tsec_0_1_t ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
!!$  !
!!$  ! --- Version fuer skalare Komponente "<ComponentName10>" [ggf. entfernen]
!!$  !
!!$  !! Setze Komponente "<ComponentName10>" in "t_io_info" auf Benutzerwert
!!$  INTERFACE set_io_info_<ComponentName10>
!!$     MODULE PROCEDURE set_io_info_<ComponentName10>_0_0 ! Objekt (Skalar) / Daten (Skalar)
!!$     MODULE PROCEDURE set_io_info_<ComponentName10>_1_0 ! Objekt (Vektor) / Daten (Skalar) 
!!$  END INTERFACE
!!$  !
!!$  ! --- Version fuer dynamische Feld-Komponente "<ComponentName10>" [ggf. entfernen]
!!$  !
!!$  !! Setze Komponente "<ComponentName10>" in "t_io_info" auf Benutzerwert; <BR>
!!$  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten.
!!$  INTERFACE set_io_info_<ComponentName10>
!!$     MODULE PROCEDURE set_io_info_<ComponentName10>_0_1 ! Objekt (Skalar) / Daten (Vektor)
!!$     MODULE PROCEDURE set_io_info_<ComponentName10>_1_1 ! Objekt (Vektor) / Daten (Vektor) 
!!$  END INTERFACE
!!$  !
!!$  ! ... ggf. Setzen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! ----------------------------------------------------------------
  ! --> nicht benoetigte GET-Interfaces bitte unbedingt loeschen <--
  ! ----------------------------------------------------------------
  !
  !! Hole Komponente "file" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt                <BR>
  !! b) vektorielles Datenobjekt            <BR>
  !! Hinweis: es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_file
     MODULE PROCEDURE get_io_info_file_0_0 ! Skalar
     MODULE PROCEDURE get_io_info_file_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "key" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt               <BR>
  !! b) vektorielles Datenobjekt           <BR>
  !! Hinweis: es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_key
     MODULE PROCEDURE get_io_info_key_0_0 ! Skalar
     MODULE PROCEDURE get_io_info_key_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "id" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt              <BR>
  !! b) vektorielles Datenobjekt          <BR>
  !! c) vekt. Datenobjekt, Dateibezeichner, Paketbezeichner <BR>
  !! Hinweis: es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_id
     MODULE PROCEDURE get_io_info_id_0_0 ! Skalar
     MODULE PROCEDURE get_io_info_id_1_0 ! Vektor
     MODULE PROCEDURE get_io_info_id_1_p ! existierender Dateibezeichner
  END INTERFACE
  !! Hole Komponente "pac" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt               <BR>
  !! b) vektorielles Datenobjekt           <BR>
  !! Hinweis: es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_pac
     MODULE PROCEDURE get_io_info_pac_0_0 ! Skalar
     MODULE PROCEDURE get_io_info_pac_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "code" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt                <BR>
  !! Hinweis: es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_code_ref
     MODULE PROCEDURE get_io_info_code_ref_0_1 ! Skalar
  END INTERFACE
  !! Hole in this%code <BR>
  !! a) skalares Datenobjekt und Nummer n
  INTERFACE get_io_info_code
     MODULE PROCEDURE get_io_info_code_0_n
  END INTERFACE
  !! Hole Komponente "dim(:)" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt                  <BR>
  !! Hinweis: es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_dim_ref
     MODULE PROCEDURE get_io_info_dim_ref_0_1 ! Skalar
  END INTERFACE
  !! Hole Komponente "var(:)" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt                  <BR>
  !! Hinweis: es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_var_ref
     MODULE PROCEDURE get_io_info_var_ref_0_1 ! Skalar
  END INTERFACE
  !! Hole Komponente "att(:)" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt                  <BR>
  !! Hinweis: es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_att_ref
     MODULE PROCEDURE get_io_info_att_ref_0_1 ! Skalar
  END INTERFACE
  !! Hole Komponente "time" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt                <BR>
  !! Hinweis: es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_time_ref
     MODULE PROCEDURE get_io_info_time_ref_0_1 ! Skalar
  END INTERFACE
  !! Hole Komponente "lasttime" aus "t_io_info"
  INTERFACE get_io_info_lasttime
     MODULE PROCEDURE get_io_info_lasttime_0_0 ! Skalar
     MODULE PROCEDURE get_io_info_lasttime_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "tsec" aus "t_io_info" <BR>
  !! a) skalares Datenobjekt                <BR>
  !! Hinweis: es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_io_info_tsec_ref
     MODULE PROCEDURE get_io_info_tsec_ref_0_1 ! Skalar
  END INTERFACE
!!$  !
!!$  ! --- Version fuer skalare Komponente "<ComponentName10>" [ggf. entfernen]
!!$  !
!!$  !! Hole Komponente "<ComponentName10>" aus "t_io_info"
!!$  INTERFACE get_io_info_<ComponentName10>
!!$     MODULE PROCEDURE get_io_info_<ComponentName10>_0_0 ! Skalar
!!$     MODULE PROCEDURE get_io_info_<ComponentName10>_1_0 ! Vektor
!!$  END INTERFACE
!!$  !
!!$  ! --- Version fuer dynamische Feld-Komponente "<ComponentName10>" [ggf. entfernen]
!!$  !
!!$  !! Hole Komponente "<ComponentName10>" aus "t_io_info"
!!$  INTERFACE get_io_info_<ComponentName10>
!!$     MODULE PROCEDURE get_io_info_<ComponentName10>_0_1 ! Skalar
!!$  END INTERFACE
!!$  !
!!$  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Ermittle die Anzahl der Objekte mit einem bestimmten Wert der Komponente <EM>pac</EM> <BR>
  !! a) vektorielles Datenobjekt und ein Textvergleichswert <BR>
  !! b) vektorielles Datenobjekt und mehrere Textvergleichswerte
  INTERFACE get_io_info_pac_count
     MODULE PROCEDURE get_io_info_pac_count_0
     MODULE PROCEDURE get_io_info_pac_count_1
  END INTERFACE
  !! Ermittle die Indexposition des n-ten Objekts mit einem bestimmten Wert der Komponenten <EM>pac</EM>
  !! a) vektorielles Datenobjekt, ein Textvergleichswert, Nummer des Objektes
  INTERFACE get_io_info_pac_idx
     MODULE PROCEDURE get_io_info_pac_idx_0
  END INTERFACE
  !! Hole Indexposition des ersten passenden Eintrags in einem Feld vom Typ "t_io_info" <BR>
  !! a) vekt. Datenobjekt, Dateibezeichner, Paketbezeichner, ID
  INTERFACE get_io_info_idx
     MODULE PROCEDURE get_io_info_idx_1_p ! existierender Dateibezeichner
  END INTERFACE
  !! Ermittle die Anzahl der Code-Kennungen <BR>
  !! a) skalares Datenobjekt
  INTERFACE get_io_info_code_count
     MODULE PROCEDURE get_io_info_code_count_0
  END INTERFACE
  !! Ermittle den Variablenname "var_name" f&uuml;r die n-te Code-Bezeichnung <BR>
  !! a) skalares Datenobjekt
  INTERFACE get_io_info_var_name
     MODULE PROCEDURE get_io_info_var_name_0_n
  END INTERFACE
  !! Ermittle das Feld "start(:)" f&uuml;r die n-te Code-Bezeichnung und einen bestimmten Termin <BR>
  !! a) skalares Datenobjekt
  INTERFACE get_io_info_start
     MODULE PROCEDURE get_io_info_start_0_n_t
  END INTERFACE
  !! Ermittle das Feld "shape(:)" f&uuml;r die n-te Code-Bezeichnung <BR>
  !! a) skalares Datenobjekt
  INTERFACE get_io_info_shape
     MODULE PROCEDURE get_io_info_shape_0_n
  END INTERFACE
  !! Ermittle die minimale Wasserbedeckung <BR>
  !! a) skalares Datenobjekt <BR>
  !! b) vektorielles Datenobjekt
  INTERFACE get_io_info_min_water_depth
     MODULE PROCEDURE get_io_info_min_water_depth_0
     MODULE PROCEDURE get_io_info_min_water_depth_1
  END INTERFACE
  !
  !! (Re-) Initialisiere die Paket-ID aller Objekte eines Pakettyps mit vorgegebener ID 
  INTERFACE init_io_info_id
     MODULE PROCEDURE init_io_info_id_1_p
  END INTERFACE
  !
  !! Pr&uuml;fe, ob die erforderlichen physikalischen Gr&ouml;&szlig;en zur Verf&uuml;gung stehen <BR>
  !! a) skalares Datenobjekt <BR>
  !! b) vektorielles Datenobjekt
  INTERFACE check_io_info_code_avail
     MODULE PROCEDURE check_io_info_code_avail_0
     MODULE PROCEDURE check_io_info_code_avail_1
  END INTERFACE 
  !! Pr&uuml;fe, ob eine Zeitangabe innerhalb der verf&uuml;gbaren Zeitraums liegt <BR>
  !! a) skalares Datenobjekt <BR>
  !! b) vektorielles Datenobjekt
  INTERFACE check_io_info_time_avail
     MODULE PROCEDURE check_io_info_time_avail_0
     MODULE PROCEDURE check_io_info_time_avail_1
  END INTERFACE
  !
  !! Ermittle, ob es das IO-Info-Objekt zwei-dimensionale Daten enth&auml;lt <BR>
  !! a) skalares Datenobjekt <BR>
  !! b) vektorielles Datenobjekt
  INTERFACE has_io_info_2d
     MODULE PROCEDURE has_io_info_2d_0
     MODULE PROCEDURE has_io_info_2d_1
  END INTERFACE
  !! Ermittle, ob es das IO-Info-Objekt drei-dimensionale Daten enth&auml;lt <BR>
  !! a) skalares Datenobjekt <BR>
  !! b) vektorielles Datenobjekt
  INTERFACE has_io_info_3d
     MODULE PROCEDURE has_io_info_3d_0
     MODULE PROCEDURE has_io_info_3d_1
  END INTERFACE
  !! Ermittle, ob f&uuml;r eine bestimmte Code-Bezeichnung eine minimale Wasserbedeckung
  !! vorhanden ist <BR>
  !! a) skalares Datenobjekt <BR>
  !! b) vektorielles Datenobjekt 
  INTERFACE has_io_info_min_water_depth
     MODULE PROCEDURE has_io_info_min_water_depth_0
     MODULE PROCEDURE has_io_info_min_water_depth_1
  END INTERFACE
  !! Ermittle, ob f&uuml;r eine bestimmte Code-Bezeichnung die <EM>unlimited</EM>-Dimension
  !! einen vorgegebenen Namen aufweist <BR>
  !! a) skalares Datenobjekt, Code-Positionsnummer, Name der Unlimited-Dimension
  INTERFACE has_io_info_code_unlimited
     MODULE PROCEDURE has_io_info_code_unlimited_0_n
  END INTERFACE
  !! Ermittle, ob die in einer Datei liegenden Daten, f&uuml;r zeitvariable Bathymetrie
  !! erzeugt wurden oder nicht <BR>
  !! a) skalares Datenobjekt
  INTERFACE has_io_info_dyn_bathymetry
     MODULE PROCEDURE has_io_info_dyn_bathymetry_0
  END INTERFACE
  !
  ! ... ggf. ergaenzen
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren (Funktionen)
  !
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !
  ! Hinweis: Operatoren wurden fuer vier verschieden Parameter-
  !          Konstellationen formuliert. Ggf. nicht sinnvolle
  !          Konstellationen entfernen oder weitere sinnvolle
  !          hinzufuegen.
  !
  !          Die eigentlich nahe liegende Form der Verwendung
  !          von Operatoren wir "==" oder "/=" hat sich nicht
  !          bewaehrt, da manche Compiler damit nicht klargekommen
  !          sind.
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_io_info" auf Gleichheit
  INTERFACE eq_io_info
     MODULE PROCEDURE eq_io_info_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_io_info_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_io_info_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_io_info_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_io_info
  PUBLIC :: clear_io_info
  PUBLIC :: setup_io_info_prn_lun
  PUBLIC :: setup_io_info_trc_lun
  PUBLIC :: new_io_info
  PUBLIC :: kill_io_info
  PUBLIC :: ok_io_info
  PUBLIC :: print_io_info
  PUBLIC :: print_io_info_static
  PUBLIC :: print_io_info_all_errors
  PUBLIC :: set_io_info_file
  PUBLIC :: set_io_info_key
  PUBLIC :: set_io_info_id
  PUBLIC :: set_io_info_pac
  PUBLIC :: set_io_info_code
  PUBLIC :: set_io_info_dim_ref
  PUBLIC :: set_io_info_var_ref
  PUBLIC :: set_io_info_att_ref
  PUBLIC :: set_io_info_time
  PUBLIC :: set_io_info_lasttime
  PUBLIC :: set_io_info_tsec
!!$  PUBLIC :: set_io_info_<ComponentName10>
  PUBLIC :: get_io_info_file
  PUBLIC :: get_io_info_key
  PUBLIC :: get_io_info_id
  PUBLIC :: get_io_info_pac
  PUBLIC :: get_io_info_code_ref
  PUBLIC :: get_io_info_code
  PUBLIC :: get_io_info_dim_ref
  PUBLIC :: get_io_info_var_ref
  PUBLIC :: get_io_info_att_ref
  PUBLIC :: get_io_info_time_ref
  PUBLIC :: get_io_info_lasttime
  PUBLIC :: get_io_info_tsec_ref
!!$  PUBLIC :: get_io_info_<ComponentName10>
  PUBLIC :: eq_io_info
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: get_io_info_pac_count       ! 
  PUBLIC :: get_io_info_pac_idx         ! 
  PUBLIC :: get_io_info_idx             ! 
  PUBLIC :: get_io_info_code_count      ! 
  PUBLIC :: get_io_info_var_name        ! 
  PUBLIC :: get_io_info_start           ! 
  PUBLIC :: get_io_info_shape           ! 
  PUBLIC :: get_io_info_min_water_depth ! 
  PUBLIC :: init_io_info_id             ! 
  PUBLIC :: check_io_info_code_avail    !
  PUBLIC :: check_io_info_time_avail    !
  PUBLIC :: has_io_info_2d              ! 
  PUBLIC :: has_io_info_3d              ! 
  PUBLIC :: has_io_info_min_water_depth ! 
  PUBLIC :: has_io_info_code_unlimited  ! 
  PUBLIC :: has_io_info_dyn_bathymetry  ! 
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
!!$  ! <kurze Beschreibung des Typs>
!!$  TYPE :: t_<DerivedTypeName>
!!$     PRIVATE
!!$     <DataType> <,Attributes> :: <ComponentName> ! <kurze Beschreibung>
!!$  END TYPE t_<DerivedTypeName>
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=09), PARAMETER :: c_modname      = 'b_io_info' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_io_info
  INTEGER           , PARAMETER :: c_nofcomp      = 11               ! ggf. modifizieren
  !! String zum Kennzeichnen ung&uuml;ltiger CHARACTER-Werte
  CHARACTER (LEN=1) , PARAMETER :: c_undef_ch='?'  !
  !! Wert zum Kennzeichnen ung&uuml;tiger ganzzahliger Gr&ouml;&szlig;en
  INTEGER           , PARAMETER :: c_undef_in=-999 ! 
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
  !! ermittle die Werte eines logischen Feldes f&uuml;r Paketbezeichner
  !! unter Ber&uuml;cksichtigung des Dateibezeichners "NONE"
  INTERFACE get_log_pac
     MODULE PROCEDURE get_log_pac_d
  END INTERFACE
  !! ermittle die Werte eines logischen Feldes f&uuml;r Stringvergleiche
  INTERFACE get_log_arr
     MODULE PROCEDURE get_log_arr_ch_d
  END INTERFACE
  !! Ermittle die Indexposition des n-ten g&uuml;tigen Eintrags 
  INTERFACE get_log_arr_idx
     MODULE PROCEDURE get_log_arr_idx_d
  END INTERFACE
  !! Ermittle die Position der Variablen f&uuml;r die n-te Code-Bezeichnung
  INTERFACE get_code_var_position
     MODULE PROCEDURE get_code_var_position_0_n
  END INTERFACE
  !! Ermittle die Position der Variablen f&uuml;r die n-te Code-Bezeichnung
  INTERFACE get_time_position
     MODULE PROCEDURE get_time_position_0_t
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
  SUBROUTINE init_io_info_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='init_io_info_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_io_info" version 1.2 of 03/22/07                    '
          WRITE(*,*) ' Copyright (C) 2007 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF 
       !
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_file       ( )
       IF ( no_error( ) ) CALL init_phy        ( )
       IF ( no_error( ) ) CALL init_dim        ( )
       IF ( no_error( ) ) CALL init_var        ( )
       IF ( no_error( ) ) CALL init_att        ( )
       IF ( no_error( ) ) CALL init_datetime   ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_io_info_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_io_info_d
  !
  !! (Re-) Initialisiere die Paket-ID aller Objekte eines Pakettyps mit vorgegebener ID <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_io_info_id_1_p ( this, var, id )
    !! Datenobjekte
    TYPE (t_io_info)  , INTENT(INOUT) :: this(:) ! 
    !! Vergleichswert f&uuml;r die Komponente "pac"
    CHARACTER (LEN=*) , INTENT(IN)    :: var     ! 
    !! Kennung ID
    INTEGER           , INTENT(IN)    :: id      ! 
    ! Hilfsvariablen
    LOGICAL , ALLOCATABLE :: l_tf(:) !  
    INTEGER :: i                     ! 
    !
    ALLOCATE(l_tf(SIZE(this)))
    !
    l_tf = get_log_pac ( this, var )
    DO i=1,SIZE(l_tf)
       IF ( .NOT. l_tf(i) ) CYCLE
       IF ( this(i)%id == id ) this(i)%id = c_undef_in
    END DO
    !
    DEALLOCATE(l_tf)
    !
  END SUBROUTINE init_io_info_id_1_p
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_io_info_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='clear_io_info_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_io_info_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_datetime ( )
       IF ( no_error( ) ) CALL clear_att ( )
       IF ( no_error( ) ) CALL clear_var ( )
       IF ( no_error( ) ) CALL clear_dim ( )
       IF ( no_error( ) ) CALL clear_phy        ( )
       IF ( no_error( ) ) CALL clear_file       ( )
       ! IF ( no_error( ) ) CALL clear_<BaseModuleName> ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error      ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_io_info_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_io_info_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_io_info_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun      ( lun )
       IF ( no_error( ) ) CALL setup_file_prn_lun       ( lun )
       IF ( no_error( ) ) CALL setup_phy_prn_lun        ( lun )
       IF ( no_error( ) ) CALL setup_dim_prn_lun        ( lun )
       IF ( no_error( ) ) CALL setup_var_prn_lun        ( lun )
       IF ( no_error( ) ) CALL setup_att_prn_lun        ( lun )
       IF ( no_error( ) ) CALL setup_datetime_prn_lun   ( lun )
    END IF
    !
  END SUBROUTINE setup_io_info_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_io_info_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_io_info_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun    ( lun )
       IF ( no_error( ) ) CALL setup_file_trc_lun     ( lun )
       IF ( no_error( ) ) CALL setup_phy_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_dim_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_var_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_att_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_datetime_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_io_info_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_io_info_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_io_info_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       CALL new_file      ( this%file         )
       CALL set_file_name ( this%file, 'NONE' )
       CALL set_file_recl ( this%file, 1      )
       CALL set_file_unit ( this%file, 0      )
       this%key      = REPEAT( ' ' , LEN(this%key) )
       this%key      = c_undef_ch
       this%id       = c_undef_in
       this%pac      = c_undef_ch
       this%lasttime = c_undef_in
!!$       this%<ComponentName10> = <Initialisierungs-Wert fuer "<ComponentName10>">
!!$       !
!!$       ! ... optional fuer dynamisch allokierbare Feld-Komponenten
!!$       !
       NULLIFY ( this%code )
       NULLIFY ( this%dim  )
       NULLIFY ( this%var  )
       NULLIFY ( this%att  )
       NULLIFY ( this%time )
       NULLIFY ( this%tsec )
!!$       ! NULLIFY ( this%<ComponentName10> )
       !
    END IF
    !
  END SUBROUTINE new_io_info_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR> 
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE new_io_info_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13) , PARAMETER :: c_upname='new_io_info_1' ! 
    !! Z&auml;hler      
    INTEGER                        :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein 
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL new_io_info_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_io_info_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_io_info_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_io_info_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL dealloc_io_info_code( this )
       IF ( no_error( ) ) CALL dealloc_io_info_time( this )
       IF ( no_error( ) ) CALL dealloc_io_info_tsec( this )
       ! IF ( no_error( ) ) CALL dealloc_io_info_<ComponentName10>( this )
       IF ( no_error( ) ) CALL new_io_info_0 ( this )
    END IF
    !
  END SUBROUTINE kill_io_info_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_io_info_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_io_info_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL kill_io_info_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_io_info_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_io_info_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_io_info_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok( 1) = ok_io_info_file ( this )
       l_ok( 2) = ok_io_info_key ( this )
       l_ok( 3) = ok_io_info_id ( this )
       l_ok( 4) = ok_io_info_pac ( this )
       l_ok( 5) = ok_io_info_code ( this )
       l_ok( 6) = ok_io_info_dim ( this )
       l_ok( 7) = ok_io_info_var ( this )
       l_ok( 8) = ok_io_info_att ( this )
       l_ok( 9) = ok_io_info_time ( this )
       l_ok(10) = ok_io_info_lasttime ( this )
       l_ok(11) = ok_io_info_tsec ( this )
!!$       l_ok(12) = ok_io_info_<ComponentName10>( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_io_info_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_io_info_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_io_info_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_io_info_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_io_info_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_io_info_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_io_info_file ( this )
       IF ( no_error( ) ) CALL print_io_info_key ( this )
       IF ( no_error( ) ) CALL print_io_info_id ( this )
       IF ( no_error( ) ) CALL print_io_info_pac ( this )
       IF ( no_error( ) ) CALL print_io_info_code ( this )
       IF ( no_error( ) ) CALL print_io_info_dim ( this )
       IF ( no_error( ) ) CALL print_io_info_var ( this )
       IF ( no_error( ) ) CALL print_io_info_att ( this )
       IF ( no_error( ) ) CALL print_io_info_time ( this )
       IF ( no_error( ) ) CALL print_io_info_lasttime ( this )
       IF ( no_error( ) ) CALL print_io_info_tsec ( this )
!!$       IF ( no_error( ) ) CALL print_io_info_<ComponentName10>( this )
       !
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
       END IF
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT('# Beginn Objekt t_io_info ------------------------------')
8001 FORMAT('# Ende   Objekt t_io_info ------------------------------')
    !
  END SUBROUTINE print_io_info_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_io_info_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) i
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          IF ( no_error( ) ) CALL print_io_info_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_io_info_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_static_d ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_io_info_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) &
            initialised, prn_op, trc_op, prn_lun, trc_lun, n_init
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_io_info_all_errors_d ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_io_info              ',/ &
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
  END SUBROUTINE print_io_info_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=26), PARAMETER :: c_upname='print_io_info_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_io_info_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "file" einen skalaren Wert zu (Skalar)  <BR>
  !! die externen Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_file_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "file"
    TYPE (t_file)    , INTENT(IN)    :: val  ! 
    !
    this%file = val
    !
  END SUBROUTINE set_io_info_file_0_0
  !
  !! weise der Komponente "file" einen skalaren Wert zu (Vektor)  <BR>
  !! die externen Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_file_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "file"
    TYPE (t_file)    , INTENT(IN)    :: val     ! 
    !
    this%file = val
    !
  END SUBROUTINE set_io_info_file_1_0
  !
  !! weise der Komponente "key" einen skalaren Wert zu (Skalar)   <BR>
  !! die externen Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_key_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "key"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%key = REPEAT( ' ', LEN(this%key) )
    this%key = val(1:MIN(LEN(this%key),LEN_TRIM(val)))
    !
  END SUBROUTINE set_io_info_key_0_0
  !
  !! weise der Komponente "key" einen skalaren Wert zu (Vektor)   <BR>
  !! die externen Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_key_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info)              , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "key"
    CHARACTER (LEN=c_max_len_key) , INTENT(IN)    :: val     ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_io_info_key_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_io_info_key_1_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar)    <BR>
  !! die externen Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_id_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    INTEGER          , INTENT(IN)    :: val  ! 
    !
    this%id = val
    !
  END SUBROUTINE set_io_info_id_0_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Vektor)    <BR>
  !! die externen Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_id_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id"
    INTEGER          , INTENT(IN)    :: val     ! 
    !
    this%id = val
    !
  END SUBROUTINE set_io_info_id_1_0
  !
  !! weise der Komponente "pac" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_pac_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "pac"
    CHARACTER (LEN=*)   , INTENT(IN) :: val  ! 
    !
    this%pac = REPEAT( ' ', LEN(this%pac) )
    this%pac = val(1:MIN(LEN(this%pac),LEN_TRIM(val)))
    !
  END SUBROUTINE set_io_info_pac_0_0
  !
  !! weise der Komponente "pac" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_pac_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "pac"
    CHARACTER (LEN=*)  , INTENT(IN)  :: val     ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_io_info_pac_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_io_info_pac_1_0
  !
  !! weise der dynamischen Komponente "code" ein Feld zu (Skalar) <BR>
  !! die Daten werden auf die interne Komponente kopiert          <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_io_info_code_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "code"
    INTEGER            , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='set_io_info_code_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_io_info_code ( this            )
       IF ( no_error( ) ) CALL alloc_io_info_code   ( this, SIZE(val) )
       IF ( no_error( ) ) this%code(:) = val(:)
    END IF
    !
  END SUBROUTINE set_io_info_code_0_1
  !
  !! weise der dynamischen Komponente "code" ein Feld zu (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert          <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_io_info_code_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "code"
    INTEGER            , INTENT(IN)  :: val(:)  ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       IF ( any_error( ) ) EXIT
       CALL set_io_info_code_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_io_info_code_1_1
  !
  !! weise der dynamischen Komponente "dim(:)" ein Feld zu (Skalar)                  <BR>
  !! von der internen Komponente wird ein Zeiger auf die externen Daten eingerichtet <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_io_info_dim_ref_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "dim"
    TYPE (t_dim)     , POINTER       :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=23) , PARAMETER   :: c_upname='set_io_info_dim_ref_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       this%dim => val
    END IF
    !
  END SUBROUTINE set_io_info_dim_ref_0_1
  !
  !! weise der dynamischen Komponente "var(:)" ein Feld zu (Skalar)                  <BR>
  !! von der internen Komponente wird ein Zeiger auf die externen Daten eingerichtet <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_io_info_var_ref_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "var"
    TYPE (t_var)     , POINTER       :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=23) , PARAMETER   :: c_upname='set_io_info_var_ref_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       this%var => val
    END IF
    !
  END SUBROUTINE set_io_info_var_ref_0_1
  !
  !! weise der dynamischen Komponente "att(:)" ein Feld zu (Skalar)                  <BR>
  !! von der internen Komponente wird ein Zeiger auf die externen Daten eingerichtet <BR>
  !! Hinweis: die Komponente "time" wird zus&auml;tzlich automatisch gesetzt         <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_io_info_att_ref_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "att"
    TYPE (t_att)     , POINTER       :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=23) , PARAMETER   :: c_upname='set_io_info_att_ref_0_1' 
    !! Indexpositionen der zeitbeschreibenden Attribute
    INTEGER , PARAMETER :: c_max_id=4                 ! 
    !! Liste der Attribut-Typen <BR>
    !! 3 = beginning_date       <BR>
    !! 4 = ending_date          <BR>
    !! 6 = single_date          <BR>
    !! 7 = multiple_dates
    INTEGER , PARAMETER :: c_id(c_max_id) = (/3,4,6,7/) ! 
    ! Hilfsvariablen
    TYPE (t_datetime) , ALLOCATABLE :: time(:) ! 
    INTEGER :: idx(c_max_id)                ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       this%att => val
       ! automatischer Transfer in die Komponente "time"
       idx = get_att_idx( this%att, c_att_name(c_id) )
       IF      ( idx(4) > 0 ) THEN
          IF ( is_att_ch( this%att(idx(4)) ) ) THEN
             ALLOCATE( time(get_att_nof_values(this%att(idx(4)))) )
             time = get_att_ch_as_datetime( this%att(idx(4)) )
             CALL set_io_info_time( this, time )
             CALL set_io_info_tsec( this, time )
             DEALLOCATE( time )
          END IF
       ELSE IF ( idx(3) > 0 ) THEN
          IF ( is_att_ch( this%att(idx(3)) ) ) THEN
             ALLOCATE( time(1) )
             time = get_att_ch_as_datetime(this%att(idx(3)))
             CALL set_io_info_time( this, time )
             CALL set_io_info_tsec( this, time )
             DEALLOCATE( time )
          END IF
       ELSE IF ( idx(1) > 0 .AND. idx(2) > 0 ) THEN
          IF ( is_att_ch( this%att(idx(1)) ) .AND. is_att_ch( this%att(idx(2)) ) ) THEN
             ALLOCATE( time(2) )
             time(1:1) = get_att_ch_as_datetime(this%att(idx(1)))
             time(2:2) = get_att_ch_as_datetime(this%att(idx(2)))
             CALL set_io_info_time( this, (/ mean_datetime( time(1), time(2) ) /) )
             CALL set_io_info_tsec( this, (/ mean_datetime( time(1), time(2) ) /) )
             DEALLOCATE(time)
          END IF
       ELSE IF ( idx(1) > 0 .AND. idx(2) < 1 ) THEN
          IF ( is_att_ch( this%att(idx(1)) ) ) THEN
             ALLOCATE( time(1) )
             time = get_att_ch_as_datetime(this%att(idx(1)))
             CALL set_io_info_time( this, time )
             CALL set_io_info_tsec( this, time )
             DEALLOCATE( time )
          END IF
       ELSE IF ( idx(1) < 1 .AND. idx(2) > 0 ) THEN
          IF ( is_att_ch( this%att(idx(2)) ) ) THEN
             ALLOCATE( time(1) )
             time = get_att_ch_as_datetime(this%att(idx(2)))
             CALL set_io_info_time( this, time )
             CALL set_io_info_tsec( this, time )
             DEALLOCATE( time )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE set_io_info_att_ref_0_1
  !
  !! weise der dynamischen Komponente "time" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_io_info_time_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info)  , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "time"
    TYPE (t_datetime) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='set_io_info_time_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_io_info_time ( this            )
       IF ( no_error( ) ) CALL alloc_io_info_time   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL new_datetime         ( this%time       )
       IF ( no_error( ) ) this%time(:) = val(:)
    END IF
    !
  END SUBROUTINE set_io_info_time_0_1
  !
  !! weise der Komponente "lasttime" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_lasttime_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "lasttime"
    INTEGER           , INTENT(IN)  :: val  ! 
    !
    this%lasttime = val
    !
  END SUBROUTINE set_io_info_lasttime_0_0
  !
  !! weise der Komponente "lasttime" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_io_info_lasttime_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "lasttime"
    INTEGER           , INTENT(IN)  :: val     ! 
    !
    this%lasttime = val
    !
  END SUBROUTINE set_io_info_lasttime_1_0
  !
  !! weise der dynamischen Komponente "tsec" ein Feld zu (Skalar) <BR>
  !! es wird ein Feld mit REAL-Zahlen &uuml;bergeben <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_io_info_tsec_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "tsec"
    REAL (KIND=Double) , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='set_io_info_tsec_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       IF ( no_error( ) ) CALL dealloc_io_info_tsec ( this            )
       IF ( no_error( ) ) CALL alloc_io_info_tsec   ( this, SIZE(val) )
       IF ( no_error( ) ) this%tsec(:) = val(:)
       !
    END IF
    !
  END SUBROUTINE set_io_info_tsec_0_1
  !
  !! weise der dynamischen Komponente "tsec" ein Feld zu (Skalar) <BR>
  !! es wird ein Feld mit Datums- und Zeitangaben &uuml;bergeben, dabei
  !! wird in der Komponente "tsec(:)" die Zeitdifferenz val(:) - val(1) abgelegt  <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_io_info_tsec_0_1_t ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_io_info)  , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "tsec" als Datums- und Zeitangabe
    TYPE (t_datetime) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='set_io_info_tsec_0_1_t' 
    ! Hilfsvariablen
    REAL (KIND=Double) :: tsec(SIZE(val)) ! 
    TYPE (t_time)      :: time(SIZE(val)) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       time = su_datetime( val, val(1) )  ! Zeitdifferenz zum ersten Termin 
       tsec = time_to_real_seconds(time)
       CALL set_io_info_tsec_0_1 ( this, tsec )
    END IF
    !
  END SUBROUTINE set_io_info_tsec_0_1_t
  !
!!$  !
!!$  !! weise der Komponente "<ComponentName10>" einen skalaren Wert zu (Skalar) <BR>
!!$  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
!!$  SUBROUTINE set_io_info_<ComponentName10>_0_0 &
!!$       ( this, &
!!$         val )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt (Skalar)
!!$    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
!!$    !! Wert f&uuml;r Komponente "<ComponentName10>"
!!$    <DataType10>          , INTENT(IN)  :: val  ! 
!!$    !
!!$    this%<ComponentName10> = val
!!$    !
!!$  END SUBROUTINE set_io_info_<ComponentName10>_0_0
!!$  !
!!$  !! weise der Komponente "<ComponentName10>" einen skalaren Wert zu (Vektor) <BR>
!!$  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
!!$  SUBROUTINE set_io_info_<ComponentName10>_1_0 &
!!$       ( this, &
!!$         val )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt (Vektor)
!!$    TYPE (t_io_info) , INTENT(INOUT) :: this(:) ! 
!!$    !! Wert f&uuml;r Komponente "<ComponentName10>"
!!$    <DataType10>          , INTENT(IN)  :: val     ! 
!!$    !
!!$    this%<ComponentName10> = val
!!$    !
!!$  END SUBROUTINE set_io_info_<ComponentName10>_1_0
!!$  !
!!$  ! --- Version fuer dynamische Feld-Komponente "<ComponentName10>" [ggf. entfernen]
!!$  !
!!$  !! weise der dynamischen Komponente "<ComponentName10>" ein Feld zu (Skalar) <BR>
!!$  !! Subroutine erzeugt Fehlermeldungen
!!$  SUBROUTINE set_io_info_<ComponentName10>_0_1 &
!!$       ( this, &
!!$         val )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt (Skalar)
!!$    TYPE (t_io_info) , INTENT(INOUT) :: this   ! 
!!$    !! Werte f&uuml;r Komponente "<ComponentName10>"
!!$    <DataType10>          , INTENT(IN)  :: val(:) ! 
!!$    !
!!$    ! Lokale Parameter und Variablen
!!$    !! Name der Subroutine
!!$    CHARACTER (LEN=31), PARAMETER :: c_upname='set_io_info_<ComponentName10>_0_1' 
!!$    !
!!$    IF ( ok_initialised ( c_upname ) ) THEN
!!$       !
!!$       IF ( no_error( ) ) CALL dealloc_io_info_<ComponentName10> ( this            )
!!$       IF ( no_error( ) ) CALL alloc_io_info_<ComponentName10>   ( this, SIZE(val) )
!!$       IF ( no_error( ) ) CALL init_io_info_<ComponentName10>    ( this            )
!!$       IF ( no_error( ) ) this%<ComponentName10>(:) = val(:)
!!$       !
!!$    END IF
!!$    !
!!$  END SUBROUTINE set_io_info_<ComponentName10>_0_1
!!$  !
!!$  !! weise der dynamischen Komponente "<ComponentName10>" ein Feld zu (Vektor) <BR>
!!$  !! Subroutine erzeugt Fehlermeldungen
!!$  SUBROUTINE set_io_info_<ComponentName10>_1_1 &
!!$       ( this, &
!!$         val )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt (Vektor)
!!$    TYPE (t_io_info) , INTENT(INOUT) :: this(:) ! 
!!$    !! Werte f&uuml;r Komponente "<ComponentName10>"
!!$    <DataType10>          , INTENT(IN)  :: val(:)  ! 
!!$    !! Z&auml;hler
!!$    INTEGER :: i ! 
!!$    !
!!$    i = 0
!!$    !
!!$    DO
!!$       !
!!$       i = i + 1
!!$       !
!!$       IF ( i > SIZE( this ) .OR. any_error( ) ) EXIT
!!$       !
!!$       CALL set_io_info_<ComponentName10>_0_1 ( this(i), val(:) )
!!$       !
!!$    END DO
!!$    !
!!$  END SUBROUTINE set_io_info_<ComponentName10>_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "file" aus einem skalaren Datenobjekt     <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben               <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_file_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "file" (Skalar)
    TYPE (t_file)                  :: val  ! 
    !
    val = this%file
    !
  END FUNCTION get_io_info_file_0_0
  !
  !! hole die Komponente "file" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben               <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_io_info_file_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "file"
    TYPE (t_file)                  :: val(SIZE(this))  ! 
    !
    val = this%file
    !
  END FUNCTION get_io_info_file_1_0
  !
  !! hole die Komponente "key" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben          <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_key_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert "key" (Skalar)
    CHARACTER (LEN=c_max_len_key) :: val  ! 
    !
    val = this%key
    !
  END FUNCTION get_io_info_key_0_0
  !
  !! hole die Komponente "key" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben              <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_key_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert "key"
    CHARACTER (LEN=c_max_len_key) :: val(SIZE(this))  ! 
    !
    val = this%key
    !
  END FUNCTION get_io_info_key_1_0
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben         <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_id_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_io_info_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben             <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_id_1_0 ( this ) &
       RESULT( val ) 
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "id"
    INTEGER :: val(SIZE(this))  ! 
    !
    val = this%id
    !
  END FUNCTION get_io_info_id_1_0
  !
  !! hole einen schon vorhandenen Wert f&uuml;r die Komponente "id" 
  !! aus einem vektoriellen Datenobjekt, f&uuml;r einen vorgegebenen
  !! Dateibezeichner sowie einen Paketbezeichner                 <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben             <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_id_1_p ( this, file, var ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_io_info)  , INTENT(IN) :: this(:) !
    !! Dateibezeichner (Skalar)
    TYPE (t_file)     , INTENT(IN) :: file    ! 
    !! Paketbezeichner
    CHARACTER (LEN=*) , INTENT(IN) :: var     ! 
    !! R&uuml;ckgabewert "id" (Skalar); falls kein Wert gefunden wird,
    !! wird "c_undef_in" zur&uuml;ckgegeben
    INTEGER :: val ! 
    ! Hilfsvariablen
    INTEGER               :: i       ! 
    LOGICAL , ALLOCATABLE :: l_tf(:) ! 
    !
    val = c_undef_in
    ALLOCATE(l_tf(SIZE(this)))
    l_tf = get_log_pac( this, var )
    !
    DO i=1,SIZE(l_tf)
       IF ( val /= c_undef_in ) EXIT
       IF ( .NOT. l_tf(i)     ) CYCLE
       IF ( eq_file_path_and_name( this(i)%file, file ) ) THEN
          IF ( this(i)%id /= c_undef_in ) val = this(i)%id 
       END IF
    END DO
    !
    DEALLOCATE ( l_tf )
    !
  END FUNCTION get_io_info_id_1_p
  !
  !! hole Zeiger auf den ersten Eintrag mit &uuml;bereinstimmenden
  !! Dateibezeichner, Paketbezeichner und ID in Liste this(:) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_idx_1_p ( this, file, pac, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_io_info)  , INTENT(IN) :: this(:) !
    !! Dateibezeichner (Skalar)
    TYPE (t_file)     , INTENT(IN) :: file    ! 
    !! Paketbezeichner
    CHARACTER (LEN=*) , INTENT(IN) :: pac     ! 
    !! ID-Bezeichner
    INTEGER           , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert "idx" (Skalar); falls kein Wert gefunden wird,
    !! wird "c_undef_in" zur&uuml;ckgegeben
    INTEGER :: val ! 
    ! Hilfsvariablen
    INTEGER               :: i       ! 
    LOGICAL , ALLOCATABLE :: l_tf(:) ! 
    !
    val = c_undef_in
    ALLOCATE(l_tf(SIZE(this)))
    l_tf = get_log_pac( this, pac )
    !
    DO i=1,SIZE(l_tf)
       IF ( val /= c_undef_in ) EXIT
       IF ( .NOT. l_tf(i)     ) CYCLE
       IF ( eq_file_path_and_name( this(i)%file, file ) ) THEN
          IF ( this(i)%id == id ) val = i
       END IF
    END DO
    !
    DEALLOCATE ( l_tf )
    !
  END FUNCTION get_io_info_idx_1_p
  !
  !! hole die Komponente "pac" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben          <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_pac_0_0 ( this ) &
    RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "pac" (Skalar)
    CHARACTER (LEN=c_max_len_pac) :: val  ! 
    !
    val = this%pac
    !
  END FUNCTION get_io_info_pac_0_0
  !
  !! hole die Komponente "pac" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben              <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_pac_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(IN) :: this(:)         ! 
    !! R&uuml;ckgabewert "pac"
    CHARACTER (LEN=c_max_len_pac) :: val(SIZE(this)) ! 
    !
    val = this%pac
    !
  END FUNCTION get_io_info_pac_1_0
  !
  !! hole die dynamische Feld-Komponente "code" aus einem skalaren Datenobjekt <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben              <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_code_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this   ! 
    !! R&uuml;ckgabewert "code" (Vektor)
    INTEGER          , POINTER     :: val(:) ! 
    !
    val => this%code
    !
  END FUNCTION get_io_info_code_ref_0_1
  !
  !! hole den Wert des n-ten Eintrags in this%code            <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_code_0_n ( this, n ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this   ! 
    !! Indexposition n in this%code(:)
    INTEGER          , INTENT(IN)  :: n      ! 
    !! Ergebnis: this%code(n)
    INTEGER                        :: val    ! 
    !
    val = c_undef_in
    IF ( ASSOCIATED(this%code) ) THEN
       IF ( n >= 1 .AND. n <= SIZE(this%code) ) val = this%code(n)
    END IF
    !
  END FUNCTION get_io_info_code_0_n
  !
  !! hole die dynamische Feld-Komponente "dim(:)" aus einem skalaren Datenobjekt <BR>
  !! es wird ein Zeiger auf die intern abgelegten Daten zur&uuml;ckgegeben          <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_dim_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "val(:)" (Vektor)
    TYPE (t_dim)         , POINTER :: val(:)   ! 
    !
    val => this%dim
    !
  END FUNCTION get_io_info_dim_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "var(:)" aus einem skalaren Datenobjekt <BR>
  !! es wird ein Zeiger auf die intern abgelegten Daten zur&uuml;ckgegeben          <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_var_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "val(:)" (Vektor)
    TYPE (t_var)         , POINTER :: val(:)   ! 
    !
    val => this%var
    !
  END FUNCTION get_io_info_var_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "att(:)" aus einem skalaren Datenobjekt <BR>
  !! es wird ein Zeiger auf die intern abgelegten Daten zur&uuml;ckgegeben          <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_att_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "val(:)" (Vektor)
    TYPE (t_att)         , POINTER :: val(:)   ! 
    !
    val => this%att
    !
  END FUNCTION get_io_info_att_ref_0_1
  !
  !! hole die dynamische Feld-Komponente "time" aus einem skalaren Datenobjekt <BR>
  !! es wird ein Zeiger auf die intern abgelegten Daten zur&uuml;ckgegeben     <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_time_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert "time" (Vektor)
    TYPE (t_datetime)   , POINTER :: val(:)  ! 
    !
    val => this%time
    !
  END FUNCTION get_io_info_time_ref_0_1
  !
  !! hole die Komponente "lasttime" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_lasttime_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "lasttime" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%lasttime
    !
  END FUNCTION get_io_info_lasttime_0_0
  !
  !! hole die Komponente "lasttime" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_lasttime_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_io_info) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "lasttime"
    INTEGER :: val(SIZE(this))  ! 
    !
    val = this%lasttime
    !
  END FUNCTION get_io_info_lasttime_1_0
  !
  !! hole die dynamische Feld-Komponente "tsec" aus einem skalaren Datenobjekt <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben              <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_tsec_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_io_info) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "tsec" (Zeiger)
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    val => this%tsec
    !
  END FUNCTION get_io_info_tsec_ref_0_1
  !
!!$  !
!!$  ! --- Version(en) fuer skalare Komponente "<ComponentName10>" [ggf. entfernen]
!!$  !
!!$  !! hole die Komponente "<ComponentName10>" aus einem skalaren Datenobjekt <BR>
!!$  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
!!$  FUNCTION get_io_info_<ComponentName10>_0_0 &
!!$       ( this ) &
!!$       RESULT( val ) 
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt (Skalar)
!!$    TYPE (t_io_info) , INTENT(IN)  :: this ! 
!!$    !! R&uuml;ckgabewert "<ComponentName10>" (Skalar)
!!$    <DataType10> :: val  ! 
!!$    !
!!$    val = this%<ComponentName10>
!!$    !
!!$  END FUNCTION get_io_info_<ComponentName10>_0_0
!!$  !
!!$  !! hole die Komponente "<ComponentName10>" aus einem vektoriellen Datenobjekt <BR>
!!$  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
!!$  FUNCTION get_io_info_<ComponentName10>_1_0 &
!!$       ( this ) &
!!$       RESULT( val ) 
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt (Vektor)
!!$    TYPE (t_io_info) , INTENT(IN)  :: this(:) ! 
!!$    !! R&uuml;ckgabewert "<ComponentName10>"
!!$    <DataType10> :: val(SIZE(this))  ! 
!!$    !
!!$    val = this%<ComponentName10>
!!$    !
!!$  END FUNCTION get_io_info_<ComponentName10>_1_0
!!$  !
!!$  ! --- Version fuer dynamische Feldkomponente "<ComponentName10>" [ggf. entfernen]
!!$  !
!!$  !! hole die dynamische Feld-Komponente "<ComponentName10>" aus einem skalaren Datenobjekt <BR>
!!$  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
!!$  FUNCTION get_io_info_<ComponentName10>_0_1 &
!!$       ( this ) &
!!$       RESULT( val ) 
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt (Skalar)
!!$    TYPE (t_io_info) , INTENT(IN)  :: this     ! 
!!$    !! R&uuml;ckgabewert "<ComponentName10>" (Vektor)
!!$    <DataType10> :: val(SIZE(this%<ComponentName10>)) ! 
!!$    !
!!$    val = this%<ComponentName10>
!!$    !
!!$  END FUNCTION get_io_info_<ComponentName10>_0_1
  !
  !! Ermittle die Anzahl der Datenobjekte in "this" deren Komponente "pac" den
  !! Wert "var" (Skalar) aufweist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_pac_count_0 ( this, var ) &
       RESULT( res )
    !! Datenobjekte
    TYPE (t_io_info)  , INTENT(IN) :: this(:) ! 
    !! Vergleichswert f&uuml;r die Komponente "pac"
    CHARACTER (LEN=*) , INTENT(IN) :: var     ! 
    !! Ergebnis: Anzahl der Datenobjekte mit Komponente "pac" == "var"
    INTEGER :: res ! 
    !
    res = COUNT( get_log_pac( this, var ) )
    !
  END FUNCTION get_io_info_pac_count_0
  !
  !! Ermittle die Anzahl der Datenobjekte in "this" deren Komponente "pac" den
  !! Wert "var" (Vektor) aufweist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_pac_count_1 ( this, var ) &
       RESULT( res )
    !! Datenobjekte
    TYPE (t_io_info)  , INTENT(IN) :: this(:) ! 
    !! Vergleichswerte f&uuml;r die Komponente "pac"
    CHARACTER (LEN=*) , INTENT(IN) :: var(:)  ! 
    !! Ergebnis: Anzahl der Datenobjekte mit Komponente "pac" == "var"
    !!           f&uuml;r alle Vergleichswerte "var(:)" 
    INTEGER :: res(SIZE(var)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_io_info_pac_count_0 ( this, var(i) )
    END DO
    !
  END FUNCTION get_io_info_pac_count_1
  !
  !! Ermittle die Indexposition des n-ten Datenobjekts deren Komponente "pac" den
  !! Wert "var" (Skalar) aufweist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_pac_idx_0 ( this, var, n ) &
       RESULT( res )
    !! Datenobjekte
    TYPE (t_io_info)  , INTENT(IN) :: this(:) ! 
    !! Vergleichswert f&uuml;r die Komponente "pac"
    CHARACTER (LEN=*) , INTENT(IN) :: var     ! 
    !! Nummer der n-ten gesuchten Position
    INTEGER           , INTENT(IN) :: n       ! 
    !! Ergebnis: Indexposition des n-ten gesuchten Datenobjekts, falls
    !! kein Objekt gefunden wird, wird c_undef_in zur&uuml;ckgegeben
    INTEGER :: res ! 
    ! Hilfsvariablen
    LOGICAL , ALLOCATABLE :: l_tf(:)          ! 
    !
    ALLOCATE(l_tf(SIZE(this)))
    !
    l_tf = get_log_pac ( this, var )
    res  = get_log_arr_idx ( l_tf, n )
    !
    DEALLOCATE(l_tf)
    !
  END FUNCTION get_io_info_pac_idx_0
  !
  !! Ermittle die Anzahl der Code-Kennungen in einem skalaren Datenobjekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_code_count_0 ( this ) &
       RESULT( res )
    !! (skalares) Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der Code-Bezeichnungen in "this"
    INTEGER :: res  ! 
    !
    IF ( ASSOCIATED( this%code ) ) THEN
       res = SIZE(this%code)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_io_info_code_count_0
  !
  !! Ermittle den Variablenname "var_name" f&uuml;r die n-te Code-Bezeichnung <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_var_name_0_n ( this, n ) &
       RESULT(res)
    !! (skalares) Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! n-tes Element in "this%code"
    INTEGER          , INTENT(IN) :: n    ! 
    !! Ergebnis: Variablenname "var_name"
    CHARACTER (LEN=c_len_var_name) :: res  ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_ch
    idx = get_code_var_position ( this, n ) 
    IF ( idx /= c_undef_in ) res = get_var_name( this%var(idx) )
    !
  END FUNCTION get_io_info_var_name_0_n
  !
  !! Ermittle das Feld "start(:)" f&uuml;r die n-te Code-Bezeichnung und eine bestimmten Termin <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_start_0_n_t ( this, n, time ) &
       RESULT(res)
    !! (skalares) Datenobjekt
    TYPE (t_io_info)  , INTENT(IN) :: this ! 
    !! n-tes Element in "this%code"
    INTEGER           , INTENT(IN) :: n    ! 
    !! aktuelles Datum (es wird der n&auml;chstgelegene Termin verwendet)
    TYPE (t_datetime) , INTENT(IN) :: time ! 
    !! Ergebnis: Feld mit Startindizes
    INTEGER , POINTER :: res(:)  ! 
    ! Hilfsvariablen
    INTEGER :: idx, jdx ! 
    !
    NULLIFY(res)
    idx = get_code_var_position ( this, n ) 
    IF ( idx /= c_undef_in ) THEN
       IF ( is_unlimited_var( this%var(idx), this%dim ) ) THEN
          IF ( valid_unlimited_dim_name( this%dim, c_dim_name(1) ) ) THEN ! Zeit
             jdx = get_time_position ( this, time )
          ELSE
             jdx = c_undef_in
          END IF
          IF ( jdx /= c_undef_in ) res => get_var_start( this%var(idx), jdx, this%dim )
       ELSE
          res => get_var_start( this%var(idx) )
       END IF
    END IF
    !
  END FUNCTION get_io_info_start_0_n_t
  !
  !! Ermittle das Feld "shape(:)" f&uuml;r die n-te Code-Bezeichnung <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_shape_0_n ( this, n ) &
       RESULT(res)
    !! (skalares) Datenobjekt
    TYPE (t_io_info)  , INTENT(IN) :: this ! 
    !! n-tes Element in "this%code"
    INTEGER           , INTENT(IN) :: n    ! 
    !! Ergebnis: Feld mit Shape der physikalischen Gr&ouml;&szlig;e
    INTEGER , POINTER :: res(:)  ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    NULLIFY(res)
    idx = get_code_var_position ( this, n ) 
    IF ( idx /= c_undef_in ) THEN
       res => get_var_shape( this%var(idx), this%dim )
    END IF
    !
  END FUNCTION get_io_info_shape_0_n
  !
  !! Ermittle die minimale Wasserbedeckung f&uuml;r ein skalares Datenobjekt <BR>
  !! falls dieser Wert nicht vorhanden ist, wird 0.0 zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_min_water_depth_0 ( this ) &
       RESULT(res)
    !! (skalares) Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Ergebnis: minimale Wasserbedeckung
    REAL (KIND=Double) :: res ! 
    !
    IF ( has_io_info_min_water_depth(this) ) THEN
       res = get_att_minimum_water_depth(this%att)
    ELSE
       res = 0.0_Double
    END IF
    !
  END FUNCTION get_io_info_min_water_depth_0
  !
  !! Ermittle die minimale Wasserbedeckungen f&uuml;r ein vektorielles Datenobjekt <BR>
  !! falls diese Wertw nicht vorhanden sind, wird jeweils 0.0 zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_io_info_min_water_depth_1 ( this ) &
       RESULT(res)
    !! (vektorielles) Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: minimale Wasserbedeckungen
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       res(i) = get_io_info_min_water_depth(this(i))
    END DO
    !
  END FUNCTION get_io_info_min_water_depth_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_io_info_0_0 ( this1, this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_io_info) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_io_info) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok( 1) = eq_file( this1%file, this2%file )
    l_ok( 2) = ( this1%key  == this2%key  )
    l_ok( 3) = ( this1%id   == this2%id   )
    l_ok( 4) = ( this1%pac  == this2%pac  )
    ! --- code ---
    IF      ( ASSOCIATED( this1%code ) .AND. ASSOCIATED( this2%code ) ) THEN
       IF ( SIZE(this1%code) == SIZE(this2%code) ) THEN
          l_ok( 5) = ALL( this1%code  == this2%code )
       ELSE
          l_ok( 5) = .false.
       END IF
    ELSE IF ( .NOT. ASSOCIATED( this1%code ) .AND. .NOT. ASSOCIATED( this2%code ) ) THEN
       l_ok( 5) = .true.
    ELSE
       l_ok( 5) = .false. 
    END IF
    ! --- dim  ---
    IF      ( ASSOCIATED( this1%dim ) .AND. ASSOCIATED( this2%dim ) ) THEN
       IF ( SIZE(this1%dim) == SIZE(this2%dim) ) THEN
          l_ok( 6) = ALL( this1%dim == this2%dim )
       ELSE
          l_ok( 6) = .false. 
       END IF
    ELSE IF ( .NOT. ASSOCIATED( this1%dim ) .AND. .NOT. ASSOCIATED( this2%dim ) ) THEN
       l_ok( 6) = .true.
    ELSE
       l_ok( 6) = .false. 
    END IF
    ! --- var  ---
    IF      ( ASSOCIATED( this1%var ) .AND. ASSOCIATED( this2%var ) ) THEN
       IF ( SIZE(this1%var) == SIZE(this2%var) ) THEN
          l_ok( 7) = ALL( this1%var == this2%var )
       ELSE
          l_ok( 7) = .false. 
       END IF
    ELSE IF ( .NOT. ASSOCIATED( this1%var ) .AND. .NOT. ASSOCIATED( this2%var ) ) THEN
       l_ok( 7) = .true.
    ELSE
       l_ok( 7) = .false. 
    END IF
    ! --- att  ---
    IF      ( ASSOCIATED( this1%att ) .AND. ASSOCIATED( this2%att ) ) THEN
       IF ( SIZE(this1%att) == SIZE(this2%att) ) THEN
          l_ok( 8) = ALL( this1%att == this2%att )
       ELSE
          l_ok( 8) = .false. 
       END IF
    ELSE IF ( .NOT. ASSOCIATED( this1%att ) .AND. .NOT. ASSOCIATED( this2%att ) ) THEN
       l_ok( 8) = .true.
    ELSE
       l_ok( 8) = .false. 
    END IF
    ! --- time ---
    IF      ( ASSOCIATED( this1%time ) .AND. ASSOCIATED( this2%time ) ) THEN
       IF ( SIZE(this1%time) == SIZE(this2%time) ) THEN
          l_ok( 9) = ALL( this1%time == this2%time )
       ELSE
          l_ok( 9) = .false. 
       END IF
    ELSE IF ( .NOT. ASSOCIATED( this1%time ) .AND. .NOT. ASSOCIATED( this2%time ) ) THEN
       l_ok( 9) = .true.
    ELSE
       l_ok( 9) = .false. 
    END IF
    ! --- lasttime ---
    l_ok(10)  = ( this1%lasttime  == this2%lasttime )
    ! --- tsec ---
    IF      ( ASSOCIATED( this1%tsec ) .AND. ASSOCIATED( this2%tsec ) ) THEN
       IF ( SIZE(this1%tsec) == SIZE(this2%tsec) ) THEN
          l_ok(11) = ALL( this1%tsec == this2%tsec )
       ELSE
          l_ok(11) = .false. 
       END IF
    ELSE IF ( .NOT. ASSOCIATED( this1%tsec ) .AND. .NOT. ASSOCIATED( this2%tsec ) ) THEN
       l_ok(11) = .true.
    ELSE
       l_ok(11) = .false. 
    END IF
!!$    ! l_ok(12) = ( this1%<ComponentName10> == this2%<ComponentName10> )
!!$    !
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_io_info_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_io_info_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_io_info) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_io_info) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    ! 
    ! Lokale Variablen
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_io_info_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_io_info_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_io_info_0_1 ( this1, this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_io_info) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_io_info) , INTENT(IN) :: this2(:) ! 
    !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_io_info_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_io_info_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_io_info_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_io_info) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_io_info) , INTENT(IN) :: this2(:) ! 
    !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_io_info_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_io_info_1_1
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
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CHECK-Methoden <<< [ERR_NO = 20000 bis 20999 ]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe die Verf&uuml;gbarkeit der physikalischen Gr&ouml;&szlig;en 
  !! in den zur Verf&uuml;gung gestellten Dateien (skalares Datenobjekt) <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE check_io_info_code_avail_0 ( this )
    !! (skalares) Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this !
    !! Name der Programmeinheit
    CHARACTER (LEN=26) , PARAMETER :: c_upname='check_io_info_code_avail_0' ! 
    ! Hilfsvariablen
    CHARACTER (LEN=10) :: l_char ! 
    INTEGER            :: i ! 
    ! 
    IF ( ASSOCIATED(this%att) .AND. ASSOCIATED(this%code) ) THEN
       DO i=1,SIZE(this%code) 
          IF ( .NOT. has_att_name_id( this%att, this%code(i) ) ) THEN
             CALL setup_error_act ( all_errors, 20000, c_upname, c_modname )
             CALL setup_error_act ( '<key>', TRIM(this%key) )
             CALL setup_error_act ( '<datafile>', TRIM(get_file_name(this%file)) )
             WRITE(l_char,'(I10)') this%code(i) ; CALL setup_error_act ( '<code>', l_char )
             CALL setup_error_act ( '<name>', TRIM(get_phy_quant_descr(this%code(i))) )
          END IF
       END DO
    END IF
    !
  END SUBROUTINE check_io_info_code_avail_0
  !
  !! Pr&uuml;fe die Verf&uuml;gbarkeit der physikalischen Gr&ouml;&szlig;en 
  !! in den zur Verf&uuml;gung gestellten Dateien (vektorielles Datenobjekt) <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE check_io_info_code_avail_1 ( this )
    !! (vektorielles) Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this(:) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL check_io_info_code_avail_0 ( this(i) )
    END DO
    !
  END SUBROUTINE check_io_info_code_avail_1
  !
  !! Pr&uuml;fe die Verf&uuml;gbarkeit eines Termins innerhalb des in den
  !! Dateien zur Verf&uuml;gung gestellten Zeitraums (skalares Datenobjekt) <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE check_io_info_time_avail_0 ( this, var )
    !! (skalares) Datenobjekt
    TYPE (t_io_info)  , INTENT(IN) :: this !
    !! Zeitangabe
    TYPE (t_datetime) , INTENT(IN) :: var  ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=26) , PARAMETER :: c_upname='check_io_info_time_avail_0' ! 
    !! Indexpositionen der zeitbeschreibenden Attribute
    INTEGER , PARAMETER :: c_max_id=4                 ! 
    !! Liste der Attribut-Typen <BR>
    !! 3 = beginning_date       <BR>
    !! 4 = ending_date          <BR>
    !! 6 = single_date          <BR>
    !! 7 = multiple_dates
    INTEGER , PARAMETER :: c_id(c_max_id) = (/3,4,6,7/) ! 
    ! Hilfsvariablen
    TYPE (t_datetime) , ALLOCATABLE :: time(:) ! 
    TYPE (t_datetime) :: a_time(1), e_time(1)  ! 
    INTEGER :: n, idx(c_max_id)                ! 
    !
    IF ( ASSOCIATED(this%att) ) THEN
       IF ( .NOT. is_datetime_in_att_period(this%att, var) ) THEN
          CALL new_datetime( a_time ) ; CALL new_datetime( e_time )
          idx = get_att_idx( this%att, c_att_name(c_id) )
          IF ( ALL(idx(1:2) > 0 ) ) THEN
             IF ( ALL( is_att_ch( this%att(idx(1:2)) ) ) ) THEN
                a_time = get_att_ch_as_datetime(this%att(idx(1)))
                e_time = get_att_ch_as_datetime(this%att(idx(2)))
             END IF
          ELSE IF ( idx(4) > 0 ) THEN
             IF ( is_att_ch( this%att(idx(4)) ) ) THEN
                ALLOCATE( time(get_att_nof_values(this%att(idx(4)))) )
                n      = SIZE(time)
                time   = get_att_ch_as_datetime( this%att(idx(4)) )
                a_time = time(1)
                e_time = time(n)
                DEALLOCATE( time )
             END IF
          ELSE IF ( idx(3) > 0 ) THEN
             IF ( is_att_ch( this%att(idx(3)) ) ) THEN
                a_time = get_att_ch_as_datetime(this%att(idx(3)))
                e_time = a_time
             END IF
          END IF
          CALL setup_error_act ( all_errors, 20010, c_upname, c_modname )
          CALL setup_error_act ( '<key>', TRIM(this%key) )
          CALL setup_error_act ( '<datafile>', TRIM(get_file_name(this%file)) )
          CALL setup_error_act ( '<datetime>', datetime_to_string(var) )
          CALL setup_error_act ( '<start>', datetime_to_string(a_time(1)) )
          CALL setup_error_act ( '<end>', datetime_to_string(e_time(1)) )
          CALL kill_datetime( a_time ) ; CALL kill_datetime( e_time )
       END IF
    END IF
    !
  END SUBROUTINE check_io_info_time_avail_0
  !
  !! Pr&uuml;fe die Verf&uuml;gbarkeit eines Termins innerhalb des in den
  !! Dateien zur Verf&uuml;gung gestellten Zeitraums (vektorielles Datenobjekt) <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE check_io_info_time_avail_1 ( this, var )
    !! (vektorielles) Datenobjekt
    TYPE (t_io_info)  , INTENT(IN) :: this(:) ! 
    !! Zeitangabe
    TYPE (t_datetime) , INTENT(IN) :: var  ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL check_io_info_time_avail_0 ( this(i), var )
    END DO
    !
  END SUBROUTINE check_io_info_time_avail_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-HAS-Methoden <<< [ERR_NO = 21000 bis 21999 ]
  ! ----------------------------------------------------------------------
  !
  !! Ermittle, ob es sich bei den in dem IO_INFO-Objekt abgelegten Daten
  !! um zwei-dimensionale Daten handelt (skalares Datenobjekt) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_io_info_2d_0 ( this ) &
       RESULT( res )
    !! (skalares) Datenobjekt
    TYPE (t_io_info) :: this  ! 
    !! Ergebnis: Pr&uuml;fergebnis
    LOGICAL :: res ! 
    ! Hilfsvariablen
    INTEGER :: n   ! 
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       n   = get_att_interfaces_count( this%att )
       res = ( n <= 2 )
    ELSE
       res = .false. 
    END IF
    !
  END FUNCTION has_io_info_2d_0
  !
  !! Ermittle, ob es sich bei den in dem IO_INFO-Objekt abgelegten Daten
  !! um zwei-dimensionale Daten handelt (vektorielles Datenobjekt) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_io_info_2d_1 ( this ) &
       RESULT( res )
    !! (vektorielles) Datenobjekt
    TYPE (t_io_info) :: this(:)  ! 
    !! Ergebnis: Pr&uuml;fergebnisse
    LOGICAL :: res(SIZE(this)) ! 
    ! Hilfsvariablen
    INTEGER :: i   ! 
    !
    res = .false.
    DO i=1,SIZE(this)
       res(i) = has_io_info_2d_0 ( this(i) )
    END DO
    !
  END FUNCTION has_io_info_2d_1
  !
  !! Ermittle, ob es sich bei den in dem IO_INFO-Objekt abgelegten Daten
  !! um drei-dimensionale Daten handelt (skalares Datenobjekt) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_io_info_3d_0 ( this ) &
       RESULT( res )
    !! (skalares) Datenobjekt
    TYPE (t_io_info) :: this  ! 
    !! Ergebnis: Pr&uuml;fergebnis
    LOGICAL :: res ! 
    ! Hilfsvariablen
    INTEGER :: n   ! 
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       n   = get_att_interfaces_count( this%att )
       res = ( n > 2 )
    ELSE
       res = .false. 
    END IF
    !
  END FUNCTION has_io_info_3d_0
  !
  !! Ermittle, ob es sich bei den in dem IO_INFO-Objekt abgelegten Daten
  !! um drei-dimensionale Daten handelt (vektorielles Datenobjekt) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_io_info_3d_1 ( this ) &
       RESULT( res )
    !! (vektorielles) Datenobjekt
    TYPE (t_io_info) :: this(:)  ! 
    !! Ergebnis: Pr&uuml;fergebnisse
    LOGICAL :: res(SIZE(this)) ! 
    ! Hilfsvariablen
    INTEGER :: i   ! 
    !
    res = .false.
    DO i=1,SIZE(this)
       res(i) = has_io_info_3d_0 ( this(i) )
    END DO
    !
  END FUNCTION has_io_info_3d_1
  !
  !! Ermittle, ob in dem IO-INFO-Objekt Informationen zu einer minimalen
  !! Wasserbedeckung vorhanden sind oder nicht (skalares Datenobjekt) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_io_info_min_water_depth_0 ( this ) &
       RESULT( res )
    !! (skalares) Datenobjekt
    TYPE (t_io_info) :: this  ! 
    !! Ergebnis: Pr&uuml;fergebnis
    LOGICAL :: res ! 
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       res = has_att_minimum_water_depth( this%att )
    ELSE
       res = .false. 
    END IF
    !
  END FUNCTION has_io_info_min_water_depth_0
  !
  !! Ermittle, ob in dem IO-INFO-Objekt Informationen zu einer minimalen
  !! Wasserbedeckung vorhanden sind oder nicht (vektorielles Datenobjekt) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_io_info_min_water_depth_1 ( this ) &
       RESULT( res )
    !! (vektorielles) Datenobjekt
    TYPE (t_io_info) :: this(:)  ! 
    !! Ergebnis: Pr&uuml;fergebnisse
    LOGICAL :: res(SIZE(this)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    res = .false.
    DO i=1,SIZE(this)
       res(i) = has_io_info_min_water_depth_0 ( this(i) )
    END DO
    !
  END FUNCTION has_io_info_min_water_depth_1
  !
  !! Ermittle, ob f&uuml;r eine bestimmte Code-Bezeichnung die <EM>unlimited</EM>-Dimension
  !! einen vorgegebenen Namen aufweist <BR>
  !! a) skalares Datenobjekt, Code-Positionsnummer, Name der Unlimited-Dimension <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_io_info_code_unlimited_0_n ( this, n, name ) &
       RESULT(res)
    !! skalares Datenobjekt 
    TYPE (t_io_info)  , INTENT(IN) :: this ! 
    !! Positionsindex in his%code(:)
    INTEGER           , INTENT(IN) :: n    ! 
    !! Name der <EM>unlimited</EM> Dimension
    CHARACTER (LEN=*) , INTENT(IN) :: name ! 
    !! Ergebnis: Testergebnis
    LOGICAL :: res ! 
    ! Hilfsvariablen
    INTEGER :: idx ! 
    !
    res = .false.
    idx = get_code_var_position ( this, n )
    IF ( idx /= c_undef_in ) THEN
       IF ( is_unlimited_var( this%var(idx), this%dim ) ) THEN
          IF ( valid_unlimited_dim_name( this%dim, name ) ) res = .true. 
       END IF
    END IF
    !
  END FUNCTION has_io_info_code_unlimited_0_n
  !
  !! Ermittle, ob die in einer Datei liegenden Daten f&uuml;r <EM>dynamische Bathymetrie</EM>
  !! erzeugt wurden oder nicht <BR>
  !! a) skalares Datenobjekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_io_info_dyn_bathymetry_0 ( this ) &
       RESULT(res)
    !! skalares Datenobjekt 
    TYPE (t_io_info)  , INTENT(IN) :: this ! 
    !! Ergebnis: Testergebnis
    LOGICAL :: res ! 
    !
    IF ( ASSOCIATED(this%att) ) THEN
       res = has_att_dynamic_bathymetry( this%att )
    ELSE
       res = .false.
    END IF
    !
  END FUNCTION has_io_info_dyn_bathymetry_0
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
       WRITE(*,*) ' *** Warnung *** Modul "b_io_info" nicht initialisiert'
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_io_info ausfuehren'
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
  SUBROUTINE init_io_info_all_errors ( )
    ! Lokale Parameter / Variablen
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
               '--> INIT_io_info ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_io_info ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "code"\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "time"\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "tsec"\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
!!$       ic = ic + 1
!!$       IF ( i == 2 ) THEN
!!$          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5120 )
!!$          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
!!$               'Fehlerkategorie: KILL-Methoden\n'//&
!!$               'De-Allocate-Fehler fuer Komponente von "t_io_info"\n'//&
!!$               'Typ-Komponente = "<ComponentName10>"\n'//&
!!$               '--> Code in Modul "b_io_info" pruefen' )
!!$       END IF
!!$       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "file"\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "key"\n'//&
               'aktuell        = <key>\n'//&
               'undefiniert    = <undef>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "id"\n'//&
               'aktuell        = <id>\n'//&
               'undefiniert    = <undef>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "pac"\n'//&
               'aktuell        = <pac>\n'//&
               'undefiniert    = <undef>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "code"\n'//&
               'Code  = <code>\n'//&
               'OK    = <ok>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "dim(:)"\n'//&
               'assoziiert = <ass>\n'//&
               'korrekt    = <true>\n'//&
               'fehlerhaft = <false>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "var(:)"\n'//&
               'assoziiert = <ass>\n'//&
               'korrekt    = <true>\n'//&
               'fehlerhaft = <false>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "att(:)"\n'//&
               'assoziiert = <ass>\n'//&
               'korrekt    = <true>\n'//&
               'fehlerhaft = <false>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "time(:)"\n'//&
               'assoziiert = <ass>\n'//&
               'korrekt    = <true>\n'//&
               'fehlerhaft = <false>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente  = "lasttime"\n'//&
               'aktuell         = <act>\n'//&
               'minimal erlaubt = <min>\n'//&
               'maximal erlaubt = <max>\n'//&
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "tsec"\n'//&
               'Komponente wurde nicht allokiert\n'//& 
               '... fuer folgende Komponente\n'//&
               'Pfad  = <path>\n'//&
               'Datei = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
!!$       ic = ic + 1
!!$       IF ( i == 2 ) THEN
!!$          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6120 )
!!$          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
!!$               'Fehlerkategorie: OK-Methoden\n'//&
!!$               'Fehler in Komponente von "t_io_info"\n'//&
!!$               'Typ-Komponente = "<ComponentName10>"\n'//&
!!$               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
!!$       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "file"\n'//&
               '... bei der Ausgabe von = "<line>"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "key"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "pac"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "code"\n'//&
               '... bei der Ausgabe von = "<line>"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "dim(:)"\n'//&
               '... bei der Ausgabe von = "<line>"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "var(:)"\n'//&
               '... bei der Ausgabe von = "<line>"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "att(:)"\n'//&
               '... bei der Ausgabe von = "<line>"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "time(:)"\n'//&
               '... bei der Ausgabe von = "<line>"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "lasttime"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
               'Typ-Komponente = "tsec"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
!!$       ic = ic + 1
!!$       IF ( i == 2 ) THEN
!!$          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7120 )
!!$          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
!!$               'Fehlerkategorie: PRINT-Methoden\n'//&
!!$               'Fehler beim Drucken von Objekt "t_io_info"\n'//&
!!$               'Typ-Komponente = "<ComponentName10>"\n'//&
!!$               '--> Code in Modul "b_io_info" / Daten pruefen' )
!!$       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_io_info"\n'//&
               '--> Code in Modul "b_io_info" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "code"\n'//&
               'Dimension 1    = <idim>\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "time"\n'//&
               'Dimension 1    = <idim>\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_io_info"\n'//&
               'Typ-Komponente = "tsec"\n'//&
               '--> Code in Modul "b_io_info" pruefen' )
       END IF
!!$       ic = ic + 1
!!$       IF ( i == 2 ) THEN
!!$          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8120 )
!!$          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
!!$               'Fehlerkategorie: NEW-Methoden\n'//&
!!$               'Allocate-Fehler fuer Komponente von "t_io_info"\n'//&
!!$               'Typ-Komponente = "<ComponentName10>"\n'//&
!!$               '--> Code in Modul "b_io_info" pruefen' )
!!$       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 20000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CHECK-Methoden\n'//&
               'erforderliche Daten sind in Datei nicht vorhanden\n'//&
               'Key   = <key>\n'//&
               'Datei = <datafile>\n'//&
               'Code  = <code>\n'//&
               'Name  = <name>\n'//&
               '--> Anforderungen / bereitgestellte Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 20010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CHECK-Methoden\n'//&
               'gewuenschter Termin ist in Datei nicht enthalten\n'//&
               'Key    = <key>\n'//&
               'Datei  = <datafile>\n'//&
               'Termin = <datetime>\n'//&
               'Start  = <start>\n'//&
               'Ende   = <end>\n'//&
               '--> Anforderungen / bereitgestellte Daten pruefen' )
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
  END SUBROUTINE init_io_info_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_io_info_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_io_info_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "code" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_io_info_code ( this, idim )
    !! Datenobjekt
    TYPE (t_io_info)   , INTENT(OUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "code"
    INTEGER            , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=18) , PARAMETER   :: c_upname='alloc_io_info_code' !
    ! Hilfsvariablen
    CHARACTER (LEN=10) :: l_char ! 
    INTEGER            :: stat   ! 
    !
    ALLOCATE ( this%code(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8050, c_upname, c_modname, stat )
       WRITE(l_char,'(I10)') idim ; CALL setup_error_act( '<idim>', l_char )
    END IF
    !
  END SUBROUTINE alloc_io_info_code
  !
  !! Allokieren der dynamischen Feld-Komponente "time" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_io_info_time ( this, idim )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(OUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "time"
    INTEGER          , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_io_info_time' !
    !! Statusvariable
    CHARACTER (LEN=10) :: l_char ! 
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%time(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8090, c_upname, c_modname, stat )
       WRITE(l_char,'(I10)') idim ; CALL setup_error_act( '<idim>', l_char )
    END IF
    !
  END SUBROUTINE alloc_io_info_time
  !
  !! Allokieren der dynamischen Feld-Komponente "tsec" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_io_info_tsec ( this, idim )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(OUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "tsec"
    INTEGER          , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_io_info_tsec' !
    !! Statusvariable
    CHARACTER (LEN=10) :: l_char ! 
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%tsec(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8110, c_upname, c_modname, stat )
       WRITE(l_char,'(I10)') idim ; CALL setup_error_act( '<idim>', l_char )
    END IF
    !
  END SUBROUTINE alloc_io_info_tsec
!!$  !
!!$  !! Allokieren der dynamischen Feld-Komponente "<ComponentName10>" <BR>
!!$  !! Subroutine erzeugt Fehlermeldungen
!!$  SUBROUTINE alloc_io_info_<ComponentName10> &
!!$       ( this, &
!!$         idim )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt
!!$    TYPE (t_io_info) , INTENT(OUT) :: this   ! 
!!$    !! Dimension der dynamischen Feld-Komponente "<ComponentName10>"
!!$    INTEGER               , INTENT(IN)  :: idim   ! 
!!$    !
!!$    ! Lokale Parameter / Variablen
!!$    !! Name der Subroutine
!!$    CHARACTER (LEN=31), PARAMETER  :: c_upname='alloc_io_info_<ComponentName10>' !
!!$    !! Statusvariable
!!$    INTEGER :: stat ! 
!!$    !
!!$    ALLOCATE ( this%<ComponentName10>(idim), STAT=stat )
!!$    !
!!$    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 8100, c_upname, c_modname, stat )
!!$    !
!!$  END SUBROUTINE alloc_io_info_<ComponentName10>
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte INIT-Routinen bitte unbedingt loeschen <---------
  ! ----------------------------------------------------------------------
  !
!!$  !
!!$  !! Initialisieren der Feld-Komponente "<ComponentName10>" mit Default-Werten <BR>
!!$  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
!!$  SUBROUTINE init_io_info_<ComponentName10> &
!!$       ( this )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt
!!$    TYPE (t_io_info) , INTENT(INOUT) :: this   ! 
!!$    !
!!$    ! Lokale Parameter / Variablen
!!$    !! Initialisierungswert <ComponentName10>
!!$    <DataType10> , PARAMETER :: c_var=<Initialisierungswert10> 
!!$    !
!!$    this%<ComponentName10>(:) = c_var
!!$    !
!!$  END SUBROUTINE init_io_info_<ComponentName10>
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "code" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_io_info_code ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_io_info_code' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%code ) ) THEN
       DEALLOCATE ( this%code, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5050, c_upname, c_modname, stat )
       NULLIFY ( this%code ) 
    END IF
    !
  END SUBROUTINE dealloc_io_info_code
  !
  !! De-Allokieren der dynamischen Feld-Komponente "time" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_io_info_time ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_io_info_time' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%time ) ) THEN
       DEALLOCATE ( this%time, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5090, c_upname, c_modname, stat )
       NULLIFY ( this%time ) 
    END IF
    !
  END SUBROUTINE dealloc_io_info_time
  !
  !! De-Allokieren der dynamischen Feld-Komponente "tsec" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_io_info_tsec ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_io_info_tsec' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%tsec ) ) THEN
       DEALLOCATE ( this%tsec, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5110, c_upname, c_modname, stat )
       NULLIFY ( this%tsec ) 
    END IF
    !
  END SUBROUTINE dealloc_io_info_tsec
!!$  !
!!$  !! De-Allokieren der dynamischen Feld-Komponente "<ComponentName10>" <BR>
!!$  !! Subroutine erzeugt Fehlermeldungen
!!$  SUBROUTINE dealloc_io_info_<ComponentName10> &
!!$       ( this )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt
!!$    TYPE (t_io_info) , INTENT(INOUT) :: this ! 
!!$    !
!!$    ! Lokale Parameter / Variablen
!!$    !! Name der Subroutine
!!$    CHARACTER (LEN=31), PARAMETER  :: c_upname='dealloc_io_info_<ComponentName10>' !
!!$    !! Statusvariable
!!$    INTEGER :: stat ! 
!!$    !
!!$    IF ( ASSOCIATED( this%<ComponentName10> ) ) THEN
!!$       !
!!$       DEALLOCATE ( this%<ComponentName10>, STAT=stat )
!!$       !
!!$       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5100, c_upname, c_modname, stat )
!!$       !
!!$       NULLIFY ( this%<ComponentName10> ) 
!!$       !
!!$    END IF
!!$    !
!!$  END SUBROUTINE dealloc_io_info_<ComponentName10>
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "file" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_file ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    ! lokale Parameter / Variablen
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_io_info_file' ! 
    !
    ok = ok_file( this%file )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<path>', get_file_path( this%file) )
       CALL setup_error_act ( '<name>', get_file_name( this%file) )
    END IF
    !
  END FUNCTION ok_io_info_file
  !
  !! Pr&uuml;fe, ob die Komponente "key" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_key ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    ! lokale Parameter / Variablen
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_io_info_key' ! 
    INTEGER :: n ! 
    !
    n  = LEN_TRIM(c_undef_ch)
    ok = ( LEN_TRIM( this%key ) > 0 .AND. this%key(1:n) .NE. c_undef_ch )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<key>', this%key )
       CALL setup_error_act ( '<undef>', c_undef_ch )
       CALL setup_error_act ( '<path>', get_file_path( this%file) )
       CALL setup_error_act ( '<name>', get_file_name( this%file) )
    END IF
    !
  END FUNCTION ok_io_info_key
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_io_info_id' ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    ok = ( this%id > 0 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(l_char,'(I10)') this%id    ; CALL setup_error_act ( '<id>', l_char )
       WRITE(l_char,'(I10)') c_undef_in ; CALL setup_error_act ( '<undef>', l_char )
       CALL setup_error_act ( '<path>', get_file_path( this%file) )
       CALL setup_error_act ( '<name>', get_file_name( this%file) )
    END IF
    !
  END FUNCTION ok_io_info_id
  !
  !! Pr&uuml;fe, ob die Komponente "pac" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_pac ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    ! lokale Parameter / Variablen
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_io_info_pac' ! 
    INTEGER :: n ! 
    !
    n  = LEN_TRIM(c_undef_ch)
    ok = ( LEN_TRIM( this%pac ) > 0 .AND. this%pac(1:n) .NE. c_undef_ch )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       CALL setup_error_act ( '<pac>', this%pac )
       CALL setup_error_act ( '<undef>', c_undef_ch )
       CALL setup_error_act ( '<path>', get_file_path( this%file) )
       CALL setup_error_act ( '<name>', get_file_name( this%file) )
    END IF
    !
  END FUNCTION ok_io_info_pac
  !
  !! Pr&uuml;fe, ob die Komponente "code" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_code ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_io_info_code' ! 
    ! Hilfsvariablen
    CHARACTER (LEN=70)    :: l_char  ! 
    LOGICAL , ALLOCATABLE :: l_ok(:) ! 
    !
    IF ( ASSOCIATED( this%code ) ) THEN
       ALLOCATE( l_ok(SIZE(this%code)) )
       l_ok = is_phy_quant_valid( this%code )
       ok   = ALL( l_ok ) 
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          l_char = REPEAT( ' ', LEN_TRIM(l_char) )
          WRITE(l_char,'(14I5)') this%code(1:MIN(SIZE(this%code),14))
          CALL setup_error_act ( '<code>', TRIM(l_char) )
          WRITE(l_char,'(14L5)') l_ok(1:MIN(SIZE(this%code),14))
          CALL setup_error_act ( '<ok>', TRIM(l_char) )
       END IF
       DEALLOCATE( l_ok )
    ELSE
       ok = .true. 
    END IF
    !
  END FUNCTION ok_io_info_code
  !
  !! Pr&uuml;fe, ob die Komponente "dim(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_dim ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_io_info_dim' ! 
    ! Hilfsvariablen
    CHARACTER (LEN=5)     :: l_char  ! 
    LOGICAL , ALLOCATABLE :: l_ok(:) ! 
    !
    ok = ASSOCIATED(this%dim)
    IF ( ok ) THEN
       ALLOCATE( l_ok(SIZE(this%dim)) )
       l_ok = ok_dim( this%dim )
       ok   = ALL( l_ok )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          CALL setup_error_act ( '<ass>', 'T' )
          WRITE(l_char,'(I5)') COUNT(       l_ok ) ; CALL setup_error_act ( '<true>', l_char )
          WRITE(l_char,'(I5)') COUNT( .NOT. l_ok ) ; CALL setup_error_act ( '<false>', l_char )
       END IF
       DEALLOCATE( l_ok )
    ELSE
       SELECT CASE ( TRIM(this%pac) )
       CASE ( 'h_grid', 'l_grid', 'p_grid' ) ! wird nicht benoetigt
          ok = .true. 
       CASE DEFAULT
          CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          CALL setup_error_act ( '<ass>', 'F' )
          CALL setup_error_act ( '<true>', 'undefined' )
          CALL setup_error_act ( '<false>', 'undefined' )
       END SELECT
    END IF
    !
  END FUNCTION ok_io_info_dim
  !
  !! Pr&uuml;fe, ob die Komponente "var(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_var ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_io_info_var' ! 
    ! Hilfsvariablen
    CHARACTER (LEN=5)     :: l_char  ! 
    LOGICAL , ALLOCATABLE :: l_ok(:) ! 
    !
    ok = ASSOCIATED(this%var)
    IF ( ok ) THEN
       ALLOCATE( l_ok(SIZE(this%var)) )
       l_ok = ok_var( this%var )
       ok   = ALL( l_ok )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          CALL setup_error_act ( '<ass>', 'T' )
          WRITE(l_char,'(I5)') COUNT(       l_ok ) ; CALL setup_error_act ( '<true>', l_char )
          WRITE(l_char,'(I5)') COUNT( .NOT. l_ok ) ; CALL setup_error_act ( '<false>', l_char )
       END IF
       DEALLOCATE( l_ok )
    ELSE
       SELECT CASE ( TRIM(this%pac) )
       CASE ( 'h_grid', 'l_grid', 'p_grid' ) ! wird nicht benoetigt
          ok = .true. 
       CASE DEFAULT
          CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          CALL setup_error_act ( '<ass>', 'F' )
          CALL setup_error_act ( '<true>', 'undefined' )
          CALL setup_error_act ( '<false>', 'undefined' )
       END SELECT
    END IF
    !
  END FUNCTION ok_io_info_var
  !
  !! Pr&uuml;fe, ob die Komponente "att(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_att ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_io_info_att' ! 
    ! Hilfsvariablen
    CHARACTER (LEN=5)     :: l_char  ! 
    LOGICAL , ALLOCATABLE :: l_ok(:) ! 
    !
    ok = ASSOCIATED(this%att)
    IF ( ok ) THEN
       ALLOCATE( l_ok(SIZE(this%att)) )
       l_ok = ok_att( this%att )
       ok   = ALL( l_ok )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6080, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          CALL setup_error_act ( '<ass>', 'T' )
          WRITE(l_char,'(I5)') COUNT(       l_ok ) ; CALL setup_error_act ( '<true>', l_char )
          WRITE(l_char,'(I5)') COUNT( .NOT. l_ok ) ; CALL setup_error_act ( '<false>', l_char )
       END IF
       DEALLOCATE( l_ok )
    ELSE
       SELECT CASE ( TRIM(this%pac) )
       CASE ( 'h_grid', 'l_grid', 'p_grid' ) ! wird nicht benoetigt
          ok = .true. 
       CASE DEFAULT
          CALL setup_error_act ( all_errors(:), 6080, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          CALL setup_error_act ( '<ass>', 'F' )
          CALL setup_error_act ( '<true>', 'undefined' )
          CALL setup_error_act ( '<false>', 'undefined' )
       END SELECT
    END IF
    !
  END FUNCTION ok_io_info_att
  !
  !! Pr&uuml;fe, ob die Komponente "time" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_time ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_io_info_time' ! 
    ! Hilfsvariablen
    CHARACTER (LEN=5)     :: l_char  ! 
    LOGICAL , ALLOCATABLE :: l_ok(:) ! 
    !
    ok = ASSOCIATED(this%time)
    IF ( ok ) THEN
       ALLOCATE( l_ok(SIZE(this%time)) )
       l_ok = ok_datetime( this%time )
       ok   = ALL( l_ok )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6090, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          CALL setup_error_act ( '<ass>', 'T' )
          WRITE(l_char,'(I5)') COUNT(       l_ok ) ; CALL setup_error_act ( '<true>', l_char )
          WRITE(l_char,'(I5)') COUNT( .NOT. l_ok ) ; CALL setup_error_act ( '<false>', l_char )
       END IF
       DEALLOCATE( l_ok )
    ELSE
       SELECT CASE ( TRIM(this%pac) )
       CASE ( 'h_grid', 'l_grid', 'p_grid' ) ! wird nicht benoetigt
          ok = .true. 
       CASE DEFAULT
          CALL setup_error_act ( all_errors(:), 6090, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          CALL setup_error_act ( '<ass>', 'F' )
          CALL setup_error_act ( '<true>', 'undefined' )
          CALL setup_error_act ( '<false>', 'undefined' )
       END SELECT
    END IF
    !
  END FUNCTION ok_io_info_time
  !
  !! Pr&uuml;fe, ob die Komponente "lasttime" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_lasttime ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_io_info_lasttime' ! 
    ! Hilfsvariablen
    CHARACTER (LEN=10) :: l_char ! 
    !
    ok = .true. 
    IF ( ASSOCIATED( this%time ) .AND. this%lasttime /= c_undef_in ) THEN
       IF ( this%lasttime < 1 .OR. this%lasttime > SIZE(this%time) ) THEN
          CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
          CALL setup_error_act ( '<path>', get_file_path( this%file) )
          CALL setup_error_act ( '<name>', get_file_name( this%file) )
          WRITE(l_char,'(I10)') this%lasttime   ; CALL setup_error_act ( '<act>', l_char )
          WRITE(l_char,'(I10)') 1               ; CALL setup_error_act ( '<min>', l_char )
          WRITE(l_char,'(I10)') SIZE(this%time) ; CALL setup_error_act ( '<max>', l_char )
          ok = .false. 
       END IF
    END IF
    !
  END FUNCTION ok_io_info_lasttime
  !
  !! Pr&uuml;fe, ob die Komponente "tsec" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_io_info_tsec ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_io_info_tsec' ! 
    !
    ok = .true.
    IF ( ASSOCIATED( this%time ) .AND. .NOT. ASSOCIATED( this%tsec ) ) THEN
       ok = .false. 
       CALL setup_error_act ( all_errors(:), 6110, c_upname, c_modname )
       CALL setup_error_act ( '<path>', get_file_path( this%file) )
       CALL setup_error_act ( '<name>', get_file_name( this%file) )
    END IF
    !
  END FUNCTION ok_io_info_tsec
!!$  !
!!$  !! Pr&uuml;fe, ob die Komponente "<ComponentName10>" eines Datenobjektes o.k. ist <BR>
!!$  !! Function erzeugt Fehlermeldungen
!!$  FUNCTION ok_io_info_<ComponentName10> &
!!$       ( this ) &
!!$       RESULT( ok )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt
!!$    TYPE (t_io_info) , INTENT(IN) :: this ! 
!!$    !
!!$    ! Rueckgabewert
!!$    !! Testergebnis
!!$    LOGICAL :: ok ! 
!!$    !
!!$    ! lokale Parameter / Variablen
!!$    !! Name der Funktion
!!$    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_io_info_<ComponentName10>' ! 
!!$    !
!!$    ok = <AllesInOrdnung(this%<ComponentName10>)
!!$    !
!!$    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
!!$    !
!!$  END FUNCTION ok_io_info_<ComponentName10>
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "file" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_file ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_io_info_file' ! 
    !! Statusvariable
    INTEGER :: stat !  
    !
    IF ( .NOT. prn_op ) RETURN
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
    IF ( stat == 0 ) THEN
       CALL print_file( this%file )
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8010, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname )
             CALL setup_error_act ( '<line>', 'Footer-Line')
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname )
          CALL setup_error_act ( '<line>', 'Data-Line')
       END IF
    ELSE
       CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<line>', 'Header-Line')
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente file  - - - - - - - - - - - - - - - - ')
8010 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_file
  !
  !! Drucke den Inhalt der Komponente "key" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_key ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_io_info_key' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%key)
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente key   - - - - - - - - - - - - - - - - ',/&
           '# wert = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_key
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_id ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    ! lokale Parameter / Variablen
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_io_info_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%id
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente id  - - - - - - - - - - - - - - - - - ',/&
           '# wert = ',I10,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_id
  !
  !! Drucke den Inhalt der Komponente "pac" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_pac ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_io_info_pac' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%pac)
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente pac - - - - - - - - - - - - - - - - - ',/&
           '# wert = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_pac
  !
  !! Drucke den Inhalt der Komponente "code" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_code ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_io_info_code' ! 
    ! Hilfsvariablen
    INTEGER :: i, stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    !
    IF ( ASSOCIATED( this%code ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat == 0 ) THEN
          DO i=1,SIZE(this%code)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8010, IOSTAT=stat ) this%code(i)
          END DO
          IF ( stat == 0 ) THEN
             WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname )
                CALL setup_error_act ( '<line>', 'Footer-Line')
             END IF
          ELSE
             CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname )
             CALL setup_error_act ( '<line>', 'Data-Line')
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
          CALL setup_error_act ( '<line>', 'Header-Line')
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente code  - - - - - - - - - - - - - - - - ')
8010 FORMAT ('# i = ',I5,', code = ',I5)
8020 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_code
  !
  !! Drucke den Inhalt der Komponente "dim" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_dim ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_io_info_dim' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    !
    IF ( ASSOCIATED( this%dim ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat == 0 ) THEN
          CALL print_dim ( this%dim )
          IF ( no_error( ) ) THEN
             WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname )
                CALL setup_error_act ( '<line>', 'Footer-Line')
             END IF
          ELSE
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname )
             CALL setup_error_act ( '<line>', 'Data-Line')
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<line>', 'Header-Line')
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dim - - - - - - - - - - - - - - - - - ')
8020 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_dim
  !
  !! Drucke den Inhalt der Komponente "var" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_var ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_io_info_var' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    !
    IF ( ASSOCIATED( this%var ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat == 0 ) THEN
          CALL print_var ( this%var )
          IF ( no_error( ) ) THEN
             WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname )
                CALL setup_error_act ( '<line>', 'Footer-Line')
             END IF
          ELSE
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname )
             CALL setup_error_act ( '<line>', 'Data-Line')
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<line>', 'Header-Line')
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente var - - - - - - - - - - - - - - - - - ')
8020 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_var
  !
  !! Drucke den Inhalt der Komponente "att" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_att ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_io_info_att' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    !
    IF ( ASSOCIATED( this%att ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat == 0 ) THEN
          CALL print_att ( this%att )
          IF ( no_error( ) ) THEN
             WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname )
                CALL setup_error_act ( '<line>', 'Footer-Line')
             END IF
          ELSE
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname )
             CALL setup_error_act ( '<line>', 'Data-Line')
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<line>', 'Header-Line')
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente att - - - - - - - - - - - - - - - - - ')
8020 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_att
  !
  !! Drucke den Inhalt der Komponente "time" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_time ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_io_info_time' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    !
    IF ( ASSOCIATED( this%time ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat == 0 ) THEN
          CALL print_datetime ( this%time )
          IF ( no_error( ) ) THEN
             WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7090, c_upname, c_modname )
                CALL setup_error_act ( '<line>', 'Footer-Line')
             END IF
          ELSE
             CALL setup_error_act ( all_errors(:), 7090, c_upname, c_modname )
             CALL setup_error_act ( '<line>', 'Data-Line')
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), 7090, c_upname, c_modname, stat )
          CALL setup_error_act ( '<line>', 'Header-Line')
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente time  - - - - - - - - - - - - - - - - ')
8020 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_time
  !
  !! Drucke den Inhalt der Komponente "lasttime" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_lasttime ( this )
    !! Datenobjekt
    TYPE (t_io_info)   , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER  :: c_upname='print_io_info_lasttime' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%lasttime
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7100, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente lasttime  - - - - - - - - - - - - - - ',/&
           '# wert = ',I10,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_lasttime
  !
  !! Drucke den Inhalt der Komponente "tsec" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_io_info_tsec ( this )
    !! Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_io_info_tsec' ! 
    !! Statusvariable
    INTEGER :: i, stat ! 
    !
    IF ( .NOT. prn_op ) RETURN
    IF ( ASSOCIATED( this%time ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat == 0 ) THEN
          DO i=1,SIZE(this%tsec)
             IF ( stat /= 0 ) EXIT
             WRITE( UNIT=prn_lun, FMT=8010, IOSTAT=stat ) this%tsec(i)
          END DO
          IF ( stat == 0 ) THEN
             WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7110, c_upname, c_modname )
                CALL setup_error_act ( '<line>', 'Footer-Line')
             END IF
          ELSE
             CALL setup_error_act ( all_errors(:), 7110, c_upname, c_modname, stat )
             CALL setup_error_act ( '<line>', 'Data-Line')
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), 7110, c_upname, c_modname, stat )
          CALL setup_error_act ( '<line>', 'Header-Line')
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente tsec  - - - - - - - - - - - - - - - - ')
8010 FORMAT ('# i = ',I10,', wert = ',G15.8)
8020 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_io_info_tsec
!!$  !
!!$  !! Drucke den Inhalt der Komponente "<ComponentName10>" eines Datenobjektes <BR>
!!$  !! Subroutine erzeugt Fehlermeldungen
!!$  SUBROUTINE print_io_info_<ComponentName10> &
!!$       ( this )
!!$    !
!!$    ! Formalparameter
!!$    !! Datenobjekt
!!$    TYPE (t_io_info) , INTENT(IN) :: this ! 
!!$    ! 
!!$    ! lokale Parameter / Variablen
!!$    !! Name der Funktion
!!$    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_io_info_<ComponentName10>' ! 
!!$    !! Statusvariable
!!$    INTEGER :: stat ! 
!!$    !
!!$    IF ( .NOT. prn_op ) RETURN
!!$    !!
!!$    WRITE &
!!$         ( UNIT    = prn_lun,  &
!!$           FMT     = 8000,     & 
!!$           IOSTAT  = stat ) ! schreibe Inhalt von this%<ComponentName10>
!!$    !
!!$    WRITE(*,*) ' *** routine '//TRIM( c_upname )//' , code missing '
!!$    !
!!$    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7120, c_upname, c_modname, stat )
!!$    !
!!$8000 FORMAT &
!!$          ('# Inhalt der Komponente <ComponentName10> - - - - - - - - - - ',/&
!!$           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
!!$    !
!!$  END SUBROUTINE print_io_info_<ComponentName10>
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
!!$  !! pr&uuml;fe Komponente "<ComponentName10>" zweier Datenobjekte auf Gleichheit <BR>
!!$  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
!!$  FUNCTION eq_io_info_<ComponentName10> &
!!$       ( this1,  &
!!$         this2 ) &
!!$         RESULT( ok )
!!$    !
!!$    ! Formalparameter
!!$    !! Referenzobjekt (Skalar)
!!$    TYPE (t_io_info) , INTENT(IN) :: this1 ! 
!!$    !! Vergleichsobjekt (Skalar)
!!$    TYPE (t_io_info) , INTENT(IN) :: this2 ! 
!!$    !
!!$    ! Rueckgabewert
!!$    !! Testergebnis (Skalar)
!!$    LOGICAL :: ok ! 
!!$    !
!!$    ok = ( this1%<ComponentName10> == this2%<ComponentName10> )
!!$    !
!!$    ! ... oder komplizierter
!!$    !
!!$  END FUNCTION eq_io_info_<ComponentName10> 
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
  ! >>> weitere Hilfsfunktionen 
  ! ----------------------------------------------------------------------
  !
  !! ermittle die Werte eines logischen Feldes f&uuml;r einen Vergleich der
  !! Paketbezeichner unter Ber&uuml;cksichtigung des Dateibezeichners "NONE" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_log_pac_d ( this, var ) &
       RESULT( res )
    !! Objektliste
    TYPE (t_io_info)  , INTENT(IN) :: this(:) ! 
    !! Vergleichstext
    CHARACTER (LEN=*) , INTENT(IN) :: var     ! 
    !! Ergebnis: Indikatorfeld f&uuml;r alle Positionen mit Textvergleich TRUE
    LOGICAL :: res(SIZE(this)) ! 
    ! Hilfsvariablen
    INTEGER :: i       ! 
    !
    res = get_log_arr_ch_d( this%pac, var )
    !
    DO i=1,SIZE(this)
       IF ( .NOT. res(i) ) CYCLE
       IF ( file_is_none( this(i)%file ) ) res(i) = .false. 
    END DO
    !
  END FUNCTION get_log_pac_d
  !
  !! ermittle die Werte eines logischen Feldes f&uuml;r Stringvergleiche <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_log_arr_ch_d ( arr, var ) &
       RESULT( res )
    !! Textfeld
    CHARACTER (LEN=*) , INTENT(IN) :: arr(:) ! 
    !! Vergleichstext
    CHARACTER (LEN=*) , INTENT(IN) :: var    ! 
    !! Ergebnis: Indikatorfeld f&uuml;r alle Positionen mit Textvergleich TRUE
    LOGICAL :: res(SIZE(arr)) ! 
    ! Hilfsvariablen
    INTEGER :: i, il ! 
    !
    il = LEN_TRIM(var)
    DO i=1,SIZE(arr)
       res(i) = .false. 
       IF ( LEN_TRIM(arr(i)) == il ) THEN
          IF ( arr(i)(1:il) == var(1:il) ) res(i) = .true.
       END IF
    END DO
    !
  END FUNCTION get_log_arr_ch_d 
  !
  !! ermittle die Indexposition des n-ten g&uuml;tigen Eintrags <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehldermeldungen
  FUNCTION get_log_arr_idx_d ( arr, n ) &
       RESULT( res )
    !! Feld mit Werten true / false
    LOGICAL , INTENT(IN) :: arr(:) ! 
    !! N-ter gesuchter g&uuml;tiger Eintrag
    INTEGER , INTENT(IN) :: n      ! 
    !! Ergebnis: Indexposition f&uuml;r den n-ten g&uuml;ltigen Eintrag <BR>
    !! falls kein Eintrag gefunden wird, wird c_undef_in zur&uuml;ckgegeben
    INTEGER :: res                 ! 
    ! Hilfsvariablen
    INTEGER :: i, m ! 
    !
    res = c_undef_in
    m   = 0
    DO i=1,SIZE(arr)
       IF ( res /= c_undef_in ) EXIT
       IF ( arr(i) ) m   = m + 1
       IF ( m == n ) res = i
    END DO
    !
  END FUNCTION get_log_arr_idx_d
  !
  !! Ermittle die Position der Variablen f&uuml;r die n-te Code-Bezeichnung <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_code_var_position_0_n ( this, n ) &
       RESULT(res)
    !! (skalares) Datenobjekt
    TYPE (t_io_info) , INTENT(IN) :: this ! 
    !! n-te Position innerhalb von this%code
    INTEGER          , INTENT(IN) :: n    ! 
    !! Ergebnis : Positionsindex in this%var, falls nicht vorhanden 
    !!            wird "c_undef_in" zur&uuml;ckgegeben
    INTEGER                       :: res  ! 
    ! Hilfsvariablen
    INTEGER :: i, code ! 
    !
    res = c_undef_in
    IF ( ASSOCIATED(this%code) .AND. ASSOCIATED(this%att) .AND. ASSOCIATED(this%var) ) THEN
       IF ( n >= 1 .AND. n <= SIZE(this%code) ) THEN
          DO i=1,SIZE(this%var)
             code = get_att_var_name_id( this%att, this%var(i) )
             IF ( code == this%code(n) ) THEN
                res = i
                EXIT
             END IF
          END DO
       END IF
    END IF
    !
  END FUNCTION get_code_var_position_0_n
  !
  !! Ermittle die Position der Variablen f&uuml;r die n-te Code-Bezeichnung <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_position_0_t ( this, time ) &
       RESULT(res)
    !! (skalares) Datenobjekt
    TYPE (t_io_info)  , INTENT(IN) :: this ! 
    !! aktueller Termin (es wird der n&auml;chstgelegene Termin gesucht)
    TYPE (t_datetime) , INTENT(IN) :: time ! 
    !! Ergebnis : Positionsindex in this%time, falls nicht vorhanden 
    !!            wird "c_undef_in" zur&uuml;ckgegeben
    INTEGER                        :: res  ! 
    ! Hilfsvariablen
    INTEGER , PARAMETER :: c_nn_min=5000   ! 
    INTEGER :: nn        ! 
    TYPE (t_time) :: d_time ! 
    REAL (KIND=Double) :: d_tsec ! 
    REAL (KIND=Double) , SAVE, ALLOCATABLE :: r_time(:) ! ... reduziert den Aufwand
    !
    res = c_undef_in
    IF ( ASSOCIATED(this%time) .AND. ASSOCIATED(this%tsec) ) THEN
       nn = MIN(SIZE(this%time),SIZE(this%tsec)) 
       IF ( ALLOCATED(r_time) ) THEN
          IF ( nn > SIZE(r_time) ) DEALLOCATE(r_time)
       END IF
       IF ( .NOT. ALLOCATED(r_time) ) ALLOCATE(r_time(MAX(nn,c_nn_min)))
       d_time       = su_datetime( time, this%time(1) )
       d_tsec       = time_to_real_seconds(d_time)
       r_time(1:nn) = ABS(this%tsec(1:nn) - d_tsec)
       res          = MINVAL(MINLOC(r_time(1:nn)))
    END IF
    !
  END FUNCTION get_time_position_0_t
  !
END MODULE b_io_info
! TailOfBaseModule --------------------------------------------------------
