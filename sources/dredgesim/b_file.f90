! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Datentyp und Routinen f&uuml;r die Dateibehandlung</h2>
!! @author Ingrid Uliczka und Jens J&uuml;rges
!! @version 1.44 vom 03/06/07, Quellcode: mod_b_file.f90
!! <HR>
!! data type and functions/subroutines to handle files under Fortran90
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) <Year> <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  1.1   : 2002-04-15 : I. Uliczka, J. Juerges :  
!                       Originalversion
!  1.4   : 2002-06-13 : G. Lang, I. Uliczka, J. Juerges :  
!                       - INIT und CLEAR mit Hierarchie
!                       - SETUP_*_prn_lun und "SETUP_*_trc_lun mit Hierarchie
!  1.5   : 2002-06-20 : J. Juerges :  
!                       Neue Fehlertexte fuer ok_file_name_status
!  1.6   : 2002-06-28 : J. Juerges :  
!                       PRINT-Ausgabe fuer CHARACTER-Variable optimiert
!  1.7   : 2002-06-28 : J. Juerges :
!                       Dateityp SOIL und MDS_ASC integriert
!  1.8   : 2002-07-01 : Jens Juerges :
!                       PRINT-Ausgaben nicht auf STDOUT
!  1.9   : 2002-07-03 : G. Lang :
!                       neuer Typ "VERTICAL"
!  1.10  : 2002-07-04 : J. Juerges :
!                       Gross-/Kleinschreibweise fuer Dateitypen wird nicht unterschieden
!  1.11  : 2002-07-05 : J. Juerges :
!                       Neue Gitternetz-Dateitypen ergaenzt
!  1.12  : 2002-07-05 : J. Juerges :
!                       Neues Konzept fuer Gross-/Kleinschreibweise
!                       der Dateitypen: Nur beim Vergl. mit den def.
!                       Typen wird nach Grossbuchstaben konvertiert.
!                       So bleiben in der Komponente %type die
!                       Originalangaben des Nutzers erhalten.
!  1.13  : 2002-07-09 : G. Lang :
!                       RECL bei UNFORMATTED/SEQUENTIAL entfernt
!  1.14  : 2002-08-07 : J. Juerges :
!                       Fehlertextdefinition vereinfacht
!  1.15  : 2002-08-08 : G. Lang :
!                       get_file_compression_ext
!  1.16  : 2002-08-13 : G. Lang :
!                       zus. Text wg. Komponente RECL
!  1.17  : 2002-08-13 : J. Juerges :
!                       MDS_ASC nach IPDS umbenannt
!  1.18  : 2002-08-13 : J. Juerges :
!                       set_char_2_uppercase_0 muss auch in
!                       auto_file_access_form_0 eingesetzt werden
!  1.19  : 2002-08-21 : G. Lang :
!                       neue Dateitypen NETCDF und HDF5
!  1.20  : 2002-08-21 : J. Juerges :
!                       auto_file_access_form fuer NETCDF und HDF5 erweitert
!  1.21  : 2002-09-03 : I. Uliczka :
!                       OK_file_type ist jetzt PUBLIC
!  1.22  : 2002-09-05 : G. Lang :
!                       DELIM-Parameter beim Oeffnen von "UNFORMATTED" Dateien entfernt
!  1.23  : 2002-09-05 : G. Lang :
!                       POSITION-Parameter entfernt beim Oeffnen von "DIRECT" Dateien
!  1.24  : 2002-09-25 : J. Juerges :
!                       is_compressed-Methode braucht kein ok_file_unit
!  1.25  : 2003-01-10 : J. Juerges :
!                       Anpassungen der Dokumentation
!  1.26  : 2003-01-29 : J. Juerges :
!                       Auslagerung der Testroutine
!  1.27  : 2003-07-10 : A. Malcherek
!                       Rekursive IO-Anweisung entfernt
!  1.28  : 2004-01-08 : P. Schade
!                       Typen UCD, UCDCLASSIC undCIN, +asciend ucd, ucdc
!  1.29  : 2004-01-19 : H. Weilbeer
!                       Bei selbstdefinierten Datentypen INTENT(OUT) -> INTENT(INOUT) gewandelt
!  1.30  : 2004-01-19 : G. Lang
!                       neuer Dateityp "Geopos_GKK"
!  1.31  : 2004-01-19 : G. Lang
!                       "Geopos_GKK" in auto_file_access_form beruecksichtigt
!  1.32  : 2004-04-28 : J. A. Jankowski
!                       Types TECPLOT (ascii), TECPLOTBIN (binary),
!                       Types TECPL10 (ascii), TECPL10BIN (binary)
!  1.33  : 2004-04-30 : P. Schade
!                       WRITE(*,*) ... version ... korrigiert
!  1.34  : 2005-03-17 : G. Lang : 
!                       Gleicheitsoperator auch als Funktion
!  1.35  : 2005-07-06 : G. Lang : 
!                       Komponente "path" sowie weitere Funktionen ergaenzt/modifiziert
!  1.36  : 2005-07-07 : G. Lang : 
!                       ok_file_path_and_name_len einfuegen
!  1.37  : 2005-08-05 : J. Juerges : 
!                       Dateityp LPROFIL hinzugefuegt
!  1.38  : 2005-08-15 : G. Lang      : 
!                       Dateityp DELFT3D hinzugefuegt
!                       neue Endungen fuer SCII-Files ergaenzt
!  1.39  : 2005-08-16 : G. Lang :
!                       Laengenbeschraenkung path+name auf 240 entfaellt
!                       Fehler in get_file_path_and_name[_unc] korrigiert
!  1.40  : 2005-12-14 : G. Lang :
!                       In "close_file" wird versucht in jedem Fall die
!                       Datei zu schliessen, auch wenn zuvor ein Fehler
!                       aufgetreten ist. 
!  1.41  : 2007-02-08 : G. Lang :
!                       Neue Funktion "remove_file" .
!  1.42  : 2007-02-08 : G. Lang :
!                       uncompress_cmds in USE vergessen - sorry!
!  1.43  : 2007-02-26 : G. Lang :
!                       neue Funktion eq_file_path_and_name
!  1.44  : 2007-03-06 : G. Lang :
!                       neue Funtion get_file_systemfile
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Bereitstellung eines Datentyps und zugeh&ouml;rigen Funktionen   <BR>
!! und Subroutinen f&uuml;r die Arbeit mit Dateien unter Fortran90. <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_file       <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:<BR>
!! <OL>
!!     <LI> unit     : Kanalnummer
!!     <LI> path     : Pfadbezeichner (ohne Anteil des Namens)
!!     <LI> name     : Dateiname (inkl. ".gz" oder ".Z")
!!     <LI> status   : Status der Datei
!!                     ['OLD','NEW','UNKNOWN','SCRATCH','REPLACE']
!!     <LI> access   : Zugriff ['SEQUENTIAL','DIRECT']
!!     <LI> form     : Format ['UNFORMATTED','FORMATTED']
!!     <LI> recl     : (maximale) Datensatzl&auml;nge in Bytes
!!     <LI> position : Position ['REWIND','APPEND','ASIS']
!!     <LI> action   : Aktion ['READ','WRITE','READWRITE']
!!     <LI> delim    : Delimiter ['APOSTROPHE','QUOTE','NONE']
!!     <LI> type     : Typ ['UNKNOWN','STDIN','STDOUT','BDF',...]
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
!!    <LI> Initialisieren des Moduls b_file mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_file mit CLEAR-Methode.
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
!!          die Methode PRINT_FILE_ALL_ERRORS.
!!                                                                    <BR>
!! <HR>
!
MODULE b_file
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit Fehler-Typ und -Routinen
  !
  USE b_error, ONLY :       &
       ! Typdefinitionen
       t_error,             &
       ! Routinen
       init_error, clear_error, no_error, any_error,              &
       new_error, kill_error, print_error,                        &
       setup_error_act, setup_error_prn_lun, setup_error_trc_lun, &
       set_error_ierr, set_error_cerr, clear_error_act
  !
  ! [A.2] BASIS-Modul mit Kommando-Methoden
  !
  USE b_cmds, ONLY :                           &
       ! Routinen
       init_cmds, clear_cmds,                  &    
       setup_cmds_prn_lun, setup_cmds_trc_lun, &
       rm_cmds, uncompress_cmds, compress_cmds
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
  ! [C.0] Konstentwerte
  !
  !! max. L&auml;nge des Pfadbezeichners "path"
  INTEGER , PUBLIC, PARAMETER :: c_file_path_len=160          ! 
  !! max. L&auml;nge des Namens "name"
  INTEGER , PUBLIC, PARAMETER :: c_file_name_len=80           ! 
  !! max. vollst&auml;ndige L&auml;nge "path" + "name"
  INTEGER , PUBLIC, PARAMETER :: c_file_path_and_name_len=240 ! 
  !! max. L&auml;nge der Komponente "status"
  INTEGER , PUBLIC, PARAMETER :: c_file_status_len=7          ! 
  !! max. L&auml;nge der Komponente "access"
  INTEGER , PUBLIC, PARAMETER :: c_file_access_len=10         ! 
  !! max. L&auml;nge der Komponente "form"
  INTEGER , PUBLIC, PARAMETER :: c_file_form_len=11           ! 
  !! max. L&auml;nge der Komponente "position"
  INTEGER , PUBLIC, PARAMETER :: c_file_position_len=6        ! 
  !! max. L&auml;nge der Komponente "action"
  INTEGER , PUBLIC, PARAMETER :: c_file_action_len=9          ! 
  !! max. L&auml;nge der Komponente "delim"
  INTEGER , PUBLIC, PARAMETER :: c_file_delim_len=10          ! 
  !! max. L&auml;nge der Komponente "type"
  INTEGER , PUBLIC, PARAMETER :: c_file_type_len=20           ! 
  !! max. L&auml;nge der Kompressionsextension
  INTEGER , PUBLIC, PARAMETER :: c_file_compr_ext_len=4       ! 
  ! 
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Datei-Datentyp; stellt alle notwendigen Daten f&uuml;r die
  !! Behandlung einer Datei zur Verf&uuml;gung                <BR>
  !! unit     : Kanalnummer                                   <BR>
  !! path     : Pfabbezeichnung                               <BR>
  !! name     : Dateiname (inkl. ".gz" oder ".Z")             <BR>
  !! status   : Status der Datei                              <BR>
  !!            ['OLD','NEW','UNKNOWN','SCRATCH','REPLACE']   <BR>
  !! access   : Zugriff ['SEQUENTIAL','DIRECT']               <BR>
  !! form     : Format ['UNFORMATTED','FORMATTED']            <BR>
  !! recl     : (maximale) Datensatzl&auml;nge in Bytes       <BR>
  !! position : Position ['REWIND','APPEND','ASIS']           <BR>
  !! action   : Aktion ['READ','WRITE','READWRITE']           <BR>
  !! delim    : Delimiter ['APOSTROPHE','QUOTE','NONE']       <BR>
  !! type     : Typ ['UNKNOWN','STDIN','STDOUT','BDF',...]    
  TYPE , PUBLIC :: t_file
     PRIVATE
     INTEGER                             :: unit     ! 
     CHARACTER (LEN=c_file_path_len)     :: path     ! 
     CHARACTER (LEN=c_file_name_len)     :: name     ! 
     CHARACTER (LEN=c_file_status_len)   :: status   ! 
     CHARACTER (LEN=c_file_access_len)   :: access   ! 
     CHARACTER (LEN=c_file_form_len)     :: form     ! 
     INTEGER                             :: recl     ! 
     CHARACTER (LEN=c_file_position_len) :: position ! 
     CHARACTER (LEN=c_file_action_len)   :: action   ! 
     CHARACTER (LEN=c_file_delim_len)    :: delim    ! 
     CHARACTER (LEN=c_file_type_len)     :: type     ! 
  END TYPE t_file
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
  INTERFACE init_file
     MODULE PROCEDURE init_file_d
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE clear_file
     MODULE PROCEDURE clear_file_d
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_file_prn_lun
     MODULE PROCEDURE setup_file_prn_lun_d
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_file_trc_lun
     MODULE PROCEDURE setup_file_trc_lun_d
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_file" (Skalar, 1D-Array)
  INTERFACE new_file
     MODULE PROCEDURE new_file_0  ! Version fuer Skalar
     MODULE PROCEDURE new_file_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_file" (Skalar, 1D-Array)
  INTERFACE kill_file
     MODULE PROCEDURE kill_file_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_file_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_file" (Skalar, 1D-Array)
  INTERFACE ok_file
     MODULE PROCEDURE ok_file_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_file_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken von Datenobjekten "t_file" (Skalar, 1D-Array)
  INTERFACE print_file
     MODULE PROCEDURE print_file_0 ! Version fuer Skalar
     MODULE PROCEDURE print_file_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten
  INTERFACE print_file_static
     MODULE PROCEDURE print_file_static_d
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls
  INTERFACE print_file_all_errors
     MODULE PROCEDURE print_file_all_errors_d
  END INTERFACE
  !
  ! ----------------------------------------------------------------
  ! --> nicht benoetigte SET-Interfaces bitte unbedingt loeschen <--
  ! ----------------------------------------------------------------
  !
  !! Setze Komponente "unit" in "t_file" auf Benutzerwert
  INTERFACE set_file_unit
     MODULE PROCEDURE set_file_unit_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_unit_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "path" in "t_file" auf Benutzerwert
  INTERFACE set_file_path
     MODULE PROCEDURE set_file_path_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_path_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "name" in "t_file" auf Benutzerwert
  INTERFACE set_file_name
     MODULE PROCEDURE set_file_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_name_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponenten "path" und "name" in "t_file" auf Benutzerwert
  INTERFACE set_file_path_and_name
     MODULE PROCEDURE set_file_path_and_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_path_and_name_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "status" in "t_file" auf Benutzerwert
  INTERFACE set_file_status
     MODULE PROCEDURE set_file_status_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_status_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "access" in "t_file" auf Benutzerwert
  INTERFACE set_file_access
     MODULE PROCEDURE set_file_access_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_access_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "form" in "t_file" auf Benutzerwert
  INTERFACE set_file_form
     MODULE PROCEDURE set_file_form_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_form_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "recl" in "t_file" auf Benutzerwert
  INTERFACE set_file_recl
     MODULE PROCEDURE set_file_recl_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_recl_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "position" in "t_file" auf Benutzerwert
  INTERFACE set_file_position
     MODULE PROCEDURE set_file_position_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_position_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "action" in "t_file" auf Benutzerwert
  INTERFACE set_file_action
     MODULE PROCEDURE set_file_action_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_action_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "delim" in "t_file" auf Benutzerwert
  INTERFACE set_file_delim
     MODULE PROCEDURE set_file_delim_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_delim_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !! Setze Komponente "type" in "t_file" auf Benutzerwert
  INTERFACE set_file_type
     MODULE PROCEDURE set_file_type_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_file_type_1_0 ! Objekt (Vektor) / Daten (Skalar)
  END INTERFACE
  !
  ! ... ggf. Setzen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! ----------------------------------------------------------------
  ! --> nicht benoetigte GET-Interfaces bitte unbedingt loeschen <--
  ! ----------------------------------------------------------------
  !
  !! Hole Komponente "unit" aus "t_file"
  INTERFACE get_file_unit
     MODULE PROCEDURE get_file_unit_0_0 ! Skalar
     MODULE PROCEDURE get_file_unit_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "path" aus "t_file"
  INTERFACE get_file_path
     MODULE PROCEDURE get_file_path_0_0 ! Skalar
     MODULE PROCEDURE get_file_path_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "name" aus "t_file"
  INTERFACE get_file_name
     MODULE PROCEDURE get_file_name_0_0 ! Skalar
     MODULE PROCEDURE get_file_name_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "path" und "name" aus "t_file"
  INTERFACE get_file_path_and_name
     MODULE PROCEDURE get_file_path_and_name_0_0 ! Skalar
     MODULE PROCEDURE get_file_path_and_name_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "status" aus "t_file"
  INTERFACE get_file_status
     MODULE PROCEDURE get_file_status_0_0 ! Skalar
     MODULE PROCEDURE get_file_status_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "access" aus "t_file"
  INTERFACE get_file_access
     MODULE PROCEDURE get_file_access_0_0 ! Skalar
     MODULE PROCEDURE get_file_access_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "form" aus "t_file"
  INTERFACE get_file_form
     MODULE PROCEDURE get_file_form_0_0 ! Skalar
     MODULE PROCEDURE get_file_form_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "recl" aus "t_file"
  INTERFACE get_file_recl
     MODULE PROCEDURE get_file_recl_0_0 ! Skalar
     MODULE PROCEDURE get_file_recl_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "position" aus "t_file"
  INTERFACE get_file_position
     MODULE PROCEDURE get_file_position_0_0 ! Skalar
     MODULE PROCEDURE get_file_position_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "action" aus "t_file"
  INTERFACE get_file_action
     MODULE PROCEDURE get_file_action_0_0 ! Skalar
     MODULE PROCEDURE get_file_action_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "delim" aus "t_file"
  INTERFACE get_file_delim
     MODULE PROCEDURE get_file_delim_0_0 ! Skalar
     MODULE PROCEDURE get_file_delim_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "type" aus "t_file"
  INTERFACE get_file_type
     MODULE PROCEDURE get_file_type_0_0 ! Skalar
     MODULE PROCEDURE get_file_type_1_0 ! Vektor
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Existiert die Kanalnummer "unit" ?
  INTERFACE unit_exists
     MODULE PROCEDURE unit_exists_0 ! Skalar
     MODULE PROCEDURE unit_exists_1 ! Vektor
  END INTERFACE
  !! ... oder existiert "unit" nicht ?
  INTERFACE unit_exists_not
     MODULE PROCEDURE unit_exists_not_0 ! Skalar
     MODULE PROCEDURE unit_exists_not_1 ! Vektor
  END INTERFACE
  !! Ist die Datei "unit" ge&ouml;ffnet ?
  INTERFACE unit_is_opened
     MODULE PROCEDURE unit_is_opened_0 ! Skalar
     MODULE PROCEDURE unit_is_opened_1 ! Vektor
  END INTERFACE
  !! ... oder ist "unit" geschlossen ?
  INTERFACE unit_is_closed
     MODULE PROCEDURE unit_is_closed_0 ! Skalar
     MODULE PROCEDURE unit_is_closed_1 ! Vektor
  END INTERFACE
  !! Ist der Dateiname "file" mit 'NONE' belegt ?
  INTERFACE file_is_none
     MODULE PROCEDURE file_is_none_0 ! Skalar
     MODULE PROCEDURE file_is_none_1 ! Vektor
  END INTERFACE
  !! Ist die Datei komprimiert ?
  INTERFACE file_is_compressed
     MODULE PROCEDURE file_is_compressed_0 ! Skalar
     MODULE PROCEDURE file_is_compressed_1 ! Vektor
  END INTERFACE
  !! ... oder ist sie unkomprimiert ?
  INTERFACE file_is_uncompressed
     MODULE PROCEDURE file_is_uncompressed_0 ! Skalar
     MODULE PROCEDURE file_is_uncompressed_1 ! Vektor
  END INTERFACE
  !! Existiert der Dateiname "file" ?
  INTERFACE file_exists
     MODULE PROCEDURE file_exists_0 ! Skalar
     MODULE PROCEDURE file_exists_1 ! Vektor
  END INTERFACE
  !! ... oder existiert "file" nicht ?
  INTERFACE file_exists_not
     MODULE PROCEDURE file_exists_not_0 ! Skalar
     MODULE PROCEDURE file_exists_not_1 ! Vektor
  END INTERFACE
  !! Ist die Datei "file" ge&ouml;ffnet ?
  INTERFACE file_is_opened
     MODULE PROCEDURE file_is_opened_0 ! Skalar
     MODULE PROCEDURE file_is_opened_1 ! Vektor
  END INTERFACE
  !! ... oder ist "file" geschlossen ?
  INTERFACE file_is_closed
     MODULE PROCEDURE file_is_closed_0 ! Skalar
     MODULE PROCEDURE file_is_closed_1 ! Vektor
  END INTERFACE
  !! Ist die Datei "file" formatiert ?
  INTERFACE file_is_formatted
     MODULE PROCEDURE file_is_formatted_0  ! Datentyp t_file wird uebergeben (Skalar)
     MODULE PROCEDURE file_is_formatted_1  ! Datentyp t_file wird uebergeben (Vektor)
     MODULE PROCEDURE file_is_formatted_f0 ! Komponente "form" wird uebergeben (Skalar)
     MODULE PROCEDURE file_is_formatted_f1 ! Komponente "form" wird uebergeben (Vektor)
  END INTERFACE
  !! ... oder ist "file" unformatiert ?
  INTERFACE file_is_unformatted
     MODULE PROCEDURE file_is_unformatted_0  ! Datentyp t_file wird uebergeben (Skalar)
     MODULE PROCEDURE file_is_unformatted_1  ! Datentyp t_file wird uebergeben (Vektor)
     MODULE PROCEDURE file_is_unformatted_f0 ! Komponente "form" wird uebergeben (Skalar)
     MODULE PROCEDURE file_is_unformatted_f1 ! Komponente "form" wird uebergeben (Vektor)
  END INTERFACE
  !! Ist der Zugriff auf "file" sequentiell ?
  INTERFACE file_is_sequential
     MODULE PROCEDURE file_is_sequential_0  ! Datentyp t_file wird uebergeben (Skalar)
     MODULE PROCEDURE file_is_sequential_1  ! Datentyp t_file wird uebergeben (Vektor)
     MODULE PROCEDURE file_is_sequential_a0 ! Komponente "access" wird uebergeben (Skalar)
     MODULE PROCEDURE file_is_sequential_a1 ! Komponente "access" wird uebergeben (Vektor)
  END INTERFACE
  !! ... oder Direkt-Zugriff ?
  INTERFACE file_is_direct
     MODULE PROCEDURE file_is_direct_0  ! Datentyp t_file wird uebergeben (Skalar)
     MODULE PROCEDURE file_is_direct_1  ! Datentyp t_file wird uebergeben (Vektor)
     MODULE PROCEDURE file_is_direct_a0 ! Komponente "access" wird uebergeben (Skalar)
     MODULE PROCEDURE file_is_direct_a1 ! Komponente "access" wird uebergeben (Vektor)
  END INTERFACE
  !! Besitzt der Dateiname eine ASCII-Endung ?
  INTERFACE file_is_ascii
     MODULE PROCEDURE file_is_ascii_0 ! Skalar
     MODULE PROCEDURE file_is_ascii_1 ! Vektor
  END INTERFACE
  !! ... oder eine Binaer-Endung ?
  INTERFACE file_is_binary
     MODULE PROCEDURE file_is_binary_0 ! Skalar
     MODULE PROCEDURE file_is_binary_1 ! Vektor
  END INTERFACE
  !! Dateiname und -status passen zusammen ?
  INTERFACE ok_file_name_status
     MODULE PROCEDURE ok_file_name_status_0
     MODULE PROCEDURE ok_file_name_status_1
  END INTERFACE
  !! Hole Dateiname ohne evt. Kompressions-Endung
  INTERFACE get_file_uncompressed_name
     MODULE PROCEDURE get_file_uncompressed_name_0 ! Skalar
     MODULE PROCEDURE get_file_uncompressed_name_1 ! Vektor
  END INTERFACE
  !! Hole L&auml;nge des Pfadnamens
  INTERFACE get_file_path_len
     MODULE PROCEDURE get_file_path_len_0 ! Skalar
     MODULE PROCEDURE get_file_path_len_1 ! Vektor
  END INTERFACE
  !! Hole L&auml;nge des Dateinamens
  INTERFACE get_file_name_len
     MODULE PROCEDURE get_file_name_len_0 ! Skalar
     MODULE PROCEDURE get_file_name_len_1 ! Vektor
  END INTERFACE
  !! Hole L&auml;nge des Pfad- und Dateinamens
  INTERFACE get_file_path_and_name_len
     MODULE PROCEDURE get_file_path_and_name_len_0 ! Skalar
     MODULE PROCEDURE get_file_path_and_name_len_1 ! Vektor
  END INTERFACE
  !! Hole L&auml;nge des Dateinamens ohne Kompressions-Endung
  INTERFACE get_file_uncompressed_name_len
     MODULE PROCEDURE get_file_uncompressed_name_len0 ! Skalar
     MODULE PROCEDURE get_file_uncompressed_name_len1 ! Vektor
  END INTERFACE
  !! Hole Typnamen des Kompressionsverfahrens
  INTERFACE get_file_compression_type
     MODULE PROCEDURE get_file_compression_type_0 ! Skalar
     MODULE PROCEDURE get_file_compression_type_1 ! Vektor
  END INTERFACE
  !! Hole die Dateiendung des Kompressionsverfahrens
  INTERFACE get_file_compression_ext
     MODULE PROCEDURE get_file_compression_ext_0 ! Skalar
     MODULE PROCEDURE get_file_compression_ext_1 ! Vektor
  END INTERFACE
  !! Hole aus eine Liste von Dateien den Dateibezeichner f&uuml;r die Systemdatei
  INTERFACE get_file_systemfile
     MODULE PROCEDURE get_file_systemfile_1 ! Vektor
  END INTERFACE
  !! Zugriff und Format aus Name und Typ ableiten
  INTERFACE auto_file_access_form
     MODULE PROCEDURE auto_file_access_form_0 ! Skalar
     MODULE PROCEDURE auto_file_access_form_1 ! Vektor
  END INTERFACE
  !! Oeffne Datei
  INTERFACE open_file
     MODULE PROCEDURE open_file_0 ! Skalar
     MODULE PROCEDURE open_file_1 ! Vektor
  END INTERFACE
  !! Schliesse Datei
  INTERFACE close_file
     MODULE PROCEDURE close_file_0 ! Skalar
     MODULE PROCEDURE close_file_1 ! Vektor
  END INTERFACE
  !! Pr&uuml;fe Filetyp
  INTERFACE ok_file_type
     MODULE PROCEDURE ok_file_type_0  ! Datentyp t_file wird uebergeben (Skalar)
     MODULE PROCEDURE ok_file_type_1  ! Datentyp t_file wird uebergeben (Vektor)
     MODULE PROCEDURE ok_file_type_t0 ! Komponente type wird uebergeben (Skalar)
     MODULE PROCEDURE ok_file_type_t1 ! Komponente type wird uebergeben (Vektor)
  END INTERFACE
  !! Entferne eine oder mehrere Dateien, falls sie vorhanden sind
  INTERFACE remove_file
     MODULE PROCEDURE remove_file_0   ! entferne eine Datei
     MODULE PROCEDURE remove_file_1   ! entferne mehrere Dateien
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
  !! Pr&uuml;fung zweier Datenobjekte "t_file" auf Gleichheit
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_file_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_file_0_1  ! Skalar / Vektor
     MODULE PROCEDURE eq_file_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_file_1_1  ! Vektor / Vektor
  END INTERFACE
  INTERFACE eq_file
     MODULE PROCEDURE eq_file_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_file_0_1  ! Skalar / Vektor
     MODULE PROCEDURE eq_file_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_file_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich von Datenobjekten auf der Basis der Konponenten "path" und "name"
  INTERFACE eq_file_path_and_name
     MODULE PROCEDURE eq_file_path_and_name_0_0 ! Skalar / Skalar
     MODULE PROCEDURE eq_file_path_and_name_0_1 ! Skalar / Vektor
     MODULE PROCEDURE eq_file_path_and_name_1_0 ! Vektor / Skalar
     MODULE PROCEDURE eq_file_path_and_name_1_1 ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_file" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_file_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_file_0_1  ! Skalar / Vektor
     MODULE PROCEDURE ne_file_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_file_1_1  ! Vektor / Vektor
  END INTERFACE
  INTERFACE ne_file
     MODULE PROCEDURE ne_file_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_file_0_1  ! Skalar / Vektor
     MODULE PROCEDURE ne_file_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_file_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_file
  PUBLIC :: clear_file
  PUBLIC :: setup_file_prn_lun
  PUBLIC :: setup_file_trc_lun
  PUBLIC :: new_file
  PUBLIC :: kill_file
  PUBLIC :: ok_file
  PUBLIC :: print_file
  PUBLIC :: print_file_static
  PUBLIC :: print_file_all_errors
  PUBLIC :: set_file_unit
  PUBLIC :: set_file_path
  PUBLIC :: set_file_name
  PUBLIC :: set_file_path_and_name
  PUBLIC :: set_file_status
  PUBLIC :: set_file_access
  PUBLIC :: set_file_form
  PUBLIC :: set_file_recl
  PUBLIC :: set_file_position
  PUBLIC :: set_file_action
  PUBLIC :: set_file_delim
  PUBLIC :: set_file_type
  PUBLIC :: get_file_unit
  PUBLIC :: get_file_path
  PUBLIC :: get_file_name
  PUBLIC :: get_file_path_and_name
  PUBLIC :: get_file_status
  PUBLIC :: get_file_access
  PUBLIC :: get_file_form
  PUBLIC :: get_file_recl
  PUBLIC :: get_file_position
  PUBLIC :: get_file_action
  PUBLIC :: get_file_delim
  PUBLIC :: get_file_type
  PUBLIC :: eq_file
  PUBLIC :: eq_file_path_and_name
  PUBLIC :: OPERATOR(==)
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: ne_file
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: unit_exists
  PUBLIC :: unit_exists_not
  PUBLIC :: unit_is_opened
  PUBLIC :: unit_is_closed
  PUBLIC :: file_is_none
  PUBLIC :: file_is_compressed
  PUBLIC :: file_is_uncompressed
  PUBLIC :: file_exists
  PUBLIC :: file_exists_not
  PUBLIC :: file_is_opened
  PUBLIC :: file_is_closed
  PUBLIC :: file_is_formatted
  PUBLIC :: file_is_unformatted
  PUBLIC :: file_is_sequential
  PUBLIC :: file_is_direct
  PUBLIC :: file_is_ascii
  PUBLIC :: file_is_binary
  PUBLIC :: ok_file_name_status
  PUBLIC :: get_file_uncompressed_name
  PUBLIC :: get_file_path_len
  PUBLIC :: get_file_name_len
  PUBLIC :: get_file_path_and_name_len
  PUBLIC :: get_file_uncompressed_name_len
  PUBLIC :: get_file_compression_type
  PUBLIC :: get_file_compression_ext
  PUBLIC :: get_file_systemfile
  PUBLIC :: auto_file_access_form
  PUBLIC :: open_file
  PUBLIC :: close_file
  PUBLIC :: ok_file_type
  PUBLIC :: remove_file
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
  CHARACTER (LEN= 6), PARAMETER :: c_modname                     = 'b_file'
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op                          = .false.
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun                         = -1 ! 
  !! Anzahl der Datenkomponenten des Typs t_file
  INTEGER           , PARAMETER :: c_nofcomp                     = 11 ! 
  !! Anzahl der definierten (erlaubten) Dateitypen
  INTEGER           , PARAMETER :: c_noffiletypes                = 36 ! 
  !! Bezeichnungen aller definierten (erlaubten) Dateitypen <BR>
  !! Hinweis1: Alle Typen in GROSSBUCHSTABEN notieren.      <BR>
  !! Hinweis2: Liste muss (noch) nicht vollstaendig sein.
  CHARACTER (LEN=10), PARAMETER :: c_filetypes(c_noffiletypes)   = (/ &
       'UNKNOWN   ', 'STDIN     ', 'STDOUT    ', 'BDF       ', 'KNOERG    ', &
       'BOEWRT    ', 'LIGHTS    ', 'FRAMES    ', 'LAYOUT    ', 'INSEL     ', &
       'ISOERG    ', 'STEERING  ', 'SOIL      ', 'IPDS      ', 'VERTICAL  ', &
       'GITTER05  ', 'SELAFIN   ', 'UNTRIM_BAW', 'UNTRIM_VC ', 'PROFIL05  ', &
       'LOCATIONS ', 'TOPO      ', 'TR2TOPO   ', 'TR2TOPOIND', 'NETCDF    ', &
       'HDF5      ', 'UCD       ', 'UCDCLASSIC', 'UCDBIN    ', 'GEOPOS_GKK', &
       'TECPLOT   ', 'TECPLOTBIN', 'TECPL10   ', 'TECPL10BIN', 'LPROFIL   ', &
       'DELFT3D   ' /)
  !! Anzahl der definierten (erlaubten) ASCII-Dateiendungen
  INTEGER           , PARAMETER :: c_noffileasciis               = 29
  !! Bezeichnungen aller definierten (erlaubten) ASCII-Dateiendungen
  CHARACTER (LEN= 5), PARAMETER :: c_fileasciis(c_noffileasciis) = (/ &
       '.DAT ',  '.dat ',  '.cfg ',  '.SDR ',  '.sdr ',  '.TRC '    , & ! 01 - 06 
       '.trc ',  '.save',  '.poly',  '.node',  '.edge',  '.ele '    , & ! 07 - 12
       '.lay ',  '.digi',  '.DIGI',  '.gkk ',  '.GKK ',  '.R   '    , & ! 13 - 18
       '.f   ',  '.f90 ',  '.r90 ',  '.ucd ',  '.ucdc',  '.plt '    , & ! 19 - 24
       '.pl10',  '.grd ',  '.enc ',  '.dry ',  '.bnd ' /)               ! 25 - 29
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error), ALLOCATABLE, SAVE :: all_errors(:)
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                    , SAVE :: initialised = .false.
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                    , SAVE :: prn_op      = c_op
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                    , SAVE :: trc_op      = c_op
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                    , SAVE :: prn_lun     = c_lun
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                    , SAVE :: trc_lun     = c_lun
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                    , SAVE :: n_init      = 0
  !
  ! [D.4] Schnittstellen
  !
  !! Hole Komponente "path" und "name" (unkomprimiert) aus "t_file"
  INTERFACE get_file_path_and_name_unc
     MODULE PROCEDURE get_file_path_and_name_unc_0_0 ! Skalar
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
  SUBROUTINE init_file_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_file" version 1.44 of 03/06/07'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_cmds ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_file_all_errors ( )
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = no_error( )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_file_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_file_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_file_all_errors ( )
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = .NOT. no_error( )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_cmds ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_file_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_file_prn_lun_d ( lun )
    !
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden
    INTEGER , INTENT(IN) :: lun !
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_file_prn_lun_d'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_cmds_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_file_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_file_trc_lun_d ( lun )
    !
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden
    INTEGER , INTENT(IN) :: lun !
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_file_trc_lun_d'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_cmds_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_file_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE new_file_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(INOUT) :: this !
    !
    this%unit      = -1
    this%path      = REPEAT (' ', LEN(this%path))
    this%name      = REPEAT (' ', LEN(this%name))
    this%status    = REPEAT (' ', LEN(this%status))
    this%access    = REPEAT (' ', LEN(this%access))
    this%form      = REPEAT (' ', LEN(this%form))
    this%position  = REPEAT (' ', LEN(this%position))
    this%action    = REPEAT (' ', LEN(this%action))
    this%delim     = REPEAT (' ', LEN(this%delim))
    this%type      = REPEAT (' ', LEN(this%type))
    !
    this%status(1:7)   = 'UNKNOWN'
    this%access(1:10)  = 'SEQUENTIAL'
    this%form(1:9)     = 'FORMATTED'
    this%recl          = HUGE(this%recl)
    this%position(1:4) = 'ASIS'
    this%action(1:9)   = 'READWRITE'
    this%delim(1:4)    = 'NONE'
    this%type(1:7)     = 'UNKNOWN'
    !
  END SUBROUTINE new_file_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE new_file_1 ( this )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file) , INTENT(INOUT) :: this(:) !
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='new_file_1' !
    !! Z&auml;hler
    INTEGER                       :: i !
    !
    DO i=1,SIZE(this)
       CALL new_file_0 ( this(i) )
    END DO
    !
  END SUBROUTINE new_file_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE kill_file_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(INOUT) :: this !
    !
    IF ( no_error( ) ) CALL new_file_0 ( this )
    !
  END SUBROUTINE kill_file_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE kill_file_1 ( this )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file) , INTENT(INOUT) :: this(:) !
    !! Z&auml;hler
    INTEGER                       :: i !
    !
    DO i=1,SIZE(this)
       CALL kill_file_0 ( this(i) )
    END DO
    !
  END SUBROUTINE kill_file_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_file_0 ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=9), PARAMETER :: c_upname='ok_file_0'
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp+1) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok( 1) = ok_file_unit              ( this )
       l_ok( 2) = ok_file_path              ( this )
       l_ok( 3) = ok_file_name              ( this )
       l_ok( 4) = ok_file_status            ( this )
       l_ok( 5) = ok_file_access            ( this )
       l_ok( 6) = ok_file_form              ( this )
       l_ok( 7) = ok_file_recl              ( this )
       l_ok( 8) = ok_file_position          ( this )
       l_ok( 9) = ok_file_action            ( this )
       l_ok(10) = ok_file_delim             ( this )
       l_ok(11) = ok_file_type              ( this )
       l_ok(12) = ok_file_path_and_name_len ( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_file_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_file_1 ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=9), PARAMETER :: c_upname='ok_file_1'
    !! Z&auml;hler
    INTEGER :: i !
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_file_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_file_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='print_file_0'
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     &
           IOSTAT  = stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
       IF ( no_error( ) ) CALL print_file_unit    ( this )
       IF ( no_error( ) ) CALL print_file_path    ( this )
       IF ( no_error( ) ) CALL print_file_name    ( this )
       IF ( no_error( ) ) CALL print_file_status  ( this )
       IF ( no_error( ) ) CALL print_file_access  ( this )
       IF ( no_error( ) ) CALL print_file_form    ( this )
       IF ( no_error( ) ) CALL print_file_recl    ( this )
       IF ( no_error( ) ) CALL print_file_position( this )
       IF ( no_error( ) ) CALL print_file_action  ( this )
       IF ( no_error( ) ) CALL print_file_delim   ( this )
       IF ( no_error( ) ) CALL print_file_type    ( this )
       IF ( no_error( ) ) THEN
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8001,     &
                 IOSTAT  = stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
             CALL setup_error_act ( '<name>', this%name )
          END IF
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT('# Beginn Objekt t_file ------------------------------')
8001 FORMAT('# Ende   Objekt t_file ------------------------------')
    !
  END SUBROUTINE print_file_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_1 ( this )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='print_file_1'
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
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     &
                 IOSTAT  = stat ) i
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
             CALL setup_error_act ( '<name>', this(i)%name )
          END IF
          IF ( no_error( ) ) CALL print_file_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_file_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_static_d ( )
    !
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_file_static_d'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     &
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, c_file_path_len, &
           c_file_name_len, c_file_path_and_name_len, c_file_status_len,           &
           c_file_access_len, c_file_form_len, c_file_position_len,                &
           c_file_action_len, c_file_delim_len, c_file_type_len,                   &
           c_file_compr_ext_len
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_file_all_errors_d ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_file         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#              initialised = ',L1,/ &
    '#                   prn_op = ',L1,/ &
    '#                   trc_op = ',L1,/ &
    '#                  prn_lun = ',I5,/ &
    '#                  trc_lun = ',I5,/ &
    '#                   n_init = ',I5,/ &
    '#          c_file_path_len = ',I5,/ &
    '#          c_file_name_len = ',I5,/ &
    '# c_file_path_and_name_len = ',I5,/ &
    '#        c_file_status_len = ',I5,/ &
    '#        c_file_access_len = ',I5,/ &
    '#          c_file_form_len = ',I5,/ &
    '#      c_file_position_len = ',I5,/ &
    '#        c_file_action_len = ',I5,/ &
    '#         c_file_delim_len = ',I5,/ &
    '#          c_file_type_len = ',I5,/ &
    '#     c_file_compr_ext_len = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------')
    !
  END SUBROUTINE print_file_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_all_errors_d ( )
    !
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_file_all_errors_d'
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_file_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8099 bis  8499]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte SET-Routinen bitte unbedingt loeschen <----------
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "unit" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_unit_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "unit"
    INTEGER       , INTENT(IN)  :: val  !
    !
    this%unit = val
    !
  END SUBROUTINE set_file_unit_0_0
  !
  !! weise der Komponente "unit" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_unit_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file) , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "unit"
    INTEGER       , INTENT(IN)  :: val     !
    !
    this(:)%unit = val
    !
  END SUBROUTINE set_file_unit_1_0
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_name_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*), INTENT(IN)  :: val    ! 
    !
    this%name = REPEAT( ' ', LEN(this%name))
    IF ( LEN_TRIM(val) > 0 ) this%name = val(1:MIN(LEN_TRIM(val),LEN(this%name)))
    !
  END SUBROUTINE set_file_name_0_0
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_name_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*), INTENT(IN)  :: val       ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_name_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_name_1_0
  !
  !! weise der Komponente "path" einen skalaren Wert zu (Skalar) <BR>
  !! der Pathdelimiter muss am Ende des Namens enthalten sein    <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_path_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "path"
    CHARACTER (LEN=*), INTENT(IN)  :: val    ! 
    !
    this%path = REPEAT( ' ', LEN(this%path))
    IF ( LEN_TRIM(val) > 0 ) this%path = val(1:MIN(LEN_TRIM(val),LEN(this%path)))
    !
  END SUBROUTINE set_file_path_0_0
  !
  !! weise der Komponente "path" einen skalaren Wert zu (Vektor) <BR>
  !! der Pathdelimiter muss am Ende des Namens enthalten sein    <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_path_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "path"
    CHARACTER (LEN=*), INTENT(IN)  :: val       ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_path_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_path_1_0
  !
  !! weise den Komponenten "path" und "name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_path_and_name_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this  !
    !! Wert f&uuml;r Komponenten "path" und "name"
    CHARACTER (LEN=*), INTENT(IN)  :: val     ! 
    !! Hilfsvariablen
    CHARACTER (LEN=c_file_path_len) :: l_path ! 
    CHARACTER (LEN=c_file_name_len) :: l_name ! 
    INTEGER                         :: idx    ! 
    !
    l_path = REPEAT( ' ', LEN(l_path) )
    l_name = REPEAT( ' ', LEN(l_name) )
    idx    = get_file_pathdelimiter_idx ( val )
    IF ( idx > 0 ) THEN
       l_path = val(1:MIN(idx,LEN(l_path)))
       l_name = val(idx+1:idx+MIN(LEN_TRIM(val)-idx,LEN(l_name)))
       CALL set_file_path_0_0 ( this, l_path )
    ELSE
       l_name = val(1:MIN(LEN_TRIM(val),LEN(l_name)))
    END IF
    CALL set_file_name_0_0 ( this, l_name )
    !
  END SUBROUTINE set_file_path_and_name_0_0
  !
  !! weise der Komponente "path_and_name" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_path_and_name_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "path_and_name"
    CHARACTER (LEN=*), INTENT(IN)  :: val       ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_path_and_name_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_path_and_name_1_0
  !
  !! weise der Komponente "status" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_status_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "status"
    CHARACTER (LEN=*), INTENT(IN)  :: val  !
    !
    this%status = REPEAT( ' ', LEN(this%status) )
    IF ( LEN_TRIM(val) > 0 ) this%status = val(1:MIN(LEN_TRIM(val),LEN(this%status)))
    !
  END SUBROUTINE set_file_status_0_0
  !
  !! weise der Komponente "status" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_status_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "status"
    CHARACTER (LEN=*), INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_status_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_status_1_0
  !
  !! weise der Komponente "access" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_access_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "access"
    CHARACTER (LEN=*), INTENT(IN)  :: val  !
    !
    this%access = REPEAT( ' ', LEN(this%access) )
    IF ( LEN_TRIM(val) > 0 ) this%access = val(1:MIN(LEN_TRIM(val),LEN(this%access)))
    !
  END SUBROUTINE set_file_access_0_0
  !
  !! weise der Komponente "access" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_access_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "access"
    CHARACTER (LEN=*), INTENT(IN)  :: val     !
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_access_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_access_1_0
  !
  !! weise der Komponente "form" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_form_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "form"
    CHARACTER (LEN=*), INTENT(IN)  :: val  !
    !
    this%form = REPEAT( ' ', LEN(this%form) )
    IF ( LEN_TRIM(val) > 0 ) this%form = val(1:MIN(LEN_TRIM(val),LEN(this%form)))
    !
  END SUBROUTINE set_file_form_0_0
  !
  !! weise der Komponente "form" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_form_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "form"
    CHARACTER (LEN=*), INTENT(IN)  :: val     !
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_form_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_form_1_0
  !
  !! weise der Komponente "recl" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_recl_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file), INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "recl"
    INTEGER      , INTENT(IN)  :: val  !
    !
    this%recl = val
    !
  END SUBROUTINE set_file_recl_0_0
  !
  !! weise der Komponente "recl" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_recl_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file), INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "recl"
    INTEGER      , INTENT(IN)  :: val     !
    !
    this(:)%recl = val
    !
  END SUBROUTINE set_file_recl_1_0
  !
  !! weise der Komponente "position" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_position_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "position"
    CHARACTER (LEN=*), INTENT(IN)  :: val  !
    !
    this%position = REPEAT( ' ', LEN(this%position) )
    IF ( LEN_TRIM(val) > 0 ) this%position = val(1:MIN(LEN_TRIM(val),LEN(this%position)))
    !
  END SUBROUTINE set_file_position_0_0
  !
  !! weise der Komponente "position" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_position_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "position"
    CHARACTER (LEN=*), INTENT(IN)  :: val     !
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO I=1,SIZE(this)
       CALL set_file_position_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_position_1_0
  !
  !! weise der Komponente "action" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_action_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "action"
    CHARACTER (LEN=*), INTENT(IN)  :: val  ! 
    !
    this%action = REPEAT( ' ', LEN(this%action) )
    IF ( LEN_TRIM(val) > 0 ) this%action = val(1:MIN(LEN_TRIM(val),LEN(this%action)))
    !
  END SUBROUTINE set_file_action_0_0
  !
  !! weise der Komponente "action" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_action_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "action"
    CHARACTER (LEN=*), INTENT(IN)  :: val     !
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_action_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_action_1_0
  !
  !! weise der Komponente "delim" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_delim_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "delim"
    CHARACTER (LEN=*), INTENT(IN)  :: val  !
    !
    this%delim = REPEAT( ' ', LEN(this%delim) )
    IF ( LEN_TRIM(val) > 0 ) this%delim = val(1:MIN(LEN_TRIM(val),LEN(this%delim)))
    !
  END SUBROUTINE set_file_delim_0_0
  !
  !! weise der Komponente "delim" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_delim_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "delim"
    CHARACTER (LEN=*), INTENT(IN)  :: val     !
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_delim_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_delim_1_0
  !
  !! Weise der Komponente "type" einen skalaren Wert zu (Skalar). <BR>
  !! Die Dateitypbezeichnung wird intern in Grossbuchstaben umgesetzt. <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_type_0_0 ( this, val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(INOUT) :: this !
    !! Wert f&uuml;r Komponente "type"
    CHARACTER (LEN=*), INTENT(IN)  :: val  !
    !
    this%type = REPEAT( ' ', LEN(this%type) )
    IF ( LEN_TRIM(val) > 0 ) this%type = val(1:MIN(LEN_TRIM(val),LEN(this%type)))
    !
  END SUBROUTINE set_file_type_0_0
  !
  !! weise der Komponente "type" einen skalaren Wert zu (Vektor). <BR>
  !! Die Dateitypbezeichnung wird intern in Grossbuchstaben umgesetzt. <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_file_type_1_0 ( this, val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(INOUT) :: this(:) !
    !! Wert f&uuml;r Komponente "type"
    CHARACTER (LEN=*), INTENT(IN)  :: val     !
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_file_type_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_file_type_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte GET-Routinen bitte unbedingt loeschen <----------
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "unit" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_unit_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "unit" (Skalar)
    INTEGER                     :: val  ! 
    !
    val = this%unit
    !
  END FUNCTION get_file_unit_0_0
  !
  !! hole die Komponente "unit" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_unit_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file) , INTENT(IN)  :: this(:) !
    !! R&uuml;ckgabewert "unit"
    INTEGER                     :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%unit
    !
  END FUNCTION get_file_unit_1_0
  !
  !! hole die Komponente "path" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_path_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)     , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "path" (Skalar)
    CHARACTER (len=c_file_path_len) :: val  ! 
    !
    val = this%path
    !
  END FUNCTION get_file_path_0_0
  !
  !! hole die Komponente "path" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_path_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)     , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "path"
    CHARACTER (len=c_file_path_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%path
    !
  END FUNCTION get_file_path_1_0
  !
  !! hole die Komponente "name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_name_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)     , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "name" (Skalar)
    CHARACTER (len=c_file_name_len) :: val  ! 
    !
    val = this%name
    !
  END FUNCTION get_file_name_0_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_name_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)     , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "name"
    CHARACTER (len=c_file_name_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%name
    !
  END FUNCTION get_file_name_1_0
  !
  !! hole die Komponenten "path" und "name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_path_and_name_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)              , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "path_and_name" (Skalar)
    CHARACTER (LEN=c_file_path_and_name_len) :: val  ! 
    !! Hilfsvariable
    INTEGER :: n ! 
    !
    val = REPEAT( ' ', LEN(val) )
    IF ( LEN_TRIM(this%path) > 0 ) val = this%path(1:MIN(LEN_TRIM(this%path),LEN(val)))
    n   = LEN_TRIM(val)
    IF ( LEN_TRIM(this%name) > 0 ) val(n+1:) = this%name(1:MIN(LEN_TRIM(this%name),LEN(val)-n))
    !
  END FUNCTION get_file_path_and_name_0_0
  !
  !! hole die Komponente "path" und "name" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_path_and_name_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)              , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "path_and_name"
    CHARACTER (len=c_file_path_and_name_len) :: val(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = get_file_path_and_name_0_0 ( this(i) )
    END DO
    !
  END FUNCTION get_file_path_and_name_1_0
  !
  !! hole die Komponente "status" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_status_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)      , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "status" (Skalar)
    CHARACTER (LEN=c_file_status_len) :: val  ! 
    !
    val = this%status
    !
  END FUNCTION get_file_status_0_0
  !
  !! hole die Komponente "status" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_status_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)      , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "status"
    CHARACTER (LEN=c_file_status_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%status
    !
  END FUNCTION get_file_status_1_0
  !
  !! hole die Komponente "access" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_access_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)      , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "access" (S1kalar)
    CHARACTER (LEN=c_file_access_len) :: val  ! 
    !
    val = this%access
    !
  END FUNCTION get_file_access_0_0
  !
  !! hole die Komponente "access" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_access_1_0 ( this ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_file)      , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "access"
    CHARACTER (LEN=c_file_access_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%access
    !
  END FUNCTION get_file_access_1_0
  !
  !! hole die Komponente "form" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_form_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "form" (Skalar)
    CHARACTER (LEN=c_file_form_len) :: val  ! 
    !
    val = this%form
    !
  END FUNCTION get_file_form_0_0
  !
  !! hole die Komponente "form" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_form_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "form"
    CHARACTER (LEN=c_file_form_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%form
    !
  END FUNCTION get_file_form_1_0
  !
  !! hole die Komponente "recl" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_recl_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "recl" (Skalar)
    INTEGER                     :: val  ! 
    !
    val = this%recl
    !
  END FUNCTION get_file_recl_0_0
  !
  !! hole die Komponente "recl" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_recl_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file) , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "recl"
    INTEGER                     :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%recl
    !
  END FUNCTION get_file_recl_1_0
  !
  !! hole die Komponente "position" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_position_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)        , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "position" (Skalar)
    CHARACTER (LEN=c_file_position_len) :: val  ! 
    !
    val = this%position
    !
  END FUNCTION get_file_position_0_0
  !
  !! hole die Komponente "position" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_position_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)        , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "position"
    CHARACTER (LEN=c_file_position_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%position
    !
  END FUNCTION get_file_position_1_0
  !
  !! hole die Komponente "action" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_action_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)      , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "action" (Skalar)
    CHARACTER (LEN=c_file_action_len) :: val  ! 
    !
    val = this%action
    !
  END FUNCTION get_file_action_0_0
  !
  !! hole die Komponente "action" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_action_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)      , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "action"
    CHARACTER (LEN=c_file_action_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%action
    !
  END FUNCTION get_file_action_1_0
  !
  !! hole die Komponente "delim" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_delim_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)     , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "delim" (Skalar)
    CHARACTER (LEN=c_file_delim_len) :: val  ! 
    !
    val = this%delim
    !
  END FUNCTION get_file_delim_0_0
  !
  !! hole die Komponente "delim" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_delim_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)     , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "delim"
    CHARACTER (LEN=c_file_delim_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%delim
    !
  END FUNCTION get_file_delim_1_0
  !
  !! hole die Komponente "type" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_type_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)    , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "type" (Skalar)
    CHARACTER (LEN=c_file_type_len) :: val  ! 
    !
    val = this%type
    !
  END FUNCTION get_file_type_0_0
  !
  !! hole die Komponente "type" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_type_1_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Vektor)
    TYPE (t_file)    , INTENT(IN)  :: this(:)         !
    !! R&uuml;ckgabewert "type"
    CHARACTER (LEN=c_file_type_len) :: val(SIZE(this)) ! 
    !
    val(:) = this(:)%type
    !
  END FUNCTION get_file_type_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_file_0_0 ( this1, this2 ) &
         RESULT( ok )
    !
    !! Objekt 1 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this1 !
    !! Objekt 2 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this2 !
    !! Testergebnis (Skalar)
    LOGICAL :: ok              !
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    ! Hinweis: sollten die zum Test der Gleichheit "==" notwendigen
    ! Operationen sehr einfach und kurz sein, so kann ggf.
    ! auf die nachfolgend gerufenen FUNCTIONs verzichtet werden.
    !
    l_ok( 1) = ( this1%unit      == this2%unit )
    l_ok( 2) = ( this1%path      == this2%path )
    l_ok( 3) = ( this1%name      == this2%name )
    l_ok( 4) = ( this1%status    == this2%status )
    l_ok( 5) = ( this1%access    == this2%access )
    l_ok( 6) = ( this1%form      == this2%form )
    l_ok( 7) = ( this1%recl      == this2%recl )
    l_ok( 8) = ( this1%position  == this2%position )
    l_ok( 9) = ( this1%action    == this2%action )
    l_ok(10) = ( this1%delim     == this2%delim )
    l_ok(11) = ( this1%type      == this2%type )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_file_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_file_1_0 ( this1, this2 ) &
         RESULT( ok )
    !
    !! Objekt 1 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this1(:) !
    !! Objekt 2 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this2    !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) !
    !! Z&auml;hler
    INTEGER :: i !
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_file_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_file_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_file_0_1 ( this1, this2 ) &
         RESULT( ok )
    !
    !! Objekt 1 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this1    !
    !! Objekt 2 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this2(:) !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) !
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_file_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_file_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_file_1_1 ( this1, this2 ) &
         RESULT( ok )
    !
    !! Objekt 1 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this1(:) !
    !! Objekt 2 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this2(:) !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) !
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_file_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_file_1_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf der Basis der Komponenten "path" und
  !! "name" auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_file_path_and_name_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this1 !
    !! Objekt 2 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this2 !
    !! Testergebnis (Vektor)
    LOGICAL :: ok ! 
    ! Hilfsvariable
    LOGICAL :: l_ok(2) ! 
    !
    l_ok(1) = ( this1%path == this2%path )
    l_ok(2) = ( this1%name == this2%name )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_file_path_and_name_0_0
  !  
  !! pr&uuml;fe Datenobjekte auf der Basis der Komponenten "path" und
  !! "name" auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_file_path_and_name_1_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this1(:) !
    !! Objekt 2 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this2    !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_file_path_and_name( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_file_path_and_name_1_0
  !
  !! pr&uuml;fe Datenobjekte auf der Basis der Komponenten "path" und
  !! "name" auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_file_path_and_name_0_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this1    !
    !! Objekt 2 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this2(:) !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_file_path_and_name( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_file_path_and_name_0_1
  !
  !! pr&uuml;fe Datenobjekte auf der Basis der Komponenten "path" und
  !! "name" auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_file_path_and_name_1_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this1(:) !
    !! Objekt 2 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this2(:) !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_file_path_and_name( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_file_path_and_name_1_1
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
  FUNCTION ne_file_0_0 ( this1, this2 ) &
       RESULT( ok )
    !
    !! Objekt 1 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this1 !
    !! Objekt 2 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this2 !
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( .NOT. eq_file_0_0 ( this1, this2 ) )
    !
  END FUNCTION ne_file_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_file_1_0 ( this1, this2 ) &
         RESULT( ok )
    !
    !! Objekt 1 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this1(:) !
    !! Objekt 2 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this2    !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) !
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_file_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ne_file_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
 FUNCTION ne_file_0_1 ( this1, this2 ) &
         RESULT( ok )
    !
    !! Objekt 1 (Skalar)
    TYPE (t_file) , INTENT(IN) :: this1    !
    !! Objekt 2 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this2(:) !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) !
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_file_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ne_file_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_file_1_1 ( this1, this2 ) &
         RESULT( ok )
    !
    !! Objekt 1 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this1(:) !
    !! Objekt 2 (Vektor)
    TYPE (t_file) , INTENT(IN) :: this2(:) !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) !
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_file_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ne_file_1_1
  !
  ! ----------------------------------------------------------------------
  !! Existiert die Kanalnummer oder nicht ?
  ! ----------------------------------------------------------------------
  !
  FUNCTION unit_exists_0 ( this ) &
       RESULT( unit_ex )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Kanalnummer existiert
    LOGICAL :: unit_ex ! 
    !
    IF ( ok_file_unit (this) ) THEN
       INQUIRE ( UNIT=this%unit, EXIST=unit_ex )
    ELSE
       unit_ex = .FALSE.
       CALL clear_error_act ( )
    ENDIF
    !
  END FUNCTION unit_exists_0
  !
  FUNCTION unit_exists_1 ( this ) &
       RESULT( unit_ex )
    !
    !! Objekt  (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Kanalnummer existiert
    LOGICAL :: unit_ex(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i=1,SIZE(this)
       unit_ex(i) = unit_exists_0(this(i))
    ENDDO
    !
  END FUNCTION unit_exists_1
  !
  FUNCTION unit_exists_not_0 ( this ) &
       RESULT( unit_ex_not )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Kanalnummer nicht existiert
    LOGICAL :: unit_ex_not  !
    !
    unit_ex_not = ( .NOT. unit_exists_0( this ) )
    !
  END FUNCTION unit_exists_not_0
  !
  FUNCTION unit_exists_not_1 ( this ) &
       RESULT( unit_ex_not )
    !
    !! Objekt  (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Kanalnummer existiert
    LOGICAL :: unit_ex_not(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       unit_ex_not(i) = unit_exists_not_0(this(i))
    ENDDO
    !
  END FUNCTION unit_exists_not_1
  !
  ! ----------------------------------------------------------------------
  !! Ist die Datei ge&ouml;ffnet oder nicht ?
  ! ----------------------------------------------------------------------
  !
  FUNCTION unit_is_opened_0 ( this ) &
       RESULT( unit_op )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Datei geoffnet
    LOGICAL :: unit_op ! 
    !
    IF ( ok_file_unit (this) ) THEN
       INQUIRE ( UNIT=this%unit, OPENED=unit_op )
    ELSE
       unit_op = .FALSE.
       CALL clear_error_act( )
    ENDIF
    !
  END FUNCTION unit_is_opened_0
  !
  FUNCTION unit_is_opened_1 ( this ) &
       RESULT( unit_op )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Datei geoffnet
    LOGICAL :: unit_op(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
          unit_op(i) = unit_is_opened_0(this(i))
    ENDDO
    !
  END FUNCTION unit_is_opened_1
  !
  FUNCTION unit_is_closed_0 ( this ) &
       RESULT( unit_cl )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Datei geschlossen
    LOGICAL :: unit_op, unit_cl
    !
    unit_cl = ( .NOT. unit_is_opened_0(this) )
    !
  END FUNCTION unit_is_closed_0
  !
  FUNCTION unit_is_closed_1 ( this ) &
       RESULT( unit_cl )
    !
    !! Objekt  (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Kanalnummer existiert
    LOGICAL :: unit_cl(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       unit_cl(i) = unit_is_closed_0(this(i))
    ENDDO
    !
  END FUNCTION unit_is_closed_1
  !
  ! ----------------------------------------------------------------------
  !! Ist der Dateiname mit 'NONE' belegt oder nicht ?
  ! ----------------------------------------------------------------------
  !
  FUNCTION file_is_none_0 ( this ) &
       RESULT( name_none )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Dateiname = NONE
    LOGICAL :: name_none
    !
    name_none = MERGE( .true., .false., ( this%name(1:4)=='NONE' .OR. this%name(1:4)=='none' ) )
    !
  END FUNCTION file_is_none_0
  !
  FUNCTION file_is_none_1 ( this ) &
       RESULT( name_none )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Dateiname = NONE
    LOGICAL :: name_none(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       name_none(i) = file_is_none_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_none_1
  !
  ! ----------------------------------------------------------------------
  !! Ist die Datei komprimiert oder nicht ?
  ! ----------------------------------------------------------------------
  !
  FUNCTION file_is_compressed_0 ( this ) &
       RESULT( file_compr )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Datei komprimiert
    LOGICAL :: file_compr
    !! L&auml;nge des Dateinamens
    INTEGER :: name_len
    !
    file_compr = .false.
    name_len   = LEN_TRIM(this%name)
    !
    IF ( (name_len > 4 .AND. this%name(name_len-3:) == '.zip')   .OR.   &
         (name_len > 3 .AND. this%name(name_len-2:) == '.gz' )   .OR.   &
         (name_len > 2 .AND. this%name(name_len-1:) == '.Z'  ))   file_compr = .true.
    !
  END FUNCTION file_is_compressed_0
  !
  FUNCTION file_is_compressed_1 ( this ) &
       RESULT( file_compr )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Datei geoffnet
    LOGICAL :: file_compr(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_compr(i) = file_is_compressed_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_compressed_1
  !
  FUNCTION file_is_uncompressed_0 ( this ) &
       RESULT( file_uncompr )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Datei unkomprimiert
    LOGICAL :: file_uncompr ! 
    !
    file_uncompr = ( .NOT. file_is_compressed_0(this) )
    !
  END FUNCTION file_is_uncompressed_0
  !
  FUNCTION file_is_uncompressed_1 ( this ) &
       RESULT( file_uncompr )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Datei unkomprimiert
    LOGICAL :: file_uncompr(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_uncompr(i) = .NOT. file_is_compressed_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_uncompressed_1
  !
  ! ----------------------------------------------------------------------
  !! Existiert der Dateiname oder nicht ?
  ! ----------------------------------------------------------------------
  !
  FUNCTION file_exists_0 ( this ) &
       RESULT( file_ex )
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Dateiname existiert
    LOGICAL :: file_ex ! 
    !! Hilfsvariable 
    CHARACTER (LEN=c_file_path_and_name_len) :: l_pn ! 
    !
    IF ( ok_file_path_and_name_len( this ) ) THEN
       l_pn = get_file_path_and_name ( this )
       INQUIRE ( FILE=l_pn(1:LEN_TRIM(l_pn)), EXIST=file_ex )
    ELSE
       file_ex = .false.
    END IF
    !
  END FUNCTION file_exists_0
  !
  FUNCTION file_exists_1 ( this ) &
       RESULT( file_ex )
    !
    !! Objekt  (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Kanalnummer existiert
    LOGICAL :: file_ex(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_ex(i) = file_exists_0(this(i))
    ENDDO
    !
  END FUNCTION file_exists_1
  !
  FUNCTION file_exists_not_0 ( this ) &
       RESULT( file_ex_not )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Dateiname nicht existiert
    LOGICAL :: file_ex_not ! 
    !
    file_ex_not = ( .NOT. file_exists_0 ( this ) )
    !
  END FUNCTION file_exists_not_0
  !
  FUNCTION file_exists_not_1 ( this ) &
       RESULT( file_ex_not )
    !
    !! Objekt  (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Dateiname existiert
    LOGICAL :: file_ex_not(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_ex_not(i) = file_exists_not_0(this(i))
    ENDDO
    !
  END FUNCTION file_exists_not_1
  !
  ! ----------------------------------------------------------------------
  !! Ist die Datei ge&ouml;ffnet oder nicht ?
  ! ----------------------------------------------------------------------
  !
  FUNCTION file_is_opened_0 ( this ) &
       RESULT( file_op )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Datei ge&ouml;ffnet
    LOGICAL :: file_op ! 
    !! Dateiname, ggf. ohne die Kompressions-Endung
    CHARACTER (len=c_file_path_and_name_len) :: l_pn ! 
    !
    IF ( ok_file_path_and_name_len( this ) ) THEN
       l_pn = get_file_path_and_name_unc ( this )
       INQUIRE ( FILE=l_pn(1:LEN_TRIM(l_pn)), OPENED=file_op )
    ELSE
       file_op = .false.
    END IF
    !
  END FUNCTION file_is_opened_0
  !
  FUNCTION file_is_opened_1 &
       ( this ) &
       RESULT( file_op )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Datei ge&ouml;ffnet
    LOGICAL :: file_op(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_op(i) = file_is_opened_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_opened_1
  !
  FUNCTION file_is_closed_0 ( this ) &
       RESULT( file_cl )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Datei geschlossen
    LOGICAL :: file_cl
    !
    file_cl = ( .NOT. file_is_opened_0 (this) )
    !
  END FUNCTION file_is_closed_0
  !
  FUNCTION file_is_closed_1 ( this ) &
       RESULT( file_cl )
    !
    !! Objekt  (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Datei geschlossen
    LOGICAL :: file_cl(SIZE(this))
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_cl(i) = file_is_closed_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_closed_1
  !
  ! ----------------------------------------------------------------------
  !! Ist die Datei formatiert oder nicht ?
  ! ----------------------------------------------------------------------
  !
  ! Datentyp t_file wird uebergeben
  FUNCTION file_is_formatted_0 ( this ) &
       RESULT( file_form )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Datei formatiert
    LOGICAL :: file_form
    !
    file_form = MERGE( .true., .false., this%form(1:9) == 'FORMATTED' )
    !
  END FUNCTION file_is_formatted_0
  !
  ! Datentyp t_file wird uebergeben
  FUNCTION file_is_formatted_1 ( this ) &
       RESULT( file_form )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Dateien formatiert
    LOGICAL :: file_form(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_form(i) = file_is_formatted_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_formatted_1
  !
  ! Komponente "form" wird uebergeben
  FUNCTION file_is_formatted_f0 ( fileformat ) &
       RESULT( file_form )
    !
    !! Objekt (Skalar)
    CHARACTER(LEN=*), INTENT(IN) :: fileformat
    !! log.Wert, ob Datei formatiert
    LOGICAL :: file_form ! 
    !
    file_form = MERGE( .true., .false., TRIM(fileformat) == 'FORMATTED' )
    !
  END FUNCTION file_is_formatted_f0
  !
  ! Komponente "form" wird uebergeben
  FUNCTION file_is_formatted_f1 ( fileformat ) &
       RESULT( file_form )
    !
    !! Objekt (Skalar)
    CHARACTER(LEN=*), INTENT(IN) :: fileformat(:) ! 
    !! log.Wert, ob alle Dateien formatiert
    LOGICAL :: file_form(SIZE(fileformat)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(fileformat)
       file_form(i) = file_is_formatted_f0(fileformat(i))
    ENDDO
    !
  END FUNCTION file_is_formatted_f1
  !
  FUNCTION file_is_unformatted_0 ( this ) &
       RESULT( file_unform )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Datei unformatiert
    LOGICAL :: file_unform
    !
    file_unform = MERGE( .true., .false., this%form(1:11) == 'UNFORMATTED' )
    !
  END FUNCTION file_is_unformatted_0
  !
  FUNCTION file_is_unformatted_1 ( this ) &
       RESULT( file_unform )
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Dateien unformatiert
    LOGICAL :: file_unform(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       file_unform(i) = file_is_unformatted_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_unformatted_1
  !
  ! Komponente "form" wird uebergeben
  FUNCTION file_is_unformatted_f0 ( fileformat ) &
       RESULT( file_unform )
    !
    !! Objekt (Skalar)
    CHARACTER(LEN=*), INTENT(IN) :: fileformat ! 
    !! log.Wert, ob Datei unformatiert
    LOGICAL :: file_unform
    !
    file_unform = MERGE( .true., .false., TRIM(fileformat) == 'UNFORMATTED' ) 
    !
  END FUNCTION file_is_unformatted_f0
  !
  ! Komponente "form" wird uebergeben
  FUNCTION file_is_unformatted_f1 ( fileformat ) &
       RESULT( file_unform )
    !
    !! Objekt (Skalar)
    CHARACTER(LEN=*), INTENT(IN) :: fileformat(:) ! 
    !! log.Wert, ob die Dateien unformatiert
    LOGICAL :: file_unform(SIZE(fileformat)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(fileformat)
       file_unform(i) = file_is_unformatted_f0(fileformat(i))
    ENDDO
    !
  END FUNCTION file_is_unformatted_f1
  !
  ! ----------------------------------------------------------------------
  !! Ist der Zugriff sequentiell oder direkt ?
  ! ----------------------------------------------------------------------
  !
  FUNCTION file_is_sequential_0 ( this ) &
       RESULT( file_seq )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Zugriff sequentiell
    LOGICAL :: file_seq
    !
    file_seq = MERGE( .true., .false., this%access(1:10) == 'SEQUENTIAL' )
    !
  END FUNCTION file_is_sequential_0
  !
  FUNCTION file_is_sequential_1 ( this ) &
       RESULT( file_seq )
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Zugriff sequentiell
    LOGICAL :: file_seq(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_seq(i) = file_is_sequential_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_sequential_1
  !
  ! Komponente "access" wird uebergeben
  FUNCTION file_is_sequential_a0 ( fileaccess ) &
       RESULT( file_acc )
    !
    !! Objekt (Skalar)
    CHARACTER(LEN=*), INTENT(IN) :: fileaccess ! 
    !! log.Wert, ob Zugriff sequentiell
    LOGICAL :: file_acc
    !
    file_acc = MERGE( .true., .false., TRIM(fileaccess) == 'SEQUENTIAL' )
    !
  END FUNCTION file_is_sequential_a0
  !
  ! Komponenten "access" werden uebergeben
  FUNCTION file_is_sequential_a1 ( fileaccess ) &
       RESULT( file_acc )
    !
    !! Objekt (Skalar)
    CHARACTER(LEN=*), INTENT(IN) :: fileaccess(:) ! 
    !! log.Wert, ob Zugriff f&uuml;r die Dateien sequentiell
    LOGICAL :: file_acc(SIZE(fileaccess)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(fileaccess)
       file_acc(i) = file_is_sequential_a0(fileaccess(i))
    ENDDO
    !
  END FUNCTION file_is_sequential_a1
  !
  FUNCTION file_is_direct_0 ( this ) &
       RESULT( file_direct )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Zugriff direkt
    LOGICAL :: file_direct ! 
    !
    file_direct = MERGE( .true., .false., this%access(1:6) == 'DIRECT' )
    !
  END FUNCTION file_is_direct_0
  !
  FUNCTION file_is_direct_1 ( this ) &
       RESULT( file_direct )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! log.Wert, ob Zugriff direct
    LOGICAL :: file_direct(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(this)
       file_direct(i) = file_is_direct_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_direct_1
  !
  ! Komponente "access" wird uebergeben
  FUNCTION file_is_direct_a0 ( fileaccess ) &
       RESULT( file_acc )
    !
    !! Objekt (Skalar)
    CHARACTER(LEN=*), INTENT(IN) :: fileaccess ! 
    !! log.Wert, ob Zugriff direkt
    LOGICAL :: file_acc ! 
    !
    file_acc = MERGE( .true., .false., TRIM(fileaccess) == 'DIRECT' )
    !
  END FUNCTION file_is_direct_a0
  !
  ! Komponenten "access" werden uebergeben
  FUNCTION file_is_direct_a1 ( fileaccess ) &
       RESULT( file_acc )
    !
    !! Objekt (Skalar)
    CHARACTER(LEN=*), INTENT(IN) :: fileaccess(:) ! 
    !! log.Wert, ob Zugriff f&uuml;r die Dateien direkt 
    LOGICAL :: file_acc(SIZE(fileaccess)) ! 
    !! Z&auml;hler
    INTEGER :: i !
    !
    DO i = 1, SIZE(fileaccess)
       file_acc(i) = file_is_direct_a0(fileaccess(i))
    ENDDO
    !
  END FUNCTION file_is_direct_a1
  !
  ! ----------------------------------------------------------------------
  !! Ist die Dateiendung ASCII oder binaer ?
  ! ----------------------------------------------------------------------
  !
  FUNCTION file_is_ascii_0 ( this ) &
       RESULT( file_ascii )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this
    !! log.Wert, ob Endung ASCII ist
    LOGICAL :: file_ascii ! 
    !! Dateiname, ggf. ohne die Kompressions-Endung
    CHARACTER (len=c_file_name_len) :: uncompressed_name ! 
    !! Laenge des Dateinames, ggf. ohne die Kompressions-Endung
    INTEGER            :: uncompressed_name_len ! 
    !! Laenge einer definierten ASCII-Dateiendung
    INTEGER            :: c_fileascii_len ! 
    !! ASCII-Endungs-Zaehler
    INTEGER            :: iascii ! 
    !
    file_ascii = .false.
    !
    IF ( .NOT. file_is_none_0(this) ) THEN
       uncompressed_name     = get_file_uncompressed_name_0(this)
       uncompressed_name_len = get_file_uncompressed_name_len0(this)
       ! Dateiendung ist ASCII, wenn eine der in c_fileasciis definierten
       ! Endungen verwendet wird
       iascii = 0
       DO
          iascii = iascii + 1
          IF ( iascii > c_noffileasciis ) EXIT
          c_fileascii_len = LEN_TRIM(c_fileasciis(iascii))
          IF ( TRIM(ADJUSTL(uncompressed_name(uncompressed_name_len-c_fileascii_len+1:))) == &
               TRIM(c_fileasciis(iascii)) ) file_ascii = .true.
          IF ( file_ascii ) EXIT
       ENDDO
    ENDIF
    !
  END FUNCTION file_is_ascii_0
  !
  FUNCTION file_is_ascii_1 ( this ) &
       RESULT( file_ascii )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:)
    !! log.Wert, ob Endung ASCII ist
    LOGICAL :: file_ascii(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       file_ascii(i) = file_is_ascii_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_ascii_1
  !
  FUNCTION file_is_binary_0 ( this ) &
       RESULT( file_binary )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this
    !! log.Wert, ob Endung binaer ist
    LOGICAL :: file_binary ! 
    !
    file_binary = .false.
    IF ( .NOT. file_is_none_0(this) ) THEN
       file_binary = ( .NOT. file_is_ascii_0(this) )
    ENDIF
    !
  END FUNCTION file_is_binary_0
  !
  FUNCTION file_is_binary_1 ( this ) &
       RESULT( file_binary )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:)
    !! log.Wert, ob Endung binaer ist
    LOGICAL :: file_binary(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       file_binary(i) = file_is_binary_0(this(i))
    ENDDO
    !
  END FUNCTION file_is_binary_1
  !
  ! ----------------------------------------------------------------------
  !! Ist die Datei ok ? ("name" existiert und "status" ist 'OLD')
  !! Funktion erzeugt Fehlermeldungen
  ! ----------------------------------------------------------------------
  !
  FUNCTION ok_file_name_status_0 ( this ) &
       RESULT( file_ok )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this !
    !! log.Wert, ob Zugriff direkt
    LOGICAL :: file_ok, file_ex ! 
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='ok_file_name_status_0'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctext ! 
    !
    file_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( file_exists_not( this ) .AND. this%status(1:3) == 'OLD'    .OR. &
            file_exists( this )     .AND. this%status(1:3) == 'NEW'  ) THEN
          file_ok = .false.
          IF ( file_exists_not( this ) .AND. this%status(1:3) == 'OLD' ) THEN
             ! Fehler setzen
             CALL setup_error_act ( all_errors(:), -1000, c_upname, c_modname )
          ELSEIF ( file_exists( this ) .AND. this%status(1:3) == 'NEW' ) THEN
             ! Fehler setzen
             CALL setup_error_act ( all_errors(:), -1001, c_upname, c_modname )
          ENDIF
          CALL setup_error_act                                               ( &
               '<..path....................................................>', &
               this%path )
          !
          CALL setup_error_act                                               ( &
               '<..name....................................................>', &
               this%name )
          !
          WRITE(ctext,'(I10)') this%unit
          CALL setup_error_act ( &
               '<..unit..>'    , &
               ctext )
          !
          CALL setup_error_act ( &
               '<..status..>'  , &
               this%status )
          !
          file_ex = file_exists(this)
          WRITE(ctext,'(L1,A9)') file_ex,'         '
          CALL setup_error_act ( &
               '<..exist..>'    , &
               ctext )
          !
       ELSE
          file_ok = .true.
       END IF
    ENDIF
    !
  END FUNCTION ok_file_name_status_0
  !
  FUNCTION ok_file_name_status_1 ( this ) &
       RESULT( file_ok )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this(:)
    !! log.Wert, ob Zugriff direkt
    LOGICAL :: file_ok(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: I
    !
    DO i = 1, SIZE(this)
       file_ok(i) = ok_file_name_status_0(this(i))
    END DO
    !
  END FUNCTION ok_file_name_status_1
  !
  ! ----------------------------------------------------------------------
  !! Hole die Komponente "name" ohne evt. Kompressions-Endungen
  ! ----------------------------------------------------------------------
  !
  FUNCTION get_file_uncompressed_name_0 ( this ) &
       RESULT( name_orig )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this
    !! Dateiname ohne evt. Endung .gz, .Z
    CHARACTER (len=c_file_name_len) :: name_orig ! 
    !! L&auml;nge des Dateinamens
    INTEGER :: name_len ! 
    !! Name der Function
    CHARACTER (LEN=28), PARAMETER :: c_upname='get_file_uncompressed_name_0'
    !
    name_orig = REPEAT( ' ', LEN(name_orig) )
    IF ( file_is_compressed(this) ) THEN
       name_len = LEN_TRIM(this%name)
       IF ( this%name(name_len-3:) == '.zip' ) name_orig = this%name(:name_len-4)
       IF ( this%name(name_len-2:) == '.gz'  ) name_orig = this%name(:name_len-3)
       IF ( this%name(name_len-1:) == '.Z'   ) name_orig = this%name(:name_len-2)
    ELSE
       name_orig = this%name
    END IF
    !
  END FUNCTION get_file_uncompressed_name_0
  !
  FUNCTION get_file_uncompressed_name_1 ( this ) &
       RESULT( name_orig )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! Dateiname ohne evt. Endung .gz, .Z
    CHARACTER (len=c_file_name_len) :: name_orig(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       name_orig(i) = get_file_uncompressed_name_0(this(i))
    END DO
    !
  END FUNCTION get_file_uncompressed_name_1
  !
  ! ----------------------------------------------------------------------
  !! Hole L&auml;nge des Pfadnamens
  ! ----------------------------------------------------------------------
  !
  FUNCTION get_file_path_len_0 ( this ) &
       RESULT( path_len )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this
    !! L&auml;nge des Pfadnamens
    INTEGER :: path_len ! 
    !
    path_len = LEN_TRIM(this%path)
    !
  END FUNCTION get_file_path_len_0
  !
  FUNCTION get_file_path_len_1 ( this ) &
       RESULT( path_len )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! L&auml;nge der Dateipathn
    INTEGER :: path_len(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       path_len(i) = get_file_path_len_0(this(i))
    END DO
    !
  END FUNCTION get_file_path_len_1
  !
  ! ----------------------------------------------------------------------
  !! Hole L&auml;nge des Dateinamens
  ! ----------------------------------------------------------------------
  !
  FUNCTION get_file_name_len_0 ( this ) &
       RESULT( name_len )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this
    !! L&auml;nge des Dateinames
    INTEGER :: name_len ! 
    !
    name_len = LEN_TRIM(this%name)
    !
  END FUNCTION get_file_name_len_0
  !
  FUNCTION get_file_name_len_1 ( this ) &
       RESULT( name_len )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! L&auml;nge der Dateinamen
    INTEGER :: name_len(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       name_len(i) = get_file_name_len_0(this(i))
    END DO
    !
  END FUNCTION get_file_name_len_1
  !
  ! ----------------------------------------------------------------------
  !! Hole L&auml;nge des Pfad- und Dateinamens
  ! ----------------------------------------------------------------------
  !
  FUNCTION get_file_path_and_name_len_0 ( this ) &
       RESULT( path_and_name_len )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this
    !! L&auml;nge des Pfadnamens
    INTEGER :: path_and_name_len ! 
    !
    path_and_name_len = get_file_path_len( this ) + get_file_name_len( this )
    !
  END FUNCTION get_file_path_and_name_len_0
  !
  FUNCTION get_file_path_and_name_len_1 ( this ) &
       RESULT( path_and_name_len )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! L&auml;nge der Dateipath_and_namen
    INTEGER :: path_and_name_len(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       path_and_name_len(i) = get_file_path_and_name_len_0(this(i))
    END DO
    !
  END FUNCTION get_file_path_and_name_len_1
  !
  ! ----------------------------------------------------------------------
  !! Hole L&auml;nge des Dateinamens ohne evt. Kompressions-Endungen
  ! ----------------------------------------------------------------------
  !
  FUNCTION get_file_uncompressed_name_len0 ( this ) &
       RESULT( name_len_orig )
    !
    !! Objekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this
    !! L&auml;nge des Dateinamens ohne evt. Endung .gz, .Z
    INTEGER :: name_len_orig ! 
    !! L&auml;nge des Dateinamens
    INTEGER :: name_len ! 
    !
    name_len = LEN_TRIM(this%name)
    IF ( file_is_compressed(this) ) THEN
       IF ( this%name(name_len-3:) == '.zip' ) name_len_orig = name_len-4
       IF ( this%name(name_len-2:) == '.gz'  ) name_len_orig = name_len-3
       IF ( this%name(name_len-1:) == '.Z'   ) name_len_orig = name_len-2
    ELSE
       name_len_orig = name_len
    END IF
    !
  END FUNCTION get_file_uncompressed_name_len0
  !
  FUNCTION get_file_uncompressed_name_len1 ( this ) &
       RESULT( name_len_orig )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! L&auml;nge der Dateinamen ohne evt. Endung .gz, .Z
    INTEGER :: name_len_orig(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       name_len_orig(i) = get_file_uncompressed_name_len0(this(i))
    END DO
    !
  END FUNCTION get_file_uncompressed_name_len1
  !
  ! ----------------------------------------------------------------------
  !! Hole Typnamen des Kompressionsverfahrens
  ! ----------------------------------------------------------------------
  !
  FUNCTION get_file_compression_type_0 ( this ) &
       RESULT( compr_type )
    !
    !! Objekt (Skalar)
    TYPE (t_file)      , INTENT(IN) :: this
    !! Typnamen des Kompressionsverfahrens
    CHARACTER (LEN=c_file_type_len) :: compr_type ! 
    !! L&auml;nge des Dateinamens
    INTEGER :: name_len ! 
    !
    compr_type = REPEAT( ' ', LEN(compr_type) )
    IF ( file_is_compressed(this) ) THEN
       name_len = LEN_TRIM(this%name)
       IF ( this%name(name_len-3:) == '.zip' ) compr_type = 'ZIP'
       IF ( this%name(name_len-2:) == '.gz'  ) compr_type = 'GZIP'
       IF ( this%name(name_len-1:) == '.Z'   ) compr_type = 'COMPRESS'
    ELSE
       compr_type = 'NONE'
    END IF
    !
  END FUNCTION get_file_compression_type_0
  !
  FUNCTION get_file_compression_type_1 ( this ) &
       RESULT( compr_type )
    !
    !! Objekt (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! Typnamen der Kompressionsverfahren
    CHARACTER (len=c_file_name_len) :: compr_type(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       compr_type(i) = get_file_compression_type_0(this(i))
    END DO
    !
  END FUNCTION get_file_compression_type_1
  ! 
  ! ----------------------------------------------------------------------
  ! Hole Extension des Kompressionsverfahrens
  ! ----------------------------------------------------------------------
  !
  !! Ermittle die Extension des Dateinamens (Skalar)
  FUNCTION get_file_compression_ext_0 ( this ) &
       RESULT( compr_ext )
    !! Objekt (Skalar)
    TYPE (t_file)            , INTENT(IN) :: this      ! 
    !! Typnamen des Kompressionsverfahrens
    CHARACTER (LEN=c_file_compr_ext_len) :: compr_ext ! 
    !! L&auml;nge des Dateinamens
    INTEGER :: name_len ! 
    !
    compr_ext = REPEAT( ' ', LEN(compr_ext) )
    IF ( file_is_compressed(this) ) THEN
       name_len = LEN_TRIM(this%name)
       IF ( this%name(name_len-3:) == '.zip' ) compr_ext(1:4) = '.zip'
       IF ( this%name(name_len-2:) == '.gz'  ) compr_ext(1:3) = '.gz'
       IF ( this%name(name_len-1:) == '.Z'   ) compr_ext(1:2) = '.Z'
    END IF
    !
  END FUNCTION get_file_compression_ext_0
  !
  !! Ermittle die Extension des Dateinamens (Vektor)
  FUNCTION get_file_compression_ext_1 ( this ) &
       RESULT( compr_ext )
    !! Objekt (Vektor)
    TYPE (t_file)           , INTENT(IN) :: this(:)               ! 
    !! Typnamen des Kompressionsverfahrens
    CHARACTER (LEN=c_file_compr_ext_len) :: compr_ext(SIZE(this)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       IF ( any_error( ) ) EXIT
       compr_ext(i) = get_file_compression_ext_0 ( this(i) )
    END DO
    !
  END FUNCTION get_file_compression_ext_1
  !
  !! Ermittle das Dateiobjekt welches eine <EM>Systemdatei<EM> ist <BR>
  !! es wird die erste Systemdatei retourniert, falls eine solche vorhanden ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_systemfile_1 ( this ) &
       RESULT(res)
    !! Dateiobjekte (Vektor)
    TYPE (t_file) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Kopie der Systemdatei (falls vorhanden)
    TYPE (t_file) :: res ! 
    ! Zeiger auf "c_filetypes" mit Systemdateitypen ----------------------
    INTEGER , PARAMETER :: c_idx(6) = (/ 16, 18, 19, 20, 21, 17 /) ! 
    ! Hilfsvariablen
    LOGICAL       :: found ! 
    INTEGER       :: i, j  ! 
    !
    CALL new_file( res )
    found = .false.
    !
    DO i=1,SIZE(this)
       IF ( found ) EXIT
       DO j=1,SIZE(c_idx)
          IF ( found ) EXIT
          IF ( set_char_2_uppercase_0(this(i)%type) == c_filetypes(c_idx(j)) ) THEN
             res   = this(i)
             found = .true.
          END IF
       END DO
    END DO
    !
  END FUNCTION get_file_systemfile_1
  !
  ! ----------------------------------------------------------------------
  !! Zugriff und Format aus Name und Typ ableiten (Skalar)
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE auto_file_access_form_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(INOUT) :: this
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='auto_file_access_form_0'
    !! Dateizugriff ['SEQUENTIAL','DIRECT']
    CHARACTER (LEN=c_file_access_len) :: access ! 
    !! Dateiformat ['UNFORMATTED','FORMATTED']
    CHARACTER (LEN=c_file_form_len)   :: form   ! 
    !
    access = REPEAT( ' ', LEN(access) )
    form   = REPEAT( ' ', LEN(form)   )
    ! Pruefe Datei-Infos
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_file_name(this) ) THEN
          IF ( ok_file_type(this) ) THEN
             CONTINUE
          ENDIF
       ENDIF
    ENDIF
    !
    ! Zuerst den Typ der Datei verwenden, erst dann ggf. mittels des Namens
    ! auf Zugriff und Format schliessen
    IF ( no_error( ) ) THEN
       IF ( file_is_none_0(this) ) THEN
          ! Datei ist nicht definiert: Zugriff und Format unveraendert lassen
          access = get_file_access_0_0(this)
          form   = get_file_form_0_0(this)
       ELSE
          SELECT CASE (set_char_2_uppercase_0(this%type))
          CASE('UNKNOWN')
             IF ( file_is_ascii_0(this) ) THEN
                access = 'SEQUENTIAL'
                form   = 'FORMATTED  '
             ELSE
                access = 'SEQUENTIAL'
                form   = 'UNFORMATTED'
             ENDIF
             !
          CASE('STDIN')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('STDOUT')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('BDF')
             IF ( file_is_ascii_0(this) ) THEN
                access = 'SEQUENTIAL'
                form   = 'FORMATTED  '
             ELSE
                access = 'DIRECT    '
                form   = 'UNFORMATTED'
             ENDIF
          CASE('KNOERG')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('BOEWRT')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('LIGHTS')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('FRAMES')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('LAYOUT')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('INSEL')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('ISOERG')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('STEERING')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('SOIL')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('IPDS')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('VERTICAL')
             IF ( file_is_ascii_0(this) ) THEN
                access = 'SEQUENTIAL'
                form   = 'FORMATTED  '
             ELSE
                access = 'SEQUENTIAL'
                form   = 'UNFORMATTED'
             ENDIF
          CASE('GITTER05')
             IF ( file_is_ascii_0(this) ) THEN
                access = 'SEQUENTIAL'
                form   = 'FORMATTED  '
             ELSE
                access = 'SEQUENTIAL'
                form   = 'UNFORMATTED'
             ENDIF
          CASE('SELAFIN')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('UNTRIM_BAW')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('UNTRIM_VC')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('PROFIL05')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('LOCATIONS')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('TOPO')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('TR2TOPO')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('TR2TOPOIND')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('NETCDF')
             access = 'DIRECT    '
             form   = 'UNFORMATTED'
          CASE('HDF5')
             access = 'DIRECT    '
             form   = 'UNFORMATTED'
          CASE('UCD')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('UCDCLASSIC')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED  '
          CASE('UCDBIN')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('GEOPOS_GKK')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED'
          CASE('TECPLOT')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED'
          CASE('TECPLOTBIN')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('TECPL10')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED'
          CASE('TECPL10BIN')
             access = 'SEQUENTIAL'
             form   = 'UNFORMATTED'
          CASE('LPROFIL')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED'
          CASE('DELFT3D')
             access = 'SEQUENTIAL'
             form   = 'FORMATTED'
          CASE DEFAULT
             CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
             CALL setup_error_act ( '<type>', this%type )
          END SELECT
       ENDIF
    ENDIF
    ! Zugriff und Format in die Datei eintragen
    IF ( no_error( ) ) THEN
       CALL set_file_access_0_0 ( this, access )
       CALL set_file_form_0_0   ( this, form )
    ENDIF
    !
  END SUBROUTINE auto_file_access_form_0
  !
  !! Zugriff und Format aus Name und Typ ableiten (Vektor)
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE auto_file_access_form_1 ( this )
    !
    !! Datenobjekt (Array)
    TYPE (t_file) , INTENT(INOUT) :: this(:)
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='auto_file_access_form_1'
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       DO i = 1, SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL auto_file_access_form_0 ( this(i) )
       END DO
    ENDIF
    !
  END SUBROUTINE auto_file_access_form_1
  !
  ! ----------------------------------------------------------------------
  !! Oeffne eine Datei
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE open_file_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(INOUT) :: this
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='open_file_0'
    !! (Erste entdeckte) nicht-offene Kanalnummer
    INTEGER            :: freeunit ! 
    !! Kanalnummer offen?
    LOGICAL            :: unit_op ! 
    !! Pfad- und Dateiname, ggf. ohne die Kompressions-Endung
    CHARACTER (LEN=c_file_path_and_name_len) :: l_pn, l_pn_unc ! 
    !! Statusvariable (=0, wenn alles ok)
    INTEGER            :: iostat ! 
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    l_pn     = get_file_path_and_name     ( this )
    l_pn_unc = get_file_path_and_name_unc ( this )
    iostat   = 0
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_file_0(this) ) THEN
          IF ( ok_file_name_status_0(this) ) THEN
             IF ( file_is_opened_0(this) ) THEN
                CALL setup_error_act ( all_errors(:), -10, c_upname, c_modname )
                CALL setup_error_act                                               ( &
                     '<..path....................................................>', &
                     this%path )
                CALL setup_error_act                                               ( &
                     '<..name....................................................>', &
                     this%name )
             ENDIF
          ENDIF
       ENDIF
    ENDIF
    ! Ggf. offene Kanalnummer finden
    IF ( no_error( ) ) THEN
       IF ( this%unit == 0 ) THEN
          freeunit = 10
          unit_op  = .FALSE.
          DO
             INQUIRE ( UNIT=freeunit, OPENED=unit_op )
             IF ( .NOT. unit_op ) EXIT
             freeunit = freeunit + 1
          ENDDO
          CALL set_file_unit_0_0 (this,freeunit)
       ENDIF
    ENDIF
    ! Sollte eine komprimierte Datei angegeben worden sein, so wird
    ! die Datei dekomprimiert und die dekomprimierte Datei wird geoeffnet.
    IF ( no_error( ) ) THEN
       IF ( file_is_compressed_0(this) .AND. file_exists_0(this) ) THEN
          CALL uncompress_cmds                  ( &
               get_file_compression_type_0(this), &
               TRIM(l_pn)                       , &
               l_pn_unc )
       ENDIF
    ENDIF
    ! Datei oeffnen
    IF ( no_error( ) ) THEN
       IF ( file_is_formatted_0(this) ) THEN
          IF ( file_is_direct_0(this) ) THEN
             ! Oeffnen einer formatgebundenen Direktzugriffsdatei
             ! - recl wird nur bei Direktzugriffsdateien verwendet
             ! - blank wird nur bei formatgebundenen Dateien verwendet
             OPEN ( &
                  UNIT    =this%unit, IOSTAT=iostat,      ERR   =999        , &
                  FILE    =l_pn_unc , STATUS=this%status, ACCESS=this%access, &
                  FORM    =this%form, RECL  =this%recl,   BLANK ='NULL'     , &
                  ACTION=this%action, DELIM =this%delim,  PAD='YES' )
          ELSE
             ! Oeffnen einer formatgebundenen, sequentiellen Datei
             ! - blank wird nur bei formatgebundenen Dateien verwendet
             OPEN ( &
                  UNIT    =this%unit,     IOSTAT=iostat,      ERR   =999        , &
                  FILE    =l_pn_unc ,     STATUS=this%status, ACCESS=this%access, &
                  FORM    =this%form,                         BLANK ='NULL'     , &
                  POSITION=this%position, ACTION=this%action, DELIM =this%delim , &
                  PAD     ='YES' )
          ENDIF
       ELSE
          IF ( file_is_direct_0(this) ) THEN
             ! Oeffnen einer formatfreien Direktzugriffsdatei
             ! - recl wird nur bei Direktzugriffsdateien verwendet
             OPEN ( &
                  UNIT    =this%unit, IOSTAT=iostat,      ERR   =999        , &
                  FILE    =l_pn_unc , STATUS=this%status, ACCESS=this%access, &
                  FORM    =this%form, RECL  =this%recl,   ACTION=this%action )
          ELSE
             ! Oeffnen einer formatfreien, sequentiellen Datei
             OPEN ( &
                  UNIT    =this%unit    , IOSTAT=iostat,      ERR   =999        , &
                  FILE    =l_pn_unc     , STATUS=this%status, ACCESS=this%access, &
                  FORM    =this%form    ,                                         &
                  POSITION=this%position, ACTION=this%action )
          ENDIF
       ENDIF
    ENDIF
    ! Fehler setzen
999 CONTINUE
    IF ( iostat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -1, c_upname, c_modname )
       WRITE(ctmp,'(I10)') iostat
       CALL setup_error_act ( &
            '<..iostat>'    , &
            ctmp )
       !
       CALL setup_error_act                                               ( &
            '<..path....................................................>', &
            this%path )
       !
       CALL setup_error_act                                               ( &
            '<..name....................................................>', &
            this%name )
       !
       WRITE(ctmp,'(I10)') this%unit
       CALL setup_error_act ( &
            '<..unit..>'    , &
            ctmp )
       !
       CALL setup_error_act ( &
            '<..sta>'       , &
            this%status )
       !
       CALL setup_error_act ( &
            '<..access>'    , &
            this%access )
       !
       CALL setup_error_act ( &
            '<..form...>'   , &
            this%form )
       !
       WRITE(ctmp,'(I10)') this%recl
       CALL setup_error_act ( &
            '<..recl..>'    , &
            ctmp )
       !
       CALL setup_error_act ( &
            '<..po>'        , &
            this%position )
       !
       CALL setup_error_act ( &
            '<..actio>'     , &
            this%action )
       !
       CALL setup_error_act ( &
            '<..delim.>'    , &
            this%delim )
       !
       CALL setup_error_act                           ( &
            '<..type................................>', &
            this%type )
       !
    ENDIF
    !
  END SUBROUTINE open_file_0
  !
  !! Oeffne eine Reihe von Dateien
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE open_file_1 ( this )
    !
    !! Datenobjekt (Array)
    TYPE (t_file) , INTENT(INOUT) :: this(:)
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='open_file_1'
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ok_initialised( c_upname ) ) THEN
       DO i = 1, SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL open_file_0 ( this(i) )
       END DO
    ENDIF
    !
  END SUBROUTINE open_file_1
  !
  ! ----------------------------------------------------------------------
  !! Schliesse eine Datei
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE close_file_0 ( this )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file) , INTENT(IN) :: this
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='close_file_0'
    !! Statusvariable (=0, wenn alles ok)
    INTEGER            :: iostat ! 
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp   ! 
    !! Pfad- und Dateiname, ggf. ohne die Kompressions-Endung
    CHARACTER (LEN=c_file_path_and_name_len) :: l_pn, l_pn_unc ! 
    !
    l_pn     = get_file_path_and_name     ( this ) ! 
    l_pn_unc = get_file_path_and_name_unc ( this ) ! 
    !
    ! Initialisierungen
    iostat = 0
    ! Pruefe Datei-Infos
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_file_0(this) ) THEN
          ! Ist die Datei geoeffnet?
          IF ( file_is_closed_0(this) ) THEN
             CALL setup_error_act ( all_errors(:), -510, c_upname, c_modname )
             CALL setup_error_act                                               ( &
                  '<..path....................................................>', &
                  this%path )
             CALL setup_error_act                                               ( &
                  '<..name....................................................>', &
                  this%name )
          ENDIF
          ! offene Datei in jedem Fall schliessen
          IF ( file_is_opened_0(this) ) THEN
             CLOSE ( UNIT=this%unit, IOSTAT=iostat, ERR=999 )
          END IF
       ENDIF
    ENDIF
    ! Fehler setzen
999 CONTINUE
    !
    IF ( iostat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -500, c_upname, c_modname )
       !
       WRITE(ctmp,'(I10)') iostat
       CALL setup_error_act ( &
            '<..iostat>'    , &
            ctmp )
       !
       CALL setup_error_act                                               ( &
            '<..path....................................................>', &
            this%path )
       !
       CALL setup_error_act                                               ( &
            '<..name....................................................>', &
            this%name )
       !
       WRITE(ctmp,'(I10)') this%unit
       CALL setup_error_act ( &
            '<..unit..>'    , &
            ctmp )
       !
       CALL setup_error_act ( &
            '<..sta>'       , &
            this%status )
       !
       CALL setup_error_act ( &
            '<..access>'    , &
            this%access )
       !
       CALL setup_error_act ( &
            '<..form...>'   , &
            this%form )
       !
       WRITE(ctmp,'(I10)') this%recl
       CALL setup_error_act ( &
            '<..recl..>'    , &
            ctmp )
       !
       CALL setup_error_act ( &
            '<..po>'        , &
            this%position )
       !
       CALL setup_error_act ( &
            '<..actio>'     , &
            this%action )
       !
       CALL setup_error_act ( &
            '<..delim.>'    , &
            this%delim )
       !
       CALL setup_error_act                           ( &
            '<..type................................>', &
            this%type )
       !
    ENDIF
    ! Sollte eine komprimierte Datei angegeben worden sein, so wird
    ! die Datei jetzt (nach dem Schliessen) komprimiert.
    IF ( no_error( ) ) THEN
       IF ( file_is_compressed_0(this) ) THEN
          IF ( file_exists_0(this) ) THEN
             CALL rm_cmds ( l_pn, '-f' )
          ENDIF
       ENDIF
    ENDIF
    IF ( no_error( ) ) THEN
       IF ( file_is_compressed_0(this) ) THEN
          CALL compress_cmds ( get_file_compression_type_0(this), l_pn_unc )
       ENDIF
    ENDIF
    !
  END SUBROUTINE close_file_0
  !
  !! Schliesse eine Reihe von Dateien
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE close_file_1 ( this )
    !! Datenobjekt (Array)
    TYPE (t_file) , INTENT(IN) :: this(:)
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='close_file_1'
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ok_initialised( c_upname ) ) THEN
       DO i = 1, SIZE(this)
          CALL close_file_0 ( this(i) )
       END DO
    ENDIF
    !
  END SUBROUTINE close_file_1
  !
  !! Pr&uuml;fe, ob die Komponente "type" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_type_0 ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL                    :: ok   ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_file_type_0' !
    !! Typ-Zaehler
    INTEGER :: itype
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ! Komponente "type" ist o.k., wenn einer der in c_filetypes definierten
       ! Typen verwendet wird
       ok    = .false.
       itype = 0
       DO
          itype = itype + 1
          IF ( itype > c_noffiletypes ) EXIT
          IF ( set_char_2_uppercase_0(TRIM(ADJUSTL(this%type))) == TRIM(c_filetypes(itype)) ) ok = .true.
          IF ( ok ) EXIT
       ENDDO
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
          CALL setup_error_act ( '<type>', this%type )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ENDIF
    !
  END FUNCTION ok_file_type_0
  !
  !! Pr&uuml;fe f&uuml;r eine Reihe von Dateien, ob die Komponente "type" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_type_1 ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt (Array)
    TYPE (t_file) , INTENT(IN) :: this(:)
    !! Testergebnis
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_file_type_1'
    !! Z&auml;hler
    INTEGER :: i
    !
    ok(:) = .false.
    IF ( ok_initialised( c_upname ) ) THEN
       DO i = 1, SIZE(this)
          ok(i) = ok_file_type_0 ( this(i) )
       END DO
    ENDIF
    !
  END FUNCTION ok_file_type_1
  !
  !! Pr&uuml;fe, ob der Dateityp o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_type_t0 ( filetype ) &
       RESULT( ok )
    !
    !! Dateityp
    CHARACTER (LEN=*), INTENT(IN) :: filetype ! 
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='ok_file_type_t0'
    !! Z&auml;hler
    INTEGER :: itype
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ! Dateityp "this" ist o.k., wenn einer der in c_filetypes definierten
       ! Typen verwendet wird
       ok    = .false.
       itype = 0
       DO
          itype = itype + 1
          IF ( itype > c_noffiletypes ) EXIT
          IF ( set_char_2_uppercase_0(TRIM(ADJUSTL(filetype))) == TRIM(c_filetypes(itype)) ) ok = .true.
          IF ( ok ) EXIT
       ENDDO
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6101, c_upname, c_modname )
          CALL setup_error_act ( '<filetype>', filetype )
       END IF
    ENDIF
    !
  END FUNCTION ok_file_type_t0
  !
  !! Pr&uuml;fe f&uuml;r eine Reihe von Dateien, ob der Dateityp "this" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_type_t1 ( filetype ) &
       RESULT( ok )
    !
    !! Datenobjekt (Array)
    CHARACTER(LEN=*) , INTENT(IN) :: filetype(:)
    !! Testergebnis
    LOGICAL :: ok(SIZE(filetype)) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='ok_file_type_t1'
    !! Z&auml;hler
    INTEGER :: i
    !
    ok(:) = .false. 
    IF ( ok_initialised( c_upname ) ) THEN
       DO i = 1, SIZE(filetype)
          ok(i) = ok_file_type_t0 ( filetype(i) )
       END DO
    ENDIF
    !
  END FUNCTION ok_file_type_t1
  !
  !! Entferne eine Datei (falls die Datei existiert)                                   <BR>
  !! falls die Datei noch offen sein sollte, wird sie vor dem L&ouml;schen geschlossen <BR>
  !! Unterprogramm erzeugt ggf. Fehlermeldungen
  SUBROUTINE remove_file_0 ( this )
    !! Objekt mit Dateibezeichnung
    TYPE (t_file) , INTENT(IN) :: this ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=13) , PARAMETER :: c_upname='remove_file_0'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( file_exists( this ) ) THEN
          IF ( file_is_opened( this ) ) CALL close_file( this )
          IF ( no_error( ) ) CALL rm_cmds( TRIM(get_file_path_and_name(this)), '-f' )
       END IF
    END IF
    !
  END SUBROUTINE remove_file_0
  !
  !! Entferne mehrere Dateien (falls die Dateien existieren) <BR>
  !! Unterprogramm erzeugt ggf. Fehlermeldungen
  SUBROUTINE remove_file_1 ( this )
    !! Objekt mit Dateibezeichnungen
    TYPE (t_file) , INTENT(IN) :: this(:) !
    !! Name der Programmeinheit
    CHARACTER (LEN=13) , PARAMETER :: c_upname='remove_file_1'
    ! lokale Variablen
    INTEGER                        :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL remove_file_0( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE remove_file_1
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
    !
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
       WRITE(*,*) ' *** Warnung *** Modul "b_file" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_file ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
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
  SUBROUTINE init_file_all_errors ( )
    !
    !! Z&auml;hlervariable
    INTEGER :: i, ic
    !
    DO i=1,2
       !
       ic = 0
       !
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 1 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist nicht initialisiert\n'//&
               '--> INIT_file ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_file ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "unit"\n'//&
               'aktuell   = <unit>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "name"\n'//&
               'aktuell        = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "status"\n'//&
               'aktuell   = <status>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "access"\n'//&
               'aktuell   = <access>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "form"\n'//&
               'aktuell   = <form>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "recl"\n'//&
               'aktuell   = <recl>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "position"\n'//&
               'aktuell   = <position>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "action"\n'//&
               'aktuell   = <action>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "delim"\n'//&
               'aktuell   = <delim>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "type"\n'//&
               'aktuell   = <type>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_file"\n'//&
               'Typ-Komponente = "path"\n'//&
               'aktuell   = <path>\n'//&
               'Dateiname = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6120 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'kumulierte Laenge von "path" und "name" zu gross\n'//&
               'aktuelle Laengen : path = <pathlen>, name = <namelen>\n'//&
               'Gesamtlaenge     : act  = <act>, req <= <req>\n'//&
               'Pfad = <path>\n'//&
               'Name = <name>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6101 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Dateityp\n'//&
               'aktuell = <filetype>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "unit"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "name"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "status"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "access"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "form"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "recl"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "position"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "action"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "delim"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "type"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_file"\n'//&
               'Typ-Komponente = "path"\n'//&
               'Dateiname = <name>\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_file"\n'//&
               '--> Code in Modul "b_file" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OPEN-Methoden\n'//&
               'Fehler beim Oeffnen einer in einem Objekt "t_file" definierten Datei.\n'//&
               'Iostat        = <..iostat>\n'//&
               'Pfad          = <..path....................................................>\n'//&
               'Dateiname     = <..name....................................................>\n'//&
               'Kanalnummer   = <..unit..>\n'//&
               'Status        = <..sta>\n'//&
               'Zugriff       = <..access>\n'//&
               'Format        = <..form...>\n'//&
               'Recl          = <..recl..>\n'//&
               'Position      = <..po>\n'//&
               'Aktion        = <..actio>\n'//&
               'Delimiter     = <..delim.>\n'//&
               'Typ der Datei = <..type................................>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: INQUIRY-Methoden\n'//&
               'Status der angegebenen Datei ist "OLD", Datei ist aber nicht vorhanden.\n'//&
               'Pfad        = <..path....................................................>\n'//&
               'Dateiname   = <..name....................................................>\n'//&
               'Kanalnummer = <..unit..>\n'//&
               'Status      = <..status..>\n'//&
               'Existenz    = <..exist..>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: INQUIRY-Methoden\n'//&
               'Status der angegebenen Datei ist "NEW", Datei ist aber bereits vorhanden.\n'//&
               'Pfad        = <..path....................................................>\n'//&
               'Dateiname   = <..name....................................................>\n'//&
               'Kanalnummer = <..unit..>\n'//&
               'Status      = <..status..>\n'//&
               'Existenz    = <..exist..>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CLOSE-Methoden\n'//&
               'Fehler beim Schliessen einer in einem Objekt "t_file" definierten Datei.\n'//&
               'Iostat        = <..iostat>\n'//&
               'Pfad          = <..path....................................................>\n'//&
               'Dateiname     = <..name....................................................>\n'//&
               'Kanalnummer   = <..unit..>\n'//&
               'Status        = <..sta>\n'//&
               'Zugriff       = <..access>\n'//&
               'Format        = <..form...>\n'//&
               'Recl          = <..recl..>\n'//&
               'Position      = <..po>\n'//&
               'Aktion        = <..actio>\n'//&
               'Delimiter     = <..delim.>\n'//&
               'Typ der Datei = <..type................................>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -10 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OPEN-Methoden\n'//&
               'Datei soll geoeffnet werden, ist aber bereits offen.\n'//&
               'Pfad      = <..path....................................................>\n'//&
               'Dateiname = <..name....................................................>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CLOSE-Methoden\n'//&
               'Datei soll geschlossen werden, ist aber bereits geschlossen.\n'//&
               'Pfad      = <..path....................................................>\n'//&
               'Dateiname = <..name....................................................>\n'//&
               '--> Daten pruefen' )
       END IF
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
    END DO
    !
  END SUBROUTINE init_file_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_file_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_file_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "unit" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_unit ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_file_unit' !
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ! Komponente "unit" ist o.k., wenn >= 0
       ok = .false.
       IF ( this%unit >= 0 ) ok = .true.
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
          WRITE(ch,'(I10)') this%unit ; CALL setup_error_act ( '<unit>', ch )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_unit
  !
  !! Pr&uuml;fe, ob die Komponente "path" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_path ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_file_path' !
    !! Hilfsvariable
    INTEGER           :: n  ! 
    CHARACTER (LEN=1) :: ch ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       n = LEN_TRIM( this%path )
       IF ( n > 0 ) THEN
          ch = this%path(n:n)
          ok = MERGE( .true., .false., ch == '/' .OR. ch == '\' ) 
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6110, c_upname, c_modname )
             CALL setup_error_act ( '<path>', this%path )
             CALL setup_error_act ( '<name>', this%name )
          END IF
       ELSE
          ok = .true.
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_path
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_name ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_file_name' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ok = MERGE( .true., .false., LEN_TRIM(this%name) > 0 ) 
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_name
  !
  !! Pr&uuml;fe, ob die Komponente "status" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_status ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_file_status' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ok = .false.
       IF ( TRIM(ADJUSTL(this%status(1:3))) == 'OLD'      .OR.    &
            TRIM(ADJUSTL(this%status(1:3))) == 'NEW'      .OR.    &
            TRIM(ADJUSTL(this%status(1:7))) == 'UNKNOWN'  .OR.    &
            TRIM(ADJUSTL(this%status(1:7))) == 'SCRATCH'  .OR.    &
            TRIM(ADJUSTL(this%status(1:7))) == 'REPLACE'  )  ok = .true.
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
          CALL setup_error_act ( '<status>', this%status )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_status
  !
  !! Pr&uuml;fe, ob die Komponente "access" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_access ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_file_access' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ok = .false.
       IF ( TRIM(ADJUSTL(this%access(1:10))) == 'SEQUENTIAL'   .OR.    &
            TRIM(ADJUSTL(this%access(1:6 ))) == 'DIRECT')   ok = .true.
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
          CALL setup_error_act ( '<access>', this%access )
          CALL setup_error_act ( '<name>', this%name )
       END IF 
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_access
  !
  !! Pr&uuml;fe, ob die Komponente "form" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_form ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_file_form' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ok = .false.
       IF ( TRIM(ADJUSTL(this%form(1:9 ))) == 'FORMATTED'   .OR.     &
            TRIM(ADJUSTL(this%form(1:11))) == 'UNFORMATTED')     ok = .true.
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
          CALL setup_error_act ( '<form>', this%form )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_form
  !
  !! Pr&uuml;fe, ob die Komponente "recl" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_recl ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_file_recl' !
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ok = MERGE( .true., .false., this%recl > 0 )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
          WRITE(ch,'(I10)') this%recl ; CALL setup_error_act ( '<recl>', ch )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ELSE
       ok = .false.
    ENDIF
    !
  END FUNCTION ok_file_recl
  !
  !! Pr&uuml;fe, ob die Komponente "position" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_position ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_file_position' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ok = .false.
       IF ( TRIM(ADJUSTL(this%position(1:6))) == 'REWIND'   .OR.       &
            TRIM(ADJUSTL(this%position(1:6))) == 'APPEND'   .OR.       &
            TRIM(ADJUSTL(this%position(1:4))) == 'ASIS') ok = .true.
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
          CALL setup_error_act ( '<position>', this%position )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_position
  !
  !! Pr&uuml;fe, ob die Komponente "action" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_action ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_file_action' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ok = .false.
       IF ( TRIM(ADJUSTL(this%action(1:4))) == 'READ'       .OR.       &
            TRIM(ADJUSTL(this%action(1:5))) == 'WRITE'      .OR.       &
            TRIM(ADJUSTL(this%action(1:9))) == 'READWRITE')   ok = .true.
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6080, c_upname, c_modname )
          CALL setup_error_act ( '<action>', this%action )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_action
  !
  !! Pr&uuml;fe, ob die Komponente "delim" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_delim ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok !
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_file_delim' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       ok = .false.
       IF ( TRIM(ADJUSTL(this%delim(1:10))) == 'APOSTROPHE'   .OR.       &
            TRIM(ADJUSTL(this%delim(1:5 ))) == 'QUOTE'        .OR.       &
            TRIM(ADJUSTL(this%delim(1:4 ))) == 'NONE')   ok = .true.
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6090, c_upname, c_modname )
          CALL setup_error_act ( '<delim>', this%delim )
          CALL setup_error_act ( '<name>', this%name )
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_delim
  !
  !! Pr&uuml;fe, ob die kombinierte L&auml;nge von "path" und "name" eines Datenobjektes o.k. ist <BR>
  !! HINWEIS: INWUIRE, OPEN vertragen nur eine begrenzte L&auml;nge <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_file_path_and_name_len ( this ) &
       RESULT( ok )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='ok_file_path_and_name_len' !
    !! max. zulaessige L&auml;nge
    INTEGER            , PARAMETER :: c_max_path_and_name_len=240           ! 
    !! Hilfsvariablen
    CHARACTER (LEN=5) :: ch ! 
    IF ( ok_initialised( c_upname ) ) THEN
       ok = ( get_file_path_and_name_len( this ) <= c_max_path_and_name_len )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6120, c_upname, c_modname )
          WRITE(ch,'(I5)') get_file_path_len( this ) ; CALL setup_error_act ( '<pathlen>', ch )
          WRITE(ch,'(I5)') get_file_name_len( this ) ; CALL setup_error_act ( '<namelen>', ch )
          WRITE(ch,'(I5)') get_file_path_and_name_len( this ) ; CALL setup_error_act ( '<act>', ch )
          WRITE(ch,'(I5)') c_max_path_and_name_len ; CALL setup_error_act ( '<req>', ch )
          CALL setup_error_act ( '<path>', get_file_path ( this ) )
          CALL setup_error_act ( '<name>', get_file_name ( this ) )
       END IF
    ELSE
       ok = .false. 
    ENDIF
    !
  END FUNCTION ok_file_path_and_name_len
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "unit" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_unit ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='print_file_unit' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%unit
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente unit  - - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# unit = ',I10)
    ENDIF
    !
  END SUBROUTINE print_file_unit
  !
  !! Drucke den Inhalt der Komponente "path" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_path ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='print_file_path' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%path)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7110, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente path  - - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# path = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_path
  !
  !! Drucke den Inhalt der Komponente "name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_name ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='print_file_name' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%name)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente name  - - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# name = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_name
  !
  !! Drucke den Inhalt der Komponente "status" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_status ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_file_status' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%status)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente status  - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# status = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_status
  !
  !! Drucke den Inhalt der Komponente "access" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_access ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_file_access' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%access)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente access  - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# access = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_access
  !
  !! Drucke den Inhalt der Komponente "form" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_form ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='print_file_form' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%form)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente form  - - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# form = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_form
  !
  !! Drucke den Inhalt der Komponente "recl" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_recl ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='print_file_recl' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%recl
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente recl  - - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# recl = ', I10)
    ENDIF
    !
  END SUBROUTINE print_file_recl
  !
  !! Drucke den Inhalt der Komponente "position" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_position ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_file_position' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%position)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7070, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente position  - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# position = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_position
  !
  !! Drucke den Inhalt der Komponente "action" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_action ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_file_action' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%action)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7080, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente action  - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# action = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_action
  !
  !! Drucke den Inhalt der Komponente "delim" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_delim ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_file_delim' !
    !! Statusvariable
    INTEGER :: stat !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%delim)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7090, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente delim - - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# delim = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_delim
  !
  !! Drucke den Inhalt der Komponente "type" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_file_type ( this )
    !
    !! Datenobjekt
    TYPE (t_file) , INTENT(IN) :: this !
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='print_file_type' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%type)
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7100, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', this%name )
       END IF
8000   FORMAT ( &
            '# Inhalt der Komponente type  - - - - - - - - - - - - - - - - ',/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',/&
            '# type = ', A)
    ENDIF
    !
  END SUBROUTINE print_file_type
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SET-Methoden <<< [ERR_NO = 8500 bis 8999]
  ! ----------------------------------------------------------------------
  !
  !! Umwandeln einer Bezeichnung in Grossbuchtstaben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION set_char_2_uppercase_0 ( inpchar ) &
       RESULT( outchar )
    !
    !! Zeichenfolge (Original)
    CHARACTER (LEN=*) , INTENT(IN) :: inpchar ! 
    !! Zeichenfolge (konvertiert in Grossbuchstaben)
    CHARACTER (LEN=LEN(inpchar))   :: outchar ! 
    !! Zaehler fuer alle Zeichen der Bezeichnung
    INTEGER :: i
    !! ASCII-Nummer eines Zeichens der Bezeichnung
    INTEGER :: nascii
    !
    outchar = inpchar
    DO i = 1, LEN(outchar)
       nascii = IACHAR(outchar(i:i))
       IF ( nascii >= 97 .AND. nascii <= 122 ) outchar(i:i) = ACHAR(nascii-32)
    ENDDO
    !
  END FUNCTION set_char_2_uppercase_0
  !
  !! Ermittle die Position des Trennzeichens f&uuml;r die Abgrenzung von
  !! "path" und "name" in einem String <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_pathdelimiter_idx ( val ) &
       RESULT( res )
    !! String mit vollst&auml;ndiger Pfad-und-Namensbezeichnung
    CHARACTER (LEN=*) , INTENT(IN) :: val ! 
    !! Position des Trennzeichens, -1 falls nicht vorhanden
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    ! 
    res = -1
    idx = INDEX( val, '/', BACK=.true. )
    IF ( idx > 0 ) res = idx
    idx = INDEX( val, '\', BACK=.true. )
    IF ( idx > 0 ) res = idx
    !
  END FUNCTION get_file_pathdelimiter_idx
  !
  !! hole die Komponenten "path" und "name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_path_and_name_unc_0_0 ( this ) &
       RESULT( val )
    !
    !! Datenobjekt (Skalar)
    TYPE (t_file)              , INTENT(IN)  :: this !
    !! R&uuml;ckgabewert "path_and_name" (Skalar)
    CHARACTER (len=c_file_path_and_name_len) :: val  ! 
    !! Hilfsvariable
    INTEGER                         :: n      ! 
    CHARACTER (LEN=c_file_name_len) :: l_name ! 
    !
    val = REPEAT( ' ', LEN(val) )
    IF ( LEN_TRIM(this%path) > 0 ) val = this%path(1:MIN(LEN_TRIM(this%path),LEN(val)))
    n   = LEN_TRIM(val)
    l_name = get_file_uncompressed_name( this )
    IF ( LEN_TRIM(l_name) > 0 ) val(n+1:) = l_name(1:MIN(LEN_TRIM(l_name),LEN(val)-n))
    !
  END FUNCTION get_file_path_and_name_unc_0_0
  !
END MODULE b_file
! TailOfBaseModule --------------------------------------------------------
