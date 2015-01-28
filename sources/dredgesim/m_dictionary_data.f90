! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Datenmodul des Paketes "dictionary"</h2>
!! @author S. Spohr
!! @version 1.3 vom 07/07/05, Quellcode: mod_m_dictionary_data.f90
!! <HR>
!! contains global data for simulation package "dictionary"       <BR>
!! <HR>
!                                                                    <BR>
!  Copyright-Hinweis
!
!  Copyright (C) 2005 Bundesanstalt fuer Wasserbau 
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-06-27 : S. Spohr : Startversion 
!  01.02 : 2002-07-11 : S. Spohr : Verbesserung Fehlertexte
!  01.03 : 2005-07-07 : G. Lang  : infile_path
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Speicherung globaler Daten f&uuml;r das Paket "dictionary";
!!   <LI> Dient dem Datenaustausch innerhalb des Paketes "dictionary";
!!   <LI> Ein Zugriff auf diese Daten von anderen Paketen aus ist nur 
!!        in Einzelf&auml;llen mit Hilfe der in "p_dictionary_ui"
!!        vorhandenen GET- und SET- (SETUP-) Methoden erlaubt.
!! </OL>
!! <HR>
!!                                                                  <BR>
!
MODULE m_dictionary_data
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten [ggf. entfernen]
  !
  ! [A.2] Basis-Modul mit Typ+Methoden "Fehlerbehandlung"
  !
  USE b_error, ONLY :       &
       !   Typdefinitionen
       t_error
  !
  ! [A.?] weitere BASIS-Module (ONLY benutzen!)
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
  IMPLICIT NONE
  !
  ! Anm.: PRIVATE kann entfallen, da alle Groessen global (innerhalb
  !       des Paketes bekannt sein sollen.
  !
  ! ---------------------------------------------------------------------
  ! [B] oeffentlich zugaengliche Deklarationen 
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Konstantwerte (Parameter) [bei Bedarf verwenden]
  !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! [B.2] Daten, die auch ausserhalb von "dictionary" verwendet werden
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  ! [B.2.1] Skalare und Felder (Originaldaten) die extern als
  !         Kopie verwendet werden
  !
  ! [B.2.2] Skalare und Felder (Kopien externer Daten)
  !
  ! [B.2.3] Skalare und Felder (Originaldaten) auf die von
  !         ausserhalb mit einem "Zeiger" verwiesen werden kann
  !
  ! [B.2.4] Skalare und Felder (externe Daten) auf die 
  !         in diesem Paket mit einem "Zeiger" verwiesen werden 
  !         kann
  !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! [B.3] Daten, die nur innerhalb von "dictionary" verwendet werden
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  ! [B.3.1] Skalare (nur paketinterne Verwendung)
  !
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Paketinitialisierung
  LOGICAL , SAVE :: initialised = .false.  ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL , SAVE :: prn_op      = .false.  ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL , SAVE :: trc_op      = .false.  ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER , SAVE :: prn_lun     = -1       ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER , SAVE :: trc_lun     = -1       !    
  !! Name der Dictionary-Datei
  CHARACTER (LEN=80) , SAVE     :: dicfile_name ! 
  !! Name der Input-Datei
  CHARACTER (LEN=80)  , SAVE    :: infile_name  ! 
  !! Pfad der Input-Datei
  CHARACTER (LEN=160) , SAVE    :: infile_path  ! 
  !
  ! [B.3.2] Felder (nur paketinterne Verwendung)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Paketes "dictionary"
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)! 
  !
END MODULE m_dictionary_data
! TailOfPackageModule -----------------------------------------------------
