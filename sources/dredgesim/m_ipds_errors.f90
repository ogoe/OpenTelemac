! -------------------------------------------------------------------------
! HeadOfPackageErrorModule ------------------------------------------------
!
!! <H2>Modul zur Definition der Fehlermeldungen des Paketes "ipds"</H2>
!! @author Jens J&uuml;rges
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_errors.f90
!! <HR>
!! definition of error messages for the "ipds" software package <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-08-08 : J. Juerges : Startversion
!  01.02 : 2002-10-17 : J. Juerges : neue Fehler fuer oeffentl. Routinen get_ipds_val_*_phy1
!  01.03 : 2003-01-10 : J. Juerges : Entwicklungsgeschichte nicht fuer f90doc
!  01.04 : 2003-01-10 : J. Juerges : Lizenzangabe durch Platzhalter ersetzt
!  01.05 : 2003-01-10 : J. Juerges : keine Beschreibungen der Interfaces dort, wo sie PUBLIC gemacht werden
!  02.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-08-25 : J. Juerges : Fehler beim Normalisieren werden unter den Fehlernummern 24??? lokal im Modul m_ipds_phyval verbucht
!  03.01 : 2007-03-13 : G. Lang    : neue Hauptversionsnummer 3
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! <OL>
!!   <LI> Definition der Fehlermeldungen des Paketes "ipds".
!! </OL>
!! <HR>
!
MODULE m_ipds_errors
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit Typ+Methoden "Fehlerbehandlung"
  USE b_error, ONLY :  &
       ! Routinen / Interfaces
       no_error,       &
       new_error,      &
       kill_error,     &
       set_error_ierr, &
       set_error_cerr
  !
  ! ---------------------------------------------------------------------
  ! [B]  Module des Paketes "ipds"
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Modul mit globalen Daten
  USE m_ipds_data, ONLY : all_errors ! 
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] Schnittstellen
  !! Initialisieren/Allokieren aller Fehlermeldungen des Paketes
  INTERFACE init_all_errors
     MODULE PROCEDURE init_all_errors_d ! 
  END INTERFACE
  !! De-Initialisieren/De-Allokieren aller Fehlermeldungen des Paketes
  INTERFACE clear_all_errors
     MODULE PROCEDURE clear_all_errors_d ! 
  END INTERFACE
  !
  ! [C.2] Liste der oeffentlichen Methoden
  PUBLIC :: init_all_errors
  PUBLIC :: clear_all_errors
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=13) , PRIVATE, PARAMETER :: c_modname='m_ipds_errors'
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
  ! Oeffentliche Methoden oder Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Packages
  SUBROUTINE init_all_errors_d ( )
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
               'Package "io_ipds" ist nicht initialisiert\n'//&
               '--> INIT_ipds ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Package "io_ipds" ist schon initialisiert\n'//&
               '--> CLEAR_ipds ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Keine Objekte "t_ipds" vorhanden\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Kein Objekt "t_ipds" mit Identifikationsnummer vorhanden\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Keine Arbeits-Objekte definiert\n'//&
               '--> Arbeits-Objekt mit SETUP_ipds_WORK_OBJECT setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "datetime"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "physet"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "region"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "mespos"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "regphyset"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer OpenMI-Komponente von "t_ipds"\n'//&
               'Typ-Komponente = <omi-component>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer neues Objekt "t_ipds_list"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente "object" in Listen-Objekt "t_ipds_list"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler Objekt "t_ipds_list"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente "object" in Listen-Objekt "t_ipds_list"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "datetime"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "physet"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "region"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "mespos"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "regphyset"\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer OpenMI-Komponente von "t_ipds"\n'//&
               'Typ-Komponente = <omi-component>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "Identifikationsnummer"\n'//&
               'Inhalt         = <id>\n'//&
               'Die Identifikationsnummer muss groesser Null sein!\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "beschreibender Name"\n'//&
               'Inhalt         = <name>\n'//&
               'Der beschreibende Name darf nicht undefiniert sein\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "Dateibeschreibung"\n'//&
               'Die Dateibeschreibung selbst ist nicht ok, oder die Dateibeschreibung\n'//&
               'entspricht keiner der implementierten Dateivarianten.\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "datetime"\n'//&
               'Inhalt         = <datetime>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "physet"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "region"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "mespos"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "regphyset"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponenten von "t_ipds"\n'//&
               'Typ-Komponenten = "regphyset" und "region"\n'//&
               'In der Komponente "regphyset" ist ein Name fuer eine Region eingetragen,\n'//&
               'es fehlen jedoch Angaben ueber die Region/en\n'//&
               '--> Mind. einen Block "region" definieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponenten von "t_ipds"\n'//&
               'Typ-Komponenten = "regphyset" und "region"\n'//&
               'In der Komponente "regphyset" ist ein Name fuer eine Region eingetragen,\n'//&
               'der in der Komponente "region" nicht enthalten ist\n'//&
               'Name der Region = <region-name>\n'//&
               '--> Namen-Angaben muessen uebereinstimmen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponenten von "t_ipds"\n'//&
               'Typ-Komponenten = "regphyset" und "mespos"\n'//&
               'In der Komponente "regphyset" ist ein Name fuer eine Messstation eingetragen,\n'//&
               'es fehlen jedoch Angaben ueber die Messstation/en\n'//&
               '--> Mind. einen Block "sampling_point" definieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6210 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponenten von "t_ipds"\n'//&
               'Typ-Komponenten = "regphyset" und "mespos"\n'//&
               'In der Komponente "regphyset" ist ein Name fuer eine Messstation eingetragen,\n'//&
               'der in der Komponente "mespos" nicht enthalten ist\n'//&
               'Name der Messstation = <mespos-name>\n'//&
               '--> Namen-Angaben muessen uebereinstimmen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6220 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "mespos_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6230 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "mespos_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6240 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "phyval_type"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6250 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "phyval_type"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6301 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fuer eine physikalische Groesse wurden Default-Werte\n'//&
               'unterschiedlichen Typs definiert.\n'//&
               'Name der physikalischen Groesse = <phy_name>\n'//&
               '--> Typ der physikalischen Groesse an den unterschiedlichen\n'//&
               '    Messstationen vereinheitlichen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6302 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fuer eine physikalische Groesse wurden Werte\n'//&
               'unterschiedlichen Typs definiert.\n'//&
               'Name der physikalischen Groesse = <phy_name>\n'//&
               '--> Typ der physikalischen Groesse an den unterschiedlichen\n'//&
               '    Messstationen vereinheitlichen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "datetime" wurde nicht allokiert\n'//&
               '--> Komponente muss einmal in der Datei angegeben werden\n'//&
               '    (Block "date_and_time")' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6520 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_ipds"\n'//&
               'Typ-Komponente = "physet" wurde nicht allokiert\n'//&
               '--> Komponente muss einmal in der Datei angegeben werden\n'//&
               '    (Block "global_constant_values")' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Fusszeilen\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_ipds"\n'//&
               'Typ-Komponente = "datetime"\n'//&
               '--> Code in Modul "io_ipds_ui" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_ipds"\n'//&
               'Typ-Komponente = "physet"\n'//&
               '--> Code in Modul "io_ipds_ui" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_ipds"\n'//&
               'Typ-Komponente = "region"\n'//&
               '--> Code in Modul "io_ipds_ui" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_ipds"\n'//&
               'Typ-Komponente = "mespos"\n'//&
               '--> Code in Modul "io_ipds_ui" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_ipds"\n'//&
               'Typ-Komponente = "regphyset"\n'//&
               '--> Code in Modul "io_ipds_ui" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_ipds"\n'//&
               'Typ-Komponente = <omi-component>\n'//&
               'Aktion         = <action>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "io_ipds_ui"\n'//&
               '--> Code in Modul "io_ipds_ui" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer ID-Nummern\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Datei-Type\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9004 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Fortran-Form\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9005 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Fortran-Access\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9006 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Fortran-Delim\n'//&
               '--> Code in Modul "io_ipds_ui" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fuer eine Datenposition und eine physikalische Groesse\n'//&
               'konnte kein Datenwert ermittelt werden.\n'//&
               'X-Koordinate   = <xxxxxxxx>\n'//&
               'Y-Koordinate   = <yyyyyyyy>\n'//&
               'Z-Koordinate   = <zzzzzzzz>\n'//&
               'Phys. Groesse  = <phy_name>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21011 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfelder "val" und "var_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "p"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21021 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "p"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfelder "l_val" und "l_var_ex"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21031 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfelder "l_val", "l_var_name" und "l_var_ex"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Ausgabefelder "val" und "var_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "l_all_var_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Ausgabefeld "nof_var"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fuer ein Datenobjekt ist fuer eine physikalische Groesse\n'//&
               'eine Varianten-Bezeichnung aufgetaucht, die nicht in der Variantenliste\n'//&
               'enthalten ist.\n'//&
               'phys. Groesse        = <phy_name>\n'//&
               'Variantenbezeichnung = <var_name>\n'//&
               '--> Code in Modul "m_ipds_physet" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "l_var_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21210 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "l_var_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21220 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Ausgabefeld "var_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21230 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "region_border"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21231 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "region_border"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21240 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "mespos_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21241 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "mespos_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21250 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfelder "idx_mespos" und "mespos_nof_physet_set"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21251 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfelder "idx_mespos" und "mespos_nof_physet_set"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21260 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfelder "mespos_phyval" und "mespos_coor"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21261 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfelder "mespos_phyval" und "mespos_coor"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Hilfsfeld "d_var_name" zu klein\n'//&
               'Anzahl Elemente (m_d_var_name) = <arraysize>\n'//&
               '--> m_d_var_name erhoehen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21400 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Anzahl physikalischer Sets fuer eine Messposition nicht ok.\n'//&
               'Erlaubt ist, dass ein Set KEINE Daten zu einer physikalischen Groesse enthaelt,\n'//&
               'oder dass die Daten in GENAU EINEM Set enthalten sind.\n'//&
               'Physikalischen Groesse = <phy_name>\n'//&
               'Anz. Sets mit weniger als KEINEN Informationen = <anz._lt0>\n'//&
               'Anz. Sets mit mehr als EINER Information       = <anz._gt1>\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Es sind mehr "Set"s fuer eine physikalische Groesse vorhanden,\n'//&
               'als zuvor ermittelt.\n'//&
               'phys. Groesse = <phy_name>\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21501 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Es sind weniger "Set"s fuer eine physikalische Groesse vorhanden,\n'//&
               'als zuvor ermittelt.\n'//&
               'phys. Groesse = <phy_name>\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'erforderliche Datei-Variante ist nicht implementiert\n'//&
               'akt: Datei-Variante = <DateiVarianteNo> \n'//&
               'req:                  [1,<MaxDateiVarianteNo>] \n'//&
               '--> Code in Modulprozedur "read_ipds_0" ("io_ipds_ui") ergaenzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23999 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Eingelesene Daten sind nicht vollstaendig ok\n'//&
               '--> Fehlertexte analysieren und Dateiinhalt verbessern' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 25001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: INTERPOLATIONS-Methoden\n'//&
               'erforderliche Interpolations-Methode ist nicht implementiert\n'//&
               'Name   der akt. Interpolations-Methode              = <NamIpolMethod>\n'//&
               'Nummer der akt. Interpolations-Methode              = <NumIpolMethod>\n'//&
               'Nummern der implementierten Interpolations-Methoden = [1-<MaxIpolMethod>]\n'//&
               '--> Code in Modulprozedur ergaenzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 25010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: INTERPOLATIONS-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfelder "mespos_var_name" und "mespos_val"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 25011 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfelder "mespos_var_name" und ""mespos_val\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -6001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private OK-Methoden\n'//&
               'erforderliche Datei-Variante ist nicht implementiert\n'//&
               'req: Datei-Type     = "<FortranFileType>"\n'//&
               '     Fortran-FORM   = "<FortranFileForm>"\n'//&
               '     Fortran-ACCESS = "<FortranFileAccess>"\n'//&
               '     Fortran-DELIM  = "<FortranFileDelim>"\n'//&
               '--> erforderliche Datei-Variante in Modul "io_ipds_ui" implementieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Block kann noch nicht gelesen werden !\n'//&
               'Block-Name = <block_name> \n'//&
               '--> Code in Modulprozedur ergaenzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Keyzeile kann noch nicht gelesen werden !\n'//&
               'Keyword = <key_name> \n'//&
               'in Block <block_name> \n'//&
               '--> Code in Modulprozedur ergaenzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Parameter kann noch nicht ausgewertet werden !\n'//&
               'Parameterinhalt = <par_inhalt> \n'//&
               'Keyword = <key_name> \n'//&
               'in Block <block_name> \n'//&
               '--> Code in Modulprozedur ergaenzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Mehr phys. Groessen in einem Block entdeckt, als gedacht."\n'//&
               '--> Code in Modulprozeduren "get_nof_phy_in_block" und\n'//&
               '    "get_phy_name_in_block" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Eine phys. Groesse in einem Block entdeckt, die zuvor unerkannt blieb."\n'//&
               '--> Code in Modulprozeduren "get_nof_phy_in_block",\n'//&
               '    "get_phy_name_in_block" und "get_nof_var_in_block" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfelder\n'//&
               '"phyval", "phy_name_in_block" und "n_var_in_block"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfelder\n'//&
               '"phyval", "phy_name_in_block" und "n_var_in_block"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfelder "i_var", "var_name","type","val"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23210 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfelder "i_var", "var_name","type","val"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "region"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23310 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "region"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23400 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "border"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23410 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "border"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "mespos"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "mespos"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23600 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "regphyset"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23610 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "regphyset"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23700 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'Allocate-Fehler fuer Hilfsfeld "mespos_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23710 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden\n'//&
               'De-Allocate-Fehler fuer Hilfsfeld "mespos_name"\n'//&
               '--> Code in Modulprozedur pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private OMI-Methoden\n'//&
               'es koennen keine Koordinaten-Objekte erzeugt werden\n'//&
               'Objektname 1    = <objectname1>\n'//&
               '--> kein externes Gitter, Sampling Points, Regional Values' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private OMI-Methoden\n'//&
               'unterschiedliche Koordinatensysteme fuer Gitter und Daten\n'//&
               'Objektname 1    = <objectname1>\n'//&
               'Gitter KS       = <gid>\n'//&
               'Daten  KS       = <did>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private OMI-Methoden\n'//&
               'es koennen keine Index-Listen-Objekte erzeugt werden\n'//&
               'Objektname 1    = <objectname1>\n'//&
               '--> kein externes Gitter, Sampling Points, Regional Values' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private OMI-Methoden\n'//&
               'es konnte kein passendes Daten-Koordinaten-Objekt ermittelt werden\n'//&
               'Objektname 1      = <objectname1>\n'//&
               'ElemSetObjektname = <indexname>\n'//&
               '--> Code pruefen' )
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
  END SUBROUTINE init_all_errors_d
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Packages
  SUBROUTINE clear_all_errors_d ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_all_errors_d
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
  ! Lokale Methoden (PRIVATE)
  !
END MODULE m_ipds_errors
! TailOfPackageErrorModule ---------------------------------------------------
