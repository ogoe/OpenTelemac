! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Modul zur Definition der Fehlermeldungen des Paketes H-Grid</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 4.9 vom 02/05/07, Quellcode: mod_m_h_grid_errors.f90
!! <HR>
!! definition of error messages for the H-Grid software package <BR>
!! <HR>
!!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Paketes
!  01.01 : 2002-07-17 : G. Lang    : Startversion
!  01.12 : 2003-03-20 : P. Schade  : + Fehlermeldungen zu m_grid_convert,
!                                    Aenderungen Header, u.a. Copyright
!  01.15 : 2003-04-02 : P. Schade  : Komponente ncsize
!  01.16 : 2003-04-02 : P. Schade  : Komponente knolg 
!  01.17 : 2003-04-16 : P. Schade  : FM bezueglich ncsize und knolg veraendert
!  01.18 : 2003-04-16 : P. Schade  : FM fuer nptfr und nptir
!  01.19 : 2003-12-02 : J. Juerges : Komponente nsi
!  01:20 : 2004-02-25 : G. Lang    : Fehlermeldung 6055 erweitert
!  01.23 : 2004-05-27 : J. Juerges : Fehlermeldungen fuer xg (Polygon-Schwerpunktkoordinaten)
!  01.25 : 2004-11-11 : G. Lang    : Fehlermeldungen wegen convert_untrim_terrace
!  02.01 : 2005-03-07 : G. Lang    : Erweiterungen OpenMI (unvollstaendig)
!  02.02 : 2005-03-10 : G. Lang    : div. Erweiterungen fuer Konfigurationsphase OpenMI
!  02.03 : 2005-03-16 : G. Lang    : Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries 
!  03.01 : 2005-07-21 : G. Lang    : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
!  04.01 : 2005-08-22 : G. Lang    : Lesen/Schreiben Delft3D-Gitternetz
!  04.02 : 2005-11-13 : G. Lang    : FM fuer ISBND(:,:)
!  04.03 : 2005-11-23 : G. Lang    : Erweiterungen fuer *.thd, *.lwl, *.ext
!  04.04 : 2005-12-28 : G. Lang    : zus. Fehlermeldung fuer refine_untrim integriert
!  04.05 : 2006-04-?? : G. Lang    : FM fuer optionale unerodierbare Tiefen huu(:), hvu(:) und hwu(:)
!  04.06 : 2006-07-26 : G. Lang    : FM in Zusammenhang mit neuer Funktion "deepen_h_grid" 
!  04.07 : 2006-08-31 : G. Lang    : FM in Zusammenhang mit neuer Komponente "dwlp" = "Depth_At_Water_Level_Points" (fuer Delft3D-Konversion)
!  04.08 : 2007-01-12 : G. Lang    : Modifikationen/Korrekturen wegen Dimensionsberechnung (9200)
!  04.09 : 2007-02-05 : G. Lang    : FM in Zusammenhang mit neuer Komponente "dg" sowie fuer FM in derive_nrand
!                                            
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Definition der Fehlermeldungen des Paketes "h_grid".
!! </OL>
!! <HR>
!
MODULE m_h_grid_errors
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten [ggf. entfernen]
  !
  ! [A.2] Basis-Modul mit Typ+Methoden "Fehlerbehandlung"
  USE b_error, ONLY :       &
       ! Routinen / Interfaces
       no_error,       &
       new_error,      &
       kill_error,     &
       set_error_ierr, &
       set_error_cerr
  !
  ! ---------------------------------------------------------------------
  ! [B]  Module des Paketes "SediMorph"
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Modul mit globalen Daten
  !
  USE m_h_grid_data, ONLY : all_errors ! 
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] Schnittstellen
  !
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
  !
  PUBLIC :: init_all_errors  ! Initialisieren der Fehlermeldungen
  PUBLIC :: clear_all_errors ! De-Initialisieren der Fehlermeldungen
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=15) , PRIVATE, PARAMETER :: c_modname='m_h_grid_errors' ! 
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
  SUBROUTINE init_all_errors_d &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='init_all_errors_d' !
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
               'Package "p_h_grid" ist nicht initialisiert\n'//&
               '--> INIT_h_grid ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Package "p_h_grid" ist schon initialisiert\n'//&
               '--> CLEAR_h_grid ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Keine Objekte "t_h_grid" vorhanden\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Kein Objekt "t_h_grid" mit Identifikationsnummer vorhanden\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Keine Arbeits-Objekte definiert\n'//&
               '--> Arbeits-Objekt mit SETUP_h_grid_WORK_OBJECT setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "<component-name>"\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "xy" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterknoten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterknoten = <...nv...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3520 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "nen" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal)   = <SIZE(..)>\n'//&
               'Anzahl Gitterpolygone = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3530 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "irand" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal)   = <SIZE(..)>\n'//&
               'Anzahl Gitterpolygone = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3540 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "ks" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal)   = <SIZE(..)>\n'//&
               'Anzahl Gitterpolygone = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3550 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "<component-name>" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterknoten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterknoten = <...nv...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3650 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "jb" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterkanten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ns...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3660 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "jt" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterkanten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ns...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3670 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "is" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal)   = <SIZE(..)>\n'//&
               'Anzahl Gitterpolygone = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3680 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "je" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterkanten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ns...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3690 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "ie" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3700 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "xc" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3710 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "xs" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterkanten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ns...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3720 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "dx" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterkanten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ns...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3730 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "dy" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterkanten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ns...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3740 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "aa" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterpolygone = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3750 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "<component-name>" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterkanten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ns...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3760 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "<component-name>" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterpolygone = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3770 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "xg" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterpolygone im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ne...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3780 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "ipobo" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterknoten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterpolygone = <...nv...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3785 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "knolg" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterknoten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterpolygone = <...nv...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3790 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "<array>" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl der Werte in "b_ms(:)" im akt. Arbeitsobjekt\n'//&
               'aktuell      = <actual>\n'//&
               'erforderlich = <required>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3800 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'die zweite Dimension der Feldgroesse von "enc" (lokal)\n'//&
               'weicht von 2 ab\n'//&
               'zweite Dimension = <dimension>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3810 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'die zweite Dimension der Feldgroesse von "dry" (lokal)\n'//&
               'weicht von 4 ab\n'//&
               'zweite Dimension = <dimension>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3820 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'die zweite Dimension der Feldgroesse von "isbnd" (lokal)\n'//&
               'weicht von 4 ab\n'//&
               'zweite Dimension = <dimension>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3830 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'die zweite bzw. dritte Dimension der Feldgroesse von "isdam" (lokal)\n'//&
               'weicht von 4 bzw. 2 ab\n'//&
               'zweite Dimension = <dimension2> [muss 4 sein]\n'//&
               'dritte Dimension = <dimension3> [muss 2 sein]\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 3840 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SETUP-Methoden\n'//&
               'Die Feldgroesse von "dg" (lokal) stimmt nicht ueberein\n'//&
               'mit der Anzahl Gitterkanten im akt. Arbeitsobjekt\n'//&
               'Feldgroesse (lokal) = <SIZE(..)>\n'//&
               'Anzahl Gitterkanten = <...ns...>\n'//&
               '--> Eingangsdaten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer neues Objekt "t_h_grid_list"\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 4002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente "object" in Listen-Objekt "t_h_grid_list"\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler Objekt "t_h_grid_list"\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente "object" in Listen-Objekt "t_h_grid_list"\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "<component-name>"\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "Identifikationsnummer"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "beschreibender Name"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "xy"\n'//&
               '... die Koordinaten zweier verschiedener Knoten sind identisch\n'//&
               'x-Koordinate   = <AktX>\n'//&
               'y-Koordinate   = <AktY>\n'//&
               'Knoten         = <Knoten1> und <Knoten2>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6015 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "xs"\n'//&
               '... die Koordinaten zweier verschiedener Knoten sind identisch\n'//&
               'x-Koordinate   = <AktX>\n'//&
               'y-Koordinate   = <AktY>\n'//&
               'Knoten         = <Knoten1> und <Knoten2>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nen"\n'//&
               '... ein Knoten ist in einem Polygon mehrfach vorhanden"\n'//&
               'Nr. lokaler Knoten 1 = <LokalKnoten1>"\n'//&
               'Nr. lokaler Knoten 2 = <LokalKnoten2>"\n'//&
               'Polygon Nr.          = <AktPolyNo>"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "irand"\n'//&
               'akt. Minimum   = <MinIrand>\n'//&
               'akt. Maximum   = <MaxIrand>\n'//&
               'erlaubt        = [0,1,2,...,<nx>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nv"\n'//&
               'aktuell = <ActNv>\n'//&
               'erlaubt = nv >= 3\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6041 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nv"\n'//&
               'Status : nv = <status-nv>, m = <status-m>, n = <status-n>\n'//&
               'Status : nv == m*n = <status-nv-m-n>\n'//&
               'nv     = <ActNv> [muss >= 9 sein]\n'//&
               'm      = <ActM>\n'//&
               'n      = <ActN>\n'//&
               'm*n    = <ActMN> [muss gleich nv sein]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "ns"\n'//&
               'aktuell = <ActNs>\n'//&
               'erlaubt = ns >= 3\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6055 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nsi"\n'//&
               'aktuell = <ActNsi>\n'//&
               'erlaubt = nsi >= 0 und nsi <= <ActNs> \n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6056 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nsf"\n'//&
               'aktuell = <ActNsf>\n'//&
               'erlaubt = nsf >= <ActNsi> und nsf <= <ActNs> \n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "ne"\n'//&
               'aktuell = <ActNe>\n'//&
               'erlaubt = ne >= 1\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "ks"\n'//&
               'akt. Minimum   = <MinKs>\n'//&
               'akt. Maximum   = <MaxKs>\n'//&
               'erlaubt        = [3,4]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "hv"\n'//&
               'akt. Minimum   = <MinHv>\n'//&
               'akt. Maximum   = <MaxHv>\n'//&
               'erlaubt        = -10001.0 <= hv <= 11022.0\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6090 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nrand"\n'//&
               'aktuell = <ActNrand>\n'//&
               'erlaubt = 3 <= nrand\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6094 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nptfr"\n'//&
               'aktuell = <nptfr>\n'//&
               'erlaubt = 1 <= nptfr <= <nv>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6095 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nptir"\n'//&
               'aktuell = <nptir>\n'//&
               'erlaubt = 1 <= nptir <= <nv>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "time"\n'//&
               'aktuell = <ActTime>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nbc"\n'//&
               'aktuell = <ActNbc>\n'//&
               'erlaubt = [0,<MaxNbc>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6120 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "hland"\n'//&
               'aktuell = <ActHland>\n'//&
               'erlaubt = hland <= -10000.0\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6130 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "angle"\n'//&
               'aktuell = <ActAngle>\n'//&
               'erlaubt = [<MinAngle>,<MaxAngle>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6140 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "text"\n'//&
               'mindestens eine Komponente dieses Feldes ist leer\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6150 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "jb"\n'//&
               'Nummer eines Startknotens ist nicht korrekt\n'//&
               'aktuell = [<ActMinStart>,<ActMaxStart>]\n'//&
               'erlaubt = [<AllMinStart>,<AllMaxStart>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6160 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "jt"\n'//&
               'Nummer eines Endknotens ist nicht korrekt\n'//&
               'aktuell = [<ActMinStart>,<ActMaxStart>]\n'//&
               'erlaubt = [<AllMinStart>,<AllMaxStart>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6170 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "is"\n'//&
               'Nummer einer Kante ist nicht korrekt\n'//&
               'Polygon = <ActPolygon>\n'//&
               'aktuell = [<ActMinStart>,<ActMaxStart>]\n'//&
               'erlaubt = [<AllMinStart>,<AllMaxStart>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6180 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "je"\n'//&
               'Nummer eines Polygons ist nicht korrekt ...\n'//&
               'Kante j   = <ActKante>\n'//&
               'aktuell   = [<ActMinStart>,<ActMaxStart>]\n'//&
               'erlaubt   = [<AllMinStart>,<AllMaxStart>]\n'//&
               '... oder je(j,1) ist Null (darf nicht (mehr) sein)\n'//&
               'je(j,1:2) = <ActJe1>, <ActJe2>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6190 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "ie"\n'//&
               'Nummer eines Polygons ist nicht korrekt\n'//&
               'Polygon = <ActPolygon>\n'//&
               'aktuell = [<ActMinStart>,<ActMaxStart>]\n'//&
               'erlaubt = [<AllMinStart>,<AllMaxStart>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6220 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "dx"\n'//&
               'Abstand zwischen Polygonzentren ist zu klein\n'//&
               'aktuell = <ActMinAbstand>\n'//&
               'erlaubt = <AllMinAbstand>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6230 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "dy"\n'//&
               'Laenge der Kanten ist zu klein\n'//&
               'aktuell = <ActMinLaenge>\n'//&
               'erlaubt = <AllMinLaenge>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6240 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "aa"\n'//&
               'Laenge der Kanten ist zu klein\n'//&
               'aktuell = <ActMinFlaeche>\n'//&
               'erlaubt = <AllMinFlaeche>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6250 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "<component-name>"\n'//&
               'akt. Minimum   = <Min>\n'//&
               'akt. Maximum   = <Max>\n'//&
               'erlaubt        = -10001.0 <= hu <= 11022.0\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6260 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "hw"\n'//&
               'akt. Minimum   = <MinHw>\n'//&
               'akt. Maximum   = <MaxHw>\n'//&
               'erlaubt        = <HLand> <  hw <= 11022.0\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6270 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "nr"\n'//&
               'aktuell = <AktNr>\n'//&
               'erlaubt = [0,<MaxNr>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6275 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "ncsize"\n'//&
               'aktuell = <AktNr>\n'//&
               'erlaubt = [0,<MaxCPU>]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6280 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "ipobo"\n'//&
               'akt. Minimum   = <MinIpobo>\n'//&
               'akt. Maximum   = <MaxIpobo>\n'//&
               'erlaubt        = 0 <= ipobo <= <AllMaxIpobo>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6285 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "knolg"\n'//&
               'akt. Minimum   = <MinKnolg>\n'//&
               'erlaubt        = 1 <= knolg \n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6290 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "dxmin"\n'//&
               'aktuell = <AktDxmin>\n'//&
               'erlaubt = 0.0 <= dxmin\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "m"\n'//&
               'aktuell = <AktM> [muss groesser als 2 sein]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6310 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "n"\n'//&
               'aktuell = <AktN> [muss groesser als 2 sein]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6320 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "enc"\n'//&
               'Status : M_min = <ok-mn>, M_max = <ok-mx>, N_min = <ok-nn>, N_max = <ok-nx>\n'//&
               'Daten  : M_min = <mn>, M_max = <mx>, M+1 = <m>\n'//&
               '         N_min = <nn>, N_max = <nx>, N+1 = <n>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6330 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "dry"\n'//&
               'Status : M_min = <ok-mn>, M_max = <ok-mx>, N_min = <ok-nn>, N_max = <ok-nx>\n'//&
               '   size(dry,2) = <ok-dry>\n'//&
               'Daten  : M_min = <mn>, M_max = <mx>, M+1 = <m>\n'//&
               '         N_min = <nn>, N_max = <nx>, N+1 = <n>\n'//&
               '   size(dry,2) = <size-dry> [muss 4 sein]\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6340 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "isbnd"\n'//&
               'Status     : ne = <ok-ne>, mn = <ok-mn>, mx = <ok-mx> \n'//&
               'Daten      : ne = <ne>, min(isbnd) = <mn>, max(isbnd) = <mx>\n'//&
               '  size(isbnd,1) = <size-isbnd>\n'//&
               '  size(bnd)     = <size-bnd>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6350 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "bnd", Randabschnitt Nr = <nr>\n'//&
               'Status     : bdry_type = <ok-bd>, data_type = <ok-da> \n'//&
               '             grid_coor = <ok-m>, <ok-n> \n'//&
               'Daten      : bdry_type = <bd>, data_type = <da>\n'//&
               '             grid_coor = (<m1>,<n1>) --> (<m2>,<n2>)\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6360 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "thd", duenner Damm Nr = <nr>\n'//&
               'Status     :      type = <ok-ty>\n'//&
               '             grid_coor = <ok-m>, <ok-n>\n'//&
               'Daten      :      type = <ty>\n'//&
               '             grid_coor = (<m1>,<n1>) --> (<m2>,<n2>)\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6370 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "lwl", lokales Wehr Nr = <nr>\n'//&
               'Status     :      type = <ok-ty>\n'//&
               '             grid_coor = <ok-m>, <ok-n>\n'//&
               '              friction = <ok-friction>\n'//&
               'Daten      :      type = <ty>\n'//&
               '             grid_coor = (<m1>,<n1>) --> (<m2>,<n2>)\n'//&
               '              friction = <friction>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6380 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "ext", 2D-Wehr Nr = <nr>\n'//&
               'Status     :      type = <ok-ty>\n'//&
               '             grid_coor = <ok-m>, <ok-n>\n'//&
               '              friction = <ok-friction>\n'//&
               'Daten      :      type = <ty>\n'//&
               '             grid_coor = (<m1>,<n1>) --> (<m2>,<n2>)\n'//&
               '              friction = <friction>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6390 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "isdam", Komponente = <dam>\n'//&
               'Status     : ne = <ok-ne>, mn = <ok-mn>, mx = <ok-mx> \n'//&
               '             kn = <ok-kn>, kx = <ok-kx> \n'//&
               'Daten      : ne = <ne>, \n'//&
               '             min(isdam(:,:,1)) = <mn>, max(isdam(:,:,1)) = <mx>\n'//&
               '             min(isdam(:,:,2)) = <kn>, max(isdam(:,:,2)) = <kx>\n'//&
               '  size(isdam,1) = <size-isdam1> \n'//&
               '  size(isdam,2) = <size-isdam2> \n'//&
               '  size(isdam,3) = <size-isdam3> \n'//&
               '  size(ndam)    = <size-ndam> [thd|lwl|ext] \n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6400 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "<component-name>"\n'//&
               'die Lage des unerodierbaren Horizonts wird unterschritten\n'//&
               'nicht erfuellt = ALL( <comp-1> >= <comp-2> )\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6410 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "dg"\n'//&
               'Laenge der Kanten ist zu klein\n'//&
               'aktuell = <ActMinLaenge>\n'//&
               'erlaubt = <AllMinLaenge>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'Typ-Komponente = "<component-name>" wurde nicht allokiert\n'//&
               '--> Komponente muss einmal mit einer Methode definiert werden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6520 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Belegung der Komponente ncsize weist auf paralleles Rechnen (TELEMAC) hin"\n'//&
               'Typ-Komponente = "nptfr" wurde trotzdem nicht allokiert\n'//&
               '--> entweder "nptfr"  mit einer Methode belegen, \n'//&
               '    oder ncsize mit setup_h_grid_ncsize(0) setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6530 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Belegung der Komponente ncsize weist auf paralleles Rechnen (TELEMAC) hin"\n'//&
               'Typ-Komponente = "nptir" wurde trotzdem nicht allokiert\n'//&
               '--> entweder "nptir"  mit einer Methode belegen, \n'//&
               '    oder ncsize mit setup_h_grid_ncsize(0) setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6540 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Belegung der Komponente ncsize weist auf paralleles Rechnen (TELEMAC) hin"\n'//&
               'Typ-Komponente = "knolg" wurde trotzdem nicht allokiert\n'//&
               '--> entweder "knolg"  mit einer Methode belegen, \n'//&
               '    oder ncsize mit setup_h_grid_ncsize(0) setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6550 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Komponente "b_ms(:)" enthaelt fehlerhafte Werte\n'//&
               'Wertebereich   : <min> bis <max> [ muss > 0 sein ]\n'//&
               'kontinuierlich : <ok> \n'//&
               '--> Komponente korrekt setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6560 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Komponente "b_ss(:)" ist nicht korrekt"\n'//&
               'assoziiert  : b_ms=<a-ms>, b_ss=<a-ss>\n'//&
               'Feldgroesse : b_ms=<s-ms>, b_ss=<s-ss>\n'//&
               '--> Komponente korrekt setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6570 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Komponente "b_s(:)" ist nicht korrekt"\n'//&
               'assoziiert  : b_ms=<a-ms>, b_s=<a-s>\n'//&
               'Feldgroesse : b_ms=<s-ms>, b_s=<s-s>\n'//&
               '--> Komponente korrekt setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6580 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Komponente "b_v(:)" ist nicht korrekt"\n'//&
               'assoziiert  : b_ms=<a-ms>, b_v=<a-v>\n'//&
               'Feldgroesse : b_ms=<s-ms>, b_v=<s-v>\n'//&
               '--> Komponente korrekt setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6590 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Komponente "b_t(:)" ist nicht korrekt"\n'//&
               'assoziiert  : b_ms=<a-ms>, b_t=<a-t>\n'//&
               'Feldgroesse : b_ms=<s-ms>, b_t=<s-t>\n'//&
               '--> Komponente korrekt setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6600 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_h_grid"\n'//&
               'die Komponente "dwlp" ist nicht korrekt"\n'//&
               'aktuell : <act>\n'//&
               'erlaubt : [<req1>,<req2>,<req3>]\n'//&
               '--> Komponente korrekt setzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Fusszeilen\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_h_grid"\n'//&
               'Typ-Komponente = <component-name>\n'//&
               'Aktion         = <action>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "p_h_grid_ui"\n'//&
               '--> Code / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer ID-Nummern\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Datei-Type\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Datei-Code\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9004 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Fortran-Form\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9005 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Fortran-Access\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9006 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren von Feld fuer implementierte Fortran-Delim\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Berechnungsverfahren fuer einen klassischen Dimensionsbezeichner \n'//&
               'ist noch nicht implementiert \n'//&
               'akt. Name = <AktDimName> \n'//&
               '--> Code in Modulprozedur "get_max_old_dimensions_0" ergaenzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               'Fehler beim Lesen von Daten aus der Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'beim Lesen von ...\n'//&
               '... <LokalerFehlerText>\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23101 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii,sequential)\n'//&
               'Fehler beim Lesen einer Zeile aus der Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23102 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               '"nrand" hat einen fehlerhaften Wert\n'//&
               'aktuell = <AktNrand>\n'//&
               'erlaubt = groesser als 2\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23103 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               '"ninnen" hat einen fehlerhaften Wert\n'//&
               'aktuell = <AktNinnen>\n'//&
               'erlaubt = groesser oder gleich 0\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23104 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) xy(:,:), depth(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23105 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) xy(:,:), depth(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23106 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               '"nelem" hat einen fehlerhaften Wert\n'//&
               'aktuell = <AktNelem>\n'//&
               'erlaubt = groesser als 0\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23107 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) nen(:,:), irand(:), ks(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23108 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) nen(:,:), irand(:), ks(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW/VC",ascii,sequential)\n'//&
               'Fehler beim Lesen von Daten aus der Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'beim Lesen von ...\n'//&
               '... <LokalerFehlerText>\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23201 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_VC",ascii,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) xy(:,:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23202 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_VC",ascii,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) xy(:,:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23203 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_VC",ascii,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) ks(:),xc(:,:),nen(:,:),is(:,:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23204 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_VC",ascii,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) ks(:),xc(:,:),nen(:,:),is(:,:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23205 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_VC",ascii,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) hu(:),jb(:),jt(:),je(:,:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23206 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_VC",ascii,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) hu(:),jb(:),jt(:),je(:,:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23207 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW",ascii,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) text(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23208 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW",ascii,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) text(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23209 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW",ascii,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) xy(:,:),hv(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23210 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW",ascii,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) xy(:,:),hv(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23211 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW",ascii,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) jb(:),jt(:),je(:,:),hu(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23212 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW",ascii,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) jb(:),jt(:),je(:,:),hu(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23213 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW",ascii,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) ks(:),is(:,:),nen(:,:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23214 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("UNTRIM_BAW",ascii,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) ks(:),is(:,:),nen(:,:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Lesen von Daten aus der Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'beim Lesen von ...\n'//&
               '... <LokalerFehlerText>\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23301 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) cnev(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               '--> Code / Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23302 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) cnev(:)\n'//&
               '--> Code / Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23303 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'erforderliche Variablen sind nicht in der Datei entahlten\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'Variable 1  = <VariableName1> / <Exist1>\n'//&
               'Variable 2  = <VariableName2> / <Exist2>\n'//&
               'Variable 3  = <VariableName3> / <Exist3>\n'//&
               'Variable 1 oder Variablen 1+2 muessen vorhanden sein\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23304 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) p_ks(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23305 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) p_ks(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23306 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) p_nen(:,:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23307 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) p_nen(:,:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23308 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) p_ipobo(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23308 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) p_ipobo(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23309 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) p_ipobo(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23310 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) p_xy(:,:),r_xy(:,:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23311 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) p_xy(:,:),r_xy(:,:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23312 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) r_tm(:,:),p_hv(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               'Dimension 2 = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23313 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) r_tm(:,:),p_hv(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23314 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ/WRITE-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Lesen von Daten aus der Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'mindestens einer der Groessen NELEM, NPOIN oder NDP ist fehlerhaft\n'//&
               'aktuell NELEM = <AktNelem> , erforderlich NELEM >= 1\n'//&
               'aktuell NPOIN = <AktNpoin> , erforderlich NPOIN >= 3\n'//&
               'aktuell NDP   = <AktNdp>   , erforderlich NDP   >= 3\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23315 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Allokieren von (lokal) p_knolg(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23316 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim De-Allokieren von (lokal) p_knolg(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 23400 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: READ-Methoden ("DELFT3D",ascii,sequential)\n'//&
               'Fehler beim Lesen von Daten aus der Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'beim Lesen von ...\n'//&
               '... <LokalerFehlerText>\n'//&
               '--> Daten in Gitterdatei pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 24100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: WRITE-Methoden ("GITTER05",ascii/binary,sequential)\n'//&
               'Fehler beim Schreiben von Daten in die Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'beim Schreiben von ...\n'//&
               '... <LokalerFehlerText>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 24200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: WRITE-Methoden ("UNTRIM_BAW/VC",ascii,sequential)\n'//&
               'Fehler beim Schreiben von Daten in die Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'beim Schreiben von ...\n'//&
               '... <LokalerFehlerText>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 24300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: WRITE-Methoden ("SELAFIN",binary,sequential)\n'//&
               'Fehler beim Schreiben von Daten in die Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'beim Schreiben von ...\n'//&
               '... <LokalerFehlerText>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 24400 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: WRITE-Methoden ("DELFT3D",ascii,sequential)\n'//&
               'Fehler beim Schreiben von Daten in die Gitterdatei\n'//&
               'Name Gitterdatei = <DateiName>\n'//&
               'beim Schreiben von ...\n'//&
               '... <LokalerFehlerText>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'nur fuer Gitter des Typs UNTRIM_BAW/_VC kann eine Red-Black-Sortierung\n'//&
               'durchgefuehrt werden\n'//&
               'akt. Datei-Variante-No   = <DateiVarianteNo> \n'//&
               'akt. Datei-Variante-Name = <DateiVarianteName> \n'//&
               '--> Methode nicht fuer akt. Gitter rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim Allokieren von (lokal) nnrr(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim De-Allokieren von (lokal) nnrr(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim Schreiben der Anzahl der roten/schwarzen Polygone\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26004 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'nur Gitter der Typen GITTER05/SELAFIN koennen in UNTRIM-Gitter konvertiert werden\n'//&
               'akt. Datei-Variante-No   = <DateiVarianteNo> \n'//&
               'akt. Datei-Variante-Name = <DateiVarianteName> \n'//&
               '--> Methode nicht fuer akt. Gitter rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26005 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim Schreiben der Anzahl der Kanten/Innenkanten\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26006 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'die berchnete und iterativ ermittelte Anzahl der \n'//&
               'Innenkanten sind zueinander nicht konsistent     \n'//&
               'Innenkanten (derive_nsi) = <ActDeriveNsi>        \n'//&
               'Innenkanten (l_nsi     ) = <ActLocalNsi>         \n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1 
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26105 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim Allokieren von (lokal) l_rand_offen(:)\n'//&
               'Dimension 1 = <AktDim1>\n'//&
               '--> Code/Speicherbelegung pruefen' )
       END IF
       ic = ic + 1 
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26106 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim De-Allokieren von (lokal) l_rand_offen(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1 
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26107 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim Allokieren von (lokal) irand(:), iv_old(:)\n'//&
               'Dimension irand  = <AktDim1>\n'//&
               'Dimension iv_old = <AktDim2>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26108 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim Schreiben eines Kommentares auf Printerausgabe\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1 
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26109 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim Allokieren von (lokal) tmp_text(:)\n'//&
               'Dimension tmp_text  = <AktDim1>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'nur Gitter des Typs SELAFIN koennen in GITTER05-Gitter konvertiert werden\n'//&
               'akt. Datei-Variante-No   = <DateiVarianteNo> \n'//&
               'akt. Datei-Variante-Name = <DateiVarianteName> \n'//&
               '--> Methode nicht fuer akt. Gitter rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26111 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Im SELAFIN-Gitter sind alle drei Knoten eines Polygons Randknoten\n'//&
               'Nummer des Knotens i = <KnotenI> \n'//&
               'Nummer des Knotens j = <KnotenJ> \n'//&
               'Nummer des Knotens k = <KnotenK> \n'//&
               '--> Gitter checken' )
       END IF
       ic = ic + 1 
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26112 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim De-Allokieren von (lokal) irand, iv_old, tmp_text\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1 
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26113 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'Fehler beim De-Allokieren von (lokal) tmp_text(:)\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&
               'nur fuer Gitter des Typs UNTRIM_BAW/_VC kann NSI berechnet werden\n'//&
               'akt. Datei-Variante-No   = <DateiVarianteNo> \n'//&
               'akt. Datei-Variante-Name = <DateiVarianteName> \n'//&
               '--> Methode nicht fuer akt. Gitter rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&  
               'nur fuer Gitter des Typs UNTRIM_BAW/_VC kann eine Terrassierung\n'//&
               'durchgefuehrt werden\n'//&
               'akt. Datei-Variante-No   = <DateiVarianteNo> \n'//&
               'akt. Datei-Variante-Name = <DateiVarianteName> \n'//&
               '--> Methode nicht fuer akt. Gitter rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26400 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&  
               'nur Gitter des Typs UNTRIM_BAW/_VC koennen durch Vierteilung\n'//&
               'weiter verfeinert werden\n'//&
               'akt. Datei-Variante-No   = <DateiVarianteNo> \n'//&
               'akt. Datei-Variante-Name = <DateiVarianteName> \n'//&
               '--> Methode nicht fuer akt. Gitter rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 26500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: CONVERT-Methoden\n'//&  
               'Vertiefungsoperation fuer aktuelles Gitter nicht verfuegbar\n'//&
               'akt. Datei-Variante-No   = <DateiVarianteNo> \n'//&
               'akt. Datei-Variante-Name = <DateiVarianteName> \n'//&
               '--> Methode nicht fuer akt. Gitter rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 50000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: AUX-Methoden\n'//&
               'externes ein-dimensionales Feld ist nicht korrekt dimensioniert\n'//&
               'Name externes Feld      = <ExtFieldName>\n'//&
               'Name interne Komponente = <IntComponentName>\n'//&
               'aktuell                 = <AktDim>\n'//&
               'erforderlich            = <ReqDim>\n'//&
               'Operator "eq"           = <Op>\n'//&
               '--> Externes Feld korrekt dimensioniert uebergeben' )
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
               '--> erforderliche Datei-Variante in Modul "p_h_grid_ui" implementieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -23001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private READ-Methoden\n'//&
               'erforderliche Datei-Variante ist nicht implementiert\n'//&
               'req: Datei-Variante = <DateiVarianteNo> \n'//&
               'akt:                  [1,<MaxDateiVarianteNo>] \n'//&
               '--> Code in Modulprozedur "read_h_grid_0" ("p_h_grid_ui") ergaenzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -24001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private WRITE-Methoden\n'//&
               'erforderliche Datei-Variante ist nicht implementiert\n'//&
               'req: Datei-Variante = <DateiVarianteNo> \n'//&
               'akt:                  [1,<MaxDateiVarianteNo>] \n'//&
               '--> Code in Modulprozedur "write_h_grid_0" ("p_h_grid_ui") ergaenzen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -25000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private DERIVE-Methoden\n'//&
               'die angeforderte Groesse kann fuer das aktuelle Gitter nicht berechnet werden\n'//&
               'angeforderte Groesse = <AngeforderteGroesse> \n'//&
               'Datei-Variante-No    = <DateiVarianteNo> \n'//&
               'Datei-Variante-Name  = <DateiVarianteName> \n'//&
               '--> Code ergaenzen oder Methode nicht rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -25001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private DERIVE-Methoden\n'//&
               'Fehler beim Allokieren von lokalen Feldern\n'//&
               'zu allokierende Groesse(n) = <AllocGroessen> \n'//&
               '--> Code / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -25002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private DERIVE-Methoden\n'//&
               'Fehler beim De-Allokieren von lokalen Feldern\n'//&
               'zu de-allokierende Groesse(n) = <DeAllocGroessen> \n'//&
               '--> Code / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -25003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private DERIVE-Methoden\n'//&
               'erforderliche Ausgangsgroessen zum Berechnen von Daten nicht vorhanden\n'//&
               'angeforderte Groesse = <AngeforderteGroesse> \n'//&
               'fehlende Groesse     = <FehlendeGroesse> \n'//&
               '--> Code / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -25004 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private DERIVE-Methoden\n'//&
               'zur Flaechenberechnung wurden die Polygone in (Teil-) Dreiecke umgeformt\n'//&
               'die (Teil-) Flaeche im <NummerPolygon>-ten Polygon ist negativ\n'//&
               'Flaechen : aa = <Flaeche>, Teil = <TeilFlaeche>\n'//&
               'Knoten 1 : nr = <nummer-1>, x = <x-1>, y = <y-1>\n'//&
               'Knoten 2 : nr = <nummer-2>, x = <x-2>, y = <y-2>\n'//&
               'Knoten 3 : nr = <nummer-3>, x = <x-3>, y = <y-3>\n'//&
               'Knoten 4 : nr = <nummer-4>, x = <x-4>, y = <y-4>\n'//&
               '-> die Knoten des Polygons sind nicht entgegen dem Uhrzeigersinn sortiert\n'//&
               '-> ... oder das Polygon ist nicht konvex\n'//&
               '--> Code / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -25005 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private DERIVE-Methoden\n'//&
               'Knotenverzeichnis der Kanten defekt!\n'//&
               '  Kante         #= <i_edge>\n'//&
               '  Erster Knoten #= <no1_nr> ; Zweiter Knoten #= <no2_nr>\n'//&
               'Die Knotennummern der Kante muessen zwischen 1 und <nof_nodes> liegen.\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -25006 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private DERIVE-Methoden\n'//&
               'die angeforderte Groesse kann nicht berechnet werden, da die Anzahl\n'//&
               'entweder die Anzahl der M- oder der N-Punkte unbekannt sind\n'//&
               'Status               : M = <ok-m>, N = <ok-n>\n'//&
               'angeforderte Groesse = <AngeforderteGroesse> \n'//&
               'Datei-Variante-No    = <DateiVarianteNo> \n'//&
               'Datei-Variante-Name  = <DateiVarianteName> \n'//&
               '--> Code ergaenzen oder Methode nicht rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -25100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private DERIVE-Methoden\n'//&
               'der implementierte Algorithmus zum Bestimmen von NRAND\n'//&
               'wurde nicht allgemein genug formuliert\n'//&
               '--> Code ergaenzen oder Methode nicht rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -26000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private READ-Methoden [DELFT3D]\n'//&
               'Bezeichnung der zu tauschenden Dateiendung nicht in Name enthalten\n'//&
               'Pfadname   = <pathname>\n'//&
               'Dateiname  = <filename>\n'//&
               'Endung alt = <extension-old>\n'//&
               'Endung neu = <extension-new>\n'//&
               '--> Parameter bei Aufruf von create_delft3d_file modifizieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: Private OMI-Methoden\n'//&
               'Komponente mit OpenMI-konformen Daten kann nicht erzeugt werden\n'//&
               'Komponente    = <omi-component>\n'//&
               'Varianten-Nr. = <no>, erforderlich = <req-no>\n'//&
               '--> Code erweitern' )
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
  SUBROUTINE clear_all_errors_d &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='clear_all_errors_d' !
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
END MODULE m_h_grid_errors
! TailOfPackageModule -----------------------------------------------------
