! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Systembefehle generieren und deren Aufruf an C-Routinen weiterleiten</h2>
!! @author Peter Schade
!! @version 1.12 vom 08/15/05, Quellcode: mod_b_cmds.f90
!! <HR>
!! Generating and calling system commands <BR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2001 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
! 01.01 : 2002-04-30 : P. Schade : Uebertragen aus den von Guntram Sei&szlig;
! 01.02 : 2002-05-14 : P. Schade : compress_cmds und uncompress_cmds erweitert um Kompressionsart ZIP
! 01.03 : 2002-05-14 : P. Schade : HTML-Doku verbessert
! 01.04 : 2002-05-15 : P. Schade : in uncompress_cmds - COMPRESS Dateinamen vertauscht
! 01.05 : 2002-05-28 : G. Lang   : Korrektur in MERGE fuer trc_op
! 01.06 : 2002-06-05 : G. Lang   : ok_initialised wurde modifiziert
! 01.07 : 2002-06-12 : G. Lang   : INIT/CLEAR und SETUP_*_PRN_LUN, SETUP_*_TRC_LUN modifiziert
! 01.08 : 2003-02-13 : P. Schade : test_cmds in HP main_test_cmds.f90 verlegt
! 01.09 : 2003-03-13 : G. Lang   : Anpassungen TV12 vom Dez. 2002
! 01.10 : 2003-03-19 : P. Schade : all_errors(:) als dynamisch allokierbares Feld
! 01.10 : 2003-03-19 : P. Schade : in init_cmds_all_errors FM korrigiert
! 01.11 : 2005-08-15 : G. Lang   : ANSI-Konformitaet
!
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Methoden zum Generieren und Aufrufen von UNIX-Systembefehlen. Die Fortran- 
!! Methoden rufen C-Routinen, die ihrerseits C-eigene Systembefehle rufen. Diese
!! sind unabhaengig vom Maschinentyp vorhanden.
!! 
!! <HR>
!!                                                                  <BR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul definiert keine Basisdatentypen.
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_cmds mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_cmds mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   <BR>
!!          andere nicht. 
!!          Routinen die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,  <BR>
!!          ob das Modul korrekt initialisiert wurde (ok_initialised)  <BR>
!!                                                                    <BR>
!! <HR>
!! Allgemein             [  0000 bis  0999 ]                        <BR>
!! 00000 = kein Fehler                                              <BR> 
!! 00001 = Modul ist nicht initialisiert.                           <BR>
!! 00002 = Modul ist schon initialisiert.                           <BR>
!! <HR>
!! INIT-Methoden         [  1000 bis  1999 ]                        <BR>
!! <HR>
!! CLEAR-Methoden        [  2000 bis  2999 ]                        <BR>
!! <HR>
!! SETUP-Methoden        [  3000 bis  3999 ]                        <BR>
!! <HR>
!! PRINT-Methoden        [  7000 bis  7999 ]                        <BR>
!! 07500 = Drucken der statischen Daten (ohne Fehlermeldungen)      <BR>
!! <HR>
!! modul-spezifische Methoden   [      < 0 ]                        <BR>
!
MODULE b_cmds
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
       init_error,          &
       clear_error,         &
       no_error,            &
       any_error,           &
       new_error,           &
       kill_error,          &
       print_error,         &
       print_error_act,     &
       setup_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
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
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
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
  !! ggf. Allokieren der statischen Daten des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_cmds
     MODULE PROCEDURE init_cmds_d ! 
  END INTERFACE
  !! ggf. De-Allokieren der statischen Daten des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_cmds
     MODULE PROCEDURE clear_cmds_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_cmds_prn_lun
     MODULE PROCEDURE setup_cmds_prn_lun_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_cmds_trc_lun
     MODULE PROCEDURE setup_cmds_trc_lun_d ! 
  END INTERFACE
  !! Alle statischen Daten des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_cmds_static
     MODULE PROCEDURE print_cmds_static_d ! 
  END INTERFACE
  !! Alle Fehlermeldungen des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_cmds_all_errors
     MODULE PROCEDURE print_cmds_all_errors_d ! 
  END INTERFACE
  !! Komprimieren einer Datei mittels compress, zip oder gzip. 
  INTERFACE compress_cmds
     MODULE PROCEDURE compress_cmds_d
  END INTERFACE
  !! Kopieren einer Datei.
  INTERFACE cp_cmds
     MODULE PROCEDURE cp_cmds_d
  END INTERFACE
  !! Wert einer Umgebungsvariablen abfragen.
  INTERFACE getenvv_cmds
     MODULE PROCEDURE getenvv_cmds_d
  END INTERFACE
  !! Erzeugen eines Verzeichnisses.
  INTERFACE mkdir_cmds
     MODULE PROCEDURE mkdir_cmds_d
  END INTERFACE
  !! Dateinamen suchen und Ergebnisse in Liste eintragen.
  INTERFACE mklist_cmds
     MODULE PROCEDURE mklist_cmds_d
  END INTERFACE
  !! Verschieben einer Datei.
  INTERFACE mv_cmds
     MODULE PROCEDURE mv_cmds_d
  END INTERFACE
  !! Pfadtrennzeichen abfragen.
  INTERFACE pathdelim_cmds
     MODULE PROCEDURE pathdelim_cmds_d
  END INTERFACE
  !! L&ouml;schen einer Datei.
  INTERFACE rm_cmds
     MODULE PROCEDURE rm_cmds_d
  END INTERFACE
  !! L&ouml;schen eines Verzeichnisses.
  INTERFACE rmdir_cmds
     MODULE PROCEDURE rmdir_cmds_d
  END INTERFACE
  !! Dekomprimieren einer Datei mittels compress, unzip oder gunzip.
  INTERFACE uncompress_cmds
     MODULE PROCEDURE uncompress_cmds_d
  END INTERFACE
  !! allgemeines UNIX-Kommando ausfuehren.
  INTERFACE unixcmd_cmds
     MODULE PROCEDURE unixcmd_cmds_d
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! [C.6] Operatoren
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_cmds               ! Initialisieren (Modul)
  PUBLIC :: clear_cmds              ! De-Initialisieren (Modul)
  PUBLIC :: setup_cmds_prn_lun      ! Setzen prn_lun 
  PUBLIC :: setup_cmds_trc_lun      ! Setzen trc_lun 
  PUBLIC :: print_cmds_static       ! Drucken aller statischen Daten
  PUBLIC :: print_cmds_all_errors   ! Drucken aller (moeglichen) Fehlermeldungen
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  ! bis auf mkdir_cmds sind die folgenden Befehle auf einem DOS-PC nicht ausfuehrbar
  ! 
  PUBLIC :: compress_cmds           ! Komprimieren einer Datei mittels compress, zip oder gzip
  PUBLIC :: cp_cmds                 ! Kopieren einer Datei
  PUBLIC :: getenvv_cmds            ! Wert einer Umgebungsvariablen abfragen
  PUBLIC :: mkdir_cmds              ! Erzeugen eines Verzeichnisses
  PUBLIC :: mklist_cmds             ! Dateinamen suchen und Ergebnisse in Liste eintragen
  PUBLIC :: mv_cmds                 ! Verschieben einer Datei
  PUBLIC :: pathdelim_cmds          ! Pfadtrennzeichen abfragen
  PUBLIC :: rm_cmds                 ! Loeschen einer Datei
  PUBLIC :: rmdir_cmds              ! Loeschen eines Verzeichnisses
  PUBLIC :: uncompress_cmds         ! Dekomprimieren einer Datei mittels compress, unzip oder gunzip
  PUBLIC :: unixcmd_cmds            ! allgemeines UNIX-Kommando ausfuehren
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'b_cmds' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) ,ALLOCATABLE,  SAVE :: all_errors(:)! 
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
  ! [D.4.1] Interfaces zu Brueckenroutinen, die in C geschrieben sind
!  INTERFACE 
!     FUNCTION sys_cmd_cmds(istring, inlentr)
!       INTEGER, INTENT(IN) :: inlentr
!       INTEGER, INTENT(IN) :: istring(inlentr+1)
!     END FUNCTION sys_cmd_cmds
!  END INTERFACE
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
  !! Allokieren/Initialisieren der statischen Daten des Moduls
  SUBROUTINE init_cmds_d  ( )
    !
    USE b_error, ONLY : DEBUG_b
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_cmds" version 1.12 of 08/15/05                      '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_cmds_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] ggf. weitere Initialsierungsmethoden rufen
       ! [1.7] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_cmds_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls
  SUBROUTINE clear_cmds_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_cmds_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] ggf. weitere De-Initialsierungsmethoden rufen
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.5] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.5.1] ggf. weitere Module de-initialisieren
       ! [1.5.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_cmds_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden
  SUBROUTINE setup_cmds_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_cmds_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_cmds_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden
  SUBROUTINE setup_cmds_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_cmds_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_cmds_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Drucken aller statischen Daten eines Moduls
  SUBROUTINE print_cmds_static_d ( )
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='print_cmds_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_cmds_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_cmds         ',/ &
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
  END SUBROUTINE print_cmds_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls
  SUBROUTINE print_cmds_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_cmds_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_cmds_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC: Modulspezifische-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! Ausf&uuml;hren eines in einem String uebergebenen UNIX-Kommandos. <BR>
  !! Das Kommando wird an eine C-Funktion &uuml;bergeben, wo der eigentliche <BR>
  !! Systembefehl ausgef&uuml;hrt wird. <BR>
  !! Der Fehlerstatus wird als Integer zurueckgegeben.
  FUNCTION unixcmd_cmds_d (c_string, c_called_by) &
       RESULT(ierr2)
    !! Zeichenkette des UNIX-Befehls, der transformiert an die C-Funktion uebergeben wird
    CHARACTER(LEN=*), INTENT(IN) :: c_string
    !! rufende Subroutine/Funktion
    CHARACTER(LEN=*), INTENT(IN) :: c_called_by
    !! Rueckgabewert: Fehlernummer aus C-Routine sys_cmd_cmds oder vom Allokieren
    INTEGER :: ierr2
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='unixcmd_cmds_d' 
    !!
    CHARACTER(LEN=10) c_ierr2
    !! Name der C-Funktion, die den eigentlichen Systembefehl absetzt
    INTEGER, EXTERNAL:: sys_cmd_cmds
    !! Zaehler
    INTEGER ::  i
    !! Stringlaengen
    INTEGER :: inlentr
    !! ASCII-Code-Nummern von c_string (Nummer des zusaetzliches Zeichens = 0)
    INTEGER, ALLOCATABLE :: istring(:)
    !
    ! [ 1.0] Speicher fuer Feld aus ASCII-Code-Nummern allokieren
    !    
    inlentr = LEN_TRIM(c_string);

    ALLOCATE(istring(inlentr+1), stat=ierr2)

    IF (ierr2 > 0 ) THEN
       
       CALL setup_error_act ( all_errors(:), -1004, c_upname, c_modname, ierr2 )
       CALL setup_error_act ( '<String>', c_string )
       RETURN
 
    ENDIF
    !
    ! [ 2.0] Feld aus ASCII-Code-Nummern generieren
    !    
    DO i=1, inlentr
       istring(i) = ichar(c_string(i:i))
    ENDDO
    
    istring(inlentr+1) = 0;
    !
    ! [ 3.0] C-Routine sys_cmd_cmds zum Absetzen des Systembefehls
    !        
!LEO replaced 
!    ierr2 = sys_cmd_cmds(istring, inlentr)
!by fortran code
!YOA Removed not handle by nag compiler
     ierr2 = 1 !SYSTEM(c_string)

    if (ierr2 /= 0 ) THEN
       
       CALL setup_error_act ( all_errors(:), -1005, c_upname, c_modname, ierr2 )
       WRITE(c_ierr2(1:10),'(I10)') ierr2
       CALL setup_error_act ( '<Methode>', c_called_by )
       CALL setup_error_act ( '<String>', c_string )
       CALL setup_error_act ( '<Systemfehlernummer>', c_ierr2 )
       RETURN

    ENDIF
    !
    ! [ 4.0] Feld aus ASCII-Code-Nummern deallokieren
    !    
    DEALLOCATE(istring)
    
  END FUNCTION unixcmd_cmds_d
  !
  !! Komprimieren einer Datei mittels der UNIX-Befehle compress, zip oder gzip. <BR>
  !! compress_cmds ist aus der Bibliothek cmds, Subroutine comprfi hervorgegangen. 
  SUBROUTINE compress_cmds_d (c_compression, c_filename)
    !
    ! Formalparameter
    !! Art der Datenkompression <BR>
    !! COMPRESS = UNIX-Tool compress <BR>
    !!      ZIP = UNIX-Tool zip <BR>
    !!     GZIP = UNIX-Tool gzip <BR>
    CHARACTER (LEN=*), INTENT(IN) :: c_compression
    !! Dateiname der zu komprimierenden Datei
    CHARACTER (LEN=*), INTENT(IN) :: c_filename    
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='compress_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_string
    !! maximal erlaubte Laenge von c_string
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_string
    CHARACTER(LEN=7) :: c_akt_len
    !! Zaehler
    INTEGER ::  i
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_string     = REPEAT(' ', LEN(c_string))
    !
    ! [ 2.0] Laenge von c_string pruefen
    !
    IF (len_trim(c_filename)+12 > LEN(c_string) ) THEN

       CALL setup_error_act ( all_errors(:), -1007, c_upname, c_modname )
       WRITE(c_akt_len,'(I7)') LEN_TRIM(c_filename) + 12
       CALL setup_error_act ( '<aktLaenge>', c_akt_len )
       WRITE(c_max_len,'(I7)') LEN(c_string)
       CALL setup_error_act ( '<maxLaenge>', c_max_len )

       RETURN

    END IF
    !
    ! [ 3.0] Befehlszeile generieren ...
    !
    IF (c_compression(1:3) == 'ZIP' .OR. &
         c_compression(1:3) == 'zip' ) THEN
       ! 
       ! [ 3.1] ... mittels ZIP
       ! 
       i = 4 + LEN_TRIM(c_filename) + 5 + LEN_TRIM(c_filename)
       c_string(1:i) = &
            'zip '//TRIM(c_filename)//&
            '.zip '//TRIM(c_filename)
       !
    ELSE IF (c_compression(1:4) == 'GZIP' .OR. &
         c_compression(1:4) == 'gzip' ) THEN
       ! 
       ! [ 3.2] ... mittels GZIP
       ! 
       i = 9 + LEN_TRIM(c_filename)
       c_string(1:i) = 'gzip -v1 '//TRIM(c_filename)
       !
    ELSE IF (c_compression(1:8) == 'COMPRESS' .OR. &
         c_compression(1:8) == 'compress') THEN
       ! 
       ! [ 3.3] ... mittels COMPRESS
       !        
       i = 12 + LEN_TRIM(c_filename)
       c_string(1:12+LEN_TRIM(c_filename)) = 'compress -v '//TRIM(c_filename)
       !
    ELSE
       ! 
       ! [ 3.4] ... ungueltiger Wert der Kompressionsangabe
       !               
       CALL setup_error_act ( all_errors(:), -1008, c_upname, c_modname )
       CALL setup_error_act ( '<Kompression>', TRIM(c_compression) )
       RETURN

    END IF
    !
    ! [ 4.0] Rufen der C-Routine zum Ausfuehren der Befehlszeile 
    !
    ierr2 = unixcmd_cmds &
         (c_string(1:i), &
         c_upname)
    !
  END SUBROUTINE compress_cmds_d
  !
  !! Kopieren einer Datei mittels des UNIX-Befehls cp. <BR>
  !! cp_cmds ist aus der Bibliothek cmds, Subroutine cpfile hervorgegangen. 
  SUBROUTINE cp_cmds_d (c_file, c_file_target, c_parameter)
    ! ... mit INTENT(IN)
    !! Dateiname der zu kopierenden Datei
    CHARACTER (LEN=*), INTENT(IN) :: c_file
    !! Dateiname des Zieldatei
    CHARACTER (LEN=*), INTENT(IN) :: c_file_target
    !! Parameter des UNIX-Befehles cp
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: c_parameter
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='cp_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_string
    !! maximal erlaubte Laenge von c_string
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_string
    CHARACTER(LEN=7) :: c_akt_len
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !! aktuelle Stringlaenge
    INTEGER :: i
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_string     = REPEAT(' ', LEN(c_string))
    !
    ! [ 2.0] Laenge von c_string pruefen
    !
    i = 3 + len_trim(c_file) + &
         1 + len_trim(c_file_target)

    IF(PRESENT(c_parameter)) i = i + 1 + len_trim(c_parameter)

    IF (i > LEN(c_string)) THEN

       CALL setup_error_act ( all_errors(:), -1009, c_upname, c_modname )
       WRITE(c_akt_len,'(I7)') i 
       CALL setup_error_act ( '<aktLaenge>', c_akt_len )
       WRITE(c_max_len,'(I7)') LEN(c_string)
       CALL setup_error_act ( '<maxLaenge>', c_max_len )
       RETURN

    END IF
    !
    ! [ 3.0] Befehlszeile generieren ...
    !
    IF(PRESENT(c_parameter)) THEN

       c_string(1 : i) = &
            'cp ' // &
            TRIM(c_parameter) // &
            ' ' // &
            TRIM(c_file) // &
            ' ' // &
            TRIM(c_file_target)

    ELSE

       c_string(1 : i) = &
            'cp ' // &
            TRIM(c_file) // &
            ' ' // &
            TRIM(c_file_target)

    ENDIF
    !
    ! [ 4.0] Rufen der C-Routine zum Ausfuehren der Befehlszeile 
    !
    ierr2 = unixcmd_cmds &
         (c_string(1:i), &
         c_upname)
    !
  END SUBROUTINE cp_cmds_d
  !
  !
  !! Den Wert einer Umgebungsvariablen (Environmentvariablen) ermitteln. <BR>
  !! getenvv_cmds ist aus der Bibliothek speci, Subroutine getenvv hervorgegangen. 
  FUNCTION getenvv_cmds_d (c_variable_name) &
       RESULT(c_variable_value)
    ! ... mit INTENT(IN)
    !! Name der Umgebungsvariablen
    CHARACTER (LEN=*), INTENT(IN) :: c_variable_name
    !! Wert der Umgebungsvariablen
    CHARACTER (LEN=80) :: c_variable_value
    !
    ! Lokale Parameter und Variablen
   !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='getenvv_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_variable_value_tmp
    !! maximal erlaubte Laenge von c_variable_value
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_variable_value_tmp
    CHARACTER(LEN=7) :: c_akt_len
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !! Zaehler
    INTEGER :: i
    !! Status nach allokieren
    INTEGER :: istat
    !! ASCII-Code-Nummern von c_variable_name (Nummer des zusaetzliches Zeichens = 0)
    INTEGER, ALLOCATABLE :: i_variable_name(:)
    !! ASCII-Code-Nummern von c_variable_value (Nummer des zusaetzliches Zeichens = 0)
    INTEGER, ALLOCATABLE :: i_variable_value(:)
    !! Laengen der Felder mit ASCII-Code-Nummern
    INTEGER :: i_name_len, i_value_len
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_variable_value     = REPEAT(' ', LEN(c_variable_value))
    c_variable_value_tmp     = REPEAT(' ', LEN(c_variable_value_tmp))
    i_name_len = len(c_variable_name)
    i_value_len = len(c_variable_value)
    !
    ! [ 2.0] Allokieren der ASCII-Code-Nummern
    !    
    ALLOCATE(i_variable_name(i_name_len+1), &
         i_variable_value(i_value_len+1), &
         stat= istat)

    IF(istat > 0 ) THEN

       CALL setup_error_act ( all_errors(:), -1015, c_upname, c_modname, ierr2 )
       CALL setup_error_act ( '<Name>', TRIM(c_variable_name) )
       CALL setup_error_act ( '<Value>', TRIM(c_variable_value) )
       RETURN
       
    ENDIF
    !
    ! [ 3.0] ASCII-Code-Nummern mit Namen der abgefragten Variablen belegen
    !    
    DO i=1, len_trim(c_variable_name)
       i_variable_name(i) = ichar( c_variable_name(i:i) )
    ENDDO

    i_variable_name( len_trim(c_variable_name) + 1 ) = 0
    !
    ! [ 4.0] C-Routine ermittelt den Wert der Umgebungsvariablen (ASCII-Code-Nummern)
    !    
!L.S. tried to replace:
!    CALL get_envv_cmds(i_variable_name, &
!         i_name_len, &
!         i_variable_value, &
!         i_value_len)
    !
    ! [ 5.0] temporaeren Wert setzen und Feld der ASCII-Code-Nummern deallokieren
    !    
!    DO i= 1, i_value_len
!       IF (i_variable_value(i) == 0) EXIT
!       c_variable_value_tmp(i:i) = achar(i_variable_value(i))
!    ENDDO
!by fortran code:
!YOA Removed not compatible with nag compiler
!    CALL GETENV(c_variable_name,c_variable_value_tmp)            

    DEALLOCATE (i_variable_name, &
         i_variable_value)
    !
    ! [ 6.0] falls Pruefung der Stringlaengen wunschgemaess
    !        -> den Wert c_variable_value setzen
    !
    IF( len_trim(c_variable_value_tmp) > len(c_variable_value) ) THEN
       
       CALL setup_error_act ( all_errors(:), -1014, c_upname, c_modname )
       WRITE(c_akt_len,'(I7)') LEN_TRIM(c_variable_value_tmp) 
       CALL setup_error_act ( '<aktLaenge>', c_akt_len )
       WRITE(c_max_len,'(I7)') LEN(c_variable_value)
       CALL setup_error_act ( '<maxLaenge>', c_max_len )
       
    ELSE
       
       c_variable_value(:len_trim(c_variable_value_tmp)) = &
            c_variable_value_tmp(:len_trim(c_variable_value_tmp)) 
    c_variable_value = '.'
    ENDIF
    !
  END FUNCTION getenvv_cmds_d
  !
  !! Erzeugen eines neuen Verzeichnisses mittels des UNIX-Befehls mkdir. <BR>
  !! mkdir_cmds ist aus der Bibliothek cmds, Subroutine crdir hervorgegangen. 
  SUBROUTINE mkdir_cmds_d (c_dir, c_parameter)
    ! ... mit INTENT(IN)
    !! Name des zu erzeugenden Verzeichnisses
    CHARACTER (LEN=*), INTENT(IN) :: c_dir
    !! (optional) Parameter fuer UNIX-Befehl mkdir
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: c_parameter
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='mkdir_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_string
    !! maximal erlaubte Laenge von c_string
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_string
    CHARACTER(LEN=7) :: c_akt_len
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !! aktuelle Stringlaenge
    INTEGER :: i
    !! existiert das Verzeichnis?
    LOGICAL :: l_exist
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_string     = REPEAT(' ', LEN(c_string))
    !
    ! [ 2.0] Datei oder Verzeichnis mit Namen c_dir bereits vorhanden?
    !
    INQUIRE &
         ( FILE=TRIM(c_dir), EXIST=l_exist )
    !
    IF(l_exist) THEN

       CALL setup_error_act ( all_errors(:), -1010, c_upname, c_modname )
       CALL setup_error_act ( '<Verzeichnis>', c_dir )

    ELSE
       !
       ! [ 3.0] Laenge der Befehlszeile pruefen
       !
       !
       IF ( PRESENT(c_parameter) ) THEN

          i = 6 + len_trim(c_parameter) + &
               1 + len_trim(c_dir) 

       ELSE

          i = 6 + len_trim(c_dir) 

       ENDIF

       IF (i > LEN(c_string)) THEN
          
          CALL setup_error_act ( all_errors(:), -1011, c_upname, c_modname )
          WRITE(c_akt_len,'(I7)') i 
          CALL setup_error_act ( '<aktLaenge>', c_akt_len )
          WRITE(c_max_len,'(I7)') LEN(c_string)
          CALL setup_error_act ( '<maxLaenge>', c_max_len )
          RETURN
          
       END IF
       !
       ! [ 4.0] Befehlszeile generieren 
       !
       IF (PRESENT(c_parameter)) THEN

          c_string(1 : i) = &
               'mkdir ' // &
               TRIM(c_parameter) // &
               ' ' // &
               TRIM(c_dir) 

       ELSE

          c_string(1 : i) = &
               'mkdir ' // &
               TRIM(c_dir) 

       ENDIF
       !
       ! [ 5.0] Rufen der C-Routine zum Ausfuehren der Befehlszeile 
       !
       ierr2 = unixcmd_cmds &
            (c_string(1:i), &
            c_upname)
    ENDIF
    !
  END SUBROUTINE mkdir_cmds_d
  !
  !
  !! mklist_cmds sucht in einem angegebenen Verzeichnis nach Dateinamen mittels des <BR>
  !! UNIX-Befehls find und schreibt die Suchergebnisse in eine Liste in eine Datei. <BR>
  !! mklist_cmds ist aus der Bibliothek cmds, Subroutine mklist hervorgegangen. 
  SUBROUTINE mklist_cmds_d (c_path, c_file_specif, c_list)
    ! ... mit INTENT(IN)
    !! Bezeichnung der mit find zu suchenden Dateien
    CHARACTER (LEN=*), INTENT(IN) :: c_file_specif
    !! Name der zu erzeugenden Datei mit der Liste
    CHARACTER (LEN=*), INTENT(IN) :: c_list
    !
    ! ... mit INTENT(INOUT)
    !! Suchpfad; aus einem ' ' wird '.')    
    CHARACTER (LEN=*), INTENT(INOUT) :: c_path
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='mklist_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_string
    !! maximal erlaubte Laenge von c_string
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_string
    CHARACTER(LEN=7) :: c_akt_len
    !! Anfuehrungszeichen oben
    CHARACTER(LEN=1) :: c_quotation_mark
    !! existiert die Listendatei bereits?
    LOGICAL :: l_exist
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !! aktuelle Stringlaenge
    INTEGER ::  i
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_string = REPEAT(' ', LEN(c_string))
    c_quotation_mark = CHAR(39)
    !
    ! [ 2.0] Stringlaengen pruefen
    !
    strlen: IF (len_trim(c_file_specif) <= 0) THEN

       CALL setup_error_act ( all_errors(:), -1012, c_upname, c_modname )
       CALL setup_error_act ( '<String>', 'Dateispezifikation' )
       
    ELSE IF (len_trim(c_list) <= 0) THEN
       
       CALL setup_error_act ( all_errors(:), -1012, c_upname, c_modname )
       CALL setup_error_act ( '<String>', 'zu erzeugender Datei mit Liste' )

    ELSE 
       ! [ 3.0] falls keine Angabe zum Suchpfad, diesen auf Arbeitsverzeichnis setzen
       !
       IF (len_trim(c_path) <= 0) c_path(1:1) = '.'
       !
       ! [ 4.0] Datei oder Verzeichnis mit Namen c_list bereits vorhanden?
       !
       INQUIRE &
         ( FILE=TRIM(c_list), EXIST=l_exist )
       !
       IF(l_exist) THEN

          CALL setup_error_act ( all_errors(:), -1010, c_upname, c_modname )
          CALL setup_error_act ( '<Verzeichnis>', c_list )
          RETURN
       
       ENDIF
       !
       ! [ 5.0] Laenge der Befehlszeile pruefen
       ! 
       i = len_trim(c_path) + len_trim(c_file_specif) + len_trim(c_list) + 24
       IF( i > len(c_string) ) THEN

          CALL setup_error_act ( all_errors(:), -1011, c_upname, c_modname )
          WRITE(c_akt_len,'(I7)') i
          CALL setup_error_act ( '<aktLaenge>', c_akt_len )
          WRITE(c_max_len,'(I7)') len(c_string)
          CALL setup_error_act ( '<maxLaenge>', c_max_len )
          RETURN
          
       END IF
       !
       ! [6.0] Befehlszeile generieren 
       !
       c_string(:i) = 'find ' // &
            TRIM(c_path) // &
            ' -name ' // &
            c_quotation_mark // &
            TRIM(c_file_specif) // &
            c_quotation_mark // &
            ' -print > '// &
            TRIM(c_list)
       
       !
       ! [ 7.0] Rufen der C-Routine zum Ausfuehren der Befehlszeile 
       !
       ierr2 = unixcmd_cmds &
            (c_string(1:i), &
            c_upname)
    ENDIF strlen
    !
  END SUBROUTINE mklist_cmds_d
  !
  !
  !! Verschieben einer Datei mittels des UNIX-Befehls mv. <BR>
  !! mv_cmds ist aus der Bibliothek cmds, Subroutine mvfile hervorgegangen. 
  SUBROUTINE mv_cmds_d (c_file, c_file_target, c_parameter)
    ! ... mit INTENT(IN)
    !! Dateiname der zu verschiebenden Datei
    CHARACTER (LEN=*), INTENT(IN) :: c_file
    !! Dateiname des Zieldatei
    CHARACTER (LEN=*), INTENT(IN) :: c_file_target
    !! Parameter fuer UNIX-Befehl mv
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: c_parameter
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='mv_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_string
    !! maximal erlaubte Laenge von c_string
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_string
    CHARACTER(LEN=7) :: c_akt_len
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !! aktuelle Stringlaenge
    INTEGER :: i
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_string     = REPEAT(' ', LEN(c_string))
    !
    ! [ 2.0] Laenge von c_string pruefen
    !
    i = 3 + len_trim(c_file) + &
         1 + len_trim(c_file_target)

    IF(PRESENT(c_parameter)) i = i + 1 + len_trim(c_parameter)

    IF (i > LEN(c_string)) THEN

       CALL setup_error_act ( all_errors(:), -1009, c_upname, c_modname )
       WRITE(c_akt_len,'(I7)') i 
       CALL setup_error_act ( '<aktLaenge>', c_akt_len )
       WRITE(c_max_len,'(I7)') LEN(c_string)
       CALL setup_error_act ( '<maxLaenge>', c_max_len )

       RETURN

    END IF
    !
    ! [ 3.0] Befehlszeile generieren ...
    !
    IF(PRESENT(c_parameter)) THEN
       
       c_string(1 : i) = &
            'mv ' // &
            TRIM(c_parameter) // &
            ' ' // &
            TRIM(c_file) // &
            ' ' // &
            TRIM(c_file_target)

    ELSE

       c_string(1 : i) = &
            'mv ' // &
            TRIM(c_file) // &
            ' ' // &
            TRIM(c_file_target)

    ENDIF
    !
    ! [ 4.0] Rufen der C-Routine zum Ausfuehren der Befehlszeile 
    !
    ierr2 = unixcmd_cmds &
         (c_string(1:i), &
         c_upname)
    !
  END SUBROUTINE mv_cmds_d
  !
  !
  !! Den Wert des Pfadtrennzeichen abhaengig vom Betriebssystem ermitteln. <BR>
  !! pathdelim_cmds ist aus der Bibliothek speci, Subroutine pathdelimiter 
  !! hervorgegangen. <BR> 
  !! typisches Ergebnis: Windows "\\", andere "/"
  FUNCTION pathdelim_cmds_d ( ) &
       RESULT(c_delimiter)
    !! Pfadtrennzeichen
    CHARACTER (LEN=2) :: c_delimiter
    CHARACTER(LEN=255) :: path
    CHARACTER(LEN=1) :: path_separator
    !L.S. replaced INTEGER, EXTERNAL:: i_path_delim_cmds
    !
!L.S. replaced 
!    c_delimiter = achar( i_path_delim_cmds() )
!by the following Fortran code
    CALL GET_ENVIRONMENT_VARIABLE('PATH',path)
    path_separator=path(1:1)
    
    IF (path_separator.EQ."/") THEN
        c_delimiter = "/"
    ELSE
        c_delimiter = "\\"
    END IF

    !
  END FUNCTION pathdelim_cmds_d
  !
  !
  !! L&ouml;schen einer Datei mittels des UNIX-Befehles rm. <BR>
  !! rm_cmds ist aus der Bibliothek cmds, Subroutine rm hervorgegangen. 
  SUBROUTINE rm_cmds_d (c_filename, c_parameter) 
    !! zu loeschende Datei
    CHARACTER(LEN=*), INTENT(IN) :: c_filename
    !! Parameter fuer den UNIX-Befehl rm, z.B. -f
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: c_parameter
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='rm_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_string
    !! maximal erlaubte Laenge von c_string
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_string
    CHARACTER(LEN=7) :: c_akt_len
    !! Zaehler
    INTEGER ::  i
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_string = REPEAT(' ', LEN(c_string))
    !
    ! [ 2.0]  Laenge von c_string pruefen
    !    
    i = 3 + len_trim(c_filename)
    !
    IF (PRESENT(c_parameter)) i = i + 1 + len_trim(c_parameter) 
    !
    IF (i > LEN(c_string)) THEN
       !
       CALL setup_error_act ( all_errors(:), -1006, c_upname, c_modname )
       WRITE(c_akt_len,'(I7)') i
       CALL setup_error_act ( '<aktLaenge>', c_akt_len )
       WRITE(c_max_len,'(I7)') LEN(c_string)
       CALL setup_error_act ( '<maxLaenge>', c_max_len )

       RETURN
       !
    END IF
    !
    ! [ 3.0] Befehlszeile generieren 
    !    
    IF(PRESENT(c_parameter)) THEN
       
       c_string(1:i) = 'rm ' // &
            c_parameter // &
            ' ' // &
            c_filename
    ELSE
       
       c_string(1:i) = 'rm ' // &
            c_filename
       
    ENDIF
    !
    ! [ 4.0] Rufen der C-Routine zum Ausfuehren der Befehlszeile 
    !
    ierr2 = unixcmd_cmds &
         (c_string(1:I), &
         c_upname)
    !
  END SUBROUTINE rm_cmds_d
  !
  !
  !! L&ouml;schen eines Verzeichnisses mittels des UNIX-Befehles rmdir. <BR>
  SUBROUTINE rmdir_cmds_d (c_filename, c_parameter) 
    !! zu loeschende Datei
    CHARACTER(LEN=*), INTENT(IN) :: c_filename
    !! Parameter fuer den UNIX-Befehl rmdir, z.B. -f
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: c_parameter
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='rmdir_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_string
    !! maximal erlaubte Laenge von c_string
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_string
    CHARACTER(LEN=7) :: c_akt_len
    !! Zaehler
    INTEGER ::  i
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_string = REPEAT(' ', LEN(c_string))
    !
    ! [ 2.0]  Laenge von c_string pruefen
    !    
    i = 6 + len_trim(c_filename)
    !
    IF (PRESENT(c_parameter)) i = i + 1 + len_trim(c_parameter) 
    !
    IF (i > LEN(c_string)) THEN
       !
       CALL setup_error_act ( all_errors(:), -1006, c_upname, c_modname )
       WRITE(c_akt_len,'(I7)') i
       CALL setup_error_act ( '<aktLaenge>', c_akt_len )
       WRITE(c_max_len,'(I7)') LEN(c_string)
       CALL setup_error_act ( '<maxLaenge>', c_max_len )

       RETURN
       !
    END IF
    !
    ! [ 3.0] Befehlszeile generieren 
    !    
    IF(PRESENT(c_parameter)) THEN
       
       c_string(1:i) = 'rmdir ' // &
            c_parameter // &
            ' ' // &
            c_filename
    ELSE
       
       c_string(1:i) = 'rmdir ' // &
            c_filename
       
    ENDIF
    !
    ! [ 4.0] Rufen der C-Routine zum Ausfuehren der Befehlszeile 
    !
    ierr2 = unixcmd_cmds &
         (c_string(1:I), &
         c_upname)
    !
  END SUBROUTINE rmdir_cmds_d
  !
  !! Dekomprimieren einer Datei unter Aufbewahrung der komprimierten Datei <BR>
  !! mittels der UNIX-Befehle compress, unzip oder gunzip. Falls eine Datei mit <BR>  
  !! dem Namen der dekomprimierten Datei existiert, wird sie ueberschrieben. <BR>
  !! uncompress_cmds ist aus der Bibliothek cmds, Subroutine uncomprfi 
  !! hervorgegangen. 
  SUBROUTINE uncompress_cmds_d (c_compression, c_file_compressed, c_file_uncompressed)
    ! ... mit INTENT(IN)
    !! Art der Datenkompression <BR>
    !! COMPRESS = UNIX-Tool compress <BR>
    !!     GZIP = UNIX-Tool gzip <BR>
    CHARACTER (LEN=*), INTENT(IN) :: c_compression
    !! Dateiname der komprimierten Datei
    CHARACTER (LEN=*), INTENT(IN) :: c_file_compressed
    !
    ! ... mit INTENT(OUT)
    !! Dateiname der entkomprimierten Datei
    CHARACTER (LEN=*), INTENT(OUT) :: c_file_uncompressed
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='uncompress_cmds_d' 
    !! UNIX-Befehlszeile
    CHARACTER(LEN=500) :: c_string
    !! maximal erlaubte Laenge von c_string
    CHARACTER(LEN=7) :: c_max_len
    !! aktuelle Laenge von c_string
    CHARACTER(LEN=7) :: c_akt_len
    !! in unixcmd_cmds generierte Fehlerkennung
    INTEGER ::  ierr2
    !! aktuelle Stringlaengen
    INTEGER :: i_len_uncompressed, i
    !
    ! ------------------------------------------------------------------
    !
    ! [ 1.0] Initialisierungen
    !    
    c_string     = REPEAT(' ', LEN(c_string))
    !
    ! [ 2.0] Pruefe, ob c_string ausreichend lang definiert wurde 
    !
    IF ( LEN_TRIM(c_file_compressed) + LEN_TRIM(c_file_uncompressed) + 17  &
         > LEN(c_string) )  THEN
       !
       CALL setup_error_act ( all_errors(:), -1007, c_upname, c_modname )
       WRITE(c_akt_len,'(I7)') LEN(c_file_compressed) + LEN(c_file_uncompressed) + 17
       CALL setup_error_act ( '<aktLaenge>', c_akt_len )
       WRITE(c_max_len,'(I7)') LEN(c_string)
       CALL setup_error_act ( '<maxLaenge>', c_max_len )

       RETURN

    END IF
    !
    ! [ 3.0] Befehlszeile generieren ...
    !
    IF (c_compression(1:3) == 'ZIP' .OR. &
         c_compression(1:3) == 'zip' ) THEN
       ! 
       ! [ 3.1] ... mittels ZIP
       !
       i_len_uncompressed = len_trim(c_file_compressed) - 4
       c_file_uncompressed = c_file_compressed(1: i_len_uncompressed)
       ! 
       i = 9 + LEN_TRIM(c_file_compressed) + 1 + LEN_TRIM(c_file_uncompressed)
       c_string(1:i) = &
            'unzip -o '//TRIM(c_file_compressed)//&
            ' '//TRIM(c_file_uncompressed)

       !
    ELSE IF (c_compression(1:4) == 'GZIP' .OR. &
         c_compression(1:4) == 'gzip' ) THEN
       ! 
       ! [ 3.2] ... mittels GZIP
       ! 
       i_len_uncompressed = len_trim(c_file_compressed) - 3
       c_file_uncompressed = c_file_compressed(1: i_len_uncompressed)
       !
       i = 10 + len_trim(c_file_compressed) + 3 + i_len_uncompressed
       c_string(1 : i) = &
            'gunzip -c '// &
            TRIM(c_file_compressed)// &
            ' > '// &
            TRIM(c_file_uncompressed)
       !
    ELSE IF (c_compression(1:8) == 'COMPRESS' .OR. &
         c_compression(1:8) == 'compress' ) THEN
       ! 
       ! [ 3.3] ... mittels COMPRESS
       !        
       i_len_uncompressed = len_trim(c_file_compressed) - 2
       c_file_uncompressed = c_file_compressed(1:i_len_uncompressed)
       !
       i = 14 + LEN_TRIM(c_file_compressed) + 3 + i_len_uncompressed
       c_string(1:i) = &
            'uncompress -c '// &
            TRIM(c_file_compressed)// &
            ' > '// &
            TRIM(c_file_uncompressed)
       !
    ELSE
       ! 
       ! [ 3.4] ... ungueltiger Wert der Kompressionsangabe
       !               
       CALL setup_error_act ( all_errors(:), -1008, c_upname, c_modname )
       CALL setup_error_act ( '<Kompression>', TRIM(c_compression) )
       RETURN
       !
    END IF
    !
    ! [ 4.0] Rufen der C-Routine zum Ausfuehren der Befehlszeile 
    !
    ierr2 = unixcmd_cmds &
         (c_string(1:i), &
         c_upname)
    !
  END SUBROUTINE uncompress_cmds_d
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
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert
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
       WRITE(*,*) ' *** Warnung *** Modul "b_cmds" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_cmds ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert
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
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls
  SUBROUTINE init_cmds_all_errors ( )
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
               '--> INIT_cmds ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_cmds ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 003
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_cmds"\n'//&
               '--> Code in Modul "b_cmds" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 004
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1004)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               'UNIX-Kommando = "<String>" \n'//&
               'Fehler beim Allokieren von istring(:) \n'//&
               '--> ausreichend Speicher? wenn ja Code pruefen ' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 005
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1005)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               'Fehler beim Aufrufen der C-Bridge sys_cmd_cmds \n'//&
               'unixcmd_cmds wurde aufgerufen von "<Methode>" \n'//&
               'UNIX-Befehl = "<String>" \n'//&
               'Systemfehler ierr2 = "<Systemfehlernummer>" \n'//&
               '--> Schreibberechtigung,Umgebungsvariablen und ggf. Code pruefen)' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 006
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1006)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               'Loeschbefehl ist zu lang\n'//&
               '        aktuelle Laenge = "<aktLaenge>" \n'//&
               'maximal erlaubte Laenge = "<maxLaenge>" \n'//&
               '--> Namen kuerzen oder maximal erlaubte Laenge heraufsetzen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 007
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1007)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               '(De-) Komprimierbefehl ist zu lang\n'//&
               '        aktuelle Laenge = "<aktLaenge>" \n'//&
               'maximal erlaubte Laenge = "<maxLaenge>" \n'//&
               '--> Namen kuerzen oder maximal erlaubte Laenge heraufsetzen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 008
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1008)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               'Kompressionsart ist fehlerhaft\n'//&
               ' erlaubt = [ZIP,GZIP,COMPRESS]\n'//&
               ' aktuell = "<Kompression>"\n'//&
               '--> andere Kompressionsart waehlen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 009
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1009)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               ' Verschiebe-/Kopierbefehl ist zu lang\n'//&
               '        aktuelle Laenge = "<aktLaenge>" \n'//&
               'maximal erlaubte Laenge = "<maxLaenge>" \n'//&
               '--> Namen kuerzen oder maximal erlaubte Laenge heraufsetzen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 010
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1010)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               'Datei oder Verzeichnis koennen nicht neu erzeugt werden\n'//&
               'weil bereits Datei oder Verzeichnis gleichen Namens existieren\n'//&
               'Verzeichnis = "<Verzeichnis>" \n'//&
               '--> Daten pruefen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 011
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1011)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               ' Befehl zur Verzeichniserzeugung ist zu lang\n'//&
               '        aktuelle Laenge = "<aktLaenge>" \n'//&
               'maximal erlaubte Laenge = "<maxLaenge>" \n'//&
               '--> Namen kuerzen oder maximal erlaubte Laenge heraufsetzen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 012
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1012)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               'String mit <String> ist leer\n'//&
               '--> Daten/Sourcecode pruefen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 013
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1013)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               ' Befehl zum Erzeugen einer Dateiliste ist zu lang\n'//&
               '        aktuelle Laenge = "<aktLaenge>" \n'//&
               'maximal erlaubte Laenge = "<maxLaenge>" \n'//&
               '--> Namen kuerzen oder maximal erlaubte Laenge heraufsetzen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 014
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1014)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               'Wert der angeforderten Umgebungsvariablen ist zu lang\n'//&
               '        aktuelle Laenge = "<aktLaenge>" \n'//&
               'maximal erlaubte Laenge = "<maxLaenge>" \n'//&
               '--> kuerzeren Wert vergeben oder maximal erlaubte Laenge heraufsetzen')
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          ! Index 015
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -1015)
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modul-spezifische Methoden \n'//&
               'Fehler beim Allokieren von i_variable_name(:), i_variable_value(:) \n'//&
               ' c_variable_name = "<Name>" \n'//&
               'c_variable_value = "<Value>" \n'//&
               '--> ausreichend Speicher? wenn ja Code pruefen' )
       END IF
       !
       ! Allokieren der Felder beim ersten Durchlauf (i==1)
       !
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    ENDDO
    !
  END SUBROUTINE init_cmds_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls
  SUBROUTINE clear_cmds_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_cmds_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
END MODULE b_cmds
! TailOfBaseModule --------------------------------------------------------
