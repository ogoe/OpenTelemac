! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Lesen und Schreiben von Gitterdaten f&uuml;r Dateityp "gitter05"</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 4.1 vom 08/22/05, Quellcode: mod_m_h_grid_gitter05.f90
!! <HR>
!! read and write methods for "gitter05" files                       <BR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2001 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  01.01 : 2002-07-11 : G. Lang   : Startversion
!  01.03 : 2003-03-20 : P. Schade : Aenderungen am Header, z.B. Copyright
!  02.01 : 2005-03-07 : G. Lang   : Erweiterungen OpenMI (unvollstaendig)
!  02.02 : 2005-03-10 : G. Lang   : div. Erweiterungen fuer Konfigurationsphase OpenMI
!  02.03 : 2005-03-16 : G. Lang   : Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries 
!  02.04 : 2005-06-23 : P. Schade : Felder xy und depth vom Typ Double elementweise mit Werten fuellen.
!  02.05 : 2005-06-23 : P. Schade : Felder xy und depth debugged
!  03.01 : 2005-07-21 : G. Lang   : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
!  04.01 : 2005-08-22 : G. Lang   : Lesen/Schreiben Delft3D-Gitternetz
!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Lesen einer Datei im Format "gitter05";
!!   <LI> Schreiben einer Datei im Format "gitter05".
!! </OL>
!! Weitere Informationen zum Dateiformat unter
!! <A HREF="http://www.hamburg.baw.de/fkb/gitter05/gi05-de.htm"><EM>gitter05.dat</EM></A>.
!! <HR>
!! <H3>Gebrauch der Methoden dieses Moduls</H3>
!! Die in diesem Modul bereitgestellten Methoden d&uuml;rfen nur
!! von zu dem Paket "p_h_grid_ui" geh&ouml;renden Modulen direkt 
!! verwendet werden. Insbesondere ist das direkte Verwenden von
!! Methoden aus anderen Paketen unzul&auml;ssig. 
!
MODULE m_h_grid_gitter05
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Double
  !
  ! [A.2] Basis-Modul "Fehler"
  !
  USE b_error, ONLY :       &
       ! Routinen
       no_error,            &
       any_error,           &
       setup_error_act
  !
  ! [A.3] Basis-Modul "File"
  !
  USE b_file, ONLY : &
       !   Routinen / Interfaces
       get_file_unit, &
       get_file_name
  ! 
  ! [A.4] Basis-Modul "Dateum+Uhrzeit"
  !
  USE b_datetime, ONLY : &
       ! Routinen / Interfaces
       get_datetime_from_system
  !
  ! ----------------------------------------------------------------------
  ! [B] gemeinsam genutzte Daten des Paketes "<PackageName>"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "<PackageName>"
  !
  USE m_h_grid_data, ONLY : &
       ! Typdefinition
       t_h_grid,   & ! 
       ! Parameter und Konstantwerte
       prn_op,     & ! Indikator Druckerausgabe
       trc_op,     & ! Indikator Drucken in Trace-Datei
       prn_lun,    & ! log. Fortran-Unit fuer Druckerausgabe
       trc_lun,    & ! log. Fortran-Unit fuer Traceausgabe
       all_errors, & ! Fehlermeldunegn
       ! Routinen / Parameter
       setup_xy_object,    &
       setup_nen_object,   &
       setup_irand_object, &
       setup_ks_object,    &
       setup_hv_object,    &
       setup_nrand_object, &
       setup_time_object,  &
       !
       get_nv_object,    &
       get_ne_object,    &
       get_xy_object,    &
       get_nen_object,   &
       get_irand_object, &
       get_ks_object,    &
       get_hv_object,    &
       get_nrand_object
  !
  ! ---------------------------------------------------------------------
  ! --> alles muss explizit deklariert werden und ist default privat
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
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  ! [C.3] Variablen [moeglichst nicht verwenden]
  ! [C.4] Schnittstellen
  !
  !! Lesen einer sequentiellen ASCII-Datei des Typs "gitter05"
  INTERFACE read_h_grid_gitter05_asc
     MODULE PROCEDURE read_h_grid_gitter05_asc_d
  END INTERFACE
  !
  !! Lesen einer sequentiellen Bin&auml;r-Datei des Typs "gitter05"
  INTERFACE read_h_grid_gitter05_bin
     MODULE PROCEDURE read_h_grid_gitter05_bin_d
  END INTERFACE
  !
  !! Schreiben einer sequentiellen ASCII-Datei des Typs "gitter05"
  INTERFACE write_h_grid_gitter05_asc
     MODULE PROCEDURE write_h_grid_gitter05_asc_d
  END INTERFACE
  !
  !! Schreiben einer sequentiellen Bin&auml;r-Datei des Typs "gitter05"
  INTERFACE write_h_grid_gitter05_bin
     MODULE PROCEDURE write_h_grid_gitter05_bin_d
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren (optional, falls sinnvoll)
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: read_h_grid_gitter05_asc   ! SEQUENTIAL, FORMATTED
  PUBLIC :: read_h_grid_gitter05_bin   ! SEQUENTIAL, UNFORMATTED
  PUBLIC :: write_h_grid_gitter05_asc  ! SEQUENTIAL, FORMATTED
  PUBLIC :: write_h_grid_gitter05_bin  ! SEQUENTIAL, UNFORMATTED
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Konstantwerte (Parameter)
  !
  !! maximale Anzahl verschiedener Fehlertexte
  INTEGER , PARAMETER :: c_noflocalerrors=8 ! 
  !! lokale Fehlertexte
  CHARACTER (LEN=45) , PARAMETER :: c_localerrors(c_noflocalerrors) = &
       (/ 'Anzahl der Knoten auf dem Rand NRAND         ', &
          'Anzahl der Knoten im Innengebiet NINNEN      ', &
          'Koordinaten und Tiefen der Knotenpunkte      ', &
          'Anzahl der Elemente im Gitternetz            ', &
          'Knotenverzeichnis und Randkennungen          ', &
          'Kopfzeile Koordinaten und Tiefen             ', &
          'Kopfzeile Knotenverzeichnis und Randkennungen', &
          'Fusszeile der Datei                          ' /)
  !
  !! Name des Moduls
  CHARACTER (LEN=17), PARAMETER :: c_modname = 'm_h_grid_gitter05' ! 
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Hilfsfeld zum Lesen einer Eingabezeile
  CHARACTER (LEN=240) :: karte ! 
  !! Hilfsfeld fuer Fehlertexte
  CHARACTER (LEN=15)  :: ctxt  ! 
  !! log. Kanalnummer f&uuml;r das Lesen der Daten
  INTEGER :: iunit  ! 
  !! Anzahl der Knoten auf dem Rand
  INTEGER           :: nrand  ! 
  INTEGER , POINTER :: p_nrand  ! 
  !! Anzahl der Knoten im Innengebiet
  INTEGER :: ninnen ! 
  !! Anzahl der Knoten im Gesamtgebiet
  INTEGER           :: iaktkno ! 
  INTEGER , POINTER :: p_iaktkno ! 
  !! Anzahl der Polygone (Dreiecke) im Gesamtgebiet
  INTEGER           :: nelem ! 
  INTEGER , POINTER :: p_nelem ! 
  !! Z&auml;hlervariable
  INTEGER :: i, j ! 
  !! Dummy-Knoten-/Elementnummer
  INTEGER :: nn ! 
  !! Statusvariable
  INTEGER :: stat, iostat ! 
  !! Zeiger auf Text in Fehlermeldung
  INTEGER :: isprun ! 
  !! tempor&auml;res Feld f&uuml;r das Knotenverzeichnis
  INTEGER            , POINTER  :: nen(:,:) ! 
  !! tempor&auml;res Feld f&uuml;r die Randkennungen
  INTEGER            , POINTER  :: irand(:) ! 
  !! tempor&auml;res Feld f&uuml;r Anzahl der Knoten/Kanten im Polygon
  INTEGER            , POINTER  :: ks(:)    ! 
  !! tempor&auml;res Feld f&uuml;r die Koordinaten
  REAL (KIND=Double) , POINTER  :: xy(:,:)    ! 
  REAL               , POINTER  :: r_xy(:,:)  ! 
  !! tempor&auml;res Feld f&uuml;r die Tiefen an den Knoten
  REAL (KIND=Double) , POINTER  :: depth(:)   ! 
  REAL               , POINTER  :: r_depth(:) ! 
  !
  ! [D.4] Schnittstellen
  ! [D.5] Assignments
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
  ! Oeffentliche Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-READ-Methoden <<< [ERR_NO = 23000 bis 23999]
  ! ----------------------------------------------------------------------
  !
  !! &Uuml;bertragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! f&uuml;r die Datei gilt FORM="FORMATTED", ACCESS="SEQUENTIAL" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_gitter05_asc_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt zur Aufnahme der Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='read_h_grid_gitter05_asc_d' !
    !! Indikator
    LOGICAL , PARAMETER :: binary=.false. ! 
    !
    CALL init_before_read ( this         )
    !
    CALL read_datablock_1 ( binary, this )
    CALL read_datablock_2 ( binary, this )
    CALL read_datablock_3 ( binary, this )
    CALL read_datablock_4 ( binary, this )
    CALL read_datablock_5 ( binary, this )
    !
    CALL setup_error_after_read ( binary, this )
    CALL init_after_read        (              )
    !
    CALL setup_time_object ( this, get_datetime_from_system ( ) )
    !
  END SUBROUTINE read_h_grid_gitter05_asc_d
  !
  !! &Uuml;bertragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! f&uuml;r die Datei gilt FORM="UNFORMATTED", ACCESS="SEQUENTIAL" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_gitter05_bin_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt zur Aufnahme der Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='read_h_grid_gitter05_bin_d' !
    !! Indikator
    LOGICAL , PARAMETER :: binary=.true. ! 
    !
    CALL init_before_read ( this         )
    !
    CALL read_datablock_1 ( binary, this )
    CALL read_datablock_2 ( binary, this )
    CALL read_datablock_3 ( binary, this )
    CALL read_datablock_4 ( binary, this )
    CALL read_datablock_5 ( binary, this )
    !
    CALL setup_error_after_read ( binary, this )
    CALL init_after_read        (              )
    !
    CALL setup_time_object ( this, get_datetime_from_system ( ) )
    !
  END SUBROUTINE read_h_grid_gitter05_bin_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-WRITE-Methoden <<< [ERR_NO = 24000 bis 24999]
  ! ----------------------------------------------------------------------
  !
  !! &Uuml;bertragen der in dem (Arbeits-) Objekt stehenden Daten in eine Datei <BR>
  !! f&uuml;r die Datei gilt FORM="FORMATTED", ACCESS="SEQUENTIAL" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_gitter05_asc_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt mit den zu schreibenden Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER  :: c_upname='write_h_grid_gitter05_asc_d' !
    !! Indikator
    LOGICAL , PARAMETER :: binary=.false. ! 
    !
    CALL init_before_write ( this )
    !
    CALL write_datablock_1 ( binary, this )
    CALL write_datablock_2 ( binary, this )
    CALL write_datablock_3 ( binary, this )
    CALL write_datablock_4 ( binary, this )
    CALL write_datablock_5 ( binary, this )
    !
    CALL setup_error_after_write ( this )
    CALL init_after_write        (      )
    !
  END SUBROUTINE write_h_grid_gitter05_asc_d
  !
  !! &Uuml;bertragen der in dem (Arbeits-) Objekt stehenden Daten in eine Datei <BR>
  !! f&uuml;r die Datei gilt FORM="UNFORMATTED", ACCESS="SEQUENTIAL" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_gitter05_bin_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt mit den zu schreibenden Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER  :: c_upname='write_h_grid_gitter05_bin_d' !
    !! Indikator
    LOGICAL , PARAMETER :: binary=.true. ! 
    !
    CALL init_before_write ( this )
    !
    CALL write_datablock_1 ( binary, this )
    CALL write_datablock_2 ( binary, this )
    CALL write_datablock_3 ( binary, this )
    CALL write_datablock_4 ( binary, this )
    CALL write_datablock_5 ( binary, this )
    !
    CALL setup_error_after_write ( this )
    CALL init_after_write        (      )
    !
  END SUBROUTINE write_h_grid_gitter05_bin_d
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
  !! Lesen einer Zeile aus ASCII-Dateien (mit optionalen Kommentarzeilen) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kartle_internal &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt zur Aufnahme der Gitterdaten
    TYPE (t_h_grid)    , POINTER :: this  ! 
    ! lokale Variablen
    CHARACTER (LEN=15) , PARAMETER  :: c_upname='kartle_internal' ! 
    !! logische Kennung
    LOGICAL :: lexit ! 
    !
    lexit = .false.
    !
    DO
       IF ( lexit ) EXIT
       !
       READ ( iunit, '(A)', IOSTAT=iostat ) karte 
       !
       IF ( iostat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23101, c_upname, c_modname, iostat, TRIM( karte ) )
          CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
          lexit = .true.
       ELSE
          SELECT CASE ( karte(1:1) )
          CASE ( '*' )
             WRITE(*,*) TRIM( karte(:) )
          CASE ( 'C', 'c', '&', '#', '@' )
             CONTINUE
          CASE DEFAULT
             lexit = .true.
          END SELECT
       END IF
       !
    END DO
    !
  END SUBROUTINE kartle_internal
  !
  !! Pr&uuml;fen des aktuellen Wertes von "nrand" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE check_nrand &
       ( )
    !
    CHARACTER (LEN=11) , PARAMETER :: c_upname='check_nrand'
    !
    IF ( nrand < 3 ) THEN
       CALL setup_error_act ( all_errors(:), 23102, c_upname, c_modname )
       WRITE(ctxt(1:10),'(I10)') nrand
       CALL setup_error_act ( '<AktNrand>', ctxt(1:10) )
    END IF
    !
  END SUBROUTINE check_nrand
  !
  !! Pr&uuml;fen des aktuellen Wertes von "ninnen" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE check_ninnen &
       ( )
    !
    CHARACTER (LEN=12) , PARAMETER :: c_upname='check_ninnen'
    !
    IF ( ninnen < 0 ) THEN
       CALL setup_error_act ( all_errors(:), 23103, c_upname, c_modname )
       WRITE(ctxt(1:10),'(I10)') ninnen
       CALL setup_error_act ( '<AktNinnen>', ctxt(1:10) )
    END IF
    !
  END SUBROUTINE check_ninnen
  !
  !! Pr&uuml;fen des aktuellen Wertes von "nelem" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE check_nelem &
       ( )
    !
    CHARACTER (LEN=11) , PARAMETER :: c_upname='check_nelem'
    !
    IF ( nelem < 1 ) THEN
       CALL setup_error_act ( all_errors(:), 23106, c_upname, c_modname )
       WRITE(ctxt(1:10),'(I10)') nelem
       CALL setup_error_act ( '<AktNelem>', ctxt(1:10) )
    END IF
    !
  END SUBROUTINE check_nelem
  !
  !! Initialisieren einiger Variablen vor dem Lesen von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_before_read &
       ( this )
    !
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    iunit  = get_file_unit ( this%file )
    karte  = REPEAT( ' ', LEN( karte ) )
    nrand  = 0
    ninnen = 0
    nelem  = 0
    stat   = 0
    iostat = 0
    isprun = 0
    !
    NULLIFY ( xy, nen, depth, irand, ks, r_xy, r_depth )
    !
  END SUBROUTINE init_before_read
  !
  !! Initialisieren einiger Variablen nach dem Lesen von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_after_read &
       ( )
    !
    NULLIFY ( xy, nen, depth, irand, ks, r_xy, r_depth )
    !
  END SUBROUTINE init_after_read
  !
  !! Ggf. einen Fehler nach dem Lesen von Daten erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_error_after_read &
       ( binary, this )
    !! Indikator: .true. == Lesen einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=22) , PARAMETER :: c_upname='setup_error_after_read' ! 
    !
    IF ( isprun /= 0 ) THEN
       SELECT CASE ( binary )
       CASE ( .true. )
          CALL setup_error_act ( all_errors(:), 23100, c_upname, c_modname, iostat )
       CASE DEFAULT
          CALL setup_error_act ( all_errors(:), 23100, c_upname, c_modname, iostat, TRIM(karte) )
       END SELECT
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       CALL setup_error_act ( '<LokalerFehlerText>', c_localerrors(isprun) )
    END IF
    !
  END SUBROUTINE setup_error_after_read
  ! 
  !! Lesen des ersten Datenblocks mit der Anzahl der Randknoten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_datablock_1 &
       ( binary, this )
    !! Indikator: .true. == Lesen einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) ) THEN
       SELECT CASE ( binary )
       CASE ( .true. )
          READ(iunit,IOSTAT=iostat) nrand   ! Anzahl Randknoten "nrand"
       CASE DEFAULT
          CALL kartle_internal ( this )
          IF ( no_error( ) ) READ(karte,*,IOSTAT=iostat) nrand ! Anzahl Randknoten "nrand"
       END SELECT
       IF ( iostat == 0 ) THEN
          CALL check_nrand ( )
       ELSE
          isprun = 1 
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_1
  !
  !! Lesen des zweiten Datenblocks mit der Anzahl der Innenknoten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_datablock_2 &
       ( binary, this )
    !! Indikator: .true. == Lesen einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       SELECT CASE ( binary )
       CASE ( .true. )
          READ(iunit,IOSTAT=iostat) ninnen   ! Anzahl Innenknoten "ninnen"
       CASE DEFAULT
          CALL kartle_internal ( this )
          IF ( no_error( ) ) READ(karte,*,IOSTAT=iostat) ninnen ! Anzahl Innenknoten "ninnen"
       END SELECT
       IF ( iostat == 0 ) THEN
          CALL check_ninnen ( )
       ELSE
          isprun = 2
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_2
  !
  !! Lesen des dritten Datenblocks mit den Knotenkoordinaten und
  !! den Knotentiefen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_datablock_3 &
       ( binary, this )
    !! Indikator: .true. == Lesen einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_3' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       iaktkno = nrand + ninnen
       SELECT CASE ( binary )
       CASE ( .true. )
          ALLOCATE (   xy(iaktkno,2),   depth(iaktkno), &
               r_xy(iaktkno,2), r_depth(iaktkno), STAT=stat )
       CASE DEFAULT
          ALLOCATE ( xy(iaktkno,2), depth(iaktkno), STAT=stat )
       END SELECT
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23104, c_upname, c_modname, stat )
       ELSE
          DO i=1,iaktkno
             IF ( any_error( ) .OR. isprun /= 0 ) EXIT
             SELECT CASE ( binary )
             CASE ( .true. )
                READ(iunit,IOSTAT=iostat) nn,(r_xy(i,j),j=1,2),r_depth(i)
                ! neu:
                ! Felder vom Typ Double elementweise mit Werten fuellen.
                ! Die alte Methode des feldweisen Uebertragens fuehrt zum Abbruch
                ! in Java (xy = r_xy)
                IF(iostat == 0) THEN
                   DO j=1,2
                      xy(i,j) = r_xy(i,j)
                   ENDDO
                   depth(i) = r_depth(i)
                ENDIF
             CASE DEFAULT
                CALL kartle_internal ( this )
                IF ( no_error( ) ) READ(karte,*,IOSTAT=iostat) nn,(xy(i,j),j=1,2),depth(i)
             END SELECT
             IF ( iostat == 0 ) THEN
                CONTINUE
             ELSE
                isprun = 3
             END IF
          END DO
          IF ( no_error( ) .AND. isprun == 0 ) THEN
             CALL setup_nrand_object ( this, nrand )
             CALL setup_xy_object    ( this, xy    )
             CALL setup_hv_object    ( this, depth )
          END IF
          SELECT CASE ( binary )
          CASE ( .true. )
             DEALLOCATE ( xy, depth, r_xy, r_depth, STAT=stat )
          CASE DEFAULT
             DEALLOCATE ( xy, depth, STAT=stat )
          END SELECT
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 23105, c_upname, c_modname, stat )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_3
  !
  !! Lesen des vierten Datenblocks mit der Anzahl der Elemente <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_datablock_4 &
       ( binary, this )
    !! Indikator: .true. == Lesen einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       SELECT CASE ( binary )
       CASE ( .true. )
          READ(iunit,IOSTAT=iostat) nelem   ! Anzahl Elemente "nelem"
       CASE DEFAULT
          CALL kartle_internal ( this )
          IF ( no_error( ) ) READ(karte,*,IOSTAT=iostat) nelem ! Anzahl Elemente "nelem"
       END SELECT
       IF ( iostat == 0 ) THEN
          CALL check_nelem ( )
       ELSE
          isprun = 4
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_4
  !
  !! Lesen des f&uuml;nften Datenblocks mit den Knotenkoordinaten und
  !! den Knotentiefen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_datablock_5 &
       ( binary, this )
    !! Indikator: .true. == Lesen einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_5' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       ALLOCATE ( nen(nelem,3), irand(nelem), ks(nelem), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23107, c_upname, c_modname, stat )
       ELSE
          DO i=1,nelem
             IF ( any_error( ) .OR. isprun /= 0 ) EXIT
             SELECT CASE ( binary )
             CASE ( .true. )
                READ(iunit,IOSTAT=iostat) (nen(i,j),j=1,3),irand(i),nn
             CASE DEFAULT
                CALL kartle_internal ( this )
                IF ( no_error( ) ) READ(karte,*,IOSTAT=iostat) (nen(i,j),j=1,3),irand(i),nn
             END SELECT
             IF ( iostat == 0 ) THEN
                CONTINUE
             ELSE
                isprun = 5
             END IF
          END DO
          IF ( no_error( ) .AND. isprun == 0 ) THEN
             ks  = 3       ! ... immer drei Knoten / Polygon
             nen = nen + 1 ! ... damit alle Indices zwischen 1 und nv liegen
             CALL setup_nen_object   ( this, nen   )
             CALL setup_irand_object ( this, irand )
             CALL setup_ks_object    ( this, ks    ) 
          END IF
          DEALLOCATE ( nen, irand, ks, STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 23108, c_upname, c_modname, stat )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_5
  !
  !! Initialisieren einiger Variablen vor dem Schreiben von Daten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_before_write &
       ( this )
    !
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    NULLIFY ( xy, nen, depth, irand, p_nrand, p_iaktkno, p_nelem )
    !
    iunit     =  get_file_unit    ( this%file )
    p_nrand   => get_nrand_object ( this      )
    p_iaktkno => get_nv_object    ( this      )
    p_nelem   => get_ne_object    ( this      )
    iaktkno   = p_iaktkno
    nrand     = p_nrand
    ninnen    = iaktkno - nrand
    nelem     = p_nelem
    stat      = 0
    iostat    = 0
    isprun    = 0
    !
    CALL check_nrand  ( )
    CALL check_ninnen ( )
    CALL check_nelem  ( )
    !
  END SUBROUTINE init_before_write
  !
  !! Initialisieren einiger Variablen nach dem Schreiben von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_after_write &
       ( )
    !
    NULLIFY ( xy, nen, depth, irand, p_nrand, p_iaktkno, p_nelem )
    !
  END SUBROUTINE init_after_write
  !
  !! Ggf. einen Fehler nach dem Schreiben von Daten erzeugen
  SUBROUTINE setup_error_after_write &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=23) , PARAMETER :: c_upname='setup_error_after_write' ! 
    !
    IF ( isprun /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 24100, c_upname, c_modname, iostat )
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       CALL setup_error_act ( '<LokalerFehlerText>', c_localerrors(isprun) )
    END IF
    !
  END SUBROUTINE setup_error_after_write
  ! 
  !! Schreiben des ersten Datenblocks mit der Anzahl der Randknoten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_datablock_1 &
       ( binary, this )
    !! Indikator: .true. == Schreiben einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) ) THEN
       SELECT CASE ( binary )
       CASE ( .true. )
          WRITE(iunit,IOSTAT=iostat) nrand ! Anzahl Randknoten "nrand"
       CASE DEFAULT
          WRITE(iunit,8000,IOSTAT=iostat) nrand ! Anzahl Randknoten "nrand"
       END SELECT
       IF ( iostat /= 0 ) isprun = 1 
    END IF
    !
8000 FORMAT ('C ANZAHL DER RANDKNOTEN NRAND',/ I10)
    !
  END SUBROUTINE write_datablock_1
  !
  !! Schreiben des zweiten Datenblocks mit der Anzahl der Innenknoten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_datablock_2 &
       ( binary, this )
    !! Indikator: .true. == Schreiben einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       SELECT CASE ( binary )
       CASE ( .true. )
          WRITE(iunit,IOSTAT=iostat) ninnen ! Anzahl Randknoten "ninnen"
       CASE DEFAULT
          WRITE(iunit,8010,IOSTAT=iostat) ninnen ! Anzahl Randknoten "ninnen"
       END SELECT
       IF ( iostat /= 0 ) isprun = 2
    END IF
    !
8010 FORMAT ('C ANZAHL DER INNENKNOTEN',/I10)
    !
  END SUBROUTINE write_datablock_2
  !
  !! Schreiben des dritten Datenblocks mit den Knotenkoordinaten und
  !! den Knotentiefen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_datablock_3 &
       ( binary, this )
    !! Indikator: .true. == Schreiben einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       xy    => get_xy_object ( this ) 
       depth => get_hv_object ( this ) 
       SELECT CASE ( binary )
       CASE ( .true. )
          DO i=1,iaktkno
             WRITE(iunit,IOSTAT=iostat) i-1,(REAL(xy(i,j)),j=1,2),REAL(depth(i))
             IF ( iostat /= 0 ) isprun = 3
             IF ( isprun /= 0 ) EXIT
          END DO
       CASE DEFAULT
          WRITE(iunit,8020,IOSTAT=iostat)
          IF ( iostat /= 0 ) isprun = 6
          DO i=1,iaktkno
             WRITE(iunit,8030,IOSTAT=iostat) i-1,(xy(i,j),j=1,2),depth(i)
             IF ( iostat /= 0 ) isprun = 3
             IF ( isprun /= 0 ) EXIT
          END DO
       END SELECT
    END IF
    !
8020 FORMAT ('C KOORDINATEN UND TIEFEN DER KNOTEN',/ &
             'C ---------------------------------',/ &
             'C DIE ERSTEN NRAND WERTE SIND DIE RANDKNOTEN',/ &
             'C KNOTENNR.  X-KOORD.  Y-KOORD.  WASSERTIEFE',/ &
             'C  >= 0      POS.NACH  POS.NACH  POS.NACH',/ &
             'C  < 0 ENDE  RECHTS    OBEN      UNTEN')
8030 FORMAT (I10,3F15.4)
    !
  END SUBROUTINE write_datablock_3
  !
  !! Schreiben des vierten Datenblocks mit der Anzahl der Elemente <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_datablock_4 &
       ( binary, this )
    !! Indikator: .true. == Schreiben einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       SELECT CASE ( binary )
       CASE ( .true. )
          WRITE(iunit,IOSTAT=iostat) nelem ! Anzahl Elemente "nelem"
       CASE DEFAULT
          WRITE(iunit,8040,IOSTAT=iostat) nelem ! Anzahl Elemente "nelem"
       END SELECT
       IF ( iostat /= 0 ) isprun = 4
    END IF
    !
8040 FORMAT ('C ANZAHL DER ELEMENTE',/ I10)
    !
  END SUBROUTINE write_datablock_4
  !
  !! Schreiben des f&uuml;nften Datenblocks mit dem Knotenverzeichnis und
  !! den Randkennungen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_datablock_5 &
       ( binary, this )
    !! Indikator: .true. == Schreiben einer Bin&auml;rdatei (sonst ASCII)
    LOGICAL , INTENT(IN) :: binary ! 
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       nen   => get_nen_object   ( this )
       irand => get_irand_object ( this )
       SELECT CASE ( binary )
       CASE ( .true. )
          DO i=1,nelem
             WRITE(iunit,IOSTAT=iostat) ((nen(i,j)-1),j=1,3),irand(i),i-1 ! subtrahiere 1
             IF ( iostat /= 0 ) isprun = 5
             IF ( isprun /= 0 ) EXIT
          END DO
       CASE DEFAULT
          WRITE(iunit,8050,IOSTAT=iostat) 
          IF ( iostat /= 0 ) isprun = 7
          DO i=1,nelem
             WRITE(iunit,8060,IOSTAT=iostat) ((nen(i,j)-1),j=1,3),irand(i),i-1 ! subtrahiere 1
             IF ( iostat /= 0 ) isprun = 5
             IF ( isprun /= 0 ) EXIT
          END DO
          WRITE(iunit,8070,IOSTAT=iostat)
          IF ( iostat /= 0 ) isprun = 8
       END SELECT
    END IF
    !
8050 FORMAT ('C ELEMENTVERZEICHNIS',/ &
             'C ------------------',/ &
             'C KNOTEN IN EXTERNER NUMERIERUNG',/ &
             'C  I /  J /  K LAGER ELEMENTNR.')
8060 FORMAT (5I10)
8070 FORMAT ('C ENDE DATEI GITTER05.DAT')
    !
  END SUBROUTINE write_datablock_5
  !
END MODULE m_h_grid_gitter05
! TailOfPackageUserInterface -----------------------------------------------
