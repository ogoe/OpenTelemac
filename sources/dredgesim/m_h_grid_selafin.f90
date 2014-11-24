! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Lesen und Schreiben von Gitterdaten f&uuml;r Dateityp "selafin"</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 4.1 vom 08/22/05, Quellcode: mod_m_h_grid_selafin.f90
!! <HR>
!! read and write methods for "selafin" files                        <BR>
!! <HR>
!! Copyright-Hinweis
!!                                                                   <BR>
!  <CopyrightWildcard>
!
! Entwicklungsgeschichte des Moduls
! 01.01 : 2002-07-21 : G. Lang    : Startversion
! 01.02 : 2003-04-09 : P. Schade  : Lesen und Schreiben von Komponente knolg 
! 01.03 : 2003-06-04 : P. Schade  : Lesen und Schreiben von nptfr, nptir
! 02.01 : 2005-03-07 : G. Lang    : Erweiterungen OpenMI (unvollstaendig)
! 02.02 : 2005-03-10 : G. Lang    : div. Erweiterungen fuer Konfigurationsphase OpenMI
! 02.03 : 2005-03-16 : G. Lang    : Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries 
! 02.04 : 2005-07-07 : G. Lang    : Zuweisung mit Typkonversion in Loop umgeschrieben
! 03.01 : 2005-07-21 : G. Lang    : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
! 04.01 : 2005-08-22 : G. Lang    : Lesen/Schreiben Delft3D-Gitternetz
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Lesen einer Datei im Format "selafin";
!!   <LI> Schreiben einer Datei im Format "selafin".
!! </OL>
!! Weitere Informationen zum Dateiformat unter
!! <A HREF="http://www.hamburg.baw.de/fkb/selafin/selaf-de.htm"><EM>selafin</EM></A>.
!! <HR>
!! <H3>Gebrauch der Methoden dieses Moduls</H3>
!! Die in diesem Modul bereitgestellten Methoden d&uuml;rfen nur
!! von zu dem Paket "h_grid" geh&ouml;renden Modulen direkt 
!! verwendet werden. Insbesondere ist das direkte Verwenden von
!! Methoden aus anderen Paketen unzul&auml;ssig. 
!
MODULE m_h_grid_selafin
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
       t_h_grid,            & ! 
       ! Parameter und Konstantwerte
       all_errors,          & ! Fehlermeldunegn
       ! Routinen / Interfaces
       setup_time_object,   &
       setup_text_object,   &
       setup_ks_object,     &
       setup_nen_object,    &
       setup_nptfr_object,  &
       setup_nptir_object,  &
       setup_ipobo_object,  &
       setup_knolg_object,  &
       setup_xy_object,     &
       setup_hv_object,     &
       !
       get_text_object,     &
       get_ne_object,       &
       get_nv_object,       &
       get_ncsize_object,   &
       get_nptfr_object,    &
       get_nptir_object,    &
       get_ks_object,       &
       get_nen_object,      &
       get_ipobo_object,    &
       get_knolg_object,    &
       get_xy_object,       &
       get_hv_object
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
  !! Lesen einer sequentiellen Bin&auml;r-Datei des Typs "selafin" (BAW)
  INTERFACE read_h_grid_selafin_seq_bin
     MODULE PROCEDURE read_h_grid_selafin_seq_bin_d
  END INTERFACE
  !! Schreiben einer sequentiellen Bin&auml;r-Datei des Typs "selafin" (BAW)
  INTERFACE write_h_grid_selafin_seq_bin
     MODULE PROCEDURE write_h_grid_selafin_seq_bin_d
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren (optional, falls sinnvoll)
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: read_h_grid_selafin_seq_bin  ! SEQUENTIAL, UNFORMATTED
  PUBLIC :: write_h_grid_selafin_seq_bin ! SEQUENTIAL, UNFORMATTED
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Konstantwerte (Parameter)
  !
  !! maximale Anzahl verschiedener (lokaler) Fehlertexte
  INTEGER , PARAMETER :: c_noflocalerrors=12 ! 
  !! lokale Fehlertexte
  CHARACTER (LEN=45) , PARAMETER :: c_localerrors(c_noflocalerrors) = & ! 
       !   123456789 123456789 123456789 123456789 12345
       (/ 'Titel der Telemac-Studie                     ' , & !  1
          'Anzahl der Telemac-Variablen                 ' , & !  2
          'Name und Einheit der gespeicherten Variablen ' , & !  3
          'zehn ganzzahlige Groessen                    ' , & !  4
!RK hier fehlt ggf. das Datum, wenn ic(10)=1
          'NELEM, NPOIN, NDP, IDUM                      ' , & !  5
          'Knotenverzeichnis IKLE der Elemente          ' , & !  6
          'Randknotenverzeichnis IPOBO                  ' , & !  7 fuer r/w_datablock_7 
          'Lesen der X-Koordinaten der Knoten           ' , & !  8
          'Lesen der Y-Koordinaten der Knoten           ' , & !  9
          'Zeitpunkt ZZ                                 ' , & ! 10
          'Telemac-Datensatz fuer den ersten Zeitpunkt  ' , & ! 11
          'Zeiger auf Knoten des Gesamtgitters KNOLG    ' /)  ! 12 fuer r/w_datablock_7 
  !
  !! maximale Anzahl vordefinierter Variablennamen
  INTEGER , PARAMETER :: c_maxcnev=3 ! 
  !! vordefinierte Variablennamen
!nd
  CHARACTER (LEN=32) , PARAMETER :: c_cnev(c_maxcnev) = & ! 
       !   123456789 123456789 123456789 12
       (/ 'BOTTOM          M               ' , &
          'FREE SURFACE    M               ' , &
          'WATER DEPTH     M               ' /)

  CHARACTER (LEN=32) , PARAMETER :: sc_cnev(c_maxcnev) = & ! 
       !   123456789 123456789 123456789 12
       (/ 'BOTTOM          m               ' , &
          'FREE SURFACE    m               ' , &
          'WATER DEPTH     m               ' /)
!nd
  !! vordefinierte Variablennamen franzoesisch
  CHARACTER (LEN=32) , PARAMETER :: fc_cnev(c_maxcnev) = & ! 
       !   123456789 123456789 123456789 12
       (/ 'FOND            M               ' , &
          'SURFACE LIBRE   M               ' , &
          'HAUTEUR D''EAU   M               ' /)
  !
  !! Name des Moduls
  CHARACTER (LEN=16), PARAMETER :: c_modname = 'm_h_grid_selafin' ! 
  !! maximale L&auml;nge des Feldes "nbv(:)"
  INTEGER           , PARAMETER :: maxnbv=2 ! 
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Hilfsfeld f&uuml;r Fehlertexte
  CHARACTER (LEN=15)  :: ctxt  ! 
  !! log. Kanalnummer f&uuml;r das Lesen der Daten
  INTEGER :: iunit  ! 
  !! Z&auml;hlervariable
  INTEGER :: i, j ! 
  !! Statusvariable (READ/WRITE)
  INTEGER :: stat, iostat ! 
  !! Zeiger auf Text in Fehlermeldung
  INTEGER :: isprun ! 
  !! Feld zum Lesen einer Kommentarzeile
  CHARACTER (LEN=80)               :: kommentar         ! 
  !! aktuelle Anzahl der Variablen mit ... <BR>
  !! (1) = linearer Interpolation <BR>
  !! (2) = quadratischer Interpolation
  INTEGER                          :: nbv(maxnbv)       ! 
  !! Textfeld Name und Einheit der Variablen <BR>
  CHARACTER (LEN=32) , ALLOCATABLE :: cnev(:)           ! 
  !! Indikator, welche der Gr&ouml;&szlig;en "c_cnev(:)" in der Datei vorhanden sind
  LOGICAL                          :: l_cnev(c_maxcnev) ! 
  !! Zeiger auf den Speicherort der Gr&ouml;&szlig;en
  INTEGER                          :: idx_cnev(c_maxcnev) ! 
  !! nicht weiter ben&ouml;tigte Dummy-Gr&ouml;&szlig;en
  INTEGER                          :: idummy(10)        ! 
!RK
  !! Datumsangabe
  INTEGER                          :: idate(3)        ! 
  !! Anzahl der Elemente
  INTEGER                          :: nelem             ! 
  !! Anzahl der Knoten
  INTEGER                          :: npoin             ! 
  !! maximale Anzahl der Knoten je Element
  INTEGER                          :: ndp               ! 
  !! skalarer Integer-Dummy
  INTEGER                          :: idum              ! 
  !! Klartext-Beschreibung des Modellgebietes
  CHARACTER (LEN=80)    , POINTER  :: p_text(:)         ! 
  !! Anzahl der Polygone
  INTEGER               , POINTER  :: p_ne              ! 
  !! Anzahl der Knoten
  INTEGER               , POINTER  :: p_nv              ! 
  !! Anzahl der Randknoten eines Teilgebietes (TELEMAC parallel)
  INTEGER               , POINTER  :: p_nptfr           ! 
  !! Anzahl der Interface-Knoten eines Teilgebietes (TELEMAC parallel)
  INTEGER               , POINTER  :: p_nptir           ! 
  !! Knotenverzeichnis
  INTEGER               , POINTER  :: p_nen(:,:)        ! 
  !! Anzahl der Knoten/Kanten im Polygon
  INTEGER               , POINTER  :: p_ks(:)           ! 
  !! Randkennungen der Knoten
  INTEGER               , POINTER  :: p_ipobo(:)        ! 
  !! Zeiger auf die Knotennummern des Gesamtgitters <BR>
  !! fuer Knoten im lokalen Grid (TELEMAC parallel rechnen)
  INTEGER               , POINTER  :: p_knolg(:)        ! 
  !! skalarer Real-Dummy
  REAL                             :: rdum              ! 
  !! Koordinaten der Knoten des Gitters
  REAL (KIND=Double)    , POINTER  :: p_xy(:,:)         ! 
  REAL                  , POINTER  :: r_xy(:,:)         ! 
  !! Tiefe an den Knoten der Elemente
  REAL (KIND=Double)    , POINTER  :: p_hv(:)           ! 
  !! Telemac-Datensatz f&uuml;r den ersten Zeitpunkt
  REAL                  , POINTER  :: r_tm(:,:)         ! 
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
  !! &Uuml;betragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! f&uuml;r die Datei gilt FORM="UNFORMATTED", ACCESS="SEQUENTIAL"            <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_selafin_seq_bin_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt zur Aufnahme der Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=29), PARAMETER  :: c_upname='read_h_grid_selafin_seq_bin_d' !
    !
    CALL init_before_readwrite ( this )
    !
    CALL read_datablock_1  ( this )
    CALL read_datablock_2  ( this )
    CALL read_datablock_3  ( this )
    CALL read_datablock_4  ( this )
    CALL read_datablock_5  ( this )
    CALL read_datablock_6  ( this )
    ! Lesen der Randkennungen der Knoten oder alternativ
    ! der Zeiger auf die Knotennummern des Gesamtgitters 
    CALL read_datablock_7  ( this )
    CALL read_datablock_8  ( this )
    CALL read_datablock_9  ( this )
    CALL read_datablock_10 ( this )
    !
    CALL setup_error_after_read ( this )
    CALL init_after_readwrite   (      )
    !
    CALL setup_time_object ( this, get_datetime_from_system ( ) )
    !
  END SUBROUTINE read_h_grid_selafin_seq_bin_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-WRITE-Methoden <<< [ERR_NO = 24000 bis 24999]
  ! ----------------------------------------------------------------------
  !
  !! &Uuml;betragen der in dem (Arbeits-) Objekt stehenden Daten in eine Datei  <BR>
  !! f&uuml;r die Datei gilt FORM="UNFORMATTED", ACCESS="SEQUENTIAL"            <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_selafin_seq_bin_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt mit den Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=30), PARAMETER  :: c_upname='write_h_grid_selafin_seq_bin_d' !
    !
    CALL init_before_readwrite ( this )
    !
    CALL write_datablock_1  ( this )
    CALL write_datablock_2  ( this )
    CALL write_datablock_3  ( this )
    CALL write_datablock_4  ( this )
    CALL write_datablock_5  ( this )
    CALL write_datablock_6  ( this )
    ! Lesen der Randkennungen der Knoten oder alternativ
    ! der Zeiger auf die Knotennummern des Gesamtgitters 
    CALL write_datablock_7  ( this )
    CALL write_datablock_8  ( this )
    CALL write_datablock_9  ( this )
    CALL write_datablock_10 ( this )
    !
    CALL setup_error_after_write ( this )
    CALL init_after_readwrite    (      )
    !
  END SUBROUTINE write_h_grid_selafin_seq_bin_d
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
  !! Ggf. einen Fehler nach dem Lesen von Daten erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_error_after_read &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=22) , PARAMETER :: c_upname='setup_error_after_read' ! 
    !
    IF ( isprun /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 23300, c_upname, c_modname, iostat )
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       CALL setup_error_act ( '<LokalerFehlerText>', c_localerrors(isprun) )
    END IF
    !
  END SUBROUTINE setup_error_after_read
  !
  !! Ggf. einen Fehler nach dem Schreiben von Daten erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_error_after_write &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=23) , PARAMETER :: c_upname='setup_error_after_write' ! 
    !
    IF ( isprun /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 24300, c_upname, c_modname, iostat )
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       CALL setup_error_act ( '<LokalerFehlerText>', c_localerrors(isprun) )
    END IF
    !
  END SUBROUTINE setup_error_after_write
  !
  !! Initialisieren einiger Variablen vor dem Lesen/Schreiben von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_before_readwrite &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    iunit     = get_file_unit ( this%file )
    stat      = 0
    iostat    = 0
    isprun    = 0
    !
    kommentar = REPEAT( ' ', LEN(kommentar) )
    nbv(:)    = 0
    idummy(:) = 0
!RK
    idate(:)  = 0
    rdum      = 0.0
    nelem     = 0
    npoin     = 0
    ndp       = 0
    idum      = 0
    l_cnev    = .false.
    idx_cnev  = 0
    !
    NULLIFY ( p_nv, p_ne, p_nptfr, p_nptir, p_nen, p_ks, &
          p_ipobo, p_knolg, p_xy, r_xy, p_hv, r_tm, p_text )
    !
  END SUBROUTINE init_before_readwrite
  !
  !! Re-Initialisieren einiger Variablen vor nach dem Lesen von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_after_readwrite &
       (  )
    !! Name der Subroutine
    CHARACTER (LEN=20) , PARAMETER :: c_upname='init_after_readwrite' ! 
    !
    NULLIFY ( p_nv, p_ne,  p_nptfr, p_nptir, p_nen, p_ks, &
          p_ipobo, p_knolg, p_xy, r_xy, p_hv, r_tm, p_text )
    !
  END SUBROUTINE init_after_readwrite
  !
  !! Lesen des Textes mit der Bezeichnung des Simulationslaufes <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE read_datablock_1 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,IOSTAT=iostat ) kommentar
       IF ( iostat == 0 ) THEN
          CALL setup_text_object  ( this, kommentar )
       ELSE
          isprun = 1 
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_1
  !
  !! Schreiben des Textes mit der Bezeichnung des Simulationslaufes <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE write_datablock_1 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_text => get_text_object ( this )
       IF ( no_error( ) ) THEN
          WRITE (iunit,IOSTAT=iostat ) p_text(1)
          IF ( iostat /= 0 ) isprun = 1
       END IF
    END IF
    !
  END SUBROUTINE write_datablock_1
  !
  !! Lesen der Anzahl der abgelegten Variablen <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE read_datablock_2 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_2' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,IOSTAT=iostat ) nbv(:)
       IF ( iostat == 0 ) THEN
          IF ( SUM( nbv(:) ) > 0 ) THEN
             ALLOCATE ( cnev( SUM( nbv(:) ) ), STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 23301, c_upname, c_modname, stat )
                WRITE(ctxt,'(I10)') SUM( nbv(:) )
                CALL setup_error_act ( '<AktDim1>', ctxt )
             END IF
          END IF
       ELSE
          isprun = 2 
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_2
  !
  !! Schreiben der Anzahl der abgelegten Variablen <BR>
  !! es wird ausschlie&szlig;lich die Variable "BOTTOM" geschrieben <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE write_datablock_2 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='write_datablock_2' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       nbv(1) = 1
       nbv(2) = 0
       WRITE (iunit,IOSTAT=iostat ) nbv(:)
       IF ( iostat /= 0 ) isprun = 2
    END IF
    !
  END SUBROUTINE write_datablock_2
  !
  !! Lesen von Name und Einheit der Variablen <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE read_datablock_3 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_3' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       DO i=1,SIZE( cnev )
          READ (iunit,IOSTAT=iostat ) cnev(i)
          IF ( iostat /= 0 ) EXIT
       END DO
       IF ( iostat == 0 ) THEN
          DO i=1,SIZE(l_cnev(:)) 
             DO j=1,SIZE(cnev(:))
!nd                IF ( cnev(j) == c_cnev(i) ) THEN
                IF ( cnev(j) == c_cnev(i) .OR. cnev(j) == fc_cnev(i) .OR. cnev(j) == sc_cnev(i) ) THEN
                   l_cnev(i)   = .true.
                   idx_cnev(i) = j
                END IF
             END DO
          END DO
          IF ( .NOT. ( l_cnev(1) .OR. ALL( l_cnev(2:3) ) ) ) THEN
             CALL setup_error_act ( all_errors(:), 23303, c_upname, c_modname )
             CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
             WRITE(ctxt,'(L10)') l_cnev(1)
             CALL setup_error_act ( '<VariableName1>', c_cnev(1) )
             CALL setup_error_act ( '<Exist1>', ctxt )
             WRITE(ctxt,'(L10)') l_cnev(2)
             CALL setup_error_act ( '<VariableName2>', c_cnev(2) )
             CALL setup_error_act ( '<Exist2>', ctxt )
             WRITE(ctxt,'(L10)') l_cnev(3)
             CALL setup_error_act ( '<VariableName3>', c_cnev(3) )
             CALL setup_error_act ( '<Exist3>', ctxt )
          END IF
       ELSE
          isprun = 3
       END IF
       DEALLOCATE ( cnev, STAT=stat )
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 23302, c_upname, c_modname, stat )
    !
    END IF
    !
  END SUBROUTINE read_datablock_3
  !
  !! Schreiben von Name und Einheit der Variablen <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE write_datablock_3 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='write_datablock_3' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       WRITE (iunit,IOSTAT=iostat ) c_cnev(1)
       IF ( iostat /= 0 ) isprun = 3
    END IF
    !
  END SUBROUTINE write_datablock_3
  !
  !! Lesen verschiedener meist weiter ben&ouml;tigter ganzzahliger Gr&ouml;&szlig;en; <BR>
  !! nur beim parallelen Rechnen mit TELEMAC werden nptfr und nptir benoetigt; <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_4 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_4' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,IOSTAT=iostat ) idummy(:)
!RK Datumsblock einlesen, wenn idummy(10) = 1
       IF(idummy(10) == 1)  READ (iunit,IOSTAT=iostat ) idate(:)
       !
       IF ( iostat == 0 ) THEN
          ! falls Komponente ncsize > 0 
          ! -> Komponenten "nptfr" und "nptir" setzen
          IF (ASSOCIATED(get_ncsize_object(this))) THEN
             IF (get_ncsize_object(this) > 0) THEN
                !
                CALL setup_nptfr_object ( this, idummy(8))
                CALL setup_nptir_object ( this, idummy(9))
                !
             ENDIF
          ENDIF
       ELSE
          isprun = 4
       ENDIF
       !
    END IF
    !
  END SUBROUTINE read_datablock_4
  !
  !! Schreiben verschiedener nicht weiter ben&ouml;tigter ganzzahliger Gr&ouml;&szlig;en <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_4 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='write_datablock_4' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       !
       IF (ASSOCIATED(get_ncsize_object(this))) THEN
          IF (get_ncsize_object(this) > 0) THEN
             !
             p_nptfr => get_nptfr_object ( this )
             IF ( no_error( ) ) THEN
                idummy(8) = p_nptfr
             END IF
             !
             p_nptir => get_nptir_object ( this )
             IF ( no_error( ) ) THEN
                idummy(9) = p_nptir
             END IF
             !
          ENDIF
       ENDIF
       !
       WRITE (iunit,IOSTAT=iostat ) idummy(:)
!RK Datumsblock schreiben, wenn idummy=10
       IF(idummy(10)==1)  WRITE (iunit,IOSTAT=iostat ) idate(:)
       IF ( iostat /= 0 ) isprun = 4
    END IF
    !
  END SUBROUTINE write_datablock_4
  !
  !! Lesen der Variablen zur Beschreibung der Gittergr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE read_datablock_5 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_5' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,IOSTAT=iostat ) nelem, npoin, ndp, idum
       IF ( iostat == 0 ) THEN
          CALL check_nelem_npoin_ndp ( this )
          IF ( no_error( ) ) THEN
             ALLOCATE ( p_ks(nelem) , STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 23304, c_upname, c_modname, stat )
                WRITE(ctxt,'(I10)') nelem
                CALL setup_error_act ( '<AktDim1>', ctxt )
             ELSE
                p_ks(:) = ndp
                CALL setup_ks_object ( this, p_ks(:) )
                DEALLOCATE ( p_ks, STAT=stat )
                IF ( stat /= 0 ) &
                     CALL setup_error_act ( all_errors(:), 23305, c_upname, c_modname, stat )
             END IF
          END IF
       ELSE
          isprun = 5
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_5
  !
  !! Schreiben der Variablen zur Beschreibung der Gittergr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE write_datablock_5 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='write_datablock_5' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_ne => get_ne_object ( this )
       p_nv => get_nv_object ( this )
       p_ks => get_ks_object ( this )
       nelem = p_ne
       npoin = p_nv
       ndp  = MAXVAL( p_ks(:) )
       CALL check_nelem_npoin_ndp ( this )
       IF ( no_error( ) ) THEN
          WRITE (iunit,IOSTAT=iostat ) nelem, npoin, ndp, idum
          IF ( iostat /= 0 ) isprun = 5
       END IF
    END IF
    !
  END SUBROUTINE write_datablock_5
  !
  !! Lesen des Knotenverzeichnisses der Elemente <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE read_datablock_6 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_6' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       ALLOCATE ( p_nen(nelem,ndp), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23306, c_upname, c_modname, stat )
          WRITE (ctxt,'(I10)') nelem
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE (ctxt,'(I10)') ndp
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          READ (iunit,IOSTAT=iostat ) ( (p_nen(i,j),j=1,ndp),i=1,nelem )
          IF ( iostat == 0 ) THEN
             CALL setup_nen_object ( this, p_nen )
          ELSE
             isprun = 6
          END IF
          DEALLOCATE ( p_nen, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act  ( all_errors(:), 23307, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_6
  !
  !! Schreiben des Knotenverzeichnisses der Elemente <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE write_datablock_6 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='write_datablock_6' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_nen => get_nen_object ( this )
       IF ( no_error( ) ) THEN
          WRITE (iunit,IOSTAT=iostat ) ( (p_nen(i,j),j=1,ndp),i=1,nelem )
          IF ( iostat /= 0 ) isprun = 6
       END IF
    END IF
    !
  END SUBROUTINE write_datablock_6
  !
  !! Lesen des Randknotenverzeichnisses oder alternativ der<BR>
  !! Zeiger auf die Knotennummern des Gesamtgitters (TELEMAC parallel rechnen)<BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE read_datablock_7 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_7' ! 
    !! log. Kennung ob ipobo gelesen werden soll
    LOGICAL :: l_ipobo
    !
    l_ipobo = .true.
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       !
       ! falls Komponente ncsize > 0 
       ! -> TELEMAC soll parallel gerechnet werden
       ! -> mit dem naechsten gelesenen Datensatz Komponente knolg fuellen
       asso: IF (ASSOCIATED(get_ncsize_object(this))) THEN
          IF (get_ncsize_object(this) > 0) THEN
             !
             l_ipobo = .false.
             ALLOCATE ( p_knolg(npoin), STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 23315, c_upname, c_modname, stat )
                WRITE (ctxt,'(I10)') npoin
                CALL setup_error_act ( '<AktDim1>', ctxt )
             ELSE
                READ (iunit,IOSTAT=iostat ) ( p_knolg(i),i=1,npoin )
                IF ( iostat == 0 ) THEN
                   CALL setup_knolg_object ( this, p_knolg )
                ELSE
                   isprun = 12
                END IF
                DEALLOCATE ( p_knolg, STAT=stat )
                IF ( stat /= 0 ) &
                     CALL setup_error_act  ( all_errors(:), 23316, c_upname, c_modname, stat )
             END IF
          END IF
       ENDIF asso
       !
       ! es soll nicht parallel gerechnet werden;
       ! mit dem naechsten gelesenen Datensatz Komponente ipobo fuellen
       IF (l_ipobo) THEN
          ALLOCATE ( p_ipobo(npoin), STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 23308, c_upname, c_modname, stat )
             WRITE (ctxt,'(I10)') npoin
             CALL setup_error_act ( '<AktDim1>', ctxt )
          ELSE
             READ (iunit,IOSTAT=iostat ) ( p_ipobo(i),i=1,npoin )
             IF ( iostat == 0 ) THEN
                CALL setup_ipobo_object ( this, p_ipobo )
             ELSE
                isprun = 7
             END IF
             DEALLOCATE ( p_ipobo, STAT=stat )
             IF ( stat /= 0 ) &
                  CALL setup_error_act  ( all_errors(:), 23309, c_upname, c_modname, stat )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_7
  !
  !! Schreiben des Randknotenverzeichnisses oder alternativ der<BR>
  !! Zeiger auf die Knotennummern des Gesamtgitters (TELEMAC parallel rechnen)<BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE write_datablock_7 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='write_datablock_7' ! 
    !! log. Kennung ob ipobo gelesen werden soll
    LOGICAL :: l_ipobo
    !
    l_ipobo = .true.
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       !
       IF (ASSOCIATED(get_ncsize_object(this))) THEN
          IF (get_ncsize_object(this) > 0) THEN
             !
             l_ipobo = .false.
             p_knolg => get_knolg_object ( this )
             IF ( no_error( ) ) THEN
                WRITE (iunit,IOSTAT=iostat ) ( p_knolg(i),i=1,npoin )
                IF ( iostat /= 0 ) isprun = 12
             END IF
             !
          ENDIF
       ENDIF
       !
       IF (l_ipobo) THEN
          p_ipobo => get_ipobo_object ( this )
          IF ( no_error( ) ) THEN
             WRITE (iunit,IOSTAT=iostat ) ( p_ipobo(i),i=1,npoin )
             IF ( iostat /= 0 ) isprun = 7
          END IF
       ENDIF
       !
    END IF
    !
  END SUBROUTINE write_datablock_7
  !
  !! Lesen der Koordinaten der Knoten <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE read_datablock_8 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_8' ! 
    !! Hilfsvariable
    INTEGER :: i, j ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       ALLOCATE ( r_xy(npoin,2), p_xy(npoin,2), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23310, c_upname, c_modname, stat )
          WRITE (ctxt,'(I10)') npoin
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE (ctxt,'(I10)') 2
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          READ (iunit,IOSTAT=iostat ) ( r_xy(i,1),i=1,npoin )
          IF ( iostat == 0 ) THEN
             READ (iunit,IOSTAT=iostat ) ( r_xy(i,2),i=1,npoin )
             IF ( iostat == 0 ) THEN
                DO j=1,2
                   DO i=1,npoin
                      p_xy(i,j) = r_xy(i,j)
                   END DO
                END DO
                CALL setup_xy_object ( this, p_xy )
             ELSE
                isprun = 9
             END IF
          ELSE
             isprun = 8
          END IF
          DEALLOCATE ( p_xy, r_xy, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act  ( all_errors(:), 23311, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_8
  !
  !! Schreiben der Koordinaten der Knoten <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE write_datablock_8 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='write_datablock_8' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_xy => get_xy_object ( this )
       IF ( no_error ( ) ) THEN
          WRITE (iunit,IOSTAT=iostat ) ( REAL(p_xy(i,1)),i=1,npoin )
          IF ( iostat == 0 ) THEN
             WRITE (iunit,IOSTAT=iostat ) ( REAL(p_xy(i,2)),i=1,npoin )
             IF ( iostat /= 0 ) isprun = 9
          ELSE
             isprun = 8
          END IF
       END IF
    END IF
    !
  END SUBROUTINE write_datablock_8
  !
  !! Lesen der (ersten) Zeitangabe <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_9 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='read_datablock_9' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,IOSTAT=iostat ) rdum
       IF ( iostat /= 0 ) isprun = 10
    END IF
    !
  END SUBROUTINE read_datablock_9
  !
  !! Schreiben der (ersten) Zeitangabe <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_9 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='write_datablock_9' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       WRITE (iunit,IOSTAT=iostat ) rdum
       IF ( iostat /= 0 ) isprun = 10
    END IF
    !
  END SUBROUTINE write_datablock_9
  !
  !! Lesen der Daten f&uuml;r den ersten Zeitpunkt <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE read_datablock_10 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17) , PARAMETER :: c_upname='read_datablock_10' ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       ALLOCATE ( r_tm(npoin,SUM(nbv(:))), p_hv(npoin), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23312, c_upname, c_modname, stat )
          WRITE (ctxt,'(I10)') npoin
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE (ctxt,'(I10)') SUM(nbv(:))
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          DO j=1,SUM(nbv(:))
             READ(iunit,IOSTAT=iostat) ( r_tm(i,j),i=1,npoin )
             IF ( iostat /= 0 ) EXIT
          END DO
          IF ( iostat == 0 ) THEN
             IF ( l_cnev(1) ) THEN
                DO i=1,npoin
                   p_hv(i) = REAL( -r_tm(i,idx_cnev(1)), KIND=Double )
                END DO
             ELSE IF ( ALL( l_cnev(2:3) ) ) THEN
                DO i=1,npoin
                   p_hv(i) = REAL( r_tm(i,idx_cnev(3))-r_tm(i,idx_cnev(2)), KIND=Double )
                END DO
             ELSE
                DO i=1,npoin
                   p_hv(i) = -10001.0_Double
                END DO
             END IF
             CALL setup_hv_object ( this, p_hv )
          ELSE
             isprun = 11
          END IF
          DEALLOCATE ( r_tm, p_hv, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 23313, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_10
  !
  !! Schreiben der Daten f&uuml;r den ersten Zeitpunkt <BR>
  !! Subroutine erzeugt Fehlermeldungen 
  SUBROUTINE write_datablock_10 &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=18) , PARAMETER :: c_upname='write_datablock_10' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_hv => get_hv_object( this )
       IF ( no_error( ) ) THEN
          WRITE(iunit,IOSTAT=iostat) ( -REAL(p_hv(i)),i=1,npoin )
          IF ( iostat /= 0 ) isprun = 11
       END IF
    END IF
    !
  END SUBROUTINE write_datablock_10
  !
  !! Pr&uuml;fen verschiedener Angaben zur Gr&ouml;&szlig;e des Gitters
  SUBROUTINE check_nelem_npoin_ndp &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='check_nelem_npoin_ndp' ! 
    !
    IF ( nelem < 1 .OR. npoin < 3 .OR. ndp < 3 ) THEN
       CALL setup_error_act ( all_errors(:), 23314, c_upname, c_modname )
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       WRITE(ctxt,'(I10)') nelem
       CALL setup_error_act ( '<AktNelem>', ctxt )
       WRITE(ctxt,'(I10)') npoin
       CALL setup_error_act ( '<AktNpoin>', ctxt )
       WRITE(ctxt,'(I10)') ndp
       CALL setup_error_act ( '<AktNdp>', ctxt )
    END IF
    !
  END SUBROUTINE check_nelem_npoin_ndp
  !
END MODULE m_h_grid_selafin
! TailOfPackageUserInterface -----------------------------------------------
