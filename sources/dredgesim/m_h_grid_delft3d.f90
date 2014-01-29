! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Lesen und Schreiben von Gitterdaten f&uuml;r Dateityp "delft3d"</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 4.3 vom 11/23/05, Quellcode: mod_m_h_grid_delft3d.f90
!! <HR>
!! read and write methods for "delft3d" files                        <BR>
!! <HR>
!! Copyright-Hinweis
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-08-22 : G. Lang    : Startversion
!  04.01 : 2005-08-22 : G. Lang    : Lesen/Schreiben Delft3D-Gitternetz
!  04.02 : 2005-11-15 : G. Lang    : Anpassungen wegen neuer Delft3D-Formate
!  04.03 : 2005-11-23 : G. Lang    : Erweiterungen *.thd, *.lwl, *.ext
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Lesen einer Datei im Format "delft3d";
!!   <LI> Schreiben einer Datei im Format "delft3d".
!! </OL>
!! Weitere Informationen zum Dateiformat unter ... noch nicht vorhanden.
!! 
!! <HR>
!! <H3>Gebrauch der Methoden dieses Moduls</H3>
!! Die in diesem Modul bereitgestellten Methoden d&uuml;rfen nur
!! von zu dem Paket "h_grid" geh&ouml;renden Modulen direkt 
!! verwendet werden. Insbesondere ist das direkte Verwenden von
!! Methoden aus anderen Paketen unzul&auml;ssig. 
!
MODULE m_h_grid_delft3d
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten
  USE b_constants, ONLY : &
       ! Parameter
       Double
  !
  ! [A.2] Basis-Modul "Fehler"
  USE b_error, ONLY :       &
       ! Routinen
       no_error,            &
       any_error,           &
       setup_error_act
  !
  ! [A.3] Basis-Modul "File"
  USE b_file, ONLY :             &
       ! Datentyp
       t_file,                   &
       ! Konstante
       c_file_path_and_name_len, &
       !   Routinen / Interfaces
       new_file,                 &
       kill_file,                &
       open_file,                &
       close_file,               &
       file_exists,              &
       get_file_unit,            &
       get_file_name,            &
       get_file_path,            &
       get_file_path_and_name,   &
       set_file_unit,            &
       set_file_path_and_name
  ! 
  ! [A.4] Basis-Modul "Datum+Uhrzeit"
  USE b_datetime, ONLY : &
       ! Routinen / Interfaces
       get_datetime_from_system
  ! 
  ! [A.5] Basis-Modul "2D-Punkte"
  USE b_point_2d, ONLY : &
       ! Datentyp
       t_point_2d,       &
       ! Routinen / Interfaces
       new_point_2d,     &
       kill_point_2d,    &
       set_point_2d_x,   &
       set_point_2d_y,   &
       inside_point_2d
  ! 
  ! [A.6] Basis-Modul "Datum+Uhrzeit"
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
  USE m_h_grid_data, ONLY :   & ! 
       ! Typdefinition
       t_h_grid,              & ! 
       t_d3d_openbc,          & ! 
       t_d3d_thd,             &
       t_d3d_weir,            &
       ! Parameter und Konstantwerte
       c_max_d3d_dry,         & ! 
       all_errors,            & ! 
       ! Routinen / Interfaces
       setup_text_object, setup_m_object, setup_n_object, setup_xy_object,      &
       setup_enc_object, setup_hv_object, setup_nen_object, setup_ks_object,    &
       setup_dry_object, setup_bnd_object, setup_thd_object, setup_lwl_object,  &
       setup_ext_object, setup_time_object, setup_irand_object,                 &
       get_text_object, get_m_object, get_n_object, get_xy_object,              &
       get_enc_object, get_hv_object, get_dry_object, get_bnd_object,           &
       get_thd_object, get_lwl_object, get_ext_object, get_time_object 
  !
  ! [B.2] Modul mit Methoden zum ermitteln abgeleiteter Groessen
  USE m_h_grid_derive, ONLY : &
       ! Routinen / Interfaces
       derive_nen, derive_irand
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
  !! Lesen einer sequentiellen ASCII-Datei des Typs "delft3d"
  INTERFACE read_h_grid_delft3d_seq_asc
     MODULE PROCEDURE read_h_grid_delft3d_seq_asc_d
  END INTERFACE
  !! Schreiben einer sequentiellen ASCII-Datei des Typs "delft3d" (BAW)
  INTERFACE write_h_grid_delft3d_seq_asc
     MODULE PROCEDURE write_h_grid_delft3d_seq_asc_d
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren (optional, falls sinnvoll)
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: read_h_grid_delft3d_seq_asc  ! SEQUENTIAL, FORMATTED
  PUBLIC :: write_h_grid_delft3d_seq_asc ! SEQUENTIAL, FORMATTED
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Konstantwerte (Parameter)
  !
  !! maximale Anzahl verschiedener (lokaler) Fehlertexte
  INTEGER , PARAMETER :: c_noflocalerrors=11 ! 
  !! lokale Fehlertexte
  CHARACTER (LEN=57) , PARAMETER :: c_localerrors(c_noflocalerrors) = & ! 
       !   123456789 123456789 123456789 123456789 123456789 1234567
       (/ 'GRD: Kommentarzeilen                                     ', &  !  1
          'GRD: Anzahl der Punkte in M- und N-Richtung              ', &  !  2
          'GRD: drei reellwertige Dummy-Zahlen                      ', &  !  3
          'GRD: x- und y-Koordinaten der Tiefenpunkte (depth points)', &  !  4
          'ENC: Umrandungspolygon(e) (Enclosure)                    ', &  !  5
          'DEP: Tiefen der Tiefenpunkte (depth points)              ', &  !  6
          'BND: Randbedingungen an offenen Modellraendern           ', &  !  7
          'DRY: dauerhaft trockene Punkte (Kontrollvolumina)        ', &  !  8
          'THD: nicht ueberstroembare duenne Daemme                 ', &  !  9
          'LWL: ueberstroembare lokale Wehre                        ', &  ! 10
          'EXT: ueberstroembare 2D-Wehre                            ' /)  ! 11
  !
  !! Name des Moduls
  CHARACTER (LEN=16), PARAMETER :: c_modname = 'm_h_grid_delft3d' ! 
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Hilfsfeld f&uuml;r das Lesen einer Datenzeile
  CHARACTER (LEN=240) :: karte        ! 
  !! Hilfsfeld f&uuml;r Fehlertexte
  CHARACTER (LEN=15)  :: ctxt         ! 
  !! log. Kanalnummer f&uuml;r das Lesen der Daten
  INTEGER             :: iunit        ! 
  !! Z&auml;hlervariable
  INTEGER             :: i, j         ! 
  !! Statusvariable (READ/WRITE)
  INTEGER             :: stat, iostat ! 
  !! Zeiger auf Text in Fehlermeldung
  INTEGER             :: isprun       ! 
  !! Dummy-Feld
  REAL (KIND=Double)  :: rdummy(3)=0.0_Double   ! 
  !
  !! Anzahl der Punkte in M- und N-Richtung
  INTEGER               , POINTER  :: p_m, p_n   ! 
  !! Berandung(en) des Gitters
  INTEGER               , POINTER  :: p_enc(:,:) ! 
  !! Knotenverzeichnis der Polygone
  INTEGER               , POINTER  :: p_nen(:,:) ! 
  !! Anzahl der Knoten/Kanten eines Polygons
  INTEGER               , POINTER  :: p_ks(:)    ! 
  !! Koordinaten trockener Punkte/Kontrollvolumina des Gitters
  INTEGER               , POINTER  :: p_dry(:,:) ! 
  !! d&uuml;nne, nicht &uuml;berstr&ouml;mbare D&auml;mme
  TYPE (t_d3d_thd)      , POINTER  :: p_thd(:)   ! 
  !! &uuml;berstr&ouml;mbare lokale Wehre
  TYPE (t_d3d_weir)     , POINTER  :: p_lwl(:)   ! 
  !! &uuml;berstr&ouml;mbare 2D-Wehre
  TYPE (t_d3d_weir)     , POINTER  :: p_ext(:)   ! 
  !! Kennung zur Kennzeichnung offener/geschlossener R&auml;nder
  INTEGER               , POINTER  :: p_irand(:) ! 
  !! Klartext-Beschreibung des Modellgebietes
  CHARACTER (LEN=80)    , POINTER  :: p_text(:)  ! 
  !! Koordinaten der Knoten des Gitters (depth points)
  REAL (KIND=Double)    , POINTER  :: p_xy(:,:)  ! 
  !! Tiefenwerte (depth points)
  REAL (KIND=Double)    , POINTER  :: p_hv(:)    ! 
  !! Randbedingungen an den offenen R&auml;ndern des Modellgebietes
  TYPE (t_d3d_openbc)   , POINTER  :: p_bnd(:)   ! 
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
  !! f&uuml;r die Datei gilt FORM="FORMATTED", ACCESS="SEQUENTIAL"            <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_delft3d_seq_asc_d ( this )
    !
    !! Aktuelles Arbeitsobjekt zur Aufnahme der Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=29), PARAMETER  :: c_upname='read_h_grid_delft3d_seq_asc_d' !
    !! Hilfsvariablen
    TYPE (t_file)                  :: l_file ! 
    !
    CALL init_before_read       ( this )
    !
    ! [1] (req) Lesen der Datei des Typs "*.grd" -------------------------
    CALL read_datablock_1   ( this ) ! Kommentare
    CALL read_datablock_2   ( this ) ! Punkte in M- und N-Richtung
    CALL read_datablock_3   ( this ) ! Dummy-Feld
    CALL read_datablock_4   ( this ) ! x- und y-Koordinaten der Tiefenpunkte
    ! [2] (req) Lesen der Datei des Typs "*.enc" -------------------------
    IF ( isprun == 0 ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.enc' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL read_datablock_5 ( this, l_file )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    ! [3] (req) Lesen der Datei des Typs "*.dep" -------------------------
    IF ( isprun == 0 ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.dep' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL read_datablock_6 ( this, l_file )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    ! [4] (opt) Lesen der Datei des Typs "*.bnd" -------------------------
    IF ( isprun == 0 ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.bnd' )
       IF ( file_exists( l_file ) ) THEN
          IF ( no_error( ) ) CALL open_file  ( l_file )
          IF ( no_error( ) ) CALL read_datablock_7 ( this, l_file )
          IF ( no_error( ) ) CALL close_file ( l_file )
       END IF
    END IF
    ! [5] (opt) Lesen der Datei des Typs "*.dry" -------------------------
    IF ( isprun == 0 ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.dry' )
       IF ( file_exists( l_file ) ) THEN
          IF ( no_error( ) ) CALL open_file  ( l_file )
          IF ( no_error( ) ) CALL read_datablock_8 ( this, l_file )
          IF ( no_error( ) ) CALL close_file ( l_file )
       END IF
    END IF
    ! [6] (opt) Lesen der Datei des Typs "*.thd" -------------------------
    IF ( isprun == 0 ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.thd' )
       IF ( file_exists( l_file ) ) THEN
          IF ( no_error( ) ) CALL open_file  ( l_file )
          IF ( no_error( ) ) CALL read_datablock_9 ( this, l_file )
          IF ( no_error( ) ) CALL close_file ( l_file )
       END IF
    END IF
    ! [7] (opt) Lesen der Datei des Typs "*.lwl" -------------------------
    IF ( isprun == 0 ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.lwl' )
       IF ( file_exists( l_file ) ) THEN
          IF ( no_error( ) ) CALL open_file  ( l_file )
          IF ( no_error( ) ) CALL read_datablock_10 ( this, l_file )
          IF ( no_error( ) ) CALL close_file ( l_file )
       END IF
    END IF
    ! [8] (opt) Lesen der Datei des Typs "*.ext" -------------------------
    IF ( isprun == 0 ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.ext' )
       IF ( file_exists( l_file ) ) THEN
          IF ( no_error( ) ) CALL open_file  ( l_file )
          IF ( no_error( ) ) CALL read_datablock_11 ( this, l_file )
          IF ( no_error( ) ) CALL close_file ( l_file )
       END IF
    END IF
    !
    ! --------------------------------------------------------------------
    CALL setup_error_after_read ( this )
    CALL init_after_read        ( this )
    !
    ! [6] (req) Knotenverzeichnis der Elemente ermitteln -----------------
    IF ( no_error( ) ) THEN
       CALL derive_nen  ( this )
    END IF
    !
    ! [7] (req) aktuelles Systemdatum transferieren ----------------------
    IF ( no_error( ) ) THEN
       CALL setup_time_object ( this, get_datetime_from_system ( ) )
    END IF
    !
  END SUBROUTINE read_h_grid_delft3d_seq_asc_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-WRITE-Methoden <<< [ERR_NO = 24000 bis 24999]
  ! ----------------------------------------------------------------------
  !
  !! &Uuml;betragen der in dem (Arbeits-) Objekt stehenden Daten in eine Datei  <BR>
  !! f&uuml;r die Datei gilt FORM="FORMATTED", ACCESS="SEQUENTIAL"              <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_delft3d_seq_asc_d ( this )
    !! Aktuelles Arbeitsobjekt mit den Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=30), PARAMETER  :: c_upname='write_h_grid_delft3d_seq_asc_d' !
    !! Hilfsvariablen
    TYPE (t_file)                  :: l_file ! 
    !
    CALL init_before_write ( this )
    !
    ! [1] Schreiben der Datei des Typs "*.grd" ---------------------------
    CALL write_datablock_1   ( this ) ! Kommentare
    CALL write_datablock_2   ( this ) ! Punkte in M- und N-Richtung
    CALL write_datablock_3   ( this ) ! Dummy-Feld
    CALL write_datablock_4   ( this ) ! x- und y-Koordinaten der Tiefenpunkte
    ! [2] Schreiben der Datei des Typs "*.enc" ---------------------------
    p_enc   => get_enc_object ( this )
    IF ( isprun == 0 .AND. ASSOCIATED(p_enc) ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.enc' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL write_datablock_5 ( this, l_file, p_enc )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    ! [3] Schreiben der Datei des Typs "*.dep" ---------------------------
    p_hv => get_hv_object ( this )
    p_m  => get_m_object  ( this )
    p_n  => get_n_object  ( this )
    IF ( isprun == 0 .AND. ASSOCIATED(p_hv) .AND. ASSOCIATED(p_m) .AND. ASSOCIATED(p_n) ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.dep' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL write_datablock_6 ( this, l_file, p_m, p_n, p_hv )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    ! [4] Schreiben der Datei des Typs "*.bnd" ---------------------------
    p_bnd => get_bnd_object ( this )
    IF ( isprun == 0 .AND. ASSOCIATED(p_bnd) ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.bnd' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL write_datablock_7 ( this, l_file, p_bnd )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    ! [5] Schreiben der Datei des Typs "*.dry" ---------------------------
    p_dry => get_dry_object ( this )
    IF ( isprun == 0 .AND. ASSOCIATED(p_dry) ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.dry' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL write_datablock_8 ( this, l_file, p_dry )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    ! [6] Schreiben der Datei des Typs "*.thd" ---------------------------
    p_thd => get_thd_object ( this )
    IF ( isprun == 0 .AND. ASSOCIATED(p_thd) ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.thd' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL write_datablock_9 ( this, l_file, p_thd )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    ! [7] Schreiben der Datei des Typs "*.lwl" ---------------------------
    p_lwl => get_lwl_object ( this )
    IF ( isprun == 0 .AND. ASSOCIATED(p_lwl) ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.lwl' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL write_datablock_10 ( this, l_file, p_lwl )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    ! [8] Schreiben der Datei des Typs "*.ext" ---------------------------
    p_ext => get_ext_object ( this )
    IF ( isprun == 0 .AND. ASSOCIATED(p_ext) ) THEN
       CALL create_delft3d_file    ( l_file, this%file, '.grd', '.ext' )
       IF ( no_error( ) ) CALL open_file  ( l_file )
       IF ( no_error( ) ) CALL write_datablock_11 ( this, l_file, p_ext )
       IF ( no_error( ) ) CALL close_file ( l_file )
    END IF
    !
    CALL setup_error_after_write ( this )
    CALL init_after_write        ( this )
    !
  END SUBROUTINE write_h_grid_delft3d_seq_asc_d
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
  ! ----------------------------------------------------------------------
  ! verschiedene allgemein nuetzliche Methoden fuer READ und WRITE
  ! ----------------------------------------------------------------------
  !
  !! Ggf. einen Fehler nach dem Lesen von Daten erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_error_after_read ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=22) , PARAMETER :: c_upname='setup_error_after_read' ! 
    !
    IF ( isprun /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 23400, c_upname, c_modname, iostat )
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       CALL setup_error_act ( '<LokalerFehlerText>', c_localerrors(isprun) )
    END IF
    !
  END SUBROUTINE setup_error_after_read
  !
  !! Ggf. einen Fehler nach dem Schreiben von Daten erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_error_after_write ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=23) , PARAMETER :: c_upname='setup_error_after_write' ! 
    !
    IF ( isprun /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 24400, c_upname, c_modname, iostat )
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       CALL setup_error_act ( '<LokalerFehlerText>', c_localerrors(isprun) )
    END IF
    !
  END SUBROUTINE setup_error_after_write
  !
  !! Initialisieren einiger Variablen vor dem Lesen von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_before_read ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    ctxt      = REPEAT( ' ', LEN(ctxt) )
    iunit     = get_file_unit ( this%file )
    stat      = 0
    iostat    = 0
    isprun    = 0
    i         = 0
    j         = 0
    !
    NULLIFY ( p_text, p_xy, p_m, p_n, p_enc, p_hv, p_nen, p_ks, p_dry, &
         p_bnd, p_thd, p_lwl, p_ext, p_irand )
    !
  END SUBROUTINE init_before_read
  !
  !! Re-Initialisieren einiger Variablen vor nach dem Lesen von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_after_read ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( ASSOCIATED( p_m      ) ) DEALLOCATE( p_m     )
    IF ( ASSOCIATED( p_n      ) ) DEALLOCATE( p_n     )
    IF ( ASSOCIATED( p_text   ) ) DEALLOCATE( p_text  )
    IF ( ASSOCIATED( p_xy     ) ) DEALLOCATE( p_xy    )
    IF ( ASSOCIATED( p_enc    ) ) DEALLOCATE( p_enc   )
    IF ( ASSOCIATED( p_hv     ) ) DEALLOCATE( p_hv    )
    IF ( ASSOCIATED( p_nen    ) ) DEALLOCATE( p_nen   )
    IF ( ASSOCIATED( p_ks     ) ) DEALLOCATE( p_ks    )
    IF ( ASSOCIATED( p_dry    ) ) DEALLOCATE( p_dry   )
    IF ( ASSOCIATED( p_bnd    ) ) DEALLOCATE( p_bnd   )
    IF ( ASSOCIATED( p_thd    ) ) DEALLOCATE( p_thd   )
    IF ( ASSOCIATED( p_lwl    ) ) DEALLOCATE( p_lwl   )
    IF ( ASSOCIATED( p_ext    ) ) DEALLOCATE( p_ext   )
    IF ( ASSOCIATED( p_bnd    ) ) DEALLOCATE( p_irand )
    !
    CALL init_before_read ( this )
    !
  END SUBROUTINE init_after_read
  !
  !! Initialisieren einiger Variablen vor dem Schreiben von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_before_write ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    ctxt      = REPEAT( ' ', LEN(ctxt) )
    iunit     = get_file_unit ( this%file )
    stat      = 0
    iostat    = 0
    isprun    = 0
    i         = 0
    j         = 0
    !
    NULLIFY ( p_text, p_xy, p_m, p_n, p_enc, p_hv, p_nen, p_ks, p_dry, &
         p_bnd, p_thd, p_lwl, p_ext, p_irand )
    !
  END SUBROUTINE init_before_write
  !
  !! Initialisieren einiger Variablen nach dem Schreiben von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_after_write ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    CALL init_before_write ( this )
    !
  END SUBROUTINE init_after_write
  !
  !! Erzeuge einen Delft3D-Dateinamen aus einem vorgegebenen Namen durch
  !! Abwandeln der Endung <BR>
  !! Funktion erzeugt Fehlermeldungen
  SUBROUTINE create_delft3d_file ( file_n, file_o, ext_o, ext_n )
    !! neuer, aus "file_o" abgewandelter Dateibezeichner
    TYPE (t_file)      , INTENT(OUT) :: file_n ! 
    !! alter Dateibezeichner
    TYPE (t_file)      , INTENT(IN)  :: file_o ! 
    !! alte Endung der Datei, die durch "ext_n" ersetzt werden soll
    CHARACTER (LEN=*)  , INTENT(IN)  :: ext_o  ! 
    !! neue Endung der Datei
    CHARACTER (LEN=*)  , INTENT(IN)  :: ext_n  ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=19) , PARAMETER   :: c_upname='create_delft3d_file' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=c_file_path_and_name_len) :: l_text ! 
    INTEGER                                  :: idx    ! 
    !
    file_n = file_o
    l_text = get_file_path_and_name ( file_o )
    idx    = INDEX( l_text, ext_o )
    IF ( idx > 0 ) THEN
       l_text(idx:) = REPEAT( ' ', c_file_path_and_name_len-idx+1 )
       l_text(idx:) = ext_n
       CALL set_file_path_and_name ( file_n, l_text )
       CALL set_file_unit          ( file_n, 0      )
    ELSE
       CALL setup_error_act ( all_errors(:), 26000, c_upname, c_modname )
       IF ( LEN_TRIM(get_file_path(file_o)) > 0 ) THEN
          CALL setup_error_act ( '<pathname>', TRIM(get_file_path(file_o)) )
       END IF
       CALL setup_error_act ( '<filename>', TRIM(get_file_name(file_o)) )
       CALL setup_error_act ( '<extension-old>', ext_o )
       CALL setup_error_act ( '<extension-new>', ext_n )
    END IF
    !
  END SUBROUTINE create_delft3d_file
  !
  ! ----------------------------------------------------------------------
  ! READ-Methoden fuer Datei des Typs "*.grd"
  ! ----------------------------------------------------------------------
  !
  !! Lesen der beschreibenden Datenrecords am Beginn der Datei des Typs "*.grd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_1 ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Hilfsvariablen
    INTEGER :: n, m, nn, mm ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       iostat = 0
       n      = 0
       DO
          IF ( iostat /= 0 ) EXIT
          karte = REPEAT( ' ', LEN(karte) )
          READ (iunit,IOSTAT=iostat,FMT='(A)') karte
          IF ( karte(1:1) == '*' ) n = n + 1
          IF ( karte(1:1) /= '*' ) EXIT
       END DO
       IF ( iostat == 0 ) THEN
          READ (karte,IOSTAT=iostat,FMT=*) mm, nn ! Probelesen
          IF ( iostat /= 0 ) THEN
             iostat = 0 
             n      = n + 1
          END IF
          IF ( n > 0 ) THEN
             REWIND   ( iunit     )
             ALLOCATE ( p_text(n) )
             m = 0
             DO
                IF ( iostat /= 0 .OR. m == n ) EXIT
                karte = REPEAT( ' ', LEN(karte) )
                READ (iunit,IOSTAT=iostat,FMT='(A)') karte
                m         = m + 1
                p_text(m) = karte(1:LEN(p_text(m)))
             END DO
             IF ( iostat == 0 ) THEN
                CALL setup_text_object ( this, p_text )
             ELSE
                isprun = 1
             END IF
          END IF
       ELSE
          isprun = 1
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_1
  !
  !! Lesen der Anzahl der Punkte in M- und N-Richtung aus einer Datei des Typs "*.grd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_2 ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Hilfsvariablen
    INTEGER :: m, n ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,IOSTAT=iostat,FMT=*) m, n
       IF ( iostat == 0 ) THEN
          CALL setup_m_object ( this, m )
          CALL setup_n_object ( this, n )
       ELSE
          isprun = 2
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_2
  !
  !! Lesen dreier reellwertiger, nicht ben&ouml;tigter Dummy-Zahlen aus einer Datei des Typs "*.grd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_3 ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,IOSTAT=iostat,FMT=*) rdummy(:)
       IF ( iostat == 0 ) THEN
          CONTINUE
       ELSE
          isprun = 3
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_3
  !
  !! Lesen der x- und y-Koordinaten der Tiefenpunkte aus einer Datei des Typs "*.grd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_4 ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Hilfsvariablen
    CHARACTER (LEN=4) :: ch        ! 
    INTEGER           :: idummy, n ! 
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_m => get_m_object ( this )
       p_n => get_n_object ( this )
       IF ( ASSOCIATED( p_m ) .AND. ASSOCIATED( p_n ) ) THEN
          IF ( p_m*p_n > 0 ) THEN
             ALLOCATE( p_xy(p_m*p_n,2) )
             ! Lies zuerst die x- und dann die y-Koordinate der Tiefenpunkte
             DO j=1,2
                n = 0
                DO i=1,p_n
                   IF ( iostat /= 0 ) EXIT
                   READ(iunit,IOSTAT=iostat,FMT=*) ch, idummy, p_xy(n+1:n+p_m,j)
                   n = n + p_m 
                END DO
             END DO
             IF ( iostat == 0 ) THEN
                CALL setup_xy_object ( this, p_xy )
             ELSE
                isprun = 4
             END IF
          END IF
       END IF
       NULLIFY ( p_m, p_n )
    END IF
    !
  END SUBROUTINE read_datablock_4
  !
  !! Lesen der Umrandungspolygone aus einer Datei des Typs "*.enc" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_5 ( this, file )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file ! 
    !! Hilfsvariablen
    INTEGER :: enc(2), n, m, l_iunit ! 
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat = 0
       n      = 0
       DO
          IF ( iostat /= 0 ) EXIT
          karte = REPEAT( ' ', LEN(karte) )
          READ (l_iunit,IOSTAT=iostat,FMT='(A)',END=9000) karte
          IF ( iostat /= 0         ) CYCLE
          IF ( karte(1:1) == '*'   ) CYCLE
          IF ( LEN_TRIM(karte) < 1 ) EXIT
          READ (karte,IOSTAT=iostat,FMT=*) enc(:)
          IF ( ANY( enc(:) <= 0 ) ) EXIT
          n = n + 1
       END DO
9000   CONTINUE
       iostat = 0
       IF ( n > 0 ) THEN
          REWIND   ( l_iunit    )
          ALLOCATE ( p_enc(n,2) )
          m = 0
          DO
             IF ( iostat /= 0 .OR. m == n ) EXIT
             READ (l_iunit,IOSTAT=iostat,FMT='(A)') karte
             IF ( iostat /= 0         ) CYCLE
             IF ( karte(1:1) == '*'   ) CYCLE
             IF ( LEN_TRIM(karte) < 1 ) EXIT
             READ (karte,IOSTAT=iostat,FMT=*) enc(:)
             m            = m + 1
             p_enc(m,1:2) = enc(1:2)
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_enc_object ( this, p_enc )
          ELSE
             isprun = 5
          END IF
       ELSE
          isprun = 5
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_5
  !
  !! Lesen der Tiefenwerte der Tiefenpunkte aus einer Datei des Typs "*.dep" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_6 ( this, file )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file ! 
    !! Hilfsvariablen
    INTEGER            :: i, n, l_iunit ! 
    REAL (KIND=Double) :: xx ! 
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit =  get_file_unit ( file )
       p_m     => get_m_object  ( this )
       p_n     => get_n_object  ( this )
       IF ( ASSOCIATED( p_m ) .AND. ASSOCIATED( p_n ) ) THEN
          IF ( p_m*p_n > 0 ) THEN
             ALLOCATE( p_hv(p_m*p_n) )
             n = 0
             ! Hinweis: das Gitter in "*.grd" ist in beiden Richtungen
             !          um 1 kleiner als das eigentliche Berechnungsgitter;
             !          oben/rechts kommt eine weitere Zeile/Spalte hinzu,
             !          deren Tiefenwerte allerdings keine praktische Bedeutung
             !          haben (xx faengt rechts die zusaetzlichen Werte ein)
             DO i=1,p_n
                IF ( iostat /= 0 ) EXIT
                READ(l_iunit,IOSTAT=iostat,FMT=*) p_hv(n+1:n+p_m), xx
                n = n + p_m 
             END DO
             IF ( iostat == 0 ) THEN
                CALL setup_hv_object ( this, p_hv )
             ELSE
                isprun = 6
             END IF
          END IF
       END IF
       NULLIFY ( p_m, p_n )
    END IF
    !
  END SUBROUTINE read_datablock_6
  !
  !! Lesen der Randbedingungen an offenen R&auml;ndern aus einer Datei des Typs "*.bnd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_7 ( this, file )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file ! 
    !! Hilfsvariablen
    INTEGER             :: n, m, l_iunit ! 
    TYPE (t_d3d_openbc) :: l_bnd         !
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat  = 0
       n       = 0
       DO
          IF ( iostat /= 0 ) EXIT
          karte = REPEAT( ' ', LEN(karte) )
          READ (l_iunit,IOSTAT=iostat,FMT='(A)',END=9000) karte
          IF ( iostat /= 0         ) CYCLE
          IF ( karte(1:1) == '*'   ) CYCLE
          IF ( LEN_TRIM(karte) < 1 ) EXIT
          READ (karte,IOSTAT=iostat,FMT=8000) l_bnd%name,       & ! 
               l_bnd%bdry_type, l_bnd%data_type, l_bnd%grid_coor(:),       & ! 
               l_bnd%refl_coef, l_bnd%prof, l_bnd%bloc_ampl, l_bnd%bloc_phas !   
          IF ( LEN_TRIM(l_bnd%name) == 0 ) EXIT
          n = n + 1
       END DO
9000   CONTINUE
       iostat = 0
       IF ( n > 0 ) THEN
          REWIND   ( l_iunit  )
          ALLOCATE ( p_bnd(n) )
          m = 0
          DO
             IF ( iostat /= 0 .OR. m == n ) EXIT
             l_bnd%bloc_ampl = REPEAT( ' ', LEN(l_bnd%bloc_ampl) )
             l_bnd%bloc_phas = REPEAT( ' ', LEN(l_bnd%bloc_phas) )
             READ (l_iunit,IOSTAT=iostat,FMT='(A)') karte
             IF ( iostat /= 0         ) CYCLE
             IF ( karte(1:1) == '*'   ) CYCLE
             IF ( LEN_TRIM(karte) < 1 ) EXIT
             READ (karte,IOSTAT=iostat,FMT=8000) l_bnd%name,                & ! 
                  l_bnd%bdry_type, l_bnd%data_type, l_bnd%grid_coor(:),       & ! 
                  l_bnd%refl_coef, l_bnd%prof, l_bnd%bloc_ampl, l_bnd%bloc_phas ! 
             m        = m + 1
             p_bnd(m) = l_bnd
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_bnd_object ( this, p_bnd )
          ELSE
             isprun = 7
          END IF
       ELSE
          isprun = 7
       END IF
    END IF
    !
    8000 FORMAT(A20,1X,A1,1X,A1,4I6,F11.0,1X,A20,1X,A12,1X,A12)
    !
  END SUBROUTINE read_datablock_7
  !
  !! Lesen der Koordinaten der dauerhaft trockenen Berechnungspunkte aus einer Datei des Typs "*.dry" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_8 ( this, file )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file ! 
    !! Hilfsvariablen
    INTEGER :: n, m, l_iunit        ! 
    INTEGER :: l_dry(c_max_d3d_dry) !      
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit =  get_file_unit ( file )
       iostat  = 0
       n       = 0
       DO
          IF ( iostat /= 0 ) EXIT
          karte = REPEAT( ' ', LEN(karte) )
          READ (l_iunit,IOSTAT=iostat,FMT='(A)',END=9000) karte
          IF ( iostat /= 0         ) CYCLE
          IF ( karte(1:1) == '*'   ) CYCLE
          IF ( LEN_TRIM(karte) < 1 ) EXIT
          READ (karte,IOSTAT=iostat,FMT=*) l_dry(:)
          IF ( ANY( l_dry(:) <= 0 ) ) EXIT
          n = n + 1
       END DO
9000   CONTINUE
       iostat = 0
       IF ( n > 0 ) THEN
          REWIND   ( l_iunit    )
          ALLOCATE ( p_dry(n,c_max_d3d_dry) )
          m = 0
          DO
             IF ( iostat /= 0 .OR. m == n ) EXIT
             karte = REPEAT( ' ', LEN(karte) )
             READ (l_iunit,IOSTAT=iostat,FMT='(A)') karte
             IF ( iostat /= 0         ) CYCLE
             IF ( karte(1:1) == '*'   ) CYCLE
             IF ( LEN_TRIM(karte) < 1 ) EXIT
             READ (karte,IOSTAT=iostat,FMT=*) l_dry(:)
             m          = m + 1
             p_dry(m,:) = l_dry(:)
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_dry_object ( this, p_dry )
          ELSE
             isprun = 8
          END IF
       ELSE
          isprun = 8
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_8
  !
  !! Lesen der Koordinaten der nicht &uuml;berstr&ouml;mbaren d&uuml;nnen 
  !! D&auml;mme aus einer Datei des Typs "*.thd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_9 ( this, file )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file ! 
    !! Hilfsvariablen
    INTEGER          :: n, m, l_iunit ! 
    TYPE (t_d3d_thd) :: l_thd         !      
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit =  get_file_unit ( file )
       iostat  = 0
       n       = 0
       DO
          IF ( iostat /= 0 ) EXIT
          karte = REPEAT( ' ', LEN(karte) )
          READ (l_iunit,IOSTAT=iostat,FMT='(A)',END=9000) karte
          IF ( iostat /= 0         ) CYCLE
          IF ( karte(1:1) == '*'   ) CYCLE
          IF ( LEN_TRIM(karte) < 1 ) EXIT
          READ (karte,IOSTAT=iostat,FMT=*) l_thd%grid_coor, l_thd%type
          IF ( ANY( l_thd%grid_coor <= 0 ) ) EXIT
          n = n + 1
       END DO
9000   CONTINUE
       iostat = 0
       IF ( n > 0 ) THEN
          REWIND   ( l_iunit    )
          ALLOCATE ( p_thd(n) )
          m = 0
          DO
             IF ( iostat /= 0 .OR. m == n ) EXIT
             karte = REPEAT( ' ', LEN(karte) )
             READ (l_iunit,IOSTAT=iostat,FMT='(A)') karte
             IF ( iostat /= 0         ) CYCLE
             IF ( karte(1:1) == '*'   ) CYCLE
             IF ( LEN_TRIM(karte) < 1 ) EXIT
             READ (karte,IOSTAT=iostat,FMT=*) l_thd%grid_coor, l_thd%type
             m        = m + 1
             p_thd(m) = l_thd
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_thd_object ( this, p_thd )
          ELSE
             isprun = 9
          END IF
       ELSE
          isprun = 9
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_9
  !
  !! Lesen der Koordinaten der &uuml;berstr&ouml;mbaren lokalen Wehre
  !! aus einer Datei des Typs "*.lwl" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_10 ( this, file )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file ! 
    !! Hilfsvariablen
    INTEGER           :: n, m, l_iunit ! 
    TYPE (t_d3d_weir) :: l_lwl         !      
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit =  get_file_unit ( file )
       iostat  = 0
       n       = 0
       DO
          IF ( iostat /= 0 ) EXIT
          karte = REPEAT( ' ', LEN(karte) )
          READ (l_iunit,IOSTAT=iostat,FMT='(A)',END=9000) karte
          IF ( iostat /= 0         ) CYCLE
          IF ( karte(1:1) == '*'   ) CYCLE
          IF ( LEN_TRIM(karte) < 1 ) EXIT
          READ (karte,IOSTAT=iostat,FMT=*) l_lwl%type, l_lwl%grid_coor, &
               l_lwl%frction, l_lwl%sill_depth
          IF ( ANY( l_lwl%grid_coor <= 0 ) ) EXIT
          n = n + 1
       END DO
9000   CONTINUE
       iostat = 0
       IF ( n > 0 ) THEN
          REWIND   ( l_iunit    )
          ALLOCATE ( p_lwl(n) )
          m = 0
          DO
             IF ( iostat /= 0 .OR. m == n ) EXIT
             karte = REPEAT( ' ', LEN(karte) )
             READ (l_iunit,IOSTAT=iostat,FMT='(A)') karte
             IF ( iostat /= 0         ) CYCLE
             IF ( karte(1:1) == '*'   ) CYCLE
             IF ( LEN_TRIM(karte) < 1 ) EXIT
             READ (karte,IOSTAT=iostat,FMT=*) l_lwl%type, l_lwl%grid_coor, &
                  l_lwl%frction, l_lwl%sill_depth
             m        = m + 1
             p_lwl(m) = l_lwl
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_lwl_object ( this, p_lwl )
          ELSE
             isprun = 10
          END IF
       ELSE
          isprun = 10
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_10
  !
  !! Lesen der Koordinaten der &uuml;berstr&ouml;mbaren 2D-Wehre
  !! aus einer Datei des Typs "*.ext" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE read_datablock_11 ( this, file )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file ! 
    !! Hilfsvariablen
    INTEGER           :: n, m, l_iunit ! 
    TYPE (t_d3d_weir) :: l_ext         !      
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit =  get_file_unit ( file )
       iostat  = 0
       n       = 0
       DO
          IF ( iostat /= 0 ) EXIT
          karte = REPEAT( ' ', LEN(karte) )
          READ (l_iunit,IOSTAT=iostat,FMT='(A)',END=9000) karte
          IF ( iostat /= 0         ) CYCLE
          IF ( karte(1:1) == '*'   ) CYCLE
          IF ( LEN_TRIM(karte) < 1 ) EXIT
          READ (karte,IOSTAT=iostat,FMT=*) l_ext%type, l_ext%grid_coor, &
               l_ext%frction, l_ext%sill_depth
          IF ( ANY( l_ext%grid_coor <= 0 ) ) EXIT
          n = n + 1
       END DO
9000   CONTINUE
       iostat = 0
       IF ( n > 0 ) THEN
          REWIND   ( l_iunit    )
          ALLOCATE ( p_ext(n) )
          m = 0
          DO
             IF ( iostat /= 0 .OR. m == n ) EXIT
             karte = REPEAT( ' ', LEN(karte) )
             READ (l_iunit,IOSTAT=iostat,FMT='(A)') karte
             IF ( iostat /= 0         ) CYCLE
             IF ( karte(1:1) == '*'   ) CYCLE
             IF ( LEN_TRIM(karte) < 1 ) EXIT
             READ (karte,IOSTAT=iostat,FMT=*) l_ext%type, l_ext%grid_coor, &
                  l_ext%frction, l_ext%sill_depth
             m        = m + 1
             p_ext(m) = l_ext
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_ext_object ( this, p_ext )
          ELSE
             isprun = 11
          END IF
       ELSE
          isprun = 11
       END IF
    END IF
    !
  END SUBROUTINE read_datablock_11
  !
  ! ----------------------------------------------------------------------
  ! WRITE-Methoden fuer Datei des Typs "*.grd"
  ! ----------------------------------------------------------------------
  !
  !! Schreiben der beschreibenden Datenrecords am Beginn der Datei des Typs "*.grd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_1 ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Hilfsvariable
    INTEGER :: i  ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_text => get_text_object ( this )
       IF ( ASSOCIATED( p_text ) ) THEN
          iostat = 0
          DO i=1,SIZE(p_text)
             IF ( iostat /= 0 ) EXIT
             karte      = REPEAT( ' ', LEN(karte) )
             karte      = p_text(i)
             karte(1:1) = '*'
             WRITE (iunit,IOSTAT=iostat,FMT='(A)') TRIM(karte)
          END DO
          IF ( IOSTAT /= 0 ) isprun = 1
       END IF
    END IF
    !
  END SUBROUTINE write_datablock_1
  !
  !! Schreiben der Anzahl der Punkte in M- und N-Richtung in eine Datei des Typs "*.grd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_2 ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_m => get_m_object ( this )
       p_n => get_n_object ( this )
       IF ( ASSOCIATED(p_m) .AND. ASSOCIATED(p_n) ) THEN
          WRITE (iunit,IOSTAT=iostat,FMT='(2I8)') p_m, p_n
          IF ( iostat /= 0 ) isprun = 2
       END IF
    END IF
    !
  END SUBROUTINE write_datablock_2
  !
  !! Schreiben dreier reellwertiger, nicht ben&ouml;tigter Dummy-Zahlen in eine Datei des Typs "*.grd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_3 ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       WRITE (iunit,IOSTAT=iostat,FMT='(3F8.0)') rdummy(:)
       IF ( iostat /= 0 ) isprun = 3
    END IF
    !
  END SUBROUTINE write_datablock_3
  !
  !! Schreiben der x- und y-Koordinaten der Tiefenpunkte in eine Datei des Typs "*.grd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_4 ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Hilfsvariable
    INTEGER , PARAMETER :: nnl=5      ! 
    INTEGER             :: l, n, nl, ia, ie ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_m  => get_m_object ( this )
       p_n  => get_n_object ( this )
       p_xy => get_xy_object ( this )
       IF ( ASSOCIATED( p_m ) .AND. ASSOCIATED( p_n ) .AND. ASSOCIATED( p_xy ) ) THEN
          ! Schreibe zuerst die x- und dann die y-Koordinate der Tiefenpunkte
          nl = p_m / nnl
          IF ( nl*nnl /= p_m ) nl = nl + 1
          DO j=1,2
             IF ( iostat /= 0 ) EXIT
             n = 0
             DO i=1,p_n
                IF ( iostat /= 0 ) EXIT
                ia = 1 ; ie = 0
                DO l=1,nl
                   IF ( iostat /= 0 ) EXIT
                   ie = MIN( ie + nnl, p_m )
                   SELECT CASE ( l )
                   CASE ( 1 )
                      WRITE (iunit,IOSTAT=iostat,FMT='(A5,I5,5F12.3)') ' ETA=', i, p_xy(n+ia:n+ie,j)
                   CASE DEFAULT 
                      WRITE (iunit,IOSTAT=iostat,FMT='(10X,5F12.3)'   )            p_xy(n+ia:n+ie,j)
                   END SELECT
                   ia = ie + 1
                END DO
                n = n + p_m
             END DO
          END DO
          IF ( iostat /= 0 ) isprun = 4
       END IF
    END IF
    !
  END SUBROUTINE write_datablock_4
  !
  !! Schreiben der Umrandungspolygone in eine Datei des Typs "*.enc" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_5 ( this, file, enc )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this     ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file     ! 
    !! Enclosure
    INTEGER         , INTENT(IN) :: enc(:,:) ! 
    !! Hilfsvariablen
    INTEGER :: l_iunit ! 
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat = 0
       DO i=1,SIZE(enc,1)
          IF ( iostat /= 0 ) EXIT
          WRITE (l_iunit,IOSTAT=iostat,FMT='(2I10)') enc(i,1:2) ! 
       END DO
       IF ( iostat /= 0 ) isprun = 5
    END IF
    !
  END SUBROUTINE write_datablock_5
  !
  !! Schreiben der Tiefenwerte der Tiefenpunkte in eine Datei des Typs "*.dep" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_6 ( this, file, m, n, hv )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid)    , POINTER    :: this  ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)      , INTENT(IN) :: file  ! 
    !! Anzahl der Tiefenpunkte in M-Richtung
    INTEGER            , INTENT(IN) :: m     ! 
    !! Anzahl der Tiefenpunkte in N-Richtung
    INTEGER            , INTENT(IN) :: n     ! 
    !! Tiefenwerte
    REAL (KIND=Double) , INTENT(IN) :: hv(:) ! 
    !! Hilfsvariablen
    REAL (KIND=Double) , PARAMETER   :: c_missing=-999.000_Double ! 
    REAL (KIND=Double) , ALLOCATABLE :: missing(:)  ! 
    INTEGER             :: l_iunit, nn, mm, ia, ie ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat  = 0
       mm      = m + 1
       nn      = n + 1
       ALLOCATE(missing(mm))
       missing(:) = c_missing
       ia = 1
       ie = 0
       DO j=1,nn
          IF ( iostat /= 0 ) EXIT
          ie = ie + m
          IF ( j < nn ) THEN
             WRITE (l_iunit,IOSTAT=iostat,FMT='(12F12.3)') hv(ia:ie),c_missing
          ELSE
             WRITE (l_iunit,IOSTAT=iostat,FMT='(12F12.3)') missing(:)
          END IF
          ia = ie + 1
       END DO
       IF ( iostat /= 0 ) isprun = 6
       DEALLOCATE(missing)
    END IF
    !
  END SUBROUTINE write_datablock_6
  !
  !! Schreiben der Randbedingungen f&uuml;r offener R&auml;nder in eine Datei des Typs "*.bnd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_7 ( this, file, bnd )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid)    , POINTER    :: this  ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)      , INTENT(IN) :: file  ! 
    !! Randbedingungen f&uuml;r offene R&auml;nder
    TYPE (t_d3d_openbc), INTENT(IN) :: bnd(:)! 
    !! Hilfsvariablen
    INTEGER             :: l_iunit ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat  = 0
       DO i=1,SIZE(bnd)
          IF ( iostat /= 0 ) EXIT
          WRITE (l_iunit,IOSTAT=iostat,FMT=8000) &
               bnd(i)%name,                      &
               bnd(i)%bdry_type, bnd(i)%data_type, bnd(i)%grid_coor(:), & ! 
               bnd(i)%refl_coef, bnd(i)%prof, bnd(i)%bloc_ampl, bnd(i)%bloc_phas !   
       END DO
       IF ( iostat /= 0 ) isprun = 7
    END IF
    !
    8000 FORMAT(A20,1X,A1,1X,A1,4I6,F11.0,1X,A20,1X,A12,1X,A12)
    !
  END SUBROUTINE write_datablock_7
  !
  !! Schreiben der Trockenpunkte in eine Datei des Typs "*.dry" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_8 ( this, file, dry )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER    :: this     ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)   , INTENT(IN) :: file     ! 
    !! Trockenpunkte
    INTEGER         , INTENT(IN) :: dry(:,:) ! 
    !! Hilfsvariablen
    INTEGER :: l_iunit ! 
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat = 0
       DO i=1,SIZE(dry,1)
          IF ( iostat /= 0 ) EXIT
          WRITE (l_iunit,IOSTAT=iostat,FMT='(4I10)') dry(i,1:4) ! 
       END DO
       IF ( iostat /= 0 ) isprun = 8
    END IF
    !
  END SUBROUTINE write_datablock_8
  !
  !! Schreiben der nicht &uuml;berstr&ouml;mbaren d&uuml;nnen D&auml;mme in eine Datei des Typs "*.thd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_9 ( this, file, thd )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid)  , POINTER    :: this   ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)    , INTENT(IN) :: file   ! 
    !! nicht &uuml;berstr&ouml;mbare d&uuml;nne D&auml;mme
    TYPE (t_d3d_thd) , INTENT(IN) :: thd(:) ! 
    !! Hilfsvariablen
    INTEGER :: l_iunit ! 
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat = 0
       DO i=1,SIZE(thd)
          IF ( iostat /= 0 ) EXIT
          WRITE (l_iunit,IOSTAT=iostat,FMT='(4I10,4X,A)') thd(i)%grid_coor, thd(i)%type ! 
       END DO
       IF ( iostat /= 0 ) isprun = 9
    END IF
    !
  END SUBROUTINE write_datablock_9
  !
  !! Schreiben der lokalen Wehre in eine Datei des Typs "*.lwl" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_10 ( this, file, lwl )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid)   , POINTER    :: this   ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)     , INTENT(IN) :: file   ! 
    !! &uuml;berstr&ouml;mbare lokale Wehre
    TYPE (t_d3d_weir) , INTENT(IN) :: lwl(:) ! 
    !! lokale Parameter
    REAL (KIND=Double) , PARAMETER :: dd=0.0_Double ! 
    !! Hilfsvariablen
    INTEGER :: l_iunit ! 
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat = 0
       DO i=1,SIZE(lwl)
          IF ( iostat /= 0 ) EXIT
          WRITE (l_iunit,IOSTAT=iostat,FMT='(A1,4X,4I10,3F10.5)') lwl(i)%type, &
               lwl(i)%grid_coor, lwl(i)%frction, lwl(i)%sill_depth, dd ! 
       END DO
       IF ( iostat /= 0 ) isprun = 10
    END IF
    !
  END SUBROUTINE write_datablock_10
  !
  !! Schreiben der 2D-Wehre in eine Datei des Typs "*.ext" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen 
  SUBROUTINE write_datablock_11 ( this, file, ext )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid)   , POINTER    :: this   ! 
    !! Dateibezeichner f&uuml;r die zu lesenden Daten
    TYPE (t_file)     , INTENT(IN) :: file   ! 
    !! &uuml;berstr&ouml;mbare lokale Wehre
    TYPE (t_d3d_weir) , INTENT(IN) :: ext(:) ! 
    !! lokale Parameter
    REAL (KIND=Double) , PARAMETER :: dd=0.0_Double ! 
    !! Hilfsvariablen
    INTEGER :: l_iunit ! 
    ! 
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       l_iunit = get_file_unit ( file )
       iostat = 0
       DO i=1,SIZE(ext)
          IF ( iostat /= 0 ) EXIT
          WRITE (l_iunit,IOSTAT=iostat,FMT='(A1,4X,4I10,3F10.5)') ext(i)%type, &
               ext(i)%grid_coor, ext(i)%frction, ext(i)%sill_depth, dd ! 
       END DO
       IF ( iostat /= 0 ) isprun = 11
    END IF
    !
  END SUBROUTINE write_datablock_11
  !
END MODULE m_h_grid_delft3d
! TailOfPackageUserInterface -----------------------------------------------
