! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>read ipds data file</h2>
!! @author J. J&uuml;rges
!! @version 3.2 vom 19.03 07, Quellcode: mod_m_ipds_readfile.f90
!! <HR>
!! read ipds data from file                                          <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-08-09 : J. Juerges  : Original
!  01.02 : 2002-09-16 : J. Juerges  : Beruecksichtigung von c_rl_depth_type und c_rl_rough_type
!  01.03 : 2002-10-16 : J. Juerges  : Die Kanalnummern des dictionary-Pakets werden nicht mehr explizit gesetzt
!  01.04 : 2003-01-10 : J. Juerges  : Entwicklungsgeschichte nicht fuer f90doc
!  01.05 : 2003-01-10 : J. Juerges  : Lizenzangabe durch Platzhalter ersetzt
!  01.06 : 2003-01-10 : J. Juerges  : keine Beschreibungen der Interfaces dort, wo sie PUBLIC gemacht werden
!  01.07 : 2003-02-10 : J. Juerges  : Fehlerdeklaration fuer Mehrprozessor-Betrieb fit gemacht
!  01.08 : 2003-09-11 : J. Juerges  : + Terminalausgabe "Datei ... wird eingelesen"
!  01.09 : 2003-09-22 : J. Juerges  : + Einlesen der neuen opt. Keyzeile "sampling_point_maxdist"
!  01.10 : 2004-01-29 : H. Weilbeer : Bei selbstdefinierten Datentypen INTENT(OUT) -> INTENT(INOUT) gewandelt
!  01.11 : 2004-04-14 : J. Juerges  : Default-Werte fuer .._validity_level veraendert (von +-99 nach +-9999)
!  01.12 : 2004-07-09 : G. Lang     : "suspendedload" beruecksichtigen 
!  01.13 : 2004-07-12 : G. Lang     : "z0_roughnesslength" beruecksichtigen 
!  01.14 : 2005-04-12 : J. Juerges  : "Porenwasser" in "Porositaet" umbenannt
!  02.01 : 2005-08-10 : G. Lang     : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-08-22 : J. Juerges  : Das neue Modul "m_ipds_phydef" beruecksichtigt
!  02.03 : 2005-08-26 : J. Juerges  : Normierung phys. Anteilsgroessen integriert
!  02.04 : 2007-01-10 : P. Schade   : get_phy_in_block: + Groessen Nr. 14 bis 22 fuer Delft3D
!  03.01 : 2007-03-13 : G. Lang     : Lesen neuer physikalischer Groessen (u_x, u_y, depo_rate)
!  03.02 : 2007-03-19 : G. Lang     : Lesen verschiedener Seegangsparameter
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! <OL>
!!   <LI> Steuert das Lesen von "ipds.dat" unter
!!        Zuhilfenahme des Paketes "p_dictionary_ui";
!!   <LI> F&uuml;hrt verschiedene Tests auf den gelesenen Daten aus;
!!   <LI> Legt die Steuerdaten in "m_ipds_data" ab.
!! </OL>
!! <HR>
!!                                                                  <BR>
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>&Ouml;ffentliche Methoden</H3>
!!                                                                  <BR>
!! <H4>Basis-Methoden</H4>
!!                                                                  <BR>
!! <UL>
!!    <LI> <EM>READ_ipds_data</EM>
!!    <OL>
!!       <LI> Lesen der Daten aus "ipds.dat" unter
!!            Zuhilfenahme des Paketes "p_dictionary_ui";
!!    </OL>
!! </UL>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen dieses Moduls werden von "io_ipds_ui" aus
!! in Anspruch genommen. Ein Verwenden der Methoden dieses Moduls
!! von anderen Paketen aus ist nicht zul&auml;ssig. F&uuml;r die
!! korrekte Verwendung der in diesem Modul befindlichen Methoden 
!! ist eine korrekte Initialisierung von "io_ipds_ui"
!! erforderlich.
!!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! siehe hierzu die Fehlermeldungen in User Interface "io_ipds_ui" 
!!
MODULE m_ipds_readfile
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       single,            &
       double
  !
  ! [A.2] Basis-Modul "Fehler"
  !
  USE b_error, ONLY :   &
       ! Routinen     
       no_error,        &
       any_error,       &
       setup_error_act
  !
  ! [A.3] Basis-Modul "Datei(en)"
  !
  USE b_file, ONLY :    &
       ! Routinen
       ok_file,         &
       open_file,       &
       set_file_action, &
       set_file_status, &
       get_file_name
  !
  ! [A.4] Basis-Modul "2D-Punkt"
  !
  USE b_point_2d, ONLY : &
       ! Typdefinition
       t_point_2d,       &
       ! Routinen
       new_point_2d,     &
       kill_point_2d,    &
       set_point_2d_x,   &
       set_point_2d_y
  !
  ! ----------------------------------------------------------------------
  ! [B] gemeinsam genutzte Daten des Paketes "io_ipds"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "io_ipds"
  !
  USE m_ipds_phydef, ONLY : &
       !   Variablen
       c_phy_idx,           &
       c_phy_keyname,       &
       c_phy_name_de
  !
  USE m_ipds_data, ONLY :      &
       !   Typdefinition
       t_ipds,                 &
       !   Variablen
       prn_lun,                & ! log. Fortran-Unit fuer Druckerausgabe
       trc_lun,                & ! log. Fortran-Unit fuer Traceausgabe
       all_errors,             & ! Fehlermeldungen
       c_nof_rl_depth_type,    &
       c_rl_depth_type,        &
       c_rl_depth_nr,          &
       c_nof_rl_rough_type,    &
       c_rl_rough_type,        &
       c_rl_rough_nr,          &
       !   Routinen
       setup_name_object,      &
       setup_datetime_object,  &
       setup_physet_object,    &
       setup_region_object,    &
       setup_mespos_object,    &
       setup_regphyset_object, &
       get_uppercase_char
  !
  ! [B.2] Daten einer Messposition
  !
  USE m_ipds_mespos, ONLY : &
       !   Typdefinition
       t_mespos,            &
       !   Daten
       c_len_mespos_name,   &
       !   Routinen
       new_mespos,          &
       kill_mespos,         &
       set_mespos_name,     &
       set_mespos_coor,     &
       set_mespos_physet
  !
  ! [B.3] Ein Satz physikalischer Groessen
  !
  USE m_ipds_physet, ONLY : &
       !   Typdefinition
       t_physet,            &
       !   Routinen
       new_physet,          &
       kill_physet,         &
       set_physet_set
  !
  ! [B.4] Daten einer physikalischen Groesse
  !
  USE m_ipds_phyval, ONLY : &
       !   Typdefinition
       t_phyval,            &
       !   Routinen
       new_phyval,          &
       set_phyval_name,     &
       set_phyval_var_name, &
       set_phyval_type,     &
       set_phyval_val,      &
       is_phyval_frac,      &
       normalize_phyval,    &
       kill_phyval
  !
  ! [B.5] Daten einer Region
  !
  USE m_ipds_region, ONLY : &
       !   Typdefinition
       t_region,            &
       !   Routinen
       new_region,          &
       kill_region,         &
       set_region_name,     &
       set_region_border
  !
  ! [B.6] Physikalische Groessen einer Region
  !
  USE m_ipds_regphyset, ONLY :       &
       !   Typdefinition
       t_regphyset,                  &
       !   Routinen
       new_regphyset,                &
       kill_regphyset,               &
       set_regphyset_region_name,    &
       set_regphyset_region_inside,  &
       set_regphyset_region_zmin,    &
       set_regphyset_region_zmax,    &
       set_regphyset_mespos_name,    &
       set_regphyset_mespos_maxdist, &
       set_regphyset_interpol_name
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
  !
  !       Hinweis: dieses Modul definiert keinen oeffentlich 
  !                zugaenglichen Datentyp.
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
  !! Lesen der Steuerdaten von Datei
  INTERFACE read_ipds_0
     MODULE PROCEDURE read_ipds_0_d
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  ! [C.5] Zuweisungen
  !
  ! [C.6] Operatoren (optional, falls sinnvoll)
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: read_ipds_0
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
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
  CHARACTER (LEN=15), PARAMETER :: c_modname = 'm_ipds_readfile'
  !! Name der Dictionary-Datei
  !LEO renamed file CHARACTER (LEN=13), PARAMETER :: c_diconame = 'ipds_dico.dat'
  CHARACTER (LEN=6), PARAMETER :: c_diconame = 'DSIDIC'
  !
  !! Definition der implementierten ein-eindeutigen Datei-Varianten <BR>
  !! Anzahl der implementierten Varianten f&uuml;r i/o
  INTEGER           , PARAMETER :: c_max_variants = 1
  !! Bezeichnung der Datei-TYPE-Varianten (nur GROSSbuchstaben verwenden)
  CHARACTER (LEN=04), PARAMETER :: c_variants_type(c_max_variants) = &
       (/ 'IPDS' /)
  !! Bezeichnung der Datei-CODE-Varianten (gemaess dirdef1)
  INTEGER, PARAMETER :: c_variants_code(c_max_variants) = & ! 
       (/ 130  /)
  !! Bezeichnung der implementieren Fortran ACCESS-Varianten
  CHARACTER (LEN=10), PARAMETER :: c_variants_access(c_max_variants) = &
       (/ 'SEQUENTIAL' /)
  !! Bezeichnung der implementieren Fortran FORM-Varianten
  CHARACTER (LEN=11), PARAMETER :: c_variants_form(c_max_variants) = &
       (/ 'FORMATTED  ' /)
  !! Bezeichnung der implementieren Fortran DELIM-Varianten
  CHARACTER (LEN=10), PARAMETER :: c_variants_delim(c_max_variants) = &
       (/ 'NONE      ' /)
  !
  !! In einer Datei zul&auml;ssige Blocknamen
  CHARACTER (LEN=22), PARAMETER :: blockname(5) = (/ &
       'date_and_time         ', &
       'global_constant_values', &
       'region                ', &
       'sampling_point        ', &
       'regional_values       ' /)
  !! Zul&auml;ssige Schl&uuml;sselw&ouml;rter in Block "date_and_time"
  CHARACTER (LEN= 8), PARAMETER :: keyname_blo1( 1) = (/ &
       'datetime' /)
  !! Zul&auml;ssige Schl&uuml;sselw&ouml;rter in Block "region"
  CHARACTER (LEN=12), PARAMETER :: keyname_blo3( 2) = (/ &
       'region_name ', &
       'border_point' /)
  !! Zul&auml;ssige Schl&uuml;sselw&ouml;rter in Block "sampling_point"
  !! (Hier duerfen keine phys. Groessen stehen, statt dessen werden diese in
  !! c_phy_keyname definiert)
  CHARACTER (LEN=19), PARAMETER :: keyname_blo4( 2) = (/ &
       'sampling_point_name', &
       'sampling_point_xy  ' /)
  !! Zul&auml;ssige Schl&uuml;sselw&ouml;rter in Block "regional_values"
  CHARACTER (LEN=22), PARAMETER :: keyname_blo5( 6) = (/ &
       'region                ', &
       'lower_validity_level  ', &
       'upper_validity_level  ', &
       'sampling_point        ', &
       'sampling_point_maxdist', &
       'interpolation_method  ' /)
  !
  ! [D.3] Variablen (statische Daten des Moduls)
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
  ! Oeffentliche Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-READ-Methoden <<< [ERR_NO = 23000 bis 23999]
  ! ----------------------------------------------------------------------
  !
  !! &Uuml;betragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_ipds_0_d ( this )
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this ! 
    !! Name der Routine
    CHARACTER (LEN=13), PARAMETER :: c_upname='read_ipds_0_d' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: string ! 
    !
    IF ( no_error( ) ) CALL set_file_action ( this%file, 'READ' )
    IF ( no_error( ) ) CALL set_file_status ( this%file, 'OLD' )
    !
    IF ( ok_file ( this%file ) ) THEN
       IF ( ok_ipds_variant_no ( this%file ) ) THEN
          !
          ! Datei oeffnen
          !
          CALL open_file ( this%file )
          !
          IF ( no_error ( ) ) THEN
             SELECT CASE ( get_ipds_variant_no ( this%file ) )
             CASE ( 1 )
                CALL read_ipds_0_v1_d ( this )
             CASE DEFAULT
                !$OMP critical
                CALL setup_error_act ( all_errors(:), 23001, c_upname, c_modname )
                WRITE ( string, '(I10)') get_ipds_variant_no ( this%file )
                CALL setup_error_act ( '<DateiVarianteNo>' , string )
                WRITE ( string, '(I10)') c_max_variants
                CALL setup_error_act ( '<MaxDateiVarianteNo>' , string )
                !$OMP end critical
             END SELECT ! Hinweis: Anzahl der CASEs muss mit "c_max_variants" uebereinstimmen
          END IF
          !
          ! Datei nicht mehr schliessen, das wird vom dictionary-Paket uebernommen
          !
          ! Den Namen der Datei als Objekt-Namen einspeichern
          IF ( no_error ( ) ) CALL setup_name_object ( this, get_file_name( this%file ) )
          !
       END IF
    END IF
    !
  END SUBROUTINE read_ipds_0_d
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
  ! -----------------------------------------------------------------------------
  ! >>> modulspezifische PRIVATE-READ-Methoden <<< [ERR_NO = -23000 bis -23999]
  ! -----------------------------------------------------------------------------
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Datei-Variante 1)
  SUBROUTINE read_ipds_0_v1_d ( this )
    !
    ! USE-Anweisungen
    USE m_dredgesim_data, ONLY : DEBUG_ds
    USE p_dictionary_ui, ONLY : &
         !   Routinen / Interfaces
         init_dictionary,       &
         clear_dictionary,      &
         read_input_file,       &
         get_nof_input_blocks,  &
         get_nof_input_lines,   &
         get_input_data
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_ipds) , POINTER :: this
    !
    ! lokale Parameter / Variablen
    !! Name der Routine
    CHARACTER (LEN=16) , PARAMETER  :: c_upname='read_ipds_0_v1_d' ! 
    !! aktuelle Anzahl gleichartiger Bl&ouml;cke
    INTEGER                         :: n_block(SIZE(blockname))
    !! Zaehler fuer gleichartige Bloecke
    INTEGER                         :: i_block
    !! aktuelle Anzahl gleichartiger Schluesselwortzeilen eines Blockes
    INTEGER                         :: n_key
    !! Zaehler fuer gleichartige Schluesselwortzeilen eines Blockes
    INTEGER                         :: i_key
    !! Zaehler fuer Parameter in Schluesselwortzeilen
    INTEGER                         :: i_par
    !! Werte aller physikalischer Groessen innerhalb eines Blockes
    TYPE (t_physet)                 :: physet
    !! Daten der Regionen
    TYPE (t_region) ,   ALLOCATABLE :: region(:)
    !! Grenz-Koordinaten einer Region
    TYPE (t_point_2d) , ALLOCATABLE :: border(:)
    !! Daten an Messpositionen
    TYPE (t_mespos) ,   ALLOCATABLE :: mespos(:)
    !! Physikalische Daten einer Region
    TYPE (t_regphyset), ALLOCATABLE :: regphyset(:)
    !! Gebietsdefinition: Innerhalb/Ausserhalb Regions-Grenzkoordinaten
    LOGICAL                         :: region_inside
    !! Liste aller Messstationsnamen einer Region
    CHARACTER (LEN=c_len_mespos_name), ALLOCATABLE :: mespos_name(:)
    !! Koordinaten einer Messposition
    TYPE (t_point_2d)               :: coor
    !! Zaehler
    INTEGER                         :: i, k
    !! De-/Allocate-Status
    INTEGER                         :: stat
    !! Character-Rueckgabewert aus Dictionary-Paket
    CHARACTER (LEN=240)             :: c_value
    !! Doppelt genauer Rueckgabewert aus Dictionary-Paket
    REAL (KIND=Double)              :: d_value
    !! Einfach genauer Rueckgabewert aus Dictionary-Paket
    REAL (KIND=Single)              :: r_value
    !! Tiefen-Extremwerte
    REAL                            :: zmin_value, zmax_value
    !! Max. Abstand einer Messstation von einem Punkt p
    REAL                            :: sampling_point_maxdist
    !! logische Variable, geforderter Parameterwert in Dictionary-Paket vorhanden?
    !! (nur relevant fuer optionale Groessen)
    LOGICAL                         :: lex
    !
    ! Hinweis: die Datei wurde schon geoeffnet
    !
    WRITE(*,*)
    WRITE(*,*) 'IPDS-Datei ',TRIM(get_file_name(this%file)),' wird eingelesen...'
    !
    ! [1.1] Initialisieren und Setup des Dictionary-Paketes
    !
    IF ( no_error( ) ) CALL init_dictionary( )
    !
    ! [1.2] Lesen und Pruefen der Daten
    !
    IF ( no_error( ) ) CALL read_input_file ( TRIM( c_diconame ), this%file )
    !
    ! [1.3] Anzahl identischer Bloecke ermitteln
    !
    n_block = 0
    !
    DO i = 1, SIZE(blockname)
       !
       IF ( any_error ( ) ) EXIT
       !
       CALL get_nof_input_blocks ( blockname(i), n_block(i) )
       !
    END DO
    !
    ! [1.4] Transferieren der Daten aus dem dictionary-Paket in das (Arbeits-) Objekt
    !
    DO i = 1, SIZE(blockname)
       !
       IF ( n_block(i) == 1 ) THEN
       	  IF (DEBUG_ds > 0) THEN 
             WRITE(*,'(A,I8,3A)') '   > Lese ',n_block(i),' Block   "',TRIM(blockname(i)),'" ...'
          END IF
       ELSE
       	  IF (DEBUG_ds > 0) THEN
             WRITE(*,'(A,I8,3A)') '   > Lese ',n_block(i),' Bloecke "',TRIM(blockname(i)),'" ...'
          END IF
       END IF
       !
       IF ( any_error( )  ) EXIT
       IF ( n_block(i) == 0 ) CYCLE
       !
       SELECT CASE ( i )
          !
       CASE ( 1 ) ! Genau ein Block "date_and_time"
          !
          i_block = 1
          !
          DO k = 1, SIZE(keyname_blo1)
             !
             CALL get_nof_input_lines ( &
                  blockname(i), i_block, keyname_blo1(k), n_key )
             !
             IF ( any_error( ) ) EXIT
             IF ( n_key == 0   ) CYCLE
             !
             i_key = 1
             !
             SELECT CASE ( k )
                !
             CASE ( 1 ) ! Genau eine Keyzeile "datetime"
                !
                CALL get_input_data &
                     ( blockname(i), i_block, keyname_blo1(k), i_key, 1, c_value, lex )
                !
                CALL setup_datetime_object ( this, c_value )
                !
             CASE DEFAULT
                !
                ! Fehler -23020 : Key-Zeile kann noch nicht gelesen werden
                !
                !$OMP critical
                CALL setup_error_act ( all_errors(:), -23020, c_upname, c_modname )
                CALL setup_error_act ( '<key_name>',   TRIM( keyname_blo1( k ) ) )
                CALL setup_error_act ( '<block_name>', TRIM( blockname( i ) ) )
                !$OMP end critical
                !
             END SELECT
             !
          END DO
          !
       CASE ( 2 ) ! Genau ein Block "global_constant_values"
          !
          i_block = 1
          !
          CALL get_phy_in_block ( blockname(i), i_block, physet )
          !
          CALL setup_physet_object ( this, physet )
          !
          CALL kill_physet ( physet )
          !
       CASE ( 3 ) ! 1 .. n_block(i) Bloecke "region"
          !
          ALLOCATE ( region( n_block(i) ), STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23300, c_upname, c_modname, stat )
          !
          CALL new_region( region )
          !
          DO i_block = 1, n_block(i)
             !
             DO k = 1, SIZE(keyname_blo3)
                !
                CALL get_nof_input_lines ( &
                     blockname(i), i_block, keyname_blo3(k), n_key )
                !
                IF ( any_error( ) ) EXIT
                IF ( n_key == 0   ) CYCLE
                !
                i_key = 1
                !
                SELECT CASE ( k )
                   !
                CASE ( 1 ) ! Genau eine Keyzeile "region_name"
                   !
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo3(k), i_key, 1, c_value, lex )
                   !
                   CALL set_region_name ( region( i_block ), c_value )
                   !
                CASE ( 2 ) ! Eine oder mehrere Keyzeile/n "border_point"
                   !
                   ALLOCATE ( border( n_key ), STAT=stat )
                   IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23400, c_upname, c_modname, stat )
                   !
                   CALL new_point_2d( border )
                   !
                   DO i_key = 1, n_key
                      !
                      i_par = 1
                      CALL get_input_data &
                           ( blockname(i), i_block, keyname_blo3(k), i_key, i_par, d_value, lex )
                      !
                      CALL set_point_2d_x ( border( i_key ), d_value )
                      !
                      i_par = 2
                      CALL get_input_data &
                           ( blockname(i), i_block, keyname_blo3(k), i_key, i_par, d_value, lex )
                      !
                      CALL set_point_2d_y ( border( i_key ), d_value )
                      !
                   END DO
                   !
                   CALL set_region_border ( region( i_block ), border )
                   !
                   CALL kill_point_2d( border )
                   !
                   DEALLOCATE ( border, STAT=stat )
                   IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23410, c_upname, c_modname, stat )
                   !
                CASE DEFAULT
                   !
                   ! Fehler -23020 : Key-Zeile kann noch nicht gelesen werden
                   !
                   !$OMP critical
                   CALL setup_error_act ( all_errors(:), -23020, c_upname, c_modname )
                   CALL setup_error_act ( '<key_name>',   TRIM( keyname_blo3( k ) ) )
                   CALL setup_error_act ( '<block_name>', TRIM( blockname( i ) ) )
                   !$OMP end critical
                   !
                END SELECT
                !
             END DO
             !
          END DO
          !
          CALL setup_region_object ( this, region )
          !
          CALL kill_region( region )
          !
          DEALLOCATE ( region, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23310, c_upname, c_modname, stat )
          !
       CASE ( 4 ) ! 1 .. n_block(i) Bloecke "sampling_point"
          !
          ALLOCATE ( mespos( n_block(i) ), STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23500, c_upname, c_modname, stat )
          !
          CALL new_mespos( mespos )
          !
          DO i_block = 1, n_block(i)
             !
             DO k = 1, SIZE(keyname_blo4)
                !
                CALL get_nof_input_lines ( &
                     blockname(i), i_block, keyname_blo4(k), n_key )
                !
                IF ( any_error( ) ) EXIT
                IF ( n_key == 0   ) CYCLE
                !
                i_key = 1
                !
                SELECT CASE ( k )
                   !
                CASE (  1 ) ! Genau eine Keyzeile "sampling_point_name"
                   !
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo4(k), i_key, 1, c_value, lex )
                   !
                   CALL set_mespos_name ( mespos( i_block ), c_value )
                   !
                CASE (  2 ) ! Genau eine Keyzeile "sampling_point_xy"
                   !
                   CALL new_point_2d ( coor )
                   !
                   i_par = 1
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo4(k), i_key, i_par, d_value, lex )
                   !
                   CALL set_point_2d_x ( coor, d_value )
                   !
                   i_par = 2
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo4(k), i_key, i_par, d_value, lex )
                   !
                   CALL set_point_2d_y ( coor, d_value )
                   !
                   CALL set_mespos_coor ( mespos( i_block ), coor )
                   !
                   CALL kill_point_2d ( coor )
                   !
                CASE DEFAULT
                   !
                   ! Fehler -23020 : Key-Zeile kann noch nicht gelesen werden
                   !
                   !$OMP critical
                   CALL setup_error_act ( all_errors(:), -23020, c_upname, c_modname )
                   CALL setup_error_act ( '<key_name>',   TRIM( keyname_blo4( k ) ) )
                   CALL setup_error_act ( '<block_name>', TRIM( blockname( i ) ) )
                   !$OMP end critical
                   !
                END SELECT
                !
             END DO
             !
             CALL get_phy_in_block ( blockname(i), i_block, physet )
             !
             CALL set_mespos_physet ( mespos( i_block ), physet )
             !
             CALL kill_physet ( physet )
             !
          END DO
          !
          CALL setup_mespos_object ( this, mespos )
          !
          CALL kill_mespos( mespos )
          !
          DEALLOCATE ( mespos, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23510, c_upname, c_modname, stat )
          !
       CASE ( 5 ) ! 1 .. n_block(i) Bloecke "regional_values"
          !
          ALLOCATE ( regphyset( n_block(i) ), STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23600, c_upname, c_modname, stat )
          !
          CALL new_regphyset( regphyset )
          !
          DO i_block = 1, n_block(i)
             !
             zmin_value             = -9999.0
             zmax_value             = +9999.0
             sampling_point_maxdist = 50.E6
             !
             DO k = 1, SIZE(keyname_blo5)
                !
                CALL get_nof_input_lines ( &
                     blockname(i), i_block, keyname_blo5(k), n_key )
                !
                IF ( any_error( ) ) EXIT
                IF ( n_key == 0   ) CYCLE
                !
                i_key = 1
                !
                SELECT CASE ( k )
                   !
                CASE ( 1 ) ! Genau eine Keyzeile "region"
                   !
                   i_par = 1
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo5(k), i_key, i_par, c_value, lex )
                   !
                   SELECT CASE ( TRIM( c_value ) )
                      !
                   CASE ( 'inside' )
                      !
                      region_inside = .TRUE.
                      !
                   CASE ( 'outside' )
                      !
                      region_inside = .FALSE.
                      !
                   CASE DEFAULT
                      !
                      ! Fehler -23030 : Parameter kann noch nicht ausgewertet werden
                      !
                      !$OMP critical
                      CALL setup_error_act ( all_errors(:), -23030, c_upname, c_modname )
                      CALL setup_error_act ( '<par_inhalt>', TRIM( c_value ) )
                      CALL setup_error_act ( '<key_name>',   TRIM( keyname_blo5( k ) ) )
                      CALL setup_error_act ( '<block_name>', TRIM( blockname(i) ) )
                      !$OMP end critical
                      !
                   END SELECT
                   !
                   CALL set_regphyset_region_inside( regphyset( i_block ), region_inside )
                   !
                   i_par = 2
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo5(k), i_key, i_par, c_value, lex )
                   !
                   CALL set_regphyset_region_name( regphyset( i_block ), c_value )
                   !
                CASE ( 2 ) ! Genau eine Keyzeile "lower_validity_level"
                   !
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo5(k), i_key, 1, r_value, lex )
                   !
                   zmin_value = r_value
                   !
                CASE ( 3 ) ! Genau eine Keyzeile "upper_validity_level"
                   !
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo5(k), i_key, 1, r_value, lex )
                   !
                   zmax_value = r_value
                   !
                CASE ( 4 ) ! Eine oder mehrere Keyzeile/n "sampling_point"
                   !
                   ALLOCATE ( mespos_name( n_key ), STAT=stat )
                   IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23700, c_upname, c_modname, stat )
                   !
                   DO i_key = 1, n_key
                      !
                      CALL get_input_data &
                           ( blockname(i), i_block, keyname_blo5(k), i_key, 1, mespos_name( i_key ), lex )
                      !
                   END DO
                   !
                   CALL set_regphyset_mespos_name( regphyset( i_block ), mespos_name )
                   !
                   DEALLOCATE ( mespos_name, STAT=stat )
                   IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23710, c_upname, c_modname, stat )
                   !
                CASE ( 5 ) ! Genau eine Keyzeile "sampling_point_maxdist"
                   !
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo5(k), i_key, 1, r_value, lex )
                   !
                   sampling_point_maxdist = r_value
                   !
                CASE ( 6 ) ! Genau eine Keyzeile "interpolation_method"
                   !
                   CALL get_input_data &
                        ( blockname(i), i_block, keyname_blo5(k), i_key, 1, c_value, lex )
                   !
                   CALL set_regphyset_interpol_name( regphyset( i_block ), c_value )
                   !
                CASE DEFAULT
                   !
                   ! Fehler -23020 : Key-Zeile kann noch nicht gelesen werden
                   !
                   !$OMP critical
                   CALL setup_error_act ( all_errors(:), -23020, c_upname, c_modname )
                   CALL setup_error_act ( '<key_name>',   TRIM( keyname_blo5( k ) ) )
                   CALL setup_error_act ( '<block_name>', TRIM( blockname( i ) ) )
                   !$OMP end critical
                   !
                END SELECT
                !
             END DO
             !
             CALL set_regphyset_region_zmin( regphyset( i_block ), zmin_value )
             !
             CALL set_regphyset_region_zmax( regphyset( i_block ), zmax_value )
             !
             CALL set_regphyset_mespos_maxdist( regphyset( i_block ), sampling_point_maxdist )
             !
          END DO
          !
          CALL setup_regphyset_object ( this, regphyset )
          !
          CALL kill_regphyset( regphyset )
          !
          DEALLOCATE ( regphyset, STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23610, c_upname, c_modname, stat )
          !
       CASE DEFAULT
          !
          ! Fehler -23010 : Block kann noch nicht gelesen werden
          !
          !$OMP critical
          CALL setup_error_act ( all_errors(:), -23010, c_upname, c_modname )
          CALL setup_error_act ( '<block_name>' , TRIM(blockname(i)) )
          !$OMP end critical
          !
       END SELECT
       !
    END DO
    !
    ! [1.5] De-Initialisieren des Dictionary-Paketes
    !
    IF ( no_error( ) ) CALL clear_dictionary( )
    !
    IF (DEBUG_ds > 0) THEN
       WRITE(*,*) '... fertig'
    END IF
    !
  END SUBROUTINE read_ipds_0_v1_d
  !
  ! -------------------------------------------------------------------------
  ! PRIVATE-Methoden ueber implementierte Datei-Varianten
  ! -------------------------------------------------------------------------
  !
  !! Ermittle f&uuml;r die Datei "this" die Datei-Varianten-Nummer <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_variant_no ( this ) &
       RESULT( ivar )
    ! USE-Anweisungen
    USE b_file, ONLY :    &
         !   Typdefinitionen
         t_file,          &
         !   Routinen / Interfaces
         get_file_form,   &
         get_file_access, &
         get_file_delim,  &
         get_file_type
    !! Datei 
    TYPE (t_file) , INTENT(IN) :: this
    !! Ergebniswert: Nummer der aktuellen Variante <BR>
    !! -1 : nicht vorhanden 
    INTEGER :: ivar ! 
    !! logisches Feld
    LOGICAL :: l_ok(4) ! 
    !! Z&auml;hler
    INTEGER :: i       ! 
    !
    ivar = -1
    i    = 0
    !
    DO
       !
       i = i + 1
       !
       IF ( i > c_max_variants .OR. ivar /= -1 .OR. any_error ( ) ) EXIT
       !
       l_ok(1) = ( TRIM(                     get_file_form  ( this )   ) == TRIM( c_variants_form(i)   ) ) 
       l_ok(2) = ( TRIM(                     get_file_access( this )   ) == TRIM( c_variants_access(i) ) ) 
       l_ok(3) = ( TRIM(                     get_file_delim ( this )   ) == TRIM( c_variants_delim(i)  ) ) 
       l_ok(4) = ( TRIM( get_uppercase_char( get_file_type  ( this ) ) ) == TRIM( c_variants_type(i)   ) ) 
       !
       ivar = MERGE ( i, -1, ALL( l_ok(:) ) )
       !
    END DO
    !
  END FUNCTION get_ipds_variant_no
  !
  !! Pr&uuml;fe ob eine implementierte Datei-Variante vorliegt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_variant_no ( this ) &
       RESULT( ok )
    ! USE-Anweisungen
    USE b_file, ONLY :    &
         !   Typdefinitionen
         t_file,          &
         !   Routinen / Interfaces
         get_file_type  , &
         get_file_form  , &
         get_file_access, &
         get_file_delim
    !! Datei 
    TYPE (t_file) , INTENT(IN) :: this ! 
    !! Ergebniswert: Erforderliche Datei-Variante ist implementiert <BR>
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_ipds_variant_no'
    !
    ok = ( get_ipds_variant_no ( this ) > 0 )
    !
    IF ( .NOT. ok ) THEN
       !
       !$OMP critical
       CALL setup_error_act ( all_errors(:), -6001, c_upname, c_modname )
       CALL setup_error_act ( '<FortranFileType>'  , get_file_type  ( this ) )
       CALL setup_error_act ( '<FortranFileForm>'  , get_file_form  ( this ) )
       CALL setup_error_act ( '<FortranFileAccess>', get_file_access( this ) )
       CALL setup_error_act ( '<FortranFileDelim>' , get_file_delim ( this ) )
       !$OMP end critical
       !
    END IF
    !
  END FUNCTION ok_ipds_variant_no
  !
  ! -------------------------------------------------------------------------
  ! PRIVATE-Methoden ueber implementierte physikalische Groessen
  ! -------------------------------------------------------------------------
  !
  !! Anzahl unterschiedlicher innerhalb eines Blockes gesetzter
  !! physikalischer Groessen ermitteln
  FUNCTION get_nof_phy_in_block ( blockname, i_block ) &
       RESULT( n_phy_in_block )
    ! USE-Anweisungen
    USE p_dictionary_ui, ONLY : &
         !   Routinen / Interfaces
         get_nof_input_lines
    !! Name des Blockes
    CHARACTER (LEN=*) :: blockname
    !! Blocknummer
    INTEGER           :: i_block
    !! Ergebniswert: Anzahl unterschiedlicher physikalischer Groessen
    INTEGER :: n_phy_in_block
    !! Zaehler
    INTEGER :: k, l, n_key
    !
    n_phy_in_block = 0
    !
    ! In nullter Naeherung ist die Anzahl unterschiedlicher Keys gleich
    ! der Anzahl physikalischer Groessen.
    !
    DO k = 1, SIZE(c_phy_keyname)
       !
       ! Anzahl der verwendeten Zeilen mit dem Key holen
       !
       CALL get_nof_input_lines ( &
            blockname, i_block, c_phy_keyname(k), n_key )
       !
       IF ( any_error( ) ) EXIT
       IF ( n_key == 0   ) CYCLE
       !
       ! Neue physikalische Groesse
       !
       n_phy_in_block = n_phy_in_block + 1
       !
    END DO
    !
  END FUNCTION get_nof_phy_in_block
  !
  !! Namen unterschiedlicher innerhalb eines Blockes gesetzter
  !! physikalischer Groessen ermitteln
  FUNCTION get_phy_name_in_block ( blockname, i_block, n_phy_in_block ) &
       RESULT( phy_name_in_block )
    !
    ! USE-Anweisungen
    USE p_dictionary_ui, ONLY : &
         !   Routinen / Interfaces
         get_nof_input_lines
    !
    ! Formalparameter
    !! Name des Blockes
    CHARACTER (LEN=*)  :: blockname
    !! Blocknummer
    INTEGER            :: i_block
    !! Anzahl unterschiedlicher physikalischer Groessen
    INTEGER            :: n_phy_in_block
    !! Ergebniswert: Namen aller physikalischer Groessen
    CHARACTER (LEN=80) :: phy_name_in_block(n_phy_in_block)
    ! Lokale Variablen
    !! Name der Function
    CHARACTER (LEN=21) , PARAMETER :: c_upname='get_phy_name_in_block'
    !! Kennung, ob der Name einer phys. Groesse bereits erkannt wurde
    LOGICAL :: phy_ex
    !! Zaehler
    INTEGER :: i, k, i_phy, n_key
    !
    phy_name_in_block = REPEAT( ' ', LEN( phy_name_in_block(1) ) )
    !
    ! Aus dem Keyschluesselnamen die physikalische Groesse ableiten.
    ! Darauf achten, dass die Namen physikalischer Groessen nicht
    ! doppelt abgelegt werden.
    !
    i_phy = 0
    !
    DO k = 1, SIZE(c_phy_keyname)
       !
       ! Anzahl der verwendeten Zeilen mit dem Key holen
       !
       CALL get_nof_input_lines ( &
            blockname, i_block, c_phy_keyname(k), n_key )
       !
       IF ( any_error( ) ) EXIT
       IF ( n_key == 0   ) CYCLE
       !
       ! Ist der Name der mit diesem Key verbundenen physikalischen Groesse
       ! bereits eingespeichert?
       !
       i      = 0
       phy_ex = .FALSE.
       !
       DO
          !
          i = i + 1
          IF ( i > i_phy ) EXIT
          !
          IF ( TRIM( phy_name_in_block(i) ) == TRIM( c_phy_name_de(c_phy_idx(k)) ) ) phy_ex = .TRUE.
          !
          IF ( phy_ex ) EXIT
          !
       END DO
       !
       ! Nur weitermachen, wenn der Name der physikalischen Groesse
       ! noch nicht enthalten ist
       !
       IF ( phy_ex ) CYCLE
       !
       ! Namen einspeichern
       !
       i_phy = i_phy + 1
       IF ( i_phy > n_phy_in_block ) CALL setup_error_act( all_errors(:), -23050, c_upname, c_modname )
       !
       phy_name_in_block(i_phy) = TRIM( c_phy_name_de(c_phy_idx(k)) )
       !
    END DO
    !
  END FUNCTION get_phy_name_in_block
  !
  !! Anzahl unterschiedlicher innerhalb eines Blockes gesetzter
  !! Varianten aller physikalischer Groessen ermitteln
  FUNCTION get_nof_var_in_block ( blockname, i_block, n_phy_in_block, phy_name_in_block ) &
       RESULT( n_var_in_block )
    ! USE-Anweisungen
    USE p_dictionary_ui, ONLY : &
         !   Routinen / Interfaces
         get_nof_input_lines
    !! Name des Blockes
    CHARACTER (LEN=*) :: blockname
    !! Blocknummer
    INTEGER           :: i_block
    !! Anzahl unterschiedlicher physikalischer Groessen
    INTEGER           :: n_phy_in_block
    !! Namen aller physikalischer Groessen
    CHARACTER (LEN=*) :: phy_name_in_block(n_phy_in_block)
    !! Ergebniswert: Anzahl Varianten aller physikalischer Groessen
    INTEGER :: n_var_in_block(n_phy_in_block)
    ! Lokale Variablen
    !! Name der Function
    CHARACTER (LEN=20) , PARAMETER :: c_upname='get_nof_var_in_block'
    !! Zaehler
    INTEGER :: idx, k, n_key
    !! Anzahl Variationen eines Keys
    INTEGER :: n_var
    !
    n_var_in_block = 0
    !
    ! Aus dem Keyschluesselnamen die physikalische Groesse ableiten.
    ! Damit den richtigen Index fuer das Feld "n_var_in_block" bestimmen.
    !
    DO k = 1, SIZE(c_phy_keyname)
       !
       ! Anzahl der verwendeten Zeilen mit dem Key holen
       !
       CALL get_nof_input_lines ( &
            blockname, i_block, c_phy_keyname(k), n_key )
       !
       IF ( any_error( ) ) EXIT
       IF ( n_key == 0   ) CYCLE
       !
       ! Den Index mittels des Namens der phys. Groesse bestimmen
       !
       idx = 0
       !
       DO
          !
          idx = idx + 1
          IF ( idx > n_phy_in_block ) EXIT
          !
          IF ( TRIM( phy_name_in_block(idx) ) == TRIM( c_phy_name_de(c_phy_idx(k)) ) ) EXIT
          !
       END DO
       !
       IF ( idx > n_phy_in_block ) CALL setup_error_act( all_errors(:), -23060, c_upname, c_modname )
       !
       ! Die akt. Anzahl Varianten fuer diese Kennung bestimmen
       !
       SELECT CASE ( k )
          !
       CASE ( 1, 2, 3, 5, 7, 9, 10, 11, 13, 14, &  ! "waterlevel", "salinity", "temperature",
             15, 16, 17, 18, 19, 20, 21, 22, 23, 24 )
          !                                    "pore_water_fraction",
          !                                    "ripple_height", "dune_height",
          !                                    "rigid_layer_depth", "rigid_layer_roughness"
          !                                    "z0_roughnesslength", "bottom_friction_*",
          !                                    "time_dependent_bathymetry", 
          !                                    "critical_stress_for_deposition",
          !                                    "critical_stress_for_erosion",
          !                                    "erodibility_parameter", "sediment_mass"  
          !                                    "current_velocity_(x-dir.)", "current_velocity_(y-dir.)"
          !                                      
          n_var = 1
          !
       CASE ( 4, 12, 25 )                ! "sediment_fraction", "suspendedload", "deposition rate"
          !
          n_var = n_key
          !
       CASE ( 6, 8 )                     ! "ripple_wavenumber", "dune_wavenumber"
          !
          n_var = 2
          !
       CASE DEFAULT
          !
          ! Fehler -23020 : Key-Zeile kann noch nicht gelesen werden
          !
          !$OMP critical
          CALL setup_error_act ( all_errors(:), -23020, c_upname, c_modname )
          CALL setup_error_act ( '<key_name>',   TRIM( c_phy_keyname( k ) ) )
          CALL setup_error_act ( '<block_name>', TRIM( blockname ) )
          !$OMP end critical
          !
       END SELECT
       !
       n_var_in_block( idx ) = n_var_in_block( idx ) + n_var
       !
    END DO
    !
  END FUNCTION get_nof_var_in_block
  !
  !! &Uuml;betragen der in einem Block einer Datei stehenden Daten zu den
  !! physikalischen Groessen <BR>
  SUBROUTINE get_phy_in_block ( blockname, i_block, physet )
    !
    ! USE-Anweisungen
    USE p_dictionary_ui, ONLY : &
         !   Routinen / Interfaces
         get_nof_input_lines,   &
         get_input_data
    !
    ! Formalparameter
    !! Name des Blockes
    CHARACTER (LEN=*), INTENT(IN)  :: blockname
    !! Blocknummer
    INTEGER          , INTENT(IN)  :: i_block
    !! Alle Daten der physikalischen Groessen eines Blocks
    TYPE (t_physet)  , INTENT(INOUT) :: physet
    !
    ! lokale Parameter / Variablen
    !! Name der Routine
    CHARACTER (LEN=16), PARAMETER   :: c_upname='get_phy_in_block'
    !! Anzahl unterschiedlicher physikalischer Groessen
    INTEGER                         :: n_phy_in_block
    !! Daten der unterschiedlichen physikalischen Groessen
    TYPE (t_phyval)   , ALLOCATABLE :: phyval(:)
    !! Namen aller physikalischen Groessen
    CHARACTER (LEN=80), ALLOCATABLE :: phy_name_in_block(:)
    !! Anzahl Varianten aller physikalischen Groessen
    INTEGER           , ALLOCATABLE :: n_var_in_block(:)
    !! Index fuer physikalische Groesse
    INTEGER                         :: idx
    !! De-/Allocate-Status
    INTEGER                         :: stat
    !! logische Variable, geforderter Parameterwert in Dictionary-Paket vorhanden?
    !! (nur relevant fuer optionale Groessen)
    LOGICAL                         :: lex
    !! aktuelle Anzahl gleichartiger Schluesselwortzeilen eines Blockes
    INTEGER                         :: n_key
    !! Zaehler fuer gleichartige Schluesselwortzeilen eines Blockes
    INTEGER                         :: i_key
    !! Max. Anzahl Variationen aller physikalischer Groessen
    INTEGER                         :: max_n_var_in_block
    !! Zaehler fuer Parameter in Schluesselwortzeilen
    INTEGER                         :: i_par
    !! Anzahl memorierter Varianten
    INTEGER           , ALLOCATABLE :: phy_i_var(:)
    !! Variantennamen aller physikalischen Groessen
    CHARACTER (LEN=80), ALLOCATABLE :: phy_var_name(:,:)
    !! Typus aller Varianten aller physikalischer Groessen
    INTEGER           , ALLOCATABLE :: phy_type(:,:)
    !! Werte aller Varianten aller physikalischer Groessen
    REAL              , ALLOCATABLE :: phy_val(:,:)
    !! Zaehler
    INTEGER                         :: k, j
    !! Hilfsstring
    CHARACTER (LEN=80)              :: c_value
    !
    ! [1.1] Initialisierungen
    !
    CALL new_physet ( physet )
    !
    ! [1.2] Anzahl physikalischer Groessen bestimmen
    !
    n_phy_in_block = get_nof_phy_in_block ( blockname, i_block )
    !
    ! [1.3] Speicher fuer die Daten bereitstellen
    !
    ALLOCATE ( &
         phyval (           n_phy_in_block ), &
         phy_name_in_block( n_phy_in_block ), &
         n_var_in_block(    n_phy_in_block ), &
         STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23100, c_upname, c_modname, stat )
    !
    CALL new_phyval ( phyval )
    !
    ! [1.4] Namen aller physikalischer Groessen holen
    !
    phy_name_in_block = get_phy_name_in_block ( blockname, i_block, n_phy_in_block )
    !
    ! [1.5] Anzahl Varianten aller physikalischer Groessen bestimmen
    !
    n_var_in_block = get_nof_var_in_block ( blockname, i_block, n_phy_in_block, phy_name_in_block )
    !
    ! [1.6] Hilfsfelder bereitstellen
    !
    max_n_var_in_block = MAXVAL( n_var_in_block )
    !
    ALLOCATE ( &
         phy_i_var    ( n_phy_in_block ),                     &
         phy_var_name ( n_phy_in_block, max_n_var_in_block ), &
         phy_type     ( n_phy_in_block, max_n_var_in_block ), &
         phy_val      ( n_phy_in_block, max_n_var_in_block ), &
         STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23200, c_upname, c_modname, stat )
    !
    phy_i_var = 0
    !
    ! [2] Daten aus dem Dictionary-Paket uebertragen
    !
    DO k = 1, SIZE(c_phy_keyname)
       !
       ! [2.1] Anzahl Schluesselwortzeilen fuer dieses Kennwort ermitteln
       !
       CALL get_nof_input_lines ( &
            blockname, i_block, c_phy_keyname(k), n_key )
       !
       IF ( any_error( ) ) EXIT
       IF ( n_key == 0   ) CYCLE
       !
       ! [2.2] Den Index mittels des Namens der phys. Groesse bestimmen
       !
       idx = 0
       !
       DO
          !
          idx = idx + 1
          IF ( idx > n_phy_in_block ) EXIT
          !
          IF ( TRIM( phy_name_in_block(idx) ) == TRIM( c_phy_name_de(c_phy_idx(k)) ) ) EXIT
          !
       END DO
       !
       IF ( idx > n_phy_in_block ) CALL setup_error_act( all_errors(:), -23060, c_upname, c_modname )
       !
       ! [2.3] Die Daten der verschiedenen physikalischen Groessen aus dem Dictionary-Paket holen
       !       und zwischenspeichern
       !
       SELECT CASE ( k )
          !
       CASE (  1 ) ! Genau eine Keyzeile "waterlevel"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE (  2 ) ! Genau eine Keyzeile "salinity"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE (  3 ) ! Genau eine Keyzeile "temperature"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE (  4 ) ! Eine oder mehrere Keyzeile/n "sediment_fraction"
          !
          DO i_key = 1, n_key
             !
             phy_i_var( idx ) = phy_i_var( idx ) + 1
             !
             phy_type( idx, phy_i_var( idx ) ) = 1
             !
             i_par = 1
             CALL get_input_data &
                  ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
                  phy_var_name( idx, phy_i_var( idx ) ), lex )
             !
             i_par = 2
             CALL get_input_data &
                  ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
                  phy_val( idx, phy_i_var( idx ) ), lex )
             !
          END DO
          !
       CASE (  5 ) ! Genau eine Keyzeile "porosity"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE (  6 ) ! Genau eine Keyzeile "ripple_wavenumber"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_type( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = 'Riffelwellenzahl (x-R.)'
          !
          i_par = 1
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_type( idx, phy_i_var( idx ) ) = 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = 'Riffelwellenzahl (y-R.)'
          !
          i_par = 2
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE (  7 ) ! Genau eine Keyzeile "ripple_height"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE (  8 ) ! Genau eine Keyzeile "dune_wavenumber"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_type( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = 'Duenenwellenzahl (x-R.)'
          !
          i_par = 1
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_type( idx, phy_i_var( idx ) ) = 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = 'Duenenwellenzahl (y-R.)'
          !
          i_par = 2
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE (  9 ) ! Genau eine Keyzeile "dune_height"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 10 ) ! Genau eine Keyzeile "rigid_layer_depth"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          i_key = 1
          !
          i_par = 1
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, c_value, lex )
          !
          j = 0
          DO
             j = j + 1
             IF ( j > c_nof_rl_depth_type ) EXIT
             !
             IF ( TRIM( c_value ) == TRIM( c_rl_depth_type( j ) ) ) EXIT
             !
          ENDDO
          !
          IF ( j > c_nof_rl_depth_type ) THEN
             !
             ! Fehler -23030 : Parameter kann noch nicht ausgewertet werden
             !
             !$OMP critical
             CALL setup_error_act ( all_errors(:), -23030, c_upname, c_modname )
             CALL setup_error_act ( '<par_inhalt>', TRIM( c_value ) )
             CALL setup_error_act ( '<key_name>',   TRIM( c_phy_keyname( k ) ) )
             CALL setup_error_act ( '<block_name>', TRIM( blockname ) )
             !$OMP end critical
             !
          ELSE
             !
             phy_type( idx, phy_i_var( idx ) ) = c_rl_depth_nr( j )
             !
          END IF
          !
          i_par = 2
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 11 ) ! Genau eine Keyzeile "rigid_layer_roughness"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          i_key = 1
          !
          i_par = 1
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, c_value, lex )
          !
          j = 0
          DO
             j = j + 1
             IF ( j > c_nof_rl_rough_type ) EXIT
             !
             IF ( TRIM( c_value ) == TRIM( c_rl_rough_type( j ) ) ) EXIT
             !
          ENDDO
          !
          IF ( j > c_nof_rl_rough_type ) THEN
             !
             ! Fehler -23030 : Parameter kann noch nicht ausgewertet werden
             !
             !$OMP critical
             CALL setup_error_act ( all_errors(:), -23030, c_upname, c_modname )
             CALL setup_error_act ( '<par_inhalt>', TRIM( c_value ) )
             CALL setup_error_act ( '<key_name>',   TRIM( c_phy_keyname( k ) ) )
             CALL setup_error_act ( '<block_name>', TRIM( blockname ) )
             !$OMP end critical
             !
          ELSE
             !
             phy_type( idx, phy_i_var( idx ) ) = c_rl_rough_nr( j )
             !
          END IF
          !
          i_par = 2
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 12 ) ! Eine oder mehrere Keyzeile/n "suspendedload"
          !
          DO i_key = 1, n_key
             !
             phy_i_var( idx ) = phy_i_var( idx ) + 1
             !
             phy_type( idx, phy_i_var( idx ) ) = 1
             !
             i_par = 1
             CALL get_input_data &
                  ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
                  phy_var_name( idx, phy_i_var( idx ) ), lex )
             !
             i_par = 2
             CALL get_input_data &
                  ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
                  phy_val( idx, phy_i_var( idx ) ), lex )
             !
          END DO
          !
       CASE ( 13 ) ! Genau eine Keyzeile "z0_roughnesslength"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
                 phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 14 ) ! Genau eine Keyzeile "bottom_friction_chezy"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 15 ) ! Genau eine Keyzeile "bottom_friction_manning_str"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 16 ) ! Genau eine Keyzeile "bottom_friction_nikuradse"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 17 ) ! Genau eine Keyzeile "bottom_friction_white_colebr"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 18 ) ! Genau eine Keyzeile "time_dependent_bathymetry"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 19 ) ! Genau eine Keyzeile "critical_stress_for_deposition"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 20 ) ! Genau eine Keyzeile "critical_stress_for_erosion"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 21 ) ! Genau eine Keyzeile "erodibility_parameter"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
          !
       CASE ( 22 ) ! Genau eine Keyzeile "sediment_mass"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          !
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          !
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 23 ) ! Genau eine Keyzeile "current_velocity_(x-dir.)"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 24 ) ! Genau eine Keyzeile "current_velocity_(y-dir.)"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 25 ) ! Eine oder mehrere Keyzeile/n "deposition rate"
          !
          DO i_key = 1, n_key
             !
             phy_i_var( idx ) = phy_i_var( idx ) + 1
             !
             phy_type( idx, phy_i_var( idx ) ) = 1
             !
             i_par = 1
             CALL get_input_data &
                  ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
                  phy_var_name( idx, phy_i_var( idx ) ), lex )
             !
             i_par = 2
             CALL get_input_data &
                  ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
                  phy_val( idx, phy_i_var( idx ) ), lex )
             !
          END DO
          !
       CASE ( 26 ) ! Genau eine Keyzeile "mean_wave_period"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 27 ) ! Genau eine Keyzeile "significant_wave_heigth"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 28 ) ! Genau eine Keyzeile "mean_wave_direction_(x-dir.)"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE ( 29 ) ! Genau eine Keyzeile "mean_wave_direction_(y-dir.)"
          !
          phy_i_var( idx ) = phy_i_var( idx ) + 1
          !
          phy_var_name( idx, phy_i_var( idx ) ) = c_phy_name_de(c_phy_idx(k))
          phy_type    ( idx, phy_i_var( idx ) ) = 1
          !
          i_key = 1
          i_par = 1
          !
          CALL get_input_data &
               ( blockname, i_block, c_phy_keyname(k), i_key, i_par, &
               phy_val( idx, phy_i_var( idx ) ), lex )
          !
       CASE DEFAULT
          !
          ! Fehler -23020 : Key-Zeile kann noch nicht gelesen werden
          !
          !$OMP critical
          CALL setup_error_act ( all_errors(:), -23020, c_upname, c_modname )
          CALL setup_error_act ( '<key_name>',   TRIM( c_phy_keyname( k ) ) )
          CALL setup_error_act ( '<block_name>', TRIM( blockname ) )
          !$OMP end critical
          !
       END SELECT
       !
    END DO
    !
    ! [3] Alle gesammelten Daten der physikalischen Groessen uebertragen
    !
    DO idx = 1, n_phy_in_block
       !
       CALL set_phyval_name (     phyval( idx ), phy_name_in_block( idx ) )
       CALL set_phyval_var_name ( phyval( idx ), phy_var_name     ( idx, 1:n_var_in_block( idx ) ) )
       CALL set_phyval_type (     phyval( idx ), phy_type         ( idx, 1:n_var_in_block( idx ) ) )
       CALL set_phyval_val (      phyval( idx ), phy_val          ( idx, 1:n_var_in_block( idx ) ) )
       !
       IF ( is_phyval_frac( phyval( idx ) ) ) &
            CALL normalize_phyval ( phyval( idx ), SUM=1., WARN=.TRUE. )
       !
    END DO
    !
    CALL set_physet_set ( physet, phyval )
    !
    ! [4] Aufraeumen
    !
    DEALLOCATE ( phy_i_var, phy_var_name, phy_type, phy_val, STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23210, c_upname, c_modname, stat )
    !
    CALL kill_phyval ( phyval )
    !
    DEALLOCATE ( phyval, phy_name_in_block, n_var_in_block, STAT=stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -23110, c_upname, c_modname, stat )
    !
  END SUBROUTINE get_phy_in_block
  ! 
END MODULE m_ipds_readfile
! TailOfPackageModule ------------------------------------------------------
