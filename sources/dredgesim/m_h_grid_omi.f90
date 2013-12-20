! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Modul zum Bereitstellen OpenMI-konformer Informationen f&uuml;r H-Grid</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 4.1 vom 08/22/05, Quellcode: mod_m_h_grid_omi.f90
!! <HR>
!! generation of OpenMI-compliant informations related to the H-Grid software package <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-03-07 : G. Lang : Erstversion
!  02.01 : 2005-03-07 : G. Lang : Erstversion, auf Version 2 angepasst
!  02.02 : 2005-03-10 : G. Lang : div. Erweiterungen fuer Konfigurationsphase OpenMI
!  02.03 : 2005-03-16 : G. Lang : Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries 
!  02.04 : 2005-03-24 : G. Lang : nur noch drei-dimensionale Koordinatenobjekte erzeugen
!  02.05 : 2005-06-01 : G. Lang : Topographie ist nicht mehr zeitabhaengig
!  03.01 : 2005-07-21 : G. Lang : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
!  04.01 : 2005-08-22 : G. Lang : Lesen/Schreiben Delft3D-Gitternetz
!
!!                                            
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Bereitstellen verschiedener Leistungen zum Erzeugen OpenMI-konformer Daten f&uuml;r "h_grid".
!! </OL>
!! <HR>
!
MODULE m_h_grid_omi
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten [ggf. entfernen]
  USE b_constants, ONLY :   &
       Double
  !
  ! [A.2] Basis-Modul mit Typ+Methoden "Fehlerbehandlung"
  USE b_error, ONLY :       &
       ! Routinen / Interfaces
       no_error,            &
       any_error,           &
       setup_error_act
  !
  ! [A.3] Basis-Modul mit Typ+Methoden "Zeitangaben"
  USE b_datetime, ONLY :   &
       ! Typdefinition
       t_datetime
  !
  ! [A.4] Basis-Modul mit Typ+Methoden "Dateibehandlung"
  USE b_file, ONLY : &
       ! Typdefinition
       t_file,       &
       ! Routinen / Interfaces
       get_file_type
  !
  ! [A.5] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme <EM>Quantit&auml;ten</EM>
  USE b_omi_quant, ONLY :      &
       ! Typdefinition
       t_omi_quant,            &
       ! Routinen / Interfaces
       new_omi_quant,          &
       kill_omi_quant,         &
       get_max_omi_quant,      &
       set_omi_quant
  !
  ! [A.6] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Koordinatensystem-Beschreibungen
  USE b_omi_space, ONLY :      &
       ! Typdefinition
       t_omi_space,            &
       ! Routinen / Interfaces
       new_omi_space,          &
       kill_omi_space,         &
       set_omi_space_id
  !
  ! [A.7] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Koordinaten-Objekte
  USE b_omi_xyz, ONLY :         &
       ! Typdefinition
       t_omi_xyz,               &
       ! Routinen / Interfaces
       new_omi_xyz,             &
       kill_omi_xyz,            &
       set_omi_xyz_id,          &
       set_omi_xyz_description, &
       set_omi_xyz_refsystem,   &
       set_omi_xyz_ztype,       &
       set_omi_xyz_ctype,       &
       set_omi_xyz_hlimit,      &
       create_omi_xyz_no_layers
  !
  ! [A.8] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Verweislisten-Objekte
  USE b_omi_ind, ONLY :         &
       ! Typdefinition
       t_omi_ind,               &
       ! Konstante
       c_len_omi_ind_id,        &
       ! Routinen / Interfaces
       new_omi_ind,             &
       kill_omi_ind,            &
       set_omi_ind_id,          &
       set_omi_ind_description, &
       set_omi_ind_stru_start,  &
       set_omi_ind_stru_len,    &
       set_omi_ind_stru_list,   &
       set_omi_ind_stru_id
  !
  ! [A.9] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme ElementSets-Objekte
  USE b_omi_ele, ONLY :         &
       ! Typdefinition
       t_omi_ele,               &
       ! Routinen / Interfaces
       new_omi_ele,             &
       kill_omi_ele,            &
       set_omi_ele_auto
  !
  ! [A.10] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Arguments-Objekte
  USE b_omi_arg, ONLY :         &
       ! Typdefinition
       t_omi_arg,               &
       ! Routinen / Interfaces
       new_omi_arg,             &
       kill_omi_arg,            &
       set_omi_arg_key,         &
       set_omi_arg_value,       &
       set_omi_arg_readonly,    &
       set_omi_arg_description
  !
  ! [A.11] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme DataOperations-Objekte
  USE b_omi_dope, ONLY :        &
       ! Typdefinition
       t_omi_dope,              &
       ! Routinen / Interfaces
       new_omi_dope,            &
       kill_omi_dope,           &
       set_omi_dope_auto
  !
  ! [A.12] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme ExchangeItems-Objekte
  USE b_omi_exch, ONLY :        &
       ! Typdefinition
       t_omi_exch,              &
       ! Routinen / Interfaces
       new_omi_exch,            &
       kill_omi_exch,           &
       set_omi_exch_quant_ref,  &
       set_omi_exch_ele_ref,    &
       set_omi_exch_dope_ref,   &
       set_omi_exch_role
  !
  ! [A.13] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme ExchangeItems-Objekte
  USE b_omi_stamp, ONLY :       &
       ! Typdefinition
       t_omi_stamp,             &
       ! Routinen / Interfaces
       new_omi_stamp,           &
       kill_omi_stamp,          &
       set_omi_stamp_modjulianday
  !
  ! ---------------------------------------------------------------------
  ! [B]  Module des Paketes "SediMorph"
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Modul mit globalen Daten
  !
  USE m_h_grid_data, ONLY :     &
       ! Typdefinition
       t_h_grid,                &
       ! Konstante             
       c_missing_h_grid_double, &
       ! Daten
       all_errors,            &
       ! Routinen / Interfaces
       setup_quant_object,    &
       setup_xyz_object,      &
       setup_ind_object,      &
       setup_ele_object,      &
       setup_dope_object,     &
       setup_exch_object,     &
       setup_stamp_object,    &
       ok_h_grid_variant_no,  &
       get_h_grid_variant_no, &
       get_file_object,       &
       get_hland_object,      &
       get_xy_object,         &
       get_hv_object,         &
       get_nv_object,         &
       get_ne_object,         &
       get_ks_object,         &
       get_jb_object,         &
       get_jt_object,         &
       get_nen_object,        &
       get_ind_object,        &
       get_xyz_object,        &
       get_ele_object,        &
       get_quant_object,      &
       get_dope_object,       &
       get_time_object,       &
       get_b_ms_object,       &
       get_b_ss_object,       &
       get_b_s_object,        &
       get_b_t_object,        &
       get_b_v_object
  !
  ! [B.2] Modul mit DERIVE-Methoden
  !
  USE m_h_grid_derive, ONLY : &
       ! Routinen / Interfaces
       derive_hv, derive_b_ms, derive_b_ss, derive_b_s, derive_b_v, &
       derive_b_t, derive_nen, derive_ks
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
  !! Erzeuge alle OpenMI-konformen Daten f&uuml;r ein Objekt des Typs "t_h_grid"
  INTERFACE create_omi
     MODULE PROCEDURE create_omi_w0
  END INTERFACE
  !
  PUBLIC :: create_omi
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Konstante
  !! maximale Anzahl der Objekte vom Typ "t_omi_dope" und "t_omi_arg" --
  INTEGER            , PARAMETER :: c_max_omi_dope_idx=1 ! 
  !! Zeiger auf c_dope_id(:) ind mod_b_omi_dope
  INTEGER            , PARAMETER :: c_omi_dope_idx(c_max_omi_dope_idx)=(/ 9 /) ! 
  ! --------------------------------------------------------------------
  !! Name des Moduls
  CHARACTER (LEN=12) , PRIVATE, PARAMETER :: c_modname='m_h_grid_omi' ! 
  !
  ! [D.3] lokale Schnittstellen
  !
  !! Erzeuge die <EM>Quantities</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_quant
     MODULE PROCEDURE create_omi_quant_w0
  END INTERFACE
  !! Erzeuge <EM>Koordinatenobjekte</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_xyz
     MODULE PROCEDURE create_omi_xyz_w0
  END INTERFACE
  !! Erzeuge <EM>Verweislisten</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_ind
     MODULE PROCEDURE create_omi_ind_w0
  END INTERFACE
  !! Erzeuge <EM>ElementSets</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_ele
     MODULE PROCEDURE create_omi_ele_w0
  END INTERFACE
  !! Erzeuge <EM>DataOperations</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_dope
     MODULE PROCEDURE create_omi_dope_w0
  END INTERFACE
  !! Erzeuge <EM>ExchangeItems</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_exch
     MODULE PROCEDURE create_omi_exch_w0
  END INTERFACE
  !! Erzeuge <EM>Stamp</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_stamp
     MODULE PROCEDURE create_omi_stamp_w0
  END INTERFACE
  !! Ermittle die Anzahl der Unterabschnitte mit vorgegebener Kennung
  INTERFACE get_ss_count
     MODULE PROCEDURE get_ss_count_w0
  END INTERFACE
  !! Ermittle die Anzahl der Kanten in den Unterabschnitten mit vorgegebener Kennung
  INTERFACE get_ss_len
     MODULE PROCEDURE get_ss_len_w0
  END INTERFACE
  !! Ermittle die Startpositionen der Kanten in den Unterabschnitten mit vorgegebener Kennung
  INTERFACE get_ss_start
     MODULE PROCEDURE get_ss_start_w0
  END INTERFACE
  !! Ermittle die Anzahl der unterschiedlichen Knoten <BR>
  !! a) in <EM>einem</EM> Unterabschnitt mit vorgegebener Kennung und Nummer <BR>
  !! b) in <EM>allen</EM> Unterabschnitten mit vorgegebener Kennung
  INTERFACE get_vv_diff_count
     MODULE PROCEDURE get_vv_diff_count_w000
     MODULE PROCEDURE get_vv_diff_count_w00
  END INTERFACE
  !! Ermittle die Liste der unterschiedlichen Knoten <BR>
  !! a) in <EM>einem</EM> Unterabschnitt mit vorgegebener Kennung und Nummer <BR>
  !! b) in <EM>allen</EM> Unterabschnitten mit vorgegebener Kennung
  INTERFACE get_vv_diff_list
     MODULE PROCEDURE get_vv_diff_list_w000
     MODULE PROCEDURE get_vv_diff_list_w00
  END INTERFACE
  !! Ermittle die Anzahl der Knoten bei polygonaler Darstellung der Randabschnitte <BR>
  !! a) in <EM>einem</EM> Unterabschnitt mit vorgegebener Kennung und Nummer <BR>
  !! b) in <EM>allen</EM> Unterabschnitten mit vorgegebener Kennung
  INTERFACE get_vv_poly_count
     MODULE PROCEDURE get_vv_poly_count_w000
     MODULE PROCEDURE get_vv_poly_count_w00
  END INTERFACE
  !! Ermittle die Liste der Knoten bei polygonaler Darstellung der Randabschnitte <BR>
  !! a) in <EM>einem</EM> Unterabschnitt mit vorgegebener Kennung und Nummer
  INTERFACE get_vv_poly_list
     MODULE PROCEDURE get_vv_poly_list_w000
  END INTERFACE
  !! Ersetze die Fragezeichen in einem Text durch eine Integer-Zahl
  INTERFACE replace_question_mark
     MODULE PROCEDURE replace_question_mark_0_0
  END INTERFACE
  !! Ermittle ein Feld mit true/false-Kennung zum Kennzeichnen des ersten
  !! Auftretens einer Zahl in einem Integer-Feld
  INTERFACE get_int_diff_ind
     MODULE PROCEDURE get_int_diff_ind_1
  END INTERFACE
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
  !! Erzeuge alle OpenMI-konformen Komponenten des aktuellen Datenobjektes "this" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this !
    !! Name des Unterprogramms
    CHARACTER (LEN=13) , PARAMETER :: c_upname='create_omi_w0' ! 
    !
    ! [1.1] Erzeugen der Beschreibungen der <EM>Quantities</EM>
    IF ( no_error( ) ) CALL create_omi_quant ( this )
    ! [1.2] Erzeugen der Koordinaten-Objekte
    IF ( no_error( ) ) CALL create_omi_xyz   ( this )
    ! [1.3] Erzeugen der Verweislisten-Objekte
    IF ( no_error( ) ) CALL create_omi_ind   ( this )
    ! [1.4] Erzeugen der ElementSets-Objekte
    IF ( no_error( ) ) CALL create_omi_ele   ( this )
    ! [1.5] Erzeugen der DataOperations-Objekte
    IF ( no_error( ) ) CALL create_omi_dope  ( this )
    ! [1.6] Erzeugen der ExchangeItems-Objekte
    IF ( no_error( ) ) CALL create_omi_exch  ( this )
    ! [1.7] Erzeugen der Zeitangabe-Objekte
    IF ( no_error( ) ) CALL create_omi_stamp ( this )
    !
  END SUBROUTINE create_omi_w0
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
  ! >>> Quantities <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !! Erzeuge die Beschreibung der OpenMI-konformen <EM>Quantities</EM> 
  !! f&uuml;r das aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_quant_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this !
    !! Name des Unterprogramms
    CHARACTER (LEN=19) , PARAMETER :: c_upname='create_omi_quant_w0' ! 
    !! Code-Kennung der zu erzeugenden Gr&ouml;&szlig;e (<EM>Quantity</EM>
    INTEGER , PARAMETER :: c_code(1) = (/ 17 /) ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10)               :: ch            ! 
    TYPE (t_omi_quant) , ALLOCATABLE :: l_quant(:)    ! 
    INTEGER                          :: i, nq, ia, ie ! 
    !
    SELECT CASE ( get_h_grid_variant_no( this ) )
    CASE (1,2,3,4,5,6) ! GITTER05,GITTER05,UNTRIM_BAW,UNTRIM_VC,SELAFIN,DELFT3D
       nq = SUM( get_max_omi_quant( c_code(:) ) )
       ALLOCATE( l_quant(nq) )
       IF ( no_error( ) ) CALL new_omi_quant ( l_quant(:) )
       ia = 1 ; ie = 0
       DO i=1,SIZE(c_code)
          IF ( any_error( ) ) EXIT
          ie = ie + get_max_omi_quant( c_code(i) )
          CALL set_omi_quant ( l_quant(ia:ie), c_code(i) )
          ia = ie + 1
       END DO
       IF ( no_error( ) ) CALL setup_quant_object ( this, l_quant(:) )
       IF ( no_error( ) ) CALL kill_omi_quant( l_quant(:) )
       IF ( ALLOCATED( l_quant ) ) DEALLOCATE( l_quant )
    CASE DEFAULT
       WRITE(ch,'(I10)') get_h_grid_variant_no( this )
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<omi-component>', 'quant(:)' )
       CALL setup_error_act ( '<no>', TRIM(ch) )
       CALL setup_error_act ( '<req-no>', '[1,2,3,4,5,6]' )
    END SELECT
    !
  END SUBROUTINE create_omi_quant_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> Koordinaten-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen Koordinaten-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_xyz_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this !
    !! Name des Unterprogramms
    CHARACTER (LEN=17) , PARAMETER :: c_upname='create_omi_xyz_w0' ! 
    !! Konstante
    INTEGER            , PARAMETER :: c_max=2 ! 
    CHARACTER (LEN=03) , PARAMETER :: c_id(c_max) = (/ 'xy ', 'xyz' /) ! 
    CHARACTER (LEN=27) , PARAMETER :: c_descr(c_max) = & ! 
         (/ 'xy-coordinates, no depth   ', 'xyz-coordinates, with depth' /) ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10)           :: ch                 ! 
    TYPE (t_omi_space)           :: space              ! 
    TYPE (t_omi_xyz)             :: xyz(c_max)         ! 
    REAL (KIND=Double) , POINTER :: p_xy(:,:), p_hv(:) ! 
    INTEGER                      :: i                  ! 
    !
    SELECT CASE ( get_h_grid_variant_no( this ) )
    CASE (1,2,3,4,5,6) ! GITTER05,GITTER05,UNTRIM_BAW,UNTRIM_VC,SELAFIN,DELFT3D
       ! [1.1] Beschreibung des Koordinatensystems erzeugen
       CALL new_omi_space    ( space ) 
       CALL set_omi_space_id ( space, 'Cartesian_Projected[Easting,Northing]Vertical[Depth]' )
       ! [1.2] Koordinatenobjekte erzeugen
       ! [1.2.1] erforderliche Daten beschaffen
       p_xy => get_xy_object( this )
       p_hv => get_hv_object( this )
       IF ( .NOT. ASSOCIATED( p_hv ) ) THEN
          CALL derive_hv ( this )
          p_hv => get_hv_object( this )
       END IF
       ! [1.2.2] Koordinatenobjekt erzeugen
       DO i=1,SIZE(xyz)
          IF ( any_error( ) ) EXIT
          CALL new_omi_xyz              ( xyz(i) )
          CALL set_omi_xyz_id           ( xyz(i), c_id(i) )
          CALL set_omi_xyz_description  ( xyz(i), c_descr(i) )
          CALL set_omi_xyz_refsystem    ( xyz(i), space )
          CALL set_omi_xyz_ztype        ( xyz(i), 0 )
          CALL set_omi_xyz_ctype        ( xyz(i), 0 )
          CALL set_omi_xyz_hlimit       ( xyz(i), 0.0_Double )
          SELECT CASE ( i )
          CASE ( 1 ) ! xy, p_hv(:) wird in "bini" memoriert
             CALL create_omi_xyz_no_layers ( xyz(i), p_xy(:,1), p_xy(:,2), bottom=p_hv(:) )
          CASE ( 2 ) ! xyz
             CALL create_omi_xyz_no_layers ( xyz(i), p_xy(:,1), p_xy(:,2), p_hv(:) )
          END SELECT
       END DO
       CALL setup_xyz_object ( this, xyz(:) )
       CALL kill_omi_space ( space )
       CALL kill_omi_xyz   ( xyz   )
       NULLIFY( p_xy, p_hv )
    CASE DEFAULT
       WRITE(ch,'(I10)') get_h_grid_variant_no( this )
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<omi-component>', 'xyz(:)' )
       CALL setup_error_act ( '<no>', TRIM(ch) )
       CALL setup_error_act ( '<req-no>', '[1,2,3,4,5,6]' )
    END SELECT
    !
  END SUBROUTINE create_omi_xyz_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> Verweisliste-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen Verweislisten-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_ind_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this !
    !! Name des Unterprogramms
    CHARACTER (LEN=17) , PARAMETER :: c_upname='create_omi_ind_w0' ! 
    !! maximale Anzahl der vordefinierten Texte f&uuml;r "t_omi_ind" -----
    INTEGER            , PARAMETER :: c_max_omi_ind=17 ! 
    !! Id's der Objekte vom Typ "t_omi_ind" 
    CHARACTER (LEN=19) , PARAMETER :: c_omi_ind_id(c_max_omi_ind) = (/ & ! 
         'AllVertices        ', & ! [ 1] als Vertices
         'HorizontalGrid     ', & ! [ 2] als Polygons
         'AllBoundaries      ', & ! [ 3] als Vertices
         'AllClosedBoundaries', & ! [ 4] als Vertices
         'AllOpenBoundaries  ', & ! [ 5] als Vertices
         'AllBoundaries      ', & ! [ 6] als Polylines
         'AllClosedBoundaries', & ! [ 7] als Polylines
         'AllOpenBoundaries  ', & ! [ 8] als Polylines
         'Boundary?????      ', & ! [ 9] als Vertices
         'Boundary?????      ', & ! [10] als Polylines
         'Boundary?????      ', & ! [11] als Lines    
         'ClosedBoundary?????', & ! [12] als Vertices
         'OpenBoundary?????  ', & ! [13] als Vertices
         'ClosedBoundary?????', & ! [14] als Polylines
         'OpenBoundary?????  ', & ! [15] als Polylines
         'ClosedBoundary?????', & ! [16] als Lines     
         'OpenBoundary?????  ' /) ! [17] als Lines     
    !! Beschreibungen der Objekte vom Typ "t_omi_ind" 
    CHARACTER (LEN=57) , PARAMETER :: c_omi_ind_descr(c_max_omi_ind) = (/ & ! 
         !1234567890 234567890 234567890 234567890 234567890 234567
         'vertices of a horizontal grid                            ', & ! [ 1]
         'unstructured horizontal grid                             ', & ! [ 2]
         'all boundary vertices of a horizontal grid               ', & ! [ 3]
         'all closed boundary vertices of a horizontal grid        ', & ! [ 4]
         'all open boundary vertices of a horizontal grid          ', & ! [ 5]
         'all boundary polylines of a horizontal grid              ', & ! [ 6]
         'all closed boundary polylines of a horizontal grid       ', & ! [ 7]
         'all open boundary polylines of a horizontal grid         ', & ! [ 8]
         'vertices of boundary ????? of a horizontal grid          ', & ! [ 9]
         'polyline for boundary ????? of a horizontal grid         ', & ! [10]
         'lines for boundary ????? of a horizontal grid            ', & ! [11]
         'vertices of closed boundary ????? of a horizontal grid   ', & ! [12]
         'vertices of open boundary ????? of a horizontal grid     ', & ! [13]
         'polyline for closed boundary ????? of a horizontal grid  ', & ! [14]
         'polyline for open boundary ????? of a horizontal grid    ', & ! [15]
         'lines for closed boundary ????? of a horizontal grid     ', & ! [16]
         'lines for open boundary ????? of a horizontal grid       ' /) ! [17]
    !! Multiplkikator f&uuml;r ind_ms, ind_cl
    INTEGER , PARAMETER :: f_ms=3, f_co=3 ! x
    !
    !! Hilfsvariablen
    CHARACTER (LEN=10)                   :: ch               !   
    CHARACTER (LEN=LEN(c_omi_ind_id))    :: ch_omi_ind_id    ! 
    CHARACTER (LEN=LEN(c_omi_ind_descr)) :: ch_omi_ind_descr ! 
    TYPE (t_omi_ind)       , ALLOCATABLE :: ind(:)                                !        
    INTEGER               :: ind_n, ind_m, ind_b, ind_ms, ind_cl, ind_op, ind_co  !  
    INTEGER               :: i, j, k, n, ia, ie, ka, ke, nsl, nms, nss, nval      !  
    INTEGER , POINTER     :: p_nv, p_ne, p_ks(:), p_nen(:,:), p_b_ms(:), p_b_v(:) ! 
    INTEGER , ALLOCATABLE :: l_stru_start(:), l_stru_len(:), l_stru_list(:)       ! 
    INTEGER , POINTER     :: p_ss_len(:), p_ss_start(:), p_vv_list(:)             ! 
    CHARACTER (LEN=c_len_omi_ind_id) , ALLOCATABLE :: l_stru_id(:)                ! 
    LOGICAL , ALLOCATABLE :: l_ind(:) ! 
    TYPE (t_file)         :: l_file ! 
    !
    SELECT CASE ( get_h_grid_variant_no( this ) )
    CASE (1,2,3,4,5,6) ! GITTER05,GITTER05,UNTRIM_BAW,UNTRIM_VC,SELAFIN,DELFT3D
       ! [1.1] Verweislisten-Objekte erzeugen       
       ! [1.1.1] erforderliche Daten bereitstellen
       p_nv  => get_nv_object  ( this )
       p_ne  => get_ne_object  ( this )
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object  ( this )
       END IF
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_b_v => get_b_v_object ( this ) ! sortierte Knotennummern der Randabschnitte
       IF ( .NOT. ASSOCIATED( p_b_v ) ) THEN
          CALL derive_b_v ( this )
          p_b_v  => get_b_v_object ( this )
       END IF
       p_b_ms => get_b_ms_object ( this )
       IF ( .NOT. ASSOCIATED( p_b_ms ) ) THEN
          CALL derive_b_ms ( this )
          p_b_ms => get_b_ms_object ( this )
       END IF
       ind_n = 0
       ! Ermittle die Anzahl der (maximal) zu erzeugenden Index-Objekte -------------
       ind_b  = 8                    ! Basis-Elementsets
       ind_ms = MAXVAL( p_b_ms )     ! Einzel-ElementSets fuer die Hauptraender 
       ind_cl = get_ss_count(this,0) ! geschlossene zusammenhaengende Raender   
       ind_op = get_ss_count(this,1) ! offene zusammenhaengende Raender   
       ind_co = ind_cl + ind_op      ! 
       ind_m  = ind_b + f_ms*ind_ms + f_co*(ind_cl+ind_op) ! Summe aller ElementSets
       ALLOCATE ( ind(ind_m) )
       ! [1.1.2] Verweisliste-Objekte erzeugen
       DO i=1,SIZE(ind)
          IF ( any_error( ) ) EXIT
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          IF      ( i == 1 ) THEN ! AllVertices [ Vertices ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             ALLOCATE ( l_stru_start(p_nv), l_stru_len(p_nv), l_stru_list(p_nv) )
             DO j=1,p_nv
                l_stru_start(j) = j
                l_stru_len(j)   = 1
                l_stru_list(j)  = j
             END DO
             ind_n            = ind_n + 1
             ch_omi_ind_id    = c_omi_ind_id(i)
             ch_omi_ind_descr = c_omi_ind_descr(i)
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i == 2 ) THEN ! HorizontalGrid [ Polygons ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             ALLOCATE ( l_stru_start(p_ne), l_stru_len(p_ne), l_stru_list(SUM(p_ks)) )
             ia = 1 ; ie = 0
             DO j=1,p_ne
                l_stru_start(j)    = ia
                l_stru_len(j)      = p_ks(j)
                ie                 = ie + p_ks(j)
                l_stru_list(ia:ie) = p_nen(j,1:p_ks(j))
                ia                 = ie + 1
             END DO
             ind_n            = ind_n + 1
             ch_omi_ind_id    = c_omi_ind_id(i)
             ch_omi_ind_descr = c_omi_ind_descr(i)
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i == 3 ) THEN ! AllBoundaries [ Vertices ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             ALLOCATE ( l_ind(SIZE(p_b_v)) )
             l_ind(:) = get_int_diff_ind ( p_b_v )
             nsl = COUNT(l_ind)
             ALLOCATE ( l_stru_start(nsl), l_stru_len(nsl), l_stru_list(nsl) )
             n = 0
             DO j=1,SIZE(p_b_v)
                IF ( .NOT. l_ind(j) ) CYCLE
                n               = n + 1
                l_stru_start(n) = n
                l_stru_len(n)   = 1
                l_stru_list(n)  = p_b_v(j)
             END DO
             ind_n            = ind_n + 1
             ch_omi_ind_id    = c_omi_ind_id(i)
             ch_omi_ind_descr = c_omi_ind_descr(i)
             DEALLOCATE( l_ind )
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i == 4 .OR.  i == 5 ) THEN ! AllClosedBoundaries, AllOpenBoundaries [ Vertices ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             l_file = get_file_object ( this )
             nval   = i - 4
             SELECT CASE ( get_file_type( l_file ) )
             CASE ( 'SELAFIN' )
                CONTINUE ! no action
             CASE DEFAULT
                p_vv_list => get_vv_diff_list(this,nval)
                IF ( ASSOCIATED( p_vv_list ) ) THEN
                   ALLOCATE( l_ind(SIZE(p_vv_list)) )
                   l_ind(:) = get_int_diff_ind( p_vv_list )
                   nsl = COUNT( l_ind )
                   ALLOCATE ( l_stru_start(nsl),l_stru_len(nsl),l_stru_list(nsl) )
                   n = 0
                   DO j=1,SIZE(p_vv_list)
                      IF ( .NOT. l_ind(j) ) CYCLE
                      n               = n + 1
                      l_stru_start(n) = n
                      l_stru_len(n)   = 1
                      l_stru_list(n)  = p_vv_list(j)
                   END DO
                   DEALLOCATE( p_vv_list ) ; NULLIFY( p_vv_list )
                   ind_n            = ind_n + 1
                   ch_omi_ind_id    = c_omi_ind_id(i)
                   ch_omi_ind_descr = c_omi_ind_descr(i)
                   DEALLOCATE( l_ind )
                END IF
             END SELECT
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i == 6 ) THEN ! AllBoundaries [ Polylines ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             nms = MAXVAL( p_b_ms )
             nsl = SIZE( p_b_v )
             ia  = 1
             ie  = 0
             ka  = 1
             ke  = 0
             IF ( nms > 0 .AND. nsl > 0 ) THEN
                ALLOCATE ( l_stru_start(nms), l_stru_len(nms), l_stru_list(nsl+nms), l_stru_id(nms) )
                DO j=1,nms
                   l_stru_start(j) = ka
                   l_stru_len(j)   = COUNT( p_b_ms == j ) + 1
                   l_stru_id(j)    = REPEAT( ' ', LEN(l_stru_id(j)) )
                   SELECT CASE ( j )
                   CASE ( 1 )
                      l_stru_id(j) = 'external boundary polyline'
                   CASE DEFAULT
                      l_stru_id(j) = 'internal boundary polyline no. nnnnn'
                      WRITE(l_stru_id(j)(32:36),'(I5.5)') j-1
                   END SELECT
                   ke              = ke + l_stru_len(j)
                   ie              = ie + l_stru_len(j) - 1
                   DO k=1,l_stru_len(j)-1
                      l_stru_list(ka+k-1) = p_b_v(ia+k-1)
                   END DO
                   l_stru_list(ke) = l_stru_list(ka)
                   ka              = ke + 1 
                   ia              = ie + 1
                END DO
                ind_n = ind_n + 1
                ch_omi_ind_id    = c_omi_ind_id(i)
                ch_omi_ind_descr = c_omi_ind_descr(i)
             END IF
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i == 7 .OR. i == 8 ) THEN ! AllClosedBoundaries, AllOpenBoundaries [ Polylines ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             nval = i-7
             l_file = get_file_object ( this )
             SELECT CASE ( get_file_type( l_file ) )
             CASE ( 'SELAFIN' )
                CONTINUE ! no action
             CASE DEFAULT
                nss = get_ss_count(this,nval)
                nsl = get_vv_poly_count(this,nval)
                ka  = 1
                ke  = 0
                IF ( nss > 0 .AND. nsl > 0 ) THEN
                   ALLOCATE(l_stru_start(nss),l_stru_len(nss),l_stru_id(nss),l_stru_list(nsl))
                   DO j=1,nss
                      l_stru_start(j) = ka
                      l_stru_len(j)   = get_vv_poly_count(this,nval,j)
                      ke              = ke + l_stru_len(j)
                      l_stru_id(j)    = REPEAT( ' ', LEN(l_stru_id(j)) )
                      SELECT CASE ( nval )
                      CASE ( 0 )
                         l_stru_id(j) = 'closed boundary polyline no. nnnnn'
                         WRITE(l_stru_id(j)(30:34),'(I5.5)') j
                      CASE ( 1 )
                         l_stru_id(j) = 'open boundary polyline no. nnnnn'
                         WRITE(l_stru_id(j)(28:32),'(I5.5)') j
                      END SELECT
                      p_vv_list => get_vv_poly_list(this,nval,j)
                      DO k=1,l_stru_len(j)
                         l_stru_list(ka+k-1) = p_vv_list(k)
                      END DO
                      DEALLOCATE(p_vv_list) ; NULLIFY(p_vv_list)
                      ka = ke + 1
                   END DO
                   ind_n            = ind_n + 1
                   ch_omi_ind_id    = c_omi_ind_id(i)
                   ch_omi_ind_descr = c_omi_ind_descr(i)
                END IF
             END SELECT
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i > ind_b .AND. i <= ind_b + ind_ms ) THEN ! Boundary????? [ Vertices ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             nss = i-ind_b
             nsl = COUNT( p_b_ms == nss )
             IF ( COUNT( p_b_ms == nss ) > 0 ) THEN
                ALLOCATE( l_ind(SIZE(p_b_v)) )
                l_ind(:) = get_int_diff_ind( p_b_v )
                nsl = COUNT( l_ind .AND. p_b_ms == nss )
                ALLOCATE ( l_stru_start(nsl), l_stru_len(nsl), l_stru_list(nsl) )
                k = 0
                DO j=1,SIZE(p_b_ms)
                   IF ( .NOT. l_ind(j)   ) CYCLE
                   IF ( p_b_ms(j) /= nss ) CYCLE
                   k               = k + 1
                   l_stru_start(k) = k
                   l_stru_len(k)   = 1
                   l_stru_list(k)  = p_b_v(j)
                END DO
                ind_n            = ind_n + 1
                ch_omi_ind_id    = replace_question_mark ( c_omi_ind_id(9), nss )
                ch_omi_ind_descr = replace_question_mark ( c_omi_ind_descr(9), nss )
                DEALLOCATE( l_ind )
             END IF
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i > ind_b+ind_ms .AND. i <= ind_b + 2*ind_ms ) THEN ! Boundary????? [ Polylines ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             nss = i-ind_b-ind_ms
             nsl = COUNT( p_b_ms == nss ) + 1
             IF ( nsl > 0 ) THEN
                ALLOCATE ( l_stru_start(1), l_stru_len(1), l_stru_list(nsl), l_stru_id(1) )
                l_stru_start(1) = 1
                l_stru_len(1)   = nsl
                l_stru_id(1)    = REPEAT( ' ', LEN(l_stru_id(1)) )
                SELECT CASE ( nss )
                CASE ( 1 )
                   l_stru_id(1) = 'external boundary polyline'
                CASE DEFAULT
                   l_stru_id(1) = 'internal boundary polyline no. nnnnn'
                   WRITE(l_stru_id(1)(32:36),'(I5.5)') nss-1
                END SELECT
                ka = 1
                ke = 0
                DO j=1,nss-1
                   ke = ke + COUNT( p_b_ms == j )
                   ka = ke + 1
                END DO
                DO j=1,nsl-1
                   l_stru_list(j) = p_b_v(ka+j-1)
                END DO
                l_stru_list(nsl) = l_stru_list(1)
                ind_n            = ind_n + 1
                ch_omi_ind_id    = replace_question_mark ( c_omi_ind_id(10), nss )
                ch_omi_ind_descr = replace_question_mark ( c_omi_ind_descr(10), nss )
             END IF
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i > ind_b+2*ind_ms .AND. i <= ind_b + 3*ind_ms ) THEN ! Boundary????? [ Lines ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             nss = i-ind_b-2*ind_ms
             nsl = COUNT( p_b_ms == nss )
             IF ( nsl > 0 ) THEN
                ALLOCATE ( l_stru_start(nsl), l_stru_len(nsl), l_stru_list(2*nsl) )
                ka = 1
                ke = 0
                DO j=1,nss-1
                   ke = ke + COUNT( p_b_ms == j )
                   ka = ke + 1
                END DO
                DO j=1,nsl
                   l_stru_start(j)    = 2*j - 1
                   l_stru_len(j)      = 2
                   l_stru_list(2*j-1) = p_b_v(ka+j-1) 
                   IF ( j < nsl ) THEN
                      l_stru_list(2*j  ) = p_b_v(ka+j  )
                   ELSE
                      l_stru_list(2*j  ) = l_stru_list(1)
                   END IF
                END DO
                ind_n            = ind_n + 1
                ch_omi_ind_id    = replace_question_mark ( c_omi_ind_id(11), nss )
                ch_omi_ind_descr = replace_question_mark ( c_omi_ind_descr(11), nss )
             END IF
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i > ind_b+f_ms*ind_ms .AND. i <= ind_b+f_ms*ind_ms+ind_co ) THEN ! Closed|OpenBoundary????? [ Vertices ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             l_file = get_file_object ( this )
             SELECT CASE ( get_file_type( l_file ) )
             CASE ( 'SELAFIN' )
                CONTINUE ! no action
             CASE DEFAULT
                nval      = MERGE( 1, 0, i > ind_b + f_ms*ind_ms + ind_cl )
                nss       = i - ind_b - f_ms*ind_ms - nval*ind_cl
                p_vv_list => get_vv_diff_list(this,nval,nss)
                IF ( ASSOCIATED(p_vv_list) ) THEN
                   ALLOCATE ( l_ind(SIZE(p_vv_list)) )
                   l_ind(:) = get_int_diff_ind ( p_vv_list )
                   nsl      = COUNT( l_ind )
                   ALLOCATE ( l_stru_start(nsl), l_stru_len(nsl), l_stru_list(nsl) )
                   n = 0
                   DO j=1,SIZE(p_vv_list)
                      IF ( .NOT. l_ind(j) ) CYCLE
                      n               = n + 1
                      l_stru_start(n) = n
                      l_stru_len(n)   = 1
                      l_stru_list(n)  = p_vv_list(j)
                   END DO
                   DEALLOCATE(p_vv_list) ; NULLIFY(p_vv_list)
                   DEALLOCATE( l_ind )
                END IF
                ind_n            = ind_n + 1
                ch_omi_ind_id    = replace_question_mark ( c_omi_ind_id(12+nval), nss )
                ch_omi_ind_descr = replace_question_mark ( c_omi_ind_descr(12+nval), nss )
             END SELECT
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i > ind_b+f_ms*ind_ms+ind_co .AND. i <= ind_b+f_ms*ind_ms+2*ind_co ) THEN ! Closed|OpenBoundary????? [ Polyline ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             l_file = get_file_object ( this )
             SELECT CASE ( get_file_type( l_file ) )
             CASE ( 'SELAFIN' )
                CONTINUE ! no action
             CASE DEFAULT
                nval      = MERGE( 1, 0, i > ind_b + f_ms*ind_ms + ind_co + ind_cl )
                nss       = i - ind_b - f_ms*ind_ms - ind_co - nval*ind_cl
                p_vv_list => get_vv_poly_list(this,nval,nss)
                IF ( ASSOCIATED(p_vv_list) ) THEN
                   nsl = SIZE(p_vv_list)
                   ALLOCATE ( l_stru_start(1), l_stru_len(1), l_stru_list(nsl), l_stru_id(1) )
                   l_stru_start(1) = 1
                   l_stru_len(1)   = nsl
                   l_stru_id(1)    = REPEAT( ' ', LEN(l_stru_id) )
                   SELECT CASE ( nval )
                   CASE ( 0 )
                      l_stru_id(1)    = 'closed boundary polyline no. nnnnn'
                      WRITE(l_stru_id(1)(30:34),'(I5.5)') nss
                   CASE ( 1 )
                      l_stru_id(1)    = 'open boundary polyline no. nnnnn'
                      WRITE(l_stru_id(1)(28:32),'(I5.5)') nss
                   END SELECT
                   DO j=1,nsl
                      l_stru_list(j)  = p_vv_list(j)
                   END DO
                   DEALLOCATE(p_vv_list) ; NULLIFY(p_vv_list)
                END IF
                ind_n            = ind_n + 1
                ch_omi_ind_id    = replace_question_mark ( c_omi_ind_id(14+nval), nss )
                ch_omi_ind_descr = replace_question_mark ( c_omi_ind_descr(14+nval), nss )
             END SELECT
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF ( i > ind_b+f_ms*ind_ms+2*ind_co .AND. i <= ind_b+f_ms*ind_ms+3*ind_co ) THEN ! Closed|OpenBoundary????? [ Lines ]
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             l_file = get_file_object ( this )
             SELECT CASE ( get_file_type( l_file ) )
             CASE ( 'SELAFIN' )
                CONTINUE ! no action
             CASE DEFAULT
                nval      = MERGE( 1, 0, i > ind_b + f_ms*ind_ms + 2*ind_co + ind_cl )
                nss       = i - ind_b - f_ms*ind_ms - 2*ind_co - nval*ind_cl
                p_vv_list => get_vv_poly_list(this,nval,nss)
                IF ( ASSOCIATED(p_vv_list) ) THEN
                   nsl = SIZE(p_vv_list) - 1
                   ALLOCATE ( l_stru_start(nsl), l_stru_len(nsl), l_stru_list(2*nsl) )
                   DO j=1,nsl
                      l_stru_start(j)    = 2*j - 1
                      l_stru_len(j)      = 2
                      l_stru_list(2*j-1) = p_vv_list(j  )
                      l_stru_list(2*j  ) = p_vv_list(j+1)
                   END DO
                   DEALLOCATE(p_vv_list) ; NULLIFY(p_vv_list)
                END IF
                ind_n            = ind_n + 1
                ch_omi_ind_id    = replace_question_mark ( c_omi_ind_id(16+nval), nss )
                ch_omi_ind_descr = replace_question_mark ( c_omi_ind_descr(16+nval), nss )
             END SELECT
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          END IF
          IF ( ALLOCATED( l_stru_start ) ) THEN
             CALL new_omi_ind             ( ind(ind_n) )
             CALL set_omi_ind_id          ( ind(ind_n), ch_omi_ind_id )
             CALL set_omi_ind_description ( ind(ind_n), ch_omi_ind_descr )
             CALL set_omi_ind_stru_start  ( ind(ind_n), l_stru_start(:) )
             CALL set_omi_ind_stru_len    ( ind(ind_n), l_stru_len(:)   )
             CALL set_omi_ind_stru_list   ( ind(ind_n), l_stru_list(:)  )
             IF ( ALLOCATED( l_stru_id    ) ) CALL set_omi_ind_stru_id ( ind(ind_n), l_stru_id(:) )
        END IF
          IF ( ALLOCATED( l_stru_start ) ) DEALLOCATE( l_stru_start )
          IF ( ALLOCATED( l_stru_len   ) ) DEALLOCATE( l_stru_len   )
          IF ( ALLOCATED( l_stru_list  ) ) DEALLOCATE( l_stru_list  )
          IF ( ALLOCATED( l_stru_id    ) ) DEALLOCATE( l_stru_id    )
       END DO
       CALL setup_ind_object ( this, ind(1:ind_n) )
       CALL kill_omi_ind     ( ind(1:ind_n)       )
       DEALLOCATE ( ind )
       NULLIFY( p_nv, p_ks, p_nen, p_b_v, p_b_ms )
    CASE DEFAULT
       WRITE(ch,'(I10)') get_h_grid_variant_no( this )
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<omi-component>', 'ind(:)' )
       CALL setup_error_act ( '<no>', TRIM(ch) )
       CALL setup_error_act ( '<req-no>', '[1,2,3,4,5,6]' )
    END SELECT
    !
  END SUBROUTINE create_omi_ind_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> ElementSets-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen Koordinaten-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_ele_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this !
    !! Name des Unterprogramms
    CHARACTER (LEN=17) , PARAMETER :: c_upname='create_omi_ele_w0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10)    :: ch            ! 
    INTEGER               :: i, j, ne, nn  ! 
    TYPE (t_omi_xyz) , POINTER :: p_xyz(:), s_xyz ! 
    TYPE (t_omi_ind) , POINTER :: p_ind(:), s_ind !
    TYPE (t_omi_ele) , ALLOCATABLE :: l_ele(:) ! 
    !
    SELECT CASE ( get_h_grid_variant_no( this ) )
    CASE (1,2,3,4,5,6) ! GITTER05,GITTER05,UNTRIM_BAW,UNTRIM_VC,SELAFIN,DELFT3D
       ! [1.1] ElementSet-Objekte erzeugen
       ! [1.1.1] erforderliche Daten bereitstellen
       p_xyz => get_xyz_object ( this )
       p_ind => get_ind_object ( this )
       ! [1.1.2] Daten transferieren
       IF ( ASSOCIATED( p_xyz ) .AND. ASSOCIATED( p_ind ) ) THEN
          ne = SIZE(p_xyz)*SIZE(p_ind)
          nn = 0
          ALLOCATE ( l_ele(ne) )
          DO i=1,SIZE(p_xyz)
             s_xyz => p_xyz(i)
             DO j=1,SIZE(p_ind)
                nn = nn + 1
                s_ind => p_ind(j)
                CALL set_omi_ele_auto ( l_ele(nn), s_xyz, s_ind, TRIM(this%name) )
             END DO
          END DO
          CALL setup_ele_object ( this, l_ele(:) )
          CALL kill_omi_ele ( l_ele(:) )
          DEALLOCATE ( l_ele )
       END IF
       NULLIFY ( p_xyz, s_xyz, p_ind, s_ind )
    CASE DEFAULT
       WRITE(ch,'(I10)') get_h_grid_variant_no( this )
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<omi-component>', 'ele(:)' )
       CALL setup_error_act ( '<no>', TRIM(ch) )
       CALL setup_error_act ( '<req-no>', '[1,2,3,4,5,6]' )
    END SELECT
    !
  END SUBROUTINE create_omi_ele_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> DataOperations-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen DataOperations-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_dope_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this !
    !! Name des Unterprogramms
    CHARACTER (LEN=18) , PARAMETER :: c_upname='create_omi_dope_w0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch                       ! 
    TYPE (t_omi_dope)  :: dope(c_max_omi_dope_idx) ! 
    REAL (KIND=Double) :: m_d(1)                   ! 
    REAL (KIND=Double) , POINTER :: p_hland        ! 
    INTEGER            :: i                        ! 
    !
    SELECT CASE ( get_h_grid_variant_no( this ) )
    CASE (1,2,3,4,5,6) ! GITTER05,GITTER05,UNTRIM_BAW,UNTRIM_VC,SELAFIN,DELFT3D
       CALL new_omi_dope ( dope(:) )
       DO i=1,SIZE(dope)
          IF ( i == 1 ) THEN
             p_hland => get_hland_object ( this )
             IF ( ASSOCIATED( p_hland ) ) THEN
                m_d(:) = p_hland
                NULLIFY( p_hland )
             ELSE
                m_d(:) = c_missing_h_grid_double
             END IF
             CALL set_omi_dope_auto( dope(i), c_omi_dope_idx(i), m_d(:) )
          ELSE
             CALL set_omi_dope_auto( dope(i), c_omi_dope_idx(i) )
          END IF
       END DO
       CALL setup_dope_object ( this, dope(:) )
       CALL kill_omi_dope ( dope(:) )
    CASE DEFAULT
       WRITE(ch,'(I10)') get_h_grid_variant_no( this )
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<omi-component>', 'dope(:)' )
       CALL setup_error_act ( '<no>', TRIM(ch) )
       CALL setup_error_act ( '<req-no>', '[1,2,3,4,5,6]' )
    END SELECT
    !
  END SUBROUTINE create_omi_dope_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> ExchangeItems-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen ExchangeItems-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_exch_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this !
    !! Name des Unterprogramms
    CHARACTER (LEN=18) , PARAMETER :: c_upname='create_omi_exch_w0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10)    :: ch                    ! 
    TYPE (t_omi_exch)  , ALLOCATABLE :: l_exch(:)  ! 
    TYPE (t_omi_quant) , POINTER     :: p_quant(:), s_quant ! 
    TYPE (t_omi_ele)   , POINTER     :: p_ele(:), s_ele ! 
    TYPE (t_omi_dope)  , POINTER     :: p_dope(:)  ! 
    INTEGER :: i, j, nx, nq, ne, nd, nn ! 
    !
    SELECT CASE ( get_h_grid_variant_no( this ) )
    CASE (1,2,3,4,5,6) ! GITTER05,GITTER05,UNTRIM_BAW,UNTRIM_VC,SELAFIN,DELFT3D
       p_quant => get_quant_object ( this )
       p_ele   => get_ele_object   ( this )
       p_dope  => get_dope_object  ( this )
       nq = MERGE( SIZE( p_quant ) , 0, ASSOCIATED( p_quant ) )
       ne = MERGE( SIZE( p_ele   ) , 0, ASSOCIATED( p_ele   ) )
       nd = MERGE( SIZE( p_dope  ) , 0, ASSOCIATED( p_dope  ) )
       nx = nq*ne
       IF ( nx > 0 ) THEN
          ALLOCATE( l_exch(nx) )
          CALL new_omi_exch  ( l_exch(:) )
          nn = 0
          DO i=1,ne
             s_ele => p_ele(i)
             DO j=1,nq
                nn = nn + 1
                s_quant => p_quant(j)
                CALL set_omi_exch_quant_ref ( l_exch(nn), s_quant )
                CALL set_omi_exch_ele_ref   ( l_exch(nn), s_ele   )
                CALL set_omi_exch_dope_ref  ( l_exch(nn), p_dope  )
                CALL set_omi_exch_role      ( l_exch(nn), 1       ) ! Export
             END DO
          END DO
          CALL setup_exch_object ( this, l_exch(:) )
          CALL kill_omi_exch ( l_exch(:) )
          DEALLOCATE( l_exch )
       END IF
       NULLIFY ( p_quant, p_ele, p_dope )
    CASE DEFAULT
       WRITE(ch,'(I10)') get_h_grid_variant_no( this )
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<omi-component>', 'exch(:)' )
       CALL setup_error_act ( '<no>', TRIM(ch) )
       CALL setup_error_act ( '<req-no>', '[1,2,3,4,5,6]' )
    END SELECT
    !
  END SUBROUTINE create_omi_exch_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> Zeitangaben-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen Zeitangaben-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_stamp_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this !
    !! Name des Unterprogramms
    CHARACTER (LEN=19) , PARAMETER :: c_upname='create_omi_stamp_w0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10)    :: ch               ! 
    !
    SELECT CASE ( get_h_grid_variant_no( this ) )
    CASE (1,2,3,4,5,6) ! GITTER05,GITTER05,UNTRIM_BAW,UNTRIM_VC,SELAFIN,DELFT3D
       CONTINUE ! aktuell wird keine Zeitangabe erzeugt
    CASE DEFAULT
       WRITE(ch,'(I10)') get_h_grid_variant_no( this )
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<omi-component>', 'stamp' )
       CALL setup_error_act ( '<no>', TRIM(ch) )
       CALL setup_error_act ( '<req-no>', '[1,2,3,4,5,6]' )
    END SELECT
    !
  END SUBROUTINE create_omi_stamp_w0
  !
  !! Hilfsfunktion zum ermitteln der Anzahl der Unterabschnitte mit 
  !! vorgegebener Kantenkennung (offen oder geschlossen)
  FUNCTION get_ss_count_w0 ( this, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER      , INTENT(IN) :: val ! 
    !! Ergebnis : Anzahl unterschiedlicher Randabschnitte
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER , POINTER :: p_b_ss(:), p_b_t(:) ! 
    INTEGER           :: i, mss              ! 
    !
    res = 0
    p_b_ss => get_b_ss_object ( this )
    IF ( .NOT. ASSOCIATED(p_b_ss) ) THEN
       CALL derive_b_ss ( this )
       p_b_ss => get_b_ss_object ( this )
    END IF
    p_b_t  => get_b_t_object  ( this )
    IF ( .NOT. ASSOCIATED(p_b_t) ) THEN
       CALL derive_b_t ( this )
       p_b_t => get_b_t_object ( this )
    END IF
    IF ( ASSOCIATED(p_b_ss) .AND. ASSOCIATED(p_b_t) ) THEN
       mss = 0
       DO i=1,SIZE(p_b_t)
          IF ( p_b_t(i) == val ) THEN
             IF ( p_b_ss(i) /= mss ) THEN
                res = res + 1
                mss = p_b_ss(i)
             END IF
          END IF
       END DO
    END IF
    NULLIFY(p_b_ss,p_b_t)
    !
  END FUNCTION get_ss_count_w0 
  !
  !! Hilfsfunktion zum ermitteln der Anzahl der (Haupt-) Randabschnitte
  FUNCTION get_ms_count_w0 ( this ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Ergebnis : Anzahl unterschiedlicher Randabschnitte
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER , POINTER :: p_b_ms(:) ! 
    !
    res = 0
    p_b_ms => get_b_ms_object ( this )
    IF ( .NOT. ASSOCIATED(p_b_ms) ) THEN
       CALL derive_b_ms ( this )
       p_b_ms => get_b_ms_object ( this )
    END IF
    !
    IF ( ASSOCIATED( p_b_ms ) ) THEN
       res = MAXVAL( p_b_ms )
       DEALLOCATE ( p_b_ms ) ; NULLIFY( p_b_ms )
    END IF
    !
  END FUNCTION get_ms_count_w0 
  !
  !! Hilfsfunktion zum ermitteln der Anzahl der in den verschiedenen
  !! Unterabschnitten (offen oder geschlossen) liegenden Kanten
  FUNCTION get_ss_len_w0 ( this, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER      , INTENT(IN) :: val ! 
    !! Ergebnis : Anzahl der Kanten in den verschiedenen Randabschnitten
    INTEGER , POINTER :: res(:) ! 
    !! Hilfsvariablen
    INTEGER , POINTER :: p_b_ss(:), p_b_t(:) ! 
    INTEGER           :: i, n, mss           ! 
    !
    NULLIFY(res)
    IF ( get_ss_count(this,val) > 0 ) THEN
       ALLOCATE( res(get_ss_count(this,val)) )
       res(:) = 0
       p_b_ss => get_b_ss_object ( this )
       p_b_t  => get_b_t_object ( this )
       IF ( ASSOCIATED(p_b_ss) .AND. ASSOCIATED(p_b_t) ) THEN
          mss = 0
          n   = 0
          DO i=1,SIZE(p_b_t)
             IF ( p_b_t(i) == val ) THEN
                IF ( p_b_ss(i) == mss ) THEN
                   res(n) = res(n) + 1
                ELSE
                   n      = n + 1
                   res(n) = res(n) + 1
                   mss    = p_b_ss(i)
                END IF
             END IF
          END DO
       END IF
    END IF
    !
  END FUNCTION get_ss_len_w0
  !
  !! Hilfsfunktion zum ermitteln der Startpositionen der Kanten 
  !! in den verschiedenen Unterabschnitten 
  FUNCTION get_ss_start_w0 ( this, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER      , INTENT(IN) :: val ! 
    !! Ergebnis : Startpositionen der Kanten in den verschiedenen Randabschnitten
    INTEGER , POINTER :: res(:) ! 
    !! Hilfsvariablen
    INTEGER , POINTER :: p_b_ss(:), p_b_t(:) ! 
    INTEGER           :: i, n, mss           ! 
    !
    NULLIFY(res)
    IF ( get_ss_count(this,val) > 0 ) THEN
       ALLOCATE( res(get_ss_count(this,val)) )
       res(:) = 0
       p_b_ss => get_b_ss_object ( this )
       p_b_t  => get_b_t_object ( this )
       IF ( ASSOCIATED(p_b_ss) .AND. ASSOCIATED(p_b_t) ) THEN
          mss = 0
          n   = 0
          DO i=1,SIZE(p_b_t)
             IF ( p_b_t(i) == val ) THEN
                IF ( p_b_ss(i) /= mss ) THEN
                   n      = n + 1
                   res(n) = i
                   mss    = p_b_ss(i)
                END IF
             END IF
          END DO
       END IF
    END IF
    !
  END FUNCTION get_ss_start_w0
  !
  !! Ermittle die Anzahl der unterschiedlichen Knoten in <EM>einem</EM> Randabschnitt
  !! mit vorgegebener Kennung und vorgegebener lfd. Nummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vv_diff_count_w000 ( this, val, lfd ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER , INTENT(IN) :: val ! 
    !! lfd. Nummer des geschlossenen / offenen Randabschnitts
    INTEGER , INTENT(IN) :: lfd ! 
    !! Ergebnis : Anzahl der unterschiedlichen Knoten
    INTEGER              :: res ! 
    !! Hilfsvariablen
    INTEGER , POINTER :: l_ss_len(:)   ! 
    INTEGER , POINTER :: l_ss_start(:) ! 
    INTEGER , POINTER :: l_b_s(:)      ! 
    INTEGER , POINTER :: l_b_v(:)      ! 
    INTEGER , POINTER :: l_jb(:)       ! 
    INTEGER , POINTER :: l_jt(:)       ! 
    INTEGER           :: nf, my_sl        ! 
    !
    res = 0
    IF ( get_ss_count( this, val ) > 0 ) THEN
       l_ss_len   => get_ss_len  ( this, val )
       l_ss_start => get_ss_start( this, val )
       l_b_s      => get_b_s_object ( this )
       l_b_v      => get_b_v_object ( this )  
       l_jb       => get_jb_object  ( this )
       l_jt       => get_jt_object  ( this )
       IF ( ASSOCIATED(l_ss_len) .AND. ASSOCIATED(l_ss_start) .AND. ASSOCIATED(l_b_s) .AND. & ! 
              ASSOCIATED(l_b_v)  .AND. ASSOCIATED(l_jb)       .AND. ASSOCIATED(l_jt)  .AND. & ! 
              lfd >= 1           .AND. lfd <= SIZE(l_ss_len)                                  ) THEN
          !
          res = l_ss_len(lfd)
          nf  = l_b_v(l_ss_start(lfd))
          my_sl  = l_b_s(l_ss_start(lfd)+l_ss_len(lfd)-1)
          IF ( l_jb(my_sl) /= nf .AND. l_jt(my_sl) /= nf ) res = res + 1
       END IF
       IF ( ASSOCIATED( l_ss_len   ) ) DEALLOCATE( l_ss_len   ) ; NULLIFY( l_ss_len   )
       IF ( ASSOCIATED( l_ss_start ) ) DEALLOCATE( l_ss_start ) ; NULLIFY( l_ss_start )
       IF ( ASSOCIATED( l_b_s      ) ) NULLIFY( l_b_s )
       IF ( ASSOCIATED( l_b_v      ) ) NULLIFY( l_b_v )
       IF ( ASSOCIATED( l_jb       ) ) NULLIFY( l_jt  )
    END IF
    !
  END FUNCTION get_vv_diff_count_w000
  !
  !! Ermittle die Anzahl der unterschiedlichen Knoten in <EM>allen</EM> Randabschnitten
  !! mit vorgegebener Kennung <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vv_diff_count_w00 ( this, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER , INTENT(IN) :: val ! 
    !! Ergebnis : Anzahl der unterschiedlichen Knoten
    INTEGER              :: res ! 
    !! Hilfsvariablen
    INTEGER              :: i   ! 
    !
    res = 0
    DO i=1,get_ss_count( this, val )
       res = res + get_vv_diff_count ( this, val, i )
    END DO
    !
  END FUNCTION get_vv_diff_count_w00
  !
  !! Ermittle die Liste der unterschiedlichen Knoten in <EM>einem</EM> Randabschnitt
  !! mit vorgegebener Kennung und vorgegebener lfd. Nummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vv_diff_list_w000 ( this, val, lfd ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER , INTENT(IN) :: val ! 
    !! lfd. Nummer des geschlossenen / offenen Randabschnitts
    INTEGER , INTENT(IN) :: lfd ! 
    !! Ergebnis : Liste der unterschiedlichen Knoten
    INTEGER , POINTER    :: res(:) ! 
    !! Hilfsvariablen
    INTEGER           :: i             ! 
    INTEGER , POINTER :: l_b_s(:)      ! 
    INTEGER , POINTER :: l_b_v(:)      ! 
    INTEGER , POINTER :: l_jb(:)       ! 
    INTEGER , POINTER :: l_jt(:)       ! 
    INTEGER , POINTER :: l_ss_len(:)   ! 
    INTEGER , POINTER :: l_ss_start(:) ! 
    INTEGER           :: nf, my_sl        ! 
    !
    NULLIFY ( res )
    IF ( get_vv_diff_count( this, val, lfd ) > 0 ) THEN
       l_ss_len   => get_ss_len     ( this, val )
       l_ss_start => get_ss_start   ( this, val )
       l_b_s      => get_b_s_object ( this )
       l_b_v      => get_b_v_object ( this )
       l_jb       => get_jb_object  ( this )
       l_jt       => get_jt_object  ( this )
       IF ( ASSOCIATED(l_ss_len) .AND. ASSOCIATED(l_ss_start) .AND. ASSOCIATED(l_b_s) .AND. & ! 
              ASSOCIATED(l_b_v)  .AND. ASSOCIATED(l_jb)       .AND. ASSOCIATED(l_jt)  .AND. & ! 
              lfd >= 1           .AND. lfd <= SIZE(l_ss_len)                                  ) THEN
          !
          ALLOCATE( res(get_vv_diff_count(this,val,lfd)) )
          DO i=1,l_ss_len(lfd)
             res(i) = l_b_v(l_ss_start(lfd)+i-1)
          END DO
          IF ( SIZE(res) > l_ss_len(lfd) ) THEN
             nf = l_b_v(l_ss_start(lfd))
             my_sl = l_b_s(l_ss_start(lfd)+l_ss_len(lfd)-1)
             res(l_ss_len(lfd)+1) = MERGE( l_jb(my_sl), l_jt(my_sl), l_jt(my_sl) == res(l_ss_len(lfd)) )
          END IF
          !
       END IF
       IF ( ASSOCIATED( l_ss_len   ) ) DEALLOCATE( l_ss_len   ) ; NULLIFY( l_ss_len   )
       IF ( ASSOCIATED( l_ss_start ) ) DEALLOCATE( l_ss_start ) ; NULLIFY( l_ss_start )
       IF ( ASSOCIATED( l_b_s      ) ) NULLIFY( l_b_s )
       IF ( ASSOCIATED( l_b_v      ) ) NULLIFY( l_b_v )
       IF ( ASSOCIATED( l_jb       ) ) NULLIFY( l_jb  )
       IF ( ASSOCIATED( l_jt       ) ) NULLIFY( l_jt  )
    END IF
    !
  END FUNCTION get_vv_diff_list_w000
  !
  !! Ermittle die Anzahl der unterschiedlichen Knoten in <EM>allen</EM> Randabschnitten
  !! mit vorgegebener Kennung <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vv_diff_list_w00 ( this, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER , INTENT(IN) :: val       ! 
    !! Ergebnis : alle unterschiedlichen Knoten
    INTEGER , POINTER    :: res(:)    ! 
    !! Hilfsvariablen
    INTEGER              :: i, ia, ie, j ! 
    INTEGER , POINTER    :: l_res(:)  ! 
    !
    NULLIFY( res )
    IF ( get_vv_diff_count(this,val) > 0 ) THEN
       ALLOCATE( res(get_vv_diff_count(this,val)) )
       ia = 1
       ie = 0
       DO i=1,get_ss_count(this,val)
          l_res => get_vv_diff_list (this,val,i)
          IF ( ASSOCIATED(l_res) ) THEN
             ie         = ie + SIZE(l_res)
             res(ia:ie) = l_res(:)
             DEALLOCATE( l_res ) ; NULLIFY( l_res )
             ia = ie + 1
          END IF
       END DO
    END IF
    !
  END FUNCTION get_vv_diff_list_w00
  !
  !! Ermittle die Anzahl der Knoten in <EM>einem</EM> als Polygonzug 
  !! darzustellenden Randabschnitt mit vorgegebener Kennung und 
  !! vorgegebener lfd. Nummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vv_poly_count_w000 ( this, val, lfd ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER , INTENT(IN) :: val ! 
    !! lfd. Nummer des geschlossenen / offenen Randabschnitts
    INTEGER , INTENT(IN) :: lfd ! 
    !! Ergebnis : Anzahl der Knoten im Polygonzug
    INTEGER              :: res ! 
    !! Hilfsvariablen
    INTEGER , POINTER :: p_ss_len(:)   ! 
    !
    res = 0
    IF ( get_ss_count( this, val ) > 0 ) THEN
       p_ss_len => get_ss_len( this, val )
       IF ( ASSOCIATED( p_ss_len ) ) THEN
          IF ( lfd >= 1 .AND. lfd <= SIZE( p_ss_len ) ) res = p_ss_len(lfd) + 1
          DEALLOCATE( p_ss_len ) ; NULLIFY( p_ss_len )
       END IF
    END IF
    !
  END FUNCTION get_vv_poly_count_w000
  !
  !! Ermittle die Anzahl der Knoten in <EM>allen</EM> als Polygonzug 
  !! darzustellenden Randabschnitten Randabschnitten mit vorgegebener Kennung <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vv_poly_count_w00 ( this, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER , INTENT(IN) :: val ! 
    !! Ergebnis : Anzahl der Knoten
    INTEGER              :: res ! 
    !! Hilfsvariablen
    INTEGER              :: i   ! 
    !
    res = 0
    DO i=1,get_ss_count( this, val )
       res = res + get_vv_poly_count ( this, val, i )
    END DO
    !
  END FUNCTION get_vv_poly_count_w00
  !
  !! Ermittle die Liste der  Knoten in <EM>einem</EM> als Polygonzug darzustellendem 
  !! Randabschnitt bei vorgegebener Kennung und vorgegebener lfd. Nummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vv_poly_list_w000 ( this, val, lfd ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Kennung f&uuml;r Rand in Komponente "b_t(:)" <BR>
    !! geschlossen = 0 <BR>
    !! offen       = 1
    INTEGER , INTENT(IN) :: val ! 
    !! lfd. Nummer des geschlossenen / offenen Randabschnitts
    INTEGER , INTENT(IN) :: lfd ! 
    !! Ergebnis : Liste der unterschiedlichen Knoten
    INTEGER , POINTER    :: res(:) ! 
    !! Hilfsvariablen
    INTEGER           :: i             ! 
    INTEGER , POINTER :: l_b_s(:)      ! 
    INTEGER , POINTER :: l_b_v(:)      ! 
    INTEGER , POINTER :: l_jb(:)       ! 
    INTEGER , POINTER :: l_jt(:)       ! 
    INTEGER , POINTER :: l_ss_len(:)   ! 
    INTEGER , POINTER :: l_ss_start(:) ! 
    INTEGER           :: nl, my_sl        ! 
    !
    NULLIFY ( res )
    IF ( get_vv_poly_count( this, val, lfd ) > 0 ) THEN
       l_ss_len   => get_ss_len     ( this, val )
       l_ss_start => get_ss_start   ( this, val )
       l_b_s      => get_b_s_object ( this )
       l_b_v      => get_b_v_object ( this )
       l_jb       => get_jb_object  ( this )
       l_jt       => get_jt_object  ( this )
       IF ( ASSOCIATED(l_ss_len) .AND. ASSOCIATED(l_ss_start) .AND. ASSOCIATED(l_b_s) .AND. & ! 
              ASSOCIATED(l_b_v)  .AND. ASSOCIATED(l_jb)       .AND. ASSOCIATED(l_jt)  .AND. & ! 
              lfd >= 1           .AND. lfd <= SIZE(l_ss_len)                                  ) THEN
          !
          ALLOCATE( res(get_vv_poly_count(this,val,lfd)) )
          DO i=1,l_ss_len(lfd)
             res(i) = l_b_v(l_ss_start(lfd)+i-1)
          END DO
          my_sl = l_b_s(l_ss_start(lfd)+l_ss_len(lfd)-1)
          res(l_ss_len(lfd)+1) = MERGE( l_jb(my_sl), l_jt(my_sl), l_jt(my_sl) == res(l_ss_len(lfd)) )
          !
       END IF
       IF ( ASSOCIATED( l_ss_len   ) ) DEALLOCATE( l_ss_len   ) ; NULLIFY( l_ss_len   )
       IF ( ASSOCIATED( l_ss_start ) ) DEALLOCATE( l_ss_start ) ; NULLIFY( l_ss_start )
       IF ( ASSOCIATED( l_b_s      ) ) NULLIFY( l_b_s )
       IF ( ASSOCIATED( l_b_v      ) ) NULLIFY( l_b_v )
       IF ( ASSOCIATED( l_jb       ) ) NULLIFY( l_jb  )
       IF ( ASSOCIATED( l_jt       ) ) NULLIFY( l_jt  )
    END IF
    !
  END FUNCTION get_vv_poly_list_w000
  !
  !! Ersetze die Zeichen ??? durch eine Integer-Zahl, falls vorhanden <BR>
  !! Funktion erzeucgt <EM>keine</EM> Fehlermeldungen
  FUNCTION replace_question_mark_0_0 ( txt, val ) &
       RESULT( res )
    !! Text mit zu ersetzender Zeichenkette "???...???"
    CHARACTER (LEN=*) , INTENT(IN) :: txt ! 
    !! Integer-Zahl, die ans Stelle der o.g. Zeichenkette eingetragen werden soll
    INTEGER           , INTENT(IN) :: val ! 
    !! Ergebnis
    CHARACTER (LEN=LEN(txt))       :: res ! 
    !! Hilfsvariable
    CHARACTER (LEN=6)              :: ch  ! 
    INTEGER                        :: ia, ie ! 
    !
    res = txt
    ia  = INDEX( txt, '?' )
    ie  = INDEX( txt, '?', BACK=.true. )
    !
    IF ( ia > 0 .AND. ie > 0 .AND. ie >= ia ) THEN
       ch = REPEAT( ' ', LEN(ch) )
       ch = '(In.n)'
       WRITE(ch(3:3),'(I1)') ie-ia+1
       WRITE(ch(5:5),'(I1)') ie-ia+1
       WRITE(res(ia:ie),ch) val
    END IF
    !
  END FUNCTION replace_question_mark_0_0
  !
  !! Ermittle ein Feld mit true/false-Kennung zum Kennzeichnen des ersten
  !! Auftretens einer Zahl in einem Integer-Feld
  FUNCTION get_int_diff_ind_1 ( var ) &
       RESULT( res )
    !! Liste mit Integer-Zahlen
    INTEGER , INTENT(IN) :: var(:) ! 
    !! Liste mit Kennung zum Kennzeichnen des erstmaligen Auftretens
    LOGICAL :: res(SIZE(var)) ! 
    !! Hilfsvariable 
    INTEGER :: i, j           ! 
    !
    res(:) = .true.
    DO i=1,SIZE(var)
       IF ( .NOT. res(i) ) CYCLE
       DO j=i+1,SIZE(var)
          IF ( .NOT. res(j) ) CYCLE
          res(j) = ( var(i) /= var(j) )
       END DO
    END DO
    !
  END FUNCTION get_int_diff_ind_1 
  !
END MODULE m_h_grid_omi
! TailOfPackageModule -----------------------------------------------------
