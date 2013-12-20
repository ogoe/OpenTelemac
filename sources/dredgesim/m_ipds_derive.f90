! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Modul zum Ableiten nicht vorhandener Informationen aus verf&uuml;gbaren 
!!     Daten f&uuml;r IPDS</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_derive.f90
!! <HR>
!! derivation of additional data related to the IPDS software package <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-08-10 : G. Lang    : Erstversion
!  02.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-08-22 : J. Juerges : Das neue Modul "m_ipds_phydef" beruecksichtigt
!  03.01 : 2007-03-13 : G. Lang    : Fehler in "get_ipds_dim_ind" korrigiert
!
!!                                            
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Bereitstellen verschiedener Leistungen zum Erzeugen OpenMI-konformer Daten f&uuml;r "ipds".
!! </OL>
!! <HR>
!
MODULE m_ipds_derive
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] Basis-Modul mit globalen Konstantwerten [ggf. entfernen]
  USE b_constants, ONLY :   &
       Double
  !
  ! [A.1.2] Basis-Modul mit Typ+Methoden "Fehlerbehandlung"
  USE b_error, ONLY :       &
       ! Routinen / Interfaces
       no_error,            &
       any_error,           &
       setup_error_act
  !
  ! [A.1.2] Basis-Modul mit Typ+Methoden "Dimensionsbezeichner"
  USE b_dim, ONLY :         &
       ! Datentyp
       t_dim,               &
       ! Konstante 
       c_max_dim_name,      &
       c_dim_name,          &
       ! Routinen / Interfaces
       new_dim,             &
       kill_dim,            &
       set_dim_id,          &
       set_dim_name,        &
       set_dim_len,         &
       get_dim_idx
  !
  ! [A.1.3] Basis-Modul mit Typ+Methoden "Variablenbezeichner"
  USE b_var, ONLY :         &
       ! Datentyp
       t_var,               &
       ! Konstante 
       ! Routinen / Interfaces
       new_var,             &
       kill_var,            &
       set_var_id,          &
       set_var_name,        &
       set_var_type,        &
       set_var_dim_id,      &
       get_var_id,          &
       get_var_name,        &
       get_var_dim_id
  !
  ! [A.1.4] Basis-Modul mit Typ+Methoden "Attributbezeichner"
  USE b_att, ONLY :         &
       ! Datentyp
       t_att,               &
       ! Konstante 
       c_att_name,          &
       ! Routinen / Interfaces
       new_att,             &
       kill_att,            &
       add_att_history,     &
       set_att_id,          &
       set_att_name,        &
       set_att_var_id,      &
       set_att_ch,          &
       set_att_in,          &
       set_att_dp
  !
  ! [A.1.5] Basis-Modul mit Typ+Methoden "Dateibehandlung"
  USE b_file, ONLY :          &
       ! Typdefinition
       ! Routinen / Interfaces
       get_file_name
  !
  ! [A.1.5] Basis-Modul mit Typ+Methoden "Dateibehandlung"
  USE b_phy, ONLY :          &
       ! Routinen / Interfaces
       get_phy_quant_code,   &
       get_phy_quant_id,     &
       get_phy_quant_descr,  &
       get_phy_unit_id  
  !
  ! ---------------------------------------------------------------------
  ! [B]  Module des Paketes "IPDS"
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Paket-Modul mit globalen Daten
  USE m_ipds_phydef, ONLY : &
       ! Daten
       c_phy_name_de,       &
       c_phy_name_en,       &
       c_dim_name_idx
  !
  USE m_ipds_data, ONLY :   &
       ! Typdefinition
       t_ipds,              &
       ! Daten
       all_errors,          &
       ! Routinen / Interfaces
       get_dim_object,      &
       get_var_object,      &
       get_var_name_object, &
       get_physet_object,   &
       add_dim_object,      &
       add_var_object,      &
       add_att_object
  !
  ! [B.2] Paket-Modul mit "physet" Datentyp und Methoden
  USE m_ipds_physet, ONLY :     &
       ! Typdefinition
       t_physet,                &
       ! Routinen / Interfaces
       index_physet_set_name,   &
       get_physet_nof_var_name, &
       get_physet_nof_set,      &
       get_physet_set
  !
  ! [B.3] Paket-Modul mit "mespos" Datetntyp und Methoden
  USE m_ipds_mespos, ONLY :     &
       ! Typdefinition
       t_mespos,                &
       ! Routinen / Interfaces
       get_mespos_nof_var_name
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
  !! Erzeuge alle OpenMI-konformen Daten f&uuml;r ein Objekt des Typs "t_dim"
  INTERFACE derive_dim
     MODULE PROCEDURE derive_dim_d
  END INTERFACE
  !! Erzeuge alle OpenMI-konformen Daten f&uuml;r ein Objekt des Typs "t_var"
  INTERFACE derive_var
     MODULE PROCEDURE derive_var_d
  END INTERFACE
  !! Erzeuge alle OpenMI-konformen Daten f&uuml;r ein Objekt des Typs "t_att"
  INTERFACE derive_att
     MODULE PROCEDURE derive_att_d
  END INTERFACE
  !
  PUBLIC :: derive_dim
  PUBLIC :: derive_var
  PUBLIC :: derive_att
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  ! --------------------------------------------------------------------
  !! Name des Moduls
  CHARACTER (LEN=13) , PRIVATE, PARAMETER :: c_modname='m_ipds_derive' ! 
  !
  ! [D.3] lokale Schnittstellen
  !
  !! Ermittle die Indikatorliste der zu erzeugenden Dimensionen
  INTERFACE get_ipds_dim_ind
     MODULE PROCEDURE get_ipds_dim_ind_d
  END INTERFACE
  !! Ermittle die Anzahl der zu erzeugenden Dimensionen
  INTERFACE get_ipds_dim_count
     MODULE PROCEDURE get_ipds_dim_count_d
  END INTERFACE
  !! Ermittle die Listenposition idx f&uuml;r das i-te Dimensionsobjekt
  !! [ "c_dim_name(idx)" ]
  INTERFACE get_ipds_dim_idx
     MODULE PROCEDURE get_ipds_dim_idx_d
  END INTERFACE
  !! Ermittle die L&auml;nge f&uuml;r das i-te Dimensionsobjekt
  !! [ "c_dim_name(idx)" ]
  INTERFACE get_ipds_dim_len
     MODULE PROCEDURE get_ipds_dim_len_d
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
  !! Ermittle die Komponente "dim(:)" aus den Informationen des Objektes <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE derive_dim_d ( this )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_ipds)   , POINTER :: this     ! 
    !! Hilfsvariablen
    TYPE (t_dim)              :: l_dim    ! 
    INTEGER                   :: i        ! 
    !
    DO i=1,get_ipds_dim_count( this )
       IF ( any_error ( ) ) EXIT
       CALL new_dim        ( l_dim )
       CALL set_dim_id     ( l_dim, get_ipds_dim_idx(this,i) )
       CALL set_dim_name   ( l_dim, c_dim_name(get_ipds_dim_idx(this,i)) )
       CALL set_dim_len    ( l_dim, get_ipds_dim_len(this,i) )
       CALL add_dim_object ( this, l_dim )
       CALL kill_dim       ( l_dim )
    END DO
    !
  END SUBROUTINE derive_dim_d
  !
  !! Ermittle die Komponente "var(:)" aus den Informationen des Objektes <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE derive_var_d ( this )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_ipds)   , POINTER :: this         ! 
    !! Hilfsvariablen
    TYPE (t_var)              :: l_var        ! 
    TYPE (t_dim) , POINTER    :: p_dim(:)     ! 
    INTEGER                   :: i, n, idx(2) ! 
    !
    p_dim => get_dim_object ( this )
    IF ( .NOT. ASSOCIATED( p_dim ) ) THEN
       CALL derive_dim ( this )
       p_dim => get_dim_object ( this )
    END IF
    IF ( ASSOCIATED( this%physet ) ) THEN
       DO i=1,SIZE(c_phy_name_de)
          IF ( any_error ( ) ) EXIT
          idx(1) = index_physet_set_name ( this%physet, c_phy_name_de(i) )
          idx(2) = index_physet_set_name ( this%physet, c_phy_name_en(i) )
          IF ( ANY( idx > 0 ) ) THEN
             CALL new_var        ( l_var )
             CALL set_var_id     ( l_var, i )
             IF ( idx(1) > 0 ) THEN
                CALL set_var_name   ( l_var, c_phy_name_de(i) )
             ELSE
                CALL set_var_name   ( l_var, c_phy_name_en(i) )
             END IF
             CALL set_var_type   ( l_var, 'DP' )
             n = COUNT( c_dim_name_idx(:,i) > 0 )
             CALL set_var_dim_id ( l_var, c_dim_name_idx(1:n,i) )
             CALL add_var_object ( this, l_var )
             CALL kill_var       ( l_var )
          END IF
       END DO
    END IF
    !
    NULLIFY( p_dim )
    !
  END SUBROUTINE derive_var_d
  !
  !! Ermittle die Komponente "att(:)" aus den Informationen des Objektes <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE derive_att_d ( this )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_ipds)   , POINTER :: this        ! 
    !! Hilfsvariablen
    TYPE (t_att)              :: l_att       ! 
    TYPE (t_dim) , POINTER    :: p_dim(:)    ! 
    TYPE (t_var) , POINTER    :: p_var(:)    ! 
    LOGICAL                   :: add         ! 
    INTEGER                   :: i, j, k, n, l, code ! 
    INTEGER , PARAMETER       :: c_glo_nof=5 ! 
    INTEGER , PARAMETER       :: c_glo_idx(c_glo_nof)=(/ 1, 2, 3, 4, 5 /) ! 
    INTEGER , PARAMETER       :: c_var_nof=6 ! 
    INTEGER , PARAMETER       :: c_var_idx(c_var_nof)=(/ 38, 39, 40, 41, 55, 56 /) ! 
    INTEGER , PARAMETER       :: c_dim_nof=2 ! 
    INTEGER , PARAMETER       :: c_dim_idx(c_dim_nof)=(/ 11, 27 /) ! 
    INTEGER                   :: l_dim_idx(c_dim_nof) ! 
    INTEGER , POINTER         :: p_dim_id(:) ! 
    CHARACTER (LEN=80) , POINTER :: var_name(:) ! 
    CHARACTER (LEN=80)        :: l_var_name  ! 
    !
    p_dim => get_dim_object ( this )
    IF ( .NOT. ASSOCIATED( p_dim ) ) THEN
       CALL derive_dim ( this )
       p_dim => get_dim_object ( this )
    END IF
    p_var => get_var_object ( this )
    IF ( .NOT. ASSOCIATED( p_var ) ) THEN
       CALL derive_var ( this )
       p_var => get_var_object ( this )
    END IF
    ! --- -----------------------------------------------------------------
    n = 0
    ! [1] globale Attribute setzen ----------------------------------------
    DO i=1,SIZE(c_glo_idx)
       IF ( any_error( ) ) EXIT
       n   = n + 1
       add = .true. 
       CALL new_att        ( l_att )
       CALL set_att_id     ( l_att, n )
       CALL set_att_name   ( l_att, c_att_name(c_glo_idx(i)) )
       CALL set_att_var_id ( l_att, 0 )
       SELECT CASE ( c_glo_idx(i) )
       CASE ( 1 ) ! title
          CALL set_att_ch      ( l_att, 'ipds data '//TRIM(this%name) )
       CASE ( 2 ) ! history
          CALL add_att_history ( l_att, c_modname, TRIM(get_file_name(this%file)) ) 
       CASE ( 3,4,5 ) ! beginning date, ending date, single date
          add = MERGE( .true., .false., ASSOCIATED(this%datetime) )
          IF ( add ) CALL set_att_ch ( l_att, this%datetime )
       END SELECT
       IF ( add ) THEN
          CALL add_att_object ( this, l_att )
          CALL kill_att       ( l_att )
       END IF
    END DO
    ! [2] variablenbezogene Attribute
    DO j=1,SIZE(p_var)
       DO i=1,SIZE(c_var_idx)
          IF ( any_error( ) ) EXIT
          n   = n + 1
          add = .true. 
          CALL new_att        ( l_att )
          CALL set_att_id     ( l_att, n )
          CALL set_att_name   ( l_att, c_att_name(c_var_idx(i)) )
          CALL set_att_var_id ( l_att, get_var_id( p_var(j) ) )
          l_var_name = get_var_name( p_var(j) )
          DO k=1,LEN(l_var_name)
             IF ( l_var_name(k:k) == '_' ) l_var_name(k:k) = ' '
          END DO
          code = get_phy_quant_code ( l_var_name )
          SELECT CASE ( c_var_idx(i) )
          CASE ( 38 ) ! long_name
             CALL set_att_ch ( l_att, get_phy_quant_descr( code ) )
          CASE ( 39 ) ! short_name
             CALL set_att_ch ( l_att, get_phy_quant_id( code ) )
          CASE ( 40 ) ! units
             CALL set_att_ch ( l_att, get_phy_unit_id( code ) )
          CASE ( 41 ) ! name_id
             CALL set_att_in ( l_att, code )
          CASE ( 55 ) ! storage_type
             CALL set_att_ch ( l_att, 'DP' )
          CASE ( 56 ) ! class_names
             add = .false.
             p_dim_id => get_var_dim_id ( p_var(j) )
             IF ( ASSOCIATED(p_dim_id) ) THEN
                l_dim_idx = get_dim_idx( p_dim(get_dim_idx(p_dim, p_dim_id)), c_dim_idx )
                IF ( ANY( l_dim_idx > 0 ) ) THEN
                   CALL get_var_name_object ( this, l_var_name, l, var_name )
                   IF ( ASSOCIATED(var_name) ) THEN
                      add = .true.
                      CALL set_att_ch ( l_att, var_name )
                      DEALLOCATE ( var_name )
                   END IF
                END IF
                DEALLOCATE(p_dim_id)
                NULLIFY(p_dim_id)
             END IF
          END SELECT
          IF ( add ) THEN
             CALL add_att_object ( this, l_att )
             CALL kill_att       ( l_att )
          END IF
       END DO
    END DO
    !
    NULLIFY( p_dim, p_var )
    !
  END SUBROUTINE derive_att_d
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
  ! >>> PRIVATE-GET-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! ermittle eine Indikatorliste mit den bereitzustellenden Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehleremeldungen
  FUNCTION get_ipds_dim_ind_d ( this ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_ipds)   , POINTER :: this ! 
    !! Ergebnis: Indikatorlistre f&uuml;r die Dimensionen in "c_dim_name"
    LOGICAL                   :: res(c_max_dim_name) ! 
    !! Hilfsvariablen
    INTEGER                       :: i, j, idx   ! 
    !
    res(:)   = .false.
    IF ( ASSOCIATED( this%physet ) ) THEN
       DO i=1,SIZE(c_phy_name_de)
          idx = index_physet_set_name ( this%physet, c_phy_name_de(i) )
          IF ( idx <= 0 ) idx = index_physet_set_name ( this%physet, c_phy_name_en(i) )
          IF ( idx > 0 ) THEN
             DO j=1,SIZE(c_dim_name_idx,1)
                IF ( c_dim_name_idx(j,i) > 0 ) res(c_dim_name_idx(j,i)) = .true.
             END DO
          END IF
       END DO
    END IF
    !
  END FUNCTION get_ipds_dim_ind_d
  !
  !! ermittle die Anzahl der verschiedenen Dimensionen des Typs "t_dim" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehleremeldungen
  FUNCTION get_ipds_dim_count_d ( this ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_ipds) , POINTER :: this ! 
    !! Ergebnis: Anzahl der verschiedenen Dimensionen "t_dim"
    INTEGER :: res ! 
    !
    res = COUNT( get_ipds_dim_ind ( this ) )
    !
  END FUNCTION get_ipds_dim_count_d
  !
  !! ermittle die Listenposition (Index) der val-ten Dimensionsbezeichnung
  !! in c_dim_name(:) <BR>
  !! Funktion erzeugt <EM>keine</EM> fehlermeldungen
  FUNCTION get_ipds_dim_idx_d ( this, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_ipds) , POINTER :: this  ! 
    !! lfd. Nummer der Dimensionbezeichnung
    INTEGER , INTENT(IN)    :: val   ! 
    !! Ergebnis: Indexlistenposition der val-ten Dimension in c_dim_name(:)
    INTEGER :: res                   ! 
    !! Hilfsvariablen
    INTEGER :: i, n                  ! 
    LOGICAL :: l_ind(c_max_dim_name) ! 
    !
    res = -1
    IF ( val >= 1 .AND. val <= get_ipds_dim_count( this ) ) THEN
       l_ind = get_ipds_dim_ind ( this )
       n     = 0
       DO i=1,SIZE(l_ind)
          IF ( res /= -1 ) EXIT
          n = MERGE( n+1, n, l_ind(i) )
          IF ( n == val ) res = i
       END DO
    END IF
    !
  END FUNCTION get_ipds_dim_idx_d
  !
  !! ermiitle die L&auml;nge der val-ten Dimensionbezeichnung <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_dim_len_d ( this, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_ipds) , POINTER :: this  ! 
    !! lfd. Nummer der Dimensionbezeichnung
    INTEGER , INTENT(IN)    :: val   ! 
    !! Ergebnis: aktuelle L&auml;nge der val-ten Dimension <BR>
    !! mit -1 wird die Recorddimension gekennzeichnet
    INTEGER :: res                   ! 
    !! Hilfsvariablen
    INTEGER                        :: i, j, k, idx, jdx ! 
    INTEGER          , ALLOCATABLE :: l_res(:)       ! 
    CHARACTER (LEN=80), POINTER    :: var_name(:)    ! 
    !
    res = 0
    IF ( val >= 1 .AND. val <= get_ipds_dim_count( this ) ) THEN
       idx = get_ipds_dim_idx( this, val )
       IF ( idx > 0 ) THEN
          IF ( ASSOCIATED(this%mespos) ) ALLOCATE(l_res(SIZE(this%mespos)))
          SELECT CASE ( idx )
          CASE (  1 ) ! time
             res = -1 ! Recorddimension (Zeit)
          CASE ( 15 ) ! vecdim1
             res = 1
          CASE ( 16, 17 ) ! vecdim_plus_amount1, vecdim2
             res = 2
          CASE ( 18, 19 ) ! vecdim_plus_amount2, vecdim3
             res = 3
          CASE ( 20 )     ! vecdim_plus_amount3
             res = 4
          CASE DEFAULT
             DO j=1,SIZE(c_dim_name_idx,2)
                IF ( res > 0 ) EXIT
                DO i=1,SIZE(c_dim_name_idx,1)
                   IF ( res > 0 ) EXIT
                   IF ( c_dim_name_idx(i,j) == idx ) THEN
                      CALL get_var_name_object ( this, c_phy_name_de(j), res, var_name )
                      IF ( ASSOCIATED(var_name) ) DEALLOCATE( var_name )
                      IF ( res <= 0 ) THEN
                         CALL get_var_name_object ( this, c_phy_name_en(j), res, var_name )
                         IF ( ASSOCIATED(var_name) ) DEALLOCATE( var_name )
                      END IF
                      res = MERGE( res/2, res, ANY( (/12,13/)==idx ) ) ! Vektorkomponenten intern als Klassen behandelt
                   END IF
                END DO
             END DO
          END SELECT
          IF ( ALLOCATED( l_res ) ) DEALLOCATE( l_res )
       END IF
    END IF
    !
  END FUNCTION get_ipds_dim_len_d
  !
  ! TailOfPackageModule -----------------------------------------------------
END MODULE m_ipds_derive
