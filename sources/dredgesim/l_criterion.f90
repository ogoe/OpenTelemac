! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! managing certain criteria for automatic initiation of dredging
!!
!
MODULE l_criterion
  !
  ! ----------------------------------------------------------------------
  ! [A] base modules with frequently used methods
  ! ----------------------------------------------------------------------
  !
  ! [A.1] base module with global constant values
  USE b_constants, ONLY : Single, Double ! 
  ! [A.2] base module with error-type and -routines 
  !
  USE b_error, ONLY : &
       ! type definitions
       t_error, &
       ! routines
       init_error, clear_error, &
       no_error, any_error, &
       new_error, kill_error, print_error, &
       setup_error_act, &
       setup_error_prn_lun, setup_error_trc_lun, &
       set_error_ierr, set_error_cerr
  !
  ! ---------------------------------------------------------------------
  ! [B]
  ! ---------------------------------------------------------------------
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] public declarations (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.0] parameter-definitions
  !
  !! max. number of symbols in "poly_name" 
  INTEGER , PUBLIC , parameter :: c_max_poly_name_len=80 ! 
  !
  ! [C.1] declaration of data type
  !
  !! poly_name    : name of dredge polygon
  !! crit_type    : criterion type
  !!                1 = bottom depth used for dregde criterion
  !!                2 = water depth used for dredge criterion
  !! crit_depth   : critical bottom/water depth at which dredging is initiated automatically
  !! dredge_depth : aim bottom/water depth which should be reached by dredging
  TYPE , PUBLIC :: t_criterion
     PRIVATE
     CHARACTER (LEN=c_max_poly_name_len) :: poly_name    ! 
     INTEGER                             :: crit_type    ! 
     REAL (KIND=Double)                  :: crit_depth   ! 
     REAL (KIND=Double)                  :: dredge_depth ! 
  END TYPE t_criterion
  !
  ! [C.2] constant values (parameter)
  !
  ! [C.3] variables
  !
  ! [C.4] interfaces
  !
  ! [C.4.1] required public interfaces
  !

  !! allocating/initializing of static data in the module
  !! initializing of static data with default values
  INTERFACE init_criterion
     MODULE PROCEDURE init_criterion_d ! 
  END INTERFACE
  !! deallocating/deinitializing of static data objects in this module
  !! reinitializing of some static data with default values
  INTERFACE clear_criterion
     MODULE PROCEDURE clear_criterion_d ! 
  END INTERFACE
  !! channel number for PRINT-methods
  !! no output: PRN_LUN = -1
  !! output: PRN_LUN = 0
  INTERFACE setup_criterion_prn_lun
     MODULE PROCEDURE setup_criterion_prn_lun_d ! 
  END INTERFACE
  !! channel number for TRACE-methods
  !! no output: TRC_LUN = -1
  !! output: TRC_LUN = 0
  INTERFACE setup_criterion_trc_lun
     MODULE PROCEDURE setup_criterion_trc_lun_d ! 
  END INTERFACE
  !! generating data objects "t_criterion" (scalar, 1D-array)
  INTERFACE new_criterion
     MODULE PROCEDURE new_criterion_0  ! scalar
     MODULE PROCEDURE new_criterion_1  ! 1D-array
  END INTERFACE
  !! kill data objects "t_criterion" (scalar, 1D-array)
  INTERFACE kill_criterion
     MODULE PROCEDURE kill_criterion_0 ! scalar
     MODULE PROCEDURE kill_criterion_1 ! 1D-array
  END INTERFACE
  !! check data objects "t_criterion" (scalar, 1D-array)
  INTERFACE ok_criterion
     MODULE PROCEDURE ok_criterion_0 ! scalar
     MODULE PROCEDURE ok_criterion_1 ! 1D-array
  END INTERFACE
  !! print data objects "t_criterion" (scalar, 1D-array)
  INTERFACE print_criterion
     MODULE PROCEDURE print_criterion_0 ! scalar
     MODULE PROCEDURE print_criterion_1 ! 1D-array
  END INTERFACE
  !! printing all static data in this module
  INTERFACE print_criterion_static
     MODULE PROCEDURE print_criterion_static_d ! 
  END INTERFACE
  !! printing all error messages in this module
  INTERFACE print_criterion_all_errors
     MODULE PROCEDURE print_criterion_all_errors_d ! 
  END INTERFACE
  !
  !! set component "poly_name" in "t_criterion" to user value
  !! a) scalar object , scalar data 
  !! b) vector object , scalar data 
  INTERFACE set_criterion_poly_name
     MODULE PROCEDURE set_criterion_poly_name_0_0 ! Objekt (scalar) / data (scalar)
     MODULE PROCEDURE set_criterion_poly_name_1_0 ! Objekt (vector) / data (scalar) 
  END INTERFACE
  !! set component "crit_type" in "t_criterion" to user value
  !! a) scalar object , scalar data 
  !! b) vector object , scalar data 
  INTERFACE set_criterion_crit_type
     MODULE PROCEDURE set_criterion_crit_type_0_0 ! Objekt (scalar) / data (scalar)
     MODULE PROCEDURE set_criterion_crit_type_1_0 ! Objekt (vector) / data (scalar) 
  END INTERFACE
  !! set component "crit_depth" in "t_criterion" to user value
  !! a) scalar object , scalar data 
  !! b) vector object , scalar data 
  INTERFACE set_criterion_crit_depth
     MODULE PROCEDURE set_criterion_crit_depth_0_0 ! Objekt (scalar) / data (scalar)
     MODULE PROCEDURE set_criterion_crit_depth_1_0 ! Objekt (vector) / data (scalar) 
  END INTERFACE
  !! set component "dredge_depth" in "t_criterion" to user value
  !! a) scalar object , scalar data 
  !! b) vector object , scalar data 
  INTERFACE set_criterion_dredge_depth
     MODULE PROCEDURE set_criterion_dredge_depth_0_0 ! Objekt (scalar) / data (scalar)
     MODULE PROCEDURE set_criterion_dredge_depth_1_0 ! Objekt (vector) / data (scalar) 
  END INTERFACE
  !
  !! get component "poly_name" out of "t_criterion" 
  !! a) scalar object
  !! b) vector object
  INTERFACE get_criterion_poly_name
     MODULE PROCEDURE get_criterion_poly_name_0_0 ! scalar
     MODULE PROCEDURE get_criterion_poly_name_1_0 ! vector
  END INTERFACE
  !! get component "crit_type" out of "t_criterion"
  !! a) scalar object , scalar data 
  !! b) vector object , scalar data 
  INTERFACE get_criterion_crit_type
     MODULE PROCEDURE get_criterion_crit_type_0_0 ! Objekt (scalar) / data (scalar)
     MODULE PROCEDURE get_criterion_crit_type_1_0 ! Objekt (vector) / data (scalar) 
  END INTERFACE
  !! get component "crit_depth" out of "t_criterion" 
  !! a) scalar object
  !! b) vector object
  INTERFACE get_criterion_crit_depth
     MODULE PROCEDURE get_criterion_crit_depth_0_0 ! scalar
     MODULE PROCEDURE get_criterion_crit_depth_1_0 ! vector
  END INTERFACE
  !! get component "dredge_depth" out of "t_criterion" 
  !! a) scalar object
  !! b) vector object
  INTERFACE get_criterion_dredge_depth
     MODULE PROCEDURE get_criterion_dredge_depth_0_0 ! scalar
     MODULE PROCEDURE get_criterion_dredge_depth_1_0 ! vector
  END INTERFACE
  !
  ! [C.4.2] optional public interfaces
  ! [C.5] assignments
  ! [C.6] operators
  ! [C.6.1] required public operators
  !
  !! check for identity of two data objects "t_criterion"
  INTERFACE eq_criterion 
     MODULE PROCEDURE eq_criterion_0_0  ! scalar / scalar
     MODULE PROCEDURE eq_criterion_0_1  ! scalar / vector 
     MODULE PROCEDURE eq_criterion_1_0  ! vector / scalar
     MODULE PROCEDURE eq_criterion_1_1  ! vector / vector
  END INTERFACE
  !
  ! [C.6.2] optional public operators
  !
  ! [C.7] list of public methods
  !
  ! [C.7.1] required public methods
  !
  PUBLIC :: init_criterion
  PUBLIC :: clear_criterion
  PUBLIC :: setup_criterion_prn_lun
  PUBLIC :: setup_criterion_trc_lun
  PUBLIC :: new_criterion
  PUBLIC :: kill_criterion
  PUBLIC :: ok_criterion
  PUBLIC :: print_criterion
  PUBLIC :: print_criterion_static
  PUBLIC :: print_criterion_all_errors
  PUBLIC :: set_criterion_poly_name
  PUBLIC :: set_criterion_crit_type
  PUBLIC :: set_criterion_crit_depth
  PUBLIC :: set_criterion_dredge_depth
  PUBLIC :: get_criterion_poly_name
  PUBLIC :: get_criterion_crit_type
  PUBLIC :: get_criterion_crit_depth
  PUBLIC :: get_criterion_dredge_depth
  PUBLIC :: eq_criterion
  !
  ! [C.7.2] optional public methods
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data and methods (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] local type defintions
  !
  ! [D.2] constant values (parameter)
  !
  !! name of module
  CHARACTER (LEN=11), parameter :: c_modname      = 'l_criterion' ! 
  !! parameter for TRACE/PRINT-methods (default)
  LOGICAL           , parameter :: c_op           = .false.          ! 
  !! channel number for TRACE/PRINT-methods (default)
  INTEGER           , parameter :: c_lun          = -1               ! 
  !! number of data components in type t_criterion
  INTEGER           , parameter :: c_nofcomp      = 4                ! ggf. modifizieren
  !
  !! undefined-value for CHARACTER-variables and -arrays
  CHARACTER (LEN=1) , PUBLIC , parameter :: c_undef_ch='?'            !  
  !! undefined-value for Integer-variables and -arrays
  INTEGER, PUBLIC, parameter             :: c_undef_in=-999             !
  !! undefined-value for REAL(Double)-variables and -arrays 
  REAL (KIND=Double), PUBLIC , parameter :: c_undef_dp=1.0E+31_Double ! 
  !
  ! [D.3] variables (static data of module)
  !
  !! array for all error messages in this module
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)! 
  !! parameter if initialization of this module was successful
  LOGICAL                , SAVE :: initialised = .false.  ! 
  !! value for realization of PRINT-methods
  LOGICAL                , SAVE :: prn_op      = c_op     ! 
  !! value for realization of TRACE-methods
  LOGICAL                , SAVE :: trc_op      = c_op     ! 
  !! channel number for PRINT-methods
  INTEGER                , SAVE :: prn_lun     = c_lun    ! 
  !! channel number for TRACE-methods
  INTEGER                , SAVE :: trc_lun     = c_lun    ! 
  !! enumerator for calls of initialization
  INTEGER                , SAVE :: n_init      = 0        ! 
  !
  ! [D.4] interfaces
  !
  ! [D.5] assignments
  !
  ! [D.6] operators
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
  ! public methods with access through PUBLIC interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-methods <<< [ERR_NO =  1000 to  1999]
  ! ----------------------------------------------------------------------
  !
  !! allocating/initializing of static data in this module 
  !! subroutine generates error messages
  SUBROUTINE init_criterion_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! name of subroutine
    CHARACTER (LEN=16), parameter :: c_upname='init_criterion_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] 
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "l_criterion" version ... <'
          WRITE(*,*) ' Copyright (C) 2007 Bundesanstalt fuer Wasserbau '
          WRITE(*,*)
       END IF
       ! [1.2] 
       CALL init_error ( )
       ! [1.3]
       initialised = .true.
       ! [1.4] allocating/initializing of error messages
       IF ( no_error( ) ) CALL init_criterion_all_errors ( ) 
       ! [1.5] initializing channel number
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] final setting of initialization variable
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    !
    n_init = n_init + 1
    !
  END SUBROUTINE init_criterion_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-methods <<< [ERR_NO =  2000 to  2999]
  ! ----------------------------------------------------------------------
  !
  !! deallocating/deinitializing of static data in this module 
  !! subroutine generates error messages
  SUBROUTINE clear_criterion_d ( )
    !! name of subroutine
    CHARACTER (LEN=17), parameter :: c_upname='clear_criterion_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] deallocating/deinitializing of error messages
       IF ( no_error( ) ) CALL clear_criterion_all_errors ( ) 
       ! [1.2] deinitializing of channel number
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] 
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] 
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    !
    n_init = n_init - 1
    !
  END SUBROUTINE clear_criterion_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-methods <<< [ERR_NO =  3000 to  3999]
  ! ----------------------------------------------------------------------
  !
  !! setting of channel number for PRINT-methods 
  !! subroutine generates error messages
  SUBROUTINE setup_criterion_prn_lun_d ( lun )
    !! current channel number for PRINT-methods 
    INTEGER , INTENT(IN) :: lun ! 
    !! name of subroutine
    CHARACTER (LEN=25), parameter :: c_upname='setup_criterion_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! 
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_criterion_prn_lun_d
  !
  !! setting of channel number for TRACE-methods 
  !! subroutine generates error messages
  SUBROUTINE setup_criterion_trc_lun_d ( lun )
    !! current channel number for TRACE-methods 
    INTEGER , INTENT(IN) :: lun ! 
    !! name of subroutine
    CHARACTER (LEN=25), parameter :: c_upname='setup_criterion_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! 
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_criterion_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-methods <<< [ERR_NO =  4000 to  4999]
  ! ----------------------------------------------------------------------
  !
  !! initializing of a new data object (scalar) 
  !! subroutine generates error messages
  SUBROUTINE new_criterion_0 ( this )
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(OUT) :: this ! 
    !! name of subroutine
    CHARACTER (LEN=15), parameter :: c_upname='new_criterion_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       this%poly_name    = REPEAT( c_undef_ch, LEN(this%poly_name) )
       this%crit_type    = c_undef_in
       this%crit_depth   = c_undef_dp
       this%dredge_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE new_criterion_0
  !
  !! initializing of a new data object (vector) 
  !! subroutine generates error messages
  SUBROUTINE new_criterion_1 ( this )
    !! data object (vector)
    TYPE (t_criterion) , INTENT(OUT) :: this(:) ! 
    !! name of subroutine
    CHARACTER (LEN=15), parameter :: c_upname='new_criterion_1' ! 
    !! counter
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL new_criterion_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_criterion_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-methods <<< [ERR_NO =  5000 to  5999]
  ! ----------------------------------------------------------------------
  !
  !! deallocating/deinitializing of a data object (scalar) 
  !! subroutine generates error messages
  SUBROUTINE kill_criterion_0 ( this )
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(INOUT) :: this ! 
    !! name of subroutine
    CHARACTER (LEN=16), parameter :: c_upname='kill_criterion_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( no_error( ) ) CALL new_criterion_0 ( this )
    END IF
    !
  END SUBROUTINE kill_criterion_0
  !
  !! deallocating/deinitializing of a data object (vector) 
  !! subroutine generates error messages
  SUBROUTINE kill_criterion_1 ( this )
    !! data object (vector)
    TYPE (t_criterion) , INTENT(INOUT) :: this(:) ! 
    !! name of subroutine
    CHARACTER (LEN=16), parameter :: c_upname='kill_criterion_1' ! 
    !! counter
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL kill_criterion_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_criterion_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-methods <<< [ERR_NO =  6000 to  6999]
  ! ----------------------------------------------------------------------
  !
  !! check if data object is valid (scalar) 
  !! subroutine generates error messages
  FUNCTION ok_criterion_0 ( this ) &
       RESULT( ok )
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! test result (scalar)
    LOGICAL :: ok ! 
    !! name of function
    CHARACTER (LEN=14), parameter :: c_upname='ok_criterion_0' 
    !! local array with test result
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN
       l_ok(1) = ok_criterion_poly_name   ( this )
       l_ok(2) = ok_criterion_crit_type   ( this )
       l_ok(3) = ok_criterion_crit_depth  ( this )
       l_ok(4) = ok_criterion_dredge_depth( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_criterion_0
  !
  !! check if data object is valid (vector) 
  !! subroutine generates error messages
  FUNCTION ok_criterion_1 ( this ) &
       RESULT( ok )
    !! data object (vector)
    TYPE (t_criterion) , INTENT(IN) :: this(:) ! 
    !! test result (vector)
    LOGICAL :: ok(SIZE(this)) ! 
    !! name of function
    CHARACTER (LEN=14), parameter :: c_upname='ok_criterion_1' 
    !! counter
    CHARACTER (LEN=5) :: l_char ! 
    INTEGER           :: i, j   ! 
    !
    ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN
       DO i=1,SIZE(this)
          ok(i) = ok_criterion_0 ( this(i) )
          DO j=i+1,SIZE(this)
             IF ( this(i)%poly_name == this(j)%poly_name ) THEN
                CALL setup_error_act ( all_errors(:), 6011, c_upname, c_modname )
                WRITE(l_char,'(I5)') i ; CALL setup_error_act ( '<pos1>', l_char )
                WRITE(l_char,'(I5)') j ; CALL setup_error_act ( '<pos2>', l_char )
                CALL setup_error_act ( '<val1>', TRIM(this(i)%poly_name) )
                CALL setup_error_act ( '<val2>', TRIM(this(j)%poly_name) )
                ok(i) = .false. 
             END IF
          END DO
       END DO
    END IF
    !
  END FUNCTION ok_criterion_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-methods <<< [ERR_NO =  7000 to  7999]
  ! ----------------------------------------------------------------------
  !
  !! print contents of a data object (scalar) 
  !! subroutine generates error messages
  SUBROUTINE print_criterion_0 ( this )
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! name of function
    CHARACTER (LEN=17), parameter :: c_upname='print_criterion_0' 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_criterion_poly_name( this )
       IF ( no_error( ) ) CALL print_criterion_crit_type( this )
       IF ( no_error( ) ) CALL print_criterion_crit_depth( this )
       IF ( no_error( ) ) CALL print_criterion_dredge_depth( this )
       !
       IF ( no_error( ) ) THEN
          !
          WRITE ( UNIT=prn_lun, FMT= 8001, IOSTAT=stat )
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
          !
       END IF
       !
    ELSE
       !
       WRITE(*,*) ' >>> subroutine '//TRIM(c_upname)//' - no printable output '
       !
    END IF
    !
8000 FORMAT('# begin object t_criterion ------------------------------')
8001 FORMAT('# end   object t_criterion ------------------------------')
    !
  END SUBROUTINE print_criterion_0
  !
  !! print contents of a data object (vector) 
  !! subroutine generates error messages
  SUBROUTINE print_criterion_1 ( this )
    !! data object (vector)
    TYPE (t_criterion) , INTENT(IN) :: this(:) ! 
    !! name of function
    CHARACTER (LEN=17), parameter :: c_upname='print_criterion_1' 
    !! counter
    INTEGER :: i ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          !
          WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT= stat ) i
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          !
          IF ( no_error( ) ) CALL print_criterion_0 ( this(i) )
          !
       END DO
       !
    ELSE
       !
       WRITE(*,*) ' >>> subroutine '//TRIM(c_upname)//' - no printable output '
       !
    END IF
    !
8000 FORMAT ('# data object index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_criterion_1
  !
  !! print all static data of the module 
  !! subroutine generates error messages
  SUBROUTINE print_criterion_static_d ( )
    !! name of function
    CHARACTER (LEN=24), parameter :: c_upname='print_criterion_static_d' 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_criterion_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> subroutine '//TRIM(c_upname)//' - no printable output '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten in Modul l_criterion             ',/ &
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
  END SUBROUTINE print_criterion_static_d
  !
  !! printing all errors of the module 
  !! subroutine generates error messages
  SUBROUTINE print_criterion_all_errors_d ( )
    !! name of function
    CHARACTER (LEN=28), parameter :: c_upname='print_criterion_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> subroutine '//TRIM(c_upname)//' - no printable output '
    END IF
    !
  END SUBROUTINE print_criterion_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-methods <<< [ERR_NO =  8000 to  8999]
  ! ----------------------------------------------------------------------
  !
  !! fill component "poly_name" with a scalar value (scalar) 
  !! subroutine does not throw error messages
  SUBROUTINE set_criterion_poly_name_0_0 ( this, val )
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(INOUT) :: this ! 
    !! value for component "poly_name"
    CHARACTER (LEN=*)  , INTENT(IN)    :: val  ! 
    !
    this%poly_name = REPEAT( ' ', LEN(this%poly_name) )
    this%poly_name = val(1:MIN(LEN(this%poly_name),LEN_TRIM(val)))
    !
  END SUBROUTINE set_criterion_poly_name_0_0
  !
  !! fill component "poly_name" with a scalar value (vector) 
  !! subroutine does not throw error messages
  SUBROUTINE set_criterion_poly_name_1_0 ( this, val )
    !! data object (vector)
    TYPE (t_criterion) , INTENT(INOUT) :: this(:) ! 
    !! value for component "poly_name"
    CHARACTER  (LEN=*) , INTENT(IN)    :: val     ! 
    ! variable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_criterion_poly_name_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_criterion_poly_name_1_0
  !
  !! fill component "crit_type" with a scalar value (scalar) 
  !! subroutine does not throw error messages
  SUBROUTINE set_criterion_crit_type_0_0 ( this, val )
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(INOUT) :: this ! 
    !! value for component "crit_type"
    INTEGER , INTENT(IN)               :: val  ! 
    !
    this%crit_type = val
    !
  END SUBROUTINE set_criterion_crit_type_0_0
  !
  !! fill component "crit_type" with a scalar value (vector) 
  !! subroutine does not throw error messages
  SUBROUTINE set_criterion_crit_type_1_0 ( this, val )
    !! data object (vector)
    TYPE (t_criterion) , INTENT(INOUT) :: this(:) ! 
    !! value for component "crit_type"
    INTEGER , INTENT(IN)               :: val     ! 
    ! variable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_criterion_crit_type_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_criterion_crit_type_1_0
  !
  !! fill component "crit_depth" with a scalar value (scalar) 
  !! subroutine does not throw error messages
  SUBROUTINE set_criterion_crit_depth_0_0 ( this, val )
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(INOUT) :: this ! 
    !! value for component "crit_depth"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%crit_depth = val
    !
  END SUBROUTINE set_criterion_crit_depth_0_0
  !
  !! fill component "crit_depth" with a scalar value (vector) 
  !! subroutine does not throw error messages
  SUBROUTINE set_criterion_crit_depth_1_0 ( this, val )
    !! data object (vector)
    TYPE (t_criterion) , INTENT(INOUT) :: this(:) ! 
    !! value for component "crit_depth"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this%crit_depth = val
    !
  END SUBROUTINE set_criterion_crit_depth_1_0
  !
  !! fill component "dredge_depth" with a scalar value (scalar) 
  !! subroutine does not throw error messages
  SUBROUTINE set_criterion_dredge_depth_0_0 ( this, val )
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(INOUT) :: this ! 
    !! value for component "dredge_depth"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%dredge_depth = val
    !
  END SUBROUTINE set_criterion_dredge_depth_0_0
  !
  !! fill component "dredge_depth" with a scalar value (vector) 
  !! subroutine does not throw error messages
  SUBROUTINE set_criterion_dredge_depth_1_0 ( this, val )
    !! data object (vector)
    TYPE (t_criterion) , INTENT(INOUT) :: this(:) ! 
    !! value for component "dredge_depth"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this%dredge_depth = val
    !
  END SUBROUTINE set_criterion_dredge_depth_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-methods <<< [ERR_NO =  9000 to  9999]
  ! ----------------------------------------------------------------------
  !
  !! get component "poly_name" out of scalar data object 
  !! function does not throw error messages
  FUNCTION get_criterion_poly_name_0_0 ( this ) &
       RESULT( val ) 
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(IN)     :: this ! 
    !! return value "poly_name" (scalar)
    CHARACTER (LEN=c_max_poly_name_len) :: val  ! 
    !
    val = this%poly_name
    !
  END FUNCTION get_criterion_poly_name_0_0
  !
  !! get component "poly_name" out of vector data object 
  !! function does not throw error messages
  FUNCTION get_criterion_poly_name_1_0 ( this ) &
       RESULT( val ) 
    !! data object (vector)
    TYPE (t_criterion) , INTENT(IN)  :: this(:) ! 
    !! return value "poly_name"
    CHARACTER (LEN=c_max_poly_name_len) :: val(SIZE(this))  ! 
    !
    val = this%poly_name
    !
  END FUNCTION get_criterion_poly_name_1_0
  !
  !! get component "crit_type" out of scalar data object 
  !! function does not throw error messages
  FUNCTION get_criterion_crit_type_0_0 ( this ) &
       RESULT( val ) 
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(IN)     :: this ! 
    !! return value "crit_type" (scalar)
    INTEGER                             :: val  ! 
    !
    val = this%crit_type
    !
  END FUNCTION get_criterion_crit_type_0_0
  !
  !! get component "crit_type" out of vector data object 
  !! function does not throw error messages
  FUNCTION get_criterion_crit_type_1_0 ( this ) &
       RESULT( val ) 
    !! data object (vector)
    TYPE (t_criterion) , INTENT(IN)  :: this(:) ! 
    !! return value "crit_type"
    INTEGER                          :: val(SIZE(this))  ! 
    !
    val = this%crit_type
    !
  END FUNCTION get_criterion_crit_type_1_0
  !
  !! get component "crit_depth" out of scalar data object 
  !! function does not throw error messages
  FUNCTION get_criterion_crit_depth_0_0 ( this ) &
       RESULT( val ) 
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(IN)  :: this ! 
    !! return value "crit_depth" (scalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%crit_depth
    !
  END FUNCTION get_criterion_crit_depth_0_0
  !
  !! get component "crit_depth" out of vector data object 
  !! function does not throw error messages
  FUNCTION get_criterion_crit_depth_1_0 ( this ) &
       RESULT( val ) 
    !! data object (vector)
    TYPE (t_criterion) , INTENT(IN)  :: this(:) ! 
    !! return value "crit_depth"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val = this%crit_depth
    !
  END FUNCTION get_criterion_crit_depth_1_0
  !
  !! get component "dredge_depth" out of scalar data object 
  !! function does not throw error messages
  FUNCTION get_criterion_dredge_depth_0_0 ( this ) &
       RESULT( val ) 
    !! data object (scalar)
    TYPE (t_criterion) , INTENT(IN)  :: this ! 
    !! return value "dredge_depth" (scalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%dredge_depth
    !
  END FUNCTION get_criterion_dredge_depth_0_0
  !
  !! get component "dredge_depth" out of vector data object 
  !! function does not throw error messages
  FUNCTION get_criterion_dredge_depth_1_0 ( this ) &
       RESULT( val ) 
    !! data object (vector)
    TYPE (t_criterion) , INTENT(IN)  :: this(:) ! 
    !! return value "dredge_depth"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val = this%dredge_depth
    !
  END FUNCTION get_criterion_dredge_depth_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-methods <<< [ERR_NO = 10000 to 10999]
  ! ----------------------------------------------------------------------
  !
  !! check identity of two data objects ( scalar / scalar ) 
  !! function does not throw error messages
  FUNCTION eq_criterion_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! object 1 (scalar)
    TYPE (t_criterion) , INTENT(IN) :: this1 ! 
    !! object 2 (scalar)
    TYPE (t_criterion) , INTENT(IN) :: this2 ! 
    !! test result (scalar)
    LOGICAL :: ok ! 
    !! test resultse for alle components
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%poly_name    == this2%poly_name    )
    l_ok(2) = ( this1%crit_type    == this2%crit_type    )
    l_ok(3) = ( this1%crit_depth   == this2%crit_depth   )
    l_ok(4) = ( this1%dredge_depth == this2%dredge_depth )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_criterion_0_0
  !
  !! check identity of two data objects ( vector / scalar ) 
  !! function does not throw error messages
  FUNCTION eq_criterion_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! object 1 (vector)
    TYPE (t_criterion) , INTENT(IN) :: this1(:) ! 
    !! object 2 (scalar)
    TYPE (t_criterion) , INTENT(IN) :: this2    ! 
    !! test result (vector)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! counter
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_criterion_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_criterion_1_0
  !
  !! check identity of two data objects ( scalar / vector ) 
  !! function does not throw error messages
  FUNCTION eq_criterion_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! object 1 (scalar)
    TYPE (t_criterion) , INTENT(IN) :: this1    ! 
    !! object 2 (vector)
    TYPE (t_criterion) , INTENT(IN) :: this2(:) ! 
    !! test result (vector)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! counter
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_criterion_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_criterion_0_1
  !
  !! check identity of two data objects ( vector / vector ) 
  !! function does not throw error messages
  FUNCTION eq_criterion_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! object 1 (vector)
    TYPE (t_criterion) , INTENT(IN) :: this1(:) ! 
    !! object 2 (vector)
    TYPE (t_criterion) , INTENT(IN) :: this2(:) ! 
    !! test result (vector)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! counter
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_criterion_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_criterion_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(+)-methods <<< [ERR_NO = 11000 to 11999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-methods <<< [ERR_NO = 12000 to 12999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(*)-methods <<< [ERR_NO = 13000 to 13999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/)-methods <<< [ERR_NO = 14000 to 14999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>)-methods <<< [ERR_NO = 15000 to 15999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-methods <<< [ERR_NO = 16000 to 16999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-methods <<< [ERR_NO = 17000 to 17999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-methods <<< [ERR_NO = 18000 to 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-methods <<< [ERR_NO = 19000 to 19999]
  ! ----------------------------------------------------------------------
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
  ! local methods:
  !
  ! ----------------------------------------------------------------------
  ! >>> GENREAL-methods <<< [ERR_NO =  0001 to  0999]
  ! ----------------------------------------------------------------------
  !
  !! setting of error condition 1 = module not initialized 
  !! function generates error messages
  FUNCTION ok_initialised ( upname ) &
       RESULT( ok )
    !! name of subroutine calling "ok_initialised"
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! test result
    LOGICAL :: ok ! 
    !! errornumber
    INTEGER            :: ierr    ! 
    !! errortext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** warning *** module "l_criterion" is not initialized '
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'error category: GENERAL'
       cerr(2) = 'module is not initialized'
       cerr(3) = '--> execute INIT_criterion '
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! setting of error condition 2 = module is already initialized 
  !! function generates error messages
  FUNCTION not_initialised ( upname )          &
       RESULT( ok )
    !! name of subroutine calling "not_initialised"
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! test result
    LOGICAL :: ok ! 
    !
    ok = .NOT. initialised
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-methods <<< [ERR_NO =  1000 to  1999]
  ! ----------------------------------------------------------------------
  !
  !! allocating/initializing of all error messages in this module 
  !! subroutine generates error messages
  SUBROUTINE init_criterion_all_errors ( )
    !! counter
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
               '--> INIT_criterion out offuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_criterion out offuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-methods\n'//&
               'Fehler in Komponente von "t_criterion"\n'//&
               'Typkomponente = "poly_name"\n'//&
               'akt = <act>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6011 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-methods\n'//&
               'Fehler in Komponente von "t_criterion"\n'//&
               'Typkomponente = "poly_name"\n'//&
               'ein Wert fuer "poly_name" kommt mehrfach vor\n'//&
               'Pos 1 = <pos1>, value 1 = <val1>\n'//&
               'Pos 2 = <pos2>, value 2 = <val2>\n'//&
               '--> Eingabesteuerdaten ueberpruefen und korrigieren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-methods\n'//&
               'Fehler in Komponente von "t_criterion"\n'//&
               'Typkomponente = "crit_type"\n'//&
               'akt = <act>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-methods\n'//&
               'Fehler in Komponente von "t_criterion"\n'//&
               'Typkomponente = "crit_depth"\n'//&
               'akt = <act>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-methods\n'//&
               'Fehler in Komponente von "t_criterion"\n'//&
               'Typkomponente = "dredge_depth"\n'//&
               'akt = <act>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-methods\n'//&
               'Drucken von Kopfzeilen\n'//&
               '--> Code in Modul "l_criterion" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-methods\n'//&
               'Drucken von Fusszeilen\n'//&
               '--> Code in Modul "l_criterion" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-methods\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "l_criterion" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-methods\n'//&
               'Fehler beim Drucken von Objekt "t_criterion"\n'//&
               'Typkomponente = "poly_name"\n'//&
               '--> Code in Modul "l_criterion" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-methods\n'//&
               'Fehler beim Drucken von Objekt "t_criterion"\n'//&
               'Typkomponente = "crit_type"\n'//&
               '--> Code in Modul "l_criterion" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-methods\n'//&
               'Fehler beim Drucken von Objekt "t_criterion"\n'//&
               'Typkomponente = "crit_depth"\n'//&
               '--> Code in Modul "l_criterion" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-methods\n'//&
               'Fehler beim Drucken von Objekt "t_criterion"\n'//&
               'Typkomponente = "dredge_depth"\n'//&
               '--> Code in Modul "l_criterion" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-methods\n'//&
               'Fehler beim Drucken statischer data out of "l_criterion"\n'//&
               '--> Code in Modul "l_criterion" / Daten pruefen' )
       END IF
       !
       ! allocating arrays after first call (i==1)
       !
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    END DO
    !
  END SUBROUTINE init_criterion_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-methods <<< [ERR_NO =  2000 to  2999]
  ! ----------------------------------------------------------------------
  !
  !! deallocating/deinitializing of all error messages in this module 
  !! subroutine generates error messages
  SUBROUTINE clear_criterion_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_criterion_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-methods <<< [ERR_NO =  3000 to  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-methods <<< [ERR_NO =  4000 to  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-methods <<< [ERR_NO =  5000 to  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-methods <<< [ERR_NO =  6000 to  6999]
  ! ----------------------------------------------------------------------
  !
  !! check if component "poly_name" of a data object is o.k. 
  !! function generates error messages
  FUNCTION ok_criterion_poly_name ( this ) &
       RESULT( ok )
    !! data object
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! test result
    LOGICAL :: ok ! 
    !! name of function
    CHARACTER (LEN=22) , parameter :: c_upname='ok_criterion_poly_name' ! 
    ! variables
    CHARACTER (LEN=c_max_poly_name_len) :: l_string ! 
    !
    l_string = REPEAT( c_undef_ch, LEN(l_string) )
    !

    !LEO BUG TODO here is an error if this%poly_name is "empty"
    !Must be fixed in m_dredgesim_steer.f90

    ok = ( LEN_TRIM(this%poly_name) > 0 .AND. this%poly_name .NE. l_string )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<act>', l_string )
    END IF
    !
  END FUNCTION ok_criterion_poly_name
  !
  !! check if component "crit_type" of a data object is o.k.
  !! function generates error messages
  FUNCTION ok_criterion_crit_type ( this ) &
       RESULT( ok )
    !! data object
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! test result
    LOGICAL :: ok ! 
    !! name of function
    CHARACTER (LEN=22) , parameter :: c_upname='ok_criterion_crit_type' ! 
    ! variable
    CHARACTER (LEN=10) :: l_string ! 
    !
    ok = ( this%crit_type /= c_undef_in )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       WRITE(l_string,'(I10)') this%crit_type ; CALL setup_error_act ( '<act>', l_string )
    END IF
    !
  END FUNCTION ok_criterion_crit_type
  !
  !! check if component "crit_depth" of a data object is o.k.
  !! function generates error messages
  FUNCTION ok_criterion_crit_depth ( this ) &
       RESULT( ok )
    !! data object
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! test result
    LOGICAL :: ok ! 
    !! name of function
    CHARACTER (LEN=23) , parameter :: c_upname='ok_criterion_crit_depth' ! 
    ! variable
    CHARACTER (LEN=15) :: l_string ! 
    !
    ok = ( this%crit_depth /= c_undef_dp )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(l_string,'(G15.8)') this%crit_depth ; CALL setup_error_act ( '<act>', l_string )
    END IF
    !
  END FUNCTION ok_criterion_crit_depth
  !
  !! check if component "dredge_depth" of a data object is o.k.
  !! function generates error messages
  FUNCTION ok_criterion_dredge_depth ( this ) &
       RESULT( ok )
    !! data object
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! test result
    LOGICAL :: ok ! 
    !! name of function
    CHARACTER (LEN=25) , parameter :: c_upname='ok_criterion_dredge_depth' ! 
    ! variable
    CHARACTER (LEN=15) :: l_string ! 
    !
    ok = ( this%dredge_depth /= c_undef_dp )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       WRITE(l_string,'(G15.8)') this%dredge_depth ; CALL setup_error_act ( '<act>', l_string )
    END IF
    !
  END FUNCTION ok_criterion_dredge_depth
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-methods <<< [ERR_NO =  7000 to  7999]
  ! ----------------------------------------------------------------------
  !
  !! print content of component "poly_name" of a data object
  !! subroutine generates error messages
  SUBROUTINE print_criterion_poly_name ( this )
    !! data object
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! name of function
    CHARACTER (LEN=25) , parameter :: c_upname='print_criterion_poly_name' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM(this%poly_name)
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
       !
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente poly_name - - - - - - - - - - - - - - ',/&
           '# value = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_criterion_poly_name
  !
  !! print content of component "crit_type" of a data object
  !! subroutine generates error messages
  SUBROUTINE print_criterion_crit_type ( this )
    !! data object
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! name of function
    CHARACTER (LEN=26) , parameter :: c_upname='print_criterion_crit_type' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%crit_type
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
       !
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente crit_type  - - - - - - - - - - - - - ',/&
           '# value = ',I10,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_criterion_crit_type
  !
  !! print content of component "crit_depth" of a data object
  !! subroutine generates error messages
  SUBROUTINE print_criterion_crit_depth ( this )
    !! data object
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! name of function
    CHARACTER (LEN=26) , parameter :: c_upname='print_criterion_crit_depth' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%crit_depth
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
       !
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente crit_depth  - - - - - - - - - - - - - ',/&
           '# value = ',G15.8,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_criterion_crit_depth
  !
  !! print content of component "dredge_depth" of a data object
  !! subroutine generates error messages
  SUBROUTINE print_criterion_dredge_depth ( this )
    !! data object
    TYPE (t_criterion) , INTENT(IN) :: this ! 
    !! name of function
    CHARACTER (LEN=28) , parameter :: c_upname='print_criterion_dredge_depth' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%dredge_depth
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
       !
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente dredge_depth  - - - - - - - - - - - - ',/&
           '# value = ',G15.8,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_criterion_dredge_depth
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-methods <<< [ERR_NO = 10000 to 10999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(+)-methods <<< [ERR_NO = 11000 to 11999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(-)-methods <<< [ERR_NO = 12000 to 12999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(*)-methods <<< [ERR_NO = 13000 to 13999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(/)-methods <<< [ERR_NO = 14000 to 14999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>)-methods <<< [ERR_NO = 15000 to 15999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-methods <<< [ERR_NO = 16000 to 16999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(<)-methods <<< [ERR_NO = 17000 to 17999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-methods <<< [ERR_NO = 18000 to 18999]
  ! ----------------------------------------------------------------------
  !
END MODULE l_criterion
! TailOfBaseModule --------------------------------------------------------
