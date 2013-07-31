! -------------------------------------------------------------------------
! HeadOfPackageSteeringModule ---------------------------------------------
!
!! reading input steering data for "dredgesim" from a file
!!
! 
MODULE m_dredgesim_steer
  !
  ! ----------------------------------------------------------------------
  ! [A] base modules with frequently used methods
  ! ----------------------------------------------------------------------
  !
  ! [A.1] base module with global constant values
  USE b_constants, ONLY : Single, Double
  ! [A.2] base module "error"
  USE b_error, ONLY :         &
       ! routines
       no_error, any_error,   &
       setup_error_act
  ! [A.3] base module "files" ------------------------------------------
  USE b_file , ONLY : &
       ! data type
       t_file, &
       ! methods
       new_file, kill_file, print_file, ok_file, &
       auto_file_access_form,  &
       set_file_path_and_name, set_file_type, set_file_unit, set_file_delim, &
       set_file_recl, set_file_status, set_file_action, set_file_position, & 
       get_file_form, get_file_access, get_file_delim, get_file_type ! 
  ! [A.4] base module with type+methods "information concerning files" -
  USE b_io_info, ONLY :    &
       ! routines / interfaces
       set_io_info_file, set_io_info_key, set_io_info_pac, set_io_info_code, &
       get_io_info_file
  !
  ! [A.5] (local) base module type+methods "dredge polygon data"
  USE l_criterion, ONLY :     &
       ! routines
       new_criterion, kill_criterion, print_criterion, ok_criterion, &
       set_criterion_poly_name, set_criterion_crit_depth, set_criterion_dredge_depth, &
       set_criterion_crit_type, &
       get_criterion_poly_name, get_criterion_crit_depth, get_criterion_dredge_depth, &
       get_criterion_crit_type
  USE b_datetime
  !
  ! ----------------------------------------------------------------------
  ! [B] data of the "dredgesim"-package
  ! ----------------------------------------------------------------------
  !
  USE m_dredgesim_data, ONLY : &
       ! constant
       prn_op, trc_op, prn_lun, trc_lun,               & !
       c_undef_ch, c_undef_in, c_undef_re, c_undef_dp, & ! 
       all_errors,                                     & ! 
       max_blo, max_key, max_par, blo, max_key_in_blo, & ! 
       key, pac, max_par_in_key, typ_par_in_key,       & ! 
       input_code,                                     &
       ! Variable
       lex_key, n_blo
  !
  ! ---------------------------------------------------------------------
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] public declarations
  ! ---------------------------------------------------------------------
  !
  !! printing all steering data PRN_LUN
  INTERFACE print_dredgesim_steer
     MODULE PROCEDURE print_dredgesim_steer_d ! 
  END INTERFACE
  !! reading steering data from "dredgesim.dat" by use of the
  !! "dictionary"-package
  INTERFACE read_dredgesim_steer
     MODULE PROCEDURE read_dredgesim_steer_d
  END INTERFACE
  ! 
  !! public interfaces
  PUBLIC :: print_dredgesim_steer
  PUBLIC :: read_dredgesim_steer
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data types, internal data and methods (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] local type definitions
  !
  ! [D.2] constant values (parameters)
  !
  !! name of the module
  CHARACTER (LEN=17), parameter :: c_modname  = 'm_dredgesim_steer' ! 
  !! name of the dictionary-file
  !LEO renamed to DSDICO CHARACTER (LEN=18), parameter :: c_diconame = 'dredgesim_dico.dat' ! 
  CHARACTER (LEN=6), parameter :: c_diconame = 'DSDICO' ! 
  !
  !! definition of implemented file-variants 
  !! number of der implemented variants for reading input steering file
  INTEGER           , parameter :: c_max_variants = 1 ! 
  !! identifier of file-TYPE-variants 
  CHARACTER (LEN=08), parameter :: c_variants_type(c_max_variants) = & ! 
       (/ 'STEERING' /) ! 
  !! identifier of file-CODE-variants (following dirdef1)
  INTEGER, parameter :: c_variants_code(c_max_variants) = & ! 
       (/ 130  /) ! 
  !! identifier of Fortran ACCESS-variants
  CHARACTER (LEN=10), parameter :: c_variants_access(c_max_variants) = & ! 
       (/ 'SEQUENTIAL' /) ! 
  !! identifier of Fortran FORM-variants
  CHARACTER (LEN=11), parameter :: c_variants_form(c_max_variants) = & ! 
       (/ 'FORMATTED  ' /) ! 
  !! identifier of Fortran DELIM-variants
  CHARACTER (LEN=10), parameter :: c_variants_delim(c_max_variants) = & ! 
       (/ 'NONE      ' /) ! 
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
  ! publice methods with access through PUBLIC interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-methods <<< [ERR_NO =  7000 to  7999]
  ! ----------------------------------------------------------------------
  !
  !! print all steering data of the dredgesim package to printer file 
  !! subroutine generates error messages
  SUBROUTINE print_dredgesim_steer_d ( )
    !
    USE m_dredgesim_data, ONLY : &
        steering_file, input_files, output_files, dredge_criterion, &
        dredging_rate,&
        get_nof_dredge_poly !LEO added get_nof_dredge_poly_d 
    !! name of the function
    CHARACTER (LEN=23), parameter :: c_upname='print_dredgesim_steer_d' ! 
    !! variables
    CHARACTER (LEN=10)  :: hlp_char ! 
    CHARACTER (LEN=240) :: aux_char ! 
    INTEGER :: stat ! 
    LOGICAL :: l_ok             !  
    INTEGER :: iblo, jblo, ikey ! 
    INTEGER :: nof_dredge_criterion !LEO added new variable
    !
    IF ( prn_op ) THEN
       ! [1.1] printing headeer -----------------------------------------
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -7001, c_upname, c_modname, stat )
       ! [1.2] printing information about ateering file -----------------
       IF ( no_error( ) ) THEN
          CALL print_file ( steering_file )
       END IF
       ! [1.3] printing steering data -----------------------------------
       IF ( no_error( ) ) THEN
          DO iblo=1,max_blo                  
             IF ( any_error( )  ) EXIT
             DO jblo=1,n_blo(iblo)         
                IF ( any_error( )  ) EXIT
                DO ikey=1,max_key_in_blo(iblo)
                   IF ( any_error() ) EXIT
                   IF ( ANY(lex_key(:,ikey,iblo)) ) THEN
                      WRITE(UNIT=prn_lun,FMT=9000,IOSTAT=stat) &
                           iblo, jblo, ikey, TRIM(blo(iblo)), TRIM(key(ikey,iblo))
                      IF ( stat /= 0 ) THEN
                         CALL setup_error_act ( all_errors(:), -7003, c_upname, c_modname, stat )
                         WRITE(hlp_char,'(I10.10)') iblo
                         CALL setup_error_act ( '<AktIblo>', hlp_char )
                         WRITE(hlp_char,'(I10.10)') ikey
                         CALL setup_error_act ( '<AktIkey>', hlp_char )
                         CALL setup_error_act ( '<AktBloNam>', TRIM(blo(iblo)) )
                         CALL setup_error_act ( '<AktKeyNam>', TRIM(key(ikey,iblo)) )
                      ELSE
                         aux_char = REPEAT( ' ', LEN(aux_char) )
                         SELECT CASE ( iblo )
                         CASE ( 1 ) ! "Input_Files"
                            CALL print_file ( get_io_info_file( input_files(ikey) ) )
                         CASE ( 2 ) ! "Output_Files"
                            WRITE(*,*) ' >>> transfer for output files not realized so far '
                            CALL print_file ( get_io_info_file( output_files(ikey) ) )
                         CASE ( 3 ) ! "Dredge_Criterion"
                            SELECT CASE ( ikey )
                            CASE ( 1 )
                               aux_char = get_criterion_poly_name( dredge_criterion(jblo) )
                            CASE ( 2 )
                               WRITE(aux_char(1:10),'(I10)'  ) get_criterion_crit_type( dredge_criterion(jblo) )
                            CASE ( 3 )
                               WRITE(aux_char(1:15),'(G15.8)') get_criterion_crit_depth( dredge_criterion(jblo) )
                            CASE ( 4 )
                               WRITE(aux_char(1:15),'(G15.8)') get_criterion_dredge_depth( dredge_criterion(jblo) )
                            CASE ( 5 )
                               WRITE(aux_char(1:15),'(G15.8)') dredging_rate( jblo )
                            END SELECT
                         END SELECT
                         IF ( LEN_TRIM(aux_char) > 0 ) WRITE(UNIT=prn_lun,FMT=9010,IOSTAT=stat) TRIM(aux_char)
                         IF ( stat /= 0 ) THEN
                            CALL setup_error_act ( all_errors(:), -7004, c_upname, c_modname, stat )
                            WRITE(hlp_char,'(I10.10)') iblo
                            CALL setup_error_act ( '<AktIblo>'    , hlp_char )
                            WRITE(hlp_char,'(I10.10)') ikey
                            CALL setup_error_act ( '<AktIkey>'    , hlp_char )
                            CALL setup_error_act ( '<AktBloNam>'  , TRIM(blo(iblo)) )
                            CALL setup_error_act ( '<AktKeyNam>'  , TRIM(key(ikey,iblo)) )
                            CALL setup_error_act ( '<AktKeyValue>', TRIM(aux_char) )
                         END IF
                      END IF
                   END IF
                END DO
             END DO
          END DO
       END IF
       ! [1.4] printing footer
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8900, IOSTAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), -7002, c_upname, c_modname, stat )
       END IF
    ELSE
       WRITE(*,*) ' >>> subroutine '//TRIM(c_upname)//' - no printable output '
    END IF
    !
    !LEO TODO check dredge_criterion / ok_criterion function must be repared only if dredge_criterion
    !exists
    !BUG check before call if dredge_criterion is allocated
    nof_dredge_criterion = get_nof_dredge_poly()
    IF (nof_dredge_criterion .GT. 0) THEN
      l_ok = ALL( ok_criterion( dredge_criterion ) )
    ELSE
      l_ok = .TRUE.
    END IF 
    !
8000 FORMAT( &
          '#----------------------------------------------------------------',/ &
          '# START: list of current steering data for p_dredgesim_ui ',/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
          '# ... in the beginning some information about the steering file ',/ &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -')
8900 FORMAT( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
          '# END: list of current steering data for p_dredgesim_ui  ',/ &
          '#----------------------------------------------------------------')
9000 FORMAT( &
          '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
          '# block number ',I2,', Instanz = ',I2,', Key-Nummer ',I2,' : ',/  &
          '# current block name = ',A,/&
          '# current key name   = ',A,/&
          '# +++ following lines contain detailled information ')
9010 FORMAT( &
          '# current value       = ',A)
    !
  END SUBROUTINE print_dredgesim_steer_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-TEST-methods <<< [ERR_NO = 19000 to 19999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-READ-methods <<< [ERR_NO = 23000 to 23999]
  ! ----------------------------------------------------------------------
  !
  !! transfer of data from steering file to data module in "dredgesim"
  !! subroutine generates error messages
  SUBROUTINE read_dredgesim_steer_d ( )
    !
    USE m_dredgesim_data, ONLY : steering_file ! 
    !
    CALL read_dredgesim_steer_0 ( steering_file )
    !
  END SUBROUTINE read_dredgesim_steer_d
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
  ! local methods (PRIVATE)
  !
  ! -----------------------------------------------------------------------------
  ! >>> module specific PRIVATE-READ-methods <<< [ERR_NO = -23000 to -23999]
  ! -----------------------------------------------------------------------------
  !
  !! reading steering data from file "this" (steering file) 
  !! subroutine generates error messages
  SUBROUTINE read_dredgesim_steer_0 ( this )
    !! file with steering data
    TYPE (t_file) , INTENT(INOUT) :: this ! 
    !! name of the routine
    CHARACTER (LEN=22) , parameter :: c_upname='read_dredgesim_steer_0' ! 
    !
    IF ( ok_file ( this ) ) THEN
       IF ( ok_dredgesim_steer_var_no ( this ) ) THEN
          SELECT CASE ( get_dredgesim_steer_var_no ( this ) )
          CASE ( 1 )
             CALL read_dredgesim_steer_v1_0 ( this )
          CASE DEFAULT
             WRITE(*,*) ' *** warning *** "read_dredgesim_steer_0" needs to be extended '
          END SELECT ! hint: number of CASEs must correspond with "c_max_variants"
       END IF
    END IF
    !
  END SUBROUTINE read_dredgesim_steer_0
  ! 
  !! reading data from steering file "this" (file-variant 1) 
  !! subroutine generates error messages
  SUBROUTINE read_dredgesim_steer_v1_0 ( this )
    ! USE-statements
    USE p_dictionary_ui, ONLY :                              &
         !   routines / interfaces
         init_dictionary, clear_dictionary,                  &
         setup_dictionary_prn_lun, setup_dictionary_trc_lun, &
         read_input_file, get_nof_input_blocks,              & 
         get_nof_input_lines, get_input_data
    !
    USE m_dredgesim_data, ONLY : &
         ! data
         input_files, output_files, dredge_criterion, &
	 dredging_rate, alloc_dredging_rate, disposal_rate, alloc_disposal_rate, &
	 nof_dispose_poly, dispose_poly_name, dispose_weighting_factor, &
	 alloc_dispose_poly_name, alloc_dispose_weighting_fac, &
	 nof_dredge_poly_tc, nof_dispose_poly_tc, alloc_dredge_poly_name_tc, dredge_poly_name_tc, &
	 alloc_dispose_poly_name_tc, dispose_poly_name_tc, alloc_dredge_sed_vol_tc, dredge_sed_vol_tc, &
	 alloc_dispose_sed_vol_tc, dispose_sed_vol_tc, alloc_predef_dredge_time_tc, predef_dredge_time_tc, &
	 alloc_predef_disp_time_tc, predef_disp_time_tc, alloc_dredge_time_tc, dredge_time_tc, &
	 alloc_disposal_time_tc, disposal_time_tc, &
	 nof_predef_disp_poly, alloc_predef_disp_poly_name, predef_disp_poly_name, &
	 alloc_predef_disp_sed_class, predef_disp_sed_class, &
	 alloc_predef_disp_sed_vol, predef_disp_sed_vol, &
	 alloc_predef_sed_distrib, predef_sed_distrib, &
	 alloc_predef_depos_time, predef_depos_time, &
	 alloc_art_bl_time, art_bl_time,&
	 observing_period, alloc_observing_period, &
	 limiting_discharge, alloc_limiting_discharge, &
         navigation_possible, alloc_navigation_possible, &
         minimum_volume, alloc_minimum_volume, &
         sector_radius, alloc_sector_radius, &
         disposing_scours, nof_iterations_disp_scours, max_error_disp_scours, min_depo_depth_disp_scours, &
         alloc_ini_obs_time, alloc_ini_time_to_observe, ini_obs_time, initial_time_to_observe, &
         disp_scours_auto, alloc_disp_scours_auto, disp_scours_tc, alloc_disp_scours_tc, disp_scours_abl, alloc_disp_scours_abl, &
         restart, write_ds_messages, alloc_list_of_disp_polys, list_of_disp_polys
	 !
    !! data object
    TYPE (t_file) , INTENT(INOUT) :: this ! 
    !! name of the routine
    CHARACTER (LEN=25) , parameter :: c_upname='read_dredgesim_steer_v1_0' !
    !
    !! current number of keywords
    INTEGER             :: n_lin(max_key,max_blo) ! 
    !! variables
    CHARACTER (LEN= 10) :: hlp_ch                 ! 
    TYPE (t_file)       :: l_file                 ! 
    LOGICAL             :: lex                          ! 
    INTEGER             :: iblo, jblo, ikey, ilin, ipar, nn, n_b , n_l, n_p, check_no! 
    CHARACTER (LEN=240)           :: var_ch, test_ch    ! 
    CHARACTER (LEN=240) , POINTER :: arr_ch(:) ! 
    LOGICAL                       :: var_lo    ! 
    LOGICAL             , POINTER :: arr_lo(:) ! 
    INTEGER                       :: var_in    ! 
    INTEGER             , POINTER :: arr_in(:) ! 
    REAL                          :: var_re    ! 
    REAL                , POINTER :: arr_re(:) ! 
    REAL (KIND=Double)            :: var_dp, test_dp    ! 
    REAL (KIND=Double)  , POINTER :: arr_dp(:) ! 
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! [1.1] initialization and setup
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    IF ( no_error( ) ) CALL init_dictionary( )
    IF ( no_error( ) ) CALL setup_dictionary_prn_lun( prn_lun )
    IF ( no_error( ) ) CALL setup_dictionary_trc_lun( trc_lun )
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! [1.2] reading and checking data
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    IF ( no_error( ) ) CALL read_input_file ( TRIM( c_diconame ), this )
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! [1.3] transfer of data to "m_dataconvert_data"
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! [1.3.1] initializations
    n_blo(:)       = 0
    n_lin(:,:)     = 0
    lex_key(:,:,:) = .false.
    check_no       = 0
    ! [1.3.2] number of identical blocks
    DO iblo=1,max_blo
       IF ( any_error ( ) ) THEN
           EXIT
       ELSE
           CALL get_nof_input_blocks ( blo(iblo), n_blo(iblo) )
       END IF 
    END DO
    ! [1.3.3] transfering data to "m_dredgesim_data"
    DO iblo=1,max_blo                    ! number of different blocks
       IF ( any_error( )  ) EXIT
       DO jblo=1,n_blo(iblo)             ! occurence of each block
          IF ( any_error( )  ) EXIT
          DO ikey=1,max_key_in_blo(iblo) ! different keys 
             CALL get_nof_input_lines ( blo(iblo),jblo,key(ikey,iblo),n_lin(ikey,iblo) )
             IF ( any_error( ) ) EXIT
             DO ilin=1,n_lin(ikey,iblo)             ! all lines
                CALL new_file      ( l_file )
                IF ( any_error( )  ) EXIT
                DO ipar=1,max_par_in_key(ikey,iblo) ! all parameters of each key
                   IF ( any_error( )  ) EXIT
                   SELECT CASE ( typ_par_in_key(ipar,ikey,iblo) )
                   CASE (  1 ) ! Character-scalar - - - - - - - - - - - - - - - - - - - - -
                      var_ch = REPEAT( c_undef_ch, LEN(var_ch) ) ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           var_ch, lex )
                   CASE (  2 ) ! Integer-scalar - - - - - - - - - - - - - - - - - - - - - -
                      var_in = c_undef_in ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           var_in, lex )
                   CASE (  3 ) ! Real-scalar  - - - - - - - - - - - - - - - - - - - - - - -
                      var_re = c_undef_re ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           var_re, lex )
                   CASE (  4 ) ! Double-scalar  - - - - - - - - - - - - - - - - - - - - - -
                      var_dp = c_undef_dp ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           var_dp, lex )
                   CASE (  5 ) ! Logical-scalar - - - - - - - - - - - - - - - - - - - - - -
                      var_lo = .false. ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           var_lo, lex )
                   CASE ( 11 ) ! Character-array - - - - - - - - - - - - - - - - - - - - - -
                      NULLIFY( arr_ch ) ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           arr_ch, lex )
                   CASE ( 12 ) ! Integer-array - - - - - - - - - - - - - - - - - - - - - - -
                      NULLIFY( arr_in ) ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           arr_in, lex )
                   CASE ( 13 ) ! Real-array  - - - - - - - - - - - - - - - - - - - - - - - -
                      NULLIFY( arr_re ) ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           arr_re, lex )
                   CASE ( 14 ) ! Double-array  - - - - - - - - - - - - - - - - - - - - - - -
                      NULLIFY( arr_dp ) ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           arr_dp, lex )
                   CASE ( 15 ) ! Logical-array - - - - - - - - - - - - - - - - - - - - - - -
                      NULLIFY( arr_lo ) ; lex = .false.
                      CALL get_input_data &           
                           ( blo(iblo), jblo, key(ikey,iblo), ilin, ipar, &
                           arr_lo, lex ) 
                   CASE DEFAULT !                 - - - - - - - - - - - - - - - - - - - - -
                      CALL setup_error_act ( all_errors(:), -23000, c_upname, c_modname )
                      CALL setup_error_act ( '<AktBlockName>', TRIM(blo(iblo)) )
                      CALL setup_error_act ( '<AktKeyName>'  , TRIM(key(ikey,iblo)) )
                      WRITE(hlp_ch,'(I10.10)') ipar
                      CALL setup_error_act ( '<AktParamNo>', hlp_ch )
                      WRITE(hlp_ch,'(I10.10)') typ_par_in_key(ipar,ikey,iblo)
                      CALL setup_error_act ( '<AktParamTyp>', hlp_ch )
                   END SELECT
                   IF ( lex ) lex_key(ipar,ikey,iblo) = .true.
                   IF ( no_error( ) .AND. lex ) THEN
                      SELECT CASE ( iblo )
                      CASE ( 1, 2 ) ! Block "Input_Files" / Block "Output_Files"
                         SELECT CASE ( ipar )
                         CASE ( 1 ) ! "<FileName>"
                            CALL set_file_path_and_name ( l_file, TRIM(var_ch) )
                         CASE ( 2 ) ! "<FileType>"
                            CALL set_file_type ( l_file, TRIM(var_ch) )
                            CALL auto_file_access_form ( l_file )
                            CALL set_file_unit   ( l_file, 0 )
                            CALL set_file_recl   ( l_file, 1 )
                            SELECT CASE ( iblo )
                            CASE ( 1 )
                               CALL set_file_status ( l_file, 'OLD' )
                               CALL set_file_action ( l_file, 'READ' )
                            CASE ( 2 )
                               CALL set_file_status ( l_file, 'UNKNOWN' )
                               CALL set_file_action ( l_file, 'WRITE' )
                               CALL set_file_position ( l_file, 'APPEND' )
                            END SELECT
                         END SELECT
                      END SELECT
                   END IF
                END DO
                ! transfering parameters and self defined data in data arrays
                IF ( ANY( lex_key(:,ikey,iblo) ) ) THEN
                   SELECT CASE ( iblo )
                   CASE ( 1 ) ! "Input_Files"
                      CALL set_io_info_file ( input_files(ikey), l_file )
                      CALL set_io_info_key  ( input_files(ikey), key(ikey,iblo) )
                      CALL set_io_info_pac  ( input_files(ikey), pac(ikey,iblo) )
                      nn = COUNT( input_code(:,ikey) > 0 )
                      IF ( nn > 0 ) CALL set_io_info_code( input_files(ikey), input_code(1:nn,ikey) )
                      IF (ikey==3) restart=.true.
                      IF (ikey==4) THEN
                      	 CALL get_input_data ( blo(iblo), iblo, key(ikey,iblo), n_lin(ikey, iblo), 1, var_ch, lex)
                      	 IF (TRIM(var_ch) == 'YES') write_ds_messages = .true.
                      END IF
                   CASE ( 2 ) ! "Output_Files"
                      CALL set_io_info_file ( output_files(ikey), l_file )
                      CALL set_io_info_key  ( output_files(ikey), key(ikey,iblo) )
                      CALL set_io_info_pac  ( output_files(ikey), pac(ikey,iblo) )
                   END SELECT
                END IF
                CALL kill_file      ( l_file )
             END DO
             ! transfering keys in data arrays
             IF ( ANY( lex_key(:,ikey,iblo) ) ) THEN
                SELECT CASE ( iblo )
                CASE ( 3 ) ! "Dredge_Criterion"
                   IF ( jblo == 1 .AND. ikey == 1 ) THEN
                      ALLOCATE( dredge_criterion(n_blo(iblo)) )
                      CALL new_criterion( dredge_criterion )					  
                   END IF
                   SELECT CASE ( ikey )
                   CASE ( 1 ) ! name of dredge polygon
                      CALL set_criterion_poly_name( dredge_criterion(jblo), TRIM(var_ch) )
                   CASE ( 2 ) ! type of dredge polygon
                      SELECT CASE ( var_ch)
                      CASE ( 'Bottom_Criterion' )
                         CALL set_criterion_crit_type( dredge_criterion(jblo), 1 )
                      CASE ( 'Water_Depth_Criterion' )
                         CALL set_criterion_crit_type( dredge_criterion(jblo), 2 )
                      CASE ( 'Referenced_Surface_Criterion' )
                         CALL set_criterion_crit_type( dredge_criterion(jblo), 3 )
                      END SELECT
                   CASE ( 3 ) ! Observing Period
                      CALL alloc_observing_period(n_blo(iblo))
                      DO n_b = 1, n_blo(iblo)
                         CALL get_input_data ( blo(iblo), n_b, key(ikey,iblo), n_lin(ikey, iblo), 1, var_in, lex)
                         observing_period(n_b)=var_in
                      END DO
                   CASE ( 4 ) ! Initial Time To Observe
                      CALL alloc_ini_obs_time(n_blo(iblo))
                      CALL alloc_ini_time_to_observe(n_blo(iblo))
                      DO n_b = 1, n_blo(iblo)
                         CALL get_input_data ( blo(iblo), n_b, key(ikey,iblo), n_lin(ikey, iblo), 1, var_ch, lex)
			 ini_obs_time(n_b)=TRIM(var_ch)
			 initial_time_to_observe(n_b)=string_to_datetime(ini_obs_time(n_b))              
		      END DO
                   CASE ( 5 ) ! Limiting Discharge
                      CALL alloc_limiting_discharge(n_blo(iblo))
                      CALL alloc_navigation_possible(n_blo(iblo))
                      DO n_b = 1, n_blo(iblo)
                         CALL get_input_data ( blo(iblo), n_b, key(ikey,iblo), n_lin(ikey, iblo), 1, var_dp, lex)
                         limiting_discharge(n_b)=var_dp
                      END DO
                   CASE ( 6 ) ! kritische Tiefe
                      CALL set_criterion_crit_depth( dredge_criterion(jblo), var_dp )					  
                   CASE ( 7 ) ! Baggertiefe
                      CALL set_criterion_dredge_depth( dredge_criterion(jblo), var_dp )					  
                   CASE ( 8 )  ! Baggerrate
                      CALL alloc_dredging_rate(n_blo(iblo))
                      DO n_b = 1, n_blo(iblo)
                         CALL get_input_data ( blo(iblo), n_b, key(ikey,iblo), n_lin(ikey, iblo), 1, var_dp, lex)
                         dredging_rate(n_b)=var_dp
                      END DO
                   CASE ( 9 )  ! Minimum Volume
                      CALL alloc_minimum_volume(n_blo(iblo))
                      CALL alloc_sector_radius(n_blo(iblo))
                      DO n_b = 1, n_blo(iblo)
                         CALL get_input_data ( blo(iblo), n_b, key(ikey,iblo), n_lin(ikey, iblo), 1, var_dp, lex)
                         minimum_volume(n_b)=var_dp
                         CALL get_input_data ( blo(iblo), n_b, key(ikey,iblo), n_lin(ikey, iblo), 2, var_dp, lex)
                         sector_radius(n_b)=var_dp
                      END DO
                   CASE ( 10 ) ! Verklapppolygon
                      IF (nof_dispose_poly < n_lin(ikey, iblo)) nof_dispose_poly=n_lin(ikey, iblo)
                      check_no=check_no+n_lin(ikey, iblo)
!                   CASE DEFAULT
!                      !Mehr Eingabezeilen gehen nicht
!                      CALL setup_error_act ( all_errors(:), -8002, c_upname, c_modname )
!                      CALL setup_error_act ( '<key_name>' , TRIM(key(ikey,iblo)) )
!                      CALL setup_error_act ( '<blockname>', TRIM(blo(iblo)     ) )
                   END SELECT
                END SELECT
                SELECT CASE ( iblo )
		CASE ( 4 ) ! "Time_Controlled_Maintenance"
		   IF ( jblo == 1 .AND. ikey == 1 ) THEN
                      nof_dredge_poly_tc=n_blo(iblo)
                      CALL alloc_dredge_poly_name_tc(nof_dredge_poly_tc)
                      CALL alloc_predef_dredge_time_tc(nof_dredge_poly_tc)
                      CALL alloc_dredge_time_tc(nof_dredge_poly_tc)
                      CALL alloc_dredge_sed_vol_tc(nof_dredge_poly_tc)
                      CALL alloc_disp_scours_tc(nof_dredge_poly_tc)
                   END IF
                   IF (jblo == 1 .AND. ikey == 4 ) THEN
                      nof_dispose_poly_tc=n_blo(iblo)
                      CALL alloc_dispose_poly_name_tc(nof_dispose_poly_tc)
                      CALL alloc_predef_disp_time_tc(nof_dispose_poly_tc)
                      CALL alloc_disposal_time_tc(nof_dispose_poly_tc)
                      CALL alloc_dispose_sed_vol_tc(nof_dispose_poly_tc)					  					  				  
                   END IF
                   SELECT CASE ( ikey )
                   CASE ( 1 ) ! name of dredge polygon for time controlled dredging
                      dredge_poly_name_tc(jblo)=TRIM(var_ch)
                   CASE ( 2 ) ! date and time of dredging
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 1, var_ch, lex)
                      predef_dredge_time_tc(jblo,1)=TRIM(var_ch)
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 2, var_ch, lex)
                      predef_dredge_time_tc(jblo,2)=TRIM(var_ch)
                      dredge_time_tc(jblo,:) = string_to_datetime ( predef_dredge_time_tc(jblo,:) )
                   CASE ( 3 ) ! total dredged volume for time controlled dredging				  
                      dredge_sed_vol_tc(jblo)=var_dp
                   CASE ( 4 ) ! name of disposal polygon for time controlled disposal
                      dispose_poly_name_tc(jblo)=TRIM(var_ch)
                   CASE ( 5 ) ! date and time of disposal
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 1, var_ch, lex)
                      predef_disp_time_tc(jblo,1)=TRIM(var_ch)
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 2, var_ch, lex)
                      predef_disp_time_tc(jblo,2)=TRIM(var_ch)
                      disposal_time_tc(jblo,:) = string_to_datetime ( predef_disp_time_tc(jblo,:) )
                   CASE ( 6 ) ! sediment volume to dispose by time controlled disposal				  
                      dispose_sed_vol_tc(jblo)=var_dp
                   CASE ( 7 ) ! disposing in scours or not				  
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 1, var_ch, lex)
                      disp_scours_tc(jblo)=TRIM(var_ch)
                   END SELECT
                END SELECT
                SELECT CASE ( iblo )
                CASE ( 5 ) ! "Artificial_Bed_Load_Supply"
                   IF ( jblo == 1 .AND. ikey == 1 ) THEN
                      nof_predef_disp_poly=n_blo(iblo)
                      CALL alloc_predef_disp_poly_name(nof_predef_disp_poly)
                      CALL alloc_predef_disp_sed_vol(nof_predef_disp_poly)
                      CALL alloc_predef_depos_time(nof_predef_disp_poly)
                      CALL alloc_art_bl_time(nof_predef_disp_poly)
                      CALL alloc_disp_scours_abl(nof_predef_disp_poly)				  
                   END IF
                   SELECT CASE ( ikey )
                   CASE ( 1 ) ! name of disposal polygon for artificial bed load supply
                      predef_disp_poly_name(jblo)=TRIM(var_ch)
                   CASE ( 2 ) ! reading sediment classes to dispose
                      WRITE(*,*) "==============================================="
                      WRITE(*,*) "==============================================="
                      WRITE(*,*) "==============================================="
                      WRITE(*,*) " LEODEBUG ---- call alloc_predef_disp_sed_class"
                      WRITE(*,*) "==============================================="
                      WRITE(*,*) "==============================================="
                      WRITE(*,*) "==============================================="
                      WRITE(*,*) "==============================================="
                      !TODO LEO if case (2) is not called, alloc_predef_sed_distrib()
                      !get's not called and predef_disp_sed_class(:,:) get's not allocated
                      !However it get's called later from prepare_dredgesim -> ext_ds_fraction_name_d
                      !-> set_ds_used_sediment_classes -> predef_disp_sed_class
                      CALL alloc_predef_disp_sed_class(n_blo(iblo), n_lin(ikey,iblo))
                      CALL alloc_predef_sed_distrib(n_blo(iblo), n_lin(ikey,iblo))					  
                      DO n_b = 1, n_blo(iblo)
                         DO n_l = 1, n_lin(ikey, iblo)
                            var_ch = REPEAT( c_undef_ch, LEN(var_ch) ); lex = .false.
                            CALL get_input_data ( blo(iblo), n_b, key(ikey,iblo), n_l, 1, var_ch, lex)
                            CALL get_input_data ( blo(iblo), n_b, key(ikey,iblo), n_l, 2, var_dp, lex)
                            predef_disp_sed_class(n_b , n_l)=TRIM(var_ch)
                            predef_sed_distrib(n_b , n_l)=var_dp
                          END DO
                      END DO
		   CASE ( 3 ) ! sediment volume to dispose by artificial bed load supply				  
                      predef_disp_sed_vol(jblo)=var_dp
                   CASE ( 4 ) ! date and time of artificial bed load supply				  
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 1, var_ch, lex)
                      predef_depos_time(jblo,1)=TRIM(var_ch)
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 2, var_ch, lex)
                      predef_depos_time(jblo,2)=TRIM(var_ch)
                      art_bl_time(jblo,:) = string_to_datetime ( predef_depos_time(jblo,:) )
                    CASE ( 5 ) ! disposing in scours or not				  
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 1, var_ch, lex)
                      disp_scours_abl(jblo)=TRIM(var_ch)
                    END SELECT
                END SELECT
                SELECT CASE ( iblo )
                CASE ( 6 ) ! "Disposing_Scours"
!                   IF ( jblo == 1 .AND. ikey == 1 ) disposing_scours=.true.
                   SELECT CASE ( ikey )
                   CASE ( 1 ) ! Number of iterations available for disposing scours
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 1, var_in, lex)
                      nof_iterations_disp_scours=var_in
                   CASE ( 2 ) ! Maximum error allowed when disposing scours
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 1, var_dp, lex)
                      max_error_disp_scours=var_dp
                   CASE ( 3 ) ! Maximum deposition depth when disposing scours				  
                      CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), 1 , 1, var_dp, lex)
                      min_depo_depth_disp_scours=var_dp
                   END SELECT
                END SELECT
             END IF
          END DO
       END DO
    END DO
    !NEW: Restricted-free definition of disposal polygons...
    !...Define as much as you want
    !Allocation and initialization of disposal fields
    IF (nof_dispose_poly >= 1) THEN
       CALL alloc_dispose_poly_name(nof_dispose_poly,SIZE(dredge_criterion))
       CALL alloc_dispose_weighting_fac(nof_dispose_poly,SIZE(dredge_criterion))
       dispose_poly_name='not_defined'
       dispose_weighting_factor=0.0_Double
       CALL alloc_disposal_rate(SIZE(dredge_criterion))
       disposal_rate=0.0_Double
       CALL alloc_disp_scours_auto(SIZE(dredge_criterion))
       disp_scours_auto='NO'
    END IF
!    PRINT*, 'check_no', check_no
    !Filling of disposal fields with data from the steering file
    DO iblo=1,max_blo                    
       IF ( any_error( )  ) EXIT
       DO jblo=1,n_blo(iblo)             
          IF ( any_error( )  ) EXIT
          DO ikey=1,max_key_in_blo(iblo) ! alle verschiedenen Keys
             CALL get_nof_input_lines ( blo(iblo),jblo,key(ikey,iblo),n_lin(ikey,iblo) )
             IF ( ANY( lex_key(:,ikey,iblo) ) ) THEN
                SELECT CASE ( iblo )
                CASE ( 3 ) ! "Dredge_Criterion"
                   SELECT CASE ( ikey )
                   CASE ( 10 ) ! Verklappdaten
                     DO n_l = 1, n_lin(ikey, iblo)
                        CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), n_l, 1, var_ch, lex)
                        CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), n_l, 2, var_dp, lex)
                        IF (lex) dispose_poly_name(n_l , jblo)=TRIM(var_ch)
                        IF (lex) dispose_weighting_factor(n_l, jblo)=var_dp							   
                     END DO
                   CASE ( 11 )  ! Verklapprate
                     CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), n_lin(ikey, iblo), 1, var_dp, lex)
                     IF (lex) disposal_rate(jblo)=var_dp							   
                   CASE ( 12 ) ! disposing in scours or not				  
                     CALL get_input_data ( blo(iblo), jblo, key(ikey,iblo), n_lin(ikey, iblo), 1, var_ch, lex)
                     IF (lex) disp_scours_auto(jblo)=TRIM(var_ch)                     
                   END SELECT
                END SELECT
             END IF
          END DO
	END DO
    END DO
    !
    !! allocating list of dispose poly names
    IF (check_no >= 1) THEN
       CALL alloc_list_of_disp_polys(check_no)
       list_of_disp_polys='no_definition'
    END IF
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! [1.4] deinitializing
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    IF ( no_error( ) ) CALL clear_dictionary( )
    !
  END SUBROUTINE read_dredgesim_steer_v1_0
  !
  ! -----------------------------------------------------------------------------
  ! >>> module specific PRIVATE-WRITE-methods <<< [ERR_NO = -24000 to -24999]
  ! -----------------------------------------------------------------------------
  !
  ! -------------------------------------------------------------------------
  ! PRIVATE-methods of implemented file-variants
  ! -------------------------------------------------------------------------
  !
  !! determine variant-number for file "this" 
  !! function does not throw error messages
  FUNCTION get_dredgesim_steer_var_no ( this ) &
       RESULT( ivar )
    ! USE-statements
    USE b_file, ONLY :    &
         !   type definitions
         t_file,          &
         !   routines / interfaces
         get_file_form, get_file_access, get_file_delim, get_file_type ! 
    !! file 
    TYPE (t_file) , INTENT(IN) :: this ! 
    !! result: number of current variant 
    !! -1 : not existent
    INTEGER :: ivar ! 
    !! name of the function
    CHARACTER (LEN=26) , parameter :: c_upname='get_dredgesim_steer_var_no' ! 
    !! variables
    LOGICAL :: l_ok(4) ! 
    INTEGER :: i       ! 
    !
    ivar = -1
    i    = 0
    !
    DO
       i = i + 1
       IF ( i > c_max_variants .OR. ivar /= -1 .OR. any_error ( ) ) EXIT
       l_ok(1) = ( TRIM( get_file_form  ( this ) ) == TRIM( c_variants_form(i)   ) ) 
       l_ok(2) = ( TRIM( get_file_access( this ) ) == TRIM( c_variants_access(i) ) ) 
       l_ok(3) = ( TRIM( get_file_delim ( this ) ) == TRIM( c_variants_delim(i)  ) ) 
       l_ok(4) = ( TRIM( get_file_type  ( this ) ) == TRIM( c_variants_type(i)   ) ) 
       ivar = MERGE ( i, -1, ALL( l_ok(:) ) )
    END DO
    !
  END FUNCTION get_dredgesim_steer_var_no
  !
  !! check if file-variant is implemented 
  !! function generates error messages
  FUNCTION ok_dredgesim_steer_var_no ( this ) &
       RESULT( ok )
    ! USE-statements
    USE b_file, ONLY :    &
         !   type definitions
         t_file,          &
         !   routines / interfaces
         get_file_type, get_file_form, get_file_access, get_file_delim
    !! file 
    TYPE (t_file) , INTENT(IN) :: this ! 
    !! result: required file-variant is implemented 
    LOGICAL :: ok ! 
    !! name of the function
    CHARACTER (LEN=25) , parameter :: c_upname='ok_dredgesim_steer_var_no' ! 
    !
    ok = ( get_dredgesim_steer_var_no ( this ) > 0 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), -6001, c_upname, c_modname )
       CALL setup_error_act ( '<FortranFileType>'  , get_file_type  ( this ) )
       CALL setup_error_act ( '<FortranFileForm>'  , get_file_form  ( this ) )
       CALL setup_error_act ( '<FortranFileAccess>', get_file_access( this ) )
       CALL setup_error_act ( '<FortranFileDelim>' , get_file_delim ( this ) )
    END IF
    !
  END FUNCTION ok_dredgesim_steer_var_no
  !
END MODULE m_dredgesim_steer
! TailOfPackageUserInterface -----------------------------------------------
