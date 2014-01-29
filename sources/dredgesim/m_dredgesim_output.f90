! -------------------------------------------------------------------------
! HeadOfPackageOutputModule -----------------------------------------------
!
!! OUTPUT-methods for "DredgeSim"
!!
MODULE m_dredgesim_output
  !
  ! ----------------------------------------------------------------------
  ! [A] base modules with frequently used methods
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] base module with global constant values ----------------------
  USE b_constants, ONLY : Single, Double
  ! [A.1.2] base module "errors" -----------------------------------------
  USE b_error, ONLY : &
       ! routines
       no_error, any_error, &
       setup_error_act
  ! [A.1.3] base module "file handling" --------------------------------
  USE b_file, ONLY : & ! 
       ! data type
       t_file, &
       ! methods
       open_file, close_file, set_file_unit, set_file_status, get_file_unit, &
       file_is_none
  ! [A.1.4] base module "date and time" -----------------------
  USE b_datetime
  ! [A.1.5] base module "information concerning files" -----------------
  USE b_io_info, ONLY : &
       ! methods
       get_io_info_file
  !
  ! [A.2.1] (local) base module type+methods "dredge polygon data" -------
  USE l_criterion, ONLY : &
       ! methods
       get_criterion_poly_name, get_criterion_crit_type
  !
  ! ----------------------------------------------------------------------
  ! [B] modules of the "dredgesim"-package
  ! ----------------------------------------------------------------------
  !
  ! [B.1] data module of the "dredgesim"-package 
  USE m_dredgesim_data, ONLY : &
       ! variables und constant values
       prn_op, trc_op, prn_lun, trc_lun, all_errors, &
       act_time, output_files, dredge_criterion, &
       total_volume, sediment_volume, water_volume, &
       fraction_name, fraction_volume, &
       ! methods
       get_nof_dredge_poly, get_nof_sediment_fraction, get_nof_nodes, &
       node_coord, end_dredge_time, &
       all_total_volume, all_sediment_volume, all_water_volume, all_fraction_volume, &
       all_total_volume_old, all_sediment_volume_old, all_water_volume_old, all_fraction_volume_old, &
       upd_out_volumes, &
       delta_dredge_node_depth, aim_node_depth, delta_disp_node_depth, disp_node_depth, &
       dispose_poly_name, node_sediment_volume_rs, nof_dispose_poly, node_fraction_volume_rs, &
       dredged_sediment_volume_to_disp, dredged_fraction_volume_to_disp, &
       disp_total_volume, disp_sediment_volume, disp_fraction_volume, &
       disp_total_volume_old, disp_sediment_volume_old, disp_fraction_volume_old, list_of_disp_polys, &
       initial_time_to_observe, observing_period, dredge_time_tc, disposal_time_tc, nof_dredge_poly_tc, &
       sediment_volume_tc, fraction_volume_tc, dispose_poly_name_tc, dredge_poly_name_tc, &
       last_obs_time_rs, next_obs_time_rs, last_act_time_rs, ini_time_rs, dispose_node_index, node_total_volume
  !  
  IMPLICIT NONE
  PRIVATE
  !
  DOUBLE PRECISION P_DSUM
  EXTERNAL P_DSUM
  ! ---------------------------------------------------------------------
  ! [C] public declarations
  ! ---------------------------------------------------------------------
  !
  !! output of dredge results on protocol file
  INTERFACE write_calculated_dredged_volume
     MODULE PROCEDURE write_calculated_dredged_vol_d
  END INTERFACE
  INTERFACE write_final_dredged_volume
     MODULE PROCEDURE write_final_dredged_vol_d
  END INTERFACE
  INTERFACE write_final_disposed_volume
     MODULE PROCEDURE write_final_disposed_vol_d
  END INTERFACE
  INTERFACE write_dredged_volumes_pop
     MODULE PROCEDURE write_dredged_volumes_pop_d
  END INTERFACE
  INTERFACE write_disposed_volumes_pop
     MODULE PROCEDURE write_disposed_volumes_pop_d
  END INTERFACE
  INTERFACE write_restart_data
     MODULE PROCEDURE write_restart_data_d
  END INTERFACE
  !
  PUBLIC :: write_calculated_dredged_volume ! 
  PUBLIC :: write_final_dredged_volume !
  PUBLIC :: write_final_disposed_volume !
  PUBLIC :: write_dredged_volumes_pop !
  PUBLIC :: write_disposed_volumes_pop !
  PUBLIC :: write_restart_data !
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data types, internal data and methods
  ! ---------------------------------------------------------------------
  !
  !! name of the module
  CHARACTER (LEN=18), PARAMETER :: c_modname  = 'm_dredgesim_output' ! 
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
  !! output of dredged volumes on protocol file  
  !! subroutine generates error messages           
  SUBROUTINE write_calculated_dredged_vol_d ( )
    !    
    !! name of subroutine
    CHARACTER (LEN=30) , PARAMETER :: c_upname='write_calculated_dredged_vol_d' !
    ! variables
    INTEGER       :: i, n, unit, stat ! 
    TYPE (t_file) :: l_file ! 
    !
    l_file = get_io_info_file(output_files(1))
    !
    IF ( .NOT. file_is_none( l_file ) ) THEN
       !
       CALL set_file_unit( l_file, 0 )
       CALL open_file( l_file )
       !
       unit = get_file_unit( l_file )
       stat = 0
       !
       DO n=1,get_nof_dredge_poly( )
          IF ( stat /= 0 ) EXIT
          IF (total_volume(n)/= 0.0_Double .AND. upd_out_volumes(n)) THEN
             WRITE (unit, 9000, IOSTAT=stat) &
                  datetime_to_string(end_dredge_time(n)), &
                  get_criterion_poly_name(dredge_criterion(n)), &
!                  TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                  get_criterion_crit_type(dredge_criterion(n)), &
                  total_volume(n), sediment_volume(n), water_volume(n)
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<print>', 'Baggerzeitpunkt ...' )
             ELSE
                DO i=1,get_nof_sediment_fraction( )
                   IF ( stat /= 0 ) EXIT
                   WRITE (unit,9010,IOSTAT=stat) fraction_volume(i,n), fraction_name(i)
!                   WRITE (unit,9010,IOSTAT=stat) fraction_volume(i,n), TRIM(fraction_name(i))
                END DO
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<print>', 'Anteil der Kornfraktionen' )
                ELSE
                   WRITE (unit,*) ' '
                END IF
             END IF
          END IF
       END DO
       !
       CALL close_file( l_file )
       !
    END IF
    !
    ! format statements -------------------------------------------------
    !
9000 FORMAT (  & 
          ' ', / & 
          ' dredging time               : ',A, / &
          ' dredge polygon name         : ',A, / &
          ' dredge criterion type       : ',I2,/ &
          ', [ 1 = bottom depth, 2 = water depth, 3 = depth related to referenced free surface ]', / &
          ' dredged total volume        : ',G15.8,' m**3 ', / &
          ' dredged sediment volume     : ',G15.8,' m**3 ', / & 
          ' pore water                  : ',G15.8,' m**3 ', / &
          ' ', / &
          ' volumes of dredged sediment fractions : ', / &
          ' ' )
9010 FORMAT ( ' ',G15.8,' m**3 for fraction "',A,'"' )
     !
!9100 FORMAT (' node-no:',I3,' x-coord: ',G15.8, ' y-coord:',G15.8, ' volume : ',G15.8,' m**3 ')
    !
  END SUBROUTINE write_calculated_dredged_vol_d
  !
  !! output of final dredged volumes on protocol file  
  !! subroutine generates error messages           
  SUBROUTINE write_final_dredged_vol_d ( )
    !    
    !! name of subroutine
    CHARACTER (LEN=25) , PARAMETER :: c_upname='write_final_dredged_vol_d' !
    ! variables
    INTEGER       :: i, n, unit, stat ! 
    TYPE (t_file) :: l_file ! 
    !
    l_file = get_io_info_file(output_files(1))
    !
    IF ( .NOT. file_is_none( l_file ) ) THEN
       !
       CALL set_file_unit( l_file, 0 )
       CALL open_file( l_file )
       !
       unit = get_file_unit( l_file )
       stat = 0
       !
       WRITE (unit, 8000, IOSTAT=stat)
       !
       DO n=1,get_nof_dredge_poly( )
          IF ( stat /= 0 ) EXIT
          IF (all_total_volume(n)==0.0_Double) CYCLE
          WRITE (unit, 8010, IOSTAT=stat) &
               get_criterion_poly_name(dredge_criterion(n)), &
               datetime_to_string(act_time), &
!               TRIM(get_criterion_poly_name(dredge_criterion(n))), &
!               get_criterion_crit_type(dredge_criterion(n)), &
               all_total_volume(n), &
               all_water_volume(n), &
               all_sediment_volume(n)
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<print>', 'Baggerzeitpunkt ...' )
          ELSE
             DO i=1,get_nof_sediment_fraction( )
                IF ( stat /= 0 ) EXIT
                WRITE (unit,8020,IOSTAT=stat) all_fraction_volume(i,n)/all_sediment_volume(n), fraction_name(i)
!                WRITE (unit,9010,IOSTAT=stat) all_fraction_volume(i,n), TRIM(fraction_name(i))
             END DO
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<print>', 'Anteil der Kornfraktionen' )
!             ELSE
!                WRITE (unit,*) ' '
             END IF
          END IF
       END DO
       !
       CALL close_file( l_file )
       !
    END IF
    !
    ! format statements -------------------------------------------------
    !
8000 FORMAT (  &
          '-------------------------------------------------------------------------------- ', / &
          '-------------------------------------------------------------------------------- ', / &
          '  FINAL RESULTS OF DREDGING ACTION:')
8010 FORMAT (  & 
          '-------------------------------------------------------------------------------- ', / &
          '  DREDGE POLYGON: ' ,A, / & 
          '  Final Time               : ',A, / &
          '  Final Total Volume       : ',G15.8,' m**3 ', / &
          '  Final Pore water Volume  : ',G15.8,' m**3 ', / &
          '  Final Sediment Volume    : ',G15.8,' m**3 ')
8020 FORMAT (  &
          '  Final Dredged Fractions  : ',G15.8,' ',A)
    !
    !
  END SUBROUTINE write_final_dredged_vol_d
  !
  !! output of dredged volumes per oberserving period on protocol file  
  !! subroutine generates error messages           
  SUBROUTINE write_dredged_volumes_pop_d ( )
    !    
    !! name of subroutine
    CHARACTER (LEN=27) , PARAMETER :: c_upname='write_dredged_volumes_pop_d' !
    ! variables
    INTEGER       :: i, n, unit, stat ! 
    TYPE (t_file) :: l_file ! 
    !
    l_file = get_io_info_file(output_files(1))
    !
    IF ( .NOT. file_is_none( l_file ) ) THEN
       !
       CALL set_file_unit( l_file, 0 )
       CALL open_file( l_file )
       !
       unit = get_file_unit( l_file )
       stat = 0
       !
       DO n=1,get_nof_dredge_poly( )
          IF ( stat /= 0 ) EXIT
!          IF (act_time >= initial_time_to_observe(n)+observing_period(n) .OR. all_total_volume(n) > 0.0_Double) THEN
             WRITE (unit, 8010, IOSTAT=stat) &
                  get_criterion_poly_name(dredge_criterion(n)), &
                  datetime_to_string(act_time), &
!                  TRIM(get_criterion_poly_name(dredge_criterion(n))), &
!                  get_criterion_crit_type(dredge_criterion(n)), &
                  all_total_volume(n)-all_total_volume_old(n), all_water_volume(n)-all_water_volume_old(n), &
                  all_sediment_volume(n)-all_sediment_volume_old(n)
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<print>', 'Baggerzeitpunkt ...' )
             ELSE
                DO i=1,get_nof_sediment_fraction( )
                   IF ( stat /= 0 ) EXIT
                   IF (all_fraction_volume(i,n)-all_fraction_volume_old(i,n) > 0.0_Double) THEN
                      WRITE (unit,8020,IOSTAT=stat) (all_fraction_volume(i,n)-all_fraction_volume_old(i,n))/&
                           (all_sediment_volume(n)-all_sediment_volume_old(n)), fraction_name(i)
                   ELSE
                      WRITE (unit,8020,IOSTAT=stat) 0.0_Double, fraction_name(i)
                   END IF
!                   WRITE (unit,9010,IOSTAT=stat) all_fraction_volume(i,n), TRIM(fraction_name(i))
                END DO
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<print>', 'Anteil der Kornfraktionen' )
!                ELSE
!                   WRITE (unit,*) ' '
                END IF
                WRITE (unit, 8000, IOSTAT=stat)
             END IF
!          END IF
       END DO
       !
       CALL close_file( l_file )
       !
    END IF
    !
    ! format statements -------------------------------------------------
    !
8000 FORMAT (  &
          '#------------------------------------------------------------------------------- ')
8010 FORMAT (  & 
          '# DREDGE POLYGON: ' ,A, / & 
          '#------------------------------------------------------------------------------- ',/&
          '  Observing Time           : ',A, / &
          '  Total Volume             : ',G15.8,' m**3 ', / &
          '  Pore Water Volume        : ',G15.8,' m**3 ', / &
          '  Sediment Volume          : ',G15.8,' m**3 ')
8020 FORMAT (  &
          '  Dredged Fractions        : ',G15.8,' ',A)    !
  END SUBROUTINE write_dredged_volumes_pop_d
  !
  !! output of final disposed volumes on protocol file  
  !! subroutine generates error messages           
  SUBROUTINE write_final_disposed_vol_d ( )
    !    
    !! name of subroutine
    CHARACTER (LEN=26) , PARAMETER :: c_upname='write_final_disposed_vol_d' !
    ! variables
    INTEGER       :: i, k, n, unit, stat ! 
    TYPE (t_file) :: l_file !
    REAL (KIND=Double), ALLOCATABLE, TARGET :: l_disp_total_volume(:), l_disp_sediment_volume(:), l_disp_fraction_volume(:,:) 
    !
    l_file = get_io_info_file(output_files(1))
    IF (ANY(disp_total_volume(:,:) > 0.0_Double)) THEN
       ALLOCATE(l_disp_total_volume(SIZE(list_of_disp_polys)))
       ALLOCATE(l_disp_sediment_volume(SIZE(list_of_disp_polys)))
       ALLOCATE(l_disp_fraction_volume(get_nof_sediment_fraction( ), SIZE(list_of_disp_polys)))
       l_disp_total_volume=0.0_Double
       l_disp_sediment_volume=0.0_Double
       l_disp_fraction_volume=0.0_Double
       DO i=1,SIZE(list_of_disp_polys)
          DO n=1,get_nof_dredge_poly( )
             DO k=1,nof_dispose_poly
                IF (dispose_poly_name(k,n)=='not_defined') THEN
                   CYCLE
                ELSE IF (dispose_poly_name(k,n)==list_of_disp_polys(i)) THEN
                   l_disp_total_volume(i)=l_disp_total_volume(i)+disp_total_volume(k,n)
                   l_disp_sediment_volume(i)=l_disp_sediment_volume(i)+disp_sediment_volume(k,n)
                   l_disp_fraction_volume(:,i)=l_disp_fraction_volume(:,i)+disp_fraction_volume(:,k,n)
                ELSE IF (list_of_disp_polys(i)=='no_definition') THEN
                   EXIT
                END IF
             END DO
          END DO
       END DO
    END IF
    !
    IF ( .NOT. file_is_none( l_file ) ) THEN
       !
       CALL set_file_unit( l_file, 0 )
       CALL open_file( l_file )
       !
       unit = get_file_unit( l_file )
       stat = 0
       !
       WRITE (unit, 8000, IOSTAT=stat)
       !

       !LEO found bug, check length of list_of_disp_polys 
       IF (ALLOCATED(list_of_disp_polys)) THEN
       DO n=1,SIZE(list_of_disp_polys)
          IF ( stat /= 0 ) EXIT
          IF (list_of_disp_polys(n)=='no_definition') CYCLE
          IF (ALLOCATED(l_disp_total_volume)) THEN
             WRITE (unit, 8010, IOSTAT=stat) &
                  list_of_disp_polys(n), &
                  datetime_to_string(act_time), &
                  l_disp_total_volume(n), &
                  l_disp_sediment_volume(n)
          ELSE
             WRITE (unit, 8010, IOSTAT=stat) &
                  list_of_disp_polys(n), &
                  datetime_to_string(act_time), &
                  0.0_Double, &
                  0.0_Double
          END IF
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<print>', 'Baggerzeitpunkt ...' )
          ELSE
             DO i=1,get_nof_sediment_fraction( )
                IF ( stat /= 0 ) EXIT
!LEO found bug               
!                IF (ALLOCATED(l_disp_sediment_volume) .AND. l_disp_sediment_volume(n) > 0.0_Double) THEN
!                   WRITE (unit,8020,IOSTAT=stat) &
!                   l_disp_fraction_volume(i,n)/l_disp_sediment_volume(n), &
!                   fraction_name(i)
!                ELSE
!                   WRITE (unit,8020,IOSTAT=stat) 0.0_Double, fraction_name(i)
!                END IF
!LEO workaround
               IF (ALLOCATED(l_disp_sediment_volume)) THEN 
                  IF (l_disp_sediment_volume(n) > 0.0_Double) THEN
                     WRITE (unit,8020,IOSTAT=stat) &
                     l_disp_fraction_volume(i,n)/l_disp_sediment_volume(n), &
                     fraction_name(i)
                  ELSE
                     WRITE (unit,8020,IOSTAT=stat) 0.0_Double, fraction_name(i)
                  END IF
                END IF
!LEo end fix
             END DO
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<print>', 'Anteil der Kornfraktionen' )
             END IF
          END IF
!          ELSE
!             WRITE (unit,*) &
!                  'NO ACTION AT ALL'
!          END IF
       END DO
       END IF !LEO 
       !
       CALL close_file( l_file )
       !
    END IF
    !
    ! format statements -------------------------------------------------
    !
8000 FORMAT (  &
          '-------------------------------------------------------------------------------- ', / &
          '-------------------------------------------------------------------------------- ', / &
          '  FINAL RESULTS OF DISPOSAL ACTION:')
8010 FORMAT (  & 
          '-------------------------------------------------------------------------------- ', / &
          '  DISPOSAL POLYGON: ' ,A, / & 
          '  Final Time               : ',A, / &
          '  Final Total Volume       : ',G15.8,' m**3 ', / &
          '  Final Sediment Volume    : ',G15.8,' m**3 ')
8020 FORMAT (  &
          '  Final Disposed Fractions : ',G15.8,' ',A)
    !
    IF (ALLOCATED(l_disp_total_volume)) THEN
        DEALLOCATE(l_disp_sediment_volume)
        DEALLOCATE(l_disp_fraction_volume)
        DEALLOCATE(l_disp_total_volume)
    END IF
    !
  END SUBROUTINE write_final_disposed_vol_d
  !
  !! output of disposed volumes per oberserving period on protocol file  
  !! subroutine generates error messages           
  SUBROUTINE write_disposed_volumes_pop_d ( )
    !    
    !! name of subroutine
    CHARACTER (LEN=28) , PARAMETER :: c_upname='write_disposed_volumes_pop_d' !
    ! variables
    INTEGER       :: i, k, n, unit, stat ! 
    TYPE (t_file) :: l_file ! 
    REAL (Kind=Double), ALLOCATABLE, TARGET :: l_disp_total_volume(:), l_disp_sediment_volume(:), l_disp_fraction_volume(:,:)
    !
    l_file = get_io_info_file(output_files(1))
    
    IF (ANY(disp_total_volume(:,:) > 0.0_Double)) THEN
       ALLOCATE(l_disp_total_volume(SIZE(list_of_disp_polys)))
       ALLOCATE(l_disp_sediment_volume(SIZE(list_of_disp_polys)))

       ALLOCATE(l_disp_fraction_volume(get_nof_sediment_fraction( ), SIZE(list_of_disp_polys)))

       l_disp_total_volume=0.0_Double
       l_disp_sediment_volume=0.0_Double
       l_disp_fraction_volume=0.0_Double

       DO i=1,SIZE(list_of_disp_polys)
          DO n=1,get_nof_dredge_poly( )
             DO k=1,nof_dispose_poly
                IF (dispose_poly_name(k,n)=='not_defined') THEN
                   CYCLE
                ELSE IF (dispose_poly_name(k,n)==list_of_disp_polys(i)) THEN
                   l_disp_total_volume(i)=l_disp_total_volume(i)+(disp_total_volume(k,n)-disp_total_volume_old(k,n))
                   l_disp_sediment_volume(i)=l_disp_sediment_volume(i)+(disp_sediment_volume(k,n)-disp_sediment_volume_old(k,n))
                   l_disp_fraction_volume(:,i)=l_disp_fraction_volume(:,i)+(disp_fraction_volume(:,k,n)- &
                        disp_fraction_volume_old(:,k,n))
                ELSE IF (list_of_disp_polys(i)=='no_definition') THEN
                   EXIT
                END IF
             END DO
          END DO
       END DO
    END IF
    !

    IF ( .NOT. file_is_none( l_file ) ) THEN
       !
       CALL set_file_unit( l_file, 0 )
       CALL open_file( l_file )
       !
       unit = get_file_unit( l_file )
       stat = 0
       !
       !LEO BUG if list_of_disp_polys is not allocated
       IF (ALLOCATED(list_of_disp_polys)) THEN !LEO check if allocated
          DO n=1,SIZE(list_of_disp_polys)
             IF ( stat /= 0 ) EXIT
             IF (list_of_disp_polys(n)=='no_definition') CYCLE
             IF (ALLOCATED(l_disp_total_volume)) THEN 
                WRITE (unit, 8010, IOSTAT=stat) &
                    list_of_disp_polys(n), &
                    datetime_to_string(act_time), &
                    l_disp_total_volume(n), &
                    l_disp_sediment_volume(n)
             ELSE
                WRITE (unit, 8010, IOSTAT=stat) &
                     list_of_disp_polys(n), &
                     datetime_to_string(act_time), &
                     0.0_Double, &
                    0.0_Double
             END IF
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<print>', 'Baggerzeitpunkt ...' )
             ELSE
                DO i=1,get_nof_sediment_fraction( )
                    IF ( stat /= 0 ) EXIT
                    !LEO BUG if l_disp_sediment_volume is not allocated
                   IF (ALLOCATED(l_disp_sediment_volume)) THEN !LEO check if allocated
                     IF (l_disp_sediment_volume(n) > 0.0_Double) THEN
                        WRITE (unit,8020,IOSTAT=stat) &
                        l_disp_fraction_volume(i,n)/l_disp_sediment_volume(n), &
                        fraction_name(i)
                     ELSE
                         WRITE (unit,8020,IOSTAT=stat) 0.0_Double, fraction_name(i)
                     END IF         
                   END IF
                END DO
                  IF ( stat /= 0 ) THEN
                    CALL setup_error_act ( all_errors, -50000, c_upname, c_modname, stat )
                    CALL setup_error_act ( '<print>', 'Anteil der Kornfraktionen' )
                  END IF
             END IF
             WRITE (unit,8000,IOSTAT=stat)
          END DO
       END IF !LEO end if
       !
       CALL close_file( l_file )
       !
    END IF
    !
    ! format statements -------------------------------------------------
    !
8000 FORMAT (  &
          '#------------------------------------------------------------------------------- ')
8010 FORMAT (  & 
          '# DISPOSAL POLYGON : ' ,A, / & 
          '#------------------------------------------------------------------------------- ', / &
          '  Observing Time           : ',A, / &
          '  Total Volume             : ',G15.8,' m**3 ', / &
          '  Sediment Volume          : ',G15.8,' m**3 ')
8020 FORMAT (  &
          '  Disposed Fractions       : ',G15.8,' ',A)
    !
    IF (ALLOCATED(l_disp_total_volume)) THEN
        DEALLOCATE(l_disp_sediment_volume)
        DEALLOCATE(l_disp_fraction_volume)
        DEALLOCATE(l_disp_total_volume)
    END IF

    !
  END SUBROUTINE write_disposed_volumes_pop_d
  !
  !! Write restart data
  SUBROUTINE write_restart_data_d
    !
    !! Sisyphe and bief data used within this procedure
    USE m_dredgesim_data, ONLY : knolg
    USE bief, ONLY : NCSIZE, ipid, nptir, NBMAXNSHARE
    CHARACTER(LEN=11) EXTENS
    EXTERNAL          EXTENS
    !
    !! name of subroutine
    CHARACTER (LEN=20) , PARAMETER :: c_upname='write_restart_data_d' !
    !
    !! Further variables
    INTEGER       :: i, k, n, restart_unit, ierror, status
    CHARACTER (LEN=31) :: restart_filename
    CHARACTER (LEN=31), ALLOCATABLE :: l_dredge_poly_name(:)
    !    
    IF (get_nof_dredge_poly( ) > 0) ALLOCATE(l_dredge_poly_name(get_nof_dredge_poly( )))
    DO n=1,get_nof_dredge_poly( )
       IF (get_criterion_crit_type(dredge_criterion(n))==1) THEN
          l_dredge_poly_name(n) = 'Bottom_Depth_Criterion'
       ELSE IF (get_criterion_crit_type(dredge_criterion(n))==2) THEN
          l_dredge_poly_name(n) = 'Water_Depth_Criterion'
       ELSE IF (get_criterion_crit_type(dredge_criterion(n))==3) THEN
          l_dredge_poly_name(n) = 'Reference_Surface_Criterion'
       END IF
    END DO
    !
    IF (NCSIZE .GT. 1) THEN
       restart_unit=8755+ipid
       restart_filename=TRIM('DSRES'//EXTENS(NCSIZE-1,IPID))
    ELSE
       restart_unit=8755
       restart_filename='DSRES'
    ENDIF
    OPEN (UNIT=restart_unit, FILE=restart_filename, STATUS='UNKNOWN', &
        ACTION='WRITE', IOSTAT=ierror)
    IF (ANY(delta_dredge_node_depth(:,:) /= 0.0_Double) .OR.&
        ANY(node_sediment_volume_rs(:,:) /= 0.0_Double) .OR.&
        ANY(delta_disp_node_depth(:,:,:) /= 0.0_Double)) THEN
        DO n=1,get_nof_dredge_poly( )
           IF (ANY(delta_dredge_node_depth(:,n) /= 0.0_Double)) THEN
              WRITE (restart_unit, FMT = 990, IOSTAT=status) &
              last_act_time_rs, &
              n, &
              -99.999_Double, &
              -99.999_Double, &
              last_obs_time_rs(n), &
              next_obs_time_rs(n), &
              ini_time_rs
           END IF
       END DO
       DO n=1,get_nof_dredge_poly( )
          DO i=1,get_nof_nodes( )
             IF (delta_dredge_node_depth(i,n) /= 0.0_Double) THEN
                IF (NCSIZE .GT. 1) THEN
                    WRITE (restart_unit, FMT = 1000, IOSTAT=status) &
                   TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                    knolg(i), &
                    delta_dredge_node_depth(i,n), &
                    aim_node_depth(i,n), &
                    TRIM(l_dredge_poly_name(n)), &
                    '---', &
                    '---'
                 ELSE
                    WRITE (restart_unit, FMT = 1000, IOSTAT=status) &
                    TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                    i, &
                    delta_dredge_node_depth(i,n), &
                    aim_node_depth(i,n), &
                    TRIM(l_dredge_poly_name(n)), &
                    '---', &
                    '---'
                 END IF
              END IF
           END DO
        END DO
        DO n=1,get_nof_dredge_poly( )
           DO i=1,get_nof_nodes( )
              IF (node_sediment_volume_rs(i,n) /= 0.0_Double) THEN
                 DO k=1,get_nof_sediment_fraction( )
                    IF (NCSIZE .GT. 1) THEN
                       WRITE (restart_unit, FMT = 1100, IOSTAT=status) &
                       TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                       knolg(i), &
                       node_sediment_volume_rs(i,n), &
                       node_fraction_volume_rs(i,k,n), &
                       TRIM(fraction_name(k)), &
                       '---', &
                       '---'
                    ELSE
                       WRITE (restart_unit, FMT = 1100, IOSTAT=status) &
                       TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                       i, &
                       node_sediment_volume_rs(i,n), &
                       node_fraction_volume_rs(i,k,n), &
                       TRIM(fraction_name(k)), &
                       '---', &
                       '---'
                    END IF
                 END DO
              END IF
           END DO
        END DO
        DO n=1,get_nof_dredge_poly( )
        DO i=1,get_nof_nodes( )
          IF (node_total_volume(i,n) /= 0.0_Double) THEN
                IF (NCSIZE .GT. 1) THEN
                    WRITE (restart_unit, FMT = 1200, IOSTAT=status) &
                    TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                    knolg(i), &
                        node_total_volume(i,n), &
                        -99.999_Double, &
                    TRIM(l_dredge_poly_name(n)), &
                    '---', &
                    '---'
                 ELSE
                    WRITE (restart_unit, FMT = 1200, IOSTAT=status) &
                    TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                    i, &
                    node_total_volume(i,n), &
                    -99.999_Double, &
                    TRIM(l_dredge_poly_name(n)), &
                    '---', &
                    '---'
                 END IF
              END IF
           END DO
        END DO
        DO n=1,get_nof_dredge_poly( )
           DO k=1,nof_dispose_poly
              DO i=1,get_nof_nodes( )
                 IF (delta_disp_node_depth(i,n,k) /= 0.0_Double) THEN
                   IF (NCSIZE .GT. 1) THEN
                       WRITE (restart_unit, FMT = 2000, IOSTAT=status) &
                      TRIM(dispose_poly_name(k,n)), &
                      knolg(i), &
                       delta_disp_node_depth(i,n,k), &
                       disp_node_depth(i,n,k), &
                       TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                       '---', &
                      '---'
                    ELSE
                       WRITE (restart_unit, FMT = 2000, IOSTAT=status) &
                       TRIM(dispose_poly_name(k,n)), &
                       i, &
                       delta_disp_node_depth(i,n,k), &
                       disp_node_depth(i,n,k), &
                       TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                       '---', &
                      '---'
                    END IF
                 ELSEIF (dispose_node_index(i,n,k) ==1 .AND. delta_disp_node_depth(i,n,k) == 0.0_Double&
                       .AND. ANY(delta_dredge_node_depth(:,n) /= 0.0_Double)) THEN
                    IF (NCSIZE .GT. 1) THEN
                       WRITE (restart_unit, FMT = 2000, IOSTAT=status) &
                      TRIM(dispose_poly_name(k,n)), &
                      knolg(i), &
                       delta_disp_node_depth(i,n,k), &
                       disp_node_depth(i,n,k), &
                       TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                       '---', &
                      '---'
                     ELSE
                       WRITE (restart_unit, FMT = 2000, IOSTAT=status) &
                       TRIM(dispose_poly_name(k,n)), &
                       i, &
                       delta_disp_node_depth(i,n,k), &
                       disp_node_depth(i,n,k), &
                       TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                       '---', &
                      '---'
                    END IF
             END IF
              END DO
           END DO
        END DO
        DO n=1,get_nof_dredge_poly( )
           DO k=1,nof_dispose_poly
              IF (dredged_sediment_volume_to_disp(n,k) /= 0.0_Double) THEN
                 DO i=1,get_nof_sediment_fraction( )
                    WRITE (restart_unit, FMT = 2100, IOSTAT=status) &
                    TRIM(dispose_poly_name(k,n)), &
                    i, &
                    dredged_sediment_volume_to_disp(n,k), &
                    dredged_fraction_volume_to_disp(n,k,i), &
                    TRIM(get_criterion_poly_name(dredge_criterion(n))), &
                    '---', &
                   '---'
                 END DO
              END IF
           END DO
        END DO
        DO n=1,nof_dredge_poly_tc
           IF (act_time >= dredge_time_tc(n,1) .AND. act_time < disposal_time_tc(n,2)) THEN
              IF (sediment_volume_tc(n) /= 0.0_Double) THEN
                 DO i=1,get_nof_sediment_fraction( )
                    WRITE (restart_unit, FMT = 2100, IOSTAT=status) &
                    TRIM(dispose_poly_name_tc(n)), &
                    i, &
                    sediment_volume_tc(n), &
                    fraction_volume_tc(i,n), &
                    TRIM(dredge_poly_name_tc(n)), &
                    '---', &
                   '---'
                 END DO
             END IF
           END IF
        END DO
    CLOSE (restart_unit)
    ELSE
    CLOSE (restart_unit, status='delete')
    END IF
     990 FORMAT ( 'criterion_times    ',A,' ',I6,' ',G15.8,' ',G15.8,' ',A,' ', A,' ', A)
    1000 FORMAT ( 'criterion_dredge   ',A,' ',I6,' ',G15.8,' ',G15.8,' ',A,' ', A,' ', A)
    1100 FORMAT ( 'dredged_fractions  ',A,' ',I6,' ',G15.8,' ',G15.8,' ',A,' ', A,' ', A)
    1200 FORMAT ( 'dredged_volumes    ',A,' ',I6,' ',G15.8,' ',G15.8,' ',A,' ', A,' ', A)
    2000 FORMAT ( 'criterion_disposal ',A,' ',I6,' ',G15.8,' ',G15.8,' ',A,' ', A,' ', A)
    2100 FORMAT ( 'fractions_to_disp  ',A,' ',I6,' ',G15.8,' ',G15.8,' ',A,' ', A,' ', A)
!   3000 FORMAT ( 'time_steered_disp  ',A,' ',I6,' ',G15.8,' ',G15.8,' ',A,' ', A,' ', A)
    !
    IF(ALLOCATED(l_dredge_poly_name)) DEALLOCATE(l_dredge_poly_name)
  END SUBROUTINE write_restart_data_d
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
END MODULE m_dredgesim_output
! TailOfPackageModule ----------------------------------------------------
