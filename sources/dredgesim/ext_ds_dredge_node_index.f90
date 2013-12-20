! HeadOfExternalUserInterfaceRoutine --------------------------------------
!
!! determine the contents of arrays with index lists 
!!
!! "dredge_node_index"
!! "dispose_node_index"
!! "dredge_node_index_tc"
!! "dispose_node_index_tc"
!! "art_bed_load_node_index"
!!
!! for DredgeSim
!! arrays indicate which elements belong to predfined polygons to operate on
!!
!
SUBROUTINE ext_ds_dredge_node_index ( )
  !
  ! [A.1.1] base module with global constant values
  USE b_constants, ONLY : &
       ! parameters / constant values
       Double
  ! [A.1.2] base module "error-handling"
  USE b_error, ONLY :   &
       ! data type
       t_error, &
       ! methods
       no_error, any_error, new_error, kill_error, &
       setup_error_act, set_error_ierr, set_error_cerr
  ! [A.1.3] base module "2D-points"
  USE b_point_2d, ONLY : &
       ! data type
       t_point_2d, &
       ! methods
       new_point_2d, kill_point_2d, set_point_2d_xy
  ! [A.1.4] base module with type+methods "information concerning files"
  USE b_io_info, ONLY : &
       ! data type
       t_io_info, &
       ! constants
       c_max_len_pac, c_max_len_key, &
       ! methods
       get_io_info_pac_count, get_io_info_pac_idx, get_io_info_file, get_io_info_id, &
       get_io_info_idx, get_io_info_key, get_io_info_att_ref, get_io_info_pac
  ! [A.1.5] base module "file-handling"
  USE b_file, ONLY : &
       ! data type
       t_file
  !
  ! [B] methods from IO-packages
  !
  ! [B.1] methods from DredgeSim to set data
  USE p_dredgesim_ui, ONLY : &
       ! methods
       get_ds_input_files_ref, get_ds_node_coord_ref, get_ds_nof_dredge_poly, &
       get_ds_dredge_poly_name, set_ds_dredge_node_index, &
       get_ds_nof_dispose_poly, get_ds_dispose_poly_name, &
       set_ds_dispose_node_index, &
       get_ds_nof_dredge_poly_tc, get_ds_dredge_poly_name_tc, &
       set_ds_dredge_node_index_tc, &
       get_ds_nof_dispose_poly_tc, get_ds_dispose_poly_name_tc, &
       set_ds_dispose_node_index_tc, &
       get_ds_nof_art_bed_load_poly, get_ds_art_bed_load_poly_name, &
       set_ds_art_bed_load_nod_index
       ! 
  ! [B.2] methods from other packages to get required data from
  USE io_ipds_ui, ONLY : &
       ! methods
       init_ipds, new_ipds, kill_ipds, clear_ipds, &
       setup_ipds_work_object, setup_ipds_file,    &
       setup_ipds_name, read_ipds, is_ipds_point_in_region, &
       is_ipds_region_defined
  !
  ! -------------------------------------------------------------------
  IMPLICIT NONE
  ! -------------------------------------------------------------------
  !
  ! local parameters and variables
  !! name of module (leave name 'externe Routine')
  CHARACTER (LEN=15) , PARAMETER :: c_modname='externe Routine'     ! 
  !! name of external subroutine
  CHARACTER (LEN=24) , PARAMETER :: c_upname='ext_ds_dredge_node_index' !  
  !! pointer to input files
  TYPE (t_io_info)   , POINTER   :: input_files(:)                  ! 
  ! variables
  TYPE (t_error) , ALLOCATABLE :: all_errors(:)                     ! 
  CHARACTER (LEN=c_max_len_pac) , PARAMETER :: c_pac(1)= (/ &       ! 
       'io_ipds   ' /)    ! 
  INTEGER :: i, j, n, nn, idx ! 
  !
  ! ------------------------------------------------------------------
  ! [1] generate temporary required local error messages
  ! ------------------------------------------------------------------
  CALL init_all_errors ( )
  ! ------------------------------------------------------------------
  ! [2] read and transfer required data
  ! ------------------------------------------------------------------
  input_files => get_ds_input_files_ref ( )
  IF ( ASSOCIATED(input_files) ) THEN
     DO i=1,SIZE(c_pac)
        IF ( any_error( ) ) EXIT
        nn = get_io_info_pac_count( input_files, c_pac(i) )
        DO j=1,nn
           IF ( any_error( ) ) EXIT
           idx = get_io_info_pac_idx( input_files, c_pac(i), j )
           SELECT CASE ( c_pac(i) ) 
           CASE ( c_pac(1) ) ! Berechnen von "dredge_node_index" aus Inhalt IPDS-Datei
              CALL compute_dredge_node_index( input_files, c_pac(i), idx )
           CASE DEFAULT
               WRITE(*,*) "==> LEODEBUG ERROR?!!!!", j,i
              !LEO
              CALL setup_error_act ( all_errors, -99000, c_upname, c_modname ) ! 
              CALL setup_error_act ( '<package>', TRIM(c_pac(i)) )             ! 
           END SELECT
        END DO
     END DO
  END IF
  ! ------------------------------------------------------------------
  ! [3] deallocating local errors
  ! ------------------------------------------------------------------
  CALL clear_all_errors ( )
  ! ------------------------------------------------------------------
  NULLIFY(input_files)
  !
CONTAINS
  !
  !! determination of elements inside polygons to operate on ...
  !! ... using methods of the "io_ipds"-package
  !! ... polygons can be dredge and / or disposal polygons
  SUBROUTINE compute_dredge_node_index ( inp_files, package, idx )
    !! list of all input files
    TYPE (t_io_info)  , INTENT(INOUT) :: inp_files(:) ! 
    !! package identifier
    CHARACTER (LEN=*) , INTENT(IN)    :: package      ! 
    !! pointer to current file to read in "inp_files(:)"
    INTEGER           , INTENT(IN)    :: idx          ! 
    ! name of subroutine
    CHARACTER (LEN=25) , PARAMETER :: c_upname='compute_dredge_node_index' ! 
    ! variables
    CHARACTER (LEN=10) :: l_char ! 
    INTEGER :: i, id, stat ! 
    INTEGER :: j_Leo
    REAL (KIND=Double)    , POINTER :: node_coord(:,:)      ! 
    LOGICAL           , ALLOCATABLE :: l_inside(:)            ! 
    INTEGER           , ALLOCATABLE :: l_dredge_node_index(:,:) !
    INTEGER           , ALLOCATABLE :: l_dispose_node_index(:,:,:)!
    INTEGER           , ALLOCATABLE :: l_dredge_node_index_tc(:,:) !
    INTEGER           , ALLOCATABLE :: l_dispose_node_index_tc(:,:) !
    INTEGER           , ALLOCATABLE :: l_art_bed_load_node_index(:,:)
    TYPE (t_point_2d) , ALLOCATABLE :: grid_points(:)       ! 
    CHARACTER (LEN=c_max_len_key) :: l_key    ! 
    TYPE (t_file)                 :: l_file   ! 
    !
    ! -------------------------------------------------------------------
    ! [1] determine essential parameters
    ! -------------------------------------------------------------------
    id = get_io_info_id  ( inp_files(idx) )
    l_file = get_io_info_file( inp_files(idx) )
    l_key  = get_io_info_key ( inp_files(idx) )
    CALL init_ipds ( )
    CALL new_ipds ( id )
    CALL setup_ipds_work_object ( id )
    CALL setup_ipds_file ( l_file )
    CALL setup_ipds_name ( l_key )
    CALL read_ipds ( )
    ! -------------------------------------------------------------------
    ! [2] getting node coordinates and change of data to "t_point_2d"
    ! -------------------------------------------------------------------
    ! Check, whether dredge or disposal polygon is defined in IPDS
!    DO i=1,get_ds_nof_dredge_poly_tc( )
!       IF (.NOT. is_ipds_region_defined(get_ds_dredge_poly_name_tc(i))) THEN
!          PRINT*, 'Bagger-/Verklapppolygon NICHT in ipds definiert:', get_ds_dredge_poly_name_tc(i)
!	   END IF
!    END DO
    !
    node_coord => get_ds_node_coord_ref ( )
    IF (( ASSOCIATED(node_coord)) .AND. no_error( ) ) THEN
       ! [2.1] getting gravitational center coordinates and change of data to "t_point_2d"
       ALLOCATE( grid_points(SIZE(node_coord,1)), l_dredge_node_index(SIZE(node_coord,1),get_ds_nof_dredge_poly( )), &
                 l_inside(SIZE(node_coord,1)), STAT=stat )
       ALLOCATE(l_dispose_node_index(SIZE(node_coord,1),get_ds_nof_dredge_poly( ), get_ds_nof_dispose_poly( ) ))
       ALLOCATE(l_dredge_node_index_tc(SIZE(node_coord,1),get_ds_nof_dredge_poly_tc( ) ))
       ALLOCATE(l_dispose_node_index_tc(SIZE(node_coord,1),get_ds_nof_dispose_poly_tc( ) ))
       ALLOCATE(l_art_bed_load_node_index(SIZE(node_coord,1),get_ds_nof_art_bed_load_poly( ) ))
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -93200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'grid_points(:),l_dredge_node_index(:,:),l_inside(:),'// &
          'l_dispose_node_index(:,:,:), l_dredge_node_index_tc(:,:), l_dispose_node_index_tc(:,:),'// &
          'l_art_bed_load_node_index(:,:)' )
          WRITE(l_char,'(I10)') SIZE(node_coord,1) ; CALL setup_error_act ( '<idim1>', l_char )
       ELSE
          CALL new_point_2d ( grid_points )
          DO i=1,SIZE(grid_points)
             CALL set_point_2d_xy ( grid_points(i), node_coord(i,1), node_coord(i,2) )
          END DO

          ! [2.2] determine index list element - dredge polygon
          l_dredge_node_index(:,:) = 0
          DO i=1,get_ds_nof_dredge_poly( )
             l_inside(:) = is_ipds_point_in_region( grid_points(:), get_ds_dredge_poly_name(i) )
             WHERE( l_inside ) l_dredge_node_index(:,i) = 1
          END DO
          ! [2.3] transfer to DredgeSim
          CALL set_ds_dredge_node_index( l_dredge_node_index )
          ! [2.4] determine index list element - disposal polygon
          l_dispose_node_index(:,:,:) = 0
          DO j_Leo=1, get_ds_nof_dredge_poly( )
             DO i=1,get_ds_nof_dispose_poly( )
                l_inside(:) = is_ipds_point_in_region( grid_points(:), get_ds_dispose_poly_name(i, j_Leo) )
                WHERE( l_inside ) l_dispose_node_index(:,j_Leo,i) = 1
             END DO
          END DO
          ! [2.5] transfer to DredgeSim
          CALL set_ds_dispose_node_index( l_dispose_node_index )
          ! [2.6] determine index list element - dredge polygon for time controlled maintenance
          l_dredge_node_index_tc(:,:) = 0
          DO i=1,get_ds_nof_dredge_poly_tc( )
             l_inside(:) = is_ipds_point_in_region( grid_points(:), get_ds_dredge_poly_name_tc(i) )
             WHERE( l_inside ) l_dredge_node_index_tc(:,i) = 1
          END DO
          ! [2.7] transfer to DredgeSim
          CALL set_ds_dredge_node_index_tc( l_dredge_node_index_tc )
          ! [2.8] determine index list element - disposal polygon for time controlled maintenance
          l_dispose_node_index_tc(:,:) = 0
          DO i=1,get_ds_nof_dispose_poly_tc( )
             l_inside(:) = is_ipds_point_in_region( grid_points(:), get_ds_dispose_poly_name_tc(i) )
             WHERE( l_inside ) l_dispose_node_index_tc(:,i) = 1
          END DO
          ! [2.9] transfer to DredgeSim
          CALL set_ds_dispose_node_index_tc( l_dispose_node_index_tc )
          ! [2.10] determine index list element - disposal polygon for artificial bed load supply
          l_art_bed_load_node_index(:,:) = 0
          DO i=1,get_ds_nof_art_bed_load_poly( )
             l_inside(:) = is_ipds_point_in_region( grid_points(:), get_ds_art_bed_load_poly_name(i) )
             WHERE( l_inside ) l_art_bed_load_node_index(:,i) = 1
          END DO
          ! [2.11] transfer to DredgeSim
          CALL set_ds_art_bed_load_nod_index( l_art_bed_load_node_index )
          ! [2.12] clearing up local data which is no longer needed
          CALL kill_point_2d ( grid_points )
          DEALLOCATE( grid_points, l_dredge_node_index, l_inside, STAT=stat )
          DEALLOCATE( l_dispose_node_index )
          DEALLOCATE( l_dredge_node_index_tc )
          DEALLOCATE( l_dispose_node_index_tc )
          DEALLOCATE( l_art_bed_load_node_index )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -94200, c_upname, c_modname, stat )
             CALL setup_error_act ( '<name>', 'center_points(:),l_dredge_node_index(:,:),l_inside(:),'// &
                                    'l_dispose_node_index(:,:,:), l_dredge_node_index_tc(:,:), l_dispose_node_index_tc(:,:),'// &
                                    'l_art_bed_load_node_index(:,:)' )
          END IF
       END IF
    END IF

    CALL kill_ipds( id )

    !LEO THis had something to do with the stupid error handlingCALL clear_ipds( )
    !TODO get the error handling working!

    !
  END SUBROUTINE compute_dredge_node_index
  !
  !! initializing all error messages for this external subroutine
  SUBROUTINE init_all_errors ( )
    !! variable
    INTEGER :: i, ic ! 
    !
    DO i=1,2
       ic = 0
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -93200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category private ALLOCATE-methods\n'//&
               'two-dimensional REAL(Double)-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               '--> check check code in external subroutine subroutine "ext_ds_dredge_node_index"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -94200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category Private DEALLOCATE-methods\n'//&
               'REAL(Double)-array\n'//&
               'name        = "<name>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -99000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category IO-methods in external subroutine\n'//&
               'method for reading data is not available\n'//&
               'package identifier = <package>\n'//&
               '--> check / extend  check code in external subroutine subroutine "ext_ds_dredge_node_index"' )
       END IF
       ! allocating arrays at first cycle (i==1)
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    END DO
    !    
  END SUBROUTINE init_all_errors
  !
  !! allocating/initializing all local required error messages 
  !! subroutine does not throw error messages
  SUBROUTINE clear_all_errors ( )
    !
    IF ( ALLOCATED( all_errors ) ) THEN
       CALL kill_error( all_errors(:) )
       DEALLOCATE ( all_errors )
    END IF
    !
  END SUBROUTINE clear_all_errors
  !
END SUBROUTINE ext_ds_dredge_node_index
