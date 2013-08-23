! Ä-------------------------------------------------------------------------
! HeadOfPackageComputeModule ----------------------------------------------
!
!! compute methods for the "dredgesim"-package
!!
!
MODULE m_dredgesim_compute
  !
  ! ----------------------------------------------------------------------
  ! [A] base modules with frequently used methods
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] base module with global constant values
  USE b_constants, ONLY : Single, Double
  ! [A.1.2] base module "errors"
  USE b_error, ONLY :         &
       ! routines
       no_error, any_error,   &
       setup_error_act
  ! [A.1.3] base module "sediment grain" ---------------------------------
  USE b_grain, ONLY :         &
      ! routines
      all_grain_available
  ! [A.1.5] base module "timestep" ---------------------------------------
  USE b_time
  ! [A.1.6] base module "date and time" ----------------------------------
  USE b_datetime
  ! [A.2.1] (local) base module type+methods "dredge polygon data" -------
  USE l_criterion, ONLY :     &
       ! data type
       t_criterion, &
       ! routines
       get_criterion_crit_type, get_criterion_crit_depth, get_criterion_dredge_depth, get_criterion_poly_name
  !
  ! ----------------------------------------------------------------------
  ! [B] data of the "dredgesim"-package
  ! ----------------------------------------------------------------------
  !
  ! [B.1] data module of the "dredgesim"-package 
  USE m_dredgesim_data, ONLY : &
       ! variables und constant values
       prn_op, trc_op, prn_lun, trc_lun, all_errors, &
       initial_time, act_time, act_timestep, old_time, &
       get_nof_poly, get_nof_dredge_poly, get_nof_nodes, &
       poly_water_depth, node_water_depth, dredge_poly_index, dredge_node_index, poly_depth, poly_area, &
       node_depth, nodes_of_poly, edgelist_of_poly, nodelist_of_poly, &
       poly_total_volume, poly_sediment_volume, poly_water_volume, poly_fraction_volume, &
       total_volume, sediment_volume, water_volume, fraction_volume, delta_dredge_poly_depth, &
       aim_poly_depth, dredge_criterion, node_porosity, cell_sediment_fraction, c_undef_dp, &
       start_dredge_time, end_dredge_time, dredging_rate, disposal_rate, nof_dispose_poly, dispose_poly_index, &
       dispose_node_index, dispose_weighting_factor, dispose_poly_name, delta_disp_poly_depth, disp_poly_depth,&
       delta_disp_node_depth,  disp_node_depth, nof_dredge_poly_tc, dredge_poly_name_tc, dredge_poly_index_tc, &
       dredge_node_index_tc, dredge_time_tc, dredge_sed_vol_tc, &
       poly_fraction_volume_tc, fraction_volume_tc, &
       nof_dispose_poly_tc, dispose_poly_name_tc, dispose_poly_index_tc, dispose_node_index_tc, disposal_time_tc, &
       dispose_sed_vol_tc,  art_bed_load_poly_index, art_bed_load_node_index, node_sediment_fraction, &
       nof_predef_disp_poly, predef_disp_sed_vol, predef_sed_distrib, art_bl_time, fraction_name, &
       predef_disp_poly_name, predef_disp_sed_class, predef_depos_time, free_surface_node_depth, &
       free_surface_poly_depth, observing_period, time_to_observe, node_fraction_volume_tc, aim_node_depth, &
       delta_dredge_node_depth, node_coord, fkm, node_sediment_volume_tc, sediment_volume_tc, node_total_volume, &
       upd_total_volume, node_sediment_volume, node_water_volume, node_fraction_volume, &
       navigation_possible, minimum_volume, sector_radius, &
       disposing_scours, nof_iterations_disp_scours, max_error_disp_scours, act_depo_depth_disp_scours, &
       min_depo_depth_disp_scours, delta_disp_node_depth_tc, delta_node_depth_abl, aim_node_depth_tc, &
       aim_node_depth_abl, old_time_to_observe, &
       all_total_volume, all_sediment_volume, all_water_volume, all_fraction_volume, upd_out_volumes, &
       all_total_volume_old, all_sediment_volume_old, all_water_volume_old, all_fraction_volume_old, &
       dredged_sediment_volume_to_disp, dredged_fraction_volume_to_disp, disp_scours_auto, disp_scours_tc, disp_scours_abl, &
       disp_node_total_volume, node_sediment_volume_rs, node_fraction_volume_rs, &
       disp_total_volume, disp_sediment_volume, disp_fraction_volume, &
       disp_total_volume_old, disp_sediment_volume_old, disp_fraction_volume_old, &
       output_time_restart, last_obs_time_rs, next_obs_time_rs, next_obs_time_rs, last_act_time_rs, &
       node_noero_depth !LEO node_noero_depth was commented out
  ! [B.2] module providing methods for interpolation of values in DredgeSim
  USE m_dredgesim_interpolations, ONLY : &
       ! methods
       interpolate_edge_to_poly, interpolate_poly_to_edge, interpolate_poly_to_node, interpolate_node_to_poly
  ! [B.3] module providing methods for output
  USE m_dredgesim_output, ONLY : &
       ! methods
       write_calculated_dredged_volume, &
       write_dredged_volumes_pop, &
       write_disposed_volumes_pop, &
       write_restart_data
  !
  IMPLICIT NONE
  PRIVATE
  !
  DOUBLE PRECISION P_DSUM
  EXTERNAL P_DSUM
  !
  ! ---------------------------------------------------------------------
  ! [C] public declarations
  ! ---------------------------------------------------------------------
  !
  !! executing a computation step
  INTERFACE compute_dredgesim
     MODULE PROCEDURE compute_dredgesim_d
  END INTERFACE
  !
  ! list of public methods
  !
  PUBLIC :: compute_dredgesim
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data types, internal data and methods
  ! ---------------------------------------------------------------------
  !
  !! name of the module
  CHARACTER (LEN=19), PARAMETER :: c_modname  = 'm_dredgesim_compute' !
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
  !! executing computation 
  !! subroutine does not throw error messages
  SUBROUTINE compute_dredgesim_d ( )
     !! using methods and data from sisyphe within this routine  
    use m_dredgesim_data, ONLY : debug_ds, node_area, node_neighb, knolg, leopr_ds
    use m_dredgesim_data, ONLY : act_timestep
    use m_dredgesim_data, ONLY : NB_NEIGHB, dimbuf,dimnhcom,list_send,& 
        buf_send,buf_recv,nh_com,nb_neighb_pt
    USE bief, ONLY : NCSIZE, ipid, nptir, NBMAXNSHARE, bief_obj
    CHARACTER(LEN=11) EXTENS
    EXTERNAL          EXTENS
    !
    !! name of subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='compute_dredgesim_d' ! 
    !
    !! array of critical bottom/waters depth at which dredging is initiated automatically
    REAL (KIND=Double), ALLOCATABLE :: crit_depth (:)     ! 
    !! array of aim bottom/water depths which should be reached by dredging
    REAL (KIND=Double), ALLOCATABLE :: dredge_depth (:)   ! 
    !! arrays for difference between current and dredge depth for time controlled maintenance and artificial bed load supply
    REAL (KIND=Double), ALLOCATABLE :: delta_dredge_node_depth_tc(:,:)
    !! variables
    LOGICAL :: l1 ! 
    INTEGER :: i, j, k, n, m, p, crit_type, ierror, stat ! 
    REAL (KIND=Double) :: mm
    !! array for checking for minimum volume in sector
    REAL (KIND=Double), ALLOCATABLE :: total_volume_in_sector(:,:)
    !! array mastering information whether a node needs to be dredged or not
    LOGICAL, ALLOCATABLE            :: node_to_be_dredged(:)
    !! array mastering information whether any node within a dredge polygon needs to be dredged or not
    REAL (KIND=Double), ALLOCATABLE :: dredge_poly_to_be_dredged(:)
    !! arrays for determination of nearest dispose polygon to dump dredged material
    REAL (KIND=Double), ALLOCATABLE :: x_Factor(:,:), diff_array(:)
    !! Variables to calculate the x_Factor
    REAL (KIND=Double)              :: x_Factor_Num, x_Factor_Denom
    !! gives amount of sediment fractions to dispose per dispose polygon when abl-action
    REAL (KIND=Double), ALLOCATABLE :: dredged_fractions_to_disp_abl (:,:)
    !! arrays for the nodal and total areas within operation sites
    REAL (KIND=Double), ALLOCATABLE :: node_areas_inside(:,:)
    REAL (KIND=Double), ALLOCATABLE :: sum_node_areas_inside (:)
    !! Array for saving dredged fractions to dispose when using a dredge criterion
    REAL (KIND=Double), ALLOCATABLE :: dredged_fractions_to_disp(:,:)
    !! local arrays supporting automatic disposal operations
    REAL (KIND=Double), ALLOCATABLE :: dredged_sediment_volume_to_disp_loc(:,:)
    REAL (KIND=Double), ALLOCATABLE :: dredged_fraction_volume_to_disp_loc(:,:,:)
    REAL (KIND=Double), ALLOCATABLE :: scour_node_depth_loc(:)
    REAL (KIND=Double), ALLOCATABLE :: disp_total_volume_loc(:,:)
    REAL (KIND=Double), ALLOCATABLE :: disp_sediment_volume_loc(:,:)
    REAL (KIND=Double), ALLOCATABLE :: disp_fraction_volume_loc(:,:,:)
    REAL (KIND=Double), ALLOCATABLE :: disp_node_total_volume_loc(:,:,:)
    !! indicator variables for dredging and disposing
    REAL (KIND=Double), ALLOCATABLE :: node_clean (:)
    REAL (KIND=Double), ALLOCATABLE :: node_supply(:)
    !! array with node depths after dredging or disposing
    REAL (KIND=Double), ALLOCATABLE :: node_depth_dredge(:)
    REAL (KIND=Double), ALLOCATABLE :: node_depth_supply(:)
    !! array with node no ero depths
    !LEO REAL (KIND=Double), ALLOCATABLE :: node_noero_depth(:)
    !! local volume arrays for operating on nodes
    REAL (KIND=Double), ALLOCATABLE :: node_total_volume_loc(:,:)
    REAL (KIND=Double), ALLOCATABLE :: total_volume_loc(:)
    REAL (KIND=Double), ALLOCATABLE :: node_sediment_volume_loc(:,:)
    REAL (KIND=Double), ALLOCATABLE :: sediment_volume_loc(:)
    REAL (KIND=Double), ALLOCATABLE :: node_water_volume_loc(:,:)
    REAL (KIND=Double), ALLOCATABLE :: water_volume_loc(:)
    REAL (KIND=Double), ALLOCATABLE :: node_fraction_volume_loc(:,:,:)
    REAL (KIND=Double), ALLOCATABLE :: fraction_volume_loc(:,:)
    !! local array of dispose poly names which are used by a dredge criterion
    CHARACTER (LEN=80), ALLOCATABLE :: disp_poly_names_loc(:)
    REAL (KIND=Double), ALLOCATABLE :: disp_weighting_factor_loc(:)
    !! arrays for calculating the time interval of time controlled maintenances and artificial bed load supply
    REAL (KIND=Double), ALLOCATABLE :: delta_t_dredge_tc (:)
    REAL (KIND=Double), ALLOCATABLE :: delta_t_disposal_tc (:) 
    REAL (KIND=Double), ALLOCATABLE :: delta_t_art_bed_load(:)
    !! associated node areas
!RK    REAL (KIND=Double), ALLOCATABLE :: node_area(:)
    !! array for saving dredged fractions to dispose when using time controlled maintenance
    REAL (KIND=Double), ALLOCATABLE :: dredged_fractions_to_disp_tc(:,:)
    !! volume array for calculating the volume disposed on a node within a scour    
    REAL (KIND=Double), ALLOCATABLE :: check_node_total_volume(:)
    !! local arrays supporting automatic disposal operations
    REAL (KIND=Double), ALLOCATABLE :: scour_node_depth(:,:)
    !! sum of all evolutions when disposing in scours
    REAL (KIND=Double)              :: sum_of_evolutions
    !! total volume disposed in a scour
    REAL (KIND=Double)              :: check_total_volume
    !! for calculating the average porosity within a scour
    REAL (KIND=Double)              :: average_porosity
    REAL (KIND=Double)              :: sum_porosity_inside
    REAL (KIND=Double)              :: nof_nodes_inside
    !! multi-used variable to detect values on interface nodes taken into account more than once
    REAL (KIND=Double)              :: doppelt, doppelt1, doppelt2, doppelt3, doppelt4
    !! little helpers for iteration when disposing in scours
    REAL (KIND=Double)              :: little_helper_1
    REAL (KIND=Double)              :: little_helper_2
    REAL (KIND=Double)              :: little_helper_3
    !!
    LOGICAL :: any_disp_poly_to_disp
    REAL*4 :: real_time0, cpu_time0
    REAL*4 :: real_time, cpu_time
    integer start, ende, rate, cmax
    real time
    INTEGER :: restart_unit, status
    CHARACTER (LEN=31) :: restart_filename

    TYPE(BIEF_OBJ) :: T13, T14
    !
    !LEO BUG!!! check if dredge_criterion exists l1 is not used :-(
    IF (get_nof_dredge_poly() .GT. 0) THEN
      l1 = ALL( get_criterion_crit_type(dredge_criterion(:)) == 1 ) 
    END IF
    !
    !! allocating local calculation parameters
    IF (.NOT. ALLOCATED(crit_depth  )) ALLOCATE( crit_depth(get_nof_dredge_poly()))
    IF (.NOT. ALLOCATED(dredge_depth)) ALLOCATE( dredge_depth(get_nof_dredge_poly()))
    !
    ALLOCATE (node_clean (get_nof_nodes( )), node_depth_dredge(get_nof_nodes( )))
    ALLOCATE (node_supply(get_nof_nodes( )), node_depth_supply(get_nof_nodes( )))
    !LEO take form m_dredgesim_data ALLOCATE (node_noero_depth(get_nof_nodes( )))
    !
    ALLOCATE(total_volume_in_sector(get_nof_nodes(), get_nof_dredge_poly()))
    total_volume_in_sector = 0.0_Double
    ALLOCATE(node_to_be_dredged(get_nof_nodes()))
    node_to_be_dredged = .false.
    ALLOCATE(dredge_poly_to_be_dredged(get_nof_dredge_poly()))
    dredge_poly_to_be_dredged = 0.0_Double
    !
    !! setting intial values for calculation parameters
    !! for disposing nodewise: node_clean is initialized respecting new data attribute, node_supply is initialized
    node_clean(:)       = 0.D0
    node_supply(:)      = 0.D0    
!das ist zeitinvariant und wird in init_and_setup gesetzt
!RK    node_noero_depth(:) = ZR%R 
! node_depth bekommt nun ein update in run_dredgesim 
!RK    node_depth(:)       = ZF%R
    !
    node_depth_dredge = node_depth
    node_depth_supply = node_depth
    !

!RK unklar, warum das hier gemacht werden muss. Das sollte doch schon 
! mit der call set_ds_node_sediment_fraction gemacht worden sein?!?
! die wird vermutlich nur einmal zur INitialisierung aufgerufen...
! Warum dann hier nicht auch ein set_ds_node_sediment_fraction?!?
! bzw. muss zur runtime noch verschiedene Dinge uebergeben werden!!!
! habe ich in die run_dredgesim subroutine in mod_p_dredgesim_ui verschoben
!     CALL update_ds_node_sediment_fraction(node_sediment_fraction)

!    DO j=1,SIZE(node_sediment_fraction,2)
!       DO i=1,get_nof_nodes( )
!          node_sediment_fraction(i,j) = AVAIL(i,1,j)
!       END DO
!   END DO
    !
    !LEO this is a Bug if no dredge_criterion exists
    IF (get_nof_dredge_poly() .GT. 0) THEN
       crit_depth    = get_criterion_crit_depth(dredge_criterion(:))
       dredge_depth  = get_criterion_dredge_depth(dredge_criterion(:))
    END IF

    !
    !! for disposing nodewise
!RK    ALLOCATE( node_area(get_nof_nodes()))
!RK    node_area = 0.0_Double
!RK    !! calculation of associated areas taken from Telemac
!RK    CALL VECTOR(T13,'=','MASBAS          ',11,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)
!RK        IF(NCSIZE.GT.1) THEN
!RK          CALL PARCOM(T13,2,MESH)
!RK        ENDIF
!RK    node_area = T13%R
     
    !
!    PRINT*, 'node_depth(308)', node_depth(308)
!    PRINT*, 'node_depth(251)', node_depth(251)
!    PRINT*, 'node_depth(14)', node_depth(14)
!    PRINT*, 'node_depth(64)', node_depth(64)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! DREDGING - automatic initiation of dredging by dredge criteria !!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
!    call system_clock(start,rate,cmax)
    !
    !! check if dredging is necessary and initiation of dredging if so
    !! loop over all dredge polygons --------------------------------------
    IF (ALLOCATED(dredge_criterion)) THEN
       !! allocation of required data
       ALLOCATE(node_total_volume_loc   (get_nof_nodes( ),get_nof_dredge_poly( )))
       ALLOCATE(node_sediment_volume_loc(get_nof_nodes( ),get_nof_dredge_poly( )))
       ALLOCATE(node_water_volume_loc   (get_nof_nodes( ),get_nof_dredge_poly( )))
       ALLOCATE(node_fraction_volume_loc(get_nof_nodes( ),SIZE(node_sediment_fraction,2),get_nof_dredge_poly( )))
       ALLOCATE(total_volume_loc        (get_nof_dredge_poly( )))
       ALLOCATE(sediment_volume_loc     (get_nof_dredge_poly( )))
       ALLOCATE(water_volume_loc        (get_nof_dredge_poly( )))
       ALLOCATE(fraction_volume_loc     (SIZE(node_sediment_fraction,2),get_nof_dredge_poly( )))
       node_total_volume_loc    = 0.0_Double
       node_sediment_volume_loc = 0.0_Double
       node_water_volume_loc    = 0.0_Double
       node_fraction_volume_loc = 0.0_Double
       total_volume_loc         = 0.0_Double
       sediment_volume_loc      = 0.0_Double
       water_volume_loc         = 0.0_Double
       fraction_volume_loc      = 0.0_Double
       !
!    call system_clock(start,rate,cmax)
    !
!    WRITE(6,*) 'upd_out_volumes Beginn', upd_out_volumes
    DO n=1,get_nof_dredge_poly( )

       crit_type = get_criterion_crit_type( dredge_criterion(n) )
       !! calculating next time for observing the bottom
       IF ( time_to_observe(n) == initial_time) THEN
       	  time_to_observe(n) = time_to_observe(n)+observing_period(n)
       END IF       
       !! checking  whether time for observation is reached
       IF ( time_to_observe(n) <= act_time .AND. navigation_possible(n) ) THEN
       old_time_to_observe(n)       = time_to_observe(n)
       upd_out_volumes(n)           = .true.
       time_to_observe(n)           = time_to_observe(n) + observing_period(n)
       !
       !! check dredging needs on at least one dredge polygon -------------
       SELECT CASE ( crit_type ) 
       CASE ( 1 ) ! bottom criterion
                  DO i=1,get_nof_nodes( )
                     IF ( dredge_node_index(i,n) == 1 .AND. node_depth(i) > crit_depth(n)) THEN
                        !! dredging is necessary
                        start_dredge_time(n) = act_time
                        dredge_poly_to_be_dredged(n)=1.0_Double
                        EXIT ! leave loop over "i", because dredge polygon n is finished
                     END IF
                  END DO
                  ! parallel routines
                  IF (NCSIZE.GT.1) THEN
                     dredge_poly_to_be_dredged(n)=p_dsum(dredge_poly_to_be_dredged(n))
                  END IF
                  IF (dredge_poly_to_be_dredged(n) >= 1.0_Double) THEN
                     !! calculate aim depths, differential depths and volumes to dredge on every element
                     !! ... of a dredge polygon if dredge criterion is fulfilled for at least one element
                     DO i=1,get_nof_nodes( )
                        IF ( dredge_node_index(i,n)==1 ) THEN
                           IF ( node_depth(i) <= dredge_depth(n)) CYCLE
                           !! calculation of aim depth
                           aim_node_depth(i,n) = dredge_depth(n)
                           !! calculation of differential depths
                           delta_dredge_node_depth(i,n)= aim_node_depth(i,n)-node_depth(i)
                           !! caculation of volumes to dredge
                           node_total_volume_loc(i,n)      = ABS(node_area(i)*(delta_dredge_node_depth(i,n)))
                           node_sediment_volume_loc(i,n)   = ABS(node_area(i)*(delta_dredge_node_depth(i,n))&
                                                             *(1.D0-node_porosity(i)))
                           node_water_volume_loc(i,n)      = ABS(node_area(i)*(delta_dredge_node_depth(i,n))&
                                                             *node_porosity(i))
!                           node_fraction_volume_loc(i,:,n) = ABS(node_sediment_fraction(i,:)*node_area(i)&
!                                                             *(delta_dredge_node_depth(i,n))*(1.D0-node_porosity(i))) 

                        ELSE
                           aim_node_depth(i,n) = node_depth(i)
                           delta_dredge_node_depth(i,n) = 0.0_Double
                        END IF
                     END DO
                  END IF
       CASE ( 2 ) ! water depth criterion
                  DO i=1,get_nof_nodes( )
                     IF ( dredge_node_index(i,n) == 1 .AND. node_water_depth(i) < crit_depth(n)) THEN
                        !! dredging is necessary
                        start_dredge_time(n) = act_time
                        dredge_poly_to_be_dredged(n)=1.0_Double
                        EXIT ! leave loop over "i", because dredge polygon n is finished
                     END IF
                  END DO
                  ! parallel routines
                  IF (NCSIZE.GT.1) THEN
                     dredge_poly_to_be_dredged(n)=p_dsum(dredge_poly_to_be_dredged(n))
                  END IF
                  IF (dredge_poly_to_be_dredged(n) >= 1.0_Double) THEN
                     !! calculate aim depths, differential depths and volumes to dredge on every element
                     !! ... of a dredge polygon if dredge criterion is fulfilled for at least one element
                     DO i=1, get_nof_nodes( )
                        IF ( dredge_node_index(i,n) == 1 ) THEN
                           IF (node_water_depth(i) >= dredge_depth(n)) CYCLE
                           !! calculation of aim depth
                           aim_node_depth(i,n) = node_depth(i)+node_water_depth(i)-dredge_depth(n)
                           !! calculation of differential depths
                           delta_dredge_node_depth(i,n)= node_depth(i)-aim_node_depth(i,n)
                           !! calculation of volumes to dredge
                           node_total_volume_loc(i,n)      = ABS(node_area(i)*delta_dredge_node_depth(i,n))
                           node_sediment_volume_loc(i,n)   = ABS(node_area(i)*delta_dredge_node_depth(i,n)&
                                                             *(1.D0-node_porosity(i)))
                           node_water_volume_loc(i,n)      = ABS(node_area(i)*delta_dredge_node_depth(i,n)&
                                                             *node_porosity(i))
                           node_fraction_volume_loc(i,:,n) = ABS(node_sediment_fraction(i,:)*node_area(i)&
                                                             *delta_dredge_node_depth(i,n)*(1.D0-node_porosity(i)))
                        ELSE
                           aim_node_depth(i,n) = node_depth(i)
                           delta_dredge_node_depth(i,n) = 0.0_Double
                        END IF
                     END DO
                  END IF
       CASE ( 3 ) ! referenced surface criterion
                  DO i=1,get_nof_nodes( )
                     IF ( dredge_node_index(i,n) == 1 .AND. ABS(free_surface_node_depth(i)&
                                                          - node_depth(i)) < crit_depth(n)) THEN 
                        !! dredging is necessary
                        start_dredge_time(n) = act_time
                        !! calculation of aim depths, differential depths and volumes to dredge on every element
                        !! ... fulfilling this criterion
                        !! calculation of aim depths
                        aim_node_depth(i,n) = free_surface_node_depth(i)-dredge_depth(n)
                        IF (aim_node_depth(i,n) < node_noero_depth(i)) THEN
                           aim_node_depth(i,n) = node_noero_depth(i)
                           !LEO included NCSIZE
                           IF (NCSIZE.GT.1) THEN
                              WRITE(6,*) 'Warning: dredge depth on node', knolg(i), 'goes deeper than rigid bed...'
                              WRITE(6,*) '... dredge depth of node', knolg(i), 'is set to the value of the rigid bed.'
                           ELSE
                              WRITE(6,*) 'Warning: dredge depth on node', i, 'goes deeper than rigid bed...'
                              WRITE(6,*) '... dredge depth of node', i, 'is set to the value of the rigid bed.'
                           END IF
                        END IF
                        !! calculation of differential depths
                        delta_dredge_node_depth(i,n) = ABS(aim_node_depth(i,n)-node_depth(i))
                        !! calculation of volumes to dredge
                        node_total_volume_loc(i,n)      = ABS(node_area(i)*delta_dredge_node_depth(i,n))
                        node_sediment_volume_loc(i,n)   = ABS(node_area(i)*delta_dredge_node_depth(i,n)*(1-node_porosity(i)))
                        node_water_volume_loc(i,n)      = ABS(node_area(i)*delta_dredge_node_depth(i,n)*node_porosity(i))
                        node_fraction_volume_loc(i,:,n) = ABS(node_sediment_fraction(i,:)*node_area(i)&
                                                          *delta_dredge_node_depth(i,n)*(1-node_porosity(i)))
                     ELSEIF (delta_dredge_node_depth(i,n) == 0.0_Double) THEN
                        aim_node_depth(i,n) = node_depth(i)
                     END IF
                  END DO
       END SELECT
       END IF
    END DO
    !
!    WRITE(6,*) 'upd_out_volumes Ende', upd_out_volumes
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden 1: ",time
    !
 END IF
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !LEO END  DREDGING - automatic initiation of dredging by dredge criteria
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    PRINT*, 'aim_node_depth(308,:)', aim_node_depth(308,:)
!    PRINT*, 'aim_node_depth(251,:)', aim_node_depth(251,:)
    !
!    call system_clock(start,rate,cmax)
    !NEU: Berechnung des fraktionierten Baggervolumens an dieser Stelle
    DO n=1,get_nof_dredge_poly( )
       IF (dredge_poly_to_be_dredged(n)>=1.0_Double) THEN
          DO m=1,SIZE(node_sediment_fraction,2)
             DO i=1,get_nof_nodes( )
                IF(delta_dredge_node_depth(i,n)/=0.0_Double) THEN
                  node_fraction_volume_loc(i,m,n) = ABS(node_sediment_fraction(i,m)*node_area(i)&
                                                             *(delta_dredge_node_depth(i,n))*(1.D0-node_porosity(i)))
                END IF
             END DO
          END DO
       END IF 
    END DO
    !
    !
    

!    call system_clock(start,rate,cmax)
    !
    !! printing the position of dredged nodes, the dredged volumes per node and the operational time on the screen
    DO n=1,get_nof_dredge_poly( )
       IF ( ALLOCATED( node_total_volume_loc ) .AND. ANY(node_total_volume_loc(:,n) > 0.0_Double) .AND. crit_type == 3) THEN
          DO i=1,get_nof_nodes( )
             IF (node_total_volume_loc(i,n) > 0.0_Double) THEN             
                IF (NCSIZE.GT.1) THEN
             	   WRITE(6 , FMT = 7120)    &
                   '%$$%',&
                   time_to_real_seconds(old_time_to_observe(n)-initial_time),&
!                   time_to_real_seconds(act_time-initial_time),&
                   fkm(i),&
                   knolg(i),&
                   node_coord(i,1),&
                   node_coord(i,2),&
                   node_total_volume_loc(i,n)
                ELSE
                   WRITE(6 , FMT = 7120)    &
                   '%$$%',&
                   time_to_real_seconds(old_time_to_observe(n)-initial_time),&
!                   time_to_real_seconds(act_time-initial_time),&
                   fkm(i),&
                   i, &
                   node_coord(i,1),&
                   node_coord(i,2),&
                   node_total_volume_loc(i,n)
                   !LEO TODO check if node_total Volume is correct!
                END IF
             END IF
          END DO
       END IF
    END DO
    !
    !! declarations for a format how to write dredging information on the screen
    7110 FORMAT (A27,' ',A52)
    7120 FORMAT (A4,' ',G15.8,' ',F8.3,' ',I6,' ',G15.8,' ',G15.8,' ',G15.8)    
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden 4: ",time
    !
!    call system_clock(start,rate,cmax)
    !
    !! checking if a minimum volume is reached
    !! ... it is checked whether a minimum volume is reached
    !! ... if not, delta_dredge_node_depth is set to zero and aim_node_depth to node_depth
    DO n=1, get_nof_dredge_poly( )
       IF(ANY(delta_dredge_node_depth(:,n) /= 0.0_Double .AND. minimum_volume(n) > 0.0_Double)) THEN
         !! calculation of sector volumes of elements and their associates
         DO i=1,get_nof_nodes( )
            IF (dredge_node_index(i,n) == 1 .AND. node_total_volume_loc(i,n) > 0.0_Double) THEN
               !! check for nodes in sector and calculate associated dredged volumes
               DO j=1,get_nof_nodes( )
                  IF (SQRT((node_coord(i,1) - node_coord(j,1))**2 + (node_coord(i,2) - node_coord(j,2))**2)&
                       < sector_radius(n) .AND. node_total_volume_loc(j,n) > 0.0_Double) THEN
                     total_volume_in_sector(i,n) = total_volume_in_sector(i,n) + node_total_volume_loc(j,n)
                  END IF 
               END DO
            END IF
         END DO
         !! check if volume of a sector is greater than the minimum volume 
         DO i=1,get_nof_nodes()
            IF (dredge_node_index(i,n) == 1 .AND. total_volume_in_sector(i,n) > minimum_volume(n)) THEN
               !! mark points in sector if volume of this sector is greater than the minimum volume
               DO j=1,get_nof_nodes( )
                  IF (SQRT((node_coord(i,1)-node_coord(j,1))**2+(node_coord(i,2)-node_coord(j,2))**2)&
                       < sector_radius(n) .AND. node_total_volume_loc(j,n) > 0.0_Double) THEN
                     node_to_be_dredged(j) = .true.
                  END IF
               END DO
            END IF
          END DO
       END IF
       !! setting aim node depths and delta_dredge_node_depths to no harming values if the nodes are not to be dredged
       DO i=1,get_nof_nodes()
          IF (.NOT. node_to_be_dredged(i) .AND. node_total_volume_loc(i,n) > 0.0_Double) THEN
             aim_node_depth(i,n)          = node_depth(i)
             delta_dredge_node_depth(i,n) = 0.0_Double
          END IF
          node_to_be_dredged(i) = .false.
       END DO
    END DO
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden 5: ",time
    !
!    call system_clock(start,rate,cmax)
    !
    !! executing deepening of nodes due to dredging
    DO n=1, get_nof_dredge_poly( )
       DO i=1,get_nof_nodes( )
          IF( delta_dredge_node_depth(i,n) /= 0.0_Double ) THEN       
             IF ( dredge_node_index(i,n) == 1 ) THEN       
                IF(node_depth(i) <= aim_node_depth(i,n)) THEN
                  delta_dredge_node_depth(i,n) = 0.0_Double
                ELSE IF(node_depth(i)-dredging_rate(n)*time_to_real_seconds(act_timestep)/node_area(i)<aim_node_depth(i,n)) THEN
                  node_clean(i)                = 1.D0
                  node_depth_dredge(i)         = aim_node_depth(i,n)
                  delta_dredge_node_depth(i,n) = 0.0_Double
                ELSE
                  node_clean(i) = 1.D0
                  node_depth_dredge(i)         = node_depth_dredge(i)-dredging_rate(n)&
                                                 *time_to_real_seconds(act_timestep)/node_area(i)
                END IF
             END IF
          ELSE
             CYCLE
          END IF
       END DO   
    END DO
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden 6: ",time
    !
!    call system_clock(start,rate,cmax)
    !
    !! correction of dredged volumes
    !! ... the therefore actual dredged volumes per time step according to a deepening are calculated here
    !! re-initialization of volume arrays for further use
    !LEO BUG check if allocated
    IF (ALLOCATED(dredge_criterion)) THEN
      node_total_volume_loc    = 0.0_Double
      node_sediment_volume_loc = 0.0_Double
      node_water_volume_loc    = 0.0_Double
      node_fraction_volume_loc = 0.0_Double
      total_volume_loc         = 0.0_Double
      sediment_volume_loc      = 0.0_Double
      water_volume_loc         = 0.0_Double
      fraction_volume_loc      = 0.0_Double
    END IF
    !
    IF ( ALLOCATED(node_total_volume) .AND. ANY(node_clean(:)==1.D0) ) THEN
       DO n=1,get_nof_dredge_poly( )
          DO i=1,get_nof_nodes( )
             IF (dredge_node_index(i,n) == 1 .AND. node_clean(i) == 1.D0) THEN
       	        node_total_volume(i,n)     = node_total_volume(i,n)+ABS(node_depth_dredge(i)&
       	                                     -node_depth(i))*node_area(i)
!       	        node_sediment_volume(i,n)  = node_sediment_volume(i,n)+ABS(node_depth_dredge(i)&
!       	                                     -node_depth(i))*node_area(i)*(1-node_porosity(i))
!       	        node_water_volume(i,n)     = node_water_volume(i,n)+ABS(node_depth_dredge(i)&
!       	                                     -node_depth(i))*node_area(i)*node_porosity(i)
!       	        node_fraction_volume(i,:,n)= node_fraction_volume(i,:,n)+ABS(node_depth_dredge(i)&
!       	                                     -node_depth(i))*node_area(i)*node_sediment_fraction(i,:)*(1-node_porosity(i))
       	     ELSE
       	     	node_total_volume(i,n)      = 0.0_Double
!       	     	node_sediment_volume(i,n)   = 0.0_Double
!       	     	node_water_volume(i,n)      = 0.0_Double
!       	     	node_fraction_volume(i,:,n) = 0.0_Double
       	     END IF
          END DO
!          !NEU: Berechnung des fraktionierten Baggervolumens an dieser Stelle
!          DO m=1,SIZE(node_sediment_fraction,2)
!             DO i=1,get_nof_nodes( )
!                IF (dredge_node_index(i,n) == 1 .AND. node_clean(i) == 1.D0) THEN
!                   node_fraction_volume(i,m,n)= node_fraction_volume(i,m,n)+ABS(node_depth_dredge(i)&
!       	                                     -node_depth(i))*node_area(i)*node_sediment_fraction(i,m)*(1-node_porosity(i))
!       	        ELSE
!       	           node_fraction_volume(i,m,n) = 0.0_Double
!       	        END IF
!       	     END DO
!          END DO
       END DO
    END IF   	
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden 7: ",time
    !
    !! determination of dispose polys for disposal of dredged material
    IF (ANY(node_total_volume(:,:)>0.0_Double)) THEN
       IF (.NOT. ALLOCATED(disp_poly_names_loc)) THEN
          ALLOCATE(disp_poly_names_loc(get_nof_nodes( )))
          ALLOCATE(disp_weighting_factor_loc(get_nof_nodes( )))
          disp_poly_names_loc = 'no_disp_action'
          disp_weighting_factor_loc = 0.0_Double  
          IF (nof_dispose_poly >=1) THEN
             ALLOCATE(x_Factor(nof_dispose_poly, get_nof_dredge_poly()))
             ALLOCATE(diff_array(get_nof_nodes( )))
             DO k=1,get_nof_dredge_poly( )
                DO n=1,nof_dispose_poly
                   x_Factor_Num   = 0.0_Double
                   x_Factor_Denom = 0.0_Double
                   DO i=1,get_nof_nodes( )
                      IF (dispose_node_index(i,k,n) == 1) THEN
                          x_Factor_Num   = x_Factor_Num+fkm(i)*node_area(i)
                          x_Factor_Denom = x_Factor_Denom+node_area(i)
                      END IF
                   END DO
                   IF (dispose_poly_name(n,k)=='not_defined') THEN
                       x_Factor(n,k)=1000000.0
                   ELSE
                       IF (x_Factor_Denom /= 0.0_Double) x_Factor(n,k) = x_Factor_Num/x_Factor_Denom
                   END IF
                END DO
                DO i=1,get_nof_nodes( )
                   IF (node_total_volume(i,k) /= 0.0_Double) THEN
                      diff_array(i)=MINVAL(ABS(x_Factor(:,k)-fkm(i)))
                   END IF
                END DO 
                DO n=1, nof_dispose_poly 
                   DO i=1,get_nof_nodes( )
                      IF (ABS(x_Factor(n,k)-fkm(i)) == diff_array(i)) THEN
                         disp_poly_names_loc(i)       = dispose_poly_name(n,k)
                         disp_weighting_factor_loc(i) = dispose_weighting_factor(n,k)
                      END IF
                   END DO
                END DO
             END DO
             DEALLOCATE(x_Factor)
             DEALLOCATE(diff_array)
          END IF
       END IF
    END IF
!    call system_clock(start,rate,cmax)
    !! printing the position of actually dredged nodes, the dredged volumes per node and the operational time on the screen
    IF ( ALLOCATED( node_total_volume ) ) THEN
       DO n=1,get_nof_dredge_poly( )
       DO i=1,get_nof_nodes( )
          IF ( node_total_volume(i,n) > 0.0_Double .AND. delta_dredge_node_depth(i,n) == 0.0_Double) THEN
             IF (crit_type == 3) THEN
                IF (NCSIZE.GT.1) THEN
             	   WRITE(6 , FMT = 9120)    &
                   '$%%$',&
                   time_to_real_seconds(old_time_to_observe(n)-initial_time),&
!                   time_to_real_seconds(time_to_observe(n)-initial_time),&
                   time_to_real_seconds(act_time-initial_time),&
                   fkm(i),&
                   knolg(i),&
                   node_coord(i,1),&
                   node_coord(i,2),&
                   node_total_volume(i,n), &
                   disp_poly_names_loc(i),&
                   disp_weighting_factor_loc(i)
                ELSE
                   WRITE(6 , FMT = 9120)    &
                   '$%%$',&
                   time_to_real_seconds(old_time_to_observe(n)-initial_time),&
!                   time_to_real_seconds(time_to_observe(n)-initial_time),&
                   time_to_real_seconds(act_time-initial_time),&
                   fkm(i),&
                   i, &
                   node_coord(i,1),&
                   node_coord(i,2),&
                   node_total_volume(i,n), &
                   disp_poly_names_loc(i),&
                   disp_weighting_factor_loc(i)
                   !LEO TODO check also this!
                END IF
             END IF
             node_total_volume_loc(i,n)    = node_total_volume(i,n)
             node_total_volume(i,n)        = 0.0_Double
!             node_sediment_volume_loc(i,n) = node_sediment_volume(i,n)
!             node_sediment_volume(i,n)     = 0.0_Double
!             node_water_volume_loc(i,n)    = node_water_volume(i,n)
!             node_water_volume(i,n)        = 0.0_Double
!             DO m=1,SIZE(node_sediment_fraction,2)
!                node_fraction_volume_loc(i,m,n) = node_fraction_volume(i,m,n)
!                node_fraction_volume(i,m,n)     = 0.0_Double
!             END DO
          END IF
       END DO
!       !NEU: Berechnung des fraktionierten Baggervolumens an dieser Stelle
!       DO m=1,SIZE(node_sediment_fraction,2)
!          DO i=1,get_nof_nodes( )
!             IF ( node_total_volume(i,n) > 0.0_Double .AND. delta_dredge_node_depth(i,n) == 0.0_Double) THEN
!                node_fraction_volume_loc(i,m,n) = node_fraction_volume(i,m,n)
!                node_fraction_volume(i,m,n)     = 0.0_Double
!             END IF
!          END DO
!       END DO
       END DO
       IF (ALLOCATED(disp_poly_names_loc))       DEALLOCATE(disp_poly_names_loc)
       IF (ALLOCATED(disp_weighting_factor_loc)) DEALLOCATE(disp_weighting_factor_loc)
    END IF
    !
    !! declarations for a format how to write information on a screen
    9110 FORMAT (A27,' ',A52)
    9120 FORMAT (A4,' ',G15.8,' ',G15.8,' ',F8.3,' ',I6,' ',G15.8,' ',G15.8,' ',G15.8,' ',A16,' ',F8.3)    
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden 8: ",time
    !
!    call system_clock(start,rate,cmax)
    !
    !! calculating the total amount of actually dredged material
    !! ... includes parallel routines
    DO n=1,get_nof_dredge_poly( )
       IF(ANY(node_total_volume_loc(:,n) > 0.0_Double)) THEN       	  
          IF (NCSIZE.GT.1) THEN
             ! different procedure from above - here the volume of dredged material of interface nodes
             ! ... is divided by the number of parallel processors the node is belonging to
             DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                IF (dredge_node_index(node_neighb(i),n)==1) THEN
                    k=1             
                    DO j=i+1,i+4
                       IF (node_neighb(j)>= 0) k=k+1
                    END DO
                    node_total_volume_loc(node_neighb(i),n)      = node_total_volume_loc(node_neighb(i),n)/k
!                    node_sediment_volume_loc(node_neighb(i),n)   = node_sediment_volume_loc(node_neighb(i),n)/k
!                    node_water_volume_loc(node_neighb(i),n)      = node_water_volume_loc(node_neighb(i),n)/k
!                    node_fraction_volume_loc(node_neighb(i),:,n) = node_fraction_volume_loc(node_neighb(i),:,n)/k
                END IF
             END DO
          END IF
          node_sediment_volume_loc(:,n)  = node_sediment_volume(:,n)+node_total_volume_loc(:,n)*(1-node_porosity(:))
          node_water_volume_loc(:,n)     = node_water_volume(:,n)+node_total_volume_loc(:,n)*node_porosity(:)
          total_volume_loc(n)    = total_volume_loc(n)    + SUM(node_total_volume_loc(:,n))
          sediment_volume_loc(n) = sediment_volume_loc(n) + SUM(node_sediment_volume_loc(:,n))
          water_volume_loc(n)    = water_volume_loc(n)    + SUM(node_water_volume_loc(:,n))
          DO m=1,SIZE(node_sediment_fraction,2)
             node_fraction_volume_loc(:,m,n)= node_fraction_volume(:,m,n)+node_sediment_volume_loc(:,n)&
                  *node_sediment_fraction(:,m)
             fraction_volume_loc(m,n) = fraction_volume_loc(m,n) + SUM(node_fraction_volume_loc(:,m,n))
          END DO
       END IF       
    END DO
    !
    !! calculating the total amount of actually dredged fractions material
    !! ... includes parallel routines    
!    DO n=1,get_nof_dredge_poly( )
!       DO m=1,SIZE(node_sediment_fraction,2)
!          IF(ANY(node_fraction_volume_loc(:,m,n) > 0.0_Double)) THEN 
!             IF (NCSIZE.GT.1) THEN
!                DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
!                   IF (dredge_node_index(node_neighb(i),n)==1) THEN
!                       k=1             
!                       DO j=i+1,i+4
!                          IF (node_neighb(j)>= 0) k=k+1
!                       END DO
!                       node_fraction_volume_loc(node_neighb(i),m,n) = node_fraction_volume_loc(node_neighb(i),m,n)/k
!                   END IF
!                END DO
!             END IF
!             fraction_volume_loc(m,n) = fraction_volume_loc(m,n) + SUM(node_fraction_volume_loc(:,m,n))
!          END IF
!       END DO
!    END DO
    !
    !! summation over all proc ids
    DO n=1,get_nof_dredge_poly( )
       IF(NCSIZE.GT.1) THEN
         total_volume_loc(n)    = p_dsum(total_volume_loc(n))
         sediment_volume_loc(n) = p_dsum(sediment_volume_loc(n))
         water_volume_loc(n)    = p_dsum(water_volume_loc(n))
         DO m=1,SIZE(node_sediment_fraction,2)
            fraction_volume_loc(m,n) = p_dsum(fraction_volume_loc(m,n))
         END DO
       END IF
       total_volume(n)      = total_volume(n)+total_volume_loc(n)
       sediment_volume(n)   = sediment_volume(n)+sediment_volume_loc(n)
       water_volume(n)      = water_volume(n)+water_volume_loc(n)
       all_total_volume(n) = all_total_volume(n) + total_volume_loc(n)
       all_sediment_volume(n) = all_sediment_volume(n) + sediment_volume_loc(n)
       all_water_volume(n) = all_water_volume(n) + water_volume_loc(n)
       DO m=1,SIZE(node_sediment_fraction,2)
          fraction_volume(m,n) = fraction_volume(m,n)+fraction_volume_loc(m,n)
          all_fraction_volume(m,n) = all_fraction_volume(m,n) + fraction_volume_loc(m,n)
       END DO
    END DO
    !    
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden 9: ",time
    !
!    call system_clock(start,rate,cmax)
    !
!    !! writing real amounts of dredged material to an output file after dredging is completed
    DO n=1, get_nof_dredge_poly( )
       IF ( total_volume (n) /= 0.0_Double .AND. total_volume_loc(n) == 0.0_Double  ) THEN
          end_dredge_time(n) = act_time-act_timestep
!          PRINT*, 'end_dredge_time', datetime_to_string(end_dredge_time(n))
!          upd_out_volumes(n) = .true.
!          IF (NCSIZE.GT.1) THEN
!              IF (ipid==0) THEN
!                  CALL write_calculated_dredged_volume ()
!              END IF
!              all_total_volume(n) = all_total_volume(n) + total_volume(n)
!              all_sediment_volume(n) = all_sediment_volume(n) + sediment_volume(n)
!              all_water_volume(n) = all_water_volume(n) + water_volume(n)
!              all_fraction_volume(:,n) = all_fraction_volume(:,n) + fraction_volume(:,n)
              IF (ALLOCATED(total_volume)) total_volume(n) = 0.0_Double
              IF (ALLOCATED(water_volume)) water_volume(n) = 0.0_Double
              IF (ALLOCATED(sediment_volume) .AND. nof_dispose_poly == 0) sediment_volume(n)   = 0.0_Double
              IF (ALLOCATED(fraction_volume) .AND. nof_dispose_poly == 0) fraction_volume(:,n) = 0.0_Double
!          ELSE
!              CALL write_calculated_dredged_volume ()
!              all_total_volume(n) = all_total_volume(n) + total_volume(n)
!              all_sediment_volume(n) = all_sediment_volume(n) + sediment_volume(n)
!              all_water_volume(n) = all_water_volume(n) + water_volume(n)
!              all_fraction_volume(:,n) = all_fraction_volume(:,n) + fraction_volume(:,n)
!              IF (ALLOCATED(total_volume)) total_volume(n) = 0.0_Double
!              IF (ALLOCATED(water_volume)) water_volume(n) = 0.0_Double
!              IF (ALLOCATED(sediment_volume) .AND. nof_dispose_poly == 0) sediment_volume(n)   = 0.0_Double
!              IF (ALLOCATED(fraction_volume) .AND. nof_dispose_poly == 0) fraction_volume(:,n) = 0.0_Double
!          END IF                
       END IF
    END DO
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden 10: ",time
    !
    !! deallocating local data
    IF ( ALLOCATED( crit_depth   ) ) DEALLOCATE( crit_depth   )
    IF ( ALLOCATED( dredge_depth ) ) DEALLOCATE( dredge_depth )
    DEALLOCATE(node_to_be_dredged)
    DEALLOCATE(dredge_poly_to_be_dredged)
    !
    !! Saving dredged sediment volumes per node for a restart
    !LEO BUG check if allocated
    IF (ALLOCATED(dredge_criterion)) THEN
      node_sediment_volume_rs = node_sediment_volume_loc
      node_fraction_volume_rs = node_fraction_volume_loc
    END IF
!    PRINT*, 'MAXVAL(node_sediment_volume_rs(:,:))', MAXVAL(node_sediment_volume_rs(:,:))
!    PRINT*, 'MINVAL(node_sediment_volume_rs(:,:))', MINVAL(node_sediment_volume_rs(:,:))
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden Kritbaggern: ",time
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! DISPOSAL - automatic deposition of automatically dredged material !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    !! 1. preparation of dipsosing dredged material
    !!    a) allocating arrays
    !!    b) transfer of element areas inside a disposal polygon
    !!    c) calculation of heightening of elements due to disposal of material
    !!    d) operations for dumping material dredged by use of referenced surface criterion
    !
    !! a)
    !
!    call system_clock(start,rate,cmax)
    !
    IF (nof_dispose_poly >=1) THEN
       !! allocation of local data
       IF (.NOT. ALLOCATED(x_Factor)) THEN
           ALLOCATE(x_Factor(nof_dispose_poly, get_nof_dredge_poly()))
       END IF       
       IF (.NOT. ALLOCATED(dredged_fractions_to_disp)) THEN
          ALLOCATE(dredged_fractions_to_disp(get_nof_nodes( ), SIZE(node_sediment_fraction,2)))
          dredged_fractions_to_disp=0.0_Double
       END IF
       IF (.NOT. ALLOCATED(dredged_sediment_volume_to_disp_loc)) THEN
          ALLOCATE(dredged_sediment_volume_to_disp_loc(get_nof_dredge_poly( ), nof_dispose_poly))
          dredged_sediment_volume_to_disp_loc=0.0_Double
       END IF
       IF (.NOT. ALLOCATED(dredged_fraction_volume_to_disp_loc)) THEN
          ALLOCATE(dredged_fraction_volume_to_disp_loc(get_nof_dredge_poly( ), nof_dispose_poly, SIZE(node_sediment_fraction,2)))
          dredged_fraction_volume_to_disp_loc=0.0_Double
       END IF
       IF (.NOT. ALLOCATED(disp_sediment_volume_loc)) THEN
          ALLOCATE(disp_sediment_volume_loc(nof_dispose_poly, get_nof_dredge_poly( )))
          disp_sediment_volume_loc=0.0_Double
       END IF
       IF (.NOT. ALLOCATED(disp_node_total_volume_loc)) THEN
          ALLOCATE(disp_node_total_volume_loc(get_nof_nodes( ), get_nof_dredge_poly( ), nof_dispose_poly))
          disp_node_total_volume_loc=0.0_Double
       END IF
       IF (.NOT. ALLOCATED(disp_total_volume_loc)) THEN
          ALLOCATE(disp_total_volume_loc(nof_dispose_poly, get_nof_dredge_poly( )))
          disp_total_volume_loc=0.0_Double
       END IF
       IF (.NOT. ALLOCATED(disp_sediment_volume_loc)) THEN
          ALLOCATE(disp_sediment_volume_loc(nof_dispose_poly, get_nof_dredge_poly( )))
          disp_sediment_volume_loc=0.0_Double
       END IF
       IF (.NOT. ALLOCATED(disp_fraction_volume_loc)) THEN
          ALLOCATE(disp_fraction_volume_loc(SIZE(node_sediment_fraction,2), nof_dispose_poly, get_nof_dredge_poly( )))
          disp_fraction_volume_loc=0.0_Double
       END IF
       !! loop over all dredge polygons
       DO k=1,get_nof_dredge_poly( )
          ALLOCATE (node_areas_inside     (get_nof_nodes( ), nof_dispose_poly))
          ALLOCATE (sum_node_areas_inside (nof_dispose_poly))       
          node_areas_inside     = 0.0_Double
          sum_node_areas_inside = 0.0_Double
          !! transfering values from dispose_poly_index to associated nodes in dispose_node_index to calculate on nodes
          DO n=1,nof_dispose_poly
             !! b)
             DO i=1,get_nof_nodes()
                IF (dispose_node_index(i,k,n)==1) node_areas_inside(i,n)=node_area(i)
             END DO
             sum_node_areas_inside(n) = SUM(node_areas_inside(:,n))
             !! for parallel computation
             IF (NCSIZE.GT.1) THEN
                !! substracting the area of interface nodes one time
                doppelt = 0.D0
                DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                  IF (dispose_node_index(node_neighb(i),k,n)==1) THEN
                     p=1             
                     DO j=i+1,i+4
                        IF (node_neighb(j)>= 0) p=p+1
                     END DO
                     doppelt = doppelt + node_area(node_neighb(i))/p
                 END IF
                END DO
                sum_node_areas_inside(n) = sum_node_areas_inside(n) - doppelt
                sum_node_areas_inside(n) = p_dsum(sum_node_areas_inside(n))
             ENDIF
          END DO
          !
          IF (.NOT. ALLOCATED(diff_array)) THEN
       	     ALLOCATE(diff_array(get_nof_nodes( )))
             diff_array=c_undef_dp
          END IF
          IF (.NOT. ALLOCATED(check_node_total_volume)) THEN
             ALLOCATE(check_node_total_volume(get_nof_nodes( )))
             check_node_total_volume=0.0_Double
       	     check_total_volume=0.0_Double
          END IF
          !
          !! calculating aim node depths (called disp_node_depth)
          !
          !! if crit_type is not equal 3 the disposal depth is calculated for all given disposal polygons according to
          !! ... the dispose weighting factor
          !! if crit_type equals 3 the next disposal polygon is calculated --> for this procedure the name of the disposal
          !! ... polygons have to resemble the position within the length of a river
          IF ( ANY(node_sediment_volume_loc(:,k) /= 0.0_Double) ) THEN
             !! c)
             crit_type = get_criterion_crit_type( dredge_criterion(k) )
             IF (crit_type /= 3) THEN
                IF (ALL(node_sediment_volume_loc(:,k)==0.0_Double)) CYCLE
                DO n=1, nof_dispose_poly
                   IF (dispose_weighting_factor(n,k) == 0.0_Double) CYCLE
                   DO i=1,get_nof_nodes( )
                      dredged_sediment_volume_to_disp_loc(k,n)   = dredged_sediment_volume_to_disp_loc(k,n)&
                                                        +node_sediment_volume_loc(i,k)
                      node_sediment_volume_loc(i,k)          = 0.0_Double
                      dredged_fraction_volume_to_disp_loc(k,n,:) = dredged_fraction_volume_to_disp_loc(k,n,:)&
                                                        +node_fraction_volume_loc(i,:,k)
                      node_fraction_volume_loc(i,:,k)        = 0.0_Double
                   END DO
                END DO
             ELSE
                DO n=1,nof_dispose_poly
                   x_Factor_Num   = 0.0_Double
                   x_Factor_Denom = 0.0_Double
                   DO i=1,get_nof_nodes( )
                      IF (dispose_node_index(i,k,n) == 1) THEN
                          x_Factor_Num   = x_Factor_Num+fkm(i)*node_area(i)
                          x_Factor_Denom = x_Factor_Denom+node_area(i)
                      END IF
                   END DO
                   IF (dispose_poly_name(n,k)=='not_defined') THEN
                      x_Factor(n,k)=1000000.0
                   ELSE
                      IF (x_Factor_Denom /= 0.0_Double) x_Factor(n,k) = x_Factor_Num/x_Factor_Denom
!				   READ (dispose_poly_name(n,k),*) x_Factor(n,k)
                   END IF
                END DO
                DO i=1,get_nof_nodes( )
                   IF (node_sediment_volume_loc(i,k) /= 0.0_Double) THEN
                      diff_array(i)=MINVAL(ABS(x_Factor(:,k)-fkm(i)))
                   END IF
                END DO 
                DO n=1, nof_dispose_poly 
                   DO i=1,get_nof_nodes( )
                      IF (ABS(x_Factor(n,k)-fkm(i)) == diff_array(i)) THEN
                         dredged_sediment_volume_to_disp_loc(k,n)   = dredged_sediment_volume_to_disp_loc(k,n)&
                                                                + node_sediment_volume_loc(i,k)
                         node_sediment_volume_loc(i,k)          = 0.0_Double
                         dredged_fraction_volume_to_disp_loc(k,n,:) = dredged_fraction_volume_to_disp_loc(k,n,:)&
                                                                + node_fraction_volume_loc(i,:,k)
                         node_fraction_volume_loc(i,:,k)        = 0.0_Double
                      END IF
                   END DO
                END DO
             END IF
          END IF
          !
          !!parallel routines
          DO n=1, nof_dispose_poly
             !! summation of total volumes to disp over proc ids
             IF (NCSIZE.GT.1) THEN
                 dredged_sediment_volume_to_disp_loc(k,n) = p_dsum(dredged_sediment_volume_to_disp_loc(k,n))
                 DO p=1,SIZE(node_sediment_fraction,2)
                   dredged_fraction_volume_to_disp_loc(k,n,p) = p_dsum(dredged_fraction_volume_to_disp_loc(k,n,p))
                 END DO
             END IF
          END DO
          DO n=1, nof_dispose_poly
             IF (ALL(delta_disp_node_depth(:,k,n)==0.0_Double)) THEN
             	dredged_sediment_volume_to_disp(k,n) = dredged_sediment_volume_to_disp_loc(k,n)
             	dredged_fraction_volume_to_disp(k,n,:) = dredged_fraction_volume_to_disp_loc(k,n,:)
             ELSE
             	dredged_sediment_volume_to_disp(k,n) = dredged_sediment_volume_to_disp(k,n)+ &
                  dredged_sediment_volume_to_disp_loc(k,n)
             	dredged_fraction_volume_to_disp(k,n,:) = dredged_fraction_volume_to_disp(k,n,:)+ &
                  dredged_fraction_volume_to_disp_loc(k,n,:)
             END IF
          END DO
          !
          !! calculating disp_node_depths for all crit types
          IF (disp_scours_auto(k) == 'YES') THEN
             disposing_scours=.true.
          ELSE
             disposing_scours=.false.
          END IF
          !
          DO n=1, nof_dispose_poly
             !! calculation of disp_node_depths
             IF (dredged_sediment_volume_to_disp_loc(k,n) > 0.0_Double) THEN
!             	PRINT*, 'dredged_sediment_volume_to_disp_loc(k,n)', dredged_sediment_volume_to_disp_loc(k,n)
!                PRINT*, 'dredged_sediment_volume_to_disp(k,n)', dredged_sediment_volume_to_disp(k,n)
!                PRINT*, 'dredged_fraction_volume_to_disp_loc(k,n,:)', dredged_fraction_volume_to_disp_loc(k,n,:)
!                PRINT*, 'dredged_fraction_volume_to_disp(k,n,:)', dredged_fraction_volume_to_disp(k,n,:)
                IF (.NOT. disposing_scours) THEN
                   !! calculating disp_node_depth if not disposing in scours
                   check_node_total_volume = 0.0_Double
                   check_total_volume      = 0.0_Double
                   sum_of_evolutions = 0.0_Double
                   sum_of_evolutions = SUM(delta_disp_node_depth(:,k,n))
                   IF (NCSIZE.GT.1) sum_of_evolutions = p_dsum(sum_of_evolutions)
                   !! startup values for scour_node_depth depending on already calculated delta_disp_node_depths
                   IF (sum_of_evolutions == 0.0_Double) disp_node_depth(:,k,n) = node_depth(:)              
                   !
                   DO j=1, get_nof_nodes( )
                      IF (dispose_weighting_factor(n,k) == 0.0_Double) THEN 
                         CYCLE
                      ELSEIF (dispose_node_index(j,k,n) == 1) THEN
                         delta_disp_node_depth(j,k,n) = delta_disp_node_depth(j,k,n)+dispose_weighting_factor(n,k)&
                                              *dredged_sediment_volume_to_disp_loc(k,n)/(sum_node_areas_inside(n)* &
                                              (1-node_porosity(j)))
                         disp_node_depth(j,k,n)       = disp_node_depth(j,k,n)+dispose_weighting_factor(n,k)&
                                              *dredged_sediment_volume_to_disp_loc(k,n)/(sum_node_areas_inside(n)*&
                                              (1-node_porosity(j)))
                         disp_node_total_volume(j,k,n) = delta_disp_node_depth(j,k,n)*node_area(j)
                      END IF
                   END DO
                   check_total_volume = SUM(check_node_total_volume(:))
                   !! parallel routines
                   IF (NCSIZE.GT.1) THEN
                      doppelt = 0.D0
                      DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                         IF (dispose_node_index(node_neighb(i),k,n) == 1 .AND.&
                             check_node_total_volume(node_neighb(i)) > 0.0_Double) THEN
                             p=1             
                             DO j=i+1,i+4
                                IF (node_neighb(j)>= 0) p=p+1
                             END DO
                             doppelt = doppelt + check_node_total_volume(node_neighb(i))/p
                         END IF
                      END DO
                      check_total_volume = check_total_volume - doppelt
                      check_total_volume = p_dsum(check_total_volume)
                   END IF
                ELSE
                   ALLOCATE(scour_node_depth_loc(get_nof_nodes()))
                   scour_node_depth_loc = 0.0_Double
                   DO i=1,get_nof_nodes( )
                      IF (delta_disp_node_depth(i,k,n)/=0.0_Double) THEN
                      	 scour_node_depth_loc(i) = disp_node_depth(i,k,n)
                      ELSE
                      	 scour_node_depth_loc(i) = node_depth(i)
                         disp_node_depth(i,k,n) = node_depth(i)
                      END IF
                   END DO
!                   !! calculating delta_disp_node_depth if disposing in scours (only for crit_type == 3)
!                   sum_of_evolutions = 0.0_Double
!                   sum_of_evolutions = SUM(delta_disp_node_depth(:,k,n))
!                   IF (NCSIZE.GT.1) sum_of_evolutions = p_dsum(sum_of_evolutions)
!                   !! startup values for scour_node_depth depending on already calculated delta_disp_node_depths
!                   IF (sum_of_evolutions == 0.0_Double) THEN
!                      scour_node_depth_loc(:) = node_depth(:)
!                      disp_node_depth(:,k,n) = node_depth(:)              
!                   ELSE
!                      scour_node_depth_loc(:) = disp_node_depth(:,k,n)
!                   END IF
                   !
!                   IF (sum_of_evolutions == 0.0_Double) THEN
                      !! iterative procedure to find act_depo_depth_disp_scours
                      act_depo_depth_disp_scours=min_depo_depth_disp_scours
                      DO m=1,nof_iterations_disp_scours
                         little_helper_1            = 0.0_Double
                         little_helper_2            = 0.0_Double
                         little_helper_3            = 0.0_Double
                         check_total_volume         = 0.0_Double
                         check_node_total_volume(:) = 0.0_Double
                         sum_porosity_inside        = 0.0_Double
                         average_porosity           = 0.0_Double
                         nof_nodes_inside           = 0
                         DO i=1,get_nof_nodes( )
                            IF (dispose_node_index(i,k,n) == 1 .AND. scour_node_depth_loc(i) < free_surface_node_depth(i)&
                                                                          -act_depo_depth_disp_scours) THEN
                                little_helper_1     = little_helper_1+free_surface_node_depth(i)*node_area(i)
                                little_helper_2     = little_helper_2+scour_node_depth_loc(i)*node_area(i)
                                little_helper_3     = little_helper_3+node_area(i)
                                sum_porosity_inside = sum_porosity_inside+node_porosity(i)
                                nof_nodes_inside    = nof_nodes_inside+1
                            END IF
                         END DO
                         !! parallel routines
                         IF (NCSIZE.GT.1) THEN
                            doppelt = 0.D0
                            DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                               IF (dispose_node_index(node_neighb(i),k,n)==1 .AND. scour_node_depth_loc(node_neighb(i))&
                                   < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                                   p=1             
                                   DO j=i+1,i+4
                                      IF (node_neighb(j) >= 0) p=p+1
                                   END DO
                                   doppelt = doppelt + free_surface_node_depth(node_neighb(i))*node_area(node_neighb(i))/p
                               END IF
                            END DO
                            little_helper_1 = little_helper_1 - doppelt
                            doppelt = 0.D0
                            DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                               IF (dispose_node_index(node_neighb(i),k,n)==1 .AND. scour_node_depth_loc(node_neighb(i))&
                                   < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                                   p=1             
                                   DO j=i+1,i+4
                                      IF (node_neighb(j)>= 0) p=p+1
                                   END DO
                                   doppelt = doppelt + scour_node_depth_loc(node_neighb(i))*node_area(node_neighb(i))/p
                               END IF
                            END DO
                            little_helper_2 = little_helper_2 - doppelt
                            doppelt = 0.D0
                            DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                               IF (dispose_node_index(node_neighb(i),k,n)==1 .AND. scour_node_depth_loc(node_neighb(i))&
                                   < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                                   p=1             
                                   DO j=i+1,i+4
                                      IF (node_neighb(j)>= 0) p=p+1
                                   END DO
                                   doppelt = doppelt + node_area(node_neighb(i))/p
                               END IF
                            END DO
                            little_helper_3     = little_helper_3 - doppelt
                            !
                            sum_porosity_inside = p_dsum(sum_porosity_inside)
                            nof_nodes_inside    = p_dsum(nof_nodes_inside)
                            !
                            little_helper_1     = p_dsum(little_helper_1)
                            little_helper_2     = p_dsum(little_helper_2)
                            little_helper_3     = p_dsum(little_helper_3)
                         END IF
                         !! calculating average porosity
                         IF (nof_nodes_inside > 0) average_porosity = sum_porosity_inside/nof_nodes_inside
                         !! calculating depth below reference surface level
                         act_depo_depth_disp_scours=(ABS(little_helper_1-little_helper_2)-&
                                                      dredged_sediment_volume_to_disp_loc(k,n)&
			      	                                    /(1-average_porosity))/little_helper_3
                         !
                         DO i=1,get_nof_nodes( )
                            IF (dispose_node_index(i,k,n) == 1 .AND. scour_node_depth_loc(i) < free_surface_node_depth(i)&
                                                                        -act_depo_depth_disp_scours) THEN
                               !! calculation of new node depths, differential depths and disposed volumes 
                               disp_node_depth(i,k,n)       = free_surface_node_depth(i)-act_depo_depth_disp_scours
                               delta_disp_node_depth(i,k,n) = disp_node_depth(i,k,n)-node_depth(i)
                               check_node_total_volume(i)     = (disp_node_depth(i,k,n)-scour_node_depth_loc(i))*node_area(i)
                            ELSE
                               delta_disp_node_depth(i,k,n) = 0.0_Double
                               disp_node_depth(i,k,n)       = node_depth(i)                       
                            END IF
                         END DO
                         !
                         check_total_volume = SUM(check_node_total_volume(:))
                         !! parallel routines
                         IF (NCSIZE.GT.1) THEN
                            doppelt = 0.D0
                            DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                               IF (dispose_node_index(node_neighb(i),k,n) == 1 .AND.&
                                  check_node_total_volume(node_neighb(i)) > 0.0_Double) THEN
                                  p=1             
                                  DO j=i+1,i+4
                                     IF (node_neighb(j)>= 0) p=p+1
                                  END DO
                                  doppelt = doppelt + check_node_total_volume(node_neighb(i))/p
                               END IF
                            END DO
                            check_total_volume = check_total_volume - doppelt
                            check_total_volume = p_dsum(check_total_volume)
                         END IF
                         !! check whether...
                         !! a) storage capacity is sufficient
                         !! b) criterion for end of iteration is fulfilled
                         !! c) iteration failed
                         IF (m == 1 .AND. act_depo_depth_disp_scours < min_depo_depth_disp_scours) THEN
                            PRINT*, 'Disposing failed: Storage capacity of the scour is lower than the volume to dispose.'
                            delta_disp_node_depth(:,k,n)=0.0_Double
                            check_total_volume=0.0_Double
                            EXIT
                         ELSEIF (ABS(check_total_volume-dredged_sediment_volume_to_disp_loc(k,n)/(1-average_porosity))&
                                 <= max_error_disp_scours) THEN
!                            PRINT*, 'Iteration for disposing scours successfully finished.'
                            EXIT
                         ELSEIF (m==nof_iterations_disp_scours .AND. ABS(check_total_volume-&
                                 dredged_sediment_volume_to_disp_loc(k,n)&
                                 /(1-average_porosity)) > max_error_disp_scours) THEN
                            PRINT*, 'Iteration failed. Increase number of iterations or maximum error for disposing scours.'
                            delta_disp_node_depth(:,k,n)=0.0_Double
                            check_total_volume=0.0_Double
                         ELSE
                            CYCLE
                         END IF
                      END DO
                   DEALLOCATE(scour_node_depth_loc)
                END IF
                !
             END IF
          END DO
          !
          !! 2. heightening of nodes due to disposal
          DO n=1,nof_dispose_poly
             DO i=1,get_nof_nodes( )
                IF ( delta_disp_node_depth(i,k,n) /= 0.0_Double ) THEN
                   disp_node_depth(i,k,n)= node_depth(i)+delta_disp_node_depth(i,k,n)
                   IF (dispose_node_index(i,k,n) == 1) THEN
                      IF (node_depth(i) >= disp_node_depth(i,k,n)) THEN
                         delta_disp_node_depth(i,k,n)  = 0.0_Double
                      ELSE IF (node_depth(i)+disposal_rate(k)*time_to_real_seconds(act_timestep)/(node_area(i)&
                             *(1-node_porosity(i))) > disp_node_depth(i,k,n)) THEN
                         node_supply(i)              = 1.D0
                         node_depth_supply(i)        = disp_node_depth(i,k,n)
                         disp_node_total_volume_loc(i,k,n) = ABS(disp_node_depth(i,k,n)-node_depth(i))*node_area(i)
                      ELSE
                         node_supply(i)              = 1.D0
                         node_depth_supply(i)        = node_depth_supply(i)+disposal_rate(k)*time_to_real_seconds(act_timestep)&
                                                    /(node_area(i)*(1-node_porosity(i)))
                         delta_disp_node_depth(i,k,n)  = delta_disp_node_depth(i,k,n)-disposal_rate(k)*&
                                                    time_to_real_seconds(act_timestep)&
                                                    /(node_area(i)*(1-node_porosity(i)))
                         disp_node_total_volume_loc(i,k,n) = ABS(disposal_rate(k)*time_to_real_seconds(act_timestep)&
                                                    /(node_area(i)*(1-node_porosity(i))))*node_area(i)
                      END IF
!                         node_sediment_fraction(i,:) = dredged_fraction_volume_to_disp(k,n,:)/dredged_sediment_volume_to_disp(k,n)
                   END IF
                END IF
             END DO
          END DO
          !deallocating local data
          IF (ALLOCATED(node_areas_inside )) DEALLOCATE(node_areas_inside)
          IF (ALLOCATED(sum_node_areas_inside )) DEALLOCATE(sum_node_areas_inside)
       END DO
       !! transfering sediment distribution to dispose
       IF (ANY(dredged_sediment_volume_to_disp /= 0.0_Double) .AND. ANY(delta_disp_node_depth /=0.0_Double) ) THEN    
          DO i=1,get_nof_nodes( )
             j=0
             DO k=1,get_nof_dredge_poly( )
		DO n=1,nof_dispose_poly
                   IF (dispose_node_index(i,k,n) == 1 .AND. dredged_sediment_volume_to_disp(k,n) > 0.0_Double) THEN
                      dredged_fractions_to_disp(i,:) = dredged_fractions_to_disp(i,:)+dredged_fraction_volume_to_disp(k,n,:)/&
                                                    dredged_sediment_volume_to_disp(k,n)&
                                                    *delta_disp_node_depth(i,k,n)/SUM(delta_disp_node_depth(i,:,:))
                      j=j+1 
                   ELSE
!                      dredged_fractions_to_disp(i,:) = 0
                   END IF
               END DO
             END DO
             IF (j>1 .AND. ANY(delta_disp_node_depth(i,:,:) > 0.0_Double)) THEN
                node_sediment_fraction(i,:) = dredged_fractions_to_disp(i,:)
             ELSEIF (j==1) THEN
             	DO k=1,get_nof_dredge_poly( )
             	   DO n=1,nof_dispose_poly
                      IF (delta_disp_node_depth(i,k,n) /= 0.0_Double) &
                         node_sediment_fraction(i,:) = dredged_fraction_volume_to_disp(k,n,:)/dredged_sediment_volume_to_disp(k,n)
                   END DO
                END DO
             END IF
             DO k=1,get_nof_dredge_poly( )
                DO n=1,nof_dispose_poly
                   IF (dispose_node_index(i,k,n) == 1 .AND. node_depth(i)+disposal_rate(k)*time_to_real_seconds(act_timestep)/&
                                                         (node_area(i) *(1-node_porosity(i))) >= disp_node_depth(i,k,n)) THEN
                      delta_disp_node_depth(i,k,n)  = 0.0_Double
                      disp_node_depth(i,k,n) = node_depth(i)
                   END IF
		END DO
             END DO
          END DO
       END IF
       !
       DO k=1,get_nof_dredge_poly( )
          IF (act_time == end_dredge_time(k)+act_timestep) THEN
             sediment_volume(k)   = 0.0_Double
             fraction_volume(:,k) = 0.0_Double
          END IF
       END DO
       !
       DO k=1,get_nof_dredge_poly( )
          DO n=1,nof_dispose_poly
             DO i=1,get_nof_nodes( )
                IF (dispose_node_index(i,k,n) == 1 .AND. disp_node_total_volume_loc(i,k,n)/=0.0_Double) THEN
                   disp_node_total_volume(i,k,n) = disp_node_total_volume(i,k,n)+disp_node_total_volume_loc(i,k,n)
!       	           disp_node_total_volume(i,k,n) = disp_node_total_volume(i,k,n)+ABS(node_depth_supply(i)&
!       	                                        -node_depth(i))*node_area(i)
!       	           disp_node_total_volume_loc(i,k,n) = ABS(node_depth_supply(i)-node_depth(i))*node_area(i)
!       	           disp_total_volume(n,k)     = disp_total_volume(n,k)+disp_node_total_volume_loc(i,n,k)
!                   disp_sediment_volume(n,k)  = disp_sediment_volume(n,k)+disp_node_total_volume_loc(i,n,k)*(1-node_porosity(i))
!                   disp_fraction_volume(:,n,k)= disp_fraction_volume(:,n,k)+dredged_fraction_volume_to_disp(k,n,:)/dredged_sediment_volume_to_disp(k,n)&
!                                                *disp_node_total_volume_loc(i,n,k)*(1-node_porosity(i))
       	        ELSE
       	     	   disp_node_total_volume(i,k,n)      = 0.0_Double
                END IF
             END DO
             !
             IF(ANY(disp_node_total_volume(:,k,n) > 0.0_Double)) THEN
                IF (NCSIZE.GT.1) THEN
                   ! the volume of dredged material of interface nodes
                   ! ... is divided by the number of parallel processors the node is belonging to
                   DO m=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                      IF (dispose_node_index(node_neighb(m),k,n)==1) THEN
                          p=1             
                          DO j=m+1,m+4
                             IF (node_neighb(j)>= 0) p=p+1 
                          END DO
                         disp_node_total_volume(node_neighb(m),k,n) = disp_node_total_volume(node_neighb(m),k,n)/p
                         disp_node_total_volume_loc(node_neighb(m),k,n) = disp_node_total_volume_loc(node_neighb(m),k,n)/p
                      END IF
                   END DO
                END IF
             END IF
             !
             DO i=1,get_nof_nodes( )
                IF (dispose_node_index(i,k,n) == 1 .AND. node_supply(i) == 1.D0 .AND. disp_node_total_volume_loc(i,k,n) /= &
                                             0.0_Double) THEN
                   disp_total_volume_loc(n,k)     = disp_total_volume_loc(n,k)+disp_node_total_volume_loc(i,k,n)
                   disp_sediment_volume_loc(n,k)  = disp_sediment_volume_loc(n,k)+disp_node_total_volume_loc(i,k,n)*(1-&
                                                      node_porosity(i))
                   disp_fraction_volume_loc(:,n,k)= disp_fraction_volume_loc(:,n,k)+dredged_fraction_volume_to_disp(k,n,:)/&
                                                dredged_sediment_volume_to_disp(k,n)&
                                                *disp_node_total_volume_loc(i,k,n)*(1-node_porosity(i))
                END IF
             END DO
             !
             !!parallel routines
             !! summation of total volumes to disp over proc ids
             IF (NCSIZE.GT.1) THEN
                 disp_total_volume_loc(n,k)     = p_dsum(disp_total_volume_loc(n,k))
                 disp_sediment_volume_loc(n,k)  = p_dsum(disp_sediment_volume_loc(n,k))
                 DO m=1,SIZE(node_sediment_fraction,2)
                    disp_fraction_volume_loc(m,n,k)= p_dsum(disp_fraction_volume_loc(m,n,k))
                 END DO
             END IF
             !
             IF (disp_total_volume_loc(n,k)/=0.0_Double) THEN
             	disp_total_volume(n,k)=disp_total_volume(n,k)+disp_total_volume_loc(n,k)
             	disp_sediment_volume(n,k)=disp_sediment_volume(n,k)+disp_sediment_volume_loc(n,k)
             	disp_fraction_volume(:,n,k)=disp_fraction_volume(:,n,k)+disp_fraction_volume_loc(:,n,k)
             END IF
             !
!             IF (ALL(disp_node_total_volume_loc(:,k,n)==0.0_Double) .AND. dredged_sediment_volume_to_disp(k,n) /= 0.0_Double .AND. &
!                 disp_sediment_volume(n,k) < disp_sediment_volume_loc(n,k) + dredged_sediment_volume_to_disp(k,n)) THEN
!                 disp_sediment_volume(n,k)     = disp_sediment_volume_loc(n,k) + dredged_sediment_volume_to_disp(k,n)
!                 disp_total_volume(n,k)        = disp_sediment_volume(n,k)/(1-MAXVAL(node_porosity(:)))
!                 disp_sediment_volume_loc(n,k) = disp_sediment_volume(n,k)
!             ELSE IF (ALL(disp_node_total_volume_loc(:,k,n)==0.0_Double) .AND. dredged_sediment_volume_to_disp(k,n) /= 0.0_Double .AND. &
!                      disp_sediment_volume(n,k) > disp_sediment_volume_loc(n,k) + dredged_sediment_volume_to_disp(k,n)) THEN
!                disp_sediment_volume(n,k)     = disp_sediment_volume_loc(n,k) + dredged_sediment_volume_to_disp(k,n)
!                disp_total_volume(n,k)        = disp_sediment_volume(n,k)/(1-MAXVAL(node_porosity(:)))
!                disp_sediment_volume_loc(n,k) = disp_sediment_volume(n,k)
!             END IF
             !
          END DO
       END DO
       !
       IF (DEBUG_ds > 0) THEN
          ! Intercative output of disposed volumes
          DO k=1,get_nof_dredge_poly( )
          DO n=1,nof_dispose_poly
          DO i=1,get_nof_nodes( )
             IF (node_supply(i)== 1.D0) THEN
                IF (disp_node_total_volume(i,k,n) > 0.0_Double .AND. delta_disp_node_depth(i,k,n)==0.0_Double) THEN 
                   IF (NCSIZE.GT.1) THEN
                      WRITE(6 , FMT = 7120)    &
                      '%§§%',&
                      time_to_real_seconds(act_time-initial_time),&
                      fkm(i),&
                      knolg(i),&
                      node_coord(i,1),&
                      node_coord(i,2),&
                      disp_node_total_volume(i,k,n)
                   ELSE
                      WRITE(6 , FMT = 7120)    &
                      '%§§%',&
                      time_to_real_seconds(act_time-initial_time),&
                      fkm(i),&
                      i, &
                      node_coord(i,1),&
                      node_coord(i,2),&
                      disp_node_total_volume(i,k,n)
                   END IF
                   disp_node_total_volume(i,k,n)=0.0_Double
                END IF
             END IF
          END DO
          END DO
          END DO
       END IF
       !
       !! setting volume arrays to zero
       DO k=1,get_nof_dredge_poly( )
          DO n=1,nof_dispose_poly
             sum_of_evolutions = 0.0_Double
             sum_of_evolutions = SUM(delta_disp_node_depth(:,k,n))
             IF (NCSIZE.GT.1) sum_of_evolutions = p_dsum(sum_of_evolutions)
             IF (sum_of_evolutions == 0.0_Double) THEN
!             	disp_sediment_volume(n,k)=disp_sediment_volume(n,k)+dredged_sediment_volume_to_disp(k,n)
!             	disp_fraction_volume(:,n,k)=disp_fraction_volume(:,n,k)+dredged_fraction_volume_to_disp(k,n,:)
!             	disp_total_volume(n,k)=disp_sediment_volume(n,k)/(1-MAXVAL(node_porosity(:)))
                disp_sediment_volume_loc(n,k) = disp_sediment_volume_loc(n,k)+dredged_sediment_volume_to_disp(k,n)
                IF (ALLOCATED(dredged_fraction_volume_to_disp)) dredged_fraction_volume_to_disp(k,n,:) = 0.0_Double
                IF (ALLOCATED(dredged_sediment_volume_to_disp)) dredged_sediment_volume_to_disp(k,n)   = 0.0_Double
                IF (ANY(disp_node_total_volume(:,k,n)/=0.0_Double)) disp_node_total_volume(:,k,n)=0.0_Double 
             ELSE
                CYCLE
             END IF
          END DO
       END DO
       !
       !deallocating local data
       IF (ALLOCATED(x_Factor             )) DEALLOCATE(x_Factor)
       IF (ALLOCATED(diff_array           )) DEALLOCATE(diff_array)
       IF (ALLOCATED(dredged_sediment_volume_to_disp_loc)) DEALLOCATE(dredged_sediment_volume_to_disp_loc)
       IF (ALLOCATED(dredged_fraction_volume_to_disp_loc)) DEALLOCATE(dredged_fraction_volume_to_disp_loc)
       IF (ALLOCATED(disp_sediment_volume_loc) .AND. ALL(disp_sediment_volume_loc(:,:) == 0.0_Double)) &
            DEALLOCATE(disp_sediment_volume_loc)
       IF (ALLOCATED(disp_node_total_volume_loc)) DEALLOCATE(disp_node_total_volume_loc)
       IF (ALLOCATED(disp_total_volume_loc)) DEALLOCATE(disp_total_volume_loc)
!       IF (ALLOCATED(disp_sediment_volume_loc)) DEALLOCATE(disp_sediment_volume_loc)
       IF (ALLOCATED(disp_fraction_volume_loc)) DEALLOCATE(disp_fraction_volume_loc)
    END IF
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden Kritverklappen: ",time
    ! 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! DREDGING - dredging initiated by time controlled maintenance !!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    !! 1. preparation of dredging operation
    !!    a) allocating data
    !!    b) determine nodes inside dredge polygons
    !
!    call system_clock(start,rate,cmax)
    !
    IF ( nof_dredge_poly_tc >=1 ) THEN
       !! allocating local data
       ALLOCATE (node_areas_inside         (get_nof_nodes( ), nof_dredge_poly_tc ))
       ALLOCATE (sum_node_areas_inside     (nof_dredge_poly_tc                   ))
       ALLOCATE (delta_dredge_node_depth_tc(get_nof_nodes( ), nof_dredge_poly_tc ))
       ALLOCATE (delta_t_dredge_tc         (nof_dredge_poly_tc))
       node_areas_inside          = 0.0_Double
       sum_node_areas_inside      = 0.0_Double
       delta_dredge_node_depth_tc = 0.0_Double
       delta_t_dredge_tc(:)       = time_to_real_seconds(dredge_time_tc(:,2)-dredge_time_tc(:,1))
       DO n=1,nof_dredge_poly_tc
          IF(delta_t_dredge_tc(n) < time_to_real_seconds(act_timestep))&
             delta_t_dredge_tc(n)=time_to_real_seconds(act_timestep)
       END DO
       !
       !! transfering values from dredge_poly_index_tc to associated nodes in dredge_node_index_tc to calculate on nodes
       DO n=1,nof_dredge_poly_tc
          DO i=1,get_nof_nodes( )
             IF (dredge_node_index_tc(i,n)==1) node_areas_inside(i,n)=node_area(i)
	  END DO
          sum_node_areas_inside(n) = SUM(node_areas_inside(:,n))
          !! for parallel computation
          IF (NCSIZE.GT.1) THEN
             !! substracting area of interface nodes
             doppelt = 0.D0
             DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                IF (dredge_node_index_tc(node_neighb(i),n)==1) THEN
                   k=1             
                   DO j=i+1,i+4
                      IF (node_neighb(j)>= 0) k=k+1
                   END DO
                   doppelt = doppelt + node_area(node_neighb(i))/k
                END IF
             END DO
             sum_node_areas_inside(n) = sum_node_areas_inside(n) - doppelt
             sum_node_areas_inside(n) = p_dsum(sum_node_areas_inside(n))
          END IF 
       END DO    
    END IF
    !
    !! 2. computation of dredging action on a dredge polygon
    DO n=1,nof_dredge_poly_tc
       !! dredging initiated if actual time is in between defined dredging interval
       IF (act_time >= dredge_time_tc(n,1) .AND. act_time < dredge_time_tc(n,2)) THEN
           DO i=1,get_nof_nodes( )
              IF (dredge_node_index_tc(i,n) == 1) THEN             
                 !! indicator variable set
                 node_clean(i)                   = 1.D0
                 delta_dredge_node_depth_tc(i,n) = dredge_sed_vol_tc(n)*time_to_real_seconds(act_timestep)&
                                                   /(delta_t_dredge_tc(n)*sum_node_areas_inside(n))
                 node_depth_dredge(i)            = node_depth_dredge(i)-delta_dredge_node_depth_tc(i,n)
                 node_sediment_volume_tc(i,n)    = dredge_sed_vol_tc(n)*node_area(i)&
                                                  /(sum_node_areas_inside(n))
                 node_fraction_volume_tc(i,:,n)  = node_sediment_fraction(i,:)*dredge_sed_vol_tc(n)*node_area(i)&
                                                  /(sum_node_areas_inside(n))
              END IF
           END DO
       END IF
    END DO
    !
    !! saving dredged fraction volume for disposal operation and grain transfer
    DO n=1,nof_dredge_poly_tc
       sediment_volume_tc(n) = SUM(node_sediment_volume_tc(:,n))
       IF(NCSIZE.GT.1) sediment_volume_tc(n)=p_dsum(sediment_volume_tc(n))
       DO i=1,SIZE(node_sediment_fraction,2)
          fraction_volume_tc(i,n) = SUM(node_fraction_volume_tc(:,i,n))
          !! parallel routines
          IF(NCSIZE.GT.1) fraction_volume_tc(i,n)=p_dsum(fraction_volume_tc(i,n))
       END DO
    END DO
    !
    IF (ALLOCATED(delta_t_dredge_tc          )) DEALLOCATE(delta_t_dredge_tc)    
    IF (ALLOCATED(node_areas_inside          )) DEALLOCATE(node_areas_inside)
    IF (ALLOCATED(sum_node_areas_inside      )) DEALLOCATE(sum_node_areas_inside)
    IF (ALLOCATED(delta_dredge_node_depth_tc )) DEALLOCATE(delta_dredge_node_depth_tc)
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden Zeitbaggern: ",time
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! DISPOSAL - deposition initiated by time controlled maintenance !!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    !! 1. preparation of disposal operation
    !!    a) allocating data
    !!    b) determine nodes inside disposal polygons
!    call system_clock(start,rate,cmax)
    !
    IF ( nof_dispose_poly_tc >=1 ) THEN
       DO n=1,nof_dispose_poly_tc
          IF (act_time >= disposal_time_tc(n,1) .AND. act_time < disposal_time_tc(n,2)) THEN
             any_disp_poly_to_disp=.true.
             EXIT
          ELSE
             any_disp_poly_to_disp=.false.
          END IF
       END DO
       !
       IF (any_disp_poly_to_disp) THEN
          !allocating data
          ALLOCATE (node_areas_inside           (get_nof_nodes( ), nof_dispose_poly_tc              ))
          ALLOCATE (sum_node_areas_inside       (nof_dispose_poly_tc                                ))
          ALLOCATE (delta_t_disposal_tc         (nof_dispose_poly_tc                                ))
          ALLOCATE (dredged_fractions_to_disp_tc(nof_dispose_poly_tc, SIZE(node_sediment_fraction,2)))
          !
          node_areas_inside      =0.0_Double
          sum_node_areas_inside  =0.0_Double
!          dispose_node_index_tc  = 0.D0
          delta_t_disposal_tc(:) = time_to_real_seconds(disposal_time_tc(:,2)-disposal_time_tc(:,1))
          DO n=1,nof_dispose_poly_tc
             IF(delta_t_disposal_tc(n) < time_to_real_seconds(act_timestep))&
                delta_t_disposal_tc(n)=time_to_real_seconds(act_timestep)
          END DO
          dredged_fractions_to_disp_tc = 0.0_Double
          !       
          IF (.NOT. ALLOCATED(check_node_total_volume)) THEN
       	      ALLOCATE(check_node_total_volume(get_nof_nodes( )))
       	      check_node_total_volume=0.0_Double
       	      check_total_volume=0.0_Double
          END IF
          !
          !! transfering values in dispose_node_index to calculate on nodes
          DO n=1,nof_dispose_poly_tc
             DO i=1,get_nof_nodes()
                IF (dispose_node_index_tc(i,n)==1) node_areas_inside(i,n)=node_area(i)
             END DO
             sum_node_areas_inside(n) = SUM(node_areas_inside(:,n))
             !! parallel routines
             IF (NCSIZE.GT.1) THEN
                !! substracting areas interface nodes
                doppelt = 0.D0
                DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                   IF (dispose_node_index_tc(node_neighb(i),n)==1) THEN
                      k=1             
                      DO j=i+1,i+4
                         IF (node_neighb(j)>= 0) k=k+1
                      END DO
                      doppelt = doppelt + node_area(node_neighb(i))/k
                   END IF
                END DO
                sum_node_areas_inside(n) = sum_node_areas_inside(n) - doppelt
                sum_node_areas_inside(n) = p_dsum(sum_node_areas_inside(n))
              END IF
          END DO    
       END IF
       !
       !! 2. computation of disposal action on the disposal polygon
       DO n=1,nof_dispose_poly_tc
          IF (disp_scours_tc(n) == 'YES') THEN
             disposing_scours=.true.
          ELSE
       	     disposing_scours=.false.
          END IF
          !! disposal action initiated
          IF (act_time >= disposal_time_tc(n,1) .AND. act_time < disposal_time_tc(n,2)) THEN
              !!Only, if a fractioned volume to dispose is present
              IF (SUM(fraction_volume_tc(:,n))>0.0_Double) THEN
                 !! calculating disp_node_depth if not disposing in scours
                 IF (.NOT. disposing_scours) THEN
                    DO i=1,get_nof_nodes( )
                       IF (dispose_node_index_tc(i,n)==1) THEN
                          !! indicator variable
                          node_supply(i) = 1.D0
                          delta_disp_node_depth_tc(i,n) = dispose_sed_vol_tc(n)*time_to_real_seconds(act_timestep)&
                                                          /(delta_t_disposal_tc(n)*sum_node_areas_inside(n))
                          node_depth_supply(i)          = node_depth_supply(i)+delta_disp_node_depth_tc(i,n)
!                          aim_node_depth_tc(i,n)        = aim_node_depth_tc(i,n)+delta_disp_node_depth_tc(i,n)
                          node_sediment_fraction(i,:)   = fraction_volume_tc(:,n)/sediment_volume_tc(n)
                          check_node_total_volume(i)    = delta_disp_node_depth_tc(i,n)*node_area(i)
                       END IF
                    END DO
                    check_total_volume = SUM(check_node_total_volume(:))
                 !! calculating disp_node_depth if disposing in scours
                 ELSEIF (disposing_scours) THEN
                    sum_of_evolutions = 0.0_Double
                    sum_of_evolutions = SUM(delta_disp_node_depth_tc(:,n))
                    IF (NCSIZE.GT.1) sum_of_evolutions = p_dsum(sum_of_evolutions) 
                    IF (sum_of_evolutions == 0.0_Double) THEN
                       !! iterative procedure to find act_depo_depth_disp_scours
	               act_depo_depth_disp_scours = min_depo_depth_disp_scours  
                       DO m=1,nof_iterations_disp_scours
                          little_helper_1            = 0.0_Double
                          little_helper_2            = 0.0_Double
                          little_helper_3            = 0.0_Double
                          check_total_volume         = 0.0_Double
                          check_node_total_volume(:) = 0.0_Double
                          DO i=1,get_nof_nodes( )
	                     IF (dispose_node_index_tc(i,n) == 1 .AND. node_depth(i) < free_surface_node_depth(i)&
		                                                   -act_depo_depth_disp_scours) THEN
                                little_helper_1 = little_helper_1+free_surface_node_depth(i)*node_area(i)
                                little_helper_2 = little_helper_2+node_depth(i)*node_area(i)
                                little_helper_3 = little_helper_3+node_area(i)
                             END IF
                          END DO
                          !! parallel routines	
                          IF (NCSIZE.GT.1) THEN
                             doppelt = 0.D0
                             DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                                IF (dispose_node_index_tc(node_neighb(i),n)==1 .AND. node_depth(node_neighb(i))&
                                    < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                                    k=1             
                                    DO j=i+1,i+4
                                       IF (node_neighb(j)>= 0) k=k+1
                                    END DO
                                    doppelt = doppelt + free_surface_node_depth(node_neighb(i))*node_area(node_neighb(i))/k
                                END IF
                             END DO
                             little_helper_1 = little_helper_1 - doppelt
                             doppelt = 0.D0
                             DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                                IF (dispose_node_index_tc(node_neighb(i),n)==1 .AND. node_depth(node_neighb(i))&
                                    < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                                    k=1             
                                    DO j=i+1,i+4
                                       IF (node_neighb(j)>= 0) k=k+1
                                    END DO
                                    doppelt = doppelt + node_depth(node_neighb(i))*node_area(node_neighb(i))/k
                                END IF
                             END DO
                             little_helper_2 = little_helper_2 - doppelt
                             doppelt = 0.D0
                             DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                                IF (dispose_node_index_tc(node_neighb(i),n)==1 .AND. node_depth(node_neighb(i))&
                                    < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                                    k=1             
                                    DO j=i+1,i+4
                                       IF (node_neighb(j)>= 0) k=k+1
                                    END DO
                                    doppelt = doppelt + node_area(node_neighb(i))/k
                                END IF
                             END DO
                             little_helper_3 = little_helper_3 - doppelt
                             !
                             little_helper_1 = p_dsum(little_helper_1)
                             little_helper_2 = p_dsum(little_helper_2)
                             little_helper_3 = p_dsum(little_helper_3)
                          END IF
                          !! calculating depth below reference surface level
                          act_depo_depth_disp_scours = (little_helper_1-little_helper_2-dispose_sed_vol_tc(n))/little_helper_3
                          !
                          DO i=1,get_nof_nodes( )
		             IF (dispose_node_index_tc(i,n)==1 .AND. node_depth(i) < free_surface_node_depth(i)&
		                                                     -act_depo_depth_disp_scours) THEN
                                !! calculation of aim node depths, differential depths and disposed volumes 
                                aim_node_depth_tc(i,n)        = free_surface_node_depth(i)-act_depo_depth_disp_scours
                                delta_disp_node_depth_tc(i,n) = aim_node_depth_tc(i,n)-node_depth(i)
                                check_node_total_volume(i)    = delta_disp_node_depth_tc(i,n)*node_area(i)
                             ELSE
                                delta_disp_node_depth_tc(i,n) = 0.0_Double
                                aim_node_depth_tc(i,n)        = node_depth(i)
                             END IF
                          END DO
                          !
                          check_total_volume=SUM(check_node_total_volume(:))
                          !! parallel routines
                          IF (NCSIZE.GT.1) THEN
                             doppelt = 0.D0
                             DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                                IF (dispose_node_index_tc(node_neighb(i),n)==1 .AND.&
                                   check_node_total_volume(node_neighb(i)) > 0.0_Double) THEN
                                   k=1             
                                   DO j=i+1,i+4
                                      IF (node_neighb(j)>= 0) k=k+1
                                   END DO
                                   doppelt = doppelt + check_node_total_volume(node_neighb(i))/k
                                END IF
                             END DO
                             check_total_volume = check_total_volume - doppelt
                             check_total_volume = p_dsum(check_total_volume)
                          END IF
                          !
                          !! check whether...
                          !! a) storage capacity is sufficient
                          !! b) criterion for end of iteration is fulfilled
                          !! c) iteration failed
                          IF (m==1 .AND. act_depo_depth_disp_scours < min_depo_depth_disp_scours) THEN
                              PRINT*, 'Disposing failed: Storage capacity of the scour is too low.'
                              delta_disp_node_depth_tc(:,n)=0.0_Double
                              check_total_volume=0.0_Double
                              EXIT
                          ELSEIF (ABS(check_total_volume-dispose_sed_vol_tc(n)) <= max_error_disp_scours) THEN
                              PRINT*, 'Iteration for disposing scours succesfully finished.'
                              EXIT
                          ELSEIF (m==nof_iterations_disp_scours .AND. ABS(check_total_volume-dispose_sed_vol_tc(n))&
                                                                  > max_error_disp_scours) THEN
                              PRINT*, 'Iteration failed. Increase number of iterations or maximum error for disposing scours.'
                          ELSE
                              CYCLE
                          END IF
                       END DO
                    END IF
                    !! end of iteration loop for finding disposing depth
                    !
                    DO i=1,get_nof_nodes( )
                       IF(dispose_node_index_tc(i,n)==1 .AND. delta_disp_node_depth_tc(i,n) > 0.0_Double) THEN
                         node_supply(i)                  = 1.D0
                         IF(node_depth(i)>= aim_node_depth_tc(i,n)) THEN
                           delta_disp_node_depth_tc(i,n) = 0.0_Double
                         ELSEIF(node_depth(i)+time_to_real_seconds(act_timestep)/delta_t_disposal_tc(n)&
                                *delta_disp_node_depth_tc(i,n)>aim_node_depth_tc(i,n)) THEN
                            node_depth_supply(i)          = aim_node_depth_tc(i,n)
                         ELSEIF(act_time==disposal_time_tc(n,2)-act_timestep) THEN
                            node_depth_supply(i)          = aim_node_depth_tc(i,n)
                         ELSE
                            node_depth_supply(i)          = node_depth_supply(i)+time_to_real_seconds(act_timestep)&
                                                            /delta_t_disposal_tc(n)*delta_disp_node_depth_tc(i,n)
                         END IF
                       END IF
                    END DO
                 END IF
              ELSE
                 PRINT*, '***Baggergut konnte nicht auf Polygon', dispose_poly_name_tc(n), 'verbracht werden.'
                 PRINT*, '***Es existiert kein Baggergut zum Verbringen.'
              END IF
          ELSE
       	      delta_disp_node_depth_tc(:,n) = 0.0_Double
          END IF
       END DO
       !! transfering sediment distribution to dispose
       IF (ANY(sediment_volume_tc /= 0.0_Double) .AND. ANY(delta_disp_node_depth_tc /=0.0_Double) ) THEN    
          DO i=1,get_nof_nodes( )
             j=0
             DO n=1,nof_dispose_poly_tc
                IF (dispose_node_index_tc(i,n) == 1 .AND. sediment_volume_tc(n) > 0.0_Double) THEN
                   dredged_fractions_to_disp_tc(n,:) = fraction_volume_tc(:,n)/sediment_volume_tc(n)&
                                                       *delta_disp_node_depth_tc(i,n)
                   j=j+1
                ELSE
                   dredged_fractions_to_disp_tc(n,:) = 0
                END IF
             END DO
             IF (j>1 .AND. ANY(delta_disp_node_depth_tc(i,:) > 0.0_Double)) THEN
                DO k=1,SIZE(node_sediment_fraction,2)
                   node_sediment_fraction(i,k) = SUM(dredged_fractions_to_disp_tc(:,k))/SUM(delta_disp_node_depth_tc(i,:))
                END DO
             END IF
          END DO       	
       END IF
    END IF
    ! deallocating local data
    IF (ALLOCATED(delta_t_disposal_tc         )) DEALLOCATE(delta_t_disposal_tc)    
    IF (ALLOCATED(node_areas_inside           )) DEALLOCATE(node_areas_inside)
    IF (ALLOCATED(sum_node_areas_inside       )) DEALLOCATE(sum_node_areas_inside)
    IF (ALLOCATED(dredged_fractions_to_disp_tc)) DEALLOCATE(dredged_fractions_to_disp_tc)
    any_disp_poly_to_disp=.false.
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden Zeitverklappen: ",time
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! DISPOSAL - deposition initiated by artificial bed load supply !!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    !! 1. preparation of disposal operation
    !!    a) allocating data
    !!    b) determine nodes inside disposal polygons
!    call system_clock(start,rate,cmax)
    !
    IF ( nof_predef_disp_poly >=1 ) THEN
       !! allocating local data
       ALLOCATE (node_areas_inside            (get_nof_nodes( ), nof_predef_disp_poly ))
       ALLOCATE (sum_node_areas_inside        (nof_predef_disp_poly                   ))
       ALLOCATE (delta_t_art_bed_load         (nof_predef_disp_poly                    ))
       ALLOCATE (dredged_fractions_to_disp_abl(nof_predef_disp_poly, SIZE(node_sediment_fraction,2)))
       !
       node_areas_inside       = 0.0_Double
       sum_node_areas_inside   = 0.0_Double
       delta_t_art_bed_load(:) = time_to_real_seconds(art_bl_time(:,2)-art_bl_time(:,1))
       DO n=1,nof_predef_disp_poly
          IF(delta_t_art_bed_load(n) < time_to_real_seconds(act_timestep))&
             delta_t_art_bed_load(n)=time_to_real_seconds(act_timestep)
       END DO
       dredged_fractions_to_disp_abl = 0.0_Double
       !
       IF (.NOT. ALLOCATED(check_node_total_volume)) THEN
       	   ALLOCATE(check_node_total_volume(get_nof_nodes( )))
       	   check_node_total_volume=0.0_Double
       	   check_total_volume=0.0_Double
       END IF
       !
       !! transfering values from art_bed_load_poly_index to associated nodes in art_bed_load_node_index to calculate on nodes
       DO n=1,nof_predef_disp_poly
          DO i=1,get_nof_nodes()
             IF (art_bed_load_node_index(i,n)==1) node_areas_inside(i,n)=node_area(i)
          END DO
          sum_node_areas_inside(n) = SUM(node_areas_inside(:,n))
          !! parallel routines
          IF (NCSIZE.GT.1) THEN
             !! substracting area of interface nodes
             doppelt = 0.D0
             DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                IF (art_bed_load_node_index(node_neighb(i),n)==1) THEN
                   k=1             
                   DO j=i+1,i+4
                      IF (node_neighb(j)>= 0) k=k+1
                   END DO
                   doppelt = doppelt + node_area(node_neighb(i))/k
                END IF
             END DO
             sum_node_areas_inside(n) = sum_node_areas_inside(n) - doppelt
             sum_node_areas_inside(n) = p_dsum(sum_node_areas_inside(n))
           END IF 
       END DO    
    END IF
    !
    !! 2. computation of disposal action on nodes of the disposal polygon
    DO n=1,nof_predef_disp_poly
       IF (disp_scours_abl(n) == 'YES') THEN
       	  disposing_scours=.true.
       ELSE
       	  disposing_scours=.false.
       END IF
       !! initiation of artificial bed load supply
       IF (act_time >= art_bl_time(n,1) .AND. act_time < art_bl_time(n,2)) THEN
       	  !! calculation of depostion depth if not disposing in scours
       	  IF (.NOT. disposing_scours) THEN
             DO i=1,get_nof_nodes( )
                IF (art_bed_load_node_index(i,n)==1) THEN
                    node_supply(i)            = 1.D0
                    delta_node_depth_abl(i,n) = predef_disp_sed_vol(n)*time_to_real_seconds(act_timestep)/&
                                                (delta_t_art_bed_load(n)*sum_node_areas_inside(n))
                    node_depth_supply(i)      = node_depth_supply(i)+delta_node_depth_abl(i,n)
                    DO k=1,SIZE(node_sediment_fraction,2)
                       !transfer of sediment fractions        
                       DO j=1, SIZE(predef_disp_sed_class,2)
                          IF (fraction_name(k)==predef_disp_sed_class(n,j)) THEN
                              node_sediment_fraction(i,k)=predef_sed_distrib(n,j)
                          END IF
                       END DO
                    END DO
                END IF
             END DO
          !! calculation of depostion depth if disposing in scours
          ELSEIF (disposing_scours) THEN
	     sum_of_evolutions = 0.0_Double
             sum_of_evolutions = SUM(delta_node_depth_abl(:,n))
             IF (NCSIZE.GT.1) sum_of_evolutions = p_dsum(sum_of_evolutions)
             IF (sum_of_evolutions==0.0_Double) THEN
	        !! iterative procedure to find act_depo_depth_disp_scours		   
                act_depo_depth_disp_scours = min_depo_depth_disp_scours
                DO m=1,nof_iterations_disp_scours
                   little_helper_1            = 0.0_Double
                   little_helper_2            = 0.0_Double
                   little_helper_3            = 0.0_Double
                   check_total_volume         = 0.0_Double
                   check_node_total_volume(:) = 0.0_Double
                   DO i=1,get_nof_nodes( )
		      IF (art_bed_load_node_index(i,n)==1) THEN
		      	 IF (node_depth(i) < free_surface_node_depth(i)&
		                                                     -act_depo_depth_disp_scours) THEN
                         little_helper_1 = little_helper_1+free_surface_node_depth(i)*node_area(i)
                         little_helper_2 = little_helper_2+node_depth(i)*node_area(i)
                         little_helper_3 = little_helper_3+node_area(i)
                         END IF
                      END IF
                   END DO
                   !! parallel routines
                   IF (NCSIZE.GT.1) THEN
                      doppelt = 0.D0
                      DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                         IF (art_bed_load_node_index(node_neighb(i),n)==1 .AND. node_depth(node_neighb(i))&
                             < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                             k=1             
                             DO j=i+1,i+4
                                IF (node_neighb(j)>= 0) k=k+1
                             END DO
                             doppelt = doppelt + free_surface_node_depth(node_neighb(i))*node_area(node_neighb(i))/k
                         END IF
                      END DO
                      little_helper_1 = little_helper_1 - doppelt
                      doppelt = 0.D0
                      DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                         IF (art_bed_load_node_index(node_neighb(i),n)==1 .AND. node_depth(node_neighb(i))&
                             < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                             k=1             
                             DO j=i+1,i+4
                                IF (node_neighb(j)>= 0) k=k+1
                             END DO
                             doppelt = doppelt + node_depth(node_neighb(i))*node_area(node_neighb(i))/k
                         END IF
                      END DO
                      little_helper_2 = little_helper_2 - doppelt
                      doppelt = 0.D0
                      DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                         IF (art_bed_load_node_index(node_neighb(i),n)==1 .AND. node_depth(node_neighb(i))&
                             < free_surface_node_depth(node_neighb(i))-act_depo_depth_disp_scours) THEN
                             k=1             
                             DO j=i+1,i+4
                                IF (node_neighb(j)>= 0) k=k+1
                             END DO
                             doppelt = doppelt + node_area(node_neighb(i))/k
                         END IF
                      END DO
                      little_helper_3 = little_helper_3 - doppelt
                      !
                      little_helper_1 = p_dsum(little_helper_1)
                      little_helper_2 = p_dsum(little_helper_2)
                      little_helper_3 = p_dsum(little_helper_3)
                   END IF
                   !! calculating depth below reference surface level
                   act_depo_depth_disp_scours=(little_helper_1-little_helper_2-predef_disp_sed_vol(n))/little_helper_3
                   !
                   DO i=1,get_nof_nodes( )
		      IF (art_bed_load_node_index(i,n)==1 .AND. node_depth(i) < free_surface_node_depth(i)&
		                                                -act_depo_depth_disp_scours) THEN
                         !! calculation of aim node depths, differential depths and disposed volumes 
                         aim_node_depth_abl(i,n)    = free_surface_node_depth(i)-act_depo_depth_disp_scours
                         delta_node_depth_abl(i,n)  = aim_node_depth_abl(i,n)-node_depth(i)
                         check_node_total_volume(i) = delta_node_depth_abl(i,n)*node_area(i)
                      ELSE
                         delta_node_depth_abl(i,n) = 0.0_Double
                         aim_node_depth_abl(i,n)   = node_depth(i)
                      END IF
                   END DO
                   !
		   check_total_volume = SUM(check_node_total_volume(:))
                   !! parallel routines
                   IF (NCSIZE.GT.1) THEN
                      doppelt = 0.D0
                      DO i=1,NBMAXNSHARE*NPTIR,NBMAXNSHARE
                         IF (art_bed_load_node_index(node_neighb(i),n)==1 .AND.& 
                             check_node_total_volume(node_neighb(i)) > 0.0_Double) THEN
                            k=1             
                            DO j=i+1,i+4
                               IF (node_neighb(j)>= 0) k=k+1
                            END DO
                            doppelt = doppelt + check_node_total_volume(node_neighb(i))/k
                         END IF
                      END DO
                      check_total_volume = check_total_volume - doppelt
                      check_total_volume = p_dsum(check_total_volume)
                   END IF
                   !
                   !! check whether...
                   !! a) storage capacity is sufficient
                   !! b) criterion for end of iteration is fulfilled
                   !! c) iteration failed
                   IF (m == 1 .AND. act_depo_depth_disp_scours < min_depo_depth_disp_scours) THEN
                       PRINT*, 'Disposing failed: Storage capacity of the scour is too low.'
                       delta_node_depth_abl(:,n)=0.0_Double
                       check_total_volume=0.0_Double
                       EXIT
                   ELSEIF (ABS(check_total_volume-predef_disp_sed_vol(n)) <= max_error_disp_scours) THEN
                       PRINT*, 'Iteration for disposing scours succesfully finished.'
                       EXIT
                   ELSEIF (m==nof_iterations_disp_scours .AND. ABS(check_total_volume-predef_disp_sed_vol(n)) > &
                           max_error_disp_scours) THEN
                       PRINT*, 'Iteration failed. Increase number of iterations or maximum error for disposing scours.'
                   ELSE
                       CYCLE
                   END IF
                END DO
             END IF
             !! end of iteration loop for finding disposing depth
             !! heightening of nodes due to disposal
             DO i=1,get_nof_nodes( )
                IF(art_bed_load_node_index(i,n)==1 .AND. delta_node_depth_abl(i,n) > 0.0_Double) THEN
                  node_supply(i)               = 1.D0
                  IF(node_depth(i)>= aim_node_depth_abl(i,n)) THEN
                    delta_node_depth_abl(i,n)  = 0.0_Double
                  ELSEIF(node_depth(i)+time_to_real_seconds(act_timestep)/delta_t_art_bed_load(n)&
                         *delta_node_depth_abl(i,n)>aim_node_depth_abl(i,n)) THEN
                     node_depth_supply(i)      = aim_node_depth_abl(i,n)
                     delta_node_depth_abl(i,n) = 0.0_Double
                  ELSEIF(act_time==art_bl_time(n,2)-act_timestep) THEN
                     node_depth_supply(i)      = aim_node_depth_abl(i,n)
                  ELSE
                     node_depth_supply(i)      = node_depth_supply(i)+time_to_real_seconds(act_timestep)&
                                                 /delta_t_art_bed_load(n)*delta_node_depth_abl(i,n)
                  END IF
                  !
                  node_sediment_fraction(i,:)=0.0_Double				 				 
                  DO k=1,SIZE(node_sediment_fraction,2)
                     !! transfer of sediment fractions			    
                     DO j=1, SIZE(predef_disp_sed_class,2)
                        IF (fraction_name(k)==predef_disp_sed_class(n,j)) THEN
                            node_sediment_fraction(i,k)=predef_sed_distrib(n,j)
                        END IF
                     END DO
                   END DO
                END IF
             END DO
          END IF
       END IF
    END DO
    !
    !! transfering sediment distribution to dispose
    !LEO TODO bug predef_disp_sed_vol was both allocated!
    IF (ALLOCATED(predef_disp_sed_vol)) THEN
      IF (ANY(predef_disp_sed_vol /= 0.0_Double) .AND. ANY(delta_node_depth_abl /=0.0_Double) ) THEN    
         DO i=1,get_nof_nodes( )
            j=0
            DO n=1,nof_predef_disp_poly
               IF (art_bed_load_node_index(i,n) == 1 .AND. predef_disp_sed_vol(n) > 0.0_Double) THEN
                  !LEO TODO here is a boundary dismatch during disposal 
                  !dredged_fractions_to_disp_abl dimesnion 2 is wrong 0.0000000000000000  0.0000000000000000
                  !should be one but is two
                  dredged_fractions_to_disp_abl(n,:) = predef_sed_distrib(n,:)*delta_node_depth_abl(i,n)
                  j=j+1
               ELSE
                  dredged_fractions_to_disp_abl(n,:) = 0
               END IF
            END DO
            IF (j>1 .AND. ANY(delta_node_depth_abl(i,:) > 0.0_Double)) THEN
               DO k=1,SIZE(node_sediment_fraction,2)
                  node_sediment_fraction(i,k) = SUM(dredged_fractions_to_disp_abl(:,k))/SUM(delta_node_depth_abl(i,:))
               END DO
            END IF
         END DO
      END IF
    END IF
    !
    !! deallocating data
    IF (ALLOCATED(delta_t_art_bed_load         )) DEALLOCATE(delta_t_art_bed_load         ) 
    IF (ALLOCATED(node_areas_inside            ))  DEALLOCATE(node_areas_inside           )
    IF (ALLOCATED(sum_node_areas_inside        ))  DEALLOCATE(sum_node_areas_inside       )
    IF (ALLOCATED(dredged_fractions_to_disp_abl)) DEALLOCATE(dredged_fractions_to_disp_abl)     
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden Geschiebezugabe: ",time
    !
!    call system_clock(start,rate,cmax)
    !
    !! final transfer of depths of dredged or disposed nodes
!    IF (ANY(node_clean(:).GE.1.D0)) THEN
    DO k=1,get_nof_nodes( )
       IF (node_clean(k).GE.1.D0) THEN
           node_depth(k) = node_depth_dredge(k)                
       ENDIF
    END DO
!    END IF
    !LEO TODO this is not nice since it cauese a dependency on bief for the parallel case
    !an should be replaced
    IF (NCSIZE.GT.1) THEN
        T13%TYPE=2 ! vector structure
        T13%N=1
        T13%R=node_depth
!RK        CALL PARCOM (T13,4,MESH)
! das ist der Aufruf aus parcom, wenn vector structure icom=2 und 
! kein quasi bubble
        CALL PARCOM2_DS(T13%R,T13%R,T13%R,get_nof_nodes(),1,4,1, &
   NB_NEIGHB,NB_NEIGHB_PT,LIST_SEND, NH_COM,DIMNHCOM,BUF_SEND,BUF_RECV,DIMBUF)
!!        CALL PARCOM2(X%R,X%R,X%R,NPOIN,NPLAN,ICOM,IAN,MESH)

!      IAN = 1

        node_depth=T13%R    
    END IF
    !
!    IF (ANY(node_supply(:).GE.1.D0)) THEN
    DO k=1,get_nof_nodes( )
       IF (node_supply(k).GE.1.D0) THEN
           node_depth(k) = node_depth_supply(k)                
       ENDIF
    END DO
!    END IF
    IF (NCSIZE.GT.1) THEN
        T14%TYPE=2
        T14%N=1
        T14%R=node_depth
!!        CALL PARCOM (T14,3,MESH)
        CALL PARCOM2_DS(T14%R,T14%R,T14%R,get_nof_nodes(),1,3,1, &
   NB_NEIGHB,NB_NEIGHB_PT,LIST_SEND, NH_COM,DIMNHCOM,BUF_SEND,BUF_RECV,DIMBUF)
!!        CALL PARCOM2(X%R,X%R,X%R,NPOIN,NPLAN,ICOM,IAN,MESH)


        node_depth=T14%R    
    END IF
    !
    !! writing real amounts of dredged material to an output file for every observing period 
    DO n=1, get_nof_dredge_poly( )
       IF ( old_time_to_observe(n) <= act_time) THEN
       	  IF (upd_out_volumes(n)) THEN
             IF (NCSIZE.GT.1) THEN
                IF (ipid==0) THEN
                   CALL write_dredged_volumes_pop ()
                   CALL write_disposed_volumes_pop ()
                END IF
             ELSE
                CALL write_dredged_volumes_pop ()
                CALL write_disposed_volumes_pop ()
             END IF
             upd_out_volumes(:)=.false.
             all_total_volume_old(:)      = all_total_volume(:)
             all_sediment_volume_old(:)   = all_sediment_volume(:)
             all_water_volume_old(:)      = all_water_volume(:)
             all_fraction_volume_old(:,:) = all_fraction_volume(:,:)
             IF (nof_dispose_poly >= 1) THEN
                disp_total_volume_old    = disp_total_volume
                disp_sediment_volume_old = disp_sediment_volume
                disp_fraction_volume_old = disp_fraction_volume
             END IF
             EXIT
          END IF                
       END IF
    END DO
    !
    ! writing restart data
    IF (output_time_restart < time_to_real_seconds(act_time-initial_time)) THEN
    	output_time_restart = time_to_real_seconds(act_time-initial_time)
    END IF
    IF(output_time_restart == time_to_real_seconds(act_time-initial_time)) THEN
    	last_obs_time_rs=datetime_to_string(old_time_to_observe(:))
    	next_obs_time_rs=datetime_to_string(time_to_observe(:))
    	last_act_time_rs=datetime_to_string(act_time)
    	CALL write_restart_data ( )
    	IF (LEOPR_ds > 1) THEN
    	   IF (act_time-initial_time==act_timestep) THEN
    	      output_time_restart = output_time_restart+LEOPR_ds*time_to_real_seconds(act_timestep)*1.0_Double-&
            time_to_real_seconds(act_timestep)*1.0_Double
    	   ELSE
    	      output_time_restart = output_time_restart+LEOPR_ds*time_to_real_seconds(act_timestep)*1.0_Double
    	   END IF
    	ELSE
    	   output_time_restart = output_time_restart+time_to_real_seconds(act_timestep)*1.0_Double
        END IF
    END IF
    !
!    call system_clock(ende)
!    time=float(ende-start)/float(rate) ! evtl. cmax beachten!
!    write(6,*) "Zeit in Sekunden Knotentransfer: ",time
    !
    IF(ALLOCATED(node_clean))               DEALLOCATE(node_clean)
    IF(ALLOCATED(node_depth_dredge))        DEALLOCATE(node_depth_dredge)
    IF(ALLOCATED(node_supply))              DEALLOCATE(node_supply)
    IF(ALLOCATED(node_depth_supply))        DEALLOCATE(node_depth_supply)
    !LEO IF(ALLOCATED(node_noero_depth))         DEALLOCATE(node_noero_depth)
    IF(ALLOCATED(total_volume_loc))         DEALLOCATE(total_volume_loc)
    IF(ALLOCATED(sediment_volume_loc))      DEALLOCATE(sediment_volume_loc)
    IF(ALLOCATED(water_volume_loc))         DEALLOCATE(water_volume_loc)
    IF(ALLOCATED(fraction_volume_loc))      DEALLOCATE(fraction_volume_loc)
    IF(ALLOCATED(node_sediment_volume_loc)) DEALLOCATE(node_sediment_volume_loc)
    IF(ALLOCATED(node_water_volume_loc))    DEALLOCATE(node_water_volume_loc)
    IF(ALLOCATED(node_fraction_volume_loc)) DEALLOCATE(node_fraction_volume_loc)
!    IF(ALLOCATED(dredge_node_index))        DEALLOCATE(dredge_node_index)
    IF(ALLOCATED(check_node_total_volume))  DEALLOCATE(check_node_total_volume)
    upd_total_volume = .false.
    disposing_scours = .false.
    !
  END SUBROUTINE compute_dredgesim_d
!RK parcom2 from bief for parallelising
!                    ******************
                     SUBROUTINE PARCOM2_DS &
!                    ******************
!
     ( X1 , X2 , X3 , NPOIN , NPLAN , ICOM , IAN , &
    NB_NEIGHB,NB_NEIGHB_PT,LIST_SEND, &
    NH_COM,DIMNHCOM,BUF_SEND,BUF_RECV,DIMBUF)

!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPLEMENTS A VECTOR AT THE INTERFACES BETWEEN
!+                SUB-DOMAINS.
!+
!+            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!+                VECTORS IN THE BLOCK ARE TREATED.
!
!note     IMPORTANT : FROM RELEASE 5.9 ON, IDENTICAL VALUES ARE
!+                     ENSURED AT INTERFACE POINTS SO THAT DIFFERENT
!+                     PROCESSORS WILL ALWAYS MAKE THE SAME DECISION
!+                     IN TESTS ON REAL NUMBERS.
!
!warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!+            IGNORED FOR THE TIME BEING
!
!history  J-M HERVOUET (LNHE)
!+        13/08/08
!+        V5P9
!+   AFTER REINHARD HINKELMANN (HANNOVER UNI.)
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IAN            |-->| NUMBER OF VECTORS TO BE CONDIDERED (1, 2 OR 3)
!| ICOM           |-->| COMMUNICATION MODE
!|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
!|                |   | = 2 : CONTRIBUTIONS ADDED
!|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
!|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
!| MESH           |-->| MESH STRUCTURE
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN          |-->| NUMBER OF POINTS
!| X1             |<->| VECTOR TO BE COMPLETED
!| X2             |<->| VECTOR TO BE COMPLETED
!| X3             |<->| VECTOR TO BE COMPLETED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY : NCSIZE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: ICOM,NPOIN,NPLAN,IAN
!
!     STRUCTURES: VECTORS OR BLOCKS
!
!RK      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      DOUBLE PRECISION, INTENT(INOUT) :: X1(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: X2(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: X3(NPOIN,NPLAN)
      INTEGER  :: NB_NEIGHB,NB_NEIGHB_PT(NB_NEIGHB),LIST_SEND(NB_NEIGHB)
      INTEGER :: DIMNHCOM
      INTEGER  ::  NH_COM(DIMNHCOM,NB_NEIGHB),DIMBUF
      DOUBLE PRECISION :: BUF_SEND(DIMBUF,NB_NEIGHB),BUF_RECV(DIMBUF,NB_NEIGHB)

!
!-----------------------------------------------------------------------
!
      CALL PARACO(X1,X2,X3,NPOIN,ICOM,IAN,NPLAN, &
             NB_NEIGHB,NB_NEIGHB_PT,LIST_SEND, &
             NH_COM,DIMNHCOM,BUF_SEND,BUF_RECV,DIMBUF)

!     &            NB_NEIGHB,NB_NEIGHB_PT%I,LIST_SEND%I,
!     &            NH_COM%I,NH_COM%DIM1,BUF_SEND%R,
!     &            BUF_RECV%R,BUF_SEND%DIM1)
!
!     RELEASE 5.9 : ENSURES SAME VALUES AT INTERFACE POINTS
!                   SHARED BY SEVERAL SUB-DOMAINS
!
      IF(ICOM.EQ.2.AND.NCSIZE.GT.2) THEN
!
      CALL PARACO(X1,X2,X3,NPOIN,1,IAN,NPLAN,&
            NB_NEIGHB,NB_NEIGHB_PT,LIST_SEND, &
             NH_COM,DIMNHCOM,BUF_SEND,BUF_RECV,DIMBUF)


!     &            MESH%NB_NEIGHB,MESH%NB_NEIGHB_PT%I,MESH%LIST_SEND%I,
!     &            MESH%NH_COM%I,MESH%NH_COM%DIM1,MESH%BUF_SEND%R,
!     &            MESH%BUF_RECV%R,MESH%BUF_SEND%DIM1)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE PARCOM2_DS





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
END MODULE m_dredgesim_compute
! TailOfPackageModule ----------------------------------------------------
