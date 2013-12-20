! -------------------------------------------------------------------------
! HeadOfPackageDataModule -------------------------------------------------
!
!! data management of the "dredgesim"-package
!!                                                                  
!
MODULE m_dredgesim_data
  !
  ! ----------------------------------------------------------------------
  ! [A] base modules with frequently used methods
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] base module with global constant values ----------------------
  USE b_constants, ONLY :  &
       ! parameter 
       Single, Double 
  ! [A.1.2] base module with type+methods "error-handling" ---------------
  USE b_error, ONLY :      &
       ! typ definitions
       t_error,            &
       ! routines / interfaces
       no_error, any_error, &
       setup_error_act 
  ! [A.1.3] base module with type+methods "file-handling" ----------------
  USE b_file, ONLY :       &
       ! typ definitions
       t_file,             &
       ! routines / interfaces
       new_file, kill_file, print_file
  ! [A.1.4] base module with type+methods "timestep" ---------------------
  USE b_time, ONLY :       &
       ! type definitions
       t_time,             &
       ! routines / interfaces
       new_time, kill_time, print_time
  ! [A.1.5] base module with type+methods "date and time" ----------------
  USE b_datetime, ONLY :   &
       ! type definitions
       t_datetime,         &
       ! routines / interfaces
       new_datetime, kill_datetime, print_datetime
  ! [A.1.6] base module with type+methods "information concerning files" -
  USE b_io_info, ONLY :    &
       ! constant
       c_max_len_pac,      & 
       ! type definitions
       t_io_info,          &
       ! routines / interfaces
       new_io_info, kill_io_info, print_io_info
  ! [A.1.7] base module with type+methods "sediment classification" ------
  USE b_grain, ONLY :      &
       ! type definitions
       t_grain,   &
       new_grain, &
       kill_grain
  ! [A.1.8] (local) base module type+methods "dredge polygon data" -------
  USE l_criterion, ONLY:   &
       ! data type
       t_criterion, &
       ! routines / interfaces
       new_criterion, kill_criterion, print_criterion
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [B] public declarations 
  ! ---------------------------------------------------------------------
  !
  ! [B.1] constant values (parameter)
  !
  !! undefined-value for CHARACTER-variables and -arrays
  CHARACTER (LEN=1) , PUBLIC , parameter :: c_undef_ch='?'            !  
  !! undefined-value for INTEGER-variables and -arrays
  INTEGER           , PUBLIC , parameter :: c_undef_in=-999           ! 
  !! undefined-value for REAL-variables and -arrays 
  REAL              , PUBLIC , parameter :: c_undef_re=1.0E+31        ! 
  !! undefined-value for REAL(Double)-variables and -arrays 
  REAL (KIND=Double), PUBLIC , parameter :: c_undef_dp=1.0E+31_Double ! 
  !! max. number of languages to set
  INTEGER           , PUBLIC , parameter :: c_max_language = 2        ! [German,Englisch]
  !! current language 
  INTEGER           , PUBLIC , parameter :: c_def_language = 1        ! [German]
  !! max. number of symbols in each element of array "fraction_name"
  INTEGER           , PUBLIC , parameter :: c_max_fraction_name_len=12 ! 
  !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! [B.2] data, useable outside "dredgesim"
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  ! [B.2.1] scalars and arrays used externally as a copy
  !
  ! [B.2.2] scalars and arrays - copy of external data
  !
  !! value to mark invalid data
  REAL (KIND=Double) , PUBLIC, SAVE :: c_threshold_double=1.0E+31_Double   ! 
  !
  !! file with steering data of the "dredgesim"-package
  TYPE (t_file)     , PUBLIC , SAVE :: steering_file ! 
  !! file for printing output in the "dredgesim"-package
  TYPE (t_file)     , PUBLIC , SAVE :: printer_file  ! 
  !! file for test output in the "dredgesim"-package
  TYPE (t_file)     , PUBLIC , SAVE :: trace_file    ! 
  !
  !! initial date and time for the "dredgesim"-package
  TYPE (t_datetime) , PUBLIC , SAVE :: initial_time  !
  !! end date and time for the "dredgesim"-package
  TYPE (t_datetime) , PUBLIC , SAVE :: end_time      ! 
  INTEGER, PUBLIC, SAVE :: debug_ds = 0
  INTEGER, PUBLIC, SAVE :: leopr_ds
  INTEGER, PUBLIC, SAVE :: ptinig_ds
  INTEGER, PUBLIC, SAVE :: NB_NEIGHB
  INTEGER, PUBLIC, SAVE :: dimbuf
  INTEGER, PUBLIC, SAVE :: dimnhcom
  !
  ! [B.2.3] scalars and arrays (original data) to point at
  !
  !! index list for the allocation of mesh element to dredge polygon 
  !! dredge_poly_index(i,:) : dredge index for the i-th element 
  !! dredge_poly_index(i,j) : i-th element belongs to j-th dredge polygon (index eq. 1) 
  !! idex eq. 0 if no dredge polygon exists for the element
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredge_poly_index(:,:)   ! 
  !! ... update variable, in case "dredge_poly_index" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_dredge_poly_index ! 
  !! index list for the allocation of mesh node to dredge polygon 
  !! dredge_node_index(i,:) : dredge index for the i-th node 
  !! dredge_node_index(i,j) : i-th node belongs to j-th dredge polygon (index eq. 1) 
  !! idex eq. 0 if no dredge polygon exists for the node
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredge_node_index(:,:)   ! 
  !! ... update variable, in case "dredge_node_index" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_dredge_node_index ! 
  !! index list for the allocation of mesh element to disposal polygon 
  !! dispose_poly_index(i,:) : disposal index for the i-th element 
  !! dispose_poly_index(i,j) : i-th element belongs to j-th disposal polygon (index eq. 1) 
  !! idex eq. 0 if no dispsosal polygon exists for the element
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dispose_poly_index(:,:)   !  
  !! ... update variable, in case "dispose_poly_index" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_dispose_poly_index !
  !! index list for the allocation of mesh node to disposal polygon 
  !! dispose_poly_index(i,:,:) : disposal index for the i-th node 
  !! dispose_poly_index(i,j,:) : i-th node belongs to j-th disposal polygon (index eq. 1) 
  !! idex eq. 0 if no dispsosal polygon exists for the node
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dispose_node_index(:,:,:)   !  
  !! ... update variable, in case "dispose_node_index" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_dispose_node_index !
  !! index list for the allocation of mesh element to dredge polygon for time steered operations
  !! dredge_poly_index_tc(i,:) : dredge index for the i-th element 
  !! dredge_poly_index_tc(i,j) : i-th element belongs to j-th dredge polygon (index eq. 1) 
  !! idex eq. 0 if no dredge polygon exists for the element
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredge_poly_index_tc(:,:)   !
  !! ... update variable, in case "dredge_poly_index_tc" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_dredge_poly_index_tc !
  !! index list for the allocation of mesh node to dredge polygon for time steered operations
  !! dredge_node_index_tc(i,:) : dredge index for the i-th node 
  !! dredge_node_index_tc(i,j) : i-th node belongs to j-th dredge polygon (index eq. 1) 
  !! idex eq. 0 if no dredge polygon exists for the node
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredge_node_index_tc(:,:)   !
  !! ... update variable, in case "dredge_node_index_tc" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_dredge_node_index_tc !
  !! index list for the allocation of mesh element to disposal polygon for time steered operations
  !! dispose_poly_index(i,:) : disposal index for the i-th element 
  !! dispose_poly_index(i,j) : i-th element belongs to j-th disposal polygon (index eq. 1) 
  !! idex eq. 0 if no dispsosal polygon exists for the element
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dispose_poly_index_tc(:,:)   !  
  !! ... update variable, in case "dispose_poly_index_tc" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_dispose_poly_index_tc !
  !! index list for the allocation of mesh node to disposal polygon for time steered operations
  !! dispose_poly_index(i,:) : disposal index for the i-th node 
  !! dispose_poly_index(i,j) : i-th node belongs to j-th disposal polygon (index eq. 1) 
  !! idex eq. 0 if no disposal polygon exists for the node
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dispose_node_index_tc(:,:)   !  
  !! ... update variable, in case "dispose_node_index_tc" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_dispose_node_index_tc !
  !! index list for the allocation of mesh element to disposal polygon for artificial bed load supply
  !! art_bed_load_poly_index(i,:) : disposal index for the i-th element 
  !! art_bed_load_poly_index(i,j) : i-th element belongs to j-th disposal polygon (index eq. 1) 
  !! idex eq. 0 if no dispsosal polygon exists for the element
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: art_bed_load_poly_index(:,:)   ! 
  !! ... update variable, in case "art_bed_load_poly_index" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_art_bed_load_poly_index !
  !! index list for the allocation of mesh node to disposal polygon for artificial bed load supply
  !! art_bed_load_node_index(i,:) : disposal index for the i-th node 
  !! art_bed_load_node_index(i,j) : i-th node belongs to j-th disposal polygon (index eq. 1) 
  !! idex eq. 0 if no dispsosal polygon exists for the node
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: art_bed_load_node_index(:,:)   ! 
  !! ... update variable, in case "art_bed_load_node_index" is updated
  LOGICAL , PUBLIC , SAVE                        :: upd_art_bed_load_node_index !
  !
  !! number of disposal polygons
  INTEGER , PUBLIC , SAVE , TARGET :: nof_dispose_poly
  !! number of dredge polygons for time controlled dredging operations
  INTEGER , PUBLIC , SAVE , TARGET :: nof_dredge_poly_tc
  !! number of disposal polygons for time controlled dredging operations
  INTEGER , PUBLIC , SAVE , TARGET :: nof_dispose_poly_tc
  !! number of disposal polygons for artificial bed load supply
  INTEGER , PUBLIC , SAVE , TARGET :: nof_predef_disp_poly
  !
  !! names of disposal polygons
  !! dispose_poly_name(i,:) gives name of i-th disposal polygon 
  !! dispose_poly_name(i,n) gives name of n-th dredge polygon belonging to i-th disposal polygon 
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dispose_poly_name(:,:)
  !! list of disposal polygon names
  !! list_of_disp_polys(i) gives name of i-th disposal polygon 
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: list_of_disp_polys(:)
  !! names of dredge polygons for time controlles dredging
  !! dredge_poly_name_tc(i) gives name of i-th dredge polygon 
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredge_poly_name_tc(:)
  !! names of disposal polygons for time controlles disposal
  !! dispose_poly_name_tc(i) gives name of i-th disposal polygon 
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dispose_poly_name_tc(:)
  !! total volume (sediment+pore water) to be dredged by time controlled dredging per dredge polygon
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredge_tot_vol_tc(:)
  !! sediment volume to be dredged by time controlled dredging per dredge polygon
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredge_sed_vol_tc(:)
  !! pore water volume to be dredged by time controlled dredging per dredge polygon
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredge_water_vol_tc(:)
  !! sediment volume to be disposed by time controlled disposal per disposal polygon
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dispose_sed_vol_tc(:)
  !! names of disposal polygons for artificial bed load supply
  !! predef_disp_poly_name(i) gives name of i-th disposal polygon
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: predef_disp_poly_name(:)
  !! names of sediment classes for artificial bed load supply
  !! predef_disp_sed_class(i,n) gives name of n-th sediment class in i-th disposal polygon
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: predef_disp_sed_class(:,:)
  !! dredging_rate per dredge polygon in m**3/s volume to be dredged per time increment
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredging_rate(:)
  !! Lower limiting value for a volume to dredge to initiate dredging action
  !! if calculated dredged volume of a dredge polygon exceeds this value dredging is initiated
  !! minimum_volume(i) gives value for dredge polygon i
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: minimum_volume(:)
  !! radius to define a circle within it is checked whether the herein dredged volume is greater than a minimum volume
  !! sector_radius(i) gives value for dredge polygon i
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: sector_radius(:)
  !! disposal_rate per dredge polygon in m**3/s volume to be disposed per time increment
  !! Attention: disposal_rate(i) gives disposal rate of i-th dredge polygon (!)
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disposal_rate(:)
  !! weighting factor for depostion of material on several disposal polygons
  !! dispose_weighting_factor(i,n) gives percentage of dredged volume from dredge polygon n to be disposed 
  !! in i-th disposal polygon
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dispose_weighting_factor(:,:)
  !! date and time of dredging operation (start, end) as CHARACTER for transfer in data type t_datetime 
  !! predef_dredge_time_tc(i,1) gives initial time of dredging on i-th dredge polygon 
  !! predef_dredge_time_tc(i,2) gives end time of dredging on i-th dredge polygon 
  !! for time controlled maintenance only
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: predef_dredge_time_tc(:,:)
  !! date and time of disposal operation (start, end) as CHARACTER for transfer in data type t_datetime 
  !! predef_disp_time_tc(i,1) gives initial time of disposing on i-th disposal polygon 
  !! predef_disp_time_tc(i,2) gives end time of disposing on i-th disposal polygon 
  !! for time controlled maintenance only
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: predef_disp_time_tc(:,:)
  !! total sediment volume to be disposed by artificial bed load supply per disposal polygon
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: predef_disp_sed_vol(:)
  !! date and time for the time of observing the bottom as CHARACTER for transfer in data type t_datetime 
  !! ini_obs_time(i)  gives the time for dredge_polygon i
  !! for dredge criterion only
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: ini_obs_time(:)
  !! time period of re-occurence of observing the bottom 
  !! observing_period(i) gives the value of this time period for dredge_polygon i
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET :: observing_period(:)
  !! Limiting discharge for decision about navigation of ships to observe the bottom 
  !! limiting_discharge (i) gives the value of this discharge for dredge_polygon i
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: limiting_discharge(:)
  !! Logical for determining whether navigation is possible or not
  !! dependent on the limiting discharge and the actual discharge dependent on the dredge polygon
  !! default is always true 
  LOGICAL , PUBLIC , ALLOCATABLE, SAVE , TARGET  :: navigation_possible(:)
  !
  !! sediment distribution to be disposed by artificial bed load supply
  !! predef_sed_distrib(i,n) percentage of sediment n to be disposed on disposal polygon i 
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: predef_sed_distrib(:,:)
  !! date and time of disposal operation (start, end) as CHARACTER for transfer in data type t_datetime 
  !! predef_depos_time(i,1) gives initial time of disposing on i-th disposal polygon 
  !! predef_depos_time(i,2) gives end time of disposing on i-th disposal polygon 
  !! for artificial bed load supply only
  CHARACTER (LEN=80) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: predef_depos_time(:,:)
  !
  !! scouring options
  !! global data
  !! logical indicating whether deposition in scours should be taken into account
  LOGICAL , PUBLIC , SAVE :: disposing_scours
  !! number of iterations available for disposing scours
  INTEGER , PUBLIC , SAVE :: nof_iterations_disp_scours
  !! maximum error allowed when disposing scours...
  !! error indicates difference between calculated storage capacity according to actual deposition 
  !! depth and given volume to dispose
  REAL(KIND=Double) , PUBLIC , SAVE :: max_error_disp_scours
  !! actual deposition depth calculated by several iterations when disposing scours
  REAL(KIND=Double) , PUBLIC , SAVE :: act_depo_depth_disp_scours
  !! maximal deposition depth when disposing scours given through steering data
  REAL(KIND=Double) , PUBLIC , SAVE :: min_depo_depth_disp_scours
  !! local data
  !! logical indicating whether deposition in scours should be taken into account
  CHARACTER (LEN=80), PUBLIC , ALLOCATABLE, SAVE, TARGET :: disp_scours_auto(:)
  !! logical indicating whether deposition in scours should be taken into account
  CHARACTER (LEN=80), PUBLIC , ALLOCATABLE, SAVE, TARGET :: disp_scours_tc(:)
  !! logical indicating whether deposition in scours should be taken into account
  CHARACTER (LEN=80), PUBLIC , ALLOCATABLE, SAVE, TARGET :: disp_scours_abl(:)
  !
  !! water depth on elements 
  !! poly_water_depth(i) : water depth on element i
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: poly_water_depth(:)  ! 
  !! ... update variable, in case "poly_water_depth" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_poly_water_depth  !
  !! list of dredge criterion types per element
  !! dredge_crit_type(i) : dredge criterion type on element i
  INTEGER , PUBLIC , ALLOCATABLE , SAVE , TARGET            :: dredge_crit_type(:)  ! 
  !! ... update variable, in case "dredge_crit_type" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_dredge_crit_type  !   
  !
  !! elementwise dredged total volume dependent on the dredge polygon 
  !! poly_total_volume(i,n) : total volume being dredged on element i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: poly_total_volume(:,:) ! 
  !! ... update variable, in case "poly_total_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_poly_total_volume  ! 
  !! nodewise dredged total volume dependent on the dredge polygon 
  !! node_total_volume(i,n) : total volume being dredged on node i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: node_total_volume(:,:) ! 
  !! ... update variable, in case "node_total_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_node_total_volume  ! 
  !! nodewise total volume to dispose dependent on the dredge and the disposal polygon 
  !! disp_node_total_volume(i,k,n) : total dredged volume of dredge polygon k being disposed on node i in disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_node_total_volume(:,:,:) ! 
  !! ... update variable, in case "disp_node_total_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_disp_node_total_volume  ! 
  !
  !! sum of dredged total volume dependent on the dredge polygon 
  !! total_volume(n) : summed total volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: total_volume(:) ! 
  !! overall sum of dredged total volume dependent on the dredge polygon at the end of the computation 
  !! all_total_volume(n) : overall summed total volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: all_total_volume(:) ! 
  !! overall sum of dredged total volume dependent on the dredge polygon at the last observing time 
  !! all_total_volume_old(n) : overall summed total volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: all_total_volume_old(:) ! 
  !! ... update variable, in case "total_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_total_volume  ! 
  !
  !! elementwise dredged sediment volume dependent on the dredge polygon 
  !! poly_sediment_volume(i,n) : sediment volume being dredged on element i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: poly_sediment_volume(:,:) ! 
  !! ... update variable, in case "poly_sediment_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_poly_sediment_volume  ! 
  !! nodewise dredged sediment volume dependent on the dredge polygon 
  !! node_sediment_volume(i,n) : sediment volume being dredged on node i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: node_sediment_volume(:,:) ! 
  !! ... update variable, in case "node_sediment_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_node_sediment_volume  ! 
  !! nodewise dredged sediment volume dependent on the dredge polygon saved for a restart 
  !! node_sediment_volume(i,n) : sediment volume being dredged on node i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: node_sediment_volume_rs(:,:) ! 
  !! ... update variable, in case "node_sediment_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_node_sediment_volume_rs  ! 
  !
  !! sum of dredged sediment volume dependent on the dredge polygon 
  !! sediment_volume(n) : summed sediment volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: sediment_volume(:) ! 
  !! overall sum of dredged sediment volume dependent on the dredge polygon at the end of the computation
  !! all_sediment_volume(n) : overall summed sediment volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: all_sediment_volume(:) ! 
  !! overall sum of dredged sediment volume dependent on the dredge polygon at the last observing time 
  !! all_sediment_volume_old(n) : overall summed sediment volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: all_sediment_volume_old(:) ! 
  !! ... update variable, in case "sediment_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_sediment_volume  ! 
  !
  !! nodewise dredged sediment volume dependent on the dredge polygon for time controlled dredging operation
  !! node_sediment_volume_tc(i,n) : sediment volume being dredged on node i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: node_sediment_volume_tc(:,:) ! 
  !! ... update variable, in case "node_sediment_volume_tc" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_node_sediment_volume_tc  ! 
  !
  !! sum of dredged sediment volume dependent on the dredge polygon for time controlled dredging operation
  !! sediment_volume_tc(n) : summed sediment volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: sediment_volume_tc(:) ! 
  !! ... update variable, in case "sediment_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_sediment_volume_tc  ! 
  !
  !! elementwise dredged water volume (pore water) dependent on the dredge polygon 
  !! poly_water_volume(i,n) : water volume being dredged on element i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: poly_water_volume(:,:) ! 
  !! ... update variable, in case "poly_water_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_poly_water_volume  ! 
  !! nodewise dredged water volume (pore water) dependent on the dredge polygon 
  !! node_water_volume(i,n) : water volume being dredged on node i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: node_water_volume(:,:) ! 
  !! ... update variable, in case "node_water_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_node_water_volume  ! 
  !
  !! sum of dredged water volume dependent on the dredge polygon 
  !! water_volume(n) : summed water volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: water_volume(:) ! 
  !! overall sum of dredged water volume dependent on the dredge polygon at the end of the computation 
  !! all_water_volume(n) : overall summed water volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: all_water_volume(:) ! 
  !! overall sum of dredged water volume dependent on the dredge polygon at the last observing time 
  !! all_water_volume_old(n) : overall summed water volume for dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: all_water_volume_old(:) ! 
  !! ... update variable, in case "water_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_water_volume  ! 
  !
  !! element- and fractionwise dredged sediment volume dependent on the dredge polygon 
  !! poly_fraction_volume(i,m,n) : sediment volume of fraction m being dredged on element i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: poly_fraction_volume(:,:,:) ! 
  !! ... update variable, in case "poly_fraction_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_poly_fraction_volume  ! 
  !! node- and fractionwise dredged sediment volume dependent on the dredge polygon 
  !! node_fraction_volume(i,m,n) : sediment volume of fraction m being dredged on element i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: node_fraction_volume(:,:,:) ! 
  !! ... update variable, in case "node_fraction_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_node_fraction_volume  ! 
  !! node- and fractionwise dredged sediment volume dependent on the dredge polygon for a restart
  !! node_fraction_volume(i,m,n) : sediment volume of fraction m being dredged on element i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: node_fraction_volume_rs(:,:,:) ! 
  !! ... update variable, in case "node_fraction_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_node_fraction_volume_rs  !
  !! timestamp for last observing time dependent on the dredge polygon for a restart
  !! last_obs_time_rs : timestamp for last observing time of dredge polygon n
  CHARACTER (LEN=19) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: last_obs_time_rs(:) !
  !! timestamp for next observing time dependent on the dredge polygon for a restart
  !! last_obs_time_rs : timestamp for next observing time of dredge polygon n
  CHARACTER (LEN=19) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: next_obs_time_rs(:) !
  !! timestamp for last act time
  !! last_act_time_rs : timestamp for last act time
  CHARACTER (LEN=19) , PUBLIC , SAVE :: last_act_time_rs !
  !! timestamp for initial time
  !! ini_time_rs : timestamp for initial time
  CHARACTER (LEN=19) , PUBLIC , SAVE :: ini_time_rs !
  !
  !! sum of dredged sediment volume dependent on the dredge polygon and the sediment fraction
  !! fraction_volume(m,n) : summed sediment volume for sediment fraction m and dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: fraction_volume(:,:) ! 
  !! overall sum of dredged sediment volume dependent on the dredge polygon and the sediment fraction
  !! all_fraction_volume(m,n) : summed sediment volume for sediment fraction m and dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: all_fraction_volume(:,:) ! 
  !! overall sum of dredged sediment volume dependent on the dredge polygon and the sediment fraction at the last observing time
  !! all_fraction_volume_old(m,n) : summed sediment volume for sediment fraction m and dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: all_fraction_volume_old(:,:) ! 
  !! ... update variable, in case "fraction_volume" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_fraction_volume  ! 
  !
  !! element- and fractionwise dredged sediment volume dependent on the dredge polygon for time controlled dredging operation
  !! poly_fraction_volume_tc(i,m,n) : sediment volume of fraction m being dredged on element i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: poly_fraction_volume_tc(:,:,:) ! 
  !! ... update variable, in case "poly_fraction_volume_tc" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_poly_fraction_volume_tc  ! 
  !
  !! node- and fractionwise dredged sediment volume dependent on the dredge polygon for time controlled dredging operation
  !! poly_fraction_volume_tc(i,m,n) : sediment volume of fraction m being dredged on node i in dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: node_fraction_volume_tc(:,:,:) ! 
  !! ... update variable, in case "node_fraction_volume_tc" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_node_fraction_volume_tc  ! 
  !
  !! sum of dredged sediment volume dependent on the dredge polygon and the sediment fraction for time controlled dredging operation
  !! fraction_volume_tc(m,n) : summed sediment volume for sediment fraction m and dredge polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: fraction_volume_tc(:,:) ! 
  !! ... update variable, in case "fraction_volume_tc" is updated
  LOGICAL            , PUBLIC , SAVE                        :: upd_fraction_volume_tc  ! 
  !
  !! update variable, in case output dredge volumes are updated
  !! upd_out_volumes(i) : i-th dredge polygon with new data for dredging output
  LOGICAL , PUBLIC , ALLOCATABLE , SAVE , TARGET :: upd_out_volumes(:) !
  !
  !! aim bottom depth to attain by dredging on each element dependent on the dredge polygon 
  !! aim_poly_depth (i,m) : aim depth on element i in dredge polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: aim_poly_depth (:,:)  
  !! aim bottom depth to attain by dredging on each node dependent on the dredge polygon 
  !! aim_node_depth (i,m) : aim depth on node i in dredge polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: aim_node_depth (:,:)  
  !! aim bottom depth to attain by time controlled dredging on each node dependent on the dredge polygon 
  !! aim_node_depth (i,m) : aim depth on node i in dredge polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: aim_node_depth_tc (:,:)  
  !! aim bottom depth to attain by artificial bed load supply on each node dependent on the dredge polygon 
  !! aim_node_depth (i,m) : aim depth on node i in dredge polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: aim_node_depth_abl (:,:)  
  !! difference between aim bottom depth to dredge and current bottom depth on each element dependent on the dredge polygon 
  !! delta_dredge_poly_depth (i,m) : difference on element i in dredge polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: delta_dredge_poly_depth (:,:)
  !! difference between aim bottom depth to dredge and current bottom depth on each node dependent on the dredge polygon 
  !! delta_dredge_node_depth (i,m) : difference on node i in dredge polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: delta_dredge_node_depth (:,:)
  !! difference between bottom depth as a result of disposal action and current bottom depth on each element dependent on the disposal polygon 
  !! delta_disp_poly_depth (i,m) : difference on element i in disposal polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: delta_disp_poly_depth (:,:)
  !! difference between bottom depth as a result of disposal action and current bottom depth on each node dependent on the disposal polygon 
  !! delta_disp_node_depth_tc (i,m,n) : difference on node i induced by deposition of dredged material from dredge polygon m in disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: delta_disp_node_depth (:,:,:)
  !! bottom depth as a result of disposal on each element dependent on the disposal polygon
  !! disp_poly_depth (i,m) : resulting bottom depth on element i in disposal polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_poly_depth (:,:)
  !! bottom depth as a result of disposal on each node dependent on the disposal polygon
  !! disp_node_depth (i,m,n) : resulting bottom depth on node i induced by deposition of dredged material from dredge polygon m in disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_node_depth (:,:,:)
  !! overall change of bottom depth to attain by dredging on each node 
  !! dzb_dredge (i) : diffenrece depth on node i
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dzb_dredge (:)  
  !! overall change of bottom depth to attain by disposing on each node 
  !! dzb_disp (i) : diffenrece depth on node i
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dzb_disp (:)  
  !! dredged volumes which shall be disposed automatically
  !! dredged_sediment_volume_to_disp (m,n) : dredged volume of dredge polygon m which shall be disposed in disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredged_sediment_volume_to_disp (:,:)
  !! dredged fraction volumes which shall be disposed automatically
  !! dredged_fraction_volume_to_disp (m,n,k) : dredged fractioned volume k in dredge polygon m which shall be disposed in disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: dredged_fraction_volume_to_disp (:,:,:)
  !! overall sum of total disposed volume dependent on the dredge polygon and the disposal polygon at the end of the computation 
  !! disp_total_volume(n,k) : overall summed total volume for dredge polygon k and associated disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_total_volume (:,:)
  !! overall sum of total disposed volume dependent on the dredge polygon and the disposal polygon at the latest observing time
  !! disp_total_volume_old(n,k) : overall summed total volume for dredge polygon k and associated disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_total_volume_old (:,:)
  !! overall sum of disposed sediment volume dependent on the dredge polygon and the disposal polygon at the end of the computation 
  !! disp_sediment_volume(n,k) : overall summed sediment volume for dredge polygon k and associated disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_sediment_volume (:,:)
  !! overall sum of disposed sediment volume dependent on the dredge polygon and the disposal polygon at the latest observing time
  !! disp_sediment_volume_old(n,k) : overall summed sediment volume for dredge polygon k and associated disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_sediment_volume_old (:,:)
  !! overall sum of disposed fraction volume dependent on the dredge polygon and the disposal polygon at the end of the computation 
  !! disp_fraction_volume(m,n,k) : overall summed volume of fraction m for dredge polygon k and associated disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_fraction_volume (:,:,:)
  !! overall sum of sediment disposed volume dependent on the dredge polygon and the disposal polygon at the latest observing time
  !! disp_fraction_volume_old(m,n,k) : overall summed volume of fraction m for dredge polygon k and associated disposal polygon n
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: disp_fraction_volume_old (:,:,:)
  !! difference between bottom depth as a result of time controlles disposal action and current bottom depth on each node dependent on the disposal polygon 
  !! delta_disp_node_depth_tc (i,m) : difference on node i in disposal polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: delta_disp_node_depth_tc (:,:)
  !! difference between bottom depth as a result of artificial bed load supply and current bottom depth on each node dependent on the disposal polygon 
  !! delta_node_depth_abl (i,m) : difference on node i in disposal polygon m
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: delta_node_depth_abl (:,:)
  !
  !! edge_related_surface(j) :: sum of element areas related to edge j
  REAL (KIND = Double), PUBLIC , ALLOCATABLE , SAVE, TARGET :: edge_related_surface(:) ! 
  !! array to take into account weighting factors in interpolations 
  !! edge_weighted_sum(j) : weighting factor for edge j
  REAL (KIND = Double), PUBLIC , ALLOCATABLE , SAVE, TARGET :: edge_weighted_sum(:) ! 
  !! node_related_surface(j) :: sum of element areas related to node j
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE, TARGET :: node_related_surface(:) ! 
  !! array to take into account weighting factors in interpolations 
  !! node_weighted_sum(j) : weighting factor for node j
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE, TARGET :: node_weighted_sum(:) ! 
  !
  !! definition of referenced free surface on each node
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: free_surface_node_depth (:)
  !! definition of referenced free surface on each element
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: free_surface_poly_depth (:)
  !! definition of stream kilometres per node or element
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: fkm (:)
  !! definition of stream kilometres per section defined in an input file
  REAL (KIND=Double) , PUBLIC , ALLOCATABLE , SAVE , TARGET :: km (:)
  !
  !! restart or not
  LOGICAL, PUBLIC, SAVE :: restart=.false.
  REAL (KIND=Double), PUBLIC, SAVE :: output_time_restart=0.0_Double
  !  
  !! write dredgesim messages
  LOGICAL, PUBLIC, SAVE :: write_ds_messages=.false.
  !
  ! [B.2.4] scalar and arrays (external data) to point at from the inside of the "dredgsim"-package
  !
  !! number of nodes per element
  !! nodes_of_poly(i) : number of nodes in element i
  INTEGER , PUBLIC , POINTER , SAVE :: nodes_of_poly(:) ! 
  !! edgelist per element 
  !! edgelist_of_poly(i,l) : index of edge l in element i
  INTEGER , PUBLIC , POINTER , SAVE :: edgelist_of_poly(:,:) ! 
  !! nodlist per element
  !! nodelist_of_poly(i,l) : index of node l in element i
  INTEGER , PUBLIC , POINTER , SAVE :: nodelist_of_poly(:,:) ! 
  !
  !! center coordinates of elements 
  !! center_coord(i,:) : coordinates for element i 
  !! center_coord(i,1) : x-coordinate of center of element i 
  !! center_coord(i,2) : y-coordinate of center of element i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: center_coord(:,:)  ! 
  !! gravitational center coordinates of elements
  !! grav_center_coord(i,:) : coordinates for element i
  !! grav_center_coord(i,1) : x-coordinate of gravitational center of element i
  !! grav_center_coord(i,2) : y--coordinate of gravitational center of element i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: grav_center_coord(:,:)  !
  !! coordinatec of nodes 
  !! node_coord(i,:) : coordinates of node i 
  !! node_coord(i,1) : x-coordinate of node i
  !! node_coord(i,2) : y-coordinate of node i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: node_coord(:,:)   
  ! 
  !! element depths
  !! poly_depth(i) : depth of element i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: poly_depth(:)  ! 
  !! element areas
  !! poly_area(i) : area of element i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: poly_area(:)  ! 
  !! node_area(i) : area of node i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: node_area(:)  ! 
 !! knolg(i) : global number of node i
  INTEGER , PUBLIC , POINTER , SAVE :: knolg(:)  !
 !! node_neighb(i) : neighbor of node i
  INTEGER , PUBLIC , POINTER , SAVE :: node_neighb(:)  !
 !! nb_neighb_pt(i) : number of shared point(i)
  INTEGER , PUBLIC , POINTER , SAVE :: nb_neighb_pt(:)  !
 !! list_send(i) : list of processor number (i)
  INTEGER , PUBLIC , POINTER , SAVE :: list_send(:)  !
 !! nh_com(i,j) : global number of point i in list of shared points with processor j
  INTEGER , PUBLIC , POINTER , SAVE :: nh_com(:,:)  !
  !! buf_send(i,j) : buffer for sending data
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: buf_send(:,:)  !
 !! buf_recv(i,j) : buffer for receiving data
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: buf_recv(:,:)  !
  !! node depths
  !! node_depth(l) : depth of node l
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: node_depth(:)  ! 
  !! edge depths
  !! edge_depth(j) : depth of edge j
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: edge_depth(:)  ! 
  !! porosity on nodes (upper layer)
  !! node_porosity(i) : porosity on node i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: node_porosity(:)  ! 
  !! sediment fraction on elements (upper layer) 
  !! cell_sediment_fraction(i,:) : sediment fractions on element i 
  !! cell_sediment_fraction(i,n) : percentage of sediment fraction n in element i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: cell_sediment_fraction(:,:)  ! 
  !! sediment fraction on nodes (upper layer) 
  !! node_sediment_fraction(i,:) : sediment fractions on node i 
  !! node_sediment_fraction(i,n) : percentage of sediment fraction n on node i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: node_sediment_fraction(:,:)  ! 
  !! water depth on edges
  !! edge_water_depth(j) : water depth on edge j
  REAL (KIND=Double) , PUBLIC, POINTER , SAVE :: edge_water_depth(:) !
  !! water depth on nodes
  !! node_water_depth(l) : water depth on node l
  REAL (KIND=Double) , PUBLIC, POINTER , SAVE :: node_water_depth(:) !
  !! non erodible layer depth of nodes
  !! node_noero_depth(i) : depth of non erodible layer on node i
  REAL (KIND=Double) , PUBLIC , POINTER , SAVE :: node_noero_depth(:)  ! 
  !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! [B.3] data, only used within "dredgesim"
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  ! [B.3.1] scalars - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  !! value for successful initialization of the "dredgesim"-package
  LOGICAL , PUBLIC , SAVE :: initialised = .false.  ! 
  !! value for realization of PRINT-Methods
  LOGICAL , PUBLIC , SAVE :: prn_op      = .false.  ! 
  !! value for realization of TRACE-Methods
  LOGICAL , PUBLIC , SAVE :: trc_op      = .false.  ! 
  !! channel number for PRINT-Methods
  INTEGER , PUBLIC , SAVE :: prn_lun     = -1       ! 
  !! channel number for TRACE-Methods
  INTEGER , PUBLIC , SAVE :: trc_lun     = -1       ! 
  !! current language setup ( 1 = German, 2 = English )
  INTEGER , PUBLIC , SAVE :: language=c_def_language ! 
  !
  !! current timestep for the "dredgesim"-package                    
  !! value is calculated in procedure "run_dredgesim" within module "p_dredgesim_ui"
  TYPE (t_time)     , PUBLIC , SAVE :: act_timestep  ! 
  !! current date and time for the "dredgesim"-package 
  !! is transfered with each call of "run_dredgesim"
  TYPE (t_datetime) , PUBLIC , SAVE :: act_time      !
  !! date and time of old timestep for the "dredgesim"-package 
  !! value is memorized in "run_dredgesim"
  TYPE (t_datetime) , PUBLIC , SAVE :: old_time      !
  !
  ! [B.3.2] arrays - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  !
  !! array for all error messages in this module
  TYPE (t_error)     , PUBLIC , ALLOCATABLE , SAVE :: all_errors(:) ! 
  !
  !! sediment classification (from file)
  TYPE (t_grain) , PUBLIC, ALLOCATABLE , SAVE :: used_sediment_classes(:)      ! 
  !! array with names of sediment fractions used by the morphodynmamic module
  CHARACTER (LEN=c_max_fraction_name_len) , PUBLIC , ALLOCATABLE , SAVE :: fraction_name(:) ! 
  !
  !! date and time for automatical initiation of dredging by dredge criteria dependent on dredge polygon
  !! value is set in "dredgesim_copmute" within compute module
  TYPE (t_datetime) , PUBLIC , ALLOCATABLE , SAVE :: start_dredge_time (:) !
  !! date and time for end of dredging by dredge criteria dependent on dredge polygon 
  !! value is set in "dredgesim_copmute" within compute module
  TYPE (t_datetime) , PUBLIC , ALLOCATABLE , SAVE :: end_dredge_time (:) !
  !! date and time to observe the bottom
  TYPE (t_datetime) , PUBLIC , ALLOCATABLE , SAVE :: time_to_observe (:)      !
  !! previous date and time to observe the bottom
  TYPE (t_datetime) , PUBLIC , ALLOCATABLE , SAVE :: old_time_to_observe (:)      !
  !! initial date and time to observe the bottom
  TYPE (t_datetime) , PUBLIC , ALLOCATABLE , SAVE :: initial_time_to_observe (:)
  !! date and time of dredging action defined by time controlled maintenance dependent on dredge polygon  
  !! value is read from steering file
  !! dredge_time_tc (i,1) : initial time for dredging on dredge polygon i 
  !! dredge_time_tc (i,2) : end time for dredging on dredge polygon i
  TYPE (t_datetime) , PUBLIC , ALLOCATABLE , SAVE :: dredge_time_tc (:,:) !
  !! date and time of disposal action defined by time controlled maintenance dependent on disposal polygon  
  !! value is read from steering file
  !! disposal_time_tc (i,1) : initial time for disposal on diposal polygon i 
  !! disposal_time_tc (i,2) : end time for disposal on disposal polygon i
  TYPE (t_datetime) , PUBLIC , ALLOCATABLE , SAVE :: disposal_time_tc (:,:) !
  !! date and time for disposal action defined by artificial bed load supply dependent on disposal polygon  
  !! value is read from steering file
  !! art_bl_time (i,1) : initial time for disposal on diposal polygon i 
  !! art_bl_time (i,2) : end time for disposal on disposal polygon i
  TYPE (t_datetime) , PUBLIC , ALLOCATABLE , SAVE :: art_bl_time (:,:) !
  !
  ! [B.3.3] parameter to steer reading of input data file - - - - - - - -
  !
  !! max. number of input blocks in steering file
  INTEGER , PUBLIC , parameter :: max_blo=6 !
  !! max. global number of keys in blocks
  INTEGER , PUBLIC , parameter :: max_key=12 !
  !! max. global number of parameters in keys
  INTEGER , PUBLIC , parameter :: max_par=2 !
  !! pointer on position of "Input_Files" in array "blo"
  INTEGER , PUBLIC , parameter :: ptr_input_files=1  ! 
  !! pointer on position of "Output_Files" in array "blo"
  INTEGER , PUBLIC , parameter :: ptr_output_files=2 ! 
  !! max. number of physical parameters to read from an input file
  INTEGER , PUBLIC , parameter :: max_input_pg=3  ! 
  !! allowed blocknames in steering file 
  CHARACTER (LEN=27) , PUBLIC , parameter :: blo(max_blo) = (/ 'Input_Files                ', &
      'Output_Files               ', 'Dredge_Criterion           ', 'Time_Controlled_Maintenance',&
      'Artificial_Bed_Load_Supply ', 'Disposing_Scours           ' /)
  !! max. number of keys in each block
  INTEGER , PUBLIC , parameter :: max_key_in_blo(max_blo) = (/ 4, 1, 12, 7, 5, 3 /) !
  !! allowed keywords in each block of the steering file
  !LEO we fill it up with blankets to ensure that it is f90/f95 conform 
  CHARACTER (LEN=26) , PUBLIC , parameter :: key(max_key,max_blo) = RESHAPE( & !
       (/ 'Input_Dredge_Polygons     ',& 
          'Input_Ref_Surface         ', &
          'Input_Restart_Data        ',& 
          'Write_DS_Messages         ',& 
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',& 
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ', &! B1
          'Output_Protocol           ',&
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',& 
          '                          ',&
          '                          ',& 
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ', &! B2
          'Polygon_Name              ',&
          'Criterion_Type            ',&
          'Observing_Period          ',&
          'Initial_Time_To_Observe   ',&
          'Limiting_Discharge        ',&
          'Critical_Depth            ', &
          'Dredging_Depth            ',&
          'Dredging_Rate             ',& 
          'Minimum_Volume            ',& 
          'Disposal_Polygon          ',&
          'Disposal_Rate             ',& 
          'Disposing_Scours          ', &! B3
          'Dredge_Polygon            ',& 
          'Dredging_Time             ',& 
          'Volume_To_Dredge          ',& 
          'Disposal_Polygon          ',& 
          'Disposal_Time             ',& 
          'Volume_To_Dispose         ', &
          'Disposing_Scours          ',& 
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',& 
          '                          ',&! B4
          'Polygon_Name              ',&
          'Sediment_Class            ',&
          'Sediment_Volume           ',&
          'Deposition_Time           ',&
          'Disposing_Scours          ',&
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',&! B5
          'Number_Of_Iterations      ',&
          'Maximum_Error             ',& 
          'Minimum_Deposition_Depth  ',&
          '                          ',&
          '                          ',&
          '                          ', &
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',&
          '                          ',&
          '                          '/), &! B6
	  (/ max_key, max_blo /) )
  !
  !! package names used to read file
  CHARACTER (LEN=c_max_len_pac) , PUBLIC , PARAMETER :: pac(max_key,max_blo) = RESHAPE( (/'io_ipds   ', &
   'undefined ','undefined ', 'undefined ', '          ', '          ', '          ', '          ', '          ', &
   '          ','          ', '          ', & ! B1
   'undefined ','          ', '          ', '          ', '          ', '          ', '          ', '          ', &
   '          ','          ', '          ', '          ',& ! B2
   'undefined ', 'undefined ', 'undefined ', 'undefined ', 'undefined ', 'undefined ', 'undefined ', 'undefined ', &
   'undefined ', 'undefined ', 'undefined ', 'undefined ', & ! B3
   'undefined ', 'undefined ', 'undefined ', 'undefined ', 'undefined ', 'undefined ', 'undefined ', '          ', &
   '          ', '          ', '          ', '          ', & ! B4
   'undefined ', 'undefined ', 'undefined ', 'undefined ', 'undefined ', '          ', '          ', '          ', &
   '          ', '          ', '          ', '          ', & ! B5
   'undefined ', 'undefined ', 'undefined ', '          ', '          ', '          ', '          ', '          ', &
   '          ', '          ', '          ', '          '/), & ! B6
       (/ max_key, max_blo /) )
  !! max. number of parameters in keys of each block
  INTEGER , PUBLIC , parameter :: max_par_in_key(max_key,max_blo) = RESHAPE( & !
       (/ 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, & ! B1
          2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! B2
          1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, & ! B3
          1, 2, 1, 1, 2, 1, 1, 0, 0, 0, 0, 0, & ! B4
          1, 2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, & ! B5
          1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/), & ! B6
         (/ max_key, max_blo /) )
  !! type of parameters of all keys and blocks
  !!  0 = not existing  
  !!  1 = Character-scalar 
  !!  2 = Integer-scalar   
  !!  3 = Real-scalar      
  !!  4 = Double-scalar    
  !!  5 = Logical-scalar   
  !! 11 = Character-array   
  !! 12 = Integer-array     
  !! 13 = Real-array        
  !! 14 = Double-array      
  !! 15 = Logical-array
  INTEGER , PUBLIC , parameter :: typ_par_in_key(max_par,max_key,max_blo) = RESHAPE( & ! 
       (/ 1,1, 1,1, 1,1, 1,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0,  & ! B1
          1,1, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0,  & ! B2
          1,0, 1,0, 2,0, 1,0, 4,0, 4,0, 4,0, 4,0, 4,4, 1,4, 4,0, 1,0,  & ! B3
          1,0, 1,1, 4,0, 1,0, 1,1, 4,0, 1,0, 0,0, 0,0, 0,0, 0,0, 0,0,  & ! B4
          1,0, 1,4, 4,0, 1,1, 1,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0,  &
          2,0, 4,0, 4,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0/), &
	  (/ max_par, max_key, max_blo /) )
  !! list of physical parameters being necessary in input files
  INTEGER , PUBLIC , parameter :: input_code(max_input_pg,max_key_in_blo(ptr_input_files)) = RESHAPE( (/ & ! 
          0,    0,    0,   & ! B1/1 : ...
	  0,    0,    0,   & ! B1/2 : ...
	  0,    0,    0,   & ! B1/3 : ...
	  0,    0,    0/), & ! B1/4 : ... 
       (/ max_input_pg,max_key_in_blo(ptr_input_files) /) )
  !! list of pointers on position of grid file in input data files ( 0 no information )
  INTEGER , PUBLIC , parameter :: input_grid_ptr(max_key_in_blo(ptr_input_files)) = (/ & ! 
          0, 0, 0, 0 /) 
  !
  !! value for existence of keys
  LOGICAL , PUBLIC , SAVE :: lex_key(max_par,max_key,max_blo) ! 
  !! number of occurrence of blocks 
  INTEGER , PUBLIC , SAVE :: n_blo(max_blo)                   ! 
  !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ! [D.3] variables (static data of module)
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  !
  !! short information about different input files
  TYPE (t_io_info) , PUBLIC , SAVE , TARGET :: input_files(max_key_in_blo(ptr_input_files)) ! 
  !! short information about different output files
  TYPE (t_io_info) , PUBLIC , SAVE , TARGET :: output_files(max_key_in_blo(ptr_output_files)) ! 
  !
  TYPE (t_criterion) , PUBLIC , ALLOCATABLE , SAVE :: dredge_criterion(:) ! 
  !
  ! [C.1] interfaces
  !
  !! initializing/allocating of all data of this package
  INTERFACE init_dredgesim_data
     MODULE PROCEDURE init_dredgesim_data_d ! 
  END INTERFACE
  !! de-initializing/de-allocating of all data
  INTERFACE clear_dredgesim_data
     MODULE PROCEDURE clear_dredgesim_data_d ! 
  END INTERFACE
  !! allocating all dynamic allocatable data (within start-phase) 
  !! here all arrays to be pointed at from ohter packages have to be allocated 
  INTERFACE alloc_dredgesim_start
     MODULE PROCEDURE alloc_dredgesim_start_d ! 
  END INTERFACE
  !! allocating all dynamic allocatable data (wihtin prepare-phase)
  !! here all arrays which are only able to be allocated after knowing the size of external arrays
  !! have to be allocated
  INTERFACE alloc_dredgesim_prepare
     MODULE PROCEDURE alloc_dredgesim_prepare_d ! 
  END INTERFACE
  !! de-allocating of all dynamic allocatable data
  INTERFACE dealloc_dredgesim_data
     MODULE PROCEDURE dealloc_dredgesim_data_d ! 
  END INTERFACE
  !! printing the shape of all data (only arrays)
  INTERFACE print_dredgesim_shape
     MODULE PROCEDURE print_dredgesim_shape_d
  END INTERFACE
  !! printing the contents of all data
  INTERFACE print_dredgesim_data
     MODULE PROCEDURE print_dredgesim_data_d
  END INTERFACE
  !
  !! setting a pointer on external data for array nodes_of_poly(:)
  INTERFACE set_ds_nodes_of_poly_ref
     MODULE PROCEDURE set_ds_nodes_of_poly_ref_d
  END INTERFACE
  !! setting a pointer on external data for array edgelist_of_poly(:,:)
  INTERFACE set_ds_edgelist_of_poly_ref
     MODULE PROCEDURE set_ds_edgelist_of_poly_ref_d
  END INTERFACE
  !! setting a pointer on external data for array nodelist_of_poly(:,:)
  INTERFACE set_ds_nodelist_of_poly_ref
     MODULE PROCEDURE set_ds_nodelist_of_poly_ref_d
  END INTERFACE
  !! setting a pointer on external data for array center_coord(:,:)
  INTERFACE set_ds_center_coord_ref
     MODULE PROCEDURE set_ds_center_coord_ref_d
  END INTERFACE
  !! setting a pointer on external data for array grav_center_coord(:,:)
  INTERFACE set_ds_grav_center_coord_ref
     MODULE PROCEDURE set_ds_grav_center_coord_ref_d
  END INTERFACE
  !! setting a pointer on external data for array node_coord(:,:)
  INTERFACE set_ds_node_coord_ref
     MODULE PROCEDURE set_ds_node_coord_ref_d
  END INTERFACE
  !! setting a pointer on external data for array poly_depth(:)
  INTERFACE set_ds_poly_depth_ref
     MODULE PROCEDURE set_ds_poly_depth_ref_d
  END INTERFACE
  !! setting a pointer on external data for array edge_water_depth(:)
  INTERFACE set_ds_edge_water_depth_ref
     MODULE PROCEDURE set_ds_edge_water_depth_ref_d
  END INTERFACE
  !! setting a pointer on external data for array node_water_depth(:)
  INTERFACE set_ds_node_water_depth_ref
     MODULE PROCEDURE set_ds_node_water_depth_ref_d
  END INTERFACE
  !! setting a pointer on external data for array node_noero_depth(:)
  INTERFACE set_ds_node_noero_depth_ref
     MODULE PROCEDURE set_ds_node_noero_depth_ref_d
  END INTERFACE
  !! setting a pointer on external data for array poly_area(:)
  INTERFACE set_ds_poly_area_ref
     MODULE PROCEDURE set_ds_poly_area_ref_d
  END INTERFACE
  !! setting a pointer on external data for array node_area(:)
  INTERFACE set_ds_node_area_ref
     MODULE PROCEDURE set_ds_node_area_ref_d
  END INTERFACE
  !! setting a pointer on external data for array knolg(:)
  INTERFACE set_ds_knolg_ref
     MODULE PROCEDURE set_ds_knolg_ref_i
  END INTERFACE
  !! setting a pointer on external data for array node_neighb(:)
  INTERFACE set_ds_node_neighb_ref
     MODULE PROCEDURE set_ds_node_neighb_ref_i
  END INTERFACE
  !! setting the number of shared points
  INTERFACE set_ds_nb_neighb_pt_ref
     MODULE PROCEDURE set_ds_nb_neighb_pt_ref_i
  END INTERFACE
  !! setting the list send
  INTERFACE set_ds_list_send_ref
     MODULE PROCEDURE set_ds_list_send_ref_i
  END INTERFACE
  !! setting the global number of shared points
  INTERFACE set_ds_nh_com_ref
     MODULE PROCEDURE set_ds_nh_com_ref_i
  END INTERFACE
  !! setting the buffer send
  INTERFACE set_ds_BUF_SEND_ref
     MODULE PROCEDURE set_ds_buf_send_ref_d
  END INTERFACE
  !! setting the buffer received
  INTERFACE set_ds_BUF_RECV_ref
     MODULE PROCEDURE set_ds_buf_recv_ref_d
  END INTERFACE
  !! setting a pointer on external data for array node_depth(:)
  INTERFACE set_ds_node_depth_ref
     MODULE PROCEDURE set_ds_node_depth_ref_d
  END INTERFACE
  !! setting a pointer on external data for array edge_depth(:)
  INTERFACE set_ds_edge_depth_ref
     MODULE PROCEDURE set_ds_edge_depth_ref_d
  END INTERFACE
  !! setting a pointer on external data for array node_porosity(:)
  INTERFACE set_ds_node_porosity_ref
     MODULE PROCEDURE set_ds_node_porosity_ref_d
  END INTERFACE
  !! setting a pointer on external data for array cell_sediment_fraction(:,:)
  INTERFACE set_ds_cell_sediment_frac_ref
     MODULE PROCEDURE set_ds_cell_sediment_frac_ref_d
  END INTERFACE
  !! setting a pointer on external data for array node_sediment_fraction(:,:)
  INTERFACE set_ds_node_sediment_frac_ref
     MODULE PROCEDURE set_ds_node_sediment_frac_ref_d
  END INTERFACE
  !
  !! transfering and saving of current values in array dredge_poly_index(:,:)
  INTERFACE store_ds_dredge_poly_index
     MODULE PROCEDURE store_ds_dredge_poly_index_d
  END INTERFACE
  !! transfering and saving of current values in array dredge_node_index(:,:)
  INTERFACE store_ds_dredge_node_index
     MODULE PROCEDURE store_ds_dredge_node_index_d
  END INTERFACE
  !! transfering and saving of current values in array dispose_poly_index(:,:)
  INTERFACE store_ds_dispose_poly_index
     MODULE PROCEDURE store_ds_dispose_poly_index_d
  END INTERFACE
  !! transfering and saving of current values in array dispose_node_index(:,:,:)
  INTERFACE store_ds_dispose_node_index
     MODULE PROCEDURE store_ds_dispose_node_index_d
  END INTERFACE
  !! transfering and saving of current values in array dredge_poly_index_tc(:,:)
  INTERFACE store_ds_dredge_poly_index_tc
     MODULE PROCEDURE store_ds_dredge_poly_index_tc_d
  END INTERFACE
  !! transfering and saving of current values in array dredge_node_index_tc(:,:)
  INTERFACE store_ds_dredge_node_index_tc
     MODULE PROCEDURE store_ds_dredge_node_index_tc_d
  END INTERFACE
  !! transfering and saving of current values in array dispose_poly_index_tc(:,:)
  INTERFACE store_ds_dispose_poly_index_tc
     MODULE PROCEDURE store_ds_dispose_poly_index_tc_d
  END INTERFACE
  !! transfering and saving of current values in array dispose_node_index_tc(:,:)
  INTERFACE store_ds_dispose_node_index_tc
     MODULE PROCEDURE store_ds_dispose_node_index_tc_d
  END INTERFACE
  !! transfering and saving of current values in array art_bed_load_poly_index(:,:)
  INTERFACE store_ds_art_bed_load_pol_index
     MODULE PROCEDURE store_ds_art_bed_load_pol_ind_d
  END INTERFACE
  !! transfering and saving of current values in array art_bed_load_node_index(:,:)
  INTERFACE store_ds_art_bed_load_nod_index
     MODULE PROCEDURE store_ds_art_bed_load_nod_ind_d
  END INTERFACE
  !! transfering and saving of current values in array fraction_name(:)
  INTERFACE store_ds_fraction_name
     MODULE PROCEDURE store_ds_fraction_name_d
  END INTERFACE
  !
  !! determine the number of elements/polygons (centers)
  INTERFACE get_nof_poly
     MODULE PROCEDURE get_nof_poly_d
  END INTERFACE
  !! determine the number of nodes
  INTERFACE get_nof_nodes
     MODULE PROCEDURE get_nof_nodes_d
  END INTERFACE
  !! determine the number of edges
  INTERFACE get_nof_edge
     MODULE PROCEDURE get_nof_edge_d
  END INTERFACE
  !! determine the number of sediment fractions
  INTERFACE get_nof_sediment_fraction
     MODULE PROCEDURE get_nof_sediment_fraction_d
  END INTERFACE
  !! determine the number of dredge polygons
  INTERFACE get_nof_dredge_poly
     MODULE PROCEDURE get_nof_dredge_poly_d
  END INTERFACE
  !! allocating the two-dimensional CHARACTER-array "dispose_poly_name"
  INTERFACE alloc_dispose_poly_name
     MODULE PROCEDURE alloc_dispose_poly_name_d
  END INTERFACE
  !! allocating the two-dimensional CHARACTER-array "list_of_disp_polys
  INTERFACE alloc_list_of_disp_polys
     MODULE PROCEDURE alloc_list_of_disp_polys_d
  END INTERFACE
  !! allocating the one-dimensional CHARACTER-array "dredge_poly_name_tc"
  INTERFACE alloc_dredge_poly_name_tc
     MODULE PROCEDURE alloc_dredge_poly_name_tc_d
  END INTERFACE
  !! allocating the one-dimensional CHARACTER-array "dispose_poly_name_tc"
  INTERFACE alloc_dispose_poly_name_tc
     MODULE PROCEDURE alloc_dispose_poly_name_tc_d
  END INTERFACE
  !! allocating the one-dimensional CHARACTER-array "predef_disp_poly_name"
  INTERFACE alloc_predef_disp_poly_name
     MODULE PROCEDURE alloc_predef_disp_poly_name_d
  END INTERFACE
  !! allocating the one-dimensional CHARACTER-array "predef_disp_sed_class"
  INTERFACE alloc_predef_disp_sed_class
     MODULE PROCEDURE alloc_predef_disp_sed_class_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "dredging_rate"
  INTERFACE alloc_dredging_rate
     MODULE PROCEDURE alloc_dredging_rate_d
  END INTERFACE
  !!  allocating the one-dimensional REAL(Double)-array "minimum_volume"
  INTERFACE alloc_minimum_volume
     MODULE PROCEDURE alloc_minimum_volume_d
  END INTERFACE
  !!  allocating the one-dimensional REAL(Double)-array "sector_radius"
  INTERFACE alloc_sector_radius
     MODULE PROCEDURE alloc_sector_radius_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "disposal_rate"
  INTERFACE alloc_disposal_rate
     MODULE PROCEDURE alloc_disposal_rate_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "dispose_weighting_factor"
  INTERFACE alloc_dispose_weighting_fac
     MODULE PROCEDURE alloc_dispose_weighting_fac_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "predef_disp_sed_vol"
  INTERFACE alloc_predef_disp_sed_vol
     MODULE PROCEDURE alloc_predef_disp_sed_vol_d
  END INTERFACE
  !! allocating the one-dimensional CHARACTER-array "ini_obs_time"
  INTERFACE alloc_ini_obs_time
     MODULE PROCEDURE alloc_ini_obs_time_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "observing_period"
  INTERFACE alloc_observing_period
     MODULE PROCEDURE alloc_observing_period_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "limiting_discharge"
  INTERFACE alloc_limiting_discharge
     MODULE PROCEDURE alloc_limiting_discharge_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "navigation_possible"
  INTERFACE alloc_navigation_possible
     MODULE PROCEDURE alloc_navigation_possible_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "dredge_sed_vol_tc"
  INTERFACE alloc_dredge_sed_vol_tc
     MODULE PROCEDURE alloc_dredge_sed_vol_tc_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "dispose_sed_vol_tc"
  INTERFACE alloc_dispose_sed_vol_tc
     MODULE PROCEDURE alloc_dispose_sed_vol_tc_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "predef_sed_distrib"
  INTERFACE alloc_predef_sed_distrib
     MODULE PROCEDURE alloc_predef_sed_distrib_d
  END INTERFACE
  !! allocating the two-dimensional CHARACTER-array "predef_dredge_time_tc"
  INTERFACE alloc_predef_dredge_time_tc
     MODULE PROCEDURE alloc_predef_dredge_time_tc_d
  END INTERFACE 
  !! allocating the two-dimensional CHARACTER-array "predef_disp_time_tc"
  INTERFACE alloc_predef_disp_time_tc
     MODULE PROCEDURE alloc_predef_disp_time_tc_d
  END INTERFACE 
  !! allocating the two-dimensional CHARACTER-array "predef_depos_time"
  INTERFACE alloc_predef_depos_time
     MODULE PROCEDURE alloc_predef_depos_time_d
  END INTERFACE 
  !! allocating the one-dimensional CHARACTER-array "disp_scours_auto"
  INTERFACE alloc_disp_scours_auto
     MODULE PROCEDURE alloc_disp_scours_auto_d
  END INTERFACE  
  !! allocating the one-dimensional CHARACTER-array "disp_scours_tc"
  INTERFACE alloc_disp_scours_tc
     MODULE PROCEDURE alloc_disp_scours_tc_d
  END INTERFACE  
  !! allocating the one-dimensional CHARACTER-array "disp_scours_abl"
  INTERFACE alloc_disp_scours_abl
     MODULE PROCEDURE alloc_disp_scours_abl_d
  END INTERFACE  
  !LEO new interface
  INTERFACE check_allocated_disp_scours
      MODULE PROCEDURE  check_allocated_disp_scours_d
  END INTERFACE  
  !
  !
  ! [C.2] publishing the interfaces
  !
  PUBLIC :: init_dredgesim_data      
  PUBLIC :: clear_dredgesim_data    
  PUBLIC :: alloc_dredgesim_start     
  PUBLIC :: alloc_dredgesim_prepare   
  PUBLIC :: dealloc_dredgesim_data    
  PUBLIC :: print_dredgesim_shape    
  PUBLIC :: print_dredgesim_data     
  PUBLIC :: set_ds_nodes_of_poly_ref
  PUBLIC :: set_ds_edgelist_of_poly_ref
  PUBLIC :: set_ds_nodelist_of_poly_ref
  PUBLIC :: set_ds_center_coord_ref
  PUBLIC :: set_ds_grav_center_coord_ref
  PUBLIC :: set_ds_node_coord_ref
  PUBLIC :: set_ds_poly_depth_ref
  PUBLIC :: set_ds_poly_area_ref
  PUBLIC :: set_ds_node_area_ref
  PUBLIC :: set_ds_knolg_ref
  PUBLIC :: set_ds_node_neighb_ref
  PUBLIC :: set_ds_nb_neighb_pt_ref 
  PUBLIC :: set_ds_list_send_ref         
  PUBLIC :: set_ds_nh_com_ref            
  PUBLIC :: set_ds_buf_recv_ref          
  PUBLIC :: set_ds_buf_send_ref
  PUBLIC :: set_ds_node_depth_ref
  PUBLIC :: set_ds_edge_depth_ref
  PUBLIC :: set_ds_node_porosity_ref
  PUBLIC :: set_ds_cell_sediment_frac_ref
  PUBLIC :: set_ds_node_sediment_frac_ref
  PUBLIC :: set_ds_edge_water_depth_ref
  PUBLIC :: set_ds_node_water_depth_ref
  PUBLIC :: set_ds_node_noero_depth_ref
  PUBLIC :: store_ds_dredge_poly_index
  PUBLIC :: store_ds_dredge_node_index
  PUBLIC :: store_ds_dispose_poly_index
  PUBLIC :: store_ds_dispose_node_index
  PUBLIC :: store_ds_dredge_poly_index_tc
  PUBLIC :: store_ds_dredge_node_index_tc
  PUBLIC :: store_ds_dispose_poly_index_tc
  PUBLIC :: store_ds_dispose_node_index_tc
  PUBLIC :: store_ds_art_bed_load_pol_index
  PUBLIC :: store_ds_art_bed_load_nod_index
  PUBLIC :: store_ds_fraction_name
  PUBLIC :: get_nof_poly
  PUBLIC :: get_nof_nodes
  PUBLIC :: get_nof_edge
  PUBLIC :: get_nof_sediment_fraction
  PUBLIC :: get_nof_dredge_poly
  PUBLIC :: alloc_dispose_poly_name
  PUBLIC :: alloc_list_of_disp_polys
  PUBLIC :: alloc_dredge_poly_name_tc
  PUBLIC :: alloc_dispose_poly_name_tc
  PUBLIC :: alloc_predef_disp_poly_name
  PUBLIC :: alloc_predef_disp_sed_class
  PUBLIC :: alloc_dredging_rate
  PUBLIC :: alloc_minimum_volume
  PUBLIC :: alloc_sector_radius
  PUBLIC :: alloc_disposal_rate
  PUBLIC :: alloc_dispose_weighting_fac
  PUBLIC :: alloc_predef_disp_sed_vol
  PUBLIC :: alloc_ini_obs_time
  PUBLIC :: alloc_ini_time_to_observe
  PUBLIC :: alloc_observing_period
  PUBLIC :: alloc_limiting_discharge
  PUBLIC :: alloc_navigation_possible
  PUBLIC :: alloc_dredge_sed_vol_tc
  PUBLIC :: alloc_dispose_sed_vol_tc
  PUBLIC :: alloc_predef_sed_distrib
  PUBLIC :: alloc_predef_dredge_time_tc
  PUBLIC :: alloc_predef_disp_time_tc
  PUBLIC :: alloc_predef_depos_time
  PUBLIC :: alloc_disp_scours_auto
  PUBLIC :: alloc_disp_scours_tc
  PUBLIC :: alloc_disp_scours_abl
  PUBLIC :: alloc_start_dredge_time
  PUBLIC :: alloc_end_dredge_time
  PUBLIC :: alloc_time_to_observe
  PUBLIC :: alloc_old_time_to_observe
  PUBLIC :: alloc_dredge_time_tc
  PUBLIC :: alloc_disposal_time_tc
  PUBLIC :: alloc_art_bl_time
  PUBLIC :: alloc_used_sediment_classes
  !LEO new interface
  PUBLIC :: check_allocated_disp_scours
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data types, internal data and methods (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] local type definitions
  ! [D.2] constant values (parameters)
  !
  !! name of the module
  CHARACTER (LEN=16) , PRIVATE, parameter :: c_modname='m_dredgesim_data' ! 
  !! length of "l_char" in some internal program units
  INTEGER            , PRIVATE, parameter :: c_len_char=40 ! 
  !! number of data sets to be printed (per dimension)
  INTEGER            , PRIVATE, parameter :: c_contents=10 ! 
  !
  ! [D.3] local interfaces
  !
  !! print the message that an array was not allocated
  INTERFACE print_array_not_alloc
     MODULE PROCEDURE print_array_not_alloc_d
  END INTERFACE
  !! print information about size of an array 
  !! a) one-dimensional CHARACTER-array     
  !! b) two-dimensional CHARACTER-array    
  !! c) one_dimensional INTEGER-array       
  !! d) two-dimensional INTEGER-array      
  !! e) one-dimensional REAL(Double)-array  
  !! f) two-dimensional REAL(Double)-array 
  !! g) three-dimensional REAL(Double)-array 
  !! h) one-dimensional FILE-array          
  !! i) one-dimensional CRITERION-array
  INTERFACE print_array_shape
     MODULE PROCEDURE print_array_shape_ch_1
     MODULE PROCEDURE print_array_shape_ch_2
     MODULE PROCEDURE print_array_shape_in_1
     MODULE PROCEDURE print_array_shape_in_2
     MODULE PROCEDURE print_array_shape_in_3
     MODULE PROCEDURE print_array_shape_dp_1
     MODULE PROCEDURE print_array_shape_dp_2
     MODULE PROCEDURE print_array_shape_dp_3
     MODULE PROCEDURE print_array_shape_fi_1
  END INTERFACE
  !! print selected information about contents of an array 
  !! a) one-dimensional CHARACTER-array     
  !! b) two-dimensional CHARACTER-array    
  !! c) ein_dimensionales INTEGER-array       
  !! d) two-dimensional INTEGER-array      
  !! e) one-dimensional REAL(Double)-array  
  !! f) two-dimensional REAL(Double)-array 
  !! g) three-dimensional REAL(Double)-array 
  !! h) one-dimensional FILE-array
  INTERFACE print_array_contents
     MODULE PROCEDURE print_array_contents_ch_1
     MODULE PROCEDURE print_array_contents_ch_2
     MODULE PROCEDURE print_array_contents_in_1
     MODULE PROCEDURE print_array_contents_in_2
     MODULE PROCEDURE print_array_contents_in_3
     MODULE PROCEDURE print_array_contents_dp_1
     MODULE PROCEDURE print_array_contents_dp_2
     MODULE PROCEDURE print_array_contents_dp_3
     MODULE PROCEDURE print_array_contents_fi_1
  END INTERFACE
  !
  !! allocating the one-dimensional REAL(Double)-array "poly_water_depth"
  INTERFACE alloc_poly_water_depth
     MODULE PROCEDURE alloc_poly_water_depth_d
  END INTERFACE
  !! allocating the one-dimensional INTEGER-array "dredge_crit_type"
  INTERFACE alloc_dredge_crit_type
     MODULE PROCEDURE alloc_dredge_crit_type_d
  END INTERFACE
  !! allocating the two-dimensional INTEGER-array "dredge_poly_index"
  INTERFACE alloc_dredge_poly_index
     MODULE PROCEDURE alloc_dredge_poly_index_d
  END INTERFACE  
  !! allocating the two-dimensional INTEGER-array "dredge_node_index"
  INTERFACE alloc_dredge_node_index
     MODULE PROCEDURE alloc_dredge_node_index_d
  END INTERFACE  
  !! allocating the two-dimensional INTEGER-array "dispose_poly_index"
  INTERFACE alloc_dispose_poly_index
     MODULE PROCEDURE alloc_dispose_poly_index_d
  END INTERFACE
  !! allocating the three-dimensional INTEGER-array "dispose_node_index"
  INTERFACE alloc_dispose_node_index
     MODULE PROCEDURE alloc_dispose_node_index_d
  END INTERFACE
  !! allocating the two-dimensional INTEGER-array "dredge_poly_index_tc"
  INTERFACE alloc_dredge_poly_index_tc
     MODULE PROCEDURE alloc_dredge_poly_index_tc_d
  END INTERFACE
  !! allocating the two-dimensional INTEGER-array "dredge_node_index_tc"
  INTERFACE alloc_dredge_node_index_tc
     MODULE PROCEDURE alloc_dredge_node_index_tc_d
  END INTERFACE
  !! allocating the two-dimensional INTEGER-array "dispose_poly_index_tc"
  INTERFACE alloc_dispose_poly_index_tc
     MODULE PROCEDURE alloc_dispose_poly_index_tc_d
  END INTERFACE
  !! allocating the two-dimensional INTEGER-array "dispose_node_index_tc"
  INTERFACE alloc_dispose_node_index_tc
     MODULE PROCEDURE alloc_dispose_node_index_tc_d
  END INTERFACE
  !! allocating the two-dimensional INTEGER-array "art_bed_load_poly_index"
  INTERFACE alloc_art_bed_load_poly_index
     MODULE PROCEDURE alloc_art_bed_load_poly_index_d
  END INTERFACE
  !! allocating the two-dimensional INTEGER-array "art_bed_load_node_index"
  INTERFACE alloc_art_bed_load_node_index
     MODULE PROCEDURE alloc_art_bed_load_node_index_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "poly_total_volume"
  INTERFACE alloc_poly_total_volume
     MODULE PROCEDURE alloc_poly_total_volume_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "node_total_volume"
  INTERFACE alloc_node_total_volume
     MODULE PROCEDURE alloc_node_total_volume_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "disp_node_total_volume"
  INTERFACE alloc_disp_node_total_volume
     MODULE PROCEDURE alloc_disp_node_total_volume_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "total_volume"
  INTERFACE alloc_total_volume
     MODULE PROCEDURE alloc_total_volume_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "all_total_volume"
  INTERFACE alloc_all_total_volume
     MODULE PROCEDURE alloc_all_total_volume_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "all_total_volume_old"
  INTERFACE alloc_all_total_volume_old
     MODULE PROCEDURE alloc_all_total_volume_old_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "poly_sediment_volume"
  INTERFACE alloc_poly_sediment_volume
     MODULE PROCEDURE alloc_poly_sediment_volume_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "node_sediment_volume"
  INTERFACE alloc_node_sediment_volume
     MODULE PROCEDURE alloc_node_sediment_volume_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "node_sediment_volume"
  INTERFACE alloc_node_sediment_volume_rs
     MODULE PROCEDURE alloc_node_sediment_volume_rs_d
  END INTERFACE
  !! allocating the one-dimensional CHARACTER-array "last_obs_time_rs"
  INTERFACE alloc_last_obs_time_rs
     MODULE PROCEDURE alloc_last_obs_time_rs_d
  END INTERFACE
  !! allocating the one-dimensional CHARACTER-array "next_obs_time_rs"
  INTERFACE alloc_next_obs_time_rs
     MODULE PROCEDURE alloc_next_obs_time_rs_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "sediment_volume"
  INTERFACE alloc_sediment_volume
     MODULE PROCEDURE alloc_sediment_volume_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "all_sediment_volume"
  INTERFACE alloc_all_sediment_volume
     MODULE PROCEDURE alloc_all_sediment_volume_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "all_sediment_volume_old"
  INTERFACE alloc_all_sediment_volume_old
     MODULE PROCEDURE alloc_all_sediment_volume_old_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "node_sediment_volume_tc"
  INTERFACE alloc_node_sediment_volume_tc
     MODULE PROCEDURE alloc_node_sediment_volume_tc_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "sediment_volume_tc"
  INTERFACE alloc_sediment_volume_tc
     MODULE PROCEDURE alloc_sediment_volume_tc_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "poly_water_volume"
  INTERFACE alloc_poly_water_volume
     MODULE PROCEDURE alloc_poly_water_volume_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "node_water_volume"
  INTERFACE alloc_node_water_volume
     MODULE PROCEDURE alloc_node_water_volume_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "water_volume"
  INTERFACE alloc_water_volume
     MODULE PROCEDURE alloc_water_volume_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "all_water_volume_old"
  INTERFACE alloc_all_water_volume_old
     MODULE PROCEDURE alloc_all_water_volume_old_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "all_water_volume"
  INTERFACE alloc_all_water_volume
     MODULE PROCEDURE alloc_all_water_volume_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "poly_fraction_volume"
  INTERFACE alloc_poly_fraction_volume
     MODULE PROCEDURE alloc_poly_fraction_volume_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "node_fraction_volume"
  INTERFACE alloc_node_fraction_volume
     MODULE PROCEDURE alloc_node_fraction_volume_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "node_fraction_volume_rs"
  INTERFACE alloc_node_fraction_volume_rs
     MODULE PROCEDURE alloc_node_fraction_volume_rs_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "fraction_volume"
  INTERFACE alloc_fraction_volume
     MODULE PROCEDURE alloc_fraction_volume_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "all_fraction_volume"
  INTERFACE alloc_all_fraction_volume
     MODULE PROCEDURE alloc_all_fraction_volume_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "all_fraction_volume_old"
  INTERFACE alloc_all_fraction_volume_old
     MODULE PROCEDURE alloc_all_fraction_volume_old_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "poly_fraction_volume_tc"
  INTERFACE alloc_poly_fraction_volume_tc
     MODULE PROCEDURE alloc_poly_fraction_volume_tc_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "node_fraction_volume_tc"
  INTERFACE alloc_node_fraction_volume_tc
     MODULE PROCEDURE alloc_node_fraction_volume_tc_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "fraction_volume_tc"
  INTERFACE alloc_fraction_volume_tc
     MODULE PROCEDURE alloc_fraction_volume_tc_d
  END INTERFACE
  !! allocating the one-dimensional LOGICAL-array "upd_out_volumes"
  INTERFACE alloc_upd_out_volumes
     MODULE PROCEDURE alloc_upd_out_volumes_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "aim_poly_depth"
  INTERFACE alloc_aim_poly_depth
     MODULE PROCEDURE alloc_aim_poly_depth_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "aim_node_depth"
  INTERFACE alloc_aim_node_depth
     MODULE PROCEDURE alloc_aim_node_depth_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "aim_node_depth_tc"
  INTERFACE alloc_aim_node_depth_tc
     MODULE PROCEDURE alloc_aim_node_depth_tc_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "aim_node_depth_abl"
  INTERFACE alloc_aim_node_depth_abl
     MODULE PROCEDURE alloc_aim_node_depth_abl_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "delta_dredge_poly_depth"
  INTERFACE alloc_delta_dredge_poly_depth
     MODULE PROCEDURE alloc_delta_dredge_poly_depth_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "delta_dredge_node_depth"
  INTERFACE alloc_delta_dredge_node_depth
     MODULE PROCEDURE alloc_delta_dredge_node_depth_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "delta_disp_poly_depth"
  INTERFACE alloc_delta_disp_poly_depth
     MODULE PROCEDURE alloc_delta_disp_poly_depth_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "delta_disp_node_depth"
  INTERFACE alloc_delta_disp_node_depth
     MODULE PROCEDURE alloc_delta_disp_node_depth_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "disp_poly_depth"
  INTERFACE alloc_disp_poly_depth
     MODULE PROCEDURE alloc_disp_poly_depth_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "disp_node_depth"
  INTERFACE alloc_disp_node_depth
     MODULE PROCEDURE alloc_disp_node_depth_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "dzb_dredge"
  INTERFACE alloc_dzb_dredge
     MODULE PROCEDURE alloc_dzb_dredge_d
  END INTERFACE
  !! allocating the one-dimensional REAL(Double)-array "dzb_disp"
  INTERFACE alloc_dzb_disp
     MODULE PROCEDURE alloc_dzb_disp_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "dredged_sediment_volume_to_disp"
  INTERFACE alloc_sediment_volume_to_disp
     MODULE PROCEDURE alloc_sediment_volume_to_disp_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "dredged_fraction_volume_to_disp"
  INTERFACE alloc_fraction_volume_to_disp
     MODULE PROCEDURE alloc_fraction_volume_to_disp_d
  END INTERFACE  
  !! allocating the two-dimensional REAL(Double)-array "disp_total_volume"
  INTERFACE alloc_disp_total_volume
     MODULE PROCEDURE alloc_disp_total_volume_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "disp_total_volume_old"
  INTERFACE alloc_disp_total_volume_old
     MODULE PROCEDURE alloc_disp_total_volume_old_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "disp_sediment_volume"
  INTERFACE alloc_disp_sediment_volume
     MODULE PROCEDURE alloc_disp_sediment_volume_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "disp_sediment_volume_old"
  INTERFACE alloc_disp_sediment_volume_old
     MODULE PROCEDURE alloc_disp_sediment_vol_old_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "disp_fraction_volume"
  INTERFACE alloc_disp_fraction_volume
     MODULE PROCEDURE alloc_disp_fraction_volume_d
  END INTERFACE
  !! allocating the three-dimensional REAL(Double)-array "disp_fraction_volume_old"
  INTERFACE alloc_disp_fraction_volume_old
     MODULE PROCEDURE alloc_disp_fraction_vol_old_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "delta_disp_node_depth_tc"
  INTERFACE alloc_delta_disp_node_depth_tc
     MODULE PROCEDURE alloc_deltadispnodedepthtc_d
  END INTERFACE
  !! allocating the two-dimensional REAL(Double)-array "delta_node_depth_abl"
  INTERFACE alloc_delta_node_depth_abl
     MODULE PROCEDURE alloc_delta_node_depth_abl_d
  END INTERFACE
  !! allocating the one-dimensional CRITERION-array "dredge_criterion"
  INTERFACe alloc_dredge_criterion
     MODULE PROCEDURE alloc_dredge_criterion_d
  END INTERFACE
  !! allocating the one-dimensionals REAL(Double)-array "edge_related_surface"
  INTERFACE alloc_edge_related_surface
     MODULE PROCEDURE alloc_edge_related_surface_d
  END INTERFACE
  !! allocating the one-dimensionals REAL(Double)-array "edge_weighted_sum"
  INTERFACE alloc_edge_weighted_sum
     MODULE PROCEDURE alloc_edge_weighted_sum_d
  END INTERFACE
  !! allocating the one-dimensionals REAL(Double)-array "node_related_surface"
  INTERFACE alloc_node_related_surface
     MODULE PROCEDURE alloc_node_related_surface_d
  END INTERFACE
  !! allocating the one-dimensionals REAL(Double)-array "node_weighted_sum"
  INTERFACE alloc_node_weighted_sum
     MODULE PROCEDURE alloc_node_weighted_sum_d
  END INTERFACE
  !! allocating the array "start_dredge_time"
  INTERFACE alloc_start_dredge_time
     MODULE PROCEDURE alloc_start_dredge_time_d
  END INTERFACE
  !! allocating the array "end_dredge_time"
  INTERFACE alloc_end_dredge_time
     MODULE PROCEDURE alloc_end_dredge_time_d
  END INTERFACE
  !! allocating the array "time_to_observe"
  INTERFACE alloc_time_to_observe
     MODULE PROCEDURE alloc_time_to_observe_d
  END INTERFACE
  !! allocating the array "old_time_to_observe"
  INTERFACE alloc_old_time_to_observe
     MODULE PROCEDURE alloc_old_time_to_observe_d
  END INTERFACE
  !! allocating the array "initial_time_to_observe"
  INTERFACE alloc_ini_time_to_observe
     MODULE PROCEDURE alloc_ini_time_to_observe_d
  END INTERFACE
  !! allocating the array "dredge_time_tc"
  INTERFACE alloc_dredge_time_tc
     MODULE PROCEDURE alloc_dredge_time_tc_d
  END INTERFACE
  !! allocating the array "disposal_time_tc"
  INTERFACE alloc_disposal_time_tc
     MODULE PROCEDURE alloc_disposal_time_tc_d
  END INTERFACE
  !! allocating the array "art_bl_time"
  INTERFACE alloc_art_bl_time
     MODULE PROCEDURE alloc_art_bl_time_d
  END INTERFACE  
  !! allocating the array "used_sediment_classes"
  INTERFACE alloc_used_sediment_classes
     MODULE PROCEDURE alloc_used_sediment_classes_d
  END INTERFACE
  !! allocating the one-dimensional CHARACTER-array "fraction_name"
  INTERFACE alloc_fraction_name
     MODULE PROCEDURE alloc_fraction_name_d
  END INTERFACE
  !
  !! deallocating the one-dimensional REAL(Double)-array "poly_water_depth"
  INTERFACE dealloc_poly_water_depth
     MODULE PROCEDURe dealloc_poly_water_depth_d
  END INTERFACE
  !! deallocating the one-dimensional INTEGER-array "dredge_crit_type"
  INTERFACE dealloc_dredge_crit_type
     MODULE PROCEDURe dealloc_dredge_crit_type_d
  END INTERFACE
  !! deallocating the two-dimensional INTEGER-array "dredge_poly_index"
  INTERFACE dealloc_dredge_poly_index
     MODULE PROCEDURe dealloc_dredge_poly_index_d
  END INTERFACE
  !! deallocating the two-dimensional INTEGER-array "dredge_node_index"
  INTERFACE dealloc_dredge_node_index
     MODULE PROCEDURe dealloc_dredge_node_index_d
  END INTERFACE
  !! deallocating the three-dimensional INTEGER-array "dispose_poly_index"
  INTERFACE dealloc_dispose_poly_index
     MODULE PROCEDURe dealloc_dispose_poly_index_d
  END INTERFACE
  !! deallocating the three-dimensional INTEGER-array "dispose_node_index"
  INTERFACE dealloc_dispose_node_index
     MODULE PROCEDURe dealloc_dispose_node_index_d
  END INTERFACE
  !! deallocating the two-dimensional INTEGER-array "dredge_poly_index_tc"
  INTERFACE dealloc_dredge_poly_index_tc
     MODULE PROCEDURe dealloc_dredge_poly_index_tc_d
  END INTERFACE
  !! deallocating the two-dimensional INTEGER-array "dredge_node_index_tc"
  INTERFACE dealloc_dredge_node_index_tc
     MODULE PROCEDURe dealloc_dredge_node_index_tc_d
  END INTERFACE
  !! deallocating the three-dimensional INTEGER-array "dispose_poly_index_tc"
  INTERFACE dealloc_dispose_poly_index_tc
     MODULE PROCEDURe dealloc_dispose_poly_index_tc_d
  END INTERFACE
  !! deallocating the two-dimensional INTEGER-array "dispose_node_index_tc"
  INTERFACE dealloc_dispose_node_index_tc
     MODULE PROCEDURe dealloc_dispose_node_index_tc_d
  END INTERFACE
  !! deallocating the one-dimensional INTEGER-array "art_bed_load_poly_index"
  INTERFACE dealloc_art_bed_load_poly_index
     MODULE PROCEDURe dealloc_art_bed_load_poly_ind_d
  END INTERFACE
  !! deallocating the one-dimensional INTEGER-array "art_bed_load_node_index"
  INTERFACE dealloc_art_bed_load_node_index
     MODULE PROCEDURe dealloc_art_bed_load_node_ind_d
  END INTERFACE
  !! deallocating the two-dimensional CHARACTER-array "dispose_poly_name"
  INTERFACE dealloc_dispose_poly_name
     MODULE PROCEDURe dealloc_dispose_poly_name_d
  END INTERFACE
  !! deallocating the two-dimensional CHARACTER-array "list_of_disp_polys"
  INTERFACE dealloc_list_of_disp_polys
     MODULE PROCEDURe dealloc_list_of_disp_polys_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "dredge_poly_name_tc"
  INTERFACE dealloc_dredge_poly_name_tc
     MODULE PROCEDURe dealloc_dredge_poly_name_tc_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "dispose_poly_name_tc"
  INTERFACE dealloc_dispose_poly_name_tc
     MODULE PROCEDURe dealloc_dispose_poly_name_tc_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "predef_disp_poly_name"
  INTERFACE dealloc_predef_disp_poly_name
     MODULE PROCEDURe dealloc_predef_disp_poly_name_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "predef_disp_sed_class"
  INTERFACE dealloc_predef_disp_sed_class
     MODULE PROCEDURe dealloc_predef_disp_sed_class_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "dredging_rate"
  INTERFACE dealloc_dredging_rate
     MODULE PROCEDURe dealloc_dredging_rate_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "minimum_volume"
  INTERFACE dealloc_minimum_volume
     MODULE PROCEDURe dealloc_minimum_volume_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "sector_radius"
  INTERFACE dealloc_sector_radius
     MODULE PROCEDURe dealloc_sector_radius_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "disposal_rate"
  INTERFACE dealloc_disposal_rate
     MODULE PROCEDURe dealloc_disposal_rate_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "disp_weighting_factor"
  INTERFACE dealloc_dispose_weighting_fac
     MODULE PROCEDURe dealloc_dispose_weighting_fac_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "predef_disp_sed_vol"
  INTERFACE dealloc_predef_disp_sed_vol
     MODULE PROCEDURe dealloc_predef_disp_sed_vol_d
  END INTERFACE
  !! deallocating the two-dimensional CHARACTER-array "ini_obs_time"
  INTERFACE dealloc_ini_obs_time
     MODULE PROCEDURe dealloc_ini_obs_time_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "observing_period"
  INTERFACE dealloc_observing_period
     MODULE PROCEDURe dealloc_observing_period_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "limiting_discharge"
  INTERFACE dealloc_limiting_discharge
     MODULE PROCEDURe dealloc_limiting_discharge_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "naviagation_possible"
  INTERFACE dealloc_navigation_possible
     MODULE PROCEDURe dealloc_navigation_possible_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "dredge_sed_vol_tc"
  INTERFACE dealloc_dredge_sed_vol_tc
     MODULE PROCEDURe dealloc_dredge_sed_vol_tc_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "dispose_sed_vol_tc"
  INTERFACE dealloc_dispose_sed_vol_tc
     MODULE PROCEDURe dealloc_dispose_sed_vol_tc_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "predef_sed_distrib"
  INTERFACE dealloc_predef_sed_distrib
     MODULE PROCEDURe dealloc_predef_sed_distrib_d
  END INTERFACE  
  !! deallocating the two-dimensional CHARACTER-array "predef_dredge_time_tc"
  INTERFACE dealloc_predef_dredge_time_tc
     MODULE PROCEDURe dealloc_predef_dredge_time_tc_d
  END INTERFACE
  !! deallocating the two-dimensional CHARACTER-array "predef_disp_time_tc"
  INTERFACE dealloc_predef_disp_time_tc
     MODULE PROCEDURe dealloc_predef_disp_time_tc_d
  END INTERFACE
  !! deallocating the two-dimensional CHARACTER-array "predef_depos_time"
  INTERFACE dealloc_predef_depos_time
     MODULE PROCEDURe dealloc_predef_depos_time_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "disp_scours_auto"
  INTERFACE dealloc_disp_scours_auto
     MODULE PROCEDURe dealloc_disp_scours_auto_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "disp_scours_tc"
  INTERFACE dealloc_disp_scours_tc
     MODULE PROCEDURe dealloc_disp_scours_tc_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "disp_scours_abl"
  INTERFACE dealloc_disp_scours_abl
     MODULE PROCEDURe dealloc_disp_scours_abl_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "poly_total_volume"
  INTERFACE dealloc_poly_total_volume
     MODULE PROCEDURe dealloc_poly_total_volume_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "node_total_volume"
  INTERFACE dealloc_node_total_volume
     MODULE PROCEDURe dealloc_node_total_volume_d
  END INTERFACE
  !! deallocating the three-dimensional REAL(Double)-array "disp_node_total_volume"
  INTERFACE dealloc_disp_node_tot_volume
     MODULE PROCEDURe dealloc_disp_node_tot_volume_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "total_volume"
  INTERFACE dealloc_total_volume
     MODULE PROCEDURe dealloc_total_volume_d
  END INTERFACE
   !! deallocating the one-dimensional REAL(Double)-array "all_total_volume"
  INTERFACE dealloc_all_total_volume
     MODULE PROCEDURe dealloc_all_total_volume_d
  END INTERFACE
   !! deallocating the one-dimensional REAL(Double)-array "all_total_volume_old"
  INTERFACE dealloc_all_total_volume_old
     MODULE PROCEDURe dealloc_all_total_volume_old_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "poly_sediment_volume"
  INTERFACE dealloc_poly_sediment_volume
     MODULE PROCEDURe dealloc_poly_sediment_volume_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "node_sediment_volume"
  INTERFACE dealloc_node_sediment_volume
     MODULE PROCEDURe dealloc_node_sediment_volume_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "node_sediment_volume"
  INTERFACE dealloc_node_sediment_volume_rs
     MODULE PROCEDURe dealloc_node_sedi_volume_rs_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "last_obs_time_rs"
  INTERFACE dealloc_last_obs_time_rs
     MODULE PROCEDURe dealloc_last_obs_time_rs_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "next_obs_time_rs"
  INTERFACE dealloc_next_obs_time_rs
     MODULE PROCEDURe dealloc_next_obs_time_rs_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "sediment_volume"
  INTERFACE dealloc_sediment_volume
     MODULE PROCEDURe dealloc_sediment_volume_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "all_sediment_volume"
  INTERFACE dealloc_all_sediment_volume
     MODULE PROCEDURe dealloc_all_sediment_volume_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "all_sediment_volume_old"
  INTERFACE dealloc_all_sediment_volume_old
     MODULE PROCEDURe dealloc_all_sediment_vol_old_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "node_sediment_volume_tc"
  INTERFACE dealloc_node_sediment_volume_tc
     MODULE PROCEDURe dealloc_node_sedi_volume_tc_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "sediment_volume_tc"
  INTERFACE dealloc_sediment_volume_tc
     MODULE PROCEDURe dealloc_sediment_volume_tc_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "poly_water_volume"
  INTERFACE dealloc_poly_water_volume
     MODULE PROCEDURe dealloc_poly_water_volume_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "node_water_volume"
  INTERFACE dealloc_node_water_volume
     MODULE PROCEDURe dealloc_node_water_volume_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "water_volume"
  INTERFACE dealloc_water_volume
     MODULE PROCEDURe dealloc_water_volume_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "all_water_volume"
  INTERFACE dealloc_all_water_volume
     MODULE PROCEDURe dealloc_all_water_volume_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "all_water_volume_old"
  INTERFACE dealloc_all_water_volume_old
     MODULE PROCEDURe dealloc_all_water_volume_old_d
  END INTERFACE
  !! deallocating the three-dimensional REAL(Double)-array "poly_fraction_volume"
  INTERFACE dealloc_poly_fraction_volume
     MODULE PROCEDURe dealloc_poly_fraction_volume_d
  END INTERFACE
  !! deallocating the three-dimensional REAL(Double)-array "node_fraction_volume"
  INTERFACE dealloc_node_fraction_volume
     MODULE PROCEDURe dealloc_node_fraction_volume_d
  END INTERFACE
  !! deallocating the three-dimensional REAL(Double)-array "node_fraction_volume_rs"
  INTERFACE dealloc_node_fraction_volume_rs
     MODULE PROCEDURe dealloc_node_frac_volume_rs_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "fraction_volume"
  INTERFACE dealloc_fraction_volume
     MODULE PROCEDURe dealloc_fraction_volume_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "all_fraction_volume"
  INTERFACE dealloc_all_fraction_volume
     MODULE PROCEDURe dealloc_all_fraction_volume_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "all_fraction_volume_old"
  INTERFACE dealloc_all_fraction_volume_old
     MODULE PROCEDURe dealloc_all_fraction_vol_old_d
  END INTERFACE
  !! deallocating the three-dimensional REAL(Double)-array "poly_fraction_volume_tc"
  INTERFACE dealloc_poly_frac_volume_tc
     MODULE PROCEDURe dealloc_poly_frac_volume_tc_d
  END INTERFACE
  !! deallocating the three-dimensional REAL(Double)-array "node_fraction_volume_tc"
  INTERFACE dealloc_node_frac_volume_tc
     MODULE PROCEDURe dealloc_node_frac_volume_tc_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "fraction_volume_tc"
  INTERFACE dealloc_fraction_volume_tc
     MODULE PROCEDURe dealloc_fraction_volume_tc_d
  END INTERFACE
  !! deallocating the one-dimensional LOGICAL-array "upd_out_volumes"
  INTERFACE dealloc_upd_out_volumes
     MODULE PROCEDURe dealloc_upd_out_volumes_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "aim_poly_depth"
  INTERFACE dealloc_aim_poly_depth
     MODULE PROCEDURe dealloc_aim_poly_depth_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "aim_node_depth"
  INTERFACE dealloc_aim_node_depth
     MODULE PROCEDURe dealloc_aim_node_depth_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "aim_node_depth_tc"
  INTERFACE dealloc_aim_node_depth_tc
     MODULE PROCEDURe dealloc_aim_node_depth_tc_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "aim_node_depth_abl"
  INTERFACE dealloc_aim_node_depth_abl
     MODULE PROCEDURe dealloc_aim_node_depth_abl_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "delta_dredge_poly_depth"
  INTERFACE dealloc_delta_dredge_poly_depth
     MODULE PROCEDURe dealloc_delta_dredge_poldepth_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "delta_dredge_node_depth"
  INTERFACE dealloc_delta_dredge_node_depth
     MODULE PROCEDURe dealloc_delta_dredge_noddepth_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "delta_disp_poly_depth"
  INTERFACE dealloc_delta_disp_poly_depth
     MODULE PROCEDURe dealloc_delta_disp_poldepth_d
  END INTERFACE
  !! deallocating the three-dimensional REAL(Double)-array "delta_disp_node_depth"
  INTERFACE dealloc_delta_disp_node_depth
     MODULE PROCEDURe dealloc_delta_disp_noddepth_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "disp_poly_depth"
  INTERFACE dealloc_disp_poly_depth
     MODULE PROCEDURe dealloc_disp_poly_depth_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "disp_node_depth"
  INTERFACE dealloc_disp_node_depth
     MODULE PROCEDURe dealloc_disp_node_depth_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "dzb_dredge"
  INTERFACE dealloc_dzb_dredge
     MODULE PROCEDURe dealloc_dzb_dredge_d
  END INTERFACE
  !! deallocating the one-dimensional REAL(Double)-array "dzb_disp"
  INTERFACE dealloc_dzb_disp
     MODULE PROCEDURe dealloc_dzb_disp_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "dredged_sediment_volume_to_disp"
  INTERFACE dealloc_sed_volume_to_disp
     MODULE PROCEDURe dealloc_sed_volume_to_disp_d
  END INTERFACE
  !! deallocating the three-dimensional REAL(Double)-array "dredged_fraction_volume_to_disp"
  INTERFACE dealloc_frac_volume_to_disp
     MODULE PROCEDURe dealloc_frac_volume_to_disp_d
  END INTERFACE
   !! deallocating the two-dimensional REAL(Double)-array "disp_total_volume"
  INTERFACE dealloc_disp_total_volume
     MODULE PROCEDURe dealloc_disp_total_volume_d
  END INTERFACE
   !! deallocating the two-dimensional REAL(Double)-array "disp_total_volume_old"
  INTERFACE dealloc_disp_total_volume_old
     MODULE PROCEDURe dealloc_disp_total_volume_old_d
  END INTERFACE
   !! deallocating the two-dimensional REAL(Double)-array "disp_sediment_volume"
  INTERFACE dealloc_disp_sediment_volume
     MODULE PROCEDURe dealloc_disp_sediment_volume_d
  END INTERFACE
   !! deallocating the two-dimensional REAL(Double)-array "disp_sediment_volume_old"
  INTERFACE dealloc_disp_sediment_volume_old
     MODULE PROCEDURe dealloc_disp_sediment_vol_old_d
  END INTERFACE
   !! deallocating the three-dimensional REAL(Double)-array "disp_fraction_volume"
  INTERFACE dealloc_disp_fraction_volume
     MODULE PROCEDURe dealloc_disp_fraction_volume_d
  END INTERFACE
   !! deallocating the three-dimensional REAL(Double)-array "disp_fraction_volume_old"
  INTERFACE dealloc_disp_fraction_volume_old
     MODULE PROCEDURe dealloc_disp_fraction_vol_old_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "delta_disp_node_depth_tc"
  INTERFACE dealloc_delta_disp_nodedepth_tc
     MODULE PROCEDURe dealloc_deltadispnodedepthtc_d
  END INTERFACE
  !! deallocating the two-dimensional REAL(Double)-array "delta_node_depth_abl"
  INTERFACE dealloc_delta_node_depth_abl
     MODULE PROCEDURe dealloc_delta_node_depth_abl_d
  END INTERFACE
  !! deallocating the one-dimensional CRITERION-array "dredge_criterion"
  INTERFACe dealloc_dredge_criterion
     MODULE PROCEDURe dealloc_dredge_criterion_d
  END INTERFACE
  !! deallocating the one-dimensionals REAL(Double)-array "edge_related_surface"
  INTERFACE dealloc_edge_related_surface
     MODULE PROCEDURE dealloc_edge_related_surface_d
  END INTERFACE
  !! deallocating the one-dimensionals REAL(Double)-array "edge_weighted_sum"
  INTERFACE dealloc_edge_weighted_sum
     MODULE PROCEDURE dealloc_edge_weighted_sum_d
  END INTERFACE
  !! deallocating the one-dimensionals REAL(Double)-array "node_related_surface"
  INTERFACE dealloc_node_related_surface
     MODULE PROCEDURE dealloc_node_related_surface_d
  END INTERFACE
  !! deallocating the one-dimensionals REAL(Double)-array "node_weighted_sum"
  INTERFACE dealloc_node_weighted_sum
     MODULE PROCEDURE dealloc_node_weighted_sum_d
  END INTERFACE  
  !! deallocating the array "start_dredge_time"
  INTERFACE dealloc_start_dredge_time
     MODULE PROCEDURE dealloc_start_dredge_time_d
  END INTERFACE
  !! deallocating the array "end_dredge_time"
  INTERFACE dealloc_end_dredge_time
     MODULE PROCEDURE dealloc_end_dredge_time_d
  END INTERFACE
  !! deallocating the array "time_to_observe"
  INTERFACE dealloc_time_to_observe
     MODULE PROCEDURE dealloc_time_to_observe_d
  END INTERFACE
  !! deallocating the array "old_time_to_observe"
  INTERFACE dealloc_old_time_to_observe
     MODULE PROCEDURE dealloc_old_time_to_observe_d
  END INTERFACE
  !! deallocating the array "initial_time_to_observe"
  INTERFACE dealloc_ini_time_to_observe
     MODULE PROCEDURE dealloc_ini_time_to_observe_d
  END INTERFACE
  !! deallocating the array "dredge_time_tc"
  INTERFACE dealloc_dredge_time_tc
     MODULE PROCEDURE dealloc_dredge_time_tc_d
  END INTERFACE
  !! deallocating the array "disposal_time_tc"
  INTERFACE dealloc_disposal_time_tc
     MODULE PROCEDURE dealloc_disposal_time_tc_d
  END INTERFACE
  !! deallocating the array "art_bl_time"
  INTERFACE dealloc_art_bl_time
     MODULE PROCEDURE dealloc_art_bl_time_d
  END INTERFACE
  !! deallocating the array "used_sediment_classes"
  INTERFACE dealloc_used_sediment_classes
     MODULE PROCEDURE dealloc_used_sediment_classes_d
  END INTERFACE
  !! deallocating the one-dimensional CHARACTER-array "fraction_name"
  INTERFACE dealloc_fraction_name
     MODULE PROCEDURe dealloc_fraction_name_d
  END INTERFACE
  !
  !! count dimension (greater than zero) in an array
  INTERFACE get_rank_count
     MODULE PROCEDURE get_rank_count_d
  END INTERFACE
  !! generating an error message if rank of an array does not match
  INTERFACE has_correct_rank
     MODULE PROCEDURE has_correct_rank_d
  END INTERFACE
  !! comparison of two shapes regarding identity
  INTERFACE has_identical_shape
     MODULE PROCEDURE has_identical_shape_d
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
  ! public methods or methods with access through PUBLIC interfaces
  !
  !! initializing all (variables) specific data of this package 
  !! except error messages
  SUBROUTINE init_dredgesim_data_d ( )
    !
    IF ( no_error( ) ) CALL dealloc_dredgesim_data ( )
    !
    IF ( no_error( ) ) CALL new_file ( steering_file )
    IF ( no_error( ) ) CALL new_file ( printer_file  )
    IF ( no_error( ) ) CALL new_file ( trace_file    )
    !
    IF ( no_error( ) ) CALL new_time ( act_timestep )
    IF ( no_error( ) ) CALL new_datetime ( act_time )
    IF ( no_error( ) ) CALL new_datetime ( old_time )
    IF ( no_error( ) ) CALL new_datetime ( end_time )
    IF ( no_error( ) ) CALL new_datetime ( initial_time )
    IF ( no_error( ) ) CALL new_io_info ( input_files )
    IF ( no_error( ) ) CALL new_io_info ( output_files )
    !
    upd_poly_water_depth        = .false.
    upd_dredge_crit_type        = .false.
    upd_dredge_poly_index       = .false.
    upd_dredge_node_index       = .false.
    upd_dispose_poly_index      = .false.
    upd_dispose_node_index      = .false.
    upd_dredge_poly_index_tc    = .false.
    upd_dredge_node_index_tc    = .false.
    upd_dispose_poly_index_tc   = .false.
    upd_dispose_node_index_tc   = .false.
    upd_art_bed_load_poly_index = .false.
    upd_art_bed_load_node_index = .false.
    upd_poly_total_volume       = .false.
    upd_node_total_volume       = .false.
    upd_disp_node_total_volume  = .false.
    upd_total_volume            = .false.
    upd_poly_sediment_volume    = .false.
    upd_node_sediment_volume    = .false.
    upd_node_sediment_volume_rs = .false.
    upd_sediment_volume         = .false.
    upd_node_sediment_volume_tc = .false.
    upd_sediment_volume_tc      = .false.
    upd_poly_water_volume       = .false.
    upd_node_water_volume       = .false.
    upd_water_volume            = .false.
    upd_poly_fraction_volume    = .false.
    upd_node_fraction_volume    = .false.
    upd_node_fraction_volume_rs = .false.
    upd_fraction_volume         = .false.
    upd_poly_fraction_volume_tc = .false.
    upd_node_fraction_volume_tc = .false.
    upd_fraction_volume_tc      = .false.
    !
    disposing_scours = .false.
    nof_iterations_disp_scours = c_undef_in
    max_error_disp_scours = c_undef_dp
    act_depo_depth_disp_scours = c_undef_dp
    min_depo_depth_disp_scours = c_undef_dp
    !
    NULLIFY ( nodes_of_poly )
    NULLIFY ( edgelist_of_poly )
    NULLIFY ( nodelist_of_poly )
    NULLIFY ( center_coord )
    NULLIFY ( grav_center_coord )
    NULLIFY ( node_coord )
    NULLIFY ( poly_depth )
    NULLIFY ( poly_area )
    NULLIFY ( node_area )
    NULLIFY ( knolg )
    NULLIFY ( node_neighb )
    NULLIFY ( nb_neighb_pt )
    NULLIFY ( list_send )
    NULLIFY ( nh_com )
    NULLIFY ( buf_recv )
    NULLIFY ( buf_send )
    NULLIFY ( node_depth )
    NULLIFY ( edge_depth )
    NULLIFY ( node_porosity )
    NULLIFY ( cell_sediment_fraction )
    NULLIFY ( node_sediment_fraction )
    NULLIFY ( edge_water_depth )
    NULLIFY ( node_water_depth )
    NULLIFY ( node_noero_depth )
    !
  END SUBROUTINE init_dredgesim_data_d
  !
  !! de-initializing all (variables) specific data of this package 
  !! except error messages
  SUBROUTINE clear_dredgesim_data_d ( )
    !
    IF ( no_error( ) ) CALL kill_file ( steering_file )
    IF ( no_error( ) ) CALL kill_file ( printer_file  )
    IF ( no_error( ) ) CALL kill_file ( trace_file    )
    !
    IF ( no_error( ) ) CALL kill_time     ( act_timestep )
    IF ( no_error( ) ) CALL kill_datetime ( act_time )
    IF ( no_error( ) ) CALL kill_datetime ( old_time )
    IF ( no_error( ) ) CALL kill_datetime ( end_time )
    IF ( no_error( ) ) CALL kill_datetime ( initial_time )
    IF ( no_error( ) ) CALL kill_io_info  ( input_files )
    IF ( no_error( ) ) CALL kill_io_info  ( output_files )
    !
    IF ( no_error( ) ) CALL init_dredgesim_data ( )
    !
  END SUBROUTINE clear_dredgesim_data_d
  !
  !! allocating all dynamic allocatbale data within start-phase 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredgesim_start_d ( )
    !
    ! >ToDo> ggf. weitere data allokieren
    !
    ! [1] arrays with TARGET-Attribut -------------------------------------
!!$    IF ( no_error( ) ) CALL alloc_<tar_ch_1> ( idim1_<tar_ch_1> )
!!$    IF ( no_error( ) ) CALL alloc_<tar_ch_2> ( idim1_<tar_ch_2>, idim2_<tar_ch_2> )
!!$    IF ( no_error( ) ) CALL alloc_<tar_in_1> ( idim1_<tar_in_1> )
!!$    IF ( no_error( ) ) CALL alloc_<tar_in_2> ( idim1_<tar_in_2>, idim2_<tar_in_2> )
!!$    IF ( no_error( ) ) CALL alloc_<tar_fi_1> ( idim1_<tar_fi_1> )
    ! [2] sonstige arrays ------------------------------------------------
!!$    IF ( no_error( ) ) CALL alloc_<arr_ch_1> ( idim1_<arr_ch_1> )
!!$    IF ( no_error( ) ) CALL alloc_<arr_ch_2> ( idim1_<arr_ch_2>, idim2_<arr_ch_2> )
!!$    IF ( no_error( ) ) CALL alloc_<arr_in_1> ( idim1_<arr_in_1> )
!!$    IF ( no_error( ) ) CALL alloc_<arr_in_2> ( idim1_<arr_in_2>, idim2_<arr_in_2> )
!!$    IF ( no_error( ) ) CALL alloc_<arr_dp_2> ( idim1_<arr_dp_2>, idim2_<arr_dp_2> )
!!$    IF ( no_error( ) ) CALL alloc_<arr_fi_1> ( idim1_<arr_fi_1> )
    !
  END SUBROUTINE alloc_dredgesim_start_d
  !
  !! allocating all dynamic allocatbale data within prepare-phase 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredgesim_prepare_d ( )
    !
    ! [1] arrays with TARGET-attribute -------------------------------------
    IF ( no_error( ) ) CALL alloc_poly_water_depth        ( get_nof_poly( ) )
    IF ( no_error( ) ) CALL alloc_dredge_crit_type        ( get_nof_poly( ) )
    IF ( no_error( ) ) CALL alloc_dredge_poly_index       ( get_nof_poly( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_dredge_node_index       ( get_nof_nodes( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_dispose_poly_index      ( get_nof_poly( ), nof_dispose_poly )
    IF ( no_error( ) ) CALL alloc_dispose_node_index      ( get_nof_nodes( ), get_nof_dredge_poly( ), nof_dispose_poly )
    IF ( no_error( ) ) CALL alloc_dredge_poly_index_tc    ( get_nof_poly( ), nof_dredge_poly_tc )
    IF ( no_error( ) ) CALL alloc_dredge_node_index_tc    ( get_nof_nodes( ), nof_dredge_poly_tc )
    IF ( no_error( ) ) CALL alloc_dispose_poly_index_tc   ( get_nof_poly( ), nof_dispose_poly_tc )
    IF ( no_error( ) ) CALL alloc_dispose_node_index_tc   ( get_nof_nodes( ), nof_dispose_poly_tc )
    IF ( no_error( ) ) CALL alloc_art_bed_load_poly_index ( get_nof_poly( ), nof_predef_disp_poly )
    IF ( no_error( ) ) CALL alloc_art_bed_load_node_index ( get_nof_nodes( ), nof_predef_disp_poly )
    IF ( no_error( ) ) CALL alloc_poly_total_volume       ( get_nof_poly( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_node_total_volume       ( get_nof_nodes( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_disp_node_total_volume  ( get_nof_nodes( ), get_nof_dredge_poly( ), nof_dispose_poly )
    IF ( no_error( ) ) CALL alloc_total_volume            ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_all_total_volume        ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_all_total_volume_old    ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_poly_sediment_volume    ( get_nof_poly( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_node_sediment_volume    ( get_nof_nodes( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_node_sediment_volume_rs ( get_nof_nodes( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_last_obs_time_rs        ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_next_obs_time_rs        ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_sediment_volume         ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_all_sediment_volume     ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_all_sediment_volume_old ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_node_sediment_volume_tc ( get_nof_nodes( ), nof_dredge_poly_tc )
    IF ( no_error( ) ) CALL alloc_sediment_volume_tc      ( nof_dredge_poly_tc )
    IF ( no_error( ) ) CALL alloc_poly_water_volume       ( get_nof_poly( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_node_water_volume       ( get_nof_nodes( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_water_volume            ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_all_water_volume        ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_all_water_volume_old    ( get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_poly_fraction_volume    ( get_nof_poly( ), get_nof_sediment_fraction( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_node_fraction_volume    ( get_nof_nodes( ), get_nof_sediment_fraction( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_node_fraction_volume_rs ( get_nof_nodes( ), get_nof_sediment_fraction( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_poly_fraction_volume_tc ( get_nof_poly( ), get_nof_sediment_fraction( ), nof_dredge_poly_tc )
    IF ( no_error( ) ) CALL alloc_node_fraction_volume_tc ( get_nof_nodes( ), get_nof_sediment_fraction( ), nof_dispose_poly_tc )
    IF ( no_error( ) ) CALL alloc_edge_related_surface    ( get_nof_edge( ) )
    IF ( no_error( ) ) CALL alloc_edge_weighted_sum       ( get_nof_edge( ) )
    IF ( no_error( ) ) CALL alloc_node_related_surface    ( get_nof_nodes( ) )
    IF ( no_error( ) ) CALL alloc_node_weighted_sum       ( get_nof_nodes( ) )
    IF ( no_error( ) ) CALL alloc_fraction_volume         ( get_nof_sediment_fraction( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_all_fraction_volume     ( get_nof_sediment_fraction( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_all_fraction_volume_old ( get_nof_sediment_fraction( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_fraction_volume_tc      ( get_nof_sediment_fraction( ), nof_dredge_poly_tc )
    IF ( no_error( ) ) CALL alloc_upd_out_volumes         ( get_nof_dredge_poly ( ) )
    IF ( no_error( ) ) CALL alloc_aim_poly_depth          ( get_nof_poly( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_aim_node_depth          ( get_nof_nodes( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_aim_node_depth_tc       ( get_nof_nodes( ), nof_dispose_poly_tc )
    IF ( no_error( ) ) CALL alloc_aim_node_depth_abl      ( get_nof_nodes( ), nof_predef_disp_poly )
    IF ( no_error( ) ) CALL alloc_delta_dredge_poly_depth ( get_nof_poly( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_delta_dredge_node_depth ( get_nof_nodes( ), get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_delta_disp_poly_depth   ( get_nof_poly( ), nof_dispose_poly )
    IF ( no_error( ) ) CALL alloc_delta_disp_node_depth   ( get_nof_nodes( ), get_nof_dredge_poly( ), nof_dispose_poly )
    IF ( no_error( ) ) CALL alloc_disp_poly_depth         ( get_nof_poly( ), nof_dispose_poly )
    IF ( no_error( ) ) CALL alloc_disp_node_depth         ( get_nof_nodes( ), get_nof_dredge_poly( ), nof_dispose_poly ) 
    IF ( no_error( ) ) CALL alloc_dzb_dredge              ( get_nof_nodes( ) )
    IF ( no_error( ) ) CALL alloc_dzb_disp                ( get_nof_nodes( ) )
    IF ( no_error( ) ) CALL alloc_sediment_volume_to_disp ( get_nof_dredge_poly( ), nof_dispose_poly )
    IF ( no_error( ) ) CALL alloc_fraction_volume_to_disp ( get_nof_dredge_poly( ), nof_dispose_poly , get_nof_sediment_fraction( ))
    IF ( no_error( ) ) CALL alloc_disp_total_volume       ( nof_dispose_poly , get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_disp_total_volume_old   ( nof_dispose_poly , get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_disp_sediment_volume    ( nof_dispose_poly , get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_disp_sediment_volume_old( nof_dispose_poly , get_nof_dredge_poly( ) )
    IF ( no_error( ) ) CALL alloc_disp_fraction_volume    ( get_nof_sediment_fraction( ), nof_dispose_poly , get_nof_dredge_poly( ))
    IF ( no_error( ) ) CALL alloc_disp_fraction_volume_old( get_nof_sediment_fraction( ), nof_dispose_poly , get_nof_dredge_poly( ))
    IF ( no_error( ) ) CALL alloc_delta_disp_node_depth_tc( get_nof_nodes( ), nof_dispose_poly_tc )
    IF ( no_error( ) ) CALL alloc_delta_node_depth_abl    ( get_nof_nodes( ), nof_predef_disp_poly )
    ! [2] ohter arrays ------------------------------------------------
    IF ( no_error( ) ) CALL alloc_fraction_name           ( get_nof_sediment_fraction( ) )
    IF ( no_error( ) ) CALL alloc_start_dredge_time       ( get_nof_dredge_poly () )
    IF ( no_error( ) ) CALL alloc_end_dredge_time         ( get_nof_dredge_poly () )
    IF ( no_error( ) ) CALL alloc_time_to_observe         ( get_nof_dredge_poly () )
    IF ( no_error( ) ) CALL alloc_old_time_to_observe     ( get_nof_dredge_poly () )
    !
  END SUBROUTINE alloc_dredgesim_prepare_d
  !
  !! deallocating of all dynamic allocatable data 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredgesim_data_d ( )
    !
    ! [1] arrays with TARGET-attribute -------------------------------------
    IF ( no_error( ) ) CALL dealloc_poly_water_depth ( )
    IF ( no_error( ) ) CALL dealloc_dredge_crit_type ( )
    IF ( no_error( ) ) CALL dealloc_dredge_poly_index ( )
    IF ( no_error( ) ) CALL dealloc_dredge_node_index ( )
    IF ( no_error( ) ) CALL dealloc_dispose_poly_index ( )
    IF ( no_error( ) ) CALL dealloc_dispose_node_index ( )
    IF ( no_error( ) ) CALL dealloc_dredge_poly_index_tc ( )
    IF ( no_error( ) ) CALL dealloc_dredge_node_index_tc ( )
    IF ( no_error( ) ) CALL dealloc_dispose_poly_index_tc ( )
    IF ( no_error( ) ) CALL dealloc_dispose_node_index_tc ( )
    IF ( no_error( ) ) CALL dealloc_art_bed_load_poly_index ( )
    IF ( no_error( ) ) CALL dealloc_art_bed_load_node_index ( )
    IF ( no_error( ) ) CALL dealloc_dredging_rate ( )
    IF ( no_error( ) ) CALl dealloc_minimum_volume ( )
    IF ( no_error( ) ) CALl dealloc_sector_radius ( )
    IF ( no_error( ) ) CALL dealloc_disposal_rate ( )
    IF ( no_error( ) ) CALL dealloc_dispose_weighting_fac ( )
    IF ( no_error( ) ) CALL dealloc_predef_disp_sed_vol ( )
    IF ( no_error( ) ) CALL dealloc_ini_obs_time ( )
    IF ( no_error( ) ) CALL dealloc_observing_period ( )
    IF ( no_error( ) ) CALl dealloc_limiting_discharge ( )
    IF ( no_error( ) ) CALl dealloc_navigation_possible ( )
    IF ( no_error( ) ) CALL dealloc_dredge_sed_vol_tc ( )
    IF ( no_error( ) ) CALL dealloc_dispose_sed_vol_tc ( )
    IF ( no_error( ) ) CALL dealloc_predef_sed_distrib ( )
    IF ( no_error( ) ) CALL dealloc_predef_dredge_time_tc ( )
    IF ( no_error( ) ) CALL dealloc_predef_disp_time_tc ( )
    IF ( no_error( ) ) CALL dealloc_predef_depos_time ( )
    IF ( no_error( ) ) CALL dealloc_disp_scours_auto ( )
    IF ( no_error( ) ) CALL dealloc_disp_scours_tc ( )
    IF ( no_error( ) ) CALL dealloc_disp_scours_abl ( )
    IF ( no_error( ) ) CALL dealloc_poly_total_volume ( )
    IF ( no_error( ) ) CALL dealloc_node_total_volume ( )
    IF ( no_error( ) ) CALL dealloc_disp_node_tot_volume ( )
    IF ( no_error( ) ) CALL dealloc_total_volume ( )
    IF ( no_error( ) ) CALL dealloc_all_total_volume ( )
    IF ( no_error( ) ) CALL dealloc_all_total_volume_old ( )
    IF ( no_error( ) ) CALL dealloc_poly_sediment_volume ( )
    IF ( no_error( ) ) CALL dealloc_node_sediment_volume ( )
    IF ( no_error( ) ) CALL dealloc_node_sediment_volume_rs ( )
    IF ( no_error( ) ) CALL dealloc_last_obs_time_rs ( )
    IF ( no_error( ) ) CALL dealloc_next_obs_time_rs ( )
    IF ( no_error( ) ) CALL dealloc_sediment_volume ( )
    IF ( no_error( ) ) CALL dealloc_all_sediment_volume ( )
    IF ( no_error( ) ) CALL dealloc_all_sediment_volume_old ( )
    IF ( no_error( ) ) CALL dealloc_node_sediment_volume_tc ( )
    IF ( no_error( ) ) CALL dealloc_sediment_volume_tc ( )
    IF ( no_error( ) ) CALL dealloc_poly_water_volume ( )
    IF ( no_error( ) ) CALL dealloc_node_water_volume ( )
    IF ( no_error( ) ) CALL dealloc_water_volume ( )
    IF ( no_error( ) ) CALL dealloc_all_water_volume ( )
    IF ( no_error( ) ) CALL dealloc_all_water_volume_old ( )
    IF ( no_error( ) ) CALL dealloc_poly_fraction_volume ( )
    IF ( no_error( ) ) CALL dealloc_node_fraction_volume ( )
    IF ( no_error( ) ) CALL dealloc_node_fraction_volume_rs ( )
    IF ( no_error( ) ) CALL dealloc_poly_frac_volume_tc ( )
    IF ( no_error( ) ) CALL dealloc_node_frac_volume_tc ( )
    IF ( no_error( ) ) CALL dealloc_edge_related_surface ( )
    IF ( no_error( ) ) CALL dealloc_edge_weighted_sum ( )
    IF ( no_error( ) ) CALL dealloc_node_related_surface ( )
    IF ( no_error( ) ) CALL dealloc_node_weighted_sum ( )
    IF ( no_error( ) ) CALL dealloc_fraction_volume ( )
    IF ( no_error( ) ) CALL dealloc_all_fraction_volume ( )
    IF ( no_error( ) ) CALL dealloc_all_fraction_volume_old ( )
    IF ( no_error( ) ) CALL dealloc_fraction_volume_tc ( )
    IF ( no_error( ) ) CALL dealloc_upd_out_volumes ( )
    IF ( no_error( ) ) CALL dealloc_aim_poly_depth ( )
    IF ( no_error( ) ) CALL dealloc_aim_node_depth ( )
    IF ( no_error( ) ) CALL dealloc_aim_node_depth_tc ( )
    IF ( no_error( ) ) CALL dealloc_aim_node_depth_abl ( )
    IF ( no_error( ) ) CALL dealloc_delta_dredge_poly_depth ( )
    IF ( no_error( ) ) CALL dealloc_delta_dredge_node_depth ( )
    IF ( no_error( ) ) CALL dealloc_delta_disp_poly_depth ( )
    IF ( no_error( ) ) CALL dealloc_delta_disp_node_depth ( )
    IF ( no_error( ) ) CALL dealloc_disp_poly_depth ( )
    IF ( no_error( ) ) CALL dealloc_disp_node_depth ( )
    IF ( no_error( ) ) CALL dealloc_dzb_dredge ( )
    IF ( no_error( ) ) CALL dealloc_dzb_disp ( )
    IF ( no_error( ) ) CALL dealloc_sed_volume_to_disp ( )
    IF ( no_error( ) ) CALL dealloc_frac_volume_to_disp ( )
    IF ( no_error( ) ) CALL dealloc_disp_total_volume ( )
    IF ( no_error( ) ) CALL dealloc_disp_total_volume_old ( )
    IF ( no_error( ) ) CALL dealloc_disp_sediment_volume ( )
    IF ( no_error( ) ) CALL dealloc_disp_sediment_volume_old ( )
    IF ( no_error( ) ) CALL dealloc_disp_fraction_volume ( )
    IF ( no_error( ) ) CALL dealloc_disp_fraction_volume_old ( )
    IF ( no_error( ) ) CALL dealloc_delta_disp_nodedepth_tc ( )
    IF ( no_error( ) ) CALL dealloc_delta_node_depth_abl ( )
    ! [2] other arrays ------------------------------------------------
    IF ( no_error( ) ) CALL dealloc_start_dredge_time ( )
    IF ( no_error( ) ) CALL dealloc_end_dredge_time ( )
    IF ( no_error( ) ) CALL dealloc_time_to_observe ( )
    IF ( no_error( ) ) CALL dealloc_old_time_to_observe ( )
    IF ( no_error( ) ) CALL dealloc_ini_time_to_observe ( )
    IF ( no_error( ) ) CALL dealloc_dredge_time_tc ( )
    IF ( no_error( ) ) CALL dealloc_disposal_time_tc ( )
    IF ( no_error( ) ) CALL dealloc_art_bl_time ( )
    IF ( no_error( ) ) CALL dealloc_used_sediment_classes ( )
    IF ( no_error( ) ) CALL dealloc_fraction_name ( )
    IF ( no_error( ) ) CALL dealloc_dredge_criterion ( )
    !
  END SUBROUTINE dealloc_dredgesim_data_d
  !
  !! printing shape of essential data of this package 
  !! subroutine generates error messages
  SUBROUTINE print_dredgesim_shape_d ( )
    !
    ! [1] Pointer-arrays -------------------------------------------------
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(nodes_of_poly) ) THEN
          CALL print_array_shape( nodes_of_poly, 'nodes_of_poly(:)',   & ! 
               'Anzahl der Knoten im Polygon' )
       ELSE
          CALL print_array_not_alloc( 'nodes_of_poly(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(edgelist_of_poly) ) THEN
          CALL print_array_shape( edgelist_of_poly, 'edgelist_of_poly(:,:)',   & ! 
               'Liste der Kantennummern der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'edgelist_of_poly(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(nodelist_of_poly) ) THEN
          CALL print_array_shape( nodelist_of_poly, 'nodelist_of_poly(:,:)',   & ! 
               'Liste der Knotennummern der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'nodelist_of_poly(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(center_coord) ) THEN
          CALL print_array_shape( center_coord, 'center_coord(:,:)',   & ! 
               'Zentrumskoordinaten der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'center_coord(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(grav_center_coord) ) THEN
          CALL print_array_shape( grav_center_coord, 'grav_center_coord(:,:)',   & ! 
               'Schwerpunktkoordinaten der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'grav_center_coord(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_coord) ) THEN
          CALL print_array_shape( node_coord, 'node_coord(:,:)',   & ! 
               'Koordinaten der Knoten des Simulationsgitters' )
       ELSE
          CALL print_array_not_alloc( 'node_coord(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(poly_depth) ) THEN
          CALL print_array_shape( poly_depth, 'poly_depth(:)',   & ! 
               'Tiefe der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'poly_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(edge_water_depth) ) THEN
          CALL print_array_shape( edge_water_depth, 'edge_water_depth(:)',   & ! 
               'Wassertiefe auf Kanten der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'edge_water_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_water_depth) ) THEN
          CALL print_array_shape( node_water_depth, 'node_water_depth(:)',   & ! 
               'Wassertiefe auf Knoten der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'node_water_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_noero_depth) ) THEN
          CALL print_array_shape( node_noero_depth, 'node_noero_depth(:)',   & ! 
               'Nicht erodierbare Tiefe auf Knoten' )
       ELSE
          CALL print_array_not_alloc( 'node_noero_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(poly_area) ) THEN
          CALL print_array_shape( poly_area, 'poly_area(:)',   & ! 
               'Flaeche der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'poly_area(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_area) ) THEN
          CALL print_array_shape( node_area, 'node_area(:)',   & ! 
               'Flaeche der Knoten' )
       ELSE
          CALL print_array_not_alloc( 'node_area(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(knolg) ) THEN
          CALL print_array_shape( knolg, 'knolg(:)',   & ! 
               'nodelist local-global' )
       ELSE
          CALL print_array_not_alloc( 'knolg(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_neighb) ) THEN
          CALL print_array_shape( node_neighb, 'node_neighb(:)',   & ! 
               'node neighbour' )
       ELSE
          CALL print_array_not_alloc( 'node_neighb(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(nb_neighb_pt) ) THEN
          CALL print_array_shape( nb_neighb_pt, 'nb_neighb_pt(:)',   & ! 
               'nb node neighbour pt' )
       ELSE
          CALL print_array_not_alloc( 'nb_neighb_pt(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(list_send) ) THEN
          CALL print_array_shape( list_send, 'list_send(:)',   & ! 
               'list send' )
       ELSE
          CALL print_array_not_alloc( 'list_send(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(nh_com) ) THEN
          CALL print_array_shape( nh_com, 'nh_com(:,B:)',   & ! 
               'nh com' )
       ELSE
          CALL print_array_not_alloc( 'nh_com(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(buf_recv) ) THEN
          CALL print_array_shape( buf_recv, 'buf_recv(:,:)',   & ! 
               'buf_recv' )
       ELSE
          CALL print_array_not_alloc( 'buf_recv(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(buf_send) ) THEN
          CALL print_array_shape( buf_send, 'buf_send(:,:)',   & ! 
               'buf_send' )
       ELSE
          CALL print_array_not_alloc( 'buf_send(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_depth) ) THEN
          CALL print_array_shape( node_depth, 'node_depth(:)',   & ! 
               'Tiefe der Knoten' )
       ELSE
          CALL print_array_not_alloc( 'node_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(edge_depth) ) THEN
          CALL print_array_shape( edge_depth, 'edge_depth(:)',   & ! 
               'Tiefe der Kanten' )
       ELSE
          CALL print_array_not_alloc( 'edge_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_porosity) ) THEN
          CALL print_array_shape( node_porosity, 'node_porosity(:)',   & ! 
               'Porositaet der Knoten' )
       ELSE
          CALL print_array_not_alloc( 'node_porosity(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(cell_sediment_fraction) ) THEN
          CALL print_array_shape( cell_sediment_fraction, 'cell_sediment_fraction(:,:)',   & ! 
               'Sedimentanteil der Fraktionen in den Polygonen' )
       ELSE
          CALL print_array_not_alloc( 'cell_sediment_fraction(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_sediment_fraction) ) THEN
          CALL print_array_shape( node_sediment_fraction, 'node_sediment_fraction(:,:)',   & ! 
               'Sedimentanteil der Fraktionen auf den Knoten' )
       ELSE
          CALL print_array_not_alloc( 'node_sediment_fraction(:,:)' )
       END IF
    END IF
    ! [2] arrays with TARGET-Attribut -------------------------------------
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_water_depth) ) THEN
          CALL print_array_shape( poly_water_depth, 'poly_water_depth(:)',   & ! 
               'Wasserueberdeckung der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'poly_water_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_crit_type) ) THEN
          CALL print_array_shape( dredge_crit_type, 'dredge_crit_type(:)',   & ! 
               'Kriteriumstyp der Polygone' )
       ELSE
          CALL print_array_not_alloc( 'dredge_crit_type(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_poly_index) ) THEN
          CALL print_array_shape( dredge_poly_index, 'dredge_poly_index(:,:)',   & ! 
               'index list mesh element - dredge polygon ' )
       ELSE
          CALL print_array_not_alloc( 'dredge_poly_index(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_node_index) ) THEN
          CALL print_array_shape( dredge_node_index, 'dredge_node_index(:,:)',   & ! 
               'index list mesh node - dredge polygon ' )
       ELSE
          CALL print_array_not_alloc( 'dredge_node_index(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dispose_poly_index) ) THEN
          CALL print_array_shape( dispose_poly_index, 'dispose_poly_index(:,:)',   & ! 
               'index list mesh element - disposal polygon' )
       ELSE
          CALL print_array_not_alloc( 'dispose_poly_index(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dispose_node_index) ) THEN
          CALL print_array_shape( dispose_node_index, 'dispose_node_index(:,:,:)',   & ! 
               'index list mesh node - disposal polygon' )
       ELSE
          CALL print_array_not_alloc( 'dispose_node_index(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_poly_index_tc) ) THEN
          CALL print_array_shape( dredge_poly_index_tc, 'dredge_poly_index_tc(:,:)',   & ! 
               'index list mesh element - dregde polygon time steered' )
       ELSE
          CALL print_array_not_alloc( 'dredge_poly_index_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_node_index_tc) ) THEN
          CALL print_array_shape( dredge_node_index_tc, 'dredge_node_index_tc(:,:)',   & ! 
               'index list mesh element - dredge polygon time steered' )
       ELSE
          CALL print_array_not_alloc( 'dredge_node_index_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dispose_poly_index_tc) ) THEN
          CALL print_array_shape( dispose_poly_index_tc, 'dispose_poly_index_tc(:,:)',   & ! 
               'index list mesh element - disposal polygon time steered' )
       ELSE
          CALL print_array_not_alloc( 'dispose_poly_index_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dispose_node_index_tc) ) THEN
          CALL print_array_shape( dispose_node_index_tc, 'dispose_node_index_tc(:,:)',   & ! 
               'index list mesh element - disposal polygon time steered' )
       ELSE
          CALL print_array_not_alloc( 'dispose_node_index_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(art_bed_load_poly_index) ) THEN
          CALL print_array_shape( art_bed_load_poly_index, 'art_bed_load_poly_index(:,:)',   & ! 
               'index list mesh element - Polygon fuer Geschiebezugabe' )
       ELSE
          CALL print_array_not_alloc( 'art_bed_load_poly_index(:,:)' )
       END IF
    END IF    
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(art_bed_load_node_index) ) THEN
          CALL print_array_shape( art_bed_load_node_index, 'art_bed_load_node_index(:,:)',   & ! 
               'index list mesh node - polygon for artificial bed load supply' )
       ELSE
          CALL print_array_not_alloc( 'art_bed_load_node_index(:,:)' )
       END IF
    END IF    
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_total_volume) ) THEN
          CALL print_array_shape( poly_total_volume, 'poly_total_volume(:,:)', & ! 
               'polygonbezogenes Gesamtvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'poly_total_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_total_volume) ) THEN
          CALL print_array_shape( node_total_volume, 'node_total_volume(:,:)', & ! 
               'knotenbezogenes Gesamtvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'node_total_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_node_total_volume) ) THEN
          CALL print_array_shape( disp_node_total_volume, 'disp_node_total_volume(:,:,:)', & ! 
               'knotenbezogenes Gesamtvolumen fuer alle Verklapppolygone' )
       ELSE
          CALL print_array_not_alloc( 'disp_node_total_volume(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(total_volume) ) THEN
          CALL print_array_shape( total_volume, 'total_volume(:)', & ! 
               'summiertes Gesamtvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'total_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_total_volume) ) THEN
          CALL print_array_shape( all_total_volume, 'all_total_volume(:)', & ! 
               'summiertes Gesamtvolumen fuer alle Baggerpolygone und den gesamten Simulationszeitraum' )
       ELSE
          CALL print_array_not_alloc( 'all_total_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_total_volume_old) ) THEN
          CALL print_array_shape( all_total_volume_old, 'all_total_volume_old(:)', & ! 
               'summiertes Gesamtvolumen fuer alle Baggerpolygone zum letzten Peilzeitpunkt' )
       ELSE
          CALL print_array_not_alloc( 'all_total_volume_old(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_sediment_volume) ) THEN
          CALL print_array_shape( poly_sediment_volume, 'poly_sediment_volume(:,:)', & ! 
               'polygonbezogenes Sedimentvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'poly_sediment_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_sediment_volume) ) THEN
          CALL print_array_shape( node_sediment_volume, 'node_sediment_volume(:,:)', & ! 
               'knotenbezogenes Sedimentvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'node_sediment_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_sediment_volume_rs) ) THEN
          CALL print_array_shape( node_sediment_volume_rs, 'node_sediment_volume_rs(:,:)', & ! 
               'knotenbezogenes Sedimentvolumen fuer alle Baggerpolygone fuer den Restart' )
       ELSE
          CALL print_array_not_alloc( 'node_sediment_volume_rs(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(last_obs_time_rs) ) THEN
          CALL print_array_shape( last_obs_time_rs, 'last_obs_time_rs(:)', & ! 
               'letzter Peilzeitpunkt fuer den Restart' )
       ELSE
          CALL print_array_not_alloc( 'last_obs_time_rs(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(next_obs_time_rs) ) THEN
          CALL print_array_shape( next_obs_time_rs, 'next_obs_time_rs(:)', & ! 
               'naechster Peilzeitpunkt fuer den Restart' )
       ELSE
          CALL print_array_not_alloc( 'next_obs_time_rs(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(sediment_volume) ) THEN
          CALL print_array_shape( sediment_volume, 'sediment_volume(:)', & ! 
               'summiertes Sedimentvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'sediment_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_sediment_volume) ) THEN
          CALL print_array_shape( all_sediment_volume, 'all_sediment_volume(:)', & ! 
               'summiertes Sedimentvolumen fuer alle Baggerpolygone und den gesamten Simulationszeitraum' )
       ELSE
          CALL print_array_not_alloc( 'all_sediment_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_sediment_volume_old) ) THEN
          CALL print_array_shape( all_sediment_volume_old, 'all_sediment_volume_old(:)', & ! 
               'summiertes Sedimentvolumen fuer alle Baggerpolygone zum letzten Peilzeitpunkt' )
       ELSE
          CALL print_array_not_alloc( 'all_sediment_volume_old(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_sediment_volume_tc) ) THEN
          CALL print_array_shape( node_sediment_volume_tc, 'node_sediment_volume_tc(:,:)', & ! 
               'knotenbezogenes Sedimentvolumen fuer alle Baggerpolygone TCM' )
       ELSE
          CALL print_array_not_alloc( 'node_sediment_volume_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(sediment_volume_tc) ) THEN
          CALL print_array_shape( sediment_volume_tc, 'sediment_volume_tc(:)', & ! 
               'summiertes Sedimentvolumen fuer alle Baggerpolygone TCM' )
       ELSE
          CALL print_array_not_alloc( 'sediment_volume_tc(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_water_volume) ) THEN
          CALL print_array_shape( poly_water_volume, 'poly_water_volume(:,:)', & ! 
               'polygonbezogenes Wasservolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'poly_water_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_water_volume) ) THEN
          CALL print_array_shape( node_water_volume, 'node_water_volume(:,:)', & ! 
               'knotenbezogenes Wasservolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'node_water_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(water_volume) ) THEN
          CALL print_array_shape( water_volume, 'water_volume(:)', & ! 
               'summiertes Wasservolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'water_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_water_volume) ) THEN
          CALL print_array_shape( all_water_volume, 'all_water_volume(:)', & ! 
               'summiertes Wasservolumen fuer alle Baggerpolygone und den gesamten Simulationszeitraum' )
       ELSE
          CALL print_array_not_alloc( 'all_water_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_water_volume_old) ) THEN
          CALL print_array_shape( all_water_volume_old, 'all_water_volume_old(:)', & ! 
               'summiertes Wasservolumen fuer alle Baggerpolygone zum letzten Peilzeitpunkt' )
       ELSE
          CALL print_array_not_alloc( 'all_water_volume_old(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_fraction_volume) ) THEN
          CALL print_array_shape( poly_fraction_volume, 'poly_fraction_volume(:,:,:)', & ! 
               'polygon- and fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'poly_fraction_volume(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_fraction_volume) ) THEN
          CALL print_array_shape( node_fraction_volume, 'node_fraction_volume(:,:,:)', & ! 
               'knoten- and fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'node_fraction_volume(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_fraction_volume_rs) ) THEN
          CALL print_array_shape( node_fraction_volume_rs, 'node_fraction_volume_rs(:,:,:)', & ! 
               'knoten- and fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone fuer den Restart' )
       ELSE
          CALL print_array_not_alloc( 'node_fraction_volume_rs(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(fraction_volume) ) THEN
          CALL print_array_shape( fraction_volume, 'fraction_volume(:,:)', & ! 
               'summiertes fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone' )
       ELSE
          CALL print_array_not_alloc( 'fraction_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_fraction_volume) ) THEN
          CALL print_array_shape( all_fraction_volume, 'all_fraction_volume(:,:)', & ! 
               'summiertes fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone und den gesamten Simulationszeitraum' )
       ELSE
          CALL print_array_not_alloc( 'all_fraction_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_fraction_volume_old) ) THEN
          CALL print_array_shape( all_fraction_volume_old, 'all_fraction_volume_old(:,:)', & ! 
               'summiertes fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone zum letzten Pailzeitpunkt' )
       ELSE
          CALL print_array_not_alloc( 'all_fraction_volume_old(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_fraction_volume_tc) ) THEN
          CALL print_array_shape( poly_fraction_volume_tc, 'poly_fraction_volume_tc(:,:,:)', & ! 
               'polygon- and fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone bei zeitgesteuerten Operationen' )
       ELSE
          CALL print_array_not_alloc( 'poly_fraction_volume_tc(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_fraction_volume_tc) ) THEN
          CALL print_array_shape( node_fraction_volume_tc, 'node_fraction_volume_tc(:,:,:)', & ! 
               'knoten- and fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone bei zeitgesteuerten Operationen' )
       ELSE
          CALL print_array_not_alloc( 'node_fraction_volume_tc(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(fraction_volume_tc) ) THEN
          CALL print_array_shape( fraction_volume_tc, 'fraction_volume_tc(:,:)', & ! 
               'summiertes fraktionsbezogenes Sedimentvolumen fuer alle Baggerpolygone bei zeitgesteuerten Operationen' )
       ELSE
          CALL print_array_not_alloc( 'fraction_volume_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(aim_poly_depth) ) THEN
          CALL print_array_shape( aim_poly_depth, 'aim_poly_depth(:,:)', & ! 
               'Zieltiefe, die durch Baggern erreicht werden soll' )
       ELSE
          CALL print_array_not_alloc( 'aim_poly_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(aim_node_depth) ) THEN
          CALL print_array_shape( aim_node_depth, 'aim_node_depth(:,:)', & ! 
               'Zieltiefe, die durch Baggern erreicht werden soll' )
       ELSE
          CALL print_array_not_alloc( 'aim_node_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(aim_node_depth_tc) ) THEN
          CALL print_array_shape( aim_node_depth_tc, 'aim_node_depth_tc(:,:)', & ! 
               'Zieltiefe, die durch zeitgesteurtes Baggern erreicht werden soll' )
       ELSE
          CALL print_array_not_alloc( 'aim_node_depth_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(aim_node_depth_abl) ) THEN
          CALL print_array_shape( aim_node_depth_abl, 'aim_node_depth_abl(:,:)', & ! 
               'Zieltiefe, die durch Geschiebezugabe erreicht werden soll' )
       ELSE
          CALL print_array_not_alloc( 'aim_node_depth_abl(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_dredge_poly_depth) ) THEN
          CALL print_array_shape( delta_dredge_poly_depth, 'delta_dredge_poly_depth(:,:)', & ! 
               'Differenz zwischen Sollbaggertiefe and aktueller Elementtiefe' )
       ELSE
          CALL print_array_not_alloc( 'delta_dredge_poly_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_dredge_node_depth) ) THEN
          CALL print_array_shape( delta_dredge_node_depth, 'delta_dredge_node_depth(:,:)', & ! 
               'Differenz zwischen Sollbaggertiefe and aktueller Knotentiefe' )
       ELSE
          CALL print_array_not_alloc( 'delta_dredge_node_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_disp_poly_depth) ) THEN
          CALL print_array_shape( delta_disp_poly_depth, 'delta_disp_poly_depth(:,:)', & ! 
               'Differenz zwischen maximaler Verklapptiefe and aktueller Elementtiefe' )
       ELSE
          CALL print_array_not_alloc( 'delta_disp_poly_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_disp_node_depth) ) THEN
          CALL print_array_shape( delta_disp_node_depth, 'delta_disp_node_depth(:,:,:)', & ! 
               'Differenz zwischen maximaler Verklapptiefe and aktueller Knotentiefe' )
       ELSE
          CALL print_array_not_alloc( 'delta_disp_node_depth(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_poly_depth) ) THEN
          CALL print_array_shape( disp_poly_depth, 'disp_poly_depth(:,:)', & ! 
               'Resultierende Zieltiefe der Elemente infolge der zu verklappenden Mengen' )
       ELSE
          CALL print_array_not_alloc( 'disp_poly_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_node_depth) ) THEN
          CALL print_array_shape( disp_node_depth, 'disp_node_depth(:,:,:)', & ! 
               'Resultierende Zieltiefe der Knoten infolge der zu verklappenden Mengen' )
       ELSE
          CALL print_array_not_alloc( 'disp_node_depth(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dzb_dredge) ) THEN
          CALL print_array_shape( dzb_dredge, 'dzb_dredge(:)', & ! 
               'Resultierende Sohlhoehenaenderung der Knoten aus Baggern' )
       ELSE
          CALL print_array_not_alloc( 'dzb_dredge(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dzb_disp) ) THEN
          CALL print_array_shape( dzb_disp, 'dzb_disp(:)', & ! 
               'Resultierende Sohlhoehenaenderung der Knoten aus Verklappen' )
       ELSE
          CALL print_array_not_alloc( 'dzb_disp(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredged_sediment_volume_to_disp) ) THEN
          CALL print_array_shape( dredged_sediment_volume_to_disp, 'dredged_sediment_volume_to_disp(:,:)', & ! 
               'Gebaggertes Volumen, das automatisch verklappt werden soll' )
       ELSE
          CALL print_array_not_alloc( 'dredged_sediment_volume_to_disp(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredged_fraction_volume_to_disp) ) THEN
          CALL print_array_shape( dredged_fraction_volume_to_disp, 'dredged_fraction_volume_to_disp(:,:,:)', & ! 
               'Gebaggertes fraktioniertes Volumen, das automatisch verklappt werden soll' )
       ELSE
          CALL print_array_not_alloc( 'dredged_fraction_volume_to_disp(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_total_volume) ) THEN
          CALL print_array_shape( disp_total_volume, 'disp_total_volume(:,:)', & ! 
               'summiertes Gesamtvolumen fuer alle Verklapppolygone pro Baggerpolygon und den gesamten Simulationszeitraum' )
       ELSE
          CALL print_array_not_alloc( 'disp_total_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_total_volume_old) ) THEN
          CALL print_array_shape( disp_total_volume_old, 'disp_total_volume_old(:,:)', & ! 
               'summiertes Gesamtvolumen fuer alle Verklapppolygone pro Baggerpolygon zum letzten Peilzeitpunkt' )
       ELSE
          CALL print_array_not_alloc( 'disp_total_volume_old(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_sediment_volume) ) THEN
          CALL print_array_shape( disp_sediment_volume, 'disp_sediment_volume(:,:)', & ! 
               'summiertes Sedimentvolumen fuer alle Verklapppolygone pro Baggerpolygon und den gesamten Simulationszeitraum' )
       ELSE
          CALL print_array_not_alloc( 'disp_sediment_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_sediment_volume_old) ) THEN
          CALL print_array_shape( disp_sediment_volume_old, 'disp_sediment_volume_old(:,:)', & ! 
               'summiertes Sedimentvolumen fuer alle Verklapppolygone pro Baggerpolygon zum letzten Peilzeitpunkt' )
       ELSE
          CALL print_array_not_alloc( 'disp_sediment_volume_old(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_fraction_volume) ) THEN
          CALL print_array_shape( disp_fraction_volume, 'disp_fraction_volume(:,:,:)', & ! 
               'summiertes Fraktionsvolumen fuer alle Verklapppolygone pro Baggerpolygon und den gesamten Simulationszeitraum' )
       ELSE
          CALL print_array_not_alloc( 'disp_fraction_volume(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_fraction_volume_old) ) THEN
          CALL print_array_shape( disp_fraction_volume_old, 'disp_fraction_volume_old(:,:,:)', & ! 
               'summiertes Fraktionsvolumen fuer alle Verklapppolygone pro Baggerpolygon zum letzten Peilzeitpunkt' )
       ELSE
          CALL print_array_not_alloc( 'disp_fraction_volume_old(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_disp_node_depth_tc) ) THEN
          CALL print_array_shape( delta_disp_node_depth_tc, 'delta_disp_node_depth_tc(:,:)', & ! 
               'Differenz zwischen maximaler Tiefe aus zeitgesteuertem Verklappen and aktueller Knotentiefe' )
       ELSE
          CALL print_array_not_alloc( 'delta_disp_node_depth_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_node_depth_abl) ) THEN
          CALL print_array_shape( delta_node_depth_abl, 'delta_node_depth_abl(:,:)', & ! 
               'Differenz zwischen maximaler Tiefe aus Geschiebezugabe and aktueller Knotentiefe' )
       ELSE
          CALL print_array_not_alloc( 'delta_node_depth_abl(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(edge_related_surface) ) THEN
          CALL print_array_shape( edge_related_surface, 'edge_related_surface(:)',   & ! 
               'Liste der zugehoerigen Oberflaechen der Polygone einer Kante' )
       ELSE
          CALL print_array_not_alloc( 'edge_related_surface(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(edge_weighted_sum) ) THEN
          CALL print_array_shape( edge_weighted_sum, 'edge_weighted_sum(:)',   & ! 
               'Liste der kantengewichteten Summen eines Polygons' )
       ELSE
          CALL print_array_not_alloc( 'edge_weighted_sum(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_related_surface) ) THEN
          CALL print_array_shape( node_related_surface, 'node_related_surface(:)',   & ! 
               'Liste der zugehoerigen Oberflaechen der Polygone einem Knoten' )
       ELSE
          CALL print_array_not_alloc( 'node_related_surface(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_weighted_sum) ) THEN
          CALL print_array_shape( node_weighted_sum, 'node_weighted_sum(:)',   & ! 
               'Liste der knotengewichteten Summen eines Polygons' )
       ELSE
          CALL print_array_not_alloc( 'node_weighted_sum(:)' )
       END IF
    END IF
    ! [3] ohter arrays ------------------------------------------------
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(fraction_name) ) THEN
          CALL print_array_shape( fraction_name, 'fraction_name(:)', & ! 
               'Namen der Sedimentfraktionen' )
       ELSE
          CALL print_array_not_alloc( 'fraction_name(:)' )
       END IF
    END IF
    !
  END SUBROUTINE print_dredgesim_shape_d
  !
  !! printing selected contents of all data 
  !! subroutine generates error messages
  SUBROUTINE print_dredgesim_data_d ( )
    !
    IF ( no_error( ) ) CALL print_file( trace_file )
    IF ( no_error( ) ) CALL print_file( printer_file )
    IF ( no_error( ) ) CALL print_file( steering_file )
    !
    IF ( no_error( ) ) CALL print_time ( act_timestep )
    IF ( no_error( ) ) CALL print_datetime ( act_time )
    IF ( no_error( ) ) CALL print_datetime ( old_time )
    IF ( no_error( ) ) CALL print_datetime ( end_time )
    IF ( no_error( ) ) CALL print_datetime ( initial_time )
    !
    ! [1] Pointer-arrays -------------------------------------------------
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(nodes_of_poly) ) THEN
          CALL print_array_contents( nodes_of_poly, 'nodes_of_poly(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(edgelist_of_poly) ) THEN
          CALL print_array_contents( edgelist_of_poly, 'edgelist_of_poly(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(nodelist_of_poly) ) THEN
          CALL print_array_contents( nodelist_of_poly, 'nodelist_of_poly(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(center_coord) ) THEN
          CALL print_array_contents( center_coord, 'center_coord(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(grav_center_coord) ) THEN
          CALL print_array_contents( grav_center_coord, 'grav_center_coord(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_coord) ) THEN
          CALL print_array_contents( node_coord, 'node_coord(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(poly_depth) ) THEN
          CALL print_array_contents( poly_depth, 'poly_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(edge_water_depth) ) THEN
          CALL print_array_contents( edge_water_depth, 'edge_water_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_water_depth) ) THEN
          CALL print_array_contents( node_water_depth, 'node_water_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_noero_depth) ) THEN
          CALL print_array_contents( node_noero_depth, 'node_noero_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(poly_area) ) THEN
          CALL print_array_contents( poly_area, 'poly_area(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_area) ) THEN
          CALL print_array_contents( node_area, 'node_area(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(knolg) ) THEN
          CALL print_array_contents( knolg, 'knolg(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_neighb) ) THEN
          CALL print_array_contents( node_neighb, 'node_neighb(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(nb_neighb_pt) ) THEN
          CALL print_array_contents( nb_neighb_pt, 'nb_neighb_pt(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(list_send) ) THEN
          CALL print_array_contents( list_send, 'list_send(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(nh_com) ) THEN
          CALL print_array_contents( nh_com, 'nh_com(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(buf_recv) ) THEN
          CALL print_array_contents( buf_recv, 'buf_recv(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(buf_send) ) THEN
          CALL print_array_contents( buf_recv, 'buf_send(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_depth) ) THEN
          CALL print_array_contents( node_depth, 'node_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(edge_depth) ) THEN
          CALL print_array_contents( edge_depth, 'edge_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_porosity) ) THEN
          CALL print_array_contents( node_porosity, 'node_porosity(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(cell_sediment_fraction) ) THEN
          CALL print_array_contents( cell_sediment_fraction, 'cell_sediment_fraction(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ASSOCIATED(node_sediment_fraction) ) THEN
          CALL print_array_contents( node_sediment_fraction, 'node_sediment_fraction(:,:)' )
       END IF
    END IF
    ! [2] arrays with TARGET-attribute ------------------------------------- 
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_water_depth) ) THEN
          CALL print_array_contents( poly_water_depth, 'poly_water_depth(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_crit_type) ) THEN
          CALL print_array_contents( dredge_crit_type, 'dredge_crit_type(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_poly_index) ) THEN
          CALL print_array_contents( dredge_poly_index, 'dredge_poly_index(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_node_index) ) THEN
          CALL print_array_contents( dredge_node_index, 'dredge_node_index(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dispose_poly_index) ) THEN
          CALL print_array_contents( dispose_poly_index, 'dispose_poly_index(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dispose_node_index) ) THEN
          CALL print_array_contents( dispose_node_index, 'dispose_node_index(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_poly_index_tc) ) THEN
          CALL print_array_contents( dredge_poly_index_tc, 'dredge_poly_index_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredge_node_index_tc) ) THEN
          CALL print_array_contents( dredge_node_index_tc, 'dredge_node_index_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dispose_poly_index_tc) ) THEN
          CALL print_array_contents( dispose_poly_index_tc, 'dispose_poly_index_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dispose_node_index_tc) ) THEN
          CALL print_array_contents( dispose_node_index_tc, 'dispose_node_index_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(art_bed_load_poly_index) ) THEN
          CALL print_array_contents( art_bed_load_poly_index, 'art_bed_load_poly_index(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(art_bed_load_node_index) ) THEN
          CALL print_array_contents( art_bed_load_node_index, 'art_bed_load_node_index(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_total_volume) ) THEN
          CALL print_array_contents( poly_total_volume, 'poly_total_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_total_volume) ) THEN
          CALL print_array_contents( node_total_volume, 'node_total_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_node_total_volume) ) THEN
          CALL print_array_contents( disp_node_total_volume, 'disp_node_total_volume(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(total_volume) ) THEN
          CALL print_array_contents( total_volume, 'total_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_total_volume) ) THEN
          CALL print_array_contents( all_total_volume, 'all_total_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_total_volume_old) ) THEN
          CALL print_array_contents( all_total_volume_old, 'all_total_volume_old(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_sediment_volume) ) THEN
          CALL print_array_contents( poly_sediment_volume, 'poly_sediment_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_sediment_volume) ) THEN
          CALL print_array_contents( node_sediment_volume, 'node_sediment_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_sediment_volume_rs) ) THEN
          CALL print_array_contents( node_sediment_volume_rs, 'node_sediment_volume_rs(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(last_obs_time_rs) ) THEN
          CALL print_array_contents( last_obs_time_rs, 'last_obs_time_rs(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(next_obs_time_rs) ) THEN
          CALL print_array_contents( next_obs_time_rs, 'next_obs_time_rs(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(sediment_volume) ) THEN
          CALL print_array_contents( sediment_volume, 'sediment_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_sediment_volume) ) THEN
          CALL print_array_contents( all_sediment_volume, 'all_sediment_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_sediment_volume_old) ) THEN
          CALL print_array_contents( all_sediment_volume_old, 'all_sediment_volume_old(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_sediment_volume_tc) ) THEN
          CALL print_array_contents( node_sediment_volume_tc, 'node_sediment_volume_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(sediment_volume_tc) ) THEN
          CALL print_array_contents( sediment_volume_tc, 'sediment_volume_tc(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_water_volume) ) THEN
          CALL print_array_contents( poly_water_volume, 'poly_water_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_water_volume) ) THEN
          CALL print_array_contents( node_water_volume, 'node_water_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(water_volume) ) THEN
          CALL print_array_contents( water_volume, 'water_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_water_volume) ) THEN
          CALL print_array_contents( all_water_volume, 'all_water_volume(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_water_volume_old) ) THEN
          CALL print_array_contents( all_water_volume_old, 'all_water_volume_old(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_fraction_volume) ) THEN
          CALL print_array_contents( poly_fraction_volume, 'poly_fraction_volume(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_fraction_volume) ) THEN
          CALL print_array_contents( node_fraction_volume, 'node_fraction_volume(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_fraction_volume_rs) ) THEN
          CALL print_array_contents( node_fraction_volume_rs, 'node_fraction_volume_rs(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(fraction_volume) ) THEN
          CALL print_array_contents( fraction_volume, 'fraction_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_fraction_volume) ) THEN
          CALL print_array_contents( all_fraction_volume, 'all_fraction_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(all_fraction_volume_old) ) THEN
          CALL print_array_contents( all_fraction_volume_old, 'all_fraction_volume_old(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(poly_fraction_volume_tc) ) THEN
          CALL print_array_contents( poly_fraction_volume_tc, 'poly_fraction_volume_tc(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_fraction_volume_tc) ) THEN
          CALL print_array_contents( node_fraction_volume_tc, 'node_fraction_volume_tc(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(fraction_volume_tc) ) THEN
          CALL print_array_contents( fraction_volume_tc, 'fraction_volume_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(aim_poly_depth) ) THEN
          CALL print_array_contents( aim_poly_depth, 'aim_poly_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(aim_node_depth) ) THEN
          CALL print_array_contents( aim_node_depth, 'aim_node_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(aim_node_depth) ) THEN
          CALL print_array_contents( aim_node_depth_tc, 'aim_node_depth_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(aim_node_depth_abl) ) THEN
          CALL print_array_contents( aim_node_depth_abl, 'aim_node_depth_abl(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_dredge_poly_depth) ) THEN
          CALL print_array_contents( delta_dredge_poly_depth, 'delta_dredge_poly_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_dredge_node_depth) ) THEN
          CALL print_array_contents( delta_dredge_node_depth, 'delta_dredge_node_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_disp_poly_depth) ) THEN
          CALL print_array_contents( delta_disp_poly_depth, 'delta_disp_poly_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_disp_node_depth) ) THEN
          CALL print_array_contents( delta_disp_node_depth, 'delta_disp_node_depth(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_poly_depth) ) THEN
          CALL print_array_contents( disp_poly_depth, 'disp_poly_depth(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_node_depth) ) THEN
          CALL print_array_contents( disp_node_depth, 'disp_node_depth(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dzb_dredge) ) THEN
          CALL print_array_contents( dzb_dredge, 'dzb_dredge(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dzb_disp) ) THEN
          CALL print_array_contents( dzb_disp, 'dzb_disp(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredged_sediment_volume_to_disp) ) THEN
          CALL print_array_contents( dredged_sediment_volume_to_disp, 'dredged_sediment_volume_to_disp(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(dredged_fraction_volume_to_disp) ) THEN
          CALL print_array_contents( dredged_fraction_volume_to_disp, 'dredged_fraction_volume_to_disp(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_total_volume) ) THEN
          CALL print_array_contents( disp_total_volume, 'disp_total_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_total_volume_old) ) THEN
          CALL print_array_contents( disp_total_volume_old, 'disp_total_volume_old(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_sediment_volume) ) THEN
          CALL print_array_contents( disp_sediment_volume, 'disp_sediment_volume(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_sediment_volume_old) ) THEN
          CALL print_array_contents( disp_sediment_volume_old, 'disp_sediment_volume_old(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_fraction_volume) ) THEN
          CALL print_array_contents( disp_fraction_volume, 'disp_fraction_volume(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(disp_fraction_volume_old) ) THEN
          CALL print_array_contents( disp_fraction_volume_old, 'disp_fraction_volume_old(:,:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_disp_node_depth_tc) ) THEN
          CALL print_array_contents( delta_disp_node_depth_tc, 'delta_disp_node_depth_tc(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(delta_node_depth_abl) ) THEN
          CALL print_array_contents( delta_node_depth_abl, 'delta_node_depth_abl(:,:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(edge_related_surface) ) THEN
          CALL print_array_contents( edge_related_surface, 'edge_related_surface(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(edge_weighted_sum) ) THEN
          CALL print_array_contents( edge_weighted_sum, 'edge_weighted_sum(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_related_surface) ) THEN
          CALL print_array_contents( node_related_surface, 'node_related_surface(:)' )
       END IF
    END IF
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(node_weighted_sum) ) THEN
          CALL print_array_contents( node_weighted_sum, 'node_weighted_sum(:)' )
       END IF
    END IF
    ! [3] sonstige arrays ------------------------------------------------
    IF ( no_error( ) ) THEN
       IF ( ALLOCATED(fraction_name) ) THEN
          CALL print_array_contents( fraction_name, 'fraction_name(:)' )
       END IF
    END IF
    !
  END SUBROUTINE print_dredgesim_data_d
  !
  ! ----------------------------------------------------------------------
  ! SET-REF-functions for pointing on external declared data
  ! ----------------------------------------------------------------------
  !
  !! setting a pointer on external data for array nodes_of_poly(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_nodes_of_poly_ref_d ( arr ) 
    !! 
    INTEGER , POINTER :: arr(:) ! 
    !
    nodes_of_poly => arr
    !
  END SUBROUTINE set_ds_nodes_of_poly_ref_d
  !
  !! setting a pointer on external data for array edgelist_of_poly(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_edgelist_of_poly_ref_d ( arr ) 
    !! 
    INTEGER , POINTER :: arr(:,:) ! 
    !
    edgelist_of_poly => arr
    !
  END SUBROUTINE set_ds_edgelist_of_poly_ref_d
  !
  !! setting a pointer on external data for array nodelist_of_poly(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_nodelist_of_poly_ref_d ( arr ) 
    !! 
    INTEGER , POINTER :: arr(:,:) ! 
    !
    nodelist_of_poly => arr
    !
  END SUBROUTINE set_ds_nodelist_of_poly_ref_d
  !
  !! setting a pointer on external data for array center_coord(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_center_coord_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:,:) ! 
    !
    center_coord => arr
    !
  END SUBROUTINE set_ds_center_coord_ref_d
  !
  !! setting a pointer on external data for array grav_center_coord(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_grav_center_coord_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:,:) ! 
    !
    grav_center_coord => arr
    !
  END SUBROUTINE set_ds_grav_center_coord_ref_d
  !
  !! setting a pointer on external data for array node_coord(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_node_coord_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:,:) ! 
    !
    node_coord => arr
    !
  END SUBROUTINE set_ds_node_coord_ref_d
  !
  !! setting a pointer on external data for array poly_depth(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_poly_depth_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    poly_depth => arr
    !
  END SUBROUTINE set_ds_poly_depth_ref_d
  !
  !! setting a pointer on external data for array edge_water_depth(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_edge_water_depth_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    edge_water_depth => arr
    !
  END SUBROUTINE set_ds_edge_water_depth_ref_d
  !
  !! setting a pointer on external data for array node_water_depth(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_node_water_depth_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    node_water_depth => arr
    !
  END SUBROUTINE set_ds_node_water_depth_ref_d
  !
  !! setting a pointer on external data for array node_noero_depth(:)
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_node_noero_depth_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    node_noero_depth => arr
    !
  END SUBROUTINE set_ds_node_noero_depth_ref_d
  !
  !! setting a pointer on external data for array poly_area(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_poly_area_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    poly_area => arr
    !
  END SUBROUTINE set_ds_poly_area_ref_d
  !! setting a pointer on external data for array node_area(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_node_area_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    node_area => arr
    !
  END SUBROUTINE set_ds_node_area_ref_d

  !! setting a pointer on external data for array knolg(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_knolg_ref_i ( arr ) 
    !! 
    INTEGER , POINTER :: arr(:) ! 
    !
    knolg => arr
    !
  END SUBROUTINE set_ds_knolg_ref_i

  !! setting a pointer on external data for array node_neighb(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_node_neighb_ref_i ( arr ) 
    !! 
    INTEGER , POINTER :: arr(:) ! 
    !
    node_neighb => arr
    !
  END SUBROUTINE set_ds_node_neighb_ref_i
  !! setting a pointer on external data for array nb_neighb_pt(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_nb_neighb_pt_ref_i ( arr ) 
    !! 
    INTEGER , POINTER :: arr(:) ! 
    !
    nb_neighb_pt => arr
    !
  END SUBROUTINE set_ds_nb_neighb_pt_ref_i
  !! setting a pointer on external data for array list_send(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_list_send_ref_i ( arr ) 
    !! 
    INTEGER , POINTER :: arr(:) ! 
    !
    list_send => arr
    !
  END SUBROUTINE set_ds_list_send_ref_i
  !! setting a pointer on external data for array nh_com(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_nh_com_ref_i ( arr ) 
    !! 
    INTEGER , POINTER :: arr(:,:) ! 
    !
    nh_com => arr
    !
  END SUBROUTINE set_ds_nh_com_ref_i
  !! setting a pointer on external data for array buf_recv(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_buf_recv_ref_d ( arr ) 
    !! 
    DOUBLE PRECISION , POINTER :: arr(:,:) ! 
    !
    buf_recv => arr
    !
  END SUBROUTINE set_ds_buf_recv_ref_d
  !! setting a pointer on external data for array buf_send(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_buf_send_ref_d ( arr ) 
    !! 
    DOUBLE PRECISION , POINTER :: arr(:,:) ! 
    !
    buf_send => arr
    !
  END SUBROUTINE set_ds_buf_send_ref_d
  !
  !! setting a pointer on external data for array node_depth(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_node_depth_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    node_depth => arr
    !
  END SUBROUTINE set_ds_node_depth_ref_d
  !
  !! setting a pointer on external data for array edge_depth(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_edge_depth_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    edge_depth => arr
    !
  END SUBROUTINE set_ds_edge_depth_ref_d
  !
  !! setting a pointer on external data for array node_porosity(:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_node_porosity_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:) ! 
    !
    node_porosity => arr
    !
  END SUBROUTINE set_ds_node_porosity_ref_d
  !
  !! setting a pointer on external data for array cell_sediment_fraction(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_cell_sediment_frac_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:,:) ! 
    !
    cell_sediment_fraction => arr
    !
  END SUBROUTINE set_ds_cell_sediment_frac_ref_d
  !
  !! setting a pointer on external data for array node_sediment_fraction(:,:) 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_node_sediment_frac_ref_d ( arr ) 
    !! 
    REAL (KIND=Double) , POINTER :: arr(:,:) ! 
    !
    node_sediment_fraction => arr
    !
  END SUBROUTINE set_ds_node_sediment_frac_ref_d
  !
  ! ----------------------------------------------------------------------
  ! STORE-functions for transfer of data (copy)
  ! ----------------------------------------------------------------------
  !
  !! transfering and saving of current values in array dredge_poly_index(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_dredge_poly_index_d ( arr ) 
    !! two-dimensional array with index list mesh element - dredge polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=17) , parameter  :: c_arr_name='dredge_poly_index' !
    !
    IF ( ALLOCATED(dredge_poly_index) ) THEN       
	   dredge_poly_index(:,:)  = arr(:,:)
       upd_dredge_poly_index = .true. 
    END IF
    !
  END SUBROUTINE store_ds_dredge_poly_index_d
  !
  !! transfering and saving of current values in array dredge_node_index(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_dredge_node_index_d ( arr ) 
    !! two-dimensional array with index list mesh node - dredge polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=17) , parameter  :: c_arr_name='dredge_node_index' !
    !
    IF ( ALLOCATED(dredge_node_index) ) THEN       
	   dredge_node_index(:,:)  = arr(:,:)
       upd_dredge_node_index = .true. 
    END IF
    !
  END SUBROUTINE store_ds_dredge_node_index_d
  !
  !! transfering and saving of current values in array dispose_poly_index(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_dispose_poly_index_d ( arr ) 
    !! two-dimensional array with index list mesh element - disposal polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=18) , parameter  :: c_arr_name='dispose_poly_index' !
    !
    IF ( ALLOCATED(dispose_poly_index) ) THEN       
	   dispose_poly_index(:,:)  = arr(:,:)
       upd_dispose_poly_index = .true. 
    END IF
    !
  END SUBROUTINE store_ds_dispose_poly_index_d
  !
  !! transfering and saving of current values in array dispose_node_index(:,:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_dispose_node_index_d ( arr ) 
    !! two-dimensional array with index list mesh node - disposal polygon
    INTEGER            , INTENT(IN) :: arr(:,:,:) ! 
    !! name of the array
    CHARACTER (LEN=18) , parameter  :: c_arr_name='dispose_node_index' !
    !
    IF ( ALLOCATED(dispose_node_index) ) THEN       
        dispose_node_index(:,:,:)  = arr(:,:,:)
        upd_dispose_node_index = .true. 
    END IF
    !
  END SUBROUTINE store_ds_dispose_node_index_d
  !  
  !! transfering and saving of current values in array dredge_poly_index_tc(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_dredge_poly_index_tc_d ( arr ) 
    !! two-dimensional array with index list mesh element - dredge polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=20) , parameter  :: c_arr_name='dredge_poly_index_tc' !
    !
    IF ( ALLOCATED(dredge_poly_index_tc) ) THEN       
         dredge_poly_index_tc(:,:)  = arr(:,:)
         upd_dredge_poly_index_tc = .true. 
    END IF
    !
  END SUBROUTINE store_ds_dredge_poly_index_tc_d
  !
  !! transfering and saving of current values in array dredge_node_index_tc(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_dredge_node_index_tc_d ( arr ) 
    !! two-dimensional array with index list mesh element - dredge polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=20) , parameter  :: c_arr_name='dredge_node_index_tc' !
    !
    IF ( ALLOCATED(dredge_node_index_tc) ) THEN       
        dredge_node_index_tc(:,:)  = arr(:,:)
        upd_dredge_node_index_tc = .true. 
    END IF
    !
  END SUBROUTINE store_ds_dredge_node_index_tc_d
  !
  !! transfering and saving of current values in array dispose_poly_index_tc(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_dispose_poly_index_tc_d ( arr ) 
    !! two-dimensional array with index list mesh element - disposal polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=21) , parameter  :: c_arr_name='dispose_poly_index_tc' !
    !
    IF ( ALLOCATED(dispose_poly_index_tc) ) THEN       
	   dispose_poly_index_tc(:,:)  = arr(:,:)
       upd_dispose_poly_index_tc = .true. 
    END IF
    !
  END SUBROUTINE store_ds_dispose_poly_index_tc_d
  !
  !! transfering and saving of current values in array dispose_node_index_tc(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_dispose_node_index_tc_d ( arr ) 
    !! two-dimensional array with index list mesh node - disposal polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=21) , parameter  :: c_arr_name='dispose_node_index_tc' !
    !
    IF ( ALLOCATED(dispose_node_index_tc) ) THEN       
        dispose_node_index_tc(:,:)  = arr(:,:)
        upd_dispose_node_index_tc = .true. 
    END IF
    !
  END SUBROUTINE store_ds_dispose_node_index_tc_d
  !
  !! transfering and saving of current values in array art_bed_load_poly_index(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_art_bed_load_pol_ind_d ( arr ) 
    !! two-dimensional array with index list mesh element - disposal polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=23) , parameter  :: c_arr_name='art_bed_load_poly_index' !
    !
    IF ( ALLOCATED(art_bed_load_poly_index) ) THEN       
	   art_bed_load_poly_index(:,:)  = arr(:,:)
       upd_art_bed_load_poly_index = .true. 
    END IF
    !
  END SUBROUTINE store_ds_art_bed_load_pol_ind_d
  !
  !! transfering and saving of current values in array art_bed_load_node_index(:,:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_art_bed_load_nod_ind_d ( arr ) 
    !! two-dimensional array with index list mesh node - disposal polygon
    INTEGER            , INTENT(IN) :: arr(:,:) ! 
    !! name of the array
    CHARACTER (LEN=23) , parameter  :: c_arr_name='art_bed_load_node_index' !
    !
    IF ( ALLOCATED(art_bed_load_node_index) ) THEN       
         art_bed_load_node_index(:,:)  = arr(:,:)
         upd_art_bed_load_node_index = .true. 
    END IF
    !
  END SUBROUTINE store_ds_art_bed_load_nod_ind_d
  !
  !! transfering and saving of current values in array fraction_name(:)       
  !! hint 1: array will be allocated if necessary            
  !! hint 2: if array is already allocated, its dimension will be checked 
  !! subroutine generates error messages
  SUBROUTINE store_ds_fraction_name_d ( arr, shp )
    !! one-dimensional array with names of sediment fractions
    CHARACTER (LEN=*)  , INTENT(IN) :: arr(:) ! 
    !! Shape der originalen data (muss with dem Speicherarray &uuml;bereinstimmen)
    INTEGER            , INTENT(IN) :: shp(:) ! 
    !! name of the array
    CHARACTER (LEN=13) , parameter  :: c_arr_name='fraction_name' !
    ! variables
    INTEGER :: i, nn ! 
    LOGICAL :: ok ! 
    !
    IF ( ALLOCATED(fraction_name) ) THEN
       ok = has_identical_shape( shp, SHAPE(fraction_name), c_arr_name ) ! FM
    ELSE
       ok = has_correct_rank( shp, 1, c_arr_name ) 
       IF ( ok ) CALL alloc_fraction_name( shp(1) ) ! FM
    END IF
    IF ( no_error( ) ) THEN
       nn = MIN(LEN(arr),LEN(fraction_name))
       DO i=1,SIZE(arr)
          fraction_name(i)(1:nn)  = arr(i)(1:nn)
       END DO
    END IF
    !
  END SUBROUTINE store_ds_fraction_name_d
  !
  ! ----------------------------------------------------------------------
  ! GET-functions
  ! ----------------------------------------------------------------------
  !
  !! determine the number of elements (Zentren) 
  !! function does not throw error messages
  FUNCTION get_nof_poly_d ( ) &
       RESULT( res )
    !! result: number of elements
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED(poly_depth) ) THEN
       res = SIZE(poly_depth)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_nof_poly_d
  !
  !! determine the number of nodes 
  !! function does not throw error messages
  FUNCTION get_nof_nodes_d ( ) &
       RESULT( res )
    !! result: number of nodes
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED(node_depth) ) THEN
       res = SIZE(node_depth)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_nof_nodes_d
  !
  !! determine the number of edges
  !! function does not throw error messages
  FUNCTION get_nof_edge_d ( ) &
       RESULT( res )
    !! result: number of edges
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED(edge_depth) ) THEN
       res = SIZE(edge_depth)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_nof_edge_d
  !
  !! determine the number of sediment fractions 
  !! function does not throw error messages
  FUNCTION get_nof_sediment_fraction_d ( ) &
       RESULT( res )
    !! result: number of sediment fractions
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED(node_sediment_fraction)  ) THEN
       res = SIZE(node_sediment_fraction,2)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_nof_sediment_fraction_d
  !
  !! determine the number of dredge polygons 
  !! function does not throw error messages
  FUNCTION get_nof_dredge_poly_d ( ) &
       RESULT( res )
    !! result: number of dredge polygons
    INTEGER :: res ! 
    !
    IF ( ALLOCATED(dredge_criterion) ) THEN
       res = SIZE(dredge_criterion)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_nof_dredge_poly_d
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
  ! ----------------------------------------------------------------------
  ! PRINT-methods
  ! ----------------------------------------------------------------------
  !
  !! print hint that an array was not allocated 
  !! subroutine generates error messages
  SUBROUTINE print_array_not_alloc_d ( name )
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name ! 
    !! name of the program unit
    CHARACTER (LEN=23) , parameter  :: c_upname='print_array_not_alloc_d' ! 
    ! variable
    INTEGER :: stat ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    WRITE(prn_lun,9000,IOSTAT=stat) TRIM(name)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7100, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# array "',A,'" ist nicht allokiert ' )
    !
  END SUBROUTINE print_array_not_alloc_d
  !
  !! print the shape of an one-dimensional CHARACTER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_ch_1 ( var, name, text )
    !! array
    CHARACTER (LEN=*)  , INTENT(IN) :: var(:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_ch_1' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char  ! 
    INTEGER                    :: stat, l ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    l      = LEN(var)
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), l, TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',I10,', LEN = ',I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_ch_1
  !
  !! print the shape of a two-dimensional CHARACTER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_ch_2 ( var, name, text )
    !! array
    CHARACTER (LEN=*)  , INTENT(IN) :: var(:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_ch_2' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char  ! 
    INTEGER                    :: stat, l ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    l      = LEN(var)
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), l, TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',2I10,', LEN = ',I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_ch_2
  !
  !! print the shape of an one-dimensional INTEGER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_in_1 ( var, name, text )
    !! array
    INTEGER            , INTENT(IN) :: var(:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_in_1' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char ! 
    INTEGER                    :: stat   ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_in_1
  !
  !! print the shape of a two-dimensional INTEGER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_in_2 ( var, name, text )
    !! array
    INTEGER            , INTENT(IN) :: var(:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_in_2' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char ! 
    INTEGER                    :: stat   ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',2I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_in_2
  !
  !! print the shape of a three-dimensional INTEGER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_in_3 ( var, name, text )
    !! array
    INTEGER            , INTENT(IN) :: var(:,:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_in_3' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char ! 
    INTEGER                    :: stat   ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',2I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_in_3  
  !
  !! print the shape of an one-dimensional REAL(Double)-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_dp_1 ( var, name, text )
    !! array
    REAL (KIND=Double) , INTENT(IN) :: var(:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_dp_1' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char ! 
    INTEGER                    :: stat   ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_dp_1
  !
  !! print the shape of a two-dimensional REAL(Double)-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_dp_2 ( var, name, text )
    !! array
    REAL (KIND=Double) , INTENT(IN) :: var(:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_dp_2' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char ! 
    INTEGER                    :: stat   ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',2I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_dp_2
  !
  !! print the shape of a three-dimensional REAL(Double)-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_dp_3 ( var, name, text )
    !! array
    REAL (KIND=Double) , INTENT(IN) :: var(:,:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_dp_3' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char ! 
    INTEGER                    :: stat   ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',3I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_dp_3
  !
  !! print the shape of an one-dimensional FILE-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_shape_fi_1 ( var, name, text )
    !! array
    TYPE (t_file)      , INTENT(IN) :: var(:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! text for explaining the content of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: text   ! 
    !! name of program unit
    CHARACTER (LEN=22) , parameter  :: c_upname='print_array_shape_fi_1' ! 
    ! variables
    CHARACTER (LEN=c_len_char) :: l_char  ! 
    INTEGER                    :: stat    ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    l_char = REPEAT( ' ', LEN(l_char) )
    l_char = name(1:MIN(LEN_TRIM(name),LEN(l_char)))
    !
    WRITE(prn_lun,9000,IOSTAT=stat) l_char, SHAPE( var ), TRIM(text)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7110, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Shape von "',A,'" = ',I10,' : ',A )
    !
  END SUBROUTINE print_array_shape_fi_1
  !
  !! print the selected contents of an one-dimensional CHARACTER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_ch_1 ( var, name )
    !! array
    CHARACTER (LEN=*)  , INTENT(IN) :: var(:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_ch_1' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1
    ie = SIZE(var)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       WRITE(prn_lun,9000,IOSTAT=stat) name, i, TRIM(var(i))
       IF ( stat /= 0 ) EXIT
    END DO
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,', value = ',A)
    !
  END SUBROUTINE print_array_contents_ch_1
  !
  !! print the selected contents of a two-dimensional CHARACTER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_ch_2 ( var, name )
    !! array
    CHARACTER (LEN=*)  , INTENT(IN) :: var(:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name     ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_ch_2' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id, j, ja, je, jd ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1                                      ; ja = 1
    ie = SIZE(var,1)                            ; je = SIZE(var,2)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents))) ; jd = MAX(1,NINT(REAL(je)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       DO j=ja,je,jd
          WRITE(prn_lun,9000,IOSTAT=stat) name, i, j, TRIM(var(i,j))
          IF ( stat /= 0 ) EXIT
       END DO
       IF ( stat /= 0 ) EXIT
    END DO
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,' j = ',I10,', value = ',A)
    !
  END SUBROUTINE print_array_contents_ch_2
  !
  !! print the selected contents of an one-dimensional INTEGER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_in_1 ( var, name )
    !! array
    INTEGER            , INTENT(IN) :: var(:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_in_1' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1
    ie = SIZE(var)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       WRITE(prn_lun,9000,IOSTAT=stat) name, i, var(i)
       IF ( stat /= 0 ) EXIT
    END DO
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,', value = ',I10)
    !
  END SUBROUTINE print_array_contents_in_1
  !
  !! print the selected contents of a two-dimensional INTEGER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_in_2 ( var, name )
    !! array
    INTEGER            , INTENT(IN) :: var(:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name     ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_in_2' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id, j, ja, je, jd ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1                                      ; ja = 1
    ie = SIZE(var,1)                            ; je = SIZE(var,2)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents))) ; jd = MAX(1,NINT(REAL(je)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       DO j=ja,je,jd
          WRITE(prn_lun,9000,IOSTAT=stat) name, i, j, var(i,j)
          IF ( stat /= 0 ) EXIT
       END DO
       IF ( stat /= 0 ) EXIT
    END DO
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,' j = ',I10,', value = ',I10)
    !
  END SUBROUTINE print_array_contents_in_2
  !
  !! print the selected contents of a three-dimensional INTEGER-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_in_3 ( var, name )
    !! array
    INTEGER            , INTENT(IN) :: var(:,:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name     ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_in_3' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id, j, ja, je, jd, k, ka, ke, kd ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1                                      ; ja = 1
    ie = SIZE(var,1)                            ; je = SIZE(var,2)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents))) ; jd = MAX(1,NINT(REAL(je)/REAL(c_contents)))
    ka = 1
    ke = SIZE(var,3)
    kd = MAX(1,NINT(REAL(ke)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       DO j=ja,je,jd
          DO k=ka,ke,kd
             WRITE(prn_lun,9000,IOSTAT=stat) name, i, j, k, var(i,j,k)
             IF ( stat /= 0 ) EXIT
          END DO
          IF ( stat /= 0 ) EXIT
       END DO
    END DO
	!
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,' j = ',I10,' k = ',I10,', value = ',I10)
    !
  END SUBROUTINE print_array_contents_in_3
  !
  !! print the selected contents of an one-dimensional REAL(Double)-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_dp_1 ( var, name )
    !! array
    REAL (KIND=Double) , INTENT(IN) :: var(:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_dp_1' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1
    ie = SIZE(var)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       WRITE(prn_lun,9000,IOSTAT=stat) name, i, var(i)
       IF ( stat /= 0 ) EXIT
    END DO
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,', value = ',G15.7)
    !
  END SUBROUTINE print_array_contents_dp_1
  !
  !! print the selected contents of a two-dimensional REAL(Double)-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_dp_2 ( var, name )
    !! array
    REAL (KIND=Double) , INTENT(IN) :: var(:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name     ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_dp_2' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id, j, ja, je, jd ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1                                      ; ja = 1
    ie = SIZE(var,1)                            ; je = SIZE(var,2)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents))) ; jd = MAX(1,NINT(REAL(je)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       DO j=ja,je,jd
          WRITE(prn_lun,9000,IOSTAT=stat) name, i, j, var(i,j)
          IF ( stat /= 0 ) EXIT
       END DO
       IF ( stat /= 0 ) EXIT
    END DO
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,' j = ',I10,', value = ',G15.7)
    !
  END SUBROUTINE print_array_contents_dp_2
  !
  !! print the selected contents of a three-dimensional REAL(Double)-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_dp_3 ( var, name )
    !! array
    REAL (KIND=Double) , INTENT(IN) :: var(:,:,:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name      ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_dp_3' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id, j, ja, je, jd, k, ka, ke, kd ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1                                      ; ja = 1
    ie = SIZE(var,1)                            ; je = SIZE(var,2)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents))) ; jd = MAX(1,NINT(REAL(je)/REAL(c_contents)))
    ka = 1
    ke = SIZE(var,3)
    kd = MAX(1,NINT(REAL(ke)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       DO j=ja,je,jd
          DO k=ka,ke,kd
             WRITE(prn_lun,9000,IOSTAT=stat) name, i, j, k, var(i,j,k)
             IF ( stat /= 0 ) EXIT
          END DO
          IF ( stat /= 0 ) EXIT
       END DO
    END DO
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,' j = ',I10,' k = ',I10,', value = ',G15.7)
    !
  END SUBROUTINE print_array_contents_dp_3
  !
  !! print the selected contents of an one-dimensional FILE-array 
  !! subroutine does not throw error messages
  SUBROUTINE print_array_contents_fi_1 ( var, name )
    !! array
    TYPE (t_file)      , INTENT(IN) :: var(:) ! 
    !! name of the array
    CHARACTER (LEN=*)  , INTENT(IN) :: name   ! 
    !! name of program unit
    CHARACTER (LEN=25) , parameter  :: c_upname='print_array_contents_fi_1' ! 
    ! variables
    INTEGER :: stat, i, ia, ie, id ! 
    !
    IF ( any_error( ) .OR. .NOT. prn_op ) RETURN
    !
    ia = 1
    ie = SIZE(var)
    id = MAX(1,NINT(REAL(ie)/REAL(c_contents)))
    !
    DO i=ia,ie,id
       WRITE(prn_lun,9000,IOSTAT=stat) name, i
       IF ( stat /= 0 ) EXIT
       CALL print_file( var(i) )
    END DO
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors, -7120, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', TRIM(name) )
    END IF
    !
9000 FORMAT ( '# Data "',A,'" : i = ',I10,', value = siehe nachfolgende data')
    !
  END SUBROUTINE print_array_contents_fi_1
  !
  ! ----------------------------------------------------------------------
  ! ALLOCATE-methods
  ! ----------------------------------------------------------------------
  !
  !! allocating the one-dimensional REAL(Double)-array "poly_water_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_poly_water_depth_d ( idim1 )
    !! first dimension of the array "poly_water_depth"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24) , parameter  :: c_upname='alloc_poly_water_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( poly_water_depth ) ) CALL dealloc_poly_water_depth ( )
    ALLOCATE ( poly_water_depth(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'poly_water_depth(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       poly_water_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_poly_water_depth_d
  !
  !! allocating the one-dimensional INTEGER-array "dredge_crit_type" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_crit_type_d ( idim1 )
    !! first dimension of the array "dredge_crit_type"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24) , parameter  :: c_upname='alloc_dredge_crit_type_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredge_crit_type ) ) CALL dealloc_dredge_crit_type ( )
    ALLOCATE ( dredge_crit_type(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredge_crit_type(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       dredge_crit_type = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dredge_crit_type_d
  !
  !! allocating the two-dimensional INTEGER-array "dredge_poly_index" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_poly_index_d ( idim1, idim2 )
    !! first dimension of the array "dredge_poly_index"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dredge_poly_index"
    INTEGER          , INTENT(IN)   :: idim2 !
    !! name of the subroutine
    CHARACTER (LEN=25) , parameter  :: c_upname='alloc_dredge_poly_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredge_poly_index ) ) CALL dealloc_dredge_poly_index ( )
    ALLOCATE ( dredge_poly_index(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredge_poly_index(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dredge_poly_index = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dredge_poly_index_d
  !
  !! allocating the two-dimensional INTEGER-array "dredge_node_index" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_node_index_d ( idim1, idim2 )
    !! first dimension of the array "dredge_node_index"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dredge_node_index"
    INTEGER          , INTENT(IN)   :: idim2 !
    !! name of the subroutine
    CHARACTER (LEN=25) , parameter  :: c_upname='alloc_dredge_node_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredge_node_index ) ) CALL dealloc_dredge_node_index ( )
    ALLOCATE ( dredge_node_index(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredge_node_index(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dredge_node_index = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dredge_node_index_d
  !
  !! allocating the two-dimensional INTEGER-array "dispose_poly_index" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dispose_poly_index_d ( idim1, idim2 )
    !! first dimension of the array "dispose_poly_index"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dispose_poly_index"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=26) , parameter  :: c_upname='alloc_dispose_poly_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dispose_poly_index ) ) CALL dealloc_dispose_poly_index ( )
    ALLOCATE ( dispose_poly_index(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dispose_poly_index(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dispose_poly_index = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dispose_poly_index_d
  !
  !! allocating the three-dimensional INTEGER-array "dispose_node_index" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dispose_node_index_d ( idim1, idim2, idim3 )
    !! first dimension of the array "dispose_node_index"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dispose_node_index"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "dispose_node_index"
    INTEGER          , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=26) , parameter  :: c_upname='alloc_dispose_node_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dispose_node_index ) ) CALL dealloc_dispose_node_index ( )
    ALLOCATE ( dispose_node_index(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dispose_node_index(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       dispose_node_index = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dispose_node_index_d
  !
  !! allocating the two-dimensional INTEGER-array "dredge_poly_index_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_poly_index_tc_d ( idim1, idim2 )
    !! first dimension of the array "dredge_poly_index_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dredge_poly_index_tc"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=28) , parameter  :: c_upname='alloc_dredge_poly_index_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredge_poly_index_tc ) ) CALL dealloc_dredge_poly_index_tc ( )
    ALLOCATE ( dredge_poly_index_tc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredge_poly_index_tc(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dredge_poly_index_tc = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dredge_poly_index_tc_d
  !
  !! allocating the two-dimensional INTEGER-array "dredge_node_index_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_node_index_tc_d ( idim1, idim2 )
    !! first dimension of the array "dredge_node_index_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dredge_node_index_tc"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=28) , parameter  :: c_upname='alloc_dredge_node_index_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredge_node_index_tc ) ) CALL dealloc_dredge_node_index_tc ( )
    ALLOCATE ( dredge_node_index_tc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredge_node_index_tc(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dredge_node_index_tc = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dredge_node_index_tc_d
  !
  !! allocating the two-dimensional INTEGER-array "dispose_poly_index_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dispose_poly_index_tc_d ( idim1, idim2 )
    !! first dimension of the array "dispose_poly_index_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dispose_poly_index_tc"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=29) , parameter  :: c_upname='alloc_dispose_poly_index_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dispose_poly_index_tc ) ) CALL dealloc_dispose_poly_index_tc ( )
    ALLOCATE ( dispose_poly_index_tc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dispose_poly_index_tc(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dispose_poly_index_tc = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dispose_poly_index_tc_d
  !
  !! allocating the two-dimensional INTEGER-array "dispose_node_index_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dispose_node_index_tc_d ( idim1, idim2 )
    !! first dimension of the array "dispose_node_index_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dispose_node_index_tc"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=29) , parameter  :: c_upname='alloc_dispose_node_index_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dispose_node_index_tc ) ) CALL dealloc_dispose_node_index_tc ( )
    ALLOCATE ( dispose_node_index_tc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dispose_node_index_tc(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dispose_node_index_tc = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_dispose_node_index_tc_d
  !
  !! allocating the two-dimensional INTEGER-array "art_bed_load_poly_index" 
  !! subroutine generates error messages
  SUBROUTINE alloc_art_bed_load_poly_index_d ( idim1, idim2 )
    !! first dimension of the array "art_bed_load_index"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "art_bed_load_index"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=31) , parameter  :: c_upname='alloc_art_bed_load_poly_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( art_bed_load_poly_index ) ) CALL dealloc_art_bed_load_poly_index ( )
    ALLOCATE ( art_bed_load_poly_index(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'art_bed_load_poly_index(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       art_bed_load_poly_index = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_art_bed_load_poly_index_d
  !
  !! allocating the two-dimensional INTEGER-array "art_bed_load_node_index" 
  !! subroutine generates error messages
  SUBROUTINE alloc_art_bed_load_node_index_d ( idim1, idim2 )
    !! first dimension of the array "art_bed_load_node_index"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "art_bed_load_node_index"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=31) , parameter  :: c_upname='alloc_art_bed_load_node_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( art_bed_load_node_index ) ) CALL dealloc_art_bed_load_node_index ( )
    ALLOCATE ( art_bed_load_node_index(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'art_bed_load_node_index(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       art_bed_load_node_index = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_art_bed_load_node_index_d
  !
  !! allocating the two-dimensional CHARACTER-array "dispose_poly_name" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dispose_poly_name_d ( idim1, idim2 )
    !! first dimension of the array "dispose_poly_name"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dispose_poly_name"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=25) , parameter  :: c_upname='alloc_dispose_poly_name_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dispose_poly_name ) ) CALL dealloc_dispose_poly_name ( )
    ALLOCATE ( dispose_poly_name(idim1, idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dispose_poly_name(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dispose_poly_name = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_dispose_poly_name_d
  !
  !! allocating the two-dimensional CHARACTER-array "list_of_disp_polys" 
  !! subroutine generates error messages
  SUBROUTINE alloc_list_of_disp_polys_d ( idim1 )
    !! first dimension of the array "list_of_disp_polys"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=26) , parameter  :: c_upname='alloc_list_of_disp_polys_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( list_of_disp_polys ) ) CALL dealloc_list_of_disp_polys ( )
    ALLOCATE ( list_of_disp_polys(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'list_of_disp_polys(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       list_of_disp_polys = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_list_of_disp_polys_d
  !
  !! allocating the one-dimensional CHARACTER-array "dredge_poly_name_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_poly_name_tc_d ( idim1 )
    !! first dimension of the array "dredge_poly_name_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=27) , parameter  :: c_upname='alloc_dredge_poly_name_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredge_poly_name_tc ) ) CALL dealloc_dredge_poly_name_tc ( )
    ALLOCATE ( dredge_poly_name_tc(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredge_poly_name_tc(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       dredge_poly_name_tc = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_dredge_poly_name_tc_d
  !! allocating the one-dimensional CHARACTER-array "dispose_poly_name_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dispose_poly_name_tc_d ( idim1 )
    !! first dimension of the array "dispose_poly_name_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=28) , parameter  :: c_upname='alloc_dispose_poly_name_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dispose_poly_name_tc ) ) CALL dealloc_dispose_poly_name_tc ( )
    ALLOCATE ( dispose_poly_name_tc(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dispose_poly_name(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       dispose_poly_name_tc = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_dispose_poly_name_tc_d
  !! allocating the one-dimensional CHARACTER-array "predef_disp_poly_name" 
  !! subroutine generates error messages
  SUBROUTINE alloc_predef_disp_poly_name_d ( idim1 )
    !! first dimension of the array "predef_disp_poly_name"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=29) , parameter  :: c_upname='alloc_predef_disp_poly_name_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( predef_disp_poly_name ) ) CALL dealloc_predef_disp_poly_name ( )
    ALLOCATE ( predef_disp_poly_name(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'predef_disp_poly_name(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       predef_disp_poly_name = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_predef_disp_poly_name_d
  !! allocating the one-dimensional CHARACTER-array "predef_disp_sed_class" 
  !! subroutine generates error messages
  SUBROUTINE alloc_predef_disp_sed_class_d ( idim1, idim2 )
    !! first dimension of the array "predef_disp_sed_class"
    INTEGER          , INTENT(IN)   :: idim1 !
    !! second dimension of the array "predef_disp_sed_class"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=29) , parameter  :: c_upname='alloc_predef_disp_sed_class_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( predef_disp_sed_class ) ) CALL dealloc_predef_disp_sed_class ( )
    ALLOCATE ( predef_disp_sed_class(idim1, idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'predef_disp_sed_class(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       predef_disp_sed_class = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_predef_disp_sed_class_d
  !! allocating the one-dimensional REAL(Double)-array "dredging_rate" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredging_rate_d ( idim1 )
    !! first dimension of the array "dredging_rate"
    INTEGER          , INTENT(IN)   :: idim1 !
    !! name of the subroutine
    CHARACTER (LEN=21) , parameter  :: c_upname='alloc_dredging_rate_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredging_rate ) ) CALL dealloc_dredging_rate ( )
    ALLOCATE ( dredging_rate(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredging_rate(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       dredging_rate = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_dredging_rate_d
  !! allocating the one-dimensional REAL(Double)-array "minimum_volume"
  !! subroutine generates error messages
  SUBROUTINE alloc_minimum_volume_d ( idim1 )
    !! first dimension of the array "minimum_volume"
    INTEGER          , INTENT(IN)   :: idim1 !
    !! name of the subroutine
    CHARACTER (LEN=22) , PARAMETER  :: c_upname='alloc_minimum_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( minimum_volume ) ) CALL dealloc_minimum_volume ( )
    ALLOCATE ( minimum_volume(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'minimum_volume(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       minimum_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_minimum_volume_d
  !! allocating the one-dimensional REAL(Double)-array "sector_radius"
  !! subroutine generates error messages
  SUBROUTINE alloc_sector_radius_d ( idim1 )
    !! first dimension of the array "sector_radius"
    INTEGER          , INTENT(IN)   :: idim1 !
    !! name of the subroutine
    CHARACTER (LEN=11) , PARAMETER  :: c_upname='alloc_sector_radius_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( sector_radius ) ) CALL dealloc_sector_radius ( )
    ALLOCATE ( sector_radius(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'sector_radius(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       sector_radius = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_sector_radius_d
  !! allocating the one-dimensional REAL(Double)-array "disposal_rate" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disposal_rate_d ( idim1 )
    !! first dimension of the array "disposal_rate"
    INTEGER          , INTENT(IN)   :: idim1 !
    !! name of the subroutine
    CHARACTER (LEN=21) , parameter  :: c_upname='alloc_disposal_rate_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disposal_rate ) ) CALL dealloc_disposal_rate ( )
    ALLOCATE ( disposal_rate(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disposal_rate(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       disposal_rate = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disposal_rate_d
  !! allocating the two-dimensional REAL(Double)-array "dispose_weighting_factor" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dispose_weighting_fac_d ( idim1, idim2 )
    !! first dimension of the array "dispose_weighting_factor"
    INTEGER          , INTENT(IN)   :: idim1 !
	!! second dimension of the array "dispose_weighting_factor"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=29) , parameter  :: c_upname='alloc_dispose_weighting_fac_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dispose_weighting_factor ) ) CALL dealloc_dispose_weighting_fac ( )
    ALLOCATE ( dispose_weighting_factor(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dispose_weighting_factor(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dispose_weighting_factor = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_dispose_weighting_fac_d
  !! allocating the one-dimensional REAL(Double)-array "predef_disp_sed_vol" 
  !! subroutine generates error messages
  SUBROUTINE alloc_predef_disp_sed_vol_d ( idim1 )
    !! first dimension of the array "predef_disp_sed_vol"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=27) , parameter  :: c_upname='alloc_predef_disp_sed_vol_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( predef_disp_sed_vol ) ) CALL dealloc_predef_disp_sed_vol ( )
    ALLOCATE ( predef_disp_sed_vol(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'predef_disp_sed_vol(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       predef_disp_sed_vol = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_predef_disp_sed_vol_d
  !! allocating the one-dimensional CHARACTER-array "ini_obs_time" 
  !! subroutine generates error messages
  SUBROUTINE alloc_ini_obs_time_d ( idim1 )
    !! first dimension of the array "ini_obs_time"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=20) , parameter  :: c_upname='alloc_ini_obs_time_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( ini_obs_time ) ) CALL dealloc_ini_obs_time ( )
    ALLOCATE ( ini_obs_time(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'ini_obs_time(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       ini_obs_time = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_ini_obs_time_d
  !! allocating the one-dimensional REAL(Double)-array "observing_period" 
  !! subroutine generates error messages
  SUBROUTINE alloc_observing_period_d ( idim1 )
    !! first dimension of the array "observing_period"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24) , parameter  :: c_upname='alloc_observing_period_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( observing_period ) ) CALL dealloc_observing_period ( )
    ALLOCATE ( observing_period(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'observing_period(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       observing_period = c_undef_in
    END IF
    !
  END SUBROUTINE alloc_observing_period_d
  !! allocating the one-dimensional REAL(Double)-array "limiting_discharge"
  !! subroutine generates error messages
  SUBROUTINE alloc_limiting_discharge_d ( idim1 )
    !! first dimension of the array "limiting_discharge"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=26) , PARAMETER  :: c_upname='alloc_limiting_discharge_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( limiting_discharge ) ) CALL dealloc_limiting_discharge ( )
    ALLOCATE ( limiting_discharge(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'limiting_discharge(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       limiting_discharge = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_limiting_discharge_d
  !! allocating the one-dimensional LOGICAL-array "navigation_possible"
  !! subroutine generates error messages
  SUBROUTINE alloc_navigation_possible_d ( idim1 )
    !! first dimension of the array "navigation_possible"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=27) , PARAMETER  :: c_upname='alloc_navigation_possible_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( navigation_possible ) ) CALL dealloc_navigation_possible ( )
    ALLOCATE ( navigation_possible(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3400, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'navigation_possible(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       navigation_possible = .true.
    END IF
    !
  END SUBROUTINE alloc_navigation_possible_d
  !! allocating the one-dimensional REAL(Double)-array "dredge_sed_vol_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_sed_vol_tc_d ( idim1 )
    !! first dimension of the array "dredge_sed_vol_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=25) , parameter  :: c_upname='alloc_dregde_sed_vol_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredge_sed_vol_tc ) ) CALL dealloc_dredge_sed_vol_tc ( )
    ALLOCATE ( dredge_sed_vol_tc(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredge_sed_vol_tc(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       dredge_sed_vol_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_dredge_sed_vol_tc_d
  !! allocating the one-dimensional REAL(Double)-array "dispose_sed_vol_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dispose_sed_vol_tc_d ( idim1 )
    !! first dimension of the array "dispose_sed_vol_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=26) , parameter  :: c_upname='alloc_dispose_sed_vol_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dispose_sed_vol_tc ) ) CALL dealloc_dispose_sed_vol_tc ( )
    ALLOCATE ( dispose_sed_vol_tc(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dispose_sed_vol_tc(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       dispose_sed_vol_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_dispose_sed_vol_tc_d
  !! allocating the one-dimensional REAL(Double)-array "predef_sed_distrib" 
  !! subroutine generates error messages
  SUBROUTINE alloc_predef_sed_distrib_d ( idim1, idim2 )
    !! first dimension of the array "predef_sed_distrib"
    INTEGER          , INTENT(IN)   :: idim1 !
	!! second dimension of the array "predef_sed_distrib"
    INTEGER          , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=26) , parameter  :: c_upname='alloc_predef_sed_distrib_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( predef_sed_distrib ) ) CALL dealloc_predef_sed_distrib ( )
    ALLOCATE ( predef_sed_distrib(idim1, idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'predef_sed_distrib(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
	   WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       predef_sed_distrib = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_predef_sed_distrib_d
  !! allocating the two-dimensional CHARACTER-array "predef_dredge_time_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_predef_dredge_time_tc_d ( idim1 )
    !! first dimension of the array "predef_dredge_time_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=29) , parameter  :: c_upname='alloc_predef_dredge_time_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( predef_dredge_time_tc ) ) CALL dealloc_predef_dredge_time_tc ( )
    ALLOCATE ( predef_dredge_time_tc(idim1,2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'predef_dredge_time_tc(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       predef_dredge_time_tc = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_predef_dredge_time_tc_d
  !! allocating the two-dimensional CHARACTER-array "predef_disp_time_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_predef_disp_time_tc_d ( idim1 )
    !! first dimension of the array "predef_disp_time_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=27) , parameter  :: c_upname='alloc_predef_disp_time_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( predef_disp_time_tc ) ) CALL dealloc_predef_disp_time_tc ( )
    ALLOCATE ( predef_disp_time_tc(idim1,2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'predef_disp_time_tc(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       predef_disp_time_tc = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_predef_disp_time_tc_d
  !! allocating the two-dimensional CHARACTER-array "predef_depos_time" 
  !! subroutine generates error messages
  SUBROUTINE alloc_predef_depos_time_d ( idim1 )
    !! first dimension of the array "predef_depos_time"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=25) , parameter  :: c_upname='alloc_predef_depos_time_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( predef_depos_time ) ) CALL dealloc_predef_depos_time ( )
    ALLOCATE ( predef_depos_time(idim1,2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'predef_depos_time(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       predef_depos_time = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_predef_depos_time_d
  !
  !! allocating the one-dimensional CHARACTER-array "disp_scours_auto" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_scours_auto_d ( idim1 )
    !! first dimension of the array "disp_scours_auto"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24) , parameter  :: c_upname='alloc_disp_scours_auto_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char !
    !
    IF ( ALLOCATED( disp_scours_auto ) ) CALL dealloc_disp_scours_auto ( )
    ALLOCATE ( disp_scours_auto(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_scours_auto(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       disp_scours_auto = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_disp_scours_auto_d

!LEO new routine check if disp_scours_auto is allocated
  FUNCTION check_allocated_disp_scours_d ( ) &
       RESULT( res )
    !! result: number of dredge polygons
    INTEGER :: res ! 
    !
    IF ( ALLOCATED(disp_scours_auto)) THEN
       res = 1
    ELSE
       res = 0
    END IF
    !
  END FUNCTION check_allocated_disp_scours_d
!LEO  ENDE 
  !
  !! allocating the one-dimensional CHARACTER-array "disp_scours_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_scours_tc_d ( idim1 )
    !! first dimension of the array "disp_scours_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=22) , parameter  :: c_upname='alloc_disp_scours_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char !
    !
    IF ( ALLOCATED( disp_scours_tc ) ) CALL dealloc_disp_scours_tc ( )
    ALLOCATE ( disp_scours_tc(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_scours_tc(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       disp_scours_tc = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_disp_scours_tc_d
  !
  !! allocating the one-dimensional CHARACTER-array "disp_scours_abl" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_scours_abl_d ( idim1 )
    !! first dimension of the array "disp_scours_abl"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=23) , parameter  :: c_upname='alloc_disp_scours_abl_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char !
    !
    IF ( ALLOCATED( disp_scours_abl ) ) CALL dealloc_disp_scours_abl ( )
    ALLOCATE ( disp_scours_abl(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_scours_abl(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       disp_scours_abl = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_disp_scours_abl_d  
  !
  !! allocating the two-dimensional REAL(Double)-array "poly_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_poly_total_volume_d ( idim1, idim2 )
    !! first dimension of the array "poly_total_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "poly_total_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='alloc_poly_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( poly_total_volume ) ) CALL dealloc_poly_total_volume ( )
    ALLOCATE ( poly_total_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'poly_total_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       poly_total_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_poly_total_volume_d
  !
  !! allocating the two-dimensional REAL(Double)-array "node_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_total_volume_d ( idim1, idim2 )
    !! first dimension of the array "node_total_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "node_total_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='alloc_node_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_total_volume ) ) CALL dealloc_node_total_volume ( )
    ALLOCATE ( node_total_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_total_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       node_total_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_total_volume_d
  !
  !! allocating the three-dimensional REAL(Double)-array "disp_node_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_node_total_volume_d ( idim1, idim2, idim3 )
    !! first dimension of the array "disp_node_total_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_node_total_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "disp_node_total_volume"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='alloc_disp_node_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_node_total_volume ) ) CALL dealloc_disp_node_tot_volume ( )
    ALLOCATE ( disp_node_total_volume(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_node_total_volume(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       disp_node_total_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_node_total_volume_d
  !
  !! allocating the one-dimensional REAL(Double)-array "total_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_total_volume_d ( idim1 )
    !! first dimension of the array "total_volume"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=20) , parameter  :: c_upname='alloc_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( total_volume ) ) CALL dealloc_total_volume ( )
    ALLOCATE ( total_volume(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'total_volume(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       total_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_total_volume_d
  !
  !! allocating the one-dimensional REAL(Double)-array "all_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_all_total_volume_d ( idim1 )
    !! first dimension of the array "all_total_volume"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24) , parameter  :: c_upname='alloc_all_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( all_total_volume ) ) CALL dealloc_all_total_volume ( )
    ALLOCATE ( all_total_volume(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'all_total_volume(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       all_total_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_all_total_volume_d
  !
  !! allocating the one-dimensional REAL(Double)-array "all_total_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE alloc_all_total_volume_old_d ( idim1 )
    !! first dimension of the array "all_total_volume_old"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=28) , parameter  :: c_upname='alloc_all_total_volume_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( all_total_volume_old ) ) CALL dealloc_all_total_volume_old ( )
    ALLOCATE ( all_total_volume_old(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'all_total_volume_old(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       all_total_volume_old = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_all_total_volume_old_d
  !
  !! allocating the two-dimensional REAL(Double)-array "poly_sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_poly_sediment_volume_d ( idim1, idim2 )
    !! first dimension of the array "poly_sediment_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "poly_sediment_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='alloc_poly_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( poly_sediment_volume ) ) CALL dealloc_poly_sediment_volume ( )
    ALLOCATE ( poly_sediment_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'poly_sediment_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       poly_sediment_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_poly_sediment_volume_d
  !
  !! allocating the two-dimensional REAL(Double)-array "node_sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_sediment_volume_d ( idim1, idim2 )
    !! first dimension of the array "node_sediment_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "node_sediment_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='alloc_node_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_sediment_volume ) ) CALL dealloc_node_sediment_volume ( )
    ALLOCATE ( node_sediment_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_sediment_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       node_sediment_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_sediment_volume_d
  !
  !! allocating the two-dimensional REAL(Double)-array "node_sediment_volume_rs" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_sediment_volume_rs_d ( idim1, idim2 )
    !! first dimension of the array "node_sediment_volume_rs"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "node_sediment_volume_rs"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_node_sediment_volume_rs_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_sediment_volume_rs ) ) CALL dealloc_node_sediment_volume_rs ( )
    ALLOCATE ( node_sediment_volume_rs(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_sediment_volume_rs(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       node_sediment_volume_rs = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_sediment_volume_rs_d
  !
  !! allocating the one-dimensional CHARACTER-array "last_obs_time_rs" 
  !! subroutine generates error messages
  SUBROUTINE alloc_last_obs_time_rs_d ( idim1 )
    !! first dimension of the array "last_obs_time_rs"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24), parameter  :: c_upname='last_obs_time_rs_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( last_obs_time_rs ) ) CALL dealloc_last_obs_time_rs ( )
    ALLOCATE ( last_obs_time_rs(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'last_obs_time_rs(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       last_obs_time_rs = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_last_obs_time_rs_d
  !
  !! allocating the one-dimensional CHARACTER-array "next_obs_time_rs" 
  !! subroutine generates error messages
  SUBROUTINE alloc_next_obs_time_rs_d ( idim1 )
    !! first dimension of the array "next_obs_time_rs"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24), parameter  :: c_upname='next_obs_time_rs_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( next_obs_time_rs ) ) CALL dealloc_next_obs_time_rs ( )
    ALLOCATE ( next_obs_time_rs(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'next_obs_time_rs(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       next_obs_time_rs = c_undef_ch
    END IF
    !
  END SUBROUTINE alloc_next_obs_time_rs_d
  !
  !! allocating the one-dimensional REAL(Double)-array "sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_sediment_volume_d ( idim1 )
    !! first dimension of the array "sediment_volume"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=23) , parameter  :: c_upname='alloc_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED (sediment_volume ) ) CALL dealloc_sediment_volume ( )
    ALLOCATE ( sediment_volume(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'sediment_volume(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       sediment_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_sediment_volume_d
  !
  !! allocating the one-dimensional REAL(Double)-array "all_sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_all_sediment_volume_d ( idim1 )
    !! first dimension of the array "all_sediment_volume"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=27) , parameter  :: c_upname='alloc_all_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( all_sediment_volume ) ) CALL dealloc_all_sediment_volume ( )
    ALLOCATE ( all_sediment_volume(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'all_sediment_volume(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       all_sediment_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_all_sediment_volume_d
  !
  !! allocating the one-dimensional REAL(Double)-array "all_sediment_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE alloc_all_sediment_volume_old_d ( idim1 )
    !! first dimension of the array "all_sediment_volume_old"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=31) , parameter  :: c_upname='alloc_all_sediment_volume_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( all_sediment_volume_old ) ) CALL dealloc_all_sediment_volume_old ( )
    ALLOCATE ( all_sediment_volume_old(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'all_sediment_volume_old(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       all_sediment_volume_old = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_all_sediment_volume_old_d
  !
  !! allocating the two-dimensional REAL(Double)-array "node_sediment_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_sediment_volume_tc_d ( idim1, idim2 )
    !! first dimension of the array "node_sediment_volume_tc"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "node_sediment_volume_tc"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_node_sediment_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_sediment_volume_tc ) ) CALL dealloc_node_sediment_volume_tc ( )
    ALLOCATE ( node_sediment_volume_tc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_sediment_volume_tc(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       node_sediment_volume_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_sediment_volume_tc_d
  !
  !! allocating the one-dimensional REAL(Double)-array "sediment_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_sediment_volume_tc_d ( idim1 )
    !! first dimension of the array "sediment_volume_tc"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=26) , parameter  :: c_upname='alloc_sediment_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( sediment_volume_tc ) ) CALL dealloc_sediment_volume_tc ( )
    ALLOCATE ( sediment_volume_tc(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'sediment_volume_tc(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       sediment_volume_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_sediment_volume_tc_d
  !
  !! allocating the two-dimensional REAL(Double)-array "poly_water_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_poly_water_volume_d ( idim1, idim2 )
    !! first dimension of the array "poly_water_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "poly_water_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='alloc_poly_water_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( poly_water_volume ) ) CALL dealloc_poly_water_volume ( )
    ALLOCATE ( poly_water_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'poly_water_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       poly_water_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_poly_water_volume_d
  !
  !! allocating the two-dimensional REAL(Double)-array "node_water_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_water_volume_d ( idim1, idim2 )
    !! first dimension of the array "node_water_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "node_water_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='alloc_node_water_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_water_volume ) ) CALL dealloc_node_water_volume ( )
    ALLOCATE ( node_water_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_water_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       node_water_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_water_volume_d
  !
  !! allocating the one-dimensional REAL(Double)-array "water_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_water_volume_d ( idim1 )
    !! first dimension of the array "water_volume"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=20) , parameter  :: c_upname='alloc_water_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( water_volume ) ) CALL dealloc_water_volume ( )
    ALLOCATE ( water_volume(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'water_volume(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       water_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_water_volume_d
  !
  !! allocating the one-dimensional REAL(Double)-array "all_water_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_all_water_volume_d ( idim1 )
    !! first dimension of the array "all_water_volume"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24) , parameter  :: c_upname='alloc_all_water_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( all_water_volume ) ) CALL dealloc_all_water_volume ( )
    ALLOCATE ( all_water_volume(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'all_water_volume(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       all_water_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_all_water_volume_d
  !
  !! allocating the one-dimensional REAL(Double)-array "all_water_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE alloc_all_water_volume_old_d ( idim1 )
    !! first dimension of the array "all_water_volume_old"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24) , parameter  :: c_upname='alloc_all_water_volume_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( all_water_volume_old ) ) CALL dealloc_all_water_volume_old ( )
    ALLOCATE ( all_water_volume_old(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'all_water_volume_old(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       all_water_volume_old = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_all_water_volume_old_d
  !
  !! allocating the three-dimensional REAL(Double)-array "poly_fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_poly_fraction_volume_d ( idim1, idim2, idim3 )
    !! first dimension of the array "poly_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "poly_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "poly_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='alloc_poly_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( poly_fraction_volume ) ) CALL dealloc_poly_fraction_volume ( )
    ALLOCATE ( poly_fraction_volume(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3220, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'poly_fraction_volume(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       poly_fraction_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_poly_fraction_volume_d
  !
  !! allocating the three-dimensional REAL(Double)-array "node_fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_fraction_volume_d ( idim1, idim2, idim3 )
    !! first dimension of the array "node_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "node_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "node_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='alloc_node_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_fraction_volume ) ) CALL dealloc_node_fraction_volume ( )
    ALLOCATE ( node_fraction_volume(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3220, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_fraction_volume(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       node_fraction_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_fraction_volume_d
  !
  !! allocating the three-dimensional REAL(Double)-array "node_fraction_volume_rs" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_fraction_volume_rs_d ( idim1, idim2, idim3 )
    !! first dimension of the array "node_fraction_volume_rs"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "node_fraction_volume_rs"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "node_fraction_volume_rs"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_node_fraction_volume_rs_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_fraction_volume_rs ) ) CALL dealloc_node_fraction_volume_rs ( )
    ALLOCATE ( node_fraction_volume_rs(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3220, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_fraction_volume_rs(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       node_fraction_volume_rs = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_fraction_volume_rs_d
  !
  !! allocating the two-dimensional REAL(Double)-array "fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_fraction_volume_d ( idim1, idim2 )
    !! first dimension of the array "fraction_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "fraction_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=23), parameter  :: c_upname='alloc_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( fraction_volume ) ) CALL dealloc_fraction_volume ( )
    ALLOCATE ( fraction_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'fraction_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       fraction_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_fraction_volume_d
  !
  !! allocating the two-dimensional REAL(Double)-array "all_fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_all_fraction_volume_d ( idim1, idim2 )
    !! first dimension of the array "all_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "all_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='alloc_all_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( all_fraction_volume ) ) CALL dealloc_all_fraction_volume ( )
    ALLOCATE ( all_fraction_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'all_fraction_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       all_fraction_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_all_fraction_volume_d
  !
  !! allocating the two-dimensional REAL(Double)-array "all_fraction_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE alloc_all_fraction_volume_old_d ( idim1, idim2 )
    !! first dimension of the array "all_fraction_volume_old"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "all_fraction_volume_old"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_all_fraction_volume_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( all_fraction_volume_old ) ) CALL dealloc_all_fraction_volume_old ( )
    ALLOCATE ( all_fraction_volume_old(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'all_fraction_volume_old(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       all_fraction_volume_old = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_all_fraction_volume_old_d
  !
  !! allocating the three-dimensional REAL(Double)-array "poly_fraction_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_poly_fraction_volume_tc_d ( idim1, idim2, idim3 )
    !! first dimension of the array "poly_fraction_volume_tc"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "poly_fraction_volume_tc"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "poly_fraction_volume_tc"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_poly_fraction_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( poly_fraction_volume_tc ) ) CALL dealloc_poly_frac_volume_tc ( )
    ALLOCATE ( poly_fraction_volume_tc(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3220, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'poly_fraction_volume_tc(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       poly_fraction_volume_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_poly_fraction_volume_tc_d
  !
  !! allocating the three-dimensional REAL(Double)-array "node_fraction_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_fraction_volume_tc_d ( idim1, idim2, idim3 )
    !! first dimension of the array "node_fraction_volume_tc"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "node_fraction_volume_tc"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "node_fraction_volume_tc"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_node_fraction_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_fraction_volume_tc ) ) CALL dealloc_node_frac_volume_tc ( )
    ALLOCATE ( node_fraction_volume_tc(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3220, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_fraction_volume_tc(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       node_fraction_volume_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_fraction_volume_tc_d
  !
  !! allocating the two-dimensional REAL(Double)-array "fraction_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_fraction_volume_tc_d ( idim1, idim2 )
    !! first dimension of the array "fraction_volume_tc"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "fraction_volume_tc"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='alloc_fraction_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( fraction_volume_tc ) ) CALL dealloc_fraction_volume_tc ( )
    ALLOCATE ( fraction_volume_tc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'fraction_volume_tc(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       fraction_volume_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_fraction_volume_tc_d
  !
  !! allocating the one-dimensional LOGICAL-array "upd_out_volumes" 
  !! subroutine generates error messages
  SUBROUTINE alloc_upd_out_volumes_d ( idim1 )
    !! first dimension of the array "upd_out_volumes"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=23), parameter  :: c_upname='alloc_upd_out_volumes_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( upd_out_volumes ) ) CALL dealloc_upd_out_volumes ( )
    ALLOCATE ( upd_out_volumes(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3400, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'upd_out_volumes(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       upd_out_volumes = .false.
    END IF
    !
  END SUBROUTINE alloc_upd_out_volumes_d
  !
  !! allocating the two-dimensional REAL(Double)-array "aim_poly_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_aim_poly_depth_d ( idim1, idim2 )
    !! first dimension of the array "aim_poly_depth"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "aim_poly_depth"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=22), parameter  :: c_upname='alloc_aim_poly_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( aim_poly_depth ) ) CALL dealloc_aim_poly_depth ( )
    ALLOCATE ( aim_poly_depth(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'aim_poly_depth(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       aim_poly_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_aim_poly_depth_d
  !
  !! allocating the two-dimensional REAL(Double)-array "aim_node_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_aim_node_depth_d ( idim1, idim2 )
    !! first dimension of the array "aim_node_depth"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "aim_node_depth"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=22), parameter  :: c_upname='alloc_aim_node_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( aim_node_depth ) ) CALL dealloc_aim_node_depth ( )
    ALLOCATE ( aim_node_depth(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'aim_node_depth(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       aim_node_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_aim_node_depth_d
  !
  !! allocating the two-dimensional REAL(Double)-array "aim_node_depth_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_aim_node_depth_tc_d ( idim1, idim2 )
    !! first dimension of the array "aim_node_depth_tc"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "aim_node_depth_tc"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='alloc_aim_node_depth_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( aim_node_depth_tc ) ) CALL dealloc_aim_node_depth_tc ( )
    ALLOCATE ( aim_node_depth_tc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'aim_node_depth_tc(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       aim_node_depth_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_aim_node_depth_tc_d
  !
  !! allocating the two-dimensional REAL(Double)-array "aim_node_depth_abl" 
  !! subroutine generates error messages
  SUBROUTINE alloc_aim_node_depth_abl_d ( idim1, idim2 )
    !! first dimension of the array "aim_node_depth_abl"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "aim_node_depth_abl"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='alloc_aim_node_depth_abl_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( aim_node_depth_abl ) ) CALL dealloc_aim_node_depth_abl ( )
    ALLOCATE ( aim_node_depth_abl(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'aim_node_depth_abl(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       aim_node_depth_abl = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_aim_node_depth_abl_d
  !
  !! allocating the two-dimensional REAL(Double)-array "delta_dredge_poly_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_delta_dredge_poly_depth_d ( idim1, idim2 )
    !! first dimension of the array "delta_dredge_poly_depth"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "delta_dredge_poly_depth"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_delta_dredge_poly_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( delta_dredge_poly_depth ) ) CALL dealloc_delta_dredge_poly_depth ( )
    ALLOCATE ( delta_dredge_poly_depth(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'delta_dredge_poly_depth(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       delta_dredge_poly_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_delta_dredge_poly_depth_d
  !
  !! allocating the two-dimensional REAL(Double)-array "delta_dredge_node_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_delta_dredge_node_depth_d ( idim1, idim2 )
    !! first dimension of the array "delta_dredge_node_depth"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "delta_dredge_node_depth"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_delta_dredge_node_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( delta_dredge_node_depth ) ) CALL dealloc_delta_dredge_node_depth ( )
    ALLOCATE ( delta_dredge_node_depth(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'delta_dredge_node_depth(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       delta_dredge_node_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_delta_dredge_node_depth_d
  !
  !! allocating the two-dimensional REAL(Double)-array "delta_disp_poly_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_delta_disp_poly_depth_d ( idim1, idim2 )
    !! first dimension of the array "delta_disp_poly_depth"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "delta_disp_poly_depth"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='alloc_delta_disp_poly_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( delta_disp_poly_depth ) ) CALL dealloc_delta_disp_poly_depth ( )
    ALLOCATE ( delta_disp_poly_depth(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'delta_disp_poly_depth(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       delta_disp_poly_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_delta_disp_poly_depth_d
  !
  !! allocating the three-dimensional REAL(Double)-array "delta_disp_node_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_delta_disp_node_depth_d ( idim1, idim2, idim3 )
    !! first dimension of the array "delta_disp_node_depth"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "delta_disp_node_depth"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "delta_disp_node_depth"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='alloc_delta_disp_node_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( delta_disp_node_depth ) ) CALL dealloc_delta_disp_node_depth ( )
    ALLOCATE ( delta_disp_node_depth(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'delta_disp_node_depth(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       delta_disp_node_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_delta_disp_node_depth_d
  !
  !! allocating the two-dimensional REAL(Double)-array "disp_poly_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_poly_depth_d ( idim1, idim2 )
    !! first dimension of the array "disp_poly_depth"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_poly_depth"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=23), parameter  :: c_upname='alloc_disp_poly_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_poly_depth ) ) CALL dealloc_disp_poly_depth ( )
    ALLOCATE ( disp_poly_depth(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_poly_depth(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       disp_poly_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_poly_depth_d
  !
  !! allocating the three-dimensional REAL(Double)-array "disp_node_depth" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_node_depth_d ( idim1, idim2, idim3 )
    !! first dimension of the array "disp_node_depth"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_node_depth"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "disp_node_depth"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=23), parameter  :: c_upname='alloc_disp_node_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_node_depth ) ) CALL dealloc_disp_node_depth ( )
    ALLOCATE ( disp_node_depth(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_node_depth(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       disp_node_depth = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_node_depth_d
  !
  !! allocating the one-dimensional REAL(Double)-array "dzb_dredge" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dzb_dredge_d ( idim1 )
    !! first dimension of the array "dzb_dredge"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=18), parameter  :: c_upname='alloc_dzb_dredge_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dzb_dredge ) ) CALL dealloc_dzb_dredge ( )
    ALLOCATE ( dzb_dredge(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dzb_dredge(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       dzb_dredge = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_dzb_dredge_d
  !
  !! allocating the one-dimensional REAL(Double)-array "dzb_disp" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dzb_disp_d ( idim1 )
    !! first dimension of the array "dzb_disp"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=16), parameter  :: c_upname='alloc_dzb_disp_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dzb_disp ) ) CALL dealloc_dzb_disp ( )
    ALLOCATE ( dzb_disp(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dzb_disp(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       dzb_disp = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_dzb_disp_d
  !
  !! allocating the two-dimensional REAL(Double)-array "dredged_sediment_volume_to_disp" 
  !! subroutine generates error messages
  SUBROUTINE alloc_sediment_volume_to_disp_d ( idim1, idim2 )
    !! first dimension of the array "dredged_sediment_volume_to_disp"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dredged_sediment_volume_to_disp"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_sediment_volume_to_disp_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredged_sediment_volume_to_disp ) ) CALL dealloc_sed_volume_to_disp ( )
    ALLOCATE ( dredged_sediment_volume_to_disp(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredged_sediment_volume_to_disp(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       dredged_sediment_volume_to_disp = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_sediment_volume_to_disp_d
  !
  !! allocating the three-dimensional REAL(Double)-array "dredged_fraction_volume_to_disp" 
  !! subroutine generates error messages
  SUBROUTINE alloc_fraction_volume_to_disp_d ( idim1, idim2, idim3 )
    !! first dimension of the array "dredged_fraction_volume_to_disp"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "dredged_fraction_volume_to_disp"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "dredged_fraction_volume_to_disp"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='alloc_fraction_volume_to_disp_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredged_fraction_volume_to_disp ) ) CALL dealloc_frac_volume_to_disp ( )
    ALLOCATE ( dredged_fraction_volume_to_disp(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredged_fraction_volume_to_disp(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim3>', l_char )
    ELSE
       dredged_fraction_volume_to_disp = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_fraction_volume_to_disp_d
  !
  !! allocating the two-dimensional REAL(Double)-array "disp_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_total_volume_d ( idim1, idim2 )
    !! first dimension of the array "disp_total_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_total_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='alloc_disp_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_total_volume ) ) CALL dealloc_disp_total_volume ( )
    ALLOCATE ( disp_total_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_total_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       disp_total_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_total_volume_d
  !
  !! allocating the two-dimensional REAL(Double)-array "disp_total_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_total_volume_old_d ( idim1, idim2 )
    !! first dimension of the array "disp_total_volume_old"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_total_volume_old"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='alloc_disp_total_volume_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_total_volume_old ) ) CALL dealloc_disp_total_volume_old ( )
    ALLOCATE ( disp_total_volume_old(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_total_volume_old(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       disp_total_volume_old = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_total_volume_old_d
  !
  !! allocating the two-dimensional REAL(Double)-array "disp_sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_sediment_volume_d ( idim1, idim2 )
    !! first dimension of the array "disp_sediment_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_sediment_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='alloc_disp_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_sediment_volume ) ) CALL dealloc_disp_sediment_volume ( )
    ALLOCATE ( disp_sediment_volume(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_sediment_volume(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       disp_sediment_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_sediment_volume_d
  !
  !! allocating the two-dimensional REAL(Double)-array "disp_sediment_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_sediment_vol_old_d ( idim1, idim2 )
    !! first dimension of the array "disp_sediment_volume_old"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_sediment_volume_old"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='alloc_disp_sediment_vol_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_sediment_volume_old ) ) CALL dealloc_disp_sediment_volume_old ( )
    ALLOCATE ( disp_sediment_volume_old(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_sediment_volume_old(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       disp_sediment_volume_old = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_sediment_vol_old_d
  !
  !! allocating the three-dimensional REAL(Double)-array "disp_fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_fraction_volume_d ( idim1, idim2, idim3 )
    !! first dimension of the array "disp_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "disp_fraction_volume"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='alloc_disp_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_fraction_volume ) ) CALL dealloc_disp_fraction_volume ( )
    ALLOCATE ( disp_fraction_volume(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_fraction_volume(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       disp_fraction_volume = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_fraction_volume_d
  !
  !! allocating the three-dimensional REAL(Double)-array "disp_fraction_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE alloc_disp_fraction_vol_old_d ( idim1, idim2, idim3 )
    !! first dimension of the array "disp_fraction_volume_old"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "disp_fraction_volume_old"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! third dimension of the array "disp_fraction_volume_old"
    INTEGER         , INTENT(IN)   :: idim3 ! 
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='alloc_disp_fraction_vol_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( disp_fraction_volume_old ) ) CALL dealloc_disp_fraction_volume_old ( )
    ALLOCATE ( disp_fraction_volume_old(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'disp_fraction_volume_old(:,:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
       WRITE(l_char,'(I10)') idim3 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       disp_fraction_volume_old = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_disp_fraction_vol_old_d
  !
  !! allocating the two-dimensional REAL(Double)-array "delta_disp_node_depth_tc" 
  !! subroutine generates error messages
  SUBROUTINE alloc_deltadispnodedepthtc_d ( idim1, idim2 )
    !! first dimension of the array "delta_disp_node_depth_tc"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "delta_disp_node_depth_tc"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='alloc_deltadispnodedepthtc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( delta_disp_node_depth_tc ) ) CALL dealloc_delta_disp_nodedepth_tc ( )
    ALLOCATE ( delta_disp_node_depth_tc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'delta_disp_node_depth_tc(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       delta_disp_node_depth_tc = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_deltadispnodedepthtc_d
  !
  !! allocating the two-dimensional REAL(Double)-array "delta_node_depth_abl" 
  !! subroutine generates error messages
  SUBROUTINE alloc_delta_node_depth_abl_d ( idim1, idim2 )
    !! first dimension of the array "delta_node_depth_abl"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! second dimension of the array "delta_node_depth_abl"
    INTEGER         , INTENT(IN)   :: idim2 ! 
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='alloc_delta_node_depth_abl_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( delta_node_depth_abl ) ) CALL dealloc_delta_node_depth_abl ( )
    ALLOCATE ( delta_node_depth_abl(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3210, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'delta_node_depth_abl(:,:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
       WRITE(l_char,'(I10)') idim2 ; CALL setup_error_act ( '<idim2>', l_char )
    ELSE
       delta_node_depth_abl = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_delta_node_depth_abl_d
  !
  !! allocating the one-dimensional FILE-array "dredge_criterion" 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_criterion_d ( idim1 )
    !! first dimension of the array "<arr_fi_1>"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=24), parameter  :: c_upname='alloc_dredge_criterion_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( dredge_criterion ) ) CALL dealloc_dredge_criterion ( )
    ALLOCATE ( dredge_criterion(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3300, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'dredge_criterion(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       CALL new_criterion ( dredge_criterion )
    END IF
    !
  END SUBROUTINE alloc_dredge_criterion_d
  !
  !! allocating the one-dimensional REAL(Double)-array "edge_related_surface" 
  !! subroutine generates error messages
  SUBROUTINE alloc_edge_related_surface_d ( idim1 )
    !! first dimension of the array "edge_related_surface"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=28) , parameter  :: c_upname='alloc_edge_related_surface_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( edge_related_surface ) ) CALL dealloc_edge_related_surface ( )
    ALLOCATE ( edge_related_surface(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'edge_related_surface(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       edge_related_surface = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_edge_related_surface_d
  !
  !! allocating the one-dimensional REAL(Double)-array "edge_weighted_sum" 
  !! subroutine generates error messages
  SUBROUTINE alloc_edge_weighted_sum_d ( idim1 )
    !! first dimension of the array "edge_weighted_sum"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=25) , parameter  :: c_upname='alloc_edge_weighted_sum_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( edge_weighted_sum ) ) CALL dealloc_edge_weighted_sum ( )
    ALLOCATE ( edge_weighted_sum(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'edge_weighted_sum(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       edge_weighted_sum = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_edge_weighted_sum_d
  !
  !! allocating the one-dimensional REAL(Double)-array "node_related_surface" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_related_surface_d ( idim1 )
    !! first dimension of the array "node_related_surface"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=28) , parameter  :: c_upname='alloc_node_related_surface_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_related_surface ) ) CALL dealloc_node_related_surface ( )
    ALLOCATE ( node_related_surface(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_related_surface(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       node_related_surface = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_related_surface_d
  !
  !! allocating the one-dimensional REAL(Double)-array "node_weighted_sum" 
  !! subroutine generates error messages
  SUBROUTINE alloc_node_weighted_sum_d ( idim1 )
    !! first dimension of the array "node_weighted_sum"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=25) , parameter  :: c_upname='alloc_node_weighted_sum_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( node_weighted_sum ) ) CALL dealloc_node_weighted_sum ( )
    ALLOCATE ( node_weighted_sum(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'node_weighted_sum(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       node_weighted_sum = c_undef_dp
    END IF
    !
  END SUBROUTINE alloc_node_weighted_sum_d
  !
  !! allocating the array start_dredge_time 
  !! subroutine generates error messages
  SUBROUTINE alloc_start_dredge_time_d &
       ( idim )
    !
    !! number of dredge polygons for start_dredge_time
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=25) , parameter  :: c_upname='alloc_start_dredge_time_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_start_dredge_time ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( start_dredge_time(idim), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'start_dredge_time' )
       ELSE
          CALL new_datetime ( start_dredge_time(:) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_start_dredge_time_d
  !! allocating the array end_dredge_time 
  !! subroutine generates error messages
  SUBROUTINE alloc_end_dredge_time_d &
       ( idim )
    !
    !! number of dredge polygons for end_dredge_time
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=23) , parameter  :: c_upname='alloc_end_dredge_time_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_end_dredge_time ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( end_dredge_time(idim), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'end_dredge_time' )
       ELSE
          CALL new_datetime ( end_dredge_time(:) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_end_dredge_time_d
  !! allocating the array time_to_observe 
  !! subroutine generates error messages
  SUBROUTINE alloc_time_to_observe_d &
       ( idim )
    !
    !! number of dredge polygons for time_to_observe
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=23) , parameter  :: c_upname='alloc_time_to_observe_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_time_to_observe ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( time_to_observe(idim), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'time_to_observe' )
       ELSE
          CALL new_datetime ( time_to_observe(:) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_time_to_observe_d
  !! allocating the array old_time_to_observe 
  !! subroutine generates error messages
  SUBROUTINE alloc_old_time_to_observe_d &
       ( idim )
    !
    !! number of dredge polygons for old_time_to_observe
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=27) , parameter  :: c_upname='alloc_old_time_to_observe_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_old_time_to_observe ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( old_time_to_observe(idim), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'old_time_to_observe' )
       ELSE
          CALL new_datetime ( old_time_to_observe(:) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_old_time_to_observe_d
  !! allocating the array initial_time_to_observe 
  !! subroutine generates error messages
  SUBROUTINE alloc_ini_time_to_observe_d &
       ( idim )
    !
    !! number of dredge polygons for initial_time_to_observe
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=27) , parameter  :: c_upname='alloc_ini_time_to_observe_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_ini_time_to_observe ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( initial_time_to_observe(idim), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'initial_time_to_observe' )
       ELSE
          CALL new_datetime ( initial_time_to_observe(:) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_ini_time_to_observe_d
  !! allocating the array dredge_time_tc 
  !! subroutine generates error messages
  SUBROUTINE alloc_dredge_time_tc_d &
       ( idim )
    !
    !! number of dredge polygons for dredge_time_tc
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=22) , parameter  :: c_upname='alloc_dredge_time_tc_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_dredge_time_tc ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( dredge_time_tc(idim, 2), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'dredge_time_tc' )
       ELSE
          CALL new_datetime ( dredge_time_tc(:,2) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_dredge_time_tc_d
  !! allocating the array disposal_time_tc 
  !! subroutine generates error messages
  SUBROUTINE alloc_disposal_time_tc_d &
       ( idim )
    !
    !! number of dredge polygons for disposal_time_tc
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=24) , parameter  :: c_upname='alloc_disposal_time_tc_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_disposal_time_tc ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( disposal_time_tc(idim, 2), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'disposal_time_tc' )
       ELSE
          CALL new_datetime ( disposal_time_tc(:,2) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_disposal_time_tc_d
  !! allocating the array art_bl_time 
  !! subroutine generates error messages
  SUBROUTINE alloc_art_bl_time_d &
       ( idim )
    !
    !! number of dredge polygons for art_bl_time
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=19) , parameter  :: c_upname='alloc_art_bl_time_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_art_bl_time ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( art_bl_time(idim, 2), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'art_bl_time' )
       ELSE
          CALL new_datetime ( art_bl_time(:,2) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_art_bl_time_d
  !! allocating the array used_sediment_classes 
  !! subroutine generates error messages
  SUBROUTINE alloc_used_sediment_classes_d &
       ( idim )
    !
    !! number of sediment classes for used_sediment_classes
    INTEGER            , INTENT(IN) :: idim ! 
    CHARACTER (LEN=29) , parameter  :: c_upname='alloc_used_sediment_classes_d'
	INTEGER :: stat
	CHARACTER (LEN=10) :: l_char
    !
    CALL dealloc_used_sediment_classes ( )
    IF ( no_error( ) ) THEN
       ALLOCATE ( used_sediment_classes(idim), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 4000, c_upname, c_modname, stat )
          WRITE(l_char,'(I10)') idim
          CALL setup_error_act ( '<arrayDim1>', l_char )
          CALL setup_error_act ( '<arrayName>', 'used_sediment_classes' )
       ELSE
          CALL new_grain ( used_sediment_classes(:) )
       END IF
    END IF
    !
  END SUBROUTINE alloc_used_sediment_classes_d
  !
  !! allocating the one-dimensional CHARACTER-array "fraction_name" 
  !! subroutine generates error messages
  SUBROUTINE alloc_fraction_name_d ( idim1 )
    !! first dimension of the array "fraction_name"
    INTEGER          , INTENT(IN)   :: idim1 ! 
    !! name of the subroutine
    CHARACTER (LEN=21) , parameter  :: c_upname='alloc_fraction_name_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    CHARACTER (LEN=10) :: l_char ! 
    !
    IF ( ALLOCATED( fraction_name ) ) CALL dealloc_fraction_name ( )
    ALLOCATE ( fraction_name(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -3200, c_upname, c_modname, stat )
       CALL setup_error_act ( '<name>', 'fraction_name(:)' )
       WRITE(l_char,'(I10)') idim1 ; CALL setup_error_act ( '<idim1>', l_char )
    ELSE
       fraction_name = REPEAT( c_undef_ch, LEN(fraction_name) )
    END IF
    !
  END SUBROUTINE alloc_fraction_name_d
  !
  ! ----------------------------------------------------------------------
  ! DEALLOCATE-methods
  ! ----------------------------------------------------------------------
  !
  !! deallocating the one-dimensional REAL(Double)-array "poly_water_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_poly_water_depth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_poly_water_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( poly_water_depth ) ) THEN
       DEALLOCATE ( poly_water_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'poly_water_depth(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_poly_water_depth_d
  !
  !! deallocating the one-dimensional INTEGER-array "dredge_crit_type" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_crit_type_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_dredge_crit_type_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredge_crit_type ) ) THEN
       DEALLOCATE ( dredge_crit_type, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredge_crit_type(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_crit_type_d
  !
  !! deallocating the one-dimensional INTEGER-array "dredge_poly_index" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_poly_index_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_dredge_poly_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredge_poly_index ) ) THEN
       DEALLOCATE ( dredge_poly_index, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredge_poly_index(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_poly_index_d
  !
  !! deallocating the one-dimensional INTEGER-array "dredge_node_index" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_node_index_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_dredge_node_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredge_node_index ) ) THEN
       DEALLOCATE ( dredge_node_index, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredge_node_index(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_node_index_d
  !
  !! deallocating the two-dimensional INTEGER-array "dispose_poly_index" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dispose_poly_index_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_dispose_poly_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dispose_poly_index ) ) THEN
       DEALLOCATE ( dispose_poly_index, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dispose_poly_index(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dispose_poly_index_d
  !
  !! deallocating the three-dimensional INTEGER-array "dispose_node_index" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dispose_node_index_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_dispose_node_index_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dispose_node_index ) ) THEN
       DEALLOCATE ( dispose_node_index, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dispose_node_index(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dispose_node_index_d
  !
  !! deallocating the two-dimensional INTEGER-array "dredge_poly_index_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_poly_index_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_dredge_poly_index_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredge_poly_index_tc ) ) THEN
       DEALLOCATE ( dredge_poly_index_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredge_poly_index_tc(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_poly_index_tc_d
  !
  !! deallocating the two-dimensional INTEGER-array "dredge_node_index_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_node_index_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_dredge_node_index_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredge_node_index_tc ) ) THEN
       DEALLOCATE ( dredge_node_index_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredge_node_index_tc(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_node_index_tc_d
  !
  !! deallocating the two-dimensional INTEGER-array "dispose_poly_index_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dispose_poly_index_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_dispose_poly_index_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dispose_poly_index_tc ) ) THEN
       DEALLOCATE ( dispose_poly_index_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dispose_poly_index_tc(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dispose_poly_index_tc_d
  !
  !! deallocating the two-dimensional INTEGER-array "dispose_node_index_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dispose_node_index_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_dispose_node_index_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dispose_node_index_tc ) ) THEN
       DEALLOCATE ( dispose_node_index_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dispose_node_index_tc(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dispose_node_index_tc_d
  !
  !! deallocating the two-dimensional INTEGER-array "art_bed_load_poly_index" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_art_bed_load_poly_ind_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_art_bed_load_poly_ind_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( art_bed_load_poly_index ) ) THEN
       DEALLOCATE ( art_bed_load_poly_index, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'art_bed_load_poly_index(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_art_bed_load_poly_ind_d
  !
  !! deallocating the two-dimensional INTEGER-array "art_bed_load_node_index" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_art_bed_load_node_ind_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_art_bed_load_node_ind_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( art_bed_load_node_index ) ) THEN
       DEALLOCATE ( art_bed_load_node_index, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'art_bed_load_node_index(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_art_bed_load_node_ind_d
  !
  !! deallocating the two-dimensional CHARACTER-array "dispose_poly_name" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dispose_poly_name_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_dispose_poly_name_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dispose_poly_name ) ) THEN
       DEALLOCATE ( dispose_poly_name, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dispose_poly_name(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dispose_poly_name_d
  !
  !! deallocating the two-dimensional CHARACTER-array "list_of_disp_polys" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_list_of_disp_polys_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_list_of_disp_polys_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( list_of_disp_polys ) ) THEN
       DEALLOCATE ( list_of_disp_polys, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'list_of_disp_polys(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_list_of_disp_polys_d
  !
  !! deallocating the one-dimensional CHARACTER-array "dredge_poly_name_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_poly_name_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_dredge_poly_name_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredge_poly_name_tc ) ) THEN
       DEALLOCATE ( dredge_poly_name_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredge_poly_name_tc(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_poly_name_tc_d
  !! deallocating the one-dimensional CHARACTER-array "dispose_poly_name_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dispose_poly_name_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_dispose_poly_name_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dispose_poly_name_tc ) ) THEN
       DEALLOCATE ( dispose_poly_name_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dispose_poly_name_tc(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dispose_poly_name_tc_d
  !! deallocating the one-dimensional CHARACTER-array "predef_disp_poly_name" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_predef_disp_poly_name_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_predef_disp_poly_name_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( predef_disp_poly_name ) ) THEN
       DEALLOCATE ( predef_disp_poly_name, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'predef_disp_poly_name(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_predef_disp_poly_name_d
  !! deallocating the one-dimensional CHARACTER-array "predef_disp_sed_class" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_predef_disp_sed_class_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_predef_disp_sed_class_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( predef_disp_sed_class ) ) THEN
       DEALLOCATE ( predef_disp_sed_class, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'predef_disp_sed_class(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_predef_disp_sed_class_d
  !! deallocating the one-dimensional REAL(Double)-array "dredging_rate" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredging_rate_d ( )
    !! name of the subroutine
    CHARACTER (LEN=23), parameter  :: c_upname='dealloc_dredging_rate_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredging_rate ) ) THEN
       DEALLOCATE ( dredging_rate, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredging_rate(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredging_rate_d
  !! deallocating the one-dimensional REAL(Double)-array "minimum_volume"
  !! subroutine generates error messages
  SUBROUTINE dealloc_minimum_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=23), PARAMETER  :: c_upname='dealloc_minimum_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( minimum_volume ) ) THEN
       DEALLOCATE ( minimum_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'minimum_volume(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_minimum_volume_d
  !! deallocating the one-dimensional REAL(Double)-array "sector_radius"
  !! subroutine generates error messages
  SUBROUTINE dealloc_sector_radius_d ( )
    !! name of the subroutine
    CHARACTER (LEN=23), PARAMETER  :: c_upname='dealloc_sector_radius_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( sector_radius ) ) THEN
       DEALLOCATE ( sector_radius, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'sector_radius(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_sector_radius_d
  !! deallocating the one-dimensional REAL(Double)-array "disposal_rate" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disposal_rate_d ( )
    !! name of the subroutine
    CHARACTER (LEN=23), parameter  :: c_upname='dealloc_disposal_rate_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disposal_rate ) ) THEN
       DEALLOCATE ( disposal_rate, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disposal_rate(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disposal_rate_d
  !! deallocating the two-dimensional REAL(Double)-array "dispose_weighting_factor" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dispose_weighting_fac_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_dispose_weighting_fac_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dispose_weighting_factor ) ) THEN
       DEALLOCATE ( dispose_weighting_factor, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dispose_weighting_factor(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dispose_weighting_fac_d
  !! deallocating the one-dimensional REAL(Double)-array "predef_disp_sed_vol" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_predef_disp_sed_vol_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_predef_disp_sed_vol_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( predef_disp_sed_vol ) ) THEN
       DEALLOCATE ( predef_disp_sed_vol, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'predef_disp_sed_vol(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_predef_disp_sed_vol_d
  !! deallocating the one-dimensional CHARACTER-array "ini_obs_time" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_ini_obs_time_d ( )
    !! name of the subroutine
    CHARACTER (LEN=22), parameter  :: c_upname='dealloc_ini_obs_time_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( ini_obs_time ) ) THEN
       DEALLOCATE ( ini_obs_time, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'ini_obs_time(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ini_obs_time_d
  !! deallocating the one-dimensional REAL(Double)-array "observing_period" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_observing_period_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_observing_period_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( observing_period ) ) THEN
       DEALLOCATE ( observing_period, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'observing_period(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_observing_period_d
  !! deallocating the one-dimensional REAL(Double)-array "limiting_discharge" <BR>
  !! subroutine generates error messages
  SUBROUTINE dealloc_limiting_discharge_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), PARAMETER  :: c_upname='dealloc_limiting_discharge_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( limiting_discharge ) ) THEN
       DEALLOCATE ( limiting_discharge, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'limiting_discharge(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_limiting_discharge_d
  !! deallocating the one-dimensional LOGICAL-array "navigation_possible" <BR>
  !! subroutine generates error messages
  SUBROUTINE dealloc_navigation_possible_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), PARAMETER  :: c_upname='dealloc_navigation_possible_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( navigation_possible ) ) THEN
       DEALLOCATE ( navigation_possible, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4400, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'navigation_possible(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_navigation_possible_d
  !! deallocating the one-dimensional REAL(Double)-array "dredge_sed_vol_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_sed_vol_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_dredge_sed_vol_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredge_sed_vol_tc ) ) THEN
       DEALLOCATE ( dredge_sed_vol_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredge_sed_vol_tc(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_sed_vol_tc_d
  !! deallocating the one-dimensional REAL(Double)-array "dispose_sed_vol_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dispose_sed_vol_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_dispose_sed_vol_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dispose_sed_vol_tc ) ) THEN
       DEALLOCATE ( dispose_sed_vol_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dispose_sed_vol_tc(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dispose_sed_vol_tc_d
  !! deallocating the one-dimensional REAL(Double)-array "predef_sed_distrib" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_predef_sed_distrib_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_predef_sed_distrib_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( predef_sed_distrib ) ) THEN
       DEALLOCATE ( predef_sed_distrib, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'predef_sed_distrib(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_predef_sed_distrib_d
  !! deallocating the two-dimensional CHARACTER-array "predef_dredge_time_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_predef_dredge_time_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_predef_dredge_time_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( predef_dredge_time_tc ) ) THEN
       DEALLOCATE ( predef_dredge_time_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'predef_dredge_time_tc(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_predef_dredge_time_tc_d
  !! deallocating the two-dimensional CHARACTER-array "predef_disp_time_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_predef_disp_time_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_predef_disp_time_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( predef_disp_time_tc ) ) THEN
       DEALLOCATE ( predef_disp_time_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'predef_disp_time_tc(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_predef_disp_time_tc_d
  !! deallocating the two-dimensional CHARACTER-array "predef_depos_time" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_predef_depos_time_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_predef_depos_time_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( predef_depos_time ) ) THEN
       DEALLOCATE ( predef_depos_time, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'predef_depos_time(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_predef_depos_time_d
  !
  !! deallocating the one-dimensional CHARACTER-array "disp_scours_auto" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_scours_auto_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_disp_scours_auto_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_scours_auto ) ) THEN
       DEALLOCATE ( disp_scours_auto, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_scours_auto(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_scours_auto_d
  !
  !! deallocating the one-dimensional CHARACTER-array "disp_scours_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_scours_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=24), parameter  :: c_upname='dealloc_disp_scours_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_scours_tc ) ) THEN
       DEALLOCATE ( disp_scours_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_scours_tc(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_scours_tc_d
  !
  !! deallocating the one-dimensional CHARACTER-array "disp_scours_abl" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_scours_abl_d ( )
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='dealloc_disp_scours_abl_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_scours_abl ) ) THEN
       DEALLOCATE ( disp_scours_abl, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_scours_abl(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_scours_abl_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "poly_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_poly_total_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_poly_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( poly_total_volume ) ) THEN
       DEALLOCATE ( poly_total_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'poly_total_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_poly_total_volume_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "node_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_total_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_node_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_total_volume ) ) THEN
       DEALLOCATE ( node_total_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_total_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_total_volume_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "disp_node_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_node_tot_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_disp_node_tot_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_node_total_volume ) ) THEN
       DEALLOCATE ( disp_node_total_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_node_total_volume(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_node_tot_volume_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "total_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_total_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=22), parameter  :: c_upname='dealloc_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( total_volume ) ) THEN
       DEALLOCATE ( total_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'total_volume(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_total_volume_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "all_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_all_total_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_all_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( all_total_volume ) ) THEN
       DEALLOCATE ( all_total_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'all_total_volume(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_all_total_volume_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "all_total_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_all_total_volume_old_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_all_total_volume_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( all_total_volume_old ) ) THEN
       DEALLOCATE ( all_total_volume_old, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'all_total_volume_old(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_all_total_volume_old_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "poly_sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_poly_sediment_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_poly_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( poly_sediment_volume ) ) THEN
       DEALLOCATE ( poly_sediment_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'poly_sediment_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_poly_sediment_volume_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "node_sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_sediment_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_node_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_sediment_volume ) ) THEN
       DEALLOCATE ( node_sediment_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_sediment_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_sediment_volume_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "node_sediment_volume_rs" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_sedi_volume_rs_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_node_sedi_volume_rs_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_sediment_volume_rs ) ) THEN
       DEALLOCATE ( node_sediment_volume_rs, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_sediment_volume_rs(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_sedi_volume_rs_d
  !
  !! deallocating the one-dimensional CHARACTER-array "last_obs_time_rs" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_last_obs_time_rs_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_last_obs_time_rs_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( last_obs_time_rs ) ) THEN
       DEALLOCATE ( last_obs_time_rs, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'last_obs_time_rs(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_last_obs_time_rs_d
  !
  !! deallocating the one-dimensional CHARACTER-array "next_obs_time_rs" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_next_obs_time_rs_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_next_obs_time_rs_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( next_obs_time_rs ) ) THEN
       DEALLOCATE ( next_obs_time_rs, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'next_obs_time_rs(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_next_obs_time_rs_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_sediment_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='dealloc_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( sediment_volume ) ) THEN
       DEALLOCATE ( sediment_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'sediment_volume(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_sediment_volume_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "all_sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_all_sediment_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_all_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( all_sediment_volume ) ) THEN
       DEALLOCATE ( all_sediment_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'all_sediment_volume(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_all_sediment_volume_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "all_sediment_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_all_sediment_vol_old_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_all_sediment_vol_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( all_sediment_volume_old ) ) THEN
       DEALLOCATE ( all_sediment_volume_old, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'all_sediment_volume_old(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_all_sediment_vol_old_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "node_sediment_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_sedi_volume_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_node_sedi_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_sediment_volume_tc ) ) THEN
       DEALLOCATE ( node_sediment_volume_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_sediment_volume_tc(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_sedi_volume_tc_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "sediment_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_sediment_volume_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_sediment_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( sediment_volume_tc ) ) THEN
       DEALLOCATE ( sediment_volume_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'sediment_volume_tc(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_sediment_volume_tc_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "poly_water_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_poly_water_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_poly_water_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( poly_water_volume ) ) THEN
       DEALLOCATE ( poly_water_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'poly_water_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_poly_water_volume_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "node_water_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_water_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_node_water_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_water_volume ) ) THEN
       DEALLOCATE ( node_water_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_water_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_water_volume_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "water_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_water_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=22), parameter  :: c_upname='dealloc_water_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( water_volume ) ) THEN
       DEALLOCATE ( water_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'water_volume(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_water_volume_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "all_water_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_all_water_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_all_water_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( all_water_volume ) ) THEN
       DEALLOCATE ( all_water_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'all_water_volume(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_all_water_volume_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "all_water_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_all_water_volume_old_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_all_water_volume_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( all_water_volume_old ) ) THEN
       DEALLOCATE ( all_water_volume_old, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'all_water_volume_old(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_all_water_volume_old_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "poly_fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_poly_fraction_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_poly_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( poly_fraction_volume ) ) THEN
       DEALLOCATE ( poly_fraction_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'poly_fraction_volume(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_poly_fraction_volume_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "node_fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_fraction_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_node_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_fraction_volume ) ) THEN
       DEALLOCATE ( node_fraction_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_fraction_volume(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_fraction_volume_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "node_fraction_volume_rs" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_frac_volume_rs_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_node_frac_volume_rs_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_fraction_volume_rs ) ) THEN
       DEALLOCATE ( node_fraction_volume_rs, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_fraction_volume_rs(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_frac_volume_rs_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_fraction_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='dealloc_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( fraction_volume ) ) THEN
       DEALLOCATE ( fraction_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'fraction_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_fraction_volume_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "all_fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_all_fraction_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_all_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( all_fraction_volume ) ) THEN
       DEALLOCATE ( all_fraction_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'all_fraction_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_all_fraction_volume_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "all_fraction_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_all_fraction_vol_old_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_all_fraction_vol_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( all_fraction_volume_old ) ) THEN
       DEALLOCATE ( all_fraction_volume_old, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'all_fraction_volume_old(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_all_fraction_vol_old_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "poly_fraction_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_poly_frac_volume_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_poly_frac_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( poly_fraction_volume_tc ) ) THEN
       DEALLOCATE ( poly_fraction_volume_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'poly_fraction_volume_tc(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_poly_frac_volume_tc_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "node_fraction_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_frac_volume_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_node_frac_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_fraction_volume_tc ) ) THEN
       DEALLOCATE ( node_fraction_volume_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_fraction_volume_tc(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_frac_volume_tc_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "fraction_volume_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_fraction_volume_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_fraction_volume_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( fraction_volume_tc ) ) THEN
       DEALLOCATE ( fraction_volume_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'fraction_volume_tc(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_fraction_volume_tc_d
  !
  !! deallocating the two-dimensional LOGICAL-array "upd_out_volumes" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_upd_out_volumes_d ( )
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='dealloc_upd_out_volumes_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( upd_out_volumes ) ) THEN
       DEALLOCATE ( upd_out_volumes, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4400, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'upd_out_volumes(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_upd_out_volumes_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "aim_poly_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_aim_poly_depth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_aim_poly_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( aim_poly_depth ) ) THEN
       DEALLOCATE ( aim_poly_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'aim_poly_depth(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_aim_poly_depth_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "aim_node_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_aim_node_depth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_aim_node_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( aim_node_depth ) ) THEN
       DEALLOCATE ( aim_node_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'aim_node_depth(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_aim_node_depth_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "aim_node_depth_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_aim_node_depth_tc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_aim_node_depth_tc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( aim_node_depth_tc ) ) THEN
       DEALLOCATE ( aim_node_depth_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'aim_node_depth_tc(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_aim_node_depth_tc_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "aim_node_depth_abl" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_aim_node_depth_abl_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_aim_node_depth_abl_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( aim_node_depth_abl ) ) THEN
       DEALLOCATE ( aim_node_depth_abl, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'aim_node_depth_abl(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_aim_node_depth_abl_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "delta_dredge_poly_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_delta_dredge_poldepth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_delta_dredge_poldepth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( delta_dredge_poly_depth ) ) THEN
       DEALLOCATE ( delta_dredge_poly_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'delta_dredge_poly_depth(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_delta_dredge_poldepth_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "delta_dredge_node_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_delta_dredge_noddepth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_delta_dredge_noddepth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( delta_dredge_node_depth ) ) THEN
       DEALLOCATE ( delta_dredge_node_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'delta_dredge_node_depth(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_delta_dredge_noddepth_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "delta_disp_poly_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_delta_disp_poldepth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_delta_disp_poldepth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( delta_disp_poly_depth ) ) THEN
       DEALLOCATE ( delta_disp_poly_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'delta_disp_poly_depth(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_delta_disp_poldepth_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "delta_disp_node_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_delta_disp_noddepth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_delta_disp_noddepth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( delta_disp_node_depth ) ) THEN
       DEALLOCATE ( delta_disp_node_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'delta_disp_node_depth(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_delta_disp_noddepth_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "disp_poly_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_poly_depth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='dealloc_disp_poly_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_poly_depth ) ) THEN
       DEALLOCATE ( disp_poly_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_poly_depth(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_poly_depth_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "disp_node_depth" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_node_depth_d ( )
    !! name of the subroutine
    CHARACTER (LEN=25), parameter  :: c_upname='dealloc_disp_node_depth_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_node_depth ) ) THEN
       DEALLOCATE ( disp_node_depth, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_node_depth(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_node_depth_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "dzb_dredge" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dzb_dredge_d ( )
    !! name of the subroutine
    CHARACTER (LEN=20), parameter  :: c_upname='dealloc_dzb_dredge_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dzb_dredge ) ) THEN
       DEALLOCATE ( dzb_dredge, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dzb_dredge(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dzb_dredge_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "dzb_disp" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dzb_disp_d ( )
    !! name of the subroutine
    CHARACTER (LEN=18), parameter  :: c_upname='dealloc_dzb_disp_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dzb_disp ) ) THEN
       DEALLOCATE ( dzb_disp, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dzb_disp(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dzb_disp_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "dredged_sediment_volume_to_disp" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_sed_volume_to_disp_d ( )
    !! name of the subroutine
    CHARACTER (LEN=28), parameter  :: c_upname='dealloc_sed_volume_to_disp_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredged_sediment_volume_to_disp ) ) THEN
       DEALLOCATE ( dredged_sediment_volume_to_disp, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredged_sediment_volume_to_disp(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_sed_volume_to_disp_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "dredged_fraction_volume_to_disp" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_frac_volume_to_disp_d ( )
    !! name of the subroutine
    CHARACTER (LEN=29), parameter  :: c_upname='dealloc_frac_volume_to_disp_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredged_fraction_volume_to_disp ) ) THEN
       DEALLOCATE ( dredged_fraction_volume_to_disp, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredged_fraction_volume_to_disp(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_frac_volume_to_disp_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "disp_total_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_total_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_disp_total_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_total_volume ) ) THEN
       DEALLOCATE ( disp_total_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_total_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_total_volume_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "disp_total_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_total_volume_old_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_disp_total_volume_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_total_volume_old ) ) THEN
       DEALLOCATE ( disp_total_volume_old, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_total_volume_old(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_total_volume_old_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "disp_sediment_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_sediment_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_disp_sediment_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_sediment_volume ) ) THEN
       DEALLOCATE ( disp_sediment_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_sediment_volume(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_sediment_volume_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "disp_sediment_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_sediment_vol_old_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_disp_sediment_vol_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_sediment_volume_old ) ) THEN
       DEALLOCATE ( disp_sediment_volume_old, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_sediment_volume_old(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_sediment_vol_old_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "disp_fraction_volume" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_fraction_volume_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_disp_fraction_volume_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_fraction_volume ) ) THEN
       DEALLOCATE ( disp_fraction_volume, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_fraction_volume(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_fraction_volume_d
  !
  !! deallocating the three-dimensional REAL(Double)-array "disp_fraction_volume_old" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disp_fraction_vol_old_d ( )
    !! name of the subroutine
    CHARACTER (LEN=31), parameter  :: c_upname='dealloc_disp_fraction_vol_old_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( disp_fraction_volume_old ) ) THEN
       DEALLOCATE ( disp_fraction_volume_old, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'disp_fraction_volume_old(:,:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disp_fraction_vol_old_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "delta_disp_node_depth_tc" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_deltadispnodedepthtc_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_deltadispnodedepthtc_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( delta_disp_node_depth_tc ) ) THEN
       DEALLOCATE ( delta_disp_node_depth_tc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'delta_disp_node_depth_tc(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_deltadispnodedepthtc_d
  !
  !! deallocating the two-dimensional REAL(Double)-array "delta_node_depth_abl" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_delta_node_depth_abl_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_delta_node_depth_abl_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( delta_node_depth_abl ) ) THEN
       DEALLOCATE ( delta_node_depth_abl, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'delta_node_depth_abl(:,:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_delta_node_depth_abl_d
  !
  !! deallocating the one-dimensional FILE-array "dredge_criterion" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_criterion_d ( )
    !! name of the subroutine
    CHARACTER (LEN=26), parameter  :: c_upname='dealloc_dredge_criterion_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( dredge_criterion ) ) THEN
       CALL kill_criterion( dredge_criterion )
       DEALLOCATE ( dredge_criterion, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'dredge_criterion(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_criterion_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "edge_related_surface" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_edge_related_surface_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_edge_related_surface_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( edge_related_surface ) ) THEN
       DEALLOCATE ( edge_related_surface, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'edge_related_surface(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_edge_related_surface_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "edge_weighted_sum" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_edge_weighted_sum_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_edge_weighted_sum_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( edge_weighted_sum ) ) THEN
       DEALLOCATE ( edge_weighted_sum, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'edge_weighted_sum(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_edge_weighted_sum_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "node_related_surface" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_related_surface_d ( )
    !! name of the subroutine
    CHARACTER (LEN=30), parameter  :: c_upname='dealloc_node_related_surface_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_related_surface ) ) THEN
       DEALLOCATE ( node_related_surface, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_related_surface(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_related_surface_d
  !
  !! deallocating the one-dimensional REAL(Double)-array "node_weighted_sum" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_node_weighted_sum_d ( )
    !! name of the subroutine
    CHARACTER (LEN=27), parameter  :: c_upname='dealloc_node_weighted_sum_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( node_weighted_sum ) ) THEN
       DEALLOCATE ( node_weighted_sum, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'node_weighted_sum(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_node_weighted_sum_d
  !
  !! deallocating the array start_dredge_time 
  !! subroutine generates error messages
  SUBROUTINE dealloc_start_dredge_time_d &
       ( )
    !
    CHARACTER (LEN=27) , parameter  :: c_upname='dealloc_start_dredge_time_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( start_dredge_time ) ) THEN
       CALL kill_datetime( start_dredge_time(:) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( start_dredge_time , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'start_dredge_time' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_start_dredge_time_d
  !! deallocating the array end_dredge_time 
  !! subroutine generates error messages
  SUBROUTINE dealloc_end_dredge_time_d &
       ( )
    !
    CHARACTER (LEN=25) , parameter  :: c_upname='dealloc_end_dredge_time_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( end_dredge_time ) ) THEN
       CALL kill_datetime( end_dredge_time(:) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( end_dredge_time , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'end_dredge_time' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_end_dredge_time_d
  !! deallocating the array time_to_observe 
  !! subroutine generates error messages
  SUBROUTINE dealloc_time_to_observe_d &
       ( )
    !
    CHARACTER (LEN=25) , parameter  :: c_upname='dealloc_time_to_observe_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( time_to_observe ) ) THEN
       CALL kill_datetime( time_to_observe(:) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( time_to_observe , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'time_to_observe' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_time_to_observe_d
  !! deallocating the array old_time_to_observe 
  !! subroutine generates error messages
  SUBROUTINE dealloc_old_time_to_observe_d &
       ( )
    !
    CHARACTER (LEN=25) , parameter  :: c_upname='dealloc_old_time_to_observe_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( old_time_to_observe ) ) THEN
       CALL kill_datetime( old_time_to_observe(:) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( old_time_to_observe , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'old_time_to_observe' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_old_time_to_observe_d
  !! deallocating the array initial_time_to_observe 
  !! subroutine generates error messages
  SUBROUTINE dealloc_ini_time_to_observe_d &
       ( )
    !
    CHARACTER (LEN=29) , parameter  :: c_upname='dealloc_ini_time_to_observe_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( initial_time_to_observe ) ) THEN
       CALL kill_datetime( initial_time_to_observe(:) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( initial_time_to_observe , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'initial_time_to_observe' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_ini_time_to_observe_d
  !! deallocating the array dredge_time_tc 
  !! subroutine generates error messages
  SUBROUTINE dealloc_dredge_time_tc_d &
       ( )
    !
    CHARACTER (LEN=24) , parameter  :: c_upname='dealloc_dredge_time_tc_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( dredge_time_tc ) ) THEN
       CALL kill_datetime( dredge_time_tc(:,2) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( dredge_time_tc , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'dredge_time_tc' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_dredge_time_tc_d
  !! deallocating the array disposal_time_tc 
  !! subroutine generates error messages
  SUBROUTINE dealloc_disposal_time_tc_d &
       ( )
    !
    CHARACTER (LEN=26) , parameter  :: c_upname='dealloc_disposal_time_tc_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( disposal_time_tc ) ) THEN
       CALL kill_datetime( disposal_time_tc(:,2) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( disposal_time_tc , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'disposal_time_tc' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_disposal_time_tc_d
  !! deallocating the array art_bl_time 
  !! subroutine generates error messages
  SUBROUTINE dealloc_art_bl_time_d &
       ( )
    !
    CHARACTER (LEN=21) , parameter  :: c_upname='dealloc_art_bl_time_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( art_bl_time ) ) THEN
       CALL kill_datetime( art_bl_time(:,2) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( art_bl_time , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'art_bl_time' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_art_bl_time_d
  !! deallocating the array used_sediment_classes 
  !! subroutine generates error messages
  SUBROUTINE dealloc_used_sediment_classes_d &
       ( )
    !
    CHARACTER (LEN=31) , parameter  :: c_upname='dealloc_used_sediment_classes_d'
	INTEGER :: stat
    !
    IF ( ALLOCATED( used_sediment_classes ) ) THEN
       CALL kill_grain( used_sediment_classes(:) )
       IF ( no_error( ) ) THEN
          DEALLOCATE ( used_sediment_classes , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 5000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<arrayName>', 'used_sediment_classes' )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE dealloc_used_sediment_classes_d
  !
  !! deallocating the one-dimensional CHARACTER-array "fraction_name" 
  !! subroutine generates error messages
  SUBROUTINE dealloc_fraction_name_d ( )
    !! name of the subroutine
    CHARACTER (LEN=23), parameter  :: c_upname='dealloc_fraction_name_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ALLOCATED( fraction_name ) ) THEN
       DEALLOCATE ( fraction_name, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -4200, c_upname, c_modname, stat )
          CALL setup_error_act ( '<name>', 'fraction_name(:)' )
       END IF
    END IF
    !
  END SUBROUTINE dealloc_fraction_name_d
  !
  ! -----------------------------------------------------------------------
  ! weitere Hilfsmethods
  ! -----------------------------------------------------------------------
  !
  !! counting dimensions (greater than zero) in an array 
  !! function does not throw error messages
  FUNCTION get_rank_count_d ( shp ) &
       RESULT(res)
    !! shape of the array
    INTEGER , INTENT(IN) :: shp(:) ! 
    !! result: number of values with an entry greater than zero
    INTEGER :: res                 ! 
    !
    res = COUNT( shp > 0 )
    !
  END FUNCTION get_rank_count_d
  !
  !! comparison of two shapes regarding identity
  !! function generates error messages
  FUNCTION has_identical_shape_d ( shp1, shp2, name ) &
       RESULT(res)
    !! shape of the array as demanded
    INTEGER           , INTENT(IN) :: shp1(:) ! 
    !! shape of the array as allocated
    INTEGER           , INTENT(IN) :: shp2(:) ! 
    !! name of the array
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! result: test result
    LOGICAL            :: res    ! 
    !! name of subroutine
    CHARACTER (LEN=21) , parameter :: c_upname='has_identical_shape_d' ! 
    ! variables
    CHARACTER (LEN=40) :: l_char ! 
    INTEGER            :: n1, n2 ! 
    !
    res = .false. 
    n1  = get_rank_count( shp1 )
    n2  = get_rank_count( shp2 )
    IF ( n1 == n2 ) THEN
       IF ( ALL( shp1(1:n1) == shp2(1:n2) ) ) res = .true.
    END IF
    IF ( .NOT. res ) THEN
       CALL setup_error_act( all_errors, -3910, c_upname, c_modname )
       CALL setup_error_act( '<name>', TRIM(name) )
       WRITE(l_char,'(I5)'  ) n1         ; CALL setup_error_act ( '<actrank>', l_char(1:5) )
       WRITE(l_char,'(I5)'  ) n2         ; CALL setup_error_act ( '<reqrank>', l_char(1:5) )
       l_char = REPEAT( ' ', LEN(l_char) )
       WRITE(l_char,'(4I10)') shp1(1:n1) ; CALL setup_error_act ( '<actshape>', TRIM(l_char) )
       l_char = REPEAT( ' ', LEN(l_char) )
       WRITE(l_char,'(4I10)') shp2(1:n2) ; CALL setup_error_act ( '<reqshape>', TRIM(l_char) )
    END IF
    !
  END FUNCTION has_identical_shape_d
  !
  !! generating an error message, if rank of an array is not sufficient 
  !! function generates error messages
  FUNCTION has_correct_rank_d ( shp, var, name ) &
       RESULT(res)
    !! array with current shape
    INTEGER           , INTENT(IN) :: shp(:) ! 
    !! aim value of current shape
    INTEGER           , INTENT(IN) :: var    ! 
    !! name of the array
    CHARACTER (LEN=*) , INTENT(IN) :: name   ! 
    !! result: test result
    LOGICAL                        :: res    ! 
    !! name of subroutine
    CHARACTER (LEN=18) , parameter :: c_upname='has_correct_rank_d' ! 
    ! variable
    CHARACTER (LEN=5) :: l_char ! 
    !
    IF ( get_rank_count( shp ) /= var ) THEN
       CALL setup_error_act( all_errors, -3900, c_upname, c_modname )
       CALL setup_error_act( '<name>', TRIM(name) )
       WRITE(l_char,'(I5)') get_rank_count( shp ) ; CALL setup_error_act ( '<act>', l_char )
       WRITE(l_char,'(I5)') var                   ; CALL setup_error_act ( '<req>', l_char )
       res = .false.
    ELSE
       res = .true. 
    END IF
    !
  END FUNCTION has_correct_rank_d
  !
END MODULE m_dredgesim_data
! TailOfPackageModule -----------------------------------------------------
