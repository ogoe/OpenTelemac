! -------------------------------------------------------------------------
! HeadOfPackageUserInterface ----------------------------------------------
!
!! interfaces and kernel of the "dredgesim"-package
!!
!
MODULE p_dredgesim_ui
  !
  ! ----------------------------------------------------------------------
  ! [A] base modules and packages with frequently used methods
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] base module with global constant values ----------------------
  USE b_constants, ONLY : Single, Double !
  ! [A.1.2] base module "error-handling" ---------------------------------
  USE b_error, ONLY : &
       ! routines
       init_error, clear_error, &
       no_error, any_error, &
       new_error, kill_error, &
       print_error, &
       setup_error_act, setup_error_prn_lun, setup_error_trc_lun, &
       set_error_ierr, set_error_cerr
  ! [A.1.3] base module "file-handling" ----------------------------------
  USE b_file, ONLY : &
       ! routines
       init_file, clear_file, &
       setup_file_prn_lun, setup_file_trc_lun
  ! [A.1.4] base module "sediment grain" ---------------------------------
  USE b_grain, ONLY :       &
       ! routines
       init_grain,          &
       clear_grain,         &
       setup_grain_prn_lun, &
       setup_grain_trc_lun
  ! [A.1.5] base module "timestep" ---------------------------------------
  USE b_time
  ! [A.1.6] base module "date and time" ----------------------------------
  USE b_datetime
  ! [A.1.7] base module "physical parameters" ----------------------------
  USE b_phy, ONLY : &
       ! routines
       init_phy, clear_phy, &
       setup_phy_prn_lun, setup_phy_trc_lun, setup_phy_language
  ! [A.1.8] base module with type+methods "information concerning files" -
  USE b_io_info, ONLY : &
       ! data type
       t_io_info,       &
       ! routines
       init_io_info, clear_io_info, &
       setup_io_info_prn_lun, setup_io_info_trc_lun
  !
  ! [A.1.9] (local) base module type+methods "dredge polygon data" -------
  USE l_criterion, ONLY : &
       ! constant
       c_max_poly_name_len, &
       ! routines
       init_criterion, clear_criterion, &
       setup_criterion_prn_lun, setup_criterion_trc_lun, &
       get_criterion_poly_name
  !
  ! Sisyphe data
  ! ----------------------------------------------------------------------
  ! [B] data of the "dredgesim"-package
  ! ----------------------------------------------------------------------
  !
  ! [B.1.1] data module of the "dredgesim"-package -----------------------
  USE m_dredgesim_data, ONLY : &
       ! variables with INTENT(OUT)
       initialised, & 
       prn_op,      & 
       trc_op,      & 
       prn_lun,     & 
       trc_lun,     & 
       all_errors,  & 
       c_max_language, c_def_language, language,       &
       c_undef_ch, c_undef_in, c_undef_re, c_undef_dp, & 
       ! methods
       init_dredgesim_data, clear_dredgesim_data,      &
       alloc_dredgesim_start, alloc_dredgesim_prepare, & 
       dealloc_dredgesim_data,                         &
       print_dredgesim_shape, print_dredgesim_data, &
!RK
       debug_ds
  !  
  ! [B.1.2] error module of the "dredgesim"-package ----------------------
  USE m_dredgesim_errors, ONLY : &
       ! methods
       init_dredgesim_errors, clear_dredgesim_errors ! >ToDo> ggf. weitere ergaenzen
  ! [B.1.3] steering module of the "dredgesim"-package -------------------
  USE m_dredgesim_steer, ONLY :  &
       ! routines / interfaces
       read_dredgesim_steer, print_dredgesim_steer
  ! [B.1.4] update module of the "dredgesim"-package ---------------------
  USE m_dredgesim_update, ONLY : &
       ! routines / interfaces
       update_dredgesim_start, update_dredgesim_prepare, update_dredgesim_run
  ! [B.1.5] compute module of the "dredgesim"-package --------------------
  USE m_dredgesim_compute, ONLY : &
       ! routines / interfaces
       compute_dredgesim
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] public declarations
  ! ---------------------------------------------------------------------
  !
  !! allocating/initializing data of the package
  INTERFACE init_dredgesim
     MODULE PROCEDURE init_dredgesim_d ! 
  END INTERFACE
  !! deallocating/deinitializing data of the package          
  INTERFACE clear_dredgesim
     MODULE PROCEDURE clear_dredgesim_d ! 
  END INTERFACE
  !
  !! channel number for PRINT-methods auf Benutzerwert setting 
  INTERFACE setup_dredgesim_prn_lun
     MODULE PROCEDURE setup_dredgesim_prn_lun_d ! 
  END INTERFACE
  !! channel number for TRACE-methods auf Benutzerwert setting 
  INTERFACE setup_dredgesim_trc_lun
     MODULE PROCEDURE setup_dredgesim_trc_lun_d ! 
  END INTERFACE
  !! index language setting 
  !! 1 = German (Default) 
  !! 2 = English         
  INTERFACE setup_dredgesim_language
     MODULE PROCEDURE setup_dredgesim_language_d ! 
  END INTERFACE
  !
  !! printing all static data of the package
  INTERFACE print_dredgesim_static
     MODULE PROCEDURE print_dredgesim_static_d ! 
  END INTERFACE
  !! printing all error messages of the package 
  INTERFACE print_dredgesim_all_errors
     MODULE PROCEDURE print_dredgesim_all_errors_d ! 
  END INTERFACE
  !
  !! start executing methods of the dredgsim package
  !! at the end of this step all required data for preparing the
  !! simulation have to be allocated and initialized
  INTERFACE start_dredgesim
     MODULE PROCEDURE start_dredgesim_d
  END INTERFACE
  !
  !! preparation of missing data for simulation
  !! at the end all required data for running the simulation have to be
  !! allocated and initialized
!LEO  INTERFACE prepare_dredgesim
!LEO     MODULE PROCEDURE prepare_dredgesim_d
!LEO  END INTERFACE
  !
  !! runnig the simulation (one timestep) 
  INTERFACE run_dredgesim
     MODULE PROCEDURE run_dredgesim_d
     MODULE PROCEDURE run_dredgesim_timestep_in_sec_d
  END INTERFACE
  !
  !! ending a simultion
  !! deallocating and deinitializing of arrays 
  !! closing open files
  INTERFACE stop_dredgesim
     MODULE PROCEDURE stop_dredgesim_d
  END INTERFACE
  !
  !! setting the steering file
  !! data is copied to data module 
  INTERFACE setup_ds_steering_file
     MODULE PROCEDURE setup_ds_steering_file_d
  END INTERFACE
  !! setting the printer file 
  !! data is copied to data module 
  INTERFACE setup_ds_printer_file
     MODULE PROCEDURE setup_ds_printer_file_d
  END INTERFACE
  !! setting the trace file 
  !! data is copied to data module 
  INTERFACE setup_ds_trace_file
     MODULE PROCEDURE setup_ds_trace_file_d
  END INTERFACE
  !! setting the initial time
  !! user specified data is copied to data module
  INTERFACE setup_ds_initial_time
     MODULE PROCEDURE setup_ds_initial_time_d
  END INTERFACE
  !! setting the end time 
  !! user specified data is copied to data module
  INTERFACE setup_ds_end_time
     MODULE PROCEDURE setup_ds_end_time_d
  END INTERFACE
  !! setting limiting value for marking unvalid data "threshold" 
  !! user specified data is copied to data module
  INTERFACE setup_ds_threshold
     MODULE PROCEDURE setup_ds_threshold_d
  END INTERFACE
  !
  !! getting the index for language setting
  !! copy of internal data is recieved
  INTERFACE get_dredgesim_language
     MODULE PROCEDURE get_dredgesim_language_d
  END INTERFACE
  !
  !! getting the steering file 
  !! copy of internal data is recieved
  INTERFACE get_ds_steering_file
     MODULE PROCEDURE get_ds_steering_file_d
  END INTERFACE
  !! getting the printer file 
  !! copy of internal data is recieved
  INTERFACE get_ds_printer_file
     MODULE PROCEDURE get_ds_printer_file_d
  END INTERFACE
  !! getting the trace file 
  !! copy of internal data is recieved
  INTERFACE get_ds_trace_file
     MODULE PROCEDURE get_ds_trace_file_d
  END INTERFACE
  !! getting the initial time 
  !! copy of data is recieved
  INTERFACE get_ds_initial_time
     MODULE PROCEDURE get_ds_initial_time_d
  END INTERFACE
  !! getting the end time 
  !! copy of data is recieved
  INTERFACE get_ds_end_time
     MODULE PROCEDURE get_ds_end_time_d
  END INTERFACE
  !! getting the current timestep 
  !! copy of internal data is recieved
  INTERFACE get_ds_act_timestep
     MODULE PROCEDURE get_ds_act_timestep_d
  END INTERFACE
  !! getting the limiting value for marking unvalid data "threshold" 
  !! copy of internal data is recieved
  INTERFACE get_ds_threshold
     MODULE PROCEDURE get_ds_threshold_d
  END INTERFACE
 !! setting the debug switch
  !! user specified data is copied to data module
  INTERFACE setup_ds_debug
     MODULE PROCEDURE setup_ds_debug_i
  END INTERFACE

  !
  ! [C.4.2] optional public interfaces
  !
  !! setting nodes of elements 
  !! pointer on external data
  INTERFACE set_ds_nodes_of_poly
     MODULE PROCEDURE set_ds_nodes_of_poly_d
  END INTERFACE
  !! setting edgelist of elements 
  !! pointer on external data
  INTERFACE set_ds_edgelist_of_poly
     MODULE PROCEDURE set_ds_edgelist_of_poly_d
  END INTERFACE
  !! setting nodelist of elements 
  !! pointer on external data
  INTERFACE set_ds_nodelist_of_poly
     MODULE PROCEDURE set_ds_nodelist_of_poly_d
  END INTERFACE
  !! setting center coordinates of elements 
  !! pointer on external data
  INTERFACE set_ds_center_coord
     MODULE PROCEDURE set_ds_center_coord_d
  END INTERFACE
  !! setting gravitaional center coordinates of elements
  !! pointer on external data
  INTERFACE set_ds_grav_center_coord
     MODULE PROCEDURE set_ds_grav_center_coord_d
  END INTERFACE
  !! setting coordinates of nodes 
  !! pointer on external data
  INTERFACE set_ds_node_coord
     MODULE PROCEDURE set_ds_node_coord_d
  END INTERFACE
  !! setting element depths 
  !! pointer on external data
  INTERFACE set_ds_poly_depth
     MODULE PROCEDURE set_ds_poly_depth_d
  END INTERFACE
  !! setting element areas 
  !! pointer on external data
  INTERFACE set_ds_poly_area
     MODULE PROCEDURE set_ds_poly_area_d
  END INTERFACE
  !! setting node areas 
  !! pointer on external data
  INTERFACE set_ds_node_area
     MODULE PROCEDURE set_ds_node_area_d
  END INTERFACE
  !! setting nodelist local global
  !! pointer on external data
  INTERFACE set_ds_knolg
     MODULE PROCEDURE set_ds_knolg_i
  END INTERFACE
  !! setting node neighbors
  !! pointer on external data
  INTERFACE set_ds_node_neighb
     MODULE PROCEDURE set_ds_node_neighb_i
  END INTERFACE
  !! setting the number of shared points
  INTERFACE set_ds_nb_neighb_pt
     MODULE PROCEDURE set_ds_nb_neighb_pt_i
  END INTERFACE
  !! setting the list send
  INTERFACE set_ds_list_send
     MODULE PROCEDURE set_ds_list_send_i
  END INTERFACE
  !! setting the global number of shared points
  INTERFACE set_ds_nh_com
     MODULE PROCEDURE set_ds_nh_com_i
  END INTERFACE
  !! setting the buffer send
  INTERFACE set_ds_buf_send
     MODULE PROCEDURE set_ds_buf_send_d
  END INTERFACE
  !! setting the buffer received
  INTERFACE set_ds_buf_recv
     MODULE PROCEDURE set_ds_buf_recv_d
  END INTERFACE
  !! setting node depths 
  !! pointer on external data
  INTERFACE set_ds_node_depth
     MODULE PROCEDURE set_ds_node_depth_d
  END INTERFACE
  !! setting non erodible node depths 
  !! pointer on external data
  INTERFACE set_ds_node_noero_depth
     MODULE PROCEDURE set_ds_node_noero_depth_d
  END INTERFACE
  !! setting edge depths
  !! pointer on external data
  INTERFACE set_ds_edge_depth
     MODULE PROCEDURE set_ds_edge_depth_d
  END INTERFACE
  !! setting water depths on edges
  !! pointer on external data
  INTERFACE set_ds_edge_water_depth
     MODULE PROCEDURE set_ds_edge_water_depth_d
  END INTERFACE
  !! setting water depths on nodes
  !! pointer on external data
  INTERFACE set_ds_node_water_depth
     MODULE PROCEDURE set_ds_node_water_depth_d
  END INTERFACE
  !! setting porosity of nodes
  !! pointer on external data
  INTERFACE set_ds_node_porosity
     MODULE PROCEDURE set_ds_node_porosity_d
  END INTERFACE
  !! setting sediment fraction on elements
  !! pointer on external data
  INTERFACE set_ds_cell_sediment_fraction
     MODULE PROCEDURE set_ds_cell_sediment_fraction_d
  END INTERFACE
  !! setting sediment fraction on nodes
  !! pointer on external data
  INTERFACE set_ds_node_sediment_fraction
     MODULE PROCEDURE set_ds_node_sediment_fraction_d
  END INTERFACE
  !! setting index list element - dredge polygon
  !! copy of data in data module
  INTERFACE set_ds_dredge_poly_index
     MODULE PROCEDURE set_ds_dredge_poly_index_d
  END INTERFACE
  !! setting index list node - dredge polygon
  !! copy of data in data module
  INTERFACE set_ds_dredge_node_index
     MODULE PROCEDURE set_ds_dredge_node_index_d
  END INTERFACE
  !! setting index list element - disposal polygon
  !! copy of data in data module
  INTERFACE set_ds_dispose_poly_index
     MODULE PROCEDURE set_ds_dispose_poly_index_d
  END INTERFACE
  !! setting index list node - disposal polygon
  !! copy of data in data module
  INTERFACE set_ds_dispose_node_index
     MODULE PROCEDURE set_ds_dispose_node_index_d
  END INTERFACE
  !! setting index list element - dredge polygon for time controlled maintenance 
  !! copy of data in data module
  INTERFACE set_ds_dredge_poly_index_tc
     MODULE PROCEDURE set_ds_dredge_poly_index_tc_d
  END INTERFACE
  !! setting index list node - dredge polygon for time controlled maintenance 
  !! copy of data in data module
  INTERFACE set_ds_dredge_node_index_tc
     MODULE PROCEDURE set_ds_dredge_node_index_tc_d
  END INTERFACE
  !! setting index list element - disposal polygon for time controlled maintenance
  !! copy of data in data module
  INTERFACE set_ds_dispose_poly_index_tc
     MODULE PROCEDURE set_ds_dispose_poly_index_tc_d
  END INTERFACE
  !! setting index list node - disposal polygon for time controlled maintenance
  !! copy of data in data module
  INTERFACE set_ds_dispose_node_index_tc
     MODULE PROCEDURE set_ds_dispose_node_index_tc_d
  END INTERFACE
  !! setting index list element - disposal polygon for artificial bed load supply
  !! copy of data in data module
  INTERFACE set_ds_art_bed_load_pol_index
     MODULE PROCEDURE set_ds_art_bed_load_pol_index_d
  END INTERFACE
  !! setting index list node - disposal polygon for artificial bed load supply
  !! copy of data in data module
  INTERFACE set_ds_art_bed_load_nod_index
     MODULE PROCEDURE set_ds_art_bed_load_nod_index_d
  END INTERFACE
  !! setting of array with names of sediment classes
  !! copy of data in data module
  INTERFACE set_ds_used_sediment_classes
     MODULE PROCEDURE set_ds_used_sediment_classes_d
  END INTERFACE
  !! setting of array with names of sediment fractions used by mrphodynamic module 
  !! copy of data in data module
  INTERFACE set_ds_fraction_name
     MODULE PROCEDURE set_ds_fraction_name_d
  END INTERFACE
  !! setting the values for navigatiom_possible <BR>
  !! data is copied
  INTERFACE set_ds_navigation_possible
     MODULE PROCEDURE set_ds_navigation_possible_d
  END INTERFACE
  !! getting the values for navigatiom_possible <BR>
  !! data is copied
  INTERFACE get_ds_navigation_possible
     MODULE PROCEDURE get_ds_navigation_possible_d
  END INTERFACE
  !! getting the values for limiting_discharge
  !! data is copied
  INTERFACE get_ds_limiting_discharge
     MODULE PROCEDURE get_ds_limiting_discharge_d
  END INTERFACE
  !
  !! getting all information about input data
  !! pointer on internal data is recieved 
  INTERFACE get_ds_input_files_ref
     MODULE PROCEDURE get_ds_input_files_ref_d
  END INTERFACE
  !! getting the x- and y-coordinates of element centers 
  !! pointer on internal data is recieved 
  INTERFACE get_ds_center_coord_ref
     MODULE PROCEDURE get_ds_center_coord_ref_d
  END INTERFACE
  !! getting the x- und y-coordinates of gravitational centers of elements 
  !! pointer on internal data is recieved 
  INTERFACE get_ds_grav_center_coord_ref
     MODULE PROCEDURE get_ds_grav_center_coord_ref_d
  END INTERFACE
  !! getting the x- und y-coordinates of nodes of elements
  !! pointer on internal data is recieved 
  INTERFACE get_ds_node_coord_ref
     MODULE PROCEDURE get_ds_node_coord_ref_d
  END INTERFACE
  !! getting the number of dredge polygondefined in steering file 
  !! copy is recieved
  INTERFACE get_ds_nof_dredge_poly
     MODULE PROCEDURE get_ds_nof_dredge_poly_d
  END INTERFACE
  !! getting the name of dredge polygon n
  !! copy is recieved
  INTERFACE get_ds_dredge_poly_name
     MODULE PROCEDURE get_ds_dredge_poly_name_0
  END INTERFACE
  !
  !! getting the number of disposal polygondefined in steering file 
  !! copy is recieved
  INTERFACE get_ds_nof_dispose_poly
     MODULE PROCEDURE get_ds_nof_dispose_poly_d
  END INTERFACE
  !! getting the fractions of sediment material on each node 
  !! copy is recieved
  INTERFACE get_ds_node_sediment_fraction
     MODULE PROCEDURE get_ds_node_sediment_fraction_d
  END INTERFACE
  !! getting the name of disposal polygon n
  !! copy is recieved
  INTERFACE get_ds_dispose_poly_name
     MODULE PROCEDURE get_ds_dispose_poly_name_0
  END INTERFACE
  !! getting the number of dredge polygonfor time controlled operations defined in steering file 
  !! copy is recieved
  INTERFACE get_ds_nof_dredge_poly_tc
     MODULE PROCEDURE get_ds_nof_dredge_poly_tc_d
  END INTERFACE
  !! getting the name of dredge polygon n for time controlled maintenance 
  !! copy is recieved
  INTERFACE get_ds_dredge_poly_name_tc
     MODULE PROCEDURE get_ds_dredge_poly_name_tc_0
  END INTERFACE
  !! getting the number of disposal polygonfor time controlled maintenance defined in steering file
  !! copy is recieved
  INTERFACE get_ds_nof_dispose_poly_tc
     MODULE PROCEDURE get_ds_nof_dispose_poly_tc_d
  END INTERFACE
  !! getting the name of disposal polygon n for time controlled maintenance 
  !! copy is recieved
  INTERFACE get_ds_dispose_poly_name_tc
     MODULE PROCEDURE get_ds_dispose_poly_name_tc_0
  END INTERFACE
  !! getting the number of polygonfor artificial bed load supply defined in steering file
  !! copy is recieved
  INTERFACE get_ds_nof_art_bed_load_poly
     MODULE PROCEDURE get_ds_nof_art_bed_load_poly_d
  END INTERFACE
  !! getting the name of polygon for artificial bed load supply 
  !! copy is recieved
  INTERFACE get_ds_art_bed_load_poly_name
     MODULE PROCEDURE get_ds_art_bed_load_poly_name_0
  END INTERFACE
  !
  ! [C.4.3] external interfaces (to be provided by the user depenedent on software to transfer from/at)
  !
  ! -------------------------------------------------------------------------------------------
  ! reading/import of initial state (mesh and initial values)
  ! -------------------------------------------------------------------------------------------
  !
  !! external routine to provide information & data from file(s)
!LEO  INTERFACE 
!LEO     SUBROUTINE ext_ds_ini_data_read ( )
!LEO     END SUBROUTINE ext_ds_ini_data_read
!LEO  END INTERFACE
!LEO  !! external routine to link information & grid from data of other packages
!LEO  INTERFACE 
!LEO     SUBROUTINE ext_ds_ini_grid_import ( )
!LEO     END SUBROUTINE ext_ds_ini_grid_import
!LEO  END INTERFACE
!LEO  !! external routine to link information & data from data of other packages
!LEO  INTERFACE 
!LEO     SUBROUTINE ext_ds_ini_data_import ( )
!LEO     END SUBROUTINE ext_ds_ini_data_import
!LEO  END INTERFACE
!LEO  !
!LEO  !! external routine to determine contents of "dredge_poly_index(:)"
!LEO  INTERFACE
!LEO     SUBROUTINE ext_ds_dredge_poly_index ( )
!LEO     END SUBROUTINE ext_ds_dredge_poly_index
!LEO  END INTERFACE
!LEO  !
!LEO  !! external routine to determine contents of "dredge_poly_index(:)"
!LEO  INTERFACE
!LEO     SUBROUTINE ext_ds_dredge_node_index ( )
!LEO     END SUBROUTINE ext_ds_dredge_node_index
!LEO  END INTERFACE
!LEO  !
!LEO  !! external routine to determine contents of "fraction_name(:)"
!LEO  INTERFACE
!LEO     SUBROUTINE ext_ds_fraction_name ( )
!LEO     END SUBROUTINE ext_ds_fraction_name
!LEO  END INTERFACE
  !
  ! [C.7] public methods
  !
  PUBLIC :: init_dredgesim             ! 
  PUBLIC :: clear_dredgesim            ! 
  PUBLIC :: setup_dredgesim_prn_lun    ! 
  PUBLIC :: setup_dredgesim_trc_lun    ! 
  PUBLIC :: setup_dredgesim_language   ! 
  PUBLIC :: print_dredgesim_static     ! 
  PUBLIC :: print_dredgesim_all_errors ! 
  PUBLIC :: get_dredgesim_language     !
  PUBLIC :: start_dredgesim            ! 
!LEO  PUBLIC :: prepare_dredgesim          ! 
  PUBLIC :: run_dredgesim              ! 
  PUBLIC :: stop_dredgesim             ! 
  PUBLIC :: setup_ds_steering_file ! 
  PUBLIC :: setup_ds_printer_file  ! 
  PUBLIC :: setup_ds_trace_file    ! 
  PUBLIC :: setup_ds_initial_time  ! 
  PUBLIC :: setup_ds_end_time      ! 
  PUBLIC :: setup_ds_threshold     ! 
  PUBLIC :: setup_ds_debug
  PUBLIC :: get_ds_steering_file   ! 
  PUBLIC :: get_ds_printer_file    ! 
  PUBLIC :: get_ds_trace_file      ! 
  PUBLIC :: get_ds_initial_time    ! 
  PUBLIC :: get_ds_end_time        ! 
  PUBLIC :: get_ds_act_timestep    ! 
  PUBLIC :: get_ds_threshold       ! 
  !
  PUBLIC :: set_ds_nodes_of_poly
  PUBLIC :: set_ds_edgelist_of_poly
  PUBLIC :: set_ds_nodelist_of_poly
  PUBLIC :: set_ds_center_coord
  PUBLIC :: set_ds_grav_center_coord
  PUBLIC :: set_ds_node_coord
  PUBLIC :: set_ds_poly_depth
  PUBLIC :: set_ds_poly_area
  PUBLIC :: set_ds_node_area
  PUBLIC :: set_ds_knolg
  PUBLIC :: set_ds_node_neighb
  PUBLIC :: set_ds_nb_neighb_pt             ! number of shared points
  PUBLIC :: set_ds_list_send                ! list of processor numbers
  PUBLIC :: set_ds_nh_com                   ! global number of shared points
  PUBLIC :: set_ds_buf_recv                 ! buffer for receiving data
  PUBLIC :: set_ds_buf_send                 ! buffer for sending data
  PUBLIC :: set_ds_node_depth
  PUBLIC :: set_ds_node_noero_depth
  PUBLIC :: set_ds_edge_depth
  PUBLIC :: set_ds_edge_water_depth
  PUBLIC :: set_ds_node_water_depth
  PUBLIC :: set_ds_node_porosity
  PUBLIC :: set_ds_cell_sediment_fraction
  PUBLIC :: set_ds_node_sediment_fraction
  PUBLIC :: set_ds_dredge_poly_index
  PUBLIC :: set_ds_dredge_node_index
  PUBLIC :: set_ds_dispose_poly_index
  PUBLIC :: set_ds_dispose_node_index
  PUBLIC :: set_ds_dredge_poly_index_tc
  PUBLIC :: set_ds_dredge_node_index_tc
  PUBLIC :: set_ds_dispose_poly_index_tc
  PUBLIC :: set_ds_dispose_node_index_tc
  PUBLIC :: set_ds_art_bed_load_pol_index
  PUBLIC :: set_ds_art_bed_load_nod_index
  PUBLIC :: set_ds_used_sediment_classes
  PUBLIC :: set_ds_fraction_name
  PUBLIC :: set_ds_navigation_possible
  PUBLIC :: get_ds_navigation_possible
  PUBLIC :: get_ds_limiting_discharge
  PUBLIC :: get_ds_input_files_ref  !
  PUBLIC :: get_ds_center_coord_ref !
  PUBLIC :: get_ds_grav_center_coord_ref !
  PUBLIC :: get_ds_node_coord_ref !
  PUBLIC :: get_ds_nof_dredge_poly  ! 
  PUBLIC :: get_ds_dredge_poly_name !
  PUBLIC :: get_ds_nof_dispose_poly  !
  PUBLIC :: get_ds_node_sediment_fraction !
  PUBLIC :: get_ds_dispose_poly_name !
  PUBLIC :: get_ds_nof_dredge_poly_tc  !
  PUBLIC :: get_ds_dredge_poly_name_tc !
  PUBLIC :: get_ds_nof_dispose_poly_tc  !
  PUBLIC :: get_ds_dispose_poly_name_tc !
  PUBLIC :: get_ds_nof_art_bed_load_poly !
  PUBLIC :: get_ds_art_bed_load_poly_name !
  !
!LEO  PUBLIC :: ext_ds_ini_data_read   ! 
!LEO PUBLIC :: ext_ds_ini_grid_import ! 
!LEO  PUBLIC :: ext_ds_ini_data_import ! 
!LEO  PUBLIC :: ext_ds_dredge_poly_index !
!LEO  PUBLIC :: ext_ds_dredge_node_index ! 
!LEO  PUBLIC :: ext_ds_fraction_name   ! 
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data types, internal data und methods (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] local type defintions
  !
  ! [D.2] constant values
  !
  !! name of the module
  CHARACTER (LEN=14), PARAMETER :: c_modname      = 'p_dredgesim_ui' ! 
  !! parameter for TRACE/PRINT-methods (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer for TRACE/PRINT-methods (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !
  ! [D.3] variables
  !
  !! enumerator for initialization
  INTEGER                , SAVE :: n_init      = 0        ! 
  !
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
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-methods <<< [ERR_NO =  1000 to  1999]
  ! ----------------------------------------------------------------------
  !
  !! allocating/initializing static data of the package 
  !! subroutine generates error messages
  SUBROUTINE init_dredgesim_d ( )
    !! name of subroutine
    use m_dredgesim_data, only : debug_ds
    CHARACTER (LEN=16), PARAMETER :: c_upname='init_dredgesim_d' ! 
    !

    IF ( .NOT. initialised ) THEN
       !
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "p_dredgesim_ui" version ... <'
          WRITE(*,*) ' Copyright (C) 2007 Bundesanstalt fuer Wasserbau '
          WRITE(*,*)
       END IF
       !
       CALL init_error ( )
       !
       IF ( no_error( ) ) CALL init_file      ( )
       IF ( no_error( ) ) CALL init_grain     ( )
       IF ( no_error( ) ) CALL init_time      ( )
       IF ( no_error( ) ) CALL init_datetime  ( )
       IF ( no_error( ) ) CALL init_phy       ( )
       IF ( no_error( ) ) CALL init_io_info   ( )
       IF ( no_error( ) ) CALL init_criterion ( )
       initialised = .true.
       IF ( no_error( ) ) CALL init_dredgesim_errors ( ) 
       IF ( no_error( ) ) CALL init_dredgesim_data   ( ) 
       prn_lun = c_lun
       trc_lun = c_lun
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    n_init = n_init + 1
    !
  END SUBROUTINE init_dredgesim_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-methods <<< [ERR_NO =  2000 to  2999]
  ! ----------------------------------------------------------------------
  !
  !! deallocating/deinitializing static data of the package 
  !! subroutine generates error messages
  SUBROUTINE clear_dredgesim_d ( )
    !! name of subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='clear_dredgesim_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       IF ( no_error( ) ) CALL clear_dredgesim_data ( ) 
       IF ( no_error( ) ) CALL clear_dredgesim_errors ( ) 
       prn_lun = c_lun
       trc_lun = c_lun
       initialised = MERGE( .false., .true., no_error( ) )
       IF ( no_error( ) ) CALL clear_criterion ( )
       IF ( no_error( ) ) CALL clear_io_info   ( )
       IF ( no_error( ) ) CALL clear_phy       ( )
       IF ( no_error( ) ) CALL clear_datetime  ( )
       IF ( no_error( ) ) CALL clear_time      ( )
       IF ( no_error( ) ) CALL clear_grain     ( )
       IF ( no_error( ) ) CALL clear_file      ( )
       IF ( no_error( ) ) CALL clear_error     ( )
    END IF
    n_init = n_init - 1
    !
  END SUBROUTINE clear_dredgesim_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-methods <<< [ERR_NO =  3000 to  3999]
  ! ----------------------------------------------------------------------
  !
  !! setting channel number for PRINT-methods
  !! subroutine generates error messages
  SUBROUTINE setup_dredgesim_prn_lun_d ( lun )
    !! current channel number 
    INTEGER , INTENT(IN) :: lun ! 
    ! local parameters and variables
    !! name of subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_dredgesim_prn_lun_d' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL setup_error_prn_lun     ( lun )
       IF ( no_error( ) ) CALL setup_file_prn_lun      ( lun )
       IF ( no_error( ) ) CALL setup_time_prn_lun      ( lun )
       IF ( no_error( ) ) CALL setup_datetime_prn_lun  ( lun )
       IF ( no_error( ) ) CALL setup_phy_prn_lun       ( lun )
       IF ( no_error( ) ) CALL setup_io_info_prn_lun   ( lun )
       IF ( no_error( ) ) CALL setup_criterion_prn_lun ( lun )
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
    END IF
    !
  END SUBROUTINE setup_dredgesim_prn_lun_d
  !
  !! setting of channel number for TRACE-methods  
  !! subroutine generates error messages
  SUBROUTINE setup_dredgesim_trc_lun_d ( lun )
    !! current channel number for TRACE-methods 
    INTEGER , INTENT(IN) :: lun ! 
    !! name of subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_dredgesim_trc_lun_d' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL setup_error_trc_lun     ( lun )
       IF ( no_error( ) ) CALL setup_file_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_time_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_datetime_trc_lun  ( lun )
       IF ( no_error( ) ) CALL setup_phy_trc_lun       ( lun )
       IF ( no_error( ) ) CALL setup_io_info_trc_lun   ( lun )
       IF ( no_error( ) ) CALL setup_criterion_trc_lun ( lun )
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
    END IF
    !
  END SUBROUTINE setup_dredgesim_trc_lun_d
  !
  !! setting index for language setup 
  !! 1 = German
  !! 2 = English 
  SUBROUTINE setup_dredgesim_language_d ( var )
    !! index for language setting (1 = German, 2 = English )
    INTEGER , INTENT(IN) :: var ! 
    !! name of subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_gei_language_d' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       language = MERGE ( var, c_def_language, ( 1 <= var .AND. var <= c_max_language ) )
       IF ( no_error( ) ) CALL setup_datetime_language ( language )
       IF ( no_error( ) ) CALL setup_phy_language      ( language ) 
    END IF
    !
  END SUBROUTINE setup_dredgesim_language_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-methods <<< [ERR_NO =  4000 to  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-methods <<< [ERR_NO =  5000 to  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-methods <<< [ERR_NO =  6000 to  6999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-methods <<< [ERR_NO =  7000 to  7999]
  ! ----------------------------------------------------------------------
  !
  !! printing all essential static data of the package 
  !! subroutine generates error messages
  SUBROUTINE print_dredgesim_static_d ( )
    !! name of function
    CHARACTER (LEN=24), PARAMETER :: c_upname='print_dredgesim_static_d' ! 
    !! stat variable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) &
            initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, &
            c_max_language, c_def_language, language, &
            c_undef_ch, c_undef_in, c_undef_re, c_undef_dp
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_dredgesim_all_errors_d ( )
    ELSE
       WRITE(*,*) ' >>> subroutine '//TRIM(c_upname)//' - no printable output '
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# current static data of p_dredgesim_ui                      ',/ &
    '# without data stored in package objects                     ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#      initialised = ',L1,/    &
    '#           prn_op = ',L1,/    &
    '#           trc_op = ',L1,/    &
    '#          prn_lun = ',I5,/    &
    '#          trc_lun = ',I5,/    &
    '#           n_init = ',I5,/    &
    '#   c_max_language = ',I5,/    &
    '#   c_def_language = ',I5,/    &
    '#         language = ',I5,/    &
    '#       c_undef_ch = ',A,/     &
    '#       c_undef_in = ',I10,/   &
    '#       c_undef_re = ',G15.7,/ &
    '#       c_undef_dp = ',G15.7,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_dredgesim_static_d
  !
  !! printing all error messages of package 
  !! subroutine generates error messages
  SUBROUTINE print_dredgesim_all_errors_d ( )
    !! name of function
    CHARACTER (LEN=28), PARAMETER :: c_upname='print_dredgesim_all_errors_d' ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       IF ( no_error( ) ) CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> subroutine '//TRIM(c_upname)//' - no pritable output '
    END IF
    !
  END SUBROUTINE print_dredgesim_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-methods <<< [ERR_NO =  8000 to  8999]
  ! ----------------------------------------------------------------------
  !
  !! setting steering file of the "dredgesim"-package 
  !! data is copied to data module 
  !! subroutine does not throw error messages
  SUBROUTINE setup_ds_steering_file_d ( var )
    USE b_file, ONLY : t_file ! 
    USE m_dredgesim_data, ONLY : steering_file ! 
    !! steering file of "dredgesim"-package
    TYPE (t_file) , INTENT(IN) :: var ! 
    !
    steering_file = var
    !
  END SUBROUTINE setup_ds_steering_file_d
  !
  !! setting printer file of "dredgesim"-package 
  !! data is copied to data module 
  !! subroutine does not throw error messages
  SUBROUTINE setup_ds_printer_file_d ( var )
    USE b_file, ONLY : t_file ! 
    USE m_dredgesim_data, ONLY : printer_file ! 
    !! printer file of the "dredgesim"-package
    TYPE (t_file) , INTENT(IN) :: var ! 
    !
    printer_file = var
    !
  END SUBROUTINE setup_ds_printer_file_d
  !  
  !! setting trace file of the "dredgesim"-package 
  !! data is copied to data module 
  !! subroutine does not throw error messages
  SUBROUTINE setup_ds_trace_file_d ( var )
    USE b_file, ONLY : t_file ! 
    USE m_dredgesim_data, ONLY : trace_file ! 
    !! trace file of the "dredgesim"-package
    TYPE (t_file) , INTENT(IN) :: var ! 
    !
    trace_file = var
    !
  END SUBROUTINE setup_ds_trace_file_d
  !  
  !! setting of initial time 
  !! subroutine does not throw error messages
  SUBROUTINE setup_ds_initial_time_d ( var )
    USE m_dredgesim_data, ONLY : initial_time, act_time, old_time ! 
    !! date and time for initial state
    TYPE (t_datetime) , INTENT(IN) :: var ! 
    !
    print*,'Leo setup_ds_initial_time'
    initial_time     = var
    act_time         = initial_time
    old_time         = initial_time
    print*,'Leo setup_ds_initial_time end'
    !
  END SUBROUTINE setup_ds_initial_time_d
  !  
  !! setting end time
  !! subroutine does not throw error messages
  SUBROUTINE setup_ds_end_time_d ( var )
    USE m_dredgesim_data, ONLY : end_time ! 
    !! date and time for end of dredgesim
    TYPE (t_datetime) , INTENT(IN) :: var ! 
    !
    end_time = var
    ! 
  END SUBROUTINE setup_ds_end_time_d
  !  
  !! transfer threshold                         
  !! subroutine does not throw error messages
  SUBROUTINE setup_ds_threshold_d ( var )
    USE m_dredgesim_data, ONLY : c_threshold_double ! 
    !! limiting value
    REAL (KIND=Double) , INTENT(IN) :: var ! 
    !
    c_threshold_double = var
    !
  END SUBROUTINE setup_ds_threshold_d
  !
  !! setting nodes per element 
  !! pointer on external data
  SUBROUTINE set_ds_nodes_of_poly_d ( nodes_of_poly ) 
    USE m_dredgesim_data, ONLY : set_ds_nodes_of_poly_ref
    !! nodes_of_poly(i) : number of nodes on element i 
    INTEGER , POINTER :: nodes_of_poly(:)  ! 
    !
    CALL set_ds_nodes_of_poly_ref ( nodes_of_poly )
    !
  END SUBROUTINE set_ds_nodes_of_poly_d
  !! setting of debug switch
  !! subroutine does not throw error messages
  SUBROUTINE setup_ds_debug_i ( var )
   ! debug switch
    USE m_dredgesim_data, ONLY : debug_ds
    INTEGER :: var
    !
    debug_ds   = var
    !
  END SUBROUTINE setup_ds_debug_i
  !
  !! array of edgelist per element
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_edgelist_of_poly_d ( edgelist_of_poly )
    !
    USE m_dredgesim_data, ONLY : set_ds_edgelist_of_poly_ref !
    !! edgelist_of_poly(i,l) : index j of edge l in element i
    INTEGER, POINTER :: edgelist_of_poly(:,:) ! 
    !
    CALL set_ds_edgelist_of_poly_ref ( edgelist_of_poly )
    !
  END SUBROUTINE set_ds_edgelist_of_poly_d
  !
  !! array of nodelist per element 
  !! subroutine does not throw error messages
  SUBROUTINE set_ds_nodelist_of_poly_d ( nodelist_of_poly )
    !
    USE m_dredgesim_data, ONLY : set_ds_nodelist_of_poly_ref !
    !! nodelist_of_poly(i,l) : index j node l in element i
    INTEGER, POINTER :: nodelist_of_poly(:,:) ! 
    !
    CALL set_ds_nodelist_of_poly_ref ( nodelist_of_poly )
    !
  END SUBROUTINE set_ds_nodelist_of_poly_d  
  !
  !! setting of center coordinates
  !! pointer on external data
  SUBROUTINE set_ds_center_coord_d ( center_coord ) 
    USE m_dredgesim_data, ONLY : set_ds_center_coord_ref
    !! center coords of elements
    !! center_coord(i,:) : coordinates for eement i 
    !! center_coord(i,1) : x-coordinate of element center i 
    !! center_coord(i,2) : y-coordinate of element center i
    REAL (KIND=Double) , POINTER :: center_coord(:,:)  ! 
    !
    CALL set_ds_center_coord_ref ( center_coord )
    !
  END SUBROUTINE set_ds_center_coord_d
  !! setting gravitational center coordinates
  !! pointer on external data
  SUBROUTINE set_ds_grav_center_coord_d ( grav_center_coord ) 
    USE m_dredgesim_data, ONLY : set_ds_grav_center_coord_ref
    !! grav center coords of elements
    !! center_coord(i,:) : coordinates for element i 
    !! center_coord(i,1) : x-coordinate of grav element center i 
    !! center_coord(i,2) : y-coordinate of grav element center i
    REAL (KIND=Double) , POINTER :: grav_center_coord(:,:)  ! 
    !
    CALL set_ds_grav_center_coord_ref ( grav_center_coord )
    !
  END SUBROUTINE set_ds_grav_center_coord_d
  !
  !! setting node coordinates
  !! pointer on external data
  SUBROUTINE set_ds_node_coord_d ( node_coord ) 
    USE m_dredgesim_data, ONLY : set_ds_node_coord_ref
    !! node coords
    REAL (KIND=Double) , POINTER :: node_coord(:,:)  ! 
    !
    CALL set_ds_node_coord_ref ( node_coord )
    !
  END SUBROUTINE set_ds_node_coord_d
  !
  !! setting element depths
  !! pointer on external data
  SUBROUTINE set_ds_poly_depth_d ( poly_depth )
    USE m_dredgesim_data, ONLY : set_ds_poly_depth_ref
    !! depths of elements
    !! poly_depth(i) : depth of element i
    REAL (KIND=Double) , POINTER :: poly_depth(:)  ! 
    !
    CALL set_ds_poly_depth_ref ( poly_depth )
    !
  END SUBROUTINE set_ds_poly_depth_d
  !
  !! setting element areas
  !! pointer on external data
  SUBROUTINE set_ds_poly_area_d ( poly_area )
    USE m_dredgesim_data, ONLY : set_ds_poly_area_ref
    !! areas of elements
    !! poly_area(i) : area of element i
    REAL (KIND=Double) , POINTER :: poly_area(:)  ! 
    !
    CALL set_ds_poly_area_ref ( poly_area )
    !
  END SUBROUTINE set_ds_poly_area_d 
  !! setting node areas
  !! pointer on external data
  SUBROUTINE set_ds_node_area_d ( node_area )
    USE m_dredgesim_data, ONLY : set_ds_node_area_ref
    !! areas of elements
    !! node_area(i) : area of node i
    REAL (KIND=Double) , POINTER :: node_area(:)  ! 
    !
    CALL set_ds_node_area_ref(node_area)
    !
  END SUBROUTINE set_ds_node_area_d 
  !! setting nodelist of global / local 
  !! pointer on external data
  SUBROUTINE set_ds_knolg_i ( knolg )
    USE m_dredgesim_data, ONLY : set_ds_knolg_ref
    !! areas of elements
    !! knolg(i) :  global number of local number i
    INTEGER , POINTER :: knolg(:)  ! 
    !
    CALL set_ds_knolg_ref ( knolg )
    !
  END SUBROUTINE set_ds_knolg_i
  !! setting node areas
  !! pointer on external data
  SUBROUTINE set_ds_node_neighb_i ( node_neighb )
    USE m_dredgesim_data, ONLY : set_ds_node_neighb_ref
    !! node neighb
    !! node_neighb(i) : neighbour of node i
    INTEGER , POINTER :: node_neighb(:)  ! 
    !
    CALL set_ds_node_neighb_ref ( node_neighb )
    !
  END SUBROUTINE set_ds_node_neighb_i
  !
  !! setting the number of points shared with a sub-domain
  !! pointer on external data
  SUBROUTINE set_ds_nb_neighb_pt_i (nb_neighb_pt)
    USE m_dredgesim_data, ONLY : set_ds_nb_neighb_pt_ref
    !
    INTEGER, POINTER  :: nb_neighb_pt(:)
    !
   CALL set_ds_nb_neighb_pt_ref ( nb_neighb_pt )
    !
  END SUBROUTINE set_ds_nb_neighb_pt_i
  !! setting the LISt_send
  !! pointer on external data
  SUBROUTINE set_ds_list_send_i (list_send)
    USE m_dredgesim_data, ONLY : set_ds_list_send_ref
    !
    ! result value
    INTEGER, POINTER  :: list_send(:)
    !
   CALL set_ds_list_send_ref (list_send)
    !
  END SUBROUTINE set_ds_list_send_i

  !! global numbers for shared points
  !! pointer on external data
  SUBROUTINE set_ds_nh_com_i (nh_com)
   USE m_dredgesim_data, ONLY : set_ds_nh_com_ref

   INTEGER, POINTER  :: nh_com(:,:)
   !
  CALL set_ds_nh_com_ref(nh_com)
    !
  END SUBROUTINE set_ds_nh_com_i
  !! getting buffer for received data
  !! pointer on external data
  SUBROUTINE set_ds_buf_recv_d(buf_recv)
   USE m_dredgesim_data, ONLY : set_ds_buf_recv_ref

   DOUBLE PRECISION, POINTER  :: buf_recv(:,:)
   !
    CALL set_ds_buf_recv_ref(buf_recv)
    !
  END SUBROUTINE set_ds_buf_recv_d
  !! getting the buffer for sending data
  !! pointer on external data
  SUBROUTINE set_ds_buf_send_d (buf_send)
   USE m_dredgesim_data, ONLY : set_ds_buf_send_ref
    !
    DOUBLE PRECISION,POINTER  :: buf_send(:,:)
    !
   CALL set_ds_buf_send_ref (buf_send)
    !
  END SUBROUTINE set_ds_buf_send_d

  !! setting node depths 
  !! pointer on external data
  SUBROUTINE set_ds_node_depth_d ( node_depth )
    USE m_dredgesim_data, ONLY : set_ds_node_depth_ref
    !! depth of nodes 
    !! node_depth(l) : depth of node l
    REAL (KIND=Double) , POINTER :: node_depth(:)  ! 
    !
    CALL set_ds_node_depth_ref ( node_depth )
    !
  END SUBROUTINE set_ds_node_depth_d
  !
  !! setting node depths 
  !! pointer on external data
  SUBROUTINE set_ds_node_noero_depth_d ( node_noero_depth )
    USE m_dredgesim_data, ONLY : set_ds_node_noero_depth_ref
    !! depth of nodes 
    !! node_depth(l) : depth of node l
    REAL (KIND=Double) , POINTER :: node_noero_depth(:)  ! 
    !
    CALL set_ds_node_noero_depth_ref ( node_noero_depth )
    !
  END SUBROUTINE set_ds_node_noero_depth_d
  !
  !! setting edge depths
  !! pointer on external data
  SUBROUTINE set_ds_edge_depth_d ( edge_depth )
    USE m_dredgesim_data, ONLY : set_ds_edge_depth_ref
    !! depth of edges 
    !! edge_depth(j) : depth of edge j
    REAL (KIND=Double) , POINTER :: edge_depth(:)  ! 
    !
    CALL set_ds_edge_depth_ref ( edge_depth )
    !
  END SUBROUTINE set_ds_edge_depth_d
  !
  !! setting water depth on edges
  !! pointer on external data
  SUBROUTINE set_ds_edge_water_depth_d ( edge_water_depth )
    USE m_dredgesim_data, ONLY : set_ds_edge_water_depth_ref
    !! water depth on edges
    !! edge_water_depth(i) : water depth on edge i
    REAL (KIND=Double) , POINTER :: edge_water_depth(:)  ! 
    !
    CALL set_ds_edge_water_depth_ref ( edge_water_depth )
    !
  END SUBROUTINE set_ds_edge_water_depth_d 
  !
  !! setting water depth on nodes
  !! pointer on external data
  SUBROUTINE set_ds_node_water_depth_d ( node_water_depth )
    USE m_dredgesim_data, ONLY : set_ds_node_water_depth_ref
    !! water depth on nodes
    !! node_water_depth(i) : water depth on edge i
    REAL (KIND=Double) , POINTER :: node_water_depth(:)  ! 
    !
    CALL set_ds_node_water_depth_ref ( node_water_depth )
    !
  END SUBROUTINE set_ds_node_water_depth_d 
  !
  !! setting porosity of elements
  !! pointer on external data
  SUBROUTINE set_ds_node_porosity_d ( node_porosity )
    USE m_dredgesim_data, ONLY : set_ds_node_porosity_ref
    !! porosity on elements (upper layer) 
    !! node_porosity(i) : porosity on node i
    REAL (KIND=Double) , POINTER :: node_porosity(:)  ! 
    !
    CALL set_ds_node_porosity_ref ( node_porosity )
    !
  END SUBROUTINE set_ds_node_porosity_d
  !
  !! setting sediment fraction on elements
  !! pointer on external data
  SUBROUTINE set_ds_cell_sediment_fraction_d ( cell_sediment_fraction )
    USE m_dredgesim_data, ONLY : set_ds_cell_sediment_frac_ref
    !! sediment fractions on elements (upper layer) 
    !! cell_sediment_fraction(i,n) : percentage of fraction n on element i
    REAL (KIND=Double) , POINTER :: cell_sediment_fraction(:,:)  ! 
    !
    CALL set_ds_cell_sediment_frac_ref ( cell_sediment_fraction )
    !
  END SUBROUTINE set_ds_cell_sediment_fraction_d
  !
  !! setting sediment fraction on nodes
  !! pointer on external data
  SUBROUTINE set_ds_node_sediment_fraction_d ( node_sediment_fraction )
    USE m_dredgesim_data, ONLY : set_ds_node_sediment_frac_ref
    !! sediment fractions on nodes (upper layer) 
    !! node_sediment_fraction(i,n) : percentage of fraction n on element i
    REAL (KIND=Double) , POINTER :: node_sediment_fraction(:,:)  ! 
    !

    CALL set_ds_node_sediment_frac_ref ( node_sediment_fraction )
    !
  END SUBROUTINE set_ds_node_sediment_fraction_d
  !
  !! setting index list element - dredge polygon
  !! copy of data in data module
  SUBROUTINE set_ds_dredge_poly_index_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_dredge_poly_index
    !! array of index list element - dredge polygon
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_dredge_poly_index( var )
    !
  END SUBROUTINE set_ds_dredge_poly_index_d
  !
  !! setting index list node - dredge polygon
  !! copy of data in data module
  SUBROUTINE set_ds_dredge_node_index_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_dredge_node_index
    !! array of index list node - dredge polygon
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_dredge_node_index( var )
    !
  END SUBROUTINE set_ds_dredge_node_index_d
  !
  !! setting index list element - disposal polygon
  !! copy of data in data module
  SUBROUTINE set_ds_dispose_poly_index_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_dispose_poly_index
    !! array of index list element - disposal polygon
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_dispose_poly_index( var )
    !
  END SUBROUTINE set_ds_dispose_poly_index_d
  !
  !! setting index list node - disposal polygon
  !! copy of data in data module
  SUBROUTINE set_ds_dispose_node_index_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_dispose_node_index
    !! array of index list node - disposal polygon
    INTEGER , INTENT(IN) :: var(:,:,:) ! 
    !
    CALL store_ds_dispose_node_index( var )
    !
  END SUBROUTINE set_ds_dispose_node_index_d
  !
  !! setting index list element - dredge polygontime controlled 
  !! copy of data in data module
  SUBROUTINE set_ds_dredge_poly_index_tc_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_dredge_poly_index_tc
    !! array of index list element - dredge polygon time controlled 
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_dredge_poly_index_tc( var )
    !
  END SUBROUTINE set_ds_dredge_poly_index_tc_d
  !
  !! setting index list node - dredge polygontime controlled 
  !! copy of data in data module
  SUBROUTINE set_ds_dredge_node_index_tc_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_dredge_node_index_tc
    !! array of index list node - dredge polygon time controlled 
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_dredge_node_index_tc( var )
    !
  END SUBROUTINE set_ds_dredge_node_index_tc_d
  !
  !! setting index list element - disposal polygon time controlled 
  !! copy of data in data module
  SUBROUTINE set_ds_dispose_poly_index_tc_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_dispose_poly_index_tc
    !! array of index list element - disposal polygon time controlled
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_dispose_poly_index_tc( var )
    !
  END SUBROUTINE set_ds_dispose_poly_index_tc_d
  !
  !! setting index list node - disposal polygon time controlled 
  !! copy of data in data module
  SUBROUTINE set_ds_dispose_node_index_tc_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_dispose_node_index_tc
    !! array of index list node - disposal polygon time controlled
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_dispose_node_index_tc( var )
    !
  END SUBROUTINE set_ds_dispose_node_index_tc_d
  !
  !! setting index list element - disposal polygon for artificial bed load supply 
  !! copy of data in data module
  SUBROUTINE set_ds_art_bed_load_pol_index_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_art_bed_load_pol_index
    !! array of index list element - dispsoal polygon for artificial bed load supply
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_art_bed_load_pol_index( var )
    !
  END SUBROUTINE set_ds_art_bed_load_pol_index_d
  !
  !! setting index list node - disposal polygon for artificial bed load supply 
  !! copy of data in data module
  SUBROUTINE set_ds_art_bed_load_nod_index_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_art_bed_load_nod_index
    !! array of index list node - dispsoal polygon for artificial bed load supply
    INTEGER , INTENT(IN) :: var(:,:) ! 
    !
    CALL store_ds_art_bed_load_nod_index( var )
    !
  END SUBROUTINE set_ds_art_bed_load_nod_index_d
  !
  !! setting array with names of sediment fractions 
  !! copy of data in data module
  SUBROUTINE set_ds_fraction_name_d ( var )
    USE m_dredgesim_data, ONLY : store_ds_fraction_name
    !! array with names of sediment fractions
    CHARACTER (LEN=*) , INTENT(IN) :: var(:) ! 
    !
    CALL store_ds_fraction_name( var, SHAPE(var) )
    !
  END SUBROUTINE set_ds_fraction_name_d
  !
  !! setting the values for "navigation_possible"
  !! data is copied
  SUBROUTINE set_ds_navigation_possible_d ( var )
    USE m_dredgesim_data, ONLY : navigation_possible
    !! array with information whether navigation is possible
    LOGICAL, INTENT(IN) :: var(:) ! 
    !
    IF ( no_error( ) ) navigation_possible(:) = var(:)
    !
  END SUBROUTINE set_ds_navigation_possible_d
  !
  !! setting data type of used sediment classes within "dredgesim"-package 
  !! copy of data in data module 
  !! subroutine generates error messages
  SUBROUTINE set_ds_used_sediment_classes_d &
       ( var )
    !
   USE b_grain, ONLY :          &
         ! type definition
         t_grain,                &
         all_grain_available
    USE m_dredgesim_data, ONLY : &
         ! Daten
         used_sediment_classes,       &
         alloc_used_sediment_classes, &
         predef_disp_sed_class
    !
    !! sediment classification
    TYPE(t_grain) , INTENT(IN) :: var(:) ! 
    !! local parameters
    CHARACTER (LEN=30), PARAMETER :: c_upname='set_ds_used_sediment_classes_d' ! 
    !! Looping
    INTEGER :: i
    !
    CALL alloc_used_sediment_classes ( SIZE(var) )

    !
    IF ( no_error( ) ) THEN
       used_sediment_classes(:) = var(:)
       !TODO LEO I think this loop is wrong since  predef_disp_sed_class(i,:)(1:40)
       !was not allocated! 
       IF ( ALLOCATED(predef_disp_sed_class)) THEN   !LEO NEW TODO check if this is correct!    
         DO i=1,SIZE(predef_disp_sed_class(:,1)) 
            IF ( .NOT. ALL( all_grain_available ( used_sediment_classes(:), predef_disp_sed_class(i,:)(1:40) ) ) ) THEN
                  PRINT*, 'Names of Sediment Fractions not Ok!'
            END IF
         END DO
       END IF !LEO END NEW
    END IF

    !
  END SUBROUTINE set_ds_used_sediment_classes_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-methods <<< [ERR_NO =  9000 to  9999]
  ! ----------------------------------------------------------------------
  !
  !! getting the index for language setting
  !! 1 = German
  !! 2 = English 
  !! copy of internal data is recieved
  FUNCTION get_dredgesim_language_d ( ) &
       RESULT( res )
    !! return value: 
    !! Index for language (1 = German, 2 = English )
    INTEGER :: res ! 
    !! name of subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_dredgesim_language_d' ! 
    !
    res = language
    !
  END FUNCTION get_dredgesim_language_d
  !
  !! getting the steering file of the "dredgesim"-package 
  !! copy of internal data is recieved 
  !! function does not throw error messages
  FUNCTION get_ds_steering_file_d ( ) &
       RESULT( var )
    USE b_file, ONLY : t_file ! 
    USE m_dredgesim_data, ONLY : steering_file ! 
    !! steering file of "dredgesim"-package
    TYPE (t_file) :: var ! 
    !
    var = steering_file
    !
  END FUNCTION get_ds_steering_file_d
  !
  !! getting the printer file of the "dredgesim"-package 
  !! copy of internal data is recieved 
  !! function does not throw error messages
  FUNCTION get_ds_printer_file_d ( ) &
       RESULT( var )
    USE b_file, ONLY : t_file ! 
    USE m_dredgesim_data, ONLY : printer_file ! 
    !! printer file of the "dredgesim"-package
    TYPE (t_file) :: var ! 
    !
    var = printer_file
    !
  END FUNCTION get_ds_printer_file_d
  !
  !! getting the trace file of the "dredgesim"-package 
  !! copy of internal data is recieved 
  !! function does not throw error messages
  FUNCTION get_ds_trace_file_d ( ) &
       RESULT( var )
    USE b_file, ONLY : t_file ! 
    USE m_dredgesim_data, ONLY : trace_file ! 
    !! trace file of the "dredgesim"-package
    TYPE (t_file) :: var ! 
    !
    var = trace_file
    !
  END FUNCTION get_ds_trace_file_d
  !
  !! getting the initial time of the "dredgesim"-package 
  !! copy of internal data is recieved 
  !! function does not throw error messages
  FUNCTION get_ds_initial_time_d ( ) &
       RESULT( var )
    USE m_dredgesim_data, ONLY : initial_time ! 
    !! initial time of the "dredgesim"-package
    TYPE (t_datetime) :: var ! 
    !
    var = initial_time
    !
  END FUNCTION get_ds_initial_time_d
  !  
  !! getting the end time of the "dredgesim"-package 
  !! copy of internal data is recieved 
  !! function does not throw error messages
  FUNCTION get_ds_end_time_d ( ) &
       RESULT( var )
    USE m_dredgesim_data, ONLY : end_time ! 
    !! end time of the "dredgesim"-package
    TYPE (t_datetime) :: var ! 
    !
    var = end_time
    !
  END FUNCTION get_ds_end_time_d
  !  
  !! getting the current timestep 
  !! function does not throw error messages
  FUNCTION get_ds_act_timestep_d ( ) &
       RESULT( var )
    USE m_dredgesim_data, ONLY : act_timestep ! 
    !! current timestep
    TYPE (t_time) :: var ! 
    !
    var = act_timestep
    !
  END FUNCTION get_ds_act_timestep_d
  !
  !! getting the threshold value 
  !! function does not throw error messages
  FUNCTION get_ds_threshold_d ( ) &
       RESULT( var )
    USE m_dredgesim_data, ONLY : c_threshold_double ! 
    !
    REAL (KIND=Double) :: var ! 
    !
    var = c_threshold_double
    !
  END FUNCTION get_ds_threshold_d
  !
  !! getting the information about input_files 
  !! function does not throw error messages
  FUNCTION get_ds_input_files_ref_d ( ) & 
       RESULT( var )
    USE m_dredgesim_data, ONLY : input_files ! 
    !
    TYPE (t_io_info) , POINTER :: var(:) ! 
    !
    var => input_files
    !
  END FUNCTION get_ds_input_files_ref_d
  !
  !! getting the x- und y-coordinates of element centers
  !! function does not throw error messages
  FUNCTION get_ds_center_coord_ref_d ( ) & 
       RESULT( var )
    USE m_dredgesim_data, ONLY : center_coord ! 
    !
    REAL (KIND=Double) , POINTER :: var(:,:) ! 
    !
    var => center_coord
    !
  END FUNCTION get_ds_center_coord_ref_d
  !
  !! getting the x- und y-coordinates element grav centers
  !! pointer on internal data is recieved       
  !! function does not throw error messages
  FUNCTION get_ds_grav_center_coord_ref_d ( ) & 
       RESULT( var )
    USE m_dredgesim_data, ONLY : grav_center_coord ! 
    !
    REAL (KIND=Double) , POINTER :: var(:,:) ! 
    !
    var => grav_center_coord
    !
  END FUNCTION get_ds_grav_center_coord_ref_d
  !
  !! getting the x- und y-coordinates of nodes
  !! pointer on internal data is recieved       
  !! function does not throw error messages
  FUNCTION get_ds_node_coord_ref_d ( ) & 
       RESULT( var )
    USE m_dredgesim_data, ONLY : node_coord ! 
    !
    REAL (KIND=Double) , POINTER :: var(:,:) ! 
    !
    var => node_coord
    !
  END FUNCTION get_ds_node_coord_ref_d
  
  !
  !! getting the number of dredge polygons defined in steering file 
  !! copy is recieved
  FUNCTION get_ds_nof_dredge_poly_d ( ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : get_nof_dredge_poly
    !
    INTEGER :: res ! 
    !
    res = get_nof_dredge_poly( )
    !
  END FUNCTION get_ds_nof_dredge_poly_d
  !
  !! getting the name of dredge polygon n
  !! copy is recieved
  FUNCTION get_ds_dredge_poly_name_0 ( n ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : c_undef_ch, dredge_criterion
    !
    INTEGER , INTENT(IN) :: n ! 
    !
    CHARACTER (LEN=c_max_poly_name_len) :: res ! 
    !
    res = REPEAT( c_undef_ch, LEN(res) )
    IF ( ALLOCATED(dredge_criterion) ) THEN
       IF ( n >= 1 .AND. n <= SIZE(dredge_criterion) ) THEN
          res = get_criterion_poly_name( dredge_criterion(n) )
       END IF
    END IF
    !
  END FUNCTION get_ds_dredge_poly_name_0
  !
  !! getting the number of disposal polygons defined in steering file 
  !! copy is recieved
  FUNCTION get_ds_nof_dispose_poly_d ( ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : nof_dispose_poly
    !
    INTEGER :: res ! 
    !
    res = nof_dispose_poly
    !
  END FUNCTION get_ds_nof_dispose_poly_d
  !
  !! getting the fractions of sediment material on each node 
  !! copy is recieved
  FUNCTION get_ds_node_sediment_fraction_d ( ) &
       RESULT( var )
    USE m_dredgesim_data, ONLY : node_sediment_fraction
    !
    REAL (KIND=Double), POINTER :: var(:,:) ! 
    !
    var => node_sediment_fraction
    !
  END FUNCTION get_ds_node_sediment_fraction_d
  !
  !! getting the name of disposal polygon n dependent on dredge polygon i 
  !! copy is recieved
  FUNCTION get_ds_dispose_poly_name_0 ( n, i ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : c_undef_ch, nof_dispose_poly, dispose_poly_name
    !! number of disposal polygon
    INTEGER , INTENT(IN) :: n !
    !! number of dredge polygon
    INTEGER , INTENT(IN) :: i ! 
    !
    CHARACTER (LEN=c_max_poly_name_len) :: res ! 
    !
    res = REPEAT( c_undef_ch, LEN(res) )
    IF ( ALLOCATED(dispose_poly_name) ) THEN
       IF ( n >= 1 .AND. n <= nof_dispose_poly ) THEN
          res = dispose_poly_name( n, i )
       END IF
    END IF
    !
  END FUNCTION get_ds_dispose_poly_name_0
  !! getting the number of dredge polygons for time controlled maintenance defined in steering file 
  !! copy is recieved
  FUNCTION get_ds_nof_dredge_poly_tc_d ( ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : nof_dredge_poly_tc
    !
    INTEGER :: res ! 
    !
    res = nof_dredge_poly_tc
    !
  END FUNCTION get_ds_nof_dredge_poly_tc_d
  !! getting the name of dredge polygon n for time controlled maintenance 
  !! copy is recieved
  FUNCTION get_ds_dredge_poly_name_tc_0 ( n ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : c_undef_ch, nof_dredge_poly_tc, dredge_poly_name_tc
    !
    INTEGER , INTENT(IN) :: n !
    !
    CHARACTER (LEN=c_max_poly_name_len) :: res ! 
    !
    res = REPEAT( c_undef_ch, LEN(res) )
    IF ( ALLOCATED(dredge_poly_name_tc) ) THEN
       IF ( n >= 1 .AND. n <= nof_dredge_poly_tc ) THEN
          res = dredge_poly_name_tc( n )
       END IF
    END IF
    !
  END FUNCTION get_ds_dredge_poly_name_tc_0
  !! getting the number of disposal polygons for time controlled maintenance defined in steering file 
  !! copy is recieved
  FUNCTION get_ds_nof_dispose_poly_tc_d ( ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : nof_dispose_poly_tc
    !
    INTEGER :: res ! 
    !
    res = nof_dispose_poly_tc
    !
  END FUNCTION get_ds_nof_dispose_poly_tc_d
  !! getting the name of disposal polygon n for time controlled maintenance 
  !! copy is recieved
  FUNCTION get_ds_dispose_poly_name_tc_0 ( n ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : c_undef_ch, nof_dispose_poly_tc, dispose_poly_name_tc
    !
    INTEGER , INTENT(IN) :: n !
    !
    CHARACTER (LEN=c_max_poly_name_len) :: res ! 
    !
    res = REPEAT( c_undef_ch, LEN(res) )
    IF ( ALLOCATED(dispose_poly_name_tc) ) THEN
       IF ( n >= 1 .AND. n <= nof_dispose_poly_tc ) THEN
          res = dispose_poly_name_tc( n )
       END IF
    END IF
    !
  END FUNCTION get_ds_dispose_poly_name_tc_0
  !! getting the number of disposal polygons for artificial bed load supply defined in steering file 
  !! copy is recieved
  FUNCTION get_ds_nof_art_bed_load_poly_d ( ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : nof_predef_disp_poly
    !
    INTEGER :: res ! 
    !
    res = nof_predef_disp_poly
    !
  END FUNCTION get_ds_nof_art_bed_load_poly_d
  !! getting the name of disposal polygon n for artificial bed load supply 
  !! copy is recieved
  FUNCTION get_ds_art_bed_load_poly_name_0 ( n ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : c_undef_ch, nof_predef_disp_poly, predef_disp_poly_name
    !
    INTEGER , INTENT(IN) :: n ! 
    !
    CHARACTER (LEN=c_max_poly_name_len) :: res ! 
    !
    res = REPEAT( c_undef_ch, LEN(res) )
    IF ( ALLOCATED(predef_disp_poly_name) ) THEN
       IF ( n >= 1 .AND. n <= nof_predef_disp_poly ) THEN
          res = predef_disp_poly_name( n )
       END IF
    END IF
    !
  END FUNCTION get_ds_art_bed_load_poly_name_0
  !! getting the values of "navigation_possible" for dredge polygon n
  !! data is copied
  FUNCTION get_ds_navigation_possible_d ( n ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : navigation_possible, get_nof_dredge_poly
    !! number of dredge polygon
    INTEGER , INTENT(IN) :: n !
    !! value of navigation_possible(n)
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ALLOCATED(navigation_possible) ) THEN
       IF ( n >= 1 .AND. n <= get_nof_dredge_poly() ) THEN
          res = navigation_possible( n )
       END IF
    END IF
    !
  END FUNCTION get_ds_navigation_possible_d
  !! getting the value of "limiting_discharge" for dredge polygon n
  !! data is copied
  FUNCTION get_ds_limiting_discharge_d ( n ) &
       RESULT( res )
    USE m_dredgesim_data, ONLY : limiting_discharge, get_nof_dredge_poly
    !! number of dredge polygon
    INTEGER , INTENT(IN) :: n !
    !! value of navigation_possible(n)
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ALLOCATED(limiting_discharge) ) THEN
       IF ( n >= 1 .AND. n <= get_nof_dredge_poly() ) THEN
          !LEO TODO the error
          !Error: Can't convert REAL(8) to LOGICAL(4) at (1)
          !from 
          !res = limiting_discharge( n )
          !is not fixed. This workaround must be checked!!!
          res = .FALSE.
          IF (n > 0) THEN
            res = .TRUE.
          END IF
          !end of workaround
       END IF
    END IF
    !
  END FUNCTION get_ds_limiting_discharge_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SIMULATIONS-methods <<< [ERR_NO = 20000 to 20999]
  ! ----------------------------------------------------------------------
  !
  !! starting of "dredgesim"-package                                      
  !! 1.) reading of steering data by use of dictionary-IO;    
  !! 2.) reading of grid information and grid data
  !! 3.) reading of data sets from files  
  !! 4.) reading of boundary data
  !! 5.) allocating and initializing further arrays and data
  SUBROUTINE start_dredgesim_d ( )
    !! name of subroutine
    use m_dredgesim_data, ONLY : debug_ds
    CHARACTER (LEN=17), PARAMETER  :: c_upname='start_dredgesim_d' ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE(*,*) ' >>> START dredgesim simulation'
       ! -----------------------------------------------------------------
       ! [1] 
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... reading steering data '
       END IF
       IF ( no_error( ) ) CALL read_dredgesim_steer       ( )
       IF (DEBUG_ds > 0) THEN
          print*,'steer',no_error()
       END IF
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... printing steering data'
       END IF
       IF ( no_error( ) ) CALL print_dredgesim_steer      ( )
       ! -----------------------------------------------------------------
       ! [2] 
       ! [3] 
       ! -----------------------------------------------------------------
!       WRITE(*,*) ' ... initial values: reading data from file (external data input) '
!       IF ( no_error( ) ) CALL ext_ds_ini_data_read ( )
!        print*,'ini_data_read',no_error()
       ! -----------------------------------------------------------------
       ! [4]
       ! [5]
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... allocating data at the end of start step '
       END IF
       IF ( no_error( ) ) CALL alloc_dredgesim_start  ( )
       IF (DEBUG_ds > 0) THEN
          print*,'alloc',no_error()
       END IF
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... calculating data at the end of start step '
       END IF
       IF ( no_error( ) ) CALL update_dredgesim_start ( )
       IF (DEBUG_ds > 0) THEN
          print*,'update',no_error()
       END IF
       !
    END IF
    !
  END SUBROUTINE start_dredgesim_d
  !
  !! preparing missing data for the dredgesim simulation          
  !! 1.) external grid import       
  !! 2.) external data import (initial state)   
  !! 3.) external data import (boundary values)
  !! 4.) allocating further data and updating existing data
  !! 5.) determine contents of required arrays before running thesimulation
  !! 6.) printing information about data structures on printer file
  !! 7.) checking data - not realized so far
  !! 8.) external data output - not realized so far
!LEO  SUBROUTINE prepare_dredgesim_d ( )
!LEO    !! name of subroutine
!LEO    use m_dredgesim_data, ONLY : debug_ds
!LEO    CHARACTER (LEN=19), PARAMETER  :: c_upname='prepare_dredgesim_d' ! 
!LEO    !
!LEO    IF ( ok_initialised( c_upname ) ) THEN
!LEO       WRITE(*,*) ' >>> PREPARE dredgesim simulation '
!LEO       ! -----------------------------------------------------------------
!LEO       ! [1]
!LEO       ! -----------------------------------------------------------------
!LEO       IF (DEBUG_ds > 0) THEN
!LEO          WRITE(*,*) ' ... initial values: importing grid (external grid import) '
!LEO       END IF
!LEO       IF ( no_error( ) ) CALL ext_ds_ini_grid_import ( )
!LEO       ! -----------------------------------------------------------------
!LEO       ! [2]
!LEO       ! -----------------------------------------------------------------
!LEO       IF (DEBUG_ds > 0) THEN
!LEO          WRITE(*,*) ' ... initial values: importing data (external data import) '
!LEO       END IF
!LEO       IF ( no_error( ) ) CALL ext_ds_ini_data_import ( )
!LEO       ! -----------------------------------------------------------------
!LEO       ! [3]
!LEO       ! [4]
!LEO       ! -----------------------------------------------------------------
!LEO       IF (DEBUG_ds > 0) THEN
!LEO          WRITE(*,*) ' ... allocating data at the end of prepare step '
!LEO       END IF
!LEO       IF ( no_error( ) ) CALL alloc_dredgesim_prepare  ( )
!LEO       IF (DEBUG_ds > 0) THEN
!LEO          WRITE(*,*) ' ... calculating data at the end of start step'
!LEO       END IF
!LEO       IF ( no_error( ) ) CALL update_dredgesim_prepare ( )
!LEO       ! -----------------------------------------------------------------
!LEO       ! [5]
!LEO       ! -----------------------------------------------------------------
!LEO       IF (DEBUG_ds > 0) THEN
!LEO          WRITE(*,*) ' ... determining index list node - dredge polygon '
!LEO       END IF
!LEO       IF ( no_error( ) ) CALL ext_ds_dredge_node_index ( )
!LEO       IF (DEBUG_ds > 0) THEN
!LEO          WRITE(*,*) ' ... transfering names of sediment fractions '
!LEO       END IF
!LEO       IF ( no_error( ) ) CALL ext_ds_fraction_name ( )
!LEO       ! -----------------------------------------------------------------
!LEO       ! [6]
!LEO       ! -----------------------------------------------------------------
!LEO       IF (DEBUG_ds > 0) THEN
!LEO          WRITE(*,*) ' ... printing array shapes '
!LEO       END IF
!LEO       IF ( no_error( ) ) CALL print_dredgesim_shape ( )
!LEO       IF (DEBUG_ds > 0) THEN
!LEO          WRITE(*,*) ' ... printing selected contents of arrays '
!LEO       END IF
!LEO       ! -----------------------------------------------------------------
!LEO       ! [7]
!LEO       ! -----------------------------------------------------------------
!LEO       !
!LEO       ! -----------------------------------------------------------------
!LEO       ! [8]
!LEO       ! -----------------------------------------------------------------
!LEO    END IF
!LEO    !
!LEO  END SUBROUTINE prepare_dredgesim_d
  !
  !! executing package
  !! execute a timestep :                    
  !! 1.) updating information about time                        
  !! 2.) updating boundary data                             
  !! 3.) executing computation
  !! 4.) updating data after coputation of timestep 
  !! 5.) output in files                       
  !! subroutine does not throw error messages
  SUBROUTINE run_dredgesim_d ( time, ZF, AVAIL )
    !
    USE m_dredgesim_data, ONLY : &
         act_timestep, old_time, act_time, cell_sediment_fraction, &
 node_sediment_fraction, get_nof_nodes
!    USE p_sisyphe_ui, ONLY : get_sm_node_depth, get_sm_node_sediment_frac
    !LEO use get_sm_node_depth
    !USE p_sisyphe_ui, ONLY : get_sm_node_depth
    use bief, ONLY : bief_obj
    !! current time
    TYPE (t_datetime) , INTENT(IN) :: time ! 
    !! name of subroutine
    CHARACTER (LEN=15), PARAMETER  :: c_upname='run_dredgesim_d' ! 
    ! variables
    CHARACTER (LEN=c_len_datetime_to_string) :: l_string ! 
    
!     DOUBLE PRECISION :: AVAIL(get_nof_nodes(), 1,size(node_sediment_fraction,2))
     DOUBLE PRECISION, POINTER :: AVAIL(:,:,:)

    TYPE (BIEF_OBJ) :: ZF
    !LEO TODO check if it is ok. I changed 
    !DOUBLE PRECISION :: ipoin, isicla
    !

    IF ( ok_initialised( c_upname ) ) THEN
       !
       WRITE(*,*) ' >>> RUN dredgesim timestep'       
       ! -----------------------------------------------------------------
       ! [1]
       ! -----------------------------------------------------------------
       old_time     = act_time
       act_time     = time
       act_timestep = su_datetime( act_time, old_time )
       l_string     = datetime_to_string( act_time )
       WRITE(*,*) ' +++ dredgesim-Time = ',TRIM(l_string)   
! update botom elevation:
!       CALL set_ds_node_depth ( get_sm_node_depth( ))
       !LEO TODO set_ds_node_depth here? It should be
       CALL set_ds_node_depth( ZF%R)

! update sediment distribution:
!LEO removed    DO ipoin=1,get_nof_nodes()
!LEO removed     DO isicla=1,size(node_sediment_fraction,2)
!LEO removed        avai_dr (ipoin,isicla) = &
!LEO removed        AVAIL(ipoin,1,isicla)
!LEO removed      END DO
!LEO removed     END DO
   
!       CALL set_ds_node_sediment_fraction ( update_sm_node_sediment_fraction( ))  
       !LEO removed CALL set_ds_node_sediment_fraction ( avai_dr)  
       ! -----------------------------------------------------------------
       ! [2]
       ! [3]
       ! -----------------------------------------------------------------
       WRITE(*,*) ' ... calculating new values in dredgesim_compute '
       IF ( no_error( ) ) CALL compute_dredgesim ( )
       ! -----------------------------------------------------------------
       ! [4]
       ! -----------------------------------------------------------------
       WRITE(*,*) ' ... updating of data at the end of run step '
       IF ( no_error( ) ) CALL update_dredgesim_run ( )
       ! -----------------------------------------------------------------
       ! [5]
       ! -----------------------------------------------------------------
       !
    END IF
    !
  END SUBROUTINE run_dredgesim_d
  ! 
  !! executing package
  !! execute a timestep :  
  SUBROUTINE  run_dredgesim_timestep_in_sec_d ( timestep_in_sec,tds_sis_node_depth,tds_hn, AVAIL, NPOIN, NSICLA, &
                                                tds_node_sediment_fraction)
    !
    USE m_dredgesim_data, ONLY : &
         act_timestep, old_time, act_time, debug_ds, &
     node_sediment_fraction, get_nof_nodes

    use bief, ONLY : bief_obj
    !
    INTEGER :: NSICLA, NPOIN
    !! current timestep in seconds
    INTEGER :: ipoin, isicla, NSICLA, NPOIN
    REAL (KIND=Double) , INTENT(IN) :: timestep_in_sec ! 
    REAL (KIND=Double) , POINTER :: var(:,:) 
    REAL (KIND=Double) , POINTER :: var2(:)     
    REAL (KIND=Double) , POINTER :: var3(:)         
    DOUBLE PRECISION, TARGET :: tds_node_sediment_fraction(NPOIN,NSICLA)
    DOUBLE PRECISION, TARGET :: tds_sis_node_depth(NPOIN)
    DOUBLE PRECISION, TARGET :: tds_hn(NPOIN)


    !! name of subroutine
    CHARACTER (LEN=31) , PARAMETER  :: c_upname='run_dredgesim_timestep_in_sec_d' ! 
    ! variables
    CHARACTER (LEN=c_len_datetime_to_string) :: l_string ! 
    !
    !Leo changed AVAIL 
     !LEO changed: DOUBLE PRECISION, POINTER :: avai_dr(:,:) to
     !LEO comes DOUBLE PRECISION, DIMENSION(:,:), POINTER :: avai_dr
     !LEO added TARGET for avial to be sure that ther is no optimization,...
     DOUBLE PRECISION, TARGET ::  AVAIL(NPOIN,1,NSICLA)

    !LEO removed TYPE (BIEF_OBJ) :: ZF
    INTEGER :: istat
    !
!LEO removed   ALLOCATE (avai_dr(NPOIN,NSICLA),STAT=istat)
    !LEO TODO give the pointers var,var2,var3 names ;-)
    var => tds_node_sediment_fraction
    var2 => tds_sis_node_depth
    var3 => tds_hn

    IF ( ok_initialised( c_upname ) ) THEN
       !
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' >>> RUN dredgesim timestep (TELEMAC) '       
       END IF
       ! -----------------------------------------------------------------
       ! [1]
       ! -----------------------------------------------------------------
       old_time     = act_time
       act_timestep = real_seconds_to_time( timestep_in_sec ) 
       act_time     = ad_time_to_datetime( act_time, act_timestep )
       l_string     = datetime_to_string( act_time )
          
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' +++ dredgesim-Time = ',TRIM(l_string)
       END IF

! update botom elevation:
!       CALL set_ds_node_depth ( get_sm_node_depth( ))
       !LEO removed this CALL set_ds_node_depth ( ZF%R)
       !LEO included CALL set_ds_node_depth ( ZF%R) again
       CALL set_ds_node_depth (var2)

!LEO removed      DO ipoin=1,NPOIN !get_nof_nodes()
!LEO removed         DO isicla=1,NSICLA !size(node_sediment_fraction,2)
!LEO removed            avai_dr(ipoin,isicla) = AVAIL(ipoin,1,isicla)
!LEO removed         END DO
!LEO removed      END DO

!      CALL set_ds_node_sediment_fraction ( update_sm_node_sediment_fraction( ))  
       !LEO replaced 
       !CALL set_ds_node_sediment_fraction ( avai_dr)  
       !LEO by
       CALL set_ds_node_sediment_fraction(var)  
       !LEO set node_water_depth
       CALL set_ds_node_water_depth(var3)

       !LEO TODO  this looks like a bug, avia_dr is deallocated after a pointer
       !was set to it --> work with the variables from p_sisyphe_ui.f90
!LEO removed       DEALLOCATE(avai_dr,STAT=istat)

       ! -----------------------------------------------------------------
       ! [2]
       ! [3]
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... calculating new values in dredgesim_compute '
       END IF
       IF ( no_error( ) ) CALL compute_dredgesim ( )
       ! -----------------------------------------------------------------
       ! [4]
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... updating of data at the end of run step '
       END IF
       IF ( no_error( ) ) CALL update_dredgesim_run ( )
       ! -----------------------------------------------------------------
       ! [5]
       ! -----------------------------------------------------------------
       !
    END IF
    !
  END SUBROUTINE run_dredgesim_timestep_in_sec_d
  !
  !! finalizing the package 
  !! 1.) closing open files
  !! 2.) deallocating data, output of amount of dredged material if not done so far
  !! subroutine does not throw error messages
  SUBROUTINE stop_dredgesim_d ( )
    USE l_criterion, ONLY : get_criterion_crit_type
    USE m_dredgesim_data, ONLY : free_surface_node_depth, free_surface_poly_depth, &
                                 all_total_volume, disp_total_volume, dredge_criterion, &
                                 last_act_time_rs, act_time
    USE m_dredgesim_output, ONLY :  write_final_dredged_volume, write_final_disposed_volume, &
                                    write_restart_data
    !Parallel routines
    use bief, ONLY : NCSIZE, ipid
    !! name of subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='stop_dredgesim_d' ! 
    !! variables
    !
    IF ( ok_initialised( c_upname ) ) THEN
       WRITE(*,*) ' >>> STOP dredgesim simulation '
       !
       ! -----------------------------------------------------------------
       ! [1]
       ! [2]
       ! -----------------------------------------------------------------
       WRITE(*,*) ' ... output of overall amount of dredged material '
       !
       IF (NCSIZE.GT.1) THEN
          IF (ipid==0) THEN
              IF (ALLOCATED(dredge_criterion)) THEN
                 CALL write_final_dredged_volume ()
                 CALL write_final_disposed_volume ()
              END IF
!             IF (ANY(all_total_volume(:) /= 0.0_Double))CALL write_final_dredged_volume ()
!             IF (ANY(disp_total_volume(:,:) /= 0.0_Double))CALL write_final_disposed_volume ()
              last_act_time_rs = datetime_to_string(act_time)
              CALL write_restart_data ( )
          END IF
       ELSE
          IF (ALLOCATED(dredge_criterion)) THEN
             CALL write_final_dredged_volume ()
             CALL write_final_disposed_volume ()
          END IF
!          IF (ANY(all_total_volume(:) /= 0.0_Double))CALL write_final_dredged_volume ()
!          IF (ANY(disp_total_volume(:,:) /= 0.0_Double))CALL write_final_disposed_volume ()
           last_act_time_rs = datetime_to_string(act_time)
           CALL write_restart_data ( )
       END IF
       !
       IF (ALLOCATED(free_surface_node_depth)) DEALLOCATE (free_surface_node_depth)
       IF (ALLOCATED(free_surface_poly_depth)) DEALLOCATE (free_surface_poly_depth)
       !
       WRITE(*,*) ' ... deallocating dredgesim data '
       IF ( no_error( ) ) CALL dealloc_dredgesim_data ( )
       !
    END IF
    !
  END SUBROUTINE stop_dredgesim_d
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
  ! >>> GENERAL-methods <<< [ERR_NO =  0001 to  0999]
  ! ----------------------------------------------------------------------
  !
  !! setting error condition 1 = module not initialized 
  !! function generates error messages
  FUNCTION ok_initialised ( upname ) &
       RESULT( ok )
    !! name of subroutine
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! result
    LOGICAL :: ok ! 
    !! number of error
    INTEGER            :: ierr    ! 
    !! error text
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** warning *** module "p_dredgesim_ui" is not initialized correctly'
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'error category: GENERAL'
       cerr(2) = 'module is not initialized'
       cerr(3) = '--> execute INIT_dredgesim'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! setting error condition 2 = module already initialized 
  !! function generates error messages
  FUNCTION not_initialised ( upname ) &
       RESULT( ok )
    !! name of subroutine
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! result
    LOGICAL :: ok ! 
    !
    ok = .NOT. initialised
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-methods <<< [ERR_NO =  1000 to  1999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-methods <<< [ERR_NO =  2000 to  2999]
  ! ----------------------------------------------------------------------
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
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-methods <<< [ERR_NO =  7000 to  7999]
  ! ----------------------------------------------------------------------
  !
END MODULE p_dredgesim_ui
! TailOfPackageUserInterface -----------------------------------------------
