! -------------------------------------------------------------------------
! HeadOfPackageUpdateModule -----------------------------------------------
!
!! update methods for the "dredgesim"-package
!!
!
MODULE m_dredgesim_update
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
  ! [A.1.3] base module "files"
  USE b_file, ONLY : &
       ! data type
       t_file, &
       ! methods
       open_file, &
       set_file_unit, &
       get_file_unit, &
       get_file_path_and_name
  ! [A.1.4] base module "dimensions"
  USE b_dim, ONLY : &
       ! data type
       t_dim
  ! [A.1.5] base module "variables"
  USE b_var, ONLY : &
       ! data type
       t_var
  ! [A.1.6] base module "attributes"
  USE b_att, ONLY : &
       ! data type
       t_att
  ! [A.1.7] base module "information concerning files"
  USE b_io_info, ONLY : &
       ! data type
       t_io_info, &
       ! constant
       c_max_len_pac, c_max_len_key, &
       ! methods
       get_io_info_pac_count, get_io_info_pac_idx, get_io_info_file, get_io_info_id, &
       get_io_info_idx, get_io_info_key, get_io_info_dim_ref, get_io_info_var_ref,   &
       get_io_info_att_ref, get_io_info_pac,                                         &
       set_io_info_id, set_io_info_dim_ref, set_io_info_var_ref, set_io_info_att_ref
  USE b_datetime
  ! [A.2.1] (local) base module type+methods "dredge polygon data" -------
  USE l_criterion, ONLY :     &
       ! data type
       t_criterion, &
       ! routines
       get_criterion_crit_type
  !
  ! ----------------------------------------------------------------------
  ! [B] data of the "dredgesim"-package
  ! ----------------------------------------------------------------------
  !
  ! [B.1] data module of the "dredgesim"-package 
  USE m_dredgesim_data, ONLY : &
       ! variables and constant values
       prn_op, trc_op, prn_lun, trc_lun, all_errors ! >ToDo> ggf. weitere Daten ergaenzen
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] public declarations
  ! ---------------------------------------------------------------------
  !
  !! updating / calculating data at the end of start-phase
  INTERFACE update_dredgesim_start
     MODULE PROCEDURE update_dredgesim_start_d
  END INTERFACE
  !! updating / calculating data at the end of prepare-phase
  INTERFACE update_dredgesim_prepare
     MODULE PROCEDURE update_dredgesim_prepare_d
  END INTERFACE
  !! updating / calculating data at the end of run-phase
  INTERFACE update_dredgesim_run
     MODULE PROCEDURE update_dredgesim_run_d
  END INTERFACE
  !
  ! list of public methods
  !
  PUBLIC :: update_dredgesim_start
  PUBLIC :: update_dredgesim_prepare
  PUBLIC :: update_dredgesim_run
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data types, internal data and methods
  ! ---------------------------------------------------------------------
  !
  !! name of the module
  CHARACTER (LEN=18), PARAMETER :: c_modname  = 'm_dredgesim_update' ! 
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
  !! updating / calculating data at the end of start-phase of the "dredgesim"-package
  !! subroutine does not throw error messages
  SUBROUTINE update_dredgesim_start_d ( )
    !! name of subroutine
    CHARACTER (LEN=24) , PARAMETER :: c_upname='update_dredgesim_start_d' ! 
    !
    ! not used by now...
    !
  END SUBROUTINE update_dredgesim_start_d
  !
  !! updating / calculating data at the end of prepare-phase of the "dredgesim"-package
  !! subroutine does not throw error messages
  SUBROUTINE update_dredgesim_prepare_d ( )
    USE b_time, ONLY : time_to_real_seconds
    USE m_dredgesim_data, ONLY : &
        leopr_ds, ptinig_ds, act_timestep, &
        poly_total_volume, poly_sediment_volume, poly_water_volume, poly_fraction_volume, &
        total_volume, sediment_volume, water_volume, fraction_volume, delta_dredge_poly_depth, &
        delta_disp_poly_depth, disp_poly_depth, &
        poly_fraction_volume_tc, fraction_volume_tc, &
        observing_period, time_to_observe, initial_time, initial_time_to_observe, &
        node_fraction_volume_tc, &
        get_nof_dredge_poly, &
        dredge_criterion, &
        node_sediment_volume_tc, sediment_volume_tc, &
        delta_dredge_node_depth, &
        node_total_volume, &
        node_sediment_volume, &
        node_water_volume, &
        node_fraction_volume, &
        delta_disp_node_depth, disp_node_depth, &
        delta_disp_node_depth_tc, &
        delta_node_depth_abl, &
        dredged_sediment_volume_to_disp, &
        dredged_fraction_volume_to_disp, &
        all_total_volume, all_sediment_volume, all_water_volume, all_fraction_volume, &
        all_total_volume_old, all_sediment_volume_old, all_water_volume_old, all_fraction_volume_old, &
        disp_scours_auto, disp_scours_tc, disp_scours_abl, &
        free_surface_node_depth, &
        disp_node_total_volume, &
        restart, node_sediment_volume_rs, node_fraction_volume_rs, &
        disp_total_volume, disp_sediment_volume, disp_fraction_volume, &
        disp_total_volume_old, disp_sediment_volume_old, disp_fraction_volume_old, &
        list_of_disp_polys, nof_dispose_poly, dispose_poly_name, &
        output_time_restart, ini_time_rs,&
        check_allocated_disp_scours !LEO added check_allocated_disp_scours
    !! name of subroutine
    CHARACTER (LEN=26) , PARAMETER :: c_upname='update_dredgesim_prepare_d' ! 
    !
    !! reading further input data 
    INTEGER                        :: n, k, i, crit_type
    !

    DO n=1,get_nof_dredge_poly( )
       crit_type = get_criterion_crit_type( dredge_criterion(n) )
       IF (crit_type == 3) THEN
       	  CALL interpolate_ref_surface_level ( )
       	  EXIT
       END IF
    END DO
    !
    IF (.NOT. ALLOCATED(free_surface_node_depth)) THEN
      !LEO TODO BUG fixed  ANY(disp_scours_auto(:) must be allocated before
      !here it is a pointer? so check if it is correct
      IF (check_allocated_disp_scours() .GT. 0) THEN
         IF (ANY(disp_scours_auto(:) == 'YES')) THEN
           CALL interpolate_ref_surface_level ( )
         ELSE IF (ANY(disp_scours_tc(:) == 'YES')) THEN
           CALL interpolate_ref_surface_level ( )
         ELSE IF (ANY(disp_scours_abl(:) == 'YES')) THEN
           CALL interpolate_ref_surface_level ( )
         END IF
      END IF
    END IF

    !
    ! initializing arrays for volume calculations
    IF (ALLOCATED(poly_total_volume)              ) poly_total_volume               = 0.0_Double
    IF (ALLOCATED(node_total_volume)              ) node_total_volume               = 0.0_Double
    IF (ALLOCATED(poly_sediment_volume)           ) poly_sediment_volume            = 0.0_Double
    IF (ALLOCATED(node_sediment_volume)           ) node_sediment_volume            = 0.0_Double
    IF (ALLOCATED(poly_water_volume)              ) poly_water_volume               = 0.0_Double
    IF (ALLOCATED(node_water_volume)              ) node_water_volume               = 0.0_Double
    IF (ALLOCATED(poly_fraction_volume)           ) poly_fraction_volume            = 0.0_Double
    IF (ALLOCATED(node_fraction_volume)           ) node_fraction_volume            = 0.0_Double
    IF (ALLOCATED(total_volume)                   ) total_volume                    = 0.0_Double
    IF (ALLOCATED(sediment_volume)                ) sediment_volume                 = 0.0_Double
    IF (ALLOCATED(water_volume)                   ) water_volume                    = 0.0_Double
    IF (ALLOCATED(fraction_volume)                ) fraction_volume                 = 0.0_Double
    IF (ALLOCATED(delta_dredge_poly_depth)        ) delta_dredge_poly_depth         = 0.0_Double
    IF (ALLOCATED(delta_dredge_node_depth)        ) delta_dredge_node_depth         = 0.0_Double
    IF (ALLOCATED(delta_disp_node_depth)          ) delta_disp_node_depth           = 0.0_Double
    IF (ALLOCATED(delta_disp_poly_depth)          ) delta_disp_poly_depth           = 0.0_Double
    IF (ALLOCATED(disp_poly_depth)                ) disp_poly_depth                 = 0.0_Double
    IF (ALLOCATED(disp_node_depth)                ) disp_node_depth                 = 0.0_Double
    IF (ALLOCATED(poly_fraction_volume_tc)        ) poly_fraction_volume_tc         = 0.0_Double
    IF (ALLOCATED(node_fraction_volume_tc)        ) node_fraction_volume_tc         = 0.0_Double
    IF (ALLOCATED(fraction_volume_tc)             ) fraction_volume_tc              = 0.0_Double
    IF (ALLOCATED(node_sediment_volume_tc)        ) node_sediment_volume_tc         = 0.0_Double
    IF (ALLOCATED(sediment_volume_tc)             ) sediment_volume_tc              = 0.0_Double
    IF (ALLOCATED(all_total_volume)               ) all_total_volume                = 0.0_Double
    IF (ALLOCATED(all_sediment_volume)            ) all_sediment_volume             = 0.0_Double
    IF (ALLOCATED(all_water_volume)               ) all_water_volume                = 0.0_Double
    IF (ALLOCATED(all_fraction_volume)            ) all_fraction_volume             = 0.0_Double
    IF (ALLOCATED(all_total_volume_old)           ) all_total_volume_old            = 0.0_Double
    IF (ALLOCATED(all_sediment_volume_old)        ) all_sediment_volume_old         = 0.0_Double
    IF (ALLOCATED(all_water_volume_old)           ) all_water_volume_old            = 0.0_Double
    IF (ALLOCATED(all_fraction_volume_old)        ) all_fraction_volume_old         = 0.0_Double
    IF (ALLOCATED(disp_total_volume)              ) disp_total_volume               = 0.0_Double
    IF (ALLOCATED(disp_sediment_volume)           ) disp_sediment_volume            = 0.0_Double
    IF (ALLOCATED(disp_fraction_volume)           ) disp_fraction_volume            = 0.0_Double
    IF (ALLOCATED(disp_total_volume_old)          ) disp_total_volume_old           = 0.0_Double
    IF (ALLOCATED(disp_sediment_volume_old)       ) disp_sediment_volume_old        = 0.0_Double
    IF (ALLOCATED(disp_fraction_volume_old)       ) disp_fraction_volume_old        = 0.0_Double
    IF (ALLOCATED(delta_disp_node_depth_tc)       ) delta_disp_node_depth_tc        = 0.0_Double
    IF (ALLOCATED(delta_node_depth_abl)           ) delta_node_depth_abl            = 0.0_Double
    IF (ALLOCATED(dredged_sediment_volume_to_disp)) dredged_sediment_volume_to_disp = 0.0_Double
    IF (ALLOCATED(dredged_fraction_volume_to_disp)) dredged_fraction_volume_to_disp = 0.0_Double
    IF (ALLOCATED(disp_node_total_volume         )) disp_node_total_volume          = 0.0_Double
    IF (ALLOCATED(node_sediment_volume_rs        )) node_sediment_volume_rs         = 0.0_Double
    IF (ALLOCATED(node_fraction_volume_rs        )) node_fraction_volume_rs         = 0.0_Double
    !
    IF (ALLOCATED(initial_time_to_observe)) THEN
    	DO n=1,get_nof_dredge_poly( )
    	   IF (initial_time_to_observe(n) < initial_time) THEN
    	      time_to_observe(n) = initial_time
    	      initial_time_to_observe(n) = initial_time
    	   ELSE
    	      time_to_observe(n) = initial_time_to_observe(n)
    	   END IF
    	END DO
    END IF    
    !
    
    ! Re-Setting the array list_of_disp_polys for output of disp sediments
    DO n=1,get_nof_dredge_poly( )
       DO k=1,nof_dispose_poly
          DO i=1,SIZE(list_of_disp_polys)
             IF (dispose_poly_name(k,n)=='not_defined') THEN
                EXIT
             ELSE IF (dispose_poly_name(k,n)==list_of_disp_polys(i)) THEN
             	EXIT
             ELSE IF (list_of_disp_polys(i)=='no_definition') THEN
             	list_of_disp_polys(i)=dispose_poly_name(k,n)
             	EXIT
             END IF
          END DO
       END DO
    END DO
    !
    !! save initial time in case of performing a restart after this simulation
    ini_time_rs = datetime_to_string(initial_time)
    !
    !! read data from the restart file 
    IF (restart) CALL read_restart_data ( )
    IF (PTINIG_ds > 0) THEN
    	output_time_restart=PTINIG_ds*time_to_real_seconds(act_timestep)*1.0_Double
    END IF
  END SUBROUTINE update_dredgesim_prepare_d
  !
  !! updating / calculating data at the end of run-phase of the "dredgesim"-package
  !! subroutine does not throw error messages
  SUBROUTINE update_dredgesim_run_d ( )
   !! name of subroutine
    CHARACTER (LEN=22) , PARAMETER :: c_upname='update_dredgesim_run_d' ! 
    !
    ! fill with meaningful code...
    !
  END SUBROUTINE update_dredgesim_run_d
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
  ! reading reference surface level
  SUBROUTINE interpolate_ref_surface_level ( )
   !
   USE m_dredgesim_data, ONLY : &
       input_files, &
       free_surface_node_depth, &
       free_surface_poly_depth, &
       get_nof_poly, &
       get_nof_nodes, &
       node_coord, &
       fkm, &
       km, &
       debug_ds
   USE m_dredgesim_interpolations, ONLY : &
       interpolate_node_to_poly
    !
    ! declarations
    TYPE (t_file) :: l_file
    ! parameters for reading a data file
    INTEGER :: unit, nof_section, status, i, ierror
    CHARACTER(LEN=100) :: filename
    REAL(KIND=Double), ALLOCATABLE :: xle(:), yle(:), zle(:), xri(:), yri(:), zri(:)
    ! parameters for interpolation methods
    INTEGER :: ns, ne, nv, np
    REAL(KIND=Double), ALLOCATABLE :: xm(:),ym(:) ! nv co-ordinates of the interpolation mesh vertices
    REAL(KIND=Double), ALLOCATABLE :: zm(:) ! Nodal values
    REAL(KIND=Double), ALLOCATABLE :: zs(:,:) ! Free surface values after hopefully successful interpolation
    INTEGER, ALLOCATABLE :: ikle(:,:) ! Connectivity table for ne elements for interpolation
    INTEGER, ALLOCATABLE :: elem(:) ! List of np elements containing interpolation points
    REAL(KIND=Double), ALLOCATABLE :: shp(:,:) ! np interpolation functions  
    !
    INTEGER :: is, ie, ip, n1, n2, n3, n4
    REAL(KIND=Double) :: a1, a2, a3, a4
    INTEGER :: i1, i2, i3, i4
    REAL(KIND=DOUBLE) :: t12,t13,t14,t22,t23,t24,delta,det,surdet 
    REAL(KIND=DOUBLE) :: xprim,yprim,xisopa,yisopa,ff,ffprim
    REAL(KIND=Double) :: epsdet=1.0e-20_Double, epsff=1.0e-10_Double 
    !
    ! getting the input file
    l_file = get_io_info_file(input_files(2))  
    ! setting file unit and name
    CALL set_file_unit( l_file, 0 )
    !
    unit = get_file_unit( l_file )
    filename = get_file_path_and_name(l_file)
    status=0
    ! open input file 
    OPEN (UNIT=unit, FILE=filename)  
    i=0

    ! reading input file for getting allocation argument
    readloop: DO
              READ(unit, *, IOSTAT=status)
              IF (status/=0) EXIT
	          i=i+1
              nof_section=i
              END DO readloop 
              CLOSE (unit)
    ! allocating calculation arrays
    ALLOCATE(xle(nof_section))
    ALLOCATE(yle(nof_section))
    ALLOCATE(zle(nof_section))
    ALLOCATE(xri(nof_section))
    ALLOCATE(yri(nof_section))
    ALLOCATE(zri(nof_section))
    ALLOCATE(km(nof_section))
    xle=0.0_Double
    yle=0.0_Double
    zle=0.0_Double
    xri=0.0_Double
    yri=0.0_Double
    zri=0.0_Double
    ! open input file again
    OPEN (UNIT=unit, FILE=filename)
    status=0
    ! reading input file for getting required data
    DO i=1,nof_section
       READ(unit,*,IOSTAT=status) xri(i), yri(i), xle(i), yle(i), zle(i), km(i)
       IF (status/=0) EXIT
    END DO
    CLOSE (unit)
    !
    zri=zle
    !WRITE(*,*) "LEDODEBUG free surface reference", zri
    !
    ! interpolation routine - triangular quadrilateral interpolation
    ! thanx to JaJa ...
    !
    ! taken from "INTER_POL"
    ns=nof_section
    nv=2*ns
    ! triangualar interpolation
    !ne=2*ns-2
    ! quadrilateral interpolation    
    ne=ns-1
    np=get_nof_nodes( )
    !
    ! allocating variables 
    ALLOCATE (xm(1:nv),ym(1:nv))
    ALLOCATE (zm(0:nv)) ! note range 0:nv 
    ALLOCATE (ikle(ne,4))
    ALLOCATE (elem(np))
    ALLOCATE (shp(np,4))
    !
    ALLOCATE(zs(get_nof_nodes( ),2))
    IF (.NOT. ALLOCATED(free_surface_node_depth)) ALLOCATE(free_surface_node_depth(get_nof_nodes( )))
    IF (.NOT. ALLOCATED(free_surface_poly_depth)) ALLOCATE(free_surface_poly_depth(get_nof_poly( ) ))
    IF (.NOT. ALLOCATED(fkm                    )) ALLOCATE(fkm(get_nof_nodes( )                    ))
    !
    ! initializing variables
    xm=0.0_Double
    ym=0.0_Double
    zm=0.0_Double
    ikle=0
    elem=0
    shp=0.0_Double
    !
    zs=0.0_Double
    free_surface_node_depth=0.0_Double
    !
    ! taken from "M_SURFINI_TRI"/"M_SURFINI_QUAD"
    DO is = 1,ns
        i = (is-1)*2 + 1
        xm(i)   = xle(is)
        xm(i+1) = xri(is)
        ym(i)   = yle(is)
        ym(i+1) = yri(is)
        zm(i)   = zle(is)
        zm(i+1) = zri(is)
    END DO
    !
    !
    ! triangular interpolation
    !DO ie=1,ne-1,2
    !   ikle(ie,1)   = ie
    !   ikle(ie,2)   = ie+1
    !   ikle(ie,3)   = ie+2
    !   ikle(ie+1,1) = ie+1
    !   ikle(ie+1,2) = ie+3
    !   ikle(ie+1,3) = ie+2
    !END DO
    !
    ! quadrilateral interpolation
    DO ie=1,ne
       ikle(ie,1) = 2*ie-1
       ikle(ie,2) = 2*ie
       ikle(ie,3) = 2*ie+2
       ikle(ie,4) = 2*ie+1 
    END DO
    !
    ! taken from "LOCATE"
    DO ip=1,np
       elem(ip) = 0
       DO ie=1,ne
          n1 = ikle(ie,1)
          n2 = ikle(ie,2)
          n3 = ikle(ie,3)
          n4 = ikle(ie,4)
          IF ( n4==0 ) THEN 
             a1 = (node_coord(ip,1)-xm(n3))*(ym(n2)-ym(n3))&
                - (node_coord(ip,2)-ym(n3))*(xm(n2)-xm(n3))
             a2 = (node_coord(ip,1)-xm(n1))*(ym(n3)-ym(n1))&
                - (node_coord(ip,2)-ym(n1))*(xm(n3)-xm(n1))
             a3 = (node_coord(ip,1)-xm(n2))*(ym(n1)-ym(n2))&
                - (node_coord(ip,2)-ym(n2))*(xm(n1)-xm(n2))
             IF ((a1.ge.0.).and.(a2.ge.0.).and.(a3.ge.0.)) THEN
		        elem(ip) = ie
			    EXIT
		     END IF
          ELSE
		     a1 = (node_coord(ip,1)-xm(n2))*(ym(n1)-ym(n2))&
                - (node_coord(ip,2)-ym(n2))*(xm(n1)-xm(n2))
             a2 = (node_coord(ip,1)-xm(n3))*(ym(n2)-ym(n3))&
                - (node_coord(ip,2)-ym(n3))*(xm(n2)-xm(n3))
             a3 = (node_coord(ip,1)-xm(n4))*(ym(n3)-ym(n4))&
                - (node_coord(ip,2)-ym(n4))*(xm(n3)-xm(n4))
             a4 = (node_coord(ip,1)-xm(n1))*(ym(n4)-ym(n1))&
                - (node_coord(ip,2)-ym(n1))*(xm(n4)-xm(n1))
             IF ((a1.ge.0.).and.(a2.ge.0.).and. (a3.ge.0.).and.(a4.ge.0)) THEN
                elem(ip) = ie
                EXIT
		     END IF 
          END IF
       END DO     
    END DO
    !
    ! taken from "QUAD_INT"
    DO ip=1,np
       IF (elem(ip) == 0) THEN 
           zs(ip,:) = 0.0_Double
       ELSE
          i1 = ikle(elem(ip),1)
          i2 = ikle(elem(ip),2)
          i3 = ikle(elem(ip),3)
          i4 = ikle(elem(ip),4)
          !
          t12=-xm(i1)+xm(i2)
          t13=-xm(i1)+xm(i4)
          t14= xm(i1)-xm(i2)+xm(i3)-xm(i4)
          t22=-ym(i1)+ym(i2)
          t23=-ym(i1)+ym(i4)
          t24= ym(i1)-ym(i2)+ym(i3)-ym(i4)
	       !
	       det = t12*t23-t22*t13

	      !
	      IF (det<epsdet) PRINT*, 'QUADINT: element ',elem(ip), ' determinant < ',epsdet
	         !
	         surdet = 1.0_Double/det
            !
	         ff     = ( t14*t23-t13*t24 ) * surdet
            ffprim = ( t12*t24-t14*t22 ) * surdet
            !
            xprim = ( t23*(node_coord(ip,1)-xm(i1))-t13*(node_coord(ip,2)-ym(i1)) )*surdet
            yprim = (-t22*(node_coord(ip,1)-xm(i1))+t12*(node_coord(ip,2)-ym(i1)) )*surdet
	         !
	         IF (ABS(ff*ffprim).lt.epsff) THEN
               xisopa=xprim/(1.0_Double + ff     * yprim)
               yisopa=yprim/(1.0_Double + ffprim * xprim)
            ELSE
               delta = SQRT((1.0_Double+ff*yprim-ffprim*xprim)**2+4.0_Double*ffprim*xprim)
               xisopa=(-(1.0_Double-ffprim*xprim+ff*yprim)+delta)/(ffprim+ffprim)
               yisopa=(-(1.0_Double+ffprim*xprim-ff*yprim)+delta)/(ff+ff)
            END IF
	         !

	        shp(elem(ip),1) = ( 1.0_Double-xisopa ) * ( 1.0_Double-yisopa )
           shp(elem(ip),2) =              xisopa   * ( 1.0_Double-yisopa )
           shp(elem(ip),3) =              xisopa   *              yisopa
           shp(elem(ip),4) = ( 1.0_Double-xisopa ) *              yisopa 
	        !
	        ! taken from "INTERPOLATE"
	        zs(ip,1) = SUM(shp(elem(ip),:)*zm(ikle(elem(ip),:)))
	        zs(ip,2) = km(elem(ip))
            !LEODEBUG Simple warning if ZS is equal to zero -> this means that the interpolation fails
            !since an element lies outside the bounding box
            IF (debug_ds > 0) THEN
                IF (zs(ip,1) <= 0.0001)  THEN   
                    WRITE(*,*) "WARNING! Interpolated free_surface_node_depth is near zero" ,zs(ip,1)
                END IF
            END IF
       END IF
    END DO
    WRITE(*,*) "LEODEBUG did interpolation"
    free_surface_node_depth = zs(:,1)
    free_surface_poly_depth = interpolate_node_to_poly(free_surface_node_depth)
    fkm                     = zs(:,2)
    !
    DEALLOCATE(xle)
    DEALLOCATE(yle)
    DEALLOCATE(zle)
    DEALLOCATE(xri)
    DEALLOCATE(yri)
    DEALLOCATE(zri)
    DEALLOCATE (xm,ym)
    DEALLOCATE (zm) ! note range 0:nv 
    DEALLOCATE (ikle)
    DEALLOCATE (elem)
    DEALLOCATE (shp)
    !
  END SUBROUTINE interpolate_ref_surface_level
  !
  SUBROUTINE read_restart_data ( )
    !
    USE m_dredgesim_data, ONLY : knolg
    !
    USE bief, ONLY : NCSIZE
    !
    USE b_datetime
    !
    USE m_dredgesim_data, ONLY : &
        input_files, &
        delta_dredge_node_depth, &
        aim_node_depth, &
        node_total_volume, &
        get_nof_nodes, &
        get_nof_dredge_poly, &
        dredge_criterion, &
        nof_dispose_poly, &
        dispose_poly_name, &
        disp_node_depth, &
        delta_disp_node_depth, &
        node_sediment_volume_rs, &
        node_fraction_volume_rs, &
        get_nof_sediment_fraction, &
        fraction_name, &
        dredged_sediment_volume_to_disp, &
        dredged_fraction_volume_to_disp, &
        nof_dredge_poly_tc, &
        dispose_poly_name_tc, &
        sediment_volume_tc, &
        fraction_volume_tc, &
        initial_time, &
        old_time_to_observe, &
        time_to_observe, &
        act_time, &
        node_total_volume
    USE l_criterion, ONLY : &
        get_criterion_poly_name
    ! declarations
    TYPE (t_file) :: l_file
    ! parameters for reading a data file
    INTEGER :: unit, status, ierror
    CHARACTER(LEN=100) :: filename, identifier, name_1, name_2, name_3, name_4
    INTEGER :: node_no, i, k, n
    REAL(KIND=Double) :: double_1, double_2, double_3
    ! getting the input file
    l_file = get_io_info_file(input_files(3))  
    ! setting file unit and name
    CALL set_file_unit( l_file, 0 )
    !
    unit = get_file_unit( l_file )
    filename = get_file_path_and_name(l_file)
    status=0
    ! open input file 
    OPEN (UNIT=unit, FILE=filename)
    ! reading input file for getting allocation argument
    readloop: DO
              READ(unit, *, IOSTAT=status) identifier, name_1, node_no, double_1, double_2, name_2, name_3, name_4
              IF (status/=0) EXIT
              IF (TRIM(identifier)=='criterion_times') THEN
                 initial_time=string_to_datetime(name_4)
                 act_time=string_to_datetime(name_1)
                 DO n=1,get_nof_dredge_poly( )
                    IF (node_no==n) THEN
                       old_time_to_observe(n)=string_to_datetime(name_2)
                       time_to_observe(n)=string_to_datetime(name_3)
                    END IF
                 END DO
              END IF
              IF (TRIM(identifier)=='criterion_dredge') THEN
                 DO n=1,get_nof_dredge_poly( )
                    IF (TRIM(name_1)==TRIM(get_criterion_poly_name(dredge_criterion(n)))) THEN
                       DO i=1,get_nof_nodes( )
                          IF(NCSIZE .GT. 1) THEN
                            IF(node_no==knolg(i)) THEN
                               delta_dredge_node_depth(i,n)=double_1   
                       	       aim_node_depth(i,n)=double_2
                       	     END IF
                          ELSE
                             IF(node_no==i) THEN
                               delta_dredge_node_depth(i,n)=double_1   
                       	       aim_node_depth(i,n)=double_2
                             END IF
                          END IF
                       END DO
                    END IF
                 END DO
              END IF
              IF (TRIM(identifier)=='dredged_fractions') THEN
                 DO n=1,get_nof_dredge_poly( )
                    IF (TRIM(name_1)==TRIM(get_criterion_poly_name(dredge_criterion(n)))) THEN
                       DO i=1,get_nof_nodes( )
                          IF(NCSIZE .GT. 1) THEN
                            IF(node_no==knolg(i)) THEN
                               node_sediment_volume_rs(i,n)=double_1   
                       	       DO k=1,get_nof_sediment_fraction( )
                       	          IF(fraction_name(k)==name_2) node_fraction_volume_rs(i,k,n)=double_2
                       	       END DO
                             END IF
                          ELSE
                             IF(node_no==i) THEN
                               node_sediment_volume_rs(i,n)=double_1   
                       	       DO k=1,get_nof_sediment_fraction( )
                       	          IF(fraction_name(k)==name_2) node_fraction_volume_rs(i,k,n)=double_2
                       	       END DO
                             END IF
                          END IF
                       END DO
                    END IF
                 END DO
              END IF
              IF (TRIM(identifier)=='dredged_volumes') THEN
                 DO n=1,get_nof_dredge_poly( )
                    IF (TRIM(name_1)==TRIM(get_criterion_poly_name(dredge_criterion(n)))) THEN
                       DO i=1,get_nof_nodes( )
                          IF(NCSIZE .GT. 1) THEN
                            IF(node_no==knolg(i)) THEN
                               node_total_volume(i,n)=double_1   
                             END IF
                          ELSE
                             IF(node_no==i) THEN
                               node_total_volume(i,n)=double_1   
                             END IF
                          END IF
                       END DO
                    END IF
                 END DO
              END IF
              IF (TRIM(identifier)=='criterion_disposal') THEN
                 DO n=1,get_nof_dredge_poly( )
                    IF (TRIM(name_2)==TRIM(get_criterion_poly_name(dredge_criterion(n)))) THEN
                       DO k=1,nof_dispose_poly
                          IF (TRIM(name_1)==TRIM(dispose_poly_name(k,n))) THEN
                             DO i=1,get_nof_nodes( )
                                IF(NCSIZE .GT. 1) THEN
                                   IF(node_no==knolg(i)) THEN
                                     delta_disp_node_depth(i,n,k)=double_1   
                       	             disp_node_depth(i,n,k)=double_2
                                   END IF
                                ELSE
                                   IF(node_no==i) THEN
                                     delta_disp_node_depth(i,n,k)=double_1   
                       	             disp_node_depth(i,n,k)=double_2
                                   END IF
                                END IF
                             END DO
                          END IF
                       END DO   
                    END IF
                 END DO
              END IF
              IF (TRIM(identifier)=='fractions_to_disp') THEN
                 DO n=1,get_nof_dredge_poly( )
                    IF (TRIM(name_2)==TRIM(get_criterion_poly_name(dredge_criterion(n)))) THEN
                       DO k=1,nof_dispose_poly
                          IF (TRIM(name_1)==TRIM(dispose_poly_name(k,n))) THEN
                             dredged_sediment_volume_to_disp(n,k)=double_1   
                             DO i=1,get_nof_sediment_fraction( )
                       	        IF(i==node_no) dredged_fraction_volume_to_disp(n,k,i)=double_2
                       	     END DO
                          END IF
                       END DO
                    END IF
                 END DO
              END IF
              IF (TRIM(identifier)=='time_steered_disp') THEN
                 DO n=1,nof_dredge_poly_tc
                    IF (TRIM(name_1)==TRIM(dispose_poly_name_tc(n))) THEN
                       sediment_volume_tc(n)=double_1   
                       DO i=1,get_nof_sediment_fraction( )
                          IF(i==node_no) fraction_volume_tc(i,n)=double_2
                       END DO
                    END IF
                 END DO
              END IF
              END DO readloop 
    CLOSE (unit)
  END SUBROUTINE read_restart_data
END MODULE m_dredgesim_update
! TailOfPackageModule ----------------------------------------------------
