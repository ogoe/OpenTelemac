!
! User interface for sisyphe
!! transfering data from sisyphe to DredgeSim
!! data for intialization procedure of DredgeSim
!!
!
MODULE p_sisyphe_ui
  !
  ! modules from h_grid, needed for geometry information
  !
  USE m_h_grid_data, ONLY : &
    ! data type
    t_h_grid
  !
  ! base module for sediment description
  !
  USE b_grain, ONLY : &
    ! data type
    t_grain
!RK
  use bief, only : bief_obj, bief_mesh
  !
  !
  IMPLICIT NONE
  PRIVATE
  !
  !! initializing DredgeSim
  INTERFACE init_and_setup_ds
     MODULE PROCEDURE init_and_setup_ds_d
  END INTERFACE
  !! clearing local data in p_sisyphe_ui
  INTERFACE clear_sisydredge
     MODULE PROCEDURE clear_sisydredge_d
  END INTERFACE
  !
  ! GET-methods
  !
  !! getting initial time from Sisyphe 
  INTERFACE get_sm_initial_time
     MODULE PROCEDURE get_sm_initial_time_d
  END INTERFACE
  !! getting the number of sediment fractions
  INTERFACE get_sm_nof_sediment_fractions
     MODULE PROCEDURE get_sm_nof_sediment_fractions_d
  END INTERFACE
  !! getting the names of sediment classes / fractions
  INTERFACE get_sm_sed_class_name
     MODULE PROCEDURE get_sm_sed_class_name_d
  END INTERFACE
  !! getting used sediment classes as a self defined data type
  !! pointer on internal data
  INTERFACE get_sm_used_sediment_classes
     MODULE PROCEDURE get_sm_used_sediment_classes_d
  END INTERFACE
  !! getting the number of nodes per poly
  !! a pointer is set
  INTERFACE get_sm_nodes_of_poly
     MODULE PROCEDURE get_sm_nodes_of_poly_ref_d
  END INTERFACE
  !! getting coordinates of gravitational centers of elements
  !! a pointer is set
  INTERFACE get_sm_grav_center_coord
     MODULE PROCEDURE get_sm_grav_center_coord_ref_d
  END INTERFACE
  !! getting coordinates of nodes
  !! a pointer is set
  INTERFACE get_sm_node_coord
     MODULE PROCEDURE get_sm_node_coord_ref_d
  END INTERFACE
  !! getting element areas 
  !! a pointer is set 
  INTERFACE get_sm_poly_area
     MODULE PROCEDURE get_sm_poly_area_ref_d
  END INTERFACE
  !! getting node depths
  !! a pointer is set
  INTERFACE get_sm_node_depth
     MODULE PROCEDURE get_sm_node_depth_ref_d
  END INTERFACE
  !! getting element depths
  !! a pointer is set
  !! dummy values, but field is needed for allocating data in DredgeSim
  INTERFACE get_sm_poly_depth
     MODULE PROCEDURE get_sm_poly_depth_ref_d
  END INTERFACE
  !! getting non erodible node depths
  !! a pointer is set
  INTERFACE get_sm_node_noero_depth
     MODULE PROCEDURE get_sm_node_noero_depth_ref_d
  END INTERFACE 
  !! getting the water node depths
  !! a pointer is set
  INTERFACE get_sm_node_water_depth
     MODULE PROCEDURE get_sm_node_water_depth_ref_d
  END INTERFACE
  !! getting node porosity
  !! either data is copied
  !! or a pointer is set
  INTERFACE get_sm_node_porosity
     MODULE PROCEDURE get_sm_node_porosity_d
     MODULE PROCEDURE get_sm_node_porosity_ref_d
  END INTERFACE  
  !! getting sediment fractions on nodes
  !! either data is copied
  !! or a pointer is set
  INTERFACE get_sm_node_sediment_fraction
     MODULE PROCEDURE get_sm_node_sediment_frac_d
     MODULE PROCEDURE get_sm_node_sediment_frac_ref_d
  END INTERFACE  
  !! getting sediment fractions on elements
  !! a pointer is set
  !! dummy values, but pointer is needed for allocating data in DredgeSim
  INTERFACE get_sm_cell_sediment_fraction
     MODULE PROCEDURE get_sm_cell_sediment_frac_ref_d
  END INTERFACE  
  !! getting edgelist of elements
  !! a pointer is set  
  INTERFACE get_sm_edgelist_of_poly
     MODULE PROCEDURE get_sm_edgelist_of_poly_ref_d
  END INTERFACE           
  !! getting nodelist of elements  
  !! a pointer is set
  INTERFACE get_sm_nodelist_of_poly
     MODULE PROCEDURE get_sm_nodelist_of_poly_ref_d
  END INTERFACE
!RK neue
  !! getting the debug on/off
!LEO remove test  INTERFACE get_sm_debug
!LEO remove test    MODULE PROCEDURE get_sm_debug_i
!LEO remove test END INTERFACE
  !! getting the node area
  INTERFACE get_sm_node_area
     MODULE PROCEDURE get_sm_node_area_d
  END INTERFACE
  !! getting the node list local / global
  INTERFACE get_sm_knolg
     MODULE PROCEDURE get_sm_knolg_i
  END INTERFACE
  !! getting the node neigbours
  INTERFACE get_sm_node_neighb
     MODULE PROCEDURE get_sm_node_neighb_i
  END INTERFACE
  !! getting the number of shared points
  INTERFACE get_sm_nb_neighb_pt
     MODULE PROCEDURE get_sm_nb_neighb_pt_i
  END INTERFACE
  !! getting the list send
  INTERFACE get_sm_list_send
     MODULE PROCEDURE get_sm_list_send_i
  END INTERFACE
  !! getting the global number of shared points
  INTERFACE get_sm_nh_com
     MODULE PROCEDURE get_sm_nh_com_i
  END INTERFACE
  !! getting the buffer send
  INTERFACE get_sm_BUF_SEND
     MODULE PROCEDURE get_sm_buf_send_d
  END INTERFACE
  !! getting the buffer received
  INTERFACE get_sm_BUF_RECV
     MODULE PROCEDURE get_sm_buf_recv_d
  END INTERFACE

  !! external routine to provide information & data from file(s)
INTERFACE ext_ds_ini_data_read
   MODULE PROCEDURE ext_ds_ini_data_read_d
END INTERFACE
!! external routine to link information & grid from data of other packages
INTERFACE ext_ds_ini_grid_import
   MODULE PROCEDURE ext_ds_ini_grid_import_d
END INTERFACE
!! external routine to link information & data from data of other packages
INTERFACE   ext_ds_ini_data_import 
   MODULE PROCEDURE ext_ds_ini_data_import_d
END INTERFACE
!
!! external routine to determine contents of "dredge_poly_index(:)"
INTERFACE ext_ds_dredge_poly_index
    MODULE PROCEDURE ext_ds_dredge_poly_index_d
END INTERFACE

!! external routine to determine contents of "dredge_poly_index(:)"
INTERFACE 
   SUBROUTINE ext_ds_dredge_node_index ()
   END SUBROUTINE ext_ds_dredge_node_index
END INTERFACE
!
!! external routine to determine contents of "fraction_name(:)"
INTERFACE ext_ds_fraction_name
   MODULE PROCEDURE ext_ds_fraction_name_d
END INTERFACE
!
INTERFACE prepare_dredgesim
    MODULE PROCEDURE prepare_dredgesim_d
END INTERFACE

!


!LEO add c_modname for the error procedure
   CHARACTER (LEN=12), PARAMETER :: c_modname      = 'p_sisyphe_ui' !
  !
  PUBLIC :: get_sm_initial_time             ! initial time and date
  PUBLIC :: get_sm_nof_sediment_fractions   ! getting number of sediment fractions
  PUBLIC :: get_sm_sed_class_name           ! getting name of defined sediment classes
  PUBLIC :: get_sm_used_sediment_classes    ! getting used sediment classes as self defined data type
  PUBLIC :: get_sm_nodes_of_poly            ! getting nodes of elements
  PUBLIC :: get_sm_grav_center_coord        ! getting gravitational center coordinates of elements
  PUBLIC :: get_sm_node_coord               ! getting node coordinates
  PUBLIC :: get_sm_poly_area                ! getting element areas
  PUBLIC :: get_sm_node_depth               ! getting node depths
  PUBLIC :: get_sm_poly_depth               ! getting element depths
  PUBLIC :: get_sm_node_noero_depth         ! getting non erodible node depths
  PUBLIC :: get_sm_node_water_depth         ! getting node water depths
  PUBLIC :: get_sm_node_porosity            ! porosity on cells
  PUBLIC :: get_sm_node_sediment_fraction   ! grain fraction on nodes
  PUBLIC :: get_sm_cell_sediment_fraction   ! grain fraction on elements
  PUBLIC :: get_sm_edgelist_of_poly         ! edgelist of elements            
  PUBLIC :: get_sm_nodelist_of_poly         ! nodelist of elements
  PUBLIC :: init_and_setup_ds               ! initialization and setup routines
  PUBLIC :: clear_sisydredge                ! clearing data
!RK new ones
!LEO test raus  PUBLIC :: get_sm_debug                    ! getting debug on/off
  PUBLIC :: get_sm_node_area                ! node area 
  PUBLIC :: get_sm_knolg                    ! node list local global
  PUBLIC :: get_sm_node_neighb              ! node neighbours
  PUBLIC :: get_sm_nb_neighb_pt             ! number of shared points
  PUBLIC :: get_sm_list_send                ! list of processor numbers
  PUBLIC :: get_sm_nh_com                   ! global number of shared points
  PUBLIC :: get_sm_buf_recv                 ! buffer for receiving data
  PUBLIC :: get_sm_buf_send                 ! buffer for sending data
  !
  PUBLIC :: ext_ds_ini_data_read   ! 
  PUBLIC :: ext_ds_ini_grid_import ! 
  PUBLIC :: ext_ds_ini_data_import ! 
  PUBLIC :: ext_ds_dredge_poly_index !
  PUBLIC :: ext_ds_dredge_node_index ! 
  PUBLIC :: ext_ds_fraction_name   !
  PUBLIC :: prepare_dredgesim

  ! definition of local data for transfer
  INTEGER, POINTER, SAVE :: edgelist_of_poly(:,:), nodelist_of_poly(:,:)
  INTEGER, TARGET, SAVE  :: nof_edges
  DOUBLE PRECISION, TARGET, ALLOCATABLE, SAVE :: ZF_poly(:)
  DOUBLE PRECISION, TARGET, ALLOCATABLE, SAVE :: poro(:)
  DOUBLE PRECISION, TARGET, ALLOCATABLE, SAVE :: avai_dr_node(:,:)
  DOUBLE PRECISION, TARGET, ALLOCATABLE, SAVE :: avai_dr_poly(:,:)
  DOUBLE PRECISION, TARGET, ALLOCATABLE, SAVE :: mesh_buf_send(:,:)
  DOUBLE PRECISION, TARGET, ALLOCATABLE, SAVE :: mesh_buf_recv(:,:)
  DOUBLE PRECISION, POINTER, SAVE :: center_coord(:,:), node_coord(:,:), poly_area(:)
  INTEGER, TARGET, ALLOCATABLE, SAVE :: nofp(:)
  INTEGER, TARGET, ALLOCATABLE, SAVE :: mesh_nh_com(:,:)
  TYPE (t_grain), ALLOCATABLE, TARGET, SAVE :: used_sediment(:)
  DOUBLE PRECISION, TARGET, ALLOCATABLE, SAVE :: node_depth_dummy(:)

  INTEGER, TARGET, SAVE :: NPOIN, NELEM, NSICLA
  INTEGER, TARGET, SAVE :: PTINIG, LEOPR, LT
  INTEGER, TARGET, SAVE :: MARDAT(3), MARTIM(3)
  DOUBLE PRECISION, TARGET, SAVE :: DT, XKV, XMVS
  DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE, SAVE::AVAIL
  DOUBLE PRECISION, DIMENSION(:), TARGET, ALLOCATABLE, SAVE :: FDM
  LOGICAL, SAVE :: MSK
 
  TYPE(BIEF_MESH), SAVE :: MESH
  TYPE (BIEF_OBJ), SAVE :: ZF, ZR, E, HN,   NLAYER
  DOUBLE PRECISION,DIMENSION(:), TARGET, ALLOCATABLE, SAVE :: ZF_NODE_DEPTH !LEO new ZF_NODE_DEPTH
  DOUBLE PRECISION,DIMENSION(:), TARGET, ALLOCATABLE, SAVE :: NODE_AREA !LEO new ZF_NODE_DEPTH


  ! 
 CONTAINS 
 !
 ! routines for data transfer
 !
 !! getting the initial time and date 
 !! data is copied
 !! function does not throw error messages
 FUNCTION get_sm_initial_time_d &
       ( ) &
       RESULT( var )
    ! 
    USE b_time, ONLY : real_seconds_to_time
    USE b_datetime
    !
    TYPE (t_datetime) :: var 
    TYPE (t_datetime) :: startdate
    DOUBLE PRECISION :: AT
    CHARACTER*4 :: chardum
    CHARACTER*19 :: chardate
    AT = LT*DT
    !
    chardate=''
    ! day
    write(chardum,'(I2)') MARDAT(3)
    READ(chardum,'(A2)') chardate(1:2)
    chardate(3:3)='.'
    ! month
    write(chardum,'(I2)') MARDAT(2)
    READ(chardum,'(A2)') chardate(4:5)
    chardate(6:6)='.'
    ! year
    write(chardum,'(I4)') MARDAT(1)
    READ(chardum,'(A4)') chardate(7:10)
    ! hour
    write(chardum,'(I2)') MARTIM(1)
    READ(chardum,'(A2)') chardate(12:13)
    chardate(14:14)='.'
    ! minute
    write(chardum,'(I2)') MARTIM(2)
    READ(chardum,'(A2)') chardate(15:16)
    chardate(17:17)='.'
    ! sec
    write(chardum,'(I2)') MARTIM(3)
    READ(chardum,'(A2)') chardate(18:19)
    !
    startdate = string_to_datetime(chardate)
    !
    var = ad_time_to_datetime( startdate,  real_seconds_to_time( AT ) )
    !
  END FUNCTION get_sm_initial_time_d
  !  
  !! getting number of defined sediment fractions from Sisyphe
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_nof_sediment_fractions_d &
       ( ) &
       RESULT( var )
    ! 
    ! result value
    INTEGER :: var
    !
    var = NSICLA
    !
  END FUNCTION get_sm_nof_sediment_fractions_d  
  !  
  !
  !! getting the name of used sediment classes from Sisyphe
  !! data is copied
  !! function does not throw error messages
  !LEO removed NSICLA as argument since it is here known
  FUNCTION get_sm_sed_class_name_d & 
       ( ) &
       RESULT( var )
    ! 
    !! number of used sediment classes in Sisyphe
    INTEGER ::  isicla, i, j !LEO NSICLA
    CHARACTER (LEN=128) :: chardum
    ! result values
    ! hint: dummy values as no names in Sisyphe but needed for DredgeSim
    CHARACTER (LEN=40) :: varname
    CHARACTER (LEN=20), dimension(:), allocatable :: var
    !

    ALLOCATE (var(NSICLA))

    var = ''
    var = ''

    varname=''
    varname(1:8) = 'Sediment'
   
    DO isicla = 1,NSICLA
      WRITE(chardum,'(I4)') int(iSICLA)  ! 
      READ(chardum,'(A4)') varname(9:12)
      j=8
      DO i=1,3
        j =j+1 
       if(varname(j:j).EQ.' ') then
          varname(j:j) = '_'
        endif
      END DO
      var(isicla) = ''
      var(isicla) = TRIM(varname)
    END DO

!LEO new code (somehow cleaner)
! 
!    DO isicla = 1,NSICLA
!       WRITE(varname, "(A8,I4)") "Sediment", isicla
!       j=8
!       DO i=1,3
!          j =j+1 
!          if(varname(j:j).EQ.' ') then
!             varname(j:j) = '_'
!          endif
!       END DO
!       var(isicla) = ''
!       var(isicla) = TRIM(varname)
!    END DO 
!LEO stop new code
  
  !
  END FUNCTION get_sm_sed_class_name_d  
  !
  !! getting used sediment classes from Sisyphe and transfer to self defined data type
  !! pointer on internal data
  !! function does not throw error messages
  FUNCTION get_sm_used_sediment_classes_d &
       ( ) &
       RESULT( var )
    ! 
    USE b_grain,  ONLY : t_grain, set_grain_name, &
        set_grain_mid, set_grain_size, set_grain_density,&
        print_grain
    ! result values
    !! hint: dummy values as no names in Sisyphe but needed for DredgeSim
    TYPE (t_grain) , POINTER :: var(:) ! 
    INTEGER :: i, j, k
    CHARACTER (LEN=40) :: gname
    CHARACTER*128 :: chardum
    CHARACTER (LEN=40), dimension(:), allocatable :: grain_name

    ALLOCATE (grain_name(NSICLA))
    !
    grain_name = ''
    gname=''
    gname(1:8) = 'Sediment'
    !
    ! building up von t_grain

    DO i=1,NSICLA
       WRITE(*,*) "LEODEBUG build grain_name" 
       WRITE(chardum,'(I4)') int(i)  !
       READ(chardum,'(A4)') gname(9:12)
       grain_name(i) = TRIM(gname)
       j=8
       DO k=1,3
          j =j+1
          if(grain_name(i)(j:j).EQ.' ') then
            grain_name(i)(j:j) = '_'
          endif
      END DO
      call set_grain_name(used_sediment(i),  grain_name(i))
      call set_grain_mid( used_sediment(i), i )
      call set_grain_size(used_sediment(i), FDM(i) )
      call set_grain_density(used_sediment(i), XMVS) 
    END DO
    !
    ! pointer...
    var => used_sediment
    !
  END FUNCTION get_sm_used_sediment_classes_d
  !
  !! getting the number of nodes per poly
  !! pointer on data
  !! function does not throw error messages  
  FUNCTION get_sm_nodes_of_poly_ref_d (  ) &
       RESULT( var )
    ! 
    !
    INTEGER , POINTER :: var(:)
    INTEGER :: i
    !
    DO i=1,NELEM
       nofp(i) = 3
    END DO
    ! 
    var => nofp
    !
  END FUNCTION get_sm_nodes_of_poly_ref_d
  !  
  !! getting gravitational center coordinates of elements
  !! pointer on data
  !! function does not throw error messages
  FUNCTION get_sm_grav_center_coord_ref_d ( ) &
       RESULT( var )
    ! 
    DOUBLE PRECISION, POINTER :: var(:,:)
    !
    var => center_coord
    !
  END FUNCTION get_sm_grav_center_coord_ref_d
  !  
  !! getting node coordinates
  !! pointer on data
  !! function does not throw error messages
  FUNCTION get_sm_node_coord_ref_d ( ) &
       RESULT( var )
    !
    DOUBLE PRECISION, POINTER :: var(:,:)
    !
    var => node_coord
    !
  END FUNCTION get_sm_node_coord_ref_d
  !  
  !! getting element areas
  !! pointer on data
  !! function does not throw error messages
  FUNCTION get_sm_poly_area_ref_d (  ) &
       RESULT( var )
    ! 
    !
    DOUBLE PRECISION, POINTER :: var(:)
    !
    var => poly_area
    !
  END FUNCTION get_sm_poly_area_ref_d
  !
  !! getting the node depths
  !! pointer on data
  !! function does not throw error messages
  FUNCTION get_sm_node_depth_ref_d ( ) &
       RESULT( var )
    ! 
    DOUBLE PRECISION, POINTER :: var(:)
    !
    !LEO old
    !var => ZF%R
    !LEO NEW
    var => ZF_NODE_DEPTH
    !
  END FUNCTION get_sm_node_depth_ref_d  
  !
  !! getting element depths
  !! pointer on data
  !! dummy values, but dimension of pointer is needed for allocation of other data
  !! function does not throw error messages
  FUNCTION get_sm_poly_depth_ref_d (  ) &
       RESULT( var )
    ! 
    DOUBLE PRECISION, POINTER :: var(:)
    !
    var => ZF_poly
    !
  END FUNCTION get_sm_poly_depth_ref_d
  !
  !! getting the non erodible node depths
  !! pointer on data
  !! function does not throw error messages
  FUNCTION get_sm_node_noero_depth_ref_d ( ) &
       RESULT( var )
    ! 
    DOUBLE PRECISION, POINTER :: var(:)
    !
    var => ZR%R
    !
  END FUNCTION get_sm_node_noero_depth_ref_d    
  !
  !! getting water depth on nodes
  !! pointer on data
  !! function does not throw error messages
  FUNCTION get_sm_node_water_depth_ref_d (  ) &
       RESULT( var )
    ! 
    !
    DOUBLE PRECISION, POINTER :: var(:)
    !
    var => HN%R
    !
  END FUNCTION get_sm_node_water_depth_ref_d
  !
  !! getting node porosity from upper layer of elements
  !! data is copied
  !! function does not throw error messages
  !! porosity is constant in Sisyphe
  FUNCTION get_sm_node_porosity_d ( nof_poin ) &
       RESULT( var )
    INTEGER , INTENT(IN) :: nof_poin ! 
    DOUBLE PRECISION :: var(nof_poin)
    var = XKV
    !
  END FUNCTION get_sm_node_porosity_d
  !
  !! getting node porosity from upper layer of elements
  !! pointer on data (dummy as porosity is constant)
  !! function does not throw error messages
  !! porosity is constant in Sisyphe
  FUNCTION get_sm_node_porosity_ref_d ( ) &
       RESULT( var )
    !
    DOUBLE PRECISION, POINTER :: var(:)
    !
    poro = XKV
    var => poro
    !
  END FUNCTION get_sm_node_porosity_ref_d
  !  
  !! get sediment fraction from upper layer of nodes
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_node_sediment_frac_d ( nof_nodes, nof_sediment_fractions ) &
       RESULT( var )
    !! number of elements
    INTEGER , INTENT(IN) :: nof_nodes
    !! number of sediment fractions
    INTEGER , INTENT(IN) :: nof_sediment_fractions ! 
    INTEGER :: ipoin, isicla
    DOUBLE PRECISION :: var(nof_nodes, nof_sediment_fractions)
    DOUBLE PRECISION :: avai_dr(nof_nodes, nof_sediment_fractions)
    !
    DO ipoin=1,nof_nodes
     DO isicla=1,nof_sediment_fractions
!      DO ilayer=1,2   !NLAYER%I(ipoin)
!        avai_dr(ipoin+(ilayer-1)*npoin, isicla) = &
        avai_dr (ipoin,isicla) = &
!RK bis v5p9        AVAIL(1,isicla,ipoin)
!ab v6p0
        AVAIL(ipoin,1,isicla)
!      END DO
     END DO
    END DO
    !
    var = avai_dr
    !
  END FUNCTION get_sm_node_sediment_frac_d  
  !  
  !! get sediment fraction from upper layer of nodes
  !! pointer is set
  !! function does not throw error messages
  FUNCTION get_sm_node_sediment_frac_ref_d ( ) &
       RESULT( var )
    !
    DOUBLE PRECISION, POINTER :: var(:,:)
    DOUBLE PRECISION  ::   avai_dr(npoin,nsicla)
    INTEGER :: ipoin, isicla
    !
    DO ipoin=1,NPOIN
     DO isicla=1,nsicla
        avai_dr(ipoin,isicla) = &
        AVAIL(ipoin,1,isicla)
     END DO
    END DO
    avai_dr_node = avai_dr
    var => avai_dr_node
    !
  END FUNCTION get_sm_node_sediment_frac_ref_d
  !
  !! getting sediment fraction from upper layer of cells
  !! pointer on data
  !! dummy values, but dimension of pointer is needed for allocation of other data
  !! function does not throw error messages
  FUNCTION get_sm_cell_sediment_frac_ref_d (  ) &
       RESULT( var )
    ! 
    DOUBLE PRECISION, POINTER :: var(:,:)
    !
    var => avai_dr_poly
    !
  END FUNCTION get_sm_cell_sediment_frac_ref_d
  !
  !! getting edgelist of elements
  !! pointer on data
  !! function does not throw error messages
  FUNCTION get_sm_edgelist_of_poly_ref_d (  ) &
       RESULT( var )
    ! 
    INTEGER , POINTER :: var(:,:) ! 
    !
    var => edgelist_of_poly
    !
  END FUNCTION get_sm_edgelist_of_poly_ref_d
  !
  !! getting nodelist of elements
  !! a pointer is set
  !! function does not throw error messages
  FUNCTION get_sm_nodelist_of_poly_ref_d (  ) &
       RESULT( var )
    !
    INTEGER , POINTER :: var(:,:) !
    !
    var => nodelist_of_poly
    ! 
  END FUNCTION get_sm_nodelist_of_poly_ref_d
  !
!RK new functions
  !! getting the switch for debug information
  !! data is copied
  !! function does not throw error messages
!LEO test raus  FUNCTION get_sm_debug_i &
!LEO test raus       ( ) &
!LEO test raus       RESULT( var )
!LEO test raus    ! 
!LEO test raus    ! result value
!LEO test raus    INTEGER :: var
!LEO test raus    !
!LEO test raus    var = DEBUG
!LEO test raus    !
!LEO test raus  END FUNCTION get_sm_debug_i
  !! getting the node area from the mesh
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_node_area_d &
       ( ) &
       RESULT( var )
    ! 
    ! result value
    DOUBLE PRECISION, POINTER  :: var(:)
    !
    !LEO old bief style var => NODE_AREA%R
    var => NODE_AREA
    !
    !
  END FUNCTION get_sm_node_area_d
  !! getting the node list local global from the mesh
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_knolg_i &
       ( ) &
       RESULT( var )
    !
    ! result value
    INTEGER, POINTER  :: var(:)
    !
    var => MESH%KNOLG%I
    !
  END FUNCTION get_sm_knolg_i
  !! getting the node neighbor from the mesh
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_node_neighb_i &
       ( ) &
       RESULT( var )
    !
    ! result value
    INTEGER, POINTER  :: var(:)
    !
    var => MESH%NACHB%I
    !
  END FUNCTION get_sm_node_neighb_i

! folgende mesh variablen braucht das parallele:
! MESH%NB_NEIGHB_PT%I: NUMBER OF POINTS SHARED WITH A SUB-DOMAIN 
! (vector von nb_neighb)
! MESH%LIST_SEND%I: 
! MESH%NH_COM%I:NH_COM(I,IL) : GLOBAL NUMBER IN THIS SUB-DOMAIN 
! OF THE POINT NUMBER I IN THE LIST OF POINTS SHARED WITH PROCESSOR 
! NUMBER IL WHOSE REAL NUMBER IS LIST_SEND(IL)
! MESH%BUF_SEND%R: BUFFER FOR SENDING DATA ..double precision
! MESH%BUF_RECV%R: BUFFER FOR RECEIVING DATA .. double prec
! MESH%BUF_SEND%DIM1: FIRST DIMENSION OF BUFFERS
  !! getting the node list local global from the mesh
  !! data is copied
  !! function does not throw error messages
  !! getting the number of points shared with a sub-domain
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_nb_neighb_pt_i &
       ( ) &
       RESULT( var )
    !
    ! result value
    INTEGER, POINTER  :: var(:)
    !
    var => MESH%NB_NEIGHB_PT%I
    !
  END FUNCTION get_sm_nb_neighb_pt_i
  !! getting the LIST OF PROCESSORS NUMBERS
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_list_send_i &
       ( ) &
       RESULT( var )
    !
    ! result value
    INTEGER, POINTER  :: var(:)
    !
    var => MESH%LIST_SEND%I
    !
  END FUNCTION get_sm_list_send_i

  !! global numbers for shared points
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_nh_com_i &
       ( ) &
       RESULT( var )
   !
    use m_dredgesim_data, only : dimbuf, nb_neighb
    ! result value
   INTEGER, POINTER  :: var(:,:)
   INTEGER :: i,j,k
  !
    k=0
    do i=1,dimbuf
     do j=1,nb_neighb
      k=k+1
      mesh_nh_com(i,j) = MESH%NH_COM%I(k)
     end do
    end do
    var => mesh_nh_com
! geht leider nicht
!   var => MESH%NH_COM%I%P
    !
  END FUNCTION get_sm_nh_com_i
  !! getting buffer for received data
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_buf_recv_d &
       ( ) &
       RESULT( var )
    !
    use m_dredgesim_data, only : dimbuf, nb_neighb
    ! result value
    DOUBLE PRECISION, POINTER  :: var(:,:)
   INTEGER :: i,j,k
  !
    k=0
    do i=1,dimbuf
     do j=1,nb_neighb
      k=k+1
      mesh_buf_recv(i,j) = MESH%BUF_RECV%R(k)
     end do
    end do
    var => mesh_buf_recv

!    var => MESH%BUF_RECV%R
    !
  END FUNCTION get_sm_buf_recv_d
  !! getting the buffer for sending data
  !! data is copied
  !! function does not throw error messages
  FUNCTION get_sm_buf_send_d &
       ( ) &
       RESULT( var )
     use m_dredgesim_data, only : dimbuf, nb_neighb
    ! result value
    DOUBLE PRECISION, POINTER  :: var(:,:)
   INTEGER :: i,j,k
  !
    k=0
    do i=1,dimbuf
     do j=1,nb_neighb
      k=k+1
      mesh_buf_send(i,j) = MESH%BUF_SEND%R(k)
     end do
    end do
!    var => mesh_buf_send   !
    nullify(var)

    !
  END FUNCTION get_sm_buf_send_d

!LEO added prepare_dredgesim
    !! preparing missing data for the dredgesim simulation          
  !! 1.) external grid import       
  !! 2.) external data import (initial state)   
  !! 3.) external data import (boundary values)
  !! 4.) allocating further data and updating existing data
  !! 5.) determine contents of required arrays before running thesimulation
  !! 6.) printing information about data structures on printer file
  !! 7.) checking data - not realized so far
  !! 8.) external data output - not realized so far
  SUBROUTINE prepare_dredgesim_d ( )
    !! name of subroutine
    use m_dredgesim_data, ONLY : debug_ds,& 
      !LEO add
      print_dredgesim_shape, alloc_dredgesim_prepare
    
    !LEO added update_dredgesim_prepare
    use m_dredgesim_update, ONLY:   update_dredgesim_prepare
    !
    use b_error, ONLY : no_error, setup_error_act
    CHARACTER (LEN=19), PARAMETER  :: c_upname_prepare ='prepare_dredgesim_d' ! 
    !
    IF ( ok_initialised( c_upname_prepare ) ) THEN
       WRITE(*,*) ' >>> PREPARE dredgesim simulation '
       ! -----------------------------------------------------------------
       ! [1]
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... initial values: importing grid (external grid import) '
       END IF
       IF ( no_error( ) ) CALL ext_ds_ini_grid_import ( )
       ! -----------------------------------------------------------------
       ! [2]
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... initial values: importing data (external data import) '
       END IF
       IF ( no_error( ) ) CALL ext_ds_ini_data_import ( )
       ! -----------------------------------------------------------------
       ! [3]
       ! [4]
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... allocating data at the end of prepare step '
       END IF
       IF ( no_error( ) ) CALL alloc_dredgesim_prepare  ( )
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... calculating data at the end of start step'
       END IF
       IF ( no_error( ) ) CALL update_dredgesim_prepare ( )
       ! -----------------------------------------------------------------
       ! [5]
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... determining index list node - dredge polygon '
       END IF
       IF ( no_error( ) ) CALL ext_ds_dredge_node_index ( )
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... transfering names of sediment fractions '
       END IF
       IF ( no_error( ) ) CALL ext_ds_fraction_name ( )
       ! -----------------------------------------------------------------
       ! [6]
       ! -----------------------------------------------------------------
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... printing array shapes '
       END IF
       IF ( no_error( ) ) CALL print_dredgesim_shape ( )
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' ... printing selected contents of arrays '
       END IF
       ! -----------------------------------------------------------------
       ! [7]
       ! -----------------------------------------------------------------
       !
       ! -----------------------------------------------------------------
       ! [8]
       ! -----------------------------------------------------------------
    END IF
    !
  END SUBROUTINE prepare_dredgesim_d

  ! -------------------------------------------------------------------
  !LEO add the ext_ subroutines 
  ! -------------------------------------------------------------------
  
  !==============================================
  !LEO be careful! Variables and code is doubled
  !
  !
  !
  !==============================================
!TODO ext_ds_ini_data_import_d
    SUBROUTINE ext_ds_ini_data_import_d ( )
       !
       ! [A.1.1] base module with global constant values
       USE b_constants, ONLY : &
          ! parameters / constant values
          Double
       ! [A.1.2] base module "error-handling"
       USE b_error, ONLY :   &
          ! routines
          no_error,        &
          any_error !LE0 not used  ,debug
       !
       !USE p_sisyphe_ui, ONLY : get_sm_DEBUG
       ! [B] methods from IO-packages
       !
      ! [B.1] methods from DredgeSim to set data
       USE p_dredgesim_ui, ONLY : &
          setup_ds_initial_time, setup_ds_debug
      ! [B.2] methods from other packages to get required data from
      !LEO USE p_sisyphe_ui, ONLY : &
      !LEO get_sm_initial_time, get_sm_debug
      USE m_dredgesim_data, ONLY : debug_ds
      !
      ! -------------------------------------------------------------------
      IMPLICIT NONE
      ! -------------------------------------------------------------------
      !
      ! local parameters and variables
      !! name of module (leave name 'externe Routine')
      !LEO raus CHARACTER (LEN=15), PARAMETER :: c_modname_a='externe Routine'  ! 
      !! name of external subroutine
      CHARACTER (LEN=22), PARAMETER :: c_upname_a='ext_ds_ini_data_import' !  
      !
      IF ( no_error( ) ) THEN
         !
         IF (DEBUG_ds > 0) THEN
            WRITE(*,*) ' name = "'//TRIM( c_upname_a )//'"'
         END IF
         CALL setup_ds_initial_time(get_sm_initial_time())
      END IF
      !
   END SUBROUTINE ext_ds_ini_data_import_d
!TODO end ext_ds_ini_data_import_d
   !
   !
   !
!TODO start ext_ds_ini_data_read_d ( )
!use _lc to get local variables
   SUBROUTINE ext_ds_ini_data_read_d ( )
      !
      ! [A.1.1] base module "constants"
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
      ! [A.1.3] base module "file-handling"
      USE b_file, ONLY : &
         ! data type
         t_file, &
         ! methods
         get_file_name
      ! [A.1.4] base module "dimensions"
      USE b_dim, ONLY : &
         ! data type
         t_dim
      ! [A.1.4] base module "variables"
      USE b_var, ONLY : &
         ! data type
         t_var
      ! [A.1.5] base module "attributes"
      USE b_att, ONLY : &
         ! data type
         t_att
      ! [A.1.6] base module with type+methods "information concerning files"
      USE b_io_info, ONLY : &
         ! data type
         t_io_info, &
         ! constants
         c_max_len_pac, c_max_len_key, &
         ! methods
         get_io_info_pac_count, get_io_info_pac_idx, get_io_info_file, get_io_info_id, &
         get_io_info_idx, get_io_info_key, get_io_info_dim_ref, get_io_info_var_ref,   &
         get_io_info_att_ref, get_io_info_pac,                                         &
         set_io_info_id, set_io_info_dim_ref, set_io_info_var_ref, set_io_info_att_ref
      !
      ! [B] methods from IO-packages
      !
      ! [B.1] methods from DredgeSim to set data
      USE p_dredgesim_ui, ONLY : &
         ! methods
         get_ds_input_files_ref
      ! 
      ! [B.2] methods from other packages to get required data from
      USE io_ipds_ui, ONLY : &
         ! methods
         init_ipds, new_ipds, read_ipds, kill_ipds, clear_ipds, ok_ipds, &
         setup_ipds_work_object, setup_ipds_file, setup_ipds_name, &
         get_ipds_dim, get_ipds_var, get_ipds_att
      !
      ! -------------------------------------------------------------------
      IMPLICIT NONE
      ! -------------------------------------------------------------------
      !
      ! local parameters and variables
      !! name of module (leave name 'externe Routine')
      !LEO raus CHARACTER (LEN=15) , PARAMETER :: c_modname='externe Routine'     ! 
      !! name of external subroutine
      CHARACTER (LEN=20) , PARAMETER :: c_upname_lc='ext_ds_ini_data_read' !  
      !! pointer to input files
      TYPE (t_io_info)   , POINTER   :: input_files_lc(:)                  ! 
      ! variables
      TYPE (t_error) , ALLOCATABLE :: all_errors_lc(:)                     ! 
      CHARACTER (LEN=c_max_len_pac) , PARAMETER :: c_pac_lc(1)= (/ &       ! 
       'io_ipds   ' /)    ! 
      INTEGER :: i_lc, j_lc, nn_lc, idx_lc ! 
      !
      ! ------------------------------------------------------------------
      ! [1] generate temporary required local error messages
      ! ------------------------------------------------------------------
      CALL init_all_errors_lc ( )
      ! ------------------------------------------------------------------
      ! [2] read and transfer required data
      ! ------------------------------------------------------------------
      input_files_lc => get_ds_input_files_ref ( )
      IF ( ASSOCIATED(input_files_lc) ) THEN
         DO i_lc=1,SIZE(c_pac_lc)
            IF ( any_error( ) ) EXIT
            nn_lc = get_io_info_pac_count( input_files_lc, c_pac_lc(i_lc) )
            DO j_lc=1,nn_lc
               IF ( any_error( ) ) EXIT
               idx_lc = get_io_info_pac_idx( input_files_lc, c_pac_lc(i_lc), j_lc )
               SELECT CASE ( c_pac_lc(i_lc) ) 
                  CASE ( c_pac_lc(1) ) ! initializing / Lesen mit methods des Pakets "io_ipds" 
                     CALL ini_data_read_io_ipds( input_files_lc, c_pac_lc(i_lc), idx_lc )
                  CASE DEFAULT
                     CALL setup_error_act ( all_errors_lc, -99000, c_upname_lc, c_modname ) ! 
                     CALL setup_error_act ( '<package>', TRIM(c_pac_lc(i_lc)) )             ! 
               END SELECT
            END DO
         END DO
      END IF
      ! ------------------------------------------------------------------
      ! [3] deallocating local errors
      ! ------------------------------------------------------------------
      CALL clear_all_errors_lc ( )
      ! ------------------------------------------------------------------
      NULLIFY(input_files_lc)
      !
   CONTAINS
      !
      !! read and transfer information from ipds-file ...
      !! ... using methods of the "io_ipds"-package
      !! ... ipds-file handles geographical information of polygons to operate on
      !
      SUBROUTINE ini_data_read_io_ipds ( inp_files_ld, package_ld, idx_ld )
      !! liste of all input files
      TYPE (t_io_info)  , INTENT(INOUT) :: inp_files_ld(:) ! 
      !! package identifier
      CHARACTER (LEN=*) , INTENT(IN)    :: package_ld      ! 
      !! pointer to current file to read in "inp_files(:)" 
      INTEGER           , INTENT(IN)    :: idx_ld          ! 
      ! name of subroutine
      CHARACTER (LEN=21) , PARAMETER :: c_upname_ld='ini_data_read_io_ipds' ! 
      ! variables
      LOGICAL                       :: l_ok_ld     ! 
      CHARACTER (LEN=10)            :: l_char_ld   ! 
      CHARACTER (LEN=c_max_len_key) :: l_key_ld    ! 
      TYPE (t_file)                 :: l_file_ld   ! 
      TYPE (t_dim)        , POINTER :: p_dim_ld(:) ! 
      TYPE (t_var)        , POINTER :: p_var_ld(:) ! 
      TYPE (t_att)        , POINTER :: p_att_ld(:) ! 
      INTEGER                       :: id_ld, jdx_ld  ! 
       !
      ! -------------------------------------------------------------------
      ! [1] determine essential parameters
      ! -------------------------------------------------------------------
      l_file_ld = get_io_info_file( inp_files_ld(idx_ld) )
      l_key_ld  = get_io_info_key ( inp_files_ld(idx_ld) )
      id_ld     = get_io_info_id  ( inp_files_ld, l_file_ld, package_ld )
      ! -------------------------------------------------------------------
      ! [2] transfer only for selected files
      ! -------------------------------------------------------------------
      IF ( id_ld < 1 ) THEN
         ! [2.1] determine new id  - - - - 
         IF ( no_error( ) ) CALL init_ipds( )
         IF ( no_error( ) ) CALL new_ipds ( id_ld )
         IF ( no_error( ) ) CALL setup_ipds_work_object ( id_ld )
         IF ( no_error( ) ) CALL setup_ipds_file ( l_file_ld )
         IF ( no_error( ) ) CALL setup_ipds_name ( l_key_ld )
         ! [2.1.1] read information informationen about files  - - -
         IF ( no_error( ) ) THEN
            WRITE(*,*) ' ... now reading info from "'//TRIM(package_ld)//'" file'
            WRITE(*,*) '     name = ',TRIM(get_file_name(l_file_ld))
            CALL read_ipds ( )
            WRITE(*,*) ' DONE '
         END IF
         IF ( no_error( ) ) l_ok_ld = ok_ipds ( )
         IF ( no_error( ) ) CALL set_io_info_id ( inp_files_ld(idx_ld), id_ld )
         ! [2.1.2] transfer of dimensions, variables and attributes - - - 
         IF ( no_error( ) ) THEN
            CALL setup_ipds_work_object ( id_ld )
            p_dim_ld => get_ipds_dim ( )
            p_var_ld => get_ipds_var ( )
            p_att_ld => get_ipds_att ( )
            ! [2.2.1] transfer of dimensions, variables and attributes - - -
            CALL set_io_info_dim_ref( inp_files_ld(idx_ld), p_dim_ld )
            CALL set_io_info_var_ref( inp_files_ld(idx_ld), p_var_ld )
            CALL set_io_info_att_ref( inp_files_ld(idx_ld), p_att_ld )
            NULLIFY(p_dim_ld,p_var_ld,p_att_ld)
         END IF
         IF ( no_error( ) ) CALL kill_ipds( id_ld )
         IF ( no_error( ) ) CALL clear_ipds( )
      ELSE
         ! [2.2] id is already known - - -
         IF ( no_error( ) ) CALL setup_ipds_work_object ( id_ld )
         IF ( no_error( ) ) CALL set_io_info_id ( inp_files_ld(idx_ld), id_ld )
         IF ( no_error( ) ) THEN
            jdx_ld = get_io_info_idx ( inp_files_ld, l_file_ld, package_ld, id_ld )
            IF ( jdx_ld < 1 ) THEN
               !LEO TODO why is all_errors global is all_errors_lc?
               CALL setup_error_act ( all_errors_lc, -99010, c_upname_ld, c_modname )
               CALL setup_error_act ( '<key>', TRIM(l_key_ld) )
               CALL setup_error_act ( '<datafile>', TRIM(get_file_name(l_file_ld)) )
               CALL setup_error_act ( '<package>', TRIM(package_ld) )
             WRITE(l_char_ld,'(I10)') id_ld ; CALL setup_error_act ( '<id>', TRIM(l_char_ld) )
            ELSE
               p_dim_ld => get_io_info_dim_ref ( inp_files_ld(jdx_ld) )
               p_var_ld => get_io_info_var_ref ( inp_files_ld(jdx_ld) )
               p_att_ld => get_io_info_att_ref ( inp_files_ld(jdx_ld) )
               CALL set_io_info_dim_ref( inp_files_ld(idx_ld), p_dim_ld )
               CALL set_io_info_var_ref( inp_files_ld(idx_ld), p_var_ld )
               CALL set_io_info_att_ref( inp_files_ld(idx_ld), p_att_ld )
               NULLIFY(p_dim_ld,p_var_ld,p_att_ld)
            END IF
         END IF
      END IF
      !
      END SUBROUTINE ini_data_read_io_ipds
      !
      !! initializing all error messages for this external subroutine
      SUBROUTINE init_all_errors_lc ( )
      !! variable
      INTEGER :: i_lc, ic_lc ! 
      !
      DO i_lc=1,2
         ic_lc = 0
         ic_lc = ic_lc + 1
         IF ( i_lc == 2 ) THEN
            IF ( no_error( ) ) CALL set_error_ierr ( all_errors_lc(ic_lc), -99000 )
            IF ( no_error( ) ) CALL set_error_cerr ( all_errors_lc(ic_lc), &
               'error category IO-methods in external subroutine\n'//&
               'method for reading data is not available\n'//&
               'package identifier = <package>\n'//&
               '--> check / extend code in external subroutine "ext_ds_ini_data_read"' )
         END IF
         ic_lc = ic_lc + 1
         IF ( i_lc == 2 ) THEN
            IF ( no_error( ) ) CALL set_error_ierr ( all_errors_lc(ic_lc), -99010 )
            IF ( no_error( ) ) CALL set_error_cerr ( all_errors_lc(ic_lc), &
               'error category IO-methods in externem Schnittstellenprogramm\n'//&
               'pointer on available file with DIM, ATT, VAR not found\n'//&
               'key     = <key>\n'//&
               'data    = <datafile>\n'//&
               'package = <package>\n'//&
               'id      = <id>\n'//&
               '--> check / extend code in external subroutine "ext_ds_ini_data_read"' )
            END IF
            ! allocating arrays at first cycle (i==1)
            IF ( i_lc == 1 ) THEN
               ALLOCATE ( all_errors_lc( ic_lc ) )
               CALL new_error( all_errors_lc(:) )
            END IF
            !
         END DO
         !    
      END SUBROUTINE init_all_errors_lc
      !
      !! allocating/initializing all local required error messages
      !! subroutine does not throw error messages
      SUBROUTINE clear_all_errors_lc ( )
         !
         IF    ( ALLOCATED( all_errors_lc ) ) THEN
            CALL kill_error( all_errors_lc(:) )
            DEALLOCATE ( all_errors_lc )
         END IF
         !
      END SUBROUTINE clear_all_errors_lc
      !
   END SUBROUTINE ext_ds_ini_data_read_d
!TODO ext_ds_ini_data_read_d ( )   
   !   
   !
   !
!TODO ext_ds_ini_grid_import_d
   SUBROUTINE ext_ds_ini_grid_import_d ( )
      !
      ! [A.1.1] base module with global constant values
      USE b_constants, ONLY : &
         ! parameters / constant values
         Double
      ! [A.1.2] base module "error-handling"
      USE b_error, ONLY :   &
         ! routines
         no_error,        &
         any_error
      !
      !USE p_sisyphe_ui, ONLY : get_sm_DEBUG
      ! [B] methods from IO-packages
      !
      ! [B.1] methods aus DredgeSim
      USE p_dredgesim_ui, ONLY : & !
            set_ds_nodes_of_poly, set_ds_grav_center_coord, set_ds_node_coord, set_ds_poly_depth, set_ds_node_water_depth, &
            set_ds_poly_area, set_ds_node_area, set_ds_node_depth, set_ds_poly_depth, set_ds_edge_depth, &
            set_ds_node_porosity, set_ds_node_sediment_fraction, set_ds_cell_sediment_fraction, &
            set_ds_edgelist_of_poly, set_ds_nodelist_of_poly, set_ds_node_noero_depth
      ! 
      ! [B.2] methods from other packages to get required data from
      !LEO USE p_sisyphe_ui, ONLY :          &
      !LEO     get_sm_nodes_of_poly, get_sm_grav_center_coord, get_sm_node_coord, &
      !LEO     get_sm_poly_area, &
      !LEO     get_sm_node_depth, get_sm_poly_depth, get_sm_node_water_depth, &
      !LEO     get_sm_node_porosity, get_sm_node_sediment_fraction, get_sm_cell_sediment_fraction, &
      !LEO     get_sm_edgelist_of_poly, get_sm_nodelist_of_poly, &
      !LEO     get_sm_node_noero_depth, get_sm_node_area
  
      USE m_dredgesim_data, ONLY : debug_ds
      !
      ! -------------------------------------------------------------------
      IMPLICIT NONE
      ! -------------------------------------------------------------------
      !
      ! local parameters and variables
      !! name of module (leave name 'externe Routine')
      !LEO raus CHARACTER (LEN=15), PARAMETER :: c_modname='externe Routine'  ! 
      !! name of external subroutine
      CHARACTER (LEN=22), PARAMETER :: c_upname_b='ext_ds_ini_grid_import' !  
      !
      IF ( no_error( ) ) THEN
         IF (DEBUG_ds > 0) THEN
            WRITE(*,*) ' name = "'//TRIM( c_upname_b )//'"'
         END IF
         CALL set_ds_nodes_of_poly ( get_sm_nodes_of_poly( ))
         CALL set_ds_edgelist_of_poly ( get_sm_edgelist_of_poly( ))
         CALL set_ds_nodelist_of_poly ( get_sm_nodelist_of_poly( ))
         CALL set_ds_grav_center_coord ( get_sm_grav_center_coord( ))
         CALL set_ds_node_coord ( get_sm_node_coord ( ))
         CALL set_ds_poly_area ( get_sm_poly_area( ))
         !LEO TODO here is a critical bug node_area was not correct
         CALL set_ds_node_area ( get_sm_node_area( ))
         CALL set_ds_node_depth ( get_sm_node_depth( ))
         CALL set_ds_poly_depth ( get_sm_poly_depth( ))
         CALL set_ds_node_water_depth ( get_sm_node_water_depth( ))
         CALL set_ds_node_porosity ( get_sm_node_porosity( ))
         CALL set_ds_node_sediment_fraction ( get_sm_node_sediment_fraction( ))
         CALL set_ds_cell_sediment_fraction ( get_sm_cell_sediment_fraction( ))
         CALL set_ds_node_noero_depth ( get_sm_node_noero_depth( ))

      END IF
      !
   END SUBROUTINE ext_ds_ini_grid_import_d
!TODO end ext_ds_ini_grid_import_d
   !
   !
   !
!TODO ext_ds_fraction_name_d
   SUBROUTINE ext_ds_fraction_name_d ( )
   !
      ! [A.1.1] base module "constants"
      USE b_constants, ONLY : &
         ! parameters / constant values
         Double
      ! [A.1.2] base module "error-handling"
      USE b_error, ONLY :   &
         ! data type
         t_error, &
         ! methods
         no_error
      !
      ! [B] methods from IO-packages
      !
      ! [B.1] methods from DredgeSim to set data
      USE p_dredgesim_ui, ONLY : &
          ! methods
          set_ds_fraction_name, set_ds_used_sediment_classes
      ! 
      ! [B.2] methods from other packages to read required data
      !LEO USE p_sisyphe_ui, ONLY : &
      !LEO ! methods
      !LEO get_sm_nof_sediment_fractions, get_sm_sed_class_name, get_sm_used_sediment_classes
      !
      ! -------------------------------------------------------------------
      IMPLICIT NONE
      ! -------------------------------------------------------------------
      !
      ! local parameters and variables
      !! name of module (leave name 'externe Routine')
      !LEO raus CHARACTER (LEN=15) , PARAMETER :: c_modname='externe Routine'     ! 
      !! name of external subroutine
      CHARACTER (LEN=20) , PARAMETER :: c_upname_c='ext_ds_fraction_name' !  
      !
      !LEO OLD:
      !IF ( no_error( ) ) nof_c = get_sm_nof_sediment_fractions ( )
      !IF ( no_error( ) .AND. nof_c > 0 ) CALL set_ds_fraction_name( get_sm_sed_class_name( nof_c ) )
      !IF ( no_error( ) ) CALL set_ds_used_sediment_classes( get_sm_used_sediment_classes( ) )
      !LEO NEW:
      IF ( no_error( ) .AND. NSICLA > 0 ) CALL set_ds_fraction_name( get_sm_sed_class_name() )
      IF ( no_error( ) ) CALL set_ds_used_sediment_classes( get_sm_used_sediment_classes( ) )
      !END LEO NEW
   !
   END SUBROUTINE ext_ds_fraction_name_d
!TODO end ext_ds_fraction_name_d

   !
   !
!TODO ext_ds_dredge_poly_index_d
!use ending _le to get local variables
   SUBROUTINE ext_ds_dredge_poly_index_d ( )
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
           get_ds_input_files_ref, get_ds_grav_center_coord_ref, get_ds_nof_dredge_poly, &
           get_ds_dredge_poly_name, set_ds_dredge_poly_index, &
           get_ds_nof_dispose_poly, get_ds_dispose_poly_name, &
           set_ds_dispose_poly_index, &
           get_ds_nof_dredge_poly_tc, get_ds_dredge_poly_name_tc, &
           set_ds_dredge_poly_index_tc, &
           get_ds_nof_dispose_poly_tc, get_ds_dispose_poly_name_tc, &
           set_ds_dispose_poly_index_tc, &
           get_ds_nof_art_bed_load_poly, get_ds_art_bed_load_poly_name, &
           set_ds_art_bed_load_pol_index
               ! 
      ! [B.2] methods from other packages to get required data from
      USE io_ipds_ui, ONLY : &
           ! methods
           init_ipds, new_ipds, kill_ipds, clear_ipds, &
           setup_ipds_work_object, setup_ipds_file,    &
           setup_ipds_name, read_ipds, is_ipds_point_in_region
      !
      ! -------------------------------------------------------------------
      IMPLICIT NONE
      ! -------------------------------------------------------------------
      !
      ! local parameters and variables
      !! name of module (leave name 'externe Routine')
      !Leo raus CHARACTER (LEN=15) , PARAMETER :: c_modname='externe Routine'     ! 
      !! name of external subroutine
      CHARACTER (LEN=24) , PARAMETER :: c_upname_le='ext_ds_dredge_poly_index' !  
      !! pointer to input files
      TYPE (t_io_info)   , POINTER   :: input_files_le(:)                  ! 
      ! variables
      TYPE (t_error) , ALLOCATABLE :: all_errors_le(:)                     ! 
      CHARACTER (LEN=c_max_len_pac) , PARAMETER :: c_pac_le(1)= (/ &       ! 
           'io_ipds   ' /)    ! 
      INTEGER :: i_le, j_le, nn_le, idx_le ! 
      !
      ! ------------------------------------------------------------------
      ! [1] generate temporary required local error messages
      ! ------------------------------------------------------------------
      CALL init_all_errors_le ( )
      ! ------------------------------------------------------------------
      ! [2] read and transfer required data
      ! ------------------------------------------------------------------
      input_files_le => get_ds_input_files_ref ( )
      IF ( ASSOCIATED(input_files_le) ) THEN
         DO i_le=1,SIZE(c_pac_le)
            IF ( any_error( ) ) EXIT
            nn_le = get_io_info_pac_count( input_files_le, c_pac_le(i_le) )
            DO j_le=1,nn_le
               IF ( any_error( ) ) EXIT
               idx_le = get_io_info_pac_idx( input_files_le, c_pac_le(i_le), j_le )
               SELECT CASE ( c_pac_le(i_le) ) 
               CASE ( c_pac_le(1) ) ! Berechnen von "dredge_poly_index" aus Inhalt IPDS-Datei
                  CALL compute_dredge_poly_index( input_files_le, c_pac_le(i_le), idx_le )
               CASE DEFAULT
                  CALL setup_error_act ( all_errors_le, -99000, c_upname_le, c_modname ) ! 
                  CALL setup_error_act ( '<package>', TRIM(c_pac_le(i_le)) )             ! 
               END SELECT
            END DO
         END DO
      END IF
      ! ------------------------------------------------------------------
      ! [3] deallocating local errors
      ! ------------------------------------------------------------------
      CALL clear_all_errors_le ( )
      ! ------------------------------------------------------------------
      NULLIFY(input_files_le)
      !
   CONTAINS
      !
      !! determination of elements inside polygons to operate on ...
      !! ... using methods of the "io_ipds"-package
      !! ... polygons can be dredge and / or disposal polygons
      SUBROUTINE compute_dredge_poly_index ( inp_files_lf, package_lf, idx_lf )
        !! list of all input files
        TYPE (t_io_info)  , INTENT(INOUT) :: inp_files_lf(:) ! 
        !! package identifier
        CHARACTER (LEN=*) , INTENT(IN)    :: package_lf      ! 
        !! pointer to current file to read in "inp_files(:)"
        INTEGER           , INTENT(IN)    :: idx_lf          ! 
        ! name of subroutine
        CHARACTER (LEN=25) , PARAMETER :: c_upname_lf='compute_dredge_poly_index' ! 
        ! variables
        CHARACTER (LEN=10) :: l_char_lf ! 
        INTEGER :: i_lf, j_lf, id_lf, stat_lf ! 
        REAL (KIND=Double)    , POINTER :: grav_center_coord_lf(:,:)      ! 
        LOGICAL           , ALLOCATABLE :: l_inside_lf(:)            ! 
        INTEGER           , ALLOCATABLE :: l_dredge_poly_index_lf(:,:) !
        INTEGER           , ALLOCATABLE :: l_dispose_poly_index_lf(:,:)!
        INTEGER           , ALLOCATABLE :: l_dredge_poly_index_tc_lf(:,:) !
        INTEGER           , ALLOCATABLE :: l_dispose_poly_index_tc_lf(:,:) !
        INTEGER           , ALLOCATABLE :: l_art_bed_load_poly_index_lf(:,:)
        TYPE (t_point_2d) , ALLOCATABLE :: center_points_lf(:)       ! 
        CHARACTER (LEN=c_max_len_key) :: l_key_lf    ! 
        TYPE (t_file)                 :: l_file_lf   ! 
        !
        ! -------------------------------------------------------------------
        ! [1] determine essential parameters
        ! -------------------------------------------------------------------
        id_lf = get_io_info_id  ( inp_files_lf(idx_lf) )
        l_file_lf = get_io_info_file( inp_files_lf(idx_lf) )
        l_key_lf  = get_io_info_key ( inp_files_lf(idx_lf) )
        CALL init_ipds ( )
        CALL new_ipds ( id_lf )
        CALL setup_ipds_work_object ( id_lf )
        CALL setup_ipds_file ( l_file_lf )
        CALL setup_ipds_name ( l_key_lf )
        CALL read_ipds ( )
        ! -------------------------------------------------------------------
        ! [2] getting gravitational center coordinates and change of data to "t_point_2d"
        ! -------------------------------------------------------------------
        grav_center_coord_lf => get_ds_grav_center_coord_ref ( )
        IF ( ASSOCIATED(grav_center_coord_lf) .AND. no_error( ) ) THEN
           ! [2.1] getting gravitational center coordinates and change of data to "t_point_2d"
           ALLOCATE( center_points_lf(SIZE(grav_center_coord_lf,1)), l_dredge_poly_index_lf(SIZE(grav_center_coord_lf,1),&
                     get_ds_nof_dredge_poly( )), &
                     l_inside_lf(SIZE(grav_center_coord_lf,1)), STAT=stat_lf )
           ALLOCATE(l_dispose_poly_index_lf(SIZE(grav_center_coord_lf,1),get_ds_nof_dispose_poly( ) ))
           ALLOCATE(l_dredge_poly_index_tc_lf(SIZE(grav_center_coord_lf,1),get_ds_nof_dredge_poly_tc( ) ))
           ALLOCATE(l_dispose_poly_index_tc_lf(SIZE(grav_center_coord_lf,1),get_ds_nof_dispose_poly_tc( ) ))
           ALLOCATE(l_art_bed_load_poly_index_lf(SIZE(grav_center_coord_lf,1),get_ds_nof_art_bed_load_poly( ) ))
           IF ( stat_lf /= 0 ) THEN
              CALL setup_error_act ( all_errors_le(:), -93200, c_upname_lf, c_modname, stat_lf )
              CALL setup_error_act ( '<name>', 'center_points_lf(:),l_dredge_poly_index_lf(:,:),l_inside_lf(:),'// &
              'l_dispose_poly_index_lf(:,:), l_dredge_poly_index_tc_lf(:,:), l_dispose_poly_index_tc_lf(:,:),'// &
              'l_art_bed_load_poly_index_lf(:,:)' )
              WRITE(l_char_lf,'(I10)') SIZE(grav_center_coord_lf,1) ; CALL setup_error_act ( '<idim1>', l_char_lf )
           ELSE
              CALL new_point_2d ( center_points_lf )
              DO i_lf=1,SIZE(center_points_lf)
                 CALL set_point_2d_xy ( center_points_lf(i_lf), grav_center_coord_lf(i_lf,1), grav_center_coord_lf(i_lf,2) )
              END DO
              ! [2.2] determine index list element - dredge polygon
              l_dredge_poly_index_lf(:,:) = 0
              DO i_lf=1,get_ds_nof_dredge_poly( )
                 l_inside_lf(:) = is_ipds_point_in_region( center_points_lf(:), get_ds_dredge_poly_name(i_lf) )
                 WHERE( l_inside_lf ) l_dredge_poly_index_lf(:,i_lf) = 1
              END DO
              ! [2.3] transfer to DredgeSim
              CALL set_ds_dredge_poly_index( l_dredge_poly_index_lf )
              ! [2.4] determine index list element - disposal polygon
              l_dispose_poly_index_lf(:,:) = 0
              DO j_lf=1, get_ds_nof_dredge_poly( )
                 DO i_lf=1,get_ds_nof_dispose_poly( )
                    l_inside_lf(:) = is_ipds_point_in_region( center_points_lf(:), get_ds_dispose_poly_name(i_lf, j_lf) )
                    WHERE( l_inside_lf ) l_dispose_poly_index_lf(:,i_lf) = 1
                 END DO
              END DO
              ! [2.5] transfer to DredgeSim
              CALL set_ds_dispose_poly_index( l_dispose_poly_index_lf )
              ! [2.6] determine index list element - dredge polygon for time controlled maintenance
              l_dredge_poly_index_tc_lf(:,:) = 0
              DO i_lf=1,get_ds_nof_dredge_poly_tc( )
                 l_inside_lf(:) = is_ipds_point_in_region( center_points_lf(:), get_ds_dredge_poly_name_tc(i_lf) )
                 WHERE( l_inside_lf ) l_dredge_poly_index_tc_lf(:,i_lf) = 1
              END DO
              ! [2.7] transfer to DredgeSim
              CALL set_ds_dredge_poly_index_tc( l_dredge_poly_index_tc_lf )
              ! [2.8] determine index list element - disposal polygon for time controlled maintenance
              l_dispose_poly_index_tc_lf(:,:) = 0
              DO i_lf=1,get_ds_nof_dispose_poly_tc( )
                 l_inside_lf(:) = is_ipds_point_in_region( center_points_lf(:), get_ds_dispose_poly_name_tc(i_lf) )
                 WHERE( l_inside_lf ) l_dispose_poly_index_tc_lf(:,i_lf) = 1
              END DO
              ! [2.9] transfer to DredgeSim
              CALL set_ds_dispose_poly_index_tc( l_dispose_poly_index_tc_lf )
              ! [2.10] determine index list element - disposal polygon for artificial bed load supply
              l_art_bed_load_poly_index_lf(:,:) = 0
              DO i_lf=1,get_ds_nof_art_bed_load_poly( )
                 l_inside_lf(:) = is_ipds_point_in_region( center_points_lf(:), get_ds_art_bed_load_poly_name(i_lf) )
                 WHERE( l_inside_lf ) l_art_bed_load_poly_index_lf(:,i_lf) = 1
              END DO
              ! [2.11] transfer to DredgeSim
              CALL set_ds_art_bed_load_pol_index( l_art_bed_load_poly_index_lf )
              ! [2.12] clearing up local data which is no longer needed
              CALL kill_point_2d ( center_points_lf )
              DEALLOCATE( center_points_lf, l_dredge_poly_index_lf, l_inside_lf, STAT=stat_lf )
              DEALLOCATE( l_dispose_poly_index_lf )
              DEALLOCATE( l_dredge_poly_index_tc_lf )
              DEALLOCATE( l_dispose_poly_index_tc_lf )
              DEALLOCATE( l_art_bed_load_poly_index_lf )
              IF ( stat_lf /= 0 ) THEN
                 CALL setup_error_act ( all_errors_le(:), -94200, c_upname_lf, c_modname, stat_lf )
                 CALL setup_error_act ( '<name>', 'center_points_lf(:),l_dredge_poly_index_lf(:,:),l_inside_lf(:),'// &
                 'l_dispose_poly_index_lf(:,:), l_dredge_poly_index_tc_lf(:,:), l_dispose_poly_index_tc_lf(:,:),'// &
                 'l_art_bed_load_poly_index_lf(:,:)' )

                 !CALL setup_error_act ( '<name>', 'center_points_lf(:),l_dredge_poly_index_lf(:,:),l_inside_lf(:), &
                 !                       l_dispose_poly_index_lf(:,:), l_dredge_poly_index_tc_lf(:,:), l_dispose_poly_index_tc_lf(:,:), &
                 !                       l_art_bed_load_poly_index_lf(:,:)' )
              END IF
           END IF
        END IF
        CALL kill_ipds( id_lf )
        CALL clear_ipds( )
        !
      END SUBROUTINE compute_dredge_poly_index
      !
      !! initializing all error messages for this external subroutine
      SUBROUTINE init_all_errors_le ( )
        !! variable
        INTEGER :: i_ld, ic_ld ! 
        !
        DO i_ld=1,2
           ic_ld = 0
           ic_ld = ic_ld + 1
           IF ( i_ld == 2 ) THEN
              IF ( no_error( ) ) CALL set_error_ierr ( all_errors_le(ic_ld), -93200 )
              IF ( no_error( ) ) CALL set_error_cerr ( all_errors_le(ic_ld), &
                   'error category private ALLOCATE-methods\n'//&
                   'two-dimensional REAL(Double)-array\n'//&
                   'name        = "<name>"\n'//&
                   'dimension 1 = "<idim1>"\n'//&
                   '--> check check code in external subroutine subroutine "ext_ds_dredge_poly_index"' )
           END IF
           ic_ld = ic_ld + 1
           IF ( i_ld == 2 ) THEN
              IF ( no_error( ) ) CALL set_error_ierr ( all_errors_le(ic_ld), -94200 )
              IF ( no_error( ) ) CALL set_error_cerr ( all_errors_le(ic_ld), &
                   'error category Private DEALLOCATE-methods\n'//&
                   'REAL(Double)-array\n'//&
                   'name        = "<name>"\n'//&
                   '--> check code / data in module "m_dredgesim_data"' )
           END IF
           ic_ld = ic_ld + 1
           IF ( i_ld == 2 ) THEN
              IF ( no_error( ) ) CALL set_error_ierr ( all_errors_le(ic_ld), -99000 )
              IF ( no_error( ) ) CALL set_error_cerr ( all_errors_le(ic_ld), &
                   'error category IO-methods in external subroutine\n'//&
                   'method for reading data is not available\n'//&
                   'package identifier = <package>\n'//&
                   '--> check / extend  check code in external subroutine subroutine "ext_ds_dredge_poly_index"' )
           END IF
           ! allocating arrays at first cycle (i==1)
           IF ( i_ld == 1 ) THEN
              ALLOCATE ( all_errors_le( ic_ld ) )
              CALL new_error( all_errors_le(:) )
           END IF
           !
        END DO
        !    
      END SUBROUTINE init_all_errors_le
      !
      !! allocating/initializing all local required error messages 
      !! subroutine does not throw error messages
      SUBROUTINE clear_all_errors_le ( )
        !
        IF ( ALLOCATED( all_errors_le ) ) THEN
           CALL kill_error( all_errors_le(:) )
           DEALLOCATE ( all_errors_le )
        END IF
        !
      END SUBROUTINE clear_all_errors_le
      !
   END SUBROUTINE ext_ds_dredge_poly_index_d
!TODO end ext_ds_dredge_poly_index_d
   !
   !
   !
   !
  ! -------------------------------------------------------------------
! LEO end of ext_ subroutines
  ! -------------------------------------------------------------------




  ! -------------------------------------------------------------------


  !! initialization and setup of DredgeSim
  !! subroutine does not throw error messages
  SUBROUTINE init_and_setup_ds_d ( & 
    ncou, dredgeinp, ngeo, ngeo_name, &
    NCSIZE, ipid, NELEM_S, NSICLA_S, NPOIN_S, NLAYER_S, &
    mesh_S, node_area_S, zf_S, E_s, zr_S, HN_S, FDM_S, avail_S, leopr_S, ptinig_S, dt_S,LT_S, &
    mardat_S, martiM_S,XKV_S,XMVS_S)

!RK ,ilmax, ikp1d, ikm1d, &
!RK    nachb1d, indpu1d, nhp1d, nhm1d) 
   ! reading geometry data by using h_grid interpreter
   USE b_file, ONLY : t_file,  &
       ! routines / interfaces
       new_file,               &
       kill_file,              &
       set_file_name,          &
       set_file_type,          &
       set_file_unit,          &
       set_file_status,        &
       set_file_form,          &
       set_file_access,        &
       set_file_delim,         &
       get_file_unit
  !
  USE p_h_grid_ui, ONLY : &
       init_h_grid,                  &
       new_h_grid,                   &
       setup_h_grid_prn_lun,         &
       setup_h_grid_trc_lun,         &
       setup_h_grid_work_object,     &
       setup_h_grid_file,            &
       setup_h_grid_name,            &  
       read_h_grid,                  &
       get_h_grid_fst_node_of_edge,  &
       get_h_grid_lst_node_of_edge,  & 
       get_h_grid_edge_center_coord, & 
       get_h_grid_nof_edges,         &
       get_h_grid_edgelist_of_poly,  &
       get_h_grid_nodelist_of_poly,  &
       get_h_grid_nof_edges,         &
       get_h_grid_grav_center_coord, &
       get_h_grid_node_coord,        &
       get_h_grid_poly_area
    !
    USE b_error, ONLY :         &
       init_error,             &
       setup_error_prn_lun,    &
       setup_error_trc_lun,    &
       any_error,              &
       print_error_act,        &
       setup_error_err_warn_on, &
       no_error
   !
   USE p_dredgesim_ui, ONLY : init_dredgesim, setup_ds_steering_file, &
      start_dredgesim, setup_dredgesim_prn_lun, &
      setup_dredgesim_trc_lun,& !,prepare_dredgesim
      set_ds_knolg !LEO added knolg

    use bief, only : bief_obj, bief_mesh
    use m_dredgesim_data, ONLY : leopr_ds, debug_ds , nb_neighb, dimbuf, dimnhcom,set_ds_node_area_ref
    
    INTEGER :: idx_slash
    !
    ! DredgeSim trace
    INTEGER            , PARAMETER :: sed_trc_lun = 81
    ! DredgeSim print
    INTEGER            , PARAMETER :: sed_prn_lun = 82
    ! h_grid id
    INTEGER                           h_grid_id
    ! h_grid variable
    TYPE (t_file) :: dredgesim_mesh
!RK Sisyphe variables
     INTEGER :: ipid, NCSIZE
     INTEGER :: ncou !,NOMBLAY
     CHARACTER(LEN=250) :: dredgeinp, ngeo_name

     TYPE (t_file) :: dredgesim_steering_file
     INTEGER :: NGEO, NELEM_S, NPOIN_S, NSICLA_S
     INTEGER :: PTINIG_S, LEOPR_S, LT_S
     INTEGER :: MARDAT_S(3), MARTIM_S(3)
     DOUBLE PRECISION :: DT_S, XKV_S, FDM_S(NSICLA_S), XMVS_S, AVAIL_S(NPOIN_S, 1,NSICLA_S)
! ich hoffe, dass das so geht und ich nicht avail(npoin, nomblay, nsicla) brauche...
     TYPE(BIEF_MESH) :: MESH_S
     TYPE (BIEF_OBJ) :: ZF_S, ZR_S, HN_S, E_S, NLAYER_S, NODE_AREA_S ! MASKEL
     
    !LS workaround to set variables global
    NELEM = NELEM_S
    NSICLA = NSICLA_S
    NPOIN = NPOIN_S
    NLAYER = NLAYER_S
    MESH = MESH_S !TODO check if this works


    !LS allocate and set ZF_NODE_DEPTH and set it to zero
    ALLOCATE (ZF_NODE_DEPTH(npoin))
    ALLOCATE (NODE_AREA(npoin))
     
    NODE_AREA = NODE_AREA_S%R !TODO check if this works
    !LEO set node_area here!
    !CALL set_ds_node_area_ref(NODE_AREA)
    !LEO TODO alls dynamic data is given by run_dredgesim!!!
    !remove the unused and wrong things here!
    ZF = ZF_S !TODO REMOVE BIEF dependency
    ZR = ZR_S !TODO REMOVE BIEF dependency
    HN = HN_S !REMOVE BIEF dependency
    E = E_S
    NLAYER = NLAYER_S
    ALLOCATE (FDM(NSICLA_S))
    FDM = FDM_S
    ALLOCATE (AVAIL(NPOIN, 1,NSICLA_S))
    AVAIL = AVAIL_S
    LEOPR = LEOPR_S
    PTINIG = PTINIG_S
    DT = DT_S
    LT = LT_S
    mardat = mardat_S
    MARTIM = MARTIM_S
    XKV = XKV_S
    XMVS = XMVS_S
    

    !LEO can't set this to Zero! ZF_NODE_DEPTH = 0.D0

!LEO THIS looky like a bug isn't it that we have to use set/get for nb_neighb?!
    !LEO removed upwards
    ! MESH%NB_NEIGHB: NUMBER OF NEIGHBOURING SUB-DOMAINS
     nb_neighb = MESH%NB_NEIGHB
    ! FIRST DIMENSION OF BUFFERS
     dimbuf = MESH%BUF_SEND%DIM1
    ! Dimension?!?
     dimnhcom = MESH%NH_COM%DIM1
    !LEO end removed upwards
!LEO

    !LEO set knolg if ncsize larger than 1
    IF (NCSIZE .GT. 1) THEN
      CALL set_ds_knolg(get_sm_knolg())
    END IF 

    OPEN (sed_prn_lun, FILE='DredgeSim.sdr')
    OPEN (sed_trc_lun, FILE='error.sdr')
    CALL init_error ()
    CALL setup_error_prn_lun ( sed_prn_lun )
    CALL setup_error_trc_lun ( sed_trc_lun )
    !
    ! initialize DredgeSim
    !
    CALL init_dredgesim ()
    CALL setup_dredgesim_prn_lun ( sed_prn_lun)
    CALL setup_dredgesim_trc_lun ( sed_trc_lun)
    !
    ! generating variable "dredgesim_steering_file", TYPE (t_file)
    !
    CALL new_file        ( dredgesim_steering_file )
    !------------------------------------------------------------------------------$
    CALL set_file_name   ( dredgesim_steering_file, TRIM(dredgeinp))
    !------------------------------------------------------------------------------$
    CALL set_file_type   ( dredgesim_steering_file, 'STEERING' )
    CALL set_file_unit   ( dredgesim_steering_file, NCOU )
    CALL set_file_status ( dredgesim_steering_file, 'OLD' )
    !    
    ! setup dredgesim-steering file
    !
    CALL setup_ds_steering_file ( dredgesim_steering_file )
    CALL init_h_grid ()
    CALL setup_h_grid_prn_lun ( sed_prn_lun )
    CALL setup_h_grid_trc_lun ( sed_trc_lun )
    idx_slash = MAX ( SCAN(ngeo_name,'/',back=.TRUE.), SCAN(ngeo_name,'\',back=.TRUE.), SCAN(ngeo_name,'\',back=.TRUE.) )
    CALL new_h_grid (h_grid_id)
    CALL new_file        ( dredgesim_mesh )
    CALL set_file_name   ( dredgesim_mesh, ngeo_name(idx_slash+1:LEN_TRIM(ngeo_name)))
    IF (debug_ds > 0) THEN
       PRINT*, 'geoname 1...', ngeo_name(idx_slash+1:LEN_TRIM(ngeo_name))
    END IF
    CALL set_file_type   ( dredgesim_mesh, 'selafin' )
    CALL set_file_unit   ( dredgesim_mesh, ngeo )
    CALL set_file_status ( dredgesim_mesh, 'OLD' )
    CALL set_file_form   ( dredgesim_mesh, 'UNFORMATTED' ) 
    CALL set_file_access ( dredgesim_mesh, 'SEQUENTIAL' )
    CALL set_file_delim  ( dredgesim_mesh, 'NONE' )
    !
    CALL setup_h_grid_work_object ( h_grid_id )
    CALL setup_h_grid_file ( dredgesim_mesh )
    CALL setup_h_grid_name ( 'HN-h-grid' )
    CLOSE (ngeo)
    CALL read_h_grid ( dredgesim_mesh, ncsize)
    ! preparing pointers
    NULLIFY ( edgelist_of_poly )
    NULLIFY ( nodelist_of_poly )
    NULLIFY ( center_coord )
    NULLIFY ( node_coord )
    NULLIFY ( poly_area )
    ! setup work object, preparing geometry information for transfer
    CALL setup_h_grid_work_object ( 'HN-h-grid' )
    edgelist_of_poly =>  get_h_grid_edgelist_of_poly()
    nodelist_of_poly =>  get_h_grid_nodelist_of_poly()
    poly_area => get_h_grid_poly_area()
    nof_edges = get_h_grid_nof_edges()
    center_coord => get_h_grid_grav_center_coord()
    node_coord => get_h_grid_node_coord() !TODO das muss schon frueher bekannt sein?
    ! allocating local data

    WRITE(*,*) "Nelem_s , Npoin" ,nelem_s,npoin
    !LEO reactivate the outcommented out ALLOCAT (ZF_poly(nelem_s))
    ALLOCATE (ZF_poly(nelem_s))
    !LEO switched poro(nelem_S) to poro(npoin)
    ALLOCATE (poro(npoin))
    ALLOCATE (avai_dr_node(npoin,nsicla))
    ALLOCATE (avai_dr_poly(nelem_S,nsicla))
    ALLOCATE (MESH_buf_recv(dimbuf,nb_neighb))
    ALLOCATE (MESH_buf_send(dimbuf,nb_neighb))
    ALLOCATE (MESH_nh_com(dimnhcom,nb_neighb))
    ALLOCATE (nofp(nelem_S))
    ALLOCATE (used_sediment(NSICLA))
    ALLOCATE (node_depth_dummy(npoin))
    node_depth_dummy = 99.D0
    !
    ! for parallel computing
    !

    IF ( ncsize > 1 ) THEN
       CALL new_h_grid (h_grid_id)
       CALL set_file_name   ( dredgesim_mesh, ngeo_name(idx_slash+1:LEN_TRIM(ngeo_name)))
       CALL setup_h_grid_work_object ( h_grid_id )
       CALL setup_h_grid_file ( dredgesim_mesh )
       CALL setup_h_grid_name ( 'total area grid' )
       CLOSE (ngeo)
       CALL read_h_grid ( dredgesim_mesh, ncsize)
    END IF

    !DredgeSim starten
    IF (debug_ds > 0) THEN
       print*,'vor start dredgesim',no_error()
    END IF

    CALL start_dredgesim ()
    IF (debug_ds > 0) THEN
       print*,'nach start dredgesim',no_error()
    END IF

    CALL prepare_dredgesim ()

    IF (debug_ds > 0) THEN
       print*,'nach prepare dredgesim',no_error()
    END IF
    !
    

                    
     leopr_ds = leopr
! das muss ich spaeter noch mal vereinfachen. das mit dem get und set Methoden
! vermutlich nur fuer Felder
!RK     debug_ds = debug
        
    IF ( any_error( ) ) THEN
        print*,'error within init_and_setup_ds !'
        print*,'stopping program'
        CALL setup_error_prn_lun(6)
        CALL print_error_act( 'Telemac-2D' )
        CALL PLANTE(0)
        STOP
    END IF   
  END SUBROUTINE init_and_setup_ds_d
  !
  !
  !! clearing local data within sisydredge
  SUBROUTINE clear_sisydredge_d
  !
  IMPLICIT NONE
  !
  DEALLOCATE( ZF_poly, poro, avai_dr_node, avai_dr_poly, nofp, &
               used_sediment, node_depth_dummy, mesh_buf_send, & 
               mesh_buf_recv, mesh_nh_com, zf_node_depth,node_area) !LEO added ZF_NODE_DEPTH, node_area
  !
  END SUBROUTINE clear_sisydredge_d
  !

!LEO add ok_initialised  
  ! Lokale Methoden (PRIVATE)
  !
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_initialised ( upname ) &
       RESULT( ok )

!LEO add initialised from m_dredgesim_data
    USE m_dredgesim_data, ONLY : initialised
    !! Name der Subroutine die "ok_initialised" ruft
    !
    !LEO added setup_error_act
    USE b_error, ONLY : setup_error_act

    CHARACTER (LEN=*) , INTENT(IN) :: upname
    !! Testergebnis
    LOGICAL :: ok
    !! Fehlernummer
    INTEGER            :: ierr
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3)
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "p_h_grid" nicht initialisiert'
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Package "p_h_grid" ist nicht initialisiert'
       cerr(3) = '--> INIT_h_grid ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised


END MODULE p_sisyphe_ui
