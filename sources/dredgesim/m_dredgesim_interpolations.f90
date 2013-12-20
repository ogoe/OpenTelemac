! -------------------------------------------------------------------------
! HeadOfPackageInterpolationsModule ---------------------------------------
!
!! module for interpolation between polygons, nodes and edges in DredgeSim 
!!
!
MODULE m_dredgesim_interpolations
  !
  ! ----------------------------------------------------------------------
  ! [A] base modules with frequently used methods
  ! ----------------------------------------------------------------------
  !
  ! [A.1] base module with global constant values [ggf. entfernen]
  USE b_constants, ONLY : &
       ! constant values
       Double
  !
  ! ---------------------------------------------------------------------
  ! [B]  modules of the "dredgesim"-package
  ! ---------------------------------------------------------------------
  !
  ! [B.1] data module of the "dredgesim"-package 
  !
  USE m_dredgesim_data, ONLY : &
       ! horizontal grid structure --------------------------------------
       nodes_of_poly, edgelist_of_poly, edge_water_depth, & !
       poly_area, get_nof_nodes, nodelist_of_poly, node_depth, &                                            !
       ! used for the interpolations
       get_nof_poly, &
       edge_related_surface, &
       edge_weighted_sum, &
       node_related_surface, &
       node_weighted_sum
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] public declarations
  ! ---------------------------------------------------------------------
  !
  ! [C.1] interfaces
  !
  !! interpolation from polygons to edges
  INTERFACE interpolate_poly_to_edge
     MODULE PROCEDURE interpolate_poly_to_edge_0
     MODULE PROCEDURE interpolate_poly_to_edge_1
     MODULE PROCEDURE interpolate_poly_to_edge_2
  END INTERFACE
  !! interpolation from edges to polygons
  INTERFACE interpolate_edge_to_poly
     MODULE PROCEDURE interpolate_edge_to_poly_0
     MODULE PROCEDURE interpolate_edge_to_poly_1
     MODULE PROCEDURE interpolate_edge_to_poly_2
  END INTERFACE
  !! interpolation from polygons to nodes
  INTERFACE interpolate_poly_to_node
     MODULE PROCEDURE interpolate_poly_to_node_0
     MODULE PROCEDURE interpolate_poly_to_node_1
     MODULE PROCEDURE interpolate_poly_to_node_2
  END INTERFACE
  !! interpolation from nodes to polys
  INTERFACE interpolate_node_to_poly
     MODULE PROCEDURE interpolate_node_to_poly_0
     MODULE PROCEDURE interpolate_node_to_poly_1
  END INTERFACE
  !
  ! [C.2] list of public methods
  !
  PUBLIC :: interpolate_poly_to_edge
  PUBLIC :: interpolate_edge_to_poly
  PUBLIC :: interpolate_poly_to_node
  PUBLIC :: interpolate_node_to_poly
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data types, internal data and methods
  ! ---------------------------------------------------------------------
  !
  !! name of the module 
  CHARACTER (LEN=26) , PARAMETER :: & 
       c_modname='m_dredgesim_interpolations' ! 
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
  !! interpolation from polygons to edges for a scalar function 
  !! subroutine does not throw any error messages
  FUNCTION interpolate_poly_to_edge_0 &
       ( poly_values ) & 
       RESULT ( var )
    !
    !! variabless
    INTEGER :: i, j !
    !! values on polygons
    REAL ( KIND = Double ), INTENT (IN) ::  poly_values (:)
    !! result : values on edges
    REAL ( KIND = Double ) ::  var( SIZE(edge_water_depth))
    !
    ! [1.1] initial values and total edge related area
    edge_related_surface = 0.0_Double
    edge_weighted_sum    = 0.0_Double
    ! [1.2] interpolation
    DO i = 1, SIZE(edgelist_of_poly,1) ! loop over all polygons
       DO j = 1, nodes_of_poly(i) ! loop over the edges of the polygons
          ! calculation of the edge related total areas           
          edge_related_surface(edgelist_of_poly(i,j)) = & 
               edge_related_surface(edgelist_of_poly(i,j)) + &
               poly_area(i)
          edge_weighted_SUM(edgelist_of_poly(i,j)) = & 
               edge_weighted_SUM(edgelist_of_poly(i,j)) + & 
               poly_values(i) * poly_area(i)
       END DO
    END DO
    var = edge_weighted_sum / edge_related_surface     
    !
  END FUNCTION interpolate_poly_to_edge_0
  !
  !! interpolation from polygons to edges for a vector function 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_poly_to_edge_1 &
       ( poly_values ) & 
       RESULT ( var )
    !
    !! variabless
    INTEGER :: i !
    !! values on polygons
    REAL ( KIND = Double ), INTENT (IN) ::  poly_values (:,:)
    !! result : values on edges
    REAL ( KIND = Double ) ::  var( SIZE(edge_water_depth), SIZE(poly_values,2) )
    ! [1.1] loop over the vector
    DO i = 1, SIZE(poly_values (:,:),2) 
       var(:,i) = interpolate_poly_to_edge_0 ( poly_values(:,i) )
    END DO
  !
  END FUNCTION interpolate_poly_to_edge_1
  !
  !! interpolation from polygons to edges for a matrix function 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_poly_to_edge_2 &
       ( poly_values ) & 
       RESULT ( var )
    !
    !! variabless
    INTEGER :: i!
    !! values on polygons
    REAL ( KIND = Double ), INTENT (IN) ::  poly_values (:,:,:)
    !! result : values on edges
    REAL ( KIND = Double ) ::  var( SIZE(edge_water_depth), SIZE(poly_values,2), & 
         SIZE(poly_values,3))
    ! [1.1] loop over the last index 
    DO i = 1, SIZE(poly_values (:,:,:),3) 
       var(:,:,i) = interpolate_poly_to_edge_1 ( poly_values(:,:,i) )
    END DO
  !
  END FUNCTION interpolate_poly_to_edge_2
  !! interpolation from edges to polys for a scalar function 
  !! attention: interpolation is done with equal weights per edge 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_edge_to_poly_0 &
       ( edge_values ) & 
       RESULT ( var )
    !! variabless
    INTEGER :: i !
	!! values on edges
    REAL ( KIND = Double ), INTENT (IN) ::  edge_values (:)
    !! result : values on polys
    REAL ( KIND = Double ) ::  var( SIZE(edgelist_of_poly,1) )
	!
    ! [1.1] 
    !
    DO i = 1, SIZE(edgelist_of_poly,1) ! loop over all polygons
       !                                           ... use only active edges (1:nodes_of_poly)
       var(i) = SUM(edge_values(edgelist_of_poly(i,1:nodes_of_poly(i)))) / nodes_of_poly(i)
    END DO
    !
  END FUNCTION interpolate_edge_to_poly_0
  !
  !! interpolation from edges to polys for a vector function 
  !! attention: interpolation is done with equal weights per edge 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_edge_to_poly_1 &
       ( edge_values ) & 
       RESULT ( var )
    !! variabless
    INTEGER :: i !
    !! values on edges
    REAL ( KIND = Double ), INTENT (IN) ::  edge_values (:,:)
	!! result : values on polys
    REAL ( KIND = Double ) ::  var( SIZE(edgelist_of_poly,1), SIZE(edge_values,2) )
	!
    ! [1.1] loop over the vector
    DO i = 1, SIZE(edge_values,2) 
       var(:,i) = interpolate_edge_to_poly_0 ( edge_values(:,i) )
    END DO
  !
  END FUNCTION interpolate_edge_to_poly_1
  !
  !! interpolation from edges to polys for a matrix function 
  !! attention: interpolation is done with equal weights per edge 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_edge_to_poly_2 &
       ( edge_values ) & 
       RESULT ( var )
    !! variabless
    INTEGER :: i!
    !! values on edges 
    REAL ( KIND = Double ), INTENT (IN) ::  edge_values (:,:,:)
	!! result : values on polys
    REAL ( KIND = Double ) ::  var( SIZE(edgelist_of_poly,1), SIZE(edge_values,2), & 
         SIZE(edge_values,3))
	!
    ! [1.1] loop over the last index 
    DO i = 1, SIZE(edge_values (:,:,:),3) 
       var(:,:,i) = interpolate_edge_to_poly_1 ( edge_values(:,:,i) )
    END DO
  !
  END FUNCTION interpolate_edge_to_poly_2
  !! interpolation from polygons to nodes for a scalar function 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_poly_to_node_0 &
       ( poly_values ) & 
       RESULT ( var )
    !
    !commented as no parallel structure first and no service modules delivering required functions
    !
    !USE m_sedimorph_parallel_xchange, ONLY : &
    !     parallel_xchange_of_nodes
    !
    !! integer counter
    INTEGER :: i, j !
    !! values on polygons
    REAL ( KIND = Double ), INTENT (IN) ::  poly_values (:)
    !! result : values on nodes
    REAL ( KIND = Double ), TARGET ::  var( SIZE(node_depth) )
    !
    ! initial values and total patch areas
    node_weighted_sum    = 0.0_Double
    node_related_surface = 0.0_Double
!$OMP PARALLEL DO PRIVATE(j)
    DO i = 1, get_nof_poly( ) ! loop over all polygons
       DO j = 1, nodes_of_poly(i) ! loop over the nodes of the polygons
          ! calculation of the total patch areas 
          node_related_surface(nodelist_of_poly(i,j)) = & 
               node_related_surface(nodelist_of_poly(i,j)) + poly_area(i)
          node_weighted_sum(nodelist_of_poly(i,j)) = & 
               node_weighted_sum(nodelist_of_poly(i,j)) + & 
               poly_values(i) * poly_area(i)
       END DO
    END DO
!$OMP END PARALLEL DO
    !
    ! [1.2] 
    !
    ! commented as no parallel structure so far and therefore no service modules delivering required functions
    ! summation of node data coming from the nodes of neighbour processors
    !CALL parallel_xchange_of_nodes ( 2, node_weighted_sum )
    !CALL parallel_xchange_of_nodes ( 2, node_related_surface )
    !
    ! [1.3] 
    ! weighted means
    var = node_weighted_sum / node_related_surface
    !
  END FUNCTION interpolate_poly_to_node_0
  !
  !! interpolation from polygons to nodes for a vector function 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_poly_to_node_1 &
       ( poly_values ) & 
       RESULT ( var )
    !! integer counter
    INTEGER :: i!
    !! values on polygons
    REAL ( KIND = Double ), INTENT (IN) ::  poly_values (:,:)
    !! result : values on nodes
    REAL ( KIND = Double ), TARGET ::  var( SIZE(node_depth), SIZE(poly_values,2) )
    ! [1.1] loop over the vector
    DO i = 1, SIZE(poly_values (:,:),2) 
       var(:,i) = interpolate_poly_to_node_0 ( poly_values(:,i) )
    END DO
    !
  END FUNCTION interpolate_poly_to_node_1
  !
  !! interpolation from polygons to nodes for a matrix function 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_poly_to_node_2 &
       ( poly_values ) & 
       RESULT ( var )
    !! integer counter
    INTEGER :: i!
    !! values on polygons
    REAL ( KIND = Double ), INTENT (IN) ::  poly_values (:,:,:)
    !! result : values on nodes
    REAL ( KIND = Double ), TARGET ::  var( SIZE(node_depth), SIZE(poly_values,2), & 
         SIZE(poly_values,3))
    ! [1.1] loop over the last index 
    DO i = 1, SIZE(poly_values (:,:,:),3) 
       var(:,:,i) = interpolate_poly_to_node_1 ( poly_values(:,:,i) )
    END DO
    !
  END FUNCTION interpolate_poly_to_node_2
  !
  !! interpolation from nodes to polys for a scalar function 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_node_to_poly_0 &
       ( node_values ) & 
       RESULT ( var )
    !
    !! integer counter
    INTEGER :: i, j !
    !! values on nodes
    REAL ( KIND = Double ), INTENT (IN) ::  node_values (:)
    !! result : values on polygons
    REAL ( KIND = Double ), TARGET ::  var( SIZE(nodelist_of_poly,1) )
    !
    ! [1.1] interpolation
    !
    var = 0._Double
    !
    DO i = 1, get_nof_poly ( )
       DO j = 1, nodes_of_poly(i)
          !LEO Fortran runtime error: Index '340' of dimension 1 of array 'node_values' above upper bound of 0
          var(i) = var(i) + node_values(nodelist_of_poly(i,j)) 
       END DO
    END DO
    !
    var = var / nodes_of_poly
    !
  END FUNCTION interpolate_node_to_poly_0
  !
  !! interpolation from nodes to polygons for a vector function 
  !! subroutine does not throw  any error messages
  FUNCTION interpolate_node_to_poly_1 &
       ( node_values ) & 
       RESULT ( var )
    !
    !! integer counter
    INTEGER :: i !
    !! values on nodes
    REAL ( KIND = Double ), INTENT (IN) ::  node_values (:,:)
    !! result : values on polygons
    REAL ( KIND = Double ), TARGET ::  var( SIZE(nodelist_of_poly,1), SIZE(node_values,2) )
    ! [1.1] loop over the vector
    DO i = 1, SIZE(node_values (:,:),2) 
       var(:,i) = interpolate_node_to_poly_0 ( node_values(:,i) )
    END DO
  !
  END FUNCTION interpolate_node_to_poly_1
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
END MODULE m_dredgesim_interpolations
! TailOfPackageModule -----------------------------------------------------
